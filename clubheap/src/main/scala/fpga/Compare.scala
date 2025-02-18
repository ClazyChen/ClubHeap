package fpga

import chisel3._
import chisel3.util._
import fpga.UIntExt._

// the COMPARE stage
// ports:
// horizontal (within a level):
// - rdata_lc_in/out: the data of the left child of the entry read (from the COMPARE.PRE stage and to the COMPARE.POST stage)
// - rdata_rc_in/out: the data of the right child of the entry read (from the COMPARE.PRE stage and to the COMPARE.POST stage)
//   one of the above two signals will be replaced by the comparison result
// - select_child_in/out: the selection of the child to be compared (from the COMPARE.PRE stage and to the COMPARE.POST stage)
// - raddr_in/out: the address of the entry actually read (from the COMPARE.PRE stage and to the COMPARE.POST stage)
// - cdata_in: the data of the child to be compared (from the COMPARE.PRE stage)
// - select_child_next_level_out: the selection of the child to be compared on the next level (to the COMPARE.POST stage)
// - new_min_valid_out: the validity of the promoted new minimum ranked element (to the COMPARE.POST stage)
// - op_next_level_out: the operation to be performed on the next level (to the COMPARE.POST stage)
// vertical (between levels):
// - op_prev_in: the operation to be performed on this level (from the COMPARE stage on the previous level)
// - link_next_in: the address of children on the next level (from the READ stage on the next level)
// - new_min_prev_out: the new minimum ranked element in cdata_in (if it is popped, to the COMPARE.POST stage on the previous level)

class Compare(val level: Int) extends Module {

    val K = Const.count_of_elements_in_each_cluster

    val io = IO(new Bundle {
        val rdata_lc_in = Input(new Cluster(level))
        val rdata_rc_in = Input(new Cluster(level))
        val cdata_in = Input(new Cluster(level))
        val select_child_in = Input(Bool())
        val raddr_in = Input(UInt(Const.link_width(level).W))

        val rdata_lc_out = Output(new Cluster(level))
        val rdata_rc_out = Output(new Cluster(level))
        val select_child_out = Output(Bool())
        val select_child_next_level_out = Output(Bool())
        val new_min_valid_out = Output(Bool())
        val op_next_level_out = Output(new Operator)
        val raddr_out = Output(UInt(Const.link_width(level).W))

        val op_prev_in = Input(new Operator)
        val new_min_prev_out = Output(new Entry)

        val link_next_in = Input(UInt(Const.link_width(level).W))
        
    })

    // the data after comparison
    val new_data = Wire(new Cluster(level))
    new_data := io.cdata_in

    // compare the rank of the new element with the rank of the existing elements
    val cmp = Wire(Vec(K, Bool()))
    for (i <- 0 until K-1) {
        cmp(i) := io.op_prev_in.push.rank < io.cdata_in.entries(i).rank
    }
    // note that for the last element, we need to compare the rank of the new element with the ranks of min_lc and min_rc
    val cmp_min_lc = io.op_prev_in.push.rank < io.cdata_in.min_lc.rank
    val cmp_min_rc = io.op_prev_in.push.rank < io.cdata_in.min_rc.rank
    val cmp_lc_rc = io.cdata_in.min_lc.rank < io.cdata_in.min_rc.rank
    cmp(K-1) := cmp_min_lc && cmp_min_rc
    val min_lc_rc = Mux(cmp_min_lc, io.cdata_in.min_lc, io.cdata_in.min_rc)
    
    // if we need to pop an element to the previous level, we select it as the new min
    //    new_min is selected from the minimum of existing elements or the new element
    val new_min = Mux(cmp(0), io.op_prev_in.push, io.cdata_in.entries(0))
    io.new_min_prev_out := new_min

    // select the child to be compared on the next level
    // for push operation (insertion-balance)
    //     select the child with fewer existing elements
    // for pop operation (by rank)
    //     select the child with a lower rank [min_lc, min_rc]
    // select_child_next = 0 : left child, 1 : right child
    val select_child_next = Mux(
        io.op_prev_in.pop,
        !cmp_lc_rc,
        io.cdata_in.diff.select_highest
    )
    io.select_child_next_level_out := RegNext(select_child_next)

    // directly forward the address to the COMPARE.POST stage (1 cycle delay)
    io.raddr_out := RegNext(io.raddr_in)

    // update the difference field
    // (child +1): push and full
    // (child -1): pop and (min_lc or min_rc exists)
    // otherwise, no change
    val child_to_push = io.op_prev_in.push.existing && !io.op_prev_in.pop && io.cdata_in.is_full
    val child_to_pop = io.op_prev_in.pop && !io.op_prev_in.push.existing && io.cdata_in.min_lc_or_min_rc_exists
    new_data.diff := io.cdata_in.diff
    when (child_to_push) {
        new_data.diff := Mux(
            io.cdata_in.diff.select_highest,
            io.cdata_in.diff + 1.U,
            io.cdata_in.diff - 1.U
        )
    }
    when (child_to_pop) {
        new_data.diff := Mux(
            cmp_lc_rc,
            io.cdata_in.diff - 1.U,
            io.cdata_in.diff + 1.U
        )
    }
    
    // update the entries of the cluster
    // (similar to a shift register)
    for (i <- 0 until K-1) {
        when (io.op_prev_in.pop) {
            // pop or replace operation
            // note that a pop operation can be considered as a replace operation
            //    (replace the minimum ranked element with a infinity)
            // there are three cases:
            // 1. shift-left : e'(i) = e(i+1) , when cmp(i) = 0 and cmp(i+1) = 0
            // 2. replace    : e'(i) = push   , when cmp(i) = 0 and cmp(i+1) = 1
            // 3. no change  : e'(i) = e(i)  , otherwise
            // here, e(K-1) is considered as min( min_lc, min_rc )
            //       which is the entry to be promoted from the next level
            new_data.entries(i) := Mux(cmp(i),
                io.cdata_in.entries(i),
                Mux(cmp(i+1),
                    io.op_prev_in.push,
                    if (i == K-2) {
                        min_lc_rc
                    } else {
                        io.cdata_in.entries(i+1)
                    }
                )
            )
        } .otherwise {
            // push operation
            // there are also three cases:
            // 1. shift-right : e'(i) = e(i-1) , when cmp(i) = 1 and cmp(i-1) = 1
            // 2. replace     : e'(i) = push   , when cmp(i) = 1 and cmp(i-1) = 0
            // 3. no change   : e'(i) = e(i)  , otherwise
            // here, e(-1) is considered as -infinity , i.e. cmp(-1) = 0 always holds
            new_data.entries(i) := (if (i == 0) {
                new_min
            } else {
                Mux(cmp(i),
                    Mux(cmp(i-1),
                        io.cdata_in.entries(i-1),
                        io.op_prev_in.push
                    ),
                    io.cdata_in.entries(i)
                )
            })

            // update the min_lc and min_rc fields
            // in push operation, min_lc | min_rc may be updated from the new element
            // there are three cases:
            // 1. shift-right : min_lc' = e(K-2) , when cmp(K-2) = 1 and cmp_min_lc and lc is selected
            // 2. replace     : min_lc' = push   , when cmp(K-2) = 0 and cmp_min_lc and lc is selected
            // 3. no change   : min_lc' = min_lc , otherwise
            when (cmp_min_lc && !io.cdata_in.diff.select_highest) {
                new_data.min_lc := Mux(cmp(K-2),
                    io.cdata_in.entries(K-2),
                    io.op_prev_in.push
                )
            }
            when (cmp_min_rc && io.cdata_in.diff.select_highest) {
                new_data.min_rc := Mux(cmp(K-2),
                    io.cdata_in.entries(K-2),
                    io.op_prev_in.push
                )
            }
        }
    }

    // update the next field
    new_data.next := io.link_next_in

    // generate the operation to be performed on the next level
    // for push operation:
    //     if lc is selected and cmp_min_lc = 1 ( min_lc is replaced by the new element or e(K-2) )
    //        then we push the old new_min to the next level
    //     rc is similar
    // for pop operation: just forward the operation to the next level
    // for replace operation:
    //     if lc is selected and cmp_min_lc = 1
    //        then the actual replace can be done in this level
    //        so we emit a replace operation with the old new_min, i.e., replace new_min with new_min itself
    //        and it will be immediately selected as new_min back to this level (equivalent to no-op)
    //     rc is similar
    val op_next_level = Reg(new Operator)
    op_next_level := io.op_prev_in
    when (cmp_min_lc && !select_child_next) {
        op_next_level.push := io.cdata_in.min_lc
    }
    when (cmp_min_rc && select_child_next) {
        op_next_level.push := io.cdata_in.min_rc
    }
    io.op_next_level_out := op_next_level

    // the validity of the promoted new minimum ranked element
    io.new_min_valid_out := io.op_prev_in.pop

    // replace the selected child with the new data
    io.rdata_lc_out := RegNext(Mux(
        io.select_child_in, // if the left child is not selected
        io.rdata_lc_in, // use the old data
        new_data // otherwise, use the new data
    ))
    io.rdata_rc_out := RegNext(Mux(
        io.select_child_in, // if the right child is selected
        new_data, // use the new data
        io.rdata_rc_in // otherwise, use the old data
    ))

    // delay 1 cycle to forward to the COMPARE.POST stage
    io.select_child_out := RegNext(io.select_child_in)
}

