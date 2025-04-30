package fpga

import chisel3._
import chisel3.util._
import fpga.UIntExt._

// The COMPARE stage of a processor
//    this module is the second stage of the ClubHeap+ pipeline

class Compare(val level: Int) extends Module {

    val K = HeapConst.count_of_elements_in_each_cluster

    val io = IO(new Bundle {

        // ports to forwarding
        val c_fwd = Output(new Stash(level))
        val fwd_c = Input(new Stash(level))

        // ports from the READ stage at this level
        val rc = Input(new Stash(level))

        // ports to the PREPARE stage at this level
        val cp = Output(new CPStash(level))
        
        // ports from the PREPARE stage at the previous level
        val op_prev_in = Input(new Operator)
        val sc_prev_in = Input(Bool()) // select the child to be compared

        // ports to the READ stage at the next level
        val addr_next_out = Output(UInt(HeapConst.addr_width(level+1).W))
        val paddr_next_out = Output(UInt(HeapConst.paddr_width(level+1).W))

        // ports to the PREPARE stage at the previous level
        val new_min_prev_out = Output(new Entry)
        val next_prev_out = Output(UInt(HeapConst.addr_width(level).W))
        
    })

    // forwarding
    io.c_fwd <> io.rc
    val c = io.fwd_c

    // generate addr/paddr for the next level
    val sc = io.sc_prev_in
    io.addr_next_out := Mux(sc, c.data_rc.next, c.data_lc.next)
    io.paddr_next_out := Cat(c.addr, Mux(sc, true.B, false.B))

    // select the child to be compared
    val data = Mux(sc, c.data_rc, c.data_lc)
    
    // compare the rank of the new element with the rank of the existing elements
    val cmp = Wire(Vec(K, Bool()))
    for (i <- 0 until K-1) {
        cmp(i) := io.op_prev_in.push.rank < data.entries(i).rank
    }
    val cmp_min_lc = io.op_prev_in.push.rank < data.min_lc.rank
    val cmp_min_rc = io.op_prev_in.push.rank < data.min_rc.rank
    val cmp_lc_rc = data.min_lc.rank < data.min_rc.rank
    cmp(K-1) := cmp_min_lc && cmp_min_rc
    val min_lc_rc = Mux(cmp_min_lc, data.min_lc, data.min_rc)

    // select the new min
    val new_min = Mux(cmp(0), io.op_prev_in.push, data.entries(0))
    io.new_min_prev_out := new_min

    // the comparison part
    val new_data = Wire(new Cluster(level))
    new_data := data

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
                data.entries(i),
                Mux(cmp(i+1),
                    io.op_prev_in.push,
                    if (i == K-2) {
                        min_lc_rc
                    } else {
                        data.entries(i+1)
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
                        data.entries(i-1),
                        io.op_prev_in.push
                    ),
                    data.entries(i)
                )
            })
        }
    }

    // return the next link
    // 1. if new_data is empty, return -1 (invalid link)
    // 2. otherwise, return the address of this cluster
    io.next_prev_out := Mux(new_data.is_empty, (-1).S.asUInt, c.addr)

    // update the min_lc and min_rc fields
    when (io.op_prev_in.pop) {
        // in pop or replace operation, min_lc | min_rc is not updated
        // 1. if selected, the new_min will be promoted from the next level (in the PREPARE stage)
        // 2. if not selected, it will hold the old value
    } .otherwise {
        // in push operation, min_lc | min_rc may be updated from the overflow entry
        // only updated when 1) selected and 2) the overflow entry is smaller
        val overflow_entry = Mux(cmp(K-2), data.entries(K-2), io.op_prev_in.push)
        when (cmp_min_lc && !data.diff.select_highest) {
            new_data.min_lc := overflow_entry
        }
        when (cmp_min_rc && data.diff.select_highest) {
            new_data.min_rc := overflow_entry
        }
    }

    // update the diff field
    // (child +1): push and full
    // (child -1): pop  and has non-empty child (i.e. min_lc or min_rc exists)
    // otherwise, no change
    val child_to_push = io.op_prev_in.push.existing && !io.op_prev_in.pop && data.is_full
    val child_to_pop  = !io.op_prev_in.push.existing && io.op_prev_in.pop && data.min_lc_or_min_rc_exists
    when (child_to_push) {
        new_data.diff := Mux(
            data.diff.select_highest,
            data.diff + 1.U,
            data.diff - 1.U
        )
    }
    when (child_to_pop) {
        new_data.diff := Mux(
            cmp_lc_rc,
            data.diff - 1.U,
            data.diff + 1.U
        )
    }

    // select the child:
    // 1. if pop/replace, select the child with a lower rank (min_lc or min_rc)
    // 2. if push, select the child with fewer existing elements (insertion-balance)
    val sc_next = Mux(
        io.op_prev_in.pop,
        !cmp_lc_rc, // 1 when min_lc < min_rc, lc is selected
        !data.diff.select_highest // 1 when diff < 0, lc is selected
    )
    io.cp.sc := RegNext(sc)
    io.cp.sc_next := RegNext(sc_next)

    // generate the next operation:
    // 1. push - if lc is selected: if e < min_lc, push(min_lc), otherwise push(e)
    // 2. pop / replace - if lc is selected: if e < min_lc, NOP, otherwise replace(e)
    //                    here we consider pop as replace with infinity
    //                    NOTE: NOP is equivalent to replace(min_lc) because it will be selected as new_min back to this level
    val op_next = Wire(new Operator)
    op_next := io.op_prev_in
    when (!sc_next && cmp_min_lc) {
        op_next.push := data.min_lc
    }
    when (sc_next && cmp_min_rc) {
        op_next.push := data.min_rc
    }
    io.cp.op_next := RegNext(op_next)

    // replace the selected child with the new data
    io.cp.data.data_lc := RegNext(Mux(
        sc, c.data_lc, new_data
    ))
    io.cp.data.data_rc := RegNext(Mux(
        !sc, c.data_rc, new_data
    ))

    // directly transfer the address and paddr
    io.cp.data.addr := RegNext(c.addr)
    io.cp.data.paddr := RegNext(c.paddr)
}

