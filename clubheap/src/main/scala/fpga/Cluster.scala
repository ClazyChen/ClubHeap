package fpga

import chisel3._
import chisel3.util._

// An entry in the cluster
//     including an existing bit, a metadata, and a rank
class Entry extends Bundle {
    val existing = Bool()
    val metadata = UInt(Const.metadata_width.W)
    val rank = UInt(Const.rank_width.W)
}

// An empty entry (as the default value of an entry)
//     - existing is false
//     - rank is +inf
object Entry {
    def empty: Entry = {
        val e = Wire(new Entry)
        e.existing := false.B
        e.metadata := DontCare
        e.rank := (-1).S.asUInt
        e
    }
}

// A cluster of entries
// - entries: the entries in the cluster (excluding the first entry, which is stored in the parent cluster)
// - min_lc: the minimum rank of the left child
// - min_rc: the minimum rank of the right child
// - diff: the difference between the size of the left subtree and the right subtree
// - next: the address of the left and right children on the next level
//         (note that the sister nodes are stored in the same address in a sister memory)
class Cluster(
    val level: Int, // the level of the cluster (starting from 1)
) extends Bundle {
    val K = Const.count_of_elements_in_each_cluster

    // the entries in the cluster
    //     (excluding the first entry, which is stored in the parent cluster)
    val entries = Vec(K-1, new Entry)

    // the stored min_lc and min_rc
    val min_lc = new Entry
    val min_rc = new Entry

    // the stored diff
    val diff = UInt(Const.diff_width(level).W)

    // the stored next
    val next = UInt(Const.link_width(level).W)

    // judge if the cluster is empty
    def is_empty: Bool = !(entries(0).existing)

    // judge if the cluster is full
    def is_full: Bool = entries(K-2).existing

    // judge if min_lc or min_rc exists
    def min_lc_or_min_rc_exists: Bool = min_lc.existing || min_rc.existing

    // convert the dynamic cluster to the static cluster
    def to_static: StaticCluster = {
        val c = Wire(new StaticCluster(level))
        c.entries := entries
        c.min_lc := min_lc
        c.min_rc := min_rc
        c.diff := diff
        c
    }
}

// An empty cluster (as the default value of a cluster)
//     - all entries are empty
//     - min_lc and min_rc are empty
//     - diff is 0
//     - next is -1
object Cluster {
    def empty(level: Int): Cluster = {
        val c = Wire(new Cluster(level))
        c.entries.foreach(_ := Entry.empty)
        c.min_lc := Entry.empty
        c.min_rc := Entry.empty
        c.diff := 0.U
        c.next := (-1).S.asUInt
        c
    }
}

// a static version of the cluster
//     (i.e. no next field)
class StaticCluster(
    val level: Int,
) extends Bundle {
    val entries = Vec(Const.count_of_elements_in_each_cluster-1, new Entry)
    val min_lc = new Entry
    val min_rc = new Entry
    val diff = UInt(Const.diff_width(level).W)

    // convert the static cluster to the dynamic cluster
    def to_dynamic: Cluster = {
        val c = Wire(new Cluster(level))
        c.entries := entries
        c.min_lc := min_lc
        c.min_rc := min_rc
        c.diff := diff
        c.next := DontCare
        c
    }
}

// the operator used in the heap
// ClubHeap supports push and pop operations at the same time
//     (i.e. push and pop are not exclusive)
class Operator extends Bundle {
    
    // push an entry to the heap
    //     if the entry.existing is true, then the entry is pushed to the heap
    //     if the entry.existing is false (i.e. push(+inf)), nothing happens
    val push = new Entry

    // pop an entry from the heap
    val pop = Bool()
}

