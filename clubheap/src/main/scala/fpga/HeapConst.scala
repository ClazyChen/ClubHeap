package fpga

import chisel3._
import chisel3.util._
import scala.math._

object HeapConst {

    // the width of the rank ( priority ) of each entry in the heap
    //     (i.e. log2(P) in the paper)
    val rank_width = 16

    // the width of the metadata of each entry in the heap
    val metadata_width = 32

    // count of ways in the heap
    // NOTE: ClubHeap is a 2-way heap
    //       (the count of ways is 2)
    val count_of_ways = 2

    // count of levels in the heap
    val count_of_levels = 13

    // count of logical partitions in the heap
    //     (i.e. M in the paper)
    val count_of_partitions = 16

    // count of elements in each cluster ( node in the heap )
    //     (i.e. K in the paper)
    val count_of_elements_in_each_cluster = 16

    // count of priorities in the heap
    //     (i.e. P in the paper)
    val count_of_priorities = pow(2, rank_width).toInt

    // the maximum number of entries in a heap with a given count of levels
    def max_entries(levels: Int): Int = {
        ceil(count_of_elements_in_each_cluster.toDouble * ((1 - pow(count_of_ways, levels)).toDouble / (1 - count_of_ways).toDouble)).toInt
    }

    // count of maximum entries in the heap
    //     (i.e. N in the paper)
    val count_of_max_entries = max_entries(count_of_levels)

    // the width of the difference field in a cluster
    //     if the cluster is at the given level (starting from 1)
    //     then the difference <= max entries of the a full heap at the lower level
    //     (i.e. log2(max_entries(count_of_levels - level)))
    def diff_width(level: Int): Int = {
        val entries = max_entries(count_of_levels - level)
        if (entries <= 1) {
            0
        } else {
            log2Ceil(entries)
        }
    }

    // the capacity of the memory (number of nodes / clusters) on a level
    // - caculated by Corollary 2 in the paper
    
    // the static capacity means every partition has the same number of nodes
    def static_capacity(level: Int): Int = count_of_partitions * (1 << (level-1))

    // the dynamic capacity means the partitions share dynamically allocated nodes
    def dynamic_capacity(level: Int): Int = {
        round((count_of_max_entries.toDouble / (
            1.0 + count_of_elements_in_each_cluster * (
                1 - 1.0 / (1 << (level - 1))
            )
        )).toFloat)
    }

    // the capacity of the memory (number of nodes / clusters) on a level
    def capacity(level: Int): Int = if (level == 0) 1 else min(static_capacity(level), dynamic_capacity(level))

    // if the dynamic capacity is less than the static capacity, then the memory is dynamic
    def is_dynamic_memory(level: Int): Boolean = if (level == 0) false else dynamic_capacity(level) < static_capacity(level)

    // if the next level is dynamic memory, then this level should be dynamic cluster
    def is_dynamic_cluster(level: Int): Boolean = if (level == 0) false else level < count_of_levels && is_dynamic_memory(level + 1)

    // the width of memory data
    // the accurate width of the memory data
    def data_width(level: Int) = {
        if (level == 0) {
            (new Entry).getWidth
        } else {
            val K = count_of_elements_in_each_cluster
            val is_the_last_level = level == count_of_levels
            if (is_the_last_level) {
                // special cluster (last level)
                // the last level has no min_lc, min_rc, diff, and next
                (Vec(K-1, new Entry)).getWidth
            } else {
                if (is_dynamic_cluster(level)) {
                    // dynamic cluster
                    (new Cluster(level)).getWidth
                } else {
                    // static cluster
                    (new StaticCluster(level)).getWidth
                }
            }
        }
    }

    // the memory depth of the memory on a level
    def data_depth(level: Int): Int = if (level <= 0) 1 else max(1, capacity(level) / 2)

    // the width of the link field of each entry in the heap
    //     (if using the dynamic memory model)
    // NOTE: The highest bit of the link field is used to represent null pointer
    //       (i.e. 1xxxxxx means null pointer, 0xxxxxx means valid pointer)
    def link_width(level: Int): Int = if (level == 0) 0 else log2Ceil(data_depth(level + 1)) + 1
    
    // the width of the address field of each entry in the heap
    def addr_width(level: Int): Int = if (level == 0) 0 else link_width(level-1)

    // physical address width = addr width of the previous level + 1
    def paddr_width(level: Int): Int = if (level == 0) 0 else addr_width(level-1) + 1
}
