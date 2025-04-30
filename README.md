# ClubHeap

This repo provides the open-source ClubHeap codes. ClubHeap (Clustered Binary Heap) is a heap-based priority queue data structure which supports fully pipelined implementation. Our experimental results show that it supports 100Gbps scheduling on an Alveo U280 FPGA.

```
Our experimental environment
- Chisel 6.3.0
- sbt 1.8.2
- Java 11.0.20
```

We are working to improve the performance of ClubHeap, so it may be different from the architectural design in NSDI 25 paper.

If you find this structure useful, please cite our paper:

```
Zhikang Chen, Haoyu Song, Zhiyu Zhang, Yang Xu, and Bin Liu. ClubHeap: A high-speed and scalable priority queue for programmable packet scheduling. In Proceedings of the 22nd USENIX Symposium on Networked Systems Design and Implementation (NSDI '25). USENIX. 2025. pages 1421-1436.
```
