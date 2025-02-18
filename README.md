# ClubHeap

This repo provides the open-source ClubHeap codes. ClubHeap (Clustered Binary Heap) is a heap-based priority queue data structure which supports fully pipelined implementation. Our experimental results show that it supports 100Gbps scheduling on an Alveo U280 FPGA.

```
Our experimental environment
- Chisel 5.0.0
- sbt 1.8.2
- Java 11.0.20
```

If you are using a newer version, there may be compilation issues due to compatibility problems. For any other issues you encounter, feel free to ask me in the GitHub issues section.

We plan to consider providing a Verilog version for developers who are not familiar with Chisel.

A paper introducing our design philosophy and key design elements will be published shortly thereafter.