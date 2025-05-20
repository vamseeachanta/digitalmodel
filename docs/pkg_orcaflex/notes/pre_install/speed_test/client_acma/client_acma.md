

## Summary

with and Without Hyperthreading

# HyperThreading

Should be turned off

# Display

# OS Request

## Conclusions

https://www.perplexity.ai/search/summarize-both-files-for-speed-2QZspwK1SJ6r2S4_E8nnmw#0

Summary of Speed Test Results
Below is a concise summary comparing the speed and scaling performance from both log files, focusing on simulation run times and parallel scaling.

System Overviews
File	OS/CPU	Physical Cores	Logical Processors	Max Threads Tested
ansys_04_2025_05_05...	Win Server 2022 / 12-core Xeon	12	N/A	12
ansys_05_2025_05_05...	Win 11 Pro / 64-core (128 threads)	64	128	128
Key Speed and Scaling Results
Threads	Best Time (File 1)	Scaling (File 1)	Best Time (File 2)	Scaling (File 2)
1	199.3 s	1.00	324.4 s	1.00
2	394.6 s	1.98	578.6 s	1.78
3/4	591.8 s (3)	2.97 (3)	1329.3 s (4)	4.10 (4)
6/8	970.3 s (6)	4.87 (6)	2522.8 s (8)	7.78 (8)
12/16	1399.2 s (12)	7.02 (12)	5740.3 s (16)	17.70 (16)
32	-	-	14200.6 s	43.77
64	-	-	18534.8 s	57.14
128	-	-	10038.6 s	30.95
Observations
File 1 (12-core system):

Best single-thread time: 199.3 s.

Scaling is nearly linear up to 3 threads, then efficiency drops as more threads are used.

At 12 threads, actual scaling is 7.02 (vs. ideal 12), showing diminishing returns as core count increases.

File 2 (64-core/128-thread system):

Best single-thread time: 324.4 s (slower per-thread than File 1).

Scaling is sub-linear at low thread counts but improves at higher counts, peaking at 57.14x for 64 threads, then dropping to 30.95x for 128 threads.

Throughput increases with more threads, but efficiency (scaling factor per thread) decreases at very high thread counts.

Conclusions
The 12-core system (File 1) delivers faster per-thread performance and maintains good scaling up to its physical core limit, but efficiency drops as it approaches maximum cores.

The 64-core/128-thread system (File 2) has a slower single-thread baseline but achieves much higher total throughput at high thread counts, despite reduced scaling efficiency at the highest levels.

Both systems demonstrate diminishing returns as core/thread counts increase, with the larger system showing more pronounced scaling inefficiencies at extreme thread counts.

In summary:

For single simulations, the 12-core system is faster.

For large numbers of parallel simulations, the 64-core/128-thread system delivers much higher aggregate throughput, though with less efficiency per thread as concurrency increases.