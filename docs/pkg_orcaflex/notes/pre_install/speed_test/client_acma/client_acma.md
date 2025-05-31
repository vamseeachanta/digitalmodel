

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

| System/Config             | Threads | Best Time (s) | Actual Scaling | Throughput (s) |
|---------------------------|---------|---------------|----------------|----------------|
| Win Server 2022, 12 cores | 1       | 199.3         | 1.00           |                |
|                           | 6       | 970.3         | 4.87           |                |
|                           | 12      | 1399.2        | 7.02           | 1399.2         |
| Win 10/11, 64c/128t, HT   | 1       | 324.4         | 1.00           |                |
|                           | 32      | 14200.6       | 43.77          |                |
|                           | 64      | 18534.8       | 57.14          |                |
|                           | 128     | 10038.6       | 30.95          | 10038.6        |
| Win 11, 64c, No HT        | 1       | 436.9         | 1.00           |                |
|                           | 32      | 14399.7       | 32.96          |                |
|                           | 64      | 18483.7       | 42.31          | 18483.7        |


Notes:

"HT" = Hyperthreading enabled; "No HT" = Hyperthreading disabled.

"Actual Scaling" is the cumulative simulation time relative to 1 thread (not speedup).

"Throughput (s)" is the best time at the maximum thread count for each system.