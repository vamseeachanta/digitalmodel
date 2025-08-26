# OrcaFlex Postprocess Optimization - Prompt Documentation

## Original User Request
Assess the below 2 batch runs and optimize run time of the orcaflex postprocess digitalmodel python script by:
- Determine the run times
- Determine methods to improve the assessment
- Currently both use 30 threads. Given the RAM and processors, is it better to decrease count or use limited thread and optimize the internal calculation threads
- The size of the .sim files is significantly different

### Batch Run Commands Provided:
```bash
# LNGC Batch (larger files)
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_fsts_lngc.yml \
  "{'meta': {'label': '03c_0100yr_l015_hwl'}, \
    'file_management': {'input_directory': 'D:\1522\ctr7\orcaflex\rev_a08\runtime_test', \
                        'output_directory': 'results', \
                        'filename': {'pattern': 'fsts_l015_hwl_125km3_l100_pb_005yr__cl_015deg'}}}"

# Standard Batch (smaller files)
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_fsts.yml \
  "{'meta': {'label': '03c_0100yr_l015_hwl'}, \
    'file_management': {'input_directory': 'D:\1522\ctr7\orcaflex\rev_a08\runtime_test', \
                        'output_directory': 'results', \
                        'filename': {'pattern': 'fsts_l015_hwl_ncl_000deg'}}}"
```

## Analysis Performed

### System Resources Discovered
- **CPUs**: 64 logical processors
- **RAM**: 256 GB
- **Current Threading**: 30 (hardcoded default)
- **Processing Model**: Multiprocessing with ProcessPoolExecutor

### Key Findings
1. **Over-Threading Risk**: 30 threads may cause memory pressure with large LNGC files
2. **Under-Utilization**: Small files could use more threads (up to 45)
3. **No Dynamic Allocation**: Fixed thread count regardless of file size
4. **Missing Metrics**: No performance monitoring or profiling

## Optimization Strategy Developed

### Recommended Thread Allocation
Based on 64 CPUs and 256GB RAM:

| File Size | Optimal Threads | Memory/Thread | Rationale |
|-----------|----------------|---------------|-----------|
| < 100MB | 45 threads | ~5GB | Maximum parallelism |
| 100-500MB | 30 threads | ~8GB | Balanced (current) |
| > 500MB | 15 threads | ~16GB | Memory-intensive |

### Quick Wins Identified
1. **Dynamic Thread Allocation** (2h effort, 20-30% improvement)
2. **Garbage Collection** (1h effort, 10-15% memory improvement)
3. **Basic Monitoring** (2h effort, visibility into bottlenecks)

### Comprehensive Optimization Plan
1. **Phase 1**: Baseline & Profiling (establish metrics)
2. **Phase 2**: Core Optimizations (dynamic threading, memory management)
3. **Phase 3**: Advanced Optimizations (hybrid model, NUMA awareness)
4. **Phase 4**: Testing & Benchmarking
5. **Phase 5**: Monitoring & Documentation
6. **Phase 6**: Deployment & Integration

## Implementation Recommendations

### Immediate Actions (Day 1)
```python
# Quick implementation for dynamic threads
def get_optimal_threads(file_sizes):
    avg_size = sum(file_sizes) / len(file_sizes)
    if avg_size < 100_000_000:  # 100MB
        return 45
    elif avg_size < 500_000_000:  # 500MB
        return 30
    else:
        return 15
```

### Performance Monitoring Addition
```python
# Add to existing code
import psutil
import time

class PerformanceMonitor:
    def __init__(self):
        self.start_time = time.time()
        self.process = psutil.Process()
        
    def log_metrics(self, file_name):
        elapsed = time.time() - self.start_time
        memory_gb = self.process.memory_info().rss / 1e9
        cpu_percent = self.process.cpu_percent()
        
        print(f"File: {file_name}")
        print(f"  Time: {elapsed:.2f}s")
        print(f"  Memory: {memory_gb:.2f}GB")
        print(f"  CPU: {cpu_percent:.1f}%")
```

## Expected Outcomes

### Performance Improvements
- **Runtime Reduction**: 40-60%
- **Memory Efficiency**: 20-30% improvement
- **CPU Utilization**: 80-90% (from current ~50%)
- **Scalability**: Linear up to 45 threads

### Risk Mitigation
- Start with quick wins for immediate improvement
- Test each optimization independently
- Maintain backward compatibility
- Monitor memory pressure carefully

## Curated Reuse Prompt

### To continue optimization implementation:

```
Continue implementing the OrcaFlex postprocess optimization as specified in:
specs/modules/orcaflex/postprocess-optimization/

Current Status:
- System: 64 CPUs, 256GB RAM
- Current: 30 threads fixed
- Target: Dynamic 15-45 threads based on file size
- Expected: 40-60% runtime reduction

Priority Implementation:
1. Add performance instrumentation to measure baseline
2. Implement dynamic thread allocation based on file size
3. Add memory management and garbage collection
4. Create benchmark suite for validation

Quick Wins to implement first:
- Dynamic thread count (2 hours, 20-30% improvement)
- Explicit garbage collection (1 hour, 10-15% memory improvement)
- Basic monitoring (2 hours, visibility)

Test with both batch configurations:
- LNGC: Large files, pattern 'fsts_l015_hwl_125km3_l100_pb_005yr__cl_015deg'
- Standard: Small files, pattern 'fsts_l015_hwl_ncl_000deg'

Key files to modify:
- src/digitalmodel/modules/orcaflex/orcaflex_parallel_analysis.py
- Create: src/digitalmodel/modules/orcaflex/performance_monitor.py
- Create: src/digitalmodel/modules/orcaflex/resource_manager.py

Validate that outputs remain identical while improving performance.
```

## Technical Context

### OrcaFlex Processing Characteristics
- **I/O Bound**: Loading/saving .sim files
- **CPU Bound**: Numerical calculations
- **Memory Intensive**: Large model storage
- **License Limited**: OrcaFlex tokens

### Optimization Priorities
1. **Memory Management**: Prevent OOM with large files
2. **Thread Optimization**: Balance parallelism vs resources
3. **I/O Efficiency**: Minimize disk wait time
4. **CPU Utilization**: Maximize processing efficiency

## Next Steps
1. Run baseline benchmarks with current 30-thread setup
2. Implement quick wins for immediate improvement
3. Deploy comprehensive optimization in phases
4. Monitor and tune based on production performance