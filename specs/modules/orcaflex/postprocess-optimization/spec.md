# OrcaFlex Postprocess Runtime Optimization Specification

## Overview
Comprehensive optimization strategy for OrcaFlex postprocessing batch runs to minimize runtime while maximizing resource utilization on high-performance hardware (64 CPUs, 256GB RAM).

## Problem Statement

### Current Situation
- Two batch runs using `digitalmodel` module for OrcaFlex postprocessing
- Both configured with 30 parallel threads (default)
- Significantly different .sim file sizes between batches
- Runtime performance not optimized for available hardware
- No runtime metrics or performance profiling available

### Batch Run Configurations
1. **LNGC Batch** (`dm_fsts_lngc.yml`):
   - Pattern: `fsts_l015_hwl_125km3_l100_pb_005yr__cl_015deg`
   - Label: `03c_0100yr_l015_hwl`
   - Expected: Larger .sim files (LNGC vessel models)

2. **Standard Batch** (`dm_fsts.yml`):
   - Pattern: `fsts_l015_hwl_ncl_000deg`
   - Label: `03c_0100yr_l015_hwl`
   - Expected: Smaller .sim files (standard models)

### Hardware Resources
- **CPUs**: 64 logical processors
- **RAM**: 256 GB
- **OS**: Windows
- **Python Environment**: UV-managed virtual environment

## Proposed Solution

### Multi-Level Optimization Strategy

#### 1. Dynamic Thread Allocation
Implement adaptive thread count based on:
- File size distribution
- Available system resources
- Memory pressure monitoring
- I/O vs CPU-bound operation detection

#### 2. Hybrid Processing Model
Combine process-level and thread-level parallelism:
- Process pool for file loading/parsing (I/O heavy)
- Thread pool for numerical computations (CPU heavy)
- Shared memory for large data structures

#### 3. Performance Profiling System
Real-time monitoring and optimization:
- Runtime metrics collection
- Resource utilization tracking
- Bottleneck identification
- Automatic parameter tuning

## Technical Architecture

### Optimization Components

#### 1. Resource Manager
```python
class ResourceManager:
    """Dynamically allocate resources based on workload"""
    
    def calculate_optimal_threads(self, file_sizes, available_ram):
        # Account for OrcaFlex memory overhead
        avg_file_size = np.mean(file_sizes)
        orcaflex_overhead = 1.5  # 50% overhead for processing
        memory_per_thread = avg_file_size * orcaflex_overhead
        
        # Calculate based on RAM constraints
        max_threads_memory = available_ram / memory_per_thread
        max_threads_cpu = cpu_count() * 0.75  # Leave 25% for system
        
        return min(max_threads_memory, max_threads_cpu)
```

#### 2. Batch Optimizer
```python
class BatchOptimizer:
    """Optimize batch processing based on file characteristics"""
    
    def group_by_size(self, files):
        # Group similar-sized files for balanced processing
        small = [f for f in files if f.size < 100MB]
        medium = [f for f in files if 100MB <= f.size < 500MB]
        large = [f for f in files if f.size >= 500MB]
        return small, medium, large
    
    def assign_thread_counts(self, groups):
        # Different thread counts for different file sizes
        return {
            'small': 45,   # More parallelism for small files
            'medium': 30,  # Balanced approach
            'large': 15    # Less parallelism, more memory per thread
        }
```

#### 3. Performance Monitor
```python
class PerformanceMonitor:
    """Track and analyze runtime performance"""
    
    def __init__(self):
        self.metrics = {
            'file_processing_times': [],
            'memory_usage': [],
            'cpu_utilization': [],
            'io_wait_time': [],
            'thread_efficiency': []
        }
    
    def analyze_bottlenecks(self):
        # Identify performance bottlenecks
        if np.mean(self.metrics['io_wait_time']) > threshold:
            return 'IO_BOUND'
        elif np.mean(self.metrics['cpu_utilization']) > 90:
            return 'CPU_BOUND'
        elif np.mean(self.metrics['memory_usage']) > 80:
            return 'MEMORY_BOUND'
        return 'BALANCED'
```

## Agent Delegation Mapping

### Primary Agent
- **OrcaFlex Agent**: Handles all OrcaFlex-specific operations

### Supporting Agents
- **Performance Agent**: Monitors and optimizes runtime
- **DevOps Agent**: System resource management
- **Testing Agent**: Performance benchmarking

## Optimization Strategies

### 1. Thread Count Optimization
Based on system analysis (64 CPUs, 256GB RAM):

#### Recommended Thread Counts by File Size
| File Size | Thread Count | Memory/Thread | Rationale |
|-----------|-------------|---------------|-----------|
| < 100MB | 45 threads | ~5GB | High parallelism, low memory needs |
| 100-500MB | 30 threads | ~8GB | Balanced approach (current) |
| > 500MB | 15 threads | ~16GB | Memory-intensive processing |

### 2. Memory Management
- **Pre-allocation**: Reserve memory pools for known operations
- **Streaming**: Process large files in chunks
- **Garbage Collection**: Explicit cleanup after each file
- **Shared Memory**: Use for read-only data across processes

### 3. I/O Optimization
- **Async I/O**: Non-blocking file operations
- **Batched Reads**: Group small file reads
- **Output Buffering**: Batch write operations
- **SSD Utilization**: Temp files on fastest storage

### 4. CPU Optimization
- **NUMA Awareness**: Pin processes to CPU nodes
- **Cache Optimization**: Process files in cache-friendly order
- **Vectorization**: Use NumPy/Pandas for bulk operations
- **JIT Compilation**: Numba for hot paths

### 5. OrcaFlex-Specific Optimizations
- **Model Caching**: Reuse loaded models when possible
- **Selective Processing**: Skip unnecessary calculations
- **Result Streaming**: Process results as available
- **License Management**: Optimize license token usage

## Performance Benchmarking

### Baseline Metrics (To Establish)
1. **Current Performance** (30 threads):
   - LNGC batch runtime: [TBD]
   - Standard batch runtime: [TBD]
   - Memory peak usage: [TBD]
   - CPU utilization: [TBD]

### Target Metrics
1. **Optimized Performance**:
   - 40-60% runtime reduction
   - 80-90% CPU utilization
   - < 80% memory utilization
   - Linear scaling up to 45 threads

### Benchmark Test Suite
```python
benchmark_configs = [
    {'threads': 15, 'file_size': 'large'},
    {'threads': 30, 'file_size': 'medium'},
    {'threads': 45, 'file_size': 'small'},
    {'threads': 60, 'file_size': 'mixed'},
    {'threads': 'dynamic', 'file_size': 'mixed'}
]
```

## Implementation Plan

### Phase 1: Profiling & Analysis
1. Add timing instrumentation
2. Memory profiling
3. I/O pattern analysis
4. CPU utilization tracking

### Phase 2: Core Optimizations
1. Dynamic thread allocation
2. Memory management improvements
3. I/O optimization
4. Batch grouping by file size

### Phase 3: Advanced Optimizations
1. Hybrid process/thread model
2. NUMA-aware processing
3. JIT compilation for hot paths
4. Predictive resource allocation

### Phase 4: Monitoring & Tuning
1. Real-time dashboard
2. Auto-tuning system
3. Performance regression tests
4. Documentation

## Configuration Schema

### Enhanced Configuration File
```yaml
# dm_fsts_optimized.yml
performance:
  threading:
    mode: dynamic  # static, dynamic, or hybrid
    min_threads: 5
    max_threads: 45
    threads_per_gb: 0.25  # Threads per GB of available RAM
    
  memory:
    max_usage_percent: 80
    gc_threshold: 75  # Trigger GC at 75% usage
    preallocation: true
    chunk_size: 100MB
    
  io:
    async_enabled: true
    buffer_size: 10MB
    use_compression: false
    temp_directory: "D:/temp"
    
  monitoring:
    enabled: true
    interval: 5  # seconds
    export_metrics: true
    metrics_file: "performance_metrics.json"
    
  optimization:
    group_by_size: true
    size_thresholds:
      small: 100MB
      large: 500MB
    thread_allocation:
      small: 45
      medium: 30
      large: 15
```

## Risk Mitigation

### Potential Risks
1. **Memory Exhaustion**: Mitigated by dynamic allocation and monitoring
2. **License Limitations**: Managed through token pooling
3. **I/O Bottlenecks**: Addressed with async operations
4. **Thread Contention**: Resolved with proper synchronization

## Success Metrics
- **Primary**: 40-60% runtime reduction
- **Secondary**: Stable memory usage < 80%
- **Tertiary**: Linear scaling with thread count
- **Quality**: Zero processing failures

## Testing Strategy

### Performance Tests
1. **Baseline Test**: Current 30-thread configuration
2. **Scaling Test**: 15, 30, 45, 60 threads
3. **Memory Test**: Large file processing
4. **Stress Test**: Maximum concurrent files
5. **Regression Test**: Performance consistency

### Validation
- Compare output files (must be identical)
- Verify numerical accuracy
- Check error handling
- Validate resource cleanup

## Future Enhancements
1. **Machine Learning**: Predictive optimization based on file patterns
2. **Distributed Processing**: Multi-machine support
3. **GPU Acceleration**: For numerical computations
4. **Cloud Integration**: Elastic compute resources
5. **Smart Caching**: Intelligent result reuse