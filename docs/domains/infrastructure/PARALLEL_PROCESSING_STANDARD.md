# Standardized Parallel Processing in DigitalModel Library

## Overview

The digitalmodel library now implements a **standardized parallel processing system** that provides consistent, intelligent worker allocation across all modules. This system automatically optimizes performance based on available resources and workload characteristics.

## Key Features

### Smart Worker Allocation Logic

The system follows this intelligent allocation strategy:

1. **Default: 30 workers** - Optimized for OrcaFlex servers (typically 30+ cores)
2. **CPU Count Limit** - Never exceeds available CPU cores to prevent oversubscription
3. **Task Count Limit** - Never uses more workers than tasks to avoid waste
4. **User Override** - Respects user-specified worker counts with warnings if excessive

### Automatic Adjustments

```
Workers = MIN(
    user_specified OR default(30),
    cpu_count,
    number_of_tasks
)
```

## Configuration

### Basic Configuration

```yaml
parallel_processing:
  enabled: true        # Enable/disable parallel processing
  max_workers: auto    # 'auto' = 30 (default), or specify number
```

### Configuration Examples

#### 1. Use Default (30 workers, auto-adjusted)
```yaml
parallel_processing:
  max_workers: auto
```

#### 2. Specify Exact Worker Count
```yaml
parallel_processing:
  max_workers: 16
```

#### 3. Disable Parallel Processing
```yaml
parallel_processing:
  enabled: false
```

## Implementation Details

### Centralized Module

All parallel processing logic is centralized in:
```python
digitalmodel.common.parallel_processing
```

### Key Functions

```python
from digitalmodel.common.parallel_processing import should_use_parallel

# Determine optimal parallel processing strategy
use_parallel, num_workers, log_msg = should_use_parallel(
    cfg=cfg,                    # Configuration dictionary
    num_items=len(files),       # Number of items to process
    module_name='Module Name'   # For logging
)
```

### Task Types

The system recognizes different task types:
- **`io_bound`** - File operations, visualization (default: 30 workers)
- **`cpu_bound`** - Computational tasks (default: CPU count)
- **`mixed`** - Combined I/O and CPU tasks (default: 30 workers)

## Usage Examples

### Example 1: Processing 100 Files on 16-Core Machine

```yaml
# Configuration
parallel_processing:
  max_workers: auto  # Requests 30 workers
```

**Result**: Uses 16 workers (limited by CPU count)
```
[Module] Using 16 workers (I/O-bound default: 30, limited by 16 CPU cores)
```

### Example 2: Processing 10 Files on 32-Core Server

```yaml
parallel_processing:
  max_workers: auto  # Requests 30 workers
```

**Result**: Uses 10 workers (limited by task count)
```
[Module] Using 10 workers (I/O-bound default: 30, adjusted to 10 tasks)
```

### Example 3: User Specifies 50 Workers on 16-Core Machine

```yaml
parallel_processing:
  max_workers: 50
```

**Result**: Uses 16 workers with warning
```
WARNING: [Module] Using 16 workers (user-specified: 50, limited by 16 CPU cores)
```

### Example 4: Processing 200 Files on OrcaFlex Server (64 cores)

```yaml
parallel_processing:
  max_workers: auto
```

**Result**: Uses 30 workers (default for I/O-bound tasks)
```
[Module] Using 30 workers (I/O-bound default)
```

## Module Integration

### Modules Using Standardized System

1. **OrcaFlex Visualization** (`opp_visualization.py`)
   - Task type: I/O-bound
   - Default: 30 workers
   - Processing: .sim file visualization

2. **OrcaFlex Post-Processing** (`opp.py`)
   - Task type: Mixed
   - Default: 30 workers
   - Processing: Analysis, summaries, time series

3. **Future Modules**
   - All new modules should use the centralized system
   - Existing modules will be migrated progressively

## Performance Characteristics

### I/O-Bound Tasks (Visualization)
- **Bottleneck**: Disk I/O, not CPU
- **Optimal Workers**: Can exceed CPU count
- **Default**: 30 workers
- **Example**: Loading .sim files, saving images

### CPU-Bound Tasks (Computation)
- **Bottleneck**: CPU processing
- **Optimal Workers**: Equal to CPU count
- **Default**: CPU count
- **Example**: Complex calculations, simulations

### Mixed Tasks (Analysis)
- **Bottleneck**: Both I/O and CPU
- **Optimal Workers**: Balance between I/O and CPU
- **Default**: 30 workers (favor parallelism)
- **Example**: Post-processing with file I/O and calculations

## Best Practices

### For Module Developers

1. **Always use the centralized system**:
```python
from digitalmodel.common.parallel_processing import should_use_parallel
```

2. **Specify task type correctly**:
```python
cfg['parallel_processing']['task_type'] = 'io_bound'  # or 'cpu_bound', 'mixed'
```

3. **Provide meaningful module names** for logging:
```python
use_parallel, num_workers, _ = should_use_parallel(
    cfg, num_items, module_name='Your Module Name'
)
```

### For Users

1. **Use defaults** - The system is optimized for typical workloads
2. **Override only when necessary** - If you have specific performance requirements
3. **Monitor performance** - Check logs to see actual worker allocation
4. **Consider your hardware** - OrcaFlex servers benefit from high worker counts

## Migration Guide

### Updating Existing Modules

Replace old logic:
```python
# OLD
max_workers = cfg.get('max_workers', cpu_count())
if len(items) > 1:
    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        ...
```

With new standardized approach:
```python
# NEW
from digitalmodel.common.parallel_processing import should_use_parallel

use_parallel, num_workers, log_msg = should_use_parallel(
    cfg, len(items), 'Module Name'
)
logger.info(log_msg)

if use_parallel:
    with ProcessPoolExecutor(max_workers=num_workers) as executor:
        ...
```

## Troubleshooting

### Issue: Not Using Expected Number of Workers

Check the logs for the actual allocation:
```
[Module] Using X workers (reason)
```

Common reasons:
- Limited by CPU cores
- Limited by number of tasks
- Parallel processing disabled

### Issue: Poor Performance with Many Workers

Consider:
- Is the task truly I/O-bound?
- Are there resource conflicts (file locks, licenses)?
- Is the system memory sufficient?

### Issue: Want Different Defaults

Override in configuration:
```yaml
parallel_processing:
  max_workers: 16  # Your preferred default
```

## Summary

The standardized parallel processing system provides:

1. **Consistent behavior** across all modules
2. **Intelligent defaults** (30 workers for OrcaFlex servers)
3. **Automatic optimization** based on resources and workload
4. **User control** when needed
5. **Clear logging** of decisions made

This ensures optimal performance across diverse hardware configurations while maintaining simplicity for users.