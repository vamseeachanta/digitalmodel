# 🔄 MANDATORY: Computational Task Breakdown Protocol

## ⏰ 10-MINUTE RULE

**CRITICAL**: Any task estimated to take >10 minutes compute time MUST be reviewed with user before execution.

## 🚨 MANDATORY ESCALATION FOR LONG TASKS

### When to Escalate:
```python
if any([
    estimated_runtime > 600,  # 10 minutes in seconds
    processing_files > 100,
    data_size > "1GB",
    iterations > 10000,
    parallel_workers > 10,
    "batch processing" in task,
    "large dataset" in description,
    "full analysis" in request
]):
    MUST_ESCALATE_FOR_BREAKDOWN()
```

### Required Escalation Format:
```markdown
🚨 LONG COMPUTATION DETECTED - BREAKDOWN REQUIRED

**Task:** [Description of computational task]
**Estimated Runtime:** [Time estimate]
**Scale:** [Number of files/iterations/data size]

**Current Approach:**
- [Planned method]
- [Resource usage]
- [Expected output]

**My Capabilities Check:**
- [ ] Can handle this data volume
- [ ] Have sufficient memory
- [ ] Algorithm is optimal
- [ ] Parallelization possible
- [ ] Incremental processing available

**Breakdown Options:**

1. **Subset Testing** (Recommended)
   - Process 10% first
   - Validate approach
   - Estimate full runtime
   - Then proceed with full set

2. **Chunked Processing**
   - Divide into [N] chunks
   - Process sequentially
   - Checkpoint between chunks
   - Allow interruption

3. **Parallel Batches**
   - Split into [N] parallel tasks
   - Process concurrently
   - Combine results
   - Monitor progress

4. **Optimized Algorithm**
   - Review current approach
   - Consider alternatives
   - Optimize before running
   - May reduce runtime to [estimate]

**Recommendation:** [Your suggested approach]

**Questions for User:**
1. Is the full dataset needed immediately?
2. Can we validate with a subset first?
3. Are there priority items to process first?
4. Is there a runtime budget/deadline?
```

## 📊 TASK BREAKDOWN STRATEGIES

### 1. Validation-First Approach
```python
def process_with_validation(data):
    # ALWAYS start with small subset
    sample = data[:10]  # or 1% of data
    
    # Validate approach
    result = process_sample(sample)
    validate_output(result)
    
    # Get user confirmation
    get_user_approval(f"""
        Sample processed successfully
        Full dataset will take: {estimate_time(data)}
        Proceed with full processing?
    """)
    
    # Then process full dataset
    if approved:
        process_full(data)
```

### 2. Incremental Processing
```python
def incremental_processing(tasks):
    chunk_size = determine_chunk_size(tasks)
    
    for i, chunk in enumerate(chunks(tasks, chunk_size)):
        # Process chunk
        result = process_chunk(chunk)
        
        # Report progress
        report_progress(f"Completed {i+1}/{total_chunks}")
        
        # Allow user intervention
        if check_user_interrupt():
            save_checkpoint(i, result)
            break
```

### 3. Priority-Based Processing
```python
def priority_processing(items):
    # Get priorities from user
    priorities = request_user_priorities(items)
    
    # Process in priority order
    high_priority = filter_priority(items, "high")
    medium_priority = filter_priority(items, "medium")
    low_priority = filter_priority(items, "low")
    
    # Process with checkpoints
    process_batch(high_priority, "High priority items")
    get_user_confirmation("Continue with medium priority?")
    process_batch(medium_priority, "Medium priority items")
    get_user_confirmation("Continue with low priority?")
    process_batch(low_priority, "Low priority items")
```

## ⚡ OPTIMIZATION BEFORE EXECUTION

### Mandatory Optimization Checks:
1. **Algorithm Efficiency**
   - Is there a faster algorithm?
   - Can we reduce complexity?
   - Are we using optimal data structures?

2. **Parallelization Opportunities**
   - Can task be parallelized?
   - What's the optimal worker count?
   - Is the overhead worth it?

3. **Caching Possibilities**
   - Can results be cached?
   - Are we recalculating anything?
   - Can we reuse previous work?

4. **Resource Optimization**
   - Memory usage acceptable?
   - Disk I/O minimized?
   - Network calls batched?

## 📈 RUNTIME ESTIMATION

### How to Estimate:
```python
def estimate_runtime(task):
    # Test with small sample
    sample_size = min(10, len(task) * 0.01)
    
    start_time = time.time()
    process_sample(task[:sample_size])
    sample_time = time.time() - start_time
    
    # Extrapolate
    estimated_total = (sample_time / sample_size) * len(task)
    
    # Add overhead factor
    overhead_factor = 1.2  # 20% overhead for larger datasets
    estimated_total *= overhead_factor
    
    return estimated_total
```

### Estimation Report:
```
Runtime Estimation:
- Sample size: 10 items
- Sample time: 30 seconds
- Total items: 1000
- Estimated time: ~50 minutes
- Confidence: 80%

⚠️ Exceeds 10-minute threshold - requesting breakdown
```

## 🎯 BREAKDOWN DECISION TREE

```
Task Requested
    │
    ├─> Estimate Runtime
    │   │
    │   ├─> < 10 minutes?
    │   │   └─> ✅ Proceed
    │   │
    │   └─> > 10 minutes?
    │       └─> 🚨 ESCALATE
    │           │
    │           ├─> Get User Input
    │           │   │
    │           │   ├─> Break Down Task?
    │           │   │   └─> Apply Strategy
    │           │   │
    │           │   ├─> Optimize First?
    │           │   │   └─> Review Algorithm
    │           │   │
    │           │   └─> Proceed As Is?
    │           │       └─> Get Confirmation
    │
    └─> Cannot Estimate?
        └─> 🚨 ESCALATE
            └─> Test with Sample First
```

## 🔧 IMPLEMENTATION PATTERNS

### Pattern 1: Always Test First
```python
# MANDATORY for >10 minute tasks
def safe_batch_processing(items):
    # Step 1: Test
    test_items = items[:5]
    test_result = process(test_items)
    
    # Step 2: Validate
    if not validate(test_result):
        escalate("Test failed - need user review")
    
    # Step 3: Estimate
    full_estimate = estimate_runtime(items)
    if full_estimate > 600:  # 10 minutes
        escalate(f"Will take {full_estimate}s - need breakdown strategy")
    
    # Step 4: Process
    return process_all(items)
```

### Pattern 2: Checkpoint Everything
```python
def checkpoint_processing(tasks):
    checkpoint_every = 100  # items
    
    for i in range(0, len(tasks), checkpoint_every):
        batch = tasks[i:i+checkpoint_every]
        
        # Process batch
        result = process_batch(batch)
        
        # Save checkpoint
        save_checkpoint(f"checkpoint_{i}.pkl", result)
        
        # Check if should continue
        if runtime_exceeded():
            notify_user("Runtime limit reached, saved checkpoint")
            break
```

## ⚠️ WARNING SIGNS

### Indicators Task Is Too Large:
- Memory usage growing rapidly
- Disk space filling up
- Progress very slow
- System becoming unresponsive
- Estimated time keeps increasing
- Errors about resource limits

### If Any Warning Sign Appears:
1. **STOP immediately**
2. **Save current progress**
3. **Escalate to user**
4. **Request smaller scope**

## 📝 MANDATORY DOCUMENTATION

### For Every Long Task:
```yaml
task_breakdown:
  original_scope: "Process 10,000 files"
  estimated_runtime: "45 minutes"
  breakdown_strategy: "chunked"
  chunk_size: 1000
  actual_runtime: "42 minutes"
  issues_encountered: ["memory pressure at 7000 files"]
  optimization_opportunities: ["could parallelize file reading"]
```

## 🎯 KEY PRINCIPLES

1. **Never surprise user with long runs**
2. **Always validate approach first**
3. **Provide breakdown options**
4. **Allow interruption/checkpointing**
5. **Document actual vs estimated**
6. **Learn from each run**

---

**REMEMBER**: The 10-minute rule is MANDATORY. When in doubt, break it down!