# Work-Stealing Load Balancer

## Overview

The Work-Stealing Scheduler implements a hybrid load balancing strategy for distributing tasks across agents based on their capabilities, load, and performance history.

## Features

- **Hybrid Strategy**: Pull-based (idle agents request work) + Push-based (scheduler intervenes on severe imbalance)
- **Capability-Aware**: Matches tasks to agents based on skill requirements
- **Priority Preservation**: High-priority tasks are protected from work stealing
- **Performance History**: Considers agent success rates when assigning tasks
- **Real-time Metrics**: Comprehensive tracking of work stealing events, utilization, and wait times
- **Thread-Safe**: Concurrent access protected with locks
- **Configurable**: YAML-based configuration for thresholds and policies

## Architecture

### Core Components

1. **WorkStealingScheduler**: Main orchestrator
   - Manages agent registration
   - Handles task submission
   - Coordinates pull/push work stealing
   - Tracks metrics

2. **Agent**: Represents task executor
   - Maintains priority-ordered task queue
   - Tracks capabilities and performance
   - Supports work stealing from queue

3. **Task**: Work unit
   - Skill requirements
   - Priority level (1-5)
   - Dependencies
   - Lifecycle timestamps

4. **WorkStealingMetrics**: Metrics collector
   - Work stealing events
   - Agent utilization samples
   - Task wait times

## Configuration

Location: `config/load-balancing-config.yaml`

### Key Settings

```yaml
load_balancing:
  strategy: "hybrid"  # pull, push, or hybrid

  thresholds:
    busy_threshold: 5        # Queue length threshold for "busy"
    idle_threshold: 2        # Queue length threshold for "idle"
    rebalance_interval_sec: 30  # Push intervention frequency
    severe_imbalance_ratio: 3.0  # Trigger push when max/min > ratio

  work_stealing:
    max_steal_per_cycle: 3      # Max tasks per rebalance
    prefer_recent_tasks: true    # LIFO vs FIFO stealing
    allow_task_migration: false  # Only queued tasks

  capabilities:
    matching_strategy: "best_fit"  # exact, best_fit, fallback
    consider_history: true         # Use performance history
    history_weight: 0.3            # History influence (0-1)

  priority:
    high_priority_threshold: 4    # Priority >= 4 is high
    preserve_high_priority: true  # Protect high-priority from stealing
```

## Usage

### Basic Example

```python
from digitalmodel.workflows.automation import (
    WorkStealingScheduler,
    Agent,
    Task
)

# Create scheduler
scheduler = WorkStealingScheduler()
scheduler.start()

# Register agents
agent1 = Agent(
    agent_id="python_worker",
    capabilities=["python", "testing"]
)
agent2 = Agent(
    agent_id="data_worker",
    capabilities=["python", "data_analysis"]
)

scheduler.register_agent(agent1)
scheduler.register_agent(agent2)

# Submit tasks
task = Task(
    task_id="analyze_data",
    skill_requirements=["data_analysis"],
    priority=3
)

scheduler.submit_task(task)

# Get metrics
metrics = scheduler.get_metrics()
print(f"Tasks migrated: {metrics['tasks_migrated']}")

# Cleanup
scheduler.stop()
```

### Pull-Based Work Stealing

```python
# Idle agent requests work
stolen_task = scheduler.request_work("idle_agent_id")

if stolen_task:
    print(f"Stole task: {stolen_task.task_id}")
```

### Manual Rebalancing

```python
# Trigger push-based rebalancing
scheduler._rebalance_push()
```

### Agent Status

```python
status = scheduler.get_agent_status()

for agent_id, info in status.items():
    print(f"{agent_id}:")
    print(f"  Queue: {info['queue_length']}")
    print(f"  Success rate: {info['success_rate']:.1%}")
    print(f"  Completed: {info['total_completed']}")
```

## Algorithms

### Task Assignment (Best-Fit)

1. Filter agents by capability match (exact for high-priority, fallback for normal)
2. Score each agent:
   ```python
   score = skill_match - (queue_length / 10) + (success_rate * history_weight)
   ```
3. Assign to highest-scoring agent

### Pull-Based Stealing

1. Idle agent requests work
2. Find busiest agents (queue > busy_threshold)
3. Attempt to steal from busiest:
   - Skip high-priority tasks (priority >= 4)
   - Prefer recent tasks if configured (LIFO)
   - Verify capability match
4. Return stolen task or None

### Push-Based Rebalancing

1. Check imbalance: `max_queue / min_queue > threshold`
2. If severe imbalance:
   - Find busy agents (queue > busy_threshold)
   - Find idle agents (queue < idle_threshold)
   - Steal up to `max_steal_per_cycle` tasks
   - Only steal low-priority tasks
   - Match capabilities before assigning

## Metrics

### Available Metrics

```python
metrics = scheduler.get_metrics()
```

Returns:
- `total_steals`: Total steal attempts
- `successful_steals`: Successful steals
- `failed_steals`: Failed steal attempts
- `success_rate`: Steal success rate
- `tasks_migrated`: Total tasks moved
- `rebalance_cycles`: Push interventions
- `avg_task_wait_time`: Average wait in queue
- `agent_count`: Registered agents
- `total_queued`: Currently queued tasks
- `busy_agents`: Agents above busy threshold
- `idle_agents`: Agents below idle threshold

### Utilization Sampling

Scheduler periodically samples agent queue lengths:

```python
# Access samples
samples = scheduler.metrics.utilization_samples

for sample in samples:
    print(f"Time: {sample['timestamp']}")
    print(f"Queues: {sample['queue_lengths']}")
```

## Scenarios

### Hot Spot

**Problem**: One agent overloaded while others idle

**Solution**:
1. Idle agents pull work via `request_work()`
2. Scheduler detects imbalance and triggers `_rebalance_push()`
3. Tasks redistributed to idle agents

### Cascading Failure

**Problem**: Agent fails with queued tasks

**Solution**:
1. Unregister failed agent: `scheduler.unregister_agent(agent_id)`
2. Returns remaining tasks
3. Resubmit tasks to redistribute among healthy agents

### Priority Protection

**Problem**: High-priority tasks delayed by work stealing

**Solution**:
- Tasks with priority >= 4 marked as high-priority
- High-priority tasks excluded from stealing
- Ensures critical work completes first

## Testing

Run comprehensive test suite:

```bash
PYTHONPATH=src python -m pytest tests/domains/automation/test_work_stealing.py -v
```

### Test Coverage

- **Basic Operations**: Registration, submission, capability matching
- **Work Stealing**: Pull/push mechanisms, priority protection
- **Load Scenarios**: Hot spot, uniform load, cascading failure, bursty traffic
- **Metrics**: Tracking, sampling, wait times
- **Edge Cases**: No agents, single agent, empty capabilities
- **Performance**: High throughput, rebalance efficiency

## Demonstration

Run interactive demo:

```bash
python examples/work_stealing_demo.py
```

Demonstrates:
1. Basic work stealing
2. Hot spot handling
3. Cascading failure recovery
4. Priority task handling
5. Comprehensive metrics

## Performance

### Characteristics

- **Throughput**: >10 tasks/second sustained
- **Latency**: <100ms task assignment
- **Rebalancing**: <1s for 20 tasks across 5 agents
- **Memory**: O(n) where n = total tasks queued

### Scalability

- Tested with 10 agents
- Supports 100+ concurrent tasks
- Thread-safe for concurrent submissions
- Background rebalancing doesn't block submissions

## Integration

### With Agent Orchestrator

```python
# Register scheduler agents from orchestrator
for agent in orchestrator.get_agents():
    scheduler_agent = Agent(
        agent_id=agent.id,
        capabilities=agent.skills
    )
    scheduler.register_agent(scheduler_agent)

# Submit orchestrator tasks to scheduler
for task in orchestrator.pending_tasks:
    scheduler_task = Task(
        task_id=task.id,
        skill_requirements=task.requirements,
        priority=task.priority
    )
    scheduler.submit_task(scheduler_task)
```

### With Workflow Systems

```python
# Before workflow execution
scheduler.start()

# During workflow
for workflow_task in workflow.tasks:
    task = Task(
        task_id=workflow_task.id,
        skill_requirements=workflow_task.skills,
        priority=workflow_task.priority,
        dependencies=workflow_task.deps
    )
    scheduler.submit_task(task)

# Monitor progress
while not workflow.complete():
    metrics = scheduler.get_metrics()
    workflow.update_progress(metrics)

# Cleanup
scheduler.stop()
```

## Best Practices

### 1. Capability Design

- Use specific skill names: `"python"`, `"data_analysis"`, not `"general"`
- Keep capability lists small (3-5 per agent)
- Match task requirements precisely to available capabilities

### 2. Priority Assignment

- Reserve priority 5 for critical tasks only
- Use priority 1-3 for normal workload
- Priority 4+ protected from work stealing

### 3. Threshold Tuning

- Busy threshold: Set to average expected queue depth
- Idle threshold: Set to 20-40% of busy threshold
- Rebalance interval: 30s good for most workloads
- Increase interval for stable workloads, decrease for volatile

### 4. Performance Monitoring

```python
# Periodic health check
def check_scheduler_health(scheduler):
    metrics = scheduler.get_metrics()

    # Check for stuck agents
    if metrics['idle_agents'] == 0 and metrics['total_queued'] > 100:
        logger.warning("All agents busy, consider scaling")

    # Check steal efficiency
    if metrics['total_steals'] > 0:
        efficiency = metrics['successful_steals'] / metrics['total_steals']
        if efficiency < 0.5:
            logger.warning(f"Low steal efficiency: {efficiency:.1%}")

    # Check imbalance
    status = scheduler.get_agent_status()
    queue_lengths = [s['queue_length'] for s in status.values()]
    variance = sum((x - sum(queue_lengths)/len(queue_lengths))**2
                   for x in queue_lengths) / len(queue_lengths)

    if variance > 10:
        logger.warning(f"High queue variance: {variance:.1f}")
```

### 5. Error Handling

```python
# Graceful degradation
try:
    scheduler.submit_task(task)
except Exception as e:
    logger.error(f"Task submission failed: {e}")
    # Fallback: direct assignment
    fallback_agent = agents[hash(task.task_id) % len(agents)]
    fallback_agent.enqueue_task(task)
```

## Troubleshooting

### High Failed Steal Rate

**Symptoms**: `failed_steals` >> `successful_steals`

**Causes**:
- Capability mismatch between agents
- Too many high-priority tasks
- Busy threshold too low

**Solutions**:
- Review agent capabilities
- Reduce high-priority task count
- Increase `busy_threshold` in config

### Poor Load Distribution

**Symptoms**: High queue variance, some agents idle while others overloaded

**Causes**:
- Rebalance interval too long
- Steal limits too conservative
- Capability constraints

**Solutions**:
- Decrease `rebalance_interval_sec`
- Increase `max_steal_per_cycle`
- Broaden agent capabilities

### Tasks Never Assigned

**Symptoms**: Tasks submitted but no agent has them

**Causes**:
- No capable agents
- All agents reject task
- Configuration error

**Solutions**:
```python
# Check agent status
status = scheduler.get_agent_status()
if not status:
    logger.error("No agents registered!")

# Check task requirements
if not any(agent.can_execute(task) for agent in agents):
    logger.error(f"No capable agent for task {task.task_id}")
```

## Future Enhancements

1. **Dynamic Capability Learning**: Agents learn new skills over time
2. **Cost-Based Assignment**: Consider task execution cost, not just queue length
3. **Predictive Rebalancing**: ML-based prediction of future load
4. **Multi-Level Queues**: Separate queues per priority level
5. **Distributed Scheduler**: Support multiple scheduler instances
6. **Task Dependencies**: Respect task dependency graphs
7. **Deadline-Aware**: Consider task deadlines in assignment

## References

- Configuration: `config/load-balancing-config.yaml`
- Implementation: `src/digitalmodel/modules/automation/work_stealing_scheduler.py`
- Tests: `tests/domains/automation/test_work_stealing.py`
- Demo: `examples/work_stealing_demo.py`
