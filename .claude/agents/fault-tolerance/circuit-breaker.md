# Circuit Breaker Agent

## Purpose
Provides fault tolerance for agent execution by implementing the circuit breaker pattern to prevent cascading failures and enable automatic recovery.

## Capabilities
- Monitor agent execution failures
- Implement three-state circuit breaker (CLOSED, OPEN, HALF-OPEN)
- Automatic fallback agent selection
- Checkpoint creation and rollback management
- Failure tracking and recovery detection

## Configuration

### Circuit Breaker States

**CLOSED (Normal Operation)**
- All requests pass through to primary agent
- Failures are counted
- Transitions to OPEN after threshold exceeded

**OPEN (Failure State)**
- All requests immediately fail-fast
- Automatic rollback triggered
- Fallback agent selected
- Transitions to HALF-OPEN after timeout

**HALF-OPEN (Recovery Testing)**
- Limited requests allowed through
- Success/failure tracked carefully
- Transitions to CLOSED on success threshold
- Returns to OPEN on any failure

### Parameters

```yaml
failure_threshold: 5           # Failures before OPEN
failure_window: 60            # Seconds to track failures
open_duration: 30             # Seconds before HALF-OPEN
success_threshold: 2          # Successes to close circuit
timeout: 30                   # Agent execution timeout (seconds)
checkpoint_retention: 5       # Number of checkpoints to keep
```

## Memory Keys

### Circuit State (per agent type)
```
fault-tolerance/circuit-breaker/<agent-type>/state
fault-tolerance/circuit-breaker/<agent-type>/failure-count
fault-tolerance/circuit-breaker/<agent-type>/last-failure-time
fault-tolerance/circuit-breaker/<agent-type>/success-count
fault-tolerance/circuit-breaker/<agent-type>/state-changed-at
```

### Metrics
```
fault-tolerance/circuit-breaker/<agent-type>/metrics/success-rate
fault-tolerance/circuit-breaker/<agent-type>/metrics/recovery-time
fault-tolerance/circuit-breaker/<agent-type>/metrics/total-failures
```

### Checkpoints
```
fault-tolerance/rollback/<workflow-id>/checkpoints/<timestamp>
```

## Failure Detection

### What Constitutes a Failure?
1. **Exceptions**: Any unhandled exception during agent execution
2. **Timeouts**: Execution exceeding 30 seconds
3. **Validation Failures**: Output validation errors

All failure types are tracked together for simplicity.

## Fallback Strategy

### Agent Selection
1. Match capabilities of failed agent
2. Check fallback agent circuit state (must be CLOSED)
3. Select first available fallback with matching capabilities
4. If all fallbacks fail: Return clear error to user

### Fallback Registry Example
```json
{
  "coder": ["backend-dev", "sparc-coder"],
  "tester": ["tdd-london-swarm"],
  "researcher": ["code-analyzer"]
}
```

## Checkpoint & Rollback

### Automatic Checkpoints
- Created before each agent execution
- Contains:
  - File contents (changed files)
  - Git commit hash
  - Timestamp
  - Workflow context

### Rollback Triggers
- Automatic when circuit transitions to OPEN
- Restores last known good state
- Reverts file changes
- Updates memory state

### Retention
- Keep last 5 checkpoints per workflow
- Oldest checkpoints purged automatically

## Workflow Integration

### Pre-Execution
1. Check circuit state for agent type
2. If OPEN: Select fallback agent
3. Create checkpoint
4. Update metrics

### Execution
1. Execute agent with timeout monitoring
2. Track execution time
3. Capture exceptions

### Post-Execution
1. Update circuit state based on result
2. Log state transitions
3. Trigger rollback if needed
4. Update success/failure counts
5. Persist metrics

## Monitoring

### Logging
- All state transitions logged to `.claude-flow/circuit-breaker.log`
- Format: `[TIMESTAMP] [AGENT-TYPE] STATE_TRANSITION: CLOSED → OPEN (5 failures)`

### Metrics Tracked
- `success_rate`: Percentage of successful executions
- `recovery_time`: Time to transition from OPEN → CLOSED
- `failure_count`: Total failures per agent type
- `circuit_opens`: Number of times circuit opened

## Usage Example

```python
from digitalmodel.modules.automation.circuit_breaker import CircuitBreaker
from digitalmodel.modules.automation.rollback_manager import RollbackManager

# Initialize
breaker = CircuitBreaker(agent_type="coder")
rollback = RollbackManager(workflow_id="feature-123")

# Create checkpoint
checkpoint_id = rollback.create_checkpoint()

try:
    # Execute with circuit breaker protection
    result = breaker.execute(
        agent_function=coder_agent.run,
        args={"task": "implement feature"},
        fallback_agents=["backend-dev", "sparc-coder"]
    )
except CircuitOpenError as e:
    # Circuit is open, fallback was attempted
    print(f"Circuit open: {e.message}")
    print(f"Using fallback: {e.fallback_agent}")
except AllAgentsFailedError as e:
    # All agents including fallbacks failed
    print(f"All agents failed: {e.message}")
    # Rollback to last checkpoint
    rollback.rollback_to(checkpoint_id)
```

## Agent Coordination

This agent coordinates with:
- **Rollback Manager**: For checkpoint creation and restoration
- **Memory Manager**: For state persistence
- **Task Orchestrator**: For fallback agent selection
- **Performance Monitor**: For metrics tracking

## Error Handling

### Circuit OPEN
- Error message: "Circuit breaker OPEN for agent {type}. Using fallback: {fallback_name}"
- Automatic rollback executed
- User notified of fallback agent

### All Fallbacks Failed
- Error message: "All agents failed for task {task_id}. Last error: {error_message}"
- Manual intervention required
- Rollback to last checkpoint completed

## Recovery

### Automatic Recovery
- Circuit automatically enters HALF-OPEN after 30 seconds
- Next execution attempt monitored closely
- 2 consecutive successes → Circuit CLOSED

### Manual Recovery
```python
breaker.reset()  # Force circuit to CLOSED state
```

## Testing Strategy

### Unit Tests
- State transitions
- Failure counting
- Timeout detection
- Fallback selection

### Integration Tests
- Full workflow with failures
- Rollback verification
- Multi-agent scenarios

### Edge Cases
- Rapid successive failures
- Timeout edge cases
- Checkpoint corruption
- Memory persistence failures
