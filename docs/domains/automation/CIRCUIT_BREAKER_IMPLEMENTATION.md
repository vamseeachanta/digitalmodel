# Circuit Breaker Pattern Implementation

## Overview

This document describes the circuit breaker pattern implementation for agent fault tolerance in the digitalmodel project.

## Components

### 1. Circuit Breaker (`circuit_breaker.py`)

Implements the three-state circuit breaker pattern (CLOSED, OPEN, HALF-OPEN) for protecting agent executions from cascading failures.

**Key Features:**
- Automatic failure detection (exceptions, timeouts, validation errors)
- Three-state transitions (CLOSED → OPEN → HALF-OPEN → CLOSED)
- Fallback agent selection with capability matching
- Metrics tracking (success rate, recovery time, failure count)
- Thread-safe concurrent access
- State persistence across sessions

**Configuration:**
```python
CircuitBreakerConfig(
    failure_threshold=5,        # Failures before OPEN
    failure_window=60,          # Seconds to track failures
    open_duration=30,           # Seconds before HALF-OPEN
    success_threshold=2,        # Successes to close circuit
    timeout=30,                 # Agent execution timeout
    checkpoint_retention=5      # Checkpoints to keep
)
```

**Usage:**
```python
from digitalmodel.workflows.automation.circuit_breaker import CircuitBreaker

breaker = CircuitBreaker(agent_type="coder")

try:
    result = breaker.execute(
        agent_function=coder_agent.run,
        args={"task": "implement feature"}
    )
except CircuitOpenError as e:
    print(f"Circuit open, using fallback: {e.fallback_agent}")
```

### 2. Rollback Manager (`rollback_manager.py`)

Manages checkpoint creation and restoration for automatic rollback on failures.

**Key Features:**
- Automatic checkpoint creation before agent execution
- File content snapshots with SHA256 hashing
- Git commit tracking
- Configurable retention policy (last N checkpoints)
- Checkpoint restoration to last known good state

**Usage:**
```python
from digitalmodel.workflows.automation.rollback_manager import RollbackManager

rollback = RollbackManager(workflow_id="feature-123")

# Create checkpoint
checkpoint_id = rollback.create_checkpoint(files=[Path("src/app.py")])

# Restore on failure
rollback.rollback_to(checkpoint_id)
```

### 3. Agent Definition (`.claude/agents/fault-tolerance/circuit-breaker.md`)

Comprehensive documentation of circuit breaker functionality, configuration, and workflow integration.

## Architecture

### Circuit States

```
CLOSED (Normal Operation)
    ↓ (5 failures in 60s)
OPEN (Failure State)
    ↓ (30s timeout)
HALF-OPEN (Recovery Testing)
    ↓ (2 consecutive successes)
CLOSED (Recovered)
```

### Fallback Strategy

1. **Capability Matching**: Select fallback with same capabilities
2. **Registry Lookup**: Use agent registry for primary→fallback mapping
3. **Circuit Check**: Ensure fallback agent circuit is CLOSED
4. **Failure Handling**: If all fallbacks fail, return error to user

**Default Fallback Registry:**
```python
{
    "coder": ["backend-dev", "sparc-coder"],
    "tester": ["tdd-london-swarm"],
    "researcher": ["code-analyzer"],
    "reviewer": ["code-review-swarm"],
    "planner": ["task-orchestrator"],
}
```

### Checkpoint & Rollback Flow

```
Pre-Execution:
1. Create checkpoint (files + git commit)
2. Store in retention buffer

Execution:
3. Monitor for failures/timeouts
4. Track execution metrics

Post-Execution (on failure):
5. Circuit opens → trigger rollback
6. Restore from latest checkpoint
7. Select fallback agent
```

## Memory Integration

Circuit breaker state is persisted to disk for cross-session recovery:

**State Files:**
- `.claude-flow/circuit-state-<agent-type>.json` - Circuit state
- `.claude-flow/checkpoints/<workflow-id>/` - Checkpoint storage
- `.claude-flow/circuit-breaker.log` - State transition logs

**Memory Keys (conceptual):**
```
fault-tolerance/circuit-breaker/<agent-type>/state
fault-tolerance/circuit-breaker/<agent-type>/metrics
fault-tolerance/rollback/<workflow-id>/checkpoints
```

## Monitoring & Metrics

### Tracked Metrics

- **success_rate**: Percentage of successful executions
- **recovery_time**: Time from OPEN to CLOSED (seconds)
- **failure_count**: Total failures per agent type
- **circuit_opens**: Number of times circuit opened
- **total_executions**: All execution attempts

### Logging

All state transitions logged to `.claude-flow/circuit-breaker.log`:

```
[2026-01-06 12:34:56] [circuit-breaker.coder] STATE_TRANSITION: CLOSED → OPEN (5 failures in 60s)
[2026-01-06 12:35:26] [circuit-breaker.coder] STATE_TRANSITION: OPEN → HALF-OPEN (after 30.0s in OPEN)
[2026-01-06 12:35:30] [circuit-breaker.coder] STATE_TRANSITION: HALF-OPEN → CLOSED (2 consecutive successes)
```

## Testing

### Test Coverage

**Circuit Breaker Tests (25 tests):**
- State transitions (CLOSED → OPEN → HALF-OPEN → CLOSED)
- Failure detection (exceptions, timeouts)
- Fallback agent selection
- Metrics tracking
- State persistence
- Concurrent access

**Rollback Manager Tests (22 tests):**
- Checkpoint creation and storage
- File content hashing
- Checkpoint retrieval
- Rollback restoration
- Retention policy enforcement
- State persistence

**Total: 47 tests, 46 passing, 1 skipped (timeout test on Windows)**

### Running Tests

```bash
# Run all automation tests
uv run python -m pytest tests/domains/automation/ -v

# Run circuit breaker tests only
uv run python -m pytest tests/domains/automation/test_circuit_breaker.py -v

# Run rollback manager tests only
uv run python -m pytest tests/domains/automation/test_rollback_manager.py -v
```

## Integration Example

### Full Workflow with Circuit Breaker & Rollback

```python
from digitalmodel.workflows.automation.circuit_breaker import CircuitBreaker
from digitalmodel.workflows.automation.rollback_manager import RollbackManager

# Setup
breaker = CircuitBreaker(agent_type="coder")
rollback = RollbackManager(workflow_id="feature-xyz")

# Create checkpoint before execution
checkpoint_id = rollback.create_checkpoint()

try:
    # Execute with circuit breaker protection
    result = breaker.execute(
        agent_function=coder_agent.run,
        args={"task": "implement authentication"}
    )

    print(f"Success: {result}")

except CircuitOpenError as e:
    # Circuit open, using fallback
    print(f"Primary agent failed, trying fallback: {e.fallback_agent}")

    # Rollback to checkpoint
    rollback.rollback_to(checkpoint_id)

    # Execute fallback agent
    fallback_breaker = CircuitBreaker(agent_type=e.fallback_agent)
    result = fallback_breaker.execute(
        agent_function=get_agent(e.fallback_agent).run,
        args={"task": "implement authentication"}
    )

except Exception as e:
    # All agents failed
    print(f"All agents failed: {e}")
    rollback.rollback_to(checkpoint_id)
    raise

# Get circuit state
state = breaker.get_state()
print(f"Circuit state: {state}")
```

### Multi-Agent Coordination

```python
from digitalmodel.workflows.automation.circuit_breaker import CircuitBreakerManager

# Manager handles multiple agent types
manager = CircuitBreakerManager()

# Execute different agent types
coder_breaker = manager.get_breaker("coder")
tester_breaker = manager.get_breaker("tester")

# Get all circuit states
all_states = manager.get_all_states()

# Reset all circuits if needed
manager.reset_all()
```

## Configuration Recommendations

### Development Environment
```python
CircuitBreakerConfig(
    failure_threshold=3,    # Lower threshold for faster feedback
    failure_window=30,      # Shorter window
    open_duration=10,       # Quick recovery attempts
    success_threshold=1,    # Single success to close
    timeout=60              # Longer timeout for debugging
)
```

### Production Environment
```python
CircuitBreakerConfig(
    failure_threshold=5,    # More tolerance
    failure_window=60,      # Standard window
    open_duration=30,       # Balanced recovery
    success_threshold=2,    # Require consistency
    timeout=30              # Strict timeout
)
```

### High-Availability Environment
```python
CircuitBreakerConfig(
    failure_threshold=10,   # High tolerance
    failure_window=120,     # Longer window
    open_duration=60,       # Conservative recovery
    success_threshold=3,    # Strong consistency requirement
    timeout=20              # Tight timeout
)
```

## Error Handling

### Circuit OPEN
- **Error**: `CircuitOpenError`
- **Behavior**: Immediate fail-fast
- **Fallback**: Automatic fallback agent selection
- **Rollback**: Automatic to last checkpoint

### All Fallbacks Failed
- **Error**: `AllAgentsFailedError`
- **Behavior**: Return error to user
- **Manual Intervention**: Required
- **Rollback**: Completed to last checkpoint

### Timeout
- **Detection**: SIGALRM (Unix) / Not supported (Windows)
- **Error**: `TimeoutError`
- **Counted As**: Failure
- **Note**: Windows tests skip timeout detection

## Performance Characteristics

### Memory Overhead
- **Per Circuit**: ~1KB (state + metrics)
- **Per Checkpoint**: File size + overhead (~2KB)
- **Retention**: Limited by checkpoint_retention config

### Execution Overhead
- **State Check**: ~0.1ms
- **Checkpoint Creation**: ~10-50ms (depends on file count)
- **Rollback**: ~20-100ms (depends on file count)
- **Logging**: Async, minimal impact

### Scalability
- **Thread-Safe**: Yes (uses threading.Lock)
- **Concurrent Agents**: Unlimited (separate circuits)
- **Checkpoint Storage**: Linear with retention count

## Future Enhancements

1. **Distributed Circuit State**: Share state across processes/machines
2. **Advanced Fallback Strategies**: Priority-based, load-based selection
3. **Metrics Export**: Prometheus, Grafana integration
4. **Notification System**: Alerts on circuit opens
5. **Circuit Dashboard**: Real-time monitoring UI
6. **Predictive Circuit Opening**: ML-based failure prediction
7. **Partial Rollback**: Selective file restoration
8. **Incremental Checkpoints**: Delta-based storage

## References

- **Agent Definition**: `.claude/agents/fault-tolerance/circuit-breaker.md`
- **Implementation**: `src/digitalmodel/modules/automation/circuit_breaker.py`
- **Rollback Manager**: `src/digitalmodel/modules/automation/rollback_manager.py`
- **Tests**: `tests/domains/automation/test_circuit_breaker.py`
- **Tests**: `tests/domains/automation/test_rollback_manager.py`

## Version History

- **v1.0.0** (2026-01-06): Initial implementation
  - Three-state circuit breaker
  - Automatic fallback selection
  - Checkpoint & rollback manager
  - 47 comprehensive tests
  - Windows-compatible fixtures
  - Microsecond-precision checkpoint IDs
