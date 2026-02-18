> **DEPRECATED (2026-02-18)**: This document describes the agent-os / claude-flow framework which has been fully removed from the ecosystem. Content is preserved for historical reference only. Do not follow these instructions.

---

# Intelligent Agent Selection - Quick Start Guide

## 5-Minute Setup

### 1. Prerequisites

```bash
# Ensure uv environment is activated
source .venv/bin/activate  # Linux/Mac
# OR
.venv\Scripts\activate     # Windows

# Verify Python dependencies
pip list | grep -E "(yaml|sqlite)"
```

### 2. Quick Test

```bash
# Run the demo to verify installation
python examples/agent_selection_demo.py

# Should see: "DEMO COMPLETE" with example selections
```

### 3. First Selection

```bash
# Basic usage
./modules/automation/agent_orchestrator.sh code-generation \
    "Create Python function to calculate factorial"

# With detailed output
./modules/automation/agent_orchestrator.sh code-generation \
    "Create Python function" --show-reasoning --verbose
```

## Common Use Cases

### Use Case 1: Code Generation Task

```bash
./modules/automation/agent_orchestrator.sh code-generation \
    "Create REST API endpoint for user authentication" \
    --with-review
```

**Output:**
- Selected agent based on past performance
- Confidence score (0-1)
- Reasoning explanation
- Execution command to run

### Use Case 2: Load Balancing

```bash
# When you know some agents are busy
./modules/automation/agent_orchestrator.sh code-refactoring \
    "Optimize database queries in UserService" \
    --agent-loads '{"agent-a": 4, "agent-b": 1}'
```

**Effect:**
- System avoids overloaded agents
- Selects from available alternatives
- Balances work distribution

### Use Case 3: Force Specific Agent

```bash
# Skip intelligent selection
./modules/automation/agent_orchestrator.sh test-creation \
    "Create unit tests for authentication module" \
    --agent claude-sonnet-4.5
```

**Effect:**
- Bypasses multi-factor scoring
- Uses specified agent directly
- Useful for testing/debugging

## Python API Quick Reference

### Select Agent

```python
from digitalmodel.workflows.automation import IntelligentAgentSelector

selector = IntelligentAgentSelector()
result = selector.select_agent(
    task_type="code-generation",
    task_description="Create user authentication API"
)

print(f"Use: {result['agent']} (confidence: {result['confidence']:.1%})")
```

### Record Performance

```python
from digitalmodel.workflows.automation import AgentPerformanceTracker

tracker = AgentPerformanceTracker()
tracker.record_task(
    agent_type="agent-a",
    task_type="code-generation",
    success=True,
    execution_time=45.5
)
```

### Query Statistics

```python
tracker = AgentPerformanceTracker()
success_rate = tracker.get_success_rate("agent-a", days=30)
avg_time = tracker.get_avg_execution_time("agent-a", days=30)

print(f"Success: {success_rate:.1%}, Avg Time: {avg_time:.1f}s")
```

## Configuration Quick Tweaks

### Prioritize Accuracy Over Speed

Edit `config/agent-selection-weights.yaml`:

```yaml
weights:
  historical_success: 0.80  # Increased from 0.70
  availability: 0.10        # Decreased from 0.15
  cost_benefit: 0.05        # Decreased from 0.10
  domain_expertise: 0.05    # Same
```

### Prioritize Speed Over Accuracy

```yaml
weights:
  historical_success: 0.60  # Decreased
  availability: 0.15        # Same
  cost_benefit: 0.20        # Increased from 0.10
  domain_expertise: 0.05    # Same
```

### Prioritize Load Balancing

```yaml
weights:
  historical_success: 0.60  # Decreased
  availability: 0.30        # Increased from 0.15
  cost_benefit: 0.05        # Decreased
  domain_expertise: 0.05    # Same
```

## Monitoring Performance

### View Recent Performance

```bash
sqlite3 .claude-flow/agent-performance.db "
  SELECT
    agent_type,
    COUNT(*) as total_tasks,
    ROUND(AVG(success) * 100, 1) as success_pct,
    ROUND(AVG(execution_time), 1) as avg_seconds
  FROM agent_tasks
  WHERE started_at >= datetime('now', '-7 days')
  GROUP BY agent_type
  ORDER BY success_pct DESC;
"
```

### Check Today's Activity

```bash
sqlite3 .claude-flow/agent-performance.db "
  SELECT agent_type, task_type, success, execution_time
  FROM agent_tasks
  WHERE DATE(started_at) = DATE('now')
  ORDER BY started_at DESC;
"
```

## Troubleshooting

### Problem: "No agents available"

**Quick Fix:**
```bash
# Check registry exists
ls -la modules/config/ai-agents-registry.json

# View registry contents
jq '.agents | keys' modules/config/ai-agents-registry.json
```

### Problem: Low confidence scores

**Quick Fix:**
```bash
# Run tasks to build history
# After each task, record result:
python -c "
from digitalmodel.workflows.automation import AgentPerformanceTracker
tracker = AgentPerformanceTracker()
tracker.record_task('agent-a', 'code-gen', True, 45.0)
"
```

### Problem: All agents unavailable

**Quick Fix:**
```yaml
# Edit config/agent-selection-weights.yaml
availability:
  max_concurrent: 10  # Increase from 5
```

## Next Steps

1. **Read Full Documentation**: `docs/modules/automation/INTELLIGENT_AGENT_SELECTION.md`
2. **Explore Tests**: `tests/modules/automation/test_agent_selector.py`
3. **Customize Weights**: `config/agent-selection-weights.yaml`
4. **View Implementation**: `src/digitalmodel/modules/automation/intelligent_agent_selector.py`

## Command Cheat Sheet

```bash
# Basic selection
./modules/automation/agent_orchestrator.sh <task-type> "<description>"

# With reasoning
./modules/automation/agent_orchestrator.sh <task-type> "<desc>" --show-reasoning

# With load balancing
./modules/automation/agent_orchestrator.sh <task-type> "<desc>" --agent-loads '{...}'

# Force agent
./modules/automation/agent_orchestrator.sh <task-type> "<desc>" --agent <name>

# With review
./modules/automation/agent_orchestrator.sh <task-type> "<desc>" --with-review

# Save report
./modules/automation/agent_orchestrator.sh <task-type> "<desc>" --output report.md

# Verbose output
./modules/automation/agent_orchestrator.sh <task-type> "<desc>" --verbose
```

## Task Types

- `code-generation` - Generate new code
- `code-refactoring` - Refactor existing code
- `test-creation` - Create tests
- `code-review` - Review code quality
- `architecture-design` - Design system architecture
- `spec-creation` - Create specifications
- `bug-fixing` - Fix bugs
- `documentation` - Create documentation
- `performance-opt` - Optimize performance
- `security-audit` - Security review

---

**Ready to start?** Run your first selection:

```bash
./modules/automation/agent_orchestrator.sh code-generation \
    "Create a simple hello world function" --show-reasoning
```
