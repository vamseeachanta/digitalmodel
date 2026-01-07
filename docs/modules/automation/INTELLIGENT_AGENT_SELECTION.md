# Intelligent Agent Selection System

## Overview

The Intelligent Agent Selection System provides multi-factor scoring to automatically select the optimal agent for any task based on:

- **Historical success rate** (70% weight): Past performance over last 30 days
- **Current availability** (15% weight): Load balancing across agents
- **Cost-benefit ratio** (10% weight): Success rate divided by execution time
- **Domain expertise** (5% weight): Keyword matching with agent capabilities

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                  Agent Orchestrator (Bash)                  │
│  • Task routing                                             │
│  • User interface                                           │
│  • Platform integration                                     │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│        IntelligentAgentSelector (Python)                    │
│  • Multi-factor scoring                                     │
│  • Agent comparison                                         │
│  • Selection reasoning                                      │
└────────────────────┬────────────────────────────────────────┘
                     │
        ┌────────────┼────────────┐
        ▼            ▼            ▼
┌──────────────┐ ┌─────────┐ ┌──────────────┐
│ Performance  │ │ Weights │ │    Agent     │
│   Database   │ │ Config  │ │  Registry    │
│  (SQLite)    │ │ (YAML)  │ │   (JSON)     │
└──────────────┘ └─────────┘ └──────────────┘
```

## Components

### 1. `intelligent_agent_selector.py`

Core Python module providing:

- `AgentPerformanceTracker`: SQLite-based performance history
- `IntelligentAgentSelector`: Multi-factor selection algorithm

### 2. `agent_orchestrator.sh` (Enhanced)

Bash wrapper that:

- Calls Python selector for intelligent selection
- Falls back to registry-based selection if needed
- Integrates with uv environment
- Generates execution plans for different platforms

### 3. `agent-selection-weights.yaml`

Configuration file for:

- Scoring weights (customizable per deployment)
- Availability constraints
- History tracking parameters
- Domain matching settings

### 4. Agent Performance Database

SQLite database schema:

```sql
CREATE TABLE agent_tasks (
    id INTEGER PRIMARY KEY,
    agent_type TEXT NOT NULL,
    task_type TEXT NOT NULL,
    started_at TIMESTAMP NOT NULL,
    completed_at TIMESTAMP,
    success BOOLEAN NOT NULL,
    execution_time REAL,
    error_count INTEGER DEFAULT 0,
    quality_score REAL DEFAULT 1.0
);
```

## Usage

### Command Line (via Orchestrator)

```bash
# Basic usage with intelligent selection
./modules/automation/agent_orchestrator.sh code-generation \
    "Create Python API with authentication"

# Show detailed reasoning
./modules/automation/agent_orchestrator.sh code-generation \
    "Create Python API" --show-reasoning

# Specify current agent loads for load balancing
./modules/automation/agent_orchestrator.sh code-generation \
    "Create Python API" --agent-loads '{"agent-a": 3, "agent-b": 1}'

# Force specific agent (skip intelligent selection)
./modules/automation/agent_orchestrator.sh code-generation \
    "Create Python API" --agent claude-sonnet-4.5
```

### Direct Python Usage

```python
from digitalmodel.modules.automation import IntelligentAgentSelector

# Initialize selector
selector = IntelligentAgentSelector(
    config_path="config/agent-selection-weights.yaml",
    registry_path="modules/config/ai-agents-registry.json",
    performance_db=".claude-flow/agent-performance.db"
)

# Select agent
result = selector.select_agent(
    task_type="code-generation",
    task_description="Create REST API with authentication",
    agent_loads={"agent-a": 2},  # Optional
    max_concurrent=5
)

print(f"Selected: {result['agent']}")
print(f"Confidence: {result['confidence']}")
print(f"Reasoning: {result['reasoning']}")
```

### Recording Performance

```python
from digitalmodel.modules.automation import AgentPerformanceTracker

tracker = AgentPerformanceTracker()

# Record successful task
tracker.record_task(
    agent_type="agent-a",
    task_type="code-generation",
    success=True,
    execution_time=45.5,
    error_count=0,
    quality_score=0.95
)

# Query success rate
success_rate = tracker.get_success_rate("agent-a", days=30)
print(f"Success rate: {success_rate:.1%}")
```

## Selection Algorithm

### Scoring Formula

```
total_score =
    0.70 × historical_success +
    0.15 × availability +
    0.10 × cost_benefit +
    0.05 × domain_expertise
```

### Component Calculations

#### 1. Historical Success (70%)

```python
success_rate = success_count / total_tasks  # Last 30 days
# Default: 0.5 for new agents with no history
```

#### 2. Availability (15%)

```python
availability = 1 - (current_load / max_concurrent)
# Returns -1 if unavailable (hard constraint)
```

#### 3. Cost-Benefit (10%)

```python
benefit = success_rate / avg_execution_time
cost_benefit = benefit / cost_per_task
# Normalized to 0-1 range
```

#### 4. Domain Expertise (5%)

```python
keyword_matches = count_overlaps(task_keywords, agent_capabilities)
domain_score = keyword_matches / len(task_keywords)
```

### Selection Process

1. **Get candidates** from registry for task type
2. **Score each agent** using multi-factor algorithm
3. **Exclude unavailable** agents (load >= max_concurrent)
4. **Sort by total score** descending
5. **Return best match** with reasoning and alternatives

## Configuration

### Adjusting Weights

Edit `config/agent-selection-weights.yaml`:

```yaml
weights:
  historical_success: 0.70  # Increase for stability
  availability: 0.15        # Increase for load balancing
  cost_benefit: 0.10        # Increase for efficiency
  domain_expertise: 0.05    # Increase for specialization
```

**Note**: Weights must sum to 1.0

### Availability Settings

```yaml
availability:
  max_concurrent: 5         # Tasks per agent
  exclude_unavailable: true # Hard constraint
```

### History Tracking

```yaml
history:
  lookback_days: 30          # Performance window
  default_success_rate: 0.5  # For new agents
  min_tasks_threshold: 5     # Reliability threshold
```

## Output Format

### Selection Result (JSON)

```json
{
  "agent": "agent-a",
  "confidence": 0.823,
  "reasoning": "Selected agent-a for code-generation based on: high success rate (90.0%), immediately available",
  "scores": {
    "historical_success": 0.900,
    "availability": 1.000,
    "cost_benefit": 0.750,
    "domain_expertise": 0.667,
    "total": 0.823
  },
  "alternatives": [
    {"agent": "agent-b", "confidence": 0.645},
    {"agent": "agent-c", "confidence": 0.512}
  ]
}
```

## Performance Tracking

### Database Location

- **Production**: `.claude-flow/agent-performance.db`
- **Demo**: `.claude-flow/agent-performance-demo.db`

### Viewing History

```bash
# Query database directly
sqlite3 .claude-flow/agent-performance.db "
  SELECT agent_type,
         COUNT(*) as total,
         SUM(success) as successes,
         ROUND(AVG(execution_time), 2) as avg_time
  FROM agent_tasks
  WHERE started_at >= datetime('now', '-30 days')
  GROUP BY agent_type;
"
```

### Maintenance

```bash
# Clean old records (older than 90 days)
sqlite3 .claude-flow/agent-performance.db "
  DELETE FROM agent_tasks
  WHERE started_at < datetime('now', '-90 days');
"

# Vacuum database
sqlite3 .claude-flow/agent-performance.db "VACUUM;"
```

## Integration Points

### With Agent Registry

The selector reads from `modules/config/ai-agents-registry.json`:

```json
{
  "agents": {
    "agent-a": {
      "platform": "claude-code",
      "type": "coder",
      "capabilities": ["python", "testing", "refactoring"]
    }
  },
  "taskTypeAgentMapping": {
    "code-generation": {
      "primary": "agent-a",
      "alternatives": ["agent-b"]
    }
  }
}
```

### With Orchestrator

The bash orchestrator:

1. Activates uv environment
2. Calls Python selector with JSON output
3. Parses result using `jq`
4. Generates platform-specific execution plan
5. Displays commands to user

### With Performance Reporting

Future integration:

- Dashboard showing agent performance trends
- Alerts for degraded performance
- Recommendations for weight tuning
- Load balancing insights

## Testing

### Run Unit Tests

```bash
# All tests
uv run python -m pytest tests/modules/automation/test_agent_selector.py -v

# Specific test class
uv run python -m pytest tests/modules/automation/test_agent_selector.py::TestIntelligentAgentSelector -v

# With coverage
uv run python -m pytest tests/modules/automation/test_agent_selector.py --cov=src/digitalmodel/modules/automation
```

### Run Demo

```bash
uv run python examples/agent_selection_demo.py
```

## Troubleshooting

### Issue: "No agents available in registry"

**Solution**: Ensure registry file exists and contains agents:

```bash
ls -la modules/config/ai-agents-registry.json
```

### Issue: "All agents are currently unavailable"

**Solution**: Check agent loads or increase `max_concurrent`:

```bash
# Reduce loads or adjust config
vim config/agent-selection-weights.yaml
```

### Issue: Low confidence scores

**Solution**: Accumulate more performance history or adjust weights:

```python
# Record more tasks for better historical data
tracker.record_task("agent-a", "task-type", success=True)
```

### Issue: Database locked

**Solution**: Close any open connections:

```bash
# List processes using database
lsof .claude-flow/agent-performance.db

# Or recreate database (lose history)
rm .claude-flow/agent-performance.db
```

## Future Enhancements

### Planned Features

1. **Machine Learning**: Train models on historical patterns
2. **Multi-objective Optimization**: Pareto optimal selection
3. **Dynamic Weights**: Auto-adjust based on outcomes
4. **Agent Specialization**: Track task-type specific performance
5. **Collaborative Filtering**: Learn from similar tasks
6. **Real-time Metrics**: Live performance monitoring
7. **A/B Testing**: Compare selection strategies

### Configuration Extensions

```yaml
# Future: ML-based selection
ml_selection:
  enabled: false
  model_path: "models/agent-selector.pkl"
  retrain_frequency: "weekly"

# Future: Agent-specific costs
cost:
  agent_costs:
    claude-opus-4.5: 2.0
    claude-sonnet-4.5: 1.0
    claude-haiku-4.0: 0.5
```

## References

- **Agent Registry**: `modules/config/ai-agents-registry.json`
- **Weights Config**: `config/agent-selection-weights.yaml`
- **Orchestrator**: `modules/automation/agent_orchestrator.sh`
- **Tests**: `tests/modules/automation/test_agent_selector.py`
- **Demo**: `examples/agent_selection_demo.py`

## Support

For issues or questions:

1. Check this documentation
2. Review test cases for examples
3. Run demo to understand behavior
4. Check database for historical data
5. Verify registry and config files

---

**Version**: 1.0.0
**Last Updated**: 2026-01-06
**Status**: Production Ready
