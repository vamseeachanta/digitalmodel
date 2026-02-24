# Agent Registry System

## Overview

The Agent Registry is a consolidated, single source of truth for all AI agents available in the digitalmodel project. It combines agent definitions from multiple sources into a unified registry with performance tracking, availability metrics, and metadata.

## Files

### 1. `.claude/agents-registry.json`
**Single source of truth** for all agent metadata, capabilities, and performance metrics.

- **Total Agents**: 54+ agents across multiple categories
- **Update Frequency**: Daily (manual sync via script)
- **Format**: JSON with comprehensive schema

### 2. `scripts/consolidate_agent_registry.py`
Python script that syncs and consolidates agent definitions from:
- `.claude/agents/*/*.md` - Agent definition files
- `modules/config/ai-agents-registry.json` - Existing registry with performance data

**Key Features**:
- Auto-generates aliases from agent names
- Merges metadata from multiple sources
- Registry precedence for performance data
- Creates backups before overwriting
- Validates consistency

### 3. `scripts/validate_agent_registry.py`
Standalone validation script that checks:
- Required fields (name, description, capabilities, performance_score)
- Performance metrics structure
- Availability metrics structure
- Alias uniqueness
- Data type correctness
- Business rule compliance

## Registry Schema

### Meta Information
```json
{
  "meta": {
    "version": "2.0.0",
    "last_updated": "2026-01-07T06:03:21Z",
    "total_agents": 54,
    "sync_sources": [...],
    "auto_sync_enabled": true
  }
}
```

### Agent Entry
```json
{
  "agent-name": {
    "name": "agent-name",
    "description": "Agent description",
    "capabilities": ["capability1", "capability2"],
    "type": "specialist",
    "definition_file": ".claude/agents/core/agent-name.md",
    "platform": "claude-flow",
    "alias": "shortname",

    "performance": {
      "success_rate": 92.5,
      "avg_execution_time": 3.2,
      "total_tasks": 150,
      "successful_tasks": 139,
      "failed_tasks": 11,
      "cost_per_task": 0.05,
      "last_updated": "2026-01-07T06:00:00Z"
    },

    "availability": {
      "is_available": true,
      "current_load": 0.3,
      "max_concurrent": 10,
      "queue_length": 2,
      "uptime_percentage": 99.2,
      "last_health_check": "2026-01-07T06:00:00Z"
    },

    "performance_history": [
      {
        "timestamp": "2026-01-06T00:00:00Z",
        "success_rate": 91.0,
        "avg_execution_time": 3.5,
        "total_tasks": 20
      }
    ],

    "performance_score": 92,
    "best_for": ["task-type-1", "task-type-2"],
    "limitations": ["limitation-1"],
    "cost_tier": "low"
  }
}
```

### Agent Categories
```json
{
  "categories": {
    "developer": ["coder", "tester"],
    "orchestrator": ["planner"],
    "validator": ["reviewer"],
    "specialist": [...]
  }
}
```

### Aliases
```json
{
  "aliases": {
    "backend": "backend-dev",
    "analyzer": "code-analyzer",
    "coder": "coder"
  }
}
```

## Performance Metrics

### Tracked Metrics (Last 30 Days)
- **success_rate**: Percentage of successful task completions (0-100%)
- **avg_execution_time**: Average time to complete tasks (seconds)
- **total_tasks**: Total number of tasks assigned
- **successful_tasks**: Number of successfully completed tasks
- **failed_tasks**: Number of failed tasks
- **cost_per_task**: Average cost in USD per task

### Availability Metrics
- **is_available**: Agent spawnable AND response time < 5s AND error rate < 10%
- **current_load**: Current utilization (0.0-1.0)
- **max_concurrent**: Maximum concurrent tasks supported (default: 10)
- **queue_length**: Number of tasks waiting for execution
- **uptime_percentage**: Availability percentage over last 30 days

## Usage

### Daily Sync (Manual)
```bash
# Activate uv environment
. .venv/Scripts/activate

# Run consolidation
python scripts/consolidate_agent_registry.py

# With options
python scripts/consolidate_agent_registry.py \
  --agents-dir .claude/agents \
  --registry-file modules/config/ai-agents-registry.json \
  --output .claude/agents-registry.json

# Dry run (preview changes)
python scripts/consolidate_agent_registry.py --dry-run

# Skip backup
python scripts/consolidate_agent_registry.py --no-backup
```

### Validation
```bash
# Validate registry
python scripts/validate_agent_registry.py .claude/agents-registry.json

# Strict mode (warnings as errors)
python scripts/validate_agent_registry.py --strict .claude/agents-registry.json
```

### Querying Agents

**Python Example**:
```python
import json
from pathlib import Path

# Load registry
with open('.claude/agents-registry.json', 'r') as f:
    registry = json.load(f)

# Get agent by name
agent = registry['agents']['coder']
print(f"Agent: {agent['name']}")
print(f"Capabilities: {agent['capabilities']}")
print(f"Performance Score: {agent['performance_score']}")

# Get agent by alias
alias = 'backend'
agent_name = registry['aliases'][alias]
agent = registry['agents'][agent_name]

# Find agents by category
developers = registry['categories']['developer']
for agent_name in developers:
    agent = registry['agents'][agent_name]
    print(f"{agent_name}: {agent['performance']['success_rate']}% success")

# Find best agents for a task
for agent_name, agent_data in registry['agents'].items():
    if 'code-generation' in agent_data['capabilities']:
        score = agent_data['performance_score']
        print(f"{agent_name}: {score}")
```

**Bash Example**:
```bash
# Get all agent names
jq -r '.agents | keys[]' .claude/agents-registry.json

# Get agents by type
jq -r '.categories.developer[]' .claude/agents-registry.json

# Get agent performance score
jq -r '.agents.coder.performance_score' .claude/agents-registry.json

# Find high-performing agents (score > 90)
jq -r '.agents | to_entries[] | select(.value.performance_score > 90) | .key' .claude/agents-registry.json

# Get agent by alias
ALIAS="backend"
AGENT=$(jq -r ".aliases.\"$ALIAS\"" .claude/agents-registry.json)
jq ".agents.\"$AGENT\"" .claude/agents-registry.json
```

## Agent Definitions

Agent definitions are stored in `.claude/agents/` with YAML frontmatter:

```markdown
---
name: agent-name
type: specialist
description: Agent description
capabilities:
  - capability1
  - capability2
priority: high
hooks:
  pre: |
    echo "Pre-task hook"
  post: |
    echo "Post-task hook"
---

# Agent Documentation

Detailed agent instructions and guidelines...
```

## Conflict Resolution

When the same agent is defined in multiple sources, **precedence rules**:

1. **Performance data**: `ai-agents-registry.json` takes precedence (has historical data)
2. **Description & capabilities**: Markdown files take precedence (source of truth for agent behavior)
3. **Metadata**: Merged from both sources

## Alias Generation Rules

Aliases are auto-generated from agent names:

- Remove common suffixes: `-agent`, `-dev`, `-coordinator`, `-manager`, `-specialist`
- If no suffix, take last word after hyphen
- Examples:
  - `backend-dev` → `backend`
  - `code-analyzer` → `analyzer`
  - `coder` → `coder`

**Uniqueness**: All aliases must be unique (validation enforced).

## Validation Rules

### Required Fields (Strict)
- `name`: Agent name
- `description`: Human-readable description
- `capabilities`: Array of capabilities
- `performance_score`: 0-100 integer

### Performance Metrics (Strict)
- `success_rate`: 0-100 float
- `avg_execution_time`: ≥ 0 float (seconds)
- `total_tasks`: ≥ 0 integer
- `successful_tasks + failed_tasks = total_tasks`

### Availability Metrics (Strict)
- `is_available`: boolean
- `current_load`: 0.0-1.0 float
- `max_concurrent`: > 0 integer
- `uptime_percentage`: 0-100 float

### Warnings (Non-blocking)
- Missing definition_file
- Definition file not found on disk

## Integration with Claude Code

The registry is designed for file-based access by Claude Code's Task tool:

```python
# In Claude Code context
import json

# Load registry
with open('.claude/agents-registry.json', 'r') as f:
    registry = json.load(f)

# Select agent for task
task_type = 'code-generation'
candidates = [
    (name, data['performance_score'])
    for name, data in registry['agents'].items()
    if task_type in data['capabilities']
]

# Sort by performance score
best_agent = sorted(candidates, key=lambda x: x[1], reverse=True)[0][0]

# Spawn agent via Task tool
Task(best_agent, "Implement feature X", "specialist")
```

## Performance History Tracking

Performance history is stored as a rolling 30-day window:

```json
{
  "performance_history": [
    {
      "timestamp": "2026-01-07T00:00:00Z",
      "success_rate": 92.5,
      "avg_execution_time": 3.2,
      "total_tasks": 20,
      "successful_tasks": 19,
      "failed_tasks": 1
    }
  ]
}
```

**Future Enhancement**: Scripts can append daily snapshots to track trends.

## Backup Strategy

- **Automatic backup**: Created before each sync (`.json.backup`)
- **Manual backup**: Use `--no-backup` flag to skip
- **Restore**: Copy `.json.backup` to `.json`

```bash
# Restore from backup
cp .claude/agents-registry.json.backup .claude/agents-registry.json
```

## Troubleshooting

### Issue: Unicode errors on Windows
**Solution**: Scripts now handle UTF-8 encoding automatically.

### Issue: Agent not found after sync
**Causes**:
1. No YAML frontmatter in markdown file
2. File path issues (not relative to project root)
3. Agent only in registry, not in markdown

**Solution**:
```bash
# Check agent markdown file has frontmatter
head -20 .claude/agents/core/agent-name.md

# Run with verbose output
python scripts/consolidate_agent_registry.py 2>&1 | grep "agent-name"
```

### Issue: Validation failures
**Common causes**:
1. Missing required fields
2. Invalid data types (string instead of number)
3. Duplicate aliases
4. Task count mismatch

**Solution**:
```bash
# Run strict validation
python scripts/validate_agent_registry.py --strict .claude/agents-registry.json

# Fix errors in registry manually or in source files
```

## Contributing

### Adding a New Agent

1. **Create markdown file**:
```bash
# Create agent file
cat > .claude/agents/core/new-agent.md <<'EOF'
---
name: new-agent
type: specialist
description: New agent description
capabilities:
  - capability1
  - capability2
priority: medium
---

# New Agent

Agent documentation...
EOF
```

2. **Run consolidation**:
```bash
python scripts/consolidate_agent_registry.py
```

3. **Validate**:
```bash
python scripts/validate_agent_registry.py
```

### Updating Agent Metadata

**Option 1**: Edit markdown source
```bash
# Edit agent definition
vim .claude/agents/core/agent-name.md

# Re-sync
python scripts/consolidate_agent_registry.py
```

**Option 2**: Edit registry directly (performance data)
```bash
# Edit registry
vim .claude/agents-registry.json

# Validate
python scripts/validate_agent_registry.py
```

## Future Enhancements

Potential improvements for the registry system:

1. **Automated daily sync**: Cron job or GitHub Actions
2. **Performance tracking**: Actual task execution logging
3. **Health checks**: Automated availability testing
4. **Cost tracking**: Integration with billing APIs
5. **Trend analysis**: Performance degradation detection
6. **Agent recommendations**: ML-based agent selection
7. **Load balancing**: Distribute tasks based on `current_load`
8. **A/B testing**: Compare agent performance on same tasks

## Related Documentation

- `.claude/agents/README.md` - Agent definitions overview
- `modules/config/ai-agents-registry.json` - Legacy registry
- `docs/modules/ai/AI_AGENT_GUIDELINES.md` - Agent usage guidelines
- `CLAUDE.md` - Project configuration

## Questions?

For issues or questions about the agent registry system:

1. Check validation output for specific errors
2. Review this README for usage patterns
3. Examine sample agents in `.claude/agents/core/`
4. Run scripts with `--help` flag for options

---

**Last Updated**: 2026-01-07
**Registry Version**: 2.0.0
**Total Agents**: 54+
