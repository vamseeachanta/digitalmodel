# Enhanced Workflow Templates System

## Overview

The Enhanced Workflow Templates system provides a powerful, reusable framework for orchestrating complex development workflows with:

- **Phase Dependencies**: Execute phases sequentially or in parallel with dependency management
- **Quality Gates**: Automatic enforcement of code coverage, linting, and type checking
- **Agent Orchestration**: Multi-agent coordination using Claude Code Task tool and MCP
- **Automatic Checkpointing**: Resume workflows from any checkpoint after interruption
- **Error Recovery**: Automatic retry and agent replacement on failures
- **Interactive Reports**: HTML reports with Plotly visualizations

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Workflow Executor                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌──────────────┐   ┌──────────────┐   ┌──────────────┐   │
│  │   Phase 1    │──▶│   Phase 2    │──▶│   Phase 3    │   │
│  │ (Sequential) │   │  (Parallel)  │   │ (Sequential) │   │
│  └──────────────┘   └──────────────┘   └──────────────┘   │
│         │                   │                   │          │
│    ┌────▼────┐         ┌────▼────┐         ┌────▼────┐   │
│    │ Agent 1 │         │ Agent 2 │         │ Agent 5 │   │
│    └─────────┘         ├─────────┤         └─────────┘   │
│                        │ Agent 3 │                        │
│                        ├─────────┤                        │
│                        │ Agent 4 │                        │
│                        └─────────┘                        │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Quality Gates Enforcer                  │  │
│  │  ✓ Code Coverage > 80%                              │  │
│  │  ✓ Linting Pass                                     │  │
│  │  ✓ Type Checking Pass                               │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Checkpoint Manager                      │  │
│  │  • Auto-save after each phase                        │  │
│  │  • Resume from any checkpoint                        │  │
│  │  • Full state recovery                               │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Key Features

### 1. Phase Dependency Management

Workflows support complex dependency graphs:

```yaml
phases:
  - id: "foundation"
    dependencies: []           # No dependencies

  - id: "backend"
    dependencies: ["foundation"]  # Waits for foundation

  - id: "frontend"
    dependencies: ["foundation"]  # Also waits for foundation

  - id: "integration"
    dependencies: ["backend", "frontend"]  # Waits for both
```

### 2. Parallel Execution

Execute multiple agents concurrently:

```yaml
- id: "testing"
  parallel: true              # Agents run in parallel
  agents:
    - type: "tester"
      role: "unit_tester"
    - type: "tester"
      role: "integration_tester"
    - type: "tester"
      role: "e2e_tester"
```

### 3. Quality Gates

Enforce quality standards automatically:

```yaml
quality_gates:
  code_coverage:
    enabled: true
    threshold: 85              # Must be ≥85%
    halt_on_failure: true      # Stop workflow on failure
  linting:
    enabled: true
    halt_on_failure: true
  type_checking:
    enabled: true
    halt_on_failure: true
```

### 4. Agent Retry & Replacement

Automatic error recovery:

1. Agent fails → Retry same agent once
2. Still fails → Spawn replacement agent
3. Replacement fails → Fail phase after 2 total failures

### 5. Checkpointing

Automatic state persistence:

- Checkpoint saved after each phase
- Full execution state captured
- Resume from any checkpoint
- `checkpoint_latest.json` always points to most recent

## File Structure

```
digitalmodel/
├── config/
│   └── workflow-templates-enhanced.yaml    # Template definitions
├── src/digitalmodel/
│   ├── modules/automation/
│   │   └── workflow_executor.py            # Executor implementation
│   └── cli/
│       └── workflow_cli.py                 # CLI interface
├── scripts/
│   └── workflow-cli.sh                     # Wrapper script
├── data/workflows/checkpoints/             # Checkpoint storage
│   └── <workflow-id>/
│       ├── checkpoint_phase_*_start.json
│       ├── checkpoint_phase_*_complete.json
│       └── checkpoint_latest.json
├── reports/
│   └── workflow_report.html                # HTML reports
└── docs/
    ├── WORKFLOW_TEMPLATES_README.md        # This file
    └── examples/workflow_examples.md       # Usage examples
```

## Quick Start

### Installation

The workflow system is built into digitalmodel. Ensure you have the uv environment:

```bash
cd /d/workspace-hub/digitalmodel
uv sync
```

### List Available Templates

```bash
./scripts/workflow-cli.sh list
```

### Execute a Workflow

```bash
# Full-stack development
./scripts/workflow-cli.sh execute full_stack_development

# Bug fix
./scripts/workflow-cli.sh execute bug_fix_workflow

# Custom workflow ID
./scripts/workflow-cli.sh execute full_stack_development \
  --workflow-id my_feature_v1
```

### Monitor Progress

```bash
# Check status
./scripts/workflow-cli.sh status <workflow-id>

# JSON output
./scripts/workflow-cli.sh status <workflow-id> --json-output
```

### Resume from Checkpoint

```bash
./scripts/workflow-cli.sh execute full_stack_development \
  --checkpoint-id data/workflows/checkpoints/<workflow-id>/checkpoint_latest.json
```

## Available Templates

### 1. Full-Stack Development (`full_stack_development`)

**Use Case**: Complete feature development from specification to deployment

**Phases**: 7 phases, ~45 minutes
- Specification & Planning
- Architecture Design
- Backend Development (parallel)
- Frontend Development (parallel)
- Comprehensive Testing (parallel)
- Code Review & QA
- Integration & Deployment Prep

**Quality Gates**: Coverage >85%, linting, type checking

### 2. Bug Fix Workflow (`bug_fix_workflow`)

**Use Case**: Systematic bug investigation and fixing

**Phases**: 4 phases, ~20 minutes
- Investigation & Root Cause Analysis
- Fix Implementation
- Test & Verification (parallel)
- Review & Approval

**Quality Gates**: Coverage >80%, linting, type checking

### 3. Refactoring Workflow (`refactoring_workflow`)

**Use Case**: Code quality improvement and technical debt reduction

**Phases**: 4 phases, ~30 minutes
- Code Analysis & Planning
- Baseline Test Coverage
- Refactoring Implementation
- Verification & QA (parallel)

**Quality Gates**: Coverage ≥80% (maintain or improve), linting, type checking

### 4. Research Workflow (`research_workflow`)

**Use Case**: Technology research and knowledge discovery

**Phases**: 3 phases, ~25 minutes
- Topic Definition
- Information Gathering (parallel)
- Synthesis & Recommendations

**Quality Gates**: None (research-focused)

### 5. Performance Optimization (`performance_optimization`)

**Use Case**: Performance analysis and optimization

**Phases**: 4 phases, ~40 minutes
- Baseline Profiling
- Bottleneck Analysis
- Optimization Implementation (parallel)
- Verification & Benchmarking

**Quality Gates**: Coverage ≥80% (maintain or improve), linting, type checking

### 6. Multi-Repo Sync (`multi_repo_sync`)

**Use Case**: Synchronize all repositories in workspace-hub

**Phases**: 4 phases, ~15 minutes
- Repository Discovery
- Git Pull/Push Synchronization (parallel)
- Dependency Updates (parallel)
- Synchronization Verification

**Quality Gates**: None (operational)

## CLI Reference

### Commands

```bash
# List templates
workflow-cli.sh list [--config <path>]

# Execute workflow
workflow-cli.sh execute <template> [options]
  --workflow-id <id>        Custom workflow ID
  --config <path>           Config file path
  --background              Run in background
  --json-output             JSON output
  --checkpoint-id <path>    Resume from checkpoint
  --report-path <path>      HTML report output

# Check status
workflow-cli.sh status <workflow-id> [options]
  --config <path>           Config file path
  --json-output             JSON output

# List checkpoints
workflow-cli.sh checkpoints [--config <path>]
```

### Examples

```bash
# Development workflow
./scripts/workflow-cli.sh execute full_stack_development \
  --workflow-id auth_feature \
  --report-path reports/auth_development.html

# Background bug fix
./scripts/workflow-cli.sh execute bug_fix_workflow \
  --background \
  --json-output

# Resume refactoring
./scripts/workflow-cli.sh execute refactoring_workflow \
  --checkpoint-id data/workflows/checkpoints/refactor_auth/checkpoint_latest.json

# Monitor status
./scripts/workflow-cli.sh status auth_feature --json-output | jq '.status'
```

## Creating Custom Templates

### Template Structure

```yaml
templates:
  my_custom_workflow:
    name: "My Custom Workflow"
    description: "Brief description"

    coordination:
      topology: "hierarchical"    # mesh, hierarchical, ring, star
      use_mcp: true              # Use MCP for coordination
      use_claude_code_tasks: true # Use Task tool for execution

    quality_gates:
      code_coverage:
        threshold: 80
      linting: true
      type_checking: true

    phases:
      - id: "phase1"
        name: "Phase 1 Name"
        dependencies: []          # Phase IDs this depends on
        parallel: false           # Execute agents in parallel?

        agents:
          - type: "coder"         # Agent type
            role: "developer"     # Role name
            task: "Task description"
            coordination: "hooks" # Coordination method

        outputs:
          - "path/to/output"      # Expected outputs

        checkpoints:
          - "checkpoint_name"     # Checkpoint names

        quality_gates:            # Override template gates
          code_coverage:
            threshold: 85
```

### Adding to Configuration

1. Edit `config/workflow-templates-enhanced.yaml`
2. Add template under `templates:` section
3. Add metadata under `template_metadata:` section
4. Test with `workflow-cli.sh list`

## Integration

### With SPARC Methodology

```bash
# Use SPARC for specification
npx claude-flow sparc run specification "Feature description"

# Execute workflow
./scripts/workflow-cli.sh execute full_stack_development
```

### With CI/CD

```yaml
# .github/workflows/development.yml
- name: Execute Development Workflow
  run: |
    ./scripts/workflow-cli.sh execute full_stack_development \
      --workflow-id ${{ github.run_id }} \
      --json-output > workflow_result.json

- name: Check Workflow Status
  run: |
    if [ $(jq -r '.success' workflow_result.json) == "true" ]; then
      echo "Workflow succeeded"
    else
      echo "Workflow failed"
      exit 1
    fi
```

### With Testing Framework

Quality gates automatically integrate:

```bash
# Coverage measured by pytest-cov
uv run pytest --cov=src --cov-report=json

# Linting by ruff
uv run ruff check .

# Type checking by mypy
uv run mypy src
```

## HTML Reports

Reports include:

- **Execution Metrics**: Status, duration, completion rates
- **Interactive Timeline**: Plotly-based phase execution visualization
- **Phase Details**: Agent status, retries, errors
- **Quality Gates**: Pass/fail status for all gates

Reports follow HTML Reporting Standards:
- Interactive Plotly visualizations
- Professional styling
- Responsive design
- Relative data paths

## Performance

### Limits

- **Max Phases**: 10 per workflow
- **Max Agents per Phase**: 10
- **Max Concurrent Workflows**: 3
- **Max Duration**: 60 minutes

### Optimization

- Use parallel execution where possible
- Break large tasks into smaller phases
- Cache intermediate results
- Use background mode for long workflows

## Troubleshooting

### Workflow Fails at Quality Gate

```bash
# Check failure details
./scripts/workflow-cli.sh status <workflow-id> --json-output

# Fix issues
# - Increase test coverage
# - Fix linting errors
# - Resolve type errors

# Resume from checkpoint
./scripts/workflow-cli.sh execute <template> \
  --checkpoint-id <checkpoint-path>
```

### Agent Failures

Workflows automatically handle agent failures:
1. Retry same agent (1 time)
2. Spawn replacement agent
3. Fail after 2 total failures per phase

### Checkpoint Issues

```bash
# List all checkpoints
./scripts/workflow-cli.sh checkpoints

# Load specific checkpoint
./scripts/workflow-cli.sh execute <template> \
  --checkpoint-id <full-path-to-checkpoint>

# Verify checkpoint integrity
python -c "import json; print(json.load(open('<checkpoint>'))['status'])"
```

## Best Practices

1. **Semantic Naming**: Use descriptive workflow IDs
2. **Background Mode**: For long-running workflows
3. **Review Reports**: Check HTML reports for insights
4. **Quality First**: Don't bypass quality gates
5. **Checkpoint Cleanup**: Remove old checkpoints periodically
6. **Template Reuse**: Create templates for common patterns
7. **Phase Granularity**: Keep phases focused and modular
8. **Parallel Where Possible**: Use parallel execution for independent tasks

## Advanced Usage

### Custom Agent Types

Define custom agents in templates:

```yaml
agents:
  - type: "custom-agent"
    role: "specialized_developer"
    task: "Specialized task description"
    coordination: "hooks"
```

### Dynamic Workflows

Generate workflows programmatically:

```python
from digitalmodel.workflows.automation.workflow_executor import WorkflowExecutor

executor = WorkflowExecutor('config/workflow-templates-enhanced.yaml')
execution = executor.create_workflow_execution('template_name')

# Modify execution
execution.phases.append(custom_phase)

# Execute
executor.execute_workflow(execution)
```

### Metrics Collection

Extract metrics from checkpoints:

```python
import json

with open('checkpoint_latest.json') as f:
    data = json.load(f)

metrics = {
    'total_duration': data['end_time'] - data['start_time'],
    'phase_count': len(data['phases']),
    'agent_count': sum(len(p['agents']) for p in data['phases']),
    'success_rate': sum(1 for p in data['phases'] if p['status'] == 'completed') / len(data['phases'])
}
```

## Support

- **Documentation**: See `docs/examples/workflow_examples.md`
- **Issues**: Report to project maintainers
- **Examples**: Check `docs/examples/` directory

## Future Enhancements

- Conditional phase execution
- Performance benchmarking gates
- Custom quality gate definitions
- Workflow templates marketplace
- Real-time progress dashboard
- Workflow analytics and insights
- Multi-workflow coordination
- Rollback and skip phase capabilities

---

**Version**: 1.0.0
**Last Updated**: 2026-01-07
**Compatibility**: digitalmodel uv environment
