# Workflow Templates - Quick Start Guide

## 30-Second Start

```bash
# 1. List available workflows
./scripts/workflow-cli.sh list

# 2. Execute a workflow
./scripts/workflow-cli.sh execute full_stack_development

# 3. Check status
./scripts/workflow-cli.sh status <workflow-id>
```

## Common Commands

### List Templates

```bash
./scripts/workflow-cli.sh list
```

Shows all 6 available workflow templates with descriptions.

### Execute Workflows

```bash
# Full-stack development (45 min, 7 phases)
./scripts/workflow-cli.sh execute full_stack_development

# Bug fix (20 min, 4 phases)
./scripts/workflow-cli.sh execute bug_fix_workflow

# Refactoring (30 min, 4 phases)
./scripts/workflow-cli.sh execute refactoring_workflow

# Research (25 min, 3 phases)
./scripts/workflow-cli.sh execute research_workflow

# Performance optimization (40 min, 4 phases)
./scripts/workflow-cli.sh execute performance_optimization

# Multi-repo sync (15 min, 4 phases)
./scripts/workflow-cli.sh execute multi_repo_sync
```

### Advanced Options

```bash
# Custom workflow ID
./scripts/workflow-cli.sh execute full_stack_development \
  --workflow-id my_feature_v1

# Background mode
./scripts/workflow-cli.sh execute bug_fix_workflow \
  --background

# JSON output
./scripts/workflow-cli.sh execute refactoring_workflow \
  --json-output

# Custom report location
./scripts/workflow-cli.sh execute full_stack_development \
  --report-path reports/my_feature_report.html
```

### Check Status

```bash
# Text output
./scripts/workflow-cli.sh status <workflow-id>

# JSON output (for automation)
./scripts/workflow-cli.sh status <workflow-id> --json-output
```

### Resume from Checkpoint

```bash
# List all checkpoints
./scripts/workflow-cli.sh checkpoints

# Resume from latest checkpoint
./scripts/workflow-cli.sh execute full_stack_development \
  --checkpoint-id data/workflows/checkpoints/<workflow-id>/checkpoint_latest.json
```

## What Each Workflow Does

### Full-Stack Development

**Best for**: New feature development, complete applications

**Phases**:
1. Specification & Planning
2. Architecture Design
3. Backend Development (parallel)
4. Frontend Development (parallel)
5. Comprehensive Testing (parallel)
6. Code Review & QA
7. Integration & Deployment Prep

**Quality**: Coverage >85%, linting, type checking

### Bug Fix Workflow

**Best for**: Bug fixes, issue resolution

**Phases**:
1. Investigation & Root Cause Analysis
2. Fix Implementation
3. Test & Verification (parallel)
4. Review & Approval

**Quality**: Coverage >80%, linting, type checking

### Refactoring Workflow

**Best for**: Code quality improvement, technical debt reduction

**Phases**:
1. Code Analysis & Planning
2. Baseline Test Coverage
3. Refactoring Implementation
4. Verification & QA (parallel)

**Quality**: Coverage ≥80% (must maintain), linting, type checking

### Research Workflow

**Best for**: Technology research, problem investigation

**Phases**:
1. Topic Definition
2. Information Gathering (parallel)
3. Synthesis & Recommendations

**Quality**: None (research-focused)

### Performance Optimization

**Best for**: Performance improvement, bottleneck resolution

**Phases**:
1. Baseline Profiling
2. Bottleneck Analysis
3. Optimization Implementation (parallel)
4. Verification & Benchmarking

**Quality**: Coverage ≥80% (maintain), linting, type checking

### Multi-Repo Sync

**Best for**: Repository synchronization, dependency updates

**Phases**:
1. Repository Discovery
2. Git Synchronization (parallel)
3. Dependency Updates (parallel)
4. Synchronization Verification

**Quality**: None (operational)

## Output Locations

### Checkpoints

```
data/workflows/checkpoints/<workflow-id>/
├── checkpoint_phase_*_start.json
├── checkpoint_phase_*_complete.json
└── checkpoint_latest.json
```

### HTML Reports

```
reports/workflow_report.html  (default)
reports/<custom-path>.html    (with --report-path)
```

### Status Data

Available via:
- `./scripts/workflow-cli.sh status <workflow-id>` (human-readable)
- `./scripts/workflow-cli.sh status <workflow-id> --json-output` (machine-readable)

## Quality Gates

All workflows (except research and multi-repo) enforce:

- **Code Coverage**: Must be ≥80% (or ≥85% for full-stack)
- **Linting**: Must pass (ruff check)
- **Type Checking**: Must pass (mypy)

**Important**: Workflow halts on quality gate failure. Fix issues and resume from checkpoint.

## Error Recovery

Automatic handling:
1. Agent fails → Retry same agent once
2. Still fails → Spawn replacement agent
3. Replacement fails → Fail phase (max 2 failures)

Resume after fixes:
```bash
./scripts/workflow-cli.sh execute <template> \
  --checkpoint-id data/workflows/checkpoints/<workflow-id>/checkpoint_latest.json
```

## Integration

### With SPARC

```bash
# Specification phase
npx claude-flow sparc run specification "Feature description"

# Then execute workflow
./scripts/workflow-cli.sh execute full_stack_development
```

### With CI/CD

```yaml
# .github/workflows/development.yml
- name: Execute Workflow
  run: |
    ./scripts/workflow-cli.sh execute full_stack_development \
      --json-output > result.json

    if [ $(jq -r '.success' result.json) != "true" ]; then
      exit 1
    fi
```

### With Testing

Quality gates automatically use:
```bash
uv run pytest --cov=src --cov-report=json  # Coverage
uv run ruff check .                         # Linting
uv run mypy src                             # Type checking
```

## Troubleshooting

### Quality Gate Failure

```bash
# Check what failed
./scripts/workflow-cli.sh status <workflow-id>

# Fix issues (increase coverage, fix linting, etc.)

# Resume from checkpoint
./scripts/workflow-cli.sh execute <template> \
  --checkpoint-id <checkpoint-path>
```

### Agent Failure

Workflows handle this automatically:
- Retry once
- Spawn replacement
- Fail after 2 failures

### Background Job Stuck

```bash
# Check status
./scripts/workflow-cli.sh status <workflow-id> --json-output

# If stuck, resume from checkpoint
./scripts/workflow-cli.sh execute <template> \
  --checkpoint-id <checkpoint-latest>
```

## Best Practices

1. **Use descriptive workflow IDs**: `--workflow-id auth_feature_v2`
2. **Background mode for long workflows**: `--background`
3. **Check reports**: Open HTML report after completion
4. **Don't bypass quality gates**: Fix issues instead
5. **Clean old checkpoints**: `rm -rf data/workflows/checkpoints/old_*`

## Getting Help

```bash
# CLI help
./scripts/workflow-cli.sh --help
./scripts/workflow-cli.sh execute --help
./scripts/workflow-cli.sh status --help

# Documentation
cat docs/WORKFLOW_TEMPLATES_README.md
cat docs/examples/workflow_examples.md
```

## Performance Limits

- Max 10 phases per workflow
- Max 10 agents per phase
- Max 3 concurrent workflows
- Max 60 minutes per workflow

## Example Session

```bash
# 1. Start full-stack development
$ ./scripts/workflow-cli.sh execute full_stack_development \
    --workflow-id oauth_feature

# Output shows progress through 7 phases...
# Generates: reports/workflow_report.html

# 2. Check final status
$ ./scripts/workflow-cli.sh status oauth_feature

# 3. View report
$ open reports/workflow_report.html  # or xdg-open on Linux
```

---

**For complete documentation**: See `docs/WORKFLOW_TEMPLATES_README.md`

**For detailed examples**: See `docs/examples/workflow_examples.md`

**For implementation details**: See `docs/WORKFLOW_IMPLEMENTATION_SUMMARY.md`
