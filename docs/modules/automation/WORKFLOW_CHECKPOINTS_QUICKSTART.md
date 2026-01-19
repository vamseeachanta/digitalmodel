# Workflow Checkpoints - Quick Start Guide

## TL;DR

```bash
# Create checkpoint
python scripts/checkpoint_cli.py create my_workflow specification --description "Initial spec"

# List checkpoints
python scripts/checkpoint_cli.py list

# Restore checkpoint
python scripts/checkpoint_cli.py restore my_workflow_specification_20260107_120000 --force

# Cleanup old checkpoints
python scripts/checkpoint_cli.py clean
```

## What It Does

Automatically saves complete workflow state (git, files, memory, agents) before each SPARC phase, allowing you to:
- **Rollback** to any previous state
- **Experiment** safely (restore if something breaks)
- **Track** workflow progression
- **Resume** work from any checkpoint

## Quick Examples

### 1. Safe Experimentation

```bash
# Before risky changes
python scripts/checkpoint_cli.py create experiment refactoring \
  --description "Before refactoring auth module" \
  --tags important

# Make changes...
# Oops, something broke!

# Restore to safety
python scripts/checkpoint_cli.py restore experiment_refactoring_20260107_120000 --force
```

### 2. SPARC Workflow Integration

Checkpoints are automatically created at each SPARC phase:

```bash
# Run SPARC TDD workflow
npx claude-flow sparc tdd "user authentication"

# Automatic checkpoints created at:
# - specification phase
# - pseudocode phase
# - architecture phase
# - refinement phase (with TDD iterations)
# - completion phase
```

List them:
```bash
python scripts/checkpoint_cli.py list --workflow-id auth_feature
```

### 3. Compare States

```bash
# Create checkpoints at different stages
python scripts/checkpoint_cli.py create v1 architecture --tags milestone
# ... develop ...
python scripts/checkpoint_cli.py create v2 completion --tags milestone

# View differences
python scripts/checkpoint_cli.py info v1_architecture_20260107_100000 --json > v1.json
python scripts/checkpoint_cli.py info v2_completion_20260107_150000 --json > v2.json
diff v1.json v2.json
```

## Configuration

Edit `config/checkpoint-policies.yaml`:

```yaml
retention:
  time_based_days: 7      # Keep last 7 days
  count_based: 10         # Keep last 10 per workflow
  keep_tagged_forever: true

compression:
  enabled: true
  threshold_mb: 1         # Compress files > 1MB
```

## CLI Reference

```bash
# Create
python scripts/checkpoint_cli.py create WORKFLOW_ID PHASE [--description DESC] [--tags TAG]

# List
python scripts/checkpoint_cli.py list [--workflow-id ID] [--phase PHASE] [--tags TAG]

# Restore
python scripts/checkpoint_cli.py restore CHECKPOINT_ID [--force] [--no-backup]

# Info
python scripts/checkpoint_cli.py info CHECKPOINT_ID [--json]

# Clean
python scripts/checkpoint_cli.py clean [--dry-run]
```

## Enable/Disable

```bash
# Disable automatic checkpoints
export CLAUDE_FLOW_CHECKPOINTS_ENABLED=false

# Enable (default)
export CLAUDE_FLOW_CHECKPOINTS_ENABLED=true
```

## Storage Location

Checkpoints are stored in `.claude-flow/checkpoints/`:

```
.claude-flow/checkpoints/
└── workflow_id/
    └── timestamp/
        ├── metadata.json    # State info
        ├── memory.json      # MCP memory
        └── files/           # File snapshots
```

## Best Practices

1. **Tag milestones**: `--tags release milestone`
2. **Descriptive names**: Include what changed
3. **Before risky changes**: Always create checkpoint
4. **Regular cleanup**: Run `clean` periodically
5. **Test restoration**: Verify backups work

## Troubleshooting

**Checkpoint creation fails:**
```bash
# Check git status
git status

# Verify in git repo
git rev-parse --show-toplevel
```

**Restoration not working:**
```bash
# Verify checkpoint exists
python scripts/checkpoint_cli.py info CHECKPOINT_ID

# Try with force
python scripts/checkpoint_cli.py restore CHECKPOINT_ID --force
```

**Out of disk space:**
```bash
# List checkpoints by size
python scripts/checkpoint_cli.py list --json | python -m json.tool

# Adjust retention (reduce days/count)
# Edit config/checkpoint-policies.yaml

# Clean up
python scripts/checkpoint_cli.py clean
```

## Integration Examples

### Pre-commit Hook

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/bash
python scripts/checkpoint_cli.py create "$(git branch --show-current)" pre_commit \
  --description "Before commit: $(git log -1 --format=%s)" \
  --tags pre-commit
```

### CI/CD

```yaml
# .github/workflows/checkpoint.yml
on:
  release:
    types: [published]
jobs:
  checkpoint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - run: |
          python scripts/checkpoint_cli.py create release "${{ github.ref_name }}" \
            --description "Release ${{ github.ref_name }}" \
            --tags release production
```

## See Also

- Full documentation: `docs/WORKFLOW_CHECKPOINTS.md`
- Configuration: `config/checkpoint-policies.yaml`
- Source code: `src/digitalmodel/modules/automation/workflow_checkpoints.py`
- Tests: `tests/test_workflow_checkpoints.py`
