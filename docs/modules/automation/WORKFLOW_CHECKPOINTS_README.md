# Workflow Checkpoint Management System

## Quick Reference

### Installation
No installation needed - already integrated into digitalmodel repository.

### Basic Commands

```bash
# Create checkpoint
python scripts/checkpoint_cli.py create WORKFLOW_ID PHASE --description "DESC"

# List checkpoints
python scripts/checkpoint_cli.py list

# Restore checkpoint  
python scripts/checkpoint_cli.py restore CHECKPOINT_ID --force

# Cleanup old checkpoints
python scripts/checkpoint_cli.py clean
```

### Files & Locations

```
src/digitalmodel/modules/automation/workflow_checkpoints.py  # Core implementation
config/checkpoint-policies.yaml                               # Configuration
scripts/checkpoint_cli.py                                     # CLI interface
scripts/checkpoint_hooks.sh                                   # Hook integration
docs/WORKFLOW_CHECKPOINTS.md                                  # Full documentation
docs/WORKFLOW_CHECKPOINTS_QUICKSTART.md                       # Quick start guide
tests/test_workflow_checkpoints.py                            # Tests (84.5% coverage)
```

### Storage

```
.claude-flow/checkpoints/
└── {workflow-id}/
    └── {timestamp}/
        ├── metadata.json    # Checkpoint info
        ├── memory.json      # MCP memory state
        └── files/           # File snapshots
```

### Features

- ✅ Full state capture (git + memory + files + agents)
- ✅ Automatic SPARC phase checkpoints
- ✅ Incremental snapshots (only changed files)
- ✅ Automatic compression (files >1MB)
- ✅ Safe restoration (with backup)
- ✅ Automatic cleanup (time + count based)
- ✅ Tag support for important checkpoints
- ✅ CLI with filtering and JSON output

### Test Coverage

- **84.5%** code coverage
- **23/25** tests passing (92% pass rate)
- Comprehensive integration tests

### Documentation

- **Full Guide**: `docs/WORKFLOW_CHECKPOINTS.md`
- **Quick Start**: `docs/WORKFLOW_CHECKPOINTS_QUICKSTART.md`
- **Implementation**: `IMPLEMENTATION_SUMMARY.md`

### Examples

```bash
# Before risky changes
python scripts/checkpoint_cli.py create experiment refactoring \
  --description "Before auth refactor" --tags important

# List by workflow
python scripts/checkpoint_cli.py list --workflow-id experiment

# Restore if needed
python scripts/checkpoint_cli.py restore experiment_refactoring_20260107_120000 --force

# Cleanup
python scripts/checkpoint_cli.py clean
```

### Configuration

Edit `config/checkpoint-policies.yaml`:

```yaml
retention:
  time_based_days: 7      # Keep last 7 days
  count_based: 10         # Keep last 10 per workflow
  keep_tagged_forever: true

compression:
  enabled: true
  threshold_mb: 1
```

### Enable/Disable

```bash
export CLAUDE_FLOW_CHECKPOINTS_ENABLED=false  # Disable
export CLAUDE_FLOW_CHECKPOINTS_ENABLED=true   # Enable (default)
```

### Support

- View full documentation: `docs/WORKFLOW_CHECKPOINTS.md`
- Check tests: `tests/test_workflow_checkpoints.py`
- Source code: `src/digitalmodel/modules/automation/workflow_checkpoints.py`
