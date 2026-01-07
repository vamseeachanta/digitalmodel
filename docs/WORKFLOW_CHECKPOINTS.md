# Workflow Checkpoint Management System

## Overview

The Workflow Checkpoint Management System provides automatic state capture and restoration for SPARC development workflows. It captures complete workflow state including git repository state, MCP memory, file snapshots, and agent metadata.

## Features

- **Full State Capture**: Git state, memory, files, and agent metadata
- **Automatic Checkpointing**: Before each SPARC phase
- **Incremental Snapshots**: Only changed files are captured
- **Compression**: Large files (>1MB) are automatically compressed
- **Safe Restoration**: Current state backed up before restoration
- **Automatic Cleanup**: Based on time and count retention policies
- **Tag Support**: Tag important checkpoints to keep forever

## Installation

The checkpoint system is integrated into the digitalmodel repository. No additional installation required.

## Quick Start

### Create a Checkpoint

```bash
# Using NPX wrapper
npx claude-flow@alpha checkpoint create my_workflow specification --description "Initial spec"

# Or directly
python scripts/checkpoint_cli.py create my_workflow specification --description "Initial spec"
```

### List Checkpoints

```bash
# List all checkpoints
npx claude-flow@alpha checkpoint list

# Filter by workflow
npx claude-flow@alpha checkpoint list --workflow-id my_workflow

# Filter by phase
npx claude-flow@alpha checkpoint list --phase architecture

# Filter by tags
npx claude-flow@alpha checkpoint list --tags important

# JSON output
npx claude-flow@alpha checkpoint list --json
```

### Restore a Checkpoint

```bash
# Restore (will prompt for confirmation)
npx claude-flow@alpha checkpoint restore my_workflow_specification_20260106_120000

# Force restore without confirmation
npx claude-flow@alpha checkpoint restore my_workflow_specification_20260106_120000 --force

# Restore without creating backup
npx claude-flow@alpha checkpoint restore my_workflow_specification_20260106_120000 --force --no-backup
```

### Get Checkpoint Info

```bash
# Detailed checkpoint information
npx claude-flow@alpha checkpoint info my_workflow_specification_20260106_120000

# JSON output
npx claude-flow@alpha checkpoint info my_workflow_specification_20260106_120000 --json
```

### Clean Up Old Checkpoints

```bash
# Clean up based on retention policies
npx claude-flow@alpha checkpoint clean

# Dry run (show what would be deleted)
npx claude-flow@alpha checkpoint clean --dry-run

# JSON output
npx claude-flow@alpha checkpoint clean --json
```

## Automatic Checkpointing

### SPARC Phase Checkpoints

Checkpoints are automatically created before each SPARC phase:
- `specification`
- `pseudocode`
- `architecture`
- `refinement`
- `completion`

### Claude-Flow Hooks Integration

The checkpoint system integrates with claude-flow hooks:

```bash
# Pre-task checkpoint
npx claude-flow@alpha hooks pre-task --description "Start feature X"
# (Internally calls: checkpoint_hooks.sh pre-task "Start feature X" workflow_id phase)

# Post-task checkpoint
npx claude-flow@alpha hooks post-task --task-id task_123
# (Internally calls: checkpoint_hooks.sh post-task task_123 workflow_id phase)

# Session-end cleanup
npx claude-flow@alpha hooks session-end
# (Internally calls: checkpoint_hooks.sh session-end workflow_id)
```

### Enable/Disable Automatic Checkpoints

```bash
# Disable checkpoints
export CLAUDE_FLOW_CHECKPOINTS_ENABLED=false

# Enable checkpoints (default)
export CLAUDE_FLOW_CHECKPOINTS_ENABLED=true
```

## Configuration

Configuration file: `config/checkpoint-policies.yaml`

### Retention Policies

```yaml
retention:
  # Keep checkpoints from last N days
  time_based_days: 7

  # Keep last N checkpoints per workflow
  count_based: 10

  # Keep tagged checkpoints forever
  keep_tagged_forever: true
```

### Compression Settings

```yaml
compression:
  # Enable compression
  enabled: true

  # Compress files larger than N MB
  threshold_mb: 1
```

### Incremental Snapshots

```yaml
incremental:
  # Only snapshot changed files
  enabled: true
```

### Automatic Cleanup

```yaml
automatic_cleanup:
  # Enable automatic cleanup
  enabled: true

  # Run cleanup on session end
  on_session_end: true
```

## Checkpoint Structure

Checkpoints are stored in `.claude-flow/checkpoints/` with the following structure:

```
.claude-flow/checkpoints/
└── {workflow-id}/
    └── {timestamp}/
        ├── metadata.json       # Checkpoint metadata
        ├── memory.json         # MCP memory state
        └── files/              # File snapshots
            ├── src/
            ├── tests/
            └── ...
```

### Metadata Format

```json
{
  "checkpoint_id": "workflow1_specification_20260106_120000",
  "workflow_id": "workflow1",
  "phase": "specification",
  "description": "Initial specification",
  "timestamp": "20260106_120000",
  "tags": ["important"],
  "git_state": {
    "commit_hash": "abc123...",
    "branch": "main",
    "author": "John Doe",
    "message": "Latest commit",
    "dirty": false,
    "staged_files": [],
    "modified_files": [],
    "untracked_files": []
  },
  "agent_states": [
    {
      "agent_id": "agent_123",
      "agent_type": "coder",
      "status": "active",
      "task_id": "task_456",
      "memory_keys": ["swarm/coder/status"]
    }
  ],
  "memory_keys": ["swarm/shared/implementation"],
  "file_count": 42,
  "total_size_bytes": 1048576,
  "compressed": true
}
```

## Use Cases

### 1. Safe Experimentation

Create a checkpoint before trying risky changes:

```bash
# Create checkpoint
npx claude-flow@alpha checkpoint create experiment refactoring \
  --description "Before major refactoring" \
  --tags important

# Make changes...

# If something breaks, restore
npx claude-flow@alpha checkpoint restore experiment_refactoring_20260106_120000 --force
```

### 2. SPARC Workflow Milestones

Checkpoints are automatically created at each SPARC phase:

```bash
# Run SPARC workflow
npx claude-flow sparc tdd "user authentication"

# Checkpoints created at:
# - specification phase
# - pseudocode phase
# - architecture phase
# - refinement phase
# - completion phase
```

### 3. Rollback to Known Good State

```bash
# List recent checkpoints
npx claude-flow@alpha checkpoint list --workflow-id my_feature

# Restore to last working state
npx claude-flow@alpha checkpoint restore my_feature_architecture_20260105_143000 --force
```

### 4. Compare States

```bash
# Get checkpoint info
npx claude-flow@alpha checkpoint info checkpoint1 > checkpoint1.json
npx claude-flow@alpha checkpoint info checkpoint2 > checkpoint2.json

# Compare with diff
diff checkpoint1.json checkpoint2.json
```

### 5. Tag Important Milestones

```bash
# Create tagged checkpoint (kept forever)
npx claude-flow@alpha checkpoint create v1_release production \
  --description "Production release v1.0" \
  --tags release production milestone
```

## CLI Reference

### `create`

Create a new checkpoint.

```bash
npx claude-flow@alpha checkpoint create WORKFLOW_ID PHASE [OPTIONS]
```

**Arguments:**
- `WORKFLOW_ID`: Unique workflow identifier
- `PHASE`: SPARC phase (specification, pseudocode, architecture, refinement, completion)

**Options:**
- `--description, -d TEXT`: Checkpoint description
- `--tags, -t TEXT`: Checkpoint tags (can be specified multiple times)
- `--json`: Output as JSON
- `--verbose, -v`: Enable verbose logging

**Example:**
```bash
npx claude-flow@alpha checkpoint create auth_feature specification \
  --description "User authentication specification" \
  --tags important \
  --tags security
```

### `restore`

Restore a checkpoint.

```bash
npx claude-flow@alpha checkpoint restore CHECKPOINT_ID [OPTIONS]
```

**Arguments:**
- `CHECKPOINT_ID`: Checkpoint ID to restore

**Options:**
- `--force, -f`: Skip confirmation prompt
- `--no-backup`: Don't create backup before restore
- `--json`: Output as JSON
- `--verbose, -v`: Enable verbose logging

**Example:**
```bash
npx claude-flow@alpha checkpoint restore auth_feature_specification_20260106_120000 --force
```

### `list`

List available checkpoints.

```bash
npx claude-flow@alpha checkpoint list [OPTIONS]
```

**Options:**
- `--workflow-id, -w TEXT`: Filter by workflow ID
- `--phase, -p TEXT`: Filter by phase
- `--tags, -t TEXT`: Filter by tags (can be specified multiple times)
- `--date-from TEXT`: Filter by start date (YYYY-MM-DD)
- `--date-to TEXT`: Filter by end date (YYYY-MM-DD)
- `--json`: Output as JSON
- `--verbose, -v`: Enable verbose logging

**Example:**
```bash
npx claude-flow@alpha checkpoint list \
  --workflow-id auth_feature \
  --phase specification \
  --tags important
```

### `info`

Show detailed checkpoint information.

```bash
npx claude-flow@alpha checkpoint info CHECKPOINT_ID [OPTIONS]
```

**Arguments:**
- `CHECKPOINT_ID`: Checkpoint ID

**Options:**
- `--json`: Output as JSON
- `--verbose, -v`: Enable verbose logging

**Example:**
```bash
npx claude-flow@alpha checkpoint info auth_feature_specification_20260106_120000
```

### `clean`

Clean up old checkpoints.

```bash
npx claude-flow@alpha checkpoint clean [OPTIONS]
```

**Options:**
- `--dry-run`: Show what would be deleted without deleting
- `--json`: Output as JSON
- `--verbose, -v`: Enable verbose logging

**Example:**
```bash
npx claude-flow@alpha checkpoint clean --dry-run
```

## Best Practices

1. **Tag Important Checkpoints**: Use tags for releases, milestones, or known good states
2. **Use Descriptive Names**: Include meaningful descriptions for easier identification
3. **Regular Cleanup**: Run cleanup periodically to manage disk space
4. **Test Restoration**: Periodically test checkpoint restoration to ensure it works
5. **Backup Before Risky Changes**: Create manual checkpoints before major refactoring
6. **Review Before Restore**: Check checkpoint info before restoring to verify it's the right one

## Troubleshooting

### Checkpoint Creation Fails

```bash
# Check git status
git status

# Ensure in git repository
git rev-parse --show-toplevel

# Check permissions
ls -la .claude-flow/checkpoints/
```

### Restoration Not Working

```bash
# Verify checkpoint exists
npx claude-flow@alpha checkpoint info CHECKPOINT_ID

# Check for dirty working directory
git status

# Try with force flag
npx claude-flow@alpha checkpoint restore CHECKPOINT_ID --force
```

### Cleanup Not Working

```bash
# Check configuration
cat config/checkpoint-policies.yaml

# Run with verbose logging
npx claude-flow@alpha checkpoint clean --verbose
```

### Out of Disk Space

```bash
# List checkpoints by size
npx claude-flow@alpha checkpoint list --json | jq '.[] | {id: .checkpoint_id, size_mb: .size_mb}'

# Reduce retention period
# Edit config/checkpoint-policies.yaml:
# retention:
#   time_based_days: 3  # Reduce from 7
#   count_based: 5      # Reduce from 10

# Run cleanup
npx claude-flow@alpha checkpoint clean
```

## Integration with Development Workflow

### 1. Pre-commit Hook

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/bash
# Create checkpoint before commit
npx claude-flow@alpha checkpoint create "$(git symbolic-ref --short HEAD)" pre_commit \
  --description "Before commit: $(git log -1 --format=%s)" \
  --tags pre-commit
```

### 2. CI/CD Integration

```yaml
# .github/workflows/checkpoint.yml
name: Create Release Checkpoint
on:
  release:
    types: [published]
jobs:
  checkpoint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Create checkpoint
        run: |
          npx claude-flow@alpha checkpoint create release "${{ github.ref_name }}" \
            --description "Release ${{ github.ref_name }}" \
            --tags release production
```

### 3. SPARC TDD Workflow

```bash
# Automatic checkpoints at each phase
npx claude-flow sparc tdd "user authentication"

# Phases automatically checkpointed:
# 1. specification
# 2. pseudocode
# 3. architecture
# 4. refinement (with TDD iterations)
# 5. completion
```

## Performance Considerations

- **Incremental Snapshots**: Only changed files are captured (configurable)
- **Compression**: Files >1MB are automatically compressed with gzip
- **Cleanup**: Automatic cleanup prevents disk space issues
- **Parallel Operations**: File operations use parallel processing where possible

## Security Considerations

- **No Secrets**: Never commit checkpoints with secrets (they're in `.claude-flow/` which should be gitignored)
- **Local Only**: Checkpoints are stored locally, not in git
- **File Permissions**: Checkpoints inherit repository file permissions
- **Backup Verification**: Always verify backups before relying on them

## Limitations

- **Git Required**: Checkpoint system requires a git repository
- **Local Storage**: Checkpoints are stored locally (not distributed)
- **No Partial Restore**: Must restore entire checkpoint (no file-level restore yet)
- **Memory Snapshots**: Only captures `.claude-flow/memory/` directory

## Future Enhancements

- [ ] Remote checkpoint storage (cloud backup)
- [ ] Partial restoration (restore specific files)
- [ ] Checkpoint diffing (compare two checkpoints)
- [ ] Checkpoint tagging UI
- [ ] Automatic checkpoint on test failures
- [ ] Integration with GitHub releases
- [ ] Checkpoint export/import
- [ ] Encrypted checkpoints for sensitive data

## Support

For issues or questions:
- Check troubleshooting section above
- Review configuration in `config/checkpoint-policies.yaml`
- Enable verbose logging with `--verbose` flag
- Check `.claude-flow/checkpoints/` directory structure
