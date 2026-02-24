# Repository Organization

## Root Directory Structure

The repository root has been cleaned and organized to contain only essential files:

### Configuration Files (Required in Root)
- `.coveragerc` - Coverage configuration
- `.editorconfig` - Editor configuration
- `.gitignore` - Git ignore rules
- `.gitmodules` - Git submodules
- `pyproject.toml` - Python project configuration
- `uv.toml` - UV package manager configuration
- `setup.py` - Package setup

### Documentation (Required in Root)
- `README.md` - Main repository documentation
- `LICENSE` - License file
- `CLAUDE.md` - AI assistant instructions
- `Makefile` - Build automation

### Convenience Scripts
- `create-spec.sh` - Runs `tools/create-spec.py`
- `slash.sh` - Runs `.agent-os/commands/slash_commands.py`

## Organized Directories

### `/tools/`
Development tools and utilities:
- `create-module-agent.py`
- `create-spec.py`
- `create-spec-enhanced.py`
- `execute-tasks.py`

### `/docs/`
Documentation files:
- `/ecosystem/` - Ecosystem documentation
  - `MANDATORY_SLASH_COMMAND_ECOSYSTEM.md`
  - `COMMANDS.md`

### `/tests/modules/orcaflex/batch_processing/`
Test files and configurations:
- `/test_configs/` - YAML test configurations
- `/test_scripts/` - Python test scripts

### `/.agent-os/commands/`
Agent OS command system:
- `slash_commands.py`
- `slash` (executable)

## Files Removed (Cleanup)

### Backup Files
- `CLAUDE.md.backup.*`
- `pyproject.toml.backup`

### Migration Files
- `migration_report.md`
- `REQUIREMENTS_MIGRATION.md`
- `requirements-consolidated.txt`

### Temporary Files
- `coverage.xml`
- `.command-link`
- `.command-registry.json`
- `sample_verification_tasks.json`
- `nul`
- `agos`

### Test Files (Moved)
- All `test_*.py` files moved to `/tests/`
- All `batch_*.yml` files moved to `/tests/`
- All `fsts_*.yml` files moved to `/tests/`

## Benefits of This Organization

1. **Clean Root** - Only essential files remain in root
2. **Logical Structure** - Files grouped by purpose
3. **Easy Navigation** - Clear directory hierarchy
4. **Maintainable** - Related files kept together
5. **Professional** - Follows Python project best practices

## Quick Access

To use the moved tools:

```bash
# Create a spec
./create-spec.sh feature-name

# Or directly
python tools/create-spec.py feature-name

# Run slash commands
./slash.sh /command-name

# Or directly
python .agent-os/commands/slash_commands.py /command-name
```