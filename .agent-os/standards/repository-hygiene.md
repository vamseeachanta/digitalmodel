# Repository Hygiene Standards

## 🧹 MANDATORY: Clean Repository Root

This document defines mandatory standards for maintaining a clean repository root directory.

## Critical Principle

**The repository root is the face of the project. It MUST remain clean, organized, and professional at all times.**

## Root Directory Whitelist

### ONLY these files are permitted in root:

#### Configuration Files (Required by Tools)
- `.gitignore` - Git ignore patterns
- `.gitmodules` - Git submodule configuration
- `.editorconfig` - Editor configuration
- `.coveragerc` - Coverage tool configuration
- `pyproject.toml` - Python project metadata
- `uv.toml` - UV package manager config
- `setup.py` - Package installation

#### Documentation (User-Facing)
- `README.md` - Project overview
- `LICENSE` - Legal license
- `CLAUDE.md` - AI assistant guidance
- `Makefile` - Build automation

#### Convenience Scripts (Optional)
- `*.sh` - Shell scripts that redirect to tools/
- `*.bat` - Windows batch files that redirect to tools/

### EVERYTHING ELSE is PROHIBITED in root

## Automatic Cleanup Protocol

### When Creating Files

Before creating ANY file in root, ask:
1. Is this file on the whitelist above?
2. If NO → Create it in the appropriate subdirectory instead

### When Detecting Violations

When ANY non-whitelisted file is found in root:

```python
if file.is_test():
    move_to("tests/modules/<appropriate_module>/")
elif file.is_config():
    move_to("config/" or "tests/.../test_configs/")
elif file.is_backup():
    delete_immediately()
elif file.is_temporary():
    delete_immediately()
elif file.is_tool():
    move_to("tools/")
elif file.is_documentation():
    move_to("docs/")
else:
    evaluate_and_move_to_appropriate_location()
```

## File Categories and Destinations

### Test Files
- **Pattern**: `test_*.py`, `*_test.py`, `test_*.yml`
- **Destination**: `/tests/modules/<module>/`
- **Subdirs**: `test_scripts/`, `test_configs/`, `test_data/`

### Configuration Files
- **Pattern**: `*.yml`, `*.yaml`, `*.json`, `*.ini`
- **Destination**: `/config/` or context-specific location
- **Exception**: `pyproject.toml`, `uv.toml` (required in root)

### Backup Files
- **Pattern**: `*.backup*`, `*.bak`, `*~`, `*.old`
- **Action**: DELETE IMMEDIATELY
- **Never**: Commit to repository

### Migration Files
- **Pattern**: `*migration*`, `*upgrade*`, `requirements-*.txt`
- **Action**: DELETE after migration complete
- **Document**: Migration steps in `/docs/migrations/`

### Temporary Files
- **Pattern**: `tmp*`, `temp*`, `*.tmp`, `nul`, `coverage.xml`
- **Action**: DELETE IMMEDIATELY
- **Use**: System temp directory for temporary work

### Generated Files
- **Pattern**: `*.pyc`, `__pycache__`, `*.egg-info`, `dist/`, `build/`
- **Action**: Add to `.gitignore` and DELETE
- **Never**: Commit to repository

### Tool Scripts
- **Pattern**: `create-*.py`, `*-tool.py`, utility scripts
- **Destination**: `/tools/`
- **Access**: Create convenience `.sh` script in root if needed

## Enforcement Rules

### For AI Agents

1. **BEFORE** creating any file in root:
   - Check whitelist
   - Use appropriate subdirectory instead

2. **AFTER** any operation:
   - Scan root for violations
   - Clean up automatically
   - Report cleanup actions

3. **WHEN** user creates files in root:
   - Immediately suggest proper location
   - Offer to move files automatically
   - Update `.gitignore` if needed

### For Developers

1. **Pre-commit Hook**: Reject commits with non-whitelisted root files
2. **CI/CD Check**: Fail builds with root directory violations
3. **Code Review**: Block PRs that add files to root

## Standard Directory Structure

```
digitalmodel/
├── .agent-os/          # Agent OS framework
│   ├── commands/       # Slash commands
│   └── standards/      # Standards like this file
├── agents/             # AI agents
├── config/             # Configuration files
├── docs/               # Documentation
├── specs/              # Specifications
├── src/                # Source code
├── tests/              # Test files
├── tools/              # Development tools
├── .gitignore          # ✅ Whitelisted
├── CLAUDE.md           # ✅ Whitelisted
├── LICENSE             # ✅ Whitelisted
├── Makefile            # ✅ Whitelisted
├── pyproject.toml      # ✅ Whitelisted
├── README.md           # ✅ Whitelisted
├── setup.py            # ✅ Whitelisted
└── uv.toml             # ✅ Whitelisted
```

## Cleanup Commands

### Manual Cleanup
```bash
# Find test files in root
find . -maxdepth 1 -name "test_*.py" -o -name "*_test.py"

# Find backup files
find . -maxdepth 1 -name "*.backup*" -o -name "*.bak"

# Find config files
find . -maxdepth 1 -name "*.yml" -o -name "*.yaml" | grep -v docker

# Delete all backup files
find . -maxdepth 1 -name "*.backup*" -delete
```

### Automated Cleanup Script
Create `/tools/cleanup-root.py`:
```python
#!/usr/bin/env python
"""Automatically clean up repository root directory."""
# Implementation here
```

## Benefits

1. **Professional Appearance**: Clean root makes good first impression
2. **Easy Navigation**: Users find what they need quickly
3. **Reduced Confusion**: Clear organization reduces errors
4. **Better Maintenance**: Easier to manage and update
5. **CI/CD Friendly**: Predictable structure for automation

## Exceptions

**NONE**. The root directory hygiene rules have no exceptions.

If a tool absolutely requires a file in root:
1. Document the requirement
2. Add to whitelist in this document
3. Update CLAUDE.md with the exception

## Monitoring

- Weekly automated scans for violations
- Pre-commit hooks to prevent violations
- CI/CD checks on every push
- Quarterly hygiene audits

---

**This standard is MANDATORY and takes precedence over any conflicting guidance.**