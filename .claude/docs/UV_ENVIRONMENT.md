# UV Environment Support

This repository now has UV environment support integrated into all commands.

## What Changed

All test, task, and spec commands now:
1. Automatically detect existing UV environments
2. Use the UV Python interpreter when available
3. Can enhance UV environments with spec-specific dependencies
4. Prevent creation of duplicate virtual environments

## Benefits

- **Faster execution**: No need to create new virtual environments
- **Consistent dependencies**: All commands use the same environment
- **Better resource usage**: Single environment per repository
- **Automatic enhancement**: Specs can add dependencies as needed

## Commands with UV Support

- `/test` - Run tests using UV environment
- `/task` - Execute tasks using UV environment  
- `/spec` - Create specs with UV-aware execution
- `/uv-env` - Manage UV environments directly

## UV Environment Manager

The UV environment manager provides:
- Automatic detection of existing UV environments
- Environment creation if needed
- Dependency management
- Python executable discovery

## Usage

Commands automatically use UV when available. No configuration needed.

To manually manage UV environments:
```bash
/uv-env info        # Show UV environment info
/uv-env ensure      # Ensure UV environment exists
/uv-env sync        # Sync dependencies
/uv-env add PACKAGE # Add a package
```

## Creating UV Environment

If your repo doesn't have UV yet:
```bash
uv init
uv venv
uv sync
```

## Requirements

UV must be installed globally:
```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

---
Last updated: 2024-01-13
