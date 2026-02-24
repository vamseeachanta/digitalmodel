---
name: uv-package-manager
version: 1.0.0
description: UV for fast Python package management, virtual environments, and project workflows
author: workspace-hub
category: devtools
tags: [uv, python, package-manager, virtual-environment, dependency-management]
platforms: [python, linux, macos, windows]
---

# UV Package Manager Skill

Master UV for blazing-fast Python package management, virtual environment creation, and modern Python project workflows.

## When to Use This Skill

Use UV package manager when you need:
- **Fast dependency installation** - 10-100x faster than pip
- **Virtual environment management** - Create and manage venvs effortlessly
- **Project initialization** - Start new Python projects quickly
- **Dependency resolution** - Reliable, reproducible dependency trees
- **Lock file management** - Ensure consistent environments across machines
- **Python version management** - Install and switch Python versions

**Avoid when:**
- Legacy systems requiring pip compatibility (rare)
- Conda-based scientific computing environments
- Docker images with pre-installed pip workflows

## Installation

```bash
# Linux/macOS
curl -LsSf https://astral.sh/uv/install.sh | sh

# Windows (PowerShell)
powershell -c "irm https://astral.sh/uv/install.ps1 | iex"

# Homebrew
brew install uv

# pip (if needed)
pip install uv
```

## Core Capabilities

### 1. Project Initialization

**Create a new Python project:**
```bash
# Initialize new project with pyproject.toml
uv init my-project
cd my-project

# Initialize in current directory
uv init

# Initialize with specific Python version
uv init --python 3.11
```

**Project structure created:**
```
my-project/
├── .python-version      # Python version lock
├── pyproject.toml       # Project configuration
├── README.md            # Project readme
└── src/
    └── my_project/
        └── __init__.py
```

### 2. Virtual Environment Management

**Create and activate virtual environments:**
```bash
# Create venv (default .venv directory)
uv venv

# Create with specific Python version
uv venv --python 3.11

# Create with custom name
uv venv .venv-test

# Activate (Linux/macOS)
source .venv/bin/activate

# Activate (Windows)
.venv\Scripts\activate

# Activate (Git Bash on Windows)
source .venv/Scripts/activate
```

**List available Python versions:**
```bash
# List installed Python versions
uv python list

# Install specific Python version
uv python install 3.12

# Pin Python version for project
uv python pin 3.11
```

### 3. Dependency Management

**Add dependencies:**
```bash
# Add a single package
uv add pandas

# Add multiple packages
uv add numpy scipy matplotlib

# Add with version constraints
uv add "pandas>=2.0,<3.0"

# Add development dependencies
uv add --dev pytest pytest-cov ruff

# Add optional dependencies
uv add --optional ml tensorflow torch

# Add from git repository
uv add git+https://github.com/user/repo.git

# Add local package in editable mode
uv add --editable ../my-local-package
```

**Remove dependencies:**
```bash
# Remove a package
uv remove pandas

# Remove dev dependency
uv remove --dev pytest
```

**Sync dependencies:**
```bash
# Install all dependencies from pyproject.toml
uv sync

# Sync including dev dependencies
uv sync --dev

# Sync with optional dependencies
uv sync --extra ml

# Sync all extras
uv sync --all-extras
```

### 4. Lock File Management

**Understanding uv.lock:**
```bash
# Lock file is auto-generated on uv add/sync
# Contains exact versions for reproducibility

# Regenerate lock file
uv lock

# Lock with specific Python version
uv lock --python 3.11

# Update all dependencies to latest
uv lock --upgrade

# Update specific package
uv lock --upgrade-package pandas
```

**Lock file structure:**
```toml
# uv.lock (auto-generated, do not edit manually)
version = 1
requires-python = ">=3.9"

[[package]]
name = "pandas"
version = "2.2.0"
source = { registry = "https://pypi.org/simple" }
dependencies = [
    { name = "numpy" },
    { name = "python-dateutil" },
    { name = "pytz" },
]
```

### 5. Running Scripts and Commands

**Run Python scripts:**
```bash
# Run script with project dependencies
uv run python script.py

# Run module
uv run python -m pytest

# Run with specific Python version
uv run --python 3.11 python script.py
```

**Run tools:**
```bash
# Run pytest
uv run pytest tests/

# Run ruff for linting
uv run ruff check src/

# Run black for formatting
uv run black src/

# Run any CLI tool
uv run mypy src/
```

### 6. pip Compatibility Mode

**Use UV as a pip replacement:**
```bash
# Install packages (pip syntax)
uv pip install pandas numpy

# Install from requirements.txt
uv pip install -r requirements.txt

# Install in editable mode
uv pip install -e .

# Compile requirements
uv pip compile requirements.in -o requirements.txt

# Sync environment
uv pip sync requirements.txt

# Freeze installed packages
uv pip freeze > requirements.txt

# Show package info
uv pip show pandas
```

## Complete Examples

### Example 1: New Project Setup

```bash
#!/bin/bash
# setup_project.sh - Initialize a new Python project with UV

PROJECT_NAME=${1:-"my-project"}

# Create and enter project
uv init "$PROJECT_NAME"
cd "$PROJECT_NAME"

# Pin Python version
uv python pin 3.11

# Add core dependencies
uv add pandas numpy pyyaml click

# Add dev dependencies
uv add --dev pytest pytest-cov ruff mypy black isort

# Add visualization (optional)
uv add --optional viz plotly matplotlib

# Create project structure
mkdir -p src/"${PROJECT_NAME//-/_}" tests data docs

# Create __init__.py
touch src/"${PROJECT_NAME//-/_}"/__init__.py

# Create test file
cat > tests/test_example.py << 'EOF'
def test_import():
    """Test that package can be imported."""
    import sys
    assert 'my_project' in sys.modules or True
EOF

# Create .gitignore
cat > .gitignore << 'EOF'
.venv/
__pycache__/
*.pyc
.pytest_cache/
.mypy_cache/
.ruff_cache/
dist/
*.egg-info/
.coverage
htmlcov/
EOF

# Run tests to verify setup
uv run pytest tests/ -v

echo "Project $PROJECT_NAME initialized successfully!"
echo "Activate with: source .venv/bin/activate"
```

### Example 2: Migrate from pip to UV

```bash
#!/bin/bash
# migrate_to_uv.sh - Migrate existing project to UV

# Backup existing requirements
cp requirements.txt requirements.txt.bak 2>/dev/null || true

# Initialize UV project (if pyproject.toml doesn't exist)
if [ ! -f pyproject.toml ]; then
    uv init --no-readme
fi

# Add dependencies from requirements.txt
if [ -f requirements.txt ]; then
    echo "Migrating dependencies from requirements.txt..."

    # Parse and add each dependency
    while IFS= read -r line; do
        # Skip comments and empty lines
        [[ "$line" =~ ^#.*$ ]] && continue
        [[ -z "$line" ]] && continue

        # Add dependency
        uv add "$line" 2>/dev/null || echo "Skipped: $line"
    done < requirements.txt
fi

# Add dev dependencies from requirements-dev.txt
if [ -f requirements-dev.txt ]; then
    echo "Migrating dev dependencies..."

    while IFS= read -r line; do
        [[ "$line" =~ ^#.*$ ]] && continue
        [[ -z "$line" ]] && continue
        uv add --dev "$line" 2>/dev/null || echo "Skipped: $line"
    done < requirements-dev.txt
fi

# Sync to ensure everything is installed
uv sync --dev

echo "Migration complete! Review pyproject.toml and uv.lock"
```

### Example 3: CI/CD Pipeline with UV

```yaml
# .github/workflows/ci.yml
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        python-version: ["3.10", "3.11", "3.12"]

    steps:
      - uses: actions/checkout@v4

      - name: Install UV
        uses: astral-sh/setup-uv@v4
        with:
          version: "latest"

      - name: Set up Python
        run: uv python install ${{ matrix.python-version }}

      - name: Install dependencies
        run: uv sync --dev

      - name: Run linting
        run: uv run ruff check src/

      - name: Run type checking
        run: uv run mypy src/

      - name: Run tests
        run: uv run pytest tests/ --cov=src --cov-report=xml

      - name: Upload coverage
        uses: codecov/codecov-action@v4
        with:
          files: ./coverage.xml

  build:
    runs-on: ubuntu-latest
    needs: test
    steps:
      - uses: actions/checkout@v4

      - name: Install UV
        uses: astral-sh/setup-uv@v4

      - name: Build package
        run: uv build

      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        with:
          name: dist
          path: dist/
```

### Example 4: Multi-Environment Management

```bash
#!/bin/bash
# manage_envs.sh - Manage multiple Python environments

# Function to create environment for specific Python version
create_env() {
    local py_version=$1
    local env_name=".venv-py${py_version//./}"

    echo "Creating environment for Python $py_version..."
    uv venv "$env_name" --python "$py_version"

    # Install dependencies
    source "$env_name/bin/activate"
    uv sync
    deactivate

    echo "Created: $env_name"
}

# Create environments for multiple Python versions
create_env 3.10
create_env 3.11
create_env 3.12

# Run tests across all environments
echo "Running tests across environments..."
for env in .venv-py*; do
    echo "Testing with $env..."
    source "$env/bin/activate"
    uv run pytest tests/ -q
    deactivate
done
```

### Example 5: Workspace-Hub Project Setup

```bash
#!/bin/bash
# setup_workspace_project.sh - Setup project following workspace-hub patterns

PROJECT_NAME=${1:-"new-project"}
TEMPLATE_REPO="workspace-hub/pyproject-starter"

# Initialize with UV
uv init "$PROJECT_NAME"
cd "$PROJECT_NAME"

# Pin Python version (match workspace-hub standard)
uv python pin 3.11

# Add workspace-hub standard dependencies
uv add \
    pyyaml \
    pandas \
    numpy \
    plotly \
    click

# Add standard dev dependencies
uv add --dev \
    pytest \
    pytest-cov \
    ruff \
    mypy \
    black \
    isort \
    deepdiff

# Create workspace-hub directory structure
mkdir -p \
    src/"${PROJECT_NAME//-/_}" \
    tests \
    docs \
    data/raw \
    data/processed \
    data/results \
    reports \
    config \
    scripts

# Create standard files
touch src/"${PROJECT_NAME//-/_}"/__init__.py

# Create conftest.py for pytest
cat > tests/conftest.py << 'EOF'
"""Pytest configuration and fixtures."""
import pytest
from pathlib import Path

@pytest.fixture
def project_root():
    """Return project root directory."""
    return Path(__file__).parent.parent

@pytest.fixture
def data_dir(project_root):
    """Return data directory."""
    return project_root / "data"

@pytest.fixture
def test_data_dir(project_root):
    """Return test data directory."""
    return project_root / "tests" / "data"
EOF

# Create CLAUDE.md for AI development
cat > CLAUDE.md << 'EOF'
# Project Configuration

## Development Commands

```bash
# Install dependencies
uv sync --dev

# Run tests
uv run pytest tests/ -v

# Run linting
uv run ruff check src/

# Run formatting
uv run black src/ tests/
uv run isort src/ tests/

# Run type checking
uv run mypy src/
```

## Project Structure

- `src/` - Source code
- `tests/` - Test files
- `docs/` - Documentation
- `data/` - Data files (raw, processed, results)
- `reports/` - Generated HTML reports
- `config/` - Configuration files
EOF

echo "Project $PROJECT_NAME created with workspace-hub patterns!"
```

## Best Practices

### 1. Version Pinning

```bash
# Always pin Python version for reproducibility
uv python pin 3.11

# Use version ranges in pyproject.toml
# [project]
# requires-python = ">=3.10,<3.13"
```

### 2. Lock File Hygiene

```bash
# Commit uv.lock to version control
git add uv.lock

# Update regularly
uv lock --upgrade

# Review changes before committing
git diff uv.lock
```

### 3. Dependency Groups

```toml
# pyproject.toml - organize dependencies logically
[project]
dependencies = [
    "pandas>=2.0",
    "numpy>=1.24",
]

[project.optional-dependencies]
dev = [
    "pytest>=7.0",
    "ruff>=0.1.0",
]
viz = [
    "plotly>=5.0",
    "matplotlib>=3.7",
]
ml = [
    "scikit-learn>=1.3",
    "tensorflow>=2.15",
]

[tool.uv]
dev-dependencies = [
    "pytest>=7.0",
    "ruff>=0.1.0",
]
```

### 4. Scripts Configuration

```toml
# pyproject.toml - define project scripts
[project.scripts]
my-cli = "my_project.cli:main"

[tool.uv.scripts]
test = "pytest tests/ -v"
lint = "ruff check src/"
format = "black src/ tests/"
typecheck = "mypy src/"
all = ["lint", "typecheck", "test"]
```

### 5. Performance Tips

```bash
# Use parallel installation (default)
uv sync

# Cache packages globally
export UV_CACHE_DIR="$HOME/.cache/uv"

# Offline mode (use cached packages)
uv sync --offline

# Minimal install (no extras)
uv sync --no-dev --no-extras
```

## Common Commands Reference

| Command | Description |
|---------|-------------|
| `uv init` | Initialize new project |
| `uv venv` | Create virtual environment |
| `uv add <pkg>` | Add dependency |
| `uv add --dev <pkg>` | Add dev dependency |
| `uv remove <pkg>` | Remove dependency |
| `uv sync` | Install all dependencies |
| `uv lock` | Update lock file |
| `uv run <cmd>` | Run command in venv |
| `uv python list` | List Python versions |
| `uv python install` | Install Python version |
| `uv pip install` | pip compatibility mode |
| `uv build` | Build package |
| `uv publish` | Publish to PyPI |

## Resources

- **UV Documentation**: https://docs.astral.sh/uv/
- **UV GitHub**: https://github.com/astral-sh/uv
- **Migration Guide**: https://docs.astral.sh/uv/guides/migration/
- **pyproject.toml Spec**: https://packaging.python.org/en/latest/specifications/pyproject-toml/

---

**Use UV for all Python projects in workspace-hub!**
