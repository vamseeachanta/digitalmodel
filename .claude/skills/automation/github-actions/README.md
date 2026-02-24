# GitHub Actions Skill

> **Quick Reference Guide**

## Overview

CI/CD automation and workflow orchestration using GitHub Actions for builds, tests, deployments, and repository automation.

**Version**: 1.0.0
**Category**: automation
**Platforms**: github, linux, macos, windows

## Quick Start

### 1. Create Workflow Directory

```bash
mkdir -p .github/workflows
```

### 2. Basic CI Workflow

```yaml
# .github/workflows/ci.yml
name: CI

on:
  push:
    branches: [main]
  pull_request:

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.11'
          cache: 'pip'

      - name: Install dependencies
        run: pip install -e ".[dev]"

      - name: Run tests
        run: pytest tests/ -v --cov=src
```

### 3. Local Testing with act

```bash
# Install act
brew install act  # macOS

# Test workflow locally
act push -l          # List jobs
act push             # Run push trigger
act -j test          # Run specific job
```

## Key Capabilities

- **Workflow Triggers**: push, pull_request, schedule, workflow_dispatch
- **Matrix Builds**: Test across OS/language versions
- **Caching**: pip, npm, Docker layers
- **Artifacts**: Upload/download build outputs
- **Reusable Workflows**: Share common CI patterns
- **Composite Actions**: Create custom actions
- **Secrets Management**: Environment-specific secrets
- **Container Builds**: Multi-arch Docker images

## Common Patterns

### Matrix Testing
```yaml
strategy:
  matrix:
    os: [ubuntu-latest, macos-latest]
    python: ['3.10', '3.11', '3.12']
```

### Caching
```yaml
- uses: actions/setup-python@v5
  with:
    python-version: '3.11'
    cache: 'pip'
```

### Artifacts
```yaml
- uses: actions/upload-artifact@v4
  with:
    name: dist
    path: dist/
```

### Reusable Workflow
```yaml
jobs:
  test:
    uses: ./.github/workflows/reusable-ci.yml
    with:
      python-version: '3.11'
```

## Files

```
github-actions/
├── SKILL.md    # Full documentation (700+ lines)
└── README.md   # This quick reference
```

## Dependencies

- GitHub repository
- Optional: act (local testing)
- Optional: actionlint (linting)

## Related Skills

- **yaml-configuration** - YAML syntax and patterns
- **bash-cli-framework** - Shell scripting in workflows
- **git-sync-manager** - Git operations automation

## Resources

- [GitHub Actions Docs](https://docs.github.com/en/actions)
- [Workflow Syntax](https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions)
- [Actions Marketplace](https://github.com/marketplace?type=actions)

---

**See SKILL.md for complete documentation with 10+ comprehensive examples.**
