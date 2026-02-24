# GitBook Documentation Skill

> **Quick Reference Guide**

## Overview

Publish documentation and books with GitBook including spaces, collections, content variants (versions), Git synchronization, team collaboration, and API integration for professional documentation sites.

**Category**: Documentation
**Version**: 1.0.0
**Platforms**: Web, API, Git

## Quick Start

### 1. Get API Token

```bash
# Get token from: https://app.gitbook.com/account/developer
export GITBOOK_API_TOKEN="gb_api_xxxxxxxx"

# Verify
curl -s -H "Authorization: Bearer $GITBOOK_API_TOKEN" \
    "https://api.gitbook.com/v1/user" | jq
```

### 2. Repository Structure

```
docs-repo/
├── README.md           # Main page
├── SUMMARY.md          # Table of contents
├── getting-started/
│   ├── README.md
│   └── installation.md
└── .gitbook.yaml       # GitBook config
```

### 3. SUMMARY.md Example

```markdown
# Summary

## Getting Started

* [Introduction](README.md)
* [Installation](getting-started/installation.md)

## User Guide

* [Overview](guide/overview.md)
* [Advanced](guide/advanced.md)
```

## Key Features

| Feature | Description |
|---------|-------------|
| Spaces | Individual documentation sites |
| Collections | Group related spaces |
| Variants | Version documentation (v1, v2, etc.) |
| Git Sync | Sync with GitHub/GitLab |
| Custom Domains | docs.example.com |
| Team Collaboration | Roles and permissions |

## API Examples

### List Spaces

```bash
curl -s -H "Authorization: Bearer $GITBOOK_API_TOKEN" \
    "https://api.gitbook.com/v1/orgs/ORG_ID/spaces"
```

### Create Space

```bash
curl -s -X POST -H "Authorization: Bearer $GITBOOK_API_TOKEN" \
    -H "Content-Type: application/json" \
    "https://api.gitbook.com/v1/orgs/ORG_ID/spaces" \
    -d '{"title": "API Docs", "visibility": "public"}'
```

### Python Client

```python
import requests
import os

class GitBookClient:
    BASE_URL = "https://api.gitbook.com/v1"

    def __init__(self, token):
        self.headers = {"Authorization": f"Bearer {token}"}

    def list_spaces(self, org_id):
        response = requests.get(
            f"{self.BASE_URL}/orgs/{org_id}/spaces",
            headers=self.headers
        )
        return response.json()

# Usage
client = GitBookClient(os.environ["GITBOOK_API_TOKEN"])
spaces = client.list_spaces("org_xxxxx")
```

## Git Sync Configuration

```yaml
# .gitbook.yaml
root: ./docs/

structure:
  readme: README.md
  summary: SUMMARY.md

redirects:
  old-page: new-page.md
```

## Variants (Versions)

```bash
# Create version variant
curl -s -X POST -H "Authorization: Bearer $GITBOOK_API_TOKEN" \
    -H "Content-Type: application/json" \
    "https://api.gitbook.com/v1/spaces/SPACE_ID/variants" \
    -d '{"title": "v2.0", "slug": "v2"}'
```

## Files

```
gitbook/
  SKILL.md    # Full documentation (900+ lines)
  README.md   # This quick reference
```

## Related Skills

- **mkdocs** - Alternative static docs generator
- **docusaurus** - React-based documentation
- **sphinx** - Python documentation

## Resources

- [GitBook Docs](https://docs.gitbook.com/)
- [API Reference](https://developer.gitbook.com/)
- [Git Sync Guide](https://docs.gitbook.com/integrations/git-sync)

---

**See SKILL.md for complete API reference, migration tools, and integration examples.**
