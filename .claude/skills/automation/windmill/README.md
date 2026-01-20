# Windmill Workflow Automation Skill

> **Quick Reference Guide**

## Overview

Developer-first workflow engine that turns scripts into workflows and UIs, supporting Python, TypeScript, Go, and Bash with approval flows, schedule management, and self-hosted deployment.

**Version**: 1.0.0
**Category**: automation
**Platforms**: linux, macos, windows, docker, kubernetes

## Quick Start

### 1. Docker Compose Setup (Recommended)

```bash
# Generate password
export POSTGRES_PASSWORD=$(openssl rand -hex 32)

# Start services
docker compose up -d

# Access UI at http://localhost:8000
# Default: admin@windmill.dev / changeme
```

### 2. Basic Python Script

```python
# scripts/fetch_data.py
import wmill

def main(
    api_url: str,
    limit: int = 100,
    include_metadata: bool = True,
):
    """
    Fetch data from API.
    Auto-generates UI for parameters.
    """
    api_key = wmill.get_resource("u/admin/api_key")

    import requests
    response = requests.get(
        api_url,
        headers={"Authorization": f"Bearer {api_key['key']}"},
        params={"limit": limit}
    )

    return response.json()
```

### 3. Windmill CLI

```bash
# Install CLI
npm install -g windmill-cli

# Login
wmill workspace add dev https://windmill.example.com

# Run script
wmill script run f/data/fetch_data \
  --data '{"api_url": "https://api.example.com"}'
```

## Key Capabilities

- **Scripts to Workflows**: Write code, get workflows
- **Auto-Generated UIs**: Parameter forms from type hints
- **Multi-Language**: Python, TypeScript, Go, Bash
- **Approval Flows**: Human-in-the-loop with audit trails
- **Schedule Management**: Cron-based with timezone support
- **Flow Orchestration**: Chain scripts with branching
- **Resource Management**: Secure credentials and configs
- **Self-Hosted**: Full control over data and execution

## Common Patterns

### Python Script with Resources
```python
import wmill

def main(data: dict):
    db = wmill.get_resource("u/admin/postgres_db")
    # Use credentials from resource
    return process(data, db)
```

### TypeScript/Deno Script
```typescript
import * as wmill from "npm:windmill-client@1";

export async function main(input: string): Promise<string> {
  const apiKey = await wmill.getResource("u/admin/api_key");
  return `Processed: ${input}`;
}
```

### Bash Script
```bash
#!/bin/bash
# Get resource via API
API_KEY=$(curl -s -H "Authorization: Bearer $WM_TOKEN" \
  "$BASE_INTERNAL_URL/api/w/$WM_WORKSPACE/resources/get/u/admin/api_key" \
  | jq -r '.value.key')

curl -H "Authorization: Bearer $API_KEY" https://api.example.com/data
```

### Flow Definition
```yaml
# flows/my_flow.yaml
summary: Data Pipeline
value:
  modules:
    - id: fetch
      value:
        type: script
        path: f/data/fetch_data
    - id: transform
      value:
        type: script
        path: f/data/transform
        input_transforms:
          data:
            type: javascript
            expr: results.fetch
```

## Files

```
windmill/
├── SKILL.md    # Full documentation (950+ lines)
└── README.md   # This quick reference
```

## Dependencies

- Docker (recommended)
- PostgreSQL
- Python 3.11+ / Deno / Go 1.21+ (based on scripts)

## Environment Variables

```bash
DATABASE_URL=postgres://windmill:pass@postgres:5432/windmill
BASE_URL=https://windmill.example.com
MODE=standalone
NUM_WORKERS=4
```

## CLI Commands

```bash
# Sync scripts
wmill sync push

# Run script
wmill script run f/path/to/script

# List resources
wmill resource list

# View job logs
wmill job get <job_id>
```

## Related Skills

- **n8n** - Visual workflow automation
- **activepieces** - No-code automation
- **airflow** - DAG-based orchestration
- **yaml-configuration** - Configuration patterns

## Resources

- [Windmill Documentation](https://www.windmill.dev/docs)
- [Script Hub](https://hub.windmill.dev/)
- [GitHub Repository](https://github.com/windmill-labs/windmill)
- [Discord Community](https://discord.gg/windmill)

---

**See SKILL.md for complete documentation with 8+ comprehensive examples.**
