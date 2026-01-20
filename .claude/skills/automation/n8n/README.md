# n8n Workflow Automation Skill

> **Quick Reference Guide**

## Overview

Open-source workflow automation platform with visual node-based editor, 400+ integrations, webhooks, and self-hosted deployment capabilities.

**Version**: 1.0.0
**Category**: automation
**Platforms**: linux, macos, windows, docker, kubernetes

## Quick Start

### 1. Docker Setup (Recommended)

```bash
# Quick start with Docker
docker run -it --rm \
  --name n8n \
  -p 5678:5678 \
  -v ~/.n8n:/home/node/.n8n \
  n8nio/n8n

# Access UI at http://localhost:5678
```

### 2. Basic Webhook Workflow

```json
{
  "nodes": [
    {
      "name": "Webhook",
      "type": "n8n-nodes-base.webhook",
      "parameters": {
        "path": "incoming-data",
        "httpMethod": "POST"
      }
    },
    {
      "name": "Transform",
      "type": "n8n-nodes-base.code",
      "parameters": {
        "jsCode": "return { processed: $input.item.json.data };"
      }
    }
  ]
}
```

### 3. npm Installation

```bash
# Install globally
npm install n8n -g

# Start n8n
n8n start
```

## Key Capabilities

- **Visual Workflow Builder**: Drag-and-drop node-based editor
- **400+ Integrations**: Slack, Gmail, Notion, Airtable, databases, APIs
- **Webhook Triggers**: Real-time event processing
- **Scheduled Execution**: Cron-based workflow scheduling
- **Code Node**: JavaScript/Python for custom logic
- **Credentials Management**: Secure, encrypted credential storage
- **Subworkflows**: Modular workflow composition
- **Error Handling**: Built-in retry and error routing

## Common Patterns

### Webhook Handler
```json
{
  "parameters": {
    "httpMethod": "POST",
    "path": "my-webhook",
    "responseMode": "responseNode"
  },
  "type": "n8n-nodes-base.webhook"
}
```

### Scheduled Trigger
```json
{
  "parameters": {
    "rule": {
      "interval": [{ "field": "cronExpression", "expression": "0 9 * * *" }]
    }
  },
  "type": "n8n-nodes-base.scheduleTrigger"
}
```

### Data Transformation
```javascript
// Code node
const items = $input.all();
return items.map(item => ({
  ...item.json,
  processed: true,
  timestamp: new Date().toISOString()
}));
```

### Conditional Branching
```json
{
  "type": "n8n-nodes-base.if",
  "parameters": {
    "conditions": {
      "number": [{ "value1": "={{ $json.amount }}", "operation": "larger", "value2": 1000 }]
    }
  }
}
```

## Files

```
n8n/
├── SKILL.md    # Full documentation (900+ lines)
└── README.md   # This quick reference
```

## Dependencies

- Docker (recommended) or Node.js 18+
- PostgreSQL (production)
- Optional: Redis (queue mode)

## Environment Variables

```bash
N8N_ENCRYPTION_KEY="32-char-encryption-key"
N8N_BASIC_AUTH_ACTIVE=true
N8N_BASIC_AUTH_USER=admin
WEBHOOK_URL=https://n8n.example.com/
DB_TYPE=postgresdb
```

## Related Skills

- **api-integration** - REST API patterns
- **activepieces** - Alternative automation platform
- **windmill** - Script-based workflows
- **github-actions** - CI/CD automation

## Resources

- [n8n Documentation](https://docs.n8n.io/)
- [Workflow Templates](https://n8n.io/workflows/)
- [Community Forum](https://community.n8n.io/)
- [GitHub Repository](https://github.com/n8n-io/n8n)

---

**See SKILL.md for complete documentation with 8+ comprehensive examples.**
