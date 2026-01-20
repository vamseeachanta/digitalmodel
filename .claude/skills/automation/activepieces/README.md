# Activepieces Workflow Automation Skill

> **Quick Reference Guide**

## Overview

Self-hosted no-code automation platform with visual flow builder, type-safe custom pieces, API integrations, and event-driven triggers.

**Version**: 1.0.0
**Category**: automation
**Platforms**: linux, macos, windows, docker, kubernetes

## Quick Start

### 1. Docker Compose Setup (Recommended)

```bash
# Generate secrets
export POSTGRES_PASSWORD=$(openssl rand -hex 32)
export AP_ENCRYPTION_KEY=$(openssl rand -hex 16)
export AP_JWT_SECRET=$(openssl rand -hex 32)

# Start services
docker compose up -d

# Access UI at http://localhost:8080
```

### 2. Basic Flow Structure

```typescript
{
  "displayName": "My Automation",
  "trigger": {
    "type": "WEBHOOK",
    "settings": {}
  },
  "steps": [
    {
      "name": "process_data",
      "type": "CODE",
      "settings": {
        "sourceCode": {
          "code": "export const code = async (inputs) => { return { processed: true }; };"
        }
      }
    }
  ]
}
```

### 3. Custom Piece CLI

```bash
# Install CLI
npm install -g @activepieces/cli

# Create new piece
ap create-piece my-custom-piece

# Build and test
npm run build
npm run test
```

## Key Capabilities

- **Visual Flow Builder**: Drag-and-drop flow design
- **Type-Safe Pieces**: TypeScript-based custom components
- **Branching Logic**: Conditional routing with expressions
- **Loops/Iterations**: Process arrays and batch data
- **Approval Flows**: Human-in-the-loop workflows
- **Webhook Triggers**: Real-time event processing
- **Scheduled Runs**: Cron-based execution
- **Connection Management**: Secure credential storage

## Common Patterns

### Webhook Trigger
```typescript
{
  "trigger": {
    "type": "WEBHOOK",
    "settings": {
      "httpMethod": "POST",
      "path": "my-webhook"
    }
  }
}
```

### Scheduled Trigger
```typescript
{
  "schedule": {
    "cronExpression": "0 9 * * 1-5",
    "timezone": "America/New_York"
  }
}
```

### Branching
```typescript
{
  "type": "BRANCH",
  "settings": {
    "conditions": [
      {
        "name": "high_priority",
        "expression": { "value": "{{input.priority}} === 'high'" }
      }
    ]
  }
}
```

### Loop Over Items
```typescript
{
  "type": "LOOP_ON_ITEMS",
  "settings": {
    "items": "{{trigger.body.orders}}",
    "steps": [/* process each item */]
  }
}
```

## Files

```
activepieces/
├── SKILL.md    # Full documentation (900+ lines)
└── README.md   # This quick reference
```

## Dependencies

- Docker (recommended)
- PostgreSQL
- Redis (optional, for queue mode)
- Node.js 18+ (for custom pieces)

## Environment Variables

```bash
AP_ENCRYPTION_KEY="32-char-encryption-key"
AP_JWT_SECRET="secure-jwt-secret"
AP_FRONTEND_URL=https://activepieces.example.com
AP_WEBHOOK_TIMEOUT_SECONDS=30
AP_SANDBOX_RUN_TIME_SECONDS=600
```

## Related Skills

- **n8n** - Alternative visual automation platform
- **windmill** - Script-based workflow engine
- **api-integration** - REST API patterns
- **yaml-configuration** - Configuration management

## Resources

- [Activepieces Documentation](https://www.activepieces.com/docs)
- [Pieces Directory](https://www.activepieces.com/pieces)
- [GitHub Repository](https://github.com/activepieces/activepieces)
- [Community Discord](https://discord.gg/activepieces)

---

**See SKILL.md for complete documentation with 8+ comprehensive examples.**
