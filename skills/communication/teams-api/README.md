# Microsoft Teams API Skill

> **Quick Reference Guide**

## Overview

Microsoft Teams automation using Graph API, Bot Framework, Adaptive Cards, and webhooks for enterprise messaging and collaboration.

**Version**: 1.0.0
**Category**: communication
**Platforms**: linux, macos, windows, azure

## Quick Start

### 1. Install Dependencies

```bash
pip install azure-identity msgraph-sdk botbuilder-core aiohttp
```

### 2. Set Environment Variables

```bash
export AZURE_TENANT_ID=your-tenant-id
export AZURE_CLIENT_ID=your-client-id
export AZURE_CLIENT_SECRET=your-client-secret
export MICROSOFT_APP_ID=your-bot-app-id
export MICROSOFT_APP_PASSWORD=your-bot-password
```

### 3. Send Webhook Message

```python
import requests

webhook_url = "https://outlook.office.com/webhook/..."

payload = {
    "@type": "MessageCard",
    "themeColor": "0076D7",
    "title": "Notification",
    "text": "Hello from Python!"
}

requests.post(webhook_url, json=payload)
```

### 4. Graph API Client

```python
from azure.identity import ClientSecretCredential
from msgraph import GraphServiceClient

credential = ClientSecretCredential(
    tenant_id="...",
    client_id="...",
    client_secret="..."
)

client = GraphServiceClient(
    credentials=credential,
    scopes=["https://graph.microsoft.com/.default"]
)
```

## Key Capabilities

- **Graph API**: Send messages, manage teams, channels, meetings
- **Bot Framework**: Build interactive Teams bots
- **Adaptive Cards**: Rich, interactive message layouts
- **Webhooks**: Simple incoming notifications
- **Proactive Messaging**: Notify users without user initiation
- **Meeting Automation**: Schedule and manage meetings

## Common Patterns

### Incoming Webhook

```python
webhook = TeamsWebhook("https://outlook.office.com/webhook/...")

webhook.send_alert(
    title="High CPU",
    message="Server usage at 92%",
    severity="warning"
)
```

### Adaptive Card

```python
card = {
    "type": "AdaptiveCard",
    "version": "1.4",
    "body": [
        {"type": "TextBlock", "text": "Alert", "size": "large"}
    ],
    "actions": [
        {"type": "Action.OpenUrl", "title": "View", "url": "..."}
    ]
}
```

### Bot Message Handler

```python
from botbuilder.core import ActivityHandler

class MyBot(ActivityHandler):
    async def on_message_activity(self, turn_context):
        await turn_context.send_activity("Hello!")
```

## Files

```
teams-api/
├── SKILL.md    # Full documentation (900+ lines)
└── README.md   # This quick reference
```

## Dependencies

- azure-identity >= 1.14.0
- msgraph-sdk >= 1.0.0
- botbuilder-core >= 4.14.0
- aiohttp >= 3.9.0

## Related Skills

- **slack-api** - Slack integration
- **github-actions** - CI/CD notifications
- **azure-functions** - Serverless bot hosting

## Resources

- [Microsoft Graph API](https://docs.microsoft.com/graph/)
- [Bot Framework SDK](https://docs.microsoft.com/azure/bot-service/)
- [Adaptive Cards Designer](https://adaptivecards.io/designer/)

---

**See SKILL.md for complete documentation with 6+ comprehensive examples.**
