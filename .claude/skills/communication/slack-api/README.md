# Slack API Skill

> **Quick Reference Guide**

## Overview

Slack bot development and workspace automation using Web API, Events API, Socket Mode, and Block Kit for building interactive messaging applications.

**Version**: 1.0.0
**Category**: communication
**Platforms**: linux, macos, windows

## Quick Start

### 1. Install Dependencies

```bash
pip install slack-bolt slack-sdk python-dotenv
```

### 2. Set Environment Variables

```bash
export SLACK_BOT_TOKEN=xoxb-your-bot-token
export SLACK_SIGNING_SECRET=your-signing-secret
export SLACK_APP_TOKEN=xapp-your-app-token  # For Socket Mode
```

### 3. Basic Bot

```python
from slack_bolt import App
from slack_bolt.adapter.socket_mode import SocketModeHandler
import os

app = App(token=os.environ["SLACK_BOT_TOKEN"])

@app.message("hello")
def say_hello(message, say):
    say(f"Hi <@{message['user']}>!")

@app.command("/greet")
def handle_greet(ack, say, command):
    ack()
    say(f"Hello from {command['user_id']}!")

if __name__ == "__main__":
    handler = SocketModeHandler(app, os.environ["SLACK_APP_TOKEN"])
    handler.start()
```

## Key Capabilities

- **Web API**: Send messages, manage channels, users, files
- **Events API**: React to messages, mentions, channel events
- **Socket Mode**: Real-time without public URL
- **Block Kit**: Rich, interactive message layouts
- **Slash Commands**: Custom `/command` integrations
- **Modals**: Multi-step form dialogs
- **Webhooks**: Simple incoming notifications

## Common Patterns

### Send Block Kit Message

```python
client.chat_postMessage(
    channel="#general",
    blocks=[
        {"type": "header", "text": {"type": "plain_text", "text": "Alert"}},
        {"type": "section", "text": {"type": "mrkdwn", "text": "*Details:* Something happened"}}
    ],
    text="Alert notification"
)
```

### Interactive Button

```python
@app.action("approve_button")
def handle_approval(ack, body, client):
    ack()
    client.chat_update(
        channel=body['channel']['id'],
        ts=body['message']['ts'],
        text="Approved!"
    )
```

### Incoming Webhook

```bash
curl -X POST -H "Content-Type: application/json" \
  -d '{"text": "Deployment complete!"}' \
  $SLACK_WEBHOOK_URL
```

## Files

```
slack-api/
├── SKILL.md    # Full documentation (850+ lines)
└── README.md   # This quick reference
```

## Dependencies

- slack-bolt >= 1.18.0
- slack-sdk >= 3.21.0
- python-dotenv >= 1.0.0
- ngrok (for local development)

## Related Skills

- **teams-api** - Microsoft Teams integration
- **github-actions** - CI/CD notifications
- **python-scientific-computing** - Python best practices

## Resources

- [Slack API Docs](https://api.slack.com/)
- [Bolt for Python](https://slack.dev/bolt-python/)
- [Block Kit Builder](https://app.slack.com/block-kit-builder/)

---

**See SKILL.md for complete documentation with 6+ comprehensive examples.**
