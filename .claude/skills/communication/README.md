# Communication Skills Library

> Integrate team communication, collaboration tools, and scheduling workflows
> Version: 1.0.0 | Last Updated: 2026-01-17

## Overview

This library contains 4 communication-focused skills for building automated workflows across team collaboration platforms. Each skill provides API patterns, webhook integrations, and best practices for streamlining team communication and scheduling.

## Quick Start

```bash
# Browse available skills
ls skills/communication/

# Read a skill
cat skills/communication/slack-api/SKILL.md

# Skills are documentation - integrate patterns into your automation
```

## Available Skills

| Skill | Description | Key Features |
|-------|-------------|--------------|
| [slack-api](./slack-api/SKILL.md) | Slack workspace automation | Messages, channels, apps, webhooks, Block Kit |
| [teams-api](./teams-api/SKILL.md) | Microsoft Teams integration | Chats, channels, meetings, Graph API |
| [miro-api](./miro-api/SKILL.md) | Visual collaboration boards | Boards, widgets, frames, real-time sync |
| [calendly-api](./calendly-api/SKILL.md) | Scheduling automation | Events, invitees, webhooks, availability |

## Skill Categories

### Messaging Platforms
- **slack-api** - Chat, channels, and app development
- **teams-api** - Microsoft ecosystem integration

### Visual Collaboration
- **miro-api** - Whiteboard and diagramming

### Scheduling
- **calendly-api** - Meeting scheduling automation

## Skill Selection Guide

| Use Case | Recommended Skill | Why |
|----------|-------------------|-----|
| Build notifications | slack-api | Webhooks, rich formatting, threading |
| DevOps alerts | slack-api | Incoming webhooks, action buttons |
| Enterprise messaging | teams-api | Microsoft 365 integration, compliance |
| Meeting scheduling | calendly-api | Availability, booking links, reminders |
| Team brainstorming | miro-api | Visual boards, templates, real-time |
| Workflow automation | slack-api | Workflow Builder, Bolt framework |
| Sprint retrospectives | miro-api | Templates, voting, sticky notes |
| Interview scheduling | calendly-api | Round-robin, team scheduling |

## Common Patterns Across Skills

### Webhook Setup
```bash
# Common webhook receiver pattern
#!/bin/bash

start_webhook_server() {
    local port="${1:-8080}"

    while true; do
        echo -e "HTTP/1.1 200 OK\n\n" | nc -l -p "$port" | while read line; do
            # Parse webhook payload
            if [[ "$line" == *"{"* ]]; then
                echo "$line" | jq '.' >> webhooks.log
            fi
        done
    done
}
```

### OAuth2 Authentication
```bash
# OAuth2 token exchange
get_access_token() {
    local client_id="$1"
    local client_secret="$2"
    local code="$3"

    curl -s -X POST "https://oauth.example.com/token" \
        -d "client_id=$client_id" \
        -d "client_secret=$client_secret" \
        -d "code=$code" \
        -d "grant_type=authorization_code"
}

# Refresh token
refresh_access_token() {
    local refresh_token="$1"

    curl -s -X POST "https://oauth.example.com/token" \
        -d "refresh_token=$refresh_token" \
        -d "grant_type=refresh_token"
}
```

### Rate Limiting
```bash
# Tier-based rate limiting
TIER1_LIMIT=1      # 1 request per second
TIER2_LIMIT=20     # 20 requests per minute
TIER3_LIMIT=50     # 50 requests per minute

rate_limited_request() {
    local tier="$1"
    shift

    case "$tier" in
        1) sleep 1 ;;
        2) sleep 3 ;;
        3) sleep 1.2 ;;
    esac

    make_request "$@"
}
```

### Error Response Handling
```bash
handle_api_response() {
    local response="$1"
    local status=$(echo "$response" | jq -r '.ok // .status // "unknown"')

    case "$status" in
        "true"|"200")
            echo "$response" | jq '.data // .'
            return 0
            ;;
        "false")
            echo "Error: $(echo "$response" | jq -r '.error')" >&2
            return 1
            ;;
        *)
            echo "Unknown response: $response" >&2
            return 1
            ;;
    esac
}
```

## Usage Examples

### Slack Message Posting
```bash
# See slack-api for complete patterns

# Simple webhook message
send_slack_webhook() {
    local webhook_url="$SLACK_WEBHOOK_URL"
    local message="$1"

    curl -s -X POST "$webhook_url" \
        -H "Content-Type: application/json" \
        -d "{\"text\": \"$message\"}"
}

# Rich Block Kit message
send_slack_blocks() {
    local channel="$1"
    local blocks="$2"

    curl -s -X POST "https://slack.com/api/chat.postMessage" \
        -H "Authorization: Bearer $SLACK_BOT_TOKEN" \
        -H "Content-Type: application/json" \
        -d "{
            \"channel\": \"$channel\",
            \"blocks\": $blocks
        }"
}

# Example Block Kit payload
blocks='[
    {
        "type": "header",
        "text": {"type": "plain_text", "text": "Deployment Complete"}
    },
    {
        "type": "section",
        "fields": [
            {"type": "mrkdwn", "text": "*Environment:*\nProduction"},
            {"type": "mrkdwn", "text": "*Version:*\nv2.1.0"}
        ]
    },
    {
        "type": "actions",
        "elements": [
            {"type": "button", "text": {"type": "plain_text", "text": "View Logs"}, "url": "https://logs.example.com"}
        ]
    }
]'

send_slack_blocks "#deployments" "$blocks"
```

### Teams Message via Graph API
```bash
# See teams-api for complete patterns

# Send channel message
send_teams_message() {
    local team_id="$1"
    local channel_id="$2"
    local message="$3"

    curl -s -X POST "https://graph.microsoft.com/v1.0/teams/$team_id/channels/$channel_id/messages" \
        -H "Authorization: Bearer $TEAMS_ACCESS_TOKEN" \
        -H "Content-Type: application/json" \
        -d "{
            \"body\": {
                \"contentType\": \"html\",
                \"content\": \"$message\"
            }
        }"
}

# Send adaptive card
send_teams_card() {
    local webhook_url="$1"
    local card="$2"

    curl -s -X POST "$webhook_url" \
        -H "Content-Type: application/json" \
        -d "{
            \"type\": \"message\",
            \"attachments\": [{
                \"contentType\": \"application/vnd.microsoft.card.adaptive\",
                \"content\": $card
            }]
        }"
}
```

### Miro Board Operations
```bash
# See miro-api for complete patterns

# Create sticky note
create_sticky() {
    local board_id="$1"
    local content="$2"
    local x="${3:-0}"
    local y="${4:-0}"

    curl -s -X POST "https://api.miro.com/v2/boards/$board_id/sticky_notes" \
        -H "Authorization: Bearer $MIRO_ACCESS_TOKEN" \
        -H "Content-Type: application/json" \
        -d "{
            \"data\": {\"content\": \"$content\"},
            \"position\": {\"x\": $x, \"y\": $y}
        }"
}

# Create frame
create_frame() {
    local board_id="$1"
    local title="$2"

    curl -s -X POST "https://api.miro.com/v2/boards/$board_id/frames" \
        -H "Authorization: Bearer $MIRO_ACCESS_TOKEN" \
        -H "Content-Type: application/json" \
        -d "{
            \"data\": {\"title\": \"$title\"},
            \"geometry\": {\"width\": 800, \"height\": 600}
        }"
}
```

### Calendly Event Management
```bash
# See calendly-api for complete patterns

# List scheduled events
list_events() {
    local user_uri="$1"
    local min_time=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    curl -s "https://api.calendly.com/scheduled_events?user=$user_uri&min_start_time=$min_time" \
        -H "Authorization: Bearer $CALENDLY_API_KEY"
}

# Get event details
get_event() {
    local event_uuid="$1"

    curl -s "https://api.calendly.com/scheduled_events/$event_uuid" \
        -H "Authorization: Bearer $CALENDLY_API_KEY"
}

# Create webhook subscription
create_webhook() {
    local organization="$1"
    local callback_url="$2"

    curl -s -X POST "https://api.calendly.com/webhook_subscriptions" \
        -H "Authorization: Bearer $CALENDLY_API_KEY" \
        -H "Content-Type: application/json" \
        -d "{
            \"url\": \"$callback_url\",
            \"events\": [\"invitee.created\", \"invitee.canceled\"],
            \"organization\": \"$organization\",
            \"scope\": \"organization\"
        }"
}
```

## Integration with Workspace-Hub

These skills enable team communication automation:

```
workspace-hub/
├── scripts/
│   ├── notify-deploy.sh         # Uses: slack-api, teams-api
│   ├── schedule-review.sh       # Uses: calendly-api
│   └── retro-board.sh           # Uses: miro-api
├── automation/
│   ├── slack-bot/               # Uses: slack-api
│   ├── teams-connector/         # Uses: teams-api
│   └── webhooks/
│       ├── slack-handler.sh
│       └── calendly-handler.sh
└── config/
    └── communication.conf       # API tokens and endpoints
```

## Best Practices

### 1. Secure Token Storage
```bash
# Use environment variables or secret managers
export SLACK_BOT_TOKEN=$(vault kv get -field=token secret/slack)

# Rotate tokens regularly
rotate_token() {
    local new_token=$(generate_new_token)
    vault kv put secret/slack token="$new_token"
}
```

### 2. Message Formatting
```bash
# Escape special characters for Slack
escape_slack() {
    local text="$1"
    text="${text//&/&amp;}"
    text="${text//</&lt;}"
    text="${text//>/&gt;}"
    echo "$text"
}

# Format mentions
format_mention() {
    local user_id="$1"
    echo "<@$user_id>"
}

# Format channel link
format_channel() {
    local channel_id="$1"
    echo "<#$channel_id>"
}
```

### 3. Idempotent Operations
```bash
# Prevent duplicate messages
SENT_MESSAGES_FILE="/tmp/sent_messages.txt"

send_once() {
    local message_hash=$(echo "$1" | md5sum | cut -d' ' -f1)

    if grep -q "$message_hash" "$SENT_MESSAGES_FILE" 2>/dev/null; then
        echo "Message already sent" >&2
        return 0
    fi

    if send_message "$1"; then
        echo "$message_hash" >> "$SENT_MESSAGES_FILE"
    fi
}
```

### 4. Webhook Verification
```bash
# Verify Slack signature
verify_slack_signature() {
    local timestamp="$1"
    local body="$2"
    local signature="$3"

    local base="v0:$timestamp:$body"
    local computed=$(echo -n "$base" | openssl dgst -sha256 -hmac "$SLACK_SIGNING_SECRET" | sed 's/.*= //')

    [ "v0=$computed" = "$signature" ]
}
```

### 5. Graceful Degradation
```bash
# Fallback when primary channel fails
notify() {
    local message="$1"

    if ! send_slack_message "$message"; then
        echo "Slack failed, trying Teams..." >&2
        if ! send_teams_message "$message"; then
            echo "Teams failed, sending email..." >&2
            send_email "$message"
        fi
    fi
}
```

## Testing Communication Skills

Validate integrations without spamming channels:

```bash
#!/bin/bash
# test_communication.sh

# Test Slack connection
test_slack_auth() {
    response=$(curl -s "https://slack.com/api/auth.test" \
        -H "Authorization: Bearer $SLACK_BOT_TOKEN")

    ok=$(echo "$response" | jq -r '.ok')
    [ "$ok" = "true" ] && echo "PASS: Slack auth" || echo "FAIL: Slack auth - $(echo "$response" | jq -r '.error')"
}

# Test Teams connection
test_teams_auth() {
    response=$(curl -s "https://graph.microsoft.com/v1.0/me" \
        -H "Authorization: Bearer $TEAMS_ACCESS_TOKEN" \
        -o /dev/null -w "%{http_code}")

    [ "$response" = "200" ] && echo "PASS: Teams auth" || echo "FAIL: Teams auth - HTTP $response"
}

# Test Calendly connection
test_calendly_auth() {
    response=$(curl -s "https://api.calendly.com/users/me" \
        -H "Authorization: Bearer $CALENDLY_API_KEY" \
        -o /dev/null -w "%{http_code}")

    [ "$response" = "200" ] && echo "PASS: Calendly auth" || echo "FAIL: Calendly auth - HTTP $response"
}

# Test Miro connection
test_miro_auth() {
    response=$(curl -s "https://api.miro.com/v1/users/me" \
        -H "Authorization: Bearer $MIRO_ACCESS_TOKEN" \
        -o /dev/null -w "%{http_code}")

    [ "$response" = "200" ] && echo "PASS: Miro auth" || echo "FAIL: Miro auth - HTTP $response"
}

# Run all tests
test_slack_auth
test_teams_auth
test_calendly_auth
test_miro_auth
```

## Contributing

When adding new skills:

1. **Document OAuth flows** - Authentication is often complex
2. **Include webhook handling** - Event-driven patterns
3. **Show rate limiting** - API throttling strategies
4. **Add formatting examples** - Rich message construction
5. **Update this README** - Add to the skills table

## Related Resources

- [Slack API Documentation](https://api.slack.com/)
- [Microsoft Graph API](https://docs.microsoft.com/graph/)
- [Miro Developer Platform](https://developers.miro.com/)
- [Calendly API Reference](https://developer.calendly.com/)
- [Block Kit Builder](https://app.slack.com/block-kit-builder/)
- [Adaptive Cards Designer](https://adaptivecards.io/designer/)

## Version History

- **1.0.0** (2026-01-17): Initial release with 4 communication skills

---

*These skills enable automated team communication, reducing manual coordination and ensuring timely notifications across development workflows.*
