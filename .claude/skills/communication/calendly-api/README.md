# Calendly API Skill

> **Quick Reference Guide**

## Overview

Calendly scheduling automation using REST API v2 for managing event types, availability, bookings, webhooks, and scheduling workflows.

**Version**: 1.0.0
**Category**: communication
**Platforms**: linux, macos, windows

## Quick Start

### 1. Install Dependencies

```bash
pip install requests python-dotenv
```

### 2. Set Environment Variables

```bash
export CALENDLY_API_KEY=your-personal-access-token
```

### 3. Basic Usage

```python
import os
import requests

API_KEY = os.environ["CALENDLY_API_KEY"]
BASE_URL = "https://api.calendly.com"

headers = {
    "Authorization": f"Bearer {API_KEY}",
    "Content-Type": "application/json",
}

# Get current user
response = requests.get(f"{BASE_URL}/users/me", headers=headers)
user = response.json()["resource"]
print(f"User: {user['name']}")

# List event types
response = requests.get(
    f"{BASE_URL}/event_types",
    headers=headers,
    params={"user": user["uri"]}
)
event_types = response.json()["collection"]
```

## Key Capabilities

- **Event Types**: List, configure scheduling options
- **Scheduled Events**: View, cancel meetings
- **Invitees**: Track attendee details, custom answers
- **Webhooks**: Real-time booking notifications
- **Scheduling Links**: Single-use, pre-filled URLs
- **Routing Forms**: Form-based scheduling

## Common Patterns

### List Upcoming Events

```python
from datetime import datetime, timedelta

params = {
    "user": user_uri,
    "min_start_time": datetime.utcnow().isoformat() + "Z",
    "max_start_time": (datetime.utcnow() + timedelta(days=7)).isoformat() + "Z",
    "status": "active",
}
response = requests.get(f"{BASE_URL}/scheduled_events", headers=headers, params=params)
```

### Create Webhook

```python
requests.post(f"{BASE_URL}/webhook_subscriptions", headers=headers, json={
    "url": "https://example.com/webhooks/calendly",
    "events": ["invitee.created", "invitee.canceled"],
    "organization": organization_uri,
    "scope": "organization"
})
```

## Files

```
calendly-api/
├── SKILL.md    # Full documentation (850+ lines)
└── README.md   # This quick reference
```

## Dependencies

- requests >= 2.31.0
- python-dotenv >= 1.0.0

## Related Skills

- **slack-api** - Booking notifications
- **teams-api** - Meeting integration
- **github-actions** - Event sync automation

## Resources

- [Calendly API Docs](https://developer.calendly.com/api-docs)
- [Developer Portal](https://developer.calendly.com/)
- [Webhooks Guide](https://developer.calendly.com/api-docs/4e4e9a77ef8bc-webhooks)

---

**See SKILL.md for complete documentation with 6+ comprehensive examples.**
