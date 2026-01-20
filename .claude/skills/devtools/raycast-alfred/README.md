# Raycast & Alfred Skill

> **Quick Reference Guide**

## Overview

macOS launcher automation with Raycast extensions (TypeScript/React) and Alfred workflows (AppleScript/Python) for keyboard-driven productivity.

**Version**: 1.0.0
**Category**: devtools
**Platforms**: macos

## Quick Start

### Raycast Script Command

```bash
#!/bin/bash

# @raycast.schemaVersion 1
# @raycast.title Open Project
# @raycast.mode silent
# @raycast.icon folder
# @raycast.argument1 { "type": "text", "placeholder": "Project name" }

PROJECT="$1"
code "$HOME/projects/$PROJECT"
echo "Opened $PROJECT"
```

### Alfred Python Script Filter

```python
#!/usr/bin/env python3
import json
import sys

query = sys.argv[1] if len(sys.argv) > 1 else ""

items = [
    {"title": f"Search: {query}", "arg": query}
]

print(json.dumps({"items": items}))
```

## Key Capabilities

- **Raycast Extensions**: TypeScript/React components
- **Alfred Workflows**: AppleScript, Python, bash scripts
- **Script Commands**: Quick bash/Python automations
- **Keyboard Shortcuts**: Global hotkeys
- **Clipboard Management**: History, transformations
- **Snippet Expansion**: Text templates
- **File Search**: Spotlight alternatives
- **Window Management**: Positioning, sizing

## Common Patterns

### Raycast TypeScript Extension

```tsx
import { List, ActionPanel, Action } from "@raycast/api";

export default function Command() {
  return (
    <List searchBarPlaceholder="Search...">
      <List.Item
        title="Item"
        actions={
          <ActionPanel>
            <Action.OpenInBrowser url="https://example.com" />
          </ActionPanel>
        }
      />
    </List>
  );
}
```

### Alfred AppleScript

```applescript
on alfred_script(q)
    tell application "Finder"
        -- Do something with q
    end tell
    return "Done"
end alfred_script
```

## Files

```
raycast-alfred/
├── SKILL.md    # Full documentation (950+ lines)
└── README.md   # This quick reference
```

## Development Setup

### Raycast

```bash
npm install -g @raycast/api
npx create-raycast-extension --name my-extension
cd my-extension && npm run dev
```

### Alfred

```
Alfred Preferences > Workflows > + > Blank Workflow
```

## Related Skills

- **cli-productivity** - Shell automation
- **vscode-extensions** - Editor customization
- **git-advanced** - Version control

## Resources

- [Raycast Developers](https://developers.raycast.com/)
- [Alfred Workflows Help](https://www.alfredapp.com/help/workflows/)
- [AppleScript Guide](https://developer.apple.com/library/archive/documentation/AppleScript/)

---

**See SKILL.md for complete documentation with 6+ comprehensive examples.**
