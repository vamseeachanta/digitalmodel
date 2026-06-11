# Agent OS and .ai Directory Integration

## Overview

This project uses both Agent OS (`.agent-os/`) and a custom AI directory (`.ai/`) structure. They serve complementary purposes:

- **`.agent-os/`** - Follows Agent OS standards for AI agent guidance
- **`.ai/`** - Contains project-specific specs and configurations

## Directory Mapping

### Agent OS Structure → Existing .ai Content

```
.agent-os/
├── product/
│   ├── overview.md         → General product description
│   ├── architecture.md     → System design overview  
│   ├── stack.md           → Technology choices
│   └── roadmap.md         → Future plans
│
├── standards/
│   ├── code-style.md      → Coding conventions
│   ├── testing.md         → Test requirements
│   └── git.md             → Version control workflow
│
└── projects/              → New feature specs
    └── YYYY-MM-DD-name/   

.ai/
├── specs/                 → Existing specifications
│   └── modules/           → Module-specific docs
│       ├── parallel_processing_opp.md
│       └── ...
│
└── CLAUDE.md             → AI assistant instructions
```

## Usage Guidelines

### For New Features
1. Create spec in `.agent-os/projects/YYYY-MM-DD-feature-name/`
2. Follow Agent OS format for specifications
3. Reference existing code patterns from `.ai/specs/`

### For Existing Modules
1. Keep current documentation in `.ai/specs/modules/`
2. Reference from Agent OS docs when needed
3. Gradually migrate to Agent OS format

### For AI Agents
When working with AI coding assistants:
1. Point them to `.agent-os/` for project understanding
2. Use `.ai/specs/` for detailed module documentation
3. Both directories work together

## Migration Strategy

### Phase 1 (Current)
- Set up basic Agent OS structure
- Keep existing .ai directory intact
- Document integration approach

### Phase 2 (Future)
- Gradually move specs to Agent OS format
- Maintain backwards compatibility
- Update references as needed

### Phase 3 (Long-term)
- Fully adopt Agent OS standards
- Archive legacy formats
- Maintain single source of truth

## Best Practices

1. **Don't Duplicate** - Avoid copying content between directories
2. **Cross-Reference** - Link between related docs
3. **Stay Consistent** - Use Agent OS format for new content
4. **Preserve History** - Keep existing docs until migrated

## Quick Reference

- New features → `.agent-os/projects/`
- Product docs → `.agent-os/product/`
- Standards → `.agent-os/standards/`
- Module specs → `.ai/specs/modules/`
- AI config → `.ai/CLAUDE.md`