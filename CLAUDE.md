# CLAUDE.md

This file provides guidance to AI assistants when working with code in this repository.

## Primary Guidance Sources

**Start with Agent OS structure:**
1. **`.agent-os/`** - Primary guidance for all AI agents
   - Read `.agent-os/README.md` first for complete overview
   - Follow `.agent-os/standards/ai-communication.md` for communication requirements
   - Apply `.agent-os/standards/code-style.md` and `.agent-os/standards/testing.md`
   - Use appropriate persona from `.agent-os/standards/agent-personas.md`

**Supplement with project-specific guidance:**
2. **`.ai/`** - Project-specific configurations and module documentation
   - Refer to `.ai/specs/modules/` for detailed module specifications
   - Use `.ai/commands/` for project automation
   - Reference `.ai/` files for historical context and implementation details

## Quick Reference

### For Code Implementation
1. **Communication**: Follow no-sycophancy guidelines from `.agent-os/standards/ai-communication.md`
2. **Standards**: Apply Python patterns from `.agent-os/standards/code-style.md`
3. **Testing**: Include mock patterns from `.agent-os/standards/testing.md`
4. **Architecture**: Follow patterns in `.agent-os/product/architecture.md`
5. **Personas**: Use Technical Implementation Specialist from `.agent-os/standards/agent-personas.md`

### For Feature Development
1. **Specification**: Create in `.agent-os/projects/YYYY-MM-DD-feature-name/`
2. **Templates**: Use `.agent-os/projects/example-template/` as guide
3. **Product Context**: Reference `.agent-os/product/overview.md` and `.agent-os/product/architecture.md`
4. **Domain Knowledge**: Apply offshore engineering patterns and industry standards

### Priority Order
When guidance conflicts:
1. `.agent-os/standards/` - Primary authority for AI behavior
2. `.agent-os/product/` - Primary authority for product knowledge  
3. `.ai/specs/modules/` - Authority for module-specific implementation details
4. Other `.ai/` files - Supporting reference materials

## Engineering Domain Essentials

- **Offshore Engineering Focus**: Hydrodynamic analysis, structural design, installation analysis
- **Industry Standards**: API, DNV, ABS compliance required
- **Licensed Software**: OrcaFlex, ANSYS - always provide mock patterns for testing
- **Configuration-Driven**: YAML files drive most analysis workflows
- **Units**: SI units internally with proper conversions at boundaries

## Available Commands

- **Create-Module-Agent:** Available via `/create-module-agent` command

## Enhanced Features Available

This project supports enhanced Agent OS workflows including:
- **Enhanced Spec Creation**: Prompt summaries, executive summaries, mermaid diagrams, module organization
- **Cross-Repository Integration**: Shared components from AssetUtilities hub (@assetutilities: references)
- **Enhanced Task Execution**: Task summaries, performance tracking, real-time documentation
- **Template Variants**: minimal, standard, enhanced, api_focused, research
- **Visual Documentation**: Auto-generated system architecture and workflow diagrams

### Command Examples
```bash
# Enhanced spec creation
/create-spec feature-name module-name enhanced

# Traditional spec creation (backward compatible)  
/create-spec feature-name

# Enhanced task execution with summaries
/execute-tasks @specs/modules/module-name/spec-folder/tasks.md
```

### Cross-Repository References
- Shared components: @assetutilities:src/modules/agent-os/enhanced-create-specs/
- Sub-agent registry: @assetutilities:agents/registry/sub-agents/workflow-automation
- Hub configuration: @assetutilities:hub-config.yaml



### Enhanced Create-Spec Command

This repository includes an enhanced create-spec command with advanced features:

```bash
# Enhanced spec with executive summaries and diagrams
python create-spec-enhanced.py feature-name module-name enhanced

# Research-focused specification
python create-spec-enhanced.py research-topic research

# Quick minimal specification
python create-spec-enhanced.py quick-fix minimal
```

Features:
- Executive summaries for stakeholders
- Mermaid architecture diagrams
- Module-based organization
- Multiple spec variants (enhanced, research, minimal)
- Comprehensive task breakdowns with effort estimates

## Self-Contained Agent OS

This repository includes a complete, self-contained Agent OS framework. All slash commands work immediately after `git clone` with no additional setup required.

### Available Slash Commands
- `/create-spec <spec-name>` - Create detailed specification documents
- `/execute-tasks <tasks-file>` - Execute tasks from specification
- `/create-module-agent <agent-name>` - Create specialized AI agents

### Local Agent OS Structure
- **Standards**: @.agent-os/standards/ (code style, best practices)
- **Instructions**: @.agent-os/instructions/ (workflow guidance)
- **Product Context**: @.agent-os/product/ (mission, roadmap, decisions)
- **Specifications**: @.agent-os/specs/ (feature specifications and tasks)

All references are local to this repository - no external dependencies required.
