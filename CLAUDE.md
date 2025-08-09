# CLAUDE.md

This file provides guidance to AI assistants when working with code in this repository.

## 🏗️ CRITICAL REPOSITORY ORGANIZATION PATTERN

**THIS REPOSITORY USES MODULE-BASED ORGANIZATION - ENFORCE ACROSS ALL SESSIONS/USERS/SYSTEMS**

All directory groups MUST follow this structure:
```
<group_name>/modules/<module_name>/
```

**Examples:**
- ✅ `specs/modules/agent-os/` 
- ✅ `specs/modules/marine-engineering/`
- ✅ `src/modules/hydrodynamics/`
- ✅ `docs/modules/installation/`
- ❌ `specs/agent-os/` (WRONG - missing modules/ level)
- ❌ `marine-specs/` (WRONG - not following pattern)

**This pattern applies to ALL directory groups:**
- `specs/modules/<module>/` - All specifications
- `src/modules/<module>/` - All source code  
- `docs/modules/<module>/` - All documentation
- `tests/modules/<module>/` - All test files
- `configs/modules/<module>/` - All configurations

**ENFORCE THIS PATTERN in every session, for every user, across all systems. Never create or accept directory structures that violate this pattern.**

## Primary Guidance Sources

**Start with Agent OS structure:**
1. **`.agent-os/`** - Primary guidance for all AI agents
   - Read `.agent-os/README.md` first for complete overview
   - Follow `.agent-os/standards/ai-communication.md` for communication requirements
   - Apply `.agent-os/standards/code-style.md` and `.agent-os/standards/testing.md`
   - Use appropriate persona from `.agent-os/standards/agent-personas.md`

**Supplement with project-specific guidance:**
2. **`specs/modules/`** - Module-based specifications following repository pattern
   - Refer to `specs/modules/<module>/` for detailed module specifications
   - All specifications MUST be located in `specs/modules/<module>/` structure
   - Legacy `.ai/` files are for historical context only

## Quick Reference

### For Code Implementation
1. **Communication**: Follow no-sycophancy guidelines from `.agent-os/standards/ai-communication.md`
2. **Standards**: Apply Python patterns from `.agent-os/standards/code-style.md`
3. **Testing**: Include mock patterns from `.agent-os/standards/testing.md`
4. **Architecture**: Follow patterns in `.agent-os/product/architecture.md`
5. **Personas**: Use Technical Implementation Specialist from `.agent-os/standards/agent-personas.md`

### For Feature Development
1. **Specification**: Create in `specs/modules/<module>/` following repository pattern
2. **Templates**: Use `.agent-os/projects/example-template/` as guide
3. **Product Context**: Reference `.agent-os/product/overview.md` and `.agent-os/product/architecture.md`
4. **Domain Knowledge**: Apply offshore engineering patterns and industry standards

### Priority Order
When guidance conflicts:
1. **Repository Organization**: `<group>/modules/<module>/` pattern is MANDATORY
2. `.agent-os/standards/` - Primary authority for AI behavior
3. `.agent-os/product/` - Primary authority for product knowledge  
4. `specs/modules/<module>/` - Authority for module-specific implementation details
5. Legacy `.ai/` files - Supporting reference materials only

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
# Enhanced spec creation (MUST include prompt.md)
/create-spec feature-name module-name enhanced

# Traditional spec creation (MUST include prompt.md)
/create-spec feature-name

# Enhanced task execution with summaries (MUST use module pattern)
/execute-tasks @specs/modules/module-name/spec-folder/tasks.md
```

### /create-spec MANDATORY Requirements
**EVERY /create-spec command MUST:**
1. **Repository Pattern**: Create specs in `specs/modules/<module>/` structure
2. **Prompt Documentation**: Create `prompt.md` with complete prompt history and curated reuse prompt
3. **Template Compliance**: Follow all established templates and patterns
4. **Module Integration**: Update relevant module READMEs and cross-references
5. **Markdown Compatibility**: Ensure ALL generated documents use markdown-compatible characters:
   - Escape angle brackets: `&lt;` `&gt;` instead of `<` `>`
   - Escape ampersands in text: `&amp;` instead of `&`
   - Escape Windows paths: `D:\\path\\file` instead of `D:\path\file`
   - Validate all special characters render properly in markdown processors
6. **Mathematical Expressions**: When technical specifications include formulas, equations, or mathematical notation:
   - Use KaTeX/LaTeX blocks for complex equations: `$$equation$$` for display mode, `$equation$` for inline
   - Simple format for AI: `$$F = ma$$` or `$\sigma = \frac{F}{A}$`
   - Include units in LaTeX format: `$$P_{wind} = \frac{1}{2}\rho C_d A v^2 \text{ [N]}$$`
   - Use for engineering calculations, statistical formulas, and mathematical relationships

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
