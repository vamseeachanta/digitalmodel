# Agent OS for DigitalModel

This directory contains the Agent OS configuration for the DigitalModel project. Agent OS helps AI coding agents understand and work with this codebase more effectively.

## Directory Structure

```
.agent-os/
├── product/           # Product documentation
│   ├── overview.md    # High-level product overview
│   ├── architecture.md # System architecture
│   ├── stack.md       # Technology stack details
│   └── roadmap.md     # Product roadmap
├── standards/         # Coding standards and conventions
│   ├── repository-organization.md # MANDATORY module-based directory pattern
│   ├── code-style.md  # Code formatting and style guide
│   ├── testing.md     # Testing requirements
│   └── git.md         # Git workflow and commit conventions
└── projects/          # Project specs organized by date
    └── YYYY-MM-DD-feature-name/
        ├── spec.md    # Feature specification
        └── tasks.md   # Implementation tasks
```

## Getting Started

1. Review the product documentation in `./product/`
2. Familiarize yourself with standards in `./standards/`
3. Check active projects in `./projects/`

## Quick Start for AI Agents

### For New AI Agents
1. **Start here**: Read this README for overview
2. **Product Understanding**: Review `./product/overview.md` and `./product/architecture.md`
3. **Communication Standards**: Follow `./standards/ai-communication.md` strictly
4. **Code Standards**: Apply `./standards/code-style.md` and `./standards/testing.md`
5. **Agent Personas**: Select appropriate persona from `./standards/agent-personas.md`
6. **Active Projects**: Check active specs in `./projects/` directory

### For Feature Development
1. **Specification First**: Create spec in `./projects/YYYY-MM-DD-feature-name/`
2. **Follow Templates**: Use `./projects/example-template/` as guide
3. **Reference Standards**: Apply all standards from `./standards/`
4. **Test Comprehensively**: Include licensed software mock patterns
5. **Document Changes**: Update relevant product documentation

## Complete Documentation Structure

```
.agent-os/
├── README.md                     # This file - start here
├── integration.md                # How Agent OS works with .ai directory
├── product/                      # Product knowledge
│   ├── overview.md              # Product description and capabilities
│   ├── architecture.md          # System architecture and patterns
│   ├── stack.md                 # Technology choices and tools
│   └── roadmap.md               # Product roadmap and priorities
├── standards/                    # AI agent standards
│   ├── repository-organization.md # MANDATORY module-based directory pattern
│   ├── ai-communication.md      # Communication style requirements
│   ├── code-style.md            # Python coding standards
│   ├── testing.md               # Testing requirements and patterns
│   ├── git.md                   # Git workflow and conventions
│   └── agent-personas.md        # Standardized AI agent roles
└── projects/                     # Feature specifications
    ├── example-template/         # Template for new features
    └── YYYY-MM-DD-feature-name/  # Active and completed projects
```

## AI Agent Workflow

### Standard Operating Procedure
1. **Repository Organization**: ALWAYS enforce `<group>/modules/<module>/` pattern from repository-organization.md
2. **Communication Check**: Verify you're following ai-communication.md standards
3. **Persona Selection**: Choose appropriate persona from agent-personas.md
4. **Product Context**: Reference product/ docs for domain knowledge
5. **Code Standards**: Apply code-style.md and testing.md requirements
6. **Specification**: Create or reference project specification in `specs/modules/<module>/`
7. **Implementation**: Follow architecture patterns and best practices
8. **Validation**: Test comprehensively including mock patterns
9. **Documentation**: Update relevant docs and cross-references

### Quality Checklist
Before completing any task:
- [ ] **Repository organization enforced** - All directories follow `<group>/modules/<module>/` pattern
- [ ] Communication follows no-sycophancy guidelines
- [ ] Code follows Python standards with type hints
- [ ] Tests include mock patterns for licensed software
- [ ] Engineering constraints and units validated
- [ ] Documentation updated and cross-referenced
- [ ] Git workflow and commit conventions followed
- [ ] Appropriate persona maintained throughout

## Integration with Existing .ai Directory

This project maintains both Agent OS and legacy AI configurations:

### Division of Responsibilities
- **`.agent-os/`** - Generic AI standards, product knowledge, architecture patterns
- **`.ai/`** - Project-specific specifications, module documentation, automation commands

### Migration Status
- ✅ **AI Communication Standards** - Migrated to `.agent-os/standards/ai-communication.md`
- ✅ **Code Standards** - Enhanced in `.agent-os/standards/code-style.md`
- ✅ **Architecture Patterns** - Integrated into `.agent-os/product/architecture.md`
- ✅ **Testing Standards** - Enhanced in `.agent-os/standards/testing.md`
- ✅ **Agent Personas** - Standardized in `.agent-os/standards/agent-personas.md`
- 🔄 **Project Specs** - Remain in `.ai/specs/modules/` (project-specific)
- 🔄 **Commands** - Remain in `.ai/commands/` (project automation)

### Reference Priority
When guidance conflicts, use this priority:
1. **Repository Organization**: `<group>/modules/<module>/` pattern from `.agent-os/standards/repository-organization.md` - MANDATORY
2. `.agent-os/standards/` - Primary authority for AI behavior
3. `.agent-os/product/` - Primary authority for product knowledge
4. `specs/modules/<module>/` - Authority for module-specific details (MUST follow pattern)
5. Legacy `.ai/` files - Supporting reference materials only

## Engineering Domain Context

### Offshore Engineering Focus
DigitalModel specializes in offshore and marine engineering:
- **Hydrodynamic Analysis**: Wave loading, vessel motions, frequency domain
- **Structural Analysis**: Fatigue, capacity calculations, finite element modeling
- **Installation Analysis**: Crane operations, weather windows, rigging
- **Pipeline Engineering**: Capacity, pressure design, lateral buckling
- **Mooring Systems**: Catenary, taut leg, dynamic positioning

### Industry Standards Integration
Code must comply with:
- **API Standards**: RP 2RD, STD 2RD, RP 1111 (pipeline design)
- **DNV Standards**: OS-F101, OS-F201, RP-F105 (offshore structures)
- **ABS Standards**: Fatigue assessment, structural design
- **Engineering Units**: SI units internally, appropriate conversions at boundaries

### Licensed Software Integration
- **OrcaFlex**: Dynamic analysis, requires commercial license
- **ANSYS AQWA**: Hydrodynamic analysis, requires license
- **ANSYS Mechanical**: Structural FEA, requires license
- Always provide mock patterns for testing without licenses