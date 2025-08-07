# Digital Model Specifications

This directory contains all specifications for the Digital Model project, consolidated from previous `.ai/specs` and `specs` locations into a unified, logically organized structure.

## Structure Overview

```
specs/
├── modules/                    # Feature and domain modules
│   ├── agent-os/              # Agent OS framework and components
│   │   ├── foundation/        # Core Agent OS framework specs
│   │   ├── integration/       # General Agent OS integration
│   │   ├── python-integration/ # Python ecosystem integration
│   │   └── slash-commands/    # Command implementations
│   ├── marine-engineering/    # Marine engineering analysis specs
│   │   └── orcaflex/         # OrcaFlex-specific specifications
│   ├── infrastructure/        # System infrastructure improvements
│   ├── development-tools/     # Development workflow enhancements
│   ├── ai-workflows/         # AI framework and workflow specs
│   └── test-suite-automation/ # Test automation system (current)
├── templates/                 # Specification templates
└── workflows/                # Development workflows and processes
```

## Module Descriptions

### 🤖 Agent OS (`modules/agent-os/`)
Comprehensive Agent OS framework for AI-assisted development.

**Foundation:**
- AI assistant repository restructure
- Framework foundation setup
- Epic implementation planning
- AI persona framework

**Integration:**
- General Agent OS integration with cross-platform compatibility
- Python ecosystem integration with development tools
- Workflow automation and configuration management

**Slash Commands:**
- `/create-module-agent` - Agent creation command
- `/create-spec` - Specification generation command

### 🚢 Marine Engineering (`modules/marine-engineering/`)
Offshore engineering analysis and simulation specifications.

**Core Features:**
- 6DOF motion analysis
- RAO data import and processing (AQWA, OrcaFlex)
- Ship design dynamics
- Marine analysis workflows

**OrcaFlex Integration:**
- Sequential processing configuration
- Object troubleshooting
- Analysis workflow optimization

### 🏗️ Infrastructure (`modules/infrastructure/`)
System-level improvements and infrastructure enhancements.

**Key Areas:**
- AI assistant indexing performance
- Dependency management improvements
- Parallel processing optimization
- System performance enhancements

### 🛠️ Development Tools (`modules/development-tools/`)
Tools and workflows for enhanced development experience.

**Features:**
- AI-friendly documentation organization
- Git repository optimization
- Development workflow automation
- Code quality improvements

### 🧠 AI Workflows (`modules/ai-workflows/`)
AI framework specifications and workflow definitions.

**Components:**
- AI framework foundation
- Project context definitions
- Workflow automation specs
- Integration patterns

### 🧪 Test Suite Automation (`modules/test-suite-automation/`)
Comprehensive test automation and maintenance system.

**Current Status:** ✅ **Active Implementation**
- Enhanced with coverage tracking
- Strategic improvement roadmap
- Module remediation tasks
- Executive reporting capabilities

## Using This Structure

### For AI Assistants
1. **Start with module context**: Read the relevant module's README and overview specs
2. **Follow specification hierarchy**: Main spec → technical specs → tasks → tests
3. **Reference templates**: Use templates for consistent specification format
4. **Update cross-references**: Maintain links between related specifications

### For Developers
1. **Feature Development**: Create specs in appropriate module directories
2. **Cross-Module Work**: Reference related specs across modules
3. **Template Usage**: Start new specs using templates from `templates/`
4. **Workflow Integration**: Follow processes defined in `workflows/`

## Specification Standards

### File Naming Convention
- Main specifications: `spec.md`
- Technical details: `sub-specs/technical-spec.md`
- Implementation tasks: `tasks.md`
- Test specifications: `sub-specs/tests.md`
- Summary reports: `test_summary.md` (for completed implementations)

### Cross-References
Use relative paths for internal references:
```markdown
See [Agent OS Framework](../agent-os/framework/spec.md) for integration details.
```

### Status Tracking
Each specification should include:
- **Status**: Draft | Active | Completed | Deprecated
- **Version**: Semantic versioning
- **Last Updated**: Date of last significant change
- **Dependencies**: Links to related specifications

## Migration Notes

This consolidation brings together specifications from:
- `.ai/specs/infrastructure/` → `modules/infrastructure/` and `modules/agent-os/foundation/`
- `.ai/specs/modules/` → `modules/marine-engineering/` and `modules/infrastructure/`
- `specs/modules/orcaflex/` → `modules/marine-engineering/orcaflex/`
- `specs/modules/agent-os-integration/` → `modules/agent-os/integration/`
- `specs/modules/agent-os-python-integration/` → `modules/agent-os/python-integration/`
- `specs/modules/docs-organization-ai-friendly/` → `modules/development-tools/`
- `specs/modules/git-repository-optimization/` → `modules/development-tools/`

All cross-references have been maintained and updated to reflect the new structure.

## Contributing

When adding new specifications:
1. Choose the appropriate module or create a new one for distinct domains
2. Use the templates from `templates/` directory
3. Follow the established naming conventions
4. Update this README with new modules or significant changes
5. Maintain cross-references to related specifications

---

*This structure supports AI-assisted development by providing clear, logical organization of all project specifications in a single, navigable hierarchy.*