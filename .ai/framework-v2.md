# AI Assistant Framework v2.0

## Directory Structure

```
.ai/
├── agent-personas/                 # AI agent role definitions
│   ├── README.md                  # Overview of personas and responsibilities
│   ├── assigned-engineer.md       # Engineering agent persona
│   ├── assigned-product-manager.md # Product management agent persona
│   ├── assigned-product-owner.md  # Product owner agent persona
│   └── templates/                 # Persona templates
│       └── persona-template.md
│
├── requirements/                   # Comprehensive requirements documentation
│   ├── README.md                  # Requirements overview and ID system
│   ├── R001-product-management.md # Overall purpose and high-level goals
│   ├── R002-product-structure.md  # Modules, features, capabilities
│   ├── R003-plans-roadmaps.md     # Development plans and roadmaps
│   ├── R004-personas.md           # User personas and stakeholders
│   ├── R005-technical-architecture.md # Application architecture
│   └── R006-devops-specifications.md # Build, testing, deployment specs
│
├── system-overview/               # System mapping and tool overview
│   ├── README.md                  # System overview guide
│   ├── devops-tools.md           # DevOps toolchain mapping
│   ├── build-pipeline.md         # Build system overview
│   ├── deployment-strategy.md    # Deployment processes
│   ├── testing-framework.md      # Testing tools and strategies
│   └── architecture-diagram.md   # System architecture visualization
│
├── workflows/                     # Workflow specifications
│   ├── README.md                  # Workflow overview and standards
│   ├── epic-workflow.md           # Epic management workflow
│   ├── feature-workflow.md        # Feature development workflow
│   ├── user-story-workflow.md     # User story implementation workflow
│   ├── bug-workflow.md            # Bug fixing workflow
│   ├── state-of-union-workflow.md # Codebase alignment review workflow
│   └── implementation/            # Existing implementation workflows
│       ├── issue-planning.md
│       ├── implementation-tracking.md
│       └── ... (existing files)
│
├── implementation-history/        # Historical implementation tracking
│   ├── README.md                  # Implementation history overview
│   ├── 2025/                      # Year-based organization
│   │   ├── epics/
│   │   │   ├── epic-issue-123.md
│   │   │   └── epic-issue-456.md
│   │   ├── features/
│   │   │   ├── feature-issue-789.md
│   │   │   └── feature-issue-101.md
│   │   ├── bugs/
│   │   │   ├── bug-issue-112.md
│   │   │   └── bug-issue-131.md
│   │   └── infrastructure/
│   │       ├── infra-issue-415.md
│   │       └── infra-issue-161.md
│   └── templates/
│       ├── epic-implementation-template.md
│       ├── feature-implementation-template.md
│       ├── bug-implementation-template.md
│       └── infrastructure-implementation-template.md
│
├── project-context.md             # High-level project context (existing)
├── settings.json                  # AI assistant settings (existing)
└── AI_GUIDELINES.md               # Main AI guidelines (existing)
```

## Framework Principles

### 1. Comprehensive Context
Provide AI agents with maximum context about:
- Project purpose and objectives
- Technical architecture and constraints
- User personas and requirements
- Development processes and standards
- Historical implementation patterns

### 2. Role-Based Personas
Define specific AI agent personas with:
- Clear responsibilities and scope
- Decision-making authority
- Interaction patterns with other personas
- Quality gates and validation criteria

### 3. Systematic Workflows
Establish structured workflows for:
- Epic and feature development
- Bug fixing and maintenance
- Code review and quality assurance
- Documentation and knowledge transfer

### 4. Traceability and History
Maintain comprehensive records of:
- Implementation decisions and rationale
- Code changes and their impacts
- Requirements evolution
- Architecture decisions

### 5. Continuous Alignment
Regular validation that:
- Code aligns with documented requirements
- Architecture matches current implementation
- Documentation reflects actual system behavior
- Workflows remain effective and relevant

## Implementation Guidelines

### Requirement IDs
All requirements use hierarchical IDs:
- `R001` - Top-level requirement
- `R001.1` - Sub-requirement
- `R001.1.1` - Detailed requirement
- `R001.1.1.a` - Implementation detail

### File Naming Conventions
- Requirements: `R###-descriptive-name.md`
- Workflows: `descriptive-workflow.md`
- Personas: `assigned-role-name.md`
- Implementation History: `type-issue-###.md`
- System Overview: `component-name.md`

### Cross-Reference Standards
- Link requirements to implementations
- Reference personas in workflows
- Connect issues to requirements
- Maintain bidirectional traceability

## Migration Plan

### Phase 1: Structure Creation
1. Create new directory structure
2. Migrate existing content to appropriate locations
3. Update file references and links

### Phase 2: Content Enhancement
1. Develop persona definitions
2. Create comprehensive requirements documentation
3. Establish system overview documentation

### Phase 3: Workflow Integration
1. Integrate existing implementation workflows
2. Create new workflow specifications
3. Establish implementation history tracking

### Phase 4: Validation and Refinement
1. Validate framework effectiveness
2. Refine based on usage patterns
3. Update documentation and processes