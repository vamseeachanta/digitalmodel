# Documentation Reorganization Specification

## Executive Summary

The current `docs/` directory structure has grown to an unsustainable 528+ directories housing only ~300 markdown files, creating significant navigation challenges for both human users and AI agents. This specification outlines a comprehensive reorganization that reduces complexity by 80% while improving content discovery and maintaining all existing knowledge value.

### Current State Problems

- **Scale Overwhelm**: 100+ top-level subcategories create choice paralysis
- **Inconsistent Organization**: Mixed `domains/` and `modules/` approaches violate repository patterns
- **Deep Nesting**: Some content requires 6+ navigation steps to reach
- **Infrastructure Mixing**: Utility folders (_tools, _config, _assets) clutter content discovery
- **Maintenance Burden**: Too many sparse/empty directories to effectively maintain
- **Discovery Friction**: Average 2-5 minutes to locate specific technical content

## Proposed New Structure

### Core Principle: Repository Pattern Enforcement

All documentation will follow the established repository pattern: `docs/modules/<module>/`

### New Simplified Hierarchy

```
docs/
├── README.md                    # Main entry point with module index
├── NAVIGATION.md               # Complete content map for AI agents
├── modules/                    # All technical content (ONLY location)
│   ├── marine-engineering/     # Consolidated naval architecture
│   │   ├── ship-design/       # Hull forms, stability, strength
│   │   ├── hydrodynamics/     # Wave theory, motions, responses
│   │   ├── moorings/          # Mooring systems, anchors
│   │   └── installations/     # Installation analysis
│   ├── offshore-engineering/   # Consolidated offshore content
│   │   ├── risers/           # Riser analysis and design
│   │   ├── pipelines/        # Pipeline design, buckling
│   │   ├── platforms/        # Fixed and floating platforms
│   │   └── umbilicals/       # Umbilical and cable systems
│   ├── drilling/             # All drilling-related content
│   │   ├── equipment/        # Drill strings, BOPs, wellheads
│   │   ├── operations/       # Procedures, automation
│   │   └── well-control/     # Safety and control systems
│   ├── materials/            # Materials and corrosion
│   │   ├── corrosion/        # Cathodic protection, coatings
│   │   ├── fatigue/          # Fatigue analysis methods
│   │   └── welding/          # Welding procedures and codes
│   ├── software-tools/       # All engineering software
│   │   ├── orcaflex/         # OrcaFlex documentation
│   │   ├── aqwa/             # AQWA documentation  
│   │   ├── ansys/            # ANSYS suite documentation
│   │   ├── freecad/          # FreeCAD and CAD tools
│   │   └── specialized/      # Blender, QGIS, etc.
│   ├── analysis-methods/     # Cross-domain analysis techniques
│   │   ├── fem/              # Finite element methods
│   │   ├── modal-analysis/   # Vibration and modal analysis
│   │   ├── signal-analysis/  # Time series, FFT, rainflow
│   │   └── risk-assessment/  # Risk and reliability methods
│   ├── standards-codes/      # Industry standards and regulations
│   │   ├── api/              # API standards
│   │   ├── dnv/              # DNV codes and practices
│   │   ├── abs/              # ABS classification rules
│   │   └── regulations/      # Government and safety regulations
│   └── project-management/   # Project and process management
│       ├── workflows/        # Engineering workflows
│       ├── quality/          # QA/QC procedures
│       └── documentation/    # Documentation standards
├── _infrastructure/          # Utility and configuration files
│   ├── tools/               # Documentation generation tools
│   ├── config/              # Configuration files
│   ├── assets/              # Shared images, diagrams
│   └── templates/           # Document templates
└── archive/                 # Legacy content (temporary)
    ├── legacy-domains/      # Original domains/ content during migration
    └── migration-logs/      # Migration tracking and validation
```

### Key Design Principles

1. **Cognitive Load Optimization**: Maximum 7±2 top-level categories
2. **Repository Pattern Compliance**: Strict `modules/<module>/` organization
3. **Clear Separation**: Content vs. infrastructure vs. archives
4. **AI-Friendly Navigation**: Structured metadata and clear entry points
5. **Hierarchical Logic**: Related content grouped by engineering discipline

## Entry Points for Humans and AI

### Human Navigation

1. **Primary Entry**: `docs/README.md` with module overview and quick links
2. **Module Indexes**: Each `modules/<module>/README.md` contains complete content inventory
3. **Cross-References**: Related modules linked in each README
4. **Search Optimization**: Key terms and aliases included in module descriptions

### AI Agent Navigation  

1. **Master Index**: `docs/NAVIGATION.md` with complete content mapping
2. **Structured Metadata**: Each module includes tags, keywords, and relationships
3. **Content Types**: Clear labeling of tutorials, references, procedures, examples
4. **Dependency Mapping**: Inter-module dependencies explicitly documented

### Example Module Structure

```
docs/modules/marine-engineering/
├── README.md                   # Module overview and content index
├── ship-design/
│   ├── _index.md              # Ship design content map
│   ├── hull-forms/            # Specific topics
│   ├── stability/
│   ├── strength/
│   └── examples/              # Working examples
├── hydrodynamics/
│   ├── _index.md
│   ├── wave-theory/
│   ├── response-analysis/
│   └── software-guides/       # Tool-specific guidance
└── MARINE_ENGINEERING.md     # Complete discipline overview
```

## Migration Strategy

### Phase 1: Foundation (Week 1)
1. Create new directory structure
2. Implement README.md files with navigation
3. Set up infrastructure directories
4. Create migration tracking system

### Phase 2: High-Value Content (Week 2-3)
1. **Priority 1**: OrcaFlex and AQWA content (most accessed)
2. **Priority 2**: Ship design and hydrodynamics
3. **Priority 3**: Pipeline and riser analysis
4. **Validation**: Verify all links and references

### Phase 3: Consolidation (Week 4)
1. Remaining domains content
2. Software tools documentation
3. Standards and codes organization
4. Cross-reference validation

### Phase 4: Cleanup (Week 5)
1. Archive original domains/ and modules/ directories
2. Update all internal links
3. Create redirects for external references
4. Final validation and testing

### Migration Validation

Each phase includes:
- **Content Inventory**: Before/after file counts
- **Link Validation**: All internal references working
- **Search Testing**: Key terms findable in &lt;30 seconds
- **User Acceptance**: Sample navigation scenarios tested

## Benefits and Impact Analysis

### Quantitative Benefits

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Total Directories | 528+ | &lt;100 | 80% reduction |
| Navigation Depth | 6+ levels | 3 levels max | 50% reduction |
| Content Discovery Time | 2-5 minutes | &lt;30 seconds | 90% improvement |
| Empty/Sparse Directories | ~40% | &lt;5% | Elimination of clutter |
| Maintenance Hours/Month | 8-12 hours | 2-4 hours | 70% reduction |

### Qualitative Benefits

**For Human Users:**
- Clear mental model of content organization
- Reduced cognitive load when searching
- Intuitive navigation paths
- Better content discoverability

**For AI Agents:**
- Structured metadata for efficient indexing
- Clear content relationships and dependencies
- Predictable navigation patterns
- Reduced hallucination risk through clear organization

**For Maintenance:**
- Single organizational pattern to maintain
- Clear rules for placing new content
- Reduced duplicate content risk
- Simplified backup and archival processes

### Risk Mitigation

**Content Loss Prevention:**
- Complete inventory before migration
- Staged migration with rollback capability
- Multiple validation checkpoints
- Archive preservation of original structure

**Reference Breaking:**
- Comprehensive link analysis
- Redirect creation for external references
- Update all internal documentation
- Communication plan for users

**Adoption Resistance:**
- Clear documentation of benefits
- Training materials for new structure
- Gradual rollout with feedback incorporation
- Success metrics tracking

## Implementation Resources

### Required Tools
- Migration scripts for content movement
- Link analysis and update tools
- Directory structure validation scripts
- Content inventory and tracking systems

### Time Investment
- **Planning and Setup**: 8 hours
- **Content Migration**: 40 hours  
- **Validation and Testing**: 16 hours
- **Documentation Updates**: 12 hours
- **Total**: ~76 hours over 5 weeks

### Success Metrics
- Navigation time reduced to &lt;30 seconds for any content
- Directory count reduced by 80%
- Zero broken internal links
- 95% user satisfaction with new structure
- Maintenance time reduced by 70%

## Next Steps

1. **Approval**: Stakeholder review and approval of proposed structure
2. **Resource Allocation**: Assign implementation team and timeline
3. **Tool Development**: Create migration and validation scripts
4. **Pilot Testing**: Test migration approach on small content subset
5. **Full Implementation**: Execute phased migration plan

This reorganization will transform the docs directory from a navigation challenge into an efficient, maintainable knowledge system that serves both human users and AI agents effectively.