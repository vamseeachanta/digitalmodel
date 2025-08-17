# OrcaFlex File Ecosystem Structure - Prompt Documentation

## Original User Request
"Create a folder structure to accommodate the orcaflex file ecosystem with digitalmodel, inputs (.yml files), .sim files, outputs, iterations etc. should we even have .dat files at all"

## Analysis Approach

### Initial Questions Considered
1. **Current State Assessment**: What is the existing folder structure and file organization?
2. **File Format Decision**: Should .dat files be deprecated in favor of YAML?
3. **Workflow Requirements**: How do iterations, batch processing, and parallel execution fit?
4. **Integration Needs**: How does this integrate with existing OrcaFlex agent and modules?
5. **Migration Strategy**: How to transition from current to proposed structure?

### Key Decisions Made
1. **YAML-Only Policy**: Complete deprecation of .dat files based on:
   - Better version control compatibility
   - Human-readable format
   - Easier automation and scripting
   - Consistent with repository standards

2. **Workspace Concept**: Introduced isolated processing environments to:
   - Prevent conflicts during parallel processing
   - Maintain clean separation between runs
   - Enable better debugging and troubleshooting

3. **Three-Stage Lifecycle**: Clear data flow pattern:
   - Inputs → Workspace (active processing) → Outputs → Archive
   - Provides clear separation of concerns
   - Enables efficient cleanup and retention

## Research Findings

### Current Structure Analysis
- **880 YAML files** vs **98 DAT files** - Clear trend toward YAML
- Existing file type detector already distinguishes formats
- Modern workflows already YAML-based
- DAT files mainly in legacy documentation

### Best Practices Identified
1. **Project-based organization** for easy navigation
2. **Timestamped workspaces** for parallel processing
3. **Structured archival** for long-term storage
4. **Clear naming conventions** for consistency

## Specification Highlights

### Core Structure
```
data/orcaflex/
├── models/        # Source models (YAML only)
├── inputs/        # Analysis configurations
├── workspace/     # Active processing area
├── outputs/       # Results and simulations
└── archive/       # Long-term storage
```

### Key Features
1. **Template Library**: Reusable components and templates
2. **Project Organization**: All files grouped by project
3. **Iteration Tracking**: Full history of iterative analyses
4. **Automated Archival**: Policy-based archive management

## Implementation Strategy

### Six-Phase Approach
1. **Foundation** - Build utilities and tools
2. **Migration** - Convert .dat to YAML
3. **Code Updates** - Update all references
4. **Integration** - Implement management systems
5. **Documentation** - Update and train
6. **Validation** - Test and rollout

### Critical Success Factors
- Zero .dat files in active use
- No workflow disruptions
- Clear migration path
- Comprehensive documentation
- User training completion

## Reuse Prompt

### For Similar Specifications
```
Create a comprehensive folder structure specification for [MODULE_NAME] that:
1. Analyzes current file organization and identifies inefficiencies
2. Proposes optimized structure with clear separation of concerns
3. Defines file naming conventions and lifecycle management
4. Includes migration strategy from current to proposed state
5. Provides detailed task breakdown with effort estimates
6. Considers version control, parallel processing, and automation needs
7. Includes archival and retention policies

Focus on:
- Eliminating duplicate file formats
- Supporting iterative workflows
- Enabling parallel processing
- Maintaining full traceability
- Simplifying automation
```

### For OrcaFlex-Specific Extensions
```
Extend the OrcaFlex file ecosystem structure to support [NEW_FEATURE]:
1. Review existing structure specification
2. Identify integration points for new feature
3. Define additional folders/files needed
4. Update naming conventions if required
5. Modify workflow to accommodate feature
6. Update task breakdown for implementation

Maintain consistency with:
- YAML-only policy
- Workspace concept
- Three-stage lifecycle
- Project-based organization
```

## Lessons Learned

### What Worked Well
1. **Comprehensive Analysis**: Deep dive into existing structure revealed clear deprecation opportunity
2. **Data-Driven Decision**: File count analysis (880 YML vs 98 DAT) supported recommendation
3. **Phased Approach**: Six-week implementation plan provides manageable transition
4. **Clear Benefits**: Quantifiable improvements in version control, automation, and maintenance

### Areas for Enhancement
1. **User Impact Assessment**: Could include more detailed user workflow analysis
2. **Performance Metrics**: Could define specific performance improvement targets
3. **Rollback Plan**: Could detail more comprehensive rollback procedures
4. **Integration Testing**: Could specify integration test scenarios

## Next Steps

### Immediate Actions
1. Review and approve specification with stakeholders
2. Create proof-of-concept for workspace manager
3. Build .dat to YAML conversion tool
4. Start documentation updates

### Long-term Considerations
1. Extend structure to other analysis modules
2. Implement cross-module file sharing
3. Create centralized model library
4. Build advanced search capabilities

## Related Specifications
- OrcaFlex Module Agent Specification
- Batch Processing Enhancement Specification
- Mooring Tension Iteration Workflow
- Signal Analysis Module Integration

## Contact and Resources
- **Specification Author**: AI Assistant (Claude)
- **Domain Expert**: OrcaFlex Module Agent
- **Implementation Team**: DigitalModel Development
- **Documentation**: `docs/modules/orcaflex/`