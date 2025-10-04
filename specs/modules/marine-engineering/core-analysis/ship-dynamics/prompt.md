# Ship Dynamics Analysis - Prompt Documentation and Reuse Patterns

## Original Request Context

### Initial User Prompt
```
Reorganize the marine-engineering specifications to follow repository best practices using the /create-spec command pattern.

Current files to reorganize:
1. epic-marine-analysis-ship-design-dynamics-2025.md
2. feature-6dof-motion-analysis-2025.md
3. user-story-aqwa-rao-data-import-2025.md
4. user-story-orcaflex-rao-data-import-2025.md
5. user-story-rao-data-import-processing-2025.md

Create proper module structure following best practices:
1. Group related specs into logical sub-modules
2. Create standardized file structure for each:
   - README.md (main specification)
   - tasks.md (implementation tasks)
   - task_summary.md (execution tracking)
   - prompt.md (original prompts and reuse patterns)
   - technical-details.md (deep technical documentation)
   - sub-specs/ folder for component specifications
```

### Project Context Analysis
The request involved reorganizing existing marine engineering specifications from a flat structure into a hierarchical module-based organization following repository best practices. The key challenge was preserving comprehensive technical content while improving organization and maintainability.

## Prompt Evolution and Decision Points

### Analysis Phase: Understanding Current State
**Decision Point 1**: How to organize disparate specifications
- **Option A**: Keep existing flat structure with minor improvements
- **Option B**: Create logical sub-modules based on technical domains
- **Option C**: Reorganize by software tool (AQWA, OrcaFlex, etc.)
- **Selected**: Option B - Technical domain organization for better logical flow

**Rationale**: Technical domain organization (ship-dynamics, rao-processing, orcaflex-integration) provides clearest separation of concerns while maintaining natural workflow progression.

### Structure Design Phase: Module Architecture
**Decision Point 2**: Sub-module granularity level
- **Option A**: Single large ship-dynamics module containing everything
- **Option B**: Fine-grained modules (motion-analysis, hydrodynamics, etc.)
- **Option C**: Three balanced sub-modules aligned with user workflows
- **Selected**: Option C - Balanced approach with ship-dynamics, rao-processing, orcaflex-integration

**Rationale**: Aligns with typical marine engineer workflows while maintaining manageable module sizes.

### Content Transformation Phase: Standardization Approach
**Decision Point 3**: How to handle existing comprehensive content
- **Option A**: Summarize and simplify existing specifications
- **Option B**: Preserve full detail while adding standardized structure
- **Option C**: Complete rewrite in new format
- **Selected**: Option B - Preserve technical depth with standardized organization

**Rationale**: Marine engineering domain requires comprehensive technical detail for regulatory compliance and safety-critical applications.

## Prompt Patterns for Similar Reorganization Tasks

### General Pattern: Technical Specification Reorganization

```
CONTEXT: [Description of current state and repository structure]
TASK: Reorganize [domain] specifications to follow repository best practices

REQUIREMENTS:
1. Group related specs into logical sub-modules based on [organizing principle]
2. Create standardized file structure for each:
   - README.md (main specification and overview)
   - tasks.md (implementation tasks with effort estimates)
   - task_summary.md (execution tracking and progress metrics)
   - prompt.md (original prompts and reuse patterns)
   - technical-details.md (deep technical documentation)
   - sub-specs/ folder for component specifications

CONSTRAINTS:
- Preserve all existing technical content
- Follow repository pattern: specs/modules/[domain]/[sub-module]/
- Maintain cross-references and dependencies
- Include comprehensive documentation for [domain] requirements

DELIVERABLES:
- Well-structured specification suite following repository standards
- Executive summaries and implementation roadmaps
- Detailed task breakdowns with effort estimates
- Technical validation and testing requirements
```

### Specific Pattern: Marine Engineering Domain

```
MARINE ENGINEERING REORGANIZATION PATTERN:

DOMAIN ANALYSIS:
- Identify major technical areas (hydrodynamics, motion analysis, software integration)
- Map user workflows (design → analysis → validation)
- Categorize by engineering discipline and analysis type

SUB-MODULE STRUCTURE:
1. [Primary Analysis Module] (ship-dynamics, structural-analysis, etc.)
   - Core engineering calculations and methods
   - Regulatory compliance requirements
   - Performance and validation standards

2. [Data Processing Module] (rao-processing, data-import, etc.)
   - Multi-format data import capabilities
   - Quality validation and interpolation
   - Standardized output formats

3. [Software Integration Module] (orcaflex-integration, aqwa-integration, etc.)
   - External software API integration
   - File format handling and conversion
   - Workflow automation capabilities

CONTENT REQUIREMENTS:
- Comprehensive technical specifications with engineering detail
- Implementation tasks with realistic effort estimates
- Quality standards with validation against experimental data
- Integration requirements with industry-standard software
- Regulatory compliance documentation
```

### Pattern: Complex Technical Domain Reorganization

```
COMPLEX DOMAIN REORGANIZATION TEMPLATE:

PREPARATION PHASE:
1. Analyze existing content structure and technical dependencies
2. Identify user personas and workflow patterns
3. Map current specifications to logical technical domains
4. Design module hierarchy based on engineering principles

STRUCTURE CREATION PHASE:
1. Create directory structure following repository pattern
2. Design standardized file templates for domain
3. Preserve comprehensive technical detail while improving organization
4. Establish clear cross-module dependencies and interfaces

CONTENT TRANSFORMATION PHASE:
1. Transform existing specifications into standardized format
2. Add missing components (tasks, tracking, technical details)
3. Create executive summaries for stakeholder communication
4. Develop comprehensive implementation roadmaps

VALIDATION PHASE:
1. Ensure all original content preserved and enhanced
2. Verify logical flow and technical accuracy
3. Validate compliance with repository standards
4. Check cross-references and dependency completeness
```

## Domain-Specific Insights for Reuse

### Marine Engineering Domain Characteristics
**Technical Complexity**: Requires comprehensive detail for safety-critical applications
**Regulatory Requirements**: Must include compliance with maritime industry standards
**Software Integration**: Heavy reliance on specialized engineering software (ANSYS AQWA, OrcaFlex)
**Validation Needs**: Experimental data comparison essential for credibility
**User Expertise**: Target users have deep domain knowledge and high quality expectations

### Key Success Factors for Marine Engineering Reorganization
1. **Preserve Technical Depth**: Marine engineers require comprehensive technical detail
2. **Maintain Regulatory Context**: Include relevant standards and compliance requirements
3. **Software Integration Focus**: Emphasize integration with industry-standard tools
4. **Validation Strategy**: Include experimental validation and benchmarking plans
5. **Implementation Realism**: Provide realistic effort estimates based on technical complexity

### Common Challenges and Solutions

#### Challenge: Maintaining Technical Accuracy During Reorganization
**Solution**: Preserve original technical content while adding organizational structure
**Pattern**: Copy detailed content into technical-details.md, create executive summaries for README.md

#### Challenge: Complex Cross-Module Dependencies
**Solution**: Clear dependency mapping and interface documentation
**Pattern**: Document dependencies in each module README, create integration tasks

#### Challenge: Balancing Detail with Usability
**Solution**: Layered documentation approach with executive summaries and detailed technical sections
**Pattern**: README.md for overview, technical-details.md for implementation specifics

## Curated Reuse Prompt for Similar Projects

### For Marine Engineering Specification Reorganization:

```
MARINE ENGINEERING SPECIFICATION REORGANIZATION

You are tasked with reorganizing marine engineering specifications following repository best practices.

CONTEXT:
- Working with safety-critical marine engineering domain
- Target users: Naval architects, marine engineers, offshore designers  
- Must preserve comprehensive technical detail for regulatory compliance
- Integration with industry software (ANSYS AQWA, OrcaFlex, etc.) essential

STRUCTURE REQUIREMENTS:
- Follow pattern: specs/modules/marine-engineering/[sub-module]/
- Create balanced sub-modules: ship-dynamics, rao-processing, orcaflex-integration
- Include standardized files: README.md, tasks.md, task_summary.md, prompt.md, technical-details.md
- Add sub-specs/ folder for component specifications

CONTENT TRANSFORMATION:
- Preserve all existing technical content and regulatory references
- Add executive summaries for stakeholder communication  
- Create detailed implementation tasks with realistic marine engineering effort estimates
- Include validation requirements with experimental data comparison
- Document software integration requirements and API dependencies

QUALITY STANDARDS:
- Technical accuracy verified against marine engineering standards
- Implementation tasks include regulatory compliance checkpoints
- Integration testing with industry-standard marine analysis software
- Comprehensive validation against experimental and benchmark data

DELIVERABLES:
- Well-organized specification suite following repository standards
- Implementation roadmap with 12+ month timeline for complex marine systems
- Technical validation strategy with industry benchmark comparisons
- Cross-module integration plan with clear dependency management
```

### For General Technical Domain Reorganization:

```
TECHNICAL DOMAIN SPECIFICATION REORGANIZATION

PREPARATION:
1. Analyze existing specifications for technical domains and user workflows
2. Identify major technical areas and logical grouping patterns
3. Map cross-dependencies and integration requirements
4. Design sub-module structure based on engineering principles

EXECUTION:  
1. Create directory structure: specs/modules/[domain]/[sub-module]/
2. Transform content preserving technical depth while improving organization
3. Add standardized components: tasks, tracking, prompt documentation
4. Create executive summaries and comprehensive implementation plans

VALIDATION:
1. Verify all original content preserved and logically organized
2. Check technical accuracy and regulatory compliance where applicable  
3. Validate implementation feasibility with realistic effort estimates
4. Confirm cross-module integration and dependency clarity

DELIVERABLES:
- Comprehensive specification suite following repository best practices
- Clear module organization with logical technical separation
- Detailed implementation roadmap with effort estimates and milestones
- Quality assurance framework with appropriate validation requirements
```

## Lessons Learned for Future Applications

### What Worked Well
1. **Comprehensive Analysis First**: Taking time to understand existing content and structure before reorganizing
2. **Domain Expertise Integration**: Incorporating marine engineering domain knowledge into organization decisions
3. **Balanced Granularity**: Creating sub-modules that align with user workflows without being too fragmented
4. **Content Preservation**: Maintaining technical depth while improving organization structure

### What Could Be Improved  
1. **Earlier Stakeholder Input**: Could have benefited from marine engineer review during structure design
2. **Integration Testing Plan**: Could have included more specific integration testing scenarios
3. **Performance Benchmarks**: Could have included more detailed performance acceptance criteria
4. **Regulatory Mapping**: Could have included more comprehensive regulatory compliance mapping

### Key Success Metrics for Future Projects
1. **Content Preservation**: 100% of original technical content maintained or enhanced
2. **Organizational Clarity**: Clear logical flow from user workflow perspective
3. **Implementation Feasibility**: Realistic effort estimates based on domain complexity
4. **Quality Standards**: Comprehensive validation strategy appropriate for domain criticality

---

*This prompt documentation provides comprehensive guidance for reusing successful specification reorganization patterns in marine engineering and other complex technical domains.*