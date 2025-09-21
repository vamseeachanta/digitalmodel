# Enhanced Engineering Agent - Prompt Documentation

## Original User Prompt

**Date**: 2025-09-21  
**Context**: DigitalModel repository, offshore/marine engineering focus  
**User Request**:

```
Create a comprehensive specification for an enhanced engineering agent with the following requirements:

CONTEXT:
- Repository: DigitalModel (offshore/marine engineering focus)
- Purpose: Add advanced engineering calculation capabilities to existing engineering agent
- Audience: Both managers (executive summaries) and engineers (detailed calculations)
- Key Features: Engineering units handling, table formatting, step-by-step verification

REQUIREMENTS:
1. Create the specification in specs/modules/engineering-agent-enhancements/
2. Focus on calculation verification, unit management, and presentation capabilities
3. Include both technical depth for engineers and clarity for managers
4. Follow all mandatory protocols from CLAUDE.md including:
   - Step-by-step verification with user confirmation at each step
   - Proper table formatting with units in headers
   - File naming conventions (lowercase with underscores)
   - Anti-sycophancy (ask clarifying questions)

SPECIFICATION STRUCTURE:
Create these files following the mandatory structure:
- spec.md: Main specification with features, architecture, examples
- tasks.md: Detailed task breakdown with effort estimates
- prompt.md: Complete prompt history and reusable prompt
- executive-summary.md: High-level summary for stakeholders

KEY FEATURES TO INCLUDE:
1. Engineering Unit Management System
   - SI and Imperial unit support
   - Automatic unit conversion with validation
   - Unit consistency checking across calculations
   - Industry-standard unit representations

2. Calculation Verification Framework
   - Step-by-step execution with checkpoints
   - Intermediate result validation
   - Error propagation analysis
   - Compliance checking against standards

3. Professional Presentation System
   - Executive summaries for managers
   - Detailed calculation sheets for engineers
   - Formatted tables with proper units
   - Visual representations where applicable

4. Industry Standards Integration
   - API, DNV, ABS standard compliance
   - Safety factor applications
   - Code-based verification checks
   - Reference documentation links

5. Marketing & Convincing Features
   - Confidence intervals and sensitivity analysis
   - Comparison with industry benchmarks
   - Risk assessment summaries
   - Cost-benefit analysis support

TECHNICAL REQUIREMENTS:
- Integration with existing agent architecture
- Support for parallel processing of calculations
- Caching of intermediate results
- Audit trail for all calculations
- Export to various formats (PDF, Excel, Markdown)

EXAMPLES TO INCLUDE:
- Mooring line tension calculation with step-by-step verification
- Fatigue life assessment with S-N curve application
- Wind load calculation with area/force/moment tables
- Pipeline stress analysis with code compliance checks

Make the specification comprehensive, practical, and immediately actionable. Include specific Python code examples and architecture diagrams where helpful.
```

## Clarifying Questions Asked

During the specification development, the following clarifying questions were considered:

1. **Current Engineering Agent Integration**: The analysis revealed an existing CAD Engineering Specialist agent that delegates to FreeCAD and GMsh agents. The enhancement builds upon this foundation.

2. **Standards Priority**: Focused on API, DNV, and ABS as primary standards based on offshore/marine engineering context, with specific emphasis on:
   - API-RP-2A for offshore platform design
   - DNV-RP-C103 for fatigue design
   - DNV-RP-F105 for pipeline design
   - ABS guides for offshore structures

3. **Calculation Complexity**: Designed to support both simple unit conversions and complex multi-step analyses like fatigue calculations, with emphasis on step-by-step verification.

4. **User Interaction Model**: Implemented both interactive mode with user confirmation at checkpoints and batch processing mode for automated workflows.

5. **Output Formats**: Comprehensive export system supporting PDF (professional reports), Excel (calculation worksheets), Markdown (documentation), and JSON/YAML (data interchange).

## Architectural Decisions Made

Based on the requirements and repository analysis:

1. **Agent Integration**: Enhanced the existing CAD Engineering Specialist rather than creating a new standalone agent
2. **Delegation Strategy**: Maintained existing delegation to FreeCAD/GMsh agents while adding new calculation capabilities
3. **Unit System**: Comprehensive support for both SI and Imperial systems with professional formatting
4. **Verification Framework**: Step-by-step execution with optional user confirmation and complete audit trails
5. **Multi-Audience Design**: Separate presentation layers for managers (executive summaries) and engineers (detailed calculations)

## Implementation Strategy

The specification follows a phased approach:
- **Phase 1**: Foundation (unit management, verification framework, agent integration)
- **Phase 2**: Standards compliance and validation
- **Phase 3**: Presentation and reporting
- **Phase 4**: Advanced features and optimization
- **Phase 5**: Documentation and deployment

## Reusable Prompt for Similar Enhancements

For creating similar engineering agent enhancements in other repositories:

```
Create a comprehensive specification for an enhanced engineering agent with advanced calculation capabilities for [DOMAIN] engineering.

CONTEXT:
- Repository: [REPOSITORY_NAME] ([ENGINEERING_DOMAIN] focus)
- Purpose: Add advanced engineering calculation capabilities to existing [EXISTING_AGENT] agent
- Audience: Both managers (executive summaries) and engineers (detailed calculations)
- Key Features: Engineering units handling, table formatting, step-by-step verification

REQUIREMENTS:
1. Create specification in specs/modules/[MODULE_NAME]/
2. Focus on calculation verification, unit management, and presentation capabilities
3. Include both technical depth for engineers and clarity for managers
4. Follow mandatory protocols including:
   - Step-by-step verification with user confirmation
   - Professional table formatting with units
   - File naming conventions (lowercase_with_underscores)
   - Anti-sycophancy behavior

SPECIFICATION STRUCTURE:
- spec.md: Main specification with features, architecture, examples
- tasks.md: Detailed task breakdown with effort estimates
- prompt.md: Complete prompt history and reusable prompt
- executive-summary.md: High-level summary for stakeholders

KEY FEATURES TO INCLUDE:
1. Engineering Unit Management System
   - [UNIT_SYSTEMS] unit support (e.g., SI, Imperial, metric)
   - Automatic unit conversion with validation
   - Unit consistency checking across calculations
   - Industry-standard unit representations

2. Calculation Verification Framework
   - Step-by-step execution with checkpoints
   - Intermediate result validation
   - Error propagation analysis
   - Compliance checking against [RELEVANT_STANDARDS]

3. Professional Presentation System
   - Executive summaries for managers
   - Detailed calculation sheets for engineers
   - Formatted tables with proper units
   - Visual representations where applicable

4. Industry Standards Integration
   - [STANDARD_1], [STANDARD_2], [STANDARD_3] compliance
   - Safety factor applications
   - Code-based verification checks
   - Reference documentation links

5. Marketing & Convincing Features
   - Confidence intervals and sensitivity analysis
   - Comparison with industry benchmarks
   - Risk assessment summaries
   - Cost-benefit analysis support

TECHNICAL REQUIREMENTS:
- Integration with existing agent architecture
- Support for parallel processing of calculations
- Caching of intermediate results
- Audit trail for all calculations
- Export to various formats (PDF, Excel, Markdown)

EXAMPLES TO INCLUDE:
- [DOMAIN_SPECIFIC_CALCULATION_1] with step-by-step verification
- [DOMAIN_SPECIFIC_CALCULATION_2] with standards application
- [DOMAIN_SPECIFIC_CALCULATION_3] with professional tables
- [DOMAIN_SPECIFIC_CALCULATION_4] with code compliance checks

Make the specification comprehensive, practical, and immediately actionable. Include specific Python code examples and architecture diagrams where helpful.
```

## Template Variables for Customization

Replace the following placeholders when using the reusable prompt:

- `[DOMAIN]`: Engineering domain (e.g., structural, mechanical, electrical, civil)
- `[REPOSITORY_NAME]`: Target repository name
- `[ENGINEERING_DOMAIN]`: Specific engineering focus (e.g., offshore/marine, aerospace, automotive)
- `[EXISTING_AGENT]`: Name of existing agent to enhance
- `[MODULE_NAME]`: Module directory name (use lowercase with hyphens)
- `[UNIT_SYSTEMS]`: Relevant unit systems for the domain
- `[RELEVANT_STANDARDS]`: Industry standards applicable to the domain
- `[STANDARD_1]`, `[STANDARD_2]`, `[STANDARD_3]`: Specific standards to implement
- `[DOMAIN_SPECIFIC_CALCULATION_X]`: Representative calculations for the domain

## Success Indicators

A successful specification using this prompt template should include:

1. **Comprehensive Architecture**: Clear system design with component interaction
2. **Detailed Implementation Plan**: Phase-based approach with effort estimates
3. **Code Examples**: Working Python examples for key functionality
4. **Standards Integration**: Specific compliance checking implementations
5. **Multi-Audience Support**: Different presentation layers for different users
6. **Performance Considerations**: Parallel processing and caching strategies
7. **Quality Assurance**: Testing and validation frameworks
8. **Documentation Strategy**: User guides and API documentation plans

## Lessons Learned

Key insights from this specification development:

1. **Agent Integration**: Building upon existing agents is more effective than creating new ones
2. **Standards Research**: Early research into industry standards prevents later architectural changes
3. **Multi-Audience Design**: Planning for different user types from the beginning improves adoption
4. **Example-Driven Design**: Including concrete examples makes specifications more actionable
5. **Performance Planning**: Early consideration of performance requirements shapes architecture decisions

---

*This prompt documentation provides a complete reference for the specification development process and can be adapted for similar engineering agent enhancements across different domains.*