# Workflow Refresh Specification

> Created: 2025-08-04
> Version: 1.0.0
> Command: /create-spec
> Scope: All repositories

## Overview

This specification defines workflow refresh behaviors for the `/create-spec` slash command across all Agent OS repositories. It ensures consistent spec creation processes and maintains alignment with the latest Agent OS standards.

## Workflow Refresh Requirements

### Pre-Spec Creation Validation

Before creating any new specification, the `/create-spec` command must:

1. **Validate Agent OS Installation**
   - Verify `.agent-os/product/` directory exists
   - Check for required files: `mission.md`, `roadmap.md`, `tech-stack.md`, `decisions.md`
   - Confirm `CLAUDE.md` file references Agent OS documentation

2. **Refresh Agent OS Standards**
   - Read latest standards from `@~/.agent-os/standards/`
   - Update local cache of global standards
   - Validate project-specific overrides in `.agent-os/product/`

3. **Roadmap Synchronization**
   - Parse current roadmap phase and priorities
   - Identify next logical feature or task
   - Validate spec alignment with product mission

### Dynamic Spec Generation

The spec creation process must adapt to:

1. **Repository Context**
   - Detect primary technology stack from existing files
   - Identify established patterns and conventions
   - Consider repository-specific requirements

2. **Product Phase Awareness**
   - Adjust complexity based on product maturity
   - Prioritize foundational features for new products
   - Focus on optimization for mature products

3. **Team Capability Assessment**
   - Consider documented team preferences
   - Adapt technical depth to documented skill levels
   - Include appropriate learning resources

### Cross-Repository Consistency

Ensure specs maintain consistency across repositories:

1. **Standard Templates**
   - Use consistent spec.md structure
   - Apply standard sub-spec categories
   - Maintain uniform task breakdown format

2. **Quality Gates**
   - Verify all specs include test specifications
   - Ensure technical specifications are complete
   - Validate task breakdown follows TDD principles

3. **Documentation Links**
   - Create proper cross-references between specs
   - Link to relevant global standards
   - Reference applicable product decisions

## Implementation Requirements

### Command Enhancement

The `/create-spec` command must be enhanced with:

1. **Context Analysis Phase**
   ```
   /create-spec [feature-description]
   → Analyzing repository context...
   → Validating Agent OS installation...
   → Checking roadmap alignment...
   → Creating specification...
   ```

2. **Validation Checkpoints**
   - Pre-creation validation
   - Mid-creation quality checks
   - Post-creation verification
   - Cross-reference validation

3. **Error Handling**
   - Missing Agent OS installation → Offer to install
   - Outdated standards → Prompt for refresh
   - Misaligned specs → Suggest roadmap review

### Repository Application

This workflow refresh must be applied to:

1. **All Existing Repositories**
   - aceengineer-admin, aceengineer-website, aceengineercode
   - achantas-data, achantas-media, acma-projects
   - ai-native-traditional-eng, assethold, assetutilities
   - client_projects, digitalmodel, doris, energy
   - frontierdeepwater, hobbies, investments
   - Lightning-SPA-App, OGManufacturing, pyproject-starter
   - rock-oil-field

2. **Future Repositories**
   - Automatic application on Agent OS installation
   - Template inclusion in new repository setup
   - Version control for workflow updates

### Monitoring and Feedback

Track workflow effectiveness through:

1. **Spec Quality Metrics**
   - Completeness scores
   - Implementation success rates
   - Time-to-completion tracking

2. **User Experience Metrics**
   - Command usage frequency
   - Error rates and resolution
   - User satisfaction feedback

3. **Cross-Repository Analysis**
   - Consistency measurements
   - Best practice identification
   - Anti-pattern detection

## Success Criteria

1. **Consistency Achievement**
   - All repositories generate specs with consistent structure
   - 95% compliance with Agent OS standards
   - Zero missing required sections

2. **Quality Improvement**
   - 90% of specs include comprehensive test coverage
   - 100% of specs align with product roadmaps
   - All technical specifications are implementable

3. **Developer Experience**
   - Sub-30-second spec creation time
   - Intuitive error messages and guidance
   - Self-correcting workflow for common issues

## Rollout Plan

1. **Phase 1: Core Implementation**
   - Implement workflow refresh logic
   - Test on 3 pilot repositories
   - Gather initial feedback

2. **Phase 2: Repository Deployment**
   - Deploy to all 20 repositories
   - Monitor for consistency issues
   - Address edge cases

3. **Phase 3: Optimization**
   - Refine based on usage patterns
   - Add advanced features
   - Document best practices

## Maintenance

This workflow specification requires:

1. **Regular Updates**
   - Quarterly review of effectiveness
   - Updates for new Agent OS features
   - Alignment with evolving standards

2. **Version Control**
   - Semantic versioning for workflow changes
   - Backward compatibility considerations
   - Migration guides for breaking changes

3. **Documentation**
   - Keep implementation guide current
   - Maintain troubleshooting resources
   - Update examples and templates