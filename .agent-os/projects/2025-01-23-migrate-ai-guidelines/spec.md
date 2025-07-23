# Migrate Generic AI Guidelines from .ai to .agent-os

## Overview
Migrate reusable AI guidelines, standards, and patterns from the `.ai` directory to the Agent OS structure in `.agent-os`, consolidating guidance for AI agents while preserving project-specific content.

## Background
The project currently has two AI guidance systems:
- `.ai/` - Contains extensive AI guidelines, code standards, and project context
- `.agent-os/` - Recently implemented Agent OS structure with basic documentation

Many guidelines in `.ai/` are generic and would be better organized under Agent OS standards, while project-specific content should remain in `.ai/`.

## Goals
- Consolidate AI communication standards into Agent OS
- Migrate generic code standards to Agent OS structure
- Enhance Agent OS architecture documentation with existing patterns
- Eliminate duplication between the two systems
- Maintain backward compatibility during transition
- Preserve all existing functionality and guidance

## User Stories

### Primary Story
As an AI coding assistant, I want consolidated guidance in the Agent OS structure so that I can quickly understand project standards without navigating multiple documentation systems.

### Supporting Stories
- As a developer, I want clear separation between generic standards and project-specific guidance
- As a team member, I want consistent AI behavior across all agents working on the project
- As a maintainer, I want a single source of truth for AI standards to reduce maintenance overhead

## Technical Design

### Migration Mapping

#### 1. AI Communication Standards
**Source:** `.ai/AI_GUIDELINES.md` (lines 14-81)
**Target:** `.agent-os/standards/ai-communication.md`
- Communication style guidelines
- No-sycophancy principles
- Banned phrases and language patterns
- Writing style requirements

#### 2. Enhanced Code Standards
**Source:** `.ai/code-guidance/python-standards.md`
**Target:** Merge into `.agent-os/standards/code-style.md`
- Engineering domain conventions
- Configuration patterns
- Error handling standards
- Import organization

#### 3. Architecture Patterns
**Source:** `.ai/code-guidance/architecture-patterns.md`
**Target:** Enhance `.agent-os/product/architecture.md`
- Vertical slice architecture details
- Configuration-driven design patterns
- Module structure patterns
- Data flow patterns

#### 4. Testing Standards Enhancement
**Source:** `.ai/AI_GUIDELINES.md` (OrcaFlex testing section)
**Target:** Enhance `.agent-os/standards/testing.md`
- Mock API patterns for licensed software
- Engineering-specific test requirements
- File structure conventions

#### 5. Project Context Integration
**Source:** `.ai/project-context.md`
**Target:** Enhance `.agent-os/product/overview.md`
- Development workflow
- Special considerations
- Domain expertise context

#### 6. AI Agent Configuration
**Source:** `.ai/agent-personas/`
**Target:** `.agent-os/standards/agent-personas.md`
- Standardize agent role definitions
- Create reusable persona templates

### Files to Migrate

#### Complete Migration (Move to .agent-os)
```
.ai/AI_GUIDELINES.md → .agent-os/standards/ai-communication.md
.ai/code-guidance/python-standards.md → Merge into .agent-os/standards/code-style.md
.ai/code-guidance/architecture-patterns.md → Merge into .agent-os/product/architecture.md
.ai/agent-personas/ → .agent-os/standards/agent-personas.md
```

#### Partial Migration (Enhance existing .agent-os files)
```
.ai/project-context.md → Enhance .agent-os/product/overview.md
.ai/README.md → Update references to point to .agent-os
```

#### Keep in .ai (Project-specific)
```
.ai/specs/modules/ (module-specific documentation)
.ai/commands/ (project automation commands)
.ai/implementation-history/ (historical tracking)
.ai/requirements/ (project requirements)
```

## Implementation Plan

### Phase 1: Content Analysis and Preparation
- [ ] Audit all .ai files for generic vs project-specific content
- [ ] Create migration mapping document
- [ ] Identify content overlaps and conflicts
- [ ] Plan content consolidation strategy

### Phase 2: Core Standards Migration
- [ ] Create `.agent-os/standards/ai-communication.md`
- [ ] Enhance `.agent-os/standards/code-style.md` with engineering patterns
- [ ] Merge architecture patterns into `.agent-os/product/architecture.md`
- [ ] Enhance `.agent-os/standards/testing.md` with OrcaFlex patterns

### Phase 3: Agent Configuration
- [ ] Migrate agent personas to `.agent-os/standards/agent-personas.md`
- [ ] Create standardized agent role definitions
- [ ] Document agent interaction patterns

### Phase 4: Integration and Updates
- [ ] Update `.agent-os/README.md` with comprehensive guidance
- [ ] Create cross-reference guide between .ai and .agent-os
- [ ] Update CLAUDE.md to reference Agent OS structure
- [ ] Create migration completion checklist

### Phase 5: Cleanup and Optimization
- [ ] Remove duplicated content from .ai directory
- [ ] Update all internal references
- [ ] Create legacy documentation for deprecated patterns
- [ ] Validate AI agent compatibility

## Testing Strategy

### Content Validation
- Verify all guidelines are preserved during migration
- Test AI agent behavior with new structure
- Validate cross-references work correctly

### Integration Testing
- Test both .ai and .agent-os structures work together
- Verify no broken links or missing references
- Validate development workflow continues smoothly

### User Acceptance Testing
- AI agents can find guidance quickly
- Developers understand new structure
- No regression in AI agent effectiveness

## Success Metrics

### Quantitative
- Reduce documentation duplication by 80%
- Consolidate 6+ files into structured Agent OS format
- Maintain 100% of existing guidance functionality

### Qualitative
- AI agents have clearer, more accessible guidance
- Reduced confusion about where to find standards
- Improved consistency in AI agent behavior
- Simplified maintenance of AI guidelines

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Content loss during migration | High | Create backup and detailed migration mapping |
| Broken AI agent workflows | High | Maintain both structures during transition period |
| Confusion about which docs to use | Medium | Clear documentation and transition guide |
| Incomplete migration | Medium | Comprehensive checklist and validation testing |

## Dependencies

### Internal
- Current .ai directory structure
- Existing Agent OS implementation
- CLAUDE.md configuration

### External
- No external dependencies

## Timeline

**Estimated Duration:** 2-3 days

### Day 1: Analysis and Preparation
- Content audit and migration planning
- Create migration mapping
- Identify conflicts and overlaps

### Day 2: Core Migration
- Migrate AI communication standards
- Enhance code standards
- Update architecture documentation

### Day 3: Integration and Testing
- Agent configuration migration
- Cross-reference updates
- Testing and validation

## Post-Migration Benefits

1. **Single Source of Truth**: Consolidated AI guidance in Agent OS structure
2. **Improved Discoverability**: Standards organized logically
3. **Reduced Maintenance**: Less duplication to maintain
4. **Better AI Agent Performance**: Clearer, more accessible guidance
5. **Standardized Approach**: Follows Agent OS best practices
6. **Future-Proof**: Extensible structure for additional standards