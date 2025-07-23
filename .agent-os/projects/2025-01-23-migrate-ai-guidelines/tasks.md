# Implementation Tasks: Migrate AI Guidelines to Agent OS

## Phase 1: Content Analysis and Preparation

### Content Audit Tasks
- [ ] **Analyze .ai/AI_GUIDELINES.md**
  - [ ] Identify generic AI communication standards (lines 14-81)
  - [ ] Separate OrcaFlex testing guidance (lines 115-176)
  - [ ] Document project-specific vs generic content
  
- [ ] **Review .ai/code-guidance/ directory**
  - [ ] Compare python-standards.md with existing .agent-os/standards/code-style.md
  - [ ] Identify enhancement opportunities for architecture patterns
  - [ ] Map content to appropriate Agent OS locations

- [ ] **Evaluate .ai/agent-personas/ directory**
  - [ ] Review assigned-engineer.md, assigned-product-manager.md, etc.
  - [ ] Determine which personas are reusable vs project-specific
  - [ ] Plan persona standardization approach

- [ ] **Create detailed migration mapping**
  - [ ] Document source → target mappings for all content
  - [ ] Identify content conflicts and overlaps
  - [ ] Plan content consolidation strategy

### Environment Preparation
- [ ] **Create migration branch**
  - [ ] Create feature branch: `feature/migrate-ai-guidelines`
  - [ ] Set up development environment
  - [ ] Create backup of current .ai directory

## Phase 2: Core Standards Migration

### AI Communication Standards
- [ ] **Create .agent-os/standards/ai-communication.md**
  - [ ] Migrate communication style guidelines from AI_GUIDELINES.md
  - [ ] Include no-sycophancy principles and banned phrases
  - [ ] Add writing style requirements and formatting standards
  - [ ] Organize content into clear sections

- [ ] **Enhance .agent-os/standards/code-style.md**
  - [ ] Merge engineering domain conventions from python-standards.md
  - [ ] Add configuration patterns and YAML standards
  - [ ] Include offshore engineering naming conventions
  - [ ] Integrate error handling patterns
  - [ ] Add import organization examples

### Architecture Documentation Enhancement
- [ ] **Update .agent-os/product/architecture.md**
  - [ ] Integrate vertical slice architecture details
  - [ ] Add configuration-driven design patterns
  - [ ] Include module structure patterns
  - [ ] Document data flow patterns
  - [ ] Add integration patterns for external software

### Testing Standards Enhancement
- [ ] **Enhance .agent-os/standards/testing.md**
  - [ ] Add OrcaFlex mock API patterns
  - [ ] Include licensed software testing strategies
  - [ ] Document file structure conventions for tests
  - [ ] Add engineering-specific test patterns
  - [ ] Include configuration testing approaches

## Phase 3: Agent Configuration and Personas

### Agent Personas Migration
- [ ] **Create .agent-os/standards/agent-personas.md**
  - [ ] Extract reusable persona patterns from .ai/agent-personas/
  - [ ] Standardize persona definitions
  - [ ] Create template for new personas
  - [ ] Document persona usage guidelines

### Project Context Integration
- [ ] **Enhance .agent-os/product/overview.md**
  - [ ] Integrate development workflow from project-context.md
  - [ ] Add special considerations section
  - [ ] Include domain expertise context
  - [ ] Document YAML configuration importance

## Phase 4: Integration and Cross-References

### Documentation Updates
- [ ] **Update .agent-os/README.md**
  - [ ] Add comprehensive usage guide
  - [ ] Document relationship with .ai directory
  - [ ] Create quick reference section
  - [ ] Add migration notes

- [ ] **Create .agent-os/migration-guide.md**
  - [ ] Document what was migrated and why
  - [ ] Explain new structure vs old structure
  - [ ] Provide transition guide for users
  - [ ] Include troubleshooting section

### Cross-Reference Updates
- [ ] **Update .ai/README.md**
  - [ ] Add references to Agent OS structure
  - [ ] Explain division of responsibilities
  - [ ] Point to .agent-os for generic standards
  
- [ ] **Update CLAUDE.md**
  - [ ] Reference Agent OS structure first
  - [ ] Maintain backward compatibility notes
  - [ ] Update instruction hierarchy

## Phase 5: Cleanup and Optimization

### Content Cleanup
- [ ] **Remove duplicated content from .ai**
  - [ ] Archive migrated content with deprecation notes
  - [ ] Update remaining files to reference Agent OS
  - [ ] Clean up unused or outdated files
  
- [ ] **Optimize .agent-os structure**
  - [ ] Ensure consistent formatting across all files
  - [ ] Add proper cross-references between documents
  - [ ] Validate all internal links work
  - [ ] Check for completeness of migrated content

### Validation and Testing
- [ ] **Content validation**
  - [ ] Verify all guidelines preserved during migration
  - [ ] Check for missing or corrupted content
  - [ ] Validate cross-references and links
  
- [ ] **Integration testing**
  - [ ] Test AI agent behavior with new structure
  - [ ] Verify development workflow still works
  - [ ] Check that both .ai and .agent-os work together
  
- [ ] **Documentation review**
  - [ ] Proofread all migrated content
  - [ ] Ensure consistent terminology
  - [ ] Validate technical accuracy

## Quality Assurance Tasks

### Pre-Migration Checklist
- [ ] All source content identified and mapped
- [ ] Migration strategy documented and approved
- [ ] Backup of original .ai directory created
- [ ] Test environment prepared

### Post-Migration Checklist
- [ ] All planned content successfully migrated
- [ ] No broken links or references
- [ ] AI agent functionality preserved
- [ ] Development workflow unaffected
- [ ] Documentation updated and accurate

### Final Validation
- [ ] **AI Agent Testing**
  - [ ] Test AI agent can find and use new guidance
  - [ ] Verify no regression in AI agent effectiveness
  - [ ] Check AI agents follow new communication standards
  
- [ ] **Developer Experience Testing**
  - [ ] Developers can navigate new structure
  - [ ] Standards are clear and accessible
  - [ ] No confusion about which docs to use

## Rollback Plan
- [ ] **If migration fails:**
  - [ ] Restore original .ai directory from backup
  - [ ] Remove incomplete .agent-os changes
  - [ ] Document lessons learned
  - [ ] Plan revised approach

## Success Criteria
- [ ] ✅ All generic AI guidelines migrated to Agent OS
- [ ] ✅ No loss of existing functionality or guidance
- [ ] ✅ Clear separation between generic and project-specific content
- [ ] ✅ Improved documentation organization and discoverability
- [ ] ✅ AI agents can effectively use new structure
- [ ] ✅ Development workflow preserved and enhanced

---
*Note: Check off tasks as completed and add new ones as discovered during implementation*