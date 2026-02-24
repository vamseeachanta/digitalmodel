# OrcaFlex Folder Structure Standardization - Tasks

## Phase 1: Foundation (Week 1)

### 1.1 Create Standard Templates
- [ ] Create folder structure template generator script
- [ ] Develop standard YAML configuration templates
- [ ] Create README.md template for modules
- [ ] Develop USAGE.md template with examples
**Estimated Time**: 4 hours

### 1.2 Update Core Modules
- [ ] Update mooring.py to use standard paths
- [ ] Modify visualization.py for standard output
- [ ] Update base configurations in src/digitalmodel/base_configs/modules/orcaflex/
- [ ] Ensure all modules support relative paths
**Estimated Time**: 6 hours

### 1.3 Create Validation Tools
- [ ] Develop path validation script
- [ ] Create folder structure checker
- [ ] Build configuration validator
- [ ] Add pre-commit hooks for validation
**Estimated Time**: 4 hours

## Phase 2: Priority Module Migration (Week 2-3)

### 2.1 Migrate mooring-tension-iteration Module
- [ ] Restructure folders to standard layout
- [ ] Update all configurations to relative paths
- [ ] Move output files to correct locations
- [ ] Update documentation
- [ ] Test complete workflow
**Estimated Time**: 8 hours

### 2.2 Migrate postprocess-optimization Module
- [ ] Apply standard folder structure
- [ ] Convert absolute to relative paths
- [ ] Reorganize output directories
- [ ] Update configuration files
- [ ] Validate functionality
**Estimated Time**: 6 hours

### 2.3 Migrate browser-interface Module
- [ ] Restructure implementation directory
- [ ] Standardize API output paths
- [ ] Update frontend file references
- [ ] Reorganize sample data
- [ ] Test browser functionality
**Estimated Time**: 8 hours

### 2.4 Migrate force-analysis Module
- [ ] Apply folder structure standard
- [ ] Update analysis configurations
- [ ] Reorganize output locations
- [ ] Update documentation
- [ ] Run validation tests
**Estimated Time**: 6 hours

## Phase 3: Complete Migration (Week 4-5)

### 3.1 Migrate Remaining Modules
- [ ] configuration-management module
- [ ] file-ecosystem-structure module
- [ ] mooring-tension-starting-position module
- [ ] orcaflex-examples-integration module
- [ ] orcawave-results-integration module
- [ ] refactor-for-scalability module
- [ ] results-dashboard module
- [ ] universal-sim-runner module
**Estimated Time**: 16 hours (2 hours per module)

### 3.2 Update Test Suites
- [ ] Update all test file paths
- [ ] Modify test configurations
- [ ] Update fixture locations
- [ ] Ensure all tests pass
**Estimated Time**: 8 hours

### 3.3 Update Cross-Module Dependencies
- [ ] Update import statements
- [ ] Fix configuration references
- [ ] Update shared utilities
- [ ] Resolve path dependencies
**Estimated Time**: 6 hours

## Phase 4: Documentation and Validation (Week 6)

### 4.1 Documentation Updates
- [ ] Update main OrcaFlex README.md
- [ ] Create migration guide
- [ ] Document standard structure
- [ ] Add usage examples
- [ ] Create troubleshooting guide
**Estimated Time**: 8 hours

### 4.2 Create Automation Tools
- [ ] Module creation script with standard structure
- [ ] Batch configuration updater
- [ ] Path migration utility
- [ ] Output reorganization tool
**Estimated Time**: 8 hours

### 4.3 Final Validation
- [ ] Run full test suite
- [ ] Validate all module structures
- [ ] Check all configurations
- [ ] Verify output locations
- [ ] Performance testing
**Estimated Time**: 6 hours

## Implementation Checklist

### Immediate Actions
1. [ ] Create folder-structure-standardization module directory
2. [ ] Implement folder template generator
3. [ ] Update mooring.py for standard paths
4. [ ] Create validation scripts

### Configuration Updates Required
1. [ ] All .yml files to use relative paths
2. [ ] Remove all absolute path references
3. [ ] Standardize output directory names
4. [ ] Update visualization output paths

### Code Updates Required
1. [ ] mooring.py - visualization output paths
2. [ ] visualization.py - output directory handling
3. [ ] opp_visualization.py - image save locations
4. [ ] All analysis modules - output path handling

### Testing Requirements
1. [ ] Unit tests for path validation
2. [ ] Integration tests for folder structure
3. [ ] End-to-end workflow tests
4. [ ] Migration validation tests

## Success Metrics
- [ ] 100% relative paths in all configurations
- [ ] Consistent folder structure across all modules
- [ ] All tests passing with new structure
- [ ] Zero breaking changes for existing workflows
- [ ] Improved developer experience metrics

## Risk Items
1. **Breaking existing workflows** - Mitigate with backward compatibility layer
2. **Lost output files during migration** - Create backup before migration
3. **Configuration conflicts** - Validate all configs before migration
4. **Test failures** - Update tests incrementally

## Dependencies
- Python 3.8+ for scripts
- PyYAML for configuration handling
- pathlib for path operations
- shutil for file operations

## Estimated Total Time
- Phase 1: 14 hours
- Phase 2: 28 hours
- Phase 3: 30 hours
- Phase 4: 22 hours
- **Total: 94 hours (approximately 2.5 weeks of focused work)**

## Priority Order
1. **Critical**: mooring-tension-iteration (most active module)
2. **High**: postprocess-optimization, browser-interface
3. **Medium**: force-analysis, results-dashboard
4. **Low**: Other modules

## Notes
- All paths MUST be relative from repository root
- Output organization is critical for review consistency
- Maintain backward compatibility during transition
- Document all changes thoroughly