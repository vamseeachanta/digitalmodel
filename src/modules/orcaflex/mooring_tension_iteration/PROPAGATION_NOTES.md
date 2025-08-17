# OrcaFlex Module Notes (DIGITALMODEL REPOSITORY ONLY)

## ⚠️ WARNING: OrcaFlex Module is Repository-Specific

**CRITICAL**: This module belongs ONLY to the DigitalModel repository.
**DO NOT PROPAGATE** OrcaFlex-specific code to other repositories.

## Module-Specific Features (NOT FOR PROPAGATION)

### OrcaFlex-Specific Features (DigitalModel Only):
1. **File Type Detection System** (`file_type_detector.py`)
   - Distinguishes between OrcaFlex models and DigitalModel configs
   - Validates batch configurations
   - Classifies entire directories

2. **Parallel Batch Processing** (`orcaflex_batch_runner.py`)
   - ThreadPoolExecutor for concurrent processing
   - Configurable worker limits
   - Thread-safe logging

3. **Simulation File Verification**
   - Automatic .sim file creation verification
   - File size checking
   - Missing file detection

4. **Comprehensive Test Suite** (`test_batch_runner.py`)
   - 20+ test cases
   - Mock mode for CI/CD
   - License-based test skipping

## What CAN Be Propagated (Generic Components Only)

### Generic Patterns Extracted from This Work:
1. **Background Test Runner** (see `.agent-os/commands/run_tests_background.py`)
2. **Parallel Processing Pattern** (concept only, not OrcaFlex implementation)
3. **File Type Detection Pattern** (pattern only, not OrcaFlex logic)

### Correct Propagation Command for Generic Components:
```bash
# Propagate ONLY generic background test runner
/propagate-commands \
  --files ".agent-os/commands/run_tests_background.py" \
  --files ".agent-os/commands/GENERIC_PROPAGATION_NOTES.md" \
  --exclude "src/modules/*" \
  --targets all
```

## Files to Propagate

### Core Implementation:
```
src/modules/orcaflex/mooring_tension_iteration/
├── batch_processing/
│   └── orcaflex_batch_runner.py
├── file_type_detector.py
└── run_orcaflex_agent.py
```

### Test Suite:
```
tests/modules/orcaflex/mooring_tension_iteration/
├── test_batch_runner.py
├── test_with_actual_models.py
└── run_tests.py
```

### Agent Configuration:
```
agents/orcaflex/workflows/
└── mooring_tension_iteration.yaml
```

## Integration Points

### 1. Update CLAUDE.md in Each Repository
Add to the module agent section:
- OrcaFlex batch processing capabilities
- File type detection features
- Parallel processing configuration

### 2. Update Agent Configuration
Ensure each repository's OrcaFlex agent includes:
- Batch processing workflow
- File type detection capability
- Parallel processing settings

### 3. Test Integration
Each repository should run:
```bash
python tests/modules/orcaflex/mooring_tension_iteration/run_tests.py
```

## Benefits for Other Repositories

1. **Faster Processing**: Parallel execution reduces batch time by ~4x
2. **Better File Management**: Automatic detection prevents wrong file usage
3. **Robust Testing**: Comprehensive test suite ensures reliability
4. **License Flexibility**: Works in mock mode without OrcaFlex license

## Repositories to Update

Priority repositories for this enhancement:
1. AssetUtilities (hub)
2. FrontierDeepwater
3. DigitalModel
4. All repositories with OrcaFlex integration

## Validation Checklist

After propagation, verify in each repository:
- [ ] File type detector correctly identifies OrcaFlex models
- [ ] Batch runner processes models in parallel
- [ ] Tests pass in mock mode
- [ ] .sim files are generated (with license)
- [ ] Module-based organization maintained

## Notes

- This enhancement follows the module-based repository pattern
- All files are in proper `modules/<module>/` structure
- Tests use actual OrcaFlex models from test directory
- Unicode characters replaced with ASCII for compatibility

## Command Creation Opportunity

💡 **Slash Command Opportunity Detected!**
This propagation process would be valuable as `/propagate-orcaflex-batch`
Benefits: 
- Automated propagation to all repos
- Consistent implementation
- Version tracking

Create as slash command? (Recommended)

---
*Last Updated: 2025-01-17*
*Ready for propagation via `/propagate-commands`*