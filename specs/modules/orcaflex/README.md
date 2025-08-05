# OrcaFlex Module Specifications

This directory contains detailed specifications for troubleshooting and configuring OrcaFlex post-processing workflows.

## Available Specifications

### 🔧 [Troubleshooting Missing Objects](./troubleshooting-missing-objects.md)
**Use Case**: Fix "NoneType object has no attribute 'name'" errors
- Root cause analysis methodology
- Code fix implementation in digitalmodel
- Testing procedures for validation
- Prevention strategies

### ⚙️ [Sequential Processing Configuration](./sequential-processing-configuration.md)  
**Use Case**: Configure OrcaFlex post-processing workflows
- Standard YAML configuration format
- Workflow management for sequential execution
- Common configuration patterns and errors
- Directory structure guidelines

## Quick Reference

### Common Issues & Solutions
| Error Type | Spec Reference | Quick Fix |
|------------|----------------|-----------|
| `'NoneType' object has no attribute 'name'` | [troubleshooting-missing-objects.md](./troubleshooting-missing-objects.md) | Apply safe object access patterns |
| `'summary_settings' KeyError` | [sequential-processing-configuration.md](./sequential-processing-configuration.md) | Add missing configuration section |
| `Object 'Line' not found in model` | [sequential-processing-configuration.md](./sequential-processing-configuration.md) | Use actual object names, not generic types |

### Configuration Templates
```bash
# Minimal test configuration
cp templates/orcaflex_test_single.yml postproc/test_[scenario].yml

# Full processing configuration  
cp templates/orcaflex_post_process_full.yml postproc/dm_fsts_[scenario].yml
```

### Standard Workflow
1. **Test Single File**: Validate configuration with one sim file
2. **Fix Object Issues**: Apply troubleshooting spec if errors occur  
3. **Configure Batch**: Set up sequential processing configuration
4. **Execute Workflow**: Run full batch processing

## File Locations

### Code Files (Apply fixes here)
- `D:\github\digitalmodel\src\digitalmodel\modules\orcaflex\orcaflex_objects.py`
- `D:\github\digitalmodel\src\digitalmodel\modules\orcaflex\all_vars.py`

### Configuration Files  
- `postproc/dm_fsts_*.yml` - Production configurations
- `postproc/test_*.yml` - Test configurations
- `postproc/run_sequential.py` - Batch execution script

### Output Locations
- `output/csv/[scenario]/` - Processed results
- `[scenario]/*.sim_error.log` - Error logs

## Getting Help

1. **Check Error Logs**: Look for patterns in `*.sim_error.log` files
2. **Follow Troubleshooting Spec**: Use systematic approach in troubleshooting-missing-objects.md
3. **Validate Configuration**: Use checklists in sequential-processing-configuration.md
4. **Test Incrementally**: Start with minimal config, add features gradually

## Contributing

When adding new specs or updating existing ones:
- Follow the same structure and format
- Include practical examples and templates
- Add troubleshooting checklists
- Reference related specifications
- Test procedures with actual scenarios

---
*Last Updated: 2025-08-05*
*Related Modules: orcaflex, post-processing, workflow management*