# OrcaFlex Folder Structure Standardization

## Overview
This specification defines a consistent folder structure standard for all OrcaFlex-related modules and submodules in the digitalmodel repository.

## 🎯 Purpose
- Ensure consistency across all OrcaFlex modules
- Use relative paths exclusively (no absolute paths)
- Provide predictable output locations for reviewers
- Simplify development and maintenance

## 📁 Standard Structure

```
specs/modules/orcaflex/<module-name>/
├── config/                 # Configuration files
├── input/                  # Input files
│   ├── models/            # .sim, .dat files
│   ├── data/              # .csv, .xlsx data
│   └── parameters/        # Parameter files
├── output/                # All outputs
│   ├── analysis/          # Analysis results (.csv, .json)
│   ├── visual/            # Visualizations (.png, .jpg, .pdf)
│   ├── reports/           # Reports (.pdf, .html, .xlsx)
│   └── logs/              # Execution logs
├── scripts/               # Processing scripts
├── tests/                 # Module tests
└── docs/                  # Documentation
```

## 🚀 Quick Start

### For New Modules
```bash
# Use the template generator (coming soon)
python tools/create-orcaflex-module.py <module-name>
```

### For Existing Modules
```bash
# Run migration script (coming soon)
python tools/migrate-to-standard.py <module-path>
```

## 📋 Key Principles

### 1. Always Use Relative Paths
```yaml
# ✅ CORRECT
input_directory: specs/modules/orcaflex/my-module/input/models

# ❌ WRONG
input_directory: D:/github/digitalmodel/specs/modules/orcaflex/...
```

### 2. Consistent Output Organization
- Analysis results → `output/analysis/`
- Visualizations → `output/visual/`
- Reports → `output/reports/`
- Logs → `output/logs/`

### 3. Standard Naming Conventions
- Module names: `lowercase-with-hyphens`
- Output files: `<model>_<type>_<timestamp>.ext`
- Config files: `<purpose>.yml`

## 📦 Module Examples

### Mooring Analysis
```
mooring-tension-iteration/
├── input/models/          # .sim files
├── input/parameters/      # target_pretension.csv
├── output/analysis/       # *_pretension_analysis.csv
└── output/visual/         # Force plots, stiffness diagrams
```

### Post-Processing
```
postprocess-optimization/
├── input/models/          # .sim files
├── input/data/           # Time series data
├── output/analysis/      # Processed results
└── output/reports/       # Summary reports
```

## 🔄 Migration Status

| Module | Status | Priority |
|--------|--------|----------|
| mooring-tension-iteration | ✅ Completed | Critical |
| postprocess-optimization | 🔄 In Progress | High |
| browser-interface | ⏳ Pending | High |
| force-analysis | ⏳ Pending | Medium |
| Other modules | ⏳ Pending | Low |

## 📚 Documentation

- [Full Specification](spec.md) - Detailed technical specification
- [Implementation Tasks](tasks.md) - Task breakdown and timeline
- [Migration Guide](docs/migration-guide.md) - Step-by-step migration instructions

## 🛠️ Tools

### Validation
```bash
# Validate module structure
python tools/validate-structure.py <module-path>

# Check path compliance
python tools/check-paths.py <config-file>
```

### Migration
```bash
# Migrate single module
python tools/migrate-module.py <module-path>

# Batch migration
python tools/batch-migrate.py --modules <list>
```

## ✅ Compliance Checklist

Before committing changes:
- [ ] All paths are relative from repository root
- [ ] Output directories follow standard structure
- [ ] File names follow naming conventions
- [ ] Configuration uses standard template
- [ ] Documentation is updated
- [ ] Tests pass with new structure

## 🤝 Contributing

When creating new OrcaFlex modules:
1. Use the standard folder structure
2. Copy configuration template
3. Follow naming conventions
4. Document in module README
5. Add to migration status table

## 📞 Support

For questions or issues:
- Check [Migration Guide](docs/migration-guide.md)
- Review [Troubleshooting](docs/troubleshooting.md)
- Open issue with tag `orcaflex-structure`

## 🎯 Benefits

- **Consistency**: Same structure everywhere
- **Predictability**: Reviewers know where to find outputs
- **Maintainability**: Easier to update and extend
- **Portability**: Works on any system (relative paths)
- **Automation**: Enables tooling and validation

---

*Last Updated: 2025-09-01*
*Specification Version: 1.0.0*