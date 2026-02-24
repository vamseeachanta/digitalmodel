# Plan: OrcaFlex Monolithic to Modular Converter

## Overview

Convert monolithic OrcaFlex YAML files to modular include format for maintainability, reusability, and parametric analysis.

## Skill Reference

See: `.claude/skills/orcaflex-monolithic-to-modular/SKILL.md`

## Implemented Structure (2026-01-21)

```
docs/modules/orcaflex/pipeline/installation/floating/
└── 30in_pipeline/
    ├── monolithic.yml           # Original 365KB single-file
    └── modular/
        ├── master.yml           # Entry point with includes
        ├── includes/
        │   ├── 01_general.yml
        │   ├── 02_var_data.yml
        │   ├── 03_environment.yml
        │   ├── 05_line_types.yml
        │   ├── 07_lines.yml
        │   ├── 08_buoys.yml
        │   ├── 09_shapes.yml
        │   ├── 10_groups.yml
        │   ├── 13_supports.yml
        │   └── 14_morison.yml
        └── inputs/
            └── parameters.yml
```

## Lessons Learned

### Naming Conventions

| Before | After | Rationale |
|--------|-------|-----------|
| `5_tug_env_6D_buoys/` | `30in_pipeline/` | Descriptive, meaningful |
| `original.yml` | `monolithic.yml` | Clear format identification |
| Files in root | `modular/` subdirectory | Clear separation |

### Include Ordering

Critical dependency order:
1. General settings → Environment → Types → Supports → Shapes → Buoys → Lines → Groups

### Key Takeaways

1. **Preserve original**: Keep monolithic.yml for reference and validation
2. **Extract parameters**: Enable parametric studies via inputs/parameters.yml
3. **Test immediately**: Load master.yml in OrcaFlex after conversion
4. **Document dependencies**: Include order comments in master.yml

## Next Steps

- [ ] Create Python automation script for conversion
- [ ] Add batch conversion support
- [ ] Implement validation comparison tool
- [ ] Create parametric study workflow

## Commits

- `dd75a032` - refactor: Reorganize floating installation example to 30in_pipeline
