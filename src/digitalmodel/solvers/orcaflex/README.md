# OrcaFlex Module

## Overview
The OrcaFlex module provides comprehensive Python integration with OrcaFlex for offshore engineering analysis, including hydrodynamic analysis, mooring systems, and vessel dynamics.

## 🏗️ Standardized Folder Structure

### MANDATORY: All OrcaFlex analyses must follow this structure
```
<analysis_directory>/
├── .dat/                    # OrcaFlex data files
│   ├── original/           # Original unmodified files
│   ├── modified/           # Modified during iteration
│   └── archive/            # Timestamped archives
│
├── .sim/                    # OrcaFlex simulation files
│   ├── baseline/           # Initial results
│   ├── iterations/         # Iteration results
│   └── final/              # Converged results
│
├── configs/                 # Configuration files
├── results/                 # Analysis outputs
│   ├── csv/
│   ├── plots/
│   └── reports/
├── logs/                    # Execution logs
└── scripts/                 # Analysis scripts
```

### Key Benefits
- **Repeatability**: Consistent file locations across all analyses
- **Organization**: Clear separation of file types and versions
- **Automation**: Scripts know exactly where to find files
- **Version Control**: .dat/original tracked, .sim files can be gitignored

See [ORCAFLEX_FOLDER_STANDARD.md](../../../specs/modules/orcaflex/mooring-tension-iteration/ORCAFLEX_FOLDER_STANDARD.md) for complete details.

## Quick Links

[OrcaFlex Documentation](https://www.orcina.com/webhelp/OrcaFlex/Default.htm)

[OrcaWave Documentation](https://www.orcina.com/webhelp/OrcaWave/Default.htm)

[OrcFxAPI Documentation](https://www.orcina.com/webhelp/OrcFxAPI/Default.htm)
