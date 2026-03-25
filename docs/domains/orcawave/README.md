# OrcaWave Diffraction Analysis

**ABOUTME**: Comprehensive documentation and automation tools for Orcina OrcaWave panel method diffraction analysis with direct integration to OrcaFlex.

---

## 📚 Quick Navigation

### ⚡ Quick Start & CLI Tools
- **[QUICK START GUIDE](diffraction/QUICK_START.md)** - Fast setup and CLI usage
- **[Diffraction Capabilities Expansion Plan](DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md)** - Roadmap and enhancements
- **Command Line Interface** - Run analyses from terminal:
  ```bash
  # OrcaWave direct CLI
  python src/digitalmodel/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress

  # Unified Diffraction CLI (AQWA + OrcaWave)
  python src/digitalmodel/modules/diffraction_cli.py orcawave --vessel sea_cypress

  # List available vessels
  python src/digitalmodel/modules/diffraction_cli.py orcawave --list-vessels

  # Dry run (validation only)
  python src/digitalmodel/modules/diffraction_cli.py orcawave --vessel sea_cypress --dry-run
  ```

---

## Overview

OrcaWave is Orcina's panel method diffraction/radiation software for computing:
- Added mass and damping matrices
- Wave excitation forces (RAOs)
- Quadratic Transfer Functions (QTFs)
- Mean drift forces

This module provides **automated workflows** for:
1. Geometry validation and quality checks
2. Batch diffraction analysis execution
3. Results conversion to OrcaFlex vessel types
4. Quality assurance and validation
5. Report generation and packaging

---

## Module Structure

```
src/digitalmodel/modules/orcawave/diffraction/
├── orchestrator.py              # Main workflow controller
├── QUICK_START.md              # Quick start guide
├── configs/
│   ├── base_diffraction_config.yml
│   └── vessels/
│       └── sea_cypress.yml     # Vessel configurations
├── scripts/
│   ├── validate_geometry.py    # Geometry validation
│   ├── convert_to_orcaflex.py  # Results converter
│   └── run_diffraction_analysis.bat
└── results/
    └── [vessel_name]/          # Results organized by vessel

specs/modules/orcawave/diffraction-analysis/
└── inputs/
    └── geometry/               # Geometry files (STL, OBJ)

docs/domains/orcawave/
├── README.md                   # This file
├── DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md
├── diffraction/
│   └── QUICK_START.md
├── examples/                   # Tutorial examples
└── L01_aqwa_benchmark/        # AQWA comparison benchmarks
```

---

## Workflow Phases

The OrcaWave orchestrator provides a **5-phase workflow**:

### Phase 1: Setup & Validation
- Load vessel configuration
- Validate geometry files (watertight, normals, panel counts)
- Check OrcaWave installation
- Create output directories

### Phase 2: OrcaWave Execution
- Generate OrcaWave input files
- Execute diffraction analysis
- Monitor progress and log results

### Phase 3: Results Processing
- Extract hydrodynamic coefficients
- Convert to OrcaFlex vessel type format
- Generate visualizations (optional)

### Phase 4: Quality Assurance
- Reciprocity checks
- Energy balance validation
- Frequency coverage verification

### Phase 5: Packaging
- Package results for distribution
- Generate summary reports
- Create README documentation

---

## Quick Examples

### Example 1: Run Complete Analysis

```bash
# Run full workflow for Sea Cypress vessel
python src/digitalmodel/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress
```

### Example 2: Validation Only

```bash
# Dry run - validate configuration without running analysis
python src/digitalmodel/modules/diffraction_cli.py orcawave --vessel sea_cypress --dry-run
```

### Example 3: Run Specific Phase

```bash
# Run only setup and validation
python src/digitalmodel/modules/orcawave/diffraction/orchestrator.py --vessel sea_cypress --phase setup

# Available phases: setup, execute, process, qa, package
```

### Example 4: Using Python API

```python
from digitalmodel.orcawave.diffraction.orchestrator import OrcaWaveOrchestrator

# Create orchestrator
orchestrator = OrcaWaveOrchestrator(vessel_name="sea_cypress")

# Run complete workflow
orchestrator.run_workflow()

# Or run specific phase
orchestrator._phase1_setup_and_validation()
```

---

## Configuration

### Vessel Configuration Example

```yaml
# configs/vessels/my_vessel.yml
vessel:
  name: "my_vessel"
  description: "FPSO vessel for GoM operations"

geometry:
  file_path: "specs/modules/orcawave/diffraction-analysis/inputs/geometry"
  primary_file: "vessel_mesh.stl"
  panel_size: 0.25  # meters

analysis:
  water_depth: 1200  # meters
  frequencies:
    min: 0.1  # rad/s
    max: 2.0
    count: 40
  headings:
    min: 0    # degrees
    max: 360
    step: 30

output:
  generate_plots: true
  export_orcaflex: true
  qa_checks: true
```

---

## Integration with OrcaFlex

OrcaWave results are automatically converted to OrcaFlex vessel type format:

1. **Hydrodynamic coefficients** exported to Excel/CSV
2. **Vessel type YAML** generated with proper structure
3. **Direct import** to OrcaFlex models

```python
# In OrcaFlex, load the generated vessel type:
import OrcFxAPI

model = OrcFxAPI.Model()
vessel = model['Vessel1']
vessel.VesselType = 'my_vessel_vessel_type.yml'
```

---

## Available Examples

### Sea Cypress Benchmark
- Located in: `specs/modules/orcawave/diffraction-analysis/`
- Full diffraction analysis with validation
- Comparison with AQWA results

### AQWA Benchmark Studies
- Located in: `docs/domains/orcawave/L01_aqwa_benchmark/`
- Cross-validation between OrcaWave and AQWA
- Panel method comparison studies

---

## Prerequisites

### Software Requirements
1. **OrcaWave** - Licensed installation required
2. **Python 3.8+** - Python environment
3. **UV Package Manager** - For dependency management

### Installation

```bash
# Install dependencies using uv
uv sync

# Verify OrcaWave installation (Windows)
where OrcaWave

# Check OrcaFlex API availability
python -c "import OrcFxAPI; print(OrcFxAPI.Version())"
```

---

## Troubleshooting

### Geometry Path Issues
The module uses relative paths from repository root. Ensure:
- Geometry files are in: `specs/modules/orcawave/diffraction-analysis/inputs/geometry/`
- Configuration automatically resolves relative paths

### Missing Dependencies
```bash
# Install all dependencies
uv sync

# h5py may need separate installation
uv add h5py
```

### OrcaWave Not Found
- Add OrcaWave to system PATH
- Set `ORCAWAVE_PATH` environment variable
- Check license availability

---

## Advanced Topics

### Batch Processing Multiple Vessels

```python
from digitalmodel.orcawave.diffraction.orchestrator import OrcaWaveOrchestrator

vessels = ['fpso_a', 'fpso_b', 'shuttle_tanker']

for vessel_name in vessels:
    orchestrator = OrcaWaveOrchestrator(vessel_name=vessel_name)
    orchestrator.run_workflow()
    print(f"Completed: {vessel_name}")
```

### Custom Geometry Validation

```bash
# Validate geometry with custom thresholds
python src/digitalmodel/modules/orcawave/diffraction/scripts/validate_geometry.py \
    --path specs/modules/orcawave/diffraction-analysis/inputs/geometry \
    --vessel my_vessel \
    --no-parallel
```

---

## API Reference

For detailed OrcaWave Python API documentation, see:
https://www.orcina.com/webhelp/OrcFxAPI/Redirector.htm?Pythonreference,Diffraction.htm

---

## External Resources

### GitHub Repositories
- [OrcaPySM1](https://github.com/praveen-kch/OrcaPySM1) - Python scripts for OrcaWave automation
- [orcawave](https://github.com/Exor8129/orcawave) - Additional OrcaWave utilities

### Orcina Documentation
- [OrcaWave Manual](https://www.orcina.com/webhelp/OrcaWave/Default.htm)
- [OrcFxAPI Reference](https://www.orcina.com/webhelp/OrcFxAPI/)

---

## Claude Code Skill

The **orcawave-analysis** skill provides specialized agent capabilities for OrcaWave analysis:

```bash
# Invoke the OrcaWave analysis skill
/orcawave-analysis
```

**Skill capabilities**:
- Expert guidance for diffraction/radiation analysis
- Panel mesh generation and optimization
- Multi-body hydrodynamic interactions
- QTF computation
- Batch processing workflows
- OrcaFlex database generation

**Location**: `.claude/skills/orcawave-analysis/SKILL.md`

---

## Support and Contribution

For questions about OrcaWave automation:
1. Check the **QUICK_START.md** guide
2. Review **DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md** for roadmap
3. Use the **orcawave-analysis** skill for expert guidance
4. See examples in `docs/domains/orcawave/examples/`

---

**Last Updated**: 2026-01-03
**Version**: Phase 1 - Workflow Hardening Complete
