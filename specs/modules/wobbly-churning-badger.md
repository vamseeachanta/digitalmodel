# BEMRosetta Integration Module

## Metadata
- **title**: BEMRosetta Integration Module
- **description**: AQWA to OrcaFlex hydrodynamic coefficient converter with QTF and mesh support
- **version**: 1.0.0
- **module**: bemrosetta
- **session.id**: wobbly-churning-badger
- **session.agent**: Claude Opus 4.5
- **review**: pending

---

## Overview

BEMRosetta is an open-source hydrodynamic coefficients converter/viewer. This module provides a focused Python interface for **AQWA → OrcaFlex** conversion workflow, including QTF (second-order forces) and mesh format conversion.

### Scope (Initial Release)
- **Primary**: AQWA output parsing (.LIS, .DAT, .QTF files)
- **Export**: OrcaFlex-compatible formats (YAML, CSV)
- **QTF**: Second-order wave forces (sum/difference frequency)
- **Mesh**: GDF, DAT, STL format conversion

### Key Capabilities
- Parse AQWA diffraction analysis outputs
- Extract RAOs, added mass, damping, and QTF coefficients
- Validate data quality (symmetry, causality, physical limits)
- Export to OrcaFlex YAML format
- Mesh format conversion for preprocessing

### External Dependencies
- `BEMRosetta_cl.exe` at `D:/software/BEMRosetta/` (optional, for extended features)
- Native Python parsers for AQWA (no external dependency required)

---

## Module Structure

```
src/digitalmodel/modules/bemrosetta/
├── __init__.py                    # Public API, feature detection
├── cli.py                         # Click-based CLI
├── models.py                      # BEMRosetta-specific data models (QTF, mesh)
├── core/
│   ├── __init__.py
│   ├── interfaces.py              # ParserInterface, ConverterInterface
│   ├── exceptions.py              # Module-specific exceptions
│   └── runner.py                  # BEMRosetta_cl.exe subprocess wrapper (optional)
├── parsers/
│   ├── __init__.py
│   ├── base.py                    # BaseParser class
│   ├── aqwa_parser.py             # AQWA .LIS, .DAT parser (primary)
│   └── qtf_parser.py              # QTF file parser (.QTF, second-order forces)
├── converters/
│   ├── __init__.py
│   ├── base.py                    # BaseConverter class
│   └── to_orcaflex.py             # Convert to OrcaFlex YAML (integrate existing)
├── mesh/
│   ├── __init__.py
│   ├── mesh_handler.py            # Panel mesh operations and validation
│   ├── gdf_handler.py             # WAMIT GDF format (read/write)
│   ├── dat_handler.py             # AQWA DAT format (read/write)
│   └── stl_handler.py             # STL format (read/write)
├── validators/
│   ├── __init__.py
│   ├── coefficient_validator.py   # Physical consistency checks
│   └── causality_checker.py       # Kramers-Kronig validation
└── MODULE_README.md               # Documentation
```

---

## Data Models

### Reuse Existing (from diffraction/output_schemas.py)
- `DiffractionResults` - Complete analysis results container
- `RAOSet`, `RAOComponent` - RAO data structures
- `AddedMassSet`, `DampingSet` - Coefficient matrices
- `HydrodynamicMatrix` - 6x6 frequency-dependent matrix
- `FrequencyData`, `HeadingData` - Discretization data
- `DOF`, `Unit` enums

### New Models (models.py)
```python
@dataclass
class BEMSolverMetadata:
    """Metadata extracted from BEM solver output"""
    solver_name: str          # WAMIT, AQWA, Nemoh, etc.
    solver_version: Optional[str]
    water_depth: float
    vessel_name: str
    body_count: int
    frequency_count: int
    heading_count: int
    includes_qtf: bool

@dataclass
class QTFData:
    """Quadratic Transfer Function (second-order forces)"""
    frequencies_1: np.ndarray
    frequencies_2: np.ndarray
    headings: np.ndarray
    qtf_coefficients: np.ndarray  # [n_f1 x n_f2 x n_head x 6]
    qtf_type: str  # "sum" or "difference"

@dataclass
class PanelMesh:
    """Panel mesh for diffraction analysis"""
    vertices: np.ndarray      # [n_vertices x 3]
    panels: np.ndarray        # [n_panels x 4]
    panel_normals: np.ndarray
    panel_areas: np.ndarray
    is_symmetric: bool
```

---

## Core Interfaces

### ParserInterface
```python
class ParserInterface(ABC):
    @property
    @abstractmethod
    def supported_extensions(self) -> List[str]: ...

    @property
    @abstractmethod
    def solver_name(self) -> str: ...

    @abstractmethod
    def parse(self, file_path: Path) -> DiffractionResults: ...

    @abstractmethod
    def validate_file(self, file_path: Path) -> bool: ...
```

### ConverterInterface
```python
class ConverterInterface(ABC):
    @property
    @abstractmethod
    def output_format(self) -> str: ...

    @abstractmethod
    def convert(self, results: DiffractionResults, output_path: Path) -> Path: ...
```

---

## CLI Commands

```bash
# Convert AQWA to OrcaFlex format
bemrosetta convert input.LIS --output-format orcaflex --output-dir ./output

# Convert with QTF data
bemrosetta convert input.LIS --qtf input.QTF --output-format orcaflex -o ./output

# Display AQWA file information
bemrosetta info input.LIS

# Validate hydrodynamic coefficients
bemrosetta validate input.LIS --strict

# Convert mesh formats (GDF, DAT, STL)
bemrosetta convert-mesh input.gdf --output-format dat --output mesh.dat
bemrosetta convert-mesh input.dat --output-format stl --output mesh.stl

# Validate mesh quality
bemrosetta validate-mesh input.gdf --check-normals --check-watertight

# Compare two data sources
bemrosetta compare file1.LIS file2.LIS --tolerance 0.05 --output report.html

# Batch processing
bemrosetta batch config.yml
```

---

## Integration Points

### With Existing Modules
1. **diffraction module** - Reuse `DiffractionResults`, `OrcaFlexExporter`
2. **hydrodynamics module** - Reuse `HydrodynamicMatrix`, `VesselProperties`
3. **orcaflex module** - Export workflow integration

### External Tool
- `BEMRosetta_cl.exe` via subprocess for format conversion
- Feature detection for graceful degradation when unavailable

---

## Implementation Tasks

### Phase 1: Core Infrastructure
- [ ] Create module directory structure
- [ ] Implement `models.py` with QTF and mesh data models
- [ ] Implement `core/interfaces.py` with abstract base classes
- [ ] Implement `core/exceptions.py` with custom exceptions
- [ ] Implement `core/runner.py` for optional BEMRosetta_cl wrapper
- [ ] Implement `__init__.py` with feature detection and public API

### Phase 2: AQWA Parsers
- [ ] Implement `parsers/base.py` base class
- [ ] Implement `parsers/aqwa_parser.py` for .LIS files (RAOs, added mass, damping)
- [ ] Implement `parsers/qtf_parser.py` for .QTF files (second-order forces)
- [ ] Integrate with existing `diffraction/aqwa_converter.py`

### Phase 3: OrcaFlex Converter
- [ ] Implement `converters/base.py` base class
- [ ] Implement `converters/to_orcaflex.py` (integrate with existing exporter)
- [ ] Support QTF export to OrcaFlex format

### Phase 4: Mesh Handlers
- [ ] Implement `mesh/mesh_handler.py` with common operations
- [ ] Implement `mesh/gdf_handler.py` for WAMIT GDF format
- [ ] Implement `mesh/dat_handler.py` for AQWA DAT format
- [ ] Implement `mesh/stl_handler.py` for STL format
- [ ] Add mesh validation (panel quality, normals, watertightness)

### Phase 5: Validators
- [ ] Implement `validators/coefficient_validator.py` (symmetry, limits)
- [ ] Implement `validators/causality_checker.py` (Kramers-Kronig)

### Phase 6: CLI & Documentation
- [ ] Implement `cli.py` with Click commands
- [ ] Add entry point to `pyproject.toml`
- [ ] Create `MODULE_README.md`

### Phase 7: Testing
- [ ] Create test fixtures (sample AQWA .LIS, .QTF, mesh files)
- [ ] Implement AQWA parser unit tests
- [ ] Implement QTF parser unit tests
- [ ] Implement OrcaFlex converter unit tests
- [ ] Implement mesh handler unit tests
- [ ] Implement validator unit tests
- [ ] Implement CLI integration tests
- [ ] Implement end-to-end integration tests

---

## Critical Files to Modify

| File | Action | Description |
|------|--------|-------------|
| `src/digitalmodel/modules/bemrosetta/__init__.py` | Create | Public API with feature detection |
| `src/digitalmodel/modules/bemrosetta/models.py` | Create | QTF and mesh data models |
| `src/digitalmodel/modules/bemrosetta/cli.py` | Create | Click CLI commands |
| `src/digitalmodel/modules/bemrosetta/parsers/aqwa_parser.py` | Create | AQWA .LIS parser (primary) |
| `src/digitalmodel/modules/bemrosetta/parsers/qtf_parser.py` | Create | QTF file parser |
| `src/digitalmodel/modules/bemrosetta/converters/to_orcaflex.py` | Create | OrcaFlex export converter |
| `src/digitalmodel/modules/bemrosetta/mesh/gdf_handler.py` | Create | WAMIT GDF mesh handler |
| `src/digitalmodel/modules/bemrosetta/mesh/dat_handler.py` | Create | AQWA DAT mesh handler |
| `src/digitalmodel/modules/bemrosetta/mesh/stl_handler.py` | Create | STL mesh handler |
| `pyproject.toml` | Edit | Add CLI entry point |
| `tests/modules/bemrosetta/conftest.py` | Create | Test fixtures |

---

## Verification Plan

### Unit Tests
```bash
uv run pytest tests/modules/bemrosetta/test_parsers.py -v
uv run pytest tests/modules/bemrosetta/test_converters.py -v
uv run pytest tests/modules/bemrosetta/test_validators.py -v
```

### CLI Tests
```bash
uv run pytest tests/modules/bemrosetta/test_cli.py -v
```

### Integration Tests
```bash
uv run pytest tests/modules/bemrosetta/test_integration.py -v -m integration
```

### Manual Verification
1. Convert sample WAMIT file to OrcaFlex format
2. Verify output loads correctly in OrcaFlex
3. Compare converted coefficients with original
4. Test with BEMRosetta_cl.exe unavailable (graceful degradation)

---

## Dependencies

### Required (already in pyproject.toml)
- numpy, scipy, pandas, click, pydantic, loguru

### Optional
- h5py (for Bemio HDF5 format)
- netCDF4 (for Capytaine .nc format)

### External
- BEMRosetta_cl.exe at `D:/software/BEMRosetta/`

---

## Notes

1. **Scope Focus**: Initial release focuses on AQWA → OrcaFlex workflow. Additional formats (WAMIT, Nemoh, HAMS) can be added as future enhancements.

2. **Native Parsers**: The module provides native Python parsers for AQWA files, ensuring functionality without external BEMRosetta executable.

3. **QTF Support**: Second-order wave forces (sum/difference frequency QTF) included for mooring and slow-drift analysis.

4. **Mesh Tools**: GDF, DAT, STL mesh format conversion included for preprocessing workflows.

5. **Integration**: Reuses existing `diffraction` module schemas (`DiffractionResults`, `RAOSet`, etc.) to ensure compatibility with OrcaFlex export workflow.

6. **BEMRosetta Executable**: Optional - provides extended features when available at `D:/software/BEMRosetta/BEMRosetta_cl.exe`.

## Future Enhancements (Out of Scope)

- WAMIT parser (.out, .1, .3, .4, .5 files)
- Nemoh parser
- HAMS parser
- Capytaine (.nc) parser
- Hydrostar parser
- Diodore parser
- Interactive mesh visualization (Plotly 3D)
