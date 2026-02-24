# OrcaFlex Pipeline Schematic Visualization Plan

**Version:** 1.0.0
**Module:** orcaflex/visualization
**Session ID:** jolly-dancing-kernighan
**Session Agent:** Claude Opus 4.5

## Objective

Create plan and elevation view schematics for the 24" pipeline installation model (`vessel_end_winch.yml`) with boundary condition markups.

## Model Summary

**Source:** `docs/modules/orcaflex/pipeline/installation/floating/24in_pipeline/monolithic/basefile/vessel_end_winch.yml`

### Key Model Components

| Component | Type | Position (Global X, Y, Z) |
|-----------|------|--------------------------|
| Pipeline End A | Fixed to seabed | [-101, 0, 4.505] |
| Pipeline End B | 6D Buoy connection | [~4795, ~-17, ~-0.6] |
| Rollers | Fixed support | [5, 0, -2] |
| Tug1 | Fixed support | [716.7, -20, 0] |
| Tug2 | Fixed support | [1533.3, -20, 0] |
| Tug3 | Fixed support | [2350, -20, 0] |
| Tug4 | Fixed support | [3166.7, -20, 0] |
| Tug5 | Fixed support | [3983.3, -20, 0] |
| Winch Anchor | Fixed seabed | [5000, 0, 2] |
| 6D Buoy1 | Free-floating vessel | [4795.15, -13.31, -1.32] |

### Boundary Conditions to Mark

1. **End A - Fixed Seabed Connection**
   - All 6 DOF constrained (infinite stiffness)
   - Symbol: Triangle with ground hatching

2. **End B - 6D Buoy Connection**
   - 6 DOF free (vessel motion)
   - Symbol: Circle with arrows indicating free motion

3. **Winch System**
   - Tension controlled (2500 N)
   - Symbol: Spring/cable with tension annotation

4. **Tug Supports (5 units)**
   - Fixed position supports
   - Symbol: Pinned support triangles

5. **Roller Support**
   - Vertical restraint only
   - Symbol: Roller support (circle + line)

6. **Buoyancy Modules (29 units)**
   - Distributed buoyancy at 1.5m spacing
   - Symbol: Small circles or buoy icons

## Implementation Approach

### Create New Visualization Module

**File:** `src/digitalmodel/modules/orcaflex/pipeline_schematic.py`

### Dependencies
- matplotlib (already used in `visualization.py`)
- plotly (for interactive HTML output)
- pyyaml (for reading model YAML)
- numpy (for coordinate calculations)

### Key Functions

```python
class PipelineSchematicGenerator:
    """Generate plan and elevation schematics with boundary conditions."""

    def __init__(self, yaml_file: str, output_dir: str):
        """Load YAML model and initialize."""

    def generate_plan_view(self) -> Figure:
        """Generate XY plane (top-down) schematic."""
        # Draw pipeline path
        # Mark End A (fixed) with triangle symbol
        # Mark End B (vessel) with 6DOF symbol
        # Show tug positions with support symbols
        # Show roller position
        # Annotate winch cable path
        # Add legend and scale

    def generate_elevation_view(self) -> Figure:
        """Generate XZ plane (side) schematic."""
        # Draw pipeline profile with catenary shape
        # Mark water surface (Z=0)
        # Mark seabed level
        # Show vertical boundary conditions
        # Annotate buoyancy modules
        # Show touchdown point if applicable

    def add_boundary_annotations(self, ax, view: str):
        """Add boundary condition symbols and labels."""

    def save_schematics(self, format: str = "html"):
        """Save both views to output directory."""
```

### Output Files

1. `pipeline_plan_view.html` - Interactive plan view
2. `pipeline_elevation_view.html` - Interactive elevation view
3. `pipeline_schematic_report.html` - Combined report with both views

## Boundary Condition Markup Legend

| Symbol | Description | Matplotlib Implementation |
|--------|-------------|--------------------------|
| Filled triangle + hatching | Fixed support (End A) | `patches.Polygon` with hatch pattern |
| Circle with roller | Roller support | `patches.Circle` + line |
| Triangle (unfilled) | Pinned support (Tugs) | `patches.Polygon` |
| 6 arrows | 6DOF free (Vessel) | `FancyArrowPatch` x 6 |
| Spring zigzag | Winch tension | Custom path |
| Small circles | Buoyancy modules | `scatter` or `Circle` patches |

## File Modifications

### New File
- `src/digitalmodel/modules/orcaflex/pipeline_schematic.py`

### Files to Reference (Read Only)
- `src/digitalmodel/modules/orcaflex/visualization.py` - Pattern for XY/XZ plotting
- `docs/modules/orcaflex/pipeline/installation/floating/24in_pipeline/monolithic/basefile/vessel_end_winch.yml` - Model data

## Verification

1. Run schematic generation on the target YAML file
2. Verify all boundary conditions are marked correctly
3. Check coordinate accuracy against model
4. Ensure legend is complete and readable
5. Validate HTML output opens in browser

## Implementation Details

### New File Location
```
src/digitalmodel/modules/orcaflex/visualization/pipeline_schematic.py
```

### Class Structure

```python
class BoundaryConditionType(Enum):
    FIXED_SEABED = "fixed_seabed"        # End A - all DOF constrained
    FREE_6DOF = "free_6dof"              # End B - 6D Buoy
    WINCH_TENSION = "winch_tension"      # Tension controlled
    FIXED_SUPPORT = "fixed_support"      # Tug supports
    VERTICAL_RESTRAINT = "vertical_restraint"  # Roller
    DISTRIBUTED_BUOYANCY = "distributed_buoyancy"  # Buoyancy modules

@dataclass
class BoundaryCondition:
    name: str
    bc_type: BoundaryConditionType
    position: Tuple[float, float, float]
    properties: Dict

class PipelineSchematicGenerator:
    def parse_model(yaml_path) -> Dict
    def create_plan_view(model_data) -> go.Figure
    def create_elevation_view(model_data) -> go.Figure
    def generate_html_report(model_data, output_path) -> None
```

### Symbol Library (Plotly traces)

| BC Type | Symbol | Color |
|---------|--------|-------|
| Fixed Seabed | Triangle + ground hatching | Brown #8B4513 |
| 6DOF Free | Circle + 6 arrows | Blue #1E90FF |
| Winch Tension | Zigzag spring | Orange #FF4500 |
| Fixed Support | Filled circle | Green #228B22 |
| Vertical Restraint | Horizontal line + arrow | Purple #9932CC |
| Buoyancy Modules | Triangle-up markers | Gold #FFD700 |

### YAML Parsing

Extract from `vessel_end_winch.yml`:
- `Lines.pipeline.StartingShapeX,Y,Z,...` → Pipeline trajectory (9927 points)
- `6DBuoys.6D buoy1.InitialX/Y/Z` → End B position
- `Winches.vessel_winch.Connection` → Winch anchor
- `BaseFile` → Reference to base model with tug/roller definitions

### Output Files

```
output/
├── pipeline_plan_view.html       # Interactive plan view
├── pipeline_elevation_view.html  # Interactive elevation view
└── pipeline_schematic_report.html # Combined report
```

## Critical Files

| File | Purpose |
|------|---------|
| `src/.../orcaflex/visualization.py` | Pattern: XY/XZ plotting with matplotlib |
| `src/.../reporting/report_generator.py` | Pattern: Plotly HTML reports |
| `src/.../common/utilities/visualization_plotly.py` | Plotly utilities |
| `docs/.../vessel_end_winch.yml` | Source model data |

## Verification

1. Run generator on `vessel_end_winch.yml`
2. Open HTML report in browser
3. Verify all 6 boundary conditions marked with correct symbols
4. Check coordinates match model (End A at X=-101, End B at X~4795)
5. Verify interactive hover shows position data
6. Confirm legend displays all boundary types
