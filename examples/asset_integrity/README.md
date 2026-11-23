# Asset Integrity FFS Examples

Working examples for the `digitalmodel.asset_integrity` fitness-for-service module.

## Examples

| Script | Description |
|--------|-------------|
| `basic_usage.py` | Minimal walkthrough of the YAML config structure for API 579 Level 1 GML. Explains the engine workflow without executing it. |
| `example_pipe_capacity.py` | Pipe capacity calculation setup for ASME B31.4 (oil) and B31.8 (gas) using the modified Barlow equation. Includes manual verification math. |
| `example_api579_gml.py` | API 579 Part 4 General Metal Loss assessment for a 16" gas pipeline. Uses simulated wall-thickness grids. Demonstrates Level 1 screening and Level 2 assessment length calculation. |
| `example_api579_lml.py` | API 579 Part 5 Local Metal Loss assessment. Shows how to define a Local Thin Area (LTA) by grid indices, Folias factor interpolation, and MAWP reduction workflow. |

## Running the Examples

Each example is self-contained and prints configuration summaries and calculation walkthroughs:

```bash
python examples/asset_integrity/basic_usage.py
python examples/asset_integrity/example_pipe_capacity.py
python examples/asset_integrity/example_api579_gml.py
python examples/asset_integrity/example_api579_lml.py
```

These scripts demonstrate the config format and methodology. They do **not** call `engine()` directly because that requires the full environment (Excel data files, matplotlib, output directories).

## Running Actual Assessments

To run an assessment through the engine:

```python
from digitalmodel.asset_integrity.engine import engine

result = engine(inputfile="path/to/your_config.yml")
```

The engine:
1. Loads your YAML and merges with package defaults
2. Sets up output directories for results, logs, and plots
3. Dispatches to the appropriate analysis (API579 or fracture_mechanics)
4. Saves CSV tables, PNG plots, and a YAML summary

## Config Structure Reference

All configs require:
- `basename`: `API579` or `fracture_mechanics`
- `Outer_Pipe`: Geometry, Material, Code
- `Design`: Load conditions (pressure, temperature, forces)
- `Material`: Material property library with grades

API 579 configs additionally need:
- `API579Parameters`: RSFa, Age, FCARateFloor, FoliasFactor table
- `ReadingSets` or `gml_simulated_grid` for GML
- `LML.LTA` for Local Metal Loss (with flaw index ranges)
- `PlotSettings` for visualization

See the test data YAMLs for complete reference:
- `src/digitalmodel/asset_integrity/tests/test_data/API579/16in_gas_b318.yml`
- `src/digitalmodel/asset_integrity/tests/test_data/API579/12in_oil_cml28_b314.yml`
