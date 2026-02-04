# WRK-032 Quickstart Guide

> How to use the modular pipeline installation input system
> Rev: 2 (post iteration-2 review — unified CLI, builder_file sections)

## Single-Run Mode (Existing Workflow)

No changes needed. Existing `spec.yml` files work as before:

```bash
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    --spec docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml \
    --output output/30in_floating/
```

### Using the new `roller_arrangement` field

Replace the legacy `rollers` field with the parametric version:

```yaml
equipment:
  roller_arrangement:
    type: v_roller
    stations:
      - position: [5, 0, -2]
        support_count: 4
        v_angle: 120
        diameter: 0.5
        friction_coefficient: 0.1
      - position: [15, 0, -2]
        support_count: 4
        v_angle: 90
        diameter: 0.3
        friction_coefficient: 0.15
        height_offset: -0.5
```

The legacy `rollers:` field still works — it auto-converts to a single-station arrangement.

## Campaign Mode (New)

### 1. Create a campaign YAML

```yaml
# campaign_30in.yml
base:
  metadata:
    name: "30in_campaign"
    structure: pipeline
    operation: installation/floating
  environment:
    water: {depth: 8, density: 1.03}
    seabed: {slope: 0.57, stiffness: {normal: 1000, shear: 100}}
    waves: {type: jonswap, height: 1.5, period: 6, direction: 180}
    current: {speed: 0.5, direction: 270}
    wind: {speed: 5, direction: 270}
  pipeline:
    name: "30'' Line"
    material: X65
    dimensions: {outer_diameter: 0.762, wall_thickness: 0.0254}
    coatings:
      corrosion: {thickness: 0.0042, density: 0.95}
      weight:
        - {name: CWC120, thickness: 0.12, density: 2.978}
    segments:
      - {type: X65+CWC120, length: 5000, segment_length: 5}
  equipment:
    roller_arrangement:
      type: v_roller
      stations:
        - {position: [5, 0, -2], support_count: 4, v_angle: 120, diameter: 0.5}
    tugs:
      count: 5
      spacing: 800
      first_position: [700, -20, 0]
      properties: {mass: 30, volume: 100}
    buoyancy_modules:
      spacing: 4
      properties: {volume: 4.91}
  simulation:
    time_step: 0.1
    stages: [8, 16]

campaign:
  water_depths: [8, 15, 30]
  route_lengths: [3000, 5000]
  environments:
    - name: "calm"
      waves: {type: airy, height: 0.5, period: 5, direction: 180}
      current: {speed: 0.3, direction: 270}
      wind: {speed: 3, direction: 270}
    - name: "storm"
      waves: {type: jonswap, height: 3.5, period: 9, direction: 180}
      current: {speed: 1.0, direction: 270}
      wind: {speed: 12, direction: 270}
  soils:
    - name: "clay"
      stiffness: {normal: 500, shear: 50}
      friction_coefficient: 0.3

output_naming: "30in_wd{water_depth}m_rl{route_length}m_{environment}_{soil}"
```

### 2. Preview the matrix

```bash
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    campaign --preview campaign_30in.yml
```

Output:
```
Campaign: 30in_campaign
Matrix: 3 depths x 2 lengths x 2 environments x 1 soil = 12 runs

 #  water_depth  route_length  environment  soil
 1           8          3000         calm   clay
 2           8          3000        storm   clay
 3           8          5000         calm   clay
...
12          30          5000        storm   clay
```

### 3. Generate all runs

```bash
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    campaign --output output/30in_campaign/ campaign_30in.yml
```

Output directory structure:
```
output/30in_campaign/
├── 30in_wd8m_rl3000m_calm_clay/
│   ├── includes/
│   │   ├── 01_general.yml
│   │   ├── 03_environment.yml
│   │   ├── 05_linetypes.yml
│   │   ├── 08_buoys.yml
│   │   ├── 07_lines.yml
│   │   └── ...
│   └── master.yml
├── 30in_wd8m_rl3000m_storm_clay/
│   ├── includes/
│   └── master.yml
└── ...
```

### 4. Resume or force-overwrite

```bash
# Resume interrupted generation (skips directories with master.yml)
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    campaign --output output/30in_campaign/ --resume campaign_30in.yml

# Force overwrite all existing runs
uv run python -m digitalmodel.solvers.orcaflex.modular_generator.cli \
    campaign --output output/30in_campaign/ --force campaign_30in.yml
```

### 5. Feed to OrcaFlex batch runner (WRK-029)

Each `master.yml` is a runnable OrcaFlex model via the existing batch framework.

## Custom Section Templates

Override specific sections with pre-authored templates:

```yaml
# In campaign.yml
sections:
  - builder_file: "03_environment.yml"
    template: "templates/custom_environment.yml"  # Your template with ${water_depth}
    variables:
      water_density: 1.025

  - builder_file: "08_buoys.yml"
    enabled: false  # Disable auto-generated roller section
```

## Programmatic API

```python
from digitalmodel.solvers.orcaflex.modular_generator.campaign import CampaignGenerator

gen = CampaignGenerator(Path("campaign_30in.yml"))

# Preview without generating files
preview = gen.preview()  # list[dict[str, float | str]]

# Generate with streaming (memory-efficient)
result = gen.generate(Path("output/"))
print(f"Generated {result.run_count} configurations")

# Resume interrupted generation
result = gen.generate(Path("output/"), resume=True)
print(f"Generated {result.run_count}, skipped {result.skipped_count}")
```
