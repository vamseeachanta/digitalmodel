# CALM Buoy Project - Quick Start Guide

## 🚀 Get Started in 5 Minutes

This guide will help you create your first CALM Buoy project in just a few steps.

---

## Step 1: Copy Example Template (30 seconds)

```bash
# Navigate to repository root
cd <private-data>\digitalmodel

# Copy example to your project
cp examples/north_sea_calm_project.yml projects/my_first_calm_project.yml
```

---

## Step 2: Edit Configuration (2 minutes)

Open `projects/my_first_calm_project.yml` and customize these key fields:

```yaml
human_input:
  project:
    name: "My First CALM Buoy"        # ← Your project name
    code: "CALM_001"                   # ← Unique project code
    client: "Your Client Name"         # ← Client name

  site:
    water_depth: 120                   # ← Water depth (meters)
    metocean_override:
      operating_conditions:
        hs_max: 2.5                    # ← Max wave height (meters)
        wind_speed_max: 15             # ← Max wind speed (m/s)

  buoy:
    outer_diameter: 12.0               # ← Buoy diameter (meters)
    draft: 10.0                        # ← Buoy draft (meters)
    mass_operating: 9200               # ← Operating mass (tonnes)

  mooring:
    number_of_lines: 6                 # ← Number of mooring lines
    line_segments:
      - name: "top_chain"
        nominal_diameter: 76           # ← Chain diameter (mm)
        length: 150                    # ← Chain length (meters)
```

**💡 Tip:** Leave other parameters as defaults for your first project!

---

## Step 3: Generate Project (1 minute)

```bash
# Generate OrcaFlex model with validation
python scripts/generate_calm_buoy_project.py \
  --config projects/my_first_calm_project.yml \
  --fidelity preliminary \
  --validate
```

**Expected Output:**

```
================================================================================
CALM BUOY PROJECT GENERATOR
================================================================================

📖 Loading configuration from: projects/my_first_calm_project.yml
✅ Configuration loaded: My First CALM Buoy

📁 Creating project directory structure at: projects/CALM_001

🔍 Validating human input parameters...
✅ All validations passed

🧮 Calculating derived parameters...
  ✓ Mooring footprint radius: 235.0 m
  ✓ Total mooring mass: 3179.5 tonnes
  ✓ Watch circle radius: 23.5 m

⚙️  Generating OrcaFlex modules (preliminary fidelity)...
  ✓ Created: CALM_001_calm_buoy.yml
  ✓ Copied 16 module files

✅ Running validation framework...
  Total files: 1
  ✅ Passed: 1
  Pass rate: 100.0%

================================================================================
✅ PROJECT GENERATION COMPLETE
================================================================================

Project location: <private-data>\CALM_001

Next steps:
  1. Review configuration: projects/CALM_001/project_config.yml
  2. Open OrcaFlex model: projects/CALM_001/orcaflex/
  3. Review validation report: projects/CALM_001/reports/validation/
```

---

## Step 4: Review Generated Files (1 minute)

Your project structure:

```
projects/CALM_001/
├── project_config.yml              # ← Updated config with AI validation
├── README.md                       # ← Project documentation
│
├── orcaflex/                       # ← OrcaFlex model files
│   ├── CALM_001_calm_buoy.yml     # ← Main file (ready to load in OrcaFlex)
│   └── modules/                    # ← 16 modular components
│       ├── _01a_units_analysis.yml
│       ├── _03c_waves_jonswap.yml
│       ├── _04_vessel_types.yml
│       └── ...
│
├── reports/validation/             # ← Validation results
│   ├── validation_*.html          # ← Interactive dashboard
│   ├── validation_*.md            # ← Markdown report
│   └── validation_*.csv           # ← CSV data
│
├── freecad/                        # ← FreeCAD files (future)
├── blender/                        # ← Blender files (future)
└── data/                           # ← Project-specific data
```

---

## Step 5: Open Validation Report (30 seconds)

```bash
# Open HTML validation report
start projects/CALM_001/reports/validation/validation_*.html

# Or view in browser
firefox projects/CALM_001/reports/validation/validation_*.html
```

**What to check:**

✅ **Level 1: YAML Syntax** - Should show "PASS"
- YAML Valid: Yes ✓
- Includes Resolved: Yes ✓
- Total Modules: 16 ✓

✅ **Level 3: Physical Consistency** - Check parameters
- Buoy geometry within ranges ✓
- Metocean conditions realistic ✓
- Mooring capacity adequate ✓

---

## Step 6: Load in OrcaFlex (Optional)

If you have OrcaFlex installed:

```bash
# Open OrcaFlex GUI
OrcaFlex projects/CALM_001/orcaflex/CALM_001_calm_buoy.yml
```

**In OrcaFlex:**
1. Calculate Statics (Ctrl+S)
2. Run Simulation (Ctrl+R)
3. View Results

---

## 🎉 Congratulations!

You've successfully created your first CALM Buoy project!

---

## Next Steps

### Option A: Run Detailed Analysis

Generate a high-fidelity model with discretised buoy geometry:

```bash
python scripts/generate_calm_buoy_project.py \
  --config projects/my_first_calm_project.yml \
  --fidelity detailed \
  --validate
```

**Difference:**
- **Preliminary:** Lumped buoy (faster, 16 modules)
- **Detailed:** Discretised buoy geometry (accurate, 17 modules)

### Option B: Customize Parameters

Edit `projects/my_first_calm_project.yml` to modify:

**Metocean Conditions:**
```yaml
metocean_override:
  operating_conditions:
    hs_max: 3.0              # Increase wave height
    tp_max: 9.0              # Increase wave period
    wind_speed_max: 20       # Increase wind speed
```

**Mooring System:**
```yaml
mooring:
  number_of_lines: 8         # Add more lines
  line_segments:
    - nominal_diameter: 84   # Larger chain diameter
      length: 200            # Longer chains
```

**Then regenerate:**
```bash
python scripts/generate_calm_buoy_project.py \
  --config projects/my_first_calm_project.yml \
  --validate
```

### Option C: Create Multiple Projects

Use different configurations for sensitivity studies:

```bash
# Project 1: Shallow water (80m)
cp projects/my_first_calm_project.yml projects/calm_shallow.yml
# Edit: water_depth: 80

# Project 2: Deep water (150m)
cp projects/my_first_calm_project.yml projects/calm_deep.yml
# Edit: water_depth: 150

# Generate both
python scripts/generate_calm_buoy_project.py --config projects/calm_shallow.yml --validate
python scripts/generate_calm_buoy_project.py --config projects/calm_deep.yml --validate
```

---

## Common Commands Reference

```bash
# Generate with validation
python scripts/generate_calm_buoy_project.py \
  --config <config_file> \
  --validate

# Generate without validation (faster)
python scripts/generate_calm_buoy_project.py \
  --config <config_file> \
  --skip-validation

# Specify output directory
python scripts/generate_calm_buoy_project.py \
  --config <config_file> \
  --output-dir projects/my_custom_location

# Generate detailed model
python scripts/generate_calm_buoy_project.py \
  --config <config_file> \
  --fidelity detailed
```

---

## Troubleshooting

### Issue: "ModuleNotFoundError: No module named 'digitalmodel'"

**Solution:**
```bash
# Activate virtual environment
.venv\Scripts\activate  # Windows
source .venv/bin/activate  # Linux/Mac

# Or set PYTHONPATH
export PYTHONPATH=$PYTHONPATH:$(pwd)/src
```

### Issue: "Validation failed: Safety factor below minimum"

**Solution:** Increase safety factors in config:
```yaml
mooring:
  safety_factor_intact: 2.0      # Increase from 1.8
  safety_factor_damaged: 1.3     # Increase from 1.25
```

### Issue: "Template file not found"

**Solution:** Verify template files exist:
```bash
ls specs/modules/orcaflex/modular-input-file/output/calm_buoy_base.yml
ls specs/modules/orcaflex/modular-input-file/output/_*.yml
```

---

## Need Help?

📚 **Full Documentation:** `docs/CALM_BUOY_PROJECT_WORKFLOW.md`

🔍 **Example Project:** `examples/north_sea_calm_project.yml`

📋 **Template Reference:** `templates/calm_buoy/project_template.yml`

🧪 **Validation Framework:** Run `python scripts/run_validation.py --help`

---

## File Locations Summary

| File Type | Location |
|-----------|----------|
| **Configuration Template** | `templates/calm_buoy/project_template.yml` |
| **Example Project** | `examples/north_sea_calm_project.yml` |
| **Your Projects** | `projects/<PROJECT_CODE>/` |
| **OrcaFlex Templates** | `specs/modules/orcaflex/modular-input-file/output/` |
| **Reference Data** | `data/raw/`, `data/processed/`, `data/results/` |
| **Generation Script** | `scripts/generate_calm_buoy_project.py` |
| **Validation Script** | `scripts/run_validation.py` |
| **Documentation** | `docs/CALM_BUOY_*.md` |

---

**Happy modeling! 🚢⚓**
