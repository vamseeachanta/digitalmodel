# Batch Processing Multiple Vessels

**Duration**: 20 minutes
**Difficulty**: Intermediate

## Overview

Learn how to efficiently process multiple vessels or configurations in parallel using the batch processing framework.

## When to Use Batch Processing

- Multiple vessels in a project (FPSO, CALM buoy, support vessels)
- Same vessel at different water depths
- Sensitivity studies with different configurations
- Fleet-wide analysis updates

## Basic Batch Configuration

Create a JSON file `fleet_config.json`:

```json
{
  "configurations": [
    {
      "vessel_name": "FPSO_A",
      "source_type": "aqwa",
      "source_path": "./data/aqwa/fpso_a_analysis",
      "water_depth": 1200.0,
      "output_dir": "./output/fpso_a"
    },
    {
      "vessel_name": "TLP_B",
      "source_type": "orcawave",
      "source_path": "./data/orcawave/tlp_b_model.sim",
      "water_depth": 800.0,
      "output_dir": "./output/tlp_b"
    },
    {
      "vessel_name": "SPAR_C",
      "source_type": "aqwa",
      "source_path": "./data/aqwa/spar_c_analysis",
      "water_depth": 1500.0,
      "output_dir": "./output/spar_c"
    }
  ],
  "parallel": true,
  "max_workers": 4
}
```

## Running Batch Processing

```bash
# Process all vessels in configuration
diffraction batch fleet_config.json

# Output:
# ================================================================================
# Batch Diffraction Processing
# ================================================================================
# Configuration: fleet_config.json
#
# Processing vessel 1/3: FPSO_A (AQWA)...
# Processing vessel 2/3: TLP_B (OrcaWave)...
# Processing vessel 3/3: SPAR_C (AQWA)...
#
# ================================================================================
# Batch Processing Summary
# ================================================================================
# Total configurations: 3
# Successful: 3
# Warnings: 0
# Failed: 0
# Total duration: 8.45 seconds
```

## Configuration Options

### Per-Vessel Settings

```json
{
  "vessel_name": "FPSO_A",           // Unique identifier
  "source_type": "aqwa",             // "aqwa" or "orcawave"
  "source_path": "./data/aqwa/...", // Folder (AQWA) or file (OrcaWave)
  "water_depth": 1200.0,             // Water depth in meters
  "output_dir": "./output/fpso_a",   // Output directory
  "validate": true,                  // Run validation (default: true)
  "formats": ["all"]                 // Export formats (default: all)
}
```

### Global Settings

```json
{
  "parallel": true,      // Enable parallel processing (default: true)
  "max_workers": 4       // Number of parallel workers (default: 4)
}
```

## Advanced Example: Sensitivity Study

Water depth sensitivity for same vessel:

```json
{
  "configurations": [
    {
      "vessel_name": "FPSO_A_Depth_1000m",
      "source_type": "aqwa",
      "source_path": "./data/aqwa/fpso_a",
      "water_depth": 1000.0,
      "output_dir": "./sensitivity/depth_1000"
    },
    {
      "vessel_name": "FPSO_A_Depth_1200m",
      "source_type": "aqwa",
      "source_path": "./data/aqwa/fpso_a",
      "water_depth": 1200.0,
      "output_dir": "./sensitivity/depth_1200"
    },
    {
      "vessel_name": "FPSO_A_Depth_1500m",
      "source_type": "aqwa",
      "source_path": "./data/aqwa/fpso_a",
      "water_depth": 1500.0,
      "output_dir": "./sensitivity/depth_1500"
    }
  ],
  "parallel": true,
  "max_workers": 3
}
```

## Selective Format Export

Export only specific formats to save time/space:

```json
{
  "vessel_name": "TLP_B",
  "source_type": "orcawave",
  "source_path": "./models/tlp_b.sim",
  "water_depth": 800.0,
  "output_dir": "./output/tlp_b",
  "formats": ["vessel_type", "excel", "summary"]  // Only these 3
}
```

Available formats:
- `"all"` - All formats (default)
- `"vessel_type"` - OrcaFlex YAML only
- `"rao_csv"` - RAO CSV
- `"added_mass_csv"` - Added mass CSV
- `"damping_csv"` - Damping CSV
- `"excel"` - Excel workbook
- `"summary"` - Summary JSON

## Mixed Source Types

Combine AQWA and OrcaWave in one batch:

```json
{
  "configurations": [
    {
      "vessel_name": "FPSO_AQWA",
      "source_type": "aqwa",
      "source_path": "./aqwa/fpso",
      "water_depth": 1200.0,
      "output_dir": "./output/fpso_aqwa"
    },
    {
      "vessel_name": "FPSO_OrcaWave",
      "source_type": "orcawave",
      "source_path": "./orcawave/fpso.sim",
      "water_depth": 1200.0,
      "output_dir": "./output/fpso_orcawave"
    }
  ],
  "parallel": false,  // Sequential for comparison
  "max_workers": 1
}
```

## Programmatic Batch Processing

```python
from pathlib import Path
from digitalmodel.modules.diffraction import (
    BatchProcessor,
    BatchConfiguration
)

# Create configuration programmatically
config = BatchConfiguration(
    configurations=[
        {
            "vessel_name": "FPSO_A",
            "source_type": "aqwa",
            "source_path": "./data/aqwa/fpso_a",
            "water_depth": 1200.0,
            "output_dir": "./output/fpso_a",
            "validate": True,
            "formats": ["all"]
        },
        {
            "vessel_name": "TLP_B",
            "source_type": "orcawave",
            "source_path": "./data/orcawave/tlp_b.sim",
            "water_depth": 800.0,
            "output_dir": "./output/tlp_b",
            "validate": True,
            "formats": ["vessel_type", "excel"]
        }
    ],
    parallel=True,
    max_workers=4
)

# Run batch processing
processor = BatchProcessor(config)
report = processor.process_all()

# Check results
print(f"Total: {report.total_configurations}")
print(f"Successful: {report.successful}")
print(f"Failed: {report.failed}")
print(f"Duration: {report.total_duration:.2f}s")

# Access individual results
for result in report.results:
    print(f"{result.vessel_name}: {result.status}")
    if result.status == "success":
        print(f"  Output: {result.output_dir}")
    elif result.status == "failed":
        print(f"  Error: {result.error_message}")
```

## Error Handling

The batch processor continues even if individual vessels fail:

```json
{
  "configurations": [
    {
      "vessel_name": "GOOD_VESSEL",
      "source_type": "aqwa",
      "source_path": "./data/good_vessel",
      "water_depth": 1200.0,
      "output_dir": "./output/good"
    },
    {
      "vessel_name": "BAD_VESSEL",
      "source_type": "aqwa",
      "source_path": "./data/missing_folder",  // This will fail
      "water_depth": 1200.0,
      "output_dir": "./output/bad"
    },
    {
      "vessel_name": "ANOTHER_GOOD",
      "source_type": "aqwa",
      "source_path": "./data/another_good",
      "water_depth": 1200.0,
      "output_dir": "./output/another"
    }
  ]
}
```

Output:
```
Processing vessel 1/3: GOOD_VESSEL...
  [OK] Success

Processing vessel 2/3: BAD_VESSEL...
  [ERROR] FileNotFoundError: ./data/missing_folder not found

Processing vessel 3/3: ANOTHER_GOOD...
  [OK] Success

================================================================================
Batch Processing Summary
================================================================================
Total configurations: 3
Successful: 2
Failed: 1
```

## Performance Optimization

### Optimal Worker Count

```python
import multiprocessing

# Use number of CPU cores
max_workers = multiprocessing.cpu_count()

# Or leave 1 core free for system
max_workers = max(1, multiprocessing.cpu_count() - 1)

# For I/O-bound tasks (file reading/writing)
max_workers = min(8, multiprocessing.cpu_count() * 2)
```

### Large Batch Processing

For dozens of vessels:

```json
{
  "configurations": [ /* ... 50 vessels ... */ ],
  "parallel": true,
  "max_workers": 8,  // Balance between speed and memory
  "batch_size": 10   // Process 10 at a time (if supported)
}
```

## Directory Structure Example

Recommended organization:

```
project/
├── config/
│   ├── fleet_config.json          # Main configuration
│   ├── sensitivity_config.json    # Sensitivity studies
│   └── comparison_config.json     # AQWA vs OrcaWave
│
├── data/
│   ├── aqwa/
│   │   ├── fpso_a/
│   │   ├── spar_c/
│   │   └── ship_e/
│   └── orcawave/
│       ├── tlp_b.sim
│       └── semi_d.sim
│
└── output/
    ├── fpso_a/
    │   ├── FPSO_A_vessel_type.yml
    │   ├── FPSO_A_RAOs.csv
    │   └── ...
    ├── tlp_b/
    └── ...
```

## Batch with Comparison

Process and compare in sequence:

```json
{
  "configurations": [
    // First, convert all AQWA
    {
      "vessel_name": "FPSO_AQWA",
      "source_type": "aqwa",
      "source_path": "./aqwa/fpso",
      "water_depth": 1200.0,
      "output_dir": "./output/aqwa/fpso"
    },
    // Then convert OrcaWave
    {
      "vessel_name": "FPSO_OrcaWave",
      "source_type": "orcawave",
      "source_path": "./orcawave/fpso.sim",
      "water_depth": 1200.0,
      "output_dir": "./output/orcawave/fpso"
    }
  ],
  "parallel": false  // Sequential to avoid conflicts
}
```

Then compare:
```bash
# After batch processing completes
diffraction compare \
    ./aqwa/fpso \
    ./orcawave/fpso.sim \
    FPSO \
    -d 1200 \
    -o ./comparison/fpso
```

## Validation Summary

After batch processing, check validation reports:

```python
from pathlib import Path
import json

def summarize_validation(output_root: Path):
    """Summarize validation across all vessels."""

    summary = {
        'pass': [],
        'warning': [],
        'fail': []
    }

    # Find all validation files
    for validation_file in output_root.glob("*//*_validation.json"):
        with open(validation_file) as f:
            report = json.load(f)

        status = report['overall_status']
        vessel = validation_file.stem.replace('_validation', '')

        summary[status.lower()].append(vessel)

    # Print summary
    print("Validation Summary")
    print("=" * 50)
    print(f"PASS: {len(summary['pass'])} vessels")
    for v in summary['pass']:
        print(f"  ✓ {v}")

    print(f"\nWARNING: {len(summary['warning'])} vessels")
    for v in summary['warning']:
        print(f"  ⚠ {v}")

    print(f"\nFAIL: {len(summary['fail'])} vessels")
    for v in summary['fail']:
        print(f"  ✗ {v}")

# Use it
summarize_validation(Path("./output"))
```

## Best Practices

1. **Test one vessel first** before batch processing
2. **Use parallel processing** for independent vessels
3. **Set reasonable max_workers** (typically 4-8)
4. **Organize output directories** by vessel name
5. **Keep configuration files** for documentation
6. **Review validation reports** after batch completes

## Troubleshooting

### Issue: Batch processing very slow

**Solutions:**
- Reduce `max_workers` (too many may cause thrashing)
- Check disk I/O (SSDs recommended for large batches)
- Process in smaller batches

### Issue: Some vessels fail

**Solutions:**
- Check individual validation reports
- Run failed vessels separately with `--verbose`
- Verify source file paths in configuration

### Issue: Out of memory

**Solutions:**
- Reduce `max_workers`
- Process in smaller batches
- Close other applications

## Next Steps

- **Tutorial 04**: [Comparing AQWA vs OrcaWave](04_comparison_example.md)
- **CLI Guide**: [Complete CLI Documentation](../CLI_GUIDE.md)

---

**Previous**: [AQWA Conversion Example](02_aqwa_conversion_example.md)
**Next**: [Comparison Example](04_comparison_example.md)
