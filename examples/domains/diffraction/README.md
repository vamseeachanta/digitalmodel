# Diffraction Analysis Examples

This directory contains example configurations and usage patterns for the diffraction analysis CLI tools.

## Files

### batch_config_example.json

Example batch processing configuration for converting multiple vessels.

**Features demonstrated**:
- Mixed AQWA and OrcaWave sources
- Different water depths per vessel
- Custom output directories
- Selective format export
- Optional validation control
- Parallel processing configuration

**Usage**:
```bash
# Copy and modify for your vessels
cp batch_config_example.json my_fleet_config.json

# Edit paths and parameters
nano my_fleet_config.json

# Run batch processing
diffraction batch my_fleet_config.json
```

## Configuration Options

### Per-Vessel Configuration

- **vessel_name**: Unique identifier for the vessel
- **source_type**: `"aqwa"` or `"orcawave"`
- **source_path**: Path to analysis folder (AQWA) or model file (OrcaWave)
- **water_depth**: Water depth in meters
- **output_dir**: Output directory for this vessel's results
- **validate**: `true` to run validation checks, `false` to skip
- **formats**: List of export formats (see below)

### Export Formats

- `"all"` - All available formats
- `"vessel_type"` - OrcaFlex vessel type YAML
- `"rao_csv"` - RAO data CSV
- `"added_mass_csv"` - Added mass matrices CSV
- `"damping_csv"` - Damping matrices CSV
- `"excel"` - Complete Excel workbook
- `"summary"` - Summary JSON

### Global Settings

- **parallel**: Enable parallel processing (`true`/`false`)
- **max_workers**: Maximum number of parallel workers (1-8 recommended)

## Example Workflows

### Single Vessel Conversion

```bash
# AQWA conversion
diffraction convert-aqwa ./data/aqwa/fpso_a FPSO_A -d 1200 -o ./output/fpso_a

# OrcaWave conversion
diffraction convert-orcawave ./data/orcawave/tlp_b.sim -d 800 -o ./output/tlp_b
```

### Comparison

```bash
# Compare AQWA vs OrcaWave results
diffraction compare \
  ./data/aqwa/fpso_a \
  ./data/orcawave/fpso_a.sim \
  FPSO_A \
  -d 1200 \
  -o ./comparison/fpso_a
```

### Batch Processing

```bash
# Process entire fleet
diffraction batch batch_config_example.json

# Process with custom configuration
diffraction batch my_custom_config.json
```

## Directory Structure

Recommended project structure:

```
project/
├── data/
│   ├── aqwa/
│   │   ├── fpso_a_analysis/
│   │   ├── spar_c_analysis/
│   │   └── ship_e_analysis/
│   └── orcawave/
│       ├── tlp_b_model.sim
│       └── semi_d_model.sim
├── output/
│   ├── fpso_a/
│   ├── tlp_b/
│   ├── spar_c/
│   ├── semi_d/
│   └── ship_e/
├── comparison/
│   └── fpso_a/
└── configs/
    └── batch_config.json
```

## Tips

1. **Start Small**: Test with one vessel before batch processing
2. **Validate First**: Always run with validation enabled initially
3. **Check Reports**: Review validation JSON files for data quality
4. **Parallel Processing**: Use max_workers = CPU cores - 1 for best performance
5. **Incremental**: Add vessels to batch config incrementally
6. **Backup**: Keep original analysis files separate from output

## See Also

- [CLI Guide](../../docs/domains/diffraction/CLI_GUIDE.md) - Complete CLI documentation
- [Module Documentation](../../docs/domains/diffraction/) - Technical reference
- [API Documentation](../../docs/domains/diffraction/API_REFERENCE.md) - Programmatic usage
