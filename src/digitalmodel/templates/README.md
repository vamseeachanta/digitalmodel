# DigitalModel Input Templates

This directory contains comprehensive input templates for all migrated engineering analysis modules in DigitalModel.

## Quick Start

1. **Choose a template** that matches your analysis type
2. **Copy the template** to your project directory
3. **Customize parameters** for your specific case
4. **Validate the input** using the template validator
5. **Run the analysis** with DigitalModel engine

## Available Templates

| Template | File | Format | Use Case |
|----------|------|--------|----------|
| API STD 2RD | `api_std_2rd_template.yml` | YAML | Pipeline design and pressure vessel analysis |
| Plate Capacity | `plate_capacity_template.json` | JSON | Plate buckling analysis per DNV-RP-C201 |
| Fatigue Analysis | `fatigue_analysis_template.yml` | YAML | Fatigue damage calculation and life assessment |
| Stress Analysis | `stress_analysis_template.json` | JSON | Von Mises stress and multi-axial stress evaluation |
| Reservoir Analysis | `reservoir_analysis_template.yml` | YAML | Reservoir characterization and petrophysical evaluation |

## Template Validation

Use the template validator to ensure your input files are correct:

```bash
# Basic validation
python template_validator.py your_input_file.yml

# Validation with detailed report
python template_validator.py your_input_file.yml --report validation_report.txt --verbose
```

The validator checks for:
- ✅ Required fields presence
- ✅ Parameter value ranges
- ✅ Engineering constraint compliance
- ✅ Material property consistency
- ✅ Units compatibility
- ✅ File format correctness

## Example Usage

### API STD 2RD Pipeline Analysis
```bash
# Copy template
cp api_std_2rd_template.yml my_pipeline.yml

# Edit parameters (material, geometry, pressure, etc.)
# Validate
python template_validator.py my_pipeline.yml

# Run analysis
python -m digitalmodel my_pipeline.yml
```

### Plate Buckling Analysis
```bash
# Copy template
cp plate_capacity_template.json deck_plate.json

# Edit plate geometry and loading
# Validate
python template_validator.py deck_plate.json

# Run analysis
python -m digitalmodel deck_plate.json
```

## Template Features

### 🎯 Engineering Standards Compliance
- API STD 2RD-2013 for pipeline design
- DNV-RP-C201 for plate buckling
- DNV-RP-C203 for fatigue analysis
- ASME VIII for pressure vessels
- Multiple code options per analysis type

### 🔧 Comprehensive Material Database
- API 5L pipeline grades (X42 through X80)
- Structural steel grades (S235, S355, S420, S460)
- ASTM steel specifications
- Aluminum alloys
- Stainless steel grades
- Complete property definitions

### 📊 Advanced Analysis Options
- Multi-component batch processing
- Parametric studies and sensitivity analysis
- Uncertainty quantification
- Environmental effects modeling
- Fatigue and fracture mechanics
- Non-linear analysis capabilities

### 🛡️ Built-in Quality Assurance
- Comprehensive validation rules
- Engineering constraint checking
- Consistency verification
- Best practice enforcement
- Automated error detection

## File Organization

```
templates/
├── README.md                           # This file
├── template_validator.py               # Validation utility
├── api_std_2rd_template.yml           # Pipeline design template
├── plate_capacity_template.json       # Plate buckling template
├── fatigue_analysis_template.yml      # Fatigue analysis template
├── stress_analysis_template.json      # Stress analysis template
└── reservoir_analysis_template.yml    # Reservoir analysis template
```

## Template Structure

Each template includes:

1. **Metadata Section**: Template identification and version information
2. **Default Configuration**: Units, logging, and output settings
3. **Analysis Parameters**: Problem-specific input parameters
4. **Material Properties**: Comprehensive material database
5. **Validation Rules**: Built-in quality checks
6. **Usage Examples**: Inline documentation and examples
7. **References**: Applicable engineering standards

## Advanced Features

### Multi-Component Analysis
Process multiple similar components using CSV input:

```yaml
multi_component:
  enabled: true
  input_file: "components.csv"
  batch_processing: true
```

### Parametric Studies
Automated parameter variation studies:

```yaml
parametric_study:
  parameters:
    wall_thickness: [0.5, 0.625, 0.75]
    material_grade: ["X60", "X65", "X70"]
  analysis_matrix: "full_factorial"
```

### Database Integration
Connect to external databases:

```yaml
data_sources:
  material_db: "postgresql://host/materials"
  load_data: "api://internal/loads"
```

## Validation Details

### Error Categories

1. **🔴 Critical Errors**: Analysis cannot proceed
   - Missing required fields
   - Values outside physical limits
   - File format errors
   - Reference errors

2. **🟡 Warnings**: Should be addressed
   - Unusual parameter values
   - Missing optional fields
   - Best practice violations
   - Potential inconsistencies

3. **🔵 Information**: Helpful guidance
   - Parameter recommendations
   - Alternative approaches
   - Reference information

### Common Validation Checks

- **Geometric Constraints**: Realistic dimensions and ratios
- **Material Properties**: Standard grades and consistent properties
- **Loading Conditions**: Reasonable stress levels and load combinations
- **Environmental Effects**: Appropriate temperature and pressure ranges
- **Safety Factors**: Industry-standard safety margins
- **Code Compliance**: Adherence to specified design standards

## Best Practices

### Template Usage
1. **Always start with the base template** - Don't create from scratch
2. **Validate before analysis** - Use the template validator
3. **Document modifications** - Track changes and assumptions
4. **Version control** - Keep template versions with projects
5. **Share validated templates** - Promote team consistency

### Parameter Selection
1. **Use realistic values** - Based on actual project data
2. **Include appropriate margins** - Follow industry practices
3. **Check units consistently** - Avoid unit conversion errors
4. **Reference standards** - Cite applicable codes and guidelines
5. **Consider uncertainties** - Account for parameter variations

### Quality Assurance
1. **Peer review** - Have templates reviewed by colleagues
2. **Benchmark testing** - Compare with known solutions
3. **Sensitivity studies** - Understand parameter impacts
4. **Documentation** - Maintain clear analysis records
5. **Continuous improvement** - Update based on lessons learned

## Contributing

To contribute new templates or improvements:

1. **Follow the template structure** established in existing files
2. **Include comprehensive validation rules** for engineering constraints
3. **Provide usage examples** and documentation
4. **Test thoroughly** with various input scenarios
5. **Submit via pull request** with detailed description

## Support

For help with templates:

1. **Check the validation report** for specific error guidance
2. **Review example files** in `examples/input_files/`
3. **Consult the Template Guide** at `docs/TEMPLATE_GUIDE.md`
4. **Ask the community** via GitHub discussions
5. **Report issues** through GitHub issues

## Version History

- **v1.0.0**: Initial template release with five analysis types
- Comprehensive validation system
- Complete material databases
- Industry standard compliance
- Advanced analysis features

## License

These templates are part of the DigitalModel project and follow the same licensing terms. See the main project LICENSE file for details.