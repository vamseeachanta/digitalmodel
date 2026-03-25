# DigitalModel Template Guide

This guide provides comprehensive information about using the DigitalModel input templates for engineering analysis workflows.

## Overview

The DigitalModel template system provides standardized, validated input configurations for various engineering analysis types. Templates ensure consistency, reduce errors, and incorporate engineering best practices.

## Available Templates

### 1. API STD 2RD Template (`api_std_2rd_template.yml`)
**Purpose**: Pipeline design calculations per API STD 2RD guidelines
**File Format**: YAML
**Use Cases**:
- Subsea pipeline design
- Offshore riser analysis
- Pressure vessel calculations
- Multi-code compliance checks

**Key Features**:
- Multiple design code support (API STD 2RD, ASME B31.4/B31.8, API RP 1111)
- Comprehensive material database with API 5L grades
- Environmental condition specifications
- Manufacturing quality factors
- Fatigue analysis parameters

### 2. Plate Capacity Template (`plate_capacity_template.json`)
**Purpose**: Plate buckling analysis using DNV-RP-C201 guidelines
**File Format**: JSON
**Use Cases**:
- Offshore platform deck structures
- Ship hull plate analysis
- Pressure vessel shell buckling
- Structural plate assessment

**Key Features**:
- Multiple material grades with predefined properties
- Comprehensive load case definitions
- Various boundary condition options
- Fatigue analysis capabilities
- Multiple design code compliance checks

### 3. Fatigue Analysis Template (`fatigue_analysis_template.yml`)
**Purpose**: Comprehensive fatigue damage calculation and life assessment
**File Format**: YAML
**Use Cases**:
- Offshore structure fatigue assessment
- Tubular joint analysis
- Welded connection evaluation
- Variable amplitude loading studies

**Key Features**:
- Multiple fatigue curve support (DNV, API, ASME, BS standards)
- Advanced cycle counting with rainflow algorithm
- Mean stress correction methods
- Environmental effects (seawater, CP, temperature)
- Uncertainty and reliability assessment

### 4. Stress Analysis Template (`stress_analysis_template.json`)
**Purpose**: Von Mises stress and multi-axial stress evaluation
**File Format**: JSON
**Use Cases**:
- Pressure vessel stress analysis
- Structural member assessment
- Combined loading evaluation
- Code compliance verification

**Key Features**:
- Multiple failure theories (von Mises, Tresca, Principal stress)
- Comprehensive material database
- Various load combination methods
- Code compliance checking (ASME, API, AISC)
- Environmental effects consideration

### 5. Reservoir Analysis Template (`reservoir_analysis_template.yml`)
**Purpose**: Reservoir characterization and petrophysical evaluation
**File Format**: YAML
**Use Cases**:
- Well log analysis
- Reservoir property modeling
- Volumetric calculations
- Production forecasting

**Key Features**:
- Complete petrophysical analysis workflow
- Multi-well stratigraphic correlation
- Advanced lithofacies classification
- Volumetric calculations with uncertainty
- Production performance analysis

## Getting Started

### 1. Choose the Appropriate Template
Select the template that matches your analysis requirements:
- **Structural Analysis**: Use plate_capacity or stress_analysis templates
- **Pipeline Design**: Use api_std_2rd template
- **Fatigue Assessment**: Use fatigue_analysis template
- **Reservoir Studies**: Use reservoir_analysis template

### 2. Copy Template to Your Project
```bash
# Copy template to your project directory
cp src/digitalmodel/templates/api_std_2rd_template.yml my_project/input.yml

# Copy example files for reference
cp -r examples/input_files/api_std_2rd/ my_project/examples/
```

### 3. Customize Template Parameters
Edit the template file to match your specific project requirements:
- Material properties
- Geometry specifications
- Loading conditions
- Environmental parameters
- Design criteria

### 4. Validate Your Input
Use the template validator to check your configuration:
```bash
# Validate your input file
python src/digitalmodel/templates/template_validator.py my_project/input.yml

# Generate validation report
python src/digitalmodel/templates/template_validator.py my_project/input.yml --report validation_report.txt
```

### 5. Run Analysis
Execute the analysis using the DigitalModel engine:
```bash
# Run analysis with your validated input
python -m digitalmodel my_project/input.yml
```

## Template Structure

### Common Sections

All templates follow a consistent structure with these common sections:

#### 1. Metadata Section
```yaml
# Template identification and version information
template_info:
  name: "Template Name"
  version: "1.0.0"
  description: "Template description"
  created_date: "2025-01-15"
```

#### 2. Default Configuration
```yaml
# Default settings and units
default:
  log_level: DEBUG
  units:
    # Unit specifications
  config:
    overwrite:
      output: true
```

#### 3. Analysis Parameters
```yaml
# Analysis-specific parameters
# Varies by template type
```

#### 4. Validation Rules
```yaml
# Built-in validation constraints
validation_rules:
  required_fields: []
  value_ranges: {}
  consistency_checks: {}
```

### Template-Specific Sections

Each template includes specialized sections relevant to its analysis type:

- **API STD 2RD**: Pipe geometry, material grades, design factors, load cases
- **Plate Capacity**: Plate groups, boundary conditions, DNV parameters
- **Fatigue Analysis**: Fatigue curves, load spectrum, cycle counting parameters
- **Stress Analysis**: Stress components, failure theories, load combinations
- **Reservoir Analysis**: Well data, stratigraphy, petrophysical models

## Best Practices

### 1. Template Customization
- Always start with the base template
- Modify only the parameters relevant to your analysis
- Preserve the overall structure and required fields
- Document any significant changes

### 2. Data Quality
- Use realistic engineering values
- Include appropriate safety factors
- Specify proper units consistently
- Provide complete material properties

### 3. Validation
- Always validate templates before analysis
- Address all errors and warnings
- Review validation reports carefully
- Re-validate after modifications

### 4. Documentation
- Document assumptions and limitations
- Include references to applicable codes
- Maintain version control for templates
- Share validated templates within teams

## Advanced Usage

### Multi-Component Analysis
Use CSV files for analyzing multiple similar components:

```yaml
# Example: Multiple pipeline segments
multi_component:
  enabled: true
  input_file: "pipeline_segments.csv"
  template_mapping:
    segment_id: "component_id"
    outer_diameter_in: "Outer_Pipe.Geometry.Nominal_OD"
    wall_thickness_in: "Outer_Pipe.Geometry.Design_WT"
```

### Parametric Studies
Create parameter variations for sensitivity analysis:

```yaml
# Example: Material grade sensitivity
parametric_study:
  enabled: true
  parameters:
    material_grade: ["API 5L X60", "API 5L X65", "API 5L X70"]
    wall_thickness: [0.5, 0.625, 0.75]
  output_format: "comparison_table"
```

### Integration with External Data
Link templates to external data sources:

```yaml
# Example: Database integration
data_sources:
  material_database:
    type: "postgresql"
    connection: "postgresql://user:pass@host:port/db"
    table: "material_properties"

  load_data:
    type: "csv"
    file_path: "loads/environmental_loads.csv"
```

## Validation Reference

### Error Types

1. **Critical Errors**: Must be fixed before analysis
   - Missing required fields
   - Values outside physical limits
   - Inconsistent material properties
   - Invalid file references

2. **Warnings**: Should be addressed but don't prevent analysis
   - Unusual parameter values
   - Missing optional fields
   - Potential inconsistencies
   - Best practice violations

3. **Information**: Helpful notes and suggestions
   - Parameter recommendations
   - Alternative approaches
   - Reference information

### Common Validation Issues

#### Missing Required Fields
```
ERROR: Required field missing: Outer_Pipe.Geometry.Nominal_OD
Solution: Add the missing field with appropriate value
```

#### Value Out of Range
```
ERROR: Value 5000 outside acceptable range [0, 2000] for wall_thickness
Solution: Check units and provide realistic value
```

#### Material Property Inconsistency
```
WARNING: Unknown material grade: Custom_Steel_X90
Solution: Use standard material grade or define properties
```

#### Engineering Constraint Violation
```
ERROR: Stress 600 MPa exceeds yield strength 355 MPa
Solution: Review loading conditions or material selection
```

## Template Development

### Creating New Templates

1. **Define Analysis Requirements**
   - Identify input parameters needed
   - Determine validation constraints
   - Specify output requirements

2. **Follow Template Structure**
   - Use consistent naming conventions
   - Include comprehensive metadata
   - Provide validation rules
   - Add usage examples

3. **Implement Validation Logic**
   - Add template type detection
   - Define validation rules
   - Include engineering constraints
   - Test with various inputs

4. **Document Template**
   - Provide clear descriptions
   - Include usage examples
   - Reference applicable standards
   - Explain validation rules

### Template Versioning

Use semantic versioning for templates:
- **Major version**: Breaking changes to structure
- **Minor version**: New features, backward compatible
- **Patch version**: Bug fixes, clarifications

## Support and Resources

### Documentation
- Template reference guides in `docs/templates/`
- Code examples in `examples/input_files/`
- API documentation for validation functions

### Community
- GitHub issues for bug reports
- Discussion forums for questions
- Contribution guidelines for improvements

### Training Materials
- Tutorial videos for each template type
- Webinar recordings on best practices
- Workshop materials for advanced usage

## Troubleshooting

### Common Issues

1. **File Format Errors**
   - Check YAML/JSON syntax
   - Verify file encoding (UTF-8)
   - Remove special characters

2. **Validation Failures**
   - Review validation report carefully
   - Check units consistency
   - Verify file paths exist

3. **Analysis Errors**
   - Ensure all required fields present
   - Check parameter reasonableness
   - Review material property completeness

4. **Performance Issues**
   - Reduce model complexity
   - Optimize parameter ranges
   - Use appropriate solver settings

### Getting Help

1. **Check Documentation**: Review this guide and template-specific documentation
2. **Validate Input**: Use template validator to identify issues
3. **Review Examples**: Compare with working example files
4. **Ask Community**: Post questions in discussion forums
5. **Report Bugs**: Submit issues via GitHub

## Conclusion

The DigitalModel template system provides a robust framework for engineering analysis input management. By following the guidelines in this document, you can:

- Create reliable, validated input configurations
- Ensure consistency across projects and teams
- Reduce errors and improve analysis quality
- Leverage engineering best practices and standards

For the most up-to-date information and examples, always refer to the latest documentation and template files in the repository.