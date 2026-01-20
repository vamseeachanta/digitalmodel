# GMSH Agent Geometry Templates

This directory contains parametric geometry templates for common offshore engineering structures and features.

## Available Templates

### 1. Offshore Platform (`offshore_platform.geo`)
Parametric model for typical offshore platform structures including:
- Configurable deck dimensions and elevation
- Variable number of jacket legs (4, 6, 8)
- Automatic bracing generation with X-bracing
- Helideck with configurable radius
- Pile foundations with mudline penetration
- Adaptive mesh refinement near structural members

**Key Parameters:**
- Platform dimensions (length, width, height)
- Water depth and mudline penetration
- Number of legs and spacing
- Helideck configuration
- Mesh density control

### 2. Mooring Line (`mooring_line.geo`)
Parametric catenary mooring line model featuring:
- Chain-wire-chain configuration
- Catenary shape calculation
- Touchdown point modeling
- Seabed interaction zone
- Optional buoyancy modules
- Detailed chain link geometry (for small models)

**Key Parameters:**
- Total line length and segment lengths
- Fairlead and anchor positions
- Line properties (diameter, weight)
- Pretension and catenary parameters
- Number of discretization segments

### 3. Seabed Terrain (`seabed_terrain.geo`)
Parametric seabed terrain with multiple features:
- General slope with configurable gradient
- Valley features with adjustable dimensions
- Mounds and hills
- Scour pits around structures
- Sand ridges and ripples
- Multiple sediment layers

**Key Parameters:**
- Domain dimensions
- Terrain feature toggles (slope, valley, mound, etc.)
- Feature dimensions and positions
- Grid resolution
- Sediment layer configuration

## Usage

### Direct GMSH Usage
```bash
# Open template in GMSH GUI
gmsh offshore_platform.geo

# Generate mesh from command line
gmsh offshore_platform.geo -3 -o platform.msh
```

### Python API Usage
```python
from agents.gmsh.utilities.template_manager import TemplateManager

# Initialize manager
manager = TemplateManager()

# List available templates
templates = manager.list_templates()

# Generate geometry with custom parameters
params = {
    'deck_length': 50.0,
    'water_depth': 150.0,
    'num_legs': 6
}
geo_file = manager.generate_geometry('offshore_platform', params)

# Create parameter study
param_ranges = {
    'water_depth': [100, 150, 200],
    'num_legs': [4, 6, 8]
}
files = manager.create_parameter_study('offshore_platform', param_ranges)
```

### CLI Usage
```bash
# Generate geometry from template
python -m agents.gmsh generate --template offshore_platform --param water_depth=150

# Run parameter study
python -m agents.gmsh parameter-study --template mooring_line \
    --vary total_length=1000,1500,2000 --vary pretension=300,500,700
```

## Parameter Customization

All templates use GMSH's `DefineNumber` syntax for parameters, allowing:
- Interactive modification in GMSH GUI
- Command-line parameter override
- Programmatic control via API
- Parameter constraints (min/max values)

### Example Parameter Definition
```geo
water_depth = DefineNumber[100.0, Name "Environment/Water Depth (m)"];
```

This creates a parameter that:
- Has default value of 100.0
- Appears in GUI under "Environment" category
- Shows units in meters
- Can be modified at runtime

## Template Development

### Creating New Templates

1. **Start with a .geo file** in this directory
2. **Use DefineNumber** for all configurable parameters
3. **Add descriptive comments** at the top of the file
4. **Group parameters** by category (e.g., Platform/, Environment/)
5. **Define physical groups** for boundary conditions
6. **Include mesh refinement fields** where appropriate

### Template Structure
```geo
// Template Name and Description
// Author: Your Name
// Version: 1.0

// ============================================================================
// PARAMETERS
// ============================================================================
param1 = DefineNumber[default, Name "Category/Parameter Name (unit)"];

// ============================================================================
// GEOMETRY CREATION
// ============================================================================
// Geometry construction code

// ============================================================================
// PHYSICAL GROUPS
// ============================================================================
Physical Surface("Surface_Name") = {surface_ids[]};

// ============================================================================
// MESH FIELDS
// ============================================================================
// Mesh refinement configuration
```

### Best Practices

1. **Parameter Naming**: Use descriptive names with units
2. **Default Values**: Choose reasonable defaults for typical use cases
3. **Validation**: Include parameter constraints where applicable
4. **Documentation**: Comment complex geometry operations
5. **Modularity**: Break complex geometries into logical sections
6. **Physical Groups**: Define groups for all important features
7. **Mesh Control**: Provide appropriate mesh refinement controls

## Integration with Workflows

Templates can be used in batch processing workflows:

```yaml
# workflow.yml
input:
  template: offshore_platform
  parameter_file: platform_params.json
  
stages:
  - name: geometry_generation
    template_params:
      water_depth: 150
      num_legs: 6
  - name: mesh_generation
    mesh_size: adaptive
  - name: quality_check
    min_quality: 0.7
```

## Template Validation

Validate templates before use:

```python
# Validate template syntax and parameters
results = manager.validate_template('offshore_platform')
print(f"Valid: {results['valid']}")
print(f"Warnings: {results['warnings']}")
```

## Export and Documentation

Generate documentation for all templates:

```python
# Export as Markdown
manager.export_template_docs('templates.md', format='markdown')

# Export as HTML
manager.export_template_docs('templates.html', format='html')

# Export as JSON
manager.export_template_docs('templates.json', format='json')
```

## Contributing

To contribute new templates:

1. Create a .geo file following the structure above
2. Test with various parameter combinations
3. Add documentation to this README
4. Submit with example use cases

## License

These templates are part of the GMSH Agent module and follow the repository's licensing terms.