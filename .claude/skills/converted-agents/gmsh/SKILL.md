---
name: gmsh
version: 1.0.0
category: engineering
description: The GMSH Agent is a specialized AI module for finite element mesh generation and manipulation. It provides automated mesh generation, quality assessment, optimization, and integration with engineer...
type: reference
tags: []
scripts_exempt: true
---
# Gmsh

# GMSH Agent

## Overview
The GMSH Agent is a specialized AI module for finite element mesh generation and manipulation. It provides automated mesh generation, quality assessment, optimization, and integration with engineering analysis tools.

## Features

### Core Capabilities
- **1D/2D/3D Mesh Generation**: Create meshes for beams, surfaces, and volumes
- **Geometry Processing**: Import and heal CAD geometries (STEP, IGES, STL)
- **Quality Assessment**: Evaluate mesh quality with industry-standard metrics
- **Mesh Optimization**: Improve mesh quality through smoothing and refinement
- **Batch Processing**: Process multiple geometries in parallel
- **Tool Integration**: Export to OrcaFlex, ANSYS, and other analysis tools

### Supported Formats
- **Import**: STEP, IGES, STL, BREP, GEO
- **Export**: MSH, VTK, CGNS, MED, INP, CDB, STL

## Installation

### Prerequisites
```bash
# Ensure Python 3.10+ is installed
python --version

# Install GMSH Python bindings
uv pip install gmsh numpy scipy pyvista pyyaml click
```

### Setup
```bash
# Navigate to agent directory
cd agents/gmsh

# Verify installation
python run_gmsh_agent.py --version
```

## Usage

### Command Line Interface

#### Basic Mesh Generation
```bash
# Generate mesh from geometry
python run_gmsh_agent.py generate \
    --input geometry.step \
    --output mesh.msh \
    --element-size 0.5
```

#### Batch Processing
```bash
# Process multiple files
python run_gmsh_agent.py batch \
    --input-directory ./geometries \
    --output-directory ./meshes \
    --config batch_config.yml \
    --parallel 4
```

#### Quality Assessment
```bash
# Assess mesh quality
python run_gmsh_agent.py assess \
    --mesh mesh.msh \
    --report quality_report.html
```

#### Mesh Optimization
```bash
# Optimize mesh quality
python run_gmsh_agent.py optimize \
    --input mesh.msh \
    --output optimized.msh \
    --method laplacian \
    --iterations 10
```

### Python API

```python
from agents.gmsh import GMSHAgent

# Initialize agent
agent = GMSHAgent(config="agent_config.json")

# Generate mesh
mesh = agent.generate_mesh(
    geometry="platform.step",
    config={
        "algorithm": "frontal-delaunay",
        "element_size": 0.5,
        "element_type": "tetrahedron"
    }
)

# Assess quality
quality = agent.assess_quality(mesh)
print(f"Minimum Jacobian: {quality['min_jacobian']}")
print(f"Average Aspect Ratio: {quality['avg_aspect_ratio']}")

# Optimize if needed
if quality['min_jacobian'] < 0.3:
    optimized = agent.optimize_mesh(
        mesh,
        method="laplacian_smoothing",
        iterations=10
    )

# Export to ANSYS
agent.export_to_ansys(mesh, "model.cdb")
```

### Integration Examples

#### OrcaFlex Integration
```python
# Generate panel mesh for hydrodynamics
panel_mesh = agent.generate_panel_mesh(
    geometry="hull.step",
    panel_size=2.0,
    refinement_zones=["waterline", "appendages"]
)

# Export to OrcaFlex
agent.export_to_orcaflex(panel_mesh, "hull_panels.dat")
```

#### ANSYS Integration
```python
# Generate structural mesh
struct_mesh = agent.generate_mesh(
    geometry="structure.step",
    element_type="hexahedron",
    boundary_layers={
        "walls": {"layers": 5, "growth_rate": 1.2}
    }
)

# Export with boundary conditions
agent.export_to_ansys(
    struct_mesh,
    output="model.cdb",
    named_selections=["inlet", "outlet", "walls"]
)
```

## Configuration

### Agent Configuration (`agent_config.json`)
```json
{
  "configuration": {
    "default_algorithm": "frontal-delaunay",
    "default_element_size": 1.0,
    "quality_thresholds": {
      "min_jacobian": 0.3,
      "max_aspect_ratio": 5.0
    },
    "performance": {
      "parallel_workers": 4,
      "memory_limit_gb": 4
    }
  }
}
```

### Batch Configuration (`batch_config.yml`)
```yaml
mesh_generation:
  algorithm: frontal-delaunay
  element_size:
    min: 0.1
    max: 1.0
  
quality_targets:
  min_jacobian: 0.4
  max_aspect_ratio: 4.0
  
optimization:
  enabled: true
  method: laplacian_smoothing
  iterations: 10
  
export:
  formats: [msh, vtk]
  directory: ./output
```

## Workflows

### Standard Meshing Workflow
1. Import geometry
2. Heal geometry defects
3. Generate mesh
4. Assess quality
5. Optimize if needed
6. Export to desired format

### Batch Processing Workflow
1. Configure batch settings
2. Process files in parallel
3. Generate quality reports
4. Optimize poor meshes
5. Export all results

## Quality Metrics

| Metric | Description | Good Range | Critical |
|--------|-------------|------------|----------|
| Jacobian | Element distortion | > 0.3 | < 0.1 |
| Aspect Ratio | Element shape | < 5.0 | > 10.0 |
| Skewness | Element skew | < 0.7 | > 0.9 |
| Orthogonality | Angle quality | > 0.3 | < 0.1 |

## Performance

- **Mesh Generation**: >100K elements/second
- **Quality Assessment**: <1 second for 1M elements
- **Optimization**: ~10 seconds per iteration for 100K elements
- **Batch Processing**: Linear scaling with CPU cores

## Troubleshooting

### Common Issues

#### GMSH Not Found
```bash
# Install GMSH Python bindings
pip install gmsh

# Verify installation
python -c "import gmsh; print(gmsh.version())"
```

#### Memory Issues with Large Meshes
```python
# Use chunked processing
agent = GMSHAgent(config={
    "performance": {
        "chunk_size": 10000,
        "memory_limit_gb": 2
    }
})
```

#### Poor Quality Meshes
```python
# Increase optimization iterations
optimized = agent.optimize_mesh(
    mesh,
    method="remeshing",
    iterations=20,
    quality_targets={
        "min_jacobian": 0.4,
        "max_aspect_ratio": 3.0
    }
)
```

## Development

### Running Tests
```bash
# Run unit tests
pytest tests/unit/

# Run integration tests
pytest tests/integration/

# Run with coverage
pytest --cov=agents.gmsh tests/
```

### Contributing
1. Follow repository coding standards
2. Add tests for new features
3. Update documentation
4. Submit PR with detailed description

## Support

### Documentation
- [GMSH Documentation](https://gmsh.info/doc/texinfo/gmsh.html)
- [Python API Reference](https://gitlab.onelab.info/gmsh/gmsh/-/blob/master/api/gmsh.py)
- [Repository Wiki](../../docs/agents/gmsh.md)

### Contact
- GitHub Issues: [Create Issue](../../issues)
- Email: support@digitalmodel.com

## License
This agent is part of the DigitalModel repository and follows the same licensing terms.

## Changelog

### Version 1.0.0 (2024-12-24)
- Initial release
- Core mesh generation functionality
- Quality assessment and optimization
- OrcaFlex and ANSYS integration
- Batch processing support

---
*GMSH Agent - Automated Finite Element Mesh Generation*


---

## Source: templates/README.md

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
