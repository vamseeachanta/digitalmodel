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