# GMSH Agent Creation - Prompt Documentation

## Original User Request
**Date:** 2024-12-24
**User:** Create gmsh agent using /create-module-agent. utilize the repo module documents along with the following external resources. this agent will be used for any finite element mesh related tasks

**Resources Provided:**
- https://gmsh.info/
- https://gmsh.info/doc/texinfo/gmsh.html
- https://pypi.org/project/gmsh/
- https://github.com/sasobadovinac/gmsh

## AI Analysis and Clarification

### Initial Assessment
The user requested creation of a GMSH agent module for finite element mesh generation tasks. This aligns with the repository's engineering analysis focus and will complement existing modules (OrcaFlex, ANSYS, Signal Analysis).

### Key Decisions Made
1. **Scope**: Focused on engineering mesh generation for offshore/marine structures
2. **Integration**: Designed to work with existing OrcaFlex and ANSYS workflows
3. **Architecture**: Following repository's mandatory agent structure pattern
4. **Standards**: Adhering to CLI consistency standards and module organization

### Assumptions
- Agent will be used primarily for engineering analysis preprocessing
- Integration with existing modules is a priority
- Batch processing capability is essential
- Quality metrics and optimization are required features

## Specification Development Process

### Step 1: Research and Analysis
Analyzed GMSH capabilities based on:
- Core mesh generation algorithms (Delaunay, Frontal, etc.)
- Geometry processing capabilities (CAD import, boolean operations)
- Python API structure and methods
- Integration patterns with FEA/CFD tools

### Step 2: Architecture Design
Designed agent structure following repository patterns:
```
agents/gmsh/
├── agent_config.json
├── capabilities.yml
├── run_gmsh_agent.py
├── workflows/
├── templates/
└── utilities/
```

### Step 3: Feature Prioritization
1. **Core Features** (Must Have)
   - Basic mesh generation (1D/2D/3D)
   - Geometry import (STEP, IGES, STL)
   - Quality metrics evaluation
   - Batch processing

2. **Integration Features** (Should Have)
   - OrcaFlex panel mesh generation
   - ANSYS format export
   - Format conversion utilities

3. **Advanced Features** (Nice to Have)
   - Adaptive refinement
   - Parallel processing optimization
   - Template geometry library

## Curated Reuse Prompt

### For Creating Similar Agent Modules
```
Create a specialized agent module for [TOOL_NAME] following these requirements:

1. **Agent Structure**: Create in `agents/[module_name]/` with:
   - agent_config.json (configuration)
   - capabilities.yml (capability definitions)
   - run_[module]_agent.py (main runner)
   - workflows/ (predefined workflows)
   - utilities/ (helper functions)
   - templates/ (domain-specific templates)

2. **Core Functionality**: Implement:
   - Basic [tool] operations
   - Batch processing capability
   - Quality validation/metrics
   - Error recovery mechanisms
   - Progress tracking and logging

3. **Integration Requirements**:
   - CLI interface with standard parameters (--input-directory, --output-directory, --pattern, etc.)
   - Python API for programmatic use
   - Integration with existing modules (specify which)
   - Export to common formats

4. **Technical Specifications**:
   - Performance targets (specify metrics)
   - Scalability requirements
   - Memory constraints
   - Parallel processing support

5. **Documentation Requirements**:
   - Comprehensive README.md
   - API documentation
   - Usage examples
   - Integration guides
   - Troubleshooting section

6. **Testing Requirements**:
   - Unit tests (>80% coverage)
   - Integration tests
   - Performance benchmarks
   - Validation suite

Please create:
1. specs/modules/[module]/[feature]/spec.md - Technical specification
2. specs/modules/[module]/[feature]/tasks.md - Task breakdown
3. specs/modules/[module]/[feature]/prompt.md - Prompt documentation
4. specs/modules/[module]/[feature]/executive-summary.md - Stakeholder summary
5. specs/modules/[module]/[feature]/diagrams/architecture.mermaid - System diagram
```

### For GMSH-Specific Tasks
```
Using the GMSH agent module, implement [specific task]:

Context:
- GMSH Python API available via `import gmsh`
- Agent located at `agents/gmsh/`
- Supports mesh generation, optimization, and format conversion

Requirements:
1. [Specific geometry/mesh requirements]
2. [Quality metrics targets]
3. [Integration needs]
4. [Performance constraints]

Expected outputs:
- Mesh files in [formats]
- Quality report
- Integration with [modules]

Use the GMSHAgent class:
```python
from digitalmodel.gmsh import GMSHAgent
agent = GMSHAgent(config="config.yml")
# Implementation here
```
```

## Implementation Guidance

### Quick Start Commands
```bash
# Create agent structure
python tools/create-module-agent.py gmsh

# Install dependencies
uv pip install gmsh numpy scipy pyvista pyyaml click

# Run agent
python agents/gmsh/run_gmsh_agent.py --show-capabilities

# Batch processing
python -m digitalmodel.gmsh.agent \
    --input-directory ./geometries \
    --output-directory ./meshes \
    --config mesh_config.yml
```

### Configuration Template
```yaml
# gmsh_config.yml
mesh_generation:
  algorithm: "frontal-delaunay"
  element_size:
    min: 0.1
    max: 1.0
  element_type: "tetrahedron"
  
quality_targets:
  min_jacobian: 0.3
  max_aspect_ratio: 5.0
  
optimization:
  iterations: 10
  smoothing: "laplacian"
```

## Lessons Learned

### Best Practices Identified
1. **Modular Design**: Separate concerns (generation, optimization, quality, export)
2. **Configuration-Driven**: Use YAML for all user-facing configuration
3. **Progressive Enhancement**: Start with basic features, add complexity incrementally
4. **Integration First**: Design with integration points from the beginning

### Potential Challenges
1. **GMSH Installation**: May require system-level dependencies
2. **Large Mesh Memory**: Need streaming/chunking for very large meshes
3. **Performance Tuning**: Balancing quality vs. speed
4. **Format Compatibility**: Different tools expect different mesh formats

### Recommended Approach
1. Start with basic mesh generation
2. Add quality metrics early
3. Implement one integration (OrcaFlex) first
4. Build batch processing on proven single-file operations
5. Optimize performance after functionality is complete

## Future Enhancements

### Potential Extensions
1. **Cloud Processing**: For very large meshes
2. **Machine Learning**: Adaptive mesh refinement based on solution
3. **GUI Integration**: Visual mesh inspection and editing
4. **Mesh Database**: Store and retrieve common meshes
5. **Automated Optimization**: AI-driven mesh quality improvement

### Integration Opportunities
1. **CAD Integration**: Direct link to CAD tools
2. **Solver Coupling**: Direct submission to FEA solvers
3. **Post-Processing**: Mesh-based result visualization
4. **Workflow Automation**: End-to-end analysis pipelines

## References and Resources

### GMSH Documentation
- Official Documentation: https://gmsh.info/doc/texinfo/gmsh.html
- Python API Reference: https://gitlab.onelab.info/gmsh/gmsh/-/blob/master/api/gmsh.py
- Tutorial Collection: https://gitlab.onelab.info/gmsh/gmsh/-/tree/master/tutorials

### Related Repository Resources
- Agent Creation Guide: @.agent-os/standards/agent-personas.md
- CLI Standards: @CLAUDE.md (CLI Consistency Standards section)
- Module Organization: @.agent-os/product/architecture.md
- Testing Standards: @.agent-os/standards/testing.md

### Industry Standards
- Mesh Quality Metrics: ISO 10303-104
- CAD Exchange: STEP AP203/AP214
- FEA Standards: NAFEMS Guidelines

---
*Document Created: 2024-12-24*
*Last Updated: 2024-12-24*
*Status: Complete*