# OrcaWave Diffraction Analysis Workflow Automation

## Overview

This specification defines an automated workflow for OrcaWave diffraction analysis that integrates GMsh mesh preprocessing, OrcaWave analysis execution, and OrcaFlex integration pipeline. The system enables seamless transformation from CAD geometry to hydrodynamic analysis results with minimal manual intervention.

## Technical Requirements

### Input Requirements
- **Primary Input**: GMsh mesh files (`.msh` format)
  - Supported: Sea Cypress_0.25 Mesh_Ascii.msh
  - Geometry format: ASCII/Binary STL, OBJ
- **Configuration**: YAML-based analysis parameters
- **Environment**: UV environment with Python 3.10+
- **Dependencies**: GMsh Python bindings, OrcaWave COM API, OrcaFlex integration libraries

### Output Requirements
- **OrcaWave Results**: RAO data, QTF matrices, hydrodynamic coefficients
- **OrcaFlex Integration**: Compatible vessel data files (`.yml`, `.json`)
- **Analysis Reports**: Automated validation and convergence reports
- **Visualization**: Polar plots, frequency response analysis

## Workflow Architecture

### Step 1: Geometry Preparation (GMsh Agent)
**Agent Responsibility**: `agents/gmsh/`
- Input validation and mesh quality assessment
- Mesh format conversion (STL → MSH → GDF)
- Waterline definition and panel optimization
- Quality metrics validation (Jacobian, aspect ratio)

**Technical Implementation**:
```python
# GMsh Agent mesh processing
mesh_quality = gmsh_agent.assess_quality(mesh_file)
if mesh_quality['min_jacobian'] < 0.3:
    mesh_file = gmsh_agent.optimize_mesh(mesh_file, method="laplacian")
gdf_file = gmsh_agent.export_to_gdf(mesh_file, format="orcawave")
```

### Step 2: OrcaWave Input Generation (AI System)
**Automated Process**:
- Parse GMsh mesh geometry into OrcaWave panel format
- Generate OrcaWave input files (`.yml` configuration)
- Apply frequency domain parameters and wave conditions
- Validate input completeness and format compliance

**Configuration Template**:
```yaml
analysis:
  type: diffraction
  frequency_range: [0.1, 2.0, 50]
  wave_directions: [0, 45, 90, 135, 180]
  water_depth: infinite
geometry:
  source: mesh_file.gdf
  coordinate_system: vessel_centered
  waterline: auto_detect
```

### Step 3: Parallel Execution Scripts (AI/User Hybrid)
**Execution Strategy**:
- **AI Responsibility**: Script generation and validation testing
- **User Responsibility**: Licensed OrcaWave execution
- **Parallel Testing**: All scripts validated on test geometries before production

**Validation Protocol**:
```python
# Parallel validation before user execution
test_results = []
for test_case in validation_suite:
    result = validate_orcawave_config(test_case)
    test_results.append(result)
    
if all(r['status'] == 'valid' for r in test_results):
    generate_production_scripts()
else:
    escalate_to_user(validation_errors)
```

### Step 4: Post-Processing Pipeline (AI System)
**Automated Data Extraction**:
- Parse OrcaWave output files (RAOs, QTFs, added mass)
- Generate OrcaFlex-compatible vessel definitions
- Create analysis reports with validation metrics
- Produce visualization outputs (polar plots, frequency responses)

**Output Processing**:
```python
# Post-processing automation
results = orcawave_parser.extract_results(output_directory)
orcaflex_data = results.to_orcaflex_format()
validation_report = generate_validation_report(results)
visualizations = create_analysis_plots(results)
```

## Agent Delegation Strategy

### Primary Agent Assignment
- **GMsh Agent**: Mesh quality control, format conversion, geometry validation
- **OrcaWave Agent**: Analysis configuration, results validation, domain expertise
- **Testing Agent**: Parallel validation, script verification, error detection
- **Documentation Agent**: Report generation, visualization creation

### Inter-Agent Communication
```yaml
delegation_matrix:
  mesh_processing:
    primary: gmsh
    support: [orcawave, testing]
  analysis_execution:
    primary: orcawave
    support: [testing, documentation]
  validation_testing:
    primary: testing
    support: [gmsh, orcawave]
  reporting:
    primary: documentation
    support: [orcawave]
```

### Agent Coordination Protocol
1. **Task Distribution**: Main workflow coordinator assigns sub-tasks to specialized agents
2. **Parallel Execution**: Independent tasks run concurrently across agent pool
3. **Quality Gates**: Each agent validates outputs before passing to next stage
4. **Error Escalation**: Failed tasks trigger user notification with diagnostic information

## Success Criteria

### Performance Metrics
- **Automation Level**: 90% of workflow steps automated
- **Processing Time**: Complete pipeline execution &lt; 30 minutes
- **Error Rate**: &lt; 5% failure rate on validated geometries
- **Validation Accuracy**: 99% agreement with manual verification

### Quality Assurance
- **Mesh Quality**: All meshes meet minimum Jacobian threshold (>0.3)
- **Analysis Convergence**: Frequency domain convergence within 2% tolerance
- **OrcaFlex Integration**: 100% compatibility with vessel model import
- **Documentation**: Complete traceability from mesh to results

### User Experience
- **Setup Time**: &lt; 5 minutes from geometry to analysis start
- **Manual Intervention**: Only for licensed software execution and final review
- **Error Reporting**: Clear diagnostic messages with recommended actions
- **Result Access**: Automated delivery of analysis outputs and reports

## Risk Management

### Technical Risks
- **Mesh Quality Issues**: Automated quality assessment with optimization fallback
- **OrcaWave License**: User-controlled execution with AI-generated validated scripts
- **Format Compatibility**: Extensive testing suite with multiple geometry types
- **Version Dependencies**: Locked dependency versions with compatibility testing

### Mitigation Strategies
- **Parallel Testing**: All configurations tested before user execution
- **Fallback Procedures**: Manual intervention paths for complex geometries
- **Validation Checkpoints**: Quality gates at each workflow stage
- **User Escalation**: Clear procedures for specialized knowledge requirements

## Implementation Approach

### Phase 1: Core Infrastructure (Weeks 1-2)
- Setup automated mesh processing pipeline
- Implement OrcaWave input file generation
- Create basic validation framework
- Establish agent communication protocols

### Phase 2: Integration &amp; Testing (Weeks 3-4)
- Integrate with existing OrcaWave and GMsh agents
- Implement parallel validation system
- Create comprehensive test suite
- Develop error handling and escalation procedures

### Phase 3: Optimization &amp; Documentation (Week 5)
- Performance optimization and parallel processing
- User documentation and workflow guides
- Final integration testing and validation
- Production deployment preparation

## Integration Points

### Existing Systems
- **OrcaWave Agent**: Domain expertise, analysis configuration, results validation
- **GMsh Agent**: Mesh processing, quality assessment, format conversion
- **Repository UV Environment**: Consistent Python environment and dependencies
- **Existing Diffraction Analysis**: Building on `specs/modules/orcawave/diffraction-analysis/`

### External Dependencies
- **OrcaWave COM API**: For licensed analysis execution
- **OrcaFlex Python API**: For vessel model integration
- **GMsh Python Bindings**: For mesh processing and conversion
- **Visualization Libraries**: For automated report generation

## Mathematical Considerations

### Hydrodynamic Analysis
The workflow implements frequency-domain analysis using panel method theory:

$$F_e(\omega) = \sum_{j=1}^{6} A_{ij}(\omega) \ddot{x}_j + B_{ij}(\omega) \dot{x}_j$$

Where:
- $F_e(\omega)$ = Excitation force in frequency domain
- $A_{ij}(\omega)$ = Added mass coefficients
- $B_{ij}(\omega)$ = Damping coefficients
- $x_j$ = Rigid body motions (6 DOF)

### Mesh Convergence Criteria
Panel mesh quality validation based on:

$$Q_{panel} = \frac{A_{min}}{A_{max}} \cdot \frac{J_{min}}{J_{ideal}}$$

Where:
- $Q_{panel}$ = Panel quality metric
- $A_{min}, A_{max}$ = Minimum and maximum panel areas
- $J_{min}$ = Minimum Jacobian determinant
- $J_{ideal}$ = Ideal Jacobian value

## Technology Stack and Implementation Architecture

### Enhanced Core Dependencies
**Managed via UV Environment** (Mandatory):
```yaml
dependencies:
  core_environment:
    - python: ">=3.10,<4.0"
    - uv: "latest"  # Mandatory for environment management
    
  mesh_processing_suite:
    - gmsh: ">=4.12.0"  # Latest stable with API improvements
    - numpy: ">=1.24.0"  # Enhanced array processing
    - scipy: ">=1.10.0"  # Advanced optimization algorithms
    - trimesh: ">=3.15.0"  # Mesh validation and repair
    
  analysis_integration_framework:
    - pywin32: "latest"  # OrcaWave COM API access
    - pyyaml: ">=6.0.1"  # Configuration file processing
    - pandas: ">=2.0.0"  # Advanced data processing
    - openpyxl: ">=3.1.0"  # Excel file handling
    - h5py: ">=3.8.0"  # High-performance data storage
    
  visualization_and_reporting:
    - matplotlib: ">=3.7.0"  # Static plotting
    - plotly: ">=5.15.0"  # Interactive visualizations
    - seaborn: ">=0.12.0"  # Statistical plotting
    - pillow: ">=10.0.0"  # Image processing
    - reportlab: ">=4.0.0"  # PDF report generation
    
  parallel_processing:
    - concurrent-futures: "built-in"  # Python stdlib
    - threading: "built-in"  # Thread-based parallelism
    - multiprocessing: "built-in"  # Process-based parallelism
    - joblib: ">=1.3.0"  # Enhanced parallel computing
    
  development_and_testing:
    - pytest: ">=7.4.0"  # Testing framework
    - pytest-xdist: ">=3.3.0"  # Parallel test execution
    - black: ">=23.0.0"  # Code formatting
    - mypy: ">=1.5.0"  # Type checking
```

### Production Environment Specifications
**Mandatory UV Environment Usage**:
```bash
# All operations MUST use UV environment
uv run python generate_orcawave_input.py --mesh-file input.msh
uv run python execute_orcawave_parallel.py --config config.yml
uv run python postprocess_orcawave_parallel.py --results-dir ./outputs

# Package management through UV
uv add numpy scipy matplotlib  # Updates pyproject.toml automatically
uv run pytest tests/  # Ensures consistent test environment
```

**System Requirements**:
- **Operating System**: Windows 10/11 (required for OrcaWave COM API)
- **Python Environment**: 3.10+ managed exclusively through UV
- **Memory**: 16GB+ recommended for large mesh processing and parallel operations
- **Storage**: 500GB+ for analysis results, mesh files, and result archives
- **CPU**: Multi-core (8+ cores recommended) for optimal parallel processing
- **License Requirements**: OrcaWave COM license for production analysis execution

---

*This specification ensures automated, efficient, and reliable OrcaWave diffraction analysis with minimal user intervention while maintaining full traceability and quality assurance.*