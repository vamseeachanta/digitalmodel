# OrcaFlex Agent Specification

## Mission

Automate OrcaFlex model creation, validation, execution, and post-processing for CALM buoy and mooring analysis projects, encoding proven patterns from successful manual work into reusable, validated workflows.

## Goals

### Primary Objectives
1. **Eliminate manual file creation** - Generate complete OrcaFlex project structures from high-level specifications
2. **Prevent configuration errors** - Validate all inputs before OrcaFlex execution
3. **Standardize workflows** - Ensure consistent patterns across all projects
4. **Accelerate project delivery** - Reduce setup time from days to hours

### Success Criteria
- Generate valid OrcaFlex files without manual editing
- Zero OrcaFlex errors from generated configurations
- 80% reduction in project setup time
- Reusable templates for all common scenarios

## Scope

### In Scope
✅ CALM buoy mooring analysis (primary use case)
✅ Base file generation (modular structure)
✅ Environmental file generation (wave, wind, current)
✅ Analysis model generation (flat file structure)
✅ Vessel configuration (types and instances)
✅ Post-processing configuration
✅ Validation and error checking
✅ Workflow orchestration

### Out of Scope (Future Phases)
❌ Riser analysis configurations
❌ Installation analysis
❌ Custom Python scripting
❌ GUI/web interface
❌ Machine learning optimizations

## User Stories

### US-1: Generate Complete CALM Buoy Project
**As a** Marine Engineer
**I want to** generate a complete OrcaFlex CALM buoy project structure from a simple specification
**So that** I can start analysis immediately without manual file creation

**Acceptance Criteria:**
- Single command creates all base files, env files, and analysis models
- Generated files are OrcaFlex-compatible
- File structure matches proven patterns
- All references are correctly linked

### US-2: Validate Vessel Configuration
**As a** Marine Engineer
**I want to** validate my vessel configuration before running OrcaFlex
**So that** I catch errors early and avoid wasted simulation time

**Acceptance Criteria:**
- RAO amplitudes validated (all positive)
- Matrix structures validated (3x3 tensors, 6x6 AMD)
- AMD method compatibility checked
- Clear error messages with fix suggestions

### US-3: Generate Environmental Files for All Conditions
**As a** Marine Engineer
**I want to** generate environmental files for all headings and return periods
**So that** I have consistent, reusable env configurations

**Acceptance Criteria:**
- Generate for 24 headings (000-345° in 15° increments)
- Generate for multiple return periods (1yr, 10yr, 100yr)
- Standalone wave, wind, current components
- Composite env files that include components

### US-4: Auto-Generate Post-Processing Configuration
**As a** Marine Engineer
**I want to** auto-generate post-processing configuration from my model
**So that** I don't manually configure object names and variables

**Acceptance Criteria:**
- Inspect .sim files to detect objects (Vessel1, CALMTop, Mooring lines)
- Generate dm_*.yml with correct object names
- Include standard outputs (6DOF, tensions)
- Configure result paths automatically

### US-5: Run Complete Workflow
**As a** Marine Engineer
**I want to** run a complete analysis workflow with one command
**So that** I can go from specification to results without manual steps

**Acceptance Criteria:**
- Single command executes: generate → validate → simulate → post-process
- Progress reporting at each stage
- Error handling with rollback capability
- Results summary generated

## Architecture

### High-Level Components

```
┌─────────────────────────────────────────────────────────────┐
│                    OrcaFlex Agent CLI                        │
│  orcaflex-agent <command> [options]                         │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     Command Router                           │
│  generate, validate, simulate, postprocess, workflow        │
└─────────────────────────────────────────────────────────────┘
                              │
            ┌─────────────────┼─────────────────┐
            ▼                 ▼                 ▼
┌───────────────────┐ ┌───────────────┐ ┌──────────────┐
│  File Generators  │ │  Validators   │ │  Executors   │
│                   │ │               │ │              │
│ • Base files      │ │ • RAO check   │ │ • Simulator  │
│ • Env files       │ │ • Matrix fmt  │ │ • Post-proc  │
│ • Analysis models │ │ • Object names│ │ • Workflow   │
│ • Post-proc cfg   │ │ • YAML syntax │ │              │
└───────────────────┘ └───────────────┘ └──────────────┘
            │                 │                 │
            └─────────────────┼─────────────────┘
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     Template Engine                          │
│  Jinja2 templates + Parameter substitution                  │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     Pattern Library                          │
│  Proven patterns from CALM buoy projects                    │
└─────────────────────────────────────────────────────────────┘
```

### Technology Stack

**Language**: Python 3.11+
**CLI Framework**: Click (argparse alternative with better UX)
**Template Engine**: Jinja2
**Validation**: Pydantic (data models) + Custom validators
**YAML**: PyYAML / ruamel.yaml (preserve formatting)
**OrcaFlex Integration**: OrcFxAPI
**Testing**: pytest
**Documentation**: Sphinx + MyST

### Directory Structure

```
src/digitalmodel/agents/orcaflex/
├── __init__.py
├── cli.py                      # Click-based CLI entry point
├── commands/
│   ├── generate.py             # File generation commands
│   ├── validate.py             # Validation commands
│   ├── simulate.py             # Simulation execution
│   ├── postprocess.py          # Post-processing commands
│   └── workflow.py             # Workflow orchestration
├── generators/
│   ├── base_files.py           # Base file generation
│   ├── env_files.py            # Environmental file generation
│   ├── analysis_models.py      # Analysis model generation
│   └── postproc_config.py      # Post-processing config generation
├── validators/
│   ├── rao_validator.py        # RAO amplitude/phase validation
│   ├── matrix_validator.py     # Tensor/AMD matrix validation
│   ├── object_validator.py     # Object name consistency
│   └── yaml_validator.py       # YAML syntax/structure
├── templates/
│   ├── base_files/
│   │   ├── 01_general.yml.j2
│   │   ├── 02_var_data.yml.j2
│   │   ├── 03_environment.yml.j2
│   │   ├── 04_vessel_type.yml.j2
│   │   └── ...
│   ├── env/
│   │   ├── wave.yml.j2
│   │   ├── wind.yml.j2
│   │   ├── current.yml.j2
│   │   └── composite_env.yml.j2
│   ├── analysis/
│   │   └── analysis_model.yml.j2
│   └── postproc/
│       └── dm_vessel.yml.j2
├── patterns/
│   ├── calm_buoy.py            # CALM buoy pattern definitions
│   ├── vessel_types.py         # Common vessel configurations
│   └── env_conditions.py       # Standard environmental conditions
└── utils/
    ├── file_utils.py           # File I/O helpers
    ├── orcaflex_utils.py       # OrcaFlex API helpers
    └── naming.py               # Naming convention utilities
```

## Commands Specification

### 1. Generate Base Files

```bash
orcaflex-agent generate base-files \
  --project calm_buoy_baltic \
  --vessel crowley650_atb \
  --output projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/base_files
```

**Generates:**
- 01_general.yml
- 02_var_data.yml
- 03_environment.yml
- 04_vessel_crowley.yml (includes _04_vessel_type_crowley650_atb.yml)
- 05_lines.yml (includes _05_line_types.yml, _07_lines.yml)
- 06_buoys.yml
- 08_groups.yml
- _90_calculated_positions_simple.yml
- calm_buoy_simple_base_crowley.yml (master include file)

**Options:**
- `--project` - Project name/identifier
- `--vessel` - Vessel type (from catalog or custom)
- `--buoy-type` - CALM buoy configuration (discretized/simple)
- `--mooring-lines` - Number of mooring lines (default: 6)
- `--output` - Output directory
- `--template` - Template set to use (default: calm_buoy)

### 2. Generate Environmental Files

```bash
orcaflex-agent generate env-files \
  --return-periods 1,10,100 \
  --headings all \
  --conditions baltic_sea \
  --output projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/base_files/env
```

**Generates:**
For each combination of return period and heading:
- `_env_wave_{period}_{heading}.yml`
- `_env_wind_{period}_{heading}.yml`
- `_env_curr_{speed}_{heading}.yml`
- `env_{period}_{heading}.yml` (composite)

**Options:**
- `--return-periods` - Comma-separated list (1,10,100)
- `--headings` - "all" or comma-separated (0,30,60,...)
- `--conditions` - Environmental condition profile (baltic_sea, north_sea, gulf_mexico)
- `--output` - Output directory
- `--standalone-only` - Generate only component files, not composites

### 3. Generate Analysis Models

```bash
orcaflex-agent generate analysis-models \
  --base-file ../base_files/calm_buoy_simple_base_crowley.yml \
  --env-dir ../base_files/env \
  --conditions "1yr:000,030,060;10yr:000,030" \
  --output projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/analysis_models
```

**Generates:**
For each condition:
- `CALM_atb_{heading}_{period}_simple.yml` (minimal YAML with references)

**Options:**
- `--base-file` - Path to master base file
- `--env-dir` - Directory containing env files
- `--conditions` - Condition specification (return_period:heading1,heading2;...)
- `--prefix` - File name prefix (default: project name)
- `--output` - Output directory
- `--include-no-load` - Also generate no-load cases

### 4. Generate Post-Processing Config

```bash
orcaflex-agent generate postproc \
  --model-dir projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/analysis_models \
  --inspect-sim CALM_atb_000deg_1yr_simple.sim \
  --output projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/scripts
```

**Generates:**
- `dm_vessel.yml` with auto-detected object names and standard outputs

**Options:**
- `--model-dir` - Directory containing .sim files
- `--inspect-sim` - Specific .sim file to inspect for objects
- `--pattern` - File pattern to match (default: auto-detect from .sim names)
- `--outputs` - Outputs to include (vessel_6dof,buoy_6dof,line_tension,time_series)
- `--output` - Output directory

### 5. Validate

```bash
orcaflex-agent validate \
  --files projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/base_files/_04_vessel_type_crowley650_atb.yml \
  --checks rao,matrix,amd,objects
```

**Checks:**
- `rao` - RAO amplitudes (all positive), phase angles (0-360°)
- `matrix` - Tensor structures (3x3), AMD matrices (6x6)
- `amd` - AMD method consistency
- `objects` - Object name references
- `yaml` - YAML syntax and structure
- `all` - Run all checks

**Options:**
- `--files` - Files to validate (glob patterns supported)
- `--checks` - Checks to run
- `--fix` - Auto-fix common issues (with backup)
- `--report` - Generate validation report

### 6. Workflow

```bash
orcaflex-agent workflow calm-buoy-operability \
  --config projects/modules/calm/baltic_039m/workflow_config.yml \
  --execute
```

**Workflow Steps:**
1. Generate base files
2. Generate env files
3. Generate analysis models
4. Validate all files
5. Run simulations (optional)
6. Post-process results (optional)
7. Generate summary report

**Options:**
- `--config` - Workflow configuration file
- `--execute` - Actually run (default: dry-run)
- `--steps` - Specific steps to run (default: all)
- `--skip-validation` - Skip validation step (not recommended)
- `--parallel` - Max parallel simulations

## Data Models

### Project Specification (Pydantic Model)

```python
from pydantic import BaseModel, Field
from typing import List, Optional, Dict

class VesselConfig(BaseModel):
    name: str
    vessel_type: str
    length: float
    breadth: float
    depth: float
    draft: float
    dwt: float
    rao_file: Optional[str] = None
    amd_file: Optional[str] = None

class EnvironmentalCondition(BaseModel):
    return_period: str  # "1yr", "10yr", "100yr"
    heading: int  # 0-345 in 15° increments
    wave_height: float
    wave_period: float
    wind_speed: float  # knots
    current_speed: float  # m/s

class MooringConfig(BaseModel):
    num_lines: int = 6
    line_type: str = "2p5inChain"
    clump_weight: float = 5.0  # tonnes
    top_chain_length: float = 40.8  # m
    bottom_chain_length: float = 104.2  # m

class CALMBuoyProject(BaseModel):
    project_name: str
    project_code: str
    vessel: VesselConfig
    mooring: MooringConfig
    environmental_conditions: List[EnvironmentalCondition]
    water_depth: float
    output_directory: str
```

### Validation Result Model

```python
class ValidationIssue(BaseModel):
    severity: str  # "error", "warning", "info"
    check: str  # "rao_amplitude", "matrix_structure", etc.
    file: str
    line: Optional[int] = None
    message: str
    suggestion: Optional[str] = None

class ValidationReport(BaseModel):
    files_checked: int
    issues: List[ValidationIssue]
    errors: int
    warnings: int
    passed: bool
```

## Template Examples

### Vessel Type Template (Jinja2)

```yaml
# _04_vessel_type_{{ vessel.name }}.yml
# Generated by OrcaFlex Agent
# Project: {{ project_name }}
# Date: {{ generation_date }}

New: VesselType1
VesselType1:
  Name: VesselType1
  Length: {{ vessel.length }}
  Draught: Draught1
  Draughts:
    - Name: Draught1
      Draught: {{ vessel.draft }}
      DisplacementMass: {{ vessel.displacement }}

  # RAOs - All amplitudes positive, directionality via phase
  RAOOrigin: [{{ vessel.rao_origin_x }}, {{ vessel.rao_origin_y }}, {{ vessel.rao_origin_z }}]
  RAOsReferToCompletion: Whole Line
  RAOResponseType: Displacement RAOs
  RAOPeriodOrFrequency: Period
  {% for rao in vessel.raos %}
  - RAODirection: {{ rao.direction }}
    RAOPeriodOrFrequency, RAOSurgeAmp, RAOSurgePhase, RAOSwayAmp, ...:
    {% for row in rao.data %}
      - [{{ row.period }}, {{ row.surge_amp|abs }}, {{ row.surge_phase }}, ...]
    {% endfor %}
  {% endfor %}

  # Moment of Inertia - Full 3x3 matrix
  MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ:
    - [{{ vessel.inertia.xx }}, 0, 0]
    - [0, {{ vessel.inertia.yy }}, 0]
    - [0, 0, {{ vessel.inertia.zz }}]

  # Added Mass and Damping - Frequency dependent
  AMDMethod: Frequency dependent
  FrequencyDependentAddedMassAndDamping:
  {% for amd in vessel.amd_data %}
    - AMDPeriodOrFrequency: {{ amd.period }}
      AddedMassMatrixX, AddedMassMatrixY, AddedMassMatrixZ, AddedMassMatrixRx, AddedMassMatrixRy, AddedMassMatrixRz:
      {% for row in amd.added_mass %}
        - {{ row }}
      {% endfor %}
      DampingX, DampingY, DampingZ, DampingRx, DampingRy, DampingRz:
      {% for row in amd.damping %}
        - {{ row }}
      {% endfor %}
  {% endfor %}

  # Wind and Current Coefficients - OCIMF standard
  {% include 'partials/ocimf_coefficients.yml.j2' %}
```

## Implementation Phases

### Phase 1: Core Generation (Week 1-2)
- [ ] CLI framework setup (Click)
- [ ] Template engine integration (Jinja2)
- [ ] Base file generator
- [ ] Environmental file generator
- [ ] Basic validation (YAML syntax)
- [ ] Unit tests for generators

**Deliverable**: Generate base files and env files from command line

### Phase 2: Validation (Week 3)
- [ ] RAO validator (amplitudes, phases)
- [ ] Matrix validator (tensors, AMD)
- [ ] Object name validator
- [ ] Validation report generator
- [ ] Auto-fix capability (with backup)
- [ ] Integration tests

**Deliverable**: Comprehensive validation suite

### Phase 3: Analysis & Post-Processing (Week 4)
- [ ] Analysis model generator
- [ ] .sim file inspector (object detection)
- [ ] Post-processing config generator
- [ ] Pattern library for common scenarios
- [ ] End-to-end tests

**Deliverable**: Complete file generation pipeline

### Phase 4: Workflow Orchestration (Week 5-6)
- [ ] Workflow engine
- [ ] Progress reporting
- [ ] Error handling and rollback
- [ ] Parallel execution support
- [ ] Summary report generation
- [ ] Full integration tests

**Deliverable**: Complete automated workflow

### Phase 5: Documentation & Polish (Week 7)
- [ ] User documentation (Sphinx)
- [ ] Tutorial videos
- [ ] Example projects
- [ ] Performance optimization
- [ ] Release preparation

**Deliverable**: Production-ready agent v1.0

## Testing Strategy

### Unit Tests
- Template rendering with various inputs
- Validation logic for each check type
- File generation functions
- Utility functions

### Integration Tests
- End-to-end file generation
- Validation of generated files
- OrcaFlex loading of generated files
- Post-processing execution

### Acceptance Tests
- Complete CALM buoy project generation
- Generate files matching reference projects
- Zero OrcaFlex errors from generated files
- Performance benchmarks (< 30s for complete project)

## Success Metrics

### Technical Metrics
- **Generation Accuracy**: 100% valid OrcaFlex files
- **Error Prevention**: 0 OrcaFlex errors from generated configs
- **Performance**: < 30 seconds for complete project generation
- **Test Coverage**: > 90% code coverage

### Business Metrics
- **Time Savings**: 80% reduction in project setup time (days → hours)
- **Error Reduction**: 95% fewer configuration errors
- **Reusability**: Templates used in 100% of new projects
- **Adoption**: 100% of marine engineering team using agent

## Risks and Mitigations

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Template complexity | High | Medium | Start simple, iterate based on real usage |
| OrcaFlex API changes | Medium | Low | Pin to specific OrcaFlex version, version matrix |
| Invalid generated files | High | Medium | Comprehensive validation before generation |
| User adoption resistance | Medium | Medium | Excellent documentation, training sessions |
| Pattern divergence | Medium | High | Regular pattern review, community contributions |

## Future Enhancements (Post-v1.0)

### Phase 6: Advanced Features
- Machine learning from successful patterns
- Auto-optimization of mooring configurations
- Results comparison across projects
- Web-based interface

### Phase 7: Expansion
- Riser analysis support
- Installation analysis patterns
- Integration with other tools (AQWA, ANSYS)
- Cloud execution support

## References

- **Learnings Document**: `docs/modules/orcaflex/CALM_BUOY_LEARNINGS_AND_PATTERNS.md`
- **Reference Projects**:
  - `projects/modules/calm/baltic_039m/TEST_OPERABILITY/`
  - `projects/modules/calm/nse_100m/TEST_OPERABILITY/`
  - `D:\1522\ctr7\rev_a09\`
- **OrcaFlex Documentation**: OrcFlex Manual v11.4
- **Optimization Guide**: `docs/ORCAFLEX_AUTO_OPTIMIZATION_GUIDE.md`

---

**Specification Status**: Draft v1.0
**Created**: 2025-11-15
**Authors**: Claude Code + Engineering Team
**Next Review**: Before Phase 1 implementation
