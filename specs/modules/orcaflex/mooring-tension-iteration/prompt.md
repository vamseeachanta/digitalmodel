# Implementation Prompt: OrcaFlex Mooring Tension Iteration System

## Complete Prompt History

### Latest Update (2025-08-24)
**User Request**: "Utilize latest prompt provided in specs\modules\orcaflex\mooring-tension-iteration\prompt.md and write a fresh spec"

**Actions Taken**:
- Created fresh spec.md (v2.0) based on latest prompt
- Rewrote tasks.md with streamlined implementation plan (36 hours total)
- Updated README.md with new executive summary and status
- Focused on current production methodology with DigitalModel integration
- Emphasized validation requirements (manual vs automated)
- All files now in root folder (revision-1 archived)

### Previous Update (2025-08-23)
**User Request**: "Add to this spec the latest mooring pretension analysis methodology"

**Methodology Details Provided**:
1. **Step 1 - Establish Initial Tension**:
   - Input: `fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat`
   - Command: `/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel.modules.orcaflex.universal pattern="fsts*125km3*pb_*.yml" input_directory="." output_directory="." validate=false`

2. **Step 2 - Post-Process Results**:
   - Input: `dm_ofx_post_fsts_lngc.yml`
   - Command: `/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30`

3. **Step 3 - Iterate Tension**:
   - Input: `fsts_l015_125km3_pb_target_mooring_pretension.csv`
   - Command: `/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml`

4. **Convergence Criteria**:
   - Repeat until change in tension < 5%
   - Monitor: `results/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv`

**Updates Applied**:
- Added "Latest Mooring Pretension Analysis Methodology" section to spec.md
- Created Phase 0 tasks in tasks.md for current methodology implementation
- Documented specific commands, file paths, and convergence criteria
- Integrated with DigitalModel's universal OrcaFlex runner and post-processing pipeline

### Original Request Context
**User Request**: "Execute the /create-spec command to create a specification for 'iterate mooring tensions for the model to target values' in the specs/modules/orcaflex/ directory structure."

**Additional Context Provided**:
- Mooring lines are named Line1, Line2, etc.
- Target tensions need to be defined by user
- Reference examples:
  - Root finding approach: `../docs/modules/orcaflex/scripts/orcfxapi_goby/58 - ScipyRootFinding.py`
  - Semi-automated iteration: `../tests/modules/orcaflex/orcaflex_analysis/moorings/ofx_mooring_analysis_test.py` with `src/digitalmodel/modules/orcaflex/mooring.py`
- Current manual process: load model, fix vessels, run model, get tensions, iterate lengths by stiffness
- Objective: achieve target tensions in OrcaFlex using Python in 1 iteration
- Relevant agent: agents\\orcaflex

### Analysis and Research Phase
**Reference Script Analysis**: 

1. **ScipyRootFinding.py** demonstrates:
   - Single-parameter optimization using `scipy.optimize.fsolve`
   - Newton-Raphson approach for iterating float length to achieve target Z-position
   - OrcaFlex integration pattern with static analysis and property modification
   - Convergence approach using residual function minimization

2. **Semi-automated Mooring Analysis** (`ofx_mooring_analysis_test.py` and `mooring.py`) provides:
   - Existing iteration framework with configurable tolerance (10%) and max iterations
   - YAML-based configuration for target pretensions
   - Line length adjustment using EA stiffness calculations: `delta_length = (length/EA) * (current_tension - target_tension)`
   - Include file generation for OrcaFlex model updates
   - Manual execution requirement between iterations (current limitation)
   - CSV output for tension analysis results

**Key Technical Insights**:
- Extension from single-parameter (targetZ) to multi-parameter (target tensions)
- Mathematical framework: solve $\mathbf{T}(\mathbf{L}) = \mathbf{T}_{target}$ where $\mathbf{L}$ is line lengths vector
- Algorithm: Multi-dimensional Newton-Raphson with Jacobian matrix $J_{ij} = \frac{\partial T_i}{\partial L_j}$
- Integration: Direct OrcaFlex API usage with model property modification

### Specification Development Approach
**Enhanced Spec Creation Pattern Applied**:
1. Executive summary for stakeholders and business case
2. Technical specification with mathematical formulation in LaTeX
3. Mermaid architecture diagrams for system design
4. Detailed user stories with acceptance criteria and effort estimates
5. Comprehensive task breakdown with dependencies and timelines
6. Complete prompt history for implementation guidance

## Curated Reuse Prompt

### For Implementation Teams

When implementing the OrcaFlex Mooring Tension Iteration System, use this prompt to ensure comprehensive coverage:

```
Implement an automated mooring tension iteration system for OrcaFlex that:

CORE FUNCTIONALITY:
- Accepts target tensions for multiple mooring lines (Line1, Line2, etc.) via YAML configuration
- Uses multi-dimensional Newton-Raphson optimization to solve T(L) = T_target
- Calculates Jacobian matrix J_ij = ∂T_i/∂L_j using finite differences
- Achieves convergence within ±1% tolerance in single iteration cycle
- Integrates directly with OrcaFlex API for model manipulation and analysis

MATHEMATICAL FOUNDATION:
- Solve non-linear system: f(L) = T(L) - T_target = 0
- Newton-Raphson updates: L^(k+1) = L^(k) - J^(-1)(L^(k)) * f(L^(k))
- Line stiffness approximation: k_eff = EA/L * cos²(θ) * f_catenary(λ)
- Convergence criteria: max|T_current - T_target|/T_target ≤ ε

SYSTEM ARCHITECTURE:
- IterationEngine: Main optimization controller
- TensionAnalyzer: OrcaFlex interface for static analysis and tension extraction
- LinePropertyManager: Automated line length adjustment with backup/restore
- ConfigurationManager: YAML-based target specification and validation

INTEGRATION REQUIREMENTS:
- OrcaFlex model compatibility with named mooring lines
- Vessel fixing during static analysis iterations
- Property backup and restoration for safety
- Comprehensive error handling and validation

REFERENCE IMPLEMENTATION:
- Build upon pattern from 58 - ScipyRootFinding.py for scipy optimization approach
- Leverage existing mooring.py semi-automated iteration logic and EA-based calculations
- Extend from single-parameter to multi-dimensional optimization
- Automate the manual execution step between iterations
- Maintain OrcaFlex integration patterns and best practices
- Follow DigitalModel repository standards and module organization

VALIDATION REQUIREMENTS:
- Compare against manual catenary calculations for simple cases
- Validate multi-line interaction effects
- Test convergence reliability across typical mooring configurations
- Ensure results match engineering expectations within specified tolerance

Create production-ready Python module following DigitalModel patterns with comprehensive testing, documentation, and error handling.
```

### For Technical Reviews

When reviewing implementations, focus on these critical aspects:

1. **Mathematical Accuracy**: Verify Jacobian calculation and Newton-Raphson implementation
2. **OrcaFlex Integration**: Ensure proper API usage, model state management, and error handling
3. **Convergence Reliability**: Test across various mooring configurations and failure modes
4. **Performance**: Validate timing requirements (&lt;2 minutes for typical models)
5. **Safety**: Confirm backup/restore mechanisms and constraint validation

### For Documentation Teams

Document the system emphasizing:

1. **Business Value**: 80% time reduction in mooring design iterations
2. **Technical Innovation**: Multi-dimensional optimization replacing manual processes
3. **Integration**: Seamless workflow with existing OrcaFlex models
4. **Validation**: Comprehensive testing against industry benchmarks

## Implementation Guidance

### Critical Success Factors
1. **Start Simple**: Begin with single-line validation before multi-line complexity
2. **Mathematical Rigor**: Ensure Jacobian calculation accuracy and numerical stability
3. **OrcaFlex Expertise**: Leverage domain knowledge for proper API usage patterns
4. **Validation Focus**: Extensive testing against known solutions and manual calculations
5. **User Experience**: Simple configuration, clear error messages, professional reporting

### Common Implementation Pitfalls
1. **Numerical Instability**: Use appropriate perturbation steps and matrix conditioning
2. **OrcaFlex API Errors**: Implement robust error handling and model validation
3. **Convergence Failures**: Design fallback strategies and diagnostic information
4. **Performance Issues**: Optimize for typical model sizes and iteration counts
5. **Configuration Complexity**: Balance flexibility with ease of use

### Repository Integration
- Follow `specs/modules/orcaflex/` organization pattern
- Use existing OrcaFlex integration patterns from `src/modules/orcaflex/`
- Leverage test frameworks for mock API development
- Maintain consistency with DigitalModel coding standards

## Expected Implementation Outcomes

### Minimum Viable Product (68 hours)
- Single-line and basic multi-line iteration capability
- YAML configuration and OrcaFlex integration
- Core Newton-Raphson solver with convergence validation
- Basic error handling and reporting

### Production System (156 hours)
- Comprehensive multi-line optimization with interaction handling
- Advanced convergence strategies and batch processing
- Professional reporting with visualizations
- Complete testing, validation, and documentation

### Success Metrics
- **Technical**: 95%+ convergence success rate, ±1% tension accuracy
- **Performance**: &lt;2 minutes for typical models, &lt;1 GB memory usage
- **Business**: 80% reduction in mooring design iteration time

This prompt provides complete context for implementing the OrcaFlex Mooring Tension Iteration System while maintaining traceability to original requirements and technical constraints.