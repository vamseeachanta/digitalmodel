# Technical Specification: OrcaFlex Mooring Tension Iteration System

## System Architecture

### Core Components

#### 1. Iteration Engine (`MooringTensionIterator`)
**Purpose**: Main orchestrator for the optimization process, building upon existing `mooring.py` patterns
**Key Methods**:
- `iterate_to_targets()`: Primary iteration loop (automates manual process)
- `check_convergence()`: Tolerance validation (configurable, default 1%)
- `generate_report()`: Results documentation (extends CSV output format)

#### 2. Tension Calculator (`TensionAnalyzer`)  
**Purpose**: Interface with OrcaFlex for tension extraction via DigitalModel framework
**Key Methods**:
- `extract_mooring_tensions()`: Get current line tensions from OrcaFlex result CSV files
- `run_static_analysis()`: Execute DigitalModel OrcaFlex analysis pipeline
- `validate_model_state()`: Ensure model readiness for analysis
- `process_sim_results()`: Post-process .sim files to extract real tension values

**CRITICAL IMPLEMENTATION REQUIREMENTS**:
- **MUST execute actual OrcaFlex analysis**: Use `python -m digitalmodel.orcaflex config.yml --input model.yml`
- **MUST process real .sim files**: Extract tensions from `results/*Line_var_data.csv`
- **MUST update includefiles**: Modify `includefile_*_mooring_line_length.yml` before each run
- **MUST backup results**: Preserve previous analysis results between iterations

#### 3. Line Modifier (`LinePropertyManager`)
**Purpose**: Automated adjustment of mooring line properties
**Key Methods**:
- `adjust_line_lengths()`: Apply calculated length modifications
- `calculate_stiffness_matrix()`: Compute line interaction effects
- `backup_original_properties()`: Save original line configurations

#### 4. Configuration Manager (`IterationConfig`)
**Purpose**: Manage target specifications and algorithm parameters
**Key Attributes**:
- `target_tensions`: Dictionary mapping line names to target values
- `convergence_tolerance`: Acceptable deviation from targets (default: 1%)
- `max_iterations`: Safety limit for iteration count (default: 10)

### Real OrcaFlex Workflow Implementation

#### CRITICAL: Actual OrcaFlex Execution Required
**The iteration system MUST execute real OrcaFlex analysis, not simulated physics.**

#### Workflow Steps (Per Iteration)
1. **Update Includefile**
   - Modify `includefile_*_mooring_line_length.yml` with new `ArcLength[1]` values
   - Backup previous includefile with timestamp
   - Ensure proper YAML formatting

2. **Execute OrcaFlex Analysis**
   - Run: `python -m digitalmodel.orcaflex dm_ofx_anal_mooring_125km3_pb.yml --input model.yml`
   - Backup existing `results/` folder before analysis
   - Monitor for analysis completion (typical 25-40 seconds)
   - Handle timeout scenarios (600 second limit)

3. **Post-Process Results**
   - Locate: `results/*Line_var_data.csv` (most recent file)
   - Extract `Effective tension` column for each mooring line
   - Match ObjectName to target line identifiers (Line01, Line02, etc.)
   - Handle missing data with appropriate fallbacks

4. **Iteration Control**
   - Calculate tension errors vs targets
   - Apply Newton-Raphson length adjustments with damping (0.8)
   - Check convergence criteria (1% tolerance)
   - Continue or terminate based on convergence/max iterations

#### File Structure Requirements
```
base_files/fsts_lngc_pretension/
├── fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.yml  # Main model
├── dm_ofx_anal_mooring_125km3_pb.yml                     # Analysis config  
├── includefile_*_mooring_line_length.yml                 # Line length data
├── 125km3_l000_pb_target_mooring_pretension.csv          # Target tensions
└── results/                                              # OrcaFlex outputs
    └── *Line_var_data.csv                               # Tension results
```

#### Common Implementation Errors to Avoid
- **❌ Simulating OrcaFlex**: Never use mathematical models instead of real analysis
- **❌ Ignoring .sim files**: Must process actual OrcaFlex result files
- **❌ Missing includefile updates**: Line lengths must be updated before each run
- **❌ Incorrect tension extraction**: Use 'Effective tension' column, not 'End force'
- **❌ File conflicts**: Backup existing results to prevent overwrites

### Mathematical Framework

#### Optimization Problem Formulation
The system solves the non-linear system:
$$\mathbf{f}(\mathbf{L}) = \mathbf{T}(\mathbf{L}) - \mathbf{T}_{target} = \mathbf{0}$$

Where:
- $\mathbf{L} = [L_1, L_2, \ldots, L_n]^T$ represents mooring line lengths
- $\mathbf{T}(\mathbf{L})$ is the tension response vector from OrcaFlex analysis
- $\mathbf{T}_{target}$ contains user-specified target tensions

#### Newton-Raphson Solution Method
The iterative update equation:
$$\mathbf{L}^{(k+1)} = \mathbf{L}^{(k)} - \mathbf{J}^{-1}(\mathbf{L}^{(k)}) \mathbf{f}(\mathbf{L}^{(k)})$$

#### Jacobian Matrix Calculation
The sensitivity matrix for line interactions:
$$J_{ij} = \frac{\partial T_i}{\partial L_j}$$

Computed using finite differences with adaptive step sizing:
$$J_{ij} \approx \frac{T_i(\mathbf{L} + h_j\mathbf{e}_j) - T_i(\mathbf{L})}{h_j}$$

Where $h_j$ is the perturbation step for line $j$, calculated as:
$$h_j = \max(0.01 \cdot L_j, 0.1 \text{ m})$$

#### Line Stiffness Approximation
For initial step estimates, the effective stiffness:
$$k_{eff,i} = \frac{EA_i}{L_{0,i}} \cos^2(\theta_i) \cdot f_{catenary}(\lambda_i)$$

Where:
- $EA_i$ is the axial stiffness of line $i$
- $L_{0,i}$ is the initial unstretched length
- $\theta_i$ is the line angle from horizontal at anchor
- $f_{catenary}(\lambda_i)$ accounts for catenary geometry effects
- $\lambda_i = \frac{w L_{0,i}^2}{T_{h,i}}$ is the dimensionless catenary parameter

#### Convergence Criteria
The system achieves convergence when:
$$\max_{i} \left| \frac{T_{current,i} - T_{target,i}}{T_{target,i}} \right| \leq \epsilon_{tol}$$

With default tolerance $\epsilon_{tol} = 0.01$ (1%).

## Current Manual Process Workflow

### Overview
The existing workflow combines manual OrcaFlex operations with digitalmodel calculations in an iterative process. This specification documents both the current manual approach and the path to automation.

### Manual Iteration Steps

#### Step 1: Input Preparation
- Load target tensions from CSV: `*_target_mooring_pretension.csv`
- Target tensions in kN, line lengths optional
- EA values can be provided or extracted from OrcaFlex model
- Fender properties from `_target_fender_force.csv` (optional)

#### Step 2: Baseline Analysis
- Run OrcaFlex static analysis: `fsts*vessel_statics_6dof.yml`
- Vessel in 6DOF mode for equilibrium position
- Generates .sim output files

#### Step 3: Result Extraction
- Execute `dm_ofx_post_fsts_lngc.yml` for post-processing
- Extracts tensions, line lengths, fender forces to CSV
- Creates baseline for comparison

#### Step 4: Length Calculation
- Run `dm_ofx_anal_mooring_*.yml` with digitalmodel
- Calculates: ΔL = L/EA × (T_current - T_target)
- Uses major governing stiffness (not combined equivalent)
- Generates includefile YAMLs (overwritten each iteration)

#### Step 5: Model Update & Iteration
- Includefiles automatically update OrcaFlex model
- Re-run static analysis (vessel finds new equilibrium)
- Manual convergence check via CSV review
- Repeat until convergence (typically 3-5 iterations)

### Current Implementation Details

1. **EA-Based Length Calculation**:
   ```python
   delta_length = (length/EA) * (current_tension - target_tension)
   new_arc_length = arc_length + sum(delta_length)
   ```

2. **File Management**:
   - Includefiles overwritten to minimize updates
   - Occasional manual backups recommended
   - Results in `fsts_lngc_pretension/results/` folder

3. **Convergence Handling**:
   - Manual review of CSV outputs
   - Tolerance typically ±1% of target
   - Manual rollback if tensions worsen
   - Option to restart from different initial conditions

## Implementation Phases

### Phase 1: Semi-Automated Workflow (MVP)
**Goal**: Automate calculation steps while maintaining manual control points

#### Components to Implement:
1. **CSV Parser Module**
   - Read target tension/length files
   - Extract EA values from CSV or OrcaFlex models
   - Validate input data completeness

2. **OrcaFlex Runner Module**
   - Execute static analysis via Python API
   - Handle .yml to .sim conversion
   - Manage file paths and outputs

3. **Result Extractor Module**
   - Post-process .sim files automatically
   - Generate CSV outputs matching current format
   - Calculate tension differences

4. **Length Calculator Module**
   - Implement ΔL = L/EA × (T_current - T_target)
   - Generate includefile YAMLs
   - Handle file overwriting with optional backup

5. **Convergence Reporter**
   - Compare tensions against targets
   - Generate iteration summary reports
   - Flag non-converging cases

#### Manual Steps Retained:
- Decision to proceed with next iteration
- Rollback decisions on failure
- Initial model selection
- Final validation of results

### Phase 2: Fully Automated Iteration
**Goal**: Complete automation with intelligent decision making

#### Enhancements:
- Automatic convergence detection
- Smart rollback on divergence
- Jacobian-based optimization (replacing simple EA)
- Parallel processing for multiple models
- Vessel position optimization

### Phase 3: Advanced Optimization
**Goal**: Sophisticated algorithms and robustness

#### Advanced Features:
- scipy.optimize integration
- Multi-dimensional Newton-Raphson
- Adaptive step sizing
- Convergence acceleration techniques
- Automated failure recovery strategies

### Algorithm Implementation

#### Primary Iteration Loop
```python
def iterate_to_targets(self, target_tensions: Dict[str, float]) -&gt; IterationResult:
    """
    Main iteration algorithm for achieving target mooring tensions.
    
    Args:
        target_tensions: Dict mapping line names to target tension values [kN]
        
    Returns:
        IterationResult containing convergence status and final tensions
    """
    # Initialize
    current_lengths = self._get_current_line_lengths()
    iteration_count = 0
    
    while iteration_count &lt; self.config.max_iterations:
        # Calculate current tensions via OrcaFlex static analysis
        current_tensions = self.tension_analyzer.extract_mooring_tensions()
        
        # Check convergence
        if self._check_convergence(current_tensions, target_tensions):
            return IterationResult(
                converged=True,
                iterations=iteration_count,
                final_tensions=current_tensions,
                final_lengths=current_lengths
            )
        
        # Calculate Jacobian matrix
        jacobian = self._compute_jacobian(current_lengths)
        
        # Compute tension residuals
        residuals = self._calculate_residuals(current_tensions, target_tensions)
        
        # Newton-Raphson update
        length_updates = self._solve_linear_system(jacobian, residuals)
        
        # Apply updates with safety limits
        current_lengths = self._apply_length_updates(current_lengths, length_updates)
        
        # Update OrcaFlex model
        self.line_modifier.adjust_line_lengths(current_lengths)
        
        iteration_count += 1
    
    # Failed to converge
    return IterationResult(converged=False, iterations=iteration_count)
```

#### Jacobian Matrix Computation
```python
def _compute_jacobian(self, current_lengths: np.ndarray) -&gt; np.ndarray:
    """
    Compute sensitivity matrix using finite differences.
    
    Args:
        current_lengths: Current line length values [m]
        
    Returns:
        Jacobian matrix J[i,j] = dT_i/dL_j
    """
    n_lines = len(current_lengths)
    jacobian = np.zeros((n_lines, n_lines))
    
    # Get baseline tensions
    baseline_tensions = self.tension_analyzer.extract_mooring_tensions()
    
    for j in range(n_lines):
        # Calculate perturbation step
        h = max(0.01 * current_lengths[j], 0.1)  # Minimum 0.1m step
        
        # Perturb line j
        perturbed_lengths = current_lengths.copy()
        perturbed_lengths[j] += h
        
        # Update model and analyze
        self.line_modifier.adjust_line_lengths(perturbed_lengths)
        perturbed_tensions = self.tension_analyzer.extract_mooring_tensions()
        
        # Compute derivatives
        for i in range(n_lines):
            jacobian[i, j] = (perturbed_tensions[i] - baseline_tensions[i]) / h
        
        # Restore original lengths
        self.line_modifier.adjust_line_lengths(current_lengths)
    
    return jacobian
```

### OrcaFlex Integration

#### Model Interface Requirements
The system requires OrcaFlex models with:
- **Mooring Lines**: Named consistently (e.g., "Line1", "Line2", "Line3", "Line4")
- **Vessel Objects**: Fixed during static analysis using `vessel.PrimaryMotion = 'Fixed'`
- **Environment**: Defined current, wind, and wave conditions
- **Line Types**: Properly configured with axial stiffness (EA) properties

#### Static Analysis Configuration
```python
def configure_static_analysis(self, model: OrcFxAPI.Model) -&gt; None:
    """Configure model for static analysis during iteration."""
    # Fix vessel position
    for vessel_name in self.config.vessel_names:
        vessel = model[vessel_name]
        vessel.PrimaryMotion = 'Fixed'
        vessel.IncludeAppliedLoads = 'Yes'
    
    # Set analysis parameters for speed
    model.general.StaticsSolver = 'Whole system'
    model.general.StaticsMaxDamping = 10.0
    model.general.StaticsMinDamping = 0.1
    model.general.StaticsMaxIterations = 1000
    model.general.StaticsConvergenceTolerance = 1e-6
```

#### Tension Extraction
```python  
def extract_mooring_tensions(self) -&gt; Dict[str, float]:
    """Extract effective tension from all mooring lines."""
    tensions = {}
    
    for line_name in self.config.mooring_line_names:
        line = self.model[line_name]
        
        # Get tension at line end (typically anchor end)
        end_tension = line.StaticResult('End A Fx', OrcFxAPI.oeEndA)
        effective_tension = line.StaticResult('Effective Tension', 
                                            arclength=0.0,  # At anchor
                                            period=OrcFxAPI.pnStaticState)
        
        tensions[line_name] = effective_tension
    
    return tensions
```

### Error Handling and Validation

#### Convergence Failure Handling
```python
def handle_convergence_failure(self, iteration_result: IterationResult) -&gt; None:
    """Handle cases where algorithm fails to converge."""
    if not iteration_result.converged:
        # Log failure details
        self.logger.warning(f"Failed to converge after {iteration_result.iterations} iterations")
        
        # Analyze potential causes
        self._diagnose_failure(iteration_result)
        
        # Attempt fallback strategies
        if self.config.enable_fallback:
            self._try_fallback_methods()
        
        # Restore original model state
        self.line_modifier.restore_original_properties()
```

#### Model Validation
```python
def validate_model_setup(self, model: OrcFxAPI.Model) -&gt; List[str]:
    """Validate model configuration for iteration compatibility."""
    warnings = []
    
    # Check mooring line configuration
    for line_name in self.config.mooring_line_names:
        if line_name not in model.objects:
            warnings.append(f"Mooring line '{line_name}' not found in model")
            continue
            
        line = model[line_name]
        
        # Validate line type has stiffness data
        line_type = model[line.LineType[0]]  # First segment line type
        if not hasattr(line_type, 'EA') or line_type.EA &lt;= 0:
            warnings.append(f"Line type for {line_name} missing axial stiffness (EA)")
        
        # Check anchor connection
        if line.EndAConnection != 'Anchored':
            warnings.append(f"Line {line_name} End A should be anchored for tension control")
    
    return warnings
```

### Configuration Specification

#### YAML Configuration Format
```yaml
# mooring_iteration_config.yml
iteration_parameters:
  convergence_tolerance: 0.01  # 1% tolerance
  max_iterations: 10
  enable_adaptive_stepping: true
  safety_factor_limits: [0.5, 2.0]  # Length change limits

target_tensions:
  Line1: 2500.0  # kN
  Line2: 2500.0  # kN  
  Line3: 2500.0  # kN
  Line4: 2500.0  # kN

model_configuration:
  vessel_names: ["Vessel1"]
  mooring_line_names: ["Line1", "Line2", "Line3", "Line4"]
  fix_vessel_during_iteration: true

algorithm_settings:
  jacobian_perturbation_ratio: 0.01  # 1% of current length
  minimum_perturbation: 0.1  # meters
  line_interaction_matrix: "auto"  # or "diagonal" for independent lines

reporting:
  generate_convergence_plot: true
  save_intermediate_results: true
  output_format: ["csv", "excel", "pdf"]
```

### Performance Optimization

#### Computational Efficiency
- **Parallel Jacobian Calculation**: Compute finite differences concurrently when multiple OrcaFlex licenses available
- **Adaptive Step Sizing**: Larger steps early in iteration, refined steps near convergence
- **Convergence Acceleration**: Aitken's Δ² method for faster convergence in well-behaved cases

#### Memory Management
- **Model State Caching**: Store original line properties for restoration
- **Result Streaming**: Process large result sets without excessive memory allocation
- **Garbage Collection**: Explicit cleanup of OrcaFlex objects between iterations

### Quality Assurance

#### Validation Test Cases
1. **Single Line Validation**: Compare against manual calculation for simple catenary
2. **Multi-Line Independence**: Verify non-interacting lines converge independently  
3. **Coupled System Validation**: Test complex interaction cases against known solutions
4. **Boundary Conditions**: Extreme target values and physically unrealistic requests
5. **Model Variations**: Different line types, water depths, and vessel configurations

#### Regression Testing
- **Automated Test Suite**: Comprehensive coverage of algorithm components
- **Performance Benchmarks**: Timing and memory usage tracking across model types
- **Result Accuracy**: Statistical validation of convergence accuracy over test cases

This technical specification provides the foundation for implementing a robust, efficient, and validated mooring tension iteration system integrated with OrcaFlex.