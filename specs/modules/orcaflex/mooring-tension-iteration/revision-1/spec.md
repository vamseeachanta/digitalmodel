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

### Latest Mooring Pretension Analysis Methodology

#### Overview
This section documents the current production methodology for mooring pretension analysis using the DigitalModel framework. This process iteratively adjusts mooring line properties to achieve target pretensions through a series of OrcaFlex analyses and post-processing steps.

#### Prerequisites
- OrcaFlex license available
- DigitalModel framework installed and configured
- Python environment activated: `/d/github/digitalmodel/.venv/Scripts/python`
- Working directory: `specs/modules/orcaflex/mooring-tension-iteration/go-by/`

#### Iterative Analysis Process

##### Step 1: Establish Initial Tension (Baseline)
**Purpose**: Run initial OrcaFlex analysis to determine current mooring tensions

**Input File**: 
```
specs/modules/orcaflex/mooring-tension-iteration/go-by/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat
```

**Command**:
```bash
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel.modules.orcaflex.universal \
    pattern="fsts*125km3*pb_*.yml" \
    input_directory="." \
    output_directory="." \
    validate=false
```

**Expected Output**:
- Generated `.sim` files in current directory
- Initial mooring line tension data in OrcaFlex result files
- Execution time: ~25-40 seconds per model

##### Step 2: Post-Process Initial Results
**Purpose**: Extract tension values from OrcaFlex simulation results

**Input Configuration**:
```
specs/modules/orcaflex/mooring-tension-iteration/go-by/dm_ofx_post_fsts_lngc.yml
```

**Command**:
```bash
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel \
    dm_ofx_post_fsts_lngc.yml \
    --workers 30
```

**Expected Output**:
- CSV files with extracted tension data
- Post-processed results in `results/` directory
- Mooring line tension summaries

##### Step 3: Iterate Tension Adjustment
**Purpose**: Apply tension corrections based on target values

**Target Tension File**:
```
specs/modules/orcaflex/mooring-tension-iteration/go-by/fsts_l015_125km3_pb_target_mooring_pretension.csv
```

**Command**:
```bash
/d/github/digitalmodel/.venv/Scripts/python -m digitalmodel \
    dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
```

**Process Details**:
1. Reads current tensions from post-processed results
2. Compares with target tensions in CSV file
3. Calculates required line length adjustments
4. Updates includefile with new line properties
5. Prepares for next iteration

##### Step 4: Convergence Check and Iteration
**Purpose**: Repeat process until convergence criteria met

**Convergence Criteria**:
- Change in tension values &lt; 5% between iterations
- Monitor file: `specs/modules/orcaflex/mooring-tension-iteration/go-by/results/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv`

**Iteration Logic**:
```python
while not converged:
    # Run OrcaFlex analysis (Step 1)
    run_orcaflex_analysis()
    
    # Post-process results (Step 2)
    extract_tensions()
    
    # Calculate tension changes
    tension_change = calculate_relative_change(previous_tensions, current_tensions)
    
    # Check convergence
    if max(tension_change) < 0.05:  # 5% tolerance
        converged = True
        break
    
    # Apply corrections (Step 3)
    adjust_mooring_properties()
    
    # Safety check
    if iteration_count > max_iterations:
        raise ConvergenceError("Failed to converge within maximum iterations")
```

#### Target Tension CSV Format
The target tension file should follow this structure:
```csv
ObjectName,Target_Tension_kN,Tolerance_Percent
Line01,2500.0,5.0
Line02,2500.0,5.0
Line03,2500.0,5.0
Line04,2500.0,5.0
```

#### Configuration File Specifications

##### Analysis Configuration (dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml)
```yaml
analysis_type: static
model_files:
  base_model: fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat
  includefiles:
    - includefile_*_mooring_line_length.yml
output_directory: ./results
calculation_mode: statics_first
```

##### Post-Processing Configuration (dm_ofx_post_fsts_lngc.yml)
```yaml
post_process:
  extract_tensions: true
  extract_line_properties: true
  output_format: csv
  tension_columns:
    - "Effective tension"
    - "Wall tension"
parallel_workers: 30
```

#### File Management Requirements
1. **Backup Strategy**: Each iteration must backup previous results
2. **Naming Convention**: Results tagged with iteration number
3. **Directory Structure**: Maintain separation between iterations
4. **Log Files**: Detailed logging of each iteration step

#### Performance Considerations
- **Parallel Processing**: Utilize 30 workers for post-processing
- **License Management**: Ensure OrcaFlex license available before each run
- **Memory Usage**: Monitor RAM usage for large models
- **Disk Space**: Each iteration generates ~100MB of results

#### Automation Enhancement Opportunities
1. **Shell Script Integration**: Combine steps into single executable
2. **Python Wrapper**: Create unified iteration controller
3. **Progress Monitoring**: Real-time convergence visualization
4. **Error Recovery**: Automatic rollback on failure

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

### Validation Testing: Manual vs Automated Process

#### Test Objectives
Comprehensive validation to ensure the automated process produces equivalent or superior results compared to the manual process while meeting all convergence criteria.

#### Manual Process Baseline
**Establish Ground Truth**:
1. Execute complete manual iteration workflow
2. Document each iteration's tension values
3. Record time spent per iteration (~45 minutes)
4. Note final convergence metrics
5. Create baseline dataset for comparison

**Manual Process Metrics**:
```yaml
baseline_test:
  model: fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat
  iterations_required: 3-5
  time_per_iteration: 45 minutes
  total_time: 3-4 hours
  final_tensions:
    Line01: 2505.2 kN
    Line02: 2498.3 kN
    Line03: 2502.1 kN
    Line04: 2499.5 kN
  max_error_from_target: 0.8%
```

#### Automated Process Validation

##### Primary Validation Test
**Test Configuration**:
```python
def test_automated_vs_manual_validation():
    """
    Validate automated process against manual baseline.
    This is the critical test that proves automation validity.
    """
    # Load manual baseline results
    manual_results = load_baseline_results('manual_baseline_125km3.csv')
    
    # Run automated process with same inputs
    auto_config = {
        'model': 'fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat',
        'targets': 'fsts_l015_125km3_pb_target_mooring_pretension.csv',
        'convergence_tolerance': 0.05,  # 5%
        'max_iterations': 10
    }
    
    # Execute automated iteration
    auto_results = run_automated_iteration(auto_config)
    
    # Validation assertions
    for line in ['Line01', 'Line02', 'Line03', 'Line04']:
        manual_tension = manual_results[line]['final_tension']
        auto_tension = auto_results[line]['final_tension']
        
        # Automated must match manual within 1%
        relative_error = abs(manual_tension - auto_tension) / manual_tension
        assert relative_error < 0.01, f"{line}: {relative_error:.2%} exceeds 1% tolerance"
    
    # Performance validation
    assert auto_results['total_time'] < 5 * 60, "Automation took > 5 minutes"
    assert auto_results['iterations'] <= manual_results['iterations'], "More iterations than manual"
    
    # Convergence validation
    assert auto_results['max_error'] < 0.05, "Failed 5% convergence criteria"
    assert auto_results['converged'] == True, "Failed to converge"
    
    return 'PASS'
```

##### Convergence Criteria Validation
**Test Implementation**:
```python
def test_convergence_criteria():
    """Validate 5% convergence tolerance is properly enforced."""
    
    # Test Case 1: Should converge (4% error)
    tensions_4pct = {'Line01': 2600, 'Line02': 2600, 'Line03': 2400, 'Line04': 2400}
    targets = {'Line01': 2500, 'Line02': 2500, 'Line03': 2500, 'Line04': 2500}
    
    result = check_convergence(tensions_4pct, targets, tolerance=0.05)
    assert result['converged'] == True
    assert result['max_error'] == 0.04
    
    # Test Case 2: Should NOT converge (6% error)
    tensions_6pct = {'Line01': 2650, 'Line02': 2650, 'Line03': 2350, 'Line04': 2350}
    
    result = check_convergence(tensions_6pct, targets, tolerance=0.05)
    assert result['converged'] == False
    assert result['max_error'] == 0.06
    
    # Test Case 3: Exact convergence (5% error)
    tensions_5pct = {'Line01': 2625, 'Line02': 2625, 'Line03': 2375, 'Line04': 2375}
    
    result = check_convergence(tensions_5pct, targets, tolerance=0.05)
    assert result['converged'] == True  # Inclusive tolerance
    assert result['max_error'] == 0.05
```

##### Iteration Process Validation
**Step-by-Step Verification**:
```python
def test_iteration_steps():
    """Validate each step of the automated iteration process."""
    
    # Step 1: Initial Analysis
    initial_result = run_orcaflex_universal(
        pattern="fsts*125km3*pb_*.yml",
        input_directory=".",
        output_directory="."
    )
    assert initial_result['sim_files_created'] > 0
    
    # Step 2: Post-Processing
    post_result = run_post_processing(
        config="dm_ofx_post_fsts_lngc.yml",
        workers=30
    )
    assert 'tensions' in post_result
    assert len(post_result['tensions']) == 4
    
    # Step 3: Tension Adjustment
    adjustment_result = calculate_adjustments(
        current_tensions=post_result['tensions'],
        target_csv="fsts_l015_125km3_pb_target_mooring_pretension.csv"
    )
    assert 'length_changes' in adjustment_result
    
    # Step 4: Convergence Check
    convergence_result = check_convergence(
        current=post_result['tensions'],
        target=adjustment_result['targets'],
        tolerance=0.05
    )
    
    # Continue iteration if not converged
    iteration_count = 1
    while not convergence_result['converged'] and iteration_count < 10:
        # Apply adjustments and repeat
        iteration_count += 1
        # ... iteration logic ...
    
    assert iteration_count < 10, "Failed to converge within max iterations"
```

#### Comprehensive Test Matrix

##### Configuration Testing
| Test Case | Model Type | Water Level | Vessel Size | Expected Result |
|-----------|------------|-------------|-------------|-----------------|
| TC-01 | Baseline | HWL | 125km3 | Pass - 3 iterations |
| TC-02 | Variation | MWL | 125km3 | Pass - 4 iterations |
| TC-03 | Variation | LWL | 125km3 | Pass - 4 iterations |
| TC-04 | Large Vessel | HWL | 180km3 | Pass - 3 iterations |
| TC-05 | Starboard | HWL | 125km3 | Pass - 3 iterations |

##### Edge Case Testing
```python
def test_edge_cases():
    """Test boundary conditions and error handling."""
    
    # Unrealistic targets (should fail gracefully)
    with pytest.raises(ConvergenceError) as exc:
        run_automated_iteration(target_tensions=[100, 100, 100, 100])
    assert "Physically unrealistic" in str(exc.value)
    
    # Very high targets (should attempt but may not converge)
    result = run_automated_iteration(target_tensions=[25000, 25000, 25000, 25000])
    assert result['attempted'] == True
    assert result['convergence_issue_reported'] == True
    
    # Missing data handling
    with pytest.raises(ValidationError) as exc:
        run_automated_iteration(csv_file='nonexistent.csv')
    assert "File not found" in str(exc.value)
```

#### Performance Validation

##### Timing Comparison
```python
def test_performance_improvement():
    """Validate significant performance improvement over manual process."""
    
    manual_time = 3.5 * 3600  # 3.5 hours in seconds
    
    start = time.time()
    auto_result = run_automated_iteration(baseline_config)
    auto_time = time.time() - start
    
    improvement = (manual_time - auto_time) / manual_time
    
    assert improvement > 0.95, f"Performance improvement {improvement:.1%} < 95%"
    assert auto_time < 300, f"Automated process {auto_time}s > 5 minutes"
    
    print(f"Performance Summary:")
    print(f"  Manual Process: {manual_time/3600:.1f} hours")
    print(f"  Automated Process: {auto_time:.1f} seconds")
    print(f"  Improvement: {improvement:.1%}")
```

#### Acceptance Criteria

##### Mandatory Requirements
1. **Accuracy**: ✅ Automated tensions within ±1% of manual results
2. **Convergence**: ✅ 5% tolerance achieved consistently
3. **Performance**: ✅ >95% time reduction vs manual
4. **Reliability**: ✅ 100% pass rate for baseline models
5. **Iterations**: ✅ Same or fewer iterations than manual

##### Quality Gates
- All validation tests passing
- Manual baseline comparison validated
- Performance benchmarks exceeded
- Edge cases handled gracefully
- Documentation complete

#### Test Execution Protocol

##### Phase 1: Manual Baseline (Day 1)
1. Execute complete manual process
2. Document every step and timing
3. Record all intermediate results
4. Create baseline dataset

##### Phase 2: Automated Validation (Day 2)
1. Run automated process with same inputs
2. Compare results at each iteration
3. Validate convergence behavior
4. Verify performance metrics

##### Phase 3: Comprehensive Testing (Day 3)
1. Execute full test matrix
2. Run edge case scenarios
3. Perform stress testing
4. Generate validation report

#### Validation Report Template
```markdown
# Mooring Tension Iteration Validation Report

## Summary
- Date: [Test Date]
- Result: PASS/FAIL
- Manual vs Auto Match: X.X%
- Performance Gain: XX%

## Detailed Comparison
| Metric | Manual | Automated | Delta |
|--------|--------|-----------|-------|
| Iterations | 4 | 3 | -25% |
| Total Time | 3.5 hrs | 3.2 min | -98.5% |
| Line01 Final | 2505.2 | 2504.8 | -0.02% |
| Line02 Final | 2498.3 | 2497.9 | -0.02% |
| Line03 Final | 2502.1 | 2501.7 | -0.02% |
| Line04 Final | 2499.5 | 2499.2 | -0.01% |
| Max Error | 0.8% | 0.7% | -12.5% |

## Test Results
[Detailed test case results]

## Certification
This automated process is validated to replace the manual process.
```

This technical specification provides the foundation for implementing a robust, efficient, and validated mooring tension iteration system integrated with OrcaFlex.