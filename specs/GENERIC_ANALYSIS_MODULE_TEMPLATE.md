# Generic Analysis Module Specification Template
## Slash Command: `/create-analysis-module`

**Template Version**: 1.0.0  
**Created**: 2025-01-24  
**Purpose**: Standardized template for creating analysis modules with mathematical validation

---

## Usage Instructions

### For AI Assistant (Slash Command):
```
/create-analysis-module [module-name] [analysis-type]

Examples:
/create-analysis-module fatigue-damage miner-rule
/create-analysis-module modal-analysis eigenvalue
/create-analysis-module stress-concentration fem
```

### For Engineers:
1. Copy this template to your module folder
2. Fill in all sections marked with `[SPECIFY]`
3. Run validation tests
4. Generate required documentation

---

## 1. MODULE IDENTIFICATION

| Field | Value |
|-------|-------|
| Module Name | `[SPECIFY: e.g., rainflow-analysis]` |
| Module Type | `[SPECIFY: signal-analysis/fatigue/structural/thermal]` |
| Version | `[SPECIFY: 1.0.0]` |
| Author | `[SPECIFY]` |
| Date Created | `[SPECIFY: YYYY-MM-DD]` |
| Status | `[SPECIFY: development/testing/production]` |

---

## 2. MODULE SPECIFICATION

### 2.1 Purpose
```
[SPECIFY: Clear description of what this module does]
Example: "Performs rainflow cycle counting on time-series data for fatigue analysis"
```

### 2.2 Scope
- **Included**: `[SPECIFY: What is covered]`
- **Excluded**: `[SPECIFY: What is NOT covered]`
- **Limitations**: `[SPECIFY: Known constraints]`

### 2.3 Standards Compliance
```
[SPECIFY: Industry standards followed]
Examples:
- ASTM E1049-85 (Rainflow Counting)
- ISO 12107:2012 (Fatigue Testing)
- API 579-1/ASME FFS-1 (Fitness for Service)
```

---

## 3. MATHEMATICAL FRAMEWORK

### 3.1 Core Equations
```
Equation 1: [NAME]
[LATEX or ASCII representation]
Where:
- Variable1 = [description] [units]
- Variable2 = [description] [units]

Example:
Equation 1: Rainflow Range
R = |S_max - S_min|
Where:
- R = Stress range [MPa]
- S_max = Maximum stress [MPa]
- S_min = Minimum stress [MPa]
```

### 3.2 Algorithm Description
```
Step 1: [SPECIFY: First calculation step]
Step 2: [SPECIFY: Second calculation step]
Step N: [SPECIFY: Final calculation step]
```

### 3.3 Numerical Methods
- **Integration Method**: `[SPECIFY: e.g., Trapezoidal, Simpson's]`
- **Solver Type**: `[SPECIFY: e.g., Direct, Iterative]`
- **Convergence Criteria**: `[SPECIFY: e.g., 1e-6 relative error]`

---

## 4. INPUT/OUTPUT SPECIFICATION

### 4.1 Input Data

| Parameter | Type | Units | Range | Required | Default |
|-----------|------|-------|-------|----------|---------|
| `[name]` | `[float/int/string]` | `[units]` | `[min-max]` | Yes/No | `[value]` |

Example:
| Parameter | Type | Units | Range | Required | Default |
|-----------|------|-------|-------|----------|---------|
| time_series | array | kN | [-∞,+∞] | Yes | - |
| sampling_rate | float | Hz | [0.1-1000] | Yes | 10 |
| bin_count | int | - | [10-100] | No | 50 |

### 4.2 Output Data

| Parameter | Type | Units | Description | File Format |
|-----------|------|-------|-------------|-------------|
| `[name]` | `[type]` | `[units]` | `[description]` | `[csv/json/hdf5]` |

Example:
| Parameter | Type | Units | Description | File Format |
|-----------|------|-------|-------------|-------------|
| cycles | array | - | Rainflow cycles | CSV |
| damage | float | - | Cumulative damage | JSON |
| spectrum | array | Hz/amplitude | Frequency spectrum | CSV |

---

## 5. IMPLEMENTATION STRUCTURE

### 5.1 Directory Structure
```
module-name/
├── input/
│   ├── config.yml           # Configuration file
│   └── data/                # Input data files
├── src/
│   ├── core/                # Core algorithms
│   ├── utils/               # Utility functions
│   └── validation/          # Validation scripts
├── output/
│   ├── results/             # Analysis results
│   ├── reports/             # Generated reports
│   └── visualizations/      # Plots and figures
├── tests/
│   ├── unit/                # Unit tests
│   └── integration/         # Integration tests
├── docs/
│   ├── theory/              # Theoretical background
│   └── validation/          # Validation reports
└── user_spec.md             # User specification
```

### 5.2 Configuration Template
```yaml
# config.yml template
project:
  name: "[PROJECT_NAME]"
  version: "[VERSION]"
  author: "[AUTHOR]"

analysis:
  type: "[ANALYSIS_TYPE]"
  method: "[METHOD_NAME]"
  parameters:
    param1: [value]
    param2: [value]

input:
  data_source: "[PATH]"
  format: "[FORMAT]"
  preprocessing:
    - step1
    - step2

output:
  directory: "[PATH]"
  formats: ["csv", "json"]
  visualizations: [true/false]

validation:
  enabled: [true/false]
  tolerance: [value]
  reference: "[PATH]"
```

---

## 6. VALIDATION FRAMEWORK

### 6.1 Test Cases

| Test ID | Description | Input | Expected Output | Tolerance | Status |
|---------|-------------|-------|-----------------|-----------|---------|
| TC-001 | `[description]` | `[input]` | `[output]` | `[tolerance]` | ⏳/✅/❌ |

### 6.2 Validation Metrics
- **Accuracy**: `[SPECIFY: e.g., <0.1% error]`
- **Precision**: `[SPECIFY: e.g., 6 decimal places]`
- **Conservation**: `[SPECIFY: e.g., mass/energy/cycles]`
- **Stability**: `[SPECIFY: e.g., condition number <1000]`

### 6.3 Verification Checklist
- [ ] Mathematical equations verified
- [ ] Dimensional analysis passed
- [ ] Boundary conditions tested
- [ ] Conservation laws checked
- [ ] Numerical stability confirmed
- [ ] Performance benchmarked
- [ ] Edge cases handled

---

## 7. DOCUMENTATION REQUIREMENTS

### 7.1 Required Documents

| Document | Purpose | Template | Status |
|----------|---------|----------|---------|
| VALIDATION_01_ENGINEERING_CALCULATIONS | Mathematical verification | `[link]` | ⏳ |
| VALIDATION_02_PRODUCTION_EXECUTION | Execution validation | `[link]` | ⏳ |
| VALIDATION_03_EXECUTION_SUMMARY | Results summary | `[link]` | ⏳ |
| VALIDATION_04_PROGRESS_LOG | Progress tracking | `[link]` | ⏳ |
| USER_MANUAL | User instructions | `[link]` | ⏳ |
| API_REFERENCE | API documentation | `[link]` | ⏳ |

### 7.2 Validation Report Sections
1. **Executive Summary**
2. **Mathematical Validation**
3. **Numerical Verification**
4. **Test Results**
5. **Performance Metrics**
6. **Conclusions**
7. **Appendices**

---

## 8. QUALITY ASSURANCE

### 8.1 Code Standards
- **Language**: `[Python/MATLAB/Julia/etc.]`
- **Style Guide**: `[PEP8/Google/etc.]`
- **Documentation**: `[Docstrings/Comments]`
- **Version Control**: `[Git]`

### 8.2 Review Process
1. **Peer Review**: Technical accuracy
2. **Engineering Review**: Physical validity
3. **Manager Review**: Business requirements
4. **QA Review**: Standards compliance

### 8.3 Performance Requirements
- **Processing Time**: `[SPECIFY: e.g., <5s per file]`
- **Memory Usage**: `[SPECIFY: e.g., <4GB RAM]`
- **Accuracy**: `[SPECIFY: e.g., 99.9%]`
- **Scalability**: `[SPECIFY: e.g., 10,000 files]`

---

## 9. INTEGRATION SPECIFICATIONS

### 9.1 Dependencies
```
[SPECIFY: Required libraries/modules]
Example:
- numpy >= 1.20.0
- scipy >= 1.7.0
- pandas >= 1.3.0
- matplotlib >= 3.4.0
```

### 9.2 API Interface
```python
# Example API structure
class AnalysisModule:
    def __init__(self, config_path: str):
        """Initialize module with configuration"""
        pass
    
    def load_data(self, data_path: str):
        """Load input data"""
        pass
    
    def run_analysis(self):
        """Execute main analysis"""
        pass
    
    def validate_results(self):
        """Validate output"""
        pass
    
    def generate_report(self):
        """Create validation report"""
        pass
```

### 9.3 Data Formats
- **Input**: `[SPECIFY: CSV/JSON/HDF5/etc.]`
- **Output**: `[SPECIFY: CSV/JSON/HDF5/etc.]`
- **Config**: `[SPECIFY: YAML/JSON/etc.]`

---

## 10. DEPLOYMENT CHECKLIST

### Pre-Production
- [ ] All tests passing
- [ ] Documentation complete
- [ ] Peer review approved
- [ ] Performance benchmarked
- [ ] Edge cases tested

### Production Release
- [ ] Version tagged
- [ ] Release notes created
- [ ] User manual updated
- [ ] API documented
- [ ] Training provided

### Post-Production
- [ ] Monitoring enabled
- [ ] Issue tracking active
- [ ] Feedback collected
- [ ] Updates planned

---

## APPENDIX A: Template Variables

Replace all `[SPECIFY]` tags with actual values:
- `[MODULE_NAME]`: Your module name (e.g., "rainflow-analysis")
- `[ANALYSIS_TYPE]`: Type of analysis (e.g., "fatigue", "modal", "thermal")
- `[VERSION]`: Semantic version (e.g., "1.0.0")
- `[AUTHOR]`: Author name and contact
- `[DATE]`: ISO date format (YYYY-MM-DD)

---

## APPENDIX B: Validation Report Templates

### B.1 Engineering Calculations Template
```markdown
# Engineering Calculation Validation
## Module: [MODULE_NAME]

### 1. Mathematical Framework
[Equations and derivations]

### 2. Numerical Implementation
[Algorithm details]

### 3. Test Cases
[Verification examples]

### 4. Results
[Validation outcomes]

### 5. Conclusions
[Pass/Fail status]
```

### B.2 Production Execution Template
```markdown
# Production Execution Report
## Module: [MODULE_NAME]

### 1. Execution Summary
- Files Processed: [NUMBER]
- Success Rate: [PERCENTAGE]
- Processing Time: [DURATION]

### 2. Results
[Key findings]

### 3. Issues
[Any problems encountered]

### 4. Recommendations
[Next steps]
```

---

## APPENDIX C: Quick Start Commands

```bash
# Create new module from template
cp specs/GENERIC_ANALYSIS_MODULE_TEMPLATE.md specs/modules/[category]/[module-name]/

# Initialize module structure
mkdir -p specs/modules/[category]/[module-name]/{input,src,output,tests,docs}

# Run validation suite
python validate_module.py --module [module-name]

# Generate reports
python generate_reports.py --module [module-name] --type all

# Package for deployment
python package_module.py --module [module-name] --version [version]
```

---

**END OF TEMPLATE**

*This template ensures consistent, high-quality analysis modules with proper validation and documentation.*