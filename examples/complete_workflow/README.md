# Complete Workflow Example: Marine Structural Analysis

This example demonstrates the complete workspace-hub development workflow from user prompt through to execution.

## Overview

This example shows how to use the engineering workflow template to create a marine structural analysis module that:
- Reads hull geometry and loading data
- Performs finite element analysis
- Validates against classification society rules
- Generates interactive HTML reports with stress visualizations

## Files in This Example

```
complete_workflow/
├── README.md                    # This file
├── user_prompt.md               # Original immutable user requirements
├── sample_config.yaml           # Example YAML configuration
├── sample_hull_data.csv         # Sample input data
└── expected_output/             # Example of expected outputs
    ├── analysis_report.html     # Interactive HTML report
    ├── results.json             # Detailed calculation results
    └── summary.csv              # Summary statistics
```

## Step-by-Step Workflow

### 1. Create User Prompt

First, document the requirements in `user_prompt.md`. This file is **immutable** - it never changes after creation.

```bash
# Copy user_prompt.md to your repository
cp examples/complete_workflow/user_prompt.md .agent-os/user_prompt.md
```

### 2. Run the Workflow Script

Execute the engineering workflow to generate all necessary files:

```bash
./templates/workflows/engineering_workflow.sh marine-structural-analysis
```

This will:
1. ✅ Read `user_prompt.md`
2. ✅ Create changelog file
3. ✅ Generate YAML configuration
4. ✅ Validate YAML schema
5. ✅ Initialize approval tracking
6. ✅ Generate pseudocode specification
7. ✅ Record approval
8. ✅ Create execution scripts

### 3. Review Generated Files

The workflow creates:

```
.agent-os/
├── user_prompt.md               # Original requirements (immutable)
├── user_prompt_changelog.md     # Change tracking
└── specs/
    └── marine-structural-analysis/
        ├── config.yaml          # Module configuration
        ├── pseudocode_v1.0.md   # Algorithm specification
        └── approval_log.md      # SPARC phase approvals
```

### 4. Approve Pseudocode

Review the generated pseudocode and approve it:

```bash
python modules/automation/approval_tracker.py \
  --spec marine-structural-analysis \
  --workspace . \
  submit \
  --phase pseudocode \
  --version "1.0" \
  --approver "John Doe" \
  --status APPROVED \
  --changes "Initial specification" \
  --comments "Pseudocode reviewed and approved"
```

Check approval status:

```bash
python modules/automation/approval_tracker.py \
  --spec marine-structural-analysis \
  --workspace . \
  status
```

### 5. Follow TDD Implementation

Create test structure:

```bash
mkdir -p tests/{unit,integration,verification}
mkdir -p src/modules/marine-structural-analysis/{analysis,validation,reporting}
```

Write tests **first**:

```bash
# Write verification tests (benchmark problems)
vim tests/verification/test_benchmark_cantilever_beam.py

# Write unit tests
vim tests/unit/test_stress_calculator.py
vim tests/unit/test_geometry_loader.py

# Run tests (should fail)
pytest tests/ -v
```

Implement code to pass tests:

```bash
# Implement analysis modules
vim src/modules/marine-structural-analysis/analysis/fem_solver.py
vim src/modules/marine-structural-analysis/analysis/stress_calculator.py
vim src/modules/marine-structural-analysis/validation/rule_checker.py

# Run tests again (should pass)
pytest tests/ --cov=src --cov-report=html
```

Verify coverage:

```bash
# Must achieve 80%+ coverage
pytest --cov=src --cov-report=term --cov-fail-under=80
```

### 6. Update Pseudocode (If Needed)

If implementation reveals changes needed:

```bash
# Create updated pseudocode
cp .agent-os/specs/marine-structural-analysis/pseudocode_v1.0.md \
   .agent-os/specs/marine-structural-analysis/pseudocode_v1.1.md

# Edit v1.1
vim .agent-os/specs/marine-structural-analysis/pseudocode_v1.1.md

# Generate diff report
python modules/automation/pseudocode_diff.py \
  --original .agent-os/specs/marine-structural-analysis/pseudocode_v1.0.md \
  --updated .agent-os/specs/marine-structural-analysis/pseudocode_v1.1.md \
  --output .agent-os/specs/marine-structural-analysis/diff_v1.0_to_v1.1.html

# Review diff and re-approve
python modules/automation/approval_tracker.py \
  --spec marine-structural-analysis \
  --workspace . \
  submit \
  --phase pseudocode \
  --version "1.1" \
  --approver "John Doe" \
  --status APPROVED \
  --changes "Added fatigue analysis module" "Updated stress calculation algorithm" \
  --comments "Changes reviewed via diff report. Enhanced functionality approved."
```

### 7. Create Module Entry Point

Create `__main__.py` for CLI access:

```python
# src/modules/marine-structural-analysis/__main__.py
import argparse
import sys
from pathlib import Path
from .analysis.pipeline import run_analysis

def main():
    parser = argparse.ArgumentParser(
        description='Marine Structural Analysis Module'
    )
    parser.add_argument(
        '--config',
        type=Path,
        required=True,
        help='Path to YAML configuration file'
    )
    parser.add_argument(
        '--output',
        type=Path,
        default=Path('reports'),
        help='Output directory for reports'
    )
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose logging'
    )
    parser.add_argument(
        '--log-calculations',
        action='store_true',
        help='Enable detailed calculation logging'
    )

    args = parser.parse_args()

    # Run analysis
    results = run_analysis(
        config_path=args.config,
        output_dir=args.output,
        verbose=args.verbose,
        log_calculations=args.log_calculations
    )

    return 0 if results['status'] == 'success' else 1

if __name__ == '__main__':
    sys.exit(main())
```

### 8. Run the Analysis

Execute using the generated script:

```bash
# Run with sample configuration
./scripts/run_marine-structural-analysis.sh \
  .agent-os/specs/marine-structural-analysis/config.yaml \
  reports/marine

# Or run directly via Python
python -m src.modules.marine-structural-analysis \
  --config .agent-os/specs/marine-structural-analysis/config.yaml \
  --output reports/marine \
  --verbose \
  --log-calculations
```

### 9. Review Generated Reports

Open the interactive HTML report:

```bash
open reports/marine/analysis_report.html
```

The report includes:
- Executive summary with key findings
- Input parameters documentation
- Interactive 3D stress plots (Plotly)
- Deformation visualizations
- Validation check results
- Detailed calculation appendix

### 10. Verify Quality

Run complete quality checks:

```bash
# Run all tests with coverage
pytest tests/ --cov=src --cov-report=html --cov-fail-under=80

# Run performance tests
pytest tests/performance/ -m performance --benchmark-only

# Check logging compliance
grep -r "logger" src/ | wc -l

# Verify all YAML configurations
find . -name "*.yaml" -exec python modules/automation/validate_yaml.py {} \;
```

## Configuration Example

See `sample_config.yaml` for a complete configuration example showing:
- Module metadata
- Execution parameters
- Input specifications
- Output definitions
- Validation rules
- Performance thresholds
- Logging configuration

## Sample Data

The `sample_hull_data.csv` contains example structural data:
- Node coordinates
- Element connectivity
- Material properties
- Loading conditions

## Expected Output

The `expected_output/` directory shows what the analysis should produce:
- Interactive HTML report with embedded Plotly visualizations
- JSON results file with all calculation details
- CSV summary with key metrics

## Troubleshooting

**YAML Validation Fails:**
```bash
# Check schema compliance
python modules/automation/validate_yaml.py config.yaml --verbose
```

**Tests Failing:**
```bash
# Run tests with detailed output
pytest tests/ -vv --tb=long
```

**Coverage Below 80%:**
```bash
# Generate coverage report and identify gaps
pytest --cov=src --cov-report=html
open htmlcov/index.html
```

**Pseudocode Changes Needed:**
```bash
# Generate diff and review impact
python modules/automation/pseudocode_diff.py \
  --original old_version.md \
  --updated new_version.md \
  --output diff_report.html
```

## Best Practices

1. **Never Modify user_prompt.md**: Use changelog for tracking changes
2. **Always Write Tests First**: Follow TDD strictly
3. **Use Approval Tracker**: Document all phase approvals
4. **Generate Diffs**: When updating pseudocode, always create diff reports
5. **Maintain Coverage**: Keep test coverage above 80%
6. **Peer Review**: For engineering modules, get technical review
7. **Interactive Visualizations**: Always use Plotly/Bokeh, never static images
8. **Comprehensive Logging**: Log at all five levels (DEBUG, INFO, WARNING, ERROR, CRITICAL)

## Additional Resources

- Development Workflow Guidelines: `docs/DEVELOPMENT_WORKFLOW_GUIDELINES.md`
- Testing Standards: `docs/TESTING_FRAMEWORK_STANDARDS.md`
- Logging Standards: `docs/LOGGING_STANDARDS.md`
- YAML Validation: `modules/automation/validate_yaml.py --help`
- Approval Tracking: `modules/automation/approval_tracker.py --help`
- Pseudocode Diff: `modules/automation/pseudocode_diff.py --help`

## Next Steps

After completing this example workflow:

1. Adapt for your specific use case
2. Create your own user_prompt.md
3. Run the appropriate workflow script (python_analysis, web_app, or engineering)
4. Follow TDD implementation
5. Generate reports and validate results

---

**Note:** This example demonstrates the complete workflow. Your actual implementation will vary based on your specific requirements, but the process remains the same.
