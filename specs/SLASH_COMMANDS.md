# Slash Commands for Analysis Modules
## Quick Reference Guide

**Version**: 1.0.0  
**Last Updated**: 2025-01-24

---

## Available Slash Commands

### 1. `/create-analysis-module`
Create a new analysis module with full structure and documentation.

**Syntax:**
```
/create-analysis-module [module-name] [analysis-type] [options]
```

**Parameters:**
- `module-name`: Name of the module (kebab-case)
- `analysis-type`: Type of analysis (fatigue|modal|thermal|structural|signal)
- `options`: Additional configuration options

**Examples:**
```bash
/create-analysis-module rainflow-counting signal --standard ASTM-E1049
/create-analysis-module fatigue-damage fatigue --method miners-rule
/create-analysis-module modal-analysis structural --solver eigenvalue
/create-analysis-module stress-concentration structural --method fem
```

**What it creates:**
- Complete directory structure
- Configuration templates
- Validation framework
- Documentation templates
- Test suite structure

---

### 2. `/validate-module`
Run validation suite on existing module.

**Syntax:**
```
/validate-module [module-path] [validation-type]
```

**Validation Types:**
- `math`: Mathematical calculations
- `numerical`: Numerical accuracy
- `performance`: Performance benchmarks
- `production`: Production readiness
- `all`: Complete validation

**Examples:**
```bash
/validate-module specs/modules/signal-analysis/rainflow math
/validate-module specs/modules/fatigue-analysis/damage all
```

---

### 3. `/generate-validation-reports`
Generate standardized validation reports.

**Syntax:**
```
/generate-validation-reports [module-path] [report-type]
```

**Report Types:**
- `engineering`: Engineering calculations validation
- `production`: Production execution report
- `summary`: Executive summary
- `progress`: Progress tracking
- `all`: All reports

**Examples:**
```bash
/generate-validation-reports rainflow-analysis engineering
/generate-validation-reports fatigue-module all
```

---

### 4. `/run-analysis`
Execute analysis module on data.

**Syntax:**
```
/run-analysis [module] [config-file] [options]
```

**Options:**
- `--parallel`: Use parallel processing
- `--validate`: Run validation after analysis
- `--report`: Generate report after completion
- `--visualize`: Create visualizations

**Examples:**
```bash
/run-analysis rainflow config/production.yml --parallel --report
/run-analysis fatigue-damage config/test.yml --validate
```

---

### 5. `/benchmark-module`
Run performance benchmarks.

**Syntax:**
```
/benchmark-module [module-path] [dataset-size]
```

**Dataset Sizes:**
- `small`: 10-100 files
- `medium`: 100-1000 files
- `large`: 1000-10000 files
- `production`: Full production dataset

**Examples:**
```bash
/benchmark-module rainflow-analysis medium
/benchmark-module fatigue-module production
```

---

### 6. `/compare-results`
Compare results between different runs or methods.

**Syntax:**
```
/compare-results [result1] [result2] [comparison-type]
```

**Comparison Types:**
- `numerical`: Numerical differences
- `statistical`: Statistical analysis
- `visual`: Visual comparison
- `all`: Complete comparison

**Examples:**
```bash
/compare-results output/run1 output/run2 numerical
/compare-results method1.csv method2.csv statistical
```

---

### 7. `/package-module`
Package module for deployment.

**Syntax:**
```
/package-module [module-path] [version] [target]
```

**Targets:**
- `development`: Dev environment
- `testing`: Test environment
- `production`: Production environment
- `archive`: Archive package

**Examples:**
```bash
/package-module rainflow-analysis 1.0.0 production
/package-module fatigue-module 2.1.0 testing
```

---

## Module Creation Workflow

### Standard Workflow Using Slash Commands:

```bash
# Step 1: Create module structure
/create-analysis-module my-analysis signal --standard ISO-12345

# Step 2: Implement calculations
# [Manual coding in src/ directory]

# Step 3: Validate mathematics
/validate-module my-analysis math

# Step 4: Run test cases
/run-analysis my-analysis config/test.yml --validate

# Step 5: Benchmark performance
/benchmark-module my-analysis medium

# Step 6: Generate validation reports
/generate-validation-reports my-analysis all

# Step 7: Run production
/run-analysis my-analysis config/production.yml --parallel --report

# Step 8: Package for deployment
/package-module my-analysis 1.0.0 production
```

---

## Report Naming Convention

All validation reports follow this naming convention:

```
VALIDATION_XX_[REPORT_TYPE].md

Where:
- XX: Two-digit sequence number (01, 02, 03...)
- REPORT_TYPE: Type of validation report

Examples:
- VALIDATION_01_ENGINEERING_CALCULATIONS.md
- VALIDATION_02_PRODUCTION_EXECUTION.md
- VALIDATION_03_EXECUTION_SUMMARY.md
- VALIDATION_04_PROGRESS_LOG.md
```

---

## Configuration File Templates

### Basic Configuration (config.yml):
```yaml
module:
  name: "module-name"
  version: "1.0.0"
  type: "analysis-type"

input:
  data_path: "path/to/data"
  format: "csv"
  
processing:
  method: "algorithm-name"
  parallel: true
  
output:
  path: "path/to/output"
  format: "csv"
  reports: true
  
validation:
  enabled: true
  tolerance: 1e-6
```

---

## Quick Tips

### For Engineers:
1. Always validate math before production
2. Use parallel processing for large datasets
3. Generate all reports for documentation
4. Keep validation tolerance appropriate to physics

### For Developers:
1. Follow the template structure exactly
2. Implement all validation interfaces
3. Document all equations and assumptions
4. Include edge case handling

### For Managers:
1. Review VALIDATION_01 for mathematical rigor
2. Check VALIDATION_02 for production readiness
3. Use VALIDATION_03 for executive summaries
4. Monitor VALIDATION_04 for progress tracking

---

## Standard Compliance

All modules created with these commands comply with:
- ISO 9001: Quality Management
- ASME V&V 10/20: Verification and Validation
- Industry-specific standards (ASTM, API, ISO)

---

## Support

For issues or questions:
- Check module documentation
- Review validation reports
- Consult the GENERIC_ANALYSIS_MODULE_TEMPLATE.md
- Contact engineering support

---

**Note**: These slash commands are designed to work with AI assistants for rapid module development and validation.