# 🔧 Cumulative Damage Analysis - Troubleshooting Guide

## Quick Diagnostic Flowchart

```
Analysis Failed?
    │
    ├─→ Files not found? → Check Section 1
    ├─→ Configuration error? → Check Section 2
    ├─→ Processing error? → Check Section 3
    ├─→ Results unexpected? → Check Section 4
    └─→ Performance issues? → Check Section 5
```

---

## Section 1: File and Path Issues

### 🔴 ERROR: "FileNotFoundError: [Errno 2] No such file or directory"

**Symptoms:**
```python
FileNotFoundError: [Errno 2] No such file or directory: 'specs/modules/...'
```

**Diagnosis Steps:**
```bash
# 1. Check current directory
pwd

# 2. List available files
ls sample_damage_results/
ls input/

# 3. Verify file naming pattern
ls sample_damage_results/ | head -5
```

**Solutions:**

✅ **Solution 1: Fix Relative Paths**
```yaml
# In config file, change absolute to relative:
input:
  damage_rates:
    folder: "sample_damage_results"  # NOT "C:/full/path/"
  fatigue_conditions:
    file: "input/fatigue_seastates_production.csv"
```

✅ **Solution 2: Run from Correct Directory**
```bash
# Always run from module directory
cd specs/modules/fatigue-analysis/cummulative_damage
python cumulative_damage_analysis.py
```

✅ **Solution 3: Check File Naming Convention**
```python
# Expected pattern:
# {config}_FC{###}_Strut{#}_{location}_damage_rate.csv
# Example: platform_FC001_Strut1_loc02_damage_rate.csv

# If your files differ, update config:
pattern: "{your_pattern}_damage_rate.csv"
```

---

### 🔴 ERROR: "No input files found"

**Symptoms:**
```
INFO - Found 0 unique config/strut/location combinations
ERROR - No input files found
```

**Solutions:**

✅ **Check File Extensions**
```bash
# Ensure files end with .csv
ls sample_damage_results/*.csv
# If no results, files may have wrong extension
```

✅ **Verify Folder Structure**
```bash
# Correct structure:
tree -L 2
# Should show:
# ├── sample_damage_results/
# │   ├── *_damage_rate.csv files
# ├── input/
# │   └── fatigue_seastates_production.csv
```

---

## Section 2: Configuration Issues

### 🟡 WARNING: "Occurrence probabilities sum to X%, expected ~100%"

**Symptoms:**
```
WARNING - Occurrence probabilities sum to 94.23%, expected ~100%
```

**Diagnosis:**
```python
# Check sea state file
import pandas as pd
df = pd.read_csv('input/fatigue_seastates_production.csv')
total = df['Occurrence (%)'].sum()
print(f"Total: {total}%")
```

**Solutions:**

✅ **Solution 1: Accept if Close (±1%)**
```yaml
# This is often acceptable
quality_checks:
  checks:
    - name: "occurrence_sum"
      tolerance: 1.0  # Allow 1% deviation
```

✅ **Solution 2: Normalize Probabilities**
```python
# Normalize to exactly 100%
df['Occurrence (%)'] = df['Occurrence (%)'] / total * 100
df.to_csv('input/fatigue_seastates_production_normalized.csv')
```

---

### 🔴 ERROR: "KeyError: 'column_name'"

**Symptoms:**
```python
KeyError: 'damage_rate_per_year'
```

**Solutions:**

✅ **Check Column Names**
```python
# Verify column names in your files
import pandas as pd
df = pd.read_csv('sample_damage_results/your_file.csv')
print(df.columns.tolist())

# Update config if different:
columns:
  - "your_stress_column"
  - "your_cycles_column"
  - "your_damage_column"
```

---

## Section 3: Processing Errors

### 🔴 ERROR: "ValueError: cannot convert float infinity to integer"

**Symptoms:**
- Error during calculation
- Related to infinite fatigue life

**Solutions:**

✅ **This is Normal for Zero Damage**
```python
# Add handling in your analysis:
if np.isinf(fatigue_life):
    fatigue_life = 999999  # Or other large number
```

---

### 🟡 WARNING: "Fatigue life inf years outside bounds"

**Symptoms:**
```
WARNING - Fatigue life inf years outside bounds [1.0, 1000.0]
```

**This is NORMAL when:**
- Damage rate is zero
- Component has no fatigue damage
- Stress below fatigue limit

**When to Investigate:**
- If you expected damage but got none
- Check if S-N curve was applied correctly
- Verify stress units (MPa vs Pa)

---

### 🔴 ERROR: "Memory Error"

**Symptoms:**
```
MemoryError: Unable to allocate array
```

**Solutions:**

✅ **Solution 1: Disable Parallel Processing**
```yaml
execution:
  parallel_processing: false
```

✅ **Solution 2: Reduce Workers**
```yaml
execution:
  parallel_processing: true
  max_workers: 2  # Reduce from 4
```

✅ **Solution 3: Process in Batches**
```python
# Modify code to process in chunks
chunk_size = 50
for i in range(0, len(combinations), chunk_size):
    batch = combinations[i:i+chunk_size]
    process_batch(batch)
```

---

## Section 4: Unexpected Results

### 🤔 All Components Show Infinite Life

**Diagnosis:**
```bash
# Check a damage file
head sample_damage_results/*FC001*.csv
# Look for non-zero damage_rate_per_year values
```

**Common Causes:**
1. S-N curve not applied
2. Stress units incorrect
3. Stress below endurance limit
4. Test data with zero damage

**Solutions:**

✅ **Verify Damage Calculations**
```python
# Check if damage files have actual damage
import pandas as pd
import glob

for file in glob.glob('sample_damage_results/*.csv')[:5]:
    df = pd.read_csv(file)
    total_damage = df['damage_rate_per_year'].sum()
    print(f"{file}: {total_damage}")
```

---

### 🤔 Fatigue Life Too Short (< 5 years)

**Diagnosis Questions:**
1. Are stress units correct? (MPa vs ksi)
2. Is SCF (Stress Concentration Factor) applied twice?
3. Is thickness correction appropriate?
4. Are cycles per year correct?

**Solutions:**

✅ **Verify Units**
```python
# Check stress magnitude
df = pd.read_csv('damage_file.csv')
print(f"Max stress: {df['stress_range_mpa'].max()} MPa")
# Typical: 50-300 MPa for steel
# If >1000, might be in wrong units
```

✅ **Check Annual Cycles**
```python
# Typical: 1-10 million cycles/year
print(f"Total cycles: {df['cycles_annual'].sum()}")
# If >100 million, might be too high
```

---

### 🤔 Critical FC Doesn't Make Sense

**Example:** Calm sea state (FC050) shows as critical

**Diagnosis:**
```python
# Check individual FC damage
result_df = pd.read_csv('output/individual_results/..._fatigue_life.csv')
print(result_df[['fatigue_condition', 'weighted_damage_rate', 'contribution_percent']])
```

**Solutions:**

✅ **Verify FC Mapping**
```bash
# Check if FC IDs match between files
ls sample_damage_results/*FC050* 
# Compare with fatigue_seastates_production.csv
```

---

## Section 5: Performance Issues

### 🐌 Analysis Running Too Slowly

**Symptoms:**
- Processing takes hours
- System becomes unresponsive

**Solutions:**

✅ **Solution 1: Enable Parallel Processing**
```yaml
execution:
  parallel_processing: true
  max_workers: 4
```

✅ **Solution 2: Skip Plotting for Testing**
```yaml
output:
  plots:
    enabled: false  # Temporarily disable
```

✅ **Solution 3: Process Subset First**
```yaml
processing:
  configurations:
    mode: "specific"
    list: ["config1"]  # Test with one first
  struts:
    mode: "specific"  
    list: [1, 2]  # Just two struts
```

---

## Section 6: Output Issues

### 📊 Plots Not Generated

**Diagnosis:**
```bash
ls output/plots/
# If empty, check log for errors
grep -i plot cumulative_damage_analysis.log
```

**Solutions:**

✅ **Check Matplotlib Backend**
```python
# Add to script
import matplotlib
matplotlib.use('Agg')  # For headless systems
import matplotlib.pyplot as plt
```

✅ **Skip Plots for Zero Damage**
```python
# Already implemented in updated code
if result.total_damage_rate == 0:
    return  # Skip plotting
```

---

### 📄 Report Not Generated

**Solutions:**

✅ **Check Dependencies**
```bash
pip install markdown pandas
```

✅ **Verify Results Exist**
```bash
# Report needs summary file
ls output/fatigue_life_summary.csv
```

---

## Section 7: Common Error Messages Decoded

| Error Message | Likely Cause | Quick Fix |
|--------------|--------------|-----------|
| `FileNotFoundError` | Wrong path | Use relative paths |
| `KeyError: 'Row'` | Column name mismatch | Check CSV headers |
| `ValueError: invalid literal` | Data format issue | Check for text in number columns |
| `MemoryError` | Too much data | Reduce parallel workers |
| `PermissionError` | File locked | Close Excel/other programs |
| `ModuleNotFoundError` | Missing package | `pip install [package]` |
| `ZeroDivisionError` | Zero damage rate | Normal for no damage |

---

## Section 8: Prevention Checklist

### Before Running Analysis

**Data Preparation:**
- [ ] Files follow naming convention
- [ ] All CSV files valid format
- [ ] Sea states file sums to ~100%
- [ ] No Excel files open

**Configuration:**
- [ ] Paths are relative, not absolute
- [ ] Column names match your files
- [ ] Output folder is writable
- [ ] Parallel processing appropriate for system

**Environment:**
- [ ] Python 3.8+ installed
- [ ] Required packages installed
- [ ] Sufficient disk space
- [ ] Running from correct directory

---

## Section 9: Getting Help

### Information to Provide

When asking for help, include:

1. **Error Message** (complete traceback)
2. **Config File** (relevant sections)
3. **Sample Data** (first few lines)
4. **What You Tried** (solutions attempted)
5. **Environment** (Python version, OS)

### Example Help Request

```
I'm getting FileNotFoundError when running analysis.

Error:
FileNotFoundError: [Errno 2] No such file or directory: 'input/fatigue_seastates_production.csv'

Config:
fatigue_conditions:
  file: "input/fatigue_seastates_production.csv"

Directory Structure:
cummulative_damage/
  ├── cumulative_damage_analysis.py
  └── input/
      └── fatigue_seastates_production.csv

Python 3.9, Windows 10
Tried: Running from different directories
```

---

## Section 10: Emergency Procedures

### 🚨 Critical Component Found (Life < 5 years)

**Immediate Actions:**
1. **STOP** - Do not delete or modify results
2. **DOCUMENT** - Screenshot/save the finding
3. **VERIFY** - Re-run analysis for that component
4. **ESCALATE** - Contact lead engineer immediately
5. **PRESERVE** - Archive all input/output files

```bash
# Quick verification
python cumulative_damage_analysis.py --config verify_critical.yml
# Where verify_critical.yml focuses on the critical component
```

### 🚨 Analysis Crashes Repeatedly

**Recovery Steps:**
1. Clear output directory
2. Run with minimal config
3. Gradually add complexity
4. Identify breaking point

```bash
# Clean start
rm -rf output/*
python cumulative_damage_analysis.py --config minimal.yml
```

---

## Quick Contact Reference

- **Technical Support**: fatigue-support@company.com
- **Emergency (Critical Finding)**: Call facility manager
- **IT Support** (Python/System): helpdesk@company.com
- **Module Developer**: Check Git history for contributors

---

*Remember: Most "errors" are just configuration issues. Stay calm and work through systematically!*