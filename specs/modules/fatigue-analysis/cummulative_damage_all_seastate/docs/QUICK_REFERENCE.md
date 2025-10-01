# ⚡ FATIGUE ANALYSIS QUICK REFERENCE CARD

## 🚀 Quick Start Commands

```bash
# Standard Run
cd specs/modules/fatigue-analysis/cummulative_damage
python cumulative_damage_analysis.py

# With Options
python cumulative_damage_analysis.py --config my_config.yml --log-level DEBUG

# Simple Runner
python run_analysis.py
```

## 📁 File Structure

```
Input Files:
{config}_FC{###}_Strut{#}_{location}_damage_rate.csv

Output Files:
output/
├── fatigue_life_summary.csv         # Main results
├── analysis_report.md                # Report
├── individual_results/*.csv         # Detailed
└── plots/*.png                       # Visuals
```

## ⚙️ Essential Config Settings

```yaml
# cumulative_damage_config.yml
processing:
  configurations:
    mode: "all"           # or "specific"
  parallel_processing: true
  max_workers: 4
  
output:
  individual_files:
    enabled: true
  plots:
    enabled: true
```

## 📊 Key Formulas

| Formula | Description |
|---------|------------|
| `D = Σ(n/N)` | Miner's Rule |
| `D_weighted = D × P(FC)` | Weighted damage |
| `Life = 1/D_total` | Fatigue life |
| `Inspection = Life/FDF` | FDF = 2-10 |

## 🎯 Critical Values

| Parameter | Good | Warning | Critical |
|-----------|------|---------|----------|
| Damage Rate | < 0.02 | 0.02-0.05 | > 0.05 |
| Fatigue Life | > 50 yr | 20-50 yr | < 20 yr |
| Contribution | < 30% | 30-50% | > 50% |

## 🐍 Python One-Liners

```python
# Read summary
df = pd.read_csv('output/fatigue_life_summary.csv')

# Find critical components
critical = df[df['fatigue_life_years'] < 20]

# Average life
avg_life = df['fatigue_life_years'].mean()

# Sort by damage
worst = df.nsmallest(10, 'fatigue_life_years')
```

## 📈 Understanding Results

```csv
# fatigue_life_summary.csv columns:
configuration      # Platform/structure ID
strut             # Component number
location          # Position on component
total_damage_rate # Annual damage (aim < 0.05)
fatigue_life_years # Expected life (aim > 20)
critical_fc       # Most damaging sea state
critical_contribution # % from worst FC
```

## 🔍 Quick Checks

```bash
# Count files processed
ls output/individual_results/ | wc -l

# Check for errors
grep ERROR cumulative_damage_analysis.log

# Find shortest life
sort -t',' -k5 -n output/fatigue_life_summary.csv | head

# View latest log entries
tail -20 cumulative_damage_analysis.log
```

## 🚨 Action Thresholds

| Life (years) | Action | Priority |
|-------------|--------|----------|
| < 5 | Immediate inspection | 🔴 Critical |
| 5-10 | Plan within 3 months | 🟠 High |
| 10-20 | Next maintenance | 🟡 Medium |
| 20-50 | Routine monitoring | 🟢 Low |
| > 50 | Standard program | ⚪ Minimal |

## 📝 Report Sections

1. **Executive Summary** - Key findings
2. **Methodology** - Analysis approach
3. **Results Overview** - Statistics
4. **Critical Findings** - Problem areas
5. **Recommendations** - Action items

## 🛠️ Common Fixes

| Issue | Solution |
|-------|----------|
| "File not found" | Check paths are relative |
| "Infinite life" | Normal if no damage |
| "Memory error" | Reduce max_workers |
| "Slow processing" | Enable parallel |

## 📞 Escalation

**Fatigue Life < 5 years:**
```
1. Stop analysis
2. Notify: Lead Engineer
3. Document: Location/component
4. Action: Immediate inspection
```

## 🔄 Workflow

```
Data Prep → Configure → Run → Review → Report
    ↓           ↓         ↓       ↓        ↓
  Files      Config    Python  Output   Share
```

## 💡 Pro Tips

1. **Always** check log file first
2. **Validate** input data format
3. **Start** with small dataset
4. **Compare** with previous analyses
5. **Document** all assumptions

## 📋 Checklist

**Before Running:**
- [ ] Input files in correct format?
- [ ] Config file updated?
- [ ] Output folder clean?
- [ ] Python environment ready?

**After Running:**
- [ ] Check summary file created
- [ ] Review critical components
- [ ] Verify plot generation
- [ ] Read analysis report
- [ ] Archive results

## 🔗 Quick Links

- Config: `cumulative_damage_config.yml`
- Script: `cumulative_damage_analysis.py`
- Log: `cumulative_damage_analysis.log`
- Results: `output/fatigue_life_summary.csv`
- Report: `output/analysis_report.md`

---
*Keep this card handy during analysis!*