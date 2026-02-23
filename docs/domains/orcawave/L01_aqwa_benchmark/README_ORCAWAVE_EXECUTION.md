# OrcaWave Benchmark Execution Guide

## 🚀 Quick Start

### Option 1: Python Script (Recommended)

```bash
# Dry run (validation only)
python run_orcawave_benchmark.py --dry-run

# Full execution with 4 threads
python run_orcawave_benchmark.py --threads 4

# Custom configuration
python run_orcawave_benchmark.py --config my_config.yml --threads 8
```

### Option 2: Batch File (Windows)

```bash
# Double-click or run from command line
run_orcawave.bat

# With custom options
run_orcawave.bat --dry-run
```

### Option 3: Existing Parallel Execution Script

```bash
# Use the full-featured parallel execution script
cd specs/modules/orcawave/diffraction-analysis/scripts
python execute_orcawave_parallel.py --config ../../../../docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.yml
```

---

## 📋 Command Options

### `run_orcawave_benchmark.py`

| Option | Description | Default |
|--------|-------------|---------|
| `--config FILE` | OrcaWave YAML configuration | `orcawave_001_ship_raos_rev2.yml` |
| `--threads N` | Parallel validation threads | `4` |
| `--dry-run` | Validate only, don't execute | `False` |
| `--skip-comparison` | Skip AQWA comparison | `False` |

### Examples

```bash
# Validate configuration
python run_orcawave_benchmark.py --dry-run

# Run with 8 threads (faster validation)
python run_orcawave_benchmark.py --threads 8

# Execute without comparison
python run_orcawave_benchmark.py --skip-comparison

# Full control
python run_orcawave_benchmark.py \
    --config orcawave_001_ship_raos_rev2.yml \
    --threads 4
```

---

## 🔧 What Each Script Does

### 1. **`run_orcawave_benchmark.py`** ⭐ (NEW - Recommended)

**Purpose:** Simple, focused script for benchmark case

**Features:**
- ✅ Parallel validation (configurable threads)
- ✅ Auto-detects OrcaWave executable
- ✅ Creates batch files for execution
- ✅ Automatic comparison with AQWA results
- ✅ Dry-run mode for validation only

**Workflow:**
```
[1/4] Validate configuration (YAML parsing, mesh files)
[2/4] Parallel validation (4 threads by default)
      - Config validation
      - Mesh quality check
      - Memory estimation
      - Output directory setup
[3/4] Execute OrcaWave
      - Creates .bat file
      - Runs analysis
      - Saves execution log
[4/4] Compare with AQWA
      - Runs run_comparison_peaks.py
      - Generates HTML report
```

**Output:**
- `orcawave_output/` - Analysis results
- `logs/` - Execution logs
- `comparison_results/` - AQWA vs OrcaWave comparison
- `run_orcawave_*.bat` - Generated batch files

---

### 2. **`execute_orcawave_parallel.py`** (Advanced)

**Purpose:** Full-featured parallel execution with extensive validation

**Location:** `specs/modules/orcawave/diffraction-analysis/scripts/`

**Features:**
- ✅ Extensive parallel validation tests
- ✅ Kramers-Kronig causality checks
- ✅ Memory usage estimation
- ✅ Numerical stability analysis
- ✅ Detailed validation reports

**Usage:**
```bash
cd specs/modules/orcawave/diffraction-analysis/scripts

# Dry run with validation
python execute_orcawave_parallel.py \
    --config ../../../../docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.yml \
    --dry-run

# Full execution
python execute_orcawave_parallel.py \
    --config ../../../../docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2.yml
```

---

### 3. **`batch_processor.py`** (Module)

**Purpose:** Process multiple vessels/configurations in parallel

**Location:** `src/digitalmodel/modules/diffraction/batch_processor.py`

**Usage:**
```python
from digitalmodel.diffraction.batch_processor import BatchProcessor, BatchConfiguration

configs = [
    BatchConfiguration(
        vessel_name="SHIP_RAOS",
        source_type="orcawave",
        source_path=Path("orcawave_001_ship_raos_rev2.yml"),
        water_depth=30.0,
        output_dir=Path("results/")
    )
]

processor = BatchProcessor(configs, max_workers=4)
results = processor.run_batch()
```

---

## 🎯 Recommended Workflow

### For Benchmark Comparison:

```bash
# Step 1: Validate configuration
python run_orcawave_benchmark.py --dry-run

# Step 2: Run OrcaWave analysis
python run_orcawave_benchmark.py --threads 4

# Step 3: Review results
# - Check: comparison_results/peak_comparison_*.html
# - Verify: 5% tolerance on peak RAO values
```

### For Multiple Configurations:

```bash
# Use the full parallel script
cd specs/modules/orcawave/diffraction-analysis/scripts
python execute_orcawave_parallel.py --config <your_config.yml>
```

---

## 📊 Parallel Processing Details

### Thread Allocation

**Validation Phase (Parallel):**
- Config parsing: Thread 1
- Mesh validation: Thread 2
- Memory estimation: Thread 3
- Output setup: Thread 4

**Execution Phase (Serial):**
- OrcaWave runs as single process
- License limitation: 1 OrcaWave instance at a time

**Multi-Configuration Batch:**
- Use `max_workers` parameter
- Process multiple vessels in parallel
- Each gets its own OrcaWave instance (if licenses available)

### Performance

**Example:** 13 periods × 111 headings = 1,443 calculations

| Threads | Validation Time | Total Time |
|---------|----------------|------------|
| 1 thread | ~12 seconds | ~25 min |
| 4 threads | ~4 seconds | ~25 min |
| 8 threads | ~3 seconds | ~25 min |

*Note: Main execution time dominated by OrcaWave analysis, not validation*

---

## ⚙️ OrcaWave Auto-Detection

Scripts search for OrcaWave in:
1. `C:\Program Files\Orcina\OrcaWave\OrcaWave.exe`
2. `C:\Program Files (x86)\Orcina\OrcaWave\OrcaWave.exe`
3. `D:\OrcaWave\OrcaWave.exe`

**Custom path:**
```bash
python execute_orcawave_parallel.py \
    --config orcawave_001_ship_raos_rev2.yml \
    --orcawave-exe "C:\Custom\Path\OrcaWave.exe"
```

---

## 📁 Output Structure

```
L01_aqwa_benchmark/
├── orcawave_001_ship_raos_rev2.yml    # Configuration
├── run_orcawave_benchmark.py          # Execution script
├── run_orcawave.bat                   # Batch launcher
├── orcawave_output/                   # OrcaWave results
│   └── *.sim                          # Results file
├── logs/                              # Execution logs
│   └── orcawave_*.log
├── comparison_results/                # AQWA comparison
│   └── peak_comparison_*.html
└── validation/                        # Validation reports
    └── validation_*.json
```

---

## 🐛 Troubleshooting

### Issue: "OrcaWave executable not found"
**Solution:** Verify installation or use `--orcawave-exe` flag

### Issue: "License error"
**Solution:** Check OrcaWave license server connectivity

### Issue: "Mesh file not found"
**Solution:** Verify mesh file path in YAML configuration

### Issue: "Timeout expired"
**Solution:** Increase timeout in script (default: 1 hour)

### Issue: "Validation failed"
**Solution:** Run with `--dry-run` to see detailed validation errors

---

## 🔗 Related Documentation

- **Comparison Guide:** `COMPARISON_SUMMARY.md`
- **Peak Analysis:** `run_comparison_peaks.py`
- **OrcaWave Skill:** `.claude/skills/orcawave-analysis/SKILL.md`
- **Full API:** `src/digitalmodel/modules/diffraction/`

---

## ✅ Success Criteria

After execution, you should have:

1. ✅ `orcawave_output/*.sim` file generated
2. ✅ `logs/orcawave_*.log` with execution details
3. ✅ `comparison_results/peak_comparison_*.html` report
4. ✅ All DOFs within 5% tolerance (in report)

---

**Last Updated:** 2026-01-05
**Version:** 1.0.0
