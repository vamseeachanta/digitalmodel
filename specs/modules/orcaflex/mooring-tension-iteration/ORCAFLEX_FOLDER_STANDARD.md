# OrcaFlex Standardized Folder Architecture

## 🏗️ MANDATORY: Standard Folder Structure for OrcaFlex Analysis

**CRITICAL DIRECTIVE**: ALL OrcaFlex analyses across the repository MUST follow this standardized folder structure to ensure repeatability, organization, and consistency.

## Standard Directory Layout

```
<analysis_directory>/
├── .dat/                    # All OrcaFlex data files (.dat)
│   ├── original/           # Original unmodified .dat files
│   ├── modified/           # Modified .dat files during iteration
│   └── archive/            # Archived versions with timestamps
│
├── .sim/                    # All OrcaFlex simulation files (.sim)
│   ├── baseline/           # Initial simulation results
│   ├── iterations/         # Iteration-specific .sim files
│   └── final/              # Converged/final .sim files
│
├── configs/                 # Configuration files
│   ├── analysis.yml        # Analysis configuration
│   ├── post_process.yml    # Post-processing configuration
│   └── targets.csv         # Target values (tensions, forces)
│
├── results/                 # Analysis outputs
│   ├── csv/                # CSV data exports
│   ├── plots/              # Visualization outputs
│   └── reports/            # Analysis reports
│
├── logs/                    # Execution logs
│   ├── iteration_logs/     # Per-iteration logs
│   └── summary.log         # Overall execution summary
│
└── scripts/                 # Analysis scripts
    ├── orchestrator.py      # Main orchestration script
    └── utilities/           # Helper scripts
```

## File Relocation Rules

### 1. Initial Setup
When starting a new OrcaFlex analysis:
```bash
# Create standard structure
mkdir -p .dat/original .dat/modified .dat/archive
mkdir -p .sim/baseline .sim/iterations .sim/final
mkdir -p configs results/csv results/plots results/reports
mkdir -p logs/iteration_logs scripts/utilities
```

### 2. File Placement Rules

| File Type | Source Location | Target Location | When to Move |
|-----------|----------------|-----------------|--------------|
| Original .dat files | Working directory | `.dat/original/` | Before any processing |
| Modified .dat files | Working directory | `.dat/modified/` | After each modification |
| Generated .sim files | Working directory | `.sim/baseline/` | Initial generation |
| Iteration .sim files | Working directory | `.sim/iterations/` | During iterations |
| Final .sim files | Working directory | `.sim/final/` | After convergence |
| Config YML files | Working directory | `configs/` | During setup |
| Target CSV files | Working directory | `configs/` | During setup |
| Output CSV files | Working directory | `results/csv/` | After generation |
| Log files | Working directory | `logs/` | After execution |

### 3. Naming Conventions

#### .dat Files
```
Original: .dat/original/<model_name>_original.dat
Modified: .dat/modified/<model_name>_iter<N>.dat
Archive:  .dat/archive/<model_name>_<timestamp>.dat
```

#### .sim Files
```
Baseline: .sim/baseline/<model_name>_baseline.sim
Iteration: .sim/iterations/<model_name>_iter<N>.sim
Final:    .sim/final/<model_name>_converged.sim
```

## Implementation for Mooring Tension Iteration

### Current Files to Relocate
```bash
# Move existing files to standard structure
cd specs/modules/orcaflex/mooring-tension-iteration/go-by/

# Create standard directories
mkdir -p .dat/original .sim/baseline configs

# Relocate files
mv fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat .dat/original/
mv fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.sim .sim/baseline/
mv dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml configs/
mv dm_ofx_post_fsts_lngc.yml configs/
mv fsts_l015_125km3_pb_target_mooring_pretension.csv configs/
mv _target_fender_force.csv configs/
```

### Updated run_models_to_sim.py Command
```bash
# Process .dat files from standard location
python run_models_to_sim.py \
    dat=true \
    pattern="*.dat" \
    input_directory=".dat/original" \
    output=".sim/baseline"
```

### Updated Orchestrator Paths
```python
# orchestrator.py path configuration
class OrcaFlexPaths:
    """Standardized path management for OrcaFlex files"""
    
    def __init__(self, base_dir="."):
        self.base = Path(base_dir)
        
        # Standard directories
        self.dat_original = self.base / ".dat/original"
        self.dat_modified = self.base / ".dat/modified"
        self.dat_archive = self.base / ".dat/archive"
        
        self.sim_baseline = self.base / ".sim/baseline"
        self.sim_iterations = self.base / ".sim/iterations"
        self.sim_final = self.base / ".sim/final"
        
        self.configs = self.base / "configs"
        self.results_csv = self.base / "results/csv"
        self.results_plots = self.base / "results/plots"
        self.logs = self.base / "logs"
    
    def ensure_structure(self):
        """Create all required directories"""
        for path in [self.dat_original, self.dat_modified, self.dat_archive,
                    self.sim_baseline, self.sim_iterations, self.sim_final,
                    self.configs, self.results_csv, self.results_plots, 
                    self.logs / "iteration_logs"]:
            path.mkdir(parents=True, exist_ok=True)
```

## Benefits of Standardization

### 1. **Repeatability**
- Consistent file locations across all analyses
- Easy to reproduce results
- Clear separation of original vs modified files

### 2. **Organization**
- No mixing of .dat, .sim, and output files
- Clear iteration history
- Archived versions for rollback

### 3. **Automation**
- Scripts know exactly where to find files
- Parallel processing simplified
- Batch operations standardized

### 4. **Version Control**
- .dat/original never modified (git tracked)
- .sim files in dedicated folder (can be gitignored due to size)
- Clear separation of source and generated files

### 5. **Scalability**
- Works for single analysis or hundreds
- Easy to process multiple models in parallel
- Standard paths for all tools and scripts

## Migration Script

```python
#!/usr/bin/env python
"""
Migrate existing OrcaFlex files to standard folder structure
"""
from pathlib import Path
import shutil
import logging

def migrate_to_standard_structure(analysis_dir="."):
    """Migrate existing files to standard folder structure"""
    
    base = Path(analysis_dir)
    
    # Create standard directories
    dirs_to_create = [
        ".dat/original", ".dat/modified", ".dat/archive",
        ".sim/baseline", ".sim/iterations", ".sim/final",
        "configs", "results/csv", "results/plots", "results/reports",
        "logs/iteration_logs", "scripts/utilities"
    ]
    
    for dir_path in dirs_to_create:
        (base / dir_path).mkdir(parents=True, exist_ok=True)
    
    # Migration mappings
    migrations = [
        # (pattern, destination)
        ("*.dat", ".dat/original"),
        ("*.sim", ".sim/baseline"),
        ("*analysis*.yml", "configs"),
        ("*post*.yml", "configs"),
        ("*target*.csv", "configs"),
        ("*fender*.csv", "configs"),
        ("*.log", "logs"),
    ]
    
    moved_files = []
    
    for pattern, dest in migrations:
        for file in base.glob(pattern):
            if not any(str(d) in str(file) for d in ['.dat', '.sim', 'configs', 'results', 'logs']):
                dest_path = base / dest / file.name
                if not dest_path.exists():
                    shutil.move(str(file), str(dest_path))
                    moved_files.append((file.name, dest))
                    logging.info(f"Moved {file.name} to {dest}/")
    
    return moved_files

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    moved = migrate_to_standard_structure()
    print(f"Migration complete. Moved {len(moved)} files.")
    for file, dest in moved:
        print(f"  {file} -> {dest}/")
```

## Enforcement Rules

### CI/CD Checks
```yaml
# .github/workflows/orcaflex_structure_check.yml
- name: Verify OrcaFlex folder structure
  run: |
    # Check that .dat files are in correct folders
    if find . -name "*.dat" -not -path "*/.dat/*" | grep -q .; then
      echo "ERROR: .dat files found outside .dat/ folder structure"
      exit 1
    fi
    
    # Check that .sim files are in correct folders
    if find . -name "*.sim" -not -path "*/.sim/*" | grep -q .; then
      echo "ERROR: .sim files found outside .sim/ folder structure"
      exit 1
    fi
```

### Git Ignore Rules
```gitignore
# Large binary files
.sim/
*.sim

# But track original .dat files
!.dat/original/*.dat

# Ignore modified and archive
.dat/modified/
.dat/archive/

# Ignore results but track structure
results/csv/*.csv
results/plots/*.png
results/reports/*.pdf
!results/*/.gitkeep
```

## Rollout Plan

### Phase 1: Documentation (Current)
- ✅ Create this standard document
- ✅ Define folder structure
- ✅ Create migration script

### Phase 2: Mooring Tension Implementation
- [ ] Migrate go-by files to new structure
- [ ] Update orchestrator.py to use standard paths
- [ ] Update run_models_to_sim.py to use standard paths
- [ ] Test complete workflow with new structure

### Phase 3: Repository-Wide Adoption
- [ ] Update all OrcaFlex modules to use standard
- [ ] Create validation hooks
- [ ] Add to CI/CD pipeline
- [ ] Update documentation

### Phase 4: Enforcement
- [ ] Make structure mandatory for all new analyses
- [ ] Add pre-commit hooks
- [ ] Create linting rules
- [ ] Generate structure automatically in `/create-spec`

## Summary

This standardized folder structure ensures:
1. **Consistency** across all OrcaFlex analyses
2. **Repeatability** of results
3. **Clear separation** of file types
4. **Version control** friendly
5. **Automation** ready
6. **Scalable** architecture

**MANDATORY**: All new OrcaFlex analyses MUST follow this structure. Existing analyses should be migrated using the provided script.