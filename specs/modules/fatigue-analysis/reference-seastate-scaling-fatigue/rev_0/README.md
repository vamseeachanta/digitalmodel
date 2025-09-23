# Reference Seastate Scaling Fatigue Analysis

## Quick Start
```bash
# Run complete verification
python verify_step_by_step.py all

# Run individual steps
python verify_step_by_step.py 1  # Directory structure
python verify_step_by_step.py 2  # Naming convention
python verify_step_by_step.py 3  # File content
python verify_step_by_step.py 4  # Data loading  
python verify_step_by_step.py 5  # Scaling calculations
python verify_step_by_step.py 6  # Output generation
```

## SS Naming Convention

| Sea State | Wind (m/s) | Hs (m) | Wind Scale | Wave Scale |
|-----------|------------|--------|------------|------------|
| SS001     | 15        | 0.75   | 2.25       | 1.50       |
| SS002     | 10        | 0.5    | 1.00       | 1.00       |
| SS003     | 20        | 1.0    | 4.00       | 2.00       |
| SS004     | 25        | 1.5    | 6.25       | 3.00       |

**References**: REF_WIND01 (10 m/s), REF_WAVE01 (0.5 m)
