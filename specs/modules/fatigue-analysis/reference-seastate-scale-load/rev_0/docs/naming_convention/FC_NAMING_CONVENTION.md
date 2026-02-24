# FC Naming Convention Explanation

## What is FC###?

**FC** stands for **Fatigue Condition** - a standardized identifier for different environmental load cases used in fatigue analysis.

### Format: FC### 
- **FC** = Fatigue Condition prefix
- **###** = Three-digit sequential number (001, 002, 003, etc.)

## Origin and Purpose

### Industry Standard Practice
The FC### naming convention is commonly used in offshore engineering for:
1. **Fatigue Analysis**: Identifying specific load cases for fatigue calculations
2. **Load Case Management**: Organizing multiple environmental conditions
3. **Traceability**: Linking results back to specific environmental parameters

### In Our Implementation
The FC numbers were assigned in the LoadScaler class (`load_scaler.py`):

```python
class LoadScaler:
    def __init__(self, data_handler: ProductionDataHandler):
        # Define fatigue conditions with unique IDs
        self.fatigue_conditions = [
            FatigueCondition(
                id=1,  # Becomes FC001
                wind_speed=15,
                wind_dir=0,
                hs=0.75,
                tp=4.0,
                # ... other parameters
            ),
            # Additional conditions: FC002, FC003, FC004...
        ]
```

## Our FC Conditions

| FC ID | Purpose | Wind (m/s) | Hs (m) | Description |
|-------|---------|------------|--------|-------------|
| FC001 | Test Condition | 15 | 0.75 | 1.5x baseline for validation |
| FC002 | Reference | 10 | 0.50 | Baseline reference condition |
| FC003 | Low Load | 5 | 0.25 | 0.5x baseline (calm conditions) |
| FC004 | High Load | 20 | 1.00 | 2x baseline (severe conditions) |

## File Naming Pattern

### Input Reference Files
```
{vessel_config}_mwl_{reference}_Strut{#}.csv
```
Example: `fsts_l015_mwl_wind01_Strut1.csv`

### Output Files (After Scaling)
```
{vessel_config}_FC{###}_Strut{#}.csv
```
Example: `fsts_l015_FC001_Strut1.csv`

This shows:
- Which vessel configuration was used
- Which fatigue condition (FC###) was applied
- Which structural element (Strut#)

## Why This Convention?

1. **Clear Identification**: FC001 immediately tells you this is fatigue condition #1
2. **Sorting**: Files sort naturally in order (FC001, FC002, FC003...)
3. **Scalability**: Can handle up to 999 conditions (FC001-FC999)
4. **Industry Alignment**: Follows common offshore engineering practices
5. **Traceability**: Easy to trace from output back to specific environmental conditions

## Verification Path

To verify which environmental conditions correspond to each FC###:
1. Check `fc_condition_mapping.csv` in `output/verification/intermediate/`
2. Review `FatigueCondition` definitions in `load_scaler.py`
3. Consult `fc_condition_mapping.md` for detailed breakdown

## Example Usage in Code

```python
# Processing a specific fatigue condition
for condition in self.fatigue_conditions:
    fc_id = f"FC{condition.id:03d}"  # Formats as FC001, FC002, etc.
    
    # Generate output filename
    output_file = f"{vessel_config}_{fc_id}_Strut{strut_num}.csv"
    
    # Apply scaling based on condition parameters
    scaled_tension = self.apply_scaling(condition, reference_data)
```

This naming convention ensures consistency across the entire fatigue analysis workflow.