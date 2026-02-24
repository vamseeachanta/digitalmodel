# Asset Categories Inventory

**Purpose:** Comprehensive inventory of asset categories for validation framework implementation
**Date:** 2025-01-23
**Status:** Active

---

## Identified Asset Categories

### 1. CALM Buoy ✅ In Progress

**Status:** Validation framework specification completed, implementation pending

**Software:** OrcaFlex
**Input Format:** YAML (modular)

**Data Available:**
```
data/raw/calm_buoy/generic_range/
├── hull_geometry_ranges.csv (9 parameters)
├── metocean_design_ranges.csv (6 conditions)
└── mooring_capacity_ranges.csv (9 components)

data/processed/calm_buoy/mature_design/
├── hydrodynamic_coefficients.csv
├── inspection_and_maintenance.csv
├── operations_matrix.csv
└── structural_components.csv

data/results/calm_buoy/project_specific/
├── environmental_conditions.csv (3 sea states)
├── mooring_line_properties.csv (18 line segments)
├── offloading_configuration.csv
└── project_metadata.csv
```

**Module:** `src/digitalmodel/modules/orcaflex/modular_input_validation/`

**Reference:** `specs/modules/orcaflex/modular-input-file/ENHANCED_VALIDATION_SPEC.md`

---

### 2. AQWA Hydrodynamic Analysis 🆕

**Status:** Extensive module implementation exists, validation needed

**Software:** ANSYS AQWA
**Input Format:** YAML configuration files

**Data Available:**
```
data/marine_engineering/hydrodynamic/
├── added_mass_omega_*.csv (frequency-dependent, 40+ files)
├── damping_omega_*.csv (frequency-dependent, 40+ files)
├── force_omega_*.csv
└── motion_omega_*.csv

data/marine_engineering/raos/
├── rao_*.csv (Response Amplitude Operators)
└── [vessel motion responses]
```

**Module:** `src/digitalmodel/modules/aqwa/`
- `aqwa_analysis.py` - Main analysis
- `aqwa_dat_files.py` - DAT file parsing
- `aqwa_lis_files.py` - LIS file parsing
- `viscous_damping_determination.py` - Damping calculations
- `aqwa_reader.py` - Data extraction

**Input Files:** `specs/modules/aqwa/viscous_damping_determination_input.yml`

**Validation Needs:**
- Level 1: YAML syntax for AQWA config files
- Level 2: AQWA software availability, DAT/LIS file validation
- Level 3: Hydrodynamic coefficient ranges, RAO reasonableness checks

---

### 3. Fatigue Analysis 🆕

**Status:** Module implementation exists, validation needed

**Software:** Custom fatigue analysis tools
**Input Format:** YAML configuration

**Data Available:**
```
data/fatigue/
├── fatigue_curves_raw_data.csv (S-N curves from standards)
├── fatigue_curves_references.csv (Reference data)
└── fatigue_curves_structured.csv (Structured fatigue data)
```

**Module:** `src/digitalmodel/modules/fatigue_analysis/`
- Fatigue curve management
- Stress cycle counting
- Life calculations

**Validation Needs:**
- Level 1: YAML syntax validation
- Level 2: Fatigue calculation engine validation
- Level 3: S-N curve compliance, DFF ranges, SCF limits

---

### 4. Mooring Components 🆕

**Status:** Data available, module implementation exists

**Software:** OrcaFlex, Custom analysis
**Input Format:** YAML, CSV

**Data Available:**
```
data/marine_engineering/mooring_components/
├── chain_properties.csv
├── wire_rope_properties.csv
├── anchor_capacities.csv
└── connector_ratings.csv
```

**Module:** `src/digitalmodel/modules/orcaflex/mooring*/`

**Validation Needs:**
- Level 1: Component data integrity
- Level 2: OrcaFlex component validation
- Level 3: Capacity checks, safety factor compliance, material specifications

---

## Summary Matrix

| Asset Category | Software | Data Available | Module Exists | Validation Status |
|----------------|----------|----------------|---------------|-------------------|
| CALM Buoy | OrcaFlex | ✅ Comprehensive | ✅ In Progress | 🟡 Spec Complete |
| AQWA Hydro | ANSYS AQWA | ✅ Extensive | ✅ Full | ⚪ Not Started |
| Fatigue | Custom | ✅ S-N Curves | ✅ Partial | ⚪ Not Started |
| Mooring Components | OrcaFlex | ✅ Component DBs | ✅ Full | ⚪ Not Started |

## Implementation Priority

**Phase 1 (Current Session):**
1. ✅ CALM Buoy - Complete specification
2. 🔄 AQWA - Create validation module structure
3. 🔄 Fatigue - Create validation module structure

**Phase 2 (Next Session):**
4. Mooring Components
5. Additional categories as identified

## Asset Category Requirements

For each asset category, the validation framework requires:

### Minimum Requirements

1. **Reference Data:**
   - `data/raw/<category>/generic_range/` - Industry standard ranges
   - `data/processed/<category>/mature_design/` - Proven configurations (optional)
   - `data/results/<category>/project_specific/` - Project targets (optional)

2. **Software Integration:**
   - Import statement with graceful fallback
   - Version detection capability
   - API abstraction layer

3. **Validation Module:**
   - `src/digitalmodel/modules/<software>/<category>_validation/`
   - Level 1, 2, 3 validators
   - Data loaders
   - Reporters

4. **Specification:**
   - `specs/modules/<software>/<category>/VALIDATION_SPEC.md`
   - Parameter mappings
   - Validation rules

5. **Directories:**
   - `reports/validation/<category>/`
   - `results/validation/<category>/` (gitignored)

## Next Steps

1. Create validation module structures for AQWA and Fatigue
2. Implement data loaders for each category
3. Create parameter mapping configurations
4. Implement Level 1 validators (syntax)
5. Implement Level 2 validators (software API)
6. Implement Level 3 validators (physical consistency)
7. Create CLI interfaces
8. Write tests

---

**Last Updated:** 2025-01-23
**Maintained By:** Validation Framework Team
