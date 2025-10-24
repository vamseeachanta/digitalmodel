# Data Reorganization Plan - Digital Model Repository

**Date:** 2025-10-24
**Status:** Proposed
**Workflow:** Domain-Driven + Pipeline-Driven (Hybrid)

## Executive Summary

Reorganize 267 files (209 CSVs) from mixed organization into a clear **domain-driven structure** with **pipeline stages** (raw/processed/results) inside each domain. This enables efficient web data enhancement, engineering analysis, ML training, and HTML report generation.

---

## Current State Analysis

### File Distribution
- **Total Files:** 267
- **CSV Files:** 209
- **JSON Files:** 4
- **Markdown Files:** 8
- **Total Size:** ~236 MB

### Current Structure Issues

```
data/
├── fatigue/                    ✅ WELL ORGANIZED (7 files, 349KB)
├── marine_engineering/         ⚠️  173 CSV files, no pipeline structure
│   ├── hydrodynamic/          (added mass, damping)
│   ├── mooring_components/    (empty)
│   └── raos/                  (empty)
├── modules/                    ⚠️  235MB, mixed vessel/equipment data
│   ├── drilling_rigs/         (3 CSV + 1 MD)
│   ├── equipment/             (scattered across 6 subdirectories)
│   ├── fpso_systems/          (1 CSV)
│   ├── pipelay_vessels/       (1 CSV)
│   ├── riser_systems/         (12 CSV files)
│   ├── subsea_systems/        (equipment/manifold - empty)
│   └── reference_materials/   (industry posters - 4 CSV)
├── ocimf/                      ✅ Domain folder exists
├── ocimf_database.csv          ❌ At root level (should be in /ocimf/)
├── raw/                        ⚠️  Only calm_buoy data
├── processed/                  ⚠️  Only calm_buoy data
└── results/                    ⚠️  Only calm_buoy data
```

### Problems Identified

1. **Inconsistent organization:** Mix of domain folders and pipeline folders
2. **File misplacement:** `ocimf_database.csv` at root instead of `/ocimf/`
3. **Sparse pipeline folders:** `raw/processed/results` only contain calm_buoy
4. **Large unorganized `modules/`:** 235MB without clear categorization
5. **Empty directories:** mooring_components, raos, subsea_systems/equipment/manifold
6. **No data catalog:** Missing README.md to guide users
7. **Inconsistent naming:** Mix of underscores, hyphens, years in filenames

---

## Proposed Structure

### Domain-Driven with Pipeline Stages

```
data/
├── README.md                           # 📋 Data catalog & organization guide
├── METADATA.json                       # Dataset inventory & statistics
│
├── fatigue/                            # ✅ Keep existing structure
│   ├── raw/
│   │   └── fatigue_curves_raw_data.csv
│   ├── processed/
│   │   ├── fatigue_curves_structured.csv
│   │   ├── fatigue_curves_structured.json
│   │   └── fatigue_curves_metadata.json
│   ├── results/
│   │   └── (analysis outputs, plots)
│   ├── references/
│   │   ├── fatigue_curves_references.csv
│   │   └── fatigue_curves_references.json
│   └── README.md
│
├── vessels/                            # 🚢 All vessel data
│   ├── raw/
│   │   ├── fpso_database_2018.csv
│   │   ├── deepwater_drilling_rigs_2014.csv
│   │   ├── jackup_rigs_2015.csv
│   │   ├── pipelay_vessels_2013.csv
│   │   └── drilling_rigs/
│   │       └── fleets.md
│   ├── processed/
│   │   └── (cleaned/standardized vessel data)
│   ├── results/
│   │   └── (analysis outputs, comparisons)
│   └── README.md                       # Vessel database documentation
│
├── mooring/                            # ⚓ Mooring & station-keeping
│   ├── raw/
│   │   ├── ocimf/
│   │   │   ├── ocimf_database.csv      # MOVE from root
│   │   │   ├── ocimf_coefficients_production.csv
│   │   │   └── ocimf_coefficients_sample.csv
│   │   ├── components/                 # mooring components data
│   │   └── calm_buoy/
│   │       └── generic_range/
│   ├── processed/
│   │   └── calm_buoy/
│   │       └── mature_design/
│   ├── results/
│   │   └── calm_buoy/
│   │       └── project_specific/
│   └── README.md
│
├── hydrodynamic/                       # 🌊 Hydrodynamic analysis data
│   ├── raw/
│   │   ├── added_mass/
│   │   │   ├── added_mass_omega_0.1000.csv
│   │   │   ├── ... (83 files)
│   │   │   └── added_mass_omega_3.0000.csv
│   │   ├── damping/
│   │   │   ├── damping_omega_0.1000.csv
│   │   │   ├── ... (90 files)
│   │   │   └── damping_omega_2.3711.csv
│   │   └── raos/
│   ├── processed/
│   │   └── (interpolated/combined datasets)
│   ├── results/
│   │   └── (motion analysis, frequency response)
│   └── README.md
│
├── equipment/                          # ⚙️ Offshore equipment
│   ├── raw/
│   │   ├── anchors/
│   │   ├── buoys/
│   │   ├── fenders/
│   │   │   ├── design_data.md
│   │   │   └── resources.md
│   │   ├── injector_heads/
│   │   ├── manifolds/
│   │   ├── spm/                        # Single Point Mooring
│   │   │   ├── spm-companies-contact.md
│   │   │   └── spm-cost-estimate-conversation.md
│   │   └── x_trees/                    # Subsea Christmas Trees
│   ├── processed/
│   │   └── (equipment specifications database)
│   ├── results/
│   │   └── (selection analyses, cost estimates)
│   └── README.md
│
├── riser_systems/                      # 🔗 Riser & flowline systems
│   ├── raw/
│   │   ├── drilling_risers/
│   │   │   ├── offshore/
│   │   │   │   ├── drilling_riser_model_properties.csv
│   │   │   │   ├── drillrigs.csv
│   │   │   │   ├── drillrigs_bop_and_bop_control_details_sheet1.csv
│   │   │   │   ├── drillrigs_drilling_equipment_and_engineering_company_details_sheet1.csv
│   │   │   │   ├── drillrigs_lifting_equipment_sheet1.csv
│   │   │   │   ├── drillrigs_mooring_and_station_keeping_sheet1.csv
│   │   │   │   ├── drillrigs_mud_pumps_sheet1.csv
│   │   │   │   ├── drillrigs_rig_data_sheet1.csv
│   │   │   │   ├── drillrigs_rig_rating_and_contract_sheet1.csv
│   │   │   │   ├── drillrigs_riser_and_tensioner_data_sheet1.csv
│   │   │   │   ├── drillrigs_vessel_particulars_sheet1.csv
│   │   │   │   ├── drillship.md
│   │   │   │   └── moonpool.md
│   │   ├── production_risers/
│   │   └── export_risers/
│   ├── processed/
│   │   └── (standardized riser databases)
│   ├── results/
│   │   └── (stress analysis, fatigue life)
│   └── README.md
│
├── reference_materials/                # 📚 Industry references
│   ├── raw/
│   │   ├── industry_posters/
│   │   │   ├── 062614drillrig_posterads_raw.csv
│   │   │   ├── 062614drillrig_posterads_process1.csv
│   │   │   ├── 062614drillrig_posterads_process2.csv
│   │   │   └── 062614drillrig_posterads_sheet4.csv
│   │   ├── standards/
│   │   └── specifications/
│   ├── processed/
│   ├── results/
│   └── README.md
│
└── templates/                          # 📄 Data templates for web scraping
    ├── vessel_template.csv
    ├── equipment_template.csv
    └── metadata_template.json
```

---

## File Mapping & Migration Plan

### 1. OCIMF Data (Priority: High)

**Current Location:**
```
data/ocimf_database.csv                          # ❌ ROOT LEVEL
data/ocimf/ocimf_coefficients_production.csv
data/ocimf/ocimf_coefficients_sample.csv
```

**New Location:**
```
data/mooring/raw/ocimf/ocimf_database.csv
data/mooring/raw/ocimf/ocimf_coefficients_production.csv
data/mooring/raw/ocimf/ocimf_coefficients_sample.csv
```

### 2. Vessels Data (Priority: High)

**Current Location:**
```
data/modules/fpso_systems/fpso_database_2018.csv
data/modules/drilling_rigs/deepwater_drilling_rigs_2014.csv
data/modules/drilling_rigs/jackup_rigs_2015.csv
data/modules/drilling_rigs/fleets.md
data/modules/pipelay_vessels/pipelay_vessels_2013.csv
```

**New Location:**
```
data/vessels/raw/fpso_database_2018.csv
data/vessels/raw/deepwater_drilling_rigs_2014.csv
data/vessels/raw/jackup_rigs_2015.csv
data/vessels/raw/drilling_rigs/fleets.md
data/vessels/raw/pipelay_vessels_2013.csv
```

### 3. Hydrodynamic Data (Priority: High)

**Current Location:**
```
data/marine_engineering/hydrodynamic/added_mass_omega_*.csv  (83 files)
data/marine_engineering/hydrodynamic/damping_omega_*.csv     (90 files)
```

**New Location:**
```
data/hydrodynamic/raw/added_mass/added_mass_omega_*.csv
data/hydrodynamic/raw/damping/damping_omega_*.csv
```

### 4. Riser Systems Data (Priority: Medium)

**Current Location:**
```
data/modules/riser_systems/drilling_risers/offshore/*.csv (12 files)
data/modules/riser_systems/drilling_risers/offshore/*.md  (2 files)
```

**New Location:**
```
data/riser_systems/raw/drilling_risers/offshore/*.csv
data/riser_systems/raw/drilling_risers/offshore/*.md
```

### 5. Equipment Data (Priority: Medium)

**Current Location:**
```
data/modules/equipment/fender/*.md
data/modules/equipment/spm/*.md
data/modules/equipment/anchor/       (empty)
data/modules/equipment/buoys/        (empty)
data/modules/equipment/injector_head/(empty)
data/modules/equipment/x_tree/       (empty)
data/modules/subsea_systems/equipment/manifold/ (empty)
```

**New Location:**
```
data/equipment/raw/fenders/*.md
data/equipment/raw/spm/*.md
data/equipment/raw/anchors/
data/equipment/raw/buoys/
data/equipment/raw/injector_heads/
data/equipment/raw/x_trees/
data/equipment/raw/manifolds/
```

### 6. Reference Materials (Priority: Low)

**Current Location:**
```
data/modules/reference_materials/industry_posters/*.csv (4 files)
```

**New Location:**
```
data/reference_materials/raw/industry_posters/*.csv
```

### 7. CALM Buoy Data (Priority: High)

**Current Location:**
```
data/raw/calm_buoy/generic_range/
data/processed/calm_buoy/mature_design/
data/results/calm_buoy/project_specific/
```

**New Location:**
```
data/mooring/raw/calm_buoy/generic_range/
data/mooring/processed/calm_buoy/mature_design/
data/mooring/results/calm_buoy/project_specific/
```

---

## Standardized Naming Conventions

### File Naming Standards

```
Format: {category}_{subcategory}_{year}.{ext}

Examples:
✅ fpso_database_2018.csv
✅ drilling_rigs_deepwater_2014.csv
✅ ocimf_coefficients_production.csv
✅ fatigue_curves_structured.csv
✅ added_mass_omega_0.1000.csv

Avoid:
❌ 062614drillrig_posterads_raw.csv        (date prefix)
❌ drillrigs_bop_and_bop_control_details_sheet1.csv (too long, "sheet1")
```

### Proposed Renaming

```
OLD: 062614drillrig_posterads_raw.csv
NEW: industry_posters_drilling_rigs_raw_2014.csv

OLD: drillrigs_bop_and_bop_control_details_sheet1.csv
NEW: drilling_rigs_bop_control_details.csv

OLD: drillrigs_drilling_equipment_and_engineering_company_details_sheet1.csv
NEW: drilling_rigs_equipment_suppliers.csv
```

---

## Implementation Plan

### Phase 1: Preparation (1 task)
1. ✅ Create this reorganization plan document
2. ⏳ Create data inventory JSON
3. ⏳ Create root-level README.md with data catalog
4. ⏳ Create domain-specific README templates

### Phase 2: Directory Structure (1 task)
1. Create new domain directories with pipeline stages
2. Remove empty directories after migration

### Phase 3: File Migration (7 tasks)
Execute in priority order:
1. **OCIMF data** → `mooring/raw/ocimf/`
2. **Vessels data** → `vessels/raw/`
3. **Hydrodynamic data** → `hydrodynamic/raw/added_mass/` and `hydrodynamic/raw/damping/`
4. **CALM buoy data** → `mooring/raw|processed|results/calm_buoy/`
5. **Riser systems** → `riser_systems/raw/drilling_risers/`
6. **Equipment data** → `equipment/raw/`
7. **Reference materials** → `reference_materials/raw/`

### Phase 4: Documentation (1 task)
1. Create domain-specific README.md files
2. Create METADATA.json with dataset statistics
3. Update fatigue/README.md with new structure
4. Create templates/ directory with data templates

### Phase 5: Validation (1 task)
1. Verify all files migrated correctly
2. Check file count matches inventory
3. Remove old empty directories
4. Run git status to track changes
5. Update any code references to old paths

### Phase 6: Git Commit
1. Stage all changes
2. Commit with detailed message
3. Create git tag: `data-reorganization-v1.0`

---

## Metadata Files to Create

### 1. `/data/README.md`
- Data catalog overview
- Domain descriptions
- Pipeline stage definitions
- Quick start guide
- Data access examples

### 2. `/data/METADATA.json`
```json
{
  "version": "1.0.0",
  "last_updated": "2025-10-24",
  "total_files": 267,
  "total_size_mb": 236,
  "domains": {
    "fatigue": {"files": 7, "size_kb": 349},
    "vessels": {"files": 5, "size_mb": 1.2},
    "mooring": {"files": 6, "size_mb": 0.5},
    "hydrodynamic": {"files": 173, "size_mb": 228},
    "equipment": {"files": 4, "size_kb": 50},
    "riser_systems": {"files": 14, "size_mb": 5},
    "reference_materials": {"files": 4, "size_mb": 1}
  }
}
```

### 3. Domain README Templates
Each domain gets a README.md with:
- Purpose & scope
- Data sources
- File descriptions
- Usage examples
- Pipeline stage descriptions
- Related domains

---

## Benefits of New Structure

### For Web Data Enhancement
✅ Clear separation of raw (web-sourced) vs processed data
✅ Templates directory for standardized web scraping
✅ Easy to add new data sources by domain

### For Engineering Analysis
✅ Domain-specific organization matches engineering workflows
✅ Pipeline stages separate input data from calculations
✅ Easy to find relevant data by discipline

### For Machine Learning
✅ Raw data clearly separated from training data
✅ Processed data ready for model input
✅ Results directory for model outputs

### For HTML Reports
✅ Results directories for HTML report generation
✅ Processed data with relative CSV paths
✅ Domain structure matches report sections

### For Collaboration
✅ Self-documenting structure
✅ README files guide new users
✅ Consistent organization across domains

---

## Risk Mitigation

### Broken References
- **Risk:** Code may reference old file paths
- **Mitigation:** Search codebase for hardcoded paths before migration
- **Action:** Update all path references in Python/config files

### Data Loss
- **Risk:** Files could be lost during migration
- **Mitigation:** Create inventory JSON before migration
- **Action:** Validate file counts after each phase

### Git History
- **Risk:** Git may track files as deleted+added vs moved
- **Mitigation:** Use `git mv` for all file moves
- **Action:** Commit by phase to preserve history

---

## Success Criteria

- ✅ All 267 files migrated to new structure
- ✅ Zero files lost (validated by inventory)
- ✅ All empty directories removed
- ✅ README.md created for data/ and each domain
- ✅ METADATA.json accurately reflects structure
- ✅ File naming standardized per conventions
- ✅ Git history preserved for all files
- ✅ No broken references in codebase

---

## Next Steps

1. **Review & Approve:** User reviews this plan
2. **Execute Phase 1:** Create documentation and inventory
3. **Execute Phases 2-5:** Migrate files by priority
4. **Validate:** Verify structure and file integrity
5. **Commit:** Git commit with detailed changelog
6. **Web Enhancement:** Begin adding updated data from web searches

---

**Document Version:** 1.0
**Last Updated:** 2025-10-24
**Status:** Awaiting Approval
