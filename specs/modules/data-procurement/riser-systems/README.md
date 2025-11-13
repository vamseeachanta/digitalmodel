# Riser Systems Data Procurement

> **Status**: Specification Complete - Ready for Implementation
> **Tier**: Tier 2.1 Priority
> **Created**: 2025-10-23
> **Spec Lines**: 1,094 (main) + 1,275 (technical) = 2,369 total

---

## Overview

Complete specification for riser systems data procurement covering production risers (SCR, flexible, TTR, hybrid), drilling risers (marine drilling riser, BOP, LMRP), buoyancy modules, VIV suppression devices, and riser joints.

---

## Specification Files

### Main Specification
**File**: @specs/modules/data-procurement/riser-systems/spec.md
**Lines**: 1,094
**Sections**:
- Executive Summary (business impact)
- System Architecture (mermaid diagrams)
- Technical Requirements (5 major categories)
- FREE Data Sources (API, DNV, manufacturers, repository)
- User Stories (4 detailed workflows)
- Spec Scope (included/out-of-scope)
- Expected Deliverables
- Performance Requirements
- Integration Points
- Configuration Examples
- Dependencies & Effort Estimation

### Technical Specification
**File**: @specs/modules/data-procurement/riser-systems/sub-specs/technical-spec.md
**Lines**: 1,275
**Sections**:
- System Architecture (detailed mermaid)
- Component Specifications (5 client classes)
- Data Models (Python dataclasses)
- Integration with Metocean Data
- OrcaFlex Export Specification
- Testing Strategy (unit & integration tests)
- Error Handling
- Performance Considerations

---

## Key Features

### Riser Categories Covered

1. **Production Risers**
   - Steel Catenary Riser (SCR)
   - Flexible Riser (Lazy-S, Lazy wave, Steep-S, Steep wave)
   - Top-Tensioned Riser (TTR) for TLPs/Spars
   - Hybrid Risers (steel + flexible)

2. **Drilling Risers**
   - Marine Drilling Riser (19.25", 21" OD)
   - Telescopic joints (50 ft stroke)
   - Flex joints (upper/lower)
   - Ball joints (wellhead)
   - Auxiliary lines (kill, choke, booster, hydraulic)
   - BOP/LMRP subsea equipment

3. **Buoyancy Modules**
   - Syntactic foam (distributed buoyancy)
   - Air-can buoyancy (concentrated)
   - Buoyancy spacing optimization

4. **VIV Suppression**
   - Helical strakes (90% VIV reduction)
   - Fairings (95% VIV reduction, lower drag)
   - VIV screening tools
   - Coverage optimization

5. **Riser Joints**
   - Flex joints (elastomeric)
   - Ball joints (mechanical)
   - Telescopic joints (slip joints)

---

## FREE Data Sources Identified

### 1. API Standards (Public Access)
- **API RP 16Q**: Marine Drilling Riser Systems
- **API RP 2RD**: Risers for Floating Production
- **API 5L**: Line Pipe Specifications
- **API 5CT**: Casing & Tubing Specifications

**Repository Locations**:
- @docs/domains/drilling/API 16 Q_mms_review_report.pdf
- @docs/domains/pipe/API - 5LPipe-Data.xlsx
- @docs/domains/pipe/API-5CT Pipe Data Book.xlsx

### 2. DNV Standards (Public Excerpts)
- **DNV-OS-F201**: Dynamic Risers
- **DNV-RP-F105**: VIV Assessment
- **DNV-RP-F204**: Riser Fatigue
- **DNV-OS-E401**: Buoyancy Units

### 3. Manufacturer Public Catalogs
**Buoyancy Modules**:
- Balmoral: https://www.balmoral.co.uk/ (Syntactic foam)
- Trelleborg: https://www.trelleborg.com/ (Buoyancy solutions)
- Matrix Composites: https://www.matrixengineered.com/ (Catalogs)

**VIV Suppression**:
- VIV Solutions (Shell): https://www.vivsolutions.com/ (Strakes)
- Trelleborg VIV: https://www.trelleborg.com/ (Fairings)

**Flex Joints**:
- Oil States Industries: https://www.oilstatesintl.com/ (Flex joints)
- Cameron (SLB): https://www.slb.com/ (Subsea equipment)

### 4. Repository Data (Existing)
- OrcaFlex drilling riser models: @docs/modules/orcaflex/risers/drilling/
- Hang-off analysis: @docs/modules/orcaflex/risers/drilling/hang-off-analysis/
- Excel automation: @docs/modules/orcaflex/risers/drilling/automation-excel/
- Riser damping: @docs/domains/risers/damping.md
- Installation: @docs/domains/risers/_installation.md
- Drilling risers: @docs/domains/drilling/drilling_risers.md

---

## Technical Architecture

### RiserClient API Structure

```
RiserClient (base)
├── ProductionRiserClient
│   ├── query_scr()
│   ├── query_flexible()
│   ├── query_ttr()
│   └── validate_api_2rd()
├── DrillingRiserClient
│   ├── query_marine_riser()
│   ├── calculate_buoyancy_requirements()
│   ├── analyze_hang_off()
│   ├── analyze_weak_point()
│   ├── validate_api_16q()
│   └── load_from_repository()
├── BuoyancyClient
│   ├── query_syntactic_foam()
│   ├── query_air_can()
│   ├── calculate_spacing()
│   └── validate_dnv_e401()
└── VIVSuppressionClient
    ├── query_helical_strakes()
    ├── query_fairings()
    ├── screen_viv_risk()
    ├── optimize_coverage()
    └── validate_dnv_f105()
```

### Output Formats Supported

1. **OrcaFlex**
   - Line type YAML
   - Vessel models
   - Buoyancy module properties
   - VIV suppression devices

2. **AQWA**
   - Riser properties
   - Hydrodynamic coefficients

3. **SHEAR7**
   - VIV input files
   - Current profiles
   - Mode shapes

4. **Generic**
   - JSON databases
   - YAML configurations
   - CSV exports

---

## Dependencies

### Critical Dependencies (Tier 1)

1. **Metocean Data** - @specs/modules/data-procurement/metocean-data/spec.md
   - Current profiles for VIV analysis
   - Wave spectra for dynamic analysis
   - Wind data for installation analysis

2. **Common Components** - @specs/modules/data-procurement/common-components/spec.md
   - 3-tier caching (L1/L2/L3)
   - Data validation framework
   - Format conversion utilities

### Optional Integrations

1. **Mooring Systems** - @specs/modules/data-procurement/mooring-systems/spec.md
   - Coupled mooring-riser analysis
   - Shared anchor data (for TTRs)

2. **Vessel Systems** - @specs/modules/data-procurement/vessel-systems/spec.md
   - Vessel RAOs for drilling riser analysis
   - Coupled vessel-riser dynamics

---

## Implementation Effort Estimation

| Phase | Component | Effort | Dependencies |
|-------|-----------|--------|--------------|
| **Phase 1** | Pipe database (API 5L/5CT) | 3 days | None |
| **Phase 2** | Property calculators | 2 days | Phase 1 |
| **Phase 3** | Buoyancy module database | 2 days | Phase 1 |
| **Phase 4** | VIV suppression database | 3 days | Phase 1 |
| **Phase 5** | Drilling riser components | 4 days | Phase 1, 2, 3 |
| **Phase 6** | OrcaFlex export | 2 days | Phase 1, 2 |
| **Phase 7** | Standards validation | 2 days | Phase 1, 2 |
| **Phase 8** | Integration & testing | 3 days | All phases |

**Total Estimated Effort**: 21 days (4.2 weeks)

**Resource Requirements**:
- Data Agent: Pipe/component database creation
- Transform Agent: Property calculators, format converters
- Testing Agent: Standards validation, test suite
- Documentation Agent: User guides, API docs

---

## Component Database Sizes

| Component Category | Estimated Count | Data Sources |
|-------------------|-----------------|--------------|
| **Pipe Specifications** | 500+ | API 5L, API 5CT Excel |
| **Flexible Risers** | 200+ | Manufacturer catalogs |
| **Buoyancy Modules** | 100+ | Balmoral, Trelleborg, Matrix |
| **VIV Devices** | 50+ | VIV Solutions, Trelleborg |
| **Flex Joints** | 50+ | Oil States, Cameron |
| **Drilling Riser Joints** | 100+ | API 16Q, repository examples |

**Total**: ~1,000 riser components

---

## User Stories (Summary)

1. **SCR Design** - Marine engineer retrieves steel pipe specs and coatings for SCR modeling in OrcaFlex

2. **Marine Riser Configuration** - Drilling engineer configures 21" riser with BOP, buoyancy modules, and auxiliary lines

3. **VIV Suppression** - VIV specialist selects helical strakes based on screening analysis for high-current zones

4. **Installation Analysis** - Installation engineer retrieves riser specs and metocean data to determine weather window requirements

---

## Integration with OrcaFlex Repository

### Existing OrcaFlex Examples
- Drilling riser automation: @docs/modules/orcaflex/risers/drilling/automation-excel/
  - SemiSub_BC.dat
  - 0113-CAL-0007-03 Drilling riser analysis.xlsx

- Hang-off analysis: @docs/modules/orcaflex/risers/drilling/hang-off-analysis/
  - Hard hang-off models
  - 10B joints configurations
  - LMRP and BOP variations

### Integration Approach
- Load existing OrcaFlex .dat files
- Parse Excel automation spreadsheets
- Extract riser configurations
- Create generic templates from examples

---

## Standards Compliance

### API Standards
- **API RP 16Q**: Marine drilling riser design, selection, operation
- **API RP 2RD**: Production riser design (SCR, TTR, flexible)
- **API 5L**: Line pipe specifications
- **API 5CT**: Casing and tubing specifications

### DNV Standards
- **DNV-OS-F201**: Dynamic risers (all types)
- **DNV-RP-F105**: Free spanning pipelines (VIV)
- **DNV-RP-F204**: Riser fatigue
- **DNV-OS-E401**: Buoyancy units

### Safety Factors
- **Strength**: 1.67 (operating), 1.25 (survival)
- **Fatigue**: 10.0 (design factor)
- **VIV**: Fatigue damage < 0.30

---

## Next Steps

### Prerequisites for Implementation
1. Complete Tier 1 dependencies:
   - Metocean Data (@specs/modules/data-procurement/metocean-data/)
   - Common Components (@specs/modules/data-procurement/common-components/)

2. Set up development environment:
   - Python 3.9+
   - Database (PostgreSQL/SQLite)
   - Redis cache (optional, L2 tier)

### Implementation Sequence
1. Start with Phase 1 (Pipe database) - no dependencies
2. Build property calculators (Phase 2)
3. Add buoyancy modules (Phase 3)
4. Add VIV suppression (Phase 4)
5. Integrate drilling risers (Phase 5)
6. Create OrcaFlex exporters (Phase 6)
7. Implement standards validators (Phase 7)
8. Integration testing (Phase 8)

---

## References

- **Main Spec**: @specs/modules/data-procurement/riser-systems/spec.md
- **Technical Spec**: @specs/modules/data-procurement/riser-systems/sub-specs/technical-spec.md
- **Common Components**: @specs/modules/data-procurement/common-components/spec.md
- **Metocean Data**: @specs/modules/data-procurement/metocean-data/spec.md
- **Future Work Plan**: @specs/modules/data-procurement/FUTURE_WORK_PLAN.md

---

*Riser Systems Specification Complete*
*Ready for Implementation - Phase 1 can begin immediately*
*Total Effort: 21 days (4.2 weeks)*
*Last Updated: 2025-10-23*
