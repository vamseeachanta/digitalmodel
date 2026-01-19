# Plan: Additional digitalmodel Skills Proposal

## Status: Ready for Approval

## Overview

Proposal for additional skills based on comprehensive review of digitalmodel repository mission and 32 modules. Currently 5 skills exist; this plan proposes 5 additional high-value skills.

---

## Current Skills (Already Implemented)

| Skill | Description | Status |
|-------|-------------|--------|
| fatigue-analysis | S-N curves, damage accumulation (DNV, API, BS, ABS) | ✅ Complete |
| mooring-design | CALM/SALM buoy, catenary analysis | ✅ Complete |
| structural-analysis | Stress, buckling, capacity checks | ✅ Complete |
| orcaflex-modeling | OrcaFlex simulation setup & running | ✅ Complete |
| orcaflex-post-processing | OPP summary, range graphs, time series | ✅ Complete |

---

## Module Analysis Summary

### Modules Explored (32 total)
- **With skills:** fatigue_analysis, mooring, orcaflex (covered by 5 existing skills)
- **Without skills:** signal_analysis, time_series, catenary, viv_analysis, aqwa, marine_analysis/hydrodynamics, rao_analysis, pipe_capacity, visualization, and 20+ others

### High-Value Modules for New Skills

| Module | Capabilities | Skill Priority |
|--------|-------------|----------------|
| **signal_analysis** | Rainflow (ASTM E1049-85), FFT, spectral analysis, OrcaFlex batch | HIGH |
| **catenary** | Riser statics, lazy wave, forces, OrcaFlex integration | HIGH |
| **viv_analysis** | VIV assessment, natural frequencies, safety factors | MEDIUM |
| **aqwa** | AQWA software, RAOs, hydrodynamic coefficients | MEDIUM |
| **marine_analysis** | Wave spectra, OCIMF loading, coefficient databases | MEDIUM |

---

## Proposed New Skills (5 Skills)

### 1. **signal-analysis** (HIGH PRIORITY)
**Location:** `digitalmodel/.claude/skills/signal-analysis/`
**Description:** Perform signal processing, rainflow cycle counting, and spectral analysis for fatigue and time series data.

**Trigger Phrases:**
- "analyze fatigue from time series"
- "compute rainflow cycles"
- "FFT spectral analysis"
- "batch signal processing"
- "frequency spectrum analysis"

**Capabilities:**
- Rainflow cycle counting (ASTM E1049-85 compliant)
- FFT and power spectral density (Welch, Periodogram)
- Time series conditioning and filtering
- OrcaFlex signal batch processing
- Damage calculation from stress cycles
- Interactive frequency plots

**Key Classes:**
- `RainflowCounter` - Peak/valley detection, cycle extraction
- `SpectralAnalyzer` - FFT, PSD computation
- `TimeSeriesProcessor` - Signal conditioning
- `GenericTimeSeriesReader` - Auto-format detection

---

### 2. **catenary-riser** (HIGH PRIORITY)
**Location:** `digitalmodel/.claude/skills/catenary-riser/`
**Description:** Analyze catenary and lazy wave riser configurations for static shape, forces, and OrcaFlex model generation.

**Trigger Phrases:**
- "catenary riser analysis"
- "lazy wave catenary"
- "riser static configuration"
- "calculate catenary forces"
- "riser OrcaFlex model"

**Capabilities:**
- Mathematical catenary equation solving
- Lazy wave catenary modeling
- Static shape and load calculation
- Tension, bending, axial force analysis
- Pipe properties (buoyancy, fluid effects)
- OrcaFlex model building and export
- Fatigue loading extraction
- Configuration summary reports

**Key Classes:**
- `CatenaryEquation` - Mathematical solver
- `CatenaryRiser` - Main analysis orchestrator
- `LazyWaveCatenary` - Dynamic catenary
- `OrcaflexModel` - Model export
- `PipeProperties` - Material properties

---

### 3. **viv-analysis** (MEDIUM PRIORITY)
**Location:** `digitalmodel/.claude/skills/viv-analysis/`
**Description:** Assess vortex-induced vibration (VIV) for risers and tubular members with natural frequency and safety factor calculations.

**Trigger Phrases:**
- "VIV analysis for risers"
- "vortex-induced vibration"
- "natural frequency calculation"
- "tubular member VIV"
- "VIV safety factors"

**Capabilities:**
- Natural frequency calculation (eigenvalue-based)
- Vortex shedding frequency analysis
- Safety factor evaluation against VIV criteria
- Tubular member assessment
- Visualization (frequencies, VS frequencies, safety factors)

**Key Classes:**
- `VIVAnalysis` - Main router
- `VIVTubularMembers` - Member assessment
- `VIVAnalysisComponents` - Component-level analysis

---

### 4. **aqwa-analysis** (MEDIUM PRIORITY)
**Location:** `digitalmodel/.claude/skills/aqwa-analysis/`
**Description:** Integrate with AQWA hydrodynamic software for RAO computation, damping analysis, and coefficient extraction.

**Trigger Phrases:**
- "AQWA hydrodynamic analysis"
- "compute RAOs"
- "hydrodynamic coefficients"
- "AQWA file processing"
- "added mass damping"

**Capabilities:**
- RAO (Response Amplitude Operator) computation
- Added mass and damping matrix extraction
- LIS/DAT/MES file parsing
- Viscous damping determination
- AQWA server integration
- Pre/post processing workflows

**Key Classes:**
- `AqwaAnalysis` - Main router
- `AqwaRAOs` - RAO computation
- `AqwaReader` - File parsing
- `AqwaPreProcess` / `AqwaPostProcess`
- `AqwaValidator` - Result validation

---

### 5. **hydrodynamics** (MEDIUM PRIORITY)
**Location:** `digitalmodel/.claude/skills/hydrodynamics/`
**Description:** Manage hydrodynamic coefficients, wave spectra, and environmental loading for vessel response analysis.

**Trigger Phrases:**
- "hydrodynamic coefficients"
- "wave spectra modeling"
- "RAO interpolation"
- "OCIMF environmental loading"
- "vessel response analysis"

**Capabilities:**
- 6×6 added mass and damping matrices
- Wave spectra models (Bretschneider, JONSWAP, etc.)
- OCIMF wind/current loading
- RAO interpolation (frequency-dependent)
- Coefficient database management
- Kramers-Kronig causality validation

**Key Classes:**
- `CoefficientDatabase` - Coefficient storage
- `FrequencyDependentMatrix` - 6×6 matrices
- `WaveSpectra` - Spectrum models
- `OCIMFLoading` - Environmental loading
- `CoefficientsInterpolator` - 2D interpolation

---

## Files to Create

| File Path | Description |
|-----------|-------------|
| `digitalmodel/.claude/skills/signal-analysis/SKILL.md` | Signal processing skill |
| `digitalmodel/.claude/skills/catenary-riser/SKILL.md` | Catenary riser analysis skill |
| `digitalmodel/.claude/skills/viv-analysis/SKILL.md` | VIV assessment skill |
| `digitalmodel/.claude/skills/aqwa-analysis/SKILL.md` | AQWA integration skill |
| `digitalmodel/.claude/skills/hydrodynamics/SKILL.md` | Hydrodynamics skill |
| `digitalmodel/.claude/skills/README.md` | Update with new skills |

---

## Updated Skill Inventory

After implementation, digitalmodel will have **10 skills**:

| # | Skill | Category | Priority |
|---|-------|----------|----------|
| 1 | fatigue-analysis | Structural & Fatigue | ✅ Complete |
| 2 | structural-analysis | Structural & Fatigue | ✅ Complete |
| 3 | mooring-design | Mooring Systems | ✅ Complete |
| 4 | orcaflex-modeling | OrcaFlex | ✅ Complete |
| 5 | orcaflex-post-processing | OrcaFlex | ✅ Complete |
| 6 | signal-analysis | Signal Processing | 🔲 Proposed |
| 7 | catenary-riser | Riser Analysis | 🔲 Proposed |
| 8 | viv-analysis | Dynamics | 🔲 Proposed |
| 9 | aqwa-analysis | Software Integration | 🔲 Proposed |
| 10 | hydrodynamics | Hydrodynamics | 🔲 Proposed |

---

## Implementation Order

1. **signal-analysis** - Foundational for fatigue workflows (connects to existing fatigue-analysis)
2. **catenary-riser** - Core riser analysis capability
3. **viv-analysis** - Complements riser analysis
4. **aqwa-analysis** - Software integration for RAOs
5. **hydrodynamics** - Foundational coefficients for all marine analysis

---

## Benefits

1. **Complete Coverage**: All major marine engineering workflows have skill support
2. **Workflow Integration**: Skills interconnect (signal→fatigue, catenary→orcaflex, aqwa→hydrodynamics)
3. **Domain Expertise**: Encoded patterns from established modules
4. **Discoverability**: Natural language triggers for automatic activation
5. **Consistency**: Standard patterns across all skills
