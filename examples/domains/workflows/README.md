# Marine Engineering Workflow Examples

This directory contains 10+ production-ready workflow examples demonstrating real-world marine engineering scenarios. Each workflow is a complete, executable Python script with professional documentation, comprehensive calculations, and publication-quality outputs.

## Quick Start

```bash
# Activate the uv environment
cd D:/workspace-hub/digitalmodel
source .venv/bin/activate  # On Unix/Mac
# or
.venv\Scripts\activate  # On Windows

# Run any workflow example
python examples/workflows/01_spread_mooring_8point.py
```

## Workflow Catalog

### 1. Spread Mooring Design (8-Point FPSO)
**File:** `01_spread_mooring_8point.py`
- **Application:** FPSO spread mooring in 1500m water depth
- **Features:**
  - 8-point symmetric mooring pattern
  - Environmental loading (wind, current, waves)
  - Chain/wire/polyester line sizing
  - Safety factor verification (API RP 2SK)
  - Fatigue life estimation
  - OrcaFlex model export
- **Standards:** API RP 2SK, DNV-OS-E301
- **Output:** PDF report, Excel summary, OrcaFlex .yml file

### 2. Turret Mooring Analysis (Weathervaning)
**File:** `02_turret_mooring.py`
- **Application:** Internal turret FPSO with weathervaning capability
- **Features:**
  - 360° heading analysis
  - Chain/wire/rope segment optimization
  - Tension exceedance probability
  - Watch circle calculation
  - Yaw stability analysis
- **Standards:** API RP 2SK, OCIMF guidelines
- **Output:** Polar plots, tension statistics, watch circle diagram

### 3. CALM Buoy Design
**File:** `03_calm_buoy.py`
- **Application:** Catenary Anchor Leg Mooring buoy for tanker loading
- **Features:**
  - Single point mooring configuration
  - Swivel stack force analysis
  - Hawser design and selection
  - Tanker offloading scenarios
  - Breakaway force verification
- **Standards:** OCIMF MEG4, API RP 2SK
- **Output:** Buoy motion analysis, hawser loads, safe operating envelope

### 4. Subsea Pipeline Installation
**File:** `04_pipeline_installation.py`
- **Application:** Pipeline installation via J-lay or S-lay vessel
- **Features:**
  - Catenary configuration during installation
  - Top tension monitoring
  - Touchdown point analysis
  - Overbend/sagbend stress calculation
  - Installation weather window analysis
- **Standards:** DNV-ST-F101, API RP 1111
- **Output:** Catenary profiles, stress plots, installation limits

### 5. Jack-Up Installation Analysis
**File:** `05_jackup_installation.py`
- **Application:** Jack-up vessel mooring during offshore installation
- **Features:**
  - 6 or 8-point mooring pattern
  - Environmental limit analysis
  - Preload determination
  - Stability during leg deployment
  - Emergency disconnect scenarios
- **Standards:** SNAME 5-5A, API RP 2A
- **Output:** Mooring plan, stability envelope, emergency procedures

### 6. Floating Wind Turbine Mooring
**File:** `06_floating_wind_turbine.py`
- **Application:** Semi-submersible floating wind turbine platform
- **Features:**
  - 3 or 4-point catenary mooring
  - Wind and wave combined loading
  - Station-keeping analysis
  - Nacelle motion response
  - Mooring fatigue assessment
- **Standards:** DNV-ST-0119, IEC 61400-3-2
- **Output:** Platform motions, mooring tensions, fatigue damage

### 7. Wave Energy Converter Mooring
**File:** `07_wave_energy_converter.py`
- **Application:** Point absorber wave energy converter mooring
- **Features:**
  - High wave loading scenarios
  - Slack line prevention
  - Fatigue considerations
  - Extreme response analysis
  - Station-keeping requirements
- **Standards:** DNV-RP-C205, EMEC guidelines
- **Output:** Tension time histories, fatigue spectra, extreme statistics

### 8. Aquaculture Mooring Design
**File:** `08_aquaculture_mooring.py`
- **Application:** Fish farm mooring grid system
- **Features:**
  - Grid mooring layout
  - Current loading dominant
  - Anchor sizing and selection
  - Net cage integration
  - Regulatory compliance check
- **Standards:** NS 9415, SINTEF recommendations
- **Output:** Grid layout, anchor forces, compliance report

### 9. Single Point Mooring Terminal
**File:** `09_spm_terminal.py`
- **Application:** Offshore SPM for crude oil tanker loading
- **Features:**
  - OCIMF wind force coefficients
  - Breakout force analysis
  - Safe operating limits (STS/SBS)
  - Current and wind envelope
  - Tanker size variations (VLCC, Suezmax)
- **Standards:** OCIMF MEG4, API RP 2SK
- **Output:** Operating envelope, breakout forces, limitation curves

### 10. DP Backup Mooring System
**File:** `10_dp_backup_mooring.py`
- **Application:** Emergency mooring for DP drilling vessel
- **Features:**
  - Quick deployment system
  - DP failure scenarios
  - Redundancy analysis (N-1, N-2)
  - Drive-off analysis
  - Emergency disconnect
- **Standards:** DNV-OS-E301, IMCA guidelines
- **Output:** Emergency procedures, drift trajectories, mooring capacity

## File Structure

Each workflow file follows this structure:

```python
#!/usr/bin/env python3
"""
[Workflow Title]

Description: [Brief description]
Application: [Real-world application]
Standards: [Industry standards referenced]
Author: Marine Engineering Team
Date: 2025-10-03
"""

# 1. IMPORTS
from marine_engineering import ...

# 2. INPUT PARAMETERS
# (Well documented with units)

# 3. ENVIRONMENTAL CONDITIONS
# (Wave, wind, current)

# 4. ANALYSIS
# (Step-by-step calculations)

# 5. RESULTS
# (Summary tables and charts)

# 6. VALIDATION
# (Compare against standards)

# 7. EXPORT
# (Generate reports and OrcaFlex files)

if __name__ == "__main__":
    main()
```

## Supporting Files

- **`utils.py`** - Shared utilities for all workflows
  - Report generation (PDF/Excel)
  - Plotting functions (publication quality)
  - Unit conversions
  - OrcaFlex export helpers
  - Standard compliance checks

- **`data/`** - Sample datasets
  - Environmental criteria
  - Component databases
  - Vessel particulars
  - Wave scatter diagrams

- **`outputs/`** - Expected outputs
  - Reference results for validation
  - Example reports
  - Sample charts

## Code Quality Standards

All workflows adhere to:

1. **PEP 8** compliance
2. **Type hints** for all functions
3. **Comprehensive docstrings** (Google style)
4. **Error handling** with meaningful messages
5. **Logging** for debugging
6. **Unit tests** in `tests/workflows/`

## Output Quality

All workflows generate:

1. **Professional Charts** (300 DPI, publication quality)
2. **Summary Tables** (formatted Excel/CSV)
3. **PDF Reports** (with calculations and results)
4. **OrcaFlex Files** (.yml format)
5. **Data Exports** (JSON/CSV for further analysis)

## Industry Standards Referenced

- **API RP 2SK** - Design and Analysis of Station-Keeping Systems
- **DNV-OS-E301** - Position Mooring
- **DNV-ST-F101** - Submarine Pipeline Systems
- **OCIMF MEG4** - Mooring Equipment Guidelines
- **DNV-ST-0119** - Floating Wind Turbine Structures
- **IEC 61400-3-2** - Wind Turbines - Part 3-2: Floating Offshore
- **NS 9415** - Marine Fish Farms (Norwegian Standard)

## Validation

Each workflow has been validated against:
- Published case studies
- Industry software (OrcaFlex, Sesam)
- Engineering handbooks
- Peer-reviewed publications

## Support and Contact

For questions or issues with these workflows:
- Email: dev@example.com
- GitHub Issues: https://github.com/username/digitalmodel/issues
- Documentation: See individual workflow headers

## License

MIT License - See LICENSE file for details

## Citation

If you use these workflows in academic work, please cite:

```
Marine Engineering Workflow Library
Digital Model Project, 2025
https://github.com/username/digitalmodel
```

---

**Last Updated:** 2025-10-03
**Version:** 1.0.0
**Maintainer:** Marine Engineering Team
