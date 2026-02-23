# Mooring System Module Examples

## Overview

Examples for mooring line design, catenary analysis, and mooring system configuration.

## Files

### Python Scripts

**`lazy_wave_example.py`** - Lazy wave riser/mooring configuration
- Lazy wave geometry design
- Buoyancy module sizing
- Catenary analysis
- Touchdown point calculations

## Usage

### Run Lazy Wave Example

```bash
python examples/domains/mooring/lazy_wave_example.py
```

## Features

- ✅ Catenary mooring analysis
- ✅ Lazy wave configuration design
- ✅ Buoyancy module optimization
- ✅ Touchdown zone analysis
- ✅ Tension distribution calculations

## Key Concepts

### Lazy Wave Configuration

```
Surface ──────┐
              │ Top catenary
              ▼
         ┌────●────┐  Buoyancy modules
         │         │
         ▼         ▼  Sagbend
     ────●─────────●──── Seabed

     Touchdown      Anchor
```

**Benefits:**
- Reduced seabed interaction
- Lower fatigue at touchdown
- Decoupled vessel motion
- Improved installation

## Related Modules

- `../calm_buoy/` - CALM buoy mooring
- `../fpso/` - FPSO spread mooring
- `../orcaflex/` - OrcaFlex modeling

## Standards Referenced

- API RP 2SK (Mooring design)
- DNV-RP-F205 (Global performance analysis)
- API RP 17B (Flexible pipe)
