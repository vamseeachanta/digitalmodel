# RAO Phase Conventions Reference

> ABOUTME: Reference document for RAO phase angle conventions across marine engineering software.
> ABOUTME: Critical for proper validation and cross-software comparison of hydrodynamic data.

## Overview

Different marine engineering software packages use different phase conventions for RAO (Response Amplitude Operator) data. This document provides a comprehensive reference for understanding and converting between conventions.

## Convention Summary

| Software | Convention | Definition | Sign |
|----------|------------|------------|------|
| **OrcaFlex/OrcaWave** | Phase Lag | Time from wave crest to max positive motion | +phase = motion lags |
| **AQWA** | Phase Lead (ISO 6954) | Angle motion leads wave elevation | +phase = motion leads |
| **WAMIT** | Phase Lead (ISO 6954) | Angle motion leads wave elevation | +phase = motion leads |
| **MOSES** | Phase Lag | Similar to OrcaFlex | +phase = motion lags |
| **HydroD/WADAM** | Phase Lead | Similar to WAMIT | +phase = motion leads |

## Convention Details

### 1. Orcina Convention (OrcaFlex, OrcaWave)

**Definition**: Phase is the **lag** from wave crest until maximum positive excursion.

**Mathematical Form**:
```
motion(t) = A × cos(ωt - φ)
```
where φ is the phase lag in the RAO.

**Wave Direction Convention**:
- 0° = Head seas (waves from bow, approaching vessel)
- 90° = Beam seas from starboard
- 180° = Following seas (waves from stern)
- 270° = Beam seas from port

**Long Period Expectations (T → ∞)**:

| DOF | Head Seas (0°) | Beam Seas (90°) | Following (180°) |
|-----|----------------|-----------------|------------------|
| Surge | Amp=1.0, Phase=-90° | Inactive | Amp=1.0, Phase=90° |
| Sway | Inactive | Amp=1.0, Phase=-90° | Inactive |
| Heave | Amp=1.0, Phase=0° | Amp=1.0, Phase=0° | Amp=1.0, Phase=0° |
| Roll | Inactive | Amp=1.0, Phase=-90° | Inactive |
| Pitch | Amp=1.0, Phase=90° | Inactive | Amp=1.0, Phase=-90° |
| Yaw | Inactive | Inactive | Inactive |

**Interpretation**:
- Phase = 0°: Motion in phase with wave crest
- Phase = 90°: Motion lags wave crest by 90° (peaks at wave zero-crossing going up)
- Phase = -90°: Motion leads wave crest by 90° (peaks at wave zero-crossing going down)
- Phase = 180°: Motion anti-phase with wave crest

### 2. ISO 6954 / AQWA / WAMIT Convention

**Definition**: Phase is the **lead** of motion over wave elevation.

**Mathematical Form**:
```
motion(t) = A × cos(ωt + φ)
```
where φ is the phase lead.

**Relationship to Orcina Convention**:
```
φ_ISO = -φ_Orcina
```

**Wave Direction Convention** (same as Orcina):
- 0° = Head seas (waves from bow)
- 90° = Beam seas from starboard
- 180° = Following seas (waves from stern)
- 270° = Beam seas from port

**Long Period Expectations (T → ∞)**:

| DOF | Head Seas (0°) | Beam Seas (90°) | Following (180°) |
|-----|----------------|-----------------|------------------|
| Surge | Amp=1.0, Phase=90° | Inactive | Amp=1.0, Phase=-90° |
| Sway | Inactive | Amp=1.0, Phase=90° | Inactive |
| Heave | Amp=1.0, Phase=0° | Amp=1.0, Phase=0° | Amp=1.0, Phase=0° |
| Roll | Inactive | Amp=1.0, Phase=90° | Inactive |
| Pitch | Amp=1.0, Phase=-90° | Inactive | Amp=1.0, Phase=90° |
| Yaw | Inactive | Inactive | Inactive |

## Physical Interpretation

### Head Seas (180° wave direction - waves from bow)

At long periods, the vessel follows the wave like a cork on water:

1. **Heave**: Vessel rises with wave crest → Phase = 0° (both conventions)
2. **Surge**:
   - Wave approaching from bow pushes vessel backward initially
   - Max backward displacement when wave crest at bow → Phase = -90° (Orcina) or +90° (ISO)
3. **Pitch**:
   - Bow pitches up as wave approaches
   - Max bow-up when wave trough at midship → Phase = 90° (Orcina) or -90° (ISO)

### Beam Seas (90° wave direction - waves from starboard)

1. **Heave**: Vessel rises with wave crest → Phase = 0° (both conventions)
2. **Sway**:
   - Wave pushes vessel to port
   - Max port displacement when wave crest at starboard → Phase = 90° (Orcina) or -90° (ISO)
3. **Roll**:
   - Vessel rolls to port as wave approaches
   - Max roll to port when wave crest at starboard → Phase = 90° (Orcina) or -90° (ISO)

### Following Seas (0° wave direction - waves from stern)

1. **Heave**: Same as head seas → Phase = 0°
2. **Surge**:
   - Wave from stern pushes vessel forward
   - Max forward when wave crest at stern → Phase = 90° (Orcina) or -90° (ISO)
3. **Pitch**:
   - Bow pitches down as wave approaches from stern
   - Max bow-down when wave trough at midship → Phase = -90° (Orcina) or +90° (ISO)

## Vessel Type Natural Period Ranges

### Ship (Conventional Hull)
- **Heave Tn**: 8-12 seconds
- **Pitch Tn**: 5-8 seconds
- **Roll Tn**: 10-25 seconds

### FPSO
- **Heave Tn**: 9-12 seconds
- **Pitch Tn**: 6-8 seconds
- **Roll Tn**: 10-20 seconds

### Semi-Submersible
- **Heave Tn**: 17-25 seconds (deep draft, low waterplane area)
- **Pitch Tn**: 17-20 seconds
- **Roll Tn**: 17-24 seconds

### SPAR
- **Heave Tn**: 20-30 seconds (very deep draft)
- **Pitch Tn**: 45-60 seconds (small waterplane, large mass moment of inertia)
- **Roll Tn**: 45-60 seconds

### Barge
- **Heave Tn**: 8-12 seconds
- **Pitch Tn**: 5-10 seconds
- **Roll Tn**: 8-15 seconds (shallow draft, wide beam)

## SPAR-Specific Characteristics

SPARs have unique characteristics due to their deep draft and small waterplane area:

1. **Extremely long pitch/roll periods** (45-60s):
   - Small waterplane area → low restoring moment
   - Large mass moment of inertia → high rotational inertia
   - Results in very slow roll/pitch response

2. **Long heave period** (20-30s):
   - Deep draft reduces wave excitation
   - Small waterplane area reduces restoring force
   - Added mass contribution from deep draft

3. **Wave transparency at short periods**:
   - Column diameter small compared to wavelength
   - Less wave diffraction, more Froude-Krylov dominated

4. **VIV susceptibility**:
   - Cylindrical form prone to vortex shedding
   - Strakes typically added to mitigate

## Semi-Submersible Specific Characteristics

Semi-submersibles have distinctive motion characteristics:

1. **Long natural periods** (17-25s all modes):
   - Pontoons provide buoyancy at depth
   - Small waterplane columns reduce restoring forces
   - Designed to avoid wave energy peak (typically 8-15s)

2. **Low wave excitation**:
   - Submerged pontoons below wave zone
   - Column diffraction cancellation effects

3. **Heading dependence**:
   - Response varies significantly with wave direction
   - Diagonal seas often most severe

4. **Coupling effects**:
   - Pitch-surge and roll-sway coupling
   - Multi-body effects if multiple columns

## Conversion Between Conventions

### From Orcina to ISO:
```python
phase_iso = -phase_orcina
```

### From ISO to Orcina:
```python
phase_orcina = -phase_iso
```

### Phase Normalization:
```python
def normalize_phase(phase: float) -> float:
    """Normalize phase to -180 to +180 range."""
    phase = phase % 360
    if phase > 180:
        phase -= 360
    return phase
```

## Detecting Source Convention

When source convention is unknown, these heuristics can help:

1. **Check heave at long periods**:
   - Phase should be ~0° regardless of convention
   - If ~180°, data may have sign error

2. **Check surge at head seas (long period)**:
   - Orcina: Phase ≈ -90°
   - ISO: Phase ≈ +90°

3. **Check file format**:
   - `.yml` from OrcaFlex → Orcina convention
   - `.lis` from AQWA → ISO convention

## Validation Guidelines

### Quality Check Thresholds

| Check Type | Pass | Warning | Fail |
|------------|------|---------|------|
| Amplitude Error | <5% | 5-10% | >10% |
| Phase Error | <10° | 10-20° | >20° |
| Peak Period Error | ±2s | ±5s | >5s |

### Common Issues

1. **180° phase shift**: Wrong convention assumed
2. **90° phase shift**: Sine vs cosine reference
3. **Sign flip in sway/roll**: Coordinate system difference
4. **Large pitch phase error at head seas**: Reference point issue

## References

1. Orcina OrcaFlex Documentation - RAO Phase Definition
2. ANSYS AQWA Theory Manual - Section 4.3 Phase Convention
3. WAMIT User Manual - Output Phase Definitions
4. ISO 6954:2000 - Mechanical vibration (phase conventions)
5. DNV-RP-C205 - Environmental Conditions and Environmental Loads
6. Newman, J.N.: Marine Hydrodynamics, MIT Press

---

**Version**: 1.0.0
**Last Updated**: 2026-01-13
**Author**: AI Agent (Claude)
