# Subsea Production Systems & Installation Ops — Code Coverage Mapping

**Date:** 2026-04-06
**Sources:** LinkedIn posts from Lloyd's Maritime Institute (LMI)
**Purpose:** Map extracted field development content to digitalmodel modules and identify gaps for future development.

---

## Source 1: Subsea Production Systems

LMI post cataloging all subsea production system components with their API 17 standard references. Provides a complete taxonomy from reservoir extraction to FPSO tie-back.

### Component Inventory

| Component | API 17 Ref | Function |
|-----------|------------|----------|
| Subsea Trees | 17D | Flow control, pressure regulation, well monitoring |
| Manifolds | 17P | Gather production from multiple wells, balance flow |
| Workover Riser | 17G | Well access for maintenance/intervention |
| Control Systems | 17F | Remote valve/sensor/equipment operation |
| Umbilicals | 17E | Power, communication, chemical delivery |
| HIPPS | 17O | High-integrity pressure protection (auto shutdown) |
| Flexible Pipes | 17 Series | Dynamic fluid transport |
| Capping Stack | 17W | Emergency well sealing (blowout) |
| ROVs & Tooling | 17H | Deepwater inspection/repair |
| Connectors & Jumpers | 17R | Link equipment for fluid transfer |
| Structures | 17P | Support/protect installations |
| General Design | 17A | Overall system requirements |

### Operational Workflow
Extraction -> Collection -> Control -> Transport -> Monitoring -> Protection

---

## Source 2: Towed Barges & Strong Currents

LMI video post on river towing operational risk — barge collision with towing tug due to current-induced loss of control.

### Hazards
- Loss of control in strong river currents
- Contact damage (barge-to-tug collision)
- Cascading operational hazards from small miscalculations

### Mitigations
- Monitor current strength and direction
- Maintain clear communication
- Adjust speed and tow configuration
- Anticipate vessel movement

---

## Issue Tracker

| Issue | Component | API / Standard | Priority |
|-------|-----------|----------------|----------|
| [#484](https://github.com/vamseeachanta/digitalmodel/issues/484) | Subsea Trees | API 17D | Medium |
| [#485](https://github.com/vamseeachanta/digitalmodel/issues/485) | Manifold Aggregation | API 17P | Medium |
| [#486](https://github.com/vamseeachanta/digitalmodel/issues/486) | Connectors & Jumpers | API 17R | Medium |
| [#487](https://github.com/vamseeachanta/digitalmodel/issues/487) | Towing / Marine Ops | DNV-ST-N001 | Low |
| [#488](https://github.com/vamseeachanta/digitalmodel/issues/488) | Umbilicals & Control | API 17E, 17F | Low |
| [#489](https://github.com/vamseeachanta/digitalmodel/issues/489) | HIPPS | API 17O | Low |
| [#490](https://github.com/vamseeachanta/digitalmodel/issues/490) | Capping Stack | API 17W | Low |
| [#491](https://github.com/vamseeachanta/digitalmodel/issues/491) | ROV Intervention | API 17H | Low |
| [#493](https://github.com/vamseeachanta/digitalmodel/issues/493) | River/Shallow-Water Currents | -- | Low |
| [#494](https://github.com/vamseeachanta/digitalmodel/issues/494) | Flexible Pipes Enhancement | API 17B, 17J | Low |

All issues grouped under Milestone #1 — [Subsea Production Systems — API 17 Expansion](https://github.com/vamseeachanta/digitalmodel/milestone/1).

---

## Code Coverage Map

### STRONG Coverage
| Area | digitalmodel Module | Standard |
|------|--------------------|----------|
| Hydrodynamics (diffraction/RAO) | hydrodynamics/diffraction/, hydrodynamics/ | DNV-RP-H103 |
| Cathodic Protection | cathodic_protection/ | DNV-RP-B401, F103, API RP 1632, ISO 15589 |
| Asset Integrity / FFS | asset_integrity/ | API 579, BS 7910, ASME B31 |
| Pipeline Design | subsea/pipeline/ | DNV-OS-F101 |
| Riser Analysis | subsea/catenary_riser/, subsea/vertical_riser/ | -- |
| Fatigue | fatigue/, structural/fatigue/ | DNV-RP-C203 |
| Free Span & VIV | subsea/pipeline/free_span/, subsea/viv_analysis/ | DNV-RP-F105 |
| On-Bottom Stability | subsea/on_bottom_stability/ | DNV-RP-F109 |
| Passing Ship / Vessel Interaction | hydrodynamics/passing_ship/ | -- |
| Field Development Schematics | field_development/schematics/ | -- |

### PARTIAL Coverage
| Area | Exists | Gap Missing |
|------|--------|------------|
| Workover Riser | drilling_riser/, vertical_riser/ | API 17G-specific modeling, stackup |
| Flexible Pipes | catenary_riser/ | API 17B/17J-specific |
| Connectors & Jumpers | OrcaFlex skill only | No native Python solver |
| Mooring Design | subsea/mooring_analysis/ | DNV-OS-E301 partial |
| Geotechnical/Anchors | geotechnical/ | DNV-RP-C212 basic |
| Pipe Capacity | structural/pipe_capacity/ | API 2RD, DNV-OS-F101 |

### NO COVERAGE (Gaps) — Now tracked as issues
| Area | API/Std | Issue | Justification |
|------|---------|-------|--------------|
| Subsea Trees | API 17D | #484 | No tree modeling, pressure/flow control |
| Subsea Manifolds | API 17P | #485 | No manifold gathering/balancing |
| HIPPS | API 17O | #489 | No pressure protection system modeling |
| Capping Stack | API 17W | #490 | No blowout containment modeling |
| ROV Tooling | API 17H | #491 | No ROV intervention modeling |
| Control Systems | API 17F | #488 | No subsea electrical/hydraulic control |
| Towing Analysis | DNV-ST-N001 | #487 | No towline catenary/bollard pull solver |
| River Current Profiles | -- | #493 | No shallow-water/river current models |
| Flexible Pipes (enhanced) | API 17B/J | #494 | No unbonded pipe cross-section/fatigue |

---

## Field Development Schematic Architecture

```
Reservoir
   |
   v
[Subsea Tree 17D] -- extraction
   |
   v
[Manifold 17P] -- collection from multiple trees
   |
   v
[Jumpers/Connectors 17R] -- fluid transfer
   |
   v
[Flexible Pipes/Flowlines] -- transport (covered)
   |
   v
[FPSO / Platform] -- surface facility

Parallel systems:
- Umbilicals 17E -- power/comms/chemicals (from FPSO to trees/manifolds)
- Control System 17F -- remote operation (subsea to surface)
- HIPPS 17O -- overpressure protection
- Capping Stack 17W -- emergency well sealing
- ROV 17H -- inspection/maintenance
```

---

## API 17 Standards Quick Reference

```
17A  - System design and classification
17B  - Flexible pipe systems
17C  - Flexible pipe interconnector systems
17D  - Subsea wellhead and tree equipment
17E  - Subsea and surface umbilicals
17F  - Subsea production control systems
17G  - Workover riser systems
17H  - Subsea production systems — ROV interface
17J  - Flexible pipe systems (unbonded)
17K  - Flexible pipe systems — testing
17O  - High integrity pressure protection systems (HIPPS)
17P  - Subsea manifolds and related structures
17R  - Connectors and jumpers
17W  - Capping stack systems
17Z  - Subsea production and distribution systems
```
