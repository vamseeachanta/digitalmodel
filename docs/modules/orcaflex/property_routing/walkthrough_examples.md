# Property Routing Walkthrough Examples

Three end-to-end traced examples demonstrating the property routing flow
from engineering specifications to OrcaFlex YAML output.

## Example 1: 6-inch Steel Catenary Riser (Pipeline Route A)

### Engineering Input

| Parameter | Value | Unit |
|-----------|-------|------|
| Outer diameter | 0.1524 | m (6") |
| Wall thickness | 0.0127 | m (0.5") |
| Material grade | X65 | API 5L |
| Concrete coating | 50mm | m |
| Coating density | 2.3 | te/m3 |
| Water depth | 500 | m |
| Riser length | 800 | m |

### Spec YAML (Input)

```yaml
metadata:
  name: 6in_scr
  structure: riser
  operation: in-place
environment:
  water:
    depth: 500
    density: 1.025
  seabed:
    stiffness:
      normal: 100
      shear: 50
    friction_coefficient: 0.5
pipeline:
  material: X65
  dimensions:
    outer_diameter: 0.1524
    wall_thickness: 0.0127
  coatings:
    - name: CWC50
      thickness: 0.05
      material_density: 2.3
  segments:
    - type: free span
      length: 800
      segment_length: 10
```

### Transformation (LineTypeBuilder)

```
OD_actual = 0.1524 + 2 x 0.05 = 0.2524 m
ID = 0.1524 - 2 x 0.0127 = 0.1270 m
A_pipe = pi/4 x (0.1524^2 - 0.1270^2) = 5.55e-3 m^2
A_coating = pi/4 x (0.2524^2 - 0.1524^2) = 3.18e-2 m^2
I = pi/64 x (0.2524^4 - 0.1270^4) = 1.87e-4 m^4

Material X65: E = 207e6 kN/m^2, rho = 7.85 te/m^3, Fy = 450e3 kN/m^2

MassPerUnitLength = 5.55e-3 x 7.85 + 3.18e-2 x 2.3 = 0.1167 te/m
EA = 207e6 x 5.55e-3 = 1.149e6 kN
EI = 207e6 x 1.87e-4 = 3.87e4 kN.m^2
```

### OrcaFlex YAML (Output)

```yaml
LineTypes:
  - Name: free span_0.252m_X65
    Category: Homogeneous pipe
    OD: 0.2524
    ID: 0.1270
    MaterialDensity: 7.85
    E: 207000000
    PoissonRatio: 0.293
    MassPerUnitLength: 0.1167
    EA: 1149000
    EI: 38700
    Cdn: 1.18
    Cdz: 0.008
    Can: 1.0
    AllowableStress: 450000
```

---

## Example 2: 3-Leg Catenary Mooring System (Mooring Route B)

### Engineering Input

| Parameter | Value | Unit |
|-----------|-------|------|
| Number of lines | 3 | - |
| Line composition | chain-wire-chain | - |
| Chain grade | R4 | DNV |
| Chain diameter | 84mm | mm |
| Wire diameter | 96mm | mm |
| Chain top length | 50 | m |
| Wire mid length | 500 | m |
| Chain bottom length | 200 | m |
| Water depth | 500 | m |
| Anchor radius | 1200 | m |
| Pretension | 500 | kN |

### Spec YAML (Input)

```yaml
metadata:
  name: 3leg_mooring
  structure: mooring
  operation: in-place
environment:
  water:
    depth: 500
    density: 1.025
  seabed:
    stiffness: {normal: 100, shear: 50}
    friction_coefficient: 0.5
mooring:
  lines:
    - name: ML1
      segments:
        - type: chain
          diameter: 0.084
          length: 200
          grade: R4
          segment_length: 10
        - type: wire_rope
          diameter: 0.096
          length: 500
          breaking_load: 8000
          segment_length: 10
        - type: chain
          diameter: 0.084
          length: 50
          grade: R4
          segment_length: 5
      anchor:
        type: anchor
        position: [1200, 0, -500]
      fairlead:
        type: fairlead
        position: [10, 0, 0]
        vessel: FPSO
      pretension: 500
      lay_azimuth: 0
    # ML2, ML3 at 120 and 240 deg azimuth...
```

### Transformation (MooringRouter)

```
Chain R4 (84mm):
  MBL = 0.0304 x 84^2 = 214.4 kN
  Mass = 0.0219e-3 x 84^2 = 0.1546 te/m
  EA = 0.854 x 84^2 x 1000 = 6025 kN
  Cd = 2.4, Ca = 1.0

Wire rope (96mm):
  Mass = 0.034e-3 x 96^2 = 0.3133 te/m
  EA = 0.45 x 8000 = 3600 kN (from MBL)
  Cd = 1.2, Ca = 1.0

Deduplication: chain_84mm_R4 (shared by top/bottom segments)
```

### OrcaFlex YAML (Output)

```yaml
LineTypes:
  - Name: chain_84mm_R4
    Category: General
    OD: 0.168          # hydro OD = 2 x bar diameter
    MassPerUnitLength: 0.1546
    EA: 6025000
    EI: [0, ~]
    Cd: [2.4, ~, 0.008]
    Ca: [1.0, ~, 0]
    AllowableTension: 214.4

  - Name: wire_96mm
    Category: General
    OD: 0.096
    MassPerUnitLength: 0.3133
    EA: 3600
    Cd: [1.2, ~, 0.008]
    Ca: [1.0, ~, 0]
    AllowableTension: 8000

Lines:
  - Name: ML1
    EndAConnection: Fixed
    EndAX: 1200
    EndAY: 0
    EndAZ: -500
    EndBConnection: Winch_ML1
    NumberOfSections: 3
    LineType, Length, NumberOfSegments:
      - [chain_84mm_R4, 200, 20]
      - [wire_96mm, 500, 50]
      - [chain_84mm_R4, 50, 10]
    StaticsStep1: Catenary

Winches:
  - Name: Winch_ML1
    Connection: FPSO
    ConnectionX: 10
    ConnectionY: 0
    ConnectionZ: 0
    WinchControl: Specified tension
    Tension: 500
```

---

## Example 3: FPSO from Hull Catalog (Vessel Route C)

### Engineering Input

| Parameter | Value | Unit |
|-----------|-------|------|
| Hull type | FPSO | - |
| LOA | 330 | m |
| Beam | 58 | m |
| Draft | 22.5 | m |
| Displacement | 350,000 | te |
| GMT | 5.0 | m |
| COG | [165, 0, -8] | m |
| Rx (roll) | 18 | m |
| Ry (pitch) | 82.5 | m |
| Rz (yaw) | 82.5 | m |
| RAO source | OrcaWave .owr | - |

### Using VesselRouter.from_hull_catalog()

```python
from digitalmodel.solvers.orcaflex.modular_generator.routers.vessel_router import VesselRouter

catalog_entry = {
    "hull_id": "fst_ship_330m",
    "name": "fpso_330m",
    "length_m": 330,
    "beam_m": 58,
    "draft_m": 22.5,
}

result = VesselRouter.from_hull_catalog(
    catalog_entry=catalog_entry,
    rao_file="data/hull_library/raos/fpso_330m.owr",
    position=[0, 0, -22.5],
    heading=0,
)
```

### OrcaFlex YAML (Output)

```yaml
VesselTypes:
  - Name: fpso_330m_type
    Length: 330
    Draught: 22.5
    DisplacementRAOCalculationFile: data/hull_library/raos/fpso_330m.owr
    WavesReferredToBy: frequency (rad/s)
    RAOOrigin: [0, 0, 0]

Vessels:
  - Name: fpso_330m
    VesselType: fpso_330m_type
    InitialX: 0
    InitialY: 0
    InitialZ: -22.5
    InitialHeading: 0
    IncludedInStatics: None
    PrimaryMotion: None
    SuperimposedMotion: RAOs + harmonics
```

---

## Architecture Summary

```
Route A (Pipeline):   PipelineSpec -> LineTypeBuilder (direct) -> YAML
Route B (Mooring):    MooringSystem -> MooringRouter -> GenericModel -> GenericModelBuilder -> YAML
Route C (Vessel):     HullCatalog + RAO -> VesselRouter -> GenericModel -> GenericModelBuilder -> YAML
```

All three routes converge through GenericModelBuilder for YAML emission, ensuring
consistent property routing rules (TYPED_FIELD_MAP, PRIORITY_KEYS, SKIP_KEYS).
