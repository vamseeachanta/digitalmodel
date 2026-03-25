---
standard: DNV-RP-B401:2021
edition: "2021"
structure_type: pipe_clamp_mattress_walking_mitigation
source_type: abstracted_client_calculation
discipline: cathodic_protection
---

# Pipe Clamp Mattress (PCM) Walking Mitigation — DNV-RP-B401:2021 CP Design

## Source
Standard: DNV-RP-B401 (2021)
Structure type: Pipe Clamp Mattress (PCM) — walking mitigation anchor system
Designer: Vendor subcontractor (specialist CP design house)
Project scope: 77 PCM units on deepwater subsea flowlines
Design life: 27 years
Water depth (max): 1910 m

## Scope

77 PCM units; each PCM consists of:
- Fibre-reinforced concrete mattress body
- Galvanised hinge wire ropes (connecting mattress halves)
- Galvanised lift wire ropes (installation and retrieval)
- N16 carbon steel rebar (concrete embedded)
- Carbon steel structural inserts (pipe clamp hardware)

Note: The PCM galvanised wire ropes and rebar are the primary CP-eligible steel surfaces.
Concrete provides partial shielding; only the exposed portions of embedded steel receive CP.

## Environment

| Parameter | Value | Unit |
|-----------|-------|------|
| Maximum water depth | 1910 | m |
| Design life | 27 | years |
| Seawater resistivity | 0.31 | Ω·m |
| Sediment resistivity | 1.00 | Ω·m |
| Min protection potential (seawater) | −0.80 | V vs SSC |
| Min protection potential (sediment) | −0.90 | V vs SSC |

## Design Criteria (Table 2)

| Parameter | Symbol | Value | Unit |
|-----------|--------|-------|------|
| Design life | tf | 27 | years |
| Protective potential (seawater) | Ec° | −0.80 | V vs SSC |
| Seawater resistivity | ρsea | 0.31 | Ω·m |
| Sediment resistivity | ρmud | 1.00 | Ω·m |
| Current density — initial | ii | 0.220 | A/m² |
| Current density — mean | im | 0.110 | A/m² |
| Current density — final | if | 0.170 | A/m² |
| Current density — concrete embedded | i_concrete | 0.001 | A/m² |
| Current density — mud buried | i_mud | 0.020 | A/m² |

## PCM Structural Description and Surface Areas (Table 3)

### Wire Rope Specifications

| Component | Designation | Diameter (mm) | Construction | Function |
|-----------|-------------|---------------|--------------|----------|
| Hinge wire ropes | — | 26 | 6×36 IWRC EIPS galvanised | Connect mattress halves |
| Lift wire ropes | — | 18 | 7×19 IWRC EIPS galvanised | Installation / retrieval |

Wire rope CP area calculation:
- 6×36 IWRC: 60% of outer wires exposed; 10 outer wires per strand
- 7×19 IWRC: 60% of outer wires exposed; 8 outer wires per strand
- N16 rebar (concrete embedded): full circumference exposed within concrete void fraction

### Surface Areas per PCM (Table 3)

| Surface | Bare Area | Area Factor | Design Area (m²) |
|---------|-----------|-------------|-----------------|
| Immersed wire ropes (seawater) | — | +15% contingency | 2.00 |
| Concrete embedded (wire ropes + rebar + concrete) | — | +15% contingency | 12.30 |

Note: The 15% contingency is applied to all surfaces per DNV-RP-B401 clause 7.3 recommended practice.

## Current Demand per PCM (Table 5)

| Phase | Current (A) |
|-------|-------------|
| Initial | 0.452 |
| Mean | 0.232 |
| Final | 0.352 |

Current breakdown by zone:
- Seawater-exposed zone (2.00 m²): ii×2.00 = 0.440 A initial; im×2.00 = 0.220 A mean; if×2.00 = 0.340 A final
- Concrete embedded zone (12.30 m²): i_concrete×12.30 = 0.012 A (constant)

Governing phase for current: initial (0.452 A)
Governing phase for mass: mean (0.232 A)

## Anode Parameters (Table 6)

| Parameter | Value | Unit |
|-----------|-------|------|
| Type | Long flush-mounted | — |
| Alloy | Aluminium (Al alloy) | — |
| Length | 1200 | mm |
| Base width | 88 | mm |
| Top width | 78 | mm |
| Height | 65 | mm |
| Net mass (per anode) | 16.9 | kg |
| Electrochemical capacity — seawater | 2000 | A·h/kg |
| Electrochemical capacity — buried | 1500 | A·h/kg |
| Closed-circuit potential — seawater | −1.05 | V vs SSC |
| Closed-circuit potential — buried | −1.00 | V vs SSC |
| Utilisation factor (long flush-mounted) | u = 0.85 | — |

## Anode Mass and Number Calculation per PCM

### Mass Requirement

Using mean current demand and buried capacity (conservative — all PCMs may be partially buried):

```
W = (Im × tf × 8760) / (ε × u)
W = (0.232 × 27 × 8760) / (1500 × 0.85)
W = 54,800.64 / 1275
W = 43.0 kg  (≈ 43.1 kg per PCM)
```

### Number of Anodes by Mass

```
Nw = W / Wa = 43.1 / 16.9 = 2.55  →  round up to 3 anodes minimum
```

### Number of Anodes by Current

Using long flush-mounted resistance formula:
Ra = ρ / (2 × S)   where S = arithmetic mean of anode length and width

At final condition (ρsea = 0.31 Ω·m), anode consumed to utilisation:
S = (1.200 + 0.088) / 2 = 0.644 m
Ra_final ≈ 0.31 / (2 × 0.644) = 0.240 Ω

Individual anode current at final:
Ia = (Ec° − Ea°) / Ra = (−0.800 − (−1.05)) / 0.240 = 0.250 / 0.240 = 1.04 A

Number by current:
Na = If / Ia = 0.352 / 1.04 = 0.34  →  round up to 1 minimum

Governing: mass requirement — minimum 3 anodes per PCM

### Recommended Design

**4 anodes per PCM** (2 per side — symmetrical placement)

Rationale:
- Mass requirement drives: 3 minimum for life
- 4 anodes selected for symmetrical geometry (2 per side of concrete mattress)
- Provides 30% mass margin above minimum (4 × 16.9 = 67.6 kg vs 43.1 kg required)

## Fleet Summary

| Parameter | Value |
|-----------|-------|
| Total PCM units | 77 |
| Anodes per PCM | 4 |
| Total anodes (fleet) | 308 |
| Total anode mass (fleet) | 308 × 16.9 = 5,205.2 kg |

## CP Protection Philosophy

- PCM anodes protect the galvanised wire ropes and embedded rebar only
- Galvanised coating provides primary corrosion barrier; CP acts as backup (secondary protection)
- No electrical continuity requirement between PCM and parent flowline (PCM is not bonded to pipeline CP)
- Anode placement: 2 anodes per side, flush-mounted on concrete body faces
- Concrete matrix provides incidental shielding; embedded steel current density reduced to 0.001 A/m²

## Gaps Found

- S4 is a vendor subcontractor document; the parent project CP documents (S1–S3) do not cross-reference
  PCM anode counts. The 77-unit fleet count and per-PCM design are taken at face value from the vendor calc.
- The exact exposed wire rope area derivation (strand geometry calculations) is internal to the vendor's
  spreadsheet; the resulting 2.00 m² and 12.30 m² design areas are taken from Table 3 of the source document.
- The vendor uses buried electrochemical capacity (1500 A·h/kg) as conservative basis for all 77 PCMs
  even though some PCMs may remain fully seawater-exposed throughout life. This is noted as a deliberate
  conservatism in the vendor calculation.

## Python cfg dict

```python
cfg = {
    "inputs": {
        "calculation_type": "DNV_RP_B401_2021",
        "standard": "DNV-RP-B401:2021",
        "design_data": {
            "structure_type": "pipe_clamp_mattress",
            "design_life": 27,           # years
            "water_depth_max_m": 1910,
            "unit_count": 77,            # total PCM units
        },
        "environment": {
            "resistivity_seawater_ohm_m": 0.31,
            "resistivity_sediment_ohm_m": 1.00,
            "min_potential_seawater_V": -0.80,   # vs SSC
            "min_potential_sediment_V": -0.90,
        },
        "current_density": {
            "initial_A_m2": 0.220,
            "mean_A_m2": 0.110,
            "final_A_m2": 0.170,
            "mud_A_m2": 0.020,
            "concrete_embedded_A_m2": 0.001,
        },
        "pcm_surface_areas": {
            # Per PCM unit, including 15% contingency
            "immersed_wire_ropes_m2": 2.00,      # seawater-exposed
            "concrete_embedded_m2": 12.30,       # wire ropes + rebar + concrete
        },
        "wire_rope_specs": {
            "hinge": {
                "diameter_mm": 26,
                "construction": "6x36_IWRC_EIPS_galvanised",
                "outer_wire_exposure_fraction": 0.60,
                "outer_wires_per_strand": 10,
            },
            "lift": {
                "diameter_mm": 18,
                "construction": "7x19_IWRC_EIPS_galvanised",
                "outer_wire_exposure_fraction": 0.60,
                "outer_wires_per_strand": 8,
            },
        },
        "current_demand_per_pcm": {
            "initial_A": 0.452,
            "mean_A": 0.232,
            "final_A": 0.352,
        },
        "anode": {
            "type": "long_flush_mounted",
            "alloy": "Al_alloy",
            "dimensions_mm": {
                "length": 1200,
                "base_width": 88,
                "top_width": 78,
                "height": 65,
            },
            "net_mass_kg": 16.9,
            "capacity_seawater_Ah_kg": 2000,
            "capacity_buried_Ah_kg": 1500,
            "ccp_seawater_V": -1.05,
            "ccp_buried_V": -1.00,
            "utilisation": 0.85,
        },
        "design_results": {
            # Per PCM
            "mass_required_kg": 43.1,
            "n_by_mass": 3,              # minimum (round up from 2.55)
            "n_by_current": 1,           # minimum (round up from 0.34)
            "n_recommended": 4,          # 2 per side, symmetric
            # Fleet
            "total_pcm_units": 77,
            "anodes_per_pcm": 4,
            "total_anodes_fleet": 308,
            "total_anode_mass_kg": 5205.2,
        },
    }
}
# Run: CathodicProtection().router(cfg)
```
