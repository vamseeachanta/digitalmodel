---
standard: DNV-RP-B401:2021
edition: "2021"
structure_type: walking_mitigation_mattress_pipe_clamp
source_type: deidentified_engineering_example
discipline: cathodic_protection
---

# Walking-Mitigation Mattress and Pipe Clamp — Mixed-Zone B401 Design

This synthetic example shows how one walking-mitigation assembly is composed from
three electrically continuous steel zones and two anode families. Dimensions and
areas are rounded teaching values. They are not a project design or a regression
oracle.

The zones are a seawater-exposed pipe clamp and lifting frame, reinforcing steel
embedded in the concrete mattress, and a buried steel frame below the mudline. The
concrete zone area is the exposed surface area of the steel reinforcement, not the
gross concrete mattress area. DNV-RP-B401 Table 8-3 gives current density on that
reinforcement-steel basis.

## Design basis

| Item | Value | Unit | Source |
|---|---:|---|---|
| Edition | 2021 | - | DNV-RP-B401 |
| Design life | 25 | yr | Example assumption |
| Surface temperature | 5 | °C | Arctic Table 8-1/8-2/8-3 column |
| Representative depth | 1000 | m | >300 m / >100 m rows |
| Seawater electrolyte resistivity | 0.30 | ohm.m | Example input |
| Sediment electrolyte resistivity | 1.00 | ohm.m | Example input |

Table 1. Rounded design inputs and their basis.

Both families use aluminium long flush-mounted anodes and the Table 8-8
utilisation factor of 0.85. The seawater family uses Table 8-6 values of
2000 Ah/kg and -1.05 V. The sediment family uses 1500 Ah/kg and -1.00 V at no
more than 30 °C. Fresh and final flush dimensions are explicit so both Table 8-7
resistance checks use physical rectangular geometry.

## Hand calculation

At 5 °C and 1000 m depth:

- seawater-exposed steel: `2.0 × (0.220, 0.110, 0.170)` =
  `(0.440, 0.220, 0.340)` A for initial, mean and final;
- buried steel: `8.0 × 0.020` = `0.160` A for every phase; and
- concrete reinforcement: `12.0 × 0.0006` = `0.0072` A for every phase,
  using Table 8-3 on reinforcement-steel area.

The sediment-family demand is `0.1672` A for every phase. B401 Eq. 2 gives:

```text
seawater: 0.220 × 25 × 8760 / (2000 × 0.85) = 28.341 kg -> 2 × 17 kg
sediment: 0.1672 × 25 × 8760 / (1500 × 0.85) = 28.719 kg -> 2 × 17 kg
```

For a long flush-mounted anode, Table 8-7 gives `R = rho / (2S)` with
`S = (length + width) / 2`. Each family is checked independently with its own
resistivity and driving voltage. Overall status is the conjunction of the family
checks; overall governing family/case is the largest raw adequacy ratio.

## Runnable engine-adapter input

```python
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

cfg = {
    "basename": "cathodic_protection",
    "inputs": {
        "calculation_type": "DNV_RP_B401_offshore",
        "design_data": {
            "edition": "2021",
            "design_life": 25.0,
            "structure_type": "walking_mitigation_mattress_pipe_clamp",
        },
        "environment": {"seawater_temperature_C": 5.0},
        "structure": {
            "zones": [
                {
                    "zone": "pipe_clamp_steel",
                    "base_zone": "submerged",
                    "depth_m": 1000.0,
                    "area_m2": 2.0,
                    "coating_category": "bare",
                    "anode_family": "seawater_anodes",
                },
                {
                    "zone": "mattress_reinforcement",
                    "base_zone": "concrete_embedded",
                    "depth_m": 1000.0,
                    "reinforcement_area_m2": 12.0,
                    "anode_family": "sediment_anodes",
                },
                {
                    "zone": "buried_frame",
                    "base_zone": "buried",
                    "area_m2": 8.0,
                    "coating_category": "bare",
                    "anode_family": "sediment_anodes",
                },
            ]
        },
        "anode_families": [
            {
                "name": "seawater_anodes",
                "environment": "seawater",
                "material": "aluminium",
                "type": "flush_mounted",
                "individual_anode_mass_kg": 17.0,
                "length_m": 1.20,
                "width_m": 0.09,
                "thickness_m": 0.07,
                "final_length_m": 1.00,
                "final_width_m": 0.06,
                "final_thickness_m": 0.05,
                "electrolyte_resistivity_ohm_m": 0.30,
            },
            {
                "name": "sediment_anodes",
                "environment": "sediments",
                "material": "aluminium",
                "type": "flush_mounted",
                "individual_anode_mass_kg": 17.0,
                "length_m": 1.20,
                "width_m": 0.09,
                "thickness_m": 0.07,
                "final_length_m": 1.00,
                "final_width_m": 0.06,
                "final_thickness_m": 0.05,
                "electrolyte_resistivity_ohm_m": 1.00,
            },
        ],
    },
}

cfg = run_cathodic_protection(cfg)
assert cfg["results"]["current_demand_A"]["mattress_reinforcement"]["area_basis"] == "reinforcement_steel"
assert cfg["results"]["anode_families"]["seawater_anodes"]["capacity_Ah_kg"] == 2000.0
assert cfg["results"]["anode_families"]["sediment_anodes"]["capacity_Ah_kg"] == 1500.0
assert cfg["results"]["anode_families"]["seawater_anodes"]["count_by_mass"] == 2
assert cfg["results"]["anode_families"]["sediment_anodes"]["count_by_mass"] == 2
assert cfg["results"]["status"]["result"] == "PASS"
```

## Interpretation

A concrete-embedded zone uses reinforcement current density, while its assigned
anodes may sit in sediment and therefore use sediment capacity, closed-circuit
potential and electrolyte resistivity. Each family remains independently auditable
in the calculation result and report.
