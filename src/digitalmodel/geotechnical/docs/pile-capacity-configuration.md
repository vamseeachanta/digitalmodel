# Pile capacity configuration

Both configuration routes use the sum of the layer thicknesses as embedded
length when `soil_layers` is nonempty. `pile.embedded_length_m` is ignored in
that case: it may be omitted, and a supplied value is neither validated nor
compared with the layer total. Single-layer input requires a finite, positive
`pile.embedded_length_m`.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-11-06 | State which input defines embedment so layered configurations have an explicit length basis. |
| 2 | 2026-11-06 | Omit ignored embedment from layered summaries and reject nonfinite JSON values to keep output unambiguous and readable by strict parsers. |
| 3 | 2026-11-06 | Keep strict JSON local to pile summaries, omit ignored soil, and validate pile design inputs before calculation. |

## Configuration schema

The following fields are relative to either `pile_capacity` for
`PileCapacityWorkflow.router` or `geotechnical.pile_axial_capacity` for
`GeotechnicalWorkflow.router`. The latter also requires
`geotechnical.calculation: pile_axial_capacity`.

| Field | Single-layer input | Layered input |
| --- | --- | --- |
| `pile.diameter_m` | Required, finite and positive, in metres | Required, finite and positive, in metres |
| `pile.embedded_length_m` | Required, finite and positive, in metres | Ignored; may be omitted |
| `soil` | Required; supplies `Su_kpa` and `sigma_v_kpa` | Ignored |
| `soil_layers` | Absent or empty | Nonempty list; each entry supplies finite, positive `thickness_m` in metres, `Su_kpa` and `sigma_v_kpa` |

An empty `soil_layers` list selects the single-layer route. For nonempty
layers, capacity is calculated over every listed thickness; the final layer
supplies the soil properties for tip resistance. No layer is truncated to
`pile.embedded_length_m`.

For example, three layers of 3 m define 9 m of embedment through either
router. Omitting `pile.embedded_length_m`, supplying a nonfinite value there,
or supplying 30 m produces the same layered capacity. Omit that ignored field
in layered configurations to keep the stated geometry unambiguous.

`PileCapacityWorkflow` omits `pile.embedded_length_m` and the ignored `soil`
block from the returned and written layered summary without changing the
supplied input dictionary. Contradictory or nonfinite values in either ignored
field do not affect the layered calculation.

`PileCapacityWorkflow` requires a finite, positive `design.factor_of_safety`
(default 2.0) and, when supplied, finite `design.applied_load_kn`. Numeric
strings are converted before validation. Invalid values raise `ValueError`
naming the field before capacity calculation or output creation. Finite zero
and negative applied loads remain accepted; no load-sign criterion is added.

Strict JSON serialization is requested only by `PileCapacityWorkflow`, before
creating its output file. The shared writer retains its existing nonfinite
serialization for other workflows, including mudmat screening results with
infinite utilization. `GeotechnicalWorkflow` writes capacity summaries without
echoing pile input fields; its returned configuration retains those fields.

## Regression evidence and limits

`TestLayeredConfigurationSchema` in
`tests/geotechnical/test_pile_capacity_composite.py` exercises those three
length cases through both routers and checks the returned capacity and JSON
summary against synthetic hand calculations. Existing geometry regressions
cover invalid diameter and layer thickness. Layered summary checks use strict
JSON decoding and confirm that ignored lengths are absent from the echoed
pile fields and that ignored soil is absent. Additional cases reject nonfinite
design inputs and nonpositive design factors before calculation or output.
The shared-writer regression checks that zero mudmat bearing resistance still
writes a failure result with infinite utilization.

This schema records existing routing behavior. It introduces no length
consistency check, soil-validation rule, empirical range or change to capacity
formulas. A future requirement to reject contradictory layered length inputs
needs a separate compatibility decision; issue pending.
