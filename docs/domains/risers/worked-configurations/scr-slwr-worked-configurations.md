# SCR and SLWR worked configurations — static screening tier

## Summary

Six generic worked configurations are evaluated with the existing open-source static screen. Inputs are assumed for demonstration; no project data are used. All results are provisional under `worked-config-1`. Feasibility requires every applicable screening constraint to pass at every offset.

| Case | Feasible / swept | Rank-1 disposition | Governing criterion evidence |
| --- | --- | --- | --- |
| scr-1000m-8in | 7 / 7 | feasible under stated screening limits | static_von_mises_utilisation: computed 0.318359, limit <= 0.67 1, margin 0.351641 at 50 m |
| scr-1500m-10in | 7 / 7 | feasible under stated screening limits | static_von_mises_utilisation: computed 0.41445, limit <= 0.67 1, margin 0.25555 at 75 m |
| scr-2000m-12in | 7 / 7 | feasible under stated screening limits | static_von_mises_utilisation: computed 0.385021, limit <= 0.67 1, margin 0.284979 at 100 m |
| slwr-1500m-8in | 92 / 108 | feasible under stated screening limits | net_buoyancy: computed 1.5, limit > 1 1, margin 0.5 at -75 m |
| slwr-2000m-10in | 93 / 108 | feasible under stated screening limits | static_von_mises_utilisation: computed 0.383286, limit <= 0.67 1, margin 0.286714 at 100 m |
| slwr-2500m-12in | 94 / 108 | feasible under stated screening limits | static_von_mises_utilisation: computed 0.345875, limit <= 0.67 1, margin 0.324125 at 125 m |

Table 1. Feasibility against the stated limits; a zero feasible count retains the rank-1 violated candidate without tuning limits.

## Scope and limitations

The method is an inextensible static cable screen with frictionless mobile touchdown. No dynamics, fatigue or code compliance is established. The screen's own limitations are quoted verbatim:

> Inextensible, frictionless mobile touchdown; horizontal near/far/cross offsets resolved into a radial static plane. Heave and rotations are unassessed. Static von Mises is sampled thin-wall stress with elastic curvature bending. No dynamic or code-compliance qualification.

The single pressure pair is internal design pressure 20.0e6 Pa and external pressure 0 Pa at surface hang-off. The screen applies it to the whole line; depth-dependent internal and external hydrostatics are not established. Minimum radius uses the screen's touchdown/sag/hog local radii. Utilisation uses sampled thin-wall von Mises stress including elastic curvature bending and pressure end force; it is not a code check.

Rank 1 follows the screen's ascending maximum touchdown/sag excursion-curvature range among feasible candidates; if none is feasible, rank 1 follows the screen's violated-candidate ordering. The curvature-range objective is a stress-range proxy, not fatigue damage. A governing offset is selected by the smallest signed margin divided by the absolute limit (floor 1e-12); ties retain offset order. Each constraint also retains its own controlling offset, because one offset need not control every limit. All four offset rows and input hashes are in [scr-slwr-results.json](scr-slwr-results.json). Floats are serialized to six significant figures; comparator dispositions use unrounded values. Raw workflow runtime is discarded.

SCR comparator: H = wz sinθ/(1 − sinθ), Ttop = H + wz, s = √(z² + 2Hz/w), x = (H/w) asinh(s/(H/w)), Rtd = H/w. θ is the selected input angle from vertical at zero offset; at other offsets it is the exposed top angle, so those checks are conditional on that angle. SLWR comparator: Vtop = w Sbare + w(1 − factor) Sbuoyed, Ttop = hypot(H, Vtop), Rsag = H/w and Rhog = H/|w(1 − factor)|. Bare suspended material is split before and after the exposed buoyed interval; the grounded tail is excluded. SLWR horizontal tension is a supplied comparator input; independent agreement of H and suspended geometry is not established by this equilibrium check.

The stress comparator uses the same thin-wall formulation as the screen, with an independent implementation and independent section selection. Both bending signs are evaluated at hang-off and touchdown, and at sag and hog for SLWR. Hoop stress is (pi − pe) OD/(2t), pressure end force is π(pi ID² − pe OD²)/4, axial stress is (T + Fpressure)/Asteel ± E OD κ/2, and utilisation is √(axial² + hoop² − axial × hoop)/yield. Hang-off uses Ttop and κ = w H/Ttop²; touchdown/sag use H and κ = w/H; hog uses H and κ = abs(w(1 − factor))/H. The maximum over those sections is retained (first named section on an exact tie). H and Ttop are exposed screen inputs to the comparator, so independent global equilibrium is not established by the stress comparison. Agreement establishes the named-section comparison within tolerance; continuous interior extrema are not established. It is not a code check and does not provide thick-wall or FE verification.

The stress criterion is 1e-6 relative before output rounding, based on numerical-noise agreement across 24 base offset rows and 18 sensitivity runs (72 offset rows, including the base rows). The observed maximum absolute difference is 5.26164e-14 %. No sampling allowance is applied. Observed governing critical sections: hang-off. Hang-off is an explicitly sampled endpoint. A sampled screen value lower than the comparator at an unsampled critical section understates that section's utilisation; the sampled screen can miss an exact critical section or govern at an interior sample. Agreement here does not exclude that limitation for other configurations.

Stress comparator not established rows: none at the stated tolerance.

## Inputs and basis

| Input | Unit | Value | Status | Source / basis |
| --- | --- | --- | --- | --- |
| Young's modulus | Pa | 207e9 | cited | [models.py](../../../../src/digitalmodel/subsea/catenary_riser/models.py)::STEEL_API_5L_X65; MPa to Pa |
| Steel density | kg/m³ | 7850 | cited | [models.py](../../../../src/digitalmodel/subsea/catenary_riser/models.py)::STEEL_API_5L_X65 |
| Specified minimum yield strength | Pa | 448e6 | cited | [models.py](../../../../src/digitalmodel/subsea/catenary_riser/models.py)::STEEL_API_5L_X65; nominal API 5L X65, MPa to Pa |
| Content density, 8/10 nominal cases | kg/m³ | 850 oil | assumed | [models.py](../../../../src/digitalmodel/subsea/catenary_riser/models.py)::PRODUCTION_OIL nominal value |
| Content density, 12 nominal cases | kg/m³ | 0 empty gas-lift proxy | assumed | worked-config-1; no operating gas density established |
| Seawater density | kg/m³ | 1025 | assumed | [models.py](../../../../src/digitalmodel/subsea/catenary_riser/models.py)::SEAWATER nominal value |
| Gravity | m/s² | 9.80665 | assumed | worked-config-1 constant acceleration |
| Internal design pressure at hang-off | Pa | 20.0e6 | assumed | worked-config-1; one pressure pair applied throughout |
| External pressure at surface hang-off | Pa | 0 | assumed | worked-config-1; no depth-dependent hydrostatics |
| Grounded bare tail | m | 0.5 × water depth | assumed | worked-config-1; frictionless mobile touchdown |
| Near / zero / far / cross offsets | m | −0.05z / 0 / +0.05z / (0, +0.05z) | assumed | z = water depth; horizontal offsets only |
| SCR angle axis, from vertical | deg | 8, 10, 12, 14, 16, 18, 20 | assumed | worked-config-1 grid |
| SLWR angle axis, from vertical | deg | 6, 8, 10, 12 | assumed | worked-config-1 grid |
| SLWR buoyancy factor axis | 1 | 1.5, 2.0, 2.5 | assumed | buoyed weight = bare weight × (1 − factor) |
| SLWR sag fraction axis | 1 | 0.1, 0.2, 0.3 | assumed | sag elevation / water depth |
| SLWR hog fraction axis | 1 | 0.25, 0.35, 0.5 | assumed | hog elevation / water depth |
| Maximum static utilisation | 1 | 0.67 | assumed | an assumed screening ratio of 0.67 (two thirds rounded to two decimals) on von Mises stress; a project shall replace it with the governing code design factor |
| Minimum bend radius | m | OD/(2 × 0.002) | assumed | 0.2% outer-fibre elastic bending strain; not a code value |
| Maximum top tension | N | 0.6 × yield strength × steel area | assumed | worked-config-1 tension screen |
| Minimum hog clearance | m | 50.0 | assumed | worked-config-1; SCR has no hog bend |
| Numerical shape closure | m | 1e-6 | cited | [static.py](../../../../src/digitalmodel/riser_configuration_screen/static.py)::CLOSURE_M |
| Pipe coating / added equipment | 1 | none included | assumed | bare-pipe displacement and uniform contents |
| Metadata | 1 | provisional=true; criteria_revision=worked-config-1 | assumed | generic assumed values for a public worked configuration; no project data |
| Routing | 1 | basename=riser_configuration_screen | cited | existing python -m digitalmodel router; per-case results folder in YAML |

Table 2. Common inputs and criteria; cited nominal properties do not establish a material certificate.

| Case / riser type | Depth (m) | OD (m) | Wall (mm) | Bare submerged weight (N/m) | Status | Source |
| --- | --- | --- | --- | --- | --- | --- |
| scr-1000m-8in | 1000 | 0.219075 | 18.263 | 726.226 | depth/size assumed; weight derived | [scr-1000m-8in.yml](../../../../examples/workflows/riser-worked-configurations/scr-1000m-8in.yml) |
| scr-1500m-10in | 1500 | 0.27305 | 21.438 | 1062.79 | depth/size assumed; weight derived | [scr-1500m-10in.yml](../../../../examples/workflows/riser-worked-configurations/scr-1500m-10in.yml) |
| scr-2000m-12in | 2000 | 0.32385 | 25.400 | 1005.37 | depth/size assumed; weight derived | [scr-2000m-12in.yml](../../../../examples/workflows/riser-worked-configurations/scr-2000m-12in.yml) |
| slwr-1500m-8in | 1500 | 0.219075 | 18.263 | 726.226 | depth/size assumed; weight derived | [slwr-1500m-8in.yml](../../../../examples/workflows/riser-worked-configurations/slwr-1500m-8in.yml) |
| slwr-2000m-10in | 2000 | 0.27305 | 21.438 | 1062.79 | depth/size assumed; weight derived | [slwr-2000m-10in.yml](../../../../examples/workflows/riser-worked-configurations/slwr-2000m-10in.yml) |
| slwr-2500m-12in | 2500 | 0.32385 | 25.400 | 1005.37 | depth/size assumed; weight derived | [slwr-2500m-12in.yml](../../../../examples/workflows/riser-worked-configurations/slwr-2500m-12in.yml) |

Table 3. Generic nominal dimensions chosen for demonstration (assumed), not a design. w = g × (ρsteel Asteel + ρcontent Abore − ρseawater Adisplaced); A = π D²/4, steel area = displaced area − bore area.

## Results per case

### scr-1000m-8in

Selected grid coordinates: hangoff_angle_deg=20.

![scr-1000m-8in elevation schematic](figures/scr-1000m-8in.svg)

Figure scr-1000m-8in. Zero-offset elevation from screen shape points; the buoyed interval uses a dashed stroke. Uniform x/z scale; the grounded tail is excluded.

| Quantity | Unit | Zero offset | Governing offset |
| --- | --- | --- | --- |
| Top tension | kN | 1103.72 | 1230.33 |
| Horizontal tension | kN | 377.495 | 504.102 |
| Top angle from vertical | deg | 20 | 24.1879 |
| Suspended length | m | 1428.15 | 1545.41 |
| Suspended horizontal span | m | 902.075 | 1069.33 |
| Minimum bend radius | m | 519.803 | 694.139 |
| Hog clearance | m | not applicable: no hog | not applicable: no hog |
| Static von Mises utilisation | 1 | 0.301537 | 0.318359 |

Table scr-1000m-8in-1. Rank-1 static quantities; governing offset 50 m. Horizontal span excludes the grounded tail.

| Criterion | Limit | Zero computed | Governing computed | Envelope computed | Controlling offset | Margin | Status | Basis |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| top_tension_n | <= 3.097e+06 N | 1.10372e+06 | 1.23033e+06 | 1.23033e+06 | 50 m | 1.86667e+06 | pass | assumed: 0.6 * specified minimum yield strength * steel area |
| bend_radius_m | >= 54.7687 m | 519.803 | 694.139 | 390.05 | -50 m | 335.281 | pass | assumed screening strain limit 0.2%; OD/(2*0.002), not a code value |
| hog_clearance_m | >= 50 m | not applicable | not applicable | not applicable | not applicable | not applicable | not_applicable | assumed minimum hog clearance of 50.0 m |
| static_von_mises_utilisation | <= 0.67 1 | 0.301537 | 0.318359 | 0.318359 | 50 m | 0.351641 | pass | an assumed screening ratio of 0.67 (two thirds rounded to two decimals) on von Mises stress; a project shall replace it with the governing code design factor |
| shape_closure | <= 1e-06 m | 5.34328e-11 | 5.07043e-11 | 5.34328e-11 | 0 m | 9.99947e-07 | pass | numerical closure tolerance from riser_configuration_screen/static.py::CLOSURE_M |

Table scr-1000m-8in-2. Each constraint is evaluated over all offsets. Margin is limit minus computed for upper limits, computed minus limit for lower limits; units follow the limit. SCR hog values are not applicable, rather than missing evidence.

| Offset | Quantity (N, m or 1) | Comparator | Screen | Difference (%) | Governing critical section | Relative tolerance (1) | Status | Missing evidence |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| zero | horizontal_tension_n | 377495 | 377495 | 2.27129e-11 | not applicable | 1e-06 | pass | none |
| zero | top_tension_n | 1.10372e+06 | 1.10372e+06 | 7.78408e-12 | not applicable | 1e-06 | pass | none |
| zero | suspended_length_m | 1428.15 | 1428.15 | 7.83307e-12 | not applicable | 1e-06 | pass | none |
| zero | horizontal_span_m | 902.075 | 902.075 | 1.83497e-11 | not applicable | 1e-06 | pass | none |
| zero | touchdown_radius_m | 519.803 | 519.803 | 3.07945e-11 | not applicable | 1e-06 | pass | none |
| governing | horizontal_tension_n | 504102 | 504102 | -3.46404e-14 | not applicable | 1e-06 | pass | none |
| governing | top_tension_n | 1.23033e+06 | 1.23033e+06 | -3.78485e-14 | not applicable | 1e-06 | pass | none |
| governing | suspended_length_m | 1545.41 | 1545.41 | -7.48886e-12 | not applicable | 1e-06 | pass | none |
| governing | horizontal_span_m | 1069.33 | 1069.33 | -1.4948e-11 | not applicable | 1e-06 | pass | none |
| governing | touchdown_radius_m | 694.139 | 694.139 | -2.57464e-11 | not applicable | 1e-06 | pass | none |
| -50 m | static_von_mises_utilisation | 0.289503 | 0.289503 | 3.83492e-14 | hang-off | 1e-06 | pass | none |
| 0 m | static_von_mises_utilisation | 0.301537 | 0.301537 | 0 | hang-off | 1e-06 | pass | none |
| 50 m | static_von_mises_utilisation | 0.318359 | 0.318359 | 0 | hang-off | 1e-06 | pass | none |
| cross (x=0, y=50) m | static_von_mises_utilisation | 0.301789 | 0.301789 | 0 | hang-off | 1e-06 | pass | none |

Table scr-1000m-8in-3. Comparator criterion: absolute relative difference ≤ 1e-6 before serialization. Signed difference = 100 × (screen − comparator)/comparator. Stress is compared at all four offsets; a difference outside tolerance is not established and requires the named evidence.

### scr-1500m-10in

Selected grid coordinates: hangoff_angle_deg=20.

![scr-1500m-10in elevation schematic](figures/scr-1500m-10in.svg)

Figure scr-1500m-10in. Zero-offset elevation from screen shape points; the buoyed interval uses a dashed stroke. Uniform x/z scale; the grounded tail is excluded.

| Quantity | Unit | Zero offset | Governing offset |
| --- | --- | --- | --- |
| Top tension | kN | 2422.84 | 2700.76 |
| Horizontal tension | kN | 828.661 | 1106.58 |
| Top angle from vertical | deg | 20 | 24.1879 |
| Suspended length | m | 2142.22 | 2318.11 |
| Suspended horizontal span | m | 1353.11 | 1604 |
| Minimum bend radius | m | 779.705 | 1041.21 |
| Hog clearance | m | not applicable: no hog | not applicable: no hog |
| Static von Mises utilisation | 1 | 0.385071 | 0.41445 |

Table scr-1500m-10in-1. Rank-1 static quantities; governing offset 75 m. Horizontal span excludes the grounded tail.

| Criterion | Limit | Zero computed | Governing computed | Envelope computed | Controlling offset | Margin | Status | Basis |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| top_tension_n | <= 4.55507e+06 N | 2.42284e+06 | 2.70076e+06 | 2.70076e+06 | 75 m | 1.8543e+06 | pass | assumed: 0.6 * specified minimum yield strength * steel area |
| bend_radius_m | >= 68.2625 m | 779.705 | 1041.21 | 585.075 | -75 m | 516.813 | pass | assumed screening strain limit 0.2%; OD/(2*0.002), not a code value |
| hog_clearance_m | >= 50 m | not applicable | not applicable | not applicable | not applicable | not applicable | not_applicable | assumed minimum hog clearance of 50.0 m |
| static_von_mises_utilisation | <= 0.67 1 | 0.385071 | 0.41445 | 0.41445 | 75 m | 0.25555 | pass | an assumed screening ratio of 0.67 (two thirds rounded to two decimals) on von Mises stress; a project shall replace it with the governing code design factor |
| shape_closure | <= 1e-06 m | 7.95808e-11 | 1.45519e-11 | 7.95808e-11 | 0 m | 9.9992e-07 | pass | numerical closure tolerance from riser_configuration_screen/static.py::CLOSURE_M |

Table scr-1500m-10in-2. Each constraint is evaluated over all offsets. Margin is limit minus computed for upper limits, computed minus limit for lower limits; units follow the limit. SCR hog values are not applicable, rather than missing evidence.

| Offset | Quantity (N, m or 1) | Comparator | Screen | Difference (%) | Governing critical section | Relative tolerance (1) | Status | Missing evidence |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| zero | horizontal_tension_n | 828661 | 828661 | 3.73553e-11 | not applicable | 1e-06 | pass | none |
| zero | top_tension_n | 2.42284e+06 | 2.42284e+06 | 1.27618e-11 | not applicable | 1e-06 | pass | none |
| zero | suspended_length_m | 2142.22 | 2142.22 | 7.79062e-12 | not applicable | 1e-06 | pass | none |
| zero | horizontal_span_m | 1353.11 | 1353.11 | 1.82489e-11 | not applicable | 1e-06 | pass | none |
| zero | touchdown_radius_m | 779.705 | 779.705 | 3.05904e-11 | not applicable | 1e-06 | pass | none |
| governing | horizontal_tension_n | 1.10658e+06 | 1.10658e+06 | 2.10405e-14 | not applicable | 1e-06 | pass | none |
| governing | top_tension_n | 2.70076e+06 | 2.70076e+06 | 1.72418e-14 | not applicable | 1e-06 | pass | none |
| governing | suspended_length_m | 2318.11 | 2318.11 | -8.98467e-12 | not applicable | 1e-06 | pass | none |
| governing | horizontal_span_m | 1604 | 1604 | -1.79177e-11 | not applicable | 1e-06 | pass | none |
| governing | touchdown_radius_m | 1041.21 | 1041.21 | -3.09e-11 | not applicable | 1e-06 | pass | none |
| -75 m | static_von_mises_utilisation | 0.36382 | 0.36382 | -1.52578e-14 | hang-off | 1e-06 | pass | none |
| 0 m | static_von_mises_utilisation | 0.385071 | 0.385071 | 0 | hang-off | 1e-06 | pass | none |
| 75 m | static_von_mises_utilisation | 0.41445 | 0.41445 | 1.33939e-14 | hang-off | 1e-06 | pass | none |
| cross (x=0, y=75) m | static_von_mises_utilisation | 0.385514 | 0.385514 | 1.43993e-14 | hang-off | 1e-06 | pass | none |

Table scr-1500m-10in-3. Comparator criterion: absolute relative difference ≤ 1e-6 before serialization. Signed difference = 100 × (screen − comparator)/comparator. Stress is compared at all four offsets; a difference outside tolerance is not established and requires the named evidence.

### scr-2000m-12in

Selected grid coordinates: hangoff_angle_deg=20.

![scr-2000m-12in elevation schematic](figures/scr-2000m-12in.svg)

Figure scr-2000m-12in. Zero-offset elevation from screen shape points; the buoyed interval uses a dashed stroke. Uniform x/z scale; the grounded tail is excluded.

| Quantity | Unit | Zero offset | Governing offset |
| --- | --- | --- | --- |
| Top tension | kN | 3055.92 | 3406.46 |
| Horizontal tension | kN | 1045.19 | 1395.73 |
| Top angle from vertical | deg | 20 | 24.1879 |
| Suspended length | m | 2856.3 | 3090.81 |
| Suspended horizontal span | m | 1804.15 | 2138.66 |
| Minimum bend radius | m | 1039.61 | 1388.28 |
| Hog clearance | m | not applicable: no hog | not applicable: no hog |
| Static von Mises utilisation | 1 | 0.359935 | 0.385021 |

Table scr-2000m-12in-1. Rank-1 static quantities; governing offset 100 m. Horizontal span excludes the grounded tail.

| Criterion | Limit | Zero computed | Governing computed | Envelope computed | Controlling offset | Margin | Status | Basis |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| top_tension_n | <= 6.40154e+06 N | 3.05592e+06 | 3.40646e+06 | 3.40646e+06 | 100 m | 2.99508e+06 | pass | assumed: 0.6 * specified minimum yield strength * steel area |
| bend_radius_m | >= 80.9625 m | 1039.61 | 1388.28 | 780.1 | -100 m | 699.138 | pass | assumed screening strain limit 0.2%; OD/(2*0.002), not a code value |
| hog_clearance_m | >= 50 m | not applicable | not applicable | not applicable | not applicable | not applicable | not_applicable | assumed minimum hog clearance of 50.0 m |
| static_von_mises_utilisation | <= 0.67 1 | 0.359935 | 0.385021 | 0.385021 | 100 m | 0.284979 | pass | an assumed screening ratio of 0.67 (two thirds rounded to two decimals) on von Mises stress; a project shall replace it with the governing code design factor |
| shape_closure | <= 1e-06 m | 6.18456e-11 | 4.54747e-13 | 6.18456e-11 | 0 m | 9.99938e-07 | pass | numerical closure tolerance from riser_configuration_screen/static.py::CLOSURE_M |

Table scr-2000m-12in-2. Each constraint is evaluated over all offsets. Margin is limit minus computed for upper limits, computed minus limit for lower limits; units follow the limit. SCR hog values are not applicable, rather than missing evidence.

| Offset | Quantity (N, m or 1) | Comparator | Screen | Difference (%) | Governing critical section | Relative tolerance (1) | Status | Missing evidence |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| zero | horizontal_tension_n | 1.04519e+06 | 1.04519e+06 | -9.04426e-12 | not applicable | 1e-06 | pass | none |
| zero | top_tension_n | 3.05592e+06 | 3.05592e+06 | -3.09332e-12 | not applicable | 1e-06 | pass | none |
| zero | suspended_length_m | 2856.3 | 2856.3 | -4.45785e-12 | not applicable | 1e-06 | pass | none |
| zero | horizontal_span_m | 1804.15 | 1804.15 | -1.04603e-11 | not applicable | 1e-06 | pass | none |
| zero | touchdown_radius_m | 1039.61 | 1039.61 | -1.75406e-11 | not applicable | 1e-06 | pass | none |
| governing | horizontal_tension_n | 1.39573e+06 | 1.39573e+06 | 6.67267e-14 | not applicable | 1e-06 | pass | none |
| governing | top_tension_n | 3.40646e+06 | 3.40646e+06 | 5.46798e-14 | not applicable | 1e-06 | pass | none |
| governing | suspended_length_m | 3090.81 | 3090.81 | 6.17941e-13 | not applicable | 1e-06 | pass | none |
| governing | horizontal_span_m | 2138.66 | 2138.66 | 1.25453e-12 | not applicable | 1e-06 | pass | none |
| governing | touchdown_radius_m | 1388.28 | 1388.28 | 2.14553e-12 | not applicable | 1e-06 | pass | none |
| -100 m | static_von_mises_utilisation | 0.341922 | 0.341922 | 0 | hang-off | 1e-06 | pass | none |
| 0 m | static_von_mises_utilisation | 0.359935 | 0.359935 | -1.54225e-14 | hang-off | 1e-06 | pass | none |
| 100 m | static_von_mises_utilisation | 0.385021 | 0.385021 | 2.88354e-14 | hang-off | 1e-06 | pass | none |
| cross (x=0, y=100) m | static_von_mises_utilisation | 0.360312 | 0.360312 | 0 | hang-off | 1e-06 | pass | none |

Table scr-2000m-12in-3. Comparator criterion: absolute relative difference ≤ 1e-6 before serialization. Signed difference = 100 × (screen − comparator)/comparator. Stress is compared at all four offsets; a difference outside tolerance is not established and requires the named evidence.

### slwr-1500m-8in

Selected grid coordinates: hangoff_angle_deg=12, buoyancy_factor=1.5, sag_fraction=0.1, hog_fraction=0.5.

![slwr-1500m-8in elevation schematic](figures/slwr-1500m-8in.svg)

Figure slwr-1500m-8in. Zero-offset elevation from screen shape points; the buoyed interval uses a dashed stroke. Uniform x/z scale; the grounded tail is excluded.

| Quantity | Unit | Zero offset | Governing offset |
| --- | --- | --- | --- |
| Top tension | kN | 1237.75 | 1231.1 |
| Horizontal tension | kN | 257.342 | 238.953 |
| Top angle from vertical | deg | 12 | 11.192 |
| Suspended length | m | 4414.75 | 4410.6 |
| Suspended horizontal span | m | 3078.74 | 2999.59 |
| Minimum bend radius | m | 354.355 | 329.034 |
| Hog clearance | m | 750 | 772.172 |
| Static von Mises utilisation | 1 | 0.315205 | 0.314079 |

Table slwr-1500m-8in-1. Rank-1 static quantities; governing offset -75 m. Horizontal span excludes the grounded tail.

| Criterion | Limit | Zero computed | Governing computed | Envelope computed | Controlling offset | Margin | Status | Basis |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| top_tension_n | <= 3.097e+06 N | 1.23775e+06 | 1.2311e+06 | 1.24552e+06 | 75 m | 1.85147e+06 | pass | assumed: 0.6 * specified minimum yield strength * steel area |
| bend_radius_m | >= 54.7687 m | 354.355 | 329.034 | 329.034 | -75 m | 274.266 | pass | assumed screening strain limit 0.2%; OD/(2*0.002), not a code value |
| hog_clearance_m | >= 50 m | 750 | 772.172 | 727.607 | 75 m | 677.607 | pass | assumed minimum hog clearance of 50.0 m |
| static_von_mises_utilisation | <= 0.67 1 | 0.315205 | 0.314079 | 0.316505 | 75 m | 0.353495 | pass | an assumed screening ratio of 0.67 (two thirds rounded to two decimals) on von Mises stress; a project shall replace it with the governing code design factor |
| shape_closure | <= 1e-06 m | 8.52651e-14 | 9.09495e-13 | 1.75987e-10 | cross (x=0, y=75) m | 9.99824e-07 | pass | numerical closure tolerance from riser_configuration_screen/static.py::CLOSURE_M |
| net_buoyancy | > 1 1 | 1.5 | 1.5 | 1.5 | -75 m | 0.5 | pass | screen domain: buoyancy_factor > 1 for a net-buoyant section |
| hog_above_sag | > 0 1 | 0.4 | 0.4 | 0.4 | -75 m | 0.4 | pass | screen domain: hog_fraction > sag_fraction |

Table slwr-1500m-8in-2. Each constraint is evaluated over all offsets. Margin is limit minus computed for upper limits, computed minus limit for lower limits; units follow the limit. SCR hog values are not applicable, rather than missing evidence.

| Offset | Quantity (N, m or 1) | Comparator | Screen | Difference (%) | Governing critical section | Relative tolerance (1) | Status | Missing evidence |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| zero | top_tension_n | 1.23775e+06 | 1.23775e+06 | -1.88108e-14 | not applicable | 1e-06 | pass | none |
| zero | sag_radius_m | 354.355 | 354.355 | 0 | not applicable | 1e-06 | pass | none |
| zero | hog_radius_m | 708.711 | 708.711 | 0 | not applicable | 1e-06 | pass | none |
| governing | top_tension_n | 1.2311e+06 | 1.2311e+06 | 0 | not applicable | 1e-06 | pass | none |
| governing | sag_radius_m | 329.034 | 329.034 | 0 | not applicable | 1e-06 | pass | none |
| governing | hog_radius_m | 658.069 | 658.069 | 0 | not applicable | 1e-06 | pass | none |
| -75 m | static_von_mises_utilisation | 0.314079 | 0.314079 | 0 | hang-off | 1e-06 | pass | none |
| 0 m | static_von_mises_utilisation | 0.315205 | 0.315205 | 0 | hang-off | 1e-06 | pass | none |
| 75 m | static_von_mises_utilisation | 0.316505 | 0.316505 | 5.26164e-14 | hang-off | 1e-06 | pass | none |
| cross (x=0, y=75) m | static_von_mises_utilisation | 0.315217 | 0.315217 | 0 | hang-off | 1e-06 | pass | none |

Table slwr-1500m-8in-3. Comparator criterion: absolute relative difference ≤ 1e-6 before serialization. Signed difference = 100 × (screen − comparator)/comparator. Stress is compared at all four offsets; a difference outside tolerance is not established and requires the named evidence.

### slwr-2000m-10in

Selected grid coordinates: hangoff_angle_deg=12, buoyancy_factor=1.5, sag_fraction=0.1, hog_fraction=0.5.

![slwr-2000m-10in elevation schematic](figures/slwr-2000m-10in.svg)

Figure slwr-2000m-10in. Zero-offset elevation from screen shape points; the buoyed interval uses a dashed stroke. Uniform x/z scale; the grounded tail is excluded.

| Quantity | Unit | Zero offset | Governing offset |
| --- | --- | --- | --- |
| Top tension | kN | 2415.16 | 2430.33 |
| Horizontal tension | kN | 502.139 | 542.006 |
| Top angle from vertical | deg | 12 | 12.8863 |
| Suspended length | m | 5886.34 | 5892.68 |
| Suspended horizontal span | m | 4104.99 | 4211.33 |
| Minimum bend radius | m | 472.474 | 509.986 |
| Hog clearance | m | 1000 | 970.142 |
| Static von Mises utilisation | 1 | 0.381466 | 0.383286 |

Table slwr-2000m-10in-1. Rank-1 static quantities; governing offset 100 m. Horizontal span excludes the grounded tail.

| Criterion | Limit | Zero computed | Governing computed | Envelope computed | Controlling offset | Margin | Status | Basis |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| top_tension_n | <= 4.55507e+06 N | 2.41516e+06 | 2.43033e+06 | 2.43033e+06 | 100 m | 2.12474e+06 | pass | assumed: 0.6 * specified minimum yield strength * steel area |
| bend_radius_m | >= 68.2625 m | 472.474 | 509.986 | 438.712 | -100 m | 370.45 | pass | assumed screening strain limit 0.2%; OD/(2*0.002), not a code value |
| hog_clearance_m | >= 50 m | 1000 | 970.142 | 970.142 | 100 m | 920.142 | pass | assumed minimum hog clearance of 50.0 m |
| static_von_mises_utilisation | <= 0.67 1 | 0.381466 | 0.383286 | 0.383286 | 100 m | 0.286714 | pass | an assumed screening ratio of 0.67 (two thirds rounded to two decimals) on von Mises stress; a project shall replace it with the governing code design factor |
| shape_closure | <= 1e-06 m | 2.27374e-13 | 3.41061e-13 | 3.07409e-10 | cross (x=0, y=100) m | 9.99693e-07 | pass | numerical closure tolerance from riser_configuration_screen/static.py::CLOSURE_M |
| net_buoyancy | > 1 1 | 1.5 | 1.5 | 1.5 | -100 m | 0.5 | pass | screen domain: buoyancy_factor > 1 for a net-buoyant section |
| hog_above_sag | > 0 1 | 0.4 | 0.4 | 0.4 | -100 m | 0.4 | pass | screen domain: hog_fraction > sag_fraction |

Table slwr-2000m-10in-2. Each constraint is evaluated over all offsets. Margin is limit minus computed for upper limits, computed minus limit for lower limits; units follow the limit. SCR hog values are not applicable, rather than missing evidence.

| Offset | Quantity (N, m or 1) | Comparator | Screen | Difference (%) | Governing critical section | Relative tolerance (1) | Status | Missing evidence |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| zero | top_tension_n | 2.41516e+06 | 2.41516e+06 | -1.92808e-14 | not applicable | 1e-06 | pass | none |
| zero | sag_radius_m | 472.474 | 472.474 | 0 | not applicable | 1e-06 | pass | none |
| zero | hog_radius_m | 944.948 | 944.948 | 0 | not applicable | 1e-06 | pass | none |
| governing | top_tension_n | 2.43033e+06 | 2.43033e+06 | 0 | not applicable | 1e-06 | pass | none |
| governing | sag_radius_m | 509.986 | 509.986 | 0 | not applicable | 1e-06 | pass | none |
| governing | hog_radius_m | 1019.97 | 1019.97 | 0 | not applicable | 1e-06 | pass | none |
| -100 m | static_von_mises_utilisation | 0.379892 | 0.379892 | 0 | hang-off | 1e-06 | pass | none |
| 0 m | static_von_mises_utilisation | 0.381466 | 0.381466 | 0 | hang-off | 1e-06 | pass | none |
| 100 m | static_von_mises_utilisation | 0.383286 | 0.383286 | 0 | hang-off | 1e-06 | pass | none |
| cross (x=0, y=100) m | static_von_mises_utilisation | 0.381482 | 0.381482 | 0 | hang-off | 1e-06 | pass | none |

Table slwr-2000m-10in-3. Comparator criterion: absolute relative difference ≤ 1e-6 before serialization. Signed difference = 100 × (screen − comparator)/comparator. Stress is compared at all four offsets; a difference outside tolerance is not established and requires the named evidence.

### slwr-2500m-12in

Selected grid coordinates: hangoff_angle_deg=12, buoyancy_factor=1.5, sag_fraction=0.1, hog_fraction=0.5.

![slwr-2500m-12in elevation schematic](figures/slwr-2500m-12in.svg)

Figure slwr-2500m-12in. Zero-offset elevation from screen shape points; the buoyed interval uses a dashed stroke. Uniform x/z scale; the grounded tail is excluded.

| Quantity | Unit | Zero offset | Governing offset |
| --- | --- | --- | --- |
| Top tension | kN | 2855.83 | 2873.78 |
| Horizontal tension | kN | 593.761 | 640.903 |
| Top angle from vertical | deg | 12 | 12.8863 |
| Suspended length | m | 7357.92 | 7365.85 |
| Suspended horizontal span | m | 5131.23 | 5264.16 |
| Minimum bend radius | m | 590.592 | 637.482 |
| Hog clearance | m | 1250 | 1212.68 |
| Static von Mises utilisation | 1 | 0.344445 | 0.345875 |

Table slwr-2500m-12in-1. Rank-1 static quantities; governing offset 125 m. Horizontal span excludes the grounded tail.

| Criterion | Limit | Zero computed | Governing computed | Envelope computed | Controlling offset | Margin | Status | Basis |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| top_tension_n | <= 6.40154e+06 N | 2.85583e+06 | 2.87378e+06 | 2.87378e+06 | 125 m | 3.52776e+06 | pass | assumed: 0.6 * specified minimum yield strength * steel area |
| bend_radius_m | >= 80.9625 m | 590.592 | 637.482 | 548.391 | -125 m | 467.428 | pass | assumed screening strain limit 0.2%; OD/(2*0.002), not a code value |
| hog_clearance_m | >= 50 m | 1250 | 1212.68 | 1212.68 | 125 m | 1162.68 | pass | assumed minimum hog clearance of 50.0 m |
| static_von_mises_utilisation | <= 0.67 1 | 0.344445 | 0.345875 | 0.345875 | 125 m | 0.324125 | pass | an assumed screening ratio of 0.67 (two thirds rounded to two decimals) on von Mises stress; a project shall replace it with the governing code design factor |
| shape_closure | <= 1e-06 m | 5.11591e-13 | 9.09495e-13 | 4.52019e-10 | cross (x=0, y=125) m | 9.99548e-07 | pass | numerical closure tolerance from riser_configuration_screen/static.py::CLOSURE_M |
| net_buoyancy | > 1 1 | 1.5 | 1.5 | 1.5 | -125 m | 0.5 | pass | screen domain: buoyancy_factor > 1 for a net-buoyant section |
| hog_above_sag | > 0 1 | 0.4 | 0.4 | 0.4 | -125 m | 0.4 | pass | screen domain: hog_fraction > sag_fraction |

Table slwr-2500m-12in-2. Each constraint is evaluated over all offsets. Margin is limit minus computed for upper limits, computed minus limit for lower limits; units follow the limit. SCR hog values are not applicable, rather than missing evidence.

| Offset | Quantity (N, m or 1) | Comparator | Screen | Difference (%) | Governing critical section | Relative tolerance (1) | Status | Missing evidence |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| zero | top_tension_n | 2.85583e+06 | 2.85583e+06 | 0 | not applicable | 1e-06 | pass | none |
| zero | sag_radius_m | 590.592 | 590.592 | 0 | not applicable | 1e-06 | pass | none |
| zero | hog_radius_m | 1181.18 | 1181.18 | 0 | not applicable | 1e-06 | pass | none |
| governing | top_tension_n | 2.87378e+06 | 2.87378e+06 | -4.86114e-14 | not applicable | 1e-06 | pass | none |
| governing | sag_radius_m | 637.482 | 637.482 | 0 | not applicable | 1e-06 | pass | none |
| governing | hog_radius_m | 1274.96 | 1274.96 | 0 | not applicable | 1e-06 | pass | none |
| -125 m | static_von_mises_utilisation | 0.343208 | 0.343208 | 0 | hang-off | 1e-06 | pass | none |
| 0 m | static_von_mises_utilisation | 0.344445 | 0.344445 | 0 | hang-off | 1e-06 | pass | none |
| 125 m | static_von_mises_utilisation | 0.345875 | 0.345875 | 0 | hang-off | 1e-06 | pass | none |
| cross (x=0, y=125) m | static_von_mises_utilisation | 0.344458 | 0.344458 | 0 | hang-off | 1e-06 | pass | none |

Table slwr-2500m-12in-3. Comparator criterion: absolute relative difference ≤ 1e-6 before serialization. Signed difference = 100 × (screen − comparator)/comparator. Stress is compared at all four offsets; a difference outside tolerance is not established and requires the named evidence.

## Size sensitivity

Generic nominal wall steps (assumed), not a vendor catalogue. Each wall step reruns the unchanged grid with recomputed submerged weight and 0.6 Sy Asteel tension limit. Each row uses the rank-1 candidate; a violated row remains infeasible. The reported metrics are the maximum top tension and utilisation and minimum bend radius over all four offsets. Percentages use the six-significant-figure metric values serialized in JSON, relative to the base wall. Candidate reselection can change geometry as well as loading.

### scr-1000m-8in

| Wall step | Wall (mm) | Weight (N/m) | Tension limit (kN) | Top tension maximum over offsets (kN) | ΔT (%) | Utilisation maximum over offsets (1) | ΔU (%) | Radius minimum over offsets (m) | ΔR (%) | Disposition |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| thinner | 15.875 | 630.985 | 2724.06 | 1068.98 | -13.1144 | 0.342673 | 7.63729 | 390.05 | 0 | feasible |
| base | 18.263 | 726.226 | 3097 | 1230.33 | 0 | 0.318359 | 0 | 390.05 | 0 | feasible |
| thicker | 22.225 | 878.819 | 3694.51 | 1488.84 | 21.0114 | 0.292419 | -8.14803 | 390.05 | 0 | feasible |

Table scr-1000m-8in-4. Wall sensitivity against the base; full selected grid coordinates and individual constraint margins are retained in JSON.

### scr-1500m-10in

| Wall step | Wall (mm) | Weight (N/m) | Tension limit (kN) | Top tension maximum over offsets (kN) | ΔT (%) | Utilisation maximum over offsets (1) | ΔU (%) | Radius minimum over offsets (m) | ΔR (%) | Disposition |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| thinner | 18.263 | 903.009 | 3929.42 | 2294.73 | -15.0339 | 0.437078 | 5.45977 | 585.075 | 0 | feasible |
| base | 21.438 | 1062.79 | 4555.07 | 2700.76 | 0 | 0.41445 | 0 | 585.075 | 0 | feasible |
| thicker | 25.400 | 1256.07 | 5311.92 | 3191.94 | 18.1867 | 0.397091 | -4.18844 | 585.075 | 0 | feasible |

Table scr-1500m-10in-4. Wall sensitivity against the base; full selected grid coordinates and individual constraint margins are retained in JSON.

### scr-2000m-12in

| Wall step | Wall (mm) | Weight (N/m) | Tension limit (kN) | Top tension maximum over offsets (kN) | ΔT (%) | Utilisation maximum over offsets (1) | ΔU (%) | Radius minimum over offsets (m) | ΔR (%) | Disposition |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| thinner | 21.438 | 739.934 | 5474.73 | 2507.1 | -26.4016 | 0.385146 | 0.0324658 | 780.1 | 0 | feasible |
| base | 25.400 | 1005.37 | 6401.54 | 3406.46 | 0 | 0.385021 | 0 | 780.1 | 0 | feasible |
| thicker | 28.575 | 1212.59 | 7125.12 | 4108.6 | 20.612 | 0.389982 | 1.2885 | 780.1 | 0 | feasible |

Table scr-2000m-12in-4. Wall sensitivity against the base; full selected grid coordinates and individual constraint margins are retained in JSON.

### slwr-1500m-8in

| Wall step | Wall (mm) | Weight (N/m) | Tension limit (kN) | Top tension maximum over offsets (kN) | ΔT (%) | Utilisation maximum over offsets (1) | ΔU (%) | Radius minimum over offsets (m) | ΔR (%) | Disposition |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| thinner | 15.875 | 630.985 | 2724.06 | 1082.18 | -13.1142 | 0.340953 | 7.72436 | 329.034 | 0 | feasible |
| base | 18.263 | 726.226 | 3097 | 1245.52 | 0 | 0.316505 | 0 | 329.034 | 0 | feasible |
| thicker | 22.225 | 878.819 | 3694.51 | 1507.23 | 21.0121 | 0.290396 | -8.24916 | 329.034 | 0 | feasible |

Table slwr-1500m-8in-4. Wall sensitivity against the base; full selected grid coordinates and individual constraint margins are retained in JSON.

### slwr-2000m-10in

| Wall step | Wall (mm) | Weight (N/m) | Tension limit (kN) | Top tension maximum over offsets (kN) | ΔT (%) | Utilisation maximum over offsets (1) | ΔU (%) | Radius minimum over offsets (m) | ΔR (%) | Disposition |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| thinner | 18.263 | 903.009 | 3929.42 | 2064.96 | -15.0338 | 0.408562 | 6.59455 | 438.712 | 0 | feasible |
| base | 21.438 | 1062.79 | 4555.07 | 2430.33 | 0 | 0.383286 | 0 | 438.712 | 0 | feasible |
| thicker | 25.400 | 1256.07 | 5311.92 | 2872.33 | 18.1868 | 0.36351 | -5.15959 | 438.712 | 0 | feasible |

Table slwr-2000m-10in-4. Wall sensitivity against the base; full selected grid coordinates and individual constraint margins are retained in JSON.

### slwr-2500m-12in

| Wall step | Wall (mm) | Weight (N/m) | Tension limit (kN) | Top tension maximum over offsets (kN) | ΔT (%) | Utilisation maximum over offsets (1) | ΔU (%) | Radius minimum over offsets (m) | ΔR (%) | Disposition |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| thinner | 21.438 | 739.934 | 5474.73 | 2115.06 | -26.4015 | 0.35676 | 3.14709 | 548.391 | 0 | feasible |
| base | 25.400 | 1005.37 | 6401.54 | 2873.78 | 0 | 0.345875 | 0 | 548.391 | 0 | feasible |
| thicker | 28.575 | 1212.59 | 7125.12 | 3466.12 | 20.6119 | 0.344078 | -0.519552 | 548.391 | 0 | feasible |

Table slwr-2500m-12in-4. Wall sensitivity against the base; full selected grid coordinates and individual constraint margins are retained in JSON.

## What is not established

- Dynamic response is not established; wave/current time histories, vessel motion transfer functions and a qualified dynamic model are absent.
- Fatigue damage and life are not computed here; stress-cycle histories, S-N curves, stress concentration factors and a fatigue design basis are absent.
- VIV response is not established; current profiles, modal response and a qualified VIV model are absent.
- A code check to a named edition is not established; the governing code edition, load combinations, design factors, collapse/burst and combined-loading checks are absent. The nominal X65 property citation does not establish code compliance.
- A vendor line-pipe catalogue match is not established; vendor dimensions, tolerances, corrosion allowance, mill certificates and procurement specifications are absent.
- Installation, seabed/soil interaction, elastic extension and equipment qualification are not established; installation cases, soil data, axial-strain equilibrium and hardware capacities are absent.

## Reproduce appendix

Run from the repository root in the existing environment. No package synchronization or network access is required.

```bash
PYTHONPATH=src uv run --no-sync python scripts/riser_capability/build_scr_slwr_worked_configurations.py
PYTHONPATH=src uv run --no-sync python scripts/riser_capability/build_scr_slwr_worked_configurations.py --check
PYTHONPATH=src uv run --no-sync python -m pytest tests/riser_capability tests/riser_configuration_screen -q
uv run --no-sync python scripts/enforcement/check-engineering-register.py docs/domains/risers/worked-configurations/scr-slwr-worked-configurations.md
```

Each generated input also uses the existing router:

```bash
PYTHONPATH=src uv run --no-sync python -m digitalmodel examples/workflows/riser-worked-configurations/scr-1000m-8in.yml
PYTHONPATH=src uv run --no-sync python -m digitalmodel examples/workflows/riser-worked-configurations/scr-1500m-10in.yml
PYTHONPATH=src uv run --no-sync python -m digitalmodel examples/workflows/riser-worked-configurations/scr-2000m-12in.yml
PYTHONPATH=src uv run --no-sync python -m digitalmodel examples/workflows/riser-worked-configurations/slwr-1500m-8in.yml
PYTHONPATH=src uv run --no-sync python -m digitalmodel examples/workflows/riser-worked-configurations/slwr-2000m-10in.yml
PYTHONPATH=src uv run --no-sync python -m digitalmodel examples/workflows/riser-worked-configurations/slwr-2500m-12in.yml
```

The router writes detailed JSON/CSV under `results/riser-worked-configurations/<case>/` and includes its own runtime metadata. The builder retains deterministic evidence without `runtime_s`; `--check` compares all six YAMLs, JSON, Markdown and six SVGs without writing them. Generated YAML changes are detected by `--check`; extend the builder's `CASES`, `SIZES` and `settings_for` to change the generated basis.

## Run-it-yourself prompt

> In this repository, read the engineering register and the SCR/SLWR worked-configuration builder. Rerun the builder, its --check mode, the riser capability and screen tests, and the register check using the commands above. Extend one generic case in the builder without changing existing source modules or loosening screening limits. Recompute weight and tension limits, retain violated candidates, compare independent equilibrium values, regenerate the report and SVGs, and name every missing qualification. Use no project data, network, commits or pushes.
