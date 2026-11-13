# NREL 5 MW floating wind turbine on the OC4-DeepCwind semi-submersible

This is the second reference case for the OrcaFlex modular generator's
floating wind model type. It uses the same builder and checks as the
[IEA 15 MW case](../iea15-volturnus-s/README.md). The design is the NREL 5 MW
reference turbine on the OC4-DeepCwind semi-submersible, moored by three
catenary lines in 200 m of water. The generated model's data are compared with
the OC4 Phase II definition report and with an OpenFAST linearisation of the
same system held in the public OpenFAST regression tests. No OrcaFlex licence
is used.

Issue: [#3970](https://github.com/vamseeachanta/digitalmodel/issues/3970).
Sources: [`SOURCES.md`](../../../wind/fowt-oc4/SOURCES.md).

## Model

[`spec.yml`](spec.yml) uses the rigid representation. The hull VesselType
carries the whole-system mass of 14,072 t, with the centre of gravity 9.894 m
below the waterline. That mass aggregates four parts:

- the report's platform, including 9,621 t of water ballast;
- the 249.7 t tower;
- the 240 t nacelle;
- the 109.6 t rotor.

Hydrostatics, added mass and radiation damping come from the public WAMIT
files. Quadratic drag comes from the report's Table 4-6. The three mooring
lines use the report's equivalent-line properties (Table 5-1). The report
publishes no minimum breaking load, so no line utilisation is computed. No
rotor thrust is applied in this case.

## Results that need no licence

| Quantity | This model | Comparator | Source | Tolerance | Difference |
|---|---|---|---|---|---|
| Displaced volume | 13,917 m³ | 13,917 m³ | Report Table 4-6 | 1 % | 0.00 % |
| Buoyancy force | 139.89 MN | 139.89 MN | Table 4-6 | 1 % | 0.00 % |
| Heave hydrostatic stiffness | 3.820 MN/m | 3.836 MN/m | Table 4-6 | 2 % | −0.41 % |
| Pitch hydrostatic stiffness, weight term excluded | −380.7 MN·m/rad | −377.6 MN·m/rad | Table 4-6 | 3 % | +0.83 % |
| Mooring vertical load, undisplaced | −1.887 MN | −1.839 MN | Eq. (5-14) | 3 % | +2.60 % |
| Mooring stiffness, surge | 70.1 kN/m | 70.8 kN/m | Eq. (5-15) | 3 % | −0.94 % |
| Mooring stiffness, heave | 19.08 kN/m | 19.1 kN/m | Eq. (5-15) | 3 % | −0.10 % |
| Mooring stiffness, pitch | 86.7 MN·m/rad | 87.3 MN·m/rad | Eq. (5-15) | 3 % | −0.68 % |
| Mooring stiffness, yaw | 116.1 MN·m/rad | 117 MN·m/rad | Eq. (5-15) | 3 % | −0.78 % |
| Mooring stiffness, surge–pitch | −105.0 kN/rad | −108 kN/rad | Eq. (5-15) | 5 % | −2.75 % |

Table 1. Hydrostatic and mooring checks against Robertson et al. (2014),
NREL/TP-5000-60601.

In still water the model is almost exactly balanced. It settles 0.5 mm higher
and trims −0.07°, and each fairlead carries 1,098 kN.

| Mode | This model, added mass at the mode's frequency | This model, infinite-frequency added mass | OpenFAST linearisation | Phase II paper (text) |
|---|---|---|---|---|
| Surge | 113.7 s (+6.4 %) | 107.9 s (+1.0 %) | 106.9 s | 0.01 Hz (100 s) |
| Heave | 17.28 s (−0.03 %) | 17.20 s (−0.5 %) | 17.28 s | 0.058 Hz (17.2 s) |
| Pitch | 25.43 s (−0.05 %) | 25.28 s (−0.7 %) | 25.45 s | 0.04 Hz (25 s) |
| Yaw | 79.8 s (−0.5 %) | 76.4 s (−4.8 %) | 80.2 s | not quoted |

Table 2. Rigid-body natural periods. Percentages are differences from the
OpenFAST linearisation. The tolerances are 10 % for surge and yaw and 5 % for
heave and pitch; all values in the first column lie within them.

Heave, pitch and yaw agree with the OpenFAST linearisation to within 0.6 %.
Surge differs by 6.4 %; the cause of this discrepancy remains unexplained.
Substituting the infinite-frequency added-mass matrix changes this model's
surge period from 113.7 s to 107.9 s, a 1.0 % difference from the OpenFAST
linearisation. This sensitivity check does not establish how the source
state-space radiation model contributes to its surge period or explain the
original difference. The heave, pitch and yaw periods move further away with
the substitution, so the frequency-dependent result is kept as primary.
Which surge period a time-domain OrcaFlex decay test reproduces is not
established without a licensed run.

The paper quotes surge, heave and pitch frequencies to one or two significant
figures. The computed frequencies lie inside the rounding band of each quoted
value: surge 0.0088 Hz, heave 0.0579 Hz and pitch 0.0393 Hz.

## Not established

- That the generated model loads and solves in OrcaFlex. No licensed load has
  been performed.
- Time-domain decay periods and damping, including the surge added-mass
  question above.
- Wave response. As in the IEA case, the VesselType carries no load RAOs.
- Any line utilisation, because the report publishes no breaking load.

## Licensed packages waiting

Five packages, one per case, each holding `spec.yml`, the generated model
(`model/master.yml`) and a `run.md` with the exact settings and the expected
values with tolerances. The OC4 basis carries no rotor thrust and no sea state,
so only still-water cases are prepared.
`scripts/fowt/write_oc4_run_packages.py <output_dir>` regenerates them. Regeneration replaces a package only when every file in it is one
the script wrote, unchanged; a folder holding anything else, such as saved
solver results, is refused and left as it is.

| Package | Expected comparator |
|---|---|
| `LC0_statics` | Surge −0.002 m, heave 0.001 m, pitch −0.075°; fairlead tensions 1,098 to 1,099 kN (2 %) |
| `LC1_surge_decay` | Period 113.7 s (10 %); OpenFAST linearisation 106.9 s (10 %); infinite-frequency value 107.9 s recorded for comparison |
| `LC2_heave_decay` | Period 17.28 s (5 %); OpenFAST linearisation 17.28 s (5 %) |
| `LC3_pitch_decay` | Period 25.43 s (5 %); OpenFAST linearisation 25.45 s (5 %) |
| `LC6_yaw_decay` | Period 79.8 s (10 %); OpenFAST linearisation 80.2 s (10 %) |

Table 3. Licensed run packages.

## Reproduce

```
python scripts/fowt/extract_oc4_generator_data.py <raw_dir> docs/domains/orcaflex/wind/fowt-oc4
python scripts/fowt/run_oc4_checks.py
python -m pytest tests/floating_wind/test_fowt_oc4.py -q
```
