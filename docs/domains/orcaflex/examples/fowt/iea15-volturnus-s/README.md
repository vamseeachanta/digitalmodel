# IEA 15 MW floating wind turbine on the VolturnUS-S semi-submersible

This example models a public reference floating offshore wind turbine with the
digitalmodel OrcaFlex modular generator. The turbine is the IEA 15 MW reference
turbine, carried by the UMaine VolturnUS-S semi-submersible and moored by
three catenary chains in 200 m of water. The generated model is compared with
the published reference values, and the comparisons that need no OrcaFlex
licence are complete. The time-domain runs that need a licence are prepared as
input packages; native loading and hydrodynamic import remain unverified.

Issue: [#3970](https://github.com/vamseeachanta/digitalmodel/issues/3970).

## Summary

| Quantity | This model (hand calculation) | Published | Difference | Tolerance |
|---|---|---|---|---|
| Displaced volume | 20,206.3 m³ | 20,206 m³ | 0.00 % | 1 % |
| Heave hydrostatic stiffness | 4.453 MN/m | 4.47 MN/m | -0.37 % | 2 % |
| Pitch hydrostatic stiffness (weight term excluded) | 2.193 GN·m/rad | 2.19 GN·m/rad | +0.16 % | 3 % |
| Still-water fairlead tension (lines 1; 2 and 3) | 2,415 kN; 2,429 kN | 2,437 kN | -0.91 %; -0.32 % | 2 % |
| Surge natural period | 135.3 s | 142.9 s | -5.3 % | 10 % |
| Heave natural period | 20.45 s | 20.41 s | +0.2 % | 5 % |
| Pitch natural period | 28.10 s | 27.78 s | +1.2 % | 5 % |
| Yaw natural period | 88.8 s | 90.9 s | -2.4 % | 10 % |

Table 1. Licence-free results against Allen et al. (2020) Tables 2, 3, 6 and 11.
Published natural periods are the inverse of frequencies given to 0.001 Hz.
That rounding alone allows the surge period to lie between 133 s and 154 s,
heave 20.2 s to 20.6 s, pitch 27.4 s to 28.2 s and yaw 87 s to 95 s.

Every value in Table 1 lies within its tolerance. All four periods also lie
inside the rounding band of the published frequency. Under the steady rated
rotor thrust of 2.457 MN, the platform settles 22.3 m downwind with a 5.3° mean
pitch. The upwind line tension rises to 4,389 kN, 19.7 % of its 22,286 kN
minimum breaking load. No published table gives these offsets, so they are
compared with the repository's earlier independent analysis of the same
platform instead (Table 5).

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-11-12 | Corrected the wave import file and readiness wording; mirrored the added-mass upper triangle and tested repeated generation to prevent invalid native inputs. |
| 2 | 2026-11-13 | The generator, not the spec, now mirrors the upper triangle of every radiation matrix (one policy for all fowt specs); the spec holds the source values and the emitted model is unchanged. The wave-case models keep first-order wave loading off until the licensed import supplies load RAOs, and `run.md` switches it on after the import. Package regeneration refuses a folder holding files it did not write, so saved results are never deleted. |

Table 1a. Revision history.

## Reference design and sources

| Source | Used for | Location | Retrieved |
|---|---|---|---|
| Allen, C., et al. (2020). *Definition of the UMaine VolturnUS-S Reference Platform Developed for the IEA Wind 15-Megawatt Offshore Reference Wind Turbine*. NREL/TP-5000-76773 | Published comparators: Tables 1–3, 6, 11, 12 | <https://docs.nlr.gov/docs/fy20osti/76773.pdf> | 2026-10-14 (as recorded in `SOURCES.md`) |
| Gaertner, E., et al. (2020). *Definition of the IEA 15-Megawatt Offshore Reference Wind Turbine*. NREL/TP-5000-75698 | Turbine definition | <https://docs.nlr.gov/docs/fy20osti/75698.pdf> | 2026-10-14 |
| IEAWindTask37/IEA-15-240-RWT, revision `e4993d6`, Apache-2.0 | Model inputs: turbine workbook, OpenFAST ElastoDyn, HydroDyn and MoorDyn inputs, WAMIT hydrostatics, added mass, damping and excitation | <https://github.com/IEAWindTask37/IEA-15-240-RWT/tree/e4993d63de10f165389534461dd544006750fe60> | 2026-11-12 |
| Orcina, OrcaFlex help, *Vessel theory: stiffness, added mass and damping* | Hydrostatic matrix convention | <https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltheory,Stiffness,addedmassanddamping.htm> | 2026-11-12 |
| Orcina public example K03, *15MW semi-sub FOWT*, held in `docs/domains/orcaflex/examples/` | Comparator for object layout, key names and two numerical cross-checks; not used as the build | repository | held |

Table 2. Sources.

Model values are curated in two sourced files. Each value carries its unit and
its source table or input file:

- [`docs/domains/orcaflex/wind/fowt-volturnus/basis.yml`](../../../wind/fowt-volturnus/basis.yml):
  system mass and inertia, hull geometry, mooring, metocean ([#2473](https://github.com/vamseeachanta/digitalmodel/pull/2473));
- [`docs/domains/orcaflex/wind/fowt-volturnus/generator_data.yml`](../../../wind/fowt-volturnus/generator_data.yml):
  rotor and nacelle geometry and mass, the steady thrust curve, the
  floating-tower station table, WAMIT hydrostatics, added mass and radiation
  damping, and quadratic damping. Its sources are listed in
  [`GENERATOR_SOURCES.md`](../../../wind/fowt-volturnus/GENERATOR_SOURCES.md).

The pinned IEA repository holds no OrcaFlex input files.

## The model

The generator's `fowt` model type turns one spec file,
[`spec.yml`](spec.yml), into an OrcaFlex model with the same object layout as
Orcina's K03 example:

- **Hull**: an OrcaFlex Vessel. Its VesselType carries mass, inertia, centre of
  mass, displaced volume, centre of buoyancy, the heave/roll/pitch hydrostatic
  stiffness, frequency-dependent added mass and radiation damping at 101
  periods, and the HydroDyn quadratic damping.
- **Tower and rotor-nacelle assembly**: carried as part of one rigid body
  (the rigid representation). A second spec,
  [`spec-flexible.yml`](spec-flexible.yml), models the tower as a
  finite-element line with a lumped rotor-nacelle buoy; see
  [Flexible representation](#flexible-representation).
- **Rotor thrust**: a steady force applied at the hub along the 6° tilted
  shaft. This stands in for the OrcaFlex Turbine object for statics, decay and
  mean-offset cases. It carries no aerodynamic damping, rotor dynamics or
  controller.
- **Mooring**: three 850 m R3 studless chains, 685 kg/m, EA 3.27 GN, from
  fairleads 14 m below the waterline to seabed anchors 837.6 m from the
  centre.

OrcaFlex expects the hydrostatic matrix to include both moment-arm terms,
buoyancy and weight. The WAMIT `.hst` file excludes the weight term, so the
generator adds m·g·(−z_G) to the roll and pitch entries. Orcina's K03 model of
the same platform obeys the same relation to within 1 %, which confirms the
convention independently of this work.

## Load cases

| Case | Description | Licence-free check | Licensed package |
|---|---|---|---|
| LC0 | Static equilibrium, still water, no wind | Equilibrium and fairlead tensions (Tables 3, 4) | `LC0_statics` |
| LC1 | Free decay in surge, +10 m from equilibrium | Natural period (Table 4) | `LC1_surge_decay` |
| LC2 | Free decay in heave, −2 m | Natural period | `LC2_heave_decay` |
| LC3 | Free decay in pitch, +5° | Natural period | `LC3_pitch_decay` |
| LC4a | Rated thrust 2.457 MN at 10.66 m/s, still water | Mean offset, pitch and line tensions (Table 5) | `LC4a_rated_thrust_statics` |
| LC4 | Operating: thrust 1.696 MN at 12 m/s; JONSWAP Hs 1.84 m, Tp 7.44 s, γ 1.0 (Allen Table 12, DLC 1.1) | Mean offset 17.46 m, mean pitch 3.26° | `LC4_operating` |
| LC5 | Parked extreme: JONSWAP Hs 10.7 m, Tp 14.2 s, γ 2.75; wind 47.5 m/s (Allen Table 12, DLC 6.1) | none | `LC5_parked_extreme` |

Table 3. Load-case matrix. Waves and thrust act along +x, toward the downwind side.

## Results that need no licence

The checks in `src/digitalmodel/floating_wind/fowt_static_checks.py` read the
generated spec itself, so they test the numbers the generator writes into the
model. The mooring is solved with an elastic catenary on a frictionless seabed.
Natural periods come from a coupled surge-heave-pitch eigen-solution, with
added mass taken from the WAMIT table at each mode's own frequency. Every value
is in [`results.json`](results.json).

### Still-water equilibrium (LC0)

The model's buoyancy exceeds its weight by 4.36 MN. The three lines pull down
with 6.06 MN at the fairleads, so the platform settles 0.382 m deeper. The
upwind overhang of the rotor-nacelle assembly puts the system centre of gravity
0.342 m upwind of the hull centre, which trims the platform by −1.42° (upwind
side down). Allen et al. do not tabulate a still-water trim, so this value has
no published comparator. A ballast trim in the reference design, if any, is not
represented.

| Line | Tension | Horizontal | Vertical | Length on seabed | Comparator |
|---|---|---|---|---|---|
| 1 (upwind, 180°) | 2,415 kN | 1,339 kN | 2,009 kN | 506 m | 2,437 kN (Table 6), −0.91 % |
| 2 and 3 | 2,429 kN | 1,341 kN | 2,025 kN | 503 m | 2,437 kN (Table 6), −0.32 % |

Table 4. Still-water fairlead tensions.

### Natural periods (LC1 to LC3, and yaw)

| Mode | This model | Repository BEM analysis (#2473) | Allen Table 11 |
|---|---|---|---|
| Surge | 135.3 s | 135.1 s (+0.2 %) | 142.9 s (−5.3 %) |
| Heave | 20.45 s | 20.39 s (+0.3 %) | 20.41 s (+0.2 %) |
| Pitch | 28.10 s | 27.93 s (+0.6 %) | 27.78 s (+1.2 %) |
| Yaw | 88.8 s | 88.6 s (+0.3 %) | 90.9 s (−2.4 %) |

Table 5. Rigid-body natural periods. Percentages are this model's difference
from each comparator.

The repository's earlier analysis computed its added mass with Capytaine and
its mooring with MoorPy. This check uses the public WAMIT data and an
independent catenary. The two agree to within 0.6 % in every mode, and the
mooring stiffness diagonals agree to within 1.2 %. The surge period's −5.3 %
difference from Allen et al. lies inside the published value's rounding band
(133 s to 154 s).

### Mean offset under rated thrust (LC4a)

| Quantity | Free in surge, heave and pitch | Rotations and draft held |
|---|---|---|
| Mean surge | 22.34 m | 23.57 m |
| Mean heave | −0.572 m | held at 0 |
| Mean pitch | 5.33° | held at 0 |
| Upwind line tension (utilisation of 22,286 kN) | 4,389 kN (0.197) | 4,423 kN (0.198) |
| Downwind line tensions | 1,970 kN | 2,009 kN |
| Comparator | none published | 23.56 m, repository analysis (#2473), +0.02 % |

Table 6. Static response to the rated rotor thrust.

The held column repeats the earlier analysis's assumptions and reproduces its
mean offset. The free column is the better estimate of the static response,
and its mean surge is 1.2 m (5.2 %) smaller. Two effects contribute. The thrust
acts along the shaft, inclined by the 6° tilt plus the 5.3° mean pitch, so its
horizontal component is 2.0 % lower. The platform also heaves down 0.57 m and
rotates, which changes the line geometry at the fairleads.

The mean offset of 22.3 m lies on the watch-circle axis (5 m to 30 m) of the
repository's [`fowt_mooring` atlas](../../../../../../atlases/fowt_mooring/). At this offset,
the atlas's template dynamic cable reaches a minimum-bend-radius utilisation of
0.029 to 0.043, for bend-radius limits of 4 m to 6 m. That cable belongs to the
atlas template, not to this design, which defines no export cable. The readout
gives an order of magnitude only.

## What is established, and what is not

Established without a licence, within the stated tolerances:

- the generated hull data reproduce the published displacement and
  hydrostatic stiffness;
- the generated mooring reproduces the published pretension;
- the generated mass, stiffness and added mass give natural periods consistent
  with the published values and with an independent earlier analysis;
- the generated OrcaFlex key names all appear in the held Orcina examples for
  the same object types.

Not established:

- **That the model loads and solves in OrcaFlex.** No licensed load or
  statics has been run. The licence checkout fails in the session that built
  the model. The packages below are the next step.
- **Time-domain decay periods and damping.** They need the LC1 to LC3 runs.
- **Wave response in LC4 and LC5.** The generated VesselType carries no load
  RAOs, so the generated wave-case models have first-order wave loading
  switched off. The wave packages require a licensed import of the first-order
  WAMIT `.out` text file, after which `run.md` switches the vessel's Wave load
  (1st order) effect on. The numeric `.1`, `.3` and `.hst` files are
  retained for reference. The generated `run.md` specifies the requested data,
  property restoration and reference-origin checks. Mean wave drift is not
  included, because no QTF data are imported.
- **The parked extreme case as a DLC 6.1 result.** Parked-rotor, nacelle and
  tower wind drag are not represented, so LC5 gives the first-order wave
  response of the moored hull only.
- **Rotor aerodynamics and control.** Thrust is a steady force. The OrcaFlex
  Turbine object would need the 50 blade stations, aerofoil polars and the
  ROSCO controller from the IEA repository. That is not built.
- **The source's rotor-nacelle centre of mass.** In the IEA workbook, the RNA
  row places the centre of mass 0.533 m further upwind than the mass-weighted
  combination of its own component rows. The model adopts the RNA row, as
  `basis.yml` does; the source discrepancy is unresolved.

## Licensed packages waiting

Eight packages, one per case, each holding `spec.yml`, the generated model
(`model/master.yml`) and a `run.md`. Each `run.md` gives the exact settings and
the expected values with tolerances. When the source directory is supplied,
the two wave cases also include the three numeric WAMIT files and the pinned
first-order text output, renamed from `.out.wamit` to `.out` without changing
its bytes. Select that `.out` file in the vessel type importer and follow
`run.md`; native loading, import, statics and dynamics remain unverified.
Without the source directory, these hydrodynamic inputs are still required.
`scripts/fowt/write_iea15_run_packages.py <output_dir>
<wamit_dir>` regenerates them. Regeneration replaces a package only when every
file in it is one the script wrote, unchanged; a folder holding anything else,
such as saved solver results, is refused and left as it is.

| Package | Expected comparator |
|---|---|
| `LC0_statics` | Surge 0.387 m, heave −0.382 m, pitch −1.42°; fairlead tensions 2,415 and 2,429 kN (2 %) |
| `LC1_surge_decay` | Period 135.3 s (5 %); Allen 142.9 s (10 %) |
| `LC2_heave_decay` | Period 20.45 s (5 %); Allen 20.41 s (5 %) |
| `LC3_pitch_decay` | Period 28.10 s (5 %); Allen 27.78 s (5 %) |
| `LC4a_rated_thrust_statics` | Surge 22.34 m, pitch 5.33°, upwind tension 4,389 kN (3 %) |
| `LC4_operating` | Mean surge 17.46 m, mean pitch 3.26° (5 %); standard deviations recorded |
| `LC5_parked_extreme` | No published comparator; statistics recorded |
| `LC0_flexible_statics` | Vessel position as `LC0_statics`; tower-top displacement −0.222 m and rotation −0.389° in vessel axes (10 %); load order per #3971 recorded |

Table 7. Licensed run packages.

## Related findings

- The generic builder writes Lines before 6D buoys. Every held Orcina example
  writes the buoys first. This matters for the flexible representation:
  [#3971](https://github.com/vamseeachanta/digitalmodel/issues/3971).
- The repository's `CatenarySolver` returns a horizontal tension 6 % above the
  directly integrated elastic solution for a stretched suspended line. Its
  inextensible limit agrees:
  [#3994](https://github.com/vamseeachanta/digitalmodel/issues/3994).
- The phase B solver runs of the OrcaFlex library's floating wind worked
  example ([#2423](https://github.com/vamseeachanta/digitalmodel/issues/2423))
  can use these packages.

## Flexible representation

[`spec-flexible.yml`](spec-flexible.yml) splits the rigid body into its three
parts:

- **Hull**: the platform alone, including ballast. It has a mass of 17,838 t,
  a centre of mass 14.4 m below the waterline and the roll, pitch and yaw
  inertias of the OpenFAST ElastoDyn file.
- **Tower**: a finite-element line from 15 m to 144.386 m above the waterline.
  Its properties come from the workbook's floating-tower table (20 stations,
  with outer diameter, wall thickness, mass per length, EI, GJ and EA). Each
  span of constant wall thickness becomes one section. The top span tapers
  from 10 m to 6.5 m and is split into four sections. Their stiffnesses come
  from tube formulas with the moduli that the table itself implies: E = 200 GPa
  and G = 79.3 GPa at every station.
- **Rotor-nacelle assembly**: a lumped 6D buoy of 945.8 t at the tower top. It
  carries the adopted tower-top inertia tensor moved to the assembly's own
  centre of mass. An OrcaFlex lumped buoy takes principal moments only
  (Orcina, OrcaFlex help, 6D buoys: common data). The assembly buoy therefore
  stays in tower-top axes and carries the mass and centre of mass, and a
  massless child buoy rigidly attached at the centre of mass, oriented on the
  tensor's principal axes (about 12.8° from the tower-top axes), carries the
  three principal moments. The pair reproduces the full tensor, products of
  inertia included, to 1e-9 of its largest moment (tested). That OrcaFlex adds
  a child buoy's moments to its parent's rigid-body inertia is not stated in
  the help pages read; the `LC0_flexible_statics` package records it as a
  check. Statics depend only on mass and centre of mass, which the parent
  carries.
- **Tower Poisson's ratio**: 0.261 at every section, from the isotropic
  relation E/(2G) − 1 with the moduli the station table implies.

The VesselType's hydrostatic weight term uses the platform alone, because
OrcaFlex applies the tower and assembly weight through their own objects.

| Check | Result | Comparator | Tolerance |
|---|---|---|---|
| Tower mass, 13 sections | 1,483.07 t | 1,483.08 t, piecewise-linear integral of the station table (`basis.yml`) | 0.01 % |
| Hull + tower + assembly mass | 20,266.84 t | 20,266.85 t, whole-system value of the rigid spec | 0.001 % |
| Combined centre of mass | within 0.02 mm | rigid spec | 1 mm |
| Combined inertia diagonal | within 0.002 % | rigid spec | 0.01 % |
| Equilibrium and natural periods of the combined body | within 0.0004 % | rigid representation | 0.01 % |
| Tower-top displacement, hull axes, hull upright | −0.174 m | cantilever formula check of the method (0.1 %) | — |
| Tower-top displacement and rotation, hull at its −1.42° equilibrium pitch | −0.222 m, −0.389° | none published | — |

Table 8. Flexible-representation checks. The tower-top values are a first-order
cantilever calculation with the base fixed to the hull. They include the tower's
own weight and the assembly weight acting at its 7.32 m upwind offset; P-delta
is excluded. A negative displacement is upwind.

Dropping the assembly's products of inertia would change the system Ixz by
4.5 %, which is 0.1 % of Ixx; the generated model keeps them.

The generated model places the Tower line, whose End A connects to the
assembly buoy, before the 6D buoys
([#3971](https://github.com/vamseeachanta/digitalmodel/issues/3971)). Whether
OrcaFlex loads that order is not established without a licensed load. The
`LC0_flexible_statics` package states the expected equilibrium and tower-top
values.

## Second reference case

The same model type and checks are applied to the NREL 5 MW turbine on the
OC4-DeepCwind semi-submersible: [`../oc4-deepcwind/README.md`](../oc4-deepcwind/README.md).
Heave, pitch and yaw periods agree with an OpenFAST linearisation of that
system to within 0.6 %, and the mooring stiffness agrees with the published
linearised matrix to within 3 %.

## Reproduce

```
python scripts/fowt/extract_iea15_generator_data.py <raw_dir> docs/domains/orcaflex/wind/fowt-volturnus/generator_data.yml
python scripts/fowt/write_iea15_example_spec.py
python scripts/fowt/run_iea15_checks.py
python -m pytest tests/orcaflex/test_fowt_generator_data.py tests/solvers/orcaflex/modular_generator/test_fowt_model_type.py tests/floating_wind/test_fowt_static_checks.py tests/floating_wind/test_fowt_iea15_flexible.py -q
```
