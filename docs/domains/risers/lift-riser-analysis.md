# Lift-riser analysis

The lift-riser functions now expose dry, contents, filled, buoyancy and signed
submerged weights. A sourced SI design-basis template connects operating and
survival snapshots to the existing gravity and axial calculations. Four bounded
flow screens cover isolated settling, horizontal deposition, vertical transport
and ideal isothermal expansion energy. Slugging is explicitly `NOT_EVALUATED`.

These calculations support screening. The way forward is to qualify physical
inputs and calibration envelopes, resolve the missing regime source, establish
project-specific criteria and evaluate coupled structural and hydraulic response
before making a design decision. No licensed solver is required or imported.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-22 | Add force components, a sourced basis and bounded flow calculations so input evidence and unevaluated checks remain visible. |

## Scope and equations

Slice 1 extends [the existing mining-lift method](mining-lift-screening.md).
`digitalmodel.subsea.mining_riser.string_weight_inventory` uses its D1/D2
geometry and mass balance. Dry weight includes wall, hardware and buoyancy
material. Contents weight uses the complete inventory regardless of axial
participation. Filled weight is dry plus contents; buoyancy is displaced-fluid
weight; submerged weight is filled minus buoyancy and may be negative. Axial
added mass is excluded from static weight. Lower-end effective force is added
separately by `free_hanging_screen`; it is absent from the weight inventory.

The existing local slurry density, piecewise top-tension integration and rod
eigenproblem are reused without changing their equations. Their sources R1–R4,
derivations D1–D7 and limitations remain in the existing method guide. In
particular, effective tension is not pressure-dependent wall tension, and the
reported extension is a pressure-neutral reference.

The new flow equations use the following publicly opened sources. Responses
were inspected on 2026-10-23 UTC; citations identify equations and principles,
not adopted numerical data. No fitted coefficient or engineering acceptance
limit is bundled.

| ID | Source and locator | Equation and applicable use |
| --- | --- | --- |
| F1 | [Slurry Transport, section 4.2, The Equilibrium of Forces](https://eng.libretexts.org/@go/page/29200), 2021 web edition, equations (4.2-5) and (4.2-6); [section 4.4](https://eng.libretexts.org/@go/page/29202) | For an isolated sphere, `w = sqrt(4*g*d*(rho_s/rho_l-1)/(3*Cd))`; `Re = rho_l*w*d/mu`. Sphere volume/projected area and dynamic pressure supply the geometric factors. The supplied drag coefficient must apply at the returned Reynolds number. |
| F2 | [Slurry Transport, section 7.8, The Limit Deposit Velocity](https://eng.libretexts.org/@go/page/31634), 2021 web edition, opening definition of the deposit Froude number | `v_ldv = FL*sqrt(2*g*D*(rho_s/rho_l-1))`. `FL` is a required sourced input for a horizontal slurry run. The factor `2` belongs to this definition. This is not a vertical-riser deposition criterion. |
| F3 | Phase-volume continuity, independent derivation using local inventory as defined in existing R1 (2016), section 6.2.1 | `j = (1-C)*v_l + C*v_s`, `w = v_l-v_s`, therefore `j_required = (1-C)*w + v_s_required`. All coefficients are identities. Requires a separately sourced effective phase slip for steady upward liquid/solid transport. |
| F4 | [University Physics Volume 2](https://openstax.org/books/university-physics-volume-2/pages/3-3-first-law-of-thermodynamics), 2016, section 3.3, isothermal work | `W = n*R*T*ln(Vout/Vin)`. For ideal isothermal gas, `Vout/Vin = Pin/Pout`; per unit time, `Pgas = Pout*Qg_out*ln(Pin/Pout)`. Equating `eta*Pgas = rho_l*g*Q_l*H` gives an energy-limited liquid-flow bound. |
| F5 | [Overview of Void Fraction Measurement Techniques, Databases and Correlations for Two-Phase Flow in Small Diameter Channels](https://doi.org/10.3390/fluids5040216), 2020 | Candidate regime reference only. The publisher returned HTTP 403. Its equations and coefficients were not adopted; `slugging_screen()` returns `NOT_EVALUATED`. |

F1 assumes a heavier isolated sphere in a steady Newtonian liquid. It excludes
hindered settling, nonspherical particles, wall effects and particle interaction.
F2 needs a factor calibrated for diameter, particle grading, concentration,
viscosity and density; it supplies no operating margin. F3 uses local, not
delivered, solids concentration and excludes gas, beds and restart transients.
An isolated settling speed cannot automatically be substituted for slurry slip.

F4 assumes ideal isothermal gas, constant liquid density, positive lift, and
equal reservoir-surface and discharge pressure. Gas flow is volumetric at
outlet absolute pressure and the same temperature, not standard-volume flow.
The expansion-to-lift efficiency is supplied explicitly and is not compressor
efficiency. Injection pressure and hydraulic losses must be established
separately. This is neither an operating-point solution nor deep-pressure
three-phase capacity. Solids, dissolution and nonideal gas are excluded.

## Design-basis template

`digitalmodel.subsea.lift_design_basis` defines immutable `Source`,
`SourcedQuantity`, `LoadCase` and `LiftDesignBasis` records. Every scalar requires
its unit and a source with a credential-free HTTPS reference, locator,
applicability conditions and explicit `assumed` flag. The software checks
presence and form; it does not authenticate a reference or qualify evidence.
Use `assumed=True` for assumptions and manufactured fixtures. No unit conversion
is implicit. A true zero is data; it does not mean missing.

The exported `BASIS_FIELDS` and `CASE_FIELDS` mappings are the exact template.
Unknown or missing fields and wrong units are rejected. The following fields
are all required, with an independent `SourcedQuantity` for each.

| Group | Fields | Units |
| --- | --- | --- |
| Immersion | `water_depth_m`, `gravity_m_s2`, `external_density_kg_m3` | m, m/s2, kg/m3 |
| Main tube | `outer_diameter_m`, `wall_thickness_m`, `material_density_kg_m3`, `youngs_modulus_pa` | m, m, kg/m3, Pa |
| Attached inventory | `contents_inertia_fraction`, `additional_mass_kg_m`, `additional_displaced_area_m2`, `axial_added_mass_kg_m` | 1, kg/m, m2, kg/m |
| Case slurry | `liquid_density_kg_m3`, `solids_density_kg_m3`, `solids_volume_fraction`, `mixture_velocity_m_s` | kg/m3, kg/m3, 1, m/s |
| Case environment | `significant_wave_height_m`, `peak_wave_period_s`, `current_speed_m_s` | m, s, m/s |
| Case top motion | `top_heave_amplitude_m`, `top_heave_period_s` | m, s |
| Case lower assembly and pump | `lower_end_force_n`, `lower_end_mass_kg`, `pump_differential_pressure_pa` | N, kg, Pa |

The template requires at least one operating case and one survival case, with
unique names. This is a template scope requirement, not a claimed standard
clause. Add further snapshots as needed. All geometric, material and period
quantities are positive; magnitudes of motion, environment, flow, pump pressure
and added inventory may be zero. Lower effective force is signed. Fractions
use their algebraic range; physical slurry packing limits require separate
evidence, as described in R1. The artificial solid endpoint is not a slurry.

`basis.section(case_name)` maps the full water depth to one uniform main tube,
using the case's local mixture density. The immersion is fully vertical and
the top axial connection is assumed load-carrying. Use the existing `Tube`,
`BundleSection` and `SlurryLayer` interfaces directly for bundles, auxiliary
connections and piecewise profiles, splitting at every common boundary.

`screen_case` reports static balance, weight inventory and a comparison with
`top_tension_limit`, which must be a positive sourced quantity in N or explicitly
`None`. With no criterion the status is `NOT_EVALUATED`. With a criterion,
`PASS` means only that top effective tension does not exceed that supplied
number. Any effective compression produces `FAIL` for the taut-string assumption.
Neither status checks wall strength or establishes compliance. Lower inertial
mass is used separately by `axial_modes`, not in the static force balance.

Wave, current, top-motion, pump and throughput inputs are recorded but do not
enter this gravity-only adapter. Strength, fatigue, forced response, VIV,
erosion and interruption/restart remain unevaluated. A survival snapshot with
zero flow does not mean deposition or restart has been checked.

## Calibration and result contract

`digitalmodel.subsea.lift_flow_screening.Calibration` requires a model name,
source and immutable mapping of finite inclusive `Interval` bounds for every
numeric argument under its exact name. F1 additionally requires
`particle_reynolds`. The model names are `isolated_sphere`,
`horizontal_deposition`, `vertical_slip` and `isothermal_energy`.

The source conditions must establish particle shape and distribution, geometry,
fluid, pressure, temperature and any other applicable restrictions. A rectangular
numeric envelope cannot establish those conditions or correlated parameter
validity. Point intervals support single attested conditions and arithmetic tests.

Missing calibration, mismatched model, incomplete fields or out-of-range inputs
return `NOT_EVALUATED`, with no numerical answer and a reason. F1 also checks
the derived Reynolds number. Invalid mathematical inputs raise `ValueError`
even if calibration is absent. Successful arithmetic returns `EVALUATED`; the
flow screens deliberately supply no pass/fail engineering threshold.

## Worked example and synthetic basis H1

Run the complete example from the repository root:

```sh
PYTHONPATH=src python examples/mining_lift_riser/analysis.py
```

All inputs are authored manufactured assumptions, not engineering data from a
publication or previous installation. The source flag explicitly preserves
that distinction. The flow examples are separate arithmetic fixtures; their
densities and gravity are deliberately different from the structural case.
They do not describe one coupled hydraulic operating condition.

| Calculation | Explicit H1 inputs and independent hand method | Answer |
| --- | --- | --- |
| Tube geometry | OD `2` m, wall `0.5` m: `Ai=pi/4`, `Aw=3*pi/4`, `Ad=pi` m2 | Exact geometric identities |
| Static inventory | Length `2` m, wall density `8` kg/m3, outside density `1` kg/m3, gravity `10` m/s2. Local liquid/solid densities `1` and `5` kg/m3 at fraction `0.25` give contents density `2` kg/m3. No additional inventory. | Dry `120*pi = 377.0` N; contents `10*pi = 31.4` N; filled `130*pi = 408.4` N; buoyancy `20*pi = 62.8` N |
| Top tension and extension | Lower effective force zero; `w=55*pi` N/m; `EA=800*3*pi/4=600*pi` N; `Ttop=w*2`; extension `w*2²/(2*EA)=11/60` m | `345.6` N; `0.2` m, rounded for display |
| Criterion comparison | Artificial top limit `350` N; `110*pi < 350`. No lower mass. | `PASS` for that comparison only |
| First axial period | Full contents participation, `mu=6.5*pi` kg/m; fixed-free analytical period `4*L*sqrt(mu/EA)`; example uses `32` rod elements | `0.8` s, rounded |
| F1 settling | `d=0.25` m, densities `10` and `2` kg/m3, viscosity `0.5` Pa s, gravity `6` m/s2, `Cd=2`: `w²=4*6*0.25*(10/2-1)/(3*2)=4`; `Re=2*2*0.25/0.5=2` | `2.0` m/s |
| F2 horizontal deposition | Same flow-fixture densities, viscosity and gravity; pipe ID `2` m, local fraction `0.25`, synthetic `FL=0.5`: `v=0.5*sqrt(2*6*2*4)=sqrt(24)` | `4.9` m/s |
| F3 vertical transport | Synthetic effective slip `2` m/s, local fraction `0.25`, required solids speed `1` m/s: `j=0.75*2+1` | `2.5` m/s |
| F4 airlift energy | Absolute pressures `8` and `2` Pa, outlet gas flow `3` m3/s, liquid density `2` kg/m3, gravity `6` m/s2, lift `4` m, efficiency `0.5`: `Pgas=6*ln(4)`; `Q=ln(4)/16` m3/s | `8.3` W; exact flow retained by the API and rounded to one decimal in the example output |
| Slugging | No readable regime source and no qualified transition criterion | `NOT_EVALUATED` |

The survival fixture retains the same inventory and explicitly sets velocity
to zero. Environment, motion and pump values in the script are artificial
basis fields, without a response prediction. Each flow fixture uses a closed
point envelope to test arithmetic; it establishes no physical applicability.

H1 tests also use attached mass `pi` kg/m, added inertia `100` kg/m and
additional displacement `10*pi` m2 to check force separation and signed buoyancy;
top limits `300` N and absent to test failed and unevaluated comparisons; and
lower effective force `-1` N to test compression. Other probes are invalid
inputs or out-of-domain points. Numerical comparisons use pytest's default
tolerance. The existing axial tests supply D5–D7 continuum checks and mesh
convergence; numerical tolerances are verification targets, not design limits.

## Verification and later slices

`tests/subsea/test_lift_riser_analysis.py` writes out independent expected
arithmetic in its docstrings and checks missing provenance, units, required
fields, defensive immutability, invalid inputs, model separation, bounded
applicability and missing checks. The existing mining-riser regression suite
continues to exercise the original density, static and modal methods. The
worked script is exercised end to end as part of the new tests.

Later slices require a readable flow-regime source and qualified slugging
calibration, nonuniform basis adapters, pressure/flow coupling, loss and
compressor models, transient transport and a forced-response assessment.
No numerical acceptance criterion for those checks is inferred here.
