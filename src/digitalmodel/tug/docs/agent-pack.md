# Tug toolkit module knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/tug` |
| Code pin | `09089a121a16069c4b657d1a320a630a23ab1dd5` |
| Status | DRAFT; implementation inventory and synthetic arithmetic only |
| Analysis family | EC |
| Import route | `digitalmodel.tug.<submodule>` |

Summary: the package provides bollard-pull and escort estimates, sampled girting
checks, fender geometry and energy screens, towline calculations, and duty-cycle
energy and emissions estimates. The example establishes explicitly parameterized
speed-factor arithmetic. Engineering qualification of the coefficients, rule
criteria and operating envelopes is NOT_EVALUATED.

### Revision history

| Revision | Date | What changed and why |
|---|---|---|
| Initial | 2026-10-28 | Record public interfaces, independent speed arithmetic and qualification gaps so callers can distinguish code behavior from design evidence. |

## 1. Purpose and scope

The scope is the tug package at the code pin. Source locators below describe
existing behavior, not primary engineering references. No rule edition or
equipment-specific suitability is established by a source comment or passing
test. The pack and tests introduce no calculation changes.

The inventory includes every authored public function, class and public class
member under the existing AST checker's contract. Dataclass fields and enum
members are described with their classes; there are no authored public methods
at the pin. Imported helpers are identified in section 9. The initializer only
describes submodules and provides no convenience exports.
[src/digitalmodel/tug/__init__.py:L1-L12]

`passing_ship_forces` and `vessel_seakeeping` are outside this pack. Their packs
and consistency tests remain follow-up under the parent multi-module issue.

## 2. Inputs and outputs

Units are encoded in parameter and field names; no runtime unit conversion
framework is enforced. `_t` denotes tonnes-force for towline and pull, but
displacement and fuel quantities are mass in tonnes. Angles are degrees,
lengths metres, power kW, energy kWh or kJ as named, and assist speed knots.
All result dataclasses are mutable records with no constructor validation.
Defaults cited below are implementation assumptions, not recommendations.

| Public symbol | Inputs, units and validation | Output contract and public data members | Evidence |
|---|---|---|---|
| `BollardPullResult` | Required fields; no checks | `propulsion` string, `installed_power_kw`, `bp_ahead_t`, `bp_astern_t`, `transverse_pull_t` | [src/digitalmodel/tug/bollard_pull.py:L33-L40] |
| `EscortForceResult` | Required fields; no checks | `speed_kn`, `steering_force_t`, `braking_force_t`, `exceeds_bollard_pull` boolean | [src/digitalmodel/tug/bollard_pull.py:L44-L50] |
| `bollard_pull` | Positive `installed_power_kw` by comparison; `propulsion` case-normalized through `Propulsion`; optional `factor_t_per_kw` override is unchecked | `BollardPullResult`; ahead from power times factor; astern and transverse pull still use package factors even with an override | [src/digitalmodel/tug/bollard_pull.py:L53-L90] |
| `escort_force` | Nonnegative `speed_kn`, positive `lateral_area_m2` by comparisons; `bp_ahead_t`, lift and drag coefficients unchecked; coefficient defaults exist | `EscortForceResult`; dynamic-pressure forces in tonnes-force; comparison flag uses steering force only | [src/digitalmodel/tug/bollard_pull.py:L93-L142] |
| `Propulsion` | Enum string values `conventional`, `conventional_nozzle`, `asd`, `tractor_vsp`, `rotor` | Members `CONVENTIONAL`, `CONVENTIONAL_NOZZLE`, `ASD`, `TRACTOR_VSP`, `ROTOR`; unknown value raises `ValueError` | [src/digitalmodel/tug/constants.py:L15-L22] |
| `OperatingPhase` | Required `name`, `power_fraction`, `duration_h`; constructor unchecked | Phase record; fraction is dimensionless, duration in hours | [src/digitalmodel/tug/emissions.py:L28-L33] |
| `HybridSizingResult` | Required fields; constructor unchecked | `total_energy_kwh`, `peak_power_kw`, `baseline_engine_kw`, `battery_power_kw`, `battery_energy_kwh`, dimensionless `peak_energy_fraction` | [src/digitalmodel/tug/emissions.py:L37-L45] |
| `EmissionsResult` | Required fields; constructor unchecked | `fuel_mass_t`, `co2_t`, `nox_rate_g_per_kwh`, `tier_iii_nox_limit_g_per_kwh`, `tier_iii_compliant` boolean | [src/digitalmodel/tug/emissions.py:L49-L56] |
| `imo_tier_iii_nox_limit` | `rated_rpm` positive by comparison; finite values not explicitly checked | Encoded piecewise rate in g/kWh; applicable regulation and edition NOT_EVALUATED | [src/digitalmodel/tug/emissions.py:L59-L72] |
| `cycle_energy` | Finite positive `installed_power_kw`; nonempty `profile`; every duration finite and nonnegative, fraction finite within the closed unit interval | Sum of phase power times duration in kWh; zero-load and zero-duration phases permitted; invalid inputs raise `ValueError` | [src/digitalmodel/tug/emissions.py:L75-L91] |
| `size_hybrid` | Same power/profile validation through `cycle_energy`; `baseline_fraction` strictly inside the unit interval; implementation default exists | `HybridSizingResult`; integrates only phase demand above baseline; zero total energy returns zero battery share; no efficiency/reserve model | [src/digitalmodel/tug/emissions.py:L94-L134] |
| `fuel_and_emissions` | Power/profile as above; case-normalized `fuel` must be a stored key; optional `sfoc_g_per_kwh`, `nox_rate_g_per_kwh` and `rated_rpm`; SFOC/NOx rates are not domain-checked | `EmissionsResult`; omitted SFOC scales with stored fuel heating-value ratio; CO2 uses stored carbon factor; compliance flag compares rate to encoded limit | [src/digitalmodel/tug/emissions.py:L137-L188] |
| `AssistedVessel` | Required `name`, `contact_low_m`, `contact_high_m`; no band-order or finite checks | Contact-band record above waterline in metres; `name` is an identifier, not a physical property | [src/digitalmodel/tug/fendering.py:L21-L26] |
| `FenderHeightResult` | Required fields; no checks | `fender_height_m`, `fender_face_height_m`, `mean_overlap_fraction`, `worst_vessel`, `worst_overlap_fraction`, `all_vessels_contact` | [src/digitalmodel/tug/fendering.py:L30-L38] |
| `BerthingEnergyResult` | Required fields; no checks | `berthing_energy_kj`, `fender_rated_energy_kj`, dimensionless `utilisation`, `adequate` boolean | [src/digitalmodel/tug/fendering.py:L42-L48] |
| `optimize_fender_height` | Nonempty `fleet`, positive `fender_face_height_m` by comparison; `search_step_m` has a default but no positive/finite validation | `FenderHeightResult`; grid scan maximizes mean face overlap; ties retain first candidate; contact flag only means positive geometric overlap | [src/digitalmodel/tug/fendering.py:L61-L110] |
| `berthing_energy` | Positive `displacement_t` mass, nonnegative normal `approach_speed_m_s`; `fender_rated_energy_kj` and mass/eccentricity/softness coefficients unchecked; defaults exist | `BerthingEnergyResult`; utilization is required/rated energy, infinite for nonpositive rating; adequacy compares utilization to unity | [src/digitalmodel/tug/fendering.py:L113-L160] |
| `GirtingResult` | Required fields; no checks | `towline_force_t`, `tow_point_lever_m`, optional `equilibrium_heel_deg`, `deck_edge_angle_deg`, optional `margin_deg`, `critical_towline_t`, `girting_safe` | [src/digitalmodel/tug/stability_girting.py:L30-L41] |
| `heeling_arm_curve` | `towline_force_t`, `tow_point_lever_m`, positive `displacement_t`; optional `heel_angles_deg`; force, lever and angles unchecked | List of `(heel_deg, heeling_arm_m)` pairs; omitted angles select the encoded sampling grid | [src/digitalmodel/tug/stability_girting.py:L44-L74] |
| `critical_towline_force` | Nonempty, ordered `gz_curve` pairs `(heel_deg, gz_m)` are a caller precondition; positive `tow_point_lever_m` checked; `displacement_t`, `deck_edge_angle_deg` not validated up front | Transverse tonnes-force from a bounded bisection over the sampled equilibrium criterion; zero if sampled positive upper bound is absent | [src/digitalmodel/tug/stability_girting.py:L125-L183] |
| `assess_girting` | Caller-supplied `gz_curve`, `towline_force_t`, `tow_point_lever_m`, `displacement_t`, `deck_edge_angle_deg`; validation is delegated and branch-dependent | `GirtingResult`; missing equilibrium sets heel/margin to `None`; `girting_safe` needs positive margin and force strictly below computed critical force | [src/digitalmodel/tug/stability_girting.py:L186-L244] |
| `angle_of_vanishing_stability` | Nonempty ordered `gz_curve`; shape, finite values and sign convention unchecked | Degrees from first positive-to-nonpositive crossing; otherwise returns last sampled angle, which does not establish an actual zero crossing | [src/digitalmodel/tug/stability_girting.py:L247-L258] |
| `is_self_righting` | `gz_curve` as above; `threshold_deg` default is an encoded assumption | Boolean comparing the previous function's returned angle with threshold; no independent self-righting qualification | [src/digitalmodel/tug/stability_girting.py:L261-L266] |
| `TowlineLoadResult` | Required fields; no checks | `static_load_t`, `dynamic_factor`, `design_load_t`, `society`, `safety_factor`, `required_mbl_t`, optional `selected_mbl_t`, optional `adequate` | [src/digitalmodel/tug/towline.py:L24-L34] |
| `speed_amplification_factor` | Nonnegative `assist_speed_kn`, positive `reference_speed_kn` by comparisons; `exponent`, `cap` and finite values unchecked; the example supplies every parameter explicitly | Dimensionless float from upper/lower clipping of speed-ratio power; section 4 demonstrates synthetic parameters only | [src/digitalmodel/tug/towline.py:L37-L65] |
| `towline_load` | Positive `bollard_pull_t`, `dynamic_factor` at least unity by comparisons; case-normalized `society` must be a stored key; optional `selected_mbl_t` unchecked | `TowlineLoadResult`; stored class factor multiplies design load; absent selection returns `None` for adequacy; no rule-edition qualification | [src/digitalmodel/tug/towline.py:L68-L114] |
| `snapback_recoil_energy` | `line_load_t` unchecked; `line_length_m`, `line_mbl_t`, `elongation_at_break` positive by comparisons; elongation default exists | kJ linear-spring energy proxy; no recoil velocity, exclusion-zone geometry or injury criterion | [src/digitalmodel/tug/towline.py:L117-L148] |

The source-file census also includes the initializer described in section 1.
`constants.py` defines the enum above and the following data: `BP_FACTOR_T_PER_KW`,
`ASTERN_FRACTION`, `CLASS_TOWLINE_SAFETY_FACTOR`, `DNV_TRANSVERSE_PULL_FRACTION`,
`TRANSVERSE_PULL_FRACTION`, `FUEL_CO2_FACTOR`, `FUEL_LHV_MJ_PER_KG`,
`GRAVITY_M_S2` and `SEAWATER_DENSITY_T_M3`. These are data, not AST function/class
definitions. Their numbers are deliberately not reproduced as qualified inputs.
[src/digitalmodel/tug/constants.py:L25-L85]

`SFOC_MGO_G_PER_KWH` is a separate empirical default in `emissions.py`.
[src/digitalmodel/tug/emissions.py:L22-L24]

## 3. Method and governing sources

The implementation routes are:

- Power and propulsion factors produce bollard pull; a caller override changes
  only the ahead factor. Escort forces use dynamic pressure and supplied hull
  area/coefficients. [src/digitalmodel/tug/bollard_pull.py:L53-L142]
- Girting compares sampled GZ against a cosine heeling arm, interpolates the
  first rising equilibrium and bisects a load bound. A strict margin/load
  comparison creates the returned boolean.
  [src/digitalmodel/tug/stability_girting.py:L92-L244]
- Fender height is a grid search over mean vertical overlap; berthing energy
  is a coefficient-weighted kinetic-energy screen.
  [src/digitalmodel/tug/fendering.py:L61-L160]
- Towline speed amplification is a clipped power law. Required MBL is a stored
  factor times amplified pull; snapback is a linear-spring energy proxy.
  [src/digitalmodel/tug/towline.py:L37-L148]
- Duty-cycle energy is summed by phase, battery demand is the portion above
  baseline, and fuel/emissions estimates use stored fuel factors and an encoded
  RPM-based limit. [src/digitalmodel/tug/emissions.py:L59-L188]

No accessible primary rule text is supplied with this change. Generic standards
mentions in source docstrings do not establish title, edition, applicability or
qualified numeric criteria. A primary-source review and project-specific input
basis are required before using the encoded defaults for an engineering decision.

## 4. Worked examples and regression examples

Example E1 is synthetic arithmetic, based on the independent hand calculation
in the executable docstring below. The reference speed, exponent and cap are
synthetic, not recommendations. The floor and cap operations describe the public
implementation [src/digitalmodel/tug/towline.py:L61-L65]. No towline class factor,
bollard-pull assumption, emission factor or stability criterion is exercised.

```python
"""E1: independent square-law calculation with synthetic parameters.

Reference speed = 2 knots, exponent = 2, cap = 4 (all synthetic).
For speeds [0, 1, 2, 3, 4, 6] knots, the dimensionless speed ratios are
[0, 1/2, 1, 3/2, 2, 3]. Squaring by hand gives raw ratios
[0, 1/4, 1, 9/4, 4, 9] = [0, 0.25, 1, 2.25, 4, 9].
For raw <= 1 the floor gives 1; for 1 < raw < 4 the factor equals raw;
for raw >= 4 the cap gives 4. Thus 1 <= factor <= 4 and the factors are
[1, 1, 1, 2.25, 4, 4]. The exact fractions are binary-representable;
no empirical tolerance is needed. Basis: these hand calculations only.
"""
from digitalmodel.tug.towline import speed_amplification_factor

speeds_kn = (0, 1, 2, 3, 4, 6)
reference_speed_kn = 2
exponent = 2
cap = 4
factors = tuple(
    speed_amplification_factor(
        assist_speed_kn=speed,
        reference_speed_kn=reference_speed_kn,
        exponent=exponent,
        cap=cap,
    )
    for speed in speeds_kn
)
```

The executable test reads this block from the pack, checks the explicit inputs
and compares its result with independently squared rational values and the
hand-derived clamped vector. Merely repeating the production formula is not the
test oracle. [tests/docs/test_tug_agent_pack.py:L92-L112]

## 5. QA gates and executable checks

Use the existing AST checker directly:

```bash
uv run --no-project python scripts/docs/check_agent_pack_functions.py --pack src/digitalmodel/tug/docs/agent-pack.md
```

Use the named test roots with scratch supplied by the launch environment:

```bash
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/tug tests/docs/test_tug_agent_pack.py tests/docs/test_agent_pack_functions.py tests/test_main_module_routing.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/tug-pack-checks"
```

The package-specific checks run the unchanged checker on this real pack, verify
the tug registry pointer, execute E1 and require the ten-section form and named
NOT_EVALUATED disciplines. Copied synthetic packages calibrate added-function,
stale-definition and public-member drift; the real source is never edited by
these negative tests. This is inventory consistency, not equation verification.
[tests/docs/test_tug_agent_pack.py:L43-L129]

## 6. Known failure modes and pitfalls

- Most physical calculations rely on sign comparisons rather than finite-value
  checks. NaN, infinity, wrong units and physically inconsistent inputs are not
  generally rejected. Duty-cycle power/profile checks are an explicit exception.
  [src/digitalmodel/tug/emissions.py:L75-L91]
- The speed function does not validate exponent or cap. A cap below the unity
  floor can return below-floor factors; a negative exponent at zero speed can
  raise `ZeroDivisionError`. E1 does not qualify these domains.
  [src/digitalmodel/tug/towline.py:L61-L65]
- Supplying a bollard-pull factor does not override astern/transverse fractions.
  A comparison flag does not establish an operating limit or class compliance.
  [src/digitalmodel/tug/bollard_pull.py:L75-L90]
- Fender search requires an ordered finite contact-band basis and positive
  finite resolution from the caller. Zero resolution divides by zero; negative
  resolution does not define a meaningful search. Tied optima retain the first
  scanned centre; no continuous optimum is established.
  [src/digitalmodel/tug/fendering.py:L84-L110]
- Girting needs ordered, nonempty, physically qualified GZ samples. The critical
  load depends on sampled bounds and interpolation; equality at the deck edge
  has no positive margin. A last-sample angle returned without a crossing is
  not evidence of self-righting. [src/digitalmodel/tug/stability_girting.py:L92-L266]
- `None` for absent selected MBL or missing equilibrium is not a measured zero.
  Nonpositive fender rating yields infinite utilization. Dataclass construction
  alone establishes no domain validity.
  [src/digitalmodel/tug/towline.py:L101-L114]
  [src/digitalmodel/tug/stability_girting.py:L221-L244]
  [src/digitalmodel/tug/fendering.py:L147-L160]
- Battery sizing omits efficiencies, reserve, degradation and transient response.
  Fuel comparisons omit lifecycle accounting; negative or nonfinite SFOC/NOx
  inputs are not comprehensively rejected.
  [src/digitalmodel/tug/emissions.py:L111-L188]
- Spring energy does not establish recoil dynamics or a personnel exclusion zone.
  A negative line-load input is squared rather than rejected.
  [src/digitalmodel/tug/towline.py:L136-L148]

## 7. Lessons log

- A complete symbol table prevents unnoticed API drift; it does not validate
  formulas, coefficients or engineering applicability.
- Explicit synthetic parameters keep arithmetic regression evidence separate
  from defaults whose source and applicability are NOT_EVALUATED.
- Existing assertion names that mention a standard or compliance demonstrate
  encoded behavior only; qualification needs a primary source and governing case.

## 8. Maintenance

The maintainer will keep this pack, the tug registry entry and package-specific
consistency tests aligned with the source. A public API change will require an
inventory update and a fresh checker run. A numerical qualification claim will
require independently sourced criteria and evidence for its intended use.

## 9. Dependencies and other implementations

The pack imports no package during AST inventory. The E1 runtime imports only
the towline module and its constants/dataclass dependencies.
[scripts/docs/check_agent_pack_functions.py:L76-L122]
[src/digitalmodel/tug/towline.py:L18-L20]

`stability_girting` imports and re-exports `compute_gz_curve` and
`compute_area_under_gz` from
`digitalmodel.naval_architecture.floating_platform_stability`. These are not
definitions authored inside this package and need no duplicate inventory row.
Call the tug APIs through their submodules; the initializer has no dispatcher,
CLI or engine registration. No alternative implementation, licensed solver or
external benchmark is qualified here.
[src/digitalmodel/tug/stability_girting.py:L23-L26]
[src/digitalmodel/tug/stability_girting.py:L269-L278]

## 10. Coverage and open items

The following is executable test coverage, not measured engineering performance:

| Existing test file | Supported observation | Evidence |
|---|---|---|
| `test_tug_toolkit.py` | Pull/escort behavior, sampled girting, contact/energy screens, towline factors and recoil trend, duty-cycle and emissions behavior | [tests/tug/test_tug_toolkit.py:L59-L315] |
| `test_towline_explicit_parameters.py` | Explicit reference/exponent behavior and clipping around explicit caps | [tests/tug/test_towline_explicit_parameters.py:L19-L132] |
| `test_girting_similarity.py` | Force/displacement scaling and zero-margin deck-edge behavior on synthetic curves | [tests/tug/test_girting_similarity.py:L48-L93] |
| `test_fender_geometry_invariants.py` | Contact-band translation and input-permutation invariance on synthetic geometry | [tests/tug/test_fender_geometry_invariants.py:L63-L101] |
| `test_hybrid_input_validation.py` | Invalid phase/power rejection, phase endpoints and independent energy/peak-shaving arithmetic | [tests/tug/test_hybrid_input_validation.py:L17-L117] |
| `test_tug_agent_pack.py` | Real inventory/registry consistency, synthetic drift rejection, exact E1 arithmetic and pack form | [tests/docs/test_tug_agent_pack.py:L43-L129] |

At the code pin, the prescribed pytest-only environment collects the following
baseline. Counts are observations from the named command in section 5, before
the new test file is included; they are not constants used by calculations.

| Test root | Collected | Passed | Failed | Skipped |
|---|---|---|---|---|
| `tests/tug` | 116 | 116 | 0 | 0 |
| `tests/docs/test_agent_pack_functions.py` | 19 | 19 | 0 | 0 |
| `tests/test_main_module_routing.py` | 9 | 4 | 5 | 0 |

Table: baseline execution observations at the code pin in the pytest-only environment.

With the new test file included, the same named roots collect 148 items:
143 pass, the same 5 routing cases fail, and 0 skip. The new file contributes
4 passing checks. These are execution observations from section 5's command;
they establish the scoped regressions only, not full-suite coverage.

The routing failures are `test_known_console_script_resolves` (distribution
entry-point metadata absent), and `test_known_subcommand_dispatches`,
`test_existing_file_goes_to_engine`, `test_unknown_non_file_arg_keeps_engine_contract`
and `test_no_args_goes_to_engine` (the run-contract import reaches an unavailable
`pandas` dependency). The relevant source and tests match the code pin; no
routing fix or dependency change is part of this pack.
[tests/test_main_module_routing.py:L19-L59]
[src/digitalmodel/__main__.py:L51-L64]
[src/digitalmodel/__main__.py:L117-L117]
[src/digitalmodel/hydrodynamics/diffraction/orcaflex_exporter.py:L12-L12]

| Discipline | Status | Missing basis and next qualification step |
|---|---|---|
| Towline class factors | NOT_EVALUATED | Accessible primary rule edition, applicability and line/equipment basis; source the governing rule before accepting required MBL. |
| Bollard-pull assumptions | NOT_EVALUATED | Qualified propulsion/power factors, measured pull and escort coefficients; establish a documented input basis and validation envelope. |
| Emission factors | NOT_EVALUATED | Accessible primary factor/limit source, fuel basis and engine data; qualify operational applicability and separate lifecycle scope. |
| Stability/girting criteria | NOT_EVALUATED | Qualified GZ curve, loading case, downflooding limit and governing rule; review sampling and equilibrium criteria against that case. |
| Fender and spring proxies | NOT_EVALUATED | Equipment/contact data, energy coefficients, material response and applicable criteria; qualify these before operational decisions. |

Table: engineering qualification gaps; no acceptance criterion is established by the arithmetic example.

The independent example covers only speed-factor floor, interior and cap behavior
with finite synthetic inputs. The full repository suite, external benchmarks,
imported GZ-helper implementation, live equipment response, transient tow dynamics,
material recoil dynamics and environmental-load coupling are uninspected here.
The parent issue remains open for the other packages and qualification follow-up.
