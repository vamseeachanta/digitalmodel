# Tug module knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/tug` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` (`origin/main` at inspection) |
| Revision / status | 0 / DRAFT; arithmetic contracts, engineering qualification incomplete |
| QA family | EC; proposed module checks below |

The toolkit estimates pull, heeling, contact, line loading and duty-cycle demand.
Empirical propulsion, class and fuel factors remain NOT_EVALUATED. The way forward
is to qualify those inputs before treating a returned boolean as acceptance.

### Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-22 | Record pinned contracts, independent arithmetic and provenance gaps for review. |

## 1. Purpose and scope

All Python files in this package are covered. The empty package facade documents
submodules but exports no calculation functions itself.
[src/digitalmodel/tug/__init__.py:L1-L12]
The pack does not alter the algorithms or the separate duty-cycle report work in
PR #3256. Default empirical values are deliberately not reproduced without an
opened source. NOT_EVALUATED means their numerical provenance/applicability was
not established; UNVERIFIED means an implemented method lacks independent
engineering confirmation. Neither status is a passing engineering check.

## 2. Inputs and outputs

Inventory keys are `file_stem.symbol`; explicit `__all__` re-exports are included.
Imported implementation conveniences without `__all__` are not public API claims.
All inputs are required unless a default is stated. Result dataclasses simply
store the named fields and apply no validation or unit conversion. SI prefixes,
tonnes and standard gravity conversions follow [NIST SP 811 (2008), Appendix
B.8](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8).
Default numerical values marked NOT_EVALUATED are available only at their code
locators; they are not recommendations.

| Public function / class | Input, units, validation and defaults | Output contract | Evidence |
|---|---|---|---|
| `constants.Propulsion` | Enum key: conventional, conventional_nozzle, asd, tractor_vsp, rotor; invalid key raises; no default | String-valued propulsion enum | [src/digitalmodel/tug/constants.py:L15-L22] |
| `bollard_pull.BollardPullResult` | propulsion string; installed_power_kw in kW; bp_ahead_t, bp_astern_t, transverse_pull_t in tonnes-force; all required, unchecked | Mutable record of those fields | [src/digitalmodel/tug/bollard_pull.py:L32-L40] |
| `bollard_pull.EscortForceResult` | speed_kn in knots, steering_force_t and braking_force_t in tonnes-force, exceeds_bollard_pull boolean; required, unchecked | Mutable record | [src/digitalmodel/tug/bollard_pull.py:L43-L50] |
| `bollard_pull.bollard_pull` | installed_power_kw > zero; propulsion lowercased and enum-checked; factor_t_per_kw defaults None, selecting BP_FACTOR_T_PER_KW (NOT_EVALUATED); explicit factor unchecked | BollardPullResult; ahead = power × factor, astern and transverse use propulsion tables | [src/digitalmodel/tug/bollard_pull.py:L53-L90] |
| `bollard_pull.escort_force` | speed_kn ≥ zero, lateral_area_m2 > zero; bp_ahead_t only comparison; lift_coefficient and drag_coefficient defaults NOT_EVALUATED; coefficient signs/finiteness unchecked | EscortForceResult; tonnes-force; flag compares steering only with supplied ahead pull | [src/digitalmodel/tug/bollard_pull.py:L93-L142] |
| `stability_girting.GirtingResult` | towline_force_t (tonnes-force), tow_point_lever_m, equilibrium_heel_deg or None, deck_edge_angle_deg, margin_deg or None, critical_towline_t, girting_safe; required, unchecked | Mutable result, missing equilibrium/margin means no crossing found | [src/digitalmodel/tug/stability_girting.py:L29-L41] |
| `stability_girting.heeling_arm_curve` | towline_force_t, tow_point_lever_m, displacement_t > zero (tonnes); heel_angles_deg defaults None → inclusive upright-to-right-angle software grid; force/lever/angles unchecked | List of (degrees, arm metres) | [src/digitalmodel/tug/stability_girting.py:L44-L74] |
| `stability_girting.critical_towline_force` | gz_curve (degrees, m), tow_point_lever_m > zero, displacement_t, deck_edge_angle_deg; other validation delegated/incomplete; no defaults | Bisection limit in tonnes-force, zero if no positive candidate sampled | [src/digitalmodel/tug/stability_girting.py:L125-L183] |
| `stability_girting.assess_girting` | gz_curve, towline_force_t, tow_point_lever_m, displacement_t, deck_edge_angle_deg; no defaults; inherits helpers' checks | GirtingResult; safe requires positive angular margin and force strictly below computed limit | [src/digitalmodel/tug/stability_girting.py:L186-L244] |
| `stability_girting.angle_of_vanishing_stability` | gz_curve (degrees, m), unchecked, no default | First positive-to-nonpositive crossing, else final sampled angle, degrees | [src/digitalmodel/tug/stability_girting.py:L247-L258] |
| `stability_girting.is_self_righting` | gz_curve; threshold_deg default NOT_EVALUATED; unchecked | Boolean comparing the above angle to threshold; no full stability certification | [src/digitalmodel/tug/stability_girting.py:L261-L266] |
| `stability_girting.compute_gz_curve` | Re-export: gm_m; heel_angles_deg=None → software grid; kg_m=None, kn_values=None; no range/finite checks | (degree, GZ m) pairs; GM sine approximation or indexed cross-curve correction | [src/digitalmodel/naval_architecture/floating_platform_stability.py:L150-L186] |
| `stability_girting.compute_area_under_gz` | Re-export: gz_curve, angle_start, angle_end in degrees; unchecked | Trapezoidal area in m rad; zero for fewer than two selected samples; no boundary interpolation | [src/digitalmodel/naval_architecture/floating_platform_stability.py:L189-L209] |
| `fendering.AssistedVessel` | name string, contact_low_m, contact_high_m relative to waterline; required, unchecked | Mutable contact-band record | [src/digitalmodel/tug/fendering.py:L20-L26] |
| `fendering.FenderHeightResult` | fender_height_m, fender_face_height_m, mean_overlap_fraction, worst_vessel, worst_overlap_fraction, all_vessels_contact; required, unchecked | Mutable metres/fractions/string/boolean record | [src/digitalmodel/tug/fendering.py:L29-L38] |
| `fendering.BerthingEnergyResult` | berthing_energy_kj, fender_rated_energy_kj, utilisation (dimensionless), adequate boolean; required, unchecked | Mutable result | [src/digitalmodel/tug/fendering.py:L41-L48] |
| `fendering.optimize_fender_height` | nonempty contact-band list; fender_face_height_m > zero; search_step_m software default at locator (NOT_EVALUATED resolution); step and band ordering unchecked | FenderHeightResult maximising mean overlap; earliest scanned candidate wins ties | [src/digitalmodel/tug/fendering.py:L61-L110] |
| `fendering.berthing_energy` | displacement_t > zero, approach_speed_m_s ≥ zero, fender_rated_energy_kj; mass_coefficient, eccentricity_coefficient, softness_coefficient defaults NOT_EVALUATED; coefficients unchecked | BerthingEnergyResult; nonpositive rating → infinite utilisation; adequate when utilisation ≤ unity | [src/digitalmodel/tug/fendering.py:L113-L160] |
| `towline.TowlineLoadResult` | static_load_t, dynamic_factor, design_load_t, society, safety_factor, required_mbl_t, selected_mbl_t or None, adequate or None; required, unchecked | Mutable record; loads in tonnes-force, factors dimensionless | [src/digitalmodel/tug/towline.py:L23-L34] |
| `towline.speed_amplification_factor` | assist_speed_kn ≥ zero, reference_speed_kn > zero; reference_speed_kn, exponent, cap defaults NOT_EVALUATED; exponent/cap unchecked | min(cap,max(unity,speed ratio to exponent)); dimensionless | [src/digitalmodel/tug/towline.py:L37-L65] |
| `towline.towline_load` | bollard_pull_t > zero, society='LR' (uppercased/checked); dynamic_factor defaults identity and must ≥ identity; selected_mbl_t=None | TowlineLoadResult; adequate None without selection; empirical safety factor NOT_EVALUATED | [src/digitalmodel/tug/towline.py:L68-L114] |
| `towline.snapback_recoil_energy` | line_load_t; line_length_m, line_mbl_t and elongation_at_break > zero; elongation default NOT_EVALUATED; negative/overbreaking load unchecked | Linear-elastic energy proxy in kJ | [src/digitalmodel/tug/towline.py:L117-L148] |
| `emissions.OperatingPhase` | name, power_fraction (dimensionless), duration_h; required, unchecked | Mutable phase; declared fraction range is not enforced | [src/digitalmodel/tug/emissions.py:L26-L32] |
| `emissions.HybridSizingResult` | total_energy_kwh, peak_power_kw, baseline_engine_kw, battery_power_kw, battery_energy_kwh, peak_energy_fraction; required, unchecked | Mutable result; usable excess energy, no recharge model | [src/digitalmodel/tug/emissions.py:L35-L44] |
| `emissions.EmissionsResult` | fuel_mass_t, co2_t, nox_rate_g_per_kwh, tier_iii_nox_limit_g_per_kwh, tier_iii_compliant; required, unchecked | Mutable mass/rate/boolean record | [src/digitalmodel/tug/emissions.py:L47-L55] |
| `emissions.imo_tier_iii_nox_limit` | rated_rpm > zero; no default or finite check | Speed-dependent limit in g/kWh; table in section 3 | [src/digitalmodel/tug/emissions.py:L58-L71] |
| `emissions.cycle_energy` | installed_power_kw > zero, nonempty profile; phase signs/ranges/finiteness unchecked; no defaults | Sum of phase power × duration, kWh | [src/digitalmodel/tug/emissions.py:L74-L80] |
| `emissions.size_hybrid` | installed_power_kw, profile as above; baseline_fraction strictly between zero and unity, default NOT_EVALUATED design assumption | HybridSizingResult; excess above fixed engine baseline only | [src/digitalmodel/tug/emissions.py:L83-L123] |
| `emissions.fuel_and_emissions` | installed_power_kw, profile; sfoc_g_per_kwh=None → LHV-scaled reference (NOT_EVALUATED); fuel='mgo', key checked; nox_rate_g_per_kwh and rated_rpm defaults NOT_EVALUATED operating assumptions | EmissionsResult; fuel mass, tank-to-wake CO2 and numerical NOx comparison; no applicability/certification decision | [src/digitalmodel/tug/emissions.py:L126-L177] |

## 3. Method and governing standards

Public sources below were opened on 2026-10-22. Live web manuals have no numbered
edition; the access date identifies the inspected version. Bibliographic mentions
inside code are not proof that a rule, edition or clause was evaluated.

| Method / constant inventory | Basis and status | Evidence |
|---|---|---|
| Ahead BP = factor × power; astern/transverse = fraction × ahead | Algebra contract only; BP_FACTOR_T_PER_KW, ASTERN_FRACTION, TRANSVERSE_PULL_FRACTION and DNV_TRANSVERSE_PULL_FRACTION: every entry NOT_EVALUATED; no opened propulsion trial or class clause | [src/digitalmodel/tug/constants.py:L25-L67] [src/digitalmodel/tug/bollard_pull.py:L75-L82] |
| Escort dynamic pressure and coefficient forces | F = rho V² A C / 2; generic dynamic-pressure construction UNVERIFIED for this hull; density, default lift/drag coefficients NOT_EVALUATED; no opened calibration source | [src/digitalmodel/tug/bollard_pull.py:L93-L135] |
| Force/unit conversions | NIST SP 811 (2008), Appendix B.8, standard acceleration, knot, tonne entries: GRAVITY_M_S2 = 9.80665 m/s²; tonne = 1000 kg; knot conversion in code is truncated relative to NIST | [src/digitalmodel/tug/constants.py:L84-L85] [src/digitalmodel/tug/bollard_pull.py:L125-L135] |
| SEAWATER_DENSITY_T_M3 | NOT_EVALUATED: fixed water density, no temperature/salinity basis opened; value not reproduced | [src/digitalmodel/tug/constants.py:L84-L85] |
| HA = (force/displacement) lever cos(heel) | Derived torque balance, [OpenStax University Physics vol. 1 (2016), §10.6, Defining Torque](https://openstax.org/books/university-physics-volume-1/pages/10-6-torque); force and displacement weight conversion cancels; tug applicability UNVERIFIED | [src/digitalmodel/tug/stability_girting.py:L68-L74] |
| Equilibrium, critical force, AVS, self-righting | First crossing with linear interpolation; bounded bisection with fixed iteration count and upper-bound multiplier; AVS first downward zero crossing; numerical choices and default self-righting threshold NOT_EVALUATED as acceptance criteria | [src/digitalmodel/tug/stability_girting.py:L92-L183] [src/digitalmodel/tug/stability_girting.py:L247-L266] |
| GZ and GZ area re-exports | GM sin(heel) or KN − KG sin(heel), selected-sample trapezoids; NOT_EVALUATED for large-angle stability; no opened applicable stability rule | [src/digitalmodel/naval_architecture/floating_platform_stability.py:L150-L209] |
| Contact overlap / optimisation | Intersection length divided by face height, arithmetic mean and finite scan; mathematical definition, no empirical coefficient; search resolution NOT_EVALUATED for design convergence | [src/digitalmodel/tug/fendering.py:L51-L110] |
| Berthing E = mass × speed² × Cm × Ce × Cs / 2 | Kinetic-energy term: [OpenStax vol. 1 (2016), §7.2, eq. 7.6](https://openstax.org/books/university-physics-volume-1/pages/7-2-kinetic-energy); every default correction factor NOT_EVALUATED; PIANC WG33/WG145 edition/clause not opened, no compliance claim | [src/digitalmodel/tug/fendering.py:L113-L160] |
| Speed amplification, design load and MBL | Capped speed power law, BP × amplification, then × class factor; every CLASS_TOWLINE_SAFETY_FACTOR entry and default reference speed/exponent/cap NOT_EVALUATED; class clause not opened | [src/digitalmodel/tug/constants.py:L46-L51] [src/digitalmodel/tug/towline.py:L37-L114] |
| Snap-back energy = F²/(2k), k = MBL/(length × break strain) | [OpenStax vol. 1 (2016), §8.1, Elastic Potential Energy](https://openstax.org/books/university-physics-volume-1/pages/8-1-potential-energy-of-a-system), linear-spring energy; default strain and conversion from nonlinear rope behaviour NOT_EVALUATED | [src/digitalmodel/tug/towline.py:L117-L148] |
| Cycle energy and hybrid demand | E = sum(P × fraction × time); peak excess and sum of positive excess energy; [OpenStax vol. 1 (2016), §7.4, eqs. 7.10–7.11](https://openstax.org/books/university-physics-volume-1/pages/7-4-power); baseline fraction NOT_EVALUATED design input; no battery losses/reserve/recharge | [src/digitalmodel/tug/emissions.py:L74-L123] |
| Fuel and CO2 | Energy × SFOC, then mass × carbon factor; reference SFOC_MGO_G_PER_KWH, all FUEL_CO2_FACTOR and FUEL_LHV_MJ_PER_KG entries NOT_EVALUATED; values not reproduced; inverse-LHV same-efficiency assumption UNVERIFIED | [src/digitalmodel/tug/constants.py:L69-L82] [src/digitalmodel/tug/emissions.py:L21-L23] [src/digitalmodel/tug/emissions.py:L157-L176] |
| Tier III numerical limit | [IMO, MARPOL Annex VI, Regulation 13, NOx Technical Code 2008 context, public Tier table](https://www.imo.org/en/ourwork/environment/pages/nitrogen-oxides-(nox)-%E2%80%93-regulation-13.aspx): 3.4 g/kWh for n < 130 rpm; 9 n^(−0.2) for 130 ≤ n < 2000; 2.0 for n ≥ 2000. Opened public summary, not a licensed standard. Numerical limit only; applicability and certification omitted | [src/digitalmodel/tug/emissions.py:L58-L71] [src/digitalmodel/tug/emissions.py:L169-L176] |

## 4. Validated worked examples and regression examples

Synthetic inputs have no operational acceptance meaning. Literal expectations in
the new tests come from the following independent arithmetic, not calls to another
implementation. Floating-point comparisons permit roundoff only.

| Example | Inputs → hand method → answer | Test node ID |
|---|---|---|
| Explicit ahead factor (algebra regression) | 1000 kW × synthetic 0.02 tonnes-force/kW = 20.0 tonnes-force. Astern/transverse factors remain NOT_EVALUATED | `tests/tug/test_agent_pack_examples.py::test_bollard_ahead_explicit_factor` |
| Upright heeling arm | 10 tonnes-force, displacement 100 tonnes, lever 2 m, angle zero: (10/100) × 2 × cos(0) = 0.2 m; §10.6 torque derivation above | `tests/tug/test_agent_pack_examples.py::test_heeling_arm_upright` |
| Identity berthing corrections | 100 tonnes, 2 m/s, all correction factors unity, synthetic rating 400 kJ: (100000 × 4 / 2)/1000 = 200.0 kJ; utilisation 200/400 = 0.5; §7.2 above | `tests/tug/test_agent_pack_examples.py::test_berthing_energy_unit_coefficients` |
| Hybrid cycle | 100 kW, low fraction 0.25 for 2 h, full fraction for 1 h: E = 100(0.25×2 + 1×1) = 150.0 kWh. Explicit baseline fraction 0.5 → engine 50.0 kW, peak 100.0 kW, battery 50.0 kW and 50.0 kWh, share 1/3; §7.4 above | `tests/tug/test_agent_pack_examples.py::test_hybrid_cycle_energy_and_peak` |
| Contact geometry | Band [0,2] m, face 2 m, step 1 m: centres 0,1,2 give overlaps 1/2,1,1/2. Best centre 1.0 m, mean/worst fractions 1.0 | `tests/tug/test_agent_pack_examples.py::test_fender_overlap_identity` |
| Explicit speed-law inputs (algebra regression) | Speed 2 kn, reference 1 kn, exponent 2, cap 5: min(5,max(1,(2/1)²)) = 4.0; no physical calibration asserted | `tests/tug/test_agent_pack_examples.py::test_speed_amplification_synthetic_inputs` |

## 5. QA gates and executable checks

Run `PYTHONPATH=src uv run --no-project --with pytest --with numpy --with scipy
--with pyyaml --with pytest-asyncio python -m pytest tests/tug -q -rA
-p no:cacheprovider --basetemp=<scratch>/tug` (one shell line).

| Gate | Executable check | Limit |
|---|---|---|
| Input refusal | `tests/tug/test_tug_toolkit.py::TestBollardPull::test_zero_power_rejected` | Tested power boundary only |
| Heeling arithmetic | `tests/tug/test_agent_pack_examples.py::test_heeling_arm_upright` | Torque construction, no intact-stability approval |
| Energy arithmetic | `tests/tug/test_agent_pack_examples.py::test_berthing_energy_unit_coefficients` | Synthetic identity factors |
| Demand accounting | `tests/tug/test_agent_pack_examples.py::test_hybrid_cycle_energy_and_peak` | No battery system acceptance |
| Public inventory / locators | `tests/docs/test_marine_agent_packs.py::test_new_pack_contracts` | Static AST and file bounds, no physics qualification |
| Every cited test collects | `tests/docs/test_marine_agent_packs.py::test_all_new_pack_node_ids_collect` | All sections, including QA gate IDs |

## 6. Known failure modes and pitfalls

| Evidence | Defect / limit | Required follow-up |
|---|---|---|
| [src/digitalmodel/tug/bollard_pull.py:L71-L82] | Power/factor finiteness and factor sign unchecked; unsourced astern/transverse factors always applied | Finite inputs and qualified propulsion factors |
| [src/digitalmodel/tug/bollard_pull.py:L122-L135] | Fixed density, unchecked force/coefficient signs/finiteness; truncated knot conversion | Qualified inputs and explicit unit basis |
| [src/digitalmodel/tug/stability_girting.py:L68-L74] | Negative/nonfinite force, lever and angles accepted | Define physical input envelope |
| [src/digitalmodel/tug/stability_girting.py:L106-L122] | Zero force returns upright before examining GZ; empty, unordered or nonfinite curves not validated | Validate GZ and displacement before screening |
| [src/digitalmodel/tug/stability_girting.py:L150-L183] | Sample-dependent critical limit; no full curve or convergence checks | Establish resolution and equilibrium envelope |
| [src/digitalmodel/tug/stability_girting.py:L247-L266] | No zero crossing returns final angle; this can label truncated positive curve self-righting | Require complete righting-range evidence |
| [src/digitalmodel/naval_architecture/floating_platform_stability.py:L176-L183] | Re-export selects KN by integer index on an assumed grid; missing KN/KG silently chooses GM sine approximation | Validate cross-curve grid and method selection |
| [src/digitalmodel/naval_architecture/floating_platform_stability.py:L195-L207] | Area excludes unprovided endpoints instead of interpolating | Require endpoint coverage |
| [src/digitalmodel/tug/fendering.py:L79-L93] | Search step not checked; zero divides, negative step can miss range; reversed bands allowed | Validate step and bands; demonstrate convergence |
| [src/digitalmodel/tug/fendering.py:L136-L159] | Negative/nonfinite coefficients and NaN rating unchecked; negative energy can satisfy adequacy | Refuse invalid coefficients and ratings |
| [src/digitalmodel/tug/towline.py:L62-L65] | Cap below unity violates documented minimum; exponent/finiteness unchecked | Validate explicit load-law inputs |
| [src/digitalmodel/tug/towline.py:L89-L103] | Nonfinite load/factor/MBL not checked; unqualified class factors control adequacy | Qualify class rule and finite inputs |
| [src/digitalmodel/tug/towline.py:L139-L148] | Negative line load squares to positive energy; load above MBL accepted; nonlinear recoil omitted | Bound loads and qualify line constitutive model |
| [src/digitalmodel/tug/emissions.py:L74-L80] | Negative duration/fraction and fractions above unity accepted; can yield negative energy | Validate operating profile |
| [src/digitalmodel/tug/emissions.py:L109-L122] | Excess-only energy omits recharge, reserve and efficiency | Separate battery system design |
| [src/digitalmodel/tug/emissions.py:L65-L71] | NaN rated speed reaches final limit branch | Reject all nonfinite speeds |
| [src/digitalmodel/tug/emissions.py:L157-L176] | Negative/nonfinite SFOC or NOx unchecked; returned compliance omits regulatory applicability and certification | Validate rates; keep arithmetic distinct from compliance |

## 7. Lessons log

A correctly evaluated product is not evidence for an empirical coefficient.
Upright heeling and identity-factor energy isolate the arithmetic from those
coefficients. A sampled curve's last point is not proof of vanishing stability.
The regression tests preserve current behaviour; defects above remain open.

## 8. Ownership

Maintainers of the tug package maintain its inventory and tests. A qualified
marine engineering reviewer must approve propulsion data, class rules, GZ,
fender data, line properties and emissions applicability before engineering use.
No named reviewer or approval is implied by this draft.

## 9. Dependencies and other implementations

Standard-library math/dataclasses/enum and the naval-architecture GZ helpers are
the calculation dependencies. Pytest supplies test execution; YAML is needed by
the registry checks. No licensed solver is required or used.
[src/digitalmodel/tug/bollard_pull.py:L20-L29]
[src/digitalmodel/tug/stability_girting.py:L20-L26]
The routed seakeeping and passing-ship workflows have separate packs and do not
supply towing-system acceptance criteria.

## 10. Coverage and open items

All public local classes/functions and the two explicit stability re-exports
appear in section 2. Six independent arithmetic examples cover ahead pull,
heeling, berthing, hybrid accounting, contact and an explicit speed law. Existing
tug regressions also exercise astern, escort, critical force, MBL, recoil and
emissions; they do not independently qualify unsourced factors. Every constants
mapping, density, gravity and reference SFOC has a status in section 3.

Open: qualify every NOT_EVALUATED coefficient; add finite/range guards in a
separate code change; validate escort and girting against independent physical
cases; resolve the documented fail-open paths; model recharge/reserves and
regulatory applicability. No source value is inferred from an unread standard.
