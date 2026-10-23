# Subsea connectors knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/subsea/connectors` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Revision / status | 0 / DRAFT; arithmetic regression evidence only |
| Analysis family | EC |
| Scope authority | Issue #3335 |

This pack documents the pinned implementation. It supplies no equipment approval,
qualified operational limit or verified licensed-standard clause basis.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 0 | 2026-10-22 | Record existing contracts, independent examples and qualification gaps. |

## 1. Purpose and scope

Homogeneous tubular end-cap thrust, combined body stress, seal/preload margin, thermal restraint, elastic curvature and signed-history fatigue handoff. API 17R and ISO 13628-1 are named in code, without a verified edition or clause basis. Family labels do not qualify equipment; flexible-layer stresses, minimum bend radius and full model assembly remain outside the implemented checks. [src/digitalmodel/subsea/connectors/__init__.py:L4-L13]

## 2. Inputs and outputs

Every locally defined public class/function and public method/property is listed,
including submodule APIs absent from package exports. Explicit exported data is also
listed. Imported library helpers and private names are excluded. Defaults below are
implementation observations; NOT_EVALUATED defaults are not design recommendations.

| Public API | Inputs, units, validation and defaults | Output contract | Evidence at code pin |
|---|---|---|---|
| `PipeSection` | outer_diameter Do and wall_thickness t m; required. Do>0, 0<t<Do/2. | Frozen homogeneous annulus; ValueError on inequality failures; finite checks incomplete. | [src/digitalmodel/subsea/connectors/section_properties.py:L21-L69] |
| `PipeSection.inner_diameter` | Read-only property; stored Do,t m; no arguments/defaults. | Di=Do-2t m. | [src/digitalmodel/subsea/connectors/section_properties.py:L42-L45] |
| `PipeSection.area` | Read-only property; stored geometry; no arguments/defaults. | Steel annulus area π(Do²-Di²)/4 m². | [src/digitalmodel/subsea/connectors/section_properties.py:L47-L52] |
| `PipeSection.bore_area` | Read-only property; stored geometry; no arguments/defaults. | Bore area πDi²/4 m². | [src/digitalmodel/subsea/connectors/section_properties.py:L54-L57] |
| `PipeSection.moment_of_inertia` | Read-only property; stored geometry; no arguments/defaults. | Diametral area moment π(Do⁴-Di⁴)/64 m⁴. | [src/digitalmodel/subsea/connectors/section_properties.py:L59-L64] |
| `PipeSection.section_modulus` | Read-only property; stored geometry; no arguments/defaults. | Elastic section modulus 2I/Do m³. | [src/digitalmodel/subsea/connectors/section_properties.py:L66-L69] |
| `ConnectorType` | String enum: mechanical, clamped, welded; enum construction validates member value. | Make-up family label; no capacity data. | [src/digitalmodel/subsea/connectors/connector_design.py:L73-L78] |
| `ConnectorMaterial` | name string, smys Pa required; youngs_modulus Pa defaults 207e9 (implementation default; NOT_EVALUATED material basis). | Frozen record; no physical validation; modulus is unused by verify_connector. | [src/digitalmodel/subsea/connectors/connector_design.py:L81-L87] |
| `ConnectorCheckResult` | Required utilisation_body/seal dimensionless; f_endcap/separation/residual N; sigma_vm/allowable_stress Pa; governing_load string; passed bool; notes defaults new empty list. | Mutable result record; no direct validation; welded residual is NaN. | [src/digitalmodel/subsea/connectors/connector_design.py:L90-L108] |
| `ConnectorCheckResult.utilisation` | Read-only property; body/seal utilisations; no arguments/defaults. | Maximum utilisation, dimensionless; no finite validation. | [src/digitalmodel/subsea/connectors/connector_design.py:L105-L108] |
| `end_cap_force` | internal_pressure Pa and seal_mean_diameter m; required, no validation. | Pressure times circular seal area N, with signed pressure and squared diameter. | [src/digitalmodel/subsea/connectors/connector_design.py:L111-L126] |
| `verify_connector` | Required section, material, internal_pressure Pa. applied_axial N, applied_moment N m, preload N, seal_seating_force N default zero; seal_mean_diameter m defaults None→bore; usage_factor defaults 0.67 (NOT_EVALUATED); connector_type defaults CLAMPED. No input validation. | ConnectorCheckResult: body/seal utilisation, loads, stress, allowable, governing string, pass flag, notes. Compression excluded only from separating demand; welded skips seal; demand ratios ≤1 pass. | [src/digitalmodel/subsea/connectors/connector_design.py:L129-L228] |
| `ThermalResult` | thermal_strain dimensionless, free_growth m, restrained_stress Pa, axial_force N; all required. | Mutable unchecked record; stress is fully restrained even when reacted force is scaled. | [src/digitalmodel/subsea/connectors/thermal_expansion.py:L37-L44] |
| `thermal_expansion_load` | section, length m, delta_temperature K or °C difference required. thermal_expansion_coeff defaults 1.17e-5/K, youngs_modulus defaults 207e9 Pa (both NOT_EVALUATED); restraint_factor defaults 1. | ThermalResult; only restraint in [0,1] checked; growth=alpha*dT*L; full stress=E*alpha*dT; reacted force=stress*A*restraint. | [src/digitalmodel/subsea/connectors/thermal_expansion.py:L47-L89] |
| `BendingResult` | curvature 1/m, bending_stress Pa, bending_moment N m, utilisation dimensionless, passed bool; all required. | Mutable unchecked record. | [src/digitalmodel/subsea/connectors/bending_analysis.py:L25-L33] |
| `bending_from_radius` | section and radius m required; youngs_modulus defaults 207e9 Pa (NOT_EVALUATED); allowable_stress defaults infinity Pa. Checks radius>0 only. | BendingResult; curvature=1/R, stress=E*Do/(2R), moment=EI/R; infinite allowable gives zero utilisation/pass, zero allowable gives infinity/fail. | [src/digitalmodel/subsea/connectors/bending_analysis.py:L36-L73] |
| `JumperType` | String enum rigid/flexible/vertical; enum construction validates member value. | Family label, not qualified construction. | [src/digitalmodel/subsea/connectors/jumper_catalog.py:L20-L23] |
| `JumperSpec` | jumper_type and description required; governing_checks default empty tuple, dynamic defaults False. | Frozen unchecked family record, not equipment geometry. | [src/digitalmodel/subsea/connectors/jumper_catalog.py:L26-L33] |
| `JUMPER_CATALOG` | Mutable exported mapping keyed by JumperType; no callable inputs; seeded family labels only. | JumperSpec records with thermal/body/seal, bending and fatigue triggers; no qualified capacities. | [src/digitalmodel/subsea/connectors/jumper_catalog.py:L36-L55] |
| `get_jumper_spec` | jumper_type key required, no explicit validation. | Shared catalogue record; native KeyError if absent; string enum keys can compare equal to strings. | [src/digitalmodel/subsea/connectors/jumper_catalog.py:L58-L60] |
| `JumperFatigueResult` | stress_history_mpa list MPa, assessment AssessmentResult required. | Frozen outer record with mutable stress list and delegated assessment. | [src/digitalmodel/subsea/connectors/fatigue.py:L25-L30] |
| `assess_jumper_fatigue` | Required spec, section, signed radius_history_m m; keyword-only youngs_modulus_pa Pa, block_years and design_years years. design_fatigue_factor and stress_concentration_factor default 1; sn_curve_name defaults D, environment air. Dynamic or fatigue trigger required; ≥2 finite nonzero radii; finite E>0, SCF≥1 and section dimensions; durations/design factor validated downstream. | JumperFatigueResult with signed E*Do/(2R)*SCF/10^6 MPa and native assessment. Curve/environment selection, rainflow, Miner damage/life and acceptance delegate to fatigue; coefficient qualification NOT_EVALUATED. | [src/digitalmodel/subsea/connectors/fatigue.py:L62-L106] |

## 3. Method and governing standards

| Method / constants | Implemented relation | Basis and status |
|---|---|---|
| Annular section | Di=Do−2t; A=π(Do²−Di²)/4; bore area=πDi²/4; I=π(Do⁴−Di⁴)/64; Z=2I/Do | Independent geometric derivation: subtract disks; integrate y² over each disk for diametral I. Coefficients are geometric. Roark, eighth edition, Table 9.1 is cited by code but not opened: external source qualification NOT_EVALUATED. [src/digitalmodel/subsea/connectors/section_properties.py:L42-L69] |
| End-cap thrust and separation | Fcap=pπDs²/4; Fsep=Fcap+max(applied axial,0); body uses Fcap+signed axial | Pressure-area definition and arithmetic; API 17R/ISO 13628-1 edition/clause basis NOT_EVALUATED. Bore substitution for seal diameter is a code default, not a proven conservative geometry. [src/digitalmodel/subsea/connectors/connector_design.py:L111-L126] [src/digitalmodel/subsea/connectors/connector_design.py:L172-L180] |
| Body equivalent stress | a=(Fcap+Faxial)/A+M/Z; h=pDi/(2t); σvm=√(a²−ah+h²); allowable=eta*SMYS; U=σvm/allowable | NOT_EVALUATED: code names API 17R and ISO 13628-1 without verified edition/clause. Thin-wall hoop and one signed bending fibre only; no external pressure, shear or thick-wall qualification. [src/digitalmodel/subsea/connectors/connector_design.py:L178-L196] |
| Seal / pass rule | residual=preload−Fsep; U=seating/residual if positive seating/residual, else zero for nonpositive seating or infinity for exhausted residual; both body/seal ratios ≤1 | NOT_EVALUATED: simplified contact model and acceptance criteria lack public clause basis. Welded connections skip seal and return NaN residual. [src/digitalmodel/subsea/connectors/connector_design.py:L198-L215] |
| Implementation defaults / advisory threshold | Usage factor, modulus, expansion coefficient, D/t advisory threshold and catalogue governing-check choices | NOT_EVALUATED: no verified material, code clause or equipment basis. Defaults and advisory labels are documented in section 2, not adopted as design inputs. [src/digitalmodel/subsea/connectors/connector_design.py:L82-L88] [src/digitalmodel/subsea/connectors/connector_design.py:L129-L140] [src/digitalmodel/subsea/connectors/connector_design.py:L191-L196] [src/digitalmodel/subsea/connectors/thermal_expansion.py:L47-L54] [src/digitalmodel/subsea/connectors/jumper_catalog.py:L36-L55] |
| Thermal restraint | strain=alpha*ΔT; growth=strain*L; full stress=E*strain; reacted force=full stress*A*r | Independent homogeneous linear-bar algebra; API 17R/Roark chapter reference is incomplete and not opened: engineering applicability NOT_EVALUATED. The multiplier r is supplied, not solved from M/U geometry. [src/digitalmodel/subsea/connectors/thermal_expansion.py:L77-L89] |
| Curvature bending | κ=1/R; σ=E Do/(2R); M=EI/R; U=σ/allowable | Independent linear strain-at-distance algebra and section integration. API 17R/API 17J/API 17B/DNV-ST-F101 editions/clauses and flexible-layer applicability NOT_EVALUATED. No manufacturer MBR appears in the calculation. [src/digitalmodel/subsea/connectors/bending_analysis.py:L59-L73] |
| Cyclic fatigue handoff | signed elastic stress × supplied SCF; Pa to MPa by decimal prefix; native rainflow/Miner damage | NOT_EVALUATED: curve D/air defaults, S-N coefficients, design factor, SCF lower-bound policy and acceptance are not independently qualified here. Constant histories and signed conversion have arithmetic tests; damage validation is delegated, not duplicated. [src/digitalmodel/subsea/connectors/fatigue.py:L44-L59] [src/digitalmodel/subsea/connectors/fatigue.py:L62-L106] |

No licensed document was opened and no standard-derived table is reproduced.
The method rows deliberately retain NOT_EVALUATED where public edition/clause
support is unavailable. Example inputs are synthetic, assumed, based on previous
experience; none represents a material, equipment capacity or design limit.

## 4. Worked examples and regression evidence

1. **End-cap**: p=4 Pa, Ds=1 m. F=pπ(Ds/2)²=4π/4=**π N**, about 3.1 N.
   Pressure-area definition and circle arithmetic; API acceptance NOT_EVALUATED.
   `tests/subsea/connectors/test_agent_pack_examples.py::test_end_cap_literal`.
2. **Thermal**: Do=2 m, t=0.5 m → Di=1 m, A=3π/4 m². Explicit alpha=0.001/K,
   ΔT=10 K, L=5 m, E=1000 Pa, r=0.5. Strain=**0.01**; growth=**0.05 m**;
   full stress=**10.0 Pa**; force=10×3π/4×0.5=**15π/4 N**, about 11.8 N.
   Independent restrained-bar algebra; physical material applicability NOT_EVALUATED.
   `tests/subsea/connectors/test_agent_pack_examples.py::test_thermal_literals`.
3. **Bending**: same section, I=π(2⁴−1⁴)/64=15π/64 m⁴. Explicit E=1000 Pa,
   R=10 m, allowable=200 Pa. κ=**0.1/m**, stress=1000×1/10=**100.0 Pa**;
   M=1000×15π/(64×10)=**375π/16 N m**, about 73.6 N m; U=**0.5**, pass.
   Independent section/strain algebra; manufacturer MBR remains NOT_EVALUATED.
   `tests/subsea/connectors/test_agent_pack_examples.py::test_bending_literals`.
4. **Body and seal**: same area, zero pressure/moment; tension=3π/4 N,
   SMYS=2 Pa, eta=1; σvm=**1.0 Pa**, Ubody=**0.5**. Preload=3π/4+4 N,
   seating=2 N gives residual=**4.0 N**, Useal=**0.5**, pass. This manufactured
   uniaxial state checks arithmetic without adopting the default usage factor.
   `tests/subsea/connectors/test_agent_pack_examples.py::test_body_and_seal_literals`.
5. **Fatigue conversion**: E=10⁹ Pa, Do=0.2 m, signed radii=(10,−10,10) m, SCF=2.
   E Do/(2R)×2 gives signed (20,−20,20)×10⁶ Pa → **(20,−20,20) MPa**.
   The example asserts conversion only; no S-N damage value is claimed sourced.
   `tests/subsea/connectors/test_agent_pack_examples.py::test_fatigue_conversion_literals`.

Tests carry full-precision independently evaluated literals for symbolic π values;
rounded prose is not the assertion tolerance or an acceptance bound.

## 5. Executable QA gates

| Gate | Executable check | Meaning |
|---|---|---|
| Public contracts and pin | `tests/docs/test_subsea_agent_packs.py::test_pack_contracts[connectors]` | API list, definition ranges, source pin and headings |
| Missing API rejection | `tests/docs/test_subsea_agent_packs.py::test_missing_public_name_fails[connectors]` | Negative function-list check |
| All cited nodes collect | `tests/docs/test_subsea_agent_packs.py::test_all_example_and_gate_nodes_collect[connectors]` | Collect examples and gates without executing bodies |
| Independent force | `tests/subsea/connectors/test_agent_pack_examples.py::test_end_cap_literal` | Pressure-area arithmetic |
| Explicit allowable | `tests/subsea/connectors/test_agent_pack_examples.py::test_body_and_seal_literals` | Body and seal demand arithmetic |

Run `python -m pytest tests/subsea/connectors` for section, connector, thermal,
bending, catalogue and fatigue integration regressions. Collection requires the
native fatigue dependencies because package import eagerly imports fatigue.
Unavailable dependencies are NOT_RUN, not physical qualification or test success.

## 6. Failure modes and silent assumptions

| ID / evidence | Finding | Detection / way forward |
|---|---|---|
| C1 [src/digitalmodel/subsea/connectors/connector_design.py:L172-L189] | No pressure/material/load/factor validation; negative allowable can produce negative utilisation and a passing body flag; zero allowable divides by zero. | No invalid-domain gate added here; enforce finite positive factors/material and declared signed load domains separately. |
| C2 [src/digitalmodel/subsea/connectors/connector_design.py:L181-L186] | One signed bending fibre only; changing moment sign can cancel axial stress and miss the opposite-fibre maximum. | Separate correction must evaluate both extrema and load combinations. |
| C3 [src/digitalmodel/subsea/connectors/connector_design.py:L172-L176] | Bore used as seal diameter may underestimate thrust when actual seal is larger; compression removed from separating force only. | Require actual seal geometry and a reviewed compression/contact policy. |
| C4 [src/digitalmodel/subsea/connectors/connector_design.py:L183-L196] | Thin-wall hoop applied to all geometry; low D/t only adds a note, with no external pressure or thick-wall solution. | Qualified stress basis required; advisory is not a gate. |
| C5 [src/digitalmodel/subsea/connectors/connector_design.py:L199-L211] | Welded enum identity skips seal; nonpositive seating demand yields zero seal utilisation for positive residual; no proof of seating adequacy. | Require explicit connection type and qualified seating load. |
| C6 [src/digitalmodel/subsea/connectors/section_properties.py:L36-L40] | Inequalities are not a complete finite-domain check; infinite outer diameter can pass. | Add finite guards separately; fatigue has an extra finite-section check. |
| C7 [src/digitalmodel/subsea/connectors/thermal_expansion.py:L77-L88] | Only restraint checked; negative/nonfinite length, modulus, alpha or temperature propagate. Returned full stress is unscaled even for free restraint. | Validate input basis and interpret stress versus reacted force separately. |
| C8 [src/digitalmodel/subsea/connectors/bending_analysis.py:L59-L72] | Infinite allowable silently passes; negative allowable can produce negative utilisation/pass; no MBR check despite catalogue label. | Supply independently qualified finite positive allowable; compare radius to qualified MBR separately. |
| C9 [src/digitalmodel/subsea/connectors/fatigue.py:L70-L74] [src/digitalmodel/subsea/connectors/fatigue.py:L95-L106] | Default curve/environment need not match construction; homogeneous signed-radius model omits mean stress, pressure and flexible layers. | Conversion test only; qualified fatigue basis and hot-spot model remain open. |
| C10 [src/digitalmodel/subsea/connectors/jumper_catalog.py:L26-L60] | Family records unchecked; exported mapping mutable, returned record is shared. | Treat as routing hints and preserve reviewed application configuration. |
| C11 [src/digitalmodel/subsea/connectors/__init__.py:L25-L26] [src/digitalmodel/subsea/connectors/fatigue.py:L16-L18] | Even mechanical-only package import requires fatigue dependencies. | Missing pyLife wheel prevents module-suite execution on this platform; separate import-boundary improvement is outside scope. |

## 7. Lessons log

| Date | Observation | Pack consequence |
|---|---|---|
| 2026-10-22 | Arithmetic regression evidence does not establish equipment or standards applicability. | Keep independent literals, input assumptions and NOT_EVALUATED engineering bases explicit. |

## 8. Ownership and review

Maintainers own inventory and regression maintenance. Subsea mechanical and fatigue
reviewers must qualify loads, seal geometry, stress model, fatigue class and material
basis before design use. The document records behaviour and defects, and changes no
calculation API or acceptance rule.

## 9. Dependencies and other implementations

Dataclasses, math and enums support mechanical code. Package import also loads
`digitalmodel.fatigue.assessment` and its NumPy, pandas, Pydantic and pyLife chain.
The AST inventory uses no runtime package imports. Test execution requires pytest;
registry checks use PyYAML. Run `PYTHONPATH=src uv run --no-project --with pytest --with pytest-asyncio --with pyyaml --with numpy --with pandas --with pydantic --with pylife python -m pytest tests/subsea/connectors -q -rA -p no:cacheprovider`
with dependencies compatible with the repository and temporary output outside it.
No external or licensed solver is needed or exercised.


Fatigue is a caller of `digitalmodel.fatigue.assessment.assess_fatigue`, not an
independent fatigue-damage implementation. The pinned implementation map already
records that caller relationship. The knowledge-pack index adds navigation without
changing method counts or delegate classifications. Tree records and ROV tools do
not automatically populate connector inputs. Open structural export and jumper
calculator work is excluded at this code pin. No manifold pack is present at the pin.

## 10. Coverage and open items

Coverage inventory: all seven package Python files and both existing test modules. Five new
independent examples cover force, thermal restraint, bending, combined/seal arithmetic
and signed fatigue conversion. Damage, S-N parameters, API/ISO acceptance and actual
equipment applicability remain NOT_EVALUATED. Resolve C1–C11 through separately
reviewed implementation changes; do not interpret a documented passing flag as
qualified design acceptance. Actual executed counts and dependency limitations belong
in the change description; a collectable node is not an executed test.

The [existing-pack compatibility audit](../../../../../docs/registry/agent-pack-drift.json)
records the unchanged baseline packs and collection limitations; its strict-schema
diagnostics require individual review before being classified as documentation defects.
