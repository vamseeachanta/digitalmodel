# Jacket and topside calculations knowledge pack

| Header | Value |
|---|---|
| Module | `structural/jacket_topside` |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/structural/jacket_topside` |
| Pack revision | 0, 2026-10-18 |
| Code revision | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | Engineering calculations; EC; AQG r0 Python calculations |
| Review role | Engineering calculations quality manager |
| Status | DRAFT; independent review required |

The code revision applies to repository citations throughout this pack; section 9 separately pins the implementation-map evidence.

## 1. Purpose and scope

The public API provides tubular section properties, member tension/compression/bending/interaction checks, joint classification, and simplified punching-shear checks. [src/digitalmodel/structural/jacket_topside/__init__.py:L10-L47]
The workflow delegates member and joint calculations and writes optional JSON and CSV summaries. [src/digitalmodel/structural/jacket_topside/workflow.py:L22-L62] [src/digitalmodel/structural/jacket_topside/workflow.py:L92-L119]
A separately imported dented-axial function predicts normalized residual capacity and compares demand; it does not establish engineering acceptance or an API assessment level. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L4-L9] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L90-L111]
The structural domain is registered as stable; this is domain metadata, not a qualification of each calculation. [docs/registry/module-routing.yaml:L400-L405]
The dented subset excludes applied bending and hydrostatic pressure and requires explicit support, dent and bow-reference conditions. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L149-L158] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L173-L190]
The member/joint checks are local capacity calculations; the workflow supplies no global structural solution or coordinate transformation. [src/digitalmodel/structural/jacket_topside/workflow.py:L22-L62] [src/digitalmodel/structural/jacket_topside/workflow.py:L77-L90]

## 2. Inputs and outputs

Input policy: require finite physical geometry and material properties, explicit loads and units, matching brace-array lengths, and a nonempty calculation set before relying on a result; these are review requirements because the legacy dataclasses do not enforce them. [src/digitalmodel/structural/jacket_topside/member_checks.py:L23-L46] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L38-L70] [src/digitalmodel/structural/jacket_topside/workflow.py:L77-L87]

| Input | Unit / convention | Range and default in code | Source |
|---|---|---|---|
| `TubularSection`: outer diameter, wall thickness, effective length | inches; effective length is already kL | No constructor guard; effective length defaults to zero and produces zero slenderness | [src/digitalmodel/structural/jacket_topside/member_checks.py:L23-L46] [src/digitalmodel/structural/jacket_topside/member_checks.py:L81-L86] |
| Yield strength, elastic modulus | ksi | Yield required; modulus has an implicit material default; confirm the explicit basis rather than inheriting it | [src/digitalmodel/structural/jacket_topside/member_checks.py:L30-L46] |
| `MemberLoads`: `fa`, `fb`, `Cm`, `is_tension` | ksi, ksi, dimensionless, boolean; stresses described as absolute magnitudes | Both stresses default to zero; moment factor has a code default; compression is the default branch | [src/digitalmodel/structural/jacket_topside/member_checks.py:L99-L119] |
| `JointGeometry`: chord/brace dimensions, angles, gap, same-side flag | inches; angles in degrees from chord axis | No physical or parallel-array guard; gap defaults to zero and same-side defaults true | [src/digitalmodel/structural/jacket_topside/joint_checks.py:L38-L70] |
| `JointLoads`: axial, in-plane and out-of-plane moment | kips and kip-in; positive axial is compression | All three loads default to zero | [src/digitalmodel/structural/jacket_topside/joint_checks.py:L103-L120] |
| `punching_shear_uc`: chord yield, brace index, chord utilization | ksi, index, dimensionless | Index defaults to first brace; utilization defaults to zero; only index bounds are explicitly rejected | [src/digitalmodel/structural/jacket_topside/joint_checks.py:L252-L292] |
| Dented-axial numeric fields | Dimensionless ratios; consistent stress units; midwall diameter and plastic-neutral-axis bow | All required; finite nonnegative numbers; diameter/thickness, slenderness and elastic/yield must be positive; envelope constants and material roundoff policy are defined in code | [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L46-L87] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L115-L147] |
| Dented-axial condition flags | Boolean confirmations | All required; simply supported, sharp midspan dent and specified bow reference true; applied bending and pressure false | [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L77-L87] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L149-L158] |
| Workflow configuration | Member/joint input mappings; output filenames | Missing mappings/lists become empty; relative output paths use configuration directory or current directory | [src/digitalmodel/structural/jacket_topside/workflow.py:L15-L19] [src/digitalmodel/structural/jacket_topside/workflow.py:L77-L90] |

No global origin, phase or spatial frame is represented by these scalar inputs; joint angle and moment-plane conventions above are the implemented local conventions. [src/digitalmodel/structural/jacket_topside/member_checks.py:L99-L119] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L49-L61] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L103-L120]

| Output | Unit / meaning | Source |
|---|---|---|
| Member result | Applied/allowable stress in ksi; dimensionless unity check; boolean pass; details dictionary | [src/digitalmodel/structural/jacket_topside/member_checks.py:L122-L131] |
| Member result dictionary | Axial branch, bending, combined and governing maximum; combined calculation still runs on the tension branch | [src/digitalmodel/structural/jacket_topside/member_checks.py:L399-L413] |
| Joint result | Applied/allowable punching stress in ksi; dimensionless unity check; pass; joint type and factors | [src/digitalmodel/structural/jacket_topside/joint_checks.py:L123-L133] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L302-L329] |
| Dented result | Dimensionless capacity/yield and demand/capacity; unsupported cases return no numbers, named flags and `outside_applicability`; source-reference tuple accompanies the result | [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L90-L111] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L181-L190] |
| Workflow files | UTF-8 JSON summary; member CSV: identifier/status/governing check/UC; joint CSV: identifier/status/type/UC; numerical units inherited from inputs | [src/digitalmodel/structural/jacket_topside/workflow.py:L65-L71] [src/digitalmodel/structural/jacket_topside/workflow.py:L92-L119] |

## 3. Method and governing standards

API references below describe code attribution, not checked normative equations: clause text not checked against the licensed copy; no licensed tabulated value is reproduced.

| Method | Implementation and reference locator | Qualification limit |
|---|---|---|
| Annulus geometry | `TubularSection` properties compute diameter, area, inertia, modulus, gyration, slenderness and ratios | Closed-form geometry; no material or geometry validation in constructor. [src/digitalmodel/structural/jacket_topside/member_checks.py:L23-L96] |
| Axial tension | `axial_tension_uc`; API RP 2A-WSD, *Planning, Designing, and Constructing Fixed Offshore Platforms—Working Stress Design*, edition 21, code-labelled axial tension, section 3.2 | Code coefficient and acceptance branch are recorded at the cited lines; standard compliance is U1. [src/digitalmodel/structural/jacket_topside/member_checks.py:L198-L227] |
| Compression | `_cc`, `_euler_fe`, `_allowable_compression`, `axial_compression_uc`; same standard/edition, code-labelled axial compression, section 3.3 | Zero slenderness uses infinite Euler stress; elastic/inelastic branches are implementation behavior, not an independent hand reference. [src/digitalmodel/structural/jacket_topside/member_checks.py:L138-L175] [src/digitalmodel/structural/jacket_topside/member_checks.py:L230-L269] |
| Bending | `_allowable_bending`, `bending_uc`; same standard/edition, code-labelled bending, section 3.4 | Allowable is unconditional despite the compactness discussion; do not use it to qualify noncompact members. [src/digitalmodel/structural/jacket_topside/member_checks.py:L178-L191] [src/digitalmodel/structural/jacket_topside/member_checks.py:L272-L301] |
| Interaction | `combined_axial_bending_uc`; same standard/edition, code-labelled combined axial/bending, section 3.5 | Maximum of two code interactions; compression allowables are used without reading `is_tension`; singular amplification is replaced by a small denominator. [src/digitalmodel/structural/jacket_topside/member_checks.py:L333-L371] |
| Joint classification | `classify_joint`; same standard/edition, code-labelled tubular joint classification, section 4.3.1 | Single brace T/Y, two braces K/X by side; all other brace counts fall back to K. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L140-L169] |
| Punching shear | `_qq_factor`, `_qf_factor`, `_applied_punching_shear`, `punching_shear_uc`; same standard/edition, code-labelled tubular joints, section 4.3.2 | Simplified calculation; factor docstring and implementation differ; invalid brace section can give zero applied stress. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L176-L211] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L214-L249] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L294-L329] |
| Dented axial residual capacity | `screen_dented_axial`, `_capacity_ratio`; Cho, *Design Approximations for Offshore Tubulars Against Collisions*, Volume I, 1987, sections 6.3.1, 6.3.2 and 6.4.2.1, equations 6.13–6.17 | Study envelope, zero-pressure subset, lower-root capacity, explicit material and support guards; interpolation is an implementation assumption. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L138-L190] |

The public thesis was opened on 2026-10-18; text from printed pages 193–194 and 197 was extracted for the study basis, equation locators and support/bow restrictions; the equation image was not decoded, so independent confirmation of its complete rendered form remains U2. (Cho, *Design Approximations for Offshore Tubulars Against Collisions*, Volume I, 1987, sections 6.3.1–6.4.2.1, [institutional copy](https://theses.gla.ac.uk/77600/1/10997383.pdf).)
The source-reference tuple is a local `SourceReference` sidecar, while member/joint result dictionaries contain no standards `Citation` sidecar. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L18-L44] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L90-L107] [src/digitalmodel/structural/jacket_topside/member_checks.py:L220-L227] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L314-L329]
The plan names edition 22, whereas implementation docstrings name edition 21; reconciliation and standard citation emission are tracked by #581. [docs/plans/2026-05-05-issue-581-api-rp-2a-wsd-structural.md:L8-L16] [src/digitalmodel/structural/jacket_topside/member_checks.py:L1-L12] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L1-L14]
API RP 2SIM, *Structural Integrity Management of Fixed Offshore Structures*, edition 1 (2014), appears in the method note as an unavailable assessment-level basis, not an implemented capacity method; clause title is unresolved and clause text not checked against the licensed copy. [docs/domains/asset-integrity/jacket-dented-axial-method.md:L111-L113] [src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml:L59-L59]

UNVERIFIED: U1 — normative compliance and appropriate edition of the member/joint calculations, including the simplified factors and corrosion reuse; #581 and #2198. [docs/domains/asset-integrity/jacket-dented-axial-method.md:L105-L110]
UNVERIFIED: U2 — complete primary equation rendering, experimental qualification and fixed-platform applicability of the dented-axial prediction; #2198. [docs/domains/asset-integrity/jacket-dented-axial-method.md:L88-L99]

## 4. Validated worked examples

Results below were recorded against the code pin on 2026-10-18; displayed comparisons are rounded, while executable tests retain full precision and their stated tolerances.
Validated means the closed-form geometry check only; the dented hand calculation and specimen exclusion are regressions, not independent equation or experimental validation. [tests/structural/jacket_topside/test_jacket_topside.py:L95-L98] [tests/structural/jacket_topside/test_dented_axial_screen.py:L65-L84] [tests/structural/jacket_topside/test_dented_axial_screen.py:L250-L270]

| Case / kind | Independent basis or regression basis; inputs | Expected quantity / tolerance | Existing test node ID | BASE / AFTER |
|---|---|---|---|---|
| V1 annular area; validated closed form | Integrating an annulus gives area pi times the difference of squared radii; synthetic fixture is in the test | `pi/4*(24**2-22.5**2)` in²; absolute error below 1e-4 in² | `tests/structural/jacket_topside/test_jacket_topside.py::TestTubularSection::test_cross_sectional_area` | PASS / PASS. [tests/structural/jacket_topside/test_jacket_topside.py:L39-L52] [tests/structural/jacket_topside/test_jacket_topside.py:L95-L98] |
| R5 dented-axial formula; synthetic arithmetic regression | Test documents Cho (1987), Volume I, equations 6.13–6.17, printed page 194; fixture in test, zero pressure; complete primary equation image remains unchecked (U2) | Capacity/yield 0.4 and demand/capacity 0.5 displayed; compare full-precision test constants, absolute tolerance 1e-10 dimensionless for decimal rounding only | `tests/structural/jacket_topside/test_dented_axial_screen.py::test_hand_calculation` | PASS / PASS. [tests/structural/jacket_topside/test_dented_axial_screen.py:L47-L84] |
| R1 member compression; regression | Expected value reuses the implementation's expression, not a checked licensed example; synthetic fixture in test | UC equals test's `fa/Fc`; absolute error below 1e-4 dimensionless | `tests/structural/jacket_topside/test_jacket_topside.py::TestAxialCompression::test_hand_calc_inelastic` | PASS / PASS. [tests/structural/jacket_topside/test_jacket_topside.py:L227-L239] |
| R2 simplified joint; regression | Synthetic loads/geometry and current algorithm | Low-load result passes and UC at most 1 dimensionless; no independent capacity reference | `tests/structural/jacket_topside/test_jacket_topside.py::TestPunchingShear::test_low_load_passes` | PASS / PASS. [tests/structural/jacket_topside/test_jacket_topside.py:L506-L527] |
| R3 specimen exclusion; regression | Published specimen data in test; material ratio is outside fitted study | Exactly 1 material flag; no numerical capacity; engineering acceptance false | `tests/structural/jacket_topside/test_dented_axial_screen.py::test_published_b1_remains_unvalidated` | PASS / PASS. [tests/structural/jacket_topside/test_dented_axial_screen.py:L250-L270] |
| R4 registered workflow; regression | `examples/workflows/jacket-member-joint-checks/input.yml`; outputs are synthetic snapshots | Member and joint UCs equal existing assertions within pytest's default approximation; member UCs display 0.4 and 0.6, joint UC 0.1 dimensionless | `tests/workflows/test_durable_workflows.py::test_workflow_registry[jacket-member-joint-checks]` | PASS / PASS: guarded optional imports permit offline execution in a temporary copy. [docs/registry/workflows.yaml:L782-L792] [tests/workflows/test_durable_workflows.py:L1125-L1136] [src/digitalmodel/engine.py:L30-L36] [src/digitalmodel/solvers/orcaflex/orcaflex_modal_analysis.py:L9-L13] |

Offline execution record: `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`; repository addopts retained, no override, single process; BASE and AFTER use the header code pin, AFTER on the uncommitted documentation tree. The workflow uses a temporary pinned source/test/example copy because the original engine writes result files beside its input. [tests/workflows/test_durable_workflows.py:L57-L94]
The package suite contains 51 member/joint cases and 54 dented cases including parameter expansion; all 105 passed at BASE and AFTER, zero failed, skipped or NOT_RUN within that suite; the separate workflow case passed at BASE and AFTER, for 106 passed cases overall and 0 NOT_RUN. [tests/structural/jacket_topside/test_jacket_topside.py:L88-L608] [tests/structural/jacket_topside/test_dented_axial_screen.py:L65-L309]
The original suite's general published-example assertion is not an identifiable independent worked-example source and is not adopted as qualification evidence. [tests/structural/jacket_topside/test_jacket_topside.py:L1-L9]

## 5. QA gates

Pack review requirements follow AQG r0, section 10, Python calculations, with G5 from section 9; checks below define reproducible acceptance criteria, not a statement that every engineering gate has been cleared.

| Gate | Applicability | Module-specific checks and numeric pass criterion | Evidence / recorded result |
|---|---|---|---|
| G0 | Applicable: input units, provenance and applicability | C01: `screen_dented_axial` rejects all 24 nonfinite/negative numeric parameter cases and 3 zero denominators: 0 invalid inputs accepted, unit cases; C02: 4 study-envelope parameters, 4 material mismatches and 5 unsupported-condition cases return their specified flags/no capacity: 0 unflagged exclusions, unit cases | Existing parameterized tests; PASS. Legacy member/joint input guards remain gaps, not covered by these checks. [tests/structural/jacket_topside/test_dented_axial_screen.py:L103-L146] [tests/structural/jacket_topside/test_dented_axial_screen.py:L176-L218] |
| G1 | Applicable: Python imports and tests; no solver output program is written | C03: package suite imports the public module and exercises checks: 105 passed cases, 0 failed cases; C05: `screen_dented_axial`, R5, capacity/yield and demand/capacity within 1e-10 dimensionless of synthetic test constants | PASS; the separate registered workflow also passed in a temporary copy; no licensed program load approval is asserted. [tests/structural/jacket_topside/test_jacket_topside.py:L15-L32] [src/digitalmodel/solvers/orcaflex/orcaflex_modal_analysis.py:L9-L13] |
| G2 | Applicable: closed form and published equation | C04: `TubularSection.cross_sectional_area`, V1, absolute area error below 1e-4 in² | C04 PASS; annular geometry is the independent reference. C05 in G1 replays arithmetic and does not qualify the primary dented equation, measured specimen strength or platform end restraints. [tests/structural/jacket_topside/test_jacket_topside.py:L95-L98] [tests/structural/jacket_topside/test_dented_axial_screen.py:L65-L84] |
| G3 | NOT_APPLICABLE: no time-domain integration in this local capacity workflow | No short dynamics; 0 dynamic states integrated | [src/digitalmodel/structural/jacket_topside/workflow.py:L22-L62] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L173-L190] |
| G4 | NOT_APPLICABLE: no diffraction body or mesh in these interfaces | No hydrostatics/mesh check; 0 diffraction meshes supplied | [src/digitalmodel/structural/jacket_topside/member_checks.py:L23-L46] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L38-L70] |
| G5 | Applicable: evidence appendix for any downstream report | C06: retain code pin, input fixture reference, expected quantity/unit, tolerance, full test node and result for all 6 section-4 cases; 0 missing fields, unit cases; unsupported cases must retain flags and no numerical capacity | Pack evidence audit PASS; workflow case executed in a temporary copy to contain generated files. Report signature and result fingerprint remain prerequisites for downstream use, not generated here. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L90-L111] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L181-L190] |

Execute C01–C05 with the package-suite command in section 4; C06 is a six-row evidence audit, with the workflow fixture and node supplied by the registry. [docs/registry/workflows.yaml:L782-L792]
No existing member or joint formula is designated as an independent hand reference for itself. [tests/structural/jacket_topside/test_jacket_topside.py:L227-L239] [tests/structural/jacket_topside/test_jacket_topside.py:L534-L553]

## 6. Known failure modes and pitfalls

sweep: 39 found

The sweep covers all five package Python files and records 39 review sites below, including intentional defaults and guarded absence, rather than treating every match as a defect; date 2026-10-18, code pin in header.
Reproduce discovery with both commands, then inspect every matched definition and its return paths:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|unit|cache|default' src/digitalmodel/structural/jacket_topside --glob '*.py'
rg -n 'get\(|setdefault|field\(|if .* else|return|0\.0|0\.85|1e-' src/digitalmodel/structural/jacket_topside --glob '*.py'
```

| Site | Symptom / cause and disposition | Existing detection / follow-up |
|---|---|---|
| S01 | Implicit modulus material default; not a measured material basis. [src/digitalmodel/structural/jacket_topside/member_checks.py:L35-L46] | no detection check exists; issue pending (1) |
| S02 | Zero effective length defaults to zero slenderness, suppressing finite-length buckling. [src/digitalmodel/structural/jacket_topside/member_checks.py:L46-L46] [src/digitalmodel/structural/jacket_topside/member_checks.py:L81-L86] | no detection check exists; issue pending (1) |
| S03 | Missing axial stress becomes zero. [src/digitalmodel/structural/jacket_topside/member_checks.py:L116-L116] | no detection check exists for omitted demand; issue pending (1) |
| S04 | Missing bending stress becomes zero. [src/digitalmodel/structural/jacket_topside/member_checks.py:L117-L117] | no detection check exists for omitted demand; issue pending (1) |
| S05 | Moment factor defaults without loading-basis validation. [src/digitalmodel/structural/jacket_topside/member_checks.py:L118-L118] | no detection check exists; issue pending (1) |
| S06 | Axial sense defaults to compression; the combined check ignores the flag. [src/digitalmodel/structural/jacket_topside/member_checks.py:L119-L119] [src/digitalmodel/structural/jacket_topside/member_checks.py:L333-L353] | no detection check exists for tension interaction correctness; issue pending (1) |
| S07 | Zero slenderness returns infinite Euler stress; intentional idealization can mask missing length. [src/digitalmodel/structural/jacket_topside/member_checks.py:L138-L146] | no detection check exists; issue pending (1) |
| S08 | Tension nonpositive allowable returns infinite UC rather than rejecting inputs. [src/digitalmodel/structural/jacket_topside/member_checks.py:L218-L226] | no detection check exists for invalid material; issue pending (1) |
| S09 | Compression nonpositive allowable returns infinite UC. [src/digitalmodel/structural/jacket_topside/member_checks.py:L249-L261] | no detection check exists for invalid material; issue pending (1) |
| S10 | Bending allowable is unconditional; compactness prose is not enforced. [src/digitalmodel/structural/jacket_topside/member_checks.py:L178-L191] | no detection check exists; #581 |
| S11 | Bending nonpositive allowable returns infinite UC. [src/digitalmodel/structural/jacket_topside/member_checks.py:L292-L300] | no detection check exists; issue pending (1) |
| S12 | Combined nonpositive compression allowable substitutes infinite axial ratio. [src/digitalmodel/structural/jacket_topside/member_checks.py:L337-L345] | no detection check exists; issue pending (1) |
| S13 | Simplified interaction substitutes infinity for nonpositive bending allowable. [src/digitalmodel/structural/jacket_topside/member_checks.py:L346-L347] | no detection check exists; issue pending (1) |
| S14 | Singular or exceeded Euler denominator is replaced by a small positive denominator. [src/digitalmodel/structural/jacket_topside/member_checks.py:L349-L350] | no detection check exists; issue pending (1) |
| S15 | Second interaction substitutes infinity for nonpositive bending allowable but divides directly by yield. [src/digitalmodel/structural/jacket_topside/member_checks.py:L352-L353] | no detection check exists; issue pending (1) |
| S16 | Member units are assumed, not converted; sign magnitudes are not normalized. [src/digitalmodel/structural/jacket_topside/member_checks.py:L7-L7] [src/digitalmodel/structural/jacket_topside/member_checks.py:L99-L119] | no detection check exists for supplied unit/signed-load mistakes; issue pending (1) |
| S17 | Gap defaults to zero; no gap term is used by the simplified factor. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L69-L70] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L294-L302] | no detection check exists; #581 |
| S18 | Same-side flag defaults true, determining two-brace classification. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L69-L70] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L162-L166] | no detection check exists for omitted orientation; issue pending (1) |
| S19 | Missing joint axial load becomes zero. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L118-L118] | no detection check exists for omitted load; issue pending (1) |
| S20 | Missing in-plane moment becomes zero. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L119-L119] | no detection check exists for omitted load; issue pending (1) |
| S21 | Missing out-of-plane moment becomes zero. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L120-L120] | no detection check exists for omitted load; issue pending (1) |
| S22 | All brace counts outside one/two fall back to K, including zero. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L157-L169] | no detection check exists; issue pending (1) |
| S23 | Near-zero geometry-factor denominator is clamped without rejecting geometry. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L193-L196] | no detection check exists; issue pending (1) |
| S24 | Chord utilization defaults zero and factor is floored; implementation differs from docstring expression. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L199-L211] | no detection check exists for omission, floor or expression mismatch; issue pending (1), #581 |
| S25 | Nonpositive brace area yields zero axial stress under load. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L240-L245] | no detection check exists; issue pending (1) |
| S26 | Nonpositive section modulus yields zero in-plane bending stress. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L243-L246] | no detection check exists; issue pending (1) |
| S27 | Nonpositive section modulus yields zero out-of-plane bending stress. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L243-L247] | no detection check exists; issue pending (1) |
| S28 | Brace selection defaults to first brace; utilization again defaults zero at public call. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L252-L257] | no detection check exists for omitted selection/reduction; issue pending (1) |
| S29 | Nonpositive punching allowable yields infinite UC; units are implicit kips/inches/ksi. [src/digitalmodel/structural/jacket_topside/joint_checks.py:L9-L9] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L302-L319] | no detection check exists for invalid geometry/material/units; issue pending (1) |
| S30 | Adapter defaults brace index to first and chord utilization to zero. [src/digitalmodel/structural/jacket_topside/workflow.py:L42-L51] | no detection check exists for missing fields; issue pending (2) |
| S31 | Missing workflow mapping becomes empty. [src/digitalmodel/structural/jacket_topside/workflow.py:L77-L78] | no detection check exists; issue pending (2) |
| S32 | Missing member list becomes empty. [src/digitalmodel/structural/jacket_topside/workflow.py:L79-L79] | no detection check exists; issue pending (2) |
| S33 | Missing joint list becomes empty; `all([])` can report PASS with zero checks. [src/digitalmodel/structural/jacket_topside/workflow.py:L80-L87] | no detection check exists; issue pending (2) |
| S34 | Missing output mapping becomes empty, so all requested evidence files may be absent. [src/digitalmodel/structural/jacket_topside/workflow.py:L89-L89] [src/digitalmodel/structural/jacket_topside/workflow.py:L98-L119] | no detection check exists; issue pending (2) |
| S35 | Missing configuration directory falls back to current directory; relative outputs can go to an unintended location. [src/digitalmodel/structural/jacket_topside/workflow.py:L15-L19] | no detection check exists; issue pending (2) |
| S36 | Zero numeric denominators are rejected in the dented path, unlike the legacy inputs. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L115-L135] | C01, PASS; guarded condition, no defect asserted |
| S37 | Outside-applicability dented input intentionally returns two absent values and named flags. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L181-L184] | C02, PASS; callers must preserve flags; #2198 for broader qualification |
| S38 | Dented units are dimensionless with consistent stress ratios; source material tolerance permits roundoff only. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L62-L74] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L144-L147] | C02, PASS for material guard; no detection check exists for an upstream unit conversion mistake; #2198 |
| S39 | Dented `float()` converts an internally computed root, not external text; no locale parsing is performed. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L161-L170] | C05, PASS; no external-text conversion defect asserted |

No swallowed exception, record-skipping `continue`, epoch fallback, empty-result cache write or external-text numeric parsing exists in these five files; empty details/result dictionaries are initialization rather than error recovery. [src/digitalmodel/structural/jacket_topside/__init__.py:L1-L49] [src/digitalmodel/structural/jacket_topside/member_checks.py:L1-L413] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L1-L329] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L1-L190] [src/digitalmodel/structural/jacket_topside/workflow.py:L1-L119]

| Other confirmed limitation | Detection / issue |
|---|---|
| Standards edition mismatch and missing `Citation` sidecars. [docs/plans/2026-05-05-issue-581-api-rp-2a-wsd-structural.md:L8-L16] [src/digitalmodel/structural/jacket_topside/member_checks.py:L1-L12] | no detection check exists; #581 |
| Dented interpolation and experimental qualification are not established by the hand check. [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L173-L179] [docs/domains/asset-integrity/jacket-dented-axial-method.md:L88-L99] | C02 guards coded envelope only; no detection check exists for experimental accuracy; #2198 |

## 7. Lessons log

Append new evidence; do not rewrite prior entries. Identifiers in this log refer to evidence cases rather than execution identifiers.

| Date | Evidence ID | Lesson | Evidence | Pack change / shared entry |
|---|---|---|---|---|
| 2026-10-18 | R5 / R3 | A synthetic arithmetic check and a specimen applicability exclusion establish different things; neither establishes measured capacity accuracy. | [tests/structural/jacket_topside/test_dented_axial_screen.py:L65-L84] [tests/structural/jacket_topside/test_dented_axial_screen.py:L250-L270] | Sections 3–6 distinguish formula evidence from experimental qualification; shared entry not assigned, #2198 |
| 2026-10-18 | S33 | An empty calculation set can report PASS because universal quantification of an empty list is true. | [src/digitalmodel/structural/jacket_topside/workflow.py:L77-L87] | Section 6 records the missing guard; shared entry not assigned, issue pending (2) |

## 8. Ownership

Review requirements: engineering calculations quality manager; deputy review role: engineering calculations deputy quality manager; producing role: calculation-pack contributor. The maintainer assigns the deputy and external review; these are required roles, not claims that review has happened.
Escalate disputed formula attribution, edition, applicability or tolerance to the family quality manager; refer cross-family changes to the maintainer. Keep this pack DRAFT until independent review and issue-number substitution are complete.

## 9. Dependencies and implementation inventory

The numeric member/joint path depends on Python math and dataclasses, with no solver launch in its functions; the dented subset additionally uses finite-value/type checks and local source-reference records. [src/digitalmodel/structural/jacket_topside/member_checks.py:L14-L16] [src/digitalmodel/structural/jacket_topside/member_checks.py:L138-L175] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L214-L249] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L115-L170]
The workflow consumes already-derived section, material and load records, delegates to member/joint checks, and exposes results to JSON/CSV consumers; its registry entry names the example input and four output files. [src/digitalmodel/structural/jacket_topside/workflow.py:L22-L62] [docs/registry/workflows.yaml:L782-L792]
No independent input-source inventory row, global load derivation or downstream report qualification is established by that interface; confirm the calculation basis before reuse. [src/digitalmodel/structural/jacket_topside/workflow.py:L22-L62]

Implementation-map evidence is pinned separately to `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, with its declared data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; the path is `docs/registry/implementation-map.yaml` at that map pin, not the package code pin.
The inventory rule is: select every map family having at least one `implementations` row whose `path` starts with `src/digitalmodel/structural/jacket_topside/`, then include every implementation of each selected family, even outside the package.
The computed selection contains 0 families and 0 rows: the full section-9 map-family inventory is empty; counted-family ranking is empty and counted grep is `NOT_APPLICABLE` because no family has an in-package implementation row.
Entries mentioning package files under rejected candidates are not implementation rows and do not change this selection. [docs/registry/implementation-map.yaml:L44478-L44484]
A8 passes with the empty inventory and that stated reason; no further-second-implementation count is asserted for an uncounted family.

Map follow-up, issue pending (3): add annular section properties, axial tension, column compression, bending, axial/bending interaction, tubular joint classification, punching shear and dented-axial residual-capacity families, grouped as appropriate; their present implementations are listed in section 3. [src/digitalmodel/structural/jacket_topside/member_checks.py:L52-L96] [src/digitalmodel/structural/jacket_topside/member_checks.py:L138-L371] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L140-L329] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L138-L190]

Whole-repository discovery, including scripts, used:

```sh
rg -n -i 'jacket.topside|dented.axial|punching.shear|classify_joint|_allowable_compression' . --glob '*.py' --glob '!.venv/**'
rg -n 'class (ColumnBuckling|MemberCapacity)|pi / (4|64)|0\.75 \* .*yield|Perry|punching.shear|Cho \(1987\)' src scripts --glob '*.py'
```

This additional discovery is map follow-up, not the counted A8 grep: `workflow._member_row` and `_joint_row` delegate and serialize, while `__init__` re-exports definitions; tests repeat formulas as checks. [src/digitalmodel/structural/jacket_topside/workflow.py:L22-L62] [src/digitalmodel/structural/jacket_topside/__init__.py:L10-L47] [tests/structural/jacket_topside/test_jacket_topside.py:L191-L207]
Other annulus implementations compute section properties in `structural/stress/vm_stress.py` and `subsea/viv_analysis/models.py`, and script-level analytical inertia in `scripts/integrations/sectionproperties_evaluation.py`; those are outside-package map gaps for the proposed geometry family, issue pending (3). [src/digitalmodel/structural/stress/vm_stress.py:L99-L118] [src/digitalmodel/subsea/viv_analysis/models.py:L74-L87] [scripts/integrations/sectionproperties_evaluation.py:L160-L173]
No uniqueness claim is made for these uncounted methods; map reconciliation must classify the remaining search hits before any consolidation.

## 10. Coverage and open items

Files read for this package: every Python file listed below; no nested implementation package exists at the code pin. [src/digitalmodel/structural/jacket_topside/__init__.py:L1-L49] [src/digitalmodel/structural/jacket_topside/member_checks.py:L1-L413] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L1-L329] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L1-L190] [src/digitalmodel/structural/jacket_topside/workflow.py:L1-L119]

| Read coverage | Evidence |
|---|---|
| Package exports and docstrings | [src/digitalmodel/structural/jacket_topside/__init__.py:L1-L49] |
| Complete member implementation | [src/digitalmodel/structural/jacket_topside/member_checks.py:L1-L413] |
| Complete joint implementation | [src/digitalmodel/structural/jacket_topside/joint_checks.py:L1-L329] |
| Complete dented-axial implementation | [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L1-L190] |
| Complete workflow adapter | [src/digitalmodel/structural/jacket_topside/workflow.py:L1-L119] |
| Both package test files; 105 expanded cases, all run | [tests/structural/jacket_topside/test_jacket_topside.py:L1-L608] [tests/structural/jacket_topside/test_dented_axial_screen.py:L1-L309] |
| Registered workflow and existing node; input and base configuration | [docs/registry/workflows.yaml:L782-L792] [tests/workflows/test_durable_workflows.py:L1125-L1136] [examples/workflows/jacket-member-joint-checks/input.yml:L1-L54] [src/digitalmodel/base_configs/modules/jacket_checks/jacket_checks.yml:L1-L12] |
| Routing and capability declarations | [docs/registry/module-routing.yaml:L400-L419] [docs/capability-map/capabilities-added.yml:L61-L65] |
| Module-specific standards plan, including planned surfaces not present in package | [docs/plans/2026-05-05-issue-581-api-rp-2a-wsd-structural.md:L8-L30] |
| Offering-program reference to the existing package | [docs/plans/2026-09-25-issue-1057-ffs-offering-program.md:L14-L28] |
| Method note and associated research note; research assertions are historical and not independently adopted | [docs/domains/asset-integrity/jacket-dented-axial-method.md:L1-L125] [docs/research/ffs-fixed-platform-damaged-member-screen.md:L1-L91] |
| Implementation-map package mentions are rejected candidates, not family inventory | [docs/registry/implementation-map.yaml:L44478-L44484] [docs/registry/implementation-map.yaml:L50631-L50637] |

Documentation discovery searched `docs/` for `jacket_topside`, `jacket-member-joint-checks`, `member_checks.py`, `joint_checks.py` and `dented_axial_screen`, then included the method/research notes above; generic jacket mentions in unrelated engineering domains do not supply module evidence.
The calculation-output citation contract was read; member/joint sidecar wiring remains an identified gap, and local dented source records do not establish a resolved standards `Citation`. [docs/plans/2026-05-05-issue-581-api-rp-2a-wsd-structural.md:L8-L16] [src/digitalmodel/structural/jacket_topside/dented_axial_screen.py:L18-L44]

| Open item | Responsible role | Tracking and completion evidence |
|---|---|---|
| U1: independent API RP 2A-WSD compliance, edition reconciliation, citation sidecars, compactness and joint-factor audit | Calculation maintainer | #581; checked eligible standard edition and clause attribution, boundary tests and resolvable result sidecars. [docs/plans/2026-05-05-issue-581-api-rp-2a-wsd-structural.md:L8-L23] |
| U2: complete primary equation rendering, experimental dented-member validation, fixed-platform applicability and corrosion reuse; broader loads, pressure, assessment-level and report integration | Calculation maintainer | #2198; published specimen comparison within established material/support scope and separately checked normative basis; no independent equation or platform qualification from R5. Source-page image extraction failed with `pypdf.errors.DependencyError: jbig2dec binary is not available.`; extracted text does not substitute for the full rendered equation. [docs/domains/asset-integrity/jacket-dented-axial-method.md:L88-L125] |
| Legacy member/joint invalid-input and fallback guards, including S01–S29 | Calculation maintainer | issue pending (1); finite input, physical section and parallel-array rejection cases, with standards audit under #581. [src/digitalmodel/structural/jacket_topside/member_checks.py:L333-L353] [src/digitalmodel/structural/jacket_topside/joint_checks.py:L193-L247] |
| Empty or incomplete workflow, implicit file destinations and missing requested evidence, S30–S35 | Workflow maintainer | issue pending (2); nonempty component and explicit configuration/output guards, with negative tests. [src/digitalmodel/structural/jacket_topside/workflow.py:L15-L19] [src/digitalmodel/structural/jacket_topside/workflow.py:L77-L119] |
| Map families absent; alternative geometry implementations await classification | Registry maintainer | issue pending (3); add section-3 methods with in-package rows and reconcile whole-repository hits. [src/digitalmodel/structural/jacket_topside/member_checks.py:L52-L96] [src/digitalmodel/structural/stress/vm_stress.py:L99-L118] |
| Independent pack review, deputy assignment and replacement of draft references by issue numbers | Maintainer | issue pending (3); independent A1–A8 clearance and numbered tracking before merge; no review claimed here |

No client-derived numerical data is included; no private sidecar identifier is asserted.
