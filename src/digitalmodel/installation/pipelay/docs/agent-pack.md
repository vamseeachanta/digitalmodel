# Installation pipelay knowledge pack

| Header | Value |
|---|---|
| Module | installation/pipelay |
| Repository | digitalmodel |
| Package | `src/digitalmodel/installation/pipelay` |
| Pack revision | 1; 2026-10-17 |
| Code revision | `6cf35f466e5c1b730951af045d8cfe0065af073f` |
| Analysis family | OrcaFlex installation; analytical screening subset |
| Quality manager | Installation quality manager; independent provider |
| Status | DRAFT; second-provider clearance pending |

Source citations, including the implementation-map evidence in section 9, resolve at the code revision above.

## 1. Purpose and scope

The package provides S-lay screening functions for longitudinal stress, concrete-coating strain, stable route radius, weld-repair length, buoyancy and lay tension. [src/digitalmodel/installation/pipelay/__init__.py:L13-L53]
The routing registry records the enclosing installation domain as beta, rather than a separate maturity for this submodule. [docs/registry/module-routing.yaml:L430-L435]
The stress calculation adds axial stress to outer-fibre bending stress and compares that signed sum with a caller-supplied fraction of SMYS; it does not evaluate both fibres or a pressure interaction. [src/digitalmodel/installation/pipelay/stress.py:L141-L161]
The lay-tension method is a free-hanging catenary screen, not a stinger-contact, statics or dynamics solver. [src/digitalmodel/installation/pipelay/integrity.py:L264-L276] [src/digitalmodel/installation/pipelay/integrity.py:L340-L363]
The package initializer's statement that stress checks live only in the adjacent analytical package is stale relative to its own stress exports; see issue pending (5). [src/digitalmodel/installation/pipelay/__init__.py:L8-L10] [src/digitalmodel/installation/pipelay/__init__.py:L27-L33]

## 2. Inputs and outputs

Functions accept scalar arguments without file parsing or automatic unit conversion. [src/digitalmodel/installation/pipelay/stress.py:L30-L32] [src/digitalmodel/installation/pipelay/integrity.py:L12-L13]
Ranges below distinguish implemented guards from requirements that remain the caller's responsibility. [src/digitalmodel/installation/pipelay/stress.py:L71-L88] [src/digitalmodel/installation/pipelay/integrity.py:L134-L145]

| Input / functions | Unit | Implemented range / default | Source |
|---|---|---|---|
| `outer_diameter`, `bend_radius`; bending/strain/stress checks | Consistent length | Radius > 0; diameter has no positivity guard; required | [src/digitalmodel/installation/pipelay/stress.py:L49-L73] [src/digitalmodel/installation/pipelay/integrity.py:L62-L82] |
| `youngs_modulus`, `smys`, `allowable_fraction`; stress/radius | Stress, stress, dimensionless | SMYS > 0 and fraction in (0,1]; modulus unchecked; required | [src/digitalmodel/installation/pipelay/stress.py:L102-L109] [src/digitalmodel/installation/pipelay/stress.py:L141-L161] [src/digitalmodel/installation/pipelay/stress.py:L164-L196] |
| `axial_force`, `steel_area`; combined stress | Force, length² | Force defaults to 0; area defaults to None and must be supplied and > 0 for nonzero force | [src/digitalmodel/installation/pipelay/stress.py:L108-L109] [src/digitalmodel/installation/pipelay/stress.py:L146-L151] [src/digitalmodel/installation/pipelay/stress.py:L76-L88] |
| `crushing_strain_limit`, `strain_concentration_factor`, `factor_of_safety` | Dimensionless | Limit required but unchecked; SCF ≥ 1, default 1; FoS > 0, default 1 | [src/digitalmodel/installation/pipelay/integrity.py:L85-L93] [src/digitalmodel/installation/pipelay/integrity.py:L107-L145] |
| `bottom_tension`, `lateral_friction`, submerged weight; route radius | Force, dimensionless, force/length | Friction and submerged weight > 0; tension unchecked; required | [src/digitalmodel/installation/pipelay/integrity.py:L166-L189] |
| `planned_radius`, `minimum_radius`; route FoS | Consistent length | Minimum > 0; planned radius unchecked; required | [src/digitalmodel/installation/pipelay/integrity.py:L192-L199] |
| Weld total/repair/minimum lengths and fraction | Consistent length; fraction dimensionless | Allowable helper guards fraction in (0,1]; predicate has no guards; minimum default is `DEFAULT_MIN_REPAIR_LENGTH`, expressed in metres | [src/digitalmodel/installation/pipelay/integrity.py:L213-L257] |
| Buoyancy / wet-weight inputs | Diameter m; density kg/m³; gravity m/s²; dry weight N/m | No guards; gravity defaults to `G_STANDARD`; other values required | [src/digitalmodel/installation/pipelay/integrity.py:L279-L324] |
| Wet weight, sagbend radius, water depth; lay tensions | Force/length, length, length | No guards and no defaults | [src/digitalmodel/installation/pipelay/integrity.py:L327-L363] |

Table 1. Scalar inputs, units, guards and defaults at the code pin; standard-attributed coefficient values are intentionally omitted.

| Output | Unit / contract | Source |
|---|---|---|
| `SmysCheck` | Longitudinal and allowable stress; dimensionless utilisation; percent SMYS; boolean `passes` uses ≤ | [src/digitalmodel/installation/pipelay/stress.py:L91-L99] [src/digitalmodel/installation/pipelay/stress.py:L153-L161] |
| `CrushingCheck` | Dimensionless strains and utilisation; boolean `passes` uses strictly < 1 | [src/digitalmodel/installation/pipelay/integrity.py:L96-L104] [src/digitalmodel/installation/pipelay/integrity.py:L138-L145] |
| Bend/route/weld radius or length; route FoS | Length; dimensionless FoS; weld predicate boolean | [src/digitalmodel/installation/pipelay/stress.py:L170-L196] [src/digitalmodel/installation/pipelay/integrity.py:L185-L199] [src/digitalmodel/installation/pipelay/integrity.py:L244-L257] |
| Buoyancy and wet weight; horizontal and top tension | N/m for SI buoyancy/weight; force for tensions | [src/digitalmodel/installation/pipelay/integrity.py:L279-L324] [src/digitalmodel/installation/pipelay/integrity.py:L327-L363] |

Table 2. Returned values; no solver input/output file format is implemented by these functions.

Positive wet weight means net downward; positive axial force adds tensile stress, while negative force reduces the single signed sum. [src/digitalmodel/installation/pipelay/integrity.py:L317-L324] [src/digitalmodel/installation/pipelay/stress.py:L148-L160]
No coordinate frame, origin or phase is used by these scalar equations. [src/digitalmodel/installation/pipelay/stress.py:L146-L161] [src/digitalmodel/installation/pipelay/integrity.py:L362-L363]
The MPa/newton/metre-area example in the stress docstring is dimensionally inconsistent with direct N/A addition: retain Pa with N and m², or explicitly convert the axial stress before combining; see issue pending (1). [src/digitalmodel/installation/pipelay/stress.py:L30-L32] [src/digitalmodel/installation/pipelay/stress.py:L76-L88] [src/digitalmodel/installation/pipelay/stress.py:L146-L160]

## 3. Method and governing standards

| Method / implementing definition | Implemented relation and criterion | Attribution and limitation |
|---|---|---|
| `bending_stress_from_radius`, `axial_stress`, `smys_stress_check` | σb = E·OD/(2R), σa = N/A, σlong = σa + σb; pass when σlong ≤ f·SMYS | DNV-ST-F101 and DNV-OS-F101, *Submarine Pipeline Systems*, are named without edition or clause title; clause text not checked against the licensed copy. [src/digitalmodel/installation/pipelay/stress.py:L34-L37] [src/digitalmodel/installation/pipelay/stress.py:L71-L88] [src/digitalmodel/installation/pipelay/stress.py:L146-L161] |
| `allowable_bend_radius` | Rmin = E·OD/(2f·SMYS), pure bending only | No additional clause locator is supplied. [src/digitalmodel/installation/pipelay/stress.py:L170-L174] [src/digitalmodel/installation/pipelay/stress.py:L192-L196] |
| `bending_strain_from_radius`, `concrete_crushing_strain`, `concrete_crushing_check` | εb = OD/(2R), εc = SCF·εb; utilisation = εc/(εlimit/FoS); pass strictly below 1 | Repository attribution: Verley and Ness (1995), with supplied SCF and crushing strain rather than a full sectional calculation. [src/digitalmodel/installation/pipelay/integrity.py:L39-L59] [src/digitalmodel/installation/pipelay/integrity.py:L80-L93] [src/digitalmodel/installation/pipelay/integrity.py:L134-L145] |
| `minimum_route_radius`, `route_radius_factor_of_safety` | Rmin = F/(S·W); FoS = Rplanned/Rmin | Local force/friction balance; no standard clause specified. [src/digitalmodel/installation/pipelay/integrity.py:L153-L163] [src/digitalmodel/installation/pipelay/integrity.py:L185-L199] |
| `weld_repair_allowable_length`, `is_weld_repair_acceptable` | First returns max(f·L, Lmin); second requires Lmin ≤ Lrepair ≤ f·L | DNV-ST-F101, *Submarine Pipeline Systems*, edition and clause title absent; clause text not checked against the licensed copy; conflicting contracts are not a hand-check reference. [src/digitalmodel/installation/pipelay/integrity.py:L203-L208] [src/digitalmodel/installation/pipelay/integrity.py:L218-L257] |
| `displaced_weight_per_length`, `submerged_weight_per_length` | b = ρgπOD²/4; wsub = wdry − b | Total coated OD for displacement; SI inputs explicitly documented. [src/digitalmodel/installation/pipelay/integrity.py:L264-L268] [src/digitalmodel/installation/pipelay/integrity.py:L279-L324] |
| `horizontal_lay_tension`, `top_lay_tension` | H = wsub·R; Ttop = H + wsub·d | Free-hanging catenary relation, not a full installation load case. [src/digitalmodel/installation/pipelay/integrity.py:L270-L276] [src/digitalmodel/installation/pipelay/integrity.py:L327-L363] |

Table 3. Complete local calculation inventory; the initializer exports these definitions rather than independently computing them. [src/digitalmodel/installation/pipelay/__init__.py:L13-L53]

UNVERIFIED: the editions, clause titles, coefficient provenance and complete concrete-crushing bibliography are not established; #1813 and issue pending (2) cover the unresolved basis. [src/digitalmodel/installation/pipelay/stress.py:L34-L46] [src/digitalmodel/installation/pipelay/integrity.py:L15-L23]
No `Citation` sidecar is emitted with either result dataclass or the weld-repair scalar; standards provenance remains an open item under #1813. [src/digitalmodel/installation/pipelay/stress.py:L91-L99] [src/digitalmodel/installation/pipelay/stress.py:L155-L161] [src/digitalmodel/installation/pipelay/integrity.py:L96-L104] [src/digitalmodel/installation/pipelay/integrity.py:L244-L257]

## 4. Validated worked examples

These eight closed-form examples use synthetic round-number inputs and independent hand arithmetic stated in the tests, rather than saved output from the implementation; they validate the elementary relation only, not a standard acceptance limit. [tests/installation/test_pipelay_stress.py:L1-L4] [tests/installation/test_pipelay_integrity.py:L1-L4]
Each row uses the existing test's `pytest.approx` assertion; no engineering tolerance is inferred from that numerical comparator. [tests/installation/test_pipelay_stress.py:L24-L39] [tests/installation/test_pipelay_integrity.py:L30-L42]
Offline baseline at the header code pin collected 34 tests: 33 passed and one existing strict expected failure; no tests skipped or NOT_RUN.

| Case | Independent expected value / inputs | Tolerance basis | Existing test node ID | Offline result |
|---|---|---|---|---|
| V1 bending stress | E=200000 stress units, OD=0.5 length, R=250 length → 200 stress units | Existing approximate equality | `tests/installation/test_pipelay_stress.py::test_bending_stress_from_radius` | PASS [tests/installation/test_pipelay_stress.py:L24-L26] |
| V2 axial stress | N=1000 force, A=0.01 length² → 100000 force/length² | Existing approximate equality | `tests/installation/test_pipelay_stress.py::test_axial_stress` | PASS [tests/installation/test_pipelay_stress.py:L37-L39] |
| V3 bending strain | OD=0.5, R=250 consistent length → 0.001 dimensionless | Existing approximate equality | `tests/installation/test_pipelay_integrity.py::test_bending_strain_from_radius` | PASS [tests/installation/test_pipelay_integrity.py:L30-L32] |
| V4 strain concentration | SCF=2, strain=0.001 → 0.002 dimensionless | Existing approximate equality | `tests/installation/test_pipelay_integrity.py::test_concrete_crushing_strain_scf` | PASS [tests/installation/test_pipelay_integrity.py:L40-L42] |
| V5 route radius | F=100000 force, S=0.5, W=2000 force/length → 100 length | Existing approximate equality | `tests/installation/test_pipelay_integrity.py::test_minimum_route_radius` | PASS [tests/installation/test_pipelay_integrity.py:L97-L99] |
| V6 route FoS | Planned=150, minimum=100 consistent length → 1.5 dimensionless | Existing approximate equality | `tests/installation/test_pipelay_integrity.py::test_route_radius_factor_of_safety` | PASS [tests/installation/test_pipelay_integrity.py:L109-L111] |
| V7 horizontal tension | W=1000 force/length, R=300 length → 300000 force | Existing approximate equality | `tests/installation/test_pipelay_integrity.py::test_horizontal_lay_tension` | PASS [tests/installation/test_pipelay_integrity.py:L178-L180] |
| V8 top tension | W=1000 force/length, d=100 length, R=300 length → 400000 force | Existing approximate equality | `tests/installation/test_pipelay_integrity.py::test_top_lay_tension` | PASS [tests/installation/test_pipelay_integrity.py:L183-L185] |

Table 4. Closed-form validation cases; consistent SI assignments give Pa, m and N where those units are used.

| Regression / interpretation case | Existing test node ID | Scope and last offline result |
|---|---|---|
| R1 default coefficients | `tests/installation/test_pipelay_stress.py::test_default_fractions` | Implementation-constant agreement only; PASS. [tests/installation/test_pipelay_stress.py:L125-L127] |
| R2 inverse/check consistency | `tests/installation/test_pipelay_stress.py::test_allowable_bend_radius_consistent_with_check` | Self-consistency, utilisation approximately 1 dimensionless; PASS. [tests/installation/test_pipelay_stress.py:L140-L144] |
| R3 weld interpretation diagnostic | `tests/installation/test_pipelay_integrity.py::test_weld_repair_minimum_fraction_interpretation_unresolved` | Existing strict expected failure; neither a passed case nor independent validation; XFAIL (governing interpretation not established). [tests/installation/test_pipelay_integrity.py:L141-L157] |

Table 5. Three regression/diagnostic examples; no snapshot is promoted to an independent engineering oracle.

## 5. QA gates

The governing family is OrcaFlex installation; AQG r0 §§3–10 supplies the gate framework, while the table below separates this scalar package from an eventual installation model.
Gate dispositions refer to this package only; they do not clear downstream program loading, statics, dynamics or report issue.

| Gate | Applicability and module-specific check | Numeric pass criterion / unit | Evidence and present disposition |
|---|---|---|---|
| G0 | C01: `smys_stress_check` raises `ValueError` for nonzero `axial_force` without `steel_area`, zero `allowable_fraction`, and zero `smys`; run both dedicated test files. C02: check SI buoyancy with `displaced_weight_per_length` and resolve source citations/input revisions | C01: 3 of 3 invalid calls rejected; 0 unexpected failed tests (counts). C02: diameter 0.5 m, density 1025 kg/m³ → 1973.67 N/m within 0.5 N/m; 0 broken citations (count) | Existing guard and buoyancy assertions plus executable check below; this does not detect mixed-unit callers (S04/S05). [tests/installation/test_pipelay_stress.py:L103-L122] [tests/installation/test_pipelay_integrity.py:L163-L168] |
| G1 | NOT_APPLICABLE to the scalar API: no OrcaFlex data file or program loader is produced; downstream C03 will open each consuming installation model in its target OrcaFlex version through the matching demo route or licensed `LoadData` route | Downstream criterion: 0 load errors, 0 warnings and 0 object-count differences (counts) | NOT_RUN downstream: licensed program and model/version evidence are absent; #2431. AQG r0 §5; local exports are calculation functions. [src/digitalmodel/installation/pipelay/__init__.py:L13-L53] |
| G2 | C04: compare `horizontal_lay_tension` and `top_lay_tension` with V7/V8; downstream each lowering stage will require hook/sling hand loads, convergence, component table and configuration images | C04: H=300000 and Ttop=400000 force units, `pytest.approx` default relative 1e-6 or absolute 1e-12, whichever is larger: 0.3 and 0.4 force units respectively; downstream hook load difference ≤ 2.0% | C04 synthetic arithmetic check; full lowering-stage QA NOT_RUN; #2431. AQG r0 §6 and §10. [tests/installation/test_pipelay_integrity.py:L178-L185] |
| G3 | NOT_APPLICABLE to a time-independent scalar API; downstream C05 will compare transient stage stress via `smys_stress_check` and compare dynamic tension with static `top_lay_tension` at splash zone and deepest stage | Downstream: 1 s completed, 0 errors/warnings (counts); tension difference ≤ 25.0%; stress utilisation ≤ 1 dimensionless with qualified input limits | NOT_RUN downstream: no time history or licensed model execution; #2431. AQG r0 §7 and §10. [src/digitalmodel/installation/pipelay/stress.py:L153-L161] [src/digitalmodel/installation/pipelay/integrity.py:L340-L363] |
| G4 | NOT_APPLICABLE: these scalar functions consume neither vessel hydrodynamics nor diffraction meshes; downstream new vessel data will require the diffraction record | Downstream record: 0 missing required G4 records (count) | No diffraction clearance is claimed. AQG r0 §8 and §10. [src/digitalmodel/installation/pipelay/stress.py:L102-L109] [src/digitalmodel/installation/pipelay/integrity.py:L340-L363] |
| G5 | C06: `smys_stress_check` returns `SmysCheck` with `longitudinal_stress`, `allowable_stress`, `utilisation`, `percent_smys` and `passes`; assert the output identities below and retain pin, node/results, citations and issue/draft references | 0 missing or inconsistent fields (count); utilisation = longitudinal_stress/allowable_stress and percent_smys = 100·longitudinal_stress/SMYS within rel 1e-9; 0 missing result/node pairs, untracked open items or altered cited sources (counts) | Executable output check below supplements the existing pure-bending assertions; report appendix/signature NOT_RUN, #2431. AQG r0 §9. [src/digitalmodel/installation/pipelay/stress.py:L91-L99] [src/digitalmodel/installation/pipelay/stress.py:L153-L161] [tests/installation/test_pipelay_stress.py:L50-L67] |

Table 6. Six gates and six check IDs: G0 has C01/C02, G1 C03 downstream, G2 C04, G3 C05 downstream, G4 conditional record only, and G5 C06.

C04 uses independently supplied synthetic inputs from V7/V8; it is not a reference for weld repair or the disputed standard-attributed fractions. [tests/installation/test_pipelay_integrity.py:L178-L185]
C01 does not test rejected non-finite/negative inputs comprehensively; C02 checks SI buoyancy arithmetic and traceability, rather than mixed-unit detection or the truth of licensed-clause attribution. [tests/installation/test_pipelay_stress.py:L115-L122] [tests/installation/test_pipelay_integrity.py:L127-L157]

The following executable C01/C02/C06 check will be run with `PYTHONPATH=src python` from the repository root; all inputs are synthetic and use a consistent unit basis. C06 checks one pure-bending result, not every possible load case.

```python
from dataclasses import asdict
from math import isclose
from digitalmodel.installation.pipelay import (
    SmysCheck, displaced_weight_per_length, smys_stress_check,
)

inputs = dict(outer_diameter=0.5, bend_radius=250.0,
              youngs_modulus=200000.0, smys=450.0, allowable_fraction=0.8)
rejected = 0
for invalid in ({"axial_force": 1.0}, {"allowable_fraction": 0.0}, {"smys": 0.0}):
    try:
        smys_stress_check(**(inputs | invalid))
    except ValueError:
        rejected += 1
assert rejected == 3
buoyancy = displaced_weight_per_length(0.5, 1025.0)
assert abs(buoyancy - 1973.67) <= 0.5
result = smys_stress_check(**inputs)
assert isinstance(result, SmysCheck)
assert set(asdict(result)) == {
    "longitudinal_stress", "allowable_stress", "utilisation", "percent_smys", "passes",
}
assert isclose(result.longitudinal_stress, 200.0, rel_tol=1e-9)
assert isclose(result.allowable_stress, 360.0, rel_tol=1e-9)
assert isclose(result.utilisation,
               result.longitudinal_stress / result.allowable_stress, rel_tol=1e-9)
assert isclose(result.percent_smys,
               100.0 * result.longitudinal_stress / inputs["smys"], rel_tol=1e-9)
assert result.passes is (result.longitudinal_stress <= result.allowable_stress)
print("C01: 3/3 rejected; C02: SI buoyancy passed; C06: 0 missing or inconsistent fields")
```

## 6. Known failure modes and pitfalls

sweep: 21 found

The count records default, unit and criterion candidates, including documented non-defects; it does not mean 21 confirmed implementation defects.
The following exact text sweep was run over all package Python files; zero swallowed exceptions, record-skipping `continue` statements, error-path None/empty returns, external-text numeric conversions, epoch defaults or cache writes were found in those files. [src/digitalmodel/installation/pipelay/__init__.py:L1-L53] [src/digitalmodel/installation/pipelay/stress.py:L1-L196] [src/digitalmodel/installation/pipelay/integrity.py:L1-L363]

```sh
rg -n -i 'except|continue|return None|return \[\]|return \{\}|= *0(\.0)?\b|1970|int\(|float\(|unit|default|cache|0\.05|0\.050' src/digitalmodel/installation/pipelay
```

| ID / location at pin | Symptom / disposition | Detection and tracking |
|---|---|---|
| S01 `stress.py:45` | Overbend coefficient is presented as common rather than clause-qualified | C01 regression R1 checks storage only; no detection check exists for engineering basis; #1813. [src/digitalmodel/installation/pipelay/stress.py:L34-L46] |
| S02 `stress.py:46` | Sagbend coefficient has the same provenance gap | C01/R1 only; no detection check exists for engineering basis; #1813. [src/digitalmodel/installation/pipelay/stress.py:L34-L46] |
| S03 `stress.py:108,147-148` | Omitted axial force silently selects pure bending; zero initialization is deliberate | C01 pure-bending/axial cases detect route changes, not omitted design loads; no detection check exists for omitted loads; issue pending (2). [src/digitalmodel/installation/pipelay/stress.py:L108-L109] [src/digitalmodel/installation/pipelay/stress.py:L146-L151] [tests/installation/test_pipelay_stress.py:L50-L86] |
| S04 `stress.py:30-32` | MPa with N/m² example can combine incompatible stresses | No detection check exists; issue pending (1). [src/digitalmodel/installation/pipelay/stress.py:L30-L32] [src/digitalmodel/installation/pipelay/stress.py:L148-L153] |
| S05 `integrity.py:12-13` | Consistent-unit assumption performs no conversion | No detection check exists for mixed units; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L12-L13] |
| S06 `integrity.py:111` | Default SCF selects no concentration unless explicitly overridden | C01 concentration case checks multiplication; no detection check exists for unsuitable default; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L107-L112] [tests/installation/test_pipelay_integrity.py:L40-L47] |
| S07 `integrity.py:112` | Default FoS selects no additional reduction | C01 checks a supplied FoS and strict boundary; no detection check exists for unsuitable default; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L107-L145] [tests/installation/test_pipelay_integrity.py:L67-L78] |
| S08 `integrity.py:123` | Example crushing strain could be mistaken for a qualified material limit; it is not an argument default | No detection check exists for material provenance; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L118-L127] |
| S09 `integrity.py:195` | Suggested route FoS target is only docstring advice; return has no pass flag | C01/V6 checks the ratio only; no detection check exists for target provenance; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L192-L199] |
| S10 `integrity.py:213` | Partial-repair fraction has no edition/clause locator | No detection check exists for standards qualification; #1813. [src/digitalmodel/installation/pipelay/integrity.py:L203-L215] |
| S11 `integrity.py:214` | Full-repair fraction has no edition/clause locator | No detection check exists for standards qualification; #1813. [src/digitalmodel/installation/pipelay/integrity.py:L203-L215] |
| S12 `integrity.py:215` | Minimum repair constant is in metres, despite generic length arguments | No detection check exists for non-metre callers; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L213-L242] |
| S13 `integrity.py:221` | Allowable helper defaults minimum, returns the larger bound | C01/R3 records unresolved semantics; issue pending (3). [src/digitalmodel/installation/pipelay/integrity.py:L218-L246] [tests/installation/test_pipelay_integrity.py:L141-L157] |
| S14 `integrity.py:253` | Predicate defaults minimum but caps at fractional bound; allowable helper can return a rejected length | C01/R3 strict expected failure, not a qualified reference; issue pending (3). [src/digitalmodel/installation/pipelay/integrity.py:L249-L257] [tests/installation/test_pipelay_integrity.py:L141-L157] |
| S15 `integrity.py:32` | Gravity is a fixed SI convention, not local measured gravity | C01 buoyancy test uses the same constant; no detection check exists for site-specific gravity; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L31-L32] [tests/installation/test_pipelay_integrity.py:L163-L168] |
| S16 `integrity.py:280` | Buoyancy defaults gravity and assumes metre diameter / SI density | C01 buoyancy arithmetic test only; no detection check exists for mixed-unit caller; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L279-L295] |
| S17 `integrity.py:302` | Wet weight separately defaults gravity and delegates buoyancy | C01 wet-weight test only; no detection check exists for wrong unit basis; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L298-L324] [tests/installation/test_pipelay_integrity.py:L171-L175] |
| S18 `integrity.py:265-268` | Buoyancy expects total coated OD; stress/strain expects steel OD | No detection check exists for swapping those diameters; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L264-L268] [src/digitalmodel/installation/pipelay/stress.py:L119-L120] |
| S19 `integrity.py:320,337,363` | Net-up weight or negative depth/radius propagates to tension without rejection | C04 validates positive inputs only; no detection check exists for invalid-sign inputs; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L317-L337] [src/digitalmodel/installation/pipelay/integrity.py:L340-L363] |
| S20 `stress.py:153-160` | Compression can cancel bending in the single signed sum; opposite fibre is not checked | No detection check exists for compressive envelope; issue pending (2). [src/digitalmodel/installation/pipelay/stress.py:L146-L161] |
| S21 `integrity.py:134-145` | Zero crushing limit divides by zero; a negative limit can yield negative utilisation and pass | No detection check exists; issue pending (2). [src/digitalmodel/installation/pipelay/integrity.py:L134-L145] |

Table 7. Sweep candidates and detection limits, recorded 2026-10-16; zero-threshold guards are explicit errors rather than silent fallbacks. [src/digitalmodel/installation/pipelay/stress.py:L71-L88] [src/digitalmodel/installation/pipelay/integrity.py:L185-L199]

Additional pitfalls: negative/zero diameter or modulus, non-finite values, negative lengths and unchecked predicate fractions are not comprehensively rejected; no detection check exists, issue pending (2). [src/digitalmodel/installation/pipelay/stress.py:L71-L88] [src/digitalmodel/installation/pipelay/stress.py:L192-L196] [src/digitalmodel/installation/pipelay/integrity.py:L244-L257] [src/digitalmodel/installation/pipelay/integrity.py:L295-L295]
The repository issue search supplied no confirmed package-specific incident; these rows are source findings, not claims of a past field failure.

## 7. Lessons log

| Date | Change reference | Lesson / evidence | Pack change | Shared-log entry |
|---|---|---|---|---|
| 2026-10-16 | Initial pack revision 0 | A returned weld length is not necessarily accepted by the predicate when bounds cross. [src/digitalmodel/installation/pipelay/integrity.py:L244-L257] | Sections 3, 4 and 6 distinguish the unresolved interpretation from validation | Not assigned; issue pending (3) |
| 2026-10-16 | Initial pack revision 0 | Unit-consistency claims do not supply a conversion; direct N/A has force/area units. [src/digitalmodel/installation/pipelay/stress.py:L30-L32] [src/digitalmodel/installation/pipelay/stress.py:L76-L88] | Sections 2 and 6 identify the mixed-unit example | Not assigned; issue pending (1) |

Table 8. Append-only public lessons; no client-derived data or private sidecar entry is used.

## 8. Ownership

Required review role: installation quality manager on a provider independent of the producer.
Required deputy role: installation quality-manager deputy; appointment remains with the maintainers under #2431.
The producing role is the calculation-documentation contributor; unresolved engineering interpretation will go to the installation quality manager, with cross-family issues referred to the maintainers.
Review clearance, a named deputy and a shared-log registration are not established by this draft; #2431 remains the installation integration follow-up.

## 9. Dependencies and implementation inventory

The local calculations depend on Python `math` and `dataclasses`, with the initializer re-exporting the integrity and stress APIs. [src/digitalmodel/installation/pipelay/integrity.py:L26-L29] [src/digitalmodel/installation/pipelay/stress.py:L40-L42] [src/digitalmodel/installation/pipelay/__init__.py:L13-L53]
The downstream modular-generator line builder imports `G_STANDARD` when constructing its analytical S-lay seed; it is not a direct call to the local stress or tension helpers. [src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L273-L292]
No external dataset or solver version is an argument of these scalar methods. [src/digitalmodel/installation/pipelay/stress.py:L102-L109] [src/digitalmodel/installation/pipelay/integrity.py:L279-L303] [src/digitalmodel/installation/pipelay/integrity.py:L340-L342]

### OrcaFlex text dependency inventory

Case-insensitive source search `rg -ni 'orcaflex|orcfxapi' src/digitalmodel/installation/pipelay --glob '*.py'` finds only the adjacent module reference in the package docstring, `digitalmodel.orcaflex.pipelay_analysis`; it is read as text, not imported by this package. [src/digitalmodel/installation/pipelay/__init__.py:L8-L10] [src/digitalmodel/installation/pipelay/__init__.py:L13-L33]
There are no literal `OrcaFlex` or `OrcFxAPI` mentions in the other two package source files, and no licensed solver is required by the local calculation definitions. [src/digitalmodel/installation/pipelay/stress.py:L1-L196] [src/digitalmodel/installation/pipelay/integrity.py:L1-L363]
The adjacent analytical module explicitly describes OrcaFlex pre-processing without requiring its licensed interface. [src/digitalmodel/orcaflex/pipelay_analysis.py:L1-L12]

### Map pin and counted families

Implementation-map document: `docs/registry/implementation-map.yaml`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`, read from the map at the header code revision `6cf35f466e5c1b730951af045d8cfe0065af073f`. [docs/registry/implementation-map.yaml:L1-L4]
The inventory was computed from all 13 families’ `implementations` rows with a path strictly inside the header package, excluding the map’s `rejected_candidates` collection; its empty result is reproduced by the procedure below.

| Accepted in-package family inventory | In-package rows | Counted-family grep |
|---|---|---|
| Empty | 0 | NOT_APPLICABLE: no family has an accepted in-package row |

Table 9. Empty inventory; rejected textual candidates do not make a counted family.

The reproducible inventory procedure is:

```python
import yaml
m = yaml.safe_load(map_text_at_named_revision)
prefix = 'src/digitalmodel/installation/pipelay/'
rows = [(f['id'], [r for r in f['implementations']
                   if r['path'].startswith(prefix)]) for f in m['families']]
assert not [(name, entries) for name, entries in rows if entries]
```

No family signals are ranked, excluded or grepped for the counted-family criterion because its inventory is empty; the broader repository search below is a follow-up finding only, issue pending (4).

### Whole-repository search and map follow-up

The source search covered every tracked Python file at the code pin, including `src`, `tests`, `scripts`, `examples` and Python examples under `docs`; it was not limited to the package.

```sh
git grep -n -E 'bending_stress_from_radius|top_lay_tension|minimum_route_radius|crushing_strain|weld_repair_allowable_length|youngs_modulus.*radius|bending_strain =.*D /' 6cf35f466e5c1b730951af045d8cfe0065af073f -- '*.py'
rg -n -i 'bending_stress|sagbend|weld_repair|crushing_strain|minimum_route_radius|top_lay_tension' scripts --glob '*.py'
```

| Other implementation / helper | Classification and reason | Map follow-up |
|---|---|---|
| `PipelayPipeProperties.submerged_weight_per_m` | Independent coated-weight/buoyancy arithmetic. [src/digitalmodel/orcaflex/pipelay_analysis.py:L82-L91] | Add buoyancy / submerged-weight family rows; issue pending (4) |
| `SLayConfig.calculate_overbend_stress` | Independent curvature/strain/stress and minimum-radius arithmetic. [src/digitalmodel/orcaflex/pipelay_analysis.py:L112-L134] | Add stress/strain/bend-radius family rows; issue pending (4) |
| `SLayConfig.calculate_sagbend_stress` | Independent horizontal-tension/radius and bending calculation. [src/digitalmodel/orcaflex/pipelay_analysis.py:L136-L169] | Review catenary/tension family membership; issue pending (4) |
| `JLayConfig.calculate_sagbend` | Independent catenary and strain calculation; outside the S-lay package. [src/digitalmodel/orcaflex/pipelay_analysis.py:L222-L260] | Review catenary/tension family membership; issue pending (4) |
| `calculate_stinger_radius` | Independent inversion of bending strain. [src/digitalmodel/orcaflex/pipelay_analysis.py:L307-L324] | Add curvature/radius family rows; issue pending (4) |
| `bending_from_radius` | Independent curvature-derived bending stress in connector screening. [src/digitalmodel/subsea/connectors/bending_analysis.py:L36-L73] | Add bending-stress family membership review; issue pending (4) |
| `armour_bending_stress` | Related independent wire-bending formula with wire thickness as lever arm; not interchangeable with steel-pipe OD. [src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py:L168-L175] | Review related curvature-stress family scope; issue pending (4) |
| `run_reel_lay` | Independent reeling/residual-curvature computation; `run_jlay` delegates to the adjacent J-lay class. [src/digitalmodel/pipelay/workflow.py:L62-L94] [src/digitalmodel/pipelay/workflow.py:L111-L154] | Separate independent formula from wrapper; issue pending (4) |
| `LinesBuilder._slay_taut_span` | Independent uniform coated-weight and static seed arithmetic; importing gravity does not delegate the algorithm. [src/digitalmodel/solvers/orcaflex/modular_generator/builders/lines_builder.py:L273-L292] | Review catenary/buoyancy map membership; issue pending (4) |

Table 10. Source-confirmed counterparts; outside-package findings are map gaps, not counted-family misses.

Map follow-up will name every section-3 family: longitudinal stress / bend radius, concrete strain/crushing, stable route radius, weld-repair limits, buoyancy/wet weight and lay tension; issue pending (4).
The broader search does not establish exhaustive detection of differently worded formulas or dynamic imports; that limit stays with issue pending (4).

## 10. Coverage and open items

All three package source files were read in full: `__init__.py`, `stress.py` and `integrity.py`; there are no nested source subpackages in the scoped package at the pin. [src/digitalmodel/installation/pipelay/__init__.py:L1-L53] [src/digitalmodel/installation/pipelay/stress.py:L1-L196] [src/digitalmodel/installation/pipelay/integrity.py:L1-L363]
The dedicated tests comprise 14 stress definitions and 20 integrity definitions, including the strict expected-failure diagnostic. [tests/installation/test_pipelay_stress.py:L24-L149] [tests/installation/test_pipelay_integrity.py:L30-L190]
No package README or pre-existing local docs were found; the relevant cross-module document is `docs/domains/pipeline-installation/coverage-2026-10.md`, whose API inventory and standard-locator gaps were read against the source. [docs/domains/pipeline-installation/coverage-2026-10.md:L67-L107]
Related docs discovered and read for context include `docs/maps/digitalmodel-operator-map.md`, `docs/registry/implementation-map.md`, `docs/migration/legacy-engineering-flow-catalog.md`, `docs/research/subsea-pipelines-industry-practice.md` and `docs/domains/_index.md`; these broader inventories do not qualify this package's coefficients. [docs/maps/digitalmodel-operator-map.md:L89-L94] [docs/registry/implementation-map.md:L13-L30] [docs/migration/legacy-engineering-flow-catalog.md:L23-L27] [docs/research/subsea-pipelines-industry-practice.md:L21-L36] [docs/domains/_index.md:L50-L56]
Other broad keyword matches concern solver catalogues, automation and data procurement rather than this package's API; no identifying discovery filenames or model values are reproduced.
No private data repository, licensed publication or solver interface was read or executed.
The calculation-output citation contract was readable; it requires sidecar provenance for standards-derived calculations, while the local return types contain only numeric/boolean outputs. [src/digitalmodel/installation/pipelay/stress.py:L91-L99] [src/digitalmodel/installation/pipelay/integrity.py:L96-L104]

| Open item / limit | Disposition and responsible role | Tracking |
|---|---|---|
| Unresolved basis in section 3: editions, clause titles, coefficients and complete concrete-crushing bibliography | Installation quality manager will establish the basis; clause text not checked against the licensed copy; missing sidecars will remain explicit | #1813; issue pending (2) |
| Mixed stress-unit example and inconsistent conversions | Maintainers will correct documentation and require an equivalence check | issue pending (1) |
| Caller validation, finite values, signed stress envelope and silent/default assumptions S03–S09, S12, S15–S21 | Installation quality manager will define supported input domain before implementation changes | issue pending (2) |
| Weld-repair interpretation, expected failure and missing shared-log entry | Installation quality manager will resolve the governing contract; no standard-based weld validation is claimed | issue pending (3) |
| Empty accepted map inventory and all section-3 method families | Maintainers will add qualified rows before those families enter the counted-family criterion | issue pending (4) |
| Stale initializer scope | Maintainers will align scope text with exports | issue pending (5) |
| No standard-qualified worked installation case, native loading, lowering stages, splash-zone/deepest-stage dynamics, diffraction linkage or signed report appendix | Independent installation quality manager will review sourced examples and native evidence; scalar hand checks do not satisfy those integration gates | #2431 |
| Deputy appointment, independent clearance and shared knowledge-log registration | Maintainers will appoint the deputy and record clearance after independent review | #2431 |
| Registry baseline has duplicate `submodules` keys in the solver entry, outside the changed installation entry | Maintainers will consolidate both mappings and add strict duplicate-key coverage; no existing entry is changed here | issue pending (6). [docs/registry/module-routing.yaml:L297-L322] |

Table 11. Open coverage and qualification items; draft references will be replaced by issue numbers before final clearance.
