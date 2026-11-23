# Riser screening knowledge pack

| Header | Value |
| --- | --- |
| Module | subsea/riser |
| Repository | digitalmodel |
| Package path | src/digitalmodel/subsea/riser |
| Pack revision | 0 |
| Code revision | c099a5e1317e6bed63d876252d87cbb12c774372 |
| Analysis family | Engineering calculations (EC); Python calculations, AQG r0 §10 |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`screen_riser_cases` passes each supplied load case to the selected registered strength strategy, selects its maximum utilisation and returns cases in descending order; equal values retain input order. [src/digitalmodel/subsea/riser/riser_case_screening.py:L66-L122]

`run_riser_screening` supplies the `riser_screening` engine route and writes Markdown and JSON; this is supplied-load screening, with pressures, tension and moment taken from configuration rather than solved from environmental loads. [src/digitalmodel/engine.py:L602-L607] [src/digitalmodel/subsea/riser/riser_case_screening.py:L135-L198]

The parent routing entry records maturity `stable`; this is routing metadata, not independent qualification of the screening method. [docs/registry/module-routing.yaml:L419-L424]

The package exports `RiserLoadCase`, `CaseScreenResult`, `screen_riser_cases` and `run_riser_screening`; `build_calc_note` is also defined in its implementation module. [src/digitalmodel/subsea/riser/__init__.py:L4-L16] [src/digitalmodel/subsea/riser/riser_case_screening.py:L204-L211]

## 2. Inputs and outputs

Inputs use SI scalar loads, with no spatial coordinates, phase or reference-frame arguments; pressures are distinct internal/external values, and the API combined strategy uses absolute tension and moment. [src/digitalmodel/subsea/riser/riser_case_screening.py:L33-L50] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L191-L206]

| Input | Unit | Range / default / source |
| --- | --- | --- |
| `geometry.outer_diameter`, `wall_thickness` | m | Required; diameter and thickness positive, thickness less than half diameter. [src/digitalmodel/structural/analysis/wall_thickness.py:L88-L109] |
| `geometry.corrosion_allowance` | m | Nonnegative; router default 0.0; the API strategy reads nominal wall thickness. [src/digitalmodel/subsea/riser/riser_case_screening.py:L139-L143] [src/digitalmodel/structural/analysis/wall_thickness.py:L106-L109] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L88-L93] |
| `material.grade`, `smys`, `smts` | text; Pa; Pa | Grade defaults to `unspecified`; strengths required, positive, and SMTS at least SMYS. [src/digitalmodel/subsea/riser/riser_case_screening.py:L144-L150] [src/digitalmodel/structural/analysis/wall_thickness.py:L143-L151] |
| `youngs_modulus`, `poissons_ratio` | Pa; dimensionless | Router defaults 207e9 Pa and 0.3; material validates positive modulus but has no Poisson-ratio check in its constructor. [src/digitalmodel/subsea/riser/riser_case_screening.py:L144-L150] [src/digitalmodel/structural/analysis/wall_thickness.py:L132-L151] |
| Case `name`, `category` | text | Name required; category defaults to `operating` in the router and is retained as metadata; it is not passed to `DesignLoads`. [src/digitalmodel/subsea/riser/riser_case_screening.py:L44-L50] [src/digitalmodel/subsea/riser/riser_case_screening.py:L151-L164] |
| `internal_pressure`, `external_pressure` | Pa | Both default to 0.0; no range validation in `RiserLoadCase`. [src/digitalmodel/subsea/riser/riser_case_screening.py:L33-L50] |
| `bending_moment`, `effective_tension` | N·m; N | Both default to 0.0; directly transferred to `DesignLoads`. [src/digitalmodel/subsea/riser/riser_case_screening.py:L39-L50] |
| `code`, `factors` | enumeration; dimensionless | Default API STD 2RD; default `DesignFactors()`; router exposes code but does not read factors from configuration. [src/digitalmodel/subsea/riser/riser_case_screening.py:L66-L96] [src/digitalmodel/subsea/riser/riser_case_screening.py:L163-L164] |
| `output_dir`, `Analysis.result_folder`, `label` | path; text | Output directory falls back to `results/riser_screening`; label defaults to `riser_screening`. [src/digitalmodel/subsea/riser/riser_case_screening.py:L187-L198] |

| Output | Unit / representation | Contract |
| --- | --- | --- |
| `CaseScreenResult.checks`, `max_utilisation`, `governing_check`, `status` | dimensionless ratios; text | Maximum over returned checks; PASS at utilisation ≤ 1.0; no checks gives zero and no governing check. [src/digitalmodel/subsea/riser/riser_case_screening.py:L94-L113] |
| `CaseScreenResult.details` | strategy-specific dictionary | Preserves strategy details in the Python result; router JSON omits these details. [src/digitalmodel/subsea/riser/riser_case_screening.py:L97-L110] [src/digitalmodel/subsea/riser/riser_case_screening.py:L174-L184] |
| `cfg.riser_screening.result` | dictionary / JSON | Code, governing case/check, maximum, overall status and ranked cases; empty cases produce overall PASS. [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L185] |
| `<label>_calcnote.md` | UTF-8 Markdown | Geometry shown in mm, strengths in MPa; utilisation table uses the API-specific burst/collapse/combined columns. [src/digitalmodel/subsea/riser/riser_case_screening.py:L194-L195] [src/digitalmodel/subsea/riser/riser_case_screening.py:L212-L256] |
| `<label>_results.json` | UTF-8 JSON | Serialises the result dictionary with indentation; no strict non-finite-number option is supplied. [src/digitalmodel/subsea/riser/riser_case_screening.py:L196-L198] |

## 3. Method and governing standards

The module's own algorithm is strategy dispatch, maximum selection, threshold comparison and stable ranking; strength equations live upstream rather than in `screen_riser_cases`. [src/digitalmodel/subsea/riser/riser_case_screening.py:L89-L122]

| Basis | Implementing definition and edition evidence | Checked scope |
| --- | --- | --- |
| API STD 2RD, *Dynamic Risers for Floating Production Systems*, 2013; Section 5 (LRFD), Method 1 as named by the note | `ApiStd2rdStrategy._check_burst`, `_check_collapse`, `_check_combined`; burst resistance, elastic/yield collapse transition and pressure-corrected linear moment/tension interaction. The edition comes from the shared code reference, while the router stores only the enumeration value. [docs/domains/risers/scr-slwr-coverage-2026-10.md:L49-L57] [src/digitalmodel/codes.py:L74-L76] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L83-L159] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L161-L226] [src/digitalmodel/subsea/riser/riser_case_screening.py:L260-L273] | Clause text not checked against the licensed copy; no tabulated factor is reproduced here. |
| DNV-ST-F201, *Dynamic Risers*, 2021, Section 5, “Bursting” as named in code | `DnvStF201Strategy._check_pressure_containment`, registered and imported by the strategy package; `run_checks` returns pressure containment only. [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L24-L28] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L130-L162] [src/digitalmodel/structural/analysis/wall_thickness_codes/__init__.py:L4-L9] | Clause text not checked against the licensed copy; no combined/collapse capability is established by this strategy. [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L130-L141] |

The DNV strategy and shared reference both name 2021; the API strategy supplies no edition selector, and the note omits the edition, so an edition disagreement is not established by these files. [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L24-L28] [src/digitalmodel/codes.py:L74-L76] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L40-L51] [src/digitalmodel/subsea/riser/riser_case_screening.py:L219-L220]

The note's statement that DNV-ST-F201 is unimplemented conflicts with the imported burst-only strategy; category metadata also does not select category-specific strength factors. These are coverage limits, not standard-compliance claims. [src/digitalmodel/subsea/riser/riser_case_screening.py:L275-L276] [src/digitalmodel/structural/analysis/wall_thickness_codes/__init__.py:L4-L9] [src/digitalmodel/subsea/riser/riser_case_screening.py:L44-L50] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L40-L51]

The JSON result and note emit no `Citation` sidecar or edition-aware reference; provenance follow-up is recorded in section 10. [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L198] [src/digitalmodel/subsea/riser/riser_case_screening.py:L219-L278]

UNVERIFIED: Exact-edition compliance and independent numerical qualification of the strength methods are not established; issue pending (3).

## 4. Validated worked examples

Validated examples: 0; the existing reference comparison imports another repository calculation and compensates its internal unit scales, so it is regression evidence rather than an independent worked example. [tests/test_riser_case_screening.py:L128-L177]

Regression examples below use inputs constructed in the test file; PASS means the node passed offline at the code pin, not licensed-standard compliance. [tests/test_riser_case_screening.py:L57-L64] [tests/test_riser_case_screening.py:L207-L213]

| Regression case / expected result and tolerance | Existing test node ID | Offline result at code pin |
| --- | --- | --- |
| Three exact check names, 0 missing names. [tests/test_riser_case_screening.py:L76-L77] | `tests/test_riser_case_screening.py::test_check_names_includes_combined` | PASS |
| Three results, positive combined utilisation and capacities; dimensionless utilisation > 0. [tests/test_riser_case_screening.py:L80-L91] | `tests/test_riser_case_screening.py::test_run_checks_returns_combined` | PASS |
| Zero loads: utilisation exactly 0.0 dimensionless. [tests/test_riser_case_screening.py:L94-L98] | `tests/test_riser_case_screening.py::test_combined_zero_loads` | PASS |
| Larger moment increases dimensionless utilisation; positive difference. [tests/test_riser_case_screening.py:L101-L109] | `tests/test_riser_case_screening.py::test_combined_monotonic_in_moment` | PASS |
| Larger pressure reduces corrected factor and increases utilisation; strict inequalities. [tests/test_riser_case_screening.py:L112-L121] | `tests/test_riser_case_screening.py::test_combined_pressure_reduces_capacity` | PASS |
| Legacy Method 1 case 1: dimensionless utilisation equal within relative `1e-9` or absolute `1e-12`; numerical parity tolerance, not a standard limit. [tests/test_riser_case_screening.py:L128-L177] | `tests/test_riser_case_screening.py::test_combined_matches_legacy_method1[0.27305-0.02144-448000000.0-531000000.0-20000000.0-10000000.0-250000.0-1500000.0]` | PASS |
| Legacy Method 1 case 2: dimensionless utilisation equal within relative `1e-9` or absolute `1e-12`; numerical parity tolerance, not a standard limit. [tests/test_riser_case_screening.py:L128-L177] | `tests/test_riser_case_screening.py::test_combined_matches_legacy_method1[0.5334-0.0254-448000000.0-531000000.0-30000000.0-15000000.0-500000.0-3000000.0]` | PASS |
| Legacy Method 1 case 3: dimensionless utilisation equal within relative `1e-9` or absolute `1e-12`; numerical parity tolerance, not a standard limit. [tests/test_riser_case_screening.py:L128-L177] | `tests/test_riser_case_screening.py::test_combined_matches_legacy_method1[0.3239-0.019-358000000.0-455000000.0-15000000.0-5000000.0-100000.0-800000.0]` | PASS |
| Legacy Method 1 case 4: dimensionless utilisation equal within relative `1e-9` or absolute `1e-12`; numerical parity tolerance, not a standard limit. [tests/test_riser_case_screening.py:L128-L177] | `tests/test_riser_case_screening.py::test_combined_matches_legacy_method1[0.27305-0.02144-552000000.0-621000000.0-25000000.0-8000000.0-400000.0-2000000.0]` | PASS |
| Corrected factor matches legacy within relative `1e-12`, dimensionless. [tests/test_riser_case_screening.py:L180-L199] | `tests/test_riser_case_screening.py::test_pressure_corrected_factor_matches_legacy` | PASS |
| Four cases preserved, 0 descending-order violations; named governing checks. [tests/test_riser_case_screening.py:L216-L227] | `tests/test_riser_case_screening.py::test_screen_ranks_by_max_utilisation` | PASS |
| Two repeated runs: 0 tuple mismatches. [tests/test_riser_case_screening.py:L230-L236] | `tests/test_riser_case_screening.py::test_screen_deterministic` | PASS |
| All cases have exactly three API checks. [tests/test_riser_case_screening.py:L239-L243] | `tests/test_riser_case_screening.py::test_screen_uses_api_std_2rd_by_default` | PASS |
| Explicit and default API selection: 0 utilisation mismatches. [tests/test_riser_case_screening.py:L246-L255] | `tests/test_riser_case_screening.py::test_screen_explicit_api_std_2rd` | PASS |

Offline execution record: `PYTHONPATH=src uv run --no-sources python -m pytest tests/test_riser_case_screening.py tests/docs/test_digitalmodel_routing_contract.py -q -rA -p no:cacheprovider --basetemp=<scratch>`; BASE: 24 run, 24 passed, 0 failed, 0 skipped, 0 NOT_RUN, including 14 module nodes and 10 routing nodes; code pin `c099a5e1317e6bed63d876252d87cbb12c774372`. AFTER: 24 run, 24 passed, 0 failed, 0 skipped, 0 NOT_RUN; identical to BASE.

The separate `tests/contracts` domain BASE run used the same form: 487 run, 487 passed, 0 failed, 0 skipped, 0 NOT_RUN; AFTER: 487 run, 487 passed, 0 failed, 0 skipped, 0 NOT_RUN; identical to BASE. No solver or licensed interface was used.

## 5. QA gates

Gate assignment follows AQG r0 §10, “Python calculations”, and §9, “G5 QA appendix”; these are pack check requirements, not a clearance signature.

| Gate | Applicability and route | Module-specific check / numerical pass criterion / evidence |
| --- | --- | --- |
| G0 | Applicable: SI input and method-reference audit | C0: `RiserLoadCase.to_loads` preserves all four explicitly supplied SI load components with 0 mismatches (Pa, Pa, N·m, N); hand-check the constructor against the source. Scratch probe PASS, 0 mismatches. Defaults and validation gaps remain in section 6. [src/digitalmodel/subsea/riser/riser_case_screening.py:L39-L50] |
| G1 | Applicable: Python import and tests; no target solver file is written | C1: run the section-4 command: 14/14 module nodes pass, 0 import errors (counts); add the 10 routing nodes and the contracts domain for registry checks. BASE PASS. [tests/test_riser_case_screening.py:L10-L24] [tests/test_riser_case_screening.py:L76-L255] [tests/docs/test_digitalmodel_routing_contract.py:L101-L120] |
| G2 | Applicable: hand replay and numerical invariants | C2: balanced zero pressure, zero moment and zero tension give combined utilisation exactly 0.0 (dimensionless); existing node and scratch hand check PASS. [tests/test_riser_case_screening.py:L94-L98] |
| G2 | Applicable: implementation arithmetic check | C3: independently evaluate the implemented area, capacities and combined-ratio arithmetic outside the strategy on the first regression input; absolute utilisation difference ≤ `1e-12` (dimensionless). Scratch replay PASS; this checks arithmetic only, not the standard's physical correctness. Legacy parity remains regression evidence. [tests/test_riser_case_screening.py:L131-L155] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L186-L206] |
| G3 | NOT_APPLICABLE | No time integration or dynamics solve is called; screening dispatches static supplied loads. [src/digitalmodel/subsea/riser/riser_case_screening.py:L89-L122] |
| G4 | NOT_APPLICABLE | No diffraction-body, mesh or hydrostatics result is produced by this route. [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L198] |
| G5 | Applicable: output evidence audit | C4: boundary audit requires 0 empty-case false PASS records and 0 missing-check `nan` cells (counts); run empty-case and DNV-selected synthetic configurations in scratch and inspect result/note. FAIL at the code pin: 1 empty-case false PASS and 3 `nan` cells; source evidence unchanged on main. Follow-up: issue pending (1), #878. [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L172] [src/digitalmodel/subsea/riser/riser_case_screening.py:L249-L256] |
| G5 | Applicable: serialisation and traceability audit | C5: one synthetic API case produces exactly 1 Markdown note and 1 JSON file, with 1 case, 3 check values and 0 serialisation mismatches (counts); scratch probe PASS. Full fingerprints, signatures and an AQG evidence appendix are absent from these outputs; #814 holds the report-provenance gap. [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L198] |

C0, C2, C3, C4 and C5 are reproducible hand/probe procedures defined by the table; their synthetic geometry/material and first nonzero load input come from the existing fixtures and first parameter row. C1 is the existing offline suite. [tests/test_riser_case_screening.py:L57-L64] [tests/test_riser_case_screening.py:L131-L131]

## 6. Known failure modes and pitfalls

sweep: 35 found

The sweep covers the complete two-file package and groups repeated occurrences only where a row cites both locations; no `except`, `continue`, error-returned empty list/dict, epoch default or external-text numeric parsing is present in those files. [src/digitalmodel/subsea/riser/__init__.py:L1-L16] [src/digitalmodel/subsea/riser/riser_case_screening.py:L1-L278]

Exact search commands (then every match and the surrounding definition were read):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|5 ?%|float\(|int\(|\.get\(|units|write_text|json.dump' src/digitalmodel/subsea/riser --glob '*.py'
rg -n 'else 0|else None|all\(|default|SI|1000|1e6|nan|PASS' src/digitalmodel/subsea/riser --glob '*.py'
```

These are findings of the code pin; defaults are documented as assumptions rather than all being asserted defective. All rows without an existing detection check explicitly say so.

| ID | Silent/default behaviour and possible symptom | Detection / follow-up | Evidence at code pin |
| --- | --- | --- | --- |
| S01 | SI scalar inputs lack unit metadata; a different unit scale is accepted without conversion | C0 checks transfer only; no detection check exists for wrong declared scale; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L12-L13] |
| S02 | Missing internal pressure defaults to zero | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L39-L39] |
| S03 | Missing external pressure defaults to zero | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L40-L40] |
| S04 | Missing moment defaults to zero | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L41-L41] |
| S05 | Missing effective tension defaults to zero | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L42-L42] |
| S06 | Result checks default to an empty dictionary | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L59-L59] |
| S07 | Result details default to an empty dictionary | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L60-L60] |
| S08 | Default governing check is absent | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L61-L61] |
| S09 | Default maximum utilisation is zero | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L62-L62] |
| S10 | Default result status is PASS before evidence is supplied | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L63-L63] |
| S11 | Missing factor object is replaced with default factors | no detection check exists for an undeclared factor assumption; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L86-L87] |
| S12 | Empty strategy checks become zero utilisation | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L100-L100] |
| S13 | Empty strategy checks have no governing check but can pass | no detection check exists; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L101-L102] |
| S14 | Omitted corrosion allowance becomes zero | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L142-L142] |
| S15 | Omitted grade becomes unspecified text | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L145-L145] |
| S16 | Omitted modulus uses an assumed material value | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L148-L148] |
| S17 | Omitted Poisson ratio uses an assumed material value | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L149-L149] |
| S18 | Omitted category becomes operating metadata | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L154-L154] |
| S19 | Router missing internal pressure becomes zero | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L155-L155] |
| S20 | Router missing external pressure becomes zero | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L156-L156] |
| S21 | Router missing bending moment becomes zero | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L157-L157] |
| S22 | Router missing effective tension becomes zero | no detection check exists; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L158-L158] |
| S23 | Missing router design-code key chooses API | C1 checks default equivalence; issue pending (2) for explicit assumption disclosure | [src/digitalmodel/subsea/riser/riser_case_screening.py:L163-L164] |
| S24 | Empty case list yields no governing case | C4 reproduces missing evidence; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L168-L168] |
| S25 | Empty case list yields no governing check | C4 reproduces missing evidence; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L169-L169] |
| S26 | Empty case list yields zero maximum | C4 reproduces missing evidence; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L170-L170] |
| S27 | Vacuous all() gives PASS in router and note | C4 reproduces false PASS; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L171-L172] |
| S28 | Missing or false-valued output directory selects another destination | C5 checks the explicit destination only; no detection check exists for unexpected fallback; #814 | [src/digitalmodel/subsea/riser/riser_case_screening.py:L187-L190] |
| S29 | Omitted label selects a shared output stem; repeated writes can replace prior output | no detection check exists; #814 | [src/digitalmodel/subsea/riser/riser_case_screening.py:L192-L198] |
| S30 | JSON writing permits non-finite numbers and persists an empty result; no result-validity guard precedes write | C4 covers empty results; no detection check exists for non-finite JSON; issue pending (1) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L198] |
| S31 | Note assumes metre-to-mm and Pa-to-MPa conversion | C5 checks file creation only; no detection check exists for input-unit mismatch; issue pending (2) | [src/digitalmodel/subsea/riser/riser_case_screening.py:L212-L241] |
| S32 | Missing burst check displays nan | C4 reproduces DNV-selected missing API column; #878 | [src/digitalmodel/subsea/riser/riser_case_screening.py:L250-L250] |
| S33 | Missing collapse check displays nan | C4 reproduces DNV-selected missing API column; #878 | [src/digitalmodel/subsea/riser/riser_case_screening.py:L251-L251] |
| S34 | Missing combined check displays nan | C4 reproduces DNV-selected missing API column; #878 | [src/digitalmodel/subsea/riser/riser_case_screening.py:L252-L252] |
| S35 | Callable default selects API when no code argument is supplied | C1 checks explicit/default equality; issue pending (2) for provenance | [src/digitalmodel/subsea/riser/riser_case_screening.py:L66-L87] |

The note's empty-list PASS also occurs in `build_calc_note`; the findings above include that repeated status path. [src/digitalmodel/subsea/riser/riser_case_screening.py:L219-L228]

| Other confirmed pitfall | Detection and existing work |
| --- | --- |
| A DNV-selected run still prints the API combined-method basis and says DNV is unimplemented. | C4; #878 covers the code-aware note and remaining F201 scope. [src/digitalmodel/subsea/riser/riser_case_screening.py:L219-L220] [src/digitalmodel/subsea/riser/riser_case_screening.py:L260-L276] |
| Category is metadata rather than a limit-state factor selector, and the router does not accept factors. | No detection check exists; issue pending (2). [src/digitalmodel/subsea/riser/riser_case_screening.py:L44-L50] [src/digitalmodel/subsea/riser/riser_case_screening.py:L151-L164] |
| Legacy utilisation parity compensates inconsistent force/moment scales; legacy absolute capacities are unsuitable as independent SI reference answers. | C3 arithmetic replay; independent reference issue pending (3). [tests/test_riser_case_screening.py:L137-L177] |
| The shared DNV reference says 2021, without separating edition and amendment in the screening output. | No detection check exists; edition-label follow-up #2166. [src/digitalmodel/codes.py:L74-L76] [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L185] |

Issue leads read: #711 concerns workflow registration rather than a confirmed numerical incident in this package; #878 concerns incomplete F201 screening/report scope; #814 concerns report provenance; #2166 concerns citation and edition labelling. No incident issue was named in the task. The two scope defects above are grounded in code rather than accepted from issue titles. [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L198] [src/digitalmodel/subsea/riser/riser_case_screening.py:L260-L276]

## 7. Lessons log

Pack maintenance rule: append dated entries; use public dataset identifiers, and retain prior rows.

| Date | Entry | Lesson | Evidence | Pack change / shared-log reference |
| --- | --- | --- | --- | --- |
| 2026-10-18 | Initial source audit | Legacy parity is a regression check and cannot independently qualify absolute SI capacities. | [tests/test_riser_case_screening.py:L137-L177] | Sections 3–6 separate arithmetic replay and regression from independent validation; shared-log entry not assigned. |
| 2026-10-18 | Initial boundary audit | Empty case evidence can receive PASS, and the note is not code-aware. | [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L172] [src/digitalmodel/subsea/riser/riser_case_screening.py:L249-L276] | C4 and section-6 findings; issue pending (1), #878. |

## 8. Ownership

Review assignment for this pack: engineering calculations quality manager; deputy: engineering calculations deputy quality manager; producing function: engineering calculations. Escalate ambiguous clauses, tolerance changes and disputed implementation classifications to that quality manager; cross-family or publication decisions belong to the maintainers. Clearance requires independent review by a different provider.

## 9. Dependencies and implementation inventory

Upstream Python dependencies are the geometry/material/load/factor dataclasses and the auto-registered strategy package; downstream outputs are the engine result dictionary and Markdown/JSON files. The legacy calculation file is a test-only reference, loaded directly rather than through its stale package initializer. [src/digitalmodel/subsea/riser/riser_case_screening.py:L21-L28] [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L201] [tests/test_riser_case_screening.py:L31-L50]

Implementation-map artifact pin: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, branch `codex/implmap-work`; its recorded data/code revision is `270c8759c374b67a7f032953e7fcdb14e0224461`. Family rows cited below are equal at the pack code pin and that map artifact pin. [docs/registry/implementation-map.yaml:L1-L3]

The package has one map family, `effective-tension-riser-stack-up`, with two in-package rows; it is the sole counted family. The full family inventory below includes all 264 map entries, including scripts and tests, without asserting that their map classifications are independently qualified. [docs/registry/implementation-map.md:L18-L18] [docs/registry/implementation-map.yaml:L4318-L4337]

In-package inventory: `RiserLoadCase.to_loads` (F1:207) and `run_riser_screening` (F1:208) are labelled independent by the map, but the source transfers supplied loads and delegates screening; neither computes a pressure-area or weight-based effective tension. Relationship correction is a map follow-up, issue pending (4). [docs/registry/implementation-map.yaml:L4318-L4337] [src/digitalmodel/subsea/riser/riser_case_screening.py:L44-L50] [src/digitalmodel/subsea/riser/riser_case_screening.py:L139-L164]

Exact counted signals, used as case-sensitive Python `re` patterns. [docs/registry/implementation-map.yaml:L2139-L2144]

```text
effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)
effective.tension|tensioner|stack.up
internal_pressure|external_pressure|fluid_density|buoyancy_module
```

No signal matches the relative package path `subsea/riser` under case-insensitive comparison, so none is excluded; at the code pin the package search produces 21 signal matches assigned to four enclosing definitions, counting module statements as one definition.

| Enclosing definition | Classification of counted-family matches |
| --- | --- |
| Module statements | Docstring and imports; not another physical implementation. [src/digitalmodel/subsea/riser/riser_case_screening.py:L1-L28] |
| `RiserLoadCase` | Dataclass fields hold supplied loads; helper/data definition. [src/digitalmodel/subsea/riser/riser_case_screening.py:L33-L42] |
| `RiserLoadCase.to_loads` | Listed F1:207; transfers values into `DesignLoads`. [src/digitalmodel/subsea/riser/riser_case_screening.py:L44-L50] |
| `run_riser_screening` | Listed F1:208; constructs input and delegates to `screen_riser_cases`. [src/digitalmodel/subsea/riser/riser_case_screening.py:L139-L164] |

Counted-family result: 0 further second implementations; the data holders and delegation contain no independent effective-tension formula. A8 inventory/grep self-check passes. [src/digitalmodel/subsea/riser/riser_case_screening.py:L33-L50] [src/digitalmodel/subsea/riser/riser_case_screening.py:L125-L201]

The same signals were searched through every tracked repository Python file, including scripts: 401 files matched, including 23 under scripts. This is discovery coverage rather than a claim that every hit is an implementation; outside-package hits do not change A8. The map's inventory includes scripts explicitly. [docs/registry/implementation-map.md:L13-L18]

Map follow-up: add supplied-load strength screening, maximum-utilisation selection and stable ranking as method families, link the upstream API and DNV strength definitions, and correct the helper relationships above; issue pending (4). [src/digitalmodel/subsea/riser/riser_case_screening.py:L89-L122] [src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:L40-L51] [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L130-L141]

Full F1 inventory uses map order and exact paths/symbols; `(module-level)` denotes a null map symbol, not a newly inferred function.

| Entry | Map path / symbol | Map evidence |
| --- | --- | --- |
| F1:001 | `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` — `main` | [docs/registry/implementation-map.yaml:L2146-L2147] |
| F1:002 | `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` — `main` | [docs/registry/implementation-map.yaml:L2156-L2157] |
| F1:003 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` — `main` | [docs/registry/implementation-map.yaml:L2166-L2167] |
| F1:004 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` — `main` | [docs/registry/implementation-map.yaml:L2176-L2177] |
| F1:005 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` — `check_r01_catenary` | [docs/registry/implementation-map.yaml:L2186-L2187] |
| F1:006 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` — `check_r02_lazy_wave` | [docs/registry/implementation-map.yaml:L2196-L2197] |
| F1:007 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` — `check_r05_lay_table` | [docs/registry/implementation-map.yaml:L2206-L2207] |
| F1:008 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` — `check_r06_multi_static` | [docs/registry/implementation-map.yaml:L2216-L2217] |
| F1:009 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` — `extract_vessel_params` | [docs/registry/implementation-map.yaml:L2226-L2227] |
| F1:010 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` — `vessel_band_specs` | [docs/registry/implementation-map.yaml:L2236-L2237] |
| F1:011 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` — `find_required_tension` | [docs/registry/implementation-map.yaml:L2248-L2249] |
| F1:012 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` — `_build_infeasible_result` | [docs/registry/implementation-map.yaml:L2260-L2261] |
| F1:013 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` — `run_single_case` | [docs/registry/implementation-map.yaml:L2270-L2271] |
| F1:014 | `examples/demos/gtm/results_store_demo04.py` — `(module-level)` | [docs/registry/implementation-map.yaml:L2283-L2284] |
| F1:015 | `examples/demos/gtm/tests/test_demo_04_results_store.py` — `_good_case` | [docs/registry/implementation-map.yaml:L2292-L2293] |
| F1:016 | `examples/demos/gtm/tests/test_demo_04_results_store.py` — `_infeasible_case` | [docs/registry/implementation-map.yaml:L2302-L2303] |
| F1:017 | `examples/demos/gtm/tests/test_demo_04_results_store.py` — `test_999_sentinel_and_none_null_round_trip` | [docs/registry/implementation-map.yaml:L2312-L2313] |
| F1:018 | `examples/domains/workflows/04_pipeline_installation.py` — `analyze_installation_catenary` | [docs/registry/implementation-map.yaml:L2325-L2326] |
| F1:019 | `examples/workflows/riser-stackup-registry-batch/run.py` — `_schedule_row` | [docs/registry/implementation-map.yaml:L2335-L2336] |
| F1:020 | `examples/workflows/riser-stackup-registry-batch/run.py` — `_assemble_row` | [docs/registry/implementation-map.yaml:L2345-L2346] |
| F1:021 | `scripts/capabilities/build_wall_thickness_3d.py` — `(module-level)` | [docs/registry/implementation-map.yaml:L2357-L2358] |
| F1:022 | `scripts/capabilities/build_wall_thickness_3d.py` — `_analyze_point` | [docs/registry/implementation-map.yaml:L2366-L2367] |
| F1:023 | `scripts/capabilities/build_wall_thickness_3d.py` — `build_study` | [docs/registry/implementation-map.yaml:L2376-L2377] |
| F1:024 | `scripts/capabilities/build_wall_thickness_3d_page.py` — `_axes` | [docs/registry/implementation-map.yaml:L2388-L2389] |
| F1:025 | `scripts/capabilities/build_wall_thickness_3d_page.py` — `_pack_rows` | [docs/registry/implementation-map.yaml:L2398-L2399] |
| F1:026 | `scripts/capabilities/build_wall_thickness_explorer.py` — `_run` | [docs/registry/implementation-map.yaml:L2408-L2409] |
| F1:027 | `scripts/orcaflex/check_generic_scr_slwr.py` — `metrics` | [docs/registry/implementation-map.yaml:L2418-L2419] |
| F1:028 | `scripts/orcaflex/check_generic_scr_slwr.py` — `calculate` | [docs/registry/implementation-map.yaml:L2428-L2429] |
| F1:029 | `scripts/orcaflex/prepare_slay_campaign.py` — `catenary` | [docs/registry/implementation-map.yaml:L2438-L2439] |
| F1:030 | `scripts/orcaflex/prepare_slay_campaign.py` — `case_spec` | [docs/registry/implementation-map.yaml:L2448-L2449] |
| F1:031 | `scripts/orcaflex/prepare_slay_campaign.py` — `prepare_case` | [docs/registry/implementation-map.yaml:L2458-L2459] |
| F1:032 | `scripts/orcaflex/prepare_slay_campaign.py` — `prepare` | [docs/registry/implementation-map.yaml:L2471-L2472] |
| F1:033 | `scripts/parametric/build_orcaflex_library.py` — `main` | [docs/registry/implementation-map.yaml:L2485-L2486] |
| F1:034 | `scripts/python/digitalmodel/analysis/APIST2RD_Methods.py` — `APISTD2RDCalculation` | [docs/registry/implementation-map.yaml:L2495-L2496] |
| F1:035 | `scripts/python/digitalmodel/analysis/RigidPipe.py` — `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | [docs/registry/implementation-map.yaml:L2505-L2506] |
| F1:036 | `scripts/python/digitalmodel/analysis/RiserStackup.py` — `StackUpDiagram` | [docs/registry/implementation-map.yaml:L2515-L2516] |
| F1:037 | `scripts/python/digitalmodel/analysis/StackUp_3000Ft.py` — `main` | [docs/registry/implementation-map.yaml:L2525-L2526] |
| F1:038 | `scripts/python/digitalmodel/analysis/StackUp_9500Ft.py` — `main` | [docs/registry/implementation-map.yaml:L2535-L2536] |
| F1:039 | `scripts/reporting/build_riser_pipe_comparison.py` — `evaluate` | [docs/registry/implementation-map.yaml:L2545-L2546] |
| F1:040 | `scripts/reporting/build_riser_pipe_comparison.py` — `_validate` | [docs/registry/implementation-map.yaml:L2557-L2558] |
| F1:041 | `scripts/reporting/build_riser_pipe_comparison.py` — `numeric_values` | [docs/registry/implementation-map.yaml:L2567-L2568] |
| F1:042 | `scripts/reporting/build_riser_pipe_comparison.py` — `_input_table` | [docs/registry/implementation-map.yaml:L2577-L2578] |
| F1:043 | `scripts/riser_database/build_tables.py` — `(module-level)` | [docs/registry/implementation-map.yaml:L2587-L2588] |
| F1:044 | `src/digitalmodel/asset_integrity/custom/PipeCapacity.py` — `VonMises_Pipe.init_calculate_required_properties` | [docs/registry/implementation-map.yaml:L2596-L2597] |
| F1:045 | `src/digitalmodel/code_checks/dnv_os_f101.py` — `local_buckling_lcc` | [docs/registry/implementation-map.yaml:L2606-L2607] |
| F1:046 | `src/digitalmodel/code_checks/workflow.py` — `_run_api_rp_2rd` | [docs/registry/implementation-map.yaml:L2616-L2617] |
| F1:047 | `src/digitalmodel/code_checks/workflow.py` — `_run_dnv_os_f201` | [docs/registry/implementation-map.yaml:L2626-L2627] |
| F1:048 | `src/digitalmodel/drilling_riser/assembly.py` — `minimum_top_tension_16q` | [docs/registry/implementation-map.yaml:L2636-L2637] |
| F1:049 | `src/digitalmodel/drilling_riser/assembly.py` — `_unit_count` | [docs/registry/implementation-map.yaml:L2646-L2647] |
| F1:050 | `src/digitalmodel/drilling_riser/assembly.py` — `check_rig_capability` | [docs/registry/implementation-map.yaml:L2656-L2657] |
| F1:051 | `src/digitalmodel/drilling_riser/campaign.py` — `case_spec` | [docs/registry/implementation-map.yaml:L2668-L2669] |
| F1:052 | `src/digitalmodel/drilling_riser/campaign.py` — `_ring_tensioners` | [docs/registry/implementation-map.yaml:L2680-L2681] |
| F1:053 | `src/digitalmodel/drilling_riser/campaign.py` — `set_line_tension` | [docs/registry/implementation-map.yaml:L2690-L2691] |
| F1:054 | `src/digitalmodel/drilling_riser/campaign.py` — `calibrate_line_tension` | [docs/registry/implementation-map.yaml:L2702-L2703] |
| F1:055 | `src/digitalmodel/drilling_riser/campaign.py` — `_statics_path` | [docs/registry/implementation-map.yaml:L2716-L2717] |
| F1:056 | `src/digitalmodel/drilling_riser/campaign.py` — `robust_statics` | [docs/registry/implementation-map.yaml:L2727-L2728] |
| F1:057 | `src/digitalmodel/drilling_riser/campaign.py` — `_tensioner_vertical_n` | [docs/registry/implementation-map.yaml:L2741-L2742] |
| F1:058 | `src/digitalmodel/drilling_riser/campaign.py` — `tensioner_vertical_target_n` | [docs/registry/implementation-map.yaml:L2751-L2752] |
| F1:059 | `src/digitalmodel/drilling_riser/campaign.py` — `physical_state_checks` | [docs/registry/implementation-map.yaml:L2761-L2762] |
| F1:060 | `src/digitalmodel/drilling_riser/campaign.py` — `RiserCampaignAdapter.prepare` | [docs/registry/implementation-map.yaml:L2775-L2776] |
| F1:061 | `src/digitalmodel/drilling_riser/campaign.py` — `RiserCampaignAdapter.extract` | [docs/registry/implementation-map.yaml:L2788-L2789] |
| F1:062 | `src/digitalmodel/drilling_riser/envelope.py` — `compute_operating_envelope` | [docs/registry/implementation-map.yaml:L2798-L2799] |
| F1:063 | `src/digitalmodel/drilling_riser/global_model/build.py` — `equivalent_string_top_tension_n` | [docs/registry/implementation-map.yaml:L2808-L2809] |
| F1:064 | `src/digitalmodel/drilling_riser/global_model/build.py` — `_tensioner_geometry` | [docs/registry/implementation-map.yaml:L2818-L2819] |
| F1:065 | `src/digitalmodel/drilling_riser/global_model/build.py` — `winch_tension_n` | [docs/registry/implementation-map.yaml:L2828-L2829] |
| F1:066 | `src/digitalmodel/drilling_riser/global_model/build.py` — `build_generic_spec` | [docs/registry/implementation-map.yaml:L2840-L2841] |
| F1:067 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` — `effective_tension_chain` | [docs/registry/implementation-map.yaml:L2853-L2854] |
| F1:068 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` — `tension_references` | [docs/registry/implementation-map.yaml:L2863-L2864] |
| F1:069 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` — `tensioner_lateral_stiffness_n_m` | [docs/registry/implementation-map.yaml:L2875-L2876] |
| F1:070 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` — `reference_periods` | [docs/registry/implementation-map.yaml:L2885-L2886] |
| F1:071 | `src/digitalmodel/drilling_riser/global_model/hang_off.py` — `hung_weight_n` | [docs/registry/implementation-map.yaml:L2897-L2898] |
| F1:072 | `src/digitalmodel/drilling_riser/global_model/open_water.py` — `OpenWaterRiserSpec._closes` | [docs/registry/implementation-map.yaml:L2908-L2909] |
| F1:073 | `src/digitalmodel/drilling_riser/global_model/open_water.py` — `build_open_water_generic_spec` | [docs/registry/implementation-map.yaml:L2918-L2919] |
| F1:074 | `src/digitalmodel/drilling_riser/global_model/open_water.py` — `tension_references` | [docs/registry/implementation-map.yaml:L2928-L2929] |
| F1:075 | `src/digitalmodel/drilling_riser/global_model/open_water.py` — `reference_periods` | [docs/registry/implementation-map.yaml:L2939-L2940] |
| F1:076 | `src/digitalmodel/drilling_riser/global_model/open_water.py` — `release_reference` | [docs/registry/implementation-map.yaml:L2949-L2950] |
| F1:077 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` — `ring_vertical_residual_n` | [docs/registry/implementation-map.yaml:L2961-L2962] |
| F1:078 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` — `open_water_physical_checks` | [docs/registry/implementation-map.yaml:L2971-L2972] |
| F1:079 | `src/digitalmodel/drilling_riser/global_model/spec.py` — `Tensioners._failed` | [docs/registry/implementation-map.yaml:L2981-L2982] |
| F1:080 | `src/digitalmodel/drilling_riser/global_model/spec.py` — `RiserGlobalModelSpec._geometry_closes` | [docs/registry/implementation-map.yaml:L2991-L2992] |
| F1:081 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` — `_missing_open_water` | [docs/registry/implementation-map.yaml:L3001-L3002] |
| F1:082 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` — `missing_channels` | [docs/registry/implementation-map.yaml:L3011-L3012] |
| F1:083 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` — `extract` | [docs/registry/implementation-map.yaml:L3023-L3024] |
| F1:084 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` — `extract_open_water` | [docs/registry/implementation-map.yaml:L3033-L3034] |
| F1:085 | `src/digitalmodel/drilling_riser/postprocess/checks.py` — `t_min` | [docs/registry/implementation-map.yaml:L3043-L3044] |
| F1:086 | `src/digitalmodel/drilling_riser/postprocess/checks.py` — `tension_setting_max` | [docs/registry/implementation-map.yaml:L3053-L3054] |
| F1:087 | `src/digitalmodel/drilling_riser/postprocess/checks.py` — `tensioner_stroke` | [docs/registry/implementation-map.yaml:L3063-L3064] |
| F1:088 | `src/digitalmodel/drilling_riser/schedule_assembly.py` — `ScheduleAssembly.minimum_top_tension_16q_kn` | [docs/registry/implementation-map.yaml:L3073-L3074] |
| F1:089 | `src/digitalmodel/drilling_riser/schedule_assembly.py` — `_load_oc_assembly` | [docs/registry/implementation-map.yaml:L3083-L3084] |
| F1:090 | `src/digitalmodel/drilling_riser/stackup.py` — `effective_tension` | [docs/registry/implementation-map.yaml:L3093-L3094] |
| F1:091 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` — `parse_snapshots` | [docs/registry/implementation-map.yaml:L3103-L3104] |
| F1:092 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` — `snapshot_to_tension_n` | [docs/registry/implementation-map.yaml:L3113-L3114] |
| F1:093 | `src/digitalmodel/drilling_riser/tsj_workflow.py` — `router` | [docs/registry/implementation-map.yaml:L3123-L3124] |
| F1:094 | `src/digitalmodel/drilling_riser/tsj_workflow.py` — `_section_result` | [docs/registry/implementation-map.yaml:L3136-L3137] |
| F1:095 | `src/digitalmodel/drilling_riser/tsj_workflow.py` — `_effective_tension_n` | [docs/registry/implementation-map.yaml:L3146-L3147] |
| F1:096 | `src/digitalmodel/drilling_riser/workflow.py` — `router` | [docs/registry/implementation-map.yaml:L3156-L3157] |
| F1:097 | `src/digitalmodel/drilling_riser/workflow.py` — `_write_profile_csv` | [docs/registry/implementation-map.yaml:L3169-L3170] |
| F1:098 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.add_riser_length_to_shear7_result` | [docs/registry/implementation-map.yaml:L3179-L3180] |
| F1:099 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | [docs/registry/implementation-map.yaml:L3189-L3190] |
| F1:100 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | [docs/registry/implementation-map.yaml:L3199-L3200] |
| F1:101 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L3209-L3210] |
| F1:102 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` | [docs/registry/implementation-map.yaml:L3219-L3220] |
| F1:103 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3232-L3233] |
| F1:104 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` — `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3242-L3243] |
| F1:105 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L3252-L3253] |
| F1:106 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.customize_fj_types` | [docs/registry/implementation-map.yaml:L3265-L3266] |
| F1:107 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.links` | [docs/registry/implementation-map.yaml:L3275-L3276] |
| F1:108 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.add_support_spring` | [docs/registry/implementation-map.yaml:L3288-L3289] |
| F1:109 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.add_tensioners` | [docs/registry/implementation-map.yaml:L3301-L3302] |
| F1:110 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.tensioner_vessel_locations` | [docs/registry/implementation-map.yaml:L3312-L3313] |
| F1:111 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.spring_properties` | [docs/registry/implementation-map.yaml:L3322-L3323] |
| F1:112 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.get_connection_definitions` | [docs/registry/implementation-map.yaml:L3332-L3333] |
| F1:113 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.get_fj_line_definitions` | [docs/registry/implementation-map.yaml:L3342-L3343] |
| F1:114 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` — `OrcaflexModelComponents.get_line_items_vertical` | [docs/registry/implementation-map.yaml:L3352-L3353] |
| F1:115 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` | [docs/registry/implementation-map.yaml:L3362-L3363] |
| F1:116 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` | [docs/registry/implementation-map.yaml:L3378-L3379] |
| F1:117 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` | [docs/registry/implementation-map.yaml:L3388-L3389] |
| F1:118 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` | [docs/registry/implementation-map.yaml:L3398-L3399] |
| F1:119 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` | [docs/registry/implementation-map.yaml:L3408-L3409] |
| F1:120 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` | [docs/registry/implementation-map.yaml:L3418-L3419] |
| F1:121 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.assign_key_properties_to_df` | [docs/registry/implementation-map.yaml:L3433-L3434] |
| F1:122 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` | [docs/registry/implementation-map.yaml:L3443-L3444] |
| F1:123 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.evaluate_effective_tension` | [docs/registry/implementation-map.yaml:L3453-L3454] |
| F1:124 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` — `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` | [docs/registry/implementation-map.yaml:L3463-L3464] |
| F1:125 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` — `Shear7ModelComponents.title` | [docs/registry/implementation-map.yaml:L3473-L3474] |
| F1:126 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` — `Shear7ModelComponents.riser_joints_to_shear7_zones` | [docs/registry/implementation-map.yaml:L3483-L3484] |
| F1:127 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` — `Shear7ModelComponents.add_block_3` | [docs/registry/implementation-map.yaml:L3493-L3494] |
| F1:128 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` | [docs/registry/implementation-map.yaml:L3503-L3504] |
| F1:129 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | [docs/registry/implementation-map.yaml:L3513-L3514] |
| F1:130 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | [docs/registry/implementation-map.yaml:L3523-L3524] |
| F1:131 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | [docs/registry/implementation-map.yaml:L3533-L3534] |
| F1:132 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` | [docs/registry/implementation-map.yaml:L3543-L3544] |
| F1:133 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3556-L3557] |
| F1:134 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` — `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | [docs/registry/implementation-map.yaml:L3566-L3567] |
| F1:135 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` — `_write_tensions_csv` | [docs/registry/implementation-map.yaml:L3576-L3577] |
| F1:136 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._line_state` | [docs/registry/implementation-map.yaml:L3586-L3587] |
| F1:137 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._solve_line` | [docs/registry/implementation-map.yaml:L3598-L3599] |
| F1:138 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` — `FPSOMooringFullWorkflow._result` | [docs/registry/implementation-map.yaml:L3608-L3609] |
| F1:139 | `src/digitalmodel/orcaflex/mooring_design.py` — `solve_catenary` | [docs/registry/implementation-map.yaml:L3618-L3619] |
| F1:140 | `src/digitalmodel/orcaflex/pipelay_analysis.py` — `JLayConfig.calculate_sagbend` | [docs/registry/implementation-map.yaml:L3628-L3629] |
| F1:141 | `src/digitalmodel/orcaflex/riser_config.py` — `SCRDesignInput.calculate_geometry` | [docs/registry/implementation-map.yaml:L3638-L3639] |
| F1:142 | `src/digitalmodel/orcaflex/riser_config.py` — `TTRDesignInput.calculate_stroke` | [docs/registry/implementation-map.yaml:L3648-L3649] |
| F1:143 | `src/digitalmodel/orcaflex/riser_config.py` — `TTRDesignInput.calculate_top_tension` | [docs/registry/implementation-map.yaml:L3658-L3659] |
| F1:144 | `src/digitalmodel/orcaflex/riser_input_schema.py` — `_TopTensionedInput.to_legacy` | [docs/registry/implementation-map.yaml:L3668-L3669] |
| F1:145 | `src/digitalmodel/orcaflex/viv_screening.py` — `BeamProperties.natural_frequency` | [docs/registry/implementation-map.yaml:L3678-L3679] |
| F1:146 | `src/digitalmodel/parametric/generate.py` — `_code_check_utilisation` | [docs/registry/implementation-map.yaml:L3688-L3689] |
| F1:147 | `src/digitalmodel/pipelay/workflow.py` — `run_jlay` | [docs/registry/implementation-map.yaml:L3698-L3699] |
| F1:148 | `src/digitalmodel/riser_configuration_screen/static.py` — `_lazy_row` | [docs/registry/implementation-map.yaml:L3708-L3709] |
| F1:149 | `src/digitalmodel/riser_configuration_screen/static.py` — `scr_offsets` | [docs/registry/implementation-map.yaml:L3718-L3719] |
| F1:150 | `src/digitalmodel/riser_configuration_screen/workflow.py` — `_validate` | [docs/registry/implementation-map.yaml:L3728-L3729] |
| F1:151 | `src/digitalmodel/riser_configuration_screen/workflow.py` — `_utilisation` | [docs/registry/implementation-map.yaml:L3738-L3739] |
| F1:152 | `src/digitalmodel/riser_configuration_screen/workflow.py` — `_checks` | [docs/registry/implementation-map.yaml:L3748-L3749] |
| F1:153 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py` — `GroupsBuilder._build_from_spec` | [docs/registry/implementation-map.yaml:L3758-L3759] |
| F1:154 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` — `LineTypeBuilder.build` | [docs/registry/implementation-map.yaml:L3768-L3769] |
| F1:155 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` — `WinchBuilder.should_generate` | [docs/registry/implementation-map.yaml:L3778-L3779] |
| F1:156 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` — `WinchBuilder.build` | [docs/registry/implementation-map.yaml:L3788-L3789] |
| F1:157 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` — `_apply_overrides` | [docs/registry/implementation-map.yaml:L3798-L3799] |
| F1:158 | `src/digitalmodel/solvers/orcaflex/mooring.py` — `Mooring.mooring_pre_tension_analysis` | [docs/registry/implementation-map.yaml:L3808-L3809] |
| F1:159 | `src/digitalmodel/solvers/orcaflex/mooring.py` — `Mooring.evaluate_line_length_next_iteration` | [docs/registry/implementation-map.yaml:L3820-L3821] |
| F1:160 | `src/digitalmodel/solvers/orcaflex/opp_range_graph.py` — `OPPRangeGraph.postProcessRange` | [docs/registry/implementation-map.yaml:L3830-L3831] |
| F1:161 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` — `OrcaFlexAnalysis.postProcessRange` | [docs/registry/implementation-map.yaml:L3840-L3841] |
| F1:162 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.orcaflex_model` | [docs/registry/implementation-map.yaml:L3850-L3851] |
| F1:163 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.customize_fj_types` | [docs/registry/implementation-map.yaml:L3863-L3864] |
| F1:164 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.links` | [docs/registry/implementation-map.yaml:L3873-L3874] |
| F1:165 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.add_support_spring` | [docs/registry/implementation-map.yaml:L3886-L3887] |
| F1:166 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.add_tensioners` | [docs/registry/implementation-map.yaml:L3899-L3900] |
| F1:167 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.tensioner_vessel_locations` | [docs/registry/implementation-map.yaml:L3910-L3911] |
| F1:168 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.spring_properties` | [docs/registry/implementation-map.yaml:L3920-L3921] |
| F1:169 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.get_connection_definitions` | [docs/registry/implementation-map.yaml:L3930-L3931] |
| F1:170 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.get_fj_line_definitions` | [docs/registry/implementation-map.yaml:L3940-L3941] |
| F1:171 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` — `OrcaflexModelComponents.get_line_items_vertical` | [docs/registry/implementation-map.yaml:L3950-L3951] |
| F1:172 | `src/digitalmodel/structural/analysis/cli.py` — `check` | [docs/registry/implementation-map.yaml:L3960-L3961] |
| F1:173 | `src/digitalmodel/structural/analysis/wall_thickness.py` — `WallThicknessAnalyzer.check_combined_loading` | [docs/registry/implementation-map.yaml:L3970-L3971] |
| F1:174 | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` — `ApiStd2rdStrategy._check_combined` | [docs/registry/implementation-map.yaml:L3980-L3981] |
| F1:175 | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` — `DnvStF101Strategy._check_combined_loading` | [docs/registry/implementation-map.yaml:L3990-L3991] |
| F1:176 | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` — `run_wall_thickness_compare` | [docs/registry/implementation-map.yaml:L4000-L4001] |
| F1:177 | `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` — `MTLookupGenerator.generate` | [docs/registry/implementation-map.yaml:L4010-L4011] |
| F1:178 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` — `ParametricSweep.run` | [docs/registry/implementation-map.yaml:L4020-L4021] |
| F1:179 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` — `PipelinePhase.to_design_loads` | [docs/registry/implementation-map.yaml:L4030-L4031] |
| F1:180 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` — `create_standard_phases` | [docs/registry/implementation-map.yaml:L4040-L4041] |
| F1:181 | `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` — `LoadScalingProcessor.process_all_conditions` | [docs/registry/implementation-map.yaml:L4050-L4051] |
| F1:182 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` — `IntegratedFatigueProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L4060-L4061] |
| F1:183 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` — `IntegratedFatigueProcessorWithNaming._process_condition_base` | [docs/registry/implementation-map.yaml:L4070-L4071] |
| F1:184 | `src/digitalmodel/structural/fatigue_apps/load_scaling.py` — `LoadScalingProcessor.load_reference_data` | [docs/registry/implementation-map.yaml:L4080-L4081] |
| F1:185 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` — `ReferenceSeaStateProcessor.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L4090-L4091] |
| F1:186 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` — `LoadScaler.process_fatigue_condition` | [docs/registry/implementation-map.yaml:L4100-L4101] |
| F1:187 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` — `LoadScaler.process_sea_state` | [docs/registry/implementation-map.yaml:L4110-L4111] |
| F1:188 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` — `main` | [docs/registry/implementation-map.yaml:L4120-L4121] |
| F1:189 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py` — `ProductionDataHandlerWithProgress.process_single_analysis` | [docs/registry/implementation-map.yaml:L4130-L4131] |
| F1:190 | `src/digitalmodel/structural/parametric_coordinator.py` — `ParametricCoordinator._run_wall_thickness` | [docs/registry/implementation-map.yaml:L4140-L4141] |
| F1:191 | `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` — `VonMises_Pipe.init_calculate_required_properties` | [docs/registry/implementation-map.yaml:L4150-L4151] |
| F1:192 | `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` — `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | [docs/registry/implementation-map.yaml:L4160-L4161] |
| F1:193 | `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` — `VonMises_Pipe.init_calculate_required_properties` | [docs/registry/implementation-map.yaml:L4170-L4171] |
| F1:194 | `src/digitalmodel/subsea/catenary_riser/cli.py` — `simple_catenary` | [docs/registry/implementation-map.yaml:L4180-L4181] |
| F1:195 | `src/digitalmodel/subsea/catenary_riser/cli.py` — `lazy_wave_analysis` | [docs/registry/implementation-map.yaml:L4190-L4191] |
| F1:196 | `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` — `LazyWaveAnalyzer.analyze` | [docs/registry/implementation-map.yaml:L4200-L4201] |
| F1:197 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer.analyze_riser` | [docs/registry/implementation-map.yaml:L4210-L4211] |
| F1:198 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer._solve_catenary_from_H` | [docs/registry/implementation-map.yaml:L4223-L4224] |
| F1:199 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer._solve_H_from_top_tension` | [docs/registry/implementation-map.yaml:L4233-L4234] |
| F1:200 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` — `SimpleCatenaryAnalyzer.check_design_limits` | [docs/registry/implementation-map.yaml:L4243-L4244] |
| F1:201 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` — `developed_min_radius` | [docs/registry/implementation-map.yaml:L4253-L4254] |
| F1:202 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` — `required_effective_ei` | [docs/registry/implementation-map.yaml:L4263-L4264] |
| F1:203 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` — `screen_bend_stiffener` | [docs/registry/implementation-map.yaml:L4273-L4274] |
| F1:204 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` — `armour_tension_stress` | [docs/registry/implementation-map.yaml:L4286-L4287] |
| F1:205 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` — `verify_flexible_pipe` | [docs/registry/implementation-map.yaml:L4296-L4297] |
| F1:206 | `src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py` — `effective_driving_force` | [docs/registry/implementation-map.yaml:L4308-L4309] |
| F1:207 | `src/digitalmodel/subsea/riser/riser_case_screening.py` — `RiserLoadCase.to_loads` | [docs/registry/implementation-map.yaml:L4318-L4319] |
| F1:208 | `src/digitalmodel/subsea/riser/riser_case_screening.py` — `run_riser_screening` | [docs/registry/implementation-map.yaml:L4328-L4329] |
| F1:209 | `src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py` — `StackUpDiagram` | [docs/registry/implementation-map.yaml:L4338-L4339] |
| F1:210 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.prepare_riser_model` | [docs/registry/implementation-map.yaml:L4348-L4349] |
| F1:211 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.read_riser_input_data` | [docs/registry/implementation-map.yaml:L4363-L4364] |
| F1:212 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.assign_key_properties_to_self` | [docs/registry/implementation-map.yaml:L4373-L4374] |
| F1:213 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.get_fea_properties` | [docs/registry/implementation-map.yaml:L4383-L4384] |
| F1:214 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.prepare_tensioner_data` | [docs/registry/implementation-map.yaml:L4393-L4394] |
| F1:215 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.evaluate_stack_up_properties` | [docs/registry/implementation-map.yaml:L4403-L4404] |
| F1:216 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.save_data` | [docs/registry/implementation-map.yaml:L4413-L4414] |
| F1:217 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` — `VerticalRiser.alter_properties_for_Shear7_software` | [docs/registry/implementation-map.yaml:L4423-L4424] |
| F1:218 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` — `FrequencyCalculator.calculate_natural_frequency` | [docs/registry/implementation-map.yaml:L4433-L4434] |
| F1:219 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` — `FrequencyCalculator._frequency_tension_controlled` | [docs/registry/implementation-map.yaml:L4445-L4446] |
| F1:220 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` — `FrequencyCalculator.calculate_effective_tension` | [docs/registry/implementation-map.yaml:L4455-L4456] |
| F1:221 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` — `FrequencyCalculator.check_stability` | [docs/registry/implementation-map.yaml:L4465-L4466] |
| F1:222 | `src/digitalmodel/viv_screening/inputs.py` — `_beam` | [docs/registry/implementation-map.yaml:L4477-L4478] |
| F1:223 | `src/digitalmodel/workflows/workflow_automation/cli.py` — `riser_analysis_cmd` | [docs/registry/implementation-map.yaml:L4487-L4488] |
| F1:224 | `src/digitalmodel/workflows/workflow_automation/templates.py` — `create_riser_analysis_template` | [docs/registry/implementation-map.yaml:L4497-L4498] |
| F1:225 | `src/digitalmodel/workflows/workflow_automation/workflows.py` — `CompleteRiserAnalysisWorkflow.create` | [docs/registry/implementation-map.yaml:L4507-L4508] |
| F1:226 | `tests/asset_integrity/test_calculations.py` — `_make_pipe_cfg` | [docs/registry/implementation-map.yaml:L4517-L4518] |
| F1:227 | `tests/capabilities/test_wall_thickness_3d_page.py` — `_study` | [docs/registry/implementation-map.yaml:L4527-L4528] |
| F1:228 | `tests/contracts/test_force_units_boundary_contract.py` — `(module-level)` | [docs/registry/implementation-map.yaml:L4537-L4538] |
| F1:229 | `tests/contracts/test_force_units_boundary_contract.py` — `test_envelope_von_mises_n_to_kn_seam` | [docs/registry/implementation-map.yaml:L4546-L4547] |
| F1:230 | `tests/drilling_riser/global_model/conftest.py` — `synthetic_spec` | [docs/registry/implementation-map.yaml:L4558-L4559] |
| F1:231 | `tests/drilling_riser/global_model/test_batch_params.py` — `test_physical_checks_use_the_remaining_tensioners` | [docs/registry/implementation-map.yaml:L4568-L4569] |
| F1:232 | `tests/drilling_riser/global_model/test_campaign.py` — `_fake` | [docs/registry/implementation-map.yaml:L4579-L4580] |
| F1:233 | `tests/drilling_riser/global_model/test_open_water.py` — `open_water_dict` | [docs/registry/implementation-map.yaml:L4590-L4591] |
| F1:234 | `tests/drilling_riser/global_model/test_open_water_campaign.py` — `_doc` | [docs/registry/implementation-map.yaml:L4600-L4601] |
| F1:235 | `tests/drilling_riser/global_model/test_orcaflex_solver.py` — `test_statics_tension_and_modes_agree_with_hand_checks` | [docs/registry/implementation-map.yaml:L4610-L4611] |
| F1:236 | `tests/drilling_riser/global_model/test_statics_path.py` — `test_branch_tolerance_and_ring_buoyancy` | [docs/registry/implementation-map.yaml:L4624-L4625] |
| F1:237 | `tests/drilling_riser/global_model/test_tensioner_representation.py` — `_vertical` | [docs/registry/implementation-map.yaml:L4635-L4636] |
| F1:238 | `tests/drilling_riser/postprocess/test_aggregate.py` — `_mc` | [docs/registry/implementation-map.yaml:L4645-L4646] |
| F1:239 | `tests/drilling_riser/postprocess/test_aggregate.py` — `_case` | [docs/registry/implementation-map.yaml:L4655-L4656] |
| F1:240 | `tests/drilling_riser/test_drive_off_worked_example.py` — `_recorded_arguments` | [docs/registry/implementation-map.yaml:L4667-L4668] |
| F1:241 | `tests/drilling_riser/test_schedule_assembly.py` — `_deltas` | [docs/registry/implementation-map.yaml:L4677-L4678] |
| F1:242 | `tests/infrastructure/unit/test_pipe_capacity_codes.py` — `_base_cfg` | [docs/registry/implementation-map.yaml:L4687-L4688] |
| F1:243 | `tests/orcaflex/test_generic_scr_slwr.py` — `test_analytical_reference_reproduces_from_basis` | [docs/registry/implementation-map.yaml:L4697-L4698] |
| F1:244 | `tests/parametric/test_atlas.py` — `_code_check_atlas` | [docs/registry/implementation-map.yaml:L4707-L4708] |
| F1:245 | `tests/reporting/test_riser_pipe_comparison.py` — `reference_input` | [docs/registry/implementation-map.yaml:L4717-L4718] |
| F1:246 | `tests/riser_configuration_screen/test_workflow.py` — `settings` | [docs/registry/implementation-map.yaml:L4727-L4728] |
| F1:247 | `tests/solvers/orcaflex/analysis/scripts/verify_configs.py` — `verify_configuration` | [docs/registry/implementation-map.yaml:L4737-L4738] |
| F1:248 | `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py` — `_make_slay_spec` | [docs/registry/implementation-map.yaml:L4747-L4748] |
| F1:249 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` — `_make_pipeline_spec` | [docs/registry/implementation-map.yaml:L4757-L4758] |
| F1:250 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` — `_make_pipeline_spec` | [docs/registry/implementation-map.yaml:L4767-L4768] |
| F1:251 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` — `_write_slay_campaign` | [docs/registry/implementation-map.yaml:L4777-L4778] |
| F1:252 | `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py` — `_make_base_spec_data` | [docs/registry/implementation-map.yaml:L4787-L4788] |
| F1:253 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` — `extract_static_range_results` | [docs/registry/implementation-map.yaml:L4797-L4798] |
| F1:254 | `tests/solvers/orcaflex/modular_generator/test_post_validator.py` — `_make_valid_model` | [docs/registry/implementation-map.yaml:L4807-L4808] |
| F1:255 | `tests/solvers/orcaflex/test_slay_layout.py` — `test_support_types_are_defined_before_vessel_references` | [docs/registry/implementation-map.yaml:L4817-L4818] |
| F1:256 | `tests/specialized/umbilical_analysis/conftest.py` — `installation_parameters` | [docs/registry/implementation-map.yaml:L4827-L4828] |
| F1:257 | `tests/subsea/catenary_riser/test_catenary.py` — `run_catenary_riser` | [docs/registry/implementation-map.yaml:L4837-L4838] |
| F1:258 | `tests/test_automation/fixtures/engineering_fixtures.py` — `get_catenary_analysis_mock` | [docs/registry/implementation-map.yaml:L4847-L4848] |
| F1:259 | `tests/test_automation/scripts/batch_fix_tests.py` — `(module-level)` | [docs/registry/implementation-map.yaml:L4857-L4858] |
| F1:260 | `tests/test_rainflow_with_effective_tension.py` — `process_effective_tension_file` | [docs/registry/implementation-map.yaml:L4866-L4867] |
| F1:261 | `tests/test_rainflow_with_effective_tension.py` — `main` | [docs/registry/implementation-map.yaml:L4876-L4877] |
| F1:262 | `tests/test_wall_thickness_compare_workflow.py` — `_cfg` | [docs/registry/implementation-map.yaml:L4888-L4889] |
| F1:263 | `tests/workflows/test_durable_workflows.py` — `test_workflow_registry` | [docs/registry/implementation-map.yaml:L4898-L4899] |
| F1:264 | `tests/workflows/test_viv_screening_workflow.py` — `screening_input` | [docs/registry/implementation-map.yaml:L4908-L4909] |

## 10. Coverage and open items

Files read for the package: `__init__.py` and `riser_case_screening.py`; there are no other tracked package files at the code pin. Both files are covered by the silent-failure sweep. [src/digitalmodel/subsea/riser/__init__.py:L1-L16] [src/digitalmodel/subsea/riser/riser_case_screening.py:L1-L278]

Module documentation discovery under `docs/` found `docs/domains/risers/scr-slwr-coverage-2026-10.md`, `docs/registry/implementation-map.md` and its YAML companion; the coverage analysis explicitly describes this screening surface and its note limitation. These documents were read; no numerical maturity or validation claim from them is adopted without the code/test evidence above. [docs/domains/risers/scr-slwr-coverage-2026-10.md:L184-L186] [docs/registry/implementation-map.md:L18-L18] [docs/registry/implementation-map.yaml:L4318-L4337]

A broader name search also found `docs/domains/risers/mining-lift-screening.md`; its documented package is `subsea.mining_riser`, so this is a separate module rather than screening-package coverage. [docs/domains/risers/mining-lift-screening.md:L1-L7]

Test coverage read and run: the 11 test definitions in `tests/test_riser_case_screening.py` expand to 14 nodes because the legacy comparison has four parameter rows; the routing contract contributes 10 nodes and the contracts domain contributes 487 nodes. The test file has no direct router/note test; the C4/C5 scratch probes supplement that gap. [tests/test_riser_case_screening.py:L76-L255] [src/digitalmodel/subsea/riser/riser_case_screening.py:L125-L211]

Additional source reading: engine routing, shared dataclasses/code references, strategy registration, API STD 2RD and DNV-ST-F201 strategies, plus the legacy stress calculation used by the tests. [src/digitalmodel/engine.py:L602-L607] [src/digitalmodel/structural/analysis/wall_thickness.py:L88-L181] [src/digitalmodel/codes.py:L74-L76] [src/digitalmodel/structural/analysis/wall_thickness_codes/__init__.py:L4-L13] [tests/test_riser_case_screening.py:L31-L50]

| Open item | Responsibility / tracking | Evidence and closure criterion |
| --- | --- | --- |
| Empty/incomplete evidence can receive PASS; non-finite output is not guarded. | Maintainers; issue pending (1) | S06–S13, S24–S27 and S30; add boundary tests with 0 false PASS records. [src/digitalmodel/subsea/riser/riser_case_screening.py:L100-L102] [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L198] |
| Inputs and assumed defaults lack an explicit unit/finiteness/category contract. | Maintainers; issue pending (2) | Disclose assumptions and reject invalid physical inputs; C0 transfer alone does not establish validity. [src/digitalmodel/subsea/riser/riser_case_screening.py:L33-L50] [src/digitalmodel/subsea/riser/riser_case_screening.py:L139-L164] |
| Exact-edition compliance and independent numerical qualification (the UNVERIFIED claim in section 3). | Maintainers; issue pending (3), #2166 | Obtain an independent API benchmark, structured citations and resolved edition labels; licensed clause text was not checked. Legacy parity does not close this item. [tests/test_riser_case_screening.py:L137-L177] [src/digitalmodel/subsea/riser/riser_case_screening.py:L260-L276] |
| Code-aware F201 reporting and missing combined/collapse F201 scope. | Maintainers; #878 | Distinguish registered pressure containment from unimplemented additional checks and stop printing API-only columns for F201. [src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:L130-L141] [src/digitalmodel/subsea/riser/riser_case_screening.py:L249-L276] |
| AQG appendix, fingerprints and complete report provenance. | Maintainers; #814 | Trace every emitted result to inputs, edition and evidence; C5 proves output consistency only. [src/digitalmodel/subsea/riser/riser_case_screening.py:L166-L198] |
| Map follow-up: helper relations and absent supplied-load screening/ranking families. | Maintainers; issue pending (4) | Inventory and classify these methods before counting their families in a later map revision. [docs/registry/implementation-map.yaml:L4318-L4337] [src/digitalmodel/subsea/riser/riser_case_screening.py:L89-L122] |

Review disposition: DRAFT; no quality clearance or independent standard validation is claimed. Existing issue references are leads and follow-up records, not evidence that a defect has been fixed. Draft references require issue numbers before final A7 clearance.
