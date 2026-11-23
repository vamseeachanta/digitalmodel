# Installation pamphlet knowledge pack

| Header | Value |
| --- | --- |
| Module | installation/installation_pamphlet |
| Repository | digitalmodel |
| Package | src/digitalmodel/installation/installation_pamphlet |
| Pack revision | 0 |
| Code revision | fd5e419b667a28fe411e1b345b40d65da98f248f |
| Analysis family | OrcaFlex installation; registry family IN |
| Quality role | OrcaFlex installation quality manager |
| Status | DRAFT |

## 1. Purpose and scope

`workflow.router(cfg)` resolves a vessel and lift items, assembles calculation results, renders HTML, writes UTF-8 HTML and JSON, and adds output locations to the configuration. [src/digitalmodel/installation/installation_pamphlet/workflow.py:L29-L113]

`calculations.py` delegates crane suitability, motion envelopes and operability, but also implements its own splash-zone force balance; the module-level claim that it only shapes engine outputs does not describe that local calculation. [src/digitalmodel/installation/installation_pamphlet/calculations.py:L7-L10] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L260-L294] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L463-L563]

The installation domain is marked beta; that classification is domain metadata, rather than a validation result for this workflow. [docs/registry/module-routing.yaml:L505-L514]

Scope includes all five package Python files and no separate sub-package; lowering dynamics are described as future verification, while the generated banner describes desktop screening and decision support. [src/digitalmodel/installation/installation_pamphlet/__init__.py:L1-L5] [src/digitalmodel/installation/installation_pamphlet/workflow.py:L1-L10] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L508-L509] [src/digitalmodel/installation/installation_pamphlet/render.py:L587-L589]

The default RAO path is a parametric proxy; supplying a vessel-basis artifact changes provenance to `actual`, which denotes ingestion rather than independently established accuracy. [src/digitalmodel/installation/installation_pamphlet/calculations.py:L73-L78] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L298-L369] [tests/installation/test_installation_pamphlet.py:L573-L600]

## 2. Inputs and outputs

Input ranges below describe implemented handling; absence of a range check is not permission to extrapolate engineering applicability. [src/digitalmodel/installation/installation_pamphlet/workflow.py:L29-L85] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L511-L552]

| Input | Unit / range handling | Default / origin | Evidence |
| --- | --- | --- | --- |
| `vessel`, `vessel_run_tokens` | Text; first case-insensitive database substring match; tokens must be a list or tuple of strings | Database selector and alias tuple in code; identities omitted here | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L44-L50] [src/digitalmodel/installation/installation_pamphlet/workflow.py:L117-L147] |
| `lift_items`, `catalogs` | Ordered records; explicit mass or catalog source/id; no schema validation at this boundary | Empty lift list and catalog mapping | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L151-L185] |
| `mass_air_te`, `lift_radius_m`, `daf` | te, m, dimensionless; converted with Python `float`, without locale normalization here | Radius and amplification defaults in code; numerical provenance remains unverified | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L34-L35] [src/digitalmodel/installation/installation_pamphlet/workflow.py:L160-L179] |
| `operations`, `tp_grid_s`, `hs_scatter_limits` | Operation strings, s, m; empty sequences select defaults | Operation list, period grid, and scatter limits are code defaults | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L94-L95] [src/digitalmodel/installation/installation_pamphlet/workflow.py:L36-L41] |
| `rao_basis` | `auto`, `barge`, `drillship`, `fpso`, `vessel`; selected strings feed dictionary lookups | `auto` in router; missing successful-run evidence selects `barge` | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L48-L50] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L30-L44] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L625-L648] |
| RAO JSON artifact, optional low/high pair | Bare or nested `RAOSet` dictionaries; both low/high paths required for a band on vessel basis | Explicit artifact wins; matching run next; any artifact-bearing run last | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L156-L223] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L232-L256] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L838-L858] |
| Structure hydrodynamics | Dimensionless Cs/Cd/Ca, bottom area m², displaced volume m³, submerged weight kN, lowering speed m/s, snap margin dimensionless | Catalog fields are mandatory within the force calculation; speed and margin have code defaults | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L463-L468] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L511-L525] |
| Completed-run records or queue directory | JSON metadata; only finished records with zero return code retained by directory scan | Explicit list overrides queue scan; otherwise empty list | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L189-L250] |
| Paths and generated label | UTF-8; artifact references try repository root before config directory; output paths relative to config directory | `results` output directory; caller-supplied label or fixed default | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L51-L69] [src/digitalmodel/installation/installation_pamphlet/workflow.py:L254-L284] |

| Output | Unit / interpretation | Evidence |
| --- | --- | --- |
| `lifts` | Air mass, hook load and SWL in te; utilization in percent; defensibility and limiting factors from suitability engine | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L274-L294] |
| `envelopes`, optional `envelope_band` | Limiting Hs in m, Tp keys in s, governing DOF; single envelopes rounded to two decimals; bounds are min/max of two cases | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L372-L401] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L433-L459] |
| `splashzone` | Per eligible structure: Hs in m, submerged weight in kN, bottom area in m², lowering speed in m/s; optional combined min with heavy-lift limit | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L554-L601] |
| `op_table` | Percent of generic scatter below supplied Hs thresholds; thresholds are supplied separately from motion envelopes | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L605-L610] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L860-L891] |
| `provenance` | Ordered task status records, selected basis and basis run ID; these are not AQG signatures | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L716-L792] |
| HTML, data JSON, config summary | Files named from config stem; summary includes output paths and `all_lifts_defensible`; browser weather script is embedded | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L86-L113] [src/digitalmodel/installation/installation_pamphlet/render.py:L501-L503] [src/digitalmodel/installation/installation_pamphlet/render.py:L700-L704] |

Proxy RAO conversion transposes magnitude arrays to frequency-by-heading order, converts period to angular frequency, fills surge/sway/yaw and all phases with zeros, and labels translation m/m and rotation deg/m; its depth is fixed metadata rather than a configurable depth envelope. [src/digitalmodel/installation/installation_pamphlet/calculations.py:L323-L367]

UNVERIFIED: coordinate origin, heading sense, phase convention and artifact unit compatibility are not established by the package's ingestion checks; issue pending (2). [src/digitalmodel/installation/installation_pamphlet/calculations.py:L192-L223] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L323-L367]

## 3. Method and governing standards

| Method | Implementing definition and behavior | Reference limit |
| --- | --- | --- |
| Crane screening | `assess_lifts` calls `vessel_suitability.assess_named`, then computes rounded mass-times-amplification hook load and SWL utilization | DNV-ST-N001 is named in report text without edition or clause | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L260-L294] [src/digitalmodel/installation/installation_pamphlet/render.py:L608-L610] |
| Motion envelope | `_build_rao_set` loads JSON or constructs proxy; `compute_envelopes` delegates to `marine_ops.operation_envelope.operation_envelope` | Renderer names DNV-RP-H103 and DNV-ST-N001, without edition or clause | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L298-L401] [src/digitalmodel/installation/installation_pamphlet/render.py:L615-L620] |
| Damping band | `compute_envelope_band` computes two single envelopes, orders each pair and preserves each bound's governing DOF | Uncertainty presentation, not a solver run | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L433-L459] |
| Splash-zone screen | `compute_splashzone_envelope` forms a quadratic from slamming, drag and inertia, solves for nonnegative Hs, and selects the largest force term | DNV-RP-H103 (2011), section 4, “Lifting through wave zone”; section 4.6, “snap forces”, as named by code | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L116-L123] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L469-L509] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L511-L563] |
| Combined lowering screen | `compute_splashzone_set` takes min(structure limit, vessel heavy-lift limit) where the latter exists | Missing catalog quantities suppress a structure row | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L566-L601] |
| Operability | `compute_operability` passes independent thresholds to `OperabilityTable` on a default `ScatterDiagram` | Not a measured local forecast or automatic consequence of the computed envelopes | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L605-L610] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L860-L891] |
| Risk presentation | Static `RISK_DATA` is rendered alongside browser heuristics; no station-keeping solver is called by this renderer | DNV-OS-E301, DNV-RP-N103 and DNV-ST-N001 appear in the static reference text | [src/digitalmodel/installation/installation_pamphlet/risk_reference.py:L1-L10] [src/digitalmodel/installation/installation_pamphlet/render.py:L59-L82] [src/digitalmodel/installation/installation_pamphlet/render.py:L667-L692] |

UNVERIFIED: DNV-RP-H103, *Modelling and Analysis of Marine Operations*, 2011, section 4 “Lifting through wave zone” and section 4.6 “snap forces”: clause text not checked against the licensed copy; interpretation and screening margins require confirmation; issue pending (5). [src/digitalmodel/orcaflex/installation_analysis.py:L1-L11] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L116-L123] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L469-L509]

UNVERIFIED: DNV-RP-C205 is named for kinematics without an edition or clause title; clause text not checked against the licensed copy; issue pending (5). [src/digitalmodel/installation/installation_pamphlet/calculations.py:L476-L485]

UNVERIFIED: DNV-ST-N001 is named for amplification and motion screening without an edition or clause title; clause text not checked against the licensed copy; issue pending (5). [src/digitalmodel/base_configs/modules/installation_pamphlet/installation_pamphlet.yml:L26-L27] [src/digitalmodel/installation/installation_pamphlet/render.py:L608-L616]

UNVERIFIED: DNV-OS-E301 is named for intact/damaged mooring and risk reference text without an edition or clause title; clause text not checked against the licensed copy; issue pending (5). [src/digitalmodel/installation/installation_pamphlet/risk_reference.py:L10-L10] [src/digitalmodel/installation/installation_pamphlet/render.py:L687-L690]

UNVERIFIED: DNV-RP-N103 is named in static risk material for marine-operation hydrodynamics without an edition or clause title; clause text not checked against the licensed copy; issue pending (5). [src/digitalmodel/installation/installation_pamphlet/risk_reference.py:L10-L10]

UNVERIFIED: standards-derived constants and static risk percentages have no checked independent source or emitted `Citation` sidecar in this package; issue pending (5). [src/digitalmodel/installation/installation_pamphlet/calculations.py:L116-L123] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L877-L896] [src/digitalmodel/installation/installation_pamphlet/risk_reference.py:L1-L10]

No conflicting numbered editions are stated in these package references; the defect is missing editions/clauses and traceable sidecars, rather than evidence for selecting a different edition. [src/digitalmodel/installation/installation_pamphlet/calculations.py:L116-L123] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L469-L509] [src/digitalmodel/installation/installation_pamphlet/risk_reference.py:L10-L10] [src/digitalmodel/installation/installation_pamphlet/render.py:L608-L616]

## 4. Validated worked examples

Validated examples: 0; the golden expectations are attributed to an upstream prototype, and the artifact and damping fixtures explicitly identify themselves as synthetic rather than benchmark data. [tests/installation/test_installation_pamphlet.py:L1-L7] [tests/installation/test_installation_pamphlet.py:L454-L457] [tests/installation/test_installation_pamphlet.py:L657-L673]

Regression examples: 6; each node below passed at the code pin before and after documentation edits, in the offline module suite (45 passed, 0 failed, 0 skipped, 0 NOT_RUN per run); the inputs and assertions are those of the cited tests. [tests/installation/test_installation_pamphlet.py:L543-L570] [tests/installation/test_installation_pamphlet.py:L620-L654] [tests/installation/test_installation_pamphlet.py:L727-L760] [tests/installation/test_installation_pamphlet.py:L438-L451] [tests/installation/test_pamphlet_confidentiality.py:L17-L30]

| Case / source | Input and expected result | Tolerance / basis | Existing node ID | Offline result |
| --- | --- | --- | --- | --- |
| Synthetic JSON round trip; repository fixture | `_synthetic_rao_set` serialized by `_write_rao_artifact`; loaded `to_dict()` equals original | 0 field differences; serialization regression | `tests/installation/test_installation_pamphlet.py::test_load_rao_set_from_artifact_roundtrips` | BASE PASS; AFTER PASS [tests/installation/test_installation_pamphlet.py:L543-L550] |
| Artifact failures; repository fixture | Missing file raises `FileNotFoundError`; malformed JSON and missing RAO keys raise `ValueError` | 0 missing exceptions across 3 cases | `tests/installation/test_installation_pamphlet.py::test_load_rao_set_from_artifact_errors` | BASE PASS; AFTER PASS [tests/installation/test_installation_pamphlet.py:L560-L570] |
| Missing artifact fallback; repository fixture | `vessel` without artifact produces `ship_proxy`, no artifact path, and the same envelope as the ship proxy | 0 field differences | `tests/installation/test_installation_pamphlet.py::test_vessel_without_artifact_is_honest_proxy` | BASE PASS; AFTER PASS [tests/installation/test_installation_pamphlet.py:L620-L642] |
| Synthetic damping pair; repository fixture | Ordered Hs bounds, heavy-lift governing DOF roll, at least one nonzero band width | Minimum width ≥ 0 m; at least 1 positive-width point; min/max use test's `pytest.approx` | `tests/installation/test_installation_pamphlet.py::test_compute_envelope_band_is_ordered_and_roll_damping_controlled` | BASE PASS; AFTER PASS [tests/installation/test_installation_pamphlet.py:L727-L760] |
| Router determinism; repository configuration | Two temporary output directories produce identical HTML and JSON bytes; catalog-specific values are not reproduced here | 0 differing bytes | `tests/installation/test_installation_pamphlet.py::test_router_is_deterministic_on_base_config` | BASE PASS; AFTER PASS [tests/installation/test_installation_pamphlet.py:L438-L451] |
| Configurable alias precedence; codenamed fixture | Matching configured alias selects its artifact and provenance; explicit artifact overrides it | 0 mismatched selections | `tests/installation/test_pamphlet_confidentiality.py::test_extra_run_token_selects_basis_artifact_and_provenance` | BASE PASS; AFTER PASS [tests/installation/test_pamphlet_confidentiality.py:L17-L30] |

Execution record: `PYTHONPATH=src uv run --no-sources python -m pytest tests/installation/test_installation_pamphlet.py tests/installation/test_pamphlet_confidentiality.py -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override, no parallel workers; code pin unchanged, documentation tree uncommitted for AFTER (execution evidence retained for review).

## 5. QA gates

Gate applicability follows AQG r0, section 10, “OrcaFlex installation”; G5 follows section 9, “QA appendix”; offline evidence does not clear a licensed downstream calculation. (AQG r0, sections 9–10.)

| Gate | Applicability and module-specific checks | Current evidence / limit |
| --- | --- | --- |
| G0 | Applicable: C01, C02, C03, C12 | Offline ingestion, queue selection and band checks PASS; strict schema, fingerprints and unit conventions remain open, issue pending (2) |
| G1 | Applicable when a pamphlet relies on a target solver result: C04 | NOT_RUN; package emits HTML/JSON rather than a target-program model, and no matching model/version/load record is supplied, issue pending (6) |
| G2 | Applicable for every lowering stage: C05, C06, C07 | C05/C06 PASS as regressions; independent stage/slings/statics comparison NOT_RUN, issue pending (6) |
| G3 | Applicable at splash-zone and deepest stages: C08 | NOT_RUN; no licensed dynamic model or result, issue pending (6) |
| G4 | Applicable if imported vessel data originate in a new diffraction run: C09 | NOT_RUN for real diffraction; synthetic ingestion is not hydrostatic or mesh validation, issue pending (6) |
| G5 | Applicable to every generated pamphlet: C10, C11 | Determinism/content checks PASS; generated task ledger lacks AQG appendix records and signatures, issue pending (6) |

The checks below define acceptance targets, not claims that missing checks have already been implemented; PASS is restricted to the named executed tests. (AQG r0, sections 3 and 13.)

| Check | Gate | Quantity, executable route and numeric criterion | Evidence / result |
| --- | --- | --- | --- |
| C01 | G0 | `load_rao_set_from_artifact`: 0 field differences after synthetic JSON round trip, 0 missed exceptions for missing/malformed input | PASS: section-4 round-trip and error nodes [tests/installation/test_installation_pamphlet.py:L543-L570] |
| C02 | G0 | `scan_completed_runs`: 0 records for missing directory; 1 accepted finished-successful fixture record, 0 accepted unfinished/failed records | PASS: `tests/installation/test_installation_pamphlet.py::test_scan_completed_runs` and `tests/installation/test_installation_pamphlet.py::test_scan_missing_dir_returns_empty`; no malformed-record diagnostic assertion [tests/installation/test_installation_pamphlet.py:L311-L337] |
| C03 | G0 | `select_rao_basis`: 0 false matches with empty aliases across 3 parameterized cases | PASS: `tests/installation/test_pamphlet_confidentiality.py::test_empty_tokens_do_not_match_every_run` [tests/installation/test_pamphlet_confidentiality.py:L33-L37] |
| C04 | G1 | Resolve the model backing `rao_artifact` or lowering results; OrcaFlex route: open the corresponding model in the exact target version using demo only where supported, otherwise licensed `LoadData`; OrcaWave route: open and validate the backing diffraction model in the recorded target version; require 0 load errors and 0 warnings (counts) and 0 version mismatches (count) | NOT_RUN; missing models/version records, no solver invoked; ingestion alone has no such record [src/digitalmodel/installation/installation_pamphlet/calculations.py:L192-L223] (AQG r0, section 5, “G1 load”) |
| C05 | G2 | `assess_lifts`: hook-load residual against air mass × configured DAF ≤ 0.05 te, utilization > 0 and ≤ 100 percent in the existing catalog regression; not independent validation of DAF | PASS: `tests/installation/test_installation_pamphlet.py::test_golden_catalog_structure_lift_is_defensible` [tests/installation/test_installation_pamphlet.py:L155-L174] |
| C06 | G2 | `assemble_result` / `compute_splashzone_set`: combined Hs residual against min(structure, heavy-lift) ≤ 0.01 m at every fixture period | PASS: `tests/installation/test_installation_pamphlet.py::test_splashzone_set_in_payload_and_provenance` [tests/installation/test_installation_pamphlet.py:L226-L239] |
| C07 | G2 | For each lowering stage compare `assess_lifts` hook load with independent stage balance; difference ≤ 2 percent; record each sling and rigging load with 0 unchecked components (count); compare submerged load with the configured catalog weight, not an assumed air-mass replacement | NOT_RUN; no stage model, sling balance or independent reference in the test suite; issue pending (6) [src/digitalmodel/installation/installation_pamphlet/calculations.py:L260-L294] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L511-L522] (AQG r0, sections 6 and 10) |
| C08 | G3 | Splash-zone and deepest-stage model: reaches 1 s, 0 warnings/errors/non-numeric values (counts), tension within ±25 percent of static and 0 newly slack/compressed lines (count); retain final image | NOT_RUN; the module explicitly leaves dynamic lowering as future work [src/digitalmodel/installation/installation_pamphlet/calculations.py:L508-L509] (AQG r0, section 7, “G3 short dynamics”) |
| C09 | G4 | Before accepting a new artifact for `_build_rao_set`, attach diffraction record with weight/buoyancy difference ≤ 1 percent, independent mesh volume difference ≤ 0.5 percent and 0 invalid normals (count); include mesh/centres/frequency checks from the same record | NOT_RUN; `RAOSet.from_dict` ingestion is not such a record, issue pending (6) [src/digitalmodel/installation/installation_pamphlet/calculations.py:L192-L223] (AQG r0, section 8, “G4 diffraction QA”) |
| C10 | G5 | `router`: 0 differing bytes between two HTML outputs and two JSON outputs for identical configuration | PASS: section-4 router determinism node [tests/installation/test_installation_pamphlet.py:L438-L451] |
| C11 | G5 | `render_pamphlet_html`: 0 missing required content strings in the existing core-section test; 0 withheld-identity display fields in the codenamed router test | PASS: `tests/installation/test_installation_pamphlet.py::test_render_contains_core_sections` and `tests/installation/test_pamphlet_confidentiality.py::test_router_uses_extra_token_and_omits_withheld_operator`; no signed appendix assertion [tests/installation/test_installation_pamphlet.py:L298-L307] [tests/installation/test_pamphlet_confidentiality.py:L49-L69] |
| C12 | G0 | `compute_envelope_band`: minimum upper-minus-lower bound ≥ 0 m at every period; at least 1 strictly positive heavy-lift width (count); 0 governing-DOF mismatches (count) | PASS: section-4 synthetic damping node [tests/installation/test_installation_pamphlet.py:L727-L760] |

Defined checks: G0 = 4, G1 = 1, G2 = 3, G3 = 1, G4 = 1, G5 = 2; total 12 (table above).

## 6. Known failure modes and pitfalls

sweep: 65 found

Sweep scope: all five package Python files, including JavaScript embedded in `render.py`; each row records one distinct site or contiguous conversion/default group, including intentional fallbacks, without classifying every one as a defect. [src/digitalmodel/installation/installation_pamphlet/render.py:L38-L134] [src/digitalmodel/installation/installation_pamphlet/workflow.py:L29-L321] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L1-L896]

Exact commands executed:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|float\(|int\(|\.get\(|setdefault|zeros|catch|\|\|' src/digitalmodel/installation/installation_pamphlet --glob '*.py'
rg -n '0\.0|0\.5|0\.05|5%|1970|cache|unit|phase|heading|density|lowering|default' src/digitalmodel/installation/installation_pamphlet --glob '*.py'
```

| Site | Symptom / cause | Existing detection / disposition |
| --- | --- | --- |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L211-L212] | Missing queue directory returns an empty result without distinguishing no runs from unavailable evidence. | C02 covers empty result, not diagnostic; issue pending (1) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L214-L218] | Broad read/JSON exception silently skips a queue record. | No detection check exists; issue pending (1) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L219-L219] | Unfinished, failed or missing-status records are filtered without a rejection count. | C02 covers finished/failed filtering; issue pending (1) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L220-L223] | Missing audit/summary/workflow/input fields become empty mappings or strings. | No detection check exists; issue pending (1) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L224-L240] | Artifact metadata falls through three fields then returned-files suffix matching, without an existence check here. | Existing scan precedence tests cover selection, not existence; C01 covers later missing-file failure; issue pending (1) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L243-L247] | Absent run ID and finish time are retained as null metadata. | No detection check exists; issue pending (1) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L189-L196] | No explicit list or queue setting returns an empty list; explicit empty list masks the queue. | No detection check exists; issue pending (1) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L295-L298] | Failed scalar item conversion is swallowed and the original value survives until JSON serialization. | No detection check exists; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L537-L540] | Degenerate quadratic or negative discriminant becomes zero Hs; negative root also clamps to zero. | No detection check exists; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L584-L586] | Lift without a dictionary catalog record is silently omitted from splash-zone output. | Existing absence regression covers whole-list omission, not per-item accounting; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L587-L588] | Catalog without coefficients or projected areas is silently omitted. | No detection check exists; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L589-L590] | Catalog without displaced volume is silently omitted. | No detection check exists; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L597-L598] | Missing vessel limit at a period falls back to the structure-only limit. | C06 covers populated limits, not the missing-key branch; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L245-L246] | Non-vessel basis ignores a supplied artifact and returns None by design. | Existing artifact-precedence regression covers this branch; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L249-L256] | Missing matching artifact can use any other run artifact; no available artifact returns None. | Existing precedence regression covers resolution; no identity check exists; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L228-L229] | Absent input becomes empty text; empty configured aliases do not match. | C03 covers empty aliases; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L637-L648] | Absent runs or no recognized diffraction strings select barge proxy. | Existing empty-basis regression; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L615-L621] | Absent workflow/input fields become empty strings before keyword classification. | No detection check exists for malformed metadata; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L660-L663] | Matching vessel record may return missing run ID; no match returns None. | No detection check exists for missing ID; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L665-L671] | Ship basis uses first matching diffraction record or first diffraction entry; absent ID or no match yields None. | Existing provenance regression covers a populated record only; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L73-L92] | Vessel without artifact uses proxy labels/provenance. | Section-4 fallback regression; existing #1538 |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L277-L288] | Mass/radius/amplification conversion assumes dot-decimal text, te and m; false SWL produces null utilization. | C05 covers normal values; no locale or invalid-SWL check exists; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L325-L338] | Proxy periods/headings converted directly; angular frequency assumed rad/s; missing DOFs and phases replaced by zeros. | No detection check exists for fidelity of these assumptions; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L343-L365] | Computed frequency/heading extents cast to float; fixed proxy depth and m/m versus deg/m labels are attached. | No detection check exists for coordinate/unit compatibility; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L390-L395] | Period float conversion and one-decimal keys can collapse nearby requested periods. | No detection check exists; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L511-L519] | External coefficient/area/volume/weight text uses float without separator handling; kN is converted to N without reconciliation to buoyancy and mass here. | No detection check exists at this boundary; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L521-L522] | Lowering speed and snap margin are float inputs; no positive-margin guard at this site. | No detection check exists; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L529-L531] | Period conversion assumes seconds and nonzero positive values. | No detection check exists for invalid periods; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L550-L562] | One-decimal period keys and rounded results may collapse requests; return fields assume kN, m² and m/s. | No detection check exists for collisions; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L605-L610] | Hs inputs cast to float in m and passed to a generic default scatter; no locale/site validation here. | No detection check exists; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L838-L858] | Band omitted unless both paths exist as truthy settings on vessel basis; single case defaults to low-labeled artifact. | Existing backward-compatibility regression covers omitted band; existing #1538 |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L860-L875] | Missing heavy-lift envelope removes combined limit; structure status uses source text; vessel display name has fallback. | C06 covers supplied heavy-lift only; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L877-L895] | Output radius/DAF/period casts repeat assumed units; no artifact becomes None; optional band keys omitted. | Existing backward-compatibility regression covers optional keys; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L116-L123] | Fixed density/gravity/speed/margin defaults lack an emitted citation sidecar. | No detection check exists; issue pending (5) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L105-L113] | Fixed damping note asserts historical numeric conditions independent of supplied artifact metadata. | No detection check exists; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L727-L765] | Static task count/title/status and weather-source wording can disagree with configuration or evidence. | No detection check exists; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L764-L779] | Risk and catalog tasks can be actual without checked source or solver evidence. | No detection check exists; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L30-L41] | Missing settings/empty operations/grids select defaults; radius, DAF, periods and scatter values use direct float conversion. | No detection check exists for missing versus explicit empty inputs; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L44-L51] | Default alias and auto basis apply; blank label becomes a fixed label. | C03 covers empty aliases; no blank-label diagnostic check exists; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L55-L69] | Absent or blank artifact settings become None, including a partial band pair. | Existing backward-compatibility regression covers a partial pair; existing #1538 |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L120-L145] | Default vessel selector and first substring match; absent display label uses key; optional particulars remain None; crane data cast to float. | No detection check exists for ambiguous selection or units; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L152-L179] | Empty lift/catalog defaults; local caches; explicit/catalog masses use float with te assumed. | C05 covers normal catalog mass, not malformed/localized text; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L254-L284] | Output/config location and stem fall back to fixed directory, config file or current directory. | C10 exercises explicit temporary paths only; issue pending (2) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L302-L307] | Out-of-repository display path falls back to an absolute path. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/workflow.py:L318-L320] | Self-check printout uses the fixed period key and prints None if absent. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L42-L49] | Browser Hs interpolation clamps outside period range and falls back to first period if no interval matches. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L51-L72] | Demo forecast and DP/mooring/verdict thresholds are fixed heuristics rather than a station-keeping solution. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L59-L60] | Browser DP risk uses truthiness default current, replacing an explicit zero. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L73-L74] | Invalid or empty forecast times leave nearest-index selection at zero without a diagnostic. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L99-L106] | Window search initializes zero length/best values and returns no window when no qualifying forecast is found; no data-completeness check precedes it. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L85-L89] | Browser display repeats truthiness default current and coerces control inputs numerically. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L121-L125] | Missing/zero live period becomes a fixed period; current is assumed; fetch errors select demo data with a status label. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L127-L133] | Demo uses a fixed index/current and relative timestamps; live refresh starts automatically on page load. | C10 verifies file bytes, not browser behavior; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L149-L159] | Missing provenance becomes empty tasks, barge basis, zero completed count and no run ID; periods cast to float. | No detection check exists; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L168-L177] | Unknown badge becomes question mark; missing section tasks become actual; unknown task state receives weakest ordering. | No detection check exists; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L219-L240] | Fixed envelope plot height and float-key lookup may clip high Hs or fail on period-key collisions. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L300-L305] | Empty task list suppresses panel; fixed status weights and unknown-state zero produce a display percentage. | No detection check exists; issue pending (4) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L327-L337] | Regex digit extraction takes the last integer in risk-share text; absent digits become zero; localized decimals are not parsed. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L357-L365] | Missing splash-zone list becomes empty; axis extent uses fixed fallback/minimum. | Existing absence regression covers empty splash list only; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L408-L411] | No governing values displays a dash; period values cast to float for lookup. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L423-L425] | Missing or empty combined limit silently suppresses that structure table row. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L430-L440] | Empty splash set suppresses block; combined table omitted unless any combined result is truthy. | Existing absence regression covers empty splash set only; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L463-L489] | Absent/empty band suppresses section; missing note becomes fixed numeric narrative; period casts repeat key assumptions. | Existing band-rendering regression covers section omission only; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L648-L652] | Fixed geographic controls and fixed Hs control limits do not use the computed envelope payload. | No detection check exists; issue pending (3) |
| [src/digitalmodel/installation/installation_pamphlet/risk_reference.py:L1-L10] | Embedded risk statistics and severity claims are constant curated text without machine-resolvable source revisions. | No detection check exists; issue pending (5) |

No error-path cache write was found: the only catalog caches are local dictionaries populated by loaders that propagate parse errors; zero-array initialization belongs to proxy RAO construction. [src/digitalmodel/installation/installation_pamphlet/workflow.py:L151-L185] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L127-L149] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L325-L338]

| Confirmed concern / historical issue | Detection / limit | Evidence |
| --- | --- | --- |
| Rendered lift row always displays PASS even if calculation defensibility differs | No detection check exists; issue pending (3); C05 only checks a passing fixture | [src/digitalmodel/installation/installation_pamphlet/render.py:L258-L268] |
| Static item table, crane-capacity narrative and monitor Hs controls do not derive from arbitrary current inputs | No detection check exists; issue pending (3) | [src/digitalmodel/installation/installation_pamphlet/render.py:L612-L613] [src/digitalmodel/installation/installation_pamphlet/render.py:L627-L652] |
| Provenance claims weather table uses motion-envelope limits, but `assemble_result` passes separate configured scatter thresholds | No detection check exists; issue pending (4) | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L759-L765] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L860-L891] |
| Task statuses and weighted progress do not establish standard compliance; missing section tasks default to actual | No detection check exists; issue pending (4) | [src/digitalmodel/installation/installation_pamphlet/render.py:L172-L177] [src/digitalmodel/installation/installation_pamphlet/render.py:L300-L324] |
| #1111 and #1538: absence of an artifact leaves vessel selection on ship proxy | C01 and section-4 fallback node detect artifact ingestion/fallback; no real-artifact result is asserted here | [tests/installation/test_installation_pamphlet.py:L573-L642] |
| #1112: missing per-structure screen was addressed by local force calculation | C06 checks combined limit and task state; independent validation and dynamics remain absent | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L463-L601] [tests/installation/test_installation_pamphlet.py:L226-L239] |
| #1113: structure catalog replaces a proxy item | C05 checks catalog lift and task state; fixture values are not independent validation | [tests/installation/test_installation_pamphlet.py:L144-L174] |
| #1105: deterministic workflow and provenance foundation | C10 checks generated-byte determinism; C11 checks rendered content | [tests/installation/test_installation_pamphlet.py:L298-L307] [tests/installation/test_installation_pamphlet.py:L438-L451] |
| #2107: map attaches an unchecked upstream parser defect to artifact readers | No detection check exists here for upstream truncation; do not infer that this module reproduces it merely from the map tag | [docs/registry/implementation-map.yaml:L11863-L11888] |

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-17 | Initial documentation | Ingested and synthetic artifacts can both receive `actual`; that flag describes data routing, not independent validation | [tests/installation/test_installation_pamphlet.py:L573-L600] | Separate regression examples from validation and licensed gate evidence | Not created; issue pending (8) |
| 2026-10-17 | Initial documentation | Byte determinism covers generated files; browser forecast and heuristic decisions remain outside the offline tests | [tests/installation/test_installation_pamphlet.py:L438-L451] [src/digitalmodel/installation/installation_pamphlet/render.py:L118-L133] | Record browser assumptions in section 6 | Not created; issue pending (8) |

## 8. Ownership

Review routing for this draft: OrcaFlex installation quality manager; deputy role: installation quality deputy; producing role: module documentation contributor; escalation route: family quality manager for acceptance criteria, standards interpretation and missing evidence, then the maintainer for unresolved cross-family matters.

UNVERIFIED: named appointments, deputy availability, shared-log registration and clearance are not established by repository evidence; issue pending (8).

## 9. Dependencies and implementation inventory

| Dependency / consumer | Boundary | Evidence |
| --- | --- | --- |
| `marine_ops.vessel_db.loader.installation_vessels` | Vessel particulars and crane curve lookup | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L117-L147] |
| `marine_ops.installation.vessel_suitability` | `LiftRequirement` and `assess_named` | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L272-L291] |
| `hydrodynamics.diffraction.output_schemas` | JSON reconstruction through `RAOSet.from_dict`; arrays, periods and headings | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L192-L223] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L314-L367] |
| `orcawave.vessel_database` | Parametric representative RAOs, not a solver invocation | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L314-L324] |
| `marine_ops.operation_envelope` | Motion criteria applied to RAO set | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L387-L400] |
| `orcaflex.weather_window` | Generic scatter / operability table; Python calculation boundary | [src/digitalmodel/installation/installation_pamphlet/calculations.py:L605-L610] |
| Catalog JSON / queue metadata | Explicit configuration references and scanner input; no private data copied into this pack | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L151-L250] |
| `render.py`, browser weather widget | HTML/JSON consumer and later forecast requests; no browser execution checked here | [src/digitalmodel/installation/installation_pamphlet/workflow.py:L86-L111] [src/digitalmodel/installation/installation_pamphlet/render.py:L76-L82] [src/digitalmodel/installation/installation_pamphlet/render.py:L118-L133] |

UNVERIFIED: target OrcaFlex/OrcaWave versions, inventory rows for upstream modules and real-artifact fingerprints are not established by this module's configuration or output schema; issue pending (6). [src/digitalmodel/installation/installation_pamphlet/workflow.py:L29-L113] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L877-L896]

Every package source mention of OrcaFlex, OrcaWave or OrcFxAPI, including lowercase module names, is inventoried below; all are text references, metadata or Python helper imports, with no solver launch in those statements. [src/digitalmodel/installation/installation_pamphlet/calculations.py:L1-L896] [src/digitalmodel/installation/installation_pamphlet/render.py:L1-L704]

| Source occurrence | Dependency meaning |
| --- | --- |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L25-L25] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L52-L52] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L58-L58] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L62-L62] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L68-L68] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L196-L196] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L198-L198] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L314-L314] | Python helper import: proxy database |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L508-L508] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L607-L607] | Python helper import: scatter/operability |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L620-L620] | String classifier for completed-run metadata |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L621-L621] | String classifier for completed-run metadata |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L633-L633] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L634-L634] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L697-L697] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L698-L698] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L701-L701] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L714-L714] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L749-L749] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L755-L755] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/calculations.py:L756-L756] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L297-L297] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L324-L324] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L446-L446] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L460-L460] | Solver-related documentation or report text; no executable solver call |
| [src/digitalmodel/installation/installation_pamphlet/render.py:L588-L588] | Solver-related documentation or report text; no executable solver call |

### Map revision and counted families

Map ref: `origin/codex/implmap-work`, commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; named data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; the map file at this ref is byte-identical to the map at the pack code pin. [docs/registry/implementation-map.yaml:L1-L4]

| Rank | Family | In-package implementation rows | Full family inventory |
| --- | --- | --- | --- |
| 1 | `rao-readers-and-orcaflex-converters` | 2 | 295 [docs/registry/implementation-map.yaml:L11863-L11888] |
| 2 | `wind-and-current-force-routes` | 1 | 135 [docs/registry/implementation-map.yaml:L15887-L15896] |

Both families are counted; fewer than three have in-package implementation rows; rows under `rejected_candidates` are excluded from implementation counts. [docs/registry/implementation-map.yaml:L11863-L11888] [docs/registry/implementation-map.yaml:L49168-L49195]

The complete family inventories below identify each implementation by its stable family ordinal and a citation to its full map row (`path`, `symbol`, line and classification); external identities are kept in that existing inventory instead of transcribed into this pack. In-package rows are `_extract_rao_dict`, `load_rao_set_from_artifact`, and `compute_splashzone_envelope`; the map's older line number for the latter resolves to the current definition at line 463. [docs/registry/implementation-map.yaml:L11863-L11888] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L463-L468]

#### rao-readers-and-orcaflex-converters

| Implementation | Scope | Full map row |
| --- | --- | --- |
| 1 | external map entry | [docs/registry/implementation-map.yaml:L9516-L9527] |
| 2 | external map entry | [docs/registry/implementation-map.yaml:L9528-L9539] |
| 3 | external map entry | [docs/registry/implementation-map.yaml:L9540-L9551] |
| 4 | external map entry | [docs/registry/implementation-map.yaml:L9552-L9563] |
| 5 | external map entry | [docs/registry/implementation-map.yaml:L9564-L9578] |
| 6 | external map entry | [docs/registry/implementation-map.yaml:L9579-L9589] |
| 7 | external map entry | [docs/registry/implementation-map.yaml:L9590-L9601] |
| 8 | external map entry | [docs/registry/implementation-map.yaml:L9602-L9613] |
| 9 | external map entry | [docs/registry/implementation-map.yaml:L9614-L9625] |
| 10 | external map entry | [docs/registry/implementation-map.yaml:L9626-L9637] |
| 11 | external map entry | [docs/registry/implementation-map.yaml:L9638-L9652] |
| 12 | external map entry | [docs/registry/implementation-map.yaml:L9653-L9664] |
| 13 | external map entry | [docs/registry/implementation-map.yaml:L9665-L9678] |
| 14 | external map entry | [docs/registry/implementation-map.yaml:L9679-L9690] |
| 15 | external map entry | [docs/registry/implementation-map.yaml:L9691-L9704] |
| 16 | external map entry | [docs/registry/implementation-map.yaml:L9705-L9715] |
| 17 | external map entry | [docs/registry/implementation-map.yaml:L9716-L9727] |
| 18 | external map entry | [docs/registry/implementation-map.yaml:L9728-L9739] |
| 19 | external map entry | [docs/registry/implementation-map.yaml:L9740-L9753] |
| 20 | external map entry | [docs/registry/implementation-map.yaml:L9754-L9765] |
| 21 | external map entry | [docs/registry/implementation-map.yaml:L9766-L9777] |
| 22 | external map entry | [docs/registry/implementation-map.yaml:L9778-L9788] |
| 23 | external map entry | [docs/registry/implementation-map.yaml:L9789-L9800] |
| 24 | external map entry | [docs/registry/implementation-map.yaml:L9801-L9812] |
| 25 | external map entry | [docs/registry/implementation-map.yaml:L9813-L9826] |
| 26 | external map entry | [docs/registry/implementation-map.yaml:L9827-L9838] |
| 27 | external map entry | [docs/registry/implementation-map.yaml:L9839-L9849] |
| 28 | external map entry | [docs/registry/implementation-map.yaml:L9850-L9861] |
| 29 | external map entry | [docs/registry/implementation-map.yaml:L9862-L9873] |
| 30 | external map entry | [docs/registry/implementation-map.yaml:L9874-L9885] |
| 31 | external map entry | [docs/registry/implementation-map.yaml:L9886-L9897] |
| 32 | external map entry | [docs/registry/implementation-map.yaml:L9898-L9909] |
| 33 | external map entry | [docs/registry/implementation-map.yaml:L9910-L9921] |
| 34 | external map entry | [docs/registry/implementation-map.yaml:L9922-L9932] |
| 35 | external map entry | [docs/registry/implementation-map.yaml:L9933-L9943] |
| 36 | external map entry | [docs/registry/implementation-map.yaml:L9944-L9954] |
| 37 | external map entry | [docs/registry/implementation-map.yaml:L9955-L9965] |
| 38 | external map entry | [docs/registry/implementation-map.yaml:L9966-L9977] |
| 39 | external map entry | [docs/registry/implementation-map.yaml:L9978-L9989] |
| 40 | external map entry | [docs/registry/implementation-map.yaml:L9990-L10004] |
| 41 | external map entry | [docs/registry/implementation-map.yaml:L10005-L10017] |
| 42 | external map entry | [docs/registry/implementation-map.yaml:L10018-L10032] |
| 43 | external map entry | [docs/registry/implementation-map.yaml:L10033-L10046] |
| 44 | external map entry | [docs/registry/implementation-map.yaml:L10047-L10060] |
| 45 | external map entry | [docs/registry/implementation-map.yaml:L10061-L10072] |
| 46 | external map entry | [docs/registry/implementation-map.yaml:L10073-L10087] |
| 47 | external map entry | [docs/registry/implementation-map.yaml:L10088-L10099] |
| 48 | external map entry | [docs/registry/implementation-map.yaml:L10100-L10111] |
| 49 | external map entry | [docs/registry/implementation-map.yaml:L10112-L10122] |
| 50 | external map entry | [docs/registry/implementation-map.yaml:L10123-L10134] |
| 51 | external map entry | [docs/registry/implementation-map.yaml:L10135-L10146] |
| 52 | external map entry | [docs/registry/implementation-map.yaml:L10147-L10158] |
| 53 | external map entry | [docs/registry/implementation-map.yaml:L10159-L10169] |
| 54 | external map entry | [docs/registry/implementation-map.yaml:L10170-L10181] |
| 55 | external map entry | [docs/registry/implementation-map.yaml:L10182-L10193] |
| 56 | external map entry | [docs/registry/implementation-map.yaml:L10194-L10207] |
| 57 | external map entry | [docs/registry/implementation-map.yaml:L10208-L10222] |
| 58 | external map entry | [docs/registry/implementation-map.yaml:L10223-L10235] |
| 59 | external map entry | [docs/registry/implementation-map.yaml:L10236-L10246] |
| 60 | external map entry | [docs/registry/implementation-map.yaml:L10247-L10260] |
| 61 | external map entry | [docs/registry/implementation-map.yaml:L10261-L10272] |
| 62 | external map entry | [docs/registry/implementation-map.yaml:L10273-L10284] |
| 63 | external map entry | [docs/registry/implementation-map.yaml:L10285-L10296] |
| 64 | external map entry | [docs/registry/implementation-map.yaml:L10297-L10307] |
| 65 | external map entry | [docs/registry/implementation-map.yaml:L10308-L10319] |
| 66 | external map entry | [docs/registry/implementation-map.yaml:L10320-L10333] |
| 67 | external map entry | [docs/registry/implementation-map.yaml:L10334-L10345] |
| 68 | external map entry | [docs/registry/implementation-map.yaml:L10346-L10356] |
| 69 | external map entry | [docs/registry/implementation-map.yaml:L10357-L10368] |
| 70 | external map entry | [docs/registry/implementation-map.yaml:L10369-L10380] |
| 71 | external map entry | [docs/registry/implementation-map.yaml:L10381-L10391] |
| 72 | external map entry | [docs/registry/implementation-map.yaml:L10392-L10403] |
| 73 | external map entry | [docs/registry/implementation-map.yaml:L10404-L10415] |
| 74 | external map entry | [docs/registry/implementation-map.yaml:L10416-L10427] |
| 75 | external map entry | [docs/registry/implementation-map.yaml:L10428-L10439] |
| 76 | external map entry | [docs/registry/implementation-map.yaml:L10440-L10451] |
| 77 | external map entry | [docs/registry/implementation-map.yaml:L10452-L10465] |
| 78 | external map entry | [docs/registry/implementation-map.yaml:L10466-L10477] |
| 79 | external map entry | [docs/registry/implementation-map.yaml:L10478-L10489] |
| 80 | external map entry | [docs/registry/implementation-map.yaml:L10490-L10501] |
| 81 | external map entry | [docs/registry/implementation-map.yaml:L10502-L10513] |
| 82 | external map entry | [docs/registry/implementation-map.yaml:L10514-L10527] |
| 83 | external map entry | [docs/registry/implementation-map.yaml:L10528-L10539] |
| 84 | external map entry | [docs/registry/implementation-map.yaml:L10540-L10551] |
| 85 | external map entry | [docs/registry/implementation-map.yaml:L10552-L10563] |
| 86 | external map entry | [docs/registry/implementation-map.yaml:L10564-L10575] |
| 87 | external map entry | [docs/registry/implementation-map.yaml:L10576-L10587] |
| 88 | external map entry | [docs/registry/implementation-map.yaml:L10588-L10599] |
| 89 | external map entry | [docs/registry/implementation-map.yaml:L10600-L10611] |
| 90 | external map entry | [docs/registry/implementation-map.yaml:L10612-L10623] |
| 91 | external map entry | [docs/registry/implementation-map.yaml:L10624-L10635] |
| 92 | external map entry | [docs/registry/implementation-map.yaml:L10636-L10647] |
| 93 | external map entry | [docs/registry/implementation-map.yaml:L10648-L10659] |
| 94 | external map entry | [docs/registry/implementation-map.yaml:L10660-L10671] |
| 95 | external map entry | [docs/registry/implementation-map.yaml:L10672-L10683] |
| 96 | external map entry | [docs/registry/implementation-map.yaml:L10684-L10699] |
| 97 | external map entry | [docs/registry/implementation-map.yaml:L10700-L10711] |
| 98 | external map entry | [docs/registry/implementation-map.yaml:L10712-L10725] |
| 99 | external map entry | [docs/registry/implementation-map.yaml:L10726-L10737] |
| 100 | external map entry | [docs/registry/implementation-map.yaml:L10738-L10749] |
| 101 | external map entry | [docs/registry/implementation-map.yaml:L10750-L10761] |
| 102 | external map entry | [docs/registry/implementation-map.yaml:L10762-L10775] |
| 103 | external map entry | [docs/registry/implementation-map.yaml:L10776-L10789] |
| 104 | external map entry | [docs/registry/implementation-map.yaml:L10790-L10801] |
| 105 | external map entry | [docs/registry/implementation-map.yaml:L10802-L10815] |
| 106 | external map entry | [docs/registry/implementation-map.yaml:L10816-L10828] |
| 107 | external map entry | [docs/registry/implementation-map.yaml:L10829-L10841] |
| 108 | external map entry | [docs/registry/implementation-map.yaml:L10842-L10853] |
| 109 | external map entry | [docs/registry/implementation-map.yaml:L10854-L10865] |
| 110 | external map entry | [docs/registry/implementation-map.yaml:L10866-L10877] |
| 111 | external map entry | [docs/registry/implementation-map.yaml:L10878-L10891] |
| 112 | external map entry | [docs/registry/implementation-map.yaml:L10892-L10905] |
| 113 | external map entry | [docs/registry/implementation-map.yaml:L10906-L10917] |
| 114 | external map entry | [docs/registry/implementation-map.yaml:L10918-L10933] |
| 115 | external map entry | [docs/registry/implementation-map.yaml:L10934-L10948] |
| 116 | external map entry | [docs/registry/implementation-map.yaml:L10949-L10962] |
| 117 | external map entry | [docs/registry/implementation-map.yaml:L10963-L10979] |
| 118 | external map entry | [docs/registry/implementation-map.yaml:L10980-L10991] |
| 119 | external map entry | [docs/registry/implementation-map.yaml:L10992-L11003] |
| 120 | external map entry | [docs/registry/implementation-map.yaml:L11004-L11015] |
| 121 | external map entry | [docs/registry/implementation-map.yaml:L11016-L11027] |
| 122 | external map entry | [docs/registry/implementation-map.yaml:L11028-L11039] |
| 123 | external map entry | [docs/registry/implementation-map.yaml:L11040-L11051] |
| 124 | external map entry | [docs/registry/implementation-map.yaml:L11052-L11063] |
| 125 | external map entry | [docs/registry/implementation-map.yaml:L11064-L11075] |
| 126 | external map entry | [docs/registry/implementation-map.yaml:L11076-L11089] |
| 127 | external map entry | [docs/registry/implementation-map.yaml:L11090-L11101] |
| 128 | external map entry | [docs/registry/implementation-map.yaml:L11102-L11113] |
| 129 | external map entry | [docs/registry/implementation-map.yaml:L11114-L11125] |
| 130 | external map entry | [docs/registry/implementation-map.yaml:L11126-L11137] |
| 131 | external map entry | [docs/registry/implementation-map.yaml:L11138-L11149] |
| 132 | external map entry | [docs/registry/implementation-map.yaml:L11150-L11163] |
| 133 | external map entry | [docs/registry/implementation-map.yaml:L11164-L11175] |
| 134 | external map entry | [docs/registry/implementation-map.yaml:L11176-L11187] |
| 135 | external map entry | [docs/registry/implementation-map.yaml:L11188-L11202] |
| 136 | external map entry | [docs/registry/implementation-map.yaml:L11203-L11213] |
| 137 | external map entry | [docs/registry/implementation-map.yaml:L11214-L11225] |
| 138 | external map entry | [docs/registry/implementation-map.yaml:L11226-L11237] |
| 139 | external map entry | [docs/registry/implementation-map.yaml:L11238-L11248] |
| 140 | external map entry | [docs/registry/implementation-map.yaml:L11249-L11260] |
| 141 | external map entry | [docs/registry/implementation-map.yaml:L11261-L11274] |
| 142 | external map entry | [docs/registry/implementation-map.yaml:L11275-L11286] |
| 143 | external map entry | [docs/registry/implementation-map.yaml:L11287-L11298] |
| 144 | external map entry | [docs/registry/implementation-map.yaml:L11299-L11310] |
| 145 | external map entry | [docs/registry/implementation-map.yaml:L11311-L11322] |
| 146 | external map entry | [docs/registry/implementation-map.yaml:L11323-L11334] |
| 147 | external map entry | [docs/registry/implementation-map.yaml:L11335-L11346] |
| 148 | external map entry | [docs/registry/implementation-map.yaml:L11347-L11360] |
| 149 | external map entry | [docs/registry/implementation-map.yaml:L11361-L11374] |
| 150 | external map entry | [docs/registry/implementation-map.yaml:L11375-L11388] |
| 151 | external map entry | [docs/registry/implementation-map.yaml:L11389-L11400] |
| 152 | external map entry | [docs/registry/implementation-map.yaml:L11401-L11414] |
| 153 | external map entry | [docs/registry/implementation-map.yaml:L11415-L11426] |
| 154 | external map entry | [docs/registry/implementation-map.yaml:L11427-L11438] |
| 155 | external map entry | [docs/registry/implementation-map.yaml:L11439-L11450] |
| 156 | external map entry | [docs/registry/implementation-map.yaml:L11451-L11462] |
| 157 | external map entry | [docs/registry/implementation-map.yaml:L11463-L11474] |
| 158 | external map entry | [docs/registry/implementation-map.yaml:L11475-L11486] |
| 159 | external map entry | [docs/registry/implementation-map.yaml:L11487-L11498] |
| 160 | external map entry | [docs/registry/implementation-map.yaml:L11499-L11510] |
| 161 | external map entry | [docs/registry/implementation-map.yaml:L11511-L11524] |
| 162 | external map entry | [docs/registry/implementation-map.yaml:L11525-L11536] |
| 163 | external map entry | [docs/registry/implementation-map.yaml:L11537-L11548] |
| 164 | external map entry | [docs/registry/implementation-map.yaml:L11549-L11560] |
| 165 | external map entry | [docs/registry/implementation-map.yaml:L11561-L11574] |
| 166 | external map entry | [docs/registry/implementation-map.yaml:L11575-L11590] |
| 167 | external map entry | [docs/registry/implementation-map.yaml:L11591-L11603] |
| 168 | external map entry | [docs/registry/implementation-map.yaml:L11604-L11619] |
| 169 | external map entry | [docs/registry/implementation-map.yaml:L11620-L11633] |
| 170 | external map entry | [docs/registry/implementation-map.yaml:L11634-L11648] |
| 171 | external map entry | [docs/registry/implementation-map.yaml:L11649-L11662] |
| 172 | external map entry | [docs/registry/implementation-map.yaml:L11663-L11674] |
| 173 | external map entry | [docs/registry/implementation-map.yaml:L11675-L11686] |
| 174 | external map entry | [docs/registry/implementation-map.yaml:L11687-L11698] |
| 175 | external map entry | [docs/registry/implementation-map.yaml:L11699-L11710] |
| 176 | external map entry | [docs/registry/implementation-map.yaml:L11711-L11724] |
| 177 | external map entry | [docs/registry/implementation-map.yaml:L11725-L11736] |
| 178 | external map entry | [docs/registry/implementation-map.yaml:L11737-L11748] |
| 179 | external map entry | [docs/registry/implementation-map.yaml:L11749-L11762] |
| 180 | external map entry | [docs/registry/implementation-map.yaml:L11763-L11774] |
| 181 | external map entry | [docs/registry/implementation-map.yaml:L11775-L11786] |
| 182 | external map entry | [docs/registry/implementation-map.yaml:L11787-L11798] |
| 183 | external map entry | [docs/registry/implementation-map.yaml:L11799-L11812] |
| 184 | external map entry | [docs/registry/implementation-map.yaml:L11813-L11824] |
| 185 | external map entry | [docs/registry/implementation-map.yaml:L11825-L11836] |
| 186 | external map entry | [docs/registry/implementation-map.yaml:L11837-L11850] |
| 187 | external map entry | [docs/registry/implementation-map.yaml:L11851-L11862] |
| 188 | package: `_extract_rao_dict` | [docs/registry/implementation-map.yaml:L11863-L11874] |
| 189 | package: `load_rao_set_from_artifact` | [docs/registry/implementation-map.yaml:L11875-L11888] |
| 190 | external map entry | [docs/registry/implementation-map.yaml:L11889-L11900] |
| 191 | external map entry | [docs/registry/implementation-map.yaml:L11901-L11912] |
| 192 | external map entry | [docs/registry/implementation-map.yaml:L11913-L11926] |
| 193 | external map entry | [docs/registry/implementation-map.yaml:L11927-L11938] |
| 194 | external map entry | [docs/registry/implementation-map.yaml:L11939-L11950] |
| 195 | external map entry | [docs/registry/implementation-map.yaml:L11951-L11964] |
| 196 | external map entry | [docs/registry/implementation-map.yaml:L11965-L11976] |
| 197 | external map entry | [docs/registry/implementation-map.yaml:L11977-L11990] |
| 198 | external map entry | [docs/registry/implementation-map.yaml:L11991-L12002] |
| 199 | external map entry | [docs/registry/implementation-map.yaml:L12003-L12014] |
| 200 | external map entry | [docs/registry/implementation-map.yaml:L12015-L12026] |
| 201 | external map entry | [docs/registry/implementation-map.yaml:L12027-L12038] |
| 202 | external map entry | [docs/registry/implementation-map.yaml:L12039-L12052] |
| 203 | external map entry | [docs/registry/implementation-map.yaml:L12053-L12064] |
| 204 | external map entry | [docs/registry/implementation-map.yaml:L12065-L12078] |
| 205 | external map entry | [docs/registry/implementation-map.yaml:L12079-L12095] |
| 206 | external map entry | [docs/registry/implementation-map.yaml:L12096-L12107] |
| 207 | external map entry | [docs/registry/implementation-map.yaml:L12108-L12119] |
| 208 | external map entry | [docs/registry/implementation-map.yaml:L12120-L12131] |
| 209 | external map entry | [docs/registry/implementation-map.yaml:L12132-L12143] |
| 210 | external map entry | [docs/registry/implementation-map.yaml:L12144-L12159] |
| 211 | external map entry | [docs/registry/implementation-map.yaml:L12160-L12173] |
| 212 | external map entry | [docs/registry/implementation-map.yaml:L12174-L12186] |
| 213 | external map entry | [docs/registry/implementation-map.yaml:L12187-L12199] |
| 214 | external map entry | [docs/registry/implementation-map.yaml:L12200-L12211] |
| 215 | external map entry | [docs/registry/implementation-map.yaml:L12212-L12226] |
| 216 | external map entry | [docs/registry/implementation-map.yaml:L12227-L12238] |
| 217 | external map entry | [docs/registry/implementation-map.yaml:L12239-L12254] |
| 218 | external map entry | [docs/registry/implementation-map.yaml:L12255-L12266] |
| 219 | external map entry | [docs/registry/implementation-map.yaml:L12267-L12278] |
| 220 | external map entry | [docs/registry/implementation-map.yaml:L12279-L12290] |
| 221 | external map entry | [docs/registry/implementation-map.yaml:L12291-L12302] |
| 222 | external map entry | [docs/registry/implementation-map.yaml:L12303-L12314] |
| 223 | external map entry | [docs/registry/implementation-map.yaml:L12315-L12326] |
| 224 | external map entry | [docs/registry/implementation-map.yaml:L12327-L12338] |
| 225 | external map entry | [docs/registry/implementation-map.yaml:L12339-L12350] |
| 226 | external map entry | [docs/registry/implementation-map.yaml:L12351-L12362] |
| 227 | external map entry | [docs/registry/implementation-map.yaml:L12363-L12376] |
| 228 | external map entry | [docs/registry/implementation-map.yaml:L12377-L12388] |
| 229 | external map entry | [docs/registry/implementation-map.yaml:L12389-L12400] |
| 230 | external map entry | [docs/registry/implementation-map.yaml:L12401-L12412] |
| 231 | external map entry | [docs/registry/implementation-map.yaml:L12413-L12427] |
| 232 | external map entry | [docs/registry/implementation-map.yaml:L12428-L12441] |
| 233 | external map entry | [docs/registry/implementation-map.yaml:L12442-L12453] |
| 234 | external map entry | [docs/registry/implementation-map.yaml:L12454-L12465] |
| 235 | external map entry | [docs/registry/implementation-map.yaml:L12466-L12477] |
| 236 | external map entry | [docs/registry/implementation-map.yaml:L12478-L12489] |
| 237 | external map entry | [docs/registry/implementation-map.yaml:L12490-L12503] |
| 238 | external map entry | [docs/registry/implementation-map.yaml:L12504-L12515] |
| 239 | external map entry | [docs/registry/implementation-map.yaml:L12516-L12529] |
| 240 | external map entry | [docs/registry/implementation-map.yaml:L12530-L12541] |
| 241 | external map entry | [docs/registry/implementation-map.yaml:L12542-L12555] |
| 242 | external map entry | [docs/registry/implementation-map.yaml:L12556-L12567] |
| 243 | external map entry | [docs/registry/implementation-map.yaml:L12568-L12582] |
| 244 | external map entry | [docs/registry/implementation-map.yaml:L12583-L12594] |
| 245 | external map entry | [docs/registry/implementation-map.yaml:L12595-L12606] |
| 246 | external map entry | [docs/registry/implementation-map.yaml:L12607-L12620] |
| 247 | external map entry | [docs/registry/implementation-map.yaml:L12621-L12632] |
| 248 | external map entry | [docs/registry/implementation-map.yaml:L12633-L12646] |
| 249 | external map entry | [docs/registry/implementation-map.yaml:L12647-L12658] |
| 250 | external map entry | [docs/registry/implementation-map.yaml:L12659-L12670] |
| 251 | external map entry | [docs/registry/implementation-map.yaml:L12671-L12682] |
| 252 | external map entry | [docs/registry/implementation-map.yaml:L12683-L12696] |
| 253 | external map entry | [docs/registry/implementation-map.yaml:L12697-L12710] |
| 254 | external map entry | [docs/registry/implementation-map.yaml:L12711-L12721] |
| 255 | external map entry | [docs/registry/implementation-map.yaml:L12722-L12733] |
| 256 | external map entry | [docs/registry/implementation-map.yaml:L12734-L12745] |
| 257 | external map entry | [docs/registry/implementation-map.yaml:L12746-L12759] |
| 258 | external map entry | [docs/registry/implementation-map.yaml:L12760-L12773] |
| 259 | external map entry | [docs/registry/implementation-map.yaml:L12774-L12787] |
| 260 | external map entry | [docs/registry/implementation-map.yaml:L12788-L12799] |
| 261 | external map entry | [docs/registry/implementation-map.yaml:L12800-L12813] |
| 262 | external map entry | [docs/registry/implementation-map.yaml:L12814-L12827] |
| 263 | external map entry | [docs/registry/implementation-map.yaml:L12828-L12839] |
| 264 | external map entry | [docs/registry/implementation-map.yaml:L12840-L12850] |
| 265 | external map entry | [docs/registry/implementation-map.yaml:L12851-L12862] |
| 266 | external map entry | [docs/registry/implementation-map.yaml:L12863-L12874] |
| 267 | external map entry | [docs/registry/implementation-map.yaml:L12875-L12886] |
| 268 | external map entry | [docs/registry/implementation-map.yaml:L12887-L12898] |
| 269 | external map entry | [docs/registry/implementation-map.yaml:L12899-L12910] |
| 270 | external map entry | [docs/registry/implementation-map.yaml:L12911-L12922] |
| 271 | external map entry | [docs/registry/implementation-map.yaml:L12923-L12934] |
| 272 | external map entry | [docs/registry/implementation-map.yaml:L12935-L12946] |
| 273 | external map entry | [docs/registry/implementation-map.yaml:L12947-L12958] |
| 274 | external map entry | [docs/registry/implementation-map.yaml:L12959-L12973] |
| 275 | external map entry | [docs/registry/implementation-map.yaml:L12974-L12986] |
| 276 | external map entry | [docs/registry/implementation-map.yaml:L12987-L12998] |
| 277 | external map entry | [docs/registry/implementation-map.yaml:L12999-L13010] |
| 278 | external map entry | [docs/registry/implementation-map.yaml:L13011-L13022] |
| 279 | external map entry | [docs/registry/implementation-map.yaml:L13023-L13034] |
| 280 | external map entry | [docs/registry/implementation-map.yaml:L13035-L13048] |
| 281 | external map entry | [docs/registry/implementation-map.yaml:L13049-L13062] |
| 282 | external map entry | [docs/registry/implementation-map.yaml:L13063-L13074] |
| 283 | external map entry | [docs/registry/implementation-map.yaml:L13075-L13086] |
| 284 | external map entry | [docs/registry/implementation-map.yaml:L13087-L13101] |
| 285 | external map entry | [docs/registry/implementation-map.yaml:L13102-L13113] |
| 286 | external map entry | [docs/registry/implementation-map.yaml:L13114-L13125] |
| 287 | external map entry | [docs/registry/implementation-map.yaml:L13126-L13139] |
| 288 | external map entry | [docs/registry/implementation-map.yaml:L13140-L13153] |
| 289 | external map entry | [docs/registry/implementation-map.yaml:L13154-L13167] |
| 290 | external map entry | [docs/registry/implementation-map.yaml:L13168-L13181] |
| 291 | external map entry | [docs/registry/implementation-map.yaml:L13182-L13193] |
| 292 | external map entry | [docs/registry/implementation-map.yaml:L13194-L13204] |
| 293 | external map entry | [docs/registry/implementation-map.yaml:L13205-L13216] |
| 294 | external map entry | [docs/registry/implementation-map.yaml:L13217-L13228] |
| 295 | external map entry | [docs/registry/implementation-map.yaml:L13229-L13240] |

#### wind-and-current-force-routes

| Implementation | Scope | Full map row |
| --- | --- | --- |
| 1 | external map entry | [docs/registry/implementation-map.yaml:L15400-L15409] |
| 2 | external map entry | [docs/registry/implementation-map.yaml:L15410-L15419] |
| 3 | external map entry | [docs/registry/implementation-map.yaml:L15420-L15428] |
| 4 | external map entry | [docs/registry/implementation-map.yaml:L15429-L15438] |
| 5 | external map entry | [docs/registry/implementation-map.yaml:L15439-L15448] |
| 6 | external map entry | [docs/registry/implementation-map.yaml:L15449-L15460] |
| 7 | external map entry | [docs/registry/implementation-map.yaml:L15461-L15470] |
| 8 | external map entry | [docs/registry/implementation-map.yaml:L15471-L15480] |
| 9 | external map entry | [docs/registry/implementation-map.yaml:L15481-L15490] |
| 10 | external map entry | [docs/registry/implementation-map.yaml:L15491-L15500] |
| 11 | external map entry | [docs/registry/implementation-map.yaml:L15501-L15510] |
| 12 | external map entry | [docs/registry/implementation-map.yaml:L15511-L15520] |
| 13 | external map entry | [docs/registry/implementation-map.yaml:L15521-L15530] |
| 14 | external map entry | [docs/registry/implementation-map.yaml:L15531-L15540] |
| 15 | external map entry | [docs/registry/implementation-map.yaml:L15541-L15550] |
| 16 | external map entry | [docs/registry/implementation-map.yaml:L15551-L15559] |
| 17 | external map entry | [docs/registry/implementation-map.yaml:L15560-L15568] |
| 18 | external map entry | [docs/registry/implementation-map.yaml:L15569-L15578] |
| 19 | external map entry | [docs/registry/implementation-map.yaml:L15579-L15587] |
| 20 | external map entry | [docs/registry/implementation-map.yaml:L15588-L15597] |
| 21 | external map entry | [docs/registry/implementation-map.yaml:L15598-L15607] |
| 22 | external map entry | [docs/registry/implementation-map.yaml:L15608-L15617] |
| 23 | external map entry | [docs/registry/implementation-map.yaml:L15618-L15629] |
| 24 | external map entry | [docs/registry/implementation-map.yaml:L15630-L15639] |
| 25 | external map entry | [docs/registry/implementation-map.yaml:L15640-L15649] |
| 26 | external map entry | [docs/registry/implementation-map.yaml:L15650-L15659] |
| 27 | external map entry | [docs/registry/implementation-map.yaml:L15660-L15669] |
| 28 | external map entry | [docs/registry/implementation-map.yaml:L15670-L15684] |
| 29 | external map entry | [docs/registry/implementation-map.yaml:L15685-L15693] |
| 30 | external map entry | [docs/registry/implementation-map.yaml:L15694-L15703] |
| 31 | external map entry | [docs/registry/implementation-map.yaml:L15704-L15713] |
| 32 | external map entry | [docs/registry/implementation-map.yaml:L15714-L15723] |
| 33 | external map entry | [docs/registry/implementation-map.yaml:L15724-L15733] |
| 34 | external map entry | [docs/registry/implementation-map.yaml:L15734-L15743] |
| 35 | external map entry | [docs/registry/implementation-map.yaml:L15744-L15753] |
| 36 | external map entry | [docs/registry/implementation-map.yaml:L15754-L15763] |
| 37 | external map entry | [docs/registry/implementation-map.yaml:L15764-L15773] |
| 38 | external map entry | [docs/registry/implementation-map.yaml:L15774-L15783] |
| 39 | external map entry | [docs/registry/implementation-map.yaml:L15784-L15793] |
| 40 | external map entry | [docs/registry/implementation-map.yaml:L15794-L15806] |
| 41 | external map entry | [docs/registry/implementation-map.yaml:L15807-L15816] |
| 42 | external map entry | [docs/registry/implementation-map.yaml:L15817-L15826] |
| 43 | external map entry | [docs/registry/implementation-map.yaml:L15827-L15836] |
| 44 | external map entry | [docs/registry/implementation-map.yaml:L15837-L15846] |
| 45 | external map entry | [docs/registry/implementation-map.yaml:L15847-L15856] |
| 46 | external map entry | [docs/registry/implementation-map.yaml:L15857-L15866] |
| 47 | external map entry | [docs/registry/implementation-map.yaml:L15867-L15876] |
| 48 | external map entry | [docs/registry/implementation-map.yaml:L15877-L15886] |
| 49 | package: `compute_splashzone_envelope` | [docs/registry/implementation-map.yaml:L15887-L15896] |
| 50 | external map entry | [docs/registry/implementation-map.yaml:L15897-L15906] |
| 51 | external map entry | [docs/registry/implementation-map.yaml:L15907-L15916] |
| 52 | external map entry | [docs/registry/implementation-map.yaml:L15917-L15926] |
| 53 | external map entry | [docs/registry/implementation-map.yaml:L15927-L15936] |
| 54 | external map entry | [docs/registry/implementation-map.yaml:L15937-L15946] |
| 55 | external map entry | [docs/registry/implementation-map.yaml:L15947-L15956] |
| 56 | external map entry | [docs/registry/implementation-map.yaml:L15957-L15965] |
| 57 | external map entry | [docs/registry/implementation-map.yaml:L15966-L15975] |
| 58 | external map entry | [docs/registry/implementation-map.yaml:L15976-L15987] |
| 59 | external map entry | [docs/registry/implementation-map.yaml:L15988-L15997] |
| 60 | external map entry | [docs/registry/implementation-map.yaml:L15998-L16009] |
| 61 | external map entry | [docs/registry/implementation-map.yaml:L16010-L16019] |
| 62 | external map entry | [docs/registry/implementation-map.yaml:L16020-L16029] |
| 63 | external map entry | [docs/registry/implementation-map.yaml:L16030-L16039] |
| 64 | external map entry | [docs/registry/implementation-map.yaml:L16040-L16049] |
| 65 | external map entry | [docs/registry/implementation-map.yaml:L16050-L16059] |
| 66 | external map entry | [docs/registry/implementation-map.yaml:L16060-L16074] |
| 67 | external map entry | [docs/registry/implementation-map.yaml:L16075-L16083] |
| 68 | external map entry | [docs/registry/implementation-map.yaml:L16084-L16093] |
| 69 | external map entry | [docs/registry/implementation-map.yaml:L16094-L16103] |
| 70 | external map entry | [docs/registry/implementation-map.yaml:L16104-L16113] |
| 71 | external map entry | [docs/registry/implementation-map.yaml:L16114-L16126] |
| 72 | external map entry | [docs/registry/implementation-map.yaml:L16127-L16136] |
| 73 | external map entry | [docs/registry/implementation-map.yaml:L16137-L16146] |
| 74 | external map entry | [docs/registry/implementation-map.yaml:L16147-L16156] |
| 75 | external map entry | [docs/registry/implementation-map.yaml:L16157-L16168] |
| 76 | external map entry | [docs/registry/implementation-map.yaml:L16169-L16178] |
| 77 | external map entry | [docs/registry/implementation-map.yaml:L16179-L16188] |
| 78 | external map entry | [docs/registry/implementation-map.yaml:L16189-L16198] |
| 79 | external map entry | [docs/registry/implementation-map.yaml:L16199-L16208] |
| 80 | external map entry | [docs/registry/implementation-map.yaml:L16209-L16218] |
| 81 | external map entry | [docs/registry/implementation-map.yaml:L16219-L16228] |
| 82 | external map entry | [docs/registry/implementation-map.yaml:L16229-L16238] |
| 83 | external map entry | [docs/registry/implementation-map.yaml:L16239-L16248] |
| 84 | external map entry | [docs/registry/implementation-map.yaml:L16249-L16258] |
| 85 | external map entry | [docs/registry/implementation-map.yaml:L16259-L16268] |
| 86 | external map entry | [docs/registry/implementation-map.yaml:L16269-L16280] |
| 87 | external map entry | [docs/registry/implementation-map.yaml:L16281-L16293] |
| 88 | external map entry | [docs/registry/implementation-map.yaml:L16294-L16304] |
| 89 | external map entry | [docs/registry/implementation-map.yaml:L16305-L16314] |
| 90 | external map entry | [docs/registry/implementation-map.yaml:L16315-L16324] |
| 91 | external map entry | [docs/registry/implementation-map.yaml:L16325-L16334] |
| 92 | external map entry | [docs/registry/implementation-map.yaml:L16335-L16344] |
| 93 | external map entry | [docs/registry/implementation-map.yaml:L16345-L16354] |
| 94 | external map entry | [docs/registry/implementation-map.yaml:L16355-L16364] |
| 95 | external map entry | [docs/registry/implementation-map.yaml:L16365-L16374] |
| 96 | external map entry | [docs/registry/implementation-map.yaml:L16375-L16384] |
| 97 | external map entry | [docs/registry/implementation-map.yaml:L16385-L16396] |
| 98 | external map entry | [docs/registry/implementation-map.yaml:L16397-L16408] |
| 99 | external map entry | [docs/registry/implementation-map.yaml:L16409-L16418] |
| 100 | external map entry | [docs/registry/implementation-map.yaml:L16419-L16428] |
| 101 | external map entry | [docs/registry/implementation-map.yaml:L16429-L16438] |
| 102 | external map entry | [docs/registry/implementation-map.yaml:L16439-L16448] |
| 103 | external map entry | [docs/registry/implementation-map.yaml:L16449-L16458] |
| 104 | external map entry | [docs/registry/implementation-map.yaml:L16459-L16468] |
| 105 | external map entry | [docs/registry/implementation-map.yaml:L16469-L16477] |
| 106 | external map entry | [docs/registry/implementation-map.yaml:L16478-L16487] |
| 107 | external map entry | [docs/registry/implementation-map.yaml:L16488-L16497] |
| 108 | external map entry | [docs/registry/implementation-map.yaml:L16498-L16507] |
| 109 | external map entry | [docs/registry/implementation-map.yaml:L16508-L16517] |
| 110 | external map entry | [docs/registry/implementation-map.yaml:L16518-L16528] |
| 111 | external map entry | [docs/registry/implementation-map.yaml:L16529-L16538] |
| 112 | external map entry | [docs/registry/implementation-map.yaml:L16539-L16549] |
| 113 | external map entry | [docs/registry/implementation-map.yaml:L16550-L16559] |
| 114 | external map entry | [docs/registry/implementation-map.yaml:L16560-L16569] |
| 115 | external map entry | [docs/registry/implementation-map.yaml:L16570-L16579] |
| 116 | external map entry | [docs/registry/implementation-map.yaml:L16580-L16589] |
| 117 | external map entry | [docs/registry/implementation-map.yaml:L16590-L16600] |
| 118 | external map entry | [docs/registry/implementation-map.yaml:L16601-L16611] |
| 119 | external map entry | [docs/registry/implementation-map.yaml:L16612-L16622] |
| 120 | external map entry | [docs/registry/implementation-map.yaml:L16623-L16633] |
| 121 | external map entry | [docs/registry/implementation-map.yaml:L16634-L16644] |
| 122 | external map entry | [docs/registry/implementation-map.yaml:L16645-L16655] |
| 123 | external map entry | [docs/registry/implementation-map.yaml:L16656-L16665] |
| 124 | external map entry | [docs/registry/implementation-map.yaml:L16666-L16675] |
| 125 | external map entry | [docs/registry/implementation-map.yaml:L16676-L16685] |
| 126 | external map entry | [docs/registry/implementation-map.yaml:L16686-L16695] |
| 127 | external map entry | [docs/registry/implementation-map.yaml:L16696-L16705] |
| 128 | external map entry | [docs/registry/implementation-map.yaml:L16706-L16715] |
| 129 | external map entry | [docs/registry/implementation-map.yaml:L16716-L16725] |
| 130 | external map entry | [docs/registry/implementation-map.yaml:L16726-L16735] |
| 131 | external map entry | [docs/registry/implementation-map.yaml:L16736-L16747] |
| 132 | external map entry | [docs/registry/implementation-map.yaml:L16748-L16758] |
| 133 | external map entry | [docs/registry/implementation-map.yaml:L16759-L16769] |
| 134 | external map entry | [docs/registry/implementation-map.yaml:L16770-L16781] |
| 135 | external map entry | [docs/registry/implementation-map.yaml:L16782-L16791] |

### Exact search and classification

Reproduction: load the two families' `search_signals` from the named map revision, apply each pattern using case-sensitive Python `re.search` to every package `.py` file at the code pin, omit a pattern only if it matches `installation/installation_pamphlet` case-insensitively, and attribute a hit to the smallest enclosing AST function/class, or the module definition. No signal was omitted; escaped patterns were used exactly as stored, without correcting their spelling. [docs/registry/implementation-map.yaml:L1-L4] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L1-L896]

Exact stored signals:

```json
[
  {
    "family": "rao-readers-and-orcaflex-converters",
    "search_signals": [
      {
        "search": "algorithm",
        "pattern": "\\.lis\\b|\\.hyd\\b|\\.owr\\b|read_rao|parse_rao|read_hyd|read_wamit|read_nemoh|parse_lis|rao_data|rao_table|RAOAmplitude|RAOPhase|DisplacementRAOs|read_csv|read_excel|loadtxt|open_dataset|netCDF4|read_hydrodynamic|parse_aqwa|parse_wamit|parse_nemoh|convert_rao|RAOInterpolator|self\\.content|_table_span|parse_aqwa_lis_file|AQWALISParser"
      },
      {
        "search": "vocabulary",
        "pattern": "\\bRAO\\b|\\braos?\\b|AQWA|WAMIT|NEMOH|HydroD|BEMRosetta|Capytaine"
      },
      {
        "search": "constants_or_keys",
        "pattern": "ADDED MASS|RADIATION DAMPING|displacement_rao|displacementRAO|DisplacementRAOs|RAOPeriodOrFrequency|RAOAmplitude|RAOPhase|RAODirection"
      }
    ]
  },
  {
    "family": "wind-and-current-force-routes",
    "search_signals": [
      {
        "search": "algorithm",
        "pattern": "(?:0\\.5|1\\s*/\\s*2).*(?:rho|density).*(?:cd|drag|area)|(?:wind|current|drag|lift)_forces?|calculate_(?:wind|current)|OCIMF|current_profile|wind_height_coefficient"
      },
      {
        "search": "vocabulary",
        "pattern": "OCIMF|wind.load|current.load|wind.force|current.force|drag.coefficient|current.profile"
      },
      {
        "search": "constants_or_keys",
        "pattern": "Cxw|Cyw|CXw|CYw|CXc|CYc|drag_coefficient|lift_coefficient|wind_speed|current_velocity"
      }
    ]
  }
]
```

| Family | Definition hit | Classification and evidence |
| --- | --- | --- |
| `rao-readers-and-orcaflex-converters` | `workflow.py` / `router` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/workflow.py:L29-L113] |
| `rao-readers-and-orcaflex-converters` | `workflow.py` / `scan_completed_runs` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/workflow.py:L199-L250] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `<module>` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/calculations.py:L9-L613] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `_effective_provenance` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/calculations.py:L73-L78] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `_extract_rao_dict` | Listed map implementation and section-3 method [src/digitalmodel/installation/installation_pamphlet/calculations.py:L156-L189] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `load_rao_set_from_artifact` | Listed map implementation and section-3 method [src/digitalmodel/installation/installation_pamphlet/calculations.py:L192-L223] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `resolve_rao_artifact` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/calculations.py:L232-L256] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `_build_rao_set` | Proxy assembly/conversion already listed in section 3; delegates representative RAOs [src/digitalmodel/installation/installation_pamphlet/calculations.py:L298-L369] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `compute_envelopes` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/calculations.py:L372-L401] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `compute_envelope_band` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/calculations.py:L404-L459] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `select_rao_basis` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/calculations.py:L625-L648] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `build_provenance` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/calculations.py:L675-L792] |
| `rao-readers-and-orcaflex-converters` | `calculations.py` / `assemble_result` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/calculations.py:L796-L896] |
| `rao-readers-and-orcaflex-converters` | `render.py` / `<module>` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/render.py:L105-L105] |
| `rao-readers-and-orcaflex-converters` | `render.py` / `_basis_h` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/render.py:L292-L298] |
| `rao-readers-and-orcaflex-converters` | `render.py` / `progress_panel` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/render.py:L300-L325] |
| `rao-readers-and-orcaflex-converters` | `render.py` / `render_pamphlet_html` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/render.py:L147-L704] |
| `rao-readers-and-orcaflex-converters` | `render.py` / `band_block` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/render.py:L465-L499] |
| `wind-and-current-force-routes` | `calculations.py` / `compute_splashzone_envelope` | Listed map implementation and section-3 method [src/digitalmodel/installation/installation_pamphlet/calculations.py:L463-L563] |
| `wind-and-current-force-routes` | `render.py` / `<module>` | Helper, data, orchestration or docstring hit; not an independent family calculation [src/digitalmodel/installation/installation_pamphlet/render.py:L78-L82] |

Counted-family result: 0 further independent implementations; the splash-zone force criterion is already mapped and documented in section 3, and proxy RAO assembly is also documented there. Helpers, payloads and docstrings are not further implementations. [src/digitalmodel/installation/installation_pamphlet/calculations.py:L156-L223] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L298-L401] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L463-L563]

Repository-wide search, including `scripts/`, used `rg -n 'def (compute_splashzone|calc_phase_3_splash|operation_envelope|assess_lifts|compute_operability)|F_slam|k_qs|snap_daf' src tests scripts --glob '*.py'`; related splash implementations outside this package are map follow-up findings, not counted-family misses within it. [src/digitalmodel/installation/jumper_installation/calculations.py:L163-L220] [src/digitalmodel/marine_ops/installation/splash_zone.py:L18-L40] [src/digitalmodel/orcaflex/installation_analysis.py:L410-L459]

Map follow-up: crane suitability, operation-envelope criteria, damping-band composition, weather-window operability, run-provenance selection and splash-specific lowering criteria need explicit method-family reconciliation where the current two families do not represent them; issue pending (7). [src/digitalmodel/installation/installation_pamphlet/calculations.py:L260-L294] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L372-L459] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L463-L610] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L625-L792]

## 10. Coverage and open items

### Coverage inventory

| Material read at the code pin | Extent / interpretation | Evidence |
| --- | --- | --- |
| Package Python files | `__init__.py`, `calculations.py`, `workflow.py`, `render.py`, `risk_reference.py`; complete source, including browser script and static data | [src/digitalmodel/installation/installation_pamphlet/__init__.py:L1-L5] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L1-L896] [src/digitalmodel/installation/installation_pamphlet/workflow.py:L1-L321] [src/digitalmodel/installation/installation_pamphlet/render.py:L1-L704] [src/digitalmodel/installation/installation_pamphlet/risk_reference.py:L1-L10] |
| Configuration | Base configuration read as text; identifiers and catalog-derived values omitted here | [src/digitalmodel/base_configs/modules/installation_pamphlet/installation_pamphlet.yml:L1-L53] |
| Tests | Complete `tests/installation/test_installation_pamphlet.py` and `tests/installation/test_pamphlet_confidentiality.py`; 39 ordinary test functions in the former, 4 functions / 6 cases in the latter | [tests/installation/test_installation_pamphlet.py:L1-L911] [tests/installation/test_pamphlet_confidentiality.py:L1-L69] |
| Routing registry | Installation domain and existing sibling pack entry | [docs/registry/module-routing.yaml:L505-L514] |
| Discovery under `docs/` | `docs/capability-map/capabilities-sections.yml`; package capability link | [docs/capability-map/capabilities-sections.yml:L144-L153] |
| Discovery under `docs/` | `docs/plans/2026-06-29-cfd-capability-assessment.md`; cross-domain provenance precedent, not evidence for current validation | [docs/plans/2026-06-29-cfd-capability-assessment.md:L16-L22] [docs/plans/2026-06-29-cfd-capability-assessment.md:L82-L92] |
| Discovery under `docs/` | `docs/plans/2026-08-04-issue-1965-published-provenance-paths.md`; disclosure-detection discussion | [docs/plans/2026-08-04-issue-1965-published-provenance-paths.md:L23-L34] |
| Discovery under `docs/` | `docs/registry/implementation-map.yaml` and `docs/registry/implementation-map.md`; relevant families and package rows read | [docs/registry/implementation-map.yaml:L11863-L11888] [docs/registry/implementation-map.md:L20-L23] |

Discovery commands: `rg --files src/digitalmodel/installation/installation_pamphlet`; `rg -n 'installation_pamphlet|installation-pamphlet' tests docs`; `rg -l -i 'installation.?pamphlet' docs`; `git ls-files '*installation*pamphlet*' '*pamphlet*'` (coverage record).

All 45 collected module nodes were run, including the three empty-alias parameter cases; section 4 lists six representative regressions, while C02, C03, C05, C06 and C11 identify additional gate evidence; the entire node/result list is retained with the execution record. [tests/installation/test_installation_pamphlet.py:L1-L911] [tests/installation/test_pamphlet_confidentiality.py:L1-L69]

### Open items and unverified claims

Each item below is assigned to the maintainer for issue registration and to the relevant family quality role for technical disposition; `issue pending (N)` refers to draft N in the accompanying issue-draft list.

| Item | Evidence / unresolved boundary | Tracking |
| --- | --- | --- |
| Queue omission diagnostics | Missing directory, malformed/failed records, missing audit fields and null run IDs can remove evidence without a diagnostic count; section-6 queue rows | issue pending (1) [src/digitalmodel/installation/installation_pamphlet/workflow.py:L189-L250] |
| Input contracts and unit/coordinate assumptions | UNVERIFIED claim in section 2; invalid/localized numeric text, period collisions, degenerate quadratic, omitted splash rows and empty inputs require checks | issue pending (2) [src/digitalmodel/installation/installation_pamphlet/calculations.py:L511-L601] [src/digitalmodel/installation/installation_pamphlet/workflow.py:L29-L41] |
| Renderer and browser integrity | PASS display, fixed narratives/controls, demo fallback, numeric-share parsing and path disclosure need negative tests; section-6 renderer rows | issue pending (3) [src/digitalmodel/installation/installation_pamphlet/render.py:L258-L268] [src/digitalmodel/installation/installation_pamphlet/render.py:L612-L652] [src/digitalmodel/installation/installation_pamphlet/workflow.py:L302-L307] |
| Provenance validity | Fixed titles/notes and task statuses are not independent accuracy evidence; artifact identity and operability source wording remain open | issue pending (4) [src/digitalmodel/installation/installation_pamphlet/calculations.py:L674-L792] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L838-L891] |
| Five standards, constants and risk sources | Six UNVERIFIED claims in section 3: five standards' unchecked editions/clauses and absent checked numeric-source/Citation sidecars; no independently validated worked example | issue pending (5) [src/digitalmodel/installation/installation_pamphlet/calculations.py:L116-L123] [src/digitalmodel/installation/installation_pamphlet/risk_reference.py:L1-L10] |
| Licensed evidence / dependency metadata | UNVERIFIED claim in section 9: versions, real-artifact fingerprints and upstream inventory rows; G1–G4 and signed G5 records absent; stage/slings comparison not performed | issue pending (6) [src/digitalmodel/installation/installation_pamphlet/calculations.py:L508-L509] [src/digitalmodel/installation/installation_pamphlet/calculations.py:L877-L896] |
| Real artifact deployment | Synthetic fixture success does not resolve actual vessel-artifact availability | existing #1538 [tests/installation/test_installation_pamphlet.py:L454-L457] [tests/installation/test_installation_pamphlet.py:L573-L642] |
| Method-map follow-up | Reconcile section-3 families and related repository implementations; preserve all counted-family rows at the named data revision | issue pending (7) [src/digitalmodel/installation/installation_pamphlet/calculations.py:L260-L610] |
| Appointments, log registration and public wording | UNVERIFIED claim in section 8; historical provenance strings in source/config/tests require codenaming; sidecar entry: to be created if retained private-derived lessons need one; no private repository read | issue pending (8) [src/digitalmodel/installation/installation_pamphlet/calculations.py:L45-L52] [src/digitalmodel/base_configs/modules/installation_pamphlet/installation_pamphlet.yml:L39-L44] [tests/installation/test_installation_pamphlet.py:L30-L34] |

No standard text or tabulated standard values are reproduced; verification of the licensed clauses, real lowering models, real diffraction results, browser behavior and independent numeric benchmarks remains outside the evidence of this draft.
