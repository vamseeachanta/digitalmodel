# Structural entry knowledge pack

| Header | Value |
| --- | --- |
| Module | structural (top-level) |
| Repository | digitalmodel |
| Path | `src/digitalmodel/structural/` — direct `.py` files only |
| Pack revision | 0 |
| Code revision | `334521c97a21c16950046f52b61586e8e8465787` |
| Analysis family | Engineering calculations (`EC`); AQG r0, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |
| Implementation-map revision | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Implementation-map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

Code citations resolve at the code revision; citations to `docs/registry/implementation-map.yaml` resolve at the separate map revision above.

## 1. Purpose and scope

The spectral entry computes discrete spectral moments, zero-crossing period, bandwidth and narrow-band fatigue damage; plate and stiffened-panel entries build sweeps and design grids around separate analyzers. [src/digitalmodel/structural/spectral_fatigue.py:L29-L67] [src/digitalmodel/structural/spectral_fatigue.py:L70-L178] [src/digitalmodel/structural/buckling_parametric.py:L101-L156] [src/digitalmodel/structural/panel_buckling_parametric.py:L128-L184] [src/digitalmodel/structural/panel_design_explorer.py:L77-L144]

The coordinator dispatches wall-thickness, fatigue and campaign studies, combines their results and builds HTML; the quickcheck entry loads example code and exports cached results. [src/digitalmodel/structural/parametric_coordinator.py:L220-L345] [src/digitalmodel/structural/parametric_report.py:L46-L113] [src/digitalmodel/structural/wall_thickness_quickcheck.py:L16-L31] [src/digitalmodel/structural/wall_thickness_quickcheck.py:L69-L100]

This pack covers the direct Python files; sub-packages are dependency boundaries in section 9, with their algorithms outside this pack's scope. The routing registry labels the structural domain `stable`; that is registry metadata rather than a demonstrated standard-conformance result. [src/digitalmodel/structural/buckling_parametric.py:L27-L31] [src/digitalmodel/structural/parametric_coordinator.py:L23-L36] [docs/registry/module-routing.yaml:L400-L417]

The spectral damage expression assumes Gaussian narrow-band stress ranges and uses one supplied S-N slope; the panel sweep fixes transverse normal stress to zero. [src/digitalmodel/structural/spectral_fatigue.py:L129-L149] [src/digitalmodel/structural/panel_buckling_parametric.py:L149-L154]

## 2. Inputs and outputs

Inputs:

| Interface | Inputs and units | Range/default and source |
| --- | --- | --- |
| `spectral_moment` | Ascending frequency in Hz, matching spectral density, moment order `n` | Empty, unequal and fewer-than-two arrays raise; `n=0` is the default; ordering, finite values and nonnegative density are not checked by this function. [src/digitalmodel/structural/spectral_fatigue.py:L29-L67] |
| `zero_crossing_period`; `bandwidth_parameter` | Moments `m0`, `m2`, `m4` in the same cyclic-frequency convention | Period rejects nonpositive `m0` or `m2`; bandwidth divides without equivalent guards and clamps a negative radicand. [src/digitalmodel/structural/spectral_fatigue.py:L70-L119] |
| `narrow_band_damage` | `m0` in MPa²; period and duration in seconds; logarithmic intercept and slope dimensionless | No argument defaults; nonpositive variance returns a zero result; the positive-variance branch divides by period without a positive-period guard. [src/digitalmodel/structural/spectral_fatigue.py:L122-L178] |
| `BucklingSweepConfig` | Thickness, breadth and length in mm; axial, transverse and shear stresses in MPa; grade keys | Required lists; factor default is encoded in the dataclass, with the default study arrays kept in code rather than reproduced here. [src/digitalmodel/structural/buckling_parametric.py:L38-L89] |
| `PanelSweepConfig` | Plate thickness, spacing, tee dimensions and span in mm; axial stress and shear in MPa | Required lists; default span, factor, grades and profiles are encoded in the configuration. [src/digitalmodel/structural/panel_buckling_parametric.py:L43-L103] |
| `ExplorerConfig` | Thickness/spacing/span in mm, load and shear in MPa | Axes, increments and selectable dimensions default in code; the grid uses rounded interval counts. [src/digitalmodel/structural/panel_design_explorer.py:L48-L68] |
| `WallThicknessParams` | Thickness, diameter and corrosion allowance in m; pressure in Pa; moment in N·m; effective tension in N | Lists are passed through to `SweepConfig`; moment, tension and corrosion allowance default to zero; five required lists are checked for emptiness. [src/digitalmodel/structural/parametric_coordinator.py:L77-L104] [src/digitalmodel/structural/parametric_coordinator.py:L255-L268] [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L43-L57] |
| `FatigueParams` | Histogram range in MPa, count in cycles; thickness in mm; SCF and DFF dimensionless | Curves and DataFrame type are checked; correction reference thickness defaults in the dataclass; other axis lists lack equivalent emptiness checks. [src/digitalmodel/structural/parametric_coordinator.py:L107-L125] [src/digitalmodel/structural/fatigue/parametric_sweep.py:L74-L103] |
| `OrcaFlexParams` | Water-depth list; optional route-length and tension lists | Only nonempty water depths are checked here; the campaign route is availability-gated. No solver was run for this pack. [src/digitalmodel/structural/parametric_coordinator.py:L128-L142] [src/digitalmodel/structural/parametric_coordinator.py:L295-L324] |
| `WallThicknessQuickCheck.router` | Config-relative or absolute cache, HTML, JSON and CSV paths | Missing output/cache keys raise; absent config directory falls back to current directory; cache processing is delegated to the example module. [src/digitalmodel/structural/wall_thickness_quickcheck.py:L34-L38] [src/digitalmodel/structural/wall_thickness_quickcheck.py:L72-L89] |

Outputs:

| Interface | Output and units | Encoding/limits |
| --- | --- | --- |
| Spectral functions | Moment integral; period in s; bandwidth dimensionless; damage dimensionless, cycles count and stress range in MPa | `FatigueDamageResult.standard` is a string label; it does not establish an edition or emit a citation sidecar. [src/digitalmodel/structural/spectral_fatigue.py:L19-L26] [src/digitalmodel/structural/spectral_fatigue.py:L60-L67] [src/digitalmodel/structural/spectral_fatigue.py:L156-L178] |
| Plate sweep | Row dictionaries; utilization dimensionless, critical stress MPa; `cases.csv` and UTF-8 `results.json` | Results are rounded; nonfinite safety factor becomes `None`; the index keys include grade, geometry and load. [src/digitalmodel/structural/buckling_parametric.py:L134-L153] [src/digitalmodel/structural/buckling_parametric.py:L217-L284] |
| Panel sweep | Rows and component utilizations; `panel_cases.csv`, UTF-8 `panel_results.json` | Index omits span, factor and profile dimensions; metadata states `validated` independently of any runtime validation check. [src/digitalmodel/structural/panel_buckling_parametric.py:L159-L181] [src/digitalmodel/structural/panel_buckling_parametric.py:L263-L329] |
| Explorer | Utilization/mode matrices and allowable sampled load in MPa | Row index is load and column index is thickness; mode codes are 0/1/2; an unknown mode defaults to 0; allowable is the last passing sampled load, possibly `None`. [src/digitalmodel/structural/panel_design_explorer.py:L43-L45] [src/digitalmodel/structural/panel_design_explorer.py:L77-L144] |
| Coordinator | `StudyResult`: DataFrame, summary and HTML | Combined studies concatenate domains; summaries can default missing status columns to zero; HTML renders only available selected columns. [src/digitalmodel/structural/parametric_coordinator.py:L184-L204] [src/digitalmodel/structural/parametric_coordinator.py:L326-L373] [src/digitalmodel/structural/parametric_report.py:L46-L61] |
| Quickcheck adapter | HTML report; sorted-key UTF-8 JSON; UTF-8 selection CSV; updated config | CSV fields include selected wall mm/in and utilization; the adapter expects selection rows and arrestor sizing in the cached payload. [src/digitalmodel/structural/wall_thickness_quickcheck.py:L41-L66] [src/digitalmodel/structural/wall_thickness_quickcheck.py:L80-L100] |

UNVERIFIED: Global coordinate frame, origin, load signs and phase conventions are not established for the coordinator's campaign inputs; define these before treating those lists as a design basis. Issue pending (2).

## 3. Method and governing standards

| Method | Implementation and basis | Standards disposition |
| --- | --- | --- |
| Discrete moment | `spectral_moment`: trapezoidal sum of `f**n * S(f)` over Hz intervals. [src/digitalmodel/structural/spectral_fatigue.py:L29-L67] | Numerical integration, without a named standard clause in the implementation. [src/digitalmodel/structural/spectral_fatigue.py:L29-L52] |
| Period and bandwidth | `sqrt(m0/m2)` and `sqrt(max(0, 1-m2**2/(m0*m4)))`; use Hz moments consistently. [src/digitalmodel/structural/spectral_fatigue.py:L70-L119] | Independent algebraic cases appear in section 4; angular moments must not be substituted without conversion. [tests/test_spectral_fatigue.py:L57-L85] [src/digitalmodel/structural/spectral_fatigue.py:L32-L39] |
| Narrow-band damage | `N=duration/Tz`, `a=10**sn_log_a`; Rayleigh moment from the gamma function multiplied by `N/a`. [src/digitalmodel/structural/spectral_fatigue.py:L129-L172] | DNV-RP-C203, *Fatigue Design of Offshore Steel Structures*, edition not specified locally, Section 4.3 as named by the docstring; clause text not checked against the licensed copy. [src/digitalmodel/structural/spectral_fatigue.py:L129-L149] [src/digitalmodel/structural/fatigue_apps/README.md:L275-L279] |
| Plate sweep and slenderness | `run_sweep` delegates capacity to `PlateBucklingAnalyzer`; `slenderness_beta=(width/thickness)*sqrt(fy/E)` is calculated locally. [src/digitalmodel/structural/buckling_parametric.py:L92-L156] | DNV-RP-C201, *Buckling Strength of Plated Structures*, 2010 in the code register; the top-level wrapper does not identify a clause. Clause text not checked against the licensed copy. [src/digitalmodel/codes.py:L62-L63] [src/digitalmodel/structural/buckling_parametric.py:L1-L13] |
| Panel sweep and design grid | Sweep and grid delegate capacity to `StiffenedPanelBucklingAnalyzer`; grid selects the last sampled passing load. [src/digitalmodel/structural/panel_buckling_parametric.py:L139-L154] [src/digitalmodel/structural/panel_design_explorer.py:L95-L124] | DNV-RP-C201, 2010 register basis, Section 7.5.2 as named by the wrapper for tripping; clause text not checked against the licensed copy. [src/digitalmodel/codes.py:L62-L63] [src/digitalmodel/structural/panel_buckling_parametric.py:L7-L16] |
| Wall-thickness and fatigue coordination | `_run_wall_thickness` and `_run_fatigue` construct/delegate parameter sets; they are not additional capacity formulas. [src/digitalmodel/structural/parametric_coordinator.py:L255-L293] | Wall-thickness defaults to DNV-ST-F101; the referenced analyzer names 2021 and API RP 1111 (2015); clause text not checked against the licensed copy. [src/digitalmodel/structural/parametric_coordinator.py:L84-L92] [src/digitalmodel/structural/analysis/wall_thickness.py:L1-L16] |
| Quickcheck | Dynamic example loading, cached calculation selection and serialization, rather than a local wall-capacity algorithm. [src/digitalmodel/structural/wall_thickness_quickcheck.py:L16-L31] [src/digitalmodel/structural/wall_thickness_quickcheck.py:L72-L100] | Workflow registry calls this a DNV/API cached report; the example README names DNV-ST-F101 (2021) and API RP 1111. Clause text not checked against the licensed copy. [docs/registry/workflows.yaml:L728-L740] [examples/structural/wall_thickness_quickcheck/README.md:L16-L33] |

The remaining inventory leads belong to sub-packages: API STD 2RD (2013) is selected by the wall-thickness analyzer; Eurocode 3 / EN 1993-1-1 (2005) belongs to column checks; API RP 2A and AWS D1.1 are listed by the fatigue curve library, with no edition or clause established by those lists. These are dependency references, not additional top-level methods. [src/digitalmodel/structural/analysis/wall_thickness.py:L35-L54] [docs/domains/codes-register.md:L24-L36] [src/digitalmodel/structural/structural_analysis/buckling.py:L144-L156] [src/digitalmodel/structural/fatigue/sn_curves.py:L314-L323]

The review document calls panel calculations preliminary and not yet validated, while the current wrapper and explorer set `validated=True`; the spectral test also labels its supplied intercept as seawater with cathodic protection while the curve-library tests distinguish environments. These are unresolved provenance/wording differences, not evidence of independent validation. Issue pending (2). [docs/domains/plate-buckling/buckling-code-review-2026-06-26.md:L43-L46] [src/digitalmodel/structural/panel_buckling_parametric.py:L306-L317] [src/digitalmodel/structural/panel_design_explorer.py:L127-L135] [tests/test_spectral_fatigue.py:L93-L104] [tests/test_spectral_fatigue_validation.py:L52-L55]

UNVERIFIED: Exact licensed editions, clause conformance and calculation-time Citation sidecars for the direct spectral formulas and sweep metadata are not established; the local spectral result carries only a standard string. Issue pending (2). [src/digitalmodel/structural/spectral_fatigue.py:L19-L26] [src/digitalmodel/structural/buckling_parametric.py:L262-L282]

## 4. Validated worked examples

Execution record: the following existing nodes passed offline against the code pin on 2026-10-18; documentation changes do not alter their calculation code. The first three use closed-form mathematics; the remaining six are regression examples and do not establish licensed-standard conformance.

| ID / kind | Source, input and expected result | Tolerance and basis | Existing test node / result |
| --- | --- | --- | --- |
| V1 / validated | Flat unit-density spectrum from 0.05 to 0.25 Hz; exact rectangular area `m0=0.20` in variance units. [tests/test_spectral_fatigue.py:L8-L16] | Absolute error <0.01 variance units, as asserted by the existing test. [tests/test_spectral_fatigue.py:L15-L16] | `tests/test_spectral_fatigue.py::TestSpectralMoments::test_m0_from_spectrum` — PASS. [tests/test_spectral_fatigue.py:L8-L16] |
| V2 / validated | Closed-form `sqrt(1/0.04)=5.0 s` from prescribed moments. [tests/test_spectral_fatigue.py:L58-L63] | Absolute error <0.01 s, existing assertion. [tests/test_spectral_fatigue.py:L62-L63] | `tests/test_spectral_fatigue.py::TestSpectralParameters::test_zero_crossing_period` — PASS. [tests/test_spectral_fatigue.py:L58-L63] |
| V3 / validated | Closed-form `sqrt(1-0.5**2/0.3)` bandwidth from prescribed moments, dimensionless. [tests/test_spectral_fatigue.py:L65-L71] | Absolute error <1e-6 dimensionless, existing assertion. [tests/test_spectral_fatigue.py:L69-L71] | `tests/test_spectral_fatigue.py::TestSpectralParameters::test_bandwidth_parameter` — PASS. [tests/test_spectral_fatigue.py:L65-L71] |
| R1 / regression | Default plate sweep returns 972 cases and its configured grade set; this is a product/count check, not an independent capacity reference. [tests/structural/test_buckling_parametric.py:L139-L142] | Exactly 972 rows; zero extra/missing grade keys. [tests/structural/test_buckling_parametric.py:L139-L142] | `tests/structural/test_buckling_parametric.py::test_default_sweep_case_count` — PASS. [tests/structural/test_buckling_parametric.py:L139-L142] |
| R2 / regression | Default panel sweep returns the configuration's Cartesian product: 756 rows. [tests/structural/test_panel_buckling_parametric.py:L47-L51] | Zero count difference, rows. [tests/structural/test_panel_buckling_parametric.py:L47-L51] | `tests/structural/test_panel_buckling_parametric.py::test_n_cases_helper_matches_default` — PASS. [tests/structural/test_panel_buckling_parametric.py:L47-L51] |
| R3 / regression | Explorer sampled boundary: the selected allowable load passes and the next available sampled load fails. [tests/structural/test_panel_design_explorer.py:L76-L86] | Selected utilization ≤1+1e-9 dimensionless; next sample >1 where present. [tests/structural/test_panel_design_explorer.py:L76-L86] | `tests/structural/test_panel_design_explorer.py::test_allowable_boundary_consistent` — PASS. [tests/structural/test_panel_design_explorer.py:L76-L86] |
| R4 / regression | Combined coordinator includes both domain summaries and combination counts. [tests/test_parametric_coordinator.py:L336-L344] | Zero missing required summary keys, keys; independent engineering values are not asserted by this node. [tests/test_parametric_coordinator.py:L336-L344] | `tests/test_parametric_coordinator.py::TestCoordinatorCombined::test_combined_summary_has_both_domain_keys` — PASS. [tests/test_parametric_coordinator.py:L336-L344] |
| R5 / regression | Quickcheck fresh computation equals committed cache for selection, sweep and arrestor sizing; no standard-wall values are reproduced here. [tests/test_wall_thickness_quickcheck_example.py:L169-L177] | Exact equality of three structures; zero differing fields, fields. [tests/test_wall_thickness_quickcheck_example.py:L169-L177] | `tests/test_wall_thickness_quickcheck_example.py::test_cache_matches_fresh_compute` — PASS. [tests/test_wall_thickness_quickcheck_example.py:L169-L177] |
| R6 / regression | Buckling workflow reproduces stored output hashes and reports reproducibility. [tests/workflow_api/test_run_workflow_buckling.py:L27-L39] | Zero digest differences, files; this golden originates from the implementation. [tests/workflow_api/test_run_workflow_buckling.py:L27-L39] | `tests/workflow_api/test_run_workflow_buckling.py::test_buckling_reference_golden_bare_in_repo_id` — PASS. [tests/workflow_api/test_run_workflow_buckling.py:L27-L39] |

UNVERIFIED: Independent narrow-band damage and full panel-capacity worked examples have not been established for the direct entry functions; existing positivity, monotonicity and golden tests are regression evidence. Issue pending (2). [tests/test_spectral_fatigue.py:L88-L149] [tests/structural/test_panel_buckling_parametric.py:L54-L88]

## 5. QA gates

AQG r0, section 10, Python calculations assigns G0 inputs/units/citations, G1 imports/tests and G2 hand/published checks; G3 and G4 are not applicable to this calculation scope; G5 applies to every family. This is a proposed module checklist, not a quality-clearance signature.

| Gate | Applicability | Checks / scope |
| --- | --- | --- |
| G0 | Applicable | C0a–C0d: input rejection and output schemas; section 2 records units and section 3 records citation gaps. |
| G1 | Applicable | C1a–C1c: direct imports/tests, routing contracts, absent-solver error path; no vendor load check is claimed. |
| G2 | Applicable | C2a–C2c: independent elementary closed forms; damage/panel independent anchors remain open in section 10. |
| G3 | NOT_APPLICABLE | No dynamic solver result is produced by the covered calculation checks. Campaign handling only constructs a combination table. [src/digitalmodel/structural/parametric_coordinator.py:L295-L324] |
| G4 | NOT_APPLICABLE | No diffraction mesh/hydrostatic solution is produced by these entries. [src/digitalmodel/structural/spectral_fatigue.py:L122-L178] [src/digitalmodel/structural/buckling_parametric.py:L127-L156] |
| G5 | Applicable | C5a–C5d: serialized results, cache regression, reproducible exports and traceable evidence. |

Run each named node with `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <node> -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<run>`. The declared test extra was used; no configuration override or deselection was used. The two registry workflow nodes write outputs and were run in a temporary copy with unchanged test bodies and input files.

| Check | Module quantity / executable evidence | Numeric pass criterion and unit | Gate |
| --- | --- | --- | --- |
| C0a | `spectral_moment` empty arrays; `tests/test_spectral_fatigue.py::TestSpectralMoments::test_empty_spectrum_raises`. [tests/test_spectral_fatigue.py:L49-L54] | Exactly 1 `ValueError`, exceptions. | G0 |
| C0b | `zero_crossing_period` zero second moment; `tests/test_spectral_fatigue.py::TestSpectralParameters::test_zero_crossing_period_zero_m2_raises`. [tests/test_spectral_fatigue.py:L80-L85] | Exactly 1 `ValueError`, exceptions. | G0 |
| C0c | Plate `run_sweep` required output keys; `tests/structural/test_buckling_parametric.py::test_row_schema_complete`. [tests/structural/test_buckling_parametric.py:L39-L48] | 0 missing schema keys, keys. | G0 |
| C0d | Panel `run_sweep` governing modes; `tests/structural/test_panel_buckling_parametric.py::test_governing_mode_always_valid`. [tests/structural/test_panel_buckling_parametric.py:L63-L65] | 0 unknown modes, modes. | G0 |
| C1a | Direct spectral, coordinator, plate, panel and explorer test files listed in section 10. [tests/test_spectral_fatigue.py:L7-L149] [tests/test_parametric_coordinator.py:L64-L409] [tests/structural/test_buckling_parametric.py:L33-L142] [tests/structural/test_panel_buckling_parametric.py:L41-L131] [tests/structural/test_panel_design_explorer.py:L31-L124] | 91 passed, 0 failed, 0 skipped, tests. | G1 |
| C1b | `tests/docs/test_digitalmodel_routing_contract.py` and `tests/contracts/`; registry and engineering contracts. [tests/docs/test_digitalmodel_routing_contract.py:L36-L49] [tests/contracts/test_runtime_dependencies.py:L1-L20] | 497 passed, 0 failed, tests. | G1 |
| C1c | Coordinator unavailable-solver error; `tests/test_parametric_coordinator.py::TestCoordinatorOrcaFlex::test_stub_error_message_mentions_orcaflex`. [tests/test_parametric_coordinator.py:L386-L397] | Exactly 1 unavailable-interface exception, exceptions; no solver launch. | G1 |
| C2a | `spectral_moment` rectangular area; V1. [tests/test_spectral_fatigue.py:L8-L16] | Absolute error <0.01 variance units. | G2 |
| C2b | `zero_crossing_period`; V2. [tests/test_spectral_fatigue.py:L58-L63] | Absolute error <0.01 s. | G2 |
| C2c | `bandwidth_parameter`; V3. [tests/test_spectral_fatigue.py:L65-L71] | Absolute error <1e-6 dimensionless. | G2 |
| C5a | Plate/panel `write_outputs` index round trips; `tests/structural/test_buckling_parametric.py::test_index_round_trip` and `tests/structural/test_panel_buckling_parametric.py::test_index_round_trip`. [tests/structural/test_buckling_parametric.py:L126-L136] [tests/structural/test_panel_buckling_parametric.py:L91-L105] | 0 mismatching exported index values, values, within existing `pytest.approx` where used; no assertion covers colliding keys. | G5 |
| C5b | Quickcheck `run_compute` versus `run_from_cache`; R5. [tests/test_wall_thickness_quickcheck_example.py:L169-L177] | 0 differing fields across 3 compared structures, fields. | G5 |
| C5c | `BucklingParametricWorkflow.router` via workflow API; R6. [tests/workflow_api/test_run_workflow_buckling.py:L27-L39] | 0 differing output digests, files. | G5 |
| C5d | Quickcheck and buckling registered output routes; `tests/workflows/test_durable_workflows.py::test_workflow_registry[wall-thickness-quickcheck]` and `tests/workflows/test_durable_workflows.py::test_workflow_registry[buckling-parametric]`. [tests/workflows/test_durable_workflows.py:L56-L93] [docs/registry/workflows.yaml:L728-L740] [docs/registry/workflows.yaml:L1249-L1260] | 0 absent registered outputs and 0 basename mismatches, files/keys. | G5 |

## 6. Known failure modes and pitfalls

Silent-failure sweep command, restricted to the direct-package scope:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|float\(|int\(|unit|cache|default|\.get\(' src/digitalmodel/structural --glob '*.py' --maxdepth 1
```

The complete direct source was also read for multiline returns, implicit empty outputs and defaults not matched by the expression; section 6 rows include benign design defaults as well as failure risks. No direct-package `continue`, error-path `return []`/`return {}`, epoch fallback or empty-result cache write was found; ordinary JSON/CSV serialization of empty sweeps is included below. [src/digitalmodel/structural/buckling_parametric.py:L101-L156] [src/digitalmodel/structural/buckling_parametric.py:L232-L284] [src/digitalmodel/structural/panel_buckling_parametric.py:L128-L184] [src/digitalmodel/structural/panel_buckling_parametric.py:L277-L329]

sweep: 51 found

| ID | Symptom / cause at code pin | Detection check | Follow-up |
| --- | --- | --- | --- |
| S01 | Inconsistent moments can become zero bandwidth after clamping; negative inputs are not distinguished from roundoff. [src/digitalmodel/structural/spectral_fatigue.py:L114-L119] | no detection check exists | issue pending (1) |
| S02 | Negative variance returns the same zero damage/cycles/range as unloaded stress; zero-variance behavior has a test, negative variance does not. [src/digitalmodel/structural/spectral_fatigue.py:L156-L159] | C1a covers the zero case only; no detection check exists for negative variance | issue pending (1) |
| S03 | Only lengths are checked: descending frequencies, NaN or negative density can propagate into moments. [src/digitalmodel/structural/spectral_fatigue.py:L53-L67] | C0a checks empty inputs only; no detection check exists for these inputs | issue pending (1) |
| S04 | Positive-variance damage divides by supplied period and accepts duration/slope without physical range guards. [src/digitalmodel/structural/spectral_fatigue.py:L162-L172] | no detection check exists | issue pending (1) |
| S05 | Absent or empty sweep selects the full default study rather than identifying incomplete input. [src/digitalmodel/structural/buckling_workflow.py:L56-L60] | no detection check exists | issue pending (3) |
| S06 | Absent result folder falls back to config directory or current-directory results. [src/digitalmodel/structural/buckling_workflow.py:L78-L83] | no detection check exists | issue pending (3) |
| S07 | External thickness values use float without locale/separator normalization or a finite-value check. [src/digitalmodel/structural/buckling_workflow.py:L62-L62] | no detection check exists | issue pending (3) |
| S08 | External breadth values use the same unqualified float conversion. [src/digitalmodel/structural/buckling_workflow.py:L63-L63] | no detection check exists | issue pending (3) |
| S09 | External span values use the same unqualified float conversion. [src/digitalmodel/structural/buckling_workflow.py:L64-L64] | no detection check exists | issue pending (3) |
| S10 | External load text uses float elementwise without unit/separator interpretation. [src/digitalmodel/structural/buckling_workflow.py:L65-L65] | no detection check exists | issue pending (3) |
| S11 | Factor text uses float and an implicit default; omitted factor is not reported as missing. [src/digitalmodel/structural/buckling_workflow.py:L67-L67] | no detection check exists | issue pending (3) |
| S12 | An empty sweep axis returns an empty row list through a zero-iteration product. [src/digitalmodel/structural/buckling_parametric.py:L114-L156] | no detection check exists | issue pending (3) |
| S13 | Nonfinite safety factor is serialized as None; the reason is not carried in that field. [src/digitalmodel/structural/buckling_parametric.py:L148-L150] | no detection check exists | issue pending (3) |
| S14 | float conversion and rounding operate on numeric analyzer results, not a locale-aware external-text parser. [src/digitalmodel/structural/buckling_parametric.py:L97-L98] | C5a covers ordinary numeric round trips only | issue pending (3) |
| S15 | Length/stress units, factor and default study/load choices are implicit configuration assumptions; listed default axes include zero transverse/shear components. [src/digitalmodel/structural/buckling_parametric.py:L38-L89] | C1a checks the default row count; no detection check exists for units or omitted design basis | issue pending (2) |
| S16 | An empty panel axis similarly returns an empty row list. [src/digitalmodel/structural/panel_buckling_parametric.py:L139-L184] | no detection check exists | issue pending (3) |
| S17 | float conversion and rounding operate on numeric results and do not validate external text. [src/digitalmodel/structural/panel_buckling_parametric.py:L106-L107] | C5a covers ordinary numeric round trips only | issue pending (3) |
| S18 | Unknown requested reference profile falls back to the first profile; an empty profile list instead raises. [src/digitalmodel/structural/panel_buckling_parametric.py:L200-L204] | no detection check exists | issue pending (3) |
| S19 | Transverse normal stress is fixed to zero, even though upstream analyzers support more general loads. [src/digitalmodel/structural/panel_buckling_parametric.py:L151-L154] | no detection check exists | issue pending (2) |
| S20 | Mode counters start at zero and get missing keys as zero before incrementing; this is counting initialization, not lost analysis data. [src/digitalmodel/structural/panel_buckling_parametric.py:L253-L258] | C1a includes the mode-split count test | issue pending (3) |
| S21 | mm/MPa units, span, factor and sample zero-shear loads are configuration assumptions. [src/digitalmodel/structural/panel_buckling_parametric.py:L43-L103] | C1a checks default counts; no detection check exists for an explicit design basis | issue pending (2) |
| S22 | Empty sweep rows can be written as empty CSV/JSON lookup structures without an incomplete-analysis flag. [src/digitalmodel/structural/buckling_parametric.py:L239-L282] | no detection check exists | issue pending (3) |
| S23 | Empty panel rows can likewise be exported with validated metadata. [src/digitalmodel/structural/panel_buckling_parametric.py:L284-L327] | no detection check exists | issue pending (3) |
| S24 | Duplicate rounded case keys silently overwrite prior index entries. [src/digitalmodel/structural/buckling_parametric.py:L254-L260] | C5a checks only unique fixture keys; no detection check exists for collisions | issue pending (3) |
| S25 | Panel keys omit span, factor and tee dimensions; duplicate names or combined outputs can overwrite indexed cases. [src/digitalmodel/structural/panel_buckling_parametric.py:L263-L304] | C5a checks only unique fixture keys; no detection check exists for collisions | issue pending (3) |
| S26 | Unknown governing mode is silently encoded as plate mode 0. [src/digitalmodel/structural/panel_design_explorer.py:L114-L118] | C0d checks panel sweep modes; no detection check exists for an unknown explorer mode | issue pending (3) |
| S27 | No passing sampled load leaves allowable as None; the boundary is a sampled last-pass value rather than a solved capacity. [src/digitalmodel/structural/panel_design_explorer.py:L104-L124] | C1a checks valid sampled boundaries; no detection check exists for an unexpectedly all-None allowable vector | issue pending (3) |
| S28 | int(round(interval/step)) can truncate or overshoot an off-grid endpoint; invalid steps lack a local guard. [src/digitalmodel/structural/panel_design_explorer.py:L66-L68] | no detection check exists | issue pending (3) |
| S29 | None config chooses all defaults; duplicate profile names are collected without rejection. [src/digitalmodel/structural/panel_design_explorer.py:L90-L93] | no detection check exists | issue pending (3) |
| S30 | Axis defaults include zero axial load and zero shear; units and increments are assumed mm/MPa. [src/digitalmodel/structural/panel_design_explorer.py:L48-L63] | C1a includes zero-load and shear response tests; no detection check exists for units | issue pending (2) |
| S31 | ImportError becomes unavailable status; this is an explicit guarded dependency path rather than silent success. [src/digitalmodel/structural/parametric_coordinator.py:L44-L48] | C1c requires an unavailable-interface exception on the campaign route | issue pending (2) |
| S32 | Omitted bending moment defaults to a zero-load list; source units are inherited as N·m. [src/digitalmodel/structural/parametric_coordinator.py:L89-L89] | no detection check exists for omission versus an intentional zero | issue pending (2) |
| S33 | Omitted effective tension defaults to a zero-load list; source units are inherited as N. [src/digitalmodel/structural/parametric_coordinator.py:L90-L90] | no detection check exists for omission versus an intentional zero | issue pending (2) |
| S34 | Omitted corrosion allowance defaults to zero; source units are inherited as m. [src/digitalmodel/structural/parametric_coordinator.py:L92-L92] | no detection check exists for omission versus an intentional zero | issue pending (2) |
| S35 | Fatigue reference thickness defaults without a local calculation-time Citation record. [src/digitalmodel/structural/parametric_coordinator.py:L119-L119] | no detection check exists | issue pending (2) |
| S36 | Missing wall-summary count becomes integer zero in a combined summary. [src/digitalmodel/structural/parametric_coordinator.py:L340-L340] | no detection check exists | issue pending (4) |
| S37 | Missing fatigue-pass count becomes integer zero in a combined summary. [src/digitalmodel/structural/parametric_coordinator.py:L341-L341] | no detection check exists | issue pending (4) |
| S38 | Missing fatigue-fail count becomes integer zero in a combined summary. [src/digitalmodel/structural/parametric_coordinator.py:L342-L342] | no detection check exists | issue pending (4) |
| S39 | Missing is_safe column becomes zero passing rows; numeric sum is converted to int. [src/digitalmodel/structural/parametric_coordinator.py:L352-L359] | no detection check exists | issue pending (4) |
| S40 | Empty wall results produce NaN maximum utilization instead of a missing-result status. [src/digitalmodel/structural/parametric_coordinator.py:L360-L360] | no detection check exists | issue pending (4) |
| S41 | Empty wall results produce NaN minimum utilization instead of a missing-result status. [src/digitalmodel/structural/parametric_coordinator.py:L361-L361] | no detection check exists | issue pending (4) |
| S42 | Missing passes_dff column becomes zero passing rows; numeric sum is converted to int. [src/digitalmodel/structural/parametric_coordinator.py:L365-L373] | no detection check exists | issue pending (4) |
| S43 | Empty wall DataFrame renders an empty table body without an incomplete-result status. [src/digitalmodel/structural/parametric_report.py:L48-L60] | no detection check exists | issue pending (4) |
| S44 | Missing display columns are silently omitted from the wall report. [src/digitalmodel/structural/parametric_report.py:L50-L53] | no detection check exists | issue pending (4) |
| S45 | Empty generic DataFrame similarly emits no table. [src/digitalmodel/structural/parametric_report.py:L94-L102] | no detection check exists | issue pending (4) |
| S46 | Missing body tag causes full HTML to be embedded rather than a format error. [src/digitalmodel/structural/parametric_report.py:L109-L113] | no detection check exists | issue pending (4) |
| S47 | Absent config directory uses current directory; missing nested mappings become empty defaults before required-key access. [src/digitalmodel/structural/wall_thickness_quickcheck.py:L72-L78] | no detection check exists | issue pending (5) |
| S48 | Empty selection gives zero rows then rows[0] fails during CSV field selection; no explicit empty-result diagnosis. [src/digitalmodel/structural/wall_thickness_quickcheck.py:L41-L66] | no detection check exists | issue pending (5) |
| S49 | Cache acceptance and report generation delegate to example code; no local schema/fingerprint validation precedes export. [src/digitalmodel/structural/wall_thickness_quickcheck.py:L80-L89] | C5b detects stale committed-cache values; no detection check exists for arbitrary cache provenance | issue pending (5) |
| S50 | Moment order defaults to zero and integration initializes a zero accumulator; these are defined integration choices rather than an error fallback. [src/digitalmodel/structural/spectral_fatigue.py:L29-L67] | C2a checks the resulting flat-spectrum integral | issue pending (1) |
| S51 | Local slenderness assumes consistent length and stress units, with no explicit unit conversion. [src/digitalmodel/structural/buckling_parametric.py:L92-L94] | C1a includes the slenderness algebra test; no detection check exists for mixed units | issue pending (2) |

The issue search did not confirm a named top-level incident from the inventory count alone. Issue #2165 concerns the separate S-N curve libraries, and #2583 concerns `src/digitalmodel/fatigue/spectral_fatigue.py`; neither is evidence that the direct spectral entry has the same frequency-convention defect. The direct entry integrates Hz moments and returns `sqrt(m0/m2)` without an angular-frequency divisor. [src/digitalmodel/structural/spectral_fatigue.py:L32-L39] [src/digitalmodel/structural/spectral_fatigue.py:L70-L94] [src/digitalmodel/structural/fatigue/sn_curves.py:L326-L339]

## 7. Lessons log

Append future evidence as new rows; preserve previous entries. This log records source-supported implementation observations, not a clearance decision.

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack revision 0 | Positive damage and duration monotonicity assertions do not establish an independently checked damage magnitude. | [tests/test_spectral_fatigue.py:L88-L149] | Separate validated elementary cases from damage regression evidence. | Not assigned |
| 2026-10-18 | Initial pack revision 0 | The wall-thickness coordinator delegates; its map classification must not be read as another capacity formula. | [src/digitalmodel/structural/parametric_coordinator.py:L255-L275] | Record the delegate and map follow-up. | Not assigned |

## 8. Ownership

Review assignment: engineering calculations quality manager; deputy assignment: engineering calculations deputy quality manager. Producing discipline: structural calculations. These are role assignments for this draft, without a claim of completed review.

Escalate ambiguous standard editions, missing independent anchors, tolerance changes and disputed implementation classifications to the family quality manager. Escalate cross-family scope conflicts to the maintainers. Must not merge before the second-provider review.

## 9. Dependencies

The plate/panel sweeps and explorer consume `structural_analysis` analyzers/material data; the coordinator consumes `analysis` and `fatigue` sweeps; the quickcheck imports example code from the source checkout. [src/digitalmodel/structural/buckling_parametric.py:L27-L31] [src/digitalmodel/structural/panel_buckling_parametric.py:L30-L35] [src/digitalmodel/structural/panel_design_explorer.py:L25-L30] [src/digitalmodel/structural/parametric_coordinator.py:L23-L36] [src/digitalmodel/structural/wall_thickness_quickcheck.py:L16-L31]

| Boundary | Inventory role and scope |
| --- | --- |
| `structural/analysis` | Separate wall-capacity package; passed-through design units and code choice. [src/digitalmodel/structural/analysis/wall_thickness_parametric.py:L43-L57] |
| `structural/fatigue` | Separate fatigue package; histogram sweep and report producer. [src/digitalmodel/structural/fatigue/parametric_sweep.py:L74-L109] |
| `structural/structural_analysis` | Separate plate/panel/column algorithms, consumed by top-level sweeps. [src/digitalmodel/structural/structural_analysis/buckling.py:L1-L17] [src/digitalmodel/structural/panel_buckling_parametric.py:L30-L35] |
| Other structural sub-packages | `fatigue_apps`, `fe`, `jacket_topside`, `offshore_container`, `offshore_resilience`, `ovality`, `parachute`, `pipe_capacity`, `pipe_cross_section`, `plate_capacity`, `stress`; separate scope, not included in the direct-file sweep. [src/digitalmodel/structural/fatigue_apps/__init__.py:L1-L1] [src/digitalmodel/structural/fe/__init__.py:L1-L1] [src/digitalmodel/structural/jacket_topside/__init__.py:L1-L1] [src/digitalmodel/structural/offshore_container/__init__.py:L1-L1] [src/digitalmodel/structural/offshore_resilience/__init__.py:L1-L1] [src/digitalmodel/structural/ovality/__init__.py:L1-L1] [src/digitalmodel/structural/parachute/__init__.py:L1-L1] [src/digitalmodel/structural/pipe_capacity/pipe_capacity.py:L1-L10] [src/digitalmodel/structural/pipe_cross_section/__init__.py:L1-L1] [src/digitalmodel/structural/plate_capacity/plateBucklingCal_G.py:L1-L10] [src/digitalmodel/structural/stress/__init__.py:L1-L1] |
| pandas and standard library | pandas builds tables/CSV/HTML; math, json, csv and importlib support direct calculations and serialization. [src/digitalmodel/structural/buckling_parametric.py:L18-L25] [src/digitalmodel/structural/parametric_report.py:L15-L20] [src/digitalmodel/structural/wall_thickness_quickcheck.py:L5-L9] |
| Workflow consumers | Registry exposes `wall-thickness-quickcheck` and `buckling-parametric`, both marked offline. [docs/registry/workflows.yaml:L728-L740] [docs/registry/workflows.yaml:L1249-L1260] |

OrcaFlex dependency inventory below covers every direct-source line containing `OrcaFlex`, `OrcFxAPI`, `ORCAFLEX`, `orcaflex` or `ofx`; occurrences are read as text, and the guarded unavailable-interface tests run without a solver. No solver version is established by those declarations. [src/digitalmodel/structural/parametric_coordinator.py:L44-L48] [tests/test_parametric_coordinator.py:L363-L397]

| File | Source lines | Dependency meaning |
| --- | --- | --- |
| `parametric_coordinator.py` | 2, 6, 10, 11 | Scope and supported campaign/error declarations. [src/digitalmodel/structural/parametric_coordinator.py:L2-L11] |
| `parametric_coordinator.py` | 41, 45, 46, 48 | Guarded interface import and availability flag. [src/digitalmodel/structural/parametric_coordinator.py:L41-L48] |
| `parametric_coordinator.py` | 55, 56 | Unavailable-interface exception. [src/digitalmodel/structural/parametric_coordinator.py:L55-L57] |
| `parametric_coordinator.py` | 69 | Campaign study enum. [src/digitalmodel/structural/parametric_coordinator.py:L69-L69] |
| `parametric_coordinator.py` | 129, 130, 133 | Campaign parameter declaration and schema reference. [src/digitalmodel/structural/parametric_coordinator.py:L129-L133] |
| `parametric_coordinator.py` | 160, 167, 168 | Optional parameter and required-block validation. [src/digitalmodel/structural/parametric_coordinator.py:L160-L168] |
| `parametric_coordinator.py` | 225, 226, 233 | Documented error and dispatch route. [src/digitalmodel/structural/parametric_coordinator.py:L225-L233] |
| `parametric_coordinator.py` | 295, 296, 297, 298, 299, 301, 303, 305, 316, 319, 323 | Availability check, combination-table generation and HTML title. [src/digitalmodel/structural/parametric_coordinator.py:L295-L323] |
| `parametric_report.py` | 9 | Report-builder scope mentions campaign results. [src/digitalmodel/structural/parametric_report.py:L8-L10] |

### Implementation-map inventory and counted-family search

The inventory below transcribes every implementation row of the two families with direct-package rows, preserving map order, symbol and relation. Other structural sub-packages do not contribute to the in-package counts. Map classifications are recorded as data, not endorsed as independent implementations.

| Rank | Family | Direct-package rows | All map rows |
| --- | --- | --- | --- |
| 1 | `effective-tension-riser-stack-up` | 1 | 264 [docs/registry/implementation-map.yaml:L2146-L4917] |
| 2 | `sn-curves-and-miner-damage` | 1 | 392 [docs/registry/implementation-map.yaml:L16814-L20948] |

Both families are counted: their direct-package counts tie, so the map file order determines rank. No signal matches the package-relative path `structural` case-insensitively, so no signal is excluded. Signals are applied exactly as Python `re`, case-sensitive, to all nine direct `.py` files at the code pin; matching lines are assigned to their smallest enclosing function/class, or one module-level definition.

#### effective-tension-riser-stack-up

Exact compiled signal strings (Python string literals):

```python
import re
signals = [
    'effective_tension|effectiveTension|top_tension|tensioner|stack_up|internal_pressure.*(?:area|A_i)|external_pressure.*(?:area|A_o)',
    'effective.tension|tensioner|stack.up',
    'internal_pressure|external_pressure|fluid_density|buoyancy_module',
]
patterns = [re.compile(signal) for signal in signals]
```

| Hit definition | Classification and code-pin evidence |
| --- | --- |
| `src/digitalmodel/structural/parametric_coordinator.py` — `WallThicknessParams` | Data container or input validation; helper. [src/digitalmodel/structural/parametric_coordinator.py:L87-L90] |
| `src/digitalmodel/structural/parametric_coordinator.py` — `WallThicknessParams.__post_init__` | Data container or input validation; helper. [src/digitalmodel/structural/parametric_coordinator.py:L101-L104] |
| `src/digitalmodel/structural/parametric_coordinator.py` — `ParametricCoordinator._run_wall_thickness` | Listed map row; delegates to ParametricSweep, not another local capacity formula. [src/digitalmodel/structural/parametric_coordinator.py:L255-L275] |
| `src/digitalmodel/structural/parametric_report.py` — `<module>` | Module prose or report-column declaration; helper. [src/digitalmodel/structural/parametric_report.py:L37-L37] |

| Map row | Path | Symbol | Map relation | Evidence at map revision |
| --- | --- | --- | --- | --- |
| 1 | `docs/domains/orcaflex/examples/modular/A01/A01 Catenary riser/run_orcaflex.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L2146-L2155] |
| 2 | `docs/domains/orcaflex/examples/modular/A01/A01 Lazy wave riser/run_orcaflex.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L2156-L2165] |
| 3 | `docs/domains/orcaflex/examples/modular/C10/C10 Multiple statics/run_orcaflex.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L2166-L2175] |
| 4 | `docs/domains/orcaflex/examples/modular/E08/PipelayConfig/run_orcaflex.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L2176-L2185] |
| 5 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r01_catenary` | `independent` | [docs/registry/implementation-map.yaml:L2186-L2195] |
| 6 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r02_lazy_wave` | `independent` | [docs/registry/implementation-map.yaml:L2196-L2205] |
| 7 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r05_lay_table` | `independent` | [docs/registry/implementation-map.yaml:L2206-L2215] |
| 8 | `docs/domains/orcaflex/examples/qa/orcaflex_example_qa.py` | `check_r06_multi_static` | `independent` | [docs/registry/implementation-map.yaml:L2216-L2225] |
| 9 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `extract_vessel_params` | `example` | [docs/registry/implementation-map.yaml:L2226-L2235] |
| 10 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `vessel_band_specs` | `example` | [docs/registry/implementation-map.yaml:L2236-L2247] |
| 11 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `find_required_tension` | `example` | [docs/registry/implementation-map.yaml:L2248-L2259] |
| 12 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `_build_infeasible_result` | `example` | [docs/registry/implementation-map.yaml:L2260-L2269] |
| 13 | `examples/demos/gtm/demo_04_shallow_water_pipelay.py` | `run_single_case` | `example` | [docs/registry/implementation-map.yaml:L2270-L2282] |
| 14 | `examples/demos/gtm/results_store_demo04.py` | `(module-level)` | `example` | [docs/registry/implementation-map.yaml:L2283-L2291] |
| 15 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_good_case` | `example` | [docs/registry/implementation-map.yaml:L2292-L2301] |
| 16 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `_infeasible_case` | `example` | [docs/registry/implementation-map.yaml:L2302-L2311] |
| 17 | `examples/demos/gtm/tests/test_demo_04_results_store.py` | `test_999_sentinel_and_none_null_round_trip` | `example` | [docs/registry/implementation-map.yaml:L2312-L2324] |
| 18 | `examples/domains/workflows/04_pipeline_installation.py` | `analyze_installation_catenary` | `example` | [docs/registry/implementation-map.yaml:L2325-L2334] |
| 19 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_schedule_row` | `example` | [docs/registry/implementation-map.yaml:L2335-L2344] |
| 20 | `examples/workflows/riser-stackup-registry-batch/run.py` | `_assemble_row` | `example` | [docs/registry/implementation-map.yaml:L2345-L2356] |
| 21 | `scripts/capabilities/build_wall_thickness_3d.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L2357-L2365] |
| 22 | `scripts/capabilities/build_wall_thickness_3d.py` | `_analyze_point` | `independent` | [docs/registry/implementation-map.yaml:L2366-L2375] |
| 23 | `scripts/capabilities/build_wall_thickness_3d.py` | `build_study` | `wrapper` | [docs/registry/implementation-map.yaml:L2376-L2387] |
| 24 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_axes` | `independent` | [docs/registry/implementation-map.yaml:L2388-L2397] |
| 25 | `scripts/capabilities/build_wall_thickness_3d_page.py` | `_pack_rows` | `independent` | [docs/registry/implementation-map.yaml:L2398-L2407] |
| 26 | `scripts/capabilities/build_wall_thickness_explorer.py` | `_run` | `independent` | [docs/registry/implementation-map.yaml:L2408-L2417] |
| 27 | `scripts/orcaflex/check_generic_scr_slwr.py` | `metrics` | `independent` | [docs/registry/implementation-map.yaml:L2418-L2427] |
| 28 | `scripts/orcaflex/check_generic_scr_slwr.py` | `calculate` | `independent` | [docs/registry/implementation-map.yaml:L2428-L2437] |
| 29 | `scripts/orcaflex/prepare_slay_campaign.py` | `catenary` | `independent` | [docs/registry/implementation-map.yaml:L2438-L2447] |
| 30 | `scripts/orcaflex/prepare_slay_campaign.py` | `case_spec` | `independent` | [docs/registry/implementation-map.yaml:L2448-L2457] |
| 31 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare_case` | `independent` | [docs/registry/implementation-map.yaml:L2458-L2470] |
| 32 | `scripts/orcaflex/prepare_slay_campaign.py` | `prepare` | `independent` | [docs/registry/implementation-map.yaml:L2471-L2484] |
| 33 | `scripts/parametric/build_orcaflex_library.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L2485-L2494] |
| 34 | `scripts/python/digitalmodel/analysis/APIST2RD_Methods.py` | `APISTD2RDCalculation` | `independent` | [docs/registry/implementation-map.yaml:L2495-L2504] |
| 35 | `scripts/python/digitalmodel/analysis/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | `independent` | [docs/registry/implementation-map.yaml:L2505-L2514] |
| 36 | `scripts/python/digitalmodel/analysis/RiserStackup.py` | `StackUpDiagram` | `independent` | [docs/registry/implementation-map.yaml:L2515-L2524] |
| 37 | `scripts/python/digitalmodel/analysis/StackUp_3000Ft.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L2525-L2534] |
| 38 | `scripts/python/digitalmodel/analysis/StackUp_9500Ft.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L2535-L2544] |
| 39 | `scripts/reporting/build_riser_pipe_comparison.py` | `evaluate` | `caller` | [docs/registry/implementation-map.yaml:L2545-L2556] |
| 40 | `scripts/reporting/build_riser_pipe_comparison.py` | `_validate` | `independent` | [docs/registry/implementation-map.yaml:L2557-L2566] |
| 41 | `scripts/reporting/build_riser_pipe_comparison.py` | `numeric_values` | `independent` | [docs/registry/implementation-map.yaml:L2567-L2576] |
| 42 | `scripts/reporting/build_riser_pipe_comparison.py` | `_input_table` | `independent` | [docs/registry/implementation-map.yaml:L2577-L2586] |
| 43 | `scripts/riser_database/build_tables.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L2587-L2595] |
| 44 | `src/digitalmodel/asset_integrity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | `independent` | [docs/registry/implementation-map.yaml:L2596-L2605] |
| 45 | `src/digitalmodel/code_checks/dnv_os_f101.py` | `local_buckling_lcc` | `independent` | [docs/registry/implementation-map.yaml:L2606-L2615] |
| 46 | `src/digitalmodel/code_checks/workflow.py` | `_run_api_rp_2rd` | `independent` | [docs/registry/implementation-map.yaml:L2616-L2625] |
| 47 | `src/digitalmodel/code_checks/workflow.py` | `_run_dnv_os_f201` | `independent` | [docs/registry/implementation-map.yaml:L2626-L2635] |
| 48 | `src/digitalmodel/drilling_riser/assembly.py` | `minimum_top_tension_16q` | `independent` | [docs/registry/implementation-map.yaml:L2636-L2645] |
| 49 | `src/digitalmodel/drilling_riser/assembly.py` | `_unit_count` | `independent` | [docs/registry/implementation-map.yaml:L2646-L2655] |
| 50 | `src/digitalmodel/drilling_riser/assembly.py` | `check_rig_capability` | `independent` | [docs/registry/implementation-map.yaml:L2656-L2667] |
| 51 | `src/digitalmodel/drilling_riser/campaign.py` | `case_spec` | `independent` | [docs/registry/implementation-map.yaml:L2668-L2679] |
| 52 | `src/digitalmodel/drilling_riser/campaign.py` | `_ring_tensioners` | `independent` | [docs/registry/implementation-map.yaml:L2680-L2689] |
| 53 | `src/digitalmodel/drilling_riser/campaign.py` | `set_line_tension` | `independent` | [docs/registry/implementation-map.yaml:L2690-L2701] |
| 54 | `src/digitalmodel/drilling_riser/campaign.py` | `calibrate_line_tension` | `independent` | [docs/registry/implementation-map.yaml:L2702-L2715] |
| 55 | `src/digitalmodel/drilling_riser/campaign.py` | `_statics_path` | `independent` | [docs/registry/implementation-map.yaml:L2716-L2726] |
| 56 | `src/digitalmodel/drilling_riser/campaign.py` | `robust_statics` | `independent` | [docs/registry/implementation-map.yaml:L2727-L2740] |
| 57 | `src/digitalmodel/drilling_riser/campaign.py` | `_tensioner_vertical_n` | `independent` | [docs/registry/implementation-map.yaml:L2741-L2750] |
| 58 | `src/digitalmodel/drilling_riser/campaign.py` | `tensioner_vertical_target_n` | `independent` | [docs/registry/implementation-map.yaml:L2751-L2760] |
| 59 | `src/digitalmodel/drilling_riser/campaign.py` | `physical_state_checks` | `independent` | [docs/registry/implementation-map.yaml:L2761-L2774] |
| 60 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.prepare` | `independent` | [docs/registry/implementation-map.yaml:L2775-L2787] |
| 61 | `src/digitalmodel/drilling_riser/campaign.py` | `RiserCampaignAdapter.extract` | `independent` | [docs/registry/implementation-map.yaml:L2788-L2797] |
| 62 | `src/digitalmodel/drilling_riser/envelope.py` | `compute_operating_envelope` | `independent` | [docs/registry/implementation-map.yaml:L2798-L2807] |
| 63 | `src/digitalmodel/drilling_riser/global_model/build.py` | `equivalent_string_top_tension_n` | `independent` | [docs/registry/implementation-map.yaml:L2808-L2817] |
| 64 | `src/digitalmodel/drilling_riser/global_model/build.py` | `_tensioner_geometry` | `independent` | [docs/registry/implementation-map.yaml:L2818-L2827] |
| 65 | `src/digitalmodel/drilling_riser/global_model/build.py` | `winch_tension_n` | `independent` | [docs/registry/implementation-map.yaml:L2828-L2839] |
| 66 | `src/digitalmodel/drilling_riser/global_model/build.py` | `build_generic_spec` | `independent` | [docs/registry/implementation-map.yaml:L2840-L2852] |
| 67 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `effective_tension_chain` | `independent` | [docs/registry/implementation-map.yaml:L2853-L2862] |
| 68 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tension_references` | `independent` | [docs/registry/implementation-map.yaml:L2863-L2874] |
| 69 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `tensioner_lateral_stiffness_n_m` | `independent` | [docs/registry/implementation-map.yaml:L2875-L2884] |
| 70 | `src/digitalmodel/drilling_riser/global_model/hand_checks.py` | `reference_periods` | `caller` | [docs/registry/implementation-map.yaml:L2885-L2896] |
| 71 | `src/digitalmodel/drilling_riser/global_model/hang_off.py` | `hung_weight_n` | `independent` | [docs/registry/implementation-map.yaml:L2897-L2907] |
| 72 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `OpenWaterRiserSpec._closes` | `independent` | [docs/registry/implementation-map.yaml:L2908-L2917] |
| 73 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `build_open_water_generic_spec` | `independent` | [docs/registry/implementation-map.yaml:L2918-L2927] |
| 74 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `tension_references` | `independent` | [docs/registry/implementation-map.yaml:L2928-L2938] |
| 75 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `reference_periods` | `independent` | [docs/registry/implementation-map.yaml:L2939-L2948] |
| 76 | `src/digitalmodel/drilling_riser/global_model/open_water.py` | `release_reference` | `independent` | [docs/registry/implementation-map.yaml:L2949-L2960] |
| 77 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `ring_vertical_residual_n` | `independent` | [docs/registry/implementation-map.yaml:L2961-L2970] |
| 78 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `open_water_physical_checks` | `independent` | [docs/registry/implementation-map.yaml:L2971-L2980] |
| 79 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `Tensioners._failed` | `independent` | [docs/registry/implementation-map.yaml:L2981-L2990] |
| 80 | `src/digitalmodel/drilling_riser/global_model/spec.py` | `RiserGlobalModelSpec._geometry_closes` | `independent` | [docs/registry/implementation-map.yaml:L2991-L3000] |
| 81 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `_missing_open_water` | `independent` | [docs/registry/implementation-map.yaml:L3001-L3010] |
| 82 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `missing_channels` | `independent` | [docs/registry/implementation-map.yaml:L3011-L3022] |
| 83 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract` | `independent` | [docs/registry/implementation-map.yaml:L3023-L3032] |
| 84 | `src/digitalmodel/drilling_riser/global_model/w5_channels.py` | `extract_open_water` | `independent` | [docs/registry/implementation-map.yaml:L3033-L3042] |
| 85 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `t_min` | `independent` | [docs/registry/implementation-map.yaml:L3043-L3052] |
| 86 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tension_setting_max` | `independent` | [docs/registry/implementation-map.yaml:L3053-L3062] |
| 87 | `src/digitalmodel/drilling_riser/postprocess/checks.py` | `tensioner_stroke` | `independent` | [docs/registry/implementation-map.yaml:L3063-L3072] |
| 88 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `ScheduleAssembly.minimum_top_tension_16q_kn` | `independent` | [docs/registry/implementation-map.yaml:L3073-L3082] |
| 89 | `src/digitalmodel/drilling_riser/schedule_assembly.py` | `_load_oc_assembly` | `independent` | [docs/registry/implementation-map.yaml:L3083-L3092] |
| 90 | `src/digitalmodel/drilling_riser/stackup.py` | `effective_tension` | `independent` | [docs/registry/implementation-map.yaml:L3093-L3102] |
| 91 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `parse_snapshots` | `independent` | [docs/registry/implementation-map.yaml:L3103-L3112] |
| 92 | `src/digitalmodel/drilling_riser/telemetry_inputs.py` | `snapshot_to_tension_n` | `independent` | [docs/registry/implementation-map.yaml:L3113-L3122] |
| 93 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `router` | `independent` | [docs/registry/implementation-map.yaml:L3123-L3135] |
| 94 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_section_result` | `independent` | [docs/registry/implementation-map.yaml:L3136-L3145] |
| 95 | `src/digitalmodel/drilling_riser/tsj_workflow.py` | `_effective_tension_n` | `independent` | [docs/registry/implementation-map.yaml:L3146-L3155] |
| 96 | `src/digitalmodel/drilling_riser/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L3156-L3168] |
| 97 | `src/digitalmodel/drilling_riser/workflow.py` | `_write_profile_csv` | `independent` | [docs/registry/implementation-map.yaml:L3169-L3178] |
| 98 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_to_shear7_result` | `independent` | [docs/registry/implementation-map.yaml:L3179-L3188] |
| 99 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | `independent` | [docs/registry/implementation-map.yaml:L3189-L3198] |
| 100 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | `independent` | [docs/registry/implementation-map.yaml:L3199-L3208] |
| 101 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.save_fatigue_summary_result` | `independent` | [docs/registry/implementation-map.yaml:L3209-L3218] |
| 102 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.add_fatigue_damage_results_to_df` | `independent` | [docs/registry/implementation-map.yaml:L3219-L3231] |
| 103 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | `independent` | [docs/registry/implementation-map.yaml:L3232-L3241] |
| 104 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | `independent` | [docs/registry/implementation-map.yaml:L3242-L3251] |
| 105 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | `independent` | [docs/registry/implementation-map.yaml:L3252-L3264] |
| 106 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | `independent` | [docs/registry/implementation-map.yaml:L3265-L3274] |
| 107 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | `wrapper` | [docs/registry/implementation-map.yaml:L3275-L3287] |
| 108 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | `independent` | [docs/registry/implementation-map.yaml:L3288-L3300] |
| 109 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | `independent` | [docs/registry/implementation-map.yaml:L3301-L3311] |
| 110 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | `independent` | [docs/registry/implementation-map.yaml:L3312-L3321] |
| 111 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | `independent` | [docs/registry/implementation-map.yaml:L3322-L3331] |
| 112 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | `independent` | [docs/registry/implementation-map.yaml:L3332-L3341] |
| 113 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | `independent` | [docs/registry/implementation-map.yaml:L3342-L3351] |
| 114 | `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | `independent` | [docs/registry/implementation-map.yaml:L3352-L3361] |
| 115 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.prepare_stack_up_properties_df` | `caller` | [docs/registry/implementation-map.yaml:L3362-L3377] |
| 116 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_from_endA_in_df` | `independent` | [docs/registry/implementation-map.yaml:L3378-L3387] |
| 117 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_shear7_x_by_L_in_df` | `independent` | [docs/registry/implementation-map.yaml:L3388-L3397] |
| 118 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_mudline_in_df` | `independent` | [docs/registry/implementation-map.yaml:L3398-L3407] |
| 119 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.populate_elevation_relative_to_MSL_in_df` | `independent` | [docs/registry/implementation-map.yaml:L3408-L3417] |
| 120 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_inservice_weights_and_tensions` | `caller` | [docs/registry/implementation-map.yaml:L3418-L3432] |
| 121 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.assign_key_properties_to_df` | `independent` | [docs/registry/implementation-map.yaml:L3433-L3442] |
| 122 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stack_in_service_weights` | `independent` | [docs/registry/implementation-map.yaml:L3443-L3452] |
| 123 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_effective_tension` | `independent` | [docs/registry/implementation-map.yaml:L3453-L3462] |
| 124 | `src/digitalmodel/infrastructure/base_solvers/marine/typical_riser_stack_up_calculations.py` | `TypicalRiserStackUpCalculations.evaluate_stretch_in_riser` | `independent` | [docs/registry/implementation-map.yaml:L3463-L3472] |
| 125 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.title` | `independent` | [docs/registry/implementation-map.yaml:L3473-L3482] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.riser_joints_to_shear7_zones` | `independent` | [docs/registry/implementation-map.yaml:L3483-L3492] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/viv/shear7_model_components.py` | `Shear7ModelComponents.add_block_3` | `independent` | [docs/registry/implementation-map.yaml:L3493-L3502] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_to_shear7_result` | `independent` | [docs/registry/implementation-map.yaml:L3503-L3512] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_riser_length_axis_to_orcaflex_result` | `independent` | [docs/registry/implementation-map.yaml:L3513-L3522] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_yearly_histogram_object_from_viv_run` | `independent` | [docs/registry/implementation-map.yaml:L3523-L3532] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.save_fatigue_summary_result` | `independent` | [docs/registry/implementation-map.yaml:L3533-L3542] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.add_fatigue_damage_results_to_df` | `copy` | [docs/registry/implementation-map.yaml:L3543-L3555] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_shear7_x_by_L_from_elevation_above_mudline` | `independent` | [docs/registry/implementation-map.yaml:L3556-L3565] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_elevation_from_end_A_from_elevation_above_mudline` | `independent` | [docs/registry/implementation-map.yaml:L3566-L3575] |
| 135 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_output.py` | `_write_tensions_csv` | `independent` | [docs/registry/implementation-map.yaml:L3576-L3585] |
| 136 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._line_state` | `independent` | [docs/registry/implementation-map.yaml:L3586-L3597] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._solve_line` | `independent` | [docs/registry/implementation-map.yaml:L3598-L3607] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._result` | `independent` | [docs/registry/implementation-map.yaml:L3608-L3617] |
| 139 | `src/digitalmodel/orcaflex/mooring_design.py` | `solve_catenary` | `independent` | [docs/registry/implementation-map.yaml:L3618-L3627] |
| 140 | `src/digitalmodel/orcaflex/pipelay_analysis.py` | `JLayConfig.calculate_sagbend` | `independent` | [docs/registry/implementation-map.yaml:L3628-L3637] |
| 141 | `src/digitalmodel/orcaflex/riser_config.py` | `SCRDesignInput.calculate_geometry` | `independent` | [docs/registry/implementation-map.yaml:L3638-L3647] |
| 142 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_stroke` | `independent` | [docs/registry/implementation-map.yaml:L3648-L3657] |
| 143 | `src/digitalmodel/orcaflex/riser_config.py` | `TTRDesignInput.calculate_top_tension` | `independent` | [docs/registry/implementation-map.yaml:L3658-L3667] |
| 144 | `src/digitalmodel/orcaflex/riser_input_schema.py` | `_TopTensionedInput.to_legacy` | `independent` | [docs/registry/implementation-map.yaml:L3668-L3677] |
| 145 | `src/digitalmodel/orcaflex/viv_screening.py` | `BeamProperties.natural_frequency` | `independent` | [docs/registry/implementation-map.yaml:L3678-L3687] |
| 146 | `src/digitalmodel/parametric/generate.py` | `_code_check_utilisation` | `independent` | [docs/registry/implementation-map.yaml:L3688-L3697] |
| 147 | `src/digitalmodel/pipelay/workflow.py` | `run_jlay` | `independent` | [docs/registry/implementation-map.yaml:L3698-L3707] |
| 148 | `src/digitalmodel/riser_configuration_screen/static.py` | `_lazy_row` | `independent` | [docs/registry/implementation-map.yaml:L3708-L3717] |
| 149 | `src/digitalmodel/riser_configuration_screen/static.py` | `scr_offsets` | `independent` | [docs/registry/implementation-map.yaml:L3718-L3727] |
| 150 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_validate` | `independent` | [docs/registry/implementation-map.yaml:L3728-L3737] |
| 151 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_utilisation` | `independent` | [docs/registry/implementation-map.yaml:L3738-L3747] |
| 152 | `src/digitalmodel/riser_configuration_screen/workflow.py` | `_checks` | `independent` | [docs/registry/implementation-map.yaml:L3748-L3757] |
| 153 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/groups_builder.py` | `GroupsBuilder._build_from_spec` | `independent` | [docs/registry/implementation-map.yaml:L3758-L3767] |
| 154 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py` | `LineTypeBuilder.build` | `independent` | [docs/registry/implementation-map.yaml:L3768-L3777] |
| 155 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.should_generate` | `independent` | [docs/registry/implementation-map.yaml:L3778-L3787] |
| 156 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/winch_builder.py` | `WinchBuilder.build` | `independent` | [docs/registry/implementation-map.yaml:L3788-L3797] |
| 157 | `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `_apply_overrides` | `independent` | [docs/registry/implementation-map.yaml:L3798-L3807] |
| 158 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.mooring_pre_tension_analysis` | `independent` | [docs/registry/implementation-map.yaml:L3808-L3819] |
| 159 | `src/digitalmodel/solvers/orcaflex/mooring.py` | `Mooring.evaluate_line_length_next_iteration` | `independent` | [docs/registry/implementation-map.yaml:L3820-L3829] |
| 160 | `src/digitalmodel/solvers/orcaflex/opp_range_graph.py` | `OPPRangeGraph.postProcessRange` | `independent` | [docs/registry/implementation-map.yaml:L3830-L3839] |
| 161 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.postProcessRange` | `independent` | [docs/registry/implementation-map.yaml:L3840-L3849] |
| 162 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.orcaflex_model` | `independent` | [docs/registry/implementation-map.yaml:L3850-L3862] |
| 163 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.customize_fj_types` | `copy` | [docs/registry/implementation-map.yaml:L3863-L3872] |
| 164 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.links` | `wrapper` | [docs/registry/implementation-map.yaml:L3873-L3885] |
| 165 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_support_spring` | `copy` | [docs/registry/implementation-map.yaml:L3886-L3898] |
| 166 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.add_tensioners` | `copy` | [docs/registry/implementation-map.yaml:L3899-L3909] |
| 167 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.tensioner_vessel_locations` | `copy` | [docs/registry/implementation-map.yaml:L3910-L3919] |
| 168 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.spring_properties` | `independent` | [docs/registry/implementation-map.yaml:L3920-L3929] |
| 169 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_connection_definitions` | `copy` | [docs/registry/implementation-map.yaml:L3930-L3939] |
| 170 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_fj_line_definitions` | `copy` | [docs/registry/implementation-map.yaml:L3940-L3949] |
| 171 | `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.get_line_items_vertical` | `independent` | [docs/registry/implementation-map.yaml:L3950-L3959] |
| 172 | `src/digitalmodel/structural/analysis/cli.py` | `check` | `independent` | [docs/registry/implementation-map.yaml:L3960-L3969] |
| 173 | `src/digitalmodel/structural/analysis/wall_thickness.py` | `WallThicknessAnalyzer.check_combined_loading` | `independent` | [docs/registry/implementation-map.yaml:L3970-L3979] |
| 174 | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py` | `ApiStd2rdStrategy._check_combined` | `independent` | [docs/registry/implementation-map.yaml:L3980-L3989] |
| 175 | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py` | `DnvStF101Strategy._check_combined_loading` | `independent` | [docs/registry/implementation-map.yaml:L3990-L3999] |
| 176 | `src/digitalmodel/structural/analysis/wall_thickness_compare_workflow.py` | `run_wall_thickness_compare` | `independent` | [docs/registry/implementation-map.yaml:L4000-L4009] |
| 177 | `src/digitalmodel/structural/analysis/wall_thickness_lookup.py` | `MTLookupGenerator.generate` | `independent` | [docs/registry/implementation-map.yaml:L4010-L4019] |
| 178 | `src/digitalmodel/structural/analysis/wall_thickness_parametric.py` | `ParametricSweep.run` | `independent` | [docs/registry/implementation-map.yaml:L4020-L4029] |
| 179 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `PipelinePhase.to_design_loads` | `independent` | [docs/registry/implementation-map.yaml:L4030-L4039] |
| 180 | `src/digitalmodel/structural/analysis/wall_thickness_phases.py` | `create_standard_phases` | `independent` | [docs/registry/implementation-map.yaml:L4040-L4049] |
| 181 | `src/digitalmodel/structural/fatigue_apps/examples/load_scaling_runner.py` | `LoadScalingProcessor.process_all_conditions` | `independent` | [docs/registry/implementation-map.yaml:L4050-L4059] |
| 182 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | `independent` | [docs/registry/implementation-map.yaml:L4060-L4069] |
| 183 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | `independent` | [docs/registry/implementation-map.yaml:L4070-L4079] |
| 184 | `src/digitalmodel/structural/fatigue_apps/load_scaling.py` | `LoadScalingProcessor.load_reference_data` | `independent` | [docs/registry/implementation-map.yaml:L4080-L4089] |
| 185 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | `independent` | [docs/registry/implementation-map.yaml:L4090-L4099] |
| 186 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_fatigue_condition` | `independent` | [docs/registry/implementation-map.yaml:L4100-L4109] |
| 187 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `LoadScaler.process_sea_state` | `independent` | [docs/registry/implementation-map.yaml:L4110-L4119] |
| 188 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L4120-L4129] |
| 189 | `src/digitalmodel/structural/fatigue_apps/strut_foundation_processor_with_progress.py` | `ProductionDataHandlerWithProgress.process_single_analysis` | `independent` | [docs/registry/implementation-map.yaml:L4130-L4139] |
| 190 | `src/digitalmodel/structural/parametric_coordinator.py` | `ParametricCoordinator._run_wall_thickness` | `independent` | [docs/registry/implementation-map.yaml:L4140-L4149] |
| 191 | `src/digitalmodel/structural/pipe_capacity/common/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | `copy` | [docs/registry/implementation-map.yaml:L4150-L4159] |
| 192 | `src/digitalmodel/structural/pipe_capacity/custom/DNV_OS_F101/RigidPipe.py` | `DNVOSF101Pipe.localBucklingCombinedLoadingLoadControlled` | `copy` | [docs/registry/implementation-map.yaml:L4160-L4169] |
| 193 | `src/digitalmodel/structural/pipe_capacity/custom/PipeCapacity.py` | `VonMises_Pipe.init_calculate_required_properties` | `copy` | [docs/registry/implementation-map.yaml:L4170-L4179] |
| 194 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `simple_catenary` | `independent` | [docs/registry/implementation-map.yaml:L4180-L4189] |
| 195 | `src/digitalmodel/subsea/catenary_riser/cli.py` | `lazy_wave_analysis` | `independent` | [docs/registry/implementation-map.yaml:L4190-L4199] |
| 196 | `src/digitalmodel/subsea/catenary_riser/lazy_wave.py` | `LazyWaveAnalyzer.analyze` | `independent` | [docs/registry/implementation-map.yaml:L4200-L4209] |
| 197 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.analyze_riser` | `caller` | [docs/registry/implementation-map.yaml:L4210-L4222] |
| 198 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_catenary_from_H` | `independent` | [docs/registry/implementation-map.yaml:L4223-L4232] |
| 199 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer._solve_H_from_top_tension` | `independent` | [docs/registry/implementation-map.yaml:L4233-L4242] |
| 200 | `src/digitalmodel/subsea/catenary_riser/simple_catenary.py` | `SimpleCatenaryAnalyzer.check_design_limits` | `independent` | [docs/registry/implementation-map.yaml:L4243-L4252] |
| 201 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `developed_min_radius` | `independent` | [docs/registry/implementation-map.yaml:L4253-L4262] |
| 202 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `required_effective_ei` | `independent` | [docs/registry/implementation-map.yaml:L4263-L4272] |
| 203 | `src/digitalmodel/subsea/flexible_pipes/bend_stiffener.py` | `screen_bend_stiffener` | `independent` | [docs/registry/implementation-map.yaml:L4273-L4285] |
| 204 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `armour_tension_stress` | `independent` | [docs/registry/implementation-map.yaml:L4286-L4295] |
| 205 | `src/digitalmodel/subsea/flexible_pipes/design_checks_17J.py` | `verify_flexible_pipe` | `independent` | [docs/registry/implementation-map.yaml:L4296-L4307] |
| 206 | `src/digitalmodel/subsea/pipeline/global_buckling/hobbs_lateral.py` | `effective_driving_force` | `independent` | [docs/registry/implementation-map.yaml:L4308-L4317] |
| 207 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `RiserLoadCase.to_loads` | `independent` | [docs/registry/implementation-map.yaml:L4318-L4327] |
| 208 | `src/digitalmodel/subsea/riser/riser_case_screening.py` | `run_riser_screening` | `independent` | [docs/registry/implementation-map.yaml:L4328-L4337] |
| 209 | `src/digitalmodel/subsea/vertical_riser/legacy/orcaRiserStackup.py` | `StackUpDiagram` | `independent` | [docs/registry/implementation-map.yaml:L4338-L4347] |
| 210 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_riser_model` | `caller` | [docs/registry/implementation-map.yaml:L4348-L4362] |
| 211 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.read_riser_input_data` | `independent` | [docs/registry/implementation-map.yaml:L4363-L4372] |
| 212 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.assign_key_properties_to_self` | `independent` | [docs/registry/implementation-map.yaml:L4373-L4382] |
| 213 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.get_fea_properties` | `independent` | [docs/registry/implementation-map.yaml:L4383-L4392] |
| 214 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.prepare_tensioner_data` | `independent` | [docs/registry/implementation-map.yaml:L4393-L4402] |
| 215 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.evaluate_stack_up_properties` | `independent` | [docs/registry/implementation-map.yaml:L4403-L4412] |
| 216 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.save_data` | `independent` | [docs/registry/implementation-map.yaml:L4413-L4422] |
| 217 | `src/digitalmodel/subsea/vertical_riser/vertical_riser_components.py` | `VerticalRiser.alter_properties_for_Shear7_software` | `independent` | [docs/registry/implementation-map.yaml:L4423-L4432] |
| 218 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_natural_frequency` | `caller` | [docs/registry/implementation-map.yaml:L4433-L4444] |
| 219 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator._frequency_tension_controlled` | `independent` | [docs/registry/implementation-map.yaml:L4445-L4454] |
| 220 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.calculate_effective_tension` | `independent` | [docs/registry/implementation-map.yaml:L4455-L4464] |
| 221 | `src/digitalmodel/subsea/viv_analysis/frequency_calculator.py` | `FrequencyCalculator.check_stability` | `caller` | [docs/registry/implementation-map.yaml:L4465-L4476] |
| 222 | `src/digitalmodel/viv_screening/inputs.py` | `_beam` | `independent` | [docs/registry/implementation-map.yaml:L4477-L4486] |
| 223 | `src/digitalmodel/workflows/workflow_automation/cli.py` | `riser_analysis_cmd` | `independent` | [docs/registry/implementation-map.yaml:L4487-L4496] |
| 224 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_riser_analysis_template` | `independent` | [docs/registry/implementation-map.yaml:L4497-L4506] |
| 225 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `CompleteRiserAnalysisWorkflow.create` | `independent` | [docs/registry/implementation-map.yaml:L4507-L4516] |
| 226 | `tests/asset_integrity/test_calculations.py` | `_make_pipe_cfg` | `test_helper` | [docs/registry/implementation-map.yaml:L4517-L4526] |
| 227 | `tests/capabilities/test_wall_thickness_3d_page.py` | `_study` | `test_helper` | [docs/registry/implementation-map.yaml:L4527-L4536] |
| 228 | `tests/contracts/test_force_units_boundary_contract.py` | `(module-level)` | `test_helper` | [docs/registry/implementation-map.yaml:L4537-L4545] |
| 229 | `tests/contracts/test_force_units_boundary_contract.py` | `test_envelope_von_mises_n_to_kn_seam` | `test_helper` | [docs/registry/implementation-map.yaml:L4546-L4557] |
| 230 | `tests/drilling_riser/global_model/conftest.py` | `synthetic_spec` | `test_helper` | [docs/registry/implementation-map.yaml:L4558-L4567] |
| 231 | `tests/drilling_riser/global_model/test_batch_params.py` | `test_physical_checks_use_the_remaining_tensioners` | `test_helper` | [docs/registry/implementation-map.yaml:L4568-L4578] |
| 232 | `tests/drilling_riser/global_model/test_campaign.py` | `_fake` | `test_helper` | [docs/registry/implementation-map.yaml:L4579-L4589] |
| 233 | `tests/drilling_riser/global_model/test_open_water.py` | `open_water_dict` | `test_helper` | [docs/registry/implementation-map.yaml:L4590-L4599] |
| 234 | `tests/drilling_riser/global_model/test_open_water_campaign.py` | `_doc` | `test_helper` | [docs/registry/implementation-map.yaml:L4600-L4609] |
| 235 | `tests/drilling_riser/global_model/test_orcaflex_solver.py` | `test_statics_tension_and_modes_agree_with_hand_checks` | `test_helper` | [docs/registry/implementation-map.yaml:L4610-L4623] |
| 236 | `tests/drilling_riser/global_model/test_statics_path.py` | `test_branch_tolerance_and_ring_buoyancy` | `test_helper` | [docs/registry/implementation-map.yaml:L4624-L4634] |
| 237 | `tests/drilling_riser/global_model/test_tensioner_representation.py` | `_vertical` | `test_helper` | [docs/registry/implementation-map.yaml:L4635-L4644] |
| 238 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_mc` | `test_helper` | [docs/registry/implementation-map.yaml:L4645-L4654] |
| 239 | `tests/drilling_riser/postprocess/test_aggregate.py` | `_case` | `test_helper` | [docs/registry/implementation-map.yaml:L4655-L4666] |
| 240 | `tests/drilling_riser/test_drive_off_worked_example.py` | `_recorded_arguments` | `test_helper` | [docs/registry/implementation-map.yaml:L4667-L4676] |
| 241 | `tests/drilling_riser/test_schedule_assembly.py` | `_deltas` | `test_helper` | [docs/registry/implementation-map.yaml:L4677-L4686] |
| 242 | `tests/infrastructure/unit/test_pipe_capacity_codes.py` | `_base_cfg` | `test_helper` | [docs/registry/implementation-map.yaml:L4687-L4696] |
| 243 | `tests/orcaflex/test_generic_scr_slwr.py` | `test_analytical_reference_reproduces_from_basis` | `test_helper` | [docs/registry/implementation-map.yaml:L4697-L4706] |
| 244 | `tests/parametric/test_atlas.py` | `_code_check_atlas` | `test_helper` | [docs/registry/implementation-map.yaml:L4707-L4716] |
| 245 | `tests/reporting/test_riser_pipe_comparison.py` | `reference_input` | `test_helper` | [docs/registry/implementation-map.yaml:L4717-L4726] |
| 246 | `tests/riser_configuration_screen/test_workflow.py` | `settings` | `test_helper` | [docs/registry/implementation-map.yaml:L4727-L4736] |
| 247 | `tests/solvers/orcaflex/analysis/scripts/verify_configs.py` | `verify_configuration` | `test_helper` | [docs/registry/implementation-map.yaml:L4737-L4746] |
| 248 | `tests/solvers/orcaflex/modular_generator/builders/test_buoys_builder.py` | `_make_slay_spec` | `test_helper` | [docs/registry/implementation-map.yaml:L4747-L4756] |
| 249 | `tests/solvers/orcaflex/modular_generator/builders/test_lines_builder.py` | `_make_pipeline_spec` | `test_helper` | [docs/registry/implementation-map.yaml:L4757-L4766] |
| 250 | `tests/solvers/orcaflex/modular_generator/builders/test_linetype_builder.py` | `_make_pipeline_spec` | `test_helper` | [docs/registry/implementation-map.yaml:L4767-L4776] |
| 251 | `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_slay_campaign` | `test_helper` | [docs/registry/implementation-map.yaml:L4777-L4786] |
| 252 | `tests/solvers/orcaflex/modular_generator/schema/test_campaign.py` | `_make_base_spec_data` | `test_helper` | [docs/registry/implementation-map.yaml:L4787-L4796] |
| 253 | `tests/solvers/orcaflex/modular_generator/test_modular_vs_monolithic.py` | `extract_static_range_results` | `test_helper` | [docs/registry/implementation-map.yaml:L4797-L4806] |
| 254 | `tests/solvers/orcaflex/modular_generator/test_post_validator.py` | `_make_valid_model` | `test_helper` | [docs/registry/implementation-map.yaml:L4807-L4816] |
| 255 | `tests/solvers/orcaflex/test_slay_layout.py` | `test_support_types_are_defined_before_vessel_references` | `test_helper` | [docs/registry/implementation-map.yaml:L4817-L4826] |
| 256 | `tests/specialized/umbilical_analysis/conftest.py` | `installation_parameters` | `test_helper` | [docs/registry/implementation-map.yaml:L4827-L4836] |
| 257 | `tests/subsea/catenary_riser/test_catenary.py` | `run_catenary_riser` | `test_helper` | [docs/registry/implementation-map.yaml:L4837-L4846] |
| 258 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_catenary_analysis_mock` | `test_helper` | [docs/registry/implementation-map.yaml:L4847-L4856] |
| 259 | `tests/test_automation/scripts/batch_fix_tests.py` | `(module-level)` | `test_helper` | [docs/registry/implementation-map.yaml:L4857-L4865] |
| 260 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | `test_helper` | [docs/registry/implementation-map.yaml:L4866-L4875] |
| 261 | `tests/test_rainflow_with_effective_tension.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L4876-L4887] |
| 262 | `tests/test_wall_thickness_compare_workflow.py` | `_cfg` | `test_helper` | [docs/registry/implementation-map.yaml:L4888-L4897] |
| 263 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | `test_helper` | [docs/registry/implementation-map.yaml:L4898-L4907] |
| 264 | `tests/workflows/test_viv_screening_workflow.py` | `screening_input` | `test_helper` | [docs/registry/implementation-map.yaml:L4908-L4917] |
#### sn-curves-and-miner-damage

Exact compiled signal strings (Python string literals):

```python
import re
signals = [
    'rainflow|miner|cycles_to_failure|log_a|log10.*(?:cycles|stress)|(?:stress|stress_range|delta_sigma).*\\*\\*.*\\bm\\b|damage_sum|thickness_correction|endurance_limit|sn_curve|SNcurve|sn_data|stress_life',
    'S.N.curve|fatigue.damage|C203|BS.?7608|RP.?2A|SNcurve|SN.?Curve',
    'thickness_correction|endurance_limit|n_cycles|sn_curve|stress_ranges',
]
patterns = [re.compile(signal) for signal in signals]
```

| Hit definition | Classification and code-pin evidence |
| --- | --- |
| `src/digitalmodel/structural/spectral_fatigue.py` — `<module>` | Module prose or report-column declaration; helper. [src/digitalmodel/structural/spectral_fatigue.py:L2-L12] |
| `src/digitalmodel/structural/spectral_fatigue.py` — `FatigueDamageResult` | Data container or input validation; helper. [src/digitalmodel/structural/spectral_fatigue.py:L21-L26] |
| `src/digitalmodel/structural/spectral_fatigue.py` — `narrow_band_damage` | Listed local damage formula. [src/digitalmodel/structural/spectral_fatigue.py:L126-L176] |

| Map row | Path | Symbol | Map relation | Evidence at map revision |
| --- | --- | --- | --- | --- |
| 1 | `docs/domains/examples/orcaflex_signal_analysis_example.py` | `example_6_custom_workflow` | `independent` | [docs/registry/implementation-map.yaml:L16814-L16823] |
| 2 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_1_basic_rainflow_counting` | `independent` | [docs/registry/implementation-map.yaml:L16824-L16833] |
| 3 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_4_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L16834-L16843] |
| 4 | `docs/domains/examples/signal_analysis_usage_example.py` | `example_6_backward_compatibility` | `independent` | [docs/registry/implementation-map.yaml:L16844-L16853] |
| 5 | `docs/domains/examples/signal_analysis_usage_example.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L16854-L16867] |
| 6 | `examples/demos/demo_sn_library.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L16868-L16879] |
| 7 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_sn_curves` | `example` | [docs/registry/implementation-map.yaml:L16880-L16889] |
| 8 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_rainflow_counting` | `example` | [docs/registry/implementation-map.yaml:L16890-L16899] |
| 9 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_damage_accumulation` | `example` | [docs/registry/implementation-map.yaml:L16900-L16909] |
| 10 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_frequency_domain_analysis` | `example` | [docs/registry/implementation-map.yaml:L16910-L16919] |
| 11 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `demonstrate_quick_analysis` | `example` | [docs/registry/implementation-map.yaml:L16920-L16929] |
| 12 | `examples/domains/fatigue/advanced_examples/complete_fatigue_analysis.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L16930-L16945] |
| 13 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `list_curves` | `example` | [docs/registry/implementation-map.yaml:L16946-L16955] |
| 14 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_cli.py` | `compare_curves` | `example` | [docs/registry/implementation-map.yaml:L16956-L16965] |
| 15 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_2_linear_log` | `example` | [docs/registry/implementation-map.yaml:L16966-L16975] |
| 16 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_3_with_scf` | `example` | [docs/registry/implementation-map.yaml:L16976-L16985] |
| 17 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_4_without_fatigue_limit` | `example` | [docs/registry/implementation-map.yaml:L16986-L16995] |
| 18 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_5_dnv_comparison` | `example` | [docs/registry/implementation-map.yaml:L16996-L17005] |
| 19 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_6_abs_comparison` | `example` | [docs/registry/implementation-map.yaml:L17006-L17015] |
| 20 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_7_reference_comparison` | `example` | [docs/registry/implementation-map.yaml:L17016-L17025] |
| 21 | `examples/domains/fatigue/advanced_examples/plot_sn_curves_examples.py` | `example_8_joint_type_comparison` | `example` | [docs/registry/implementation-map.yaml:L17026-L17035] |
| 22 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_1_basic_sn_curves` | `example` | [docs/registry/implementation-map.yaml:L17036-L17045] |
| 23 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_2_multislope_curves` | `example` | [docs/registry/implementation-map.yaml:L17046-L17055] |
| 24 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_3_time_domain_analysis` | `example` | [docs/registry/implementation-map.yaml:L17056-L17065] |
| 25 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_4_frequency_domain_analysis` | `example` | [docs/registry/implementation-map.yaml:L17066-L17075] |
| 26 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_5_high_stress_range_analysis` | `example` | [docs/registry/implementation-map.yaml:L17076-L17085] |
| 27 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `example_6_shear_data_analysis` | `example` | [docs/registry/implementation-map.yaml:L17086-L17095] |
| 28 | `examples/domains/fatigue/fatigue_analysis_examples.py` | `run_all_examples` | `example` | [docs/registry/implementation-map.yaml:L17096-L17112] |
| 29 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_well_log_analysis` | `example` | [docs/registry/implementation-map.yaml:L17113-L17122] |
| 30 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `example_stratigraphic_correlation` | `example` | [docs/registry/implementation-map.yaml:L17123-L17132] |
| 31 | `examples/domains/reservoir/reservoir_analysis_examples.py` | `main` | `example` | [docs/registry/implementation-map.yaml:L17133-L17145] |
| 32 | `scripts/capabilities/build_onepagers.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L17146-L17154] |
| 33 | `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L17155-L17163] |
| 34 | `scripts/python/digitalmodel/tools/check_process_fatigue_single.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17164-L17173] |
| 35 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_analysis` | `independent` | [docs/registry/implementation-map.yaml:L17174-L17183] |
| 36 | `scripts/python/digitalmodel/tools/migrate_yaml_configs.py` | `YAMLConfigMigrator._migrate_time_series_components` | `independent` | [docs/registry/implementation-map.yaml:L17184-L17193] |
| 37 | `scripts/python/digitalmodel/tools/process_fatigue_data.py` | `process_fatigue_files` | `independent` | [docs/registry/implementation-map.yaml:L17194-L17203] |
| 38 | `scripts/python/digitalmodel/tools/process_fatigue_final.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17204-L17213] |
| 39 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_fatigue_file` | `independent` | [docs/registry/implementation-map.yaml:L17214-L17223] |
| 40 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `process_sample_file` | `independent` | [docs/registry/implementation-map.yaml:L17224-L17235] |
| 41 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_rainflow.py` | `batch_process_all_files` | `independent` | [docs/registry/implementation-map.yaml:L17236-L17246] |
| 42 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `rainflow_counting` | `independent` | [docs/registry/implementation-map.yaml:L17247-L17256] |
| 43 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `process_fatigue_file` | `independent` | [docs/registry/implementation-map.yaml:L17257-L17268] |
| 44 | `scripts/python/digitalmodel/tools/process_fatigue_simplified_standalone.py` | `batch_process` | `independent` | [docs/registry/implementation-map.yaml:L17269-L17280] |
| 45 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v2.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17281-L17290] |
| 46 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v3.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17291-L17300] |
| 47 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v4.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17301-L17310] |
| 48 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v5.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17311-L17320] |
| 49 | `scripts/python/digitalmodel/tools/process_fatigue_single_test_v6.py` | `process_single_test_file` | `independent` | [docs/registry/implementation-map.yaml:L17321-L17330] |
| 50 | `scripts/riser_database/build_tables.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L17331-L17339] |
| 51 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `SNcurve.from_dnv` | `independent` | [docs/registry/implementation-map.yaml:L17340-L17349] |
| 52 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L17350-L17359] |
| 53 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.calculate_fatigue_damage` | `independent` | [docs/registry/implementation-map.yaml:L17360-L17371] |
| 54 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.generate_sn_curve_apdl` | `independent` | [docs/registry/implementation-map.yaml:L17372-L17381] |
| 55 | `src/digitalmodel/ansys/fatigue_postprocessor.py` | `FatiguePostprocessor.create_damage_summary_df` | `independent` | [docs/registry/implementation-map.yaml:L17382-L17391] |
| 56 | `src/digitalmodel/benchmarks/inventory.py` | `build_model_inventory` | `independent` | [docs/registry/implementation-map.yaml:L17392-L17401] |
| 57 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `half_cycles` | `independent` | [docs/registry/implementation-map.yaml:L17402-L17411] |
| 58 | `src/digitalmodel/drilling_riser/global_model/fatigue_channels.py` | `extract` | `independent` | [docs/registry/implementation-map.yaml:L17412-L17423] |
| 59 | `src/digitalmodel/engine.py` | `engine` | `caller` | [docs/registry/implementation-map.yaml:L17424-L17441] |
| 60 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `router` | `independent` | [docs/registry/implementation-map.yaml:L17442-L17454] |
| 61 | `src/digitalmodel/fatg_spectral_fatigue/workflow.py` | `_sn_curve` | `independent` | [docs/registry/implementation-map.yaml:L17455-L17464] |
| 62 | `src/digitalmodel/fatigue/assessment.py` | `_case_histogram` | `independent` | [docs/registry/implementation-map.yaml:L17465-L17474] |
| 63 | `src/digitalmodel/fatigue/assessment.py` | `assess_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L17475-L17488] |
| 64 | `src/digitalmodel/fatigue/counting_contract.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L17489-L17497] |
| 65 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pylife_4pt` | `independent` | [docs/registry/implementation-map.yaml:L17498-L17507] |
| 66 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_pypi_rainflow` | `independent` | [docs/registry/implementation-map.yaml:L17508-L17517] |
| 67 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_sigproc_astm` | `independent` | [docs/registry/implementation-map.yaml:L17518-L17527] |
| 68 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_calm_buoy` | `independent` | [docs/registry/implementation-map.yaml:L17528-L17537] |
| 69 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counting` | `independent` | [docs/registry/implementation-map.yaml:L17538-L17547] |
| 70 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_fapps_counter` | `independent` | [docs/registry/implementation-map.yaml:L17548-L17557] |
| 71 | `src/digitalmodel/fatigue/counting_contract.py` | `_adapt_struct_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L17558-L17567] |
| 72 | `src/digitalmodel/fatigue/counting_contract.py` | `_is_counting_name` | `independent` | [docs/registry/implementation-map.yaml:L17568-L17577] |
| 73 | `src/digitalmodel/fatigue/damage.py` | `miner_damage` | `independent` | [docs/registry/implementation-map.yaml:L17578-L17587] |
| 74 | `src/digitalmodel/fatigue/damage.py` | `design_life_check` | `independent` | [docs/registry/implementation-map.yaml:L17588-L17599] |
| 75 | `src/digitalmodel/fatigue/environmental_correction.py` | `environment_correction` | `independent` | [docs/registry/implementation-map.yaml:L17600-L17609] |
| 76 | `src/digitalmodel/fatigue/environmental_correction.py` | `apply_environment_to_sn` | `independent` | [docs/registry/implementation-map.yaml:L17610-L17621] |
| 77 | `src/digitalmodel/fatigue/fatg.py` | `fatg_sea_state_damage` | `independent` | [docs/registry/implementation-map.yaml:L17622-L17631] |
| 78 | `src/digitalmodel/fatigue/fatg.py` | `fatg_annual_damage` | `independent` | [docs/registry/implementation-map.yaml:L17632-L17643] |
| 79 | `src/digitalmodel/fatigue/fatigue_reporting.py` | `_sec_fatigue_locations` | `independent` | [docs/registry/implementation-map.yaml:L17644-L17653] |
| 80 | `src/digitalmodel/fatigue/rainflow.py` | `fatigue_life` | `independent` | [docs/registry/implementation-map.yaml:L17654-L17666] |
| 81 | `src/digitalmodel/fatigue/sn_curves.py` | `_compute_sd` | `independent` | [docs/registry/implementation-map.yaml:L17667-L17676] |
| 82 | `src/digitalmodel/fatigue/sn_curves.py` | `_build_curve_dict` | `caller` | [docs/registry/implementation-map.yaml:L17677-L17688] |
| 83 | `src/digitalmodel/fatigue/sn_curves.py` | `get_sn_curve` | `caller` | [docs/registry/implementation-map.yaml:L17689-L17699] |
| 84 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.knee_stress` | `wrapper` | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 85 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.switch_stress` | `wrapper` | [docs/registry/implementation-map.yaml:L17712-L17723] |
| 86 | `src/digitalmodel/fatigue/sn_library.py` | `SNCurveRecord.cycles` | `wrapper` | [docs/registry/implementation-map.yaml:L17724-L17735] |
| 87 | `src/digitalmodel/fatigue/sn_library.py` | `_knee_stress` | `independent` | [docs/registry/implementation-map.yaml:L17736-L17745] |
| 88 | `src/digitalmodel/fatigue/sn_library.py` | `_switch_stress` | `caller` | [docs/registry/implementation-map.yaml:L17746-L17756] |
| 89 | `src/digitalmodel/fatigue/sn_library.py` | `_bilinear_cycles` | `independent` | [docs/registry/implementation-map.yaml:L17757-L17766] |
| 90 | `src/digitalmodel/fatigue/sn_library.py` | `_sd` | `independent` | [docs/registry/implementation-map.yaml:L17767-L17776] |
| 91 | `src/digitalmodel/fatigue/sn_library.py` | `_build_dnv_curves` | `independent` | [docs/registry/implementation-map.yaml:L17777-L17786] |
| 92 | `src/digitalmodel/fatigue/sn_library.py` | `_build_api_curves` | `independent` | [docs/registry/implementation-map.yaml:L17787-L17796] |
| 93 | `src/digitalmodel/fatigue/sn_library.py` | `_build_bs7608_curves` | `independent` | [docs/registry/implementation-map.yaml:L17797-L17808] |
| 94 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iiw_curves` | `independent` | [docs/registry/implementation-map.yaml:L17809-L17819] |
| 95 | `src/digitalmodel/fatigue/sn_library.py` | `_build_eurocode3_curves` | `independent` | [docs/registry/implementation-map.yaml:L17820-L17830] |
| 96 | `src/digitalmodel/fatigue/sn_library.py` | `_build_norsok_curves` | `caller` | [docs/registry/implementation-map.yaml:L17831-L17841] |
| 97 | `src/digitalmodel/fatigue/sn_library.py` | `_build_aws_curves` | `independent` | [docs/registry/implementation-map.yaml:L17842-L17851] |
| 98 | `src/digitalmodel/fatigue/sn_library.py` | `_build_asme_curves` | `independent` | [docs/registry/implementation-map.yaml:L17852-L17861] |
| 99 | `src/digitalmodel/fatigue/sn_library.py` | `_build_en13445_curves` | `independent` | [docs/registry/implementation-map.yaml:L17862-L17872] |
| 100 | `src/digitalmodel/fatigue/sn_library.py` | `_build_pd5500_curves` | `caller` | [docs/registry/implementation-map.yaml:L17873-L17883] |
| 101 | `src/digitalmodel/fatigue/sn_library.py` | `_build_abs_curves` | `independent` | [docs/registry/implementation-map.yaml:L17884-L17893] |
| 102 | `src/digitalmodel/fatigue/sn_library.py` | `_build_iso19902_curves` | `independent` | [docs/registry/implementation-map.yaml:L17894-L17904] |
| 103 | `src/digitalmodel/fatigue/sn_library.py` | `_build_csa_curves` | `independent` | [docs/registry/implementation-map.yaml:L17905-L17914] |
| 104 | `src/digitalmodel/fatigue/sn_library.py` | `_build_hse_curves` | `independent` | [docs/registry/implementation-map.yaml:L17915-L17925] |
| 105 | `src/digitalmodel/fatigue/sn_library.py` | `_build_doe_curves` | `caller` | [docs/registry/implementation-map.yaml:L17926-L17936] |
| 106 | `src/digitalmodel/fatigue/sn_library.py` | `_build_gl_curves` | `independent` | [docs/registry/implementation-map.yaml:L17937-L17946] |
| 107 | `src/digitalmodel/fatigue/sn_library.py` | `_build_jis_curves` | `independent` | [docs/registry/implementation-map.yaml:L17947-L17956] |
| 108 | `src/digitalmodel/fatigue/sn_library.py` | `_ensure_loaded` | `independent` | [docs/registry/implementation-map.yaml:L17957-L17966] |
| 109 | `src/digitalmodel/fatigue/sn_library.py` | `summary_table` | `caller` | [docs/registry/implementation-map.yaml:L17967-L17978] |
| 110 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_curve_info` | `independent` | [docs/registry/implementation-map.yaml:L17979-L17988] |
| 111 | `src/digitalmodel/fatigue/sn_library_api.py` | `_record_to_sncurve` | `independent` | [docs/registry/implementation-map.yaml:L17989-L17998] |
| 112 | `src/digitalmodel/fatigue/sn_library_api.py` | `get_curve` | `wrapper` | [docs/registry/implementation-map.yaml:L17999-L18010] |
| 113 | `src/digitalmodel/fatigue/sn_library_api.py` | `calculate_endurance` | `wrapper` | [docs/registry/implementation-map.yaml:L18011-L18023] |
| 114 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `wirsching_light_damage` | `independent` | [docs/registry/implementation-map.yaml:L18024-L18033] |
| 115 | `src/digitalmodel/fatigue/spectral_fatigue.py` | `benasciutti_tovo_damage` | `independent` | [docs/registry/implementation-map.yaml:L18034-L18043] |
| 116 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `weld_fatigue_quickcheck` | `independent` | [docs/registry/implementation-map.yaml:L18044-L18054] |
| 117 | `src/digitalmodel/fatigue/weld_fatigue_quickcheck.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L18055-L18064] |
| 118 | `src/digitalmodel/fatigue/woehler_fitting.py` | `generate_test_data` | `independent` | [docs/registry/implementation-map.yaml:L18065-L18074] |
| 119 | `src/digitalmodel/fatigue/woehler_fitting.py` | `_fallback_log_regression` | `independent` | [docs/registry/implementation-map.yaml:L18075-L18084] |
| 120 | `src/digitalmodel/fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18085-L18102] |
| 121 | `src/digitalmodel/fatigue/workflow.py` | `_curve_ids` | `independent` | [docs/registry/implementation-map.yaml:L18103-L18112] |
| 122 | `src/digitalmodel/fatigue/workflow.py` | `_stress_ranges` | `independent` | [docs/registry/implementation-map.yaml:L18113-L18122] |
| 123 | `src/digitalmodel/fatigue/workflow.py` | `_curve_summary` | `independent` | [docs/registry/implementation-map.yaml:L18123-L18132] |
| 124 | `src/digitalmodel/fatigue/workflow.py` | `_curve_csv_path` | `independent` | [docs/registry/implementation-map.yaml:L18133-L18144] |
| 125 | `src/digitalmodel/fatigue/workflow.py` | `_input_stem` | `independent` | [docs/registry/implementation-map.yaml:L18145-L18154] |
| 126 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.router` | `caller` | [docs/registry/implementation-map.yaml:L18155-L18167] |
| 127 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_sn_data` | `independent` | [docs/registry/implementation-map.yaml:L18168-L18177] |
| 128 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_rainflow_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18178-L18187] |
| 129 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.damage_from_timetrace` | `independent` | [docs/registry/implementation-map.yaml:L18188-L18200] |
| 130 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis.py` | `FatigueAnalysis.get_rainflow_from_timetrace` | `independent` | [docs/registry/implementation-map.yaml:L18201-L18210] |
| 131 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18211-L18220] |
| 132 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.router` | `wrapper` | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 133 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_lcf_timetrace_rainflow_and_fatigue_damage` | `independent` | [docs/registry/implementation-map.yaml:L18233-L18244] |
| 134 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_damage_output_df_for_timetrace` | `wrapper` | [docs/registry/implementation-map.yaml:L18245-L18256] |
| 135 | `src/digitalmodel/infrastructure/base_solvers/marine/ship/ship_fatigue_analysis.py` | `ShipFatigueAnalysis.get_fatigue_df_definition_for_timetrace` | `independent` | [docs/registry/implementation-map.yaml:L18257-L18266] |
| 136 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.orcaflex_wave_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18267-L18276] |
| 137 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `SNcurve.cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L18277-L18286] |
| 138 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `RainflowFatigue.count_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18287-L18296] |
| 139 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `MinersRuleDamage.compute` | `independent` | [docs/registry/implementation-map.yaml:L18297-L18306] |
| 140 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `ScatterDiagramFatigue.compute` | `independent` | [docs/registry/implementation-map.yaml:L18307-L18316] |
| 141 | `src/digitalmodel/marine_ops/marine_engineering/calm_buoy_fatigue.py` | `compute_fatigue_life` | `independent` | [docs/registry/implementation-map.yaml:L18317-L18326] |
| 142 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.add_well_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18327-L18336] |
| 143 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.set_well_order` | `independent` | [docs/registry/implementation-map.yaml:L18337-L18346] |
| 144 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `StratigraphicAnalysis.create_flattened_section` | `independent` | [docs/registry/implementation-map.yaml:L18347-L18356] |
| 145 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `WellCorrelation.add_well_data` | `independent` | [docs/registry/implementation-map.yaml:L18357-L18366] |
| 146 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `CoreAnalysis.correlate_core_to_logs` | `independent` | [docs/registry/implementation-map.yaml:L18367-L18376] |
| 147 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.add_well_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18377-L18386] |
| 148 | `src/digitalmodel/marine_ops/reservoir/analysis.py` | `ReservoirAnalysis.run_comprehensive_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18387-L18396] |
| 149 | `src/digitalmodel/mooring_fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18397-L18410] |
| 150 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_evaluate_line` | `independent` | [docs/registry/implementation-map.yaml:L18411-L18421] |
| 151 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_history_bins` | `independent` | [docs/registry/implementation-map.yaml:L18422-L18431] |
| 152 | `src/digitalmodel/mooring_fatigue/workflow.py` | `_curve_settings` | `independent` | [docs/registry/implementation-map.yaml:L18432-L18441] |
| 153 | `src/digitalmodel/parametric/generate.py` | `_mooring_fatigue_damage` | `independent` | [docs/registry/implementation-map.yaml:L18442-L18452] |
| 154 | `src/digitalmodel/parametric/generate.py` | `_free_span_utilisation` | `independent` | [docs/registry/implementation-map.yaml:L18453-L18462] |
| 155 | `src/digitalmodel/parametric/generate.py` | `_span_rectification_utilisation` | `independent` | [docs/registry/implementation-map.yaml:L18463-L18472] |
| 156 | `src/digitalmodel/parametric/refresh.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L18473-L18481] |
| 157 | `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18482-L18493] |
| 158 | `src/digitalmodel/riser_fatigue/touchdown.py` | `assess_touchdown_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L18494-L18503] |
| 159 | `src/digitalmodel/riser_fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18504-L18515] |
| 160 | `src/digitalmodel/riser_fatigue/workflow.py` | `_viv_damage` | `independent` | [docs/registry/implementation-map.yaml:L18516-L18526] |
| 161 | `src/digitalmodel/riser_fatigue/workflow.py` | `_curve_settings` | `independent` | [docs/registry/implementation-map.yaml:L18527-L18536] |
| 162 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` | `independent` | [docs/registry/implementation-map.yaml:L18537-L18546] |
| 163 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.get_rainflow_from_timetrace` | `independent` | [docs/registry/implementation-map.yaml:L18547-L18556] |
| 164 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `FatigueAnalysisAdapter.damage_from_rainflow_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18557-L18566] |
| 165 | `src/digitalmodel/signal_processing/signal_analysis/adapters.py` | `OrcaFlexAdapter.RainflowHalfCycles` | `independent` | [docs/registry/implementation-map.yaml:L18567-L18576] |
| 166 | `src/digitalmodel/signal_processing/signal_analysis/cli.py` | `rainflow_cmd` | `independent` | [docs/registry/implementation-map.yaml:L18577-L18586] |
| 167 | `src/digitalmodel/signal_processing/signal_analysis/core/rainflow.py` | `RainflowCounter.count_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18587-L18596] |
| 168 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve._bilinear` | `independent` | [docs/registry/implementation-map.yaml:L18597-L18606] |
| 169 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/curves.py` | `SNCurve.from_test_data` | `independent` | [docs/registry/implementation-map.yaml:L18607-L18616] |
| 170 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L18617-L18632] |
| 171 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._miners_rule` | `independent` | [docs/registry/implementation-map.yaml:L18633-L18644] |
| 172 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._modified_miners_rule` | `independent` | [docs/registry/implementation-map.yaml:L18645-L18655] |
| 173 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._nonlinear_damage` | `independent` | [docs/registry/implementation-map.yaml:L18656-L18666] |
| 174 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._calculate_allowable_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18667-L18676] |
| 175 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._apply_mean_stress_correction` | `independent` | [docs/registry/implementation-map.yaml:L18677-L18686] |
| 176 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator._estimate_cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L18687-L18696] |
| 177 | `src/digitalmodel/signal_processing/signal_analysis/fatigue/damage.py` | `FatigueDamageCalculator.calculate_equivalent_stress` | `independent` | [docs/registry/implementation-map.yaml:L18697-L18706] |
| 178 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `create_parser` | `independent` | [docs/registry/implementation-map.yaml:L18707-L18716] |
| 179 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `build_config_from_args` | `independent` | [docs/registry/implementation-map.yaml:L18717-L18726] |
| 180 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/__main__.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L18727-L18739] |
| 181 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._analyze_column` | `independent` | [docs/registry/implementation-map.yaml:L18740-L18749] |
| 182 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_outputs` | `independent` | [docs/registry/implementation-map.yaml:L18750-L18759] |
| 183 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/analyzer.py` | `TimeSeriesAnalyzer._generate_summary` | `independent` | [docs/registry/implementation-map.yaml:L18760-L18769] |
| 184 | `src/digitalmodel/signal_processing/signal_analysis/orcaflex/error_handling.py` | `SafeAnalyzer.safe_rainflow_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18770-L18779] |
| 185 | `src/digitalmodel/signal_processing/time_series/time_series_analysis.py` | `TimeSeriesAnalysis.router` | `independent` | [docs/registry/implementation-map.yaml:L18780-L18789] |
| 186 | `src/digitalmodel/signal_processing/time_series/time_series_components.py` | `TimeSeriesComponents.count_cycles` | `independent` | [docs/registry/implementation-map.yaml:L18790-L18799] |
| 187 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_histogram_object_from_orcaflex_run` | `independent` | [docs/registry/implementation-map.yaml:L18800-L18809] |
| 188 | `src/digitalmodel/solvers/orcaflex/opp_time_series.py` | `OPPTimeSeries.get_detailed_histogram_df_columns` | `independent` | [docs/registry/implementation-map.yaml:L18810-L18819] |
| 189 | `src/digitalmodel/solvers/orcaflex/opp_time_series_v2.py` | `OPPTimeSeriesV2.process_histograms` | `independent` | [docs/registry/implementation-map.yaml:L18820-L18829] |
| 190 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` | `independent` | [docs/registry/implementation-map.yaml:L18830-L18839] |
| 191 | `src/digitalmodel/solvers/orcaflex/orcaflex_analysis_components.py` | `OrcaFlexAnalysis.get_detailed_histogram_df_columns` | `copy` | [docs/registry/implementation-map.yaml:L18840-L18849] |
| 192 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._perform_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18850-L18861] |
| 193 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._parallel_fatigue_analysis` | `independent` | [docs/registry/implementation-map.yaml:L18862-L18871] |
| 194 | `src/digitalmodel/solvers/orcaflex/time_trace_processor.py` | `OrcaFlexTimeTraceProcessor._analyze_single_trace` | `independent` | [docs/registry/implementation-map.yaml:L18872-L18881] |
| 195 | `src/digitalmodel/spectral_fatigue/workflow.py` | `router` | `caller` | [docs/registry/implementation-map.yaml:L18882-L18893] |
| 196 | `src/digitalmodel/spectral_fatigue/workflow.py` | `_sn_curve` | `independent` | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 197 | `src/digitalmodel/structural/fatigue/__init__.py` | `quick_fatigue_analysis` | `caller` | [docs/registry/implementation-map.yaml:L18904-L18917] |
| 198 | `src/digitalmodel/structural/fatigue/__init__.py` | `get_module_info` | `independent` | [docs/registry/implementation-map.yaml:L18918-L18927] |
| 199 | `src/digitalmodel/structural/fatigue/analysis.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L18928-L18936] |
| 200 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_sn_curve` | `wrapper` | [docs/registry/implementation-map.yaml:L18937-L18947] |
| 201 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_damage_calculator` | `independent` | [docs/registry/implementation-map.yaml:L18948-L18957] |
| 202 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._setup_rainflow_counter` | `independent` | [docs/registry/implementation-map.yaml:L18958-L18967] |
| 203 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_time_series` | `independent` | [docs/registry/implementation-map.yaml:L18968-L18979] |
| 204 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` | `independent` | [docs/registry/implementation-map.yaml:L18980-L18989] |
| 205 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.calculate_high_stress_range_life` | `independent` | [docs/registry/implementation-map.yaml:L18990-L18999] |
| 206 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_shear_data` | `independent` | [docs/registry/implementation-map.yaml:L19000-L19009] |
| 207 | `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine._perform_engineering_validation` | `independent` | [docs/registry/implementation-map.yaml:L19010-L19019] |
| 208 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_time_domain_analysis` | `independent` | [docs/registry/implementation-map.yaml:L19020-L19029] |
| 209 | `src/digitalmodel/structural/fatigue/analysis.py` | `quick_frequency_domain_analysis` | `independent` | [docs/registry/implementation-map.yaml:L19030-L19039] |
| 210 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L19040-L19048] |
| 211 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19049-L19060] |
| 212 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `LinearDamageAccumulation._apply_mean_stress_correction` | `independent` | [docs/registry/implementation-map.yaml:L19061-L19070] |
| 213 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `ModifiedMinersRule.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 214 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation.calculate_damage` | `wrapper` | [docs/registry/implementation-map.yaml:L19081-L19094] |
| 215 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._marco_starkey_damage` | `independent` | [docs/registry/implementation-map.yaml:L19095-L19104] |
| 216 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._corten_dolan_damage` | `independent` | [docs/registry/implementation-map.yaml:L19105-L19114] |
| 217 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `NonlinearDamageAccumulation._double_linear_damage` | `independent` | [docs/registry/implementation-map.yaml:L19115-L19124] |
| 218 | `src/digitalmodel/structural/fatigue/damage_accumulation.py` | `compare_damage_methods` | `independent` | [docs/registry/implementation-map.yaml:L19125-L19134] |
| 219 | `src/digitalmodel/structural/fatigue/design_code_report.py` | `_build_curve` | `independent` | [docs/registry/implementation-map.yaml:L19135-L19144] |
| 220 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `failure_probability` | `independent` | [docs/registry/implementation-map.yaml:L19145-L19154] |
| 221 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `sensitivity_analysis` | `independent` | [docs/registry/implementation-map.yaml:L19155-L19164] |
| 222 | `src/digitalmodel/structural/fatigue/fatigue_reliability.py` | `monte_carlo_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L19165-L19174] |
| 223 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L19175-L19183] |
| 224 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19184-L19193] |
| 225 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `DirlikMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19194-L19203] |
| 226 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `TovoBenasciuttiMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19204-L19213] |
| 227 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `SingleMomentMethod.calculate_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19214-L19223] |
| 228 | `src/digitalmodel/structural/fatigue/frequency_domain.py` | `compare_frequency_methods` | `independent` | [docs/registry/implementation-map.yaml:L19224-L19233] |
| 229 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `run_sweep` | `independent` | [docs/registry/implementation-map.yaml:L19234-L19243] |
| 230 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `compute_sensitivity._single_damage` | `independent` | [docs/registry/implementation-map.yaml:L19244-L19253] |
| 231 | `src/digitalmodel/structural/fatigue/parametric_sweep.py` | `_build_scatter_json` | `independent` | [docs/registry/implementation-map.yaml:L19254-L19263] |
| 232 | `src/digitalmodel/structural/fatigue/rainflow.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L19264-L19272] |
| 233 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.count_cycles` | `caller` | [docs/registry/implementation-map.yaml:L19273-L19284] |
| 234 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter._rainflow_count` | `independent` | [docs/registry/implementation-map.yaml:L19285-L19294] |
| 235 | `src/digitalmodel/structural/fatigue/rainflow.py` | `RainflowCounter.extract_cycles_with_means` | `caller` | [docs/registry/implementation-map.yaml:L19295-L19306] |
| 236 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_count` | `independent` | [docs/registry/implementation-map.yaml:L19307-L19316] |
| 237 | `src/digitalmodel/structural/fatigue/rainflow.py` | `rainflow_with_means` | `independent` | [docs/registry/implementation-map.yaml:L19317-L19326] |
| 238 | `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `scatter_fatigue_damage` | `independent` | [docs/registry/implementation-map.yaml:L19327-L19336] |
| 239 | `src/digitalmodel/structural/fatigue/skill.py` | `fatigue_assessment` | `independent` | [docs/registry/implementation-map.yaml:L19337-L19348] |
| 240 | `src/digitalmodel/structural/fatigue/sn_curve_plotter.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19349-L19358] |
| 241 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L19359-L19367] |
| 242 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves._ensure_loaded` | `independent` | [docs/registry/implementation-map.yaml:L19368-L19377] |
| 243 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.register_custom_curve` | `independent` | [docs/registry/implementation-map.yaml:L19378-L19387] |
| 244 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_dnv_c203_curve` | `independent` | [docs/registry/implementation-map.yaml:L19388-L19397] |
| 245 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_curve` | `independent` | [docs/registry/implementation-map.yaml:L19398-L19407] |
| 246 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `StandardSNCurves.get_multislope_curve` | `independent` | [docs/registry/implementation-map.yaml:L19408-L19417] |
| 247 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `ThicknessCorrection.apply_thickness_effect` | `independent` | [docs/registry/implementation-map.yaml:L19418-L19427] |
| 248 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `SNDataFitting.fit_power_law` | `independent` | [docs/registry/implementation-map.yaml:L19428-L19437] |
| 249 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_dnv_curve` | `independent` | [docs/registry/implementation-map.yaml:L19438-L19447] |
| 250 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_api_curve` | `independent` | [docs/registry/implementation-map.yaml:L19448-L19457] |
| 251 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `get_bs_curve` | `independent` | [docs/registry/implementation-map.yaml:L19458-L19467] |
| 252 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `_dnv_curves_from_c203` | `independent` | [docs/registry/implementation-map.yaml:L19468-L19477] |
| 253 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_cp` | `independent` | [docs/registry/implementation-map.yaml:L19478-L19487] |
| 254 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_dnv_seawater_free` | `independent` | [docs/registry/implementation-map.yaml:L19488-L19497] |
| 255 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_chain_seawater_cp` | `independent` | [docs/registry/implementation-map.yaml:L19498-L19507] |
| 256 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `pipeline_girth_weld` | `caller` | [docs/registry/implementation-map.yaml:L19508-L19520] |
| 257 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `scr_touchdown` | `caller` | [docs/registry/implementation-map.yaml:L19521-L19533] |
| 258 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `mooring_chain` | `caller` | [docs/registry/implementation-map.yaml:L19534-L19546] |
| 259 | `src/digitalmodel/structural/fatigue/worked_examples.py` | `_compute_example` | `independent` | [docs/registry/implementation-map.yaml:L19547-L19556] |
| 260 | `src/digitalmodel/structural/fatigue_apps/__main__.py` | `run_analysis` | `independent` | [docs/registry/implementation-map.yaml:L19557-L19568] |
| 261 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve._load_sn_parameters` | `independent` | [docs/registry/implementation-map.yaml:L19569-L19578] |
| 262 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `SNCurve.calculate_cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L19579-L19588] |
| 263 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `FatigueDamageCalculator.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19589-L19598] |
| 264 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19599-L19608] |
| 265 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L19609-L19617] |
| 266 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a1` | `independent` | [docs/registry/implementation-map.yaml:L19618-L19627] |
| 267 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `SNCurveParameters.a2` | `independent` | [docs/registry/implementation-map.yaml:L19628-L19637] |
| 268 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.cycles_to_failure` | `independent` | [docs/registry/implementation-map.yaml:L19638-L19647] |
| 269 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19648-L19659] |
| 270 | `src/digitalmodel/structural/fatigue_apps/fatigue_damage_calculator.py` | `FatigueDamageCalculator.export_results` | `independent` | [docs/registry/implementation-map.yaml:L19660-L19669] |
| 271 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `FatigueFileNamer.get_rainflow_filename` | `independent` | [docs/registry/implementation-map.yaml:L19670-L19679] |
| 272 | `src/digitalmodel/structural/fatigue_apps/file_namer.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L19680-L19688] |
| 273 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `IntegratedFatigueProcessor.process_fatigue_condition` | `independent` | [docs/registry/implementation-map.yaml:L19689-L19698] |
| 274 | `src/digitalmodel/structural/fatigue_apps/integrated_processor.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19699-L19708] |
| 275 | `src/digitalmodel/structural/fatigue_apps/integrated_processor_with_naming.py` | `IntegratedFatigueProcessorWithNaming._process_condition_base` | `independent` | [docs/registry/implementation-map.yaml:L19709-L19718] |
| 276 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `(module-level)` | `independent` | [docs/registry/implementation-map.yaml:L19719-L19727] |
| 277 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.count_cycles` | `caller` | [docs/registry/implementation-map.yaml:L19728-L19739] |
| 278 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter._rainflow_algorithm` | `independent` | [docs/registry/implementation-map.yaml:L19740-L19749] |
| 279 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `RainflowCounter.export_results` | `independent` | [docs/registry/implementation-map.yaml:L19750-L19759] |
| 280 | `src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` | `rainflow_count` | `independent` | [docs/registry/implementation-map.yaml:L19760-L19769] |
| 281 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_time_series` | `independent` | [docs/registry/implementation-map.yaml:L19770-L19779] |
| 282 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.process_batch` | `independent` | [docs/registry/implementation-map.yaml:L19780-L19791] |
| 283 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `RainflowCounter.save_cycle_data` | `independent` | [docs/registry/implementation-map.yaml:L19792-L19801] |
| 284 | `src/digitalmodel/structural/fatigue_apps/rainflow_counting.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19802-L19811] |
| 285 | `src/digitalmodel/structural/fatigue_apps/reference_seastate_processor.py` | `ReferenceSeaStateProcessor.process_fatigue_condition` | `independent` | [docs/registry/implementation-map.yaml:L19812-L19821] |
| 286 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `TensionStressConverter.process_batch` | `independent` | [docs/registry/implementation-map.yaml:L19822-L19831] |
| 287 | `src/digitalmodel/structural/fatigue_apps/tension_stress_conversion.py` | `main` | `independent` | [docs/registry/implementation-map.yaml:L19832-L19841] |
| 288 | `src/digitalmodel/structural/fatigue_apps/visualizer.py` | `generate_all_visualizations` | `independent` | [docs/registry/implementation-map.yaml:L19842-L19851] |
| 289 | `src/digitalmodel/structural/spectral_fatigue.py` | `narrow_band_damage` | `independent` | [docs/registry/implementation-map.yaml:L19852-L19861] |
| 290 | `src/digitalmodel/subsea/connectors/fatigue.py` | `assess_jumper_fatigue` | `caller` | [docs/registry/implementation-map.yaml:L19862-L19873] |
| 291 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `_dnv_params` | `independent` | [docs/registry/implementation-map.yaml:L19874-L19883] |
| 292 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `get_sn_curve` | `independent` | [docs/registry/implementation-map.yaml:L19884-L19895] |
| 293 | `src/digitalmodel/subsea/pipeline/free_span/span_fatigue_damage.py` | `SpanFatigueDamage._load_sn_curve` | `caller` | [docs/registry/implementation-map.yaml:L19896-L19907] |
| 294 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_damage` | `independent` | [docs/registry/implementation-map.yaml:L19908-L19917] |
| 295 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.calculate_fatigue_life` | `independent` | [docs/registry/implementation-map.yaml:L19918-L19927] |
| 296 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.analyze_viv_fatigue` | `independent` | [docs/registry/implementation-map.yaml:L19928-L19940] |
| 297 | `src/digitalmodel/subsea/viv_analysis/fatigue.py` | `VIVFatigueCalculator.cumulative_damage` | `caller` | [docs/registry/implementation-map.yaml:L19941-L19952] |
| 298 | `src/digitalmodel/viv_screening/inputs.py` | `parse_inputs` | `independent` | [docs/registry/implementation-map.yaml:L19953-L19962] |
| 299 | `src/digitalmodel/viv_screening/workflow.py` | `router` | `independent` | [docs/registry/implementation-map.yaml:L19963-L19976] |
| 300 | `src/digitalmodel/viv_screening/workflow.py` | `_damage_rate` | `independent` | [docs/registry/implementation-map.yaml:L19977-L19987] |
| 301 | `src/digitalmodel/viv_screening/workflow.py` | `_provenance` | `independent` | [docs/registry/implementation-map.yaml:L19988-L19997] |
| 302 | `src/digitalmodel/web/digitaltwinfeed/app.py` | `page` | `independent` | [docs/registry/implementation-map.yaml:L19998-L20007] |
| 303 | `tests/ansys/padeye_pressure_bundle.py` | `_check_receipt` | `test_helper` | [docs/registry/implementation-map.yaml:L20008-L20017] |
| 304 | `tests/ansys/padeye_pressure_bundle.py` | `verify_pressure_bundle` | `test_helper` | [docs/registry/implementation-map.yaml:L20018-L20029] |
| 305 | `tests/ansys/test_fatigue_postprocessor.py` | `TestThicknessCorrection.test_known_correction` | `test_helper` | [docs/registry/implementation-map.yaml:L20030-L20039] |
| 306 | `tests/ansys/test_padeye_pressure_bundle.py` | `bundle` | `test_helper` | [docs/registry/implementation-map.yaml:L20040-L20049] |
| 307 | `tests/fatigue/test_assessment.py` | `histogram_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20050-L20061] |
| 308 | `tests/fatigue/test_c203_consumers.py` | `_span_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20062-L20071] |
| 309 | `tests/fatigue/test_c203_sn_values.py` | `_non_dnv_bilinear` | `test_helper` | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 310 | `tests/fatigue/test_c203_sn_values.py` | `test_non_dnv_bilinear_switch_stays_at_endurance_limit` | `test_helper` | [docs/registry/implementation-map.yaml:L20082-L20095] |
| 311 | `tests/fatigue/test_c203_variable_amplitude.py` | `_signal_e` | `test_helper` | [docs/registry/implementation-map.yaml:L20096-L20105] |
| 312 | `tests/fatigue/test_c203_variable_amplitude.py` | `_user_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20106-L20115] |
| 313 | `tests/fatigue/test_c203_variable_amplitude.py` | `_span_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20116-L20125] |
| 314 | `tests/fatigue/test_c203_variable_amplitude.py` | `_fresh_standard_curves` | `test_helper` | [docs/registry/implementation-map.yaml:L20126-L20135] |
| 315 | `tests/fatigue/test_c203_variable_amplitude.py` | `_data_dir` | `test_helper` | [docs/registry/implementation-map.yaml:L20136-L20145] |
| 316 | `tests/fatigue/test_damage.py` | `f_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20146-L20156] |
| 317 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_above_ref` | `test_helper` | [docs/registry/implementation-map.yaml:L20157-L20166] |
| 318 | `tests/fatigue/test_damage.py` | `TestThicknessCorrection.test_correction_formula` | `test_helper` | [docs/registry/implementation-map.yaml:L20167-L20176] |
| 319 | `tests/fatigue/test_fatg.py` | `TestAgainstTransliterationSweep.test_all_presets_and_curves` | `test_helper` | [docs/registry/implementation-map.yaml:L20177-L20188] |
| 320 | `tests/fatigue/test_fatg_workflow.py` | `_cfg` | `test_helper` | [docs/registry/implementation-map.yaml:L20189-L20198] |
| 321 | `tests/fatigue/test_fatg_workflow.py` | `_library_reference` | `test_helper` | [docs/registry/implementation-map.yaml:L20199-L20209] |
| 322 | `tests/fatigue/test_fatigue_reporting.py` | `_sample_locations` | `test_helper` | [docs/registry/implementation-map.yaml:L20210-L20219] |
| 323 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls` | `test_helper` | [docs/registry/implementation-map.yaml:L20220-L20232] |
| 324 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.calm_scatter` | `test_helper` | [docs/registry/implementation-map.yaml:L20233-L20242] |
| 325 | `tests/fatigue/test_rainflow_invariants.py` | `_damage_calls.fapps_batch` | `test_helper` | [docs/registry/implementation-map.yaml:L20243-L20252] |
| 326 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_f_curve_at_100mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20253-L20263] |
| 327 | `tests/fatigue/test_sn_curves.py` | `TestGetSNCurve.test_d_curve_at_100mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20264-L20274] |
| 328 | `tests/fatigue/test_weld_fatigue_quickcheck.py` | `_hand_N` | `test_helper` | [docs/registry/implementation-map.yaml:L20275-L20284] |
| 329 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_chain_known_value` | `test_helper` | [docs/registry/implementation-map.yaml:L20285-L20294] |
| 330 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_cycles_to_failure_wire_rope_known_value` | `test_helper` | [docs/registry/implementation-map.yaml:L20295-L20304] |
| 331 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestSNcurve.test_custom_sn_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20305-L20314] |
| 332 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_single_range_known_damage` | `test_helper` | [docs/registry/implementation-map.yaml:L20315-L20324] |
| 333 | `tests/marine_ops/marine_engineering/test_calm_buoy_fatigue.py` | `TestMinersRuleDamage.test_damage_additivity` | `test_helper` | [docs/registry/implementation-map.yaml:L20325-L20334] |
| 334 | `tests/parametric/test_atlas.py` | `_small_atlas` | `test_helper` | [docs/registry/implementation-map.yaml:L20335-L20344] |
| 335 | `tests/signal_processing/signal_analysis/fatigue_analysis_reference/process_fatigue_final.py` | `process_single_test_file` | `test_helper` | [docs/registry/implementation-map.yaml:L20345-L20354] |
| 336 | `tests/signal_processing/signal_analysis/reference_fatigue_processor.py` | `process_single_test_file` | `test_helper` | [docs/registry/implementation-map.yaml:L20355-L20364] |
| 337 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `(module-level)` | `test_helper` | [docs/registry/implementation-map.yaml:L20365-L20373] |
| 338 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `analyze_signal` | `test_helper` | [docs/registry/implementation-map.yaml:L20374-L20383] |
| 339 | `tests/signal_processing/signal_analysis/test_generate_outputs.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L20384-L20395] |
| 340 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.analyze_tension` | `test_helper` | [docs/registry/implementation-map.yaml:L20396-L20405] |
| 341 | `tests/signal_processing/signal_analysis/test_orcaflex_tension_analysis.py` | `OrcaFlexTensionAnalyzer.export_results` | `test_helper` | [docs/registry/implementation-map.yaml:L20406-L20415] |
| 342 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `run_rainflow_for_timeseries` | `test_helper` | [docs/registry/implementation-map.yaml:L20416-L20425] |
| 343 | `tests/signal_processing/signal_analysis/test_rainflow_timeseries1.py` | `test_rainflow_for_timeseries` | `test_helper` | [docs/registry/implementation-map.yaml:L20426-L20437] |
| 344 | `tests/signal_processing/signal_analysis/validate_module.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L20438-L20452] |
| 345 | `tests/solvers/test_integration_solvers.py` | `mock_solver_factory.MockSolverFactory.create_fatigue_solver` | `test_helper` | [docs/registry/implementation-map.yaml:L20453-L20462] |
| 346 | `tests/structural/fatigue/conftest.py` | `simple_sn_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20463-L20472] |
| 347 | `tests/structural/fatigue/test_damage_accumulation.py` | `dnv_d_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20473-L20482] |
| 348 | `tests/structural/fatigue/test_damage_accumulation.py` | `simple_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20483-L20492] |
| 349 | `tests/structural/fatigue/test_damage_accumulation.py` | `curve_with_material` | `test_helper` | [docs/registry/implementation-map.yaml:L20493-L20502] |
| 350 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_multislope_curve_implementation` | `test_helper` | [docs/registry/implementation-map.yaml:L20503-L20512] |
| 351 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestSNCurveMigration.test_curve_continuity` | `test_helper` | [docs/registry/implementation-map.yaml:L20513-L20522] |
| 352 | `tests/structural/fatigue/test_fatigue_migration.py` | `TestFrequencyDomainMigration.test_basic_frequency_analysis` | `test_helper` | [docs/registry/implementation-map.yaml:L20523-L20532] |
| 353 | `tests/structural/fatigue/test_fatigue_reliability.py` | `simple_sn` | `test_helper` | [docs/registry/implementation-map.yaml:L20533-L20542] |
| 354 | `tests/structural/fatigue/test_fatigue_reliability.py` | `mild_sn` | `test_helper` | [docs/registry/implementation-map.yaml:L20543-L20552] |
| 355 | `tests/structural/fatigue/test_fatigue_skill.py` | `_simple_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20553-L20562] |
| 356 | `tests/structural/fatigue/test_fatigue_skill.py` | `TestOffshoreScenario.test_high_cycle_low_stress_result` | `test_helper` | [docs/registry/implementation-map.yaml:L20563-L20574] |
| 357 | `tests/structural/fatigue/test_frequency_domain.py` | `simple_sn_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20575-L20584] |
| 358 | `tests/structural/fatigue/test_rainflow.py` | `default_counter` | `test_helper` | [docs/registry/implementation-map.yaml:L20585-L20594] |
| 359 | `tests/structural/fatigue/test_scatter_fatigue.py` | `simple_sn` | `test_helper` | [docs/registry/implementation-map.yaml:L20595-L20604] |
| 360 | `tests/structural/fatigue/test_scatter_fatigue.py` | `TestFatigueLife.test_fatigue_life_inverse_of_damage` | `test_helper` | [docs/registry/implementation-map.yaml:L20605-L20616] |
| 361 | `tests/structural/fatigue/test_sn_curves.py` | `simple_power_law` | `test_helper` | [docs/registry/implementation-map.yaml:L20617-L20626] |
| 362 | `tests/structural/fatigue/test_sn_curves.py` | `power_law_with_limit` | `test_helper` | [docs/registry/implementation-map.yaml:L20627-L20636] |
| 363 | `tests/structural/fatigue/test_sn_curves.py` | `bilinear_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20637-L20646] |
| 364 | `tests/structural/fatigue/test_sn_curves.py` | `dnv_d_curve` | `test_helper` | [docs/registry/implementation-map.yaml:L20647-L20656] |
| 365 | `tests/structural/fatigue/test_sn_curves.py` | `TestBS7608RevisedCurveParameters.test_bs7608_revised_d_known_n_at_50mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20657-L20666] |
| 366 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f_air_known_n_at_100mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20667-L20676] |
| 367 | `tests/structural/fatigue/test_sn_curves.py` | `TestISO19902CurveParameters.test_iso19902_f2_sw_cp_known_n_at_50mpa` | `test_helper` | [docs/registry/implementation-map.yaml:L20677-L20686] |
| 368 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_single_slope_cases` | `test_helper` | [docs/registry/implementation-map.yaml:L20687-L20696] |
| 369 | `tests/structural/fatigue/test_sn_curves_yaml.py` | `_all_multislope_cases` | `test_helper` | [docs/registry/implementation-map.yaml:L20697-L20706] |
| 370 | `tests/structural/fatigue_analysis/conftest.py` | `sn_curve_data` | `test_helper` | [docs/registry/implementation-map.yaml:L20707-L20716] |
| 371 | `tests/structural/fatigue_analysis/test_fatigue_analysis_sn.py` | `run_fatigue_analysis` | `test_helper` | [docs/registry/implementation-map.yaml:L20717-L20727] |
| 372 | `tests/structural/fatigue_analysis/test_fatigue_analysis_timetrace.py` | `run_fatigue_analysis` | `test_helper` | [docs/registry/implementation-map.yaml:L20728-L20738] |
| 373 | `tests/subsea/pipeline/test_free_span_f105.py` | `ref_input` | `test_helper` | [docs/registry/implementation-map.yaml:L20739-L20748] |
| 374 | `tests/subsea/viv_analysis/test_viv_analysis_unit.py` | `TestVIVFatigueCalculator.test_cumulative_damage` | `test_helper` | [docs/registry/implementation-map.yaml:L20749-L20758] |
| 375 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_orcaflex_post_process_mock` | `test_helper` | [docs/registry/implementation-map.yaml:L20759-L20768] |
| 376 | `tests/test_automation/scripts/comprehensive_fix_campaign.py` | `ComprehensiveTestFixCampaign.generate_mock_data` | `test_helper` | [docs/registry/implementation-map.yaml:L20769-L20778] |
| 377 | `tests/test_complete_fatigue_pipeline.py` | `process_configuration` | `test_helper` | [docs/registry/implementation-map.yaml:L20779-L20788] |
| 378 | `tests/test_complete_fatigue_pipeline.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L20789-L20800] |
| 379 | `tests/test_fatigue_basic.py` | `run_all_tests` | `test_helper` | [docs/registry/implementation-map.yaml:L20801-L20810] |
| 380 | `tests/test_rainflow_with_effective_tension.py` | `process_effective_tension_file` | `test_helper` | [docs/registry/implementation-map.yaml:L20811-L20820] |
| 381 | `tests/test_rainflow_with_effective_tension.py` | `main` | `test_helper` | [docs/registry/implementation-map.yaml:L20821-L20831] |
| 382 | `tests/test_reservoir_framework.py` | `run_performance_tests` | `test_helper` | [docs/registry/implementation-map.yaml:L20832-L20841] |
| 383 | `tests/test_spectral_fatigue_validation.py` | `TestSNCurveDNVReference.test_dnv_d_curve_parameters` | `test_helper` | [docs/registry/implementation-map.yaml:L20842-L20853] |
| 384 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | `test_helper` | [docs/registry/implementation-map.yaml:L20854-L20864] |
| 385 | `tests/workflows/test_durable_workflows.py` | `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` | `test_helper` | [docs/registry/implementation-map.yaml:L20865-L20875] |
| 386 | `tests/workflows/test_durable_workflows.py` | `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` | `test_helper` | [docs/registry/implementation-map.yaml:L20876-L20886] |
| 387 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_allowable_cycles` | `independent` | [docs/registry/implementation-map.yaml:L20887-L20897] |
| 388 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `PowerLawSNCurve.get_stress_range` | `independent` | [docs/registry/implementation-map.yaml:L20898-L20907] |
| 389 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_allowable_cycles` | `independent` | [docs/registry/implementation-map.yaml:L20908-L20917] |
| 390 | `src/digitalmodel/structural/fatigue/sn_curves.py` | `BilinearSNCurve.get_stress_range` | `independent` | [docs/registry/implementation-map.yaml:L20918-L20928] |
| 391 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_allowable_cycles` | `independent` | [docs/registry/implementation-map.yaml:L20929-L20938] |
| 392 | `src/digitalmodel/subsea/pipeline/free_span/_bilinear_sn.py` | `BilinearSNCurve.get_stress_range` | `independent` | [docs/registry/implementation-map.yaml:L20939-L20948] |

Counted-family outcome: 656 of 656 mapped rows inventoried; 0 further second implementations inside the direct package. Helpers above neither compute an additional family result nor change the count. The coordinator map row needs a delegate classification; issue pending (6). [src/digitalmodel/structural/parametric_coordinator.py:L255-L275] [src/digitalmodel/structural/spectral_fatigue.py:L122-L178]

### Repository-wide follow-up

Repository-wide search included every tracked Python file, including `scripts/`, with `git grep -n -E 'def (spectral_moment|narrow_band_damage|slenderness_beta)|class (PlateBuckling|ElasticBuckling|StiffenedPanelBuckling)' HEAD -- '*.py'`; `HEAD` equaled the code pin. An additional `rg` search used the same pattern over `src scripts`. Source alternatives include the infrastructure plate calculator, legacy structural adapter, structural-analysis analyzers and separate fatigue/hydrodynamic moment implementations. These are map follow-up findings outside the direct package, not counted-family misses. Issue pending (6). [docs/domains/plate-buckling/buckling-code-review-2026-06-26.md:L9-L21] [src/digitalmodel/infrastructure/calculations/plate_buckling.py:L91-L127] [src/digitalmodel/fatigue/spectral_fatigue.py:L150-L170] [src/digitalmodel/hydrodynamics/seakeeping.py:L56-L70]

Map follow-up: add plate/panel capacity orchestration, local slenderness and cyclic-frequency spectral moments/period/bandwidth as explicit method families or relationships; the map has no direct-package rows for them. Families with no in-package map rows are not counted until the map adds them. Issue pending (6). [src/digitalmodel/structural/buckling_parametric.py:L92-L156] [src/digitalmodel/structural/panel_buckling_parametric.py:L128-L184] [src/digitalmodel/structural/panel_design_explorer.py:L95-L124] [src/digitalmodel/structural/spectral_fatigue.py:L29-L119]

## 10. Coverage and open items

Direct source read: `__init__.py`, `spectral_fatigue.py`, `buckling_parametric.py`, `buckling_workflow.py`, `panel_buckling_parametric.py`, `panel_design_explorer.py`, `parametric_coordinator.py`, `parametric_report.py` and `wall_thickness_quickcheck.py`; sub-package algorithms remain separate dependency scope. [src/digitalmodel/structural/__init__.py:L1-L1] [src/digitalmodel/structural/spectral_fatigue.py:L1-L178] [src/digitalmodel/structural/buckling_parametric.py:L1-L284] [src/digitalmodel/structural/buckling_workflow.py:L1-L83] [src/digitalmodel/structural/panel_buckling_parametric.py:L1-L329] [src/digitalmodel/structural/panel_design_explorer.py:L1-L159] [src/digitalmodel/structural/parametric_coordinator.py:L1-L373] [src/digitalmodel/structural/parametric_report.py:L1-L113] [src/digitalmodel/structural/wall_thickness_quickcheck.py:L1-L100]

The direct module-name search discovered the following relevant documentation; broader `structural` mentions also include other package scopes. The list distinguishes directly inspected basis documents from incidental discovery references, rather than treating every mention as an engineering reference.

| Documentation found | Read disposition / open reference |
| --- | --- |
| `docs/domains/plate-buckling/buckling-code-review-2026-06-26.md` | Read for wrapper methods and preliminary/validated wording discrepancy; issue pending (2). [docs/domains/plate-buckling/buckling-code-review-2026-06-26.md:L39-L65] |
| `docs/domains/codes-register.md` | Read for code identifiers, editions and documented unresolved labels; issue pending (2). [docs/domains/codes-register.md:L22-L58] |
| `docs/maps/digitalmodel-operator-map.md`; `docs/README.md`; `docs/domains/README.md` | Navigation and package scope, not numerical qualification; no change to those documents is proposed; issue pending (2) covers remaining qualification. [docs/maps/digitalmodel-operator-map.md:L100-L108] [docs/README.md:L1-L20] [docs/domains/README.md:L1-L15] |
| `docs/registry/implementation-map.md`; `docs/registry/implementation-map.yaml` | Inventory context and separately pinned family rows; issue pending (6). [docs/registry/implementation-map.md:L13-L29] [docs/registry/implementation-map.yaml:L1-L4] |
| `docs/registry/workflows.yaml`; `examples/structural/wall_thickness_quickcheck/README.md` | Read registered routes and cache workflow; issue pending (5). [docs/registry/workflows.yaml:L728-L740] [docs/registry/workflows.yaml:L1249-L1260] [examples/structural/wall_thickness_quickcheck/README.md:L1-L14] |
| `src/digitalmodel/structural/structural_analysis/README.md`; `src/digitalmodel/structural/fatigue_apps/README.md`; `src/digitalmodel/structural/stress/README.md` | Sub-package discovery/boundary documentation; their methods are excluded from this top-level pack; issue pending (2) covers unresolved referenced standards. [src/digitalmodel/structural/structural_analysis/README.md:L1-L5] [src/digitalmodel/structural/fatigue_apps/README.md:L275-L279] [src/digitalmodel/structural/stress/README.md:L1-L10] |
| `docs/standards/2026-06-28-determinism-golden-refresh.md` | Read golden semantics; issue pending (2) preserves distinction from independent verification. [docs/standards/2026-06-28-determinism-golden-refresh.md:L1-L15] |
| `docs/domains/fatigue/fatg-legacy-spectral-fatigue.md`; `docs/domains/reporting/standard-report-pack.md`; `docs/domains/fire_safety/foam-system-sizing.md` | Discovery mentions other spectral/report workflows; not evidence about the direct spectral implementation; issue pending (2) covers any further cross-workflow qualification. [docs/domains/fatigue/fatg-legacy-spectral-fatigue.md:L1-L8] [docs/domains/reporting/standard-report-pack.md:L115-L120] [docs/domains/fire_safety/foam-system-sizing.md:L111-L116] |
| `docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md`; `docs/plans/2026-07-11-issue-1505-synthetic-viv-parametric-hf-pilot.md`; `docs/plans/evidence/issue-2082-ci-discovery.md`; `docs/plans/parametric-refresh/adding-an-atlas.md` | Discovery context only; code/tests govern this pack; issue pending (2) covers any unreviewed engineering assertions. [docs/plans/2026-07-03-issue-1358-motion-forecast-keystone.md:L10-L14] [docs/plans/2026-07-11-issue-1505-synthetic-viv-parametric-hf-pilot.md:L60-L65] [docs/plans/evidence/issue-2082-ci-discovery.md:L1-L10] [docs/plans/parametric-refresh/adding-an-atlas.md:L74-L79] |
| One additional planning document with an identifying process name | Source-reference discovery only; naming and any necessary nonidentifying codenaming remain issue pending (2). |

Offline execution record at code revision `334521c97a21c16950046f52b61586e8e8465787`: the main command covers the nine named test files below plus the contracts domain; the two registry nodes run separately in a temporary copy. Results are recorded for BASE and the final uncommitted documentation tree; no module code is changed.

| Test path / scope | BASE passed / failed / skipped / NOT_RUN | After passed / failed / skipped / NOT_RUN |
| --- | --- | --- |
| `tests/test_spectral_fatigue.py` | 13 / 0 / 0 / 0 | 13 / 0 / 0 / 0 |
| `tests/test_parametric_coordinator.py` | 46 / 0 / 0 / 0 | 46 / 0 / 0 / 0 |
| `tests/structural/test_buckling_parametric.py` | 11 / 0 / 0 / 0 | 11 / 0 / 0 / 0 |
| `tests/structural/test_panel_buckling_parametric.py` | 11 / 0 / 0 / 0 | 11 / 0 / 0 / 0 |
| `tests/structural/test_panel_design_explorer.py` | 10 / 0 / 0 / 0 | 10 / 0 / 0 / 0 |
| `tests/test_wall_thickness_quickcheck_example.py` | 8 / 0 / 0 / 0 | 8 / 0 / 0 / 0 |
| `tests/workflow_api/test_run_workflow_buckling.py` | 2 / 0 / 0 / 0 | 2 / 0 / 0 / 0 |
| `tests/workflow_api/test_run_workflow_wall_thickness.py` | 2 / 0 / 0 / 0 | 2 / 0 / 0 / 0 |
| `tests/docs/test_digitalmodel_routing_contract.py` | 10 / 0 / 0 / 0 | 10 / 0 / 0 / 0 |
| `tests/contracts/` | 487 / 0 / 0 / 0 | 487 / 0 / 0 / 0 |
| Two registered workflow nodes (section 5 C5d) | 2 / 0 / 0 / 0 | 2 / 0 / 0 / 0 |
| Total | 602 / 0 / 0 / 0 | 602 / 0 / 0 / 0 |

Commands: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <listed test files> tests/contracts -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<run>`; and the same form with the two C5d nodes in the temporary copy. The full uv executable was used in execution; paths are expressed generically here. The direct-package tests total 91, derived from execution rather than the earlier inventory count. No licensed solver or installed vendor interface was used; guarded missing-interface tests execute the error path.

| Open item | Maintainer disposition / tracking |
| --- | --- |
| U1: campaign coordinate frame, origin, signs and phase conventions (section 2) | Establish a complete design basis before campaign use; issue pending (2). |
| U2: exact standard editions, clause conformance and local Citation sidecars (section 3) | Licensed clause text has not been checked; resolve conflicting/absent labels and retain no licensed table values in this pack; issue pending (2). |
| U3: independent narrow-band damage and panel-capacity anchors (section 4) | Current damage/sweep/cache/golden examples do not establish these anchors; issue pending (2). |
| Spectral input rejection and inconsistent moments | Add tests for risks S01–S04; preserve valid elementary checks; issue pending (1). [src/digitalmodel/structural/spectral_fatigue.py:L53-L67] [src/digitalmodel/structural/spectral_fatigue.py:L114-L172] |
| Sweep/explorer validation, empty results, reference fallbacks and index collisions | Add explicit negative-case and schema checks; issue pending (3). [src/digitalmodel/structural/buckling_workflow.py:L56-L83] [src/digitalmodel/structural/panel_design_explorer.py:L66-L68] [src/digitalmodel/structural/panel_design_explorer.py:L114-L124] [src/digitalmodel/structural/panel_buckling_parametric.py:L263-L327] |
| Missing coordinator counts and report omissions | Reject or explicitly flag incomplete results; issue pending (4). [src/digitalmodel/structural/parametric_coordinator.py:L340-L373] [src/digitalmodel/structural/parametric_report.py:L46-L113] |
| Quickcheck source-checkout resources and arbitrary-cache provenance | Test installation boundary, cache schema/provenance and empty selections; issue pending (5). [src/digitalmodel/structural/wall_thickness_quickcheck.py:L16-L31] [src/digitalmodel/structural/wall_thickness_quickcheck.py:L41-L100] |
| Implementation map: delegate classification and families without direct-package rows | Add/adjust map rows before counting new families; issue pending (6). [src/digitalmodel/structural/parametric_coordinator.py:L255-L275] [src/digitalmodel/structural/buckling_parametric.py:L92-L156] [src/digitalmodel/structural/spectral_fatigue.py:L29-L119] |
| Historical curve-library and other-package spectral defects | Related issues #2165 and #2583 remain dependency context; do not use those separate code paths as independent references without their own disposition. [src/digitalmodel/structural/fatigue/sn_curves.py:L326-L339] [src/digitalmodel/fatigue/spectral_fatigue.py:L150-L170] |
| Nonidentifying discovery names and documentation reconciliation | Resolve the remaining planning-document display name and preliminary/validated discrepancy; issue pending (2). [docs/domains/plate-buckling/buckling-code-review-2026-06-26.md:L43-L46] [src/digitalmodel/structural/panel_buckling_parametric.py:L306-L317] |

The issue drafts are grouped by cause into six entries. Draft markers require replacement with opened issue numbers before final acceptance of section 10; no issue creation or quality clearance is claimed by this draft.
