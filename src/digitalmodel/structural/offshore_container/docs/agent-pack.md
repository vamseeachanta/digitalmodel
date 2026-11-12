# Offshore container knowledge pack

| Header | Value |
| --- | --- |
| Module | structural/offshore_container |
| Repository | digitalmodel |
| Path | `src/digitalmodel/structural/offshore_container` |
| Pack revision | 0 |
| Code revision | `df9787d346c226aad513cab9d248f7be841b37d5` |
| Analysis family | Engineering calculations (`EC`); AQG r0 Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |
| Implementation-map revision | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` |
| Implementation-map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |

Code citations use the code revision; implementation-map citations use the separate map revision above. Execution records below describe offline observations on the pinned code, with test definitions cited beside each result.

## 1. Purpose and scope

The module screens an offshore-container lift using axial top-rail stress, floor-beam bending and pad-eye axial stress, returning their maximum utilization; the code explicitly excludes certification by this reduced-order calculation. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L1-L7] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L136-L162]

The geometry assumes a centred master link above a rectangular four-corner attachment plan, ideal sharp-cornered square hollow sections and a simply supported floor load shared between the long rails; these assumptions do not establish capacity for corner radii, corrosion, joints, buckling or combined-action resistance. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L88-L97] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L143-L158] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L139-L151]

The registry marks the structural domain `stable`; it supplies no separate maturity assessment for this package at the code pin. [docs/registry/module-routing.yaml:L506-L530]

## 2. Inputs and outputs

Inputs are Python arguments rather than an input-file format; the calculation and its section constructors perform arithmetic directly without range or finite-value guards. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L59-L82] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L162]

| Input | Unit and convention | Range, default and source |
| --- | --- | --- |
| `SHS.b`, `SHS.t` | m; outer width and wall thickness | No constructor defaults or enforced range; the worked geometry requires positive thickness and `2t < b`. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L59-L82] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L141-L146] |
| `SHS.from_mm(b_mm, t_mm)` | mm in; m stored | Exact division by the millimetre conversion factor; no explicit input checks. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L80-L82] |
| `length`, `width`, `lift_height` | m; link height above attachment plane; angle measured from vertical | Required arguments; no enforced positive range, although the worked example requires positive height. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L88-L97] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L122-L130] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L141-L146] |
| `rating_kg` | kg; gross mass excluding lifting set | Required, unguarded input; no payload-to-gross conversion is performed. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L124-L126] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L138-L157] |
| `fy`, `padeye_area_m2` | Pa and m² | Representative defaults are in the signature; neither is checked for positivity. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L130] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L134-L155] |
| `factors` | Load, usage and tare factors dimensionless; angle field in degrees | Omission selects `DesignFactors`; default values are referenced, not reproduced here; custom factors have no validation. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L42-L56] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L132-L157] |
| `n_padeyes` | count | Signature default and clamping bounds are code-defined; nonintegral values are not rejected and the single-eye factor is not selected. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L139] |
| `factor_citations(repo_root)` | Optional filesystem root | Default `None` is passed to the citation resolver; errors are caught in the package wrapper. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L168-L196] [src/digitalmodel/citations/registry.py:L72-L91] |

The local origin is the plan centroid in the attachment plane, with height towards the link; no global axes, phase convention or file coordinate transform is implemented. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L88-L97]

| Output | Unit and meaning | Source |
| --- | --- | --- |
| `SHS.area`, `.inertia`, `.modulus` | m², m⁴, m³; ideal concentric square properties | [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L66-L82] |
| `sling_angle_to_vertical`, `allowable_stress` | rad and Pa respectively | [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L88-L102] |
| `Utilization.top_rail`, `.floor`, `.pad_eye`, `.governing` | Dimensionless stress ratios and their maximum | [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L105-L113] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L143-L162] |
| `sling_angle_to_vertical_deg`, `rsl_kN`, `FL_kN` | Degrees, kN per pad eye and kN total primary design load | [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L105-L113] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L159-L162] |
| `factor_citations` | Mapping of factor names to `CitedValue`, or empty dict on resolution failure | [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L178-L196] [src/digitalmodel/citations/registry.py:L83-L91] |

The external sweep script writes PNG figures and a Markdown README next to itself; its numerical values and figures are generated outputs rather than independent design references. [examples/structural/offshore_container_utilization/run_utilization_curves.py:L55-L67] [examples/structural/offshore_container_utilization/run_utilization_curves.py:L151-L201]

## 3. Method and governing standards

The code and example README both identify DNVGL-ST-E271 (DNV 2.7-1), edition August 2017; the citation registry records revision `2017`, with no competing edition in these sources. Clause text not checked against the licensed copy; titles and clause references below are the code's attributions, not a standards-compliance determination. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L1-L28] [examples/structural/offshore_container_utilization/README.md:L1-L8] [src/digitalmodel/citations/registry.py:L55-L68]

| Method | Implementation and basis | Citation sidecar / limitation |
| --- | --- | --- |
| SHS properties | `SHS.area`, `.inertia`, `.modulus`: outer square less inner square, centroidal integration and extreme-fibre division | Independent closed-form derivation D1, without licensed coefficients. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L66-L82] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L47-L71] |
| Sling geometry | `sling_angle_to_vertical`: half-diagonal and `atan2` of horizontal reach and height | Closed-form right triangle D2; the nominal angle field is not read by `utilization`. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L88-L97] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L132-L140] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L73-L84] |
| Rating definition | Gross mass argument and lifting-set exclusion | DNVGL-ST-E271, 2017, §1.6 as named in the code; clause text not checked against the licensed copy. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L25-L28] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L124-L130] |
| Allowable stress | `allowable_stress`: `usage_factor * fy`; load and resistance coefficients are referenced at their definitions only | DNVGL-ST-E271, 2017, §4.2.1 as named in the code; sidecar key `allowable_usage_factor`; clause text not checked against the licensed copy. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L100-L102] [src/digitalmodel/citations/registry.py:L55-L68] |
| Primary and floor loads | `utilization`: factored gross load; tare deducted for internal load, half shared to each long rail, simply supported uniform-load bending | DNVGL-ST-E271, 2017, §4.2.3.1 as named in the code; sidecar key `primary_structure_load`; tare fraction is a screening assumption, not independently established here. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L149-L162] [src/digitalmodel/citations/registry.py:L62-L68] |
| Pad-eye and top rail | `utilization`: clamped count, load divided across the count less one, cosine resolution and tangent horizontal pull, then net-area stresses | DNVGL-ST-E271, 2017, §4.2.3.1 as named in the code; sidecar keys `padeye_load`, `single_padeye_load`, `default_sling_angle_to_vertical_deg`; the latter two fields do not select calculation branches. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L132-L147] [src/digitalmodel/citations/registry.py:L62-L68] |
| Governing result | `utilization`: maximum of three separate stress ratios | No combined-action, corner-post or buckling term appears in the calculation. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L143-L162] |
| Factor provenance | `factor_citations` calls `get_offshore_container_factor` for five keys; getter validates `Citation` and returns `CitedValue` | The wrapper catches `CitationResolutionError` and returns an empty mapping, even though its docstring describes fail-closed behavior when a page resolves; section 6 records this discrepancy. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L168-L196] [src/digitalmodel/citations/registry.py:L72-L91] |

No standards-derived coefficient or licensed table value is transcribed into this pack; the executable geometry reference is limited to section properties and sling angle. [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L148-L151]

## 4. Validated worked examples and regression examples

Execution record: BASE `df9787d346c226aad513cab9d248f7be841b37d5`, offline pytest, 13 run / 13 passed / 0 failed / 0 skipped / 0 NOT_RUN; AFTER on the same pin: 13 run / 13 passed / 0 failed / 0 skipped / 0 NOT_RUN. The two documented cases use the independent D1 integration and D2 right triangle; the other eleven are recorded conservatively as regression checks of implemented behavior. [tests/unit/structural/test_documented_shs_sling_geometry.py:L22-L28] [tests/unit/structural/test_documented_shs_sling_geometry.py:L56-L83] [tests/unit/structural/test_offshore_container_dnv271.py:L19-L104]

Command form: `PYTHONPATH=src uv run --no-sources python -m pytest tests/unit/structural/test_offshore_container_dnv271.py tests/unit/structural/test_documented_shs_sling_geometry.py -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-<phase>`; no addopts override and no parallel test workers.

| Case / class | Independent basis or implemented expectation; tolerance | Existing test node ID | BASE / AFTER |
| --- | --- | --- | --- |
| V1: documented section and sling / validated | Synthetic `b=0.2 m`, `t=0.05 m`, `L=6.0 m`, `B=8.0 m`, `h=5.0 m`; D1 gives `A=3.0e-2 m²`, `I=1.25e-4 m⁴`, `W=1.25e-3 m³`; D2 gives `π/4 rad`, `45.0°`; relative `1e-12` for properties and absolute `1e-12 rad` / `1e-12 degree` for angles, for floating-point roundoff only. [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L27-L84] [tests/unit/structural/test_documented_shs_sling_geometry.py:L56-L70] | `tests/unit/structural/test_documented_shs_sling_geometry.py::test_documented_shs_sling_geometry` | PASS / PASS |
| V2: documented unit routes / validated | Same D1 hand values, metre inputs and millimetre conversion; both constructors compared to the independent expected values and to each other at relative `1e-12`, absolute zero in each output unit. [tests/unit/structural/test_documented_shs_sling_geometry.py:L73-L83] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L37-L71] | `tests/unit/structural/test_documented_shs_sling_geometry.py::test_documented_shs_unit_equivalence` | PASS / PASS |
| R1: section properties / regression | Area formula and modulus/inertia relation in m² and m³; `pytest.approx` without explicit tolerance. [tests/unit/structural/test_offshore_container_dnv271.py:L19-L23] | `tests/unit/structural/test_offshore_container_dnv271.py::test_shs_section_props` | PASS / PASS |
| R2: equal height/reach angle / regression | `45.0°`; `pytest.approx` without explicit tolerance. [tests/unit/structural/test_offshore_container_dnv271.py:L26-L29] | `tests/unit/structural/test_offshore_container_dnv271.py::test_sling_angle_to_vertical_45` | PASS / PASS |
| R3: allowable / regression | Code usage-factor product in Pa; `pytest.approx` without explicit tolerance; no licensed-copy validation. [tests/unit/structural/test_offshore_container_dnv271.py:L32-L35] | `tests/unit/structural/test_offshore_container_dnv271.py::test_allowable_is_085_of_yield` | PASS / PASS |
| R4: factor fixtures / regression | Exact equality to the code's dimensionless factor fixtures; not independent standards evidence. [tests/unit/structural/test_offshore_container_dnv271.py:L38-L42] | `tests/unit/structural/test_offshore_container_dnv271.py::test_dnv_factor_values` | PASS / PASS |
| R5: primary load / regression | Code factor times gross mass and gravity, converted to kN; relative tolerance `1e-6`. [tests/unit/structural/test_offshore_container_dnv271.py:L45-L47] | `tests/unit/structural/test_offshore_container_dnv271.py::test_FL_is_2p5_R_g` | PASS / PASS |
| R6: rating response / regression | Higher rating gives strictly higher governing dimensionless utilization. [tests/unit/structural/test_offshore_container_dnv271.py:L50-L54] | `tests/unit/structural/test_offshore_container_dnv271.py::test_util_increases_with_rating` | PASS / PASS |
| R7: sling response / regression | Steeper geometry gives strictly lower top-rail and pad-eye dimensionless utilization. [tests/unit/structural/test_offshore_container_dnv271.py:L57-L64] | `tests/unit/structural/test_offshore_container_dnv271.py::test_steeper_sling_reduces_padeye_and_toprail` | PASS / PASS |
| R8: floor invariance / regression | Dimensionless floor ratio unchanged with angle; `pytest.approx` without explicit tolerance. [tests/unit/structural/test_offshore_container_dnv271.py:L67-L72] | `tests/unit/structural/test_offshore_container_dnv271.py::test_floor_is_angle_independent` | PASS / PASS |
| R9: section response / regression | Larger selected section gives strictly lower governing dimensionless utilization. [tests/unit/structural/test_offshore_container_dnv271.py:L75-L78] | `tests/unit/structural/test_offshore_container_dnv271.py::test_bigger_section_reduces_utilization` | PASS / PASS |
| R10: valid citation fixture / regression | Exact code-factor equality and code ID match against synthetic frontmatter; no licensed source value established. [tests/unit/structural/test_offshore_container_dnv271.py:L83-L93] | `tests/unit/structural/test_offshore_container_dnv271.py::test_factor_citation_resolves_with_fixture` | PASS / PASS |
| R11: absent page / regression | A `RuntimeWarning` and mapping with zero entries are required; this confirms fallback behavior rather than fail-closed provenance. [tests/unit/structural/test_offshore_container_dnv271.py:L96-L104] | `tests/unit/structural/test_offshore_container_dnv271.py::test_factor_citations_degrade_gracefully` | PASS / PASS |

The documented test extracts the example's sole Python block unchanged, executes it in a temporary working directory and checks that it writes no output files; no source file or licensed interface is executed beyond the pure calculation module. [tests/unit/structural/test_documented_shs_sling_geometry.py:L31-L53] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L95-L127]

The sweep README is generated from `utilization` itself and is not an independent expected-value source; full utilization validation and a tested report builder remain open under #3325. [examples/structural/offshore_container_utilization/run_utilization_curves.py:L151-L201] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L148-L153]

## 5. QA gates

Required gate set: AQG r0 §10, Python calculations; G5 applies under AQG r0 §9. These are proposed pack checks, not a claim of signed gate clearance.

| Gate | Applicability and route | Module-specific check / numeric pass criterion / evidence |
| --- | --- | --- |
| G0 | Applicable: Python inputs, SI units and factor provenance | EC0 and EC4 below; units and citation fixtures are exercised without a solver. [tests/unit/structural/test_documented_shs_sling_geometry.py:L73-L83] [tests/unit/structural/test_offshore_container_dnv271.py:L83-L104] |
| G1 | Applicable: import the public calculation module and run offline pytest; no target-program deck is written | EC1 below; zero failed imports and 13 passing test nodes, unit: count. [src/digitalmodel/structural/offshore_container/__init__.py:L3-L23] [tests/unit/structural/test_offshore_container_dnv271.py:L7-L14] [tests/unit/structural/test_documented_shs_sling_geometry.py:L31-L83] |
| G2 | Applicable: independent hand geometry, with full load/resistance validation still open | EC2 and EC3 below; the hand geometry is not a reference for the known input or pad-eye defects. [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L47-L84] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L148-L153] |
| G3 | NOT_APPLICABLE: no dynamics integrator or time-series output in this pure static screening calculation | Zero required dynamic cases, unit: count. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L162] |
| G4 | NOT_APPLICABLE: no diffraction body, mesh or hydrodynamic solver in the calculation | Zero required diffraction cases, unit: count. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L162] |
| G5 | Applicable: retain the test evidence and explicit limits for each screening result | EC5 below; report automation remains open under #3325. [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L148-L153] |

| Check ID / gate | Executable route and pass criterion | Evidence / status |
| --- | --- | --- |
| EC0 / G0 | Run V2: metre and millimetre outputs match the independent expected SI values, relative error ≤ `1e-12` (dimensionless), absolute allowance `0 m`, `0 m²`, `0 m⁴`, `0 m³`. [tests/unit/structural/test_documented_shs_sling_geometry.py:L73-L83] | V2 result in section 4; BASE PASS, AFTER PASS. |
| EC4 / G0 | Run R10 and R11: exactly one expected code ID, factor equality with zero difference in dimensionless units, and zero entries on missing page with a warning. This detects the fallback, not a malformed-metadata rejection. [tests/unit/structural/test_offshore_container_dnv271.py:L83-L104] | R10/R11 results; BASE PASS, AFTER PASS. |
| EC1 / G1 | Run both module test files using section 4's command; require 13 passed, 0 failed, 0 skipped, 0 NOT_RUN, unit: test count. [tests/unit/structural/test_offshore_container_dnv271.py:L19-L104] [tests/unit/structural/test_documented_shs_sling_geometry.py:L56-L83] | Pytest node results; BASE PASS, AFTER PASS. |
| EC2 / G2 | Run V1: both section constructors return D1's area, inertia and modulus within relative `1e-12` (dimensionless), with absolute allowance zero in m², m⁴ and m³. [tests/unit/structural/test_documented_shs_sling_geometry.py:L56-L62] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L47-L71] | V1 section-property assertions; BASE PASS, AFTER PASS. |
| EC3 / G2 | Run V1: angle differs from `π/4 rad` and `45.0°` by ≤ `1e-12 rad` and ≤ `1e-12 degree`, respectively, with no relative allowance. [tests/unit/structural/test_documented_shs_sling_geometry.py:L63-L70] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L73-L84] | V1 angle assertions; BASE PASS, AFTER PASS. |
| EC5 / G5 | Hand-audit the section-4 node list against both pytest logs and the cited definitions: 13 rows per run, 0 missing node results and 0 result differences, unit: count; retain the pin, commands and independent/regression classifications. [tests/unit/structural/test_offshore_container_dnv271.py:L19-L104] [tests/unit/structural/test_documented_shs_sling_geometry.py:L56-L83] | Section 4 and execution logs; PASS: 13 nodes in each run, 0 missing, 0 differences. |

## 6. Known failure modes and pitfalls

sweep: 16 found

The sweep covered both Python files recursively; the commands used were `rg -n 'except|continue|return None|return \[\]|return \{\}|\b(?:int|float)\(|=[[:space:]]*0([.]0)?\b|1970|cache|unit|default|tare_fraction' src/digitalmodel/structural/offshore_container --glob '*.py'` and `rg -n 'raise|assert|if |else|warn|return|float|int|factor|kg|mm|kN|Pa|rad' src/digitalmodel/structural/offshore_container --glob '*.py'`, followed by reading all definitions. Counts below are distinct behaviors, including unit assumptions and defaults, rather than a count of matching lines. [src/digitalmodel/structural/offshore_container/__init__.py:L1-L23] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L31-L206]

| Finding | Symptom and cause at code pin | Detection / tracking |
| --- | --- | --- |
| S1: SHS domain | No guard rejects nonpositive width/thickness or `2t ≥ b`; algebra can return zero or unphysical properties. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L59-L78] | no detection check exists; issue pending (1). |
| S2: SHS unit assumption | `SHS` stores metres, while `from_mm` always divides external arguments; a caller using the wrong constructor can silently mis-scale the section. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L59-L82] | EC0 checks valid conversion only; no detection check exists for mislabeled caller units; issue pending (1). |
| S3: sling geometry | `hypot` removes signs of plan dimensions and `atan2` accepts nonpositive height; no physical-domain validation precedes these operations. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L88-L97] | EC3 checks the valid triangle only; no detection check exists for invalid geometry; issue pending (1). |
| S4: allowable inputs | The default `usage_factor` is selected without material/applicability confirmation; unguarded strength and custom usage factor can give zero, negative or nonfinite allowable stress. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L51-L56] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L100-L102] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L132-L155] | no detection check exists; issue pending (1). |
| S5: material default | Omitted `fy` selects a representative material strength without checking the actual material. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L130] | no detection check exists; issue pending (1). |
| S6: pad-eye area default | Omitted net area selects a representative area; zero and negative areas are unguarded in stress division. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L130] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L146-L147] | no detection check exists; issue pending (1). |
| S7: tare default | The fixed tare fraction affects internal load without a supplied tare mass or range check; custom factors can produce negative internal load. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L51-L56] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L149-L155] | no detection check exists; issue pending (1). |
| S8: pad-eye count | Count default, silent clamping and acceptance of nonintegral values can conceal the actual arrangement; the single-eye factor is never selected. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L51-L55] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L139] | no detection check exists; issue pending (2). |
| S9: nominal angle field | Changing the default-angle factor does not change the geometry-derived angle; it is neither used as a default nor a limit. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L51-L55] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L132-L140] | no detection check exists; issue pending (2). |
| S10: factor selection / provenance | Omission selects unchecked `DesignFactors`; custom factor values are not passed to the independent registry getter, so emitted citations need not describe the actual custom calculation. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L132-L157] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L178-L186] [src/digitalmodel/citations/registry.py:L81-L91] | no detection check exists; issue pending (3). |
| S11: error swallowing | Every `CitationResolutionError`, including malformed metadata, becomes `{}`; the code does not implement the docstring's resolved-page fail-closed distinction. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L168-L195] | EC4 detects missing-page fallback; no detection check exists for malformed-page rejection; issue pending (3), related #990. |
| S12: warning suppression | A process-global flag suppresses subsequent warnings; later calls can return an empty mapping without another warning. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L165-L195] | no detection check exists; issue pending (3). |
| S13: partial result loss | A later resolution failure returns an empty dict instead of the factors already accumulated in `out`. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L181-L196] | no detection check exists; issue pending (3). |
| S14: SI and finite-value assumptions | Mass, plan length, gravity, strength and net area flow directly into arithmetic; no external text conversion, finite-value guard or unit metadata prevents negative mass, mixed units or nonfinite output. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L39-L39] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L162] | no detection check exists; issue pending (1). |
| S15: primary-load factor default | `structure_load_factor` defaults to a fixed screening coefficient without checking the actual load case; this default feeds primary and floor loads. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L51-L56] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L149-L157] | R4 checks a fixture only; no detection check exists for load-case applicability; issue pending (1). |
| S16: pad-eye factor default | `padeye_load_factor` defaults to a fixed coefficient and is used for every clamped count without a load-case applicability check. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L51-L56] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L136-L140] | R4 checks a fixture only; no detection check exists for load-case applicability; issue pending (1). |

No `continue`, `return None`, error-path empty-list return, external-text `int()`/`float()` conversion, epoch default or empty-result cache write occurs in the package; the empty-dict error return is S11–S13, and geometric divisors and output scaling are unit/formula operations rather than fallback constants. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L31-L206] [src/digitalmodel/structural/offshore_container/__init__.py:L1-L23]

Read-only issue review covered #973 (initial screening scope), #990 (factor traceability), #1266 (gallery discovery) and #3325 (module-report work); none supplied a confirmed numerical incident beyond the code discrepancies above. The report's open status is also recorded by the worked example; the design-factor fixtures establish behavior, not independent licensed-copy agreement. [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L148-L153] [tests/unit/structural/test_offshore_container_dnv271.py:L32-L47] [tests/unit/structural/test_offshore_container_dnv271.py:L83-L104]

## 7. Lessons log

Append entries rather than replacing prior evidence.

| Date | Entry | Lesson and evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-11-04 | Initial pack | Independent geometry checks establish section properties and angle only; they do not validate standards-derived utilization factors or input-domain rejection. [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L130-L153] | Separate validated geometry from regression behavior; list fallback and default hazards. | Not assigned; issue pending (3). |

## 8. Ownership

Required review roles: engineering calculations quality manager and engineering calculations deputy quality manager; production role: calculation/documentation contributor. These are review responsibilities for this DRAFT, not a record of an appointment or clearance (AQG r0 §11).

Route unresolved engineering criteria, standard-edition questions and classification disagreements to the engineering calculations quality manager; the maintainer handles repository scope and issue publication after independent review (AQG r0 §11).

## 9. Dependencies and implementation inventory

The public package re-exports the calculation classes, catalogue and functions; its calculation dependency is Python standard-library arithmetic, while the optional provenance function imports the citation registry and schema when called. [src/digitalmodel/structural/offshore_container/__init__.py:L3-L23] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L31-L37] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L177-L179]

The citation getter validates synthetic or configured wiki metadata and returns a `CitedValue`; the external sweep consumer adds NumPy and Matplotlib, delegates all load/utilization computation to the listed `utilization` definition and writes its own presentation artifacts. [src/digitalmodel/citations/registry.py:L72-L91] [examples/structural/offshore_container_utilization/run_utilization_curves.py:L18-L43] [examples/structural/offshore_container_utilization/run_utilization_curves.py:L55-L133] [examples/structural/offshore_container_utilization/run_utilization_curves.py:L151-L201]

No solver version or private dataset is required by these two test modules; the documented example uses synthetic geometry and writes only JSON to standard output. [tests/unit/structural/test_offshore_container_dnv271.py:L7-L14] [tests/unit/structural/test_offshore_container_dnv271.py:L83-L104] [tests/unit/structural/test_documented_shs_sling_geometry.py:L31-L53] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L19-L43] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L86-L127]

Implementation-map inventory at the header's map revision: **0 module map families, 0 implementation rows**. The two path hits, `Utilization` and `utilization`, are rejected lexical candidates for `cathodic-protection-calculations`, explicitly excluded from implementation counts pending inspection; they do not establish an in-package implementation family. [docs/registry/implementation-map.yaml:L46711-L46724]

| A8 inventory item | Result and disposition |
| --- | --- |
| Families with at least one in-package implementation row | Empty inventory; there is no counted-family ranking. [docs/registry/implementation-map.yaml:L46711-L46724] |
| Counted-family grep | NOT_APPLICABLE: no map family has an implementation row in this package; there are no counted signals to apply or path-matching signals to omit. [docs/registry/implementation-map.yaml:L46711-L46724] |
| Map follow-up | Add reviewed families for SHS section properties, sling geometry and offshore-container load/resistance screening before counting them; issue pending (4). [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L59-L162] |

Whole-repository discovery, including `scripts/`, searched tracked Python files at the code pin with Python `re`, case-sensitive: `DNV.?2.?7.?1|[Ee]271|dnv_2_7_1|sling_angle_to_vertical|\bclass SHS\b` and `\bSHS\b|square hollow|half_diag`; each hit was assigned to its innermost function/class or module-level block. This supplementary discovery is not a counted-family grep and does not establish exhaustive implementation-map coverage.

| Discovered definition / location | Classification |
| --- | --- |
| `SHS.area`, `.inertia`, `.modulus`, `.from_mm`; `sling_angle_to_vertical`; `allowable_stress`; `utilization` | In-package formulas/conversion listed in section 3. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L59-L162] |
| `DesignFactors`, `Utilization`, `SECTION_CATALOGUE`; package exports | Data holders and export helpers, not additional computed implementations. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L42-L56] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L105-L113] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L199-L206] [src/digitalmodel/structural/offshore_container/__init__.py:L3-L23] |
| `factor_citations`, `get_offshore_container_factor` | Provenance wrapper and registry getter, not second structural calculations. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L168-L196] [src/digitalmodel/citations/registry.py:L55-L91] |
| `curve_vs_rating`, `curve_vs_sling_angle`, `curve_vs_aspect_ratio`, `envelope_heatmap`, `write_report` | Delegation, geometry for sweep inputs and serialization/plotting around the listed calculation; no independent load/resistance result. [examples/structural/offshore_container_utilization/run_utilization_curves.py:L55-L133] [examples/structural/offshore_container_utilization/run_utilization_curves.py:L151-L201] |
| Capability specification in `scripts/capabilities/build_onepagers.py` | Presentation metadata pointing to the external sweep; no structural formula. [scripts/capabilities/build_onepagers.py:L267-L277] |
| Regression test definitions | Tests of listed methods, not further implementations for A8. [tests/unit/structural/test_offshore_container_dnv271.py:L19-L104] |

The documentary example is outside the `.py` discovery search; its embedded Python delegates to the listed section and angle methods, and its separate hand derivations are the independent geometry references of section 4. [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L47-L84] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L95-L127]

## 10. Coverage and open items

Coverage: every package Python file and subfolder was read: `__init__.py`, `dnv_2_7_1.py` and `docs/worked-shs-sling-geometry.md`; the parent structural pack explicitly limits its own scope to direct files, leaving this package separate. [src/digitalmodel/structural/offshore_container/__init__.py:L1-L23] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L1-L206] [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L1-L153] [src/digitalmodel/structural/docs/agent-pack.md:L7-L24]

Test discovery read both `tests/unit/structural/test_offshore_container_dnv271.py` and `tests/unit/structural/test_documented_shs_sling_geometry.py`; the re-count is eleven plus two executable test definitions, with all thirteen nodes listed in section 4. [tests/unit/structural/test_offshore_container_dnv271.py:L19-L104] [tests/unit/structural/test_documented_shs_sling_geometry.py:L56-L83]

Documentation discovery used `rg -l -i 'offshore.container|dnv_2_7_1|dnv.?st.?e271|dnvgl.?st.?e271' docs src/digitalmodel/structural examples/structural`; relevant engineering documentation is the package worked example, parent pack, external sweep README and sweep script, with the module-routing structural entry read separately. [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L1-L153] [src/digitalmodel/structural/docs/agent-pack.md:L7-L24] [examples/structural/offshore_container_utilization/README.md:L1-L56] [examples/structural/offshore_container_utilization/run_utilization_curves.py:L1-L214] [docs/registry/module-routing.yaml:L506-L530]

Additional discovery surfaces read for relevant entries: `docs/capability-map/capability-cards.json`, `docs/capability-map/capabilities-sections.yml`, `docs/capability-map/repo-architecture.json`, `docs/capability-map/repo-architecture.mmd`, `docs/capability-map/repo-architecture.html`, `docs/archive/archive-candidates-2026-10-08.summary.json`, `docs/registry/agent-pack-audit/production-nde-functions-3.json` and `docs/registry/implementation-map.yaml`; these inventory/navigation records are not independent expected-value sources. [docs/capability-map/capability-cards.json:L247-L253] [docs/capability-map/capabilities-sections.yml:L129-L137] [docs/capability-map/repo-architecture.json:L191-L198] [examples/structural/offshore_container_utilization/run_utilization_curves.py:L151-L201]

The citation contract was readable; the pack records the existing getter and its failure behavior rather than claiming that arithmetic automatically emits checked sidecars. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L132-L162] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L168-L196]

| Open item | Responsible role / tracking | Evidence and limit |
| --- | --- | --- |
| Input-domain and unit assumptions, S1–S7 and S14–S16 | Calculation maintainer; issue pending (1) | Existing geometry tests do not establish invalid-domain rejection. [tests/unit/structural/test_documented_shs_sling_geometry.py:L56-L83] [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L116-L162] |
| Count and nominal-angle behavior, S8–S9 | Calculation maintainer; issue pending (2) | Single-eye and nominal-angle factors are not calculation selectors. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L132-L147] |
| Citation error modes, custom-factor provenance and durable lesson link, S10–S13 | Calculation maintainer; issue pending (3), related #990 | Wrapper fallback and one-shot warning need additional negative tests. [src/digitalmodel/structural/offshore_container/dnv_2_7_1.py:L165-L196] |
| Implementation-map coverage | Map maintainer; issue pending (4) | SHS, sling geometry and offshore-container screening are not inventoried families for this package at the map pin. [docs/registry/implementation-map.yaml:L46711-L46724] |
| UNVERIFIED: implemented coefficients and clause interpretations agree with the licensed standard | Engineering calculations quality manager; #3325, related #990 | Clause text not checked against the licensed copy; code fixtures and synthetic frontmatter cannot establish this agreement. [tests/unit/structural/test_offshore_container_dnv271.py:L32-L47] [tests/unit/structural/test_offshore_container_dnv271.py:L83-L93] |
| Full utilization independent example, report builder and tested sweep output | Calculation maintainer; #3325 | Only geometry has the independent executable worked example; the sweep report computes its own expected-looking values. [src/digitalmodel/structural/offshore_container/docs/worked-shs-sling-geometry.md:L148-L153] [examples/structural/offshore_container_utilization/run_utilization_curves.py:L151-L201] |
