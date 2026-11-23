# Field development schematics knowledge pack

| Header | Value |
|---|---|
| Module | `field_development/schematics` |
| Repository | `digitalmodel` |
| Package | `src/digitalmodel/field_development/schematics` |
| Pack revision | 1 |
| Code revision | `f1d58a2ac0db65e68b5b690f0a609ef4746d114e` |
| Implementation-map data pin | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` on `codex/implmap-work`; map citations use this pin |
| Analysis family | Engineering calculations (`EC`); AQG r0, Python calculations |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT; clearance remains with the independent quality manager |

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 0 | 2026-10-18 | Initial package inventory and check coverage were recorded. |
| 1 | 2026-10-19 | Saturation, projection overlap, transparency and test limits are clarified; map citations are pinned to prevent revision drift. |

## 1. Purpose and scope

The package exports `SubseaTiebackSchematic`, `PlatformSchematic`, `FpsoSpreadSchematic` and `render_figure`; these construct Matplotlib figures and write SVG or PNG files. [src/digitalmodel/field_development/schematics/__init__.py:L11-L21] [src/digitalmodel/field_development/schematics/renderer.py:L21-L74]

The domain maturity is `beta`; this is routing metadata rather than a validation result. [docs/registry/module-routing.yaml:L141-L146]

Scope includes the `elements/` subpackage, schematic placement, annotations and file rendering; the parent API is described only at its boundary, where it dispatches to these classes and closes the saved figure. [src/digitalmodel/field_development/schematics/elements/__init__.py:L4-L44] [src/digitalmodel/field_development/schematic_generator.py:L113-L130] [src/digitalmodel/field_development/schematic_generator.py:L145-L181]

Use these outputs as illustrative layouts: the tieback uses fixed canvas positions and an interpolated curve, the platform draws a jacket and conductors, and the spread layout projects angle cosines onto a side view. [src/digitalmodel/field_development/schematics/subsea_tieback.py:L96-L141] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L273-L308] [src/digitalmodel/field_development/schematics/platform_standalone.py:L112-L164] [src/digitalmodel/field_development/schematics/fpso_spread.py:L140-L169]

Do not treat the scale annotation or drawn lines as a surveyed layout, a catenary equilibrium result, a mooring capacity check or an engineering design acceptance criterion. [src/digitalmodel/field_development/schematics/elements/annotations.py:L49-L73] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L273-L308] [src/digitalmodel/field_development/schematics/fpso_spread.py:L140-L189]

## 2. Inputs and outputs

The direct constructors accept the following inputs; clamping describes implemented behavior, not a recommended engineering range. [src/digitalmodel/field_development/schematics/subsea_tieback.py:L69-L81] [src/digitalmodel/field_development/schematics/platform_standalone.py:L59-L72] [src/digitalmodel/field_development/schematics/fpso_spread.py:L63-L79]

| Input / route | Unit and range behavior | Default / upstream source | Evidence |
|---|---|---|---|
| `water_depth_m`, all constructors | m; converted with `float`, no positivity or finiteness guard here | Required; parent API rejects values at or below zero | [src/digitalmodel/field_development/schematics/subsea_tieback.py:L69-L94] [src/digitalmodel/field_development/schematics/platform_standalone.py:L59-L79] [src/digitalmodel/field_development/schematics/fpso_spread.py:L63-L79] [src/digitalmodel/field_development/schematic_generator.py:L82-L89] |
| `n_templates`, tieback | count; clamped to 1–6 | Required directly; parent defaults to 1 | [src/digitalmodel/field_development/schematics/subsea_tieback.py:L69-L81] [src/digitalmodel/field_development/schematic_generator.py:L91-L94] |
| `flowline_length_km`, tieback | km; `float`; scale factor has a lower bound of 1.0, but annotation receives the supplied length | Required directly; parent defaults to 5.0 km | [src/digitalmodel/field_development/schematics/subsea_tieback.py:L69-L81] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L245-L251] [src/digitalmodel/field_development/schematic_generator.py:L91-L95] |
| `host_type`, tieback | Text; normalized to uppercase except `fixed`; unrecognized types use a platform patch | Required directly; parent defaults to FPSO | [src/digitalmodel/field_development/schematics/subsea_tieback.py:L69-L81] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L310-L318] [src/digitalmodel/field_development/schematic_generator.py:L91-L95] |
| `n_wells`, tieback render | count; clamped to 1–8 by `render`; helper alone only caps at 8 | 3 | [src/digitalmodel/field_development/schematics/subsea_tieback.py:L115-L141] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L147-L171] |
| `n_wells`, platform | count; clamped to 1–8 | Required | [src/digitalmodel/field_development/schematics/platform_standalone.py:L59-L72] |
| `host_type`, platform | Text label; jacket geometry does not branch on the label | Required | [src/digitalmodel/field_development/schematics/platform_standalone.py:L59-L72] [src/digitalmodel/field_development/schematics/platform_standalone.py:L112-L126] [src/digitalmodel/field_development/schematics/platform_standalone.py:L188-L198] |
| `n_satellite_wells`, platform | count; minimum 0, no upper cap | 0 | [src/digitalmodel/field_development/schematics/platform_standalone.py:L59-L72] [src/digitalmodel/field_development/schematics/platform_standalone.py:L203-L222] |
| `satellite_tieback_km`, platform | km; `float`; annotation minimum 1.0 km, satellite positions remain fixed canvas interpolation | 3.0 km | [src/digitalmodel/field_development/schematics/platform_standalone.py:L59-L72] [src/digitalmodel/field_development/schematics/platform_standalone.py:L187-L187] [src/digitalmodel/field_development/schematics/platform_standalone.py:L203-L222] |
| `n_mooring_lines`, spread | count; minimum 3, no upper cap | Required directly; parent defaults to 8 | [src/digitalmodel/field_development/schematics/fpso_spread.py:L63-L90] [src/digitalmodel/field_development/schematic_generator.py:L166-L172] |
| `n_subsea_wells`, spread | count; minimum 1, no upper cap | Required directly; parent uses wells times maximum of templates and 1 | [src/digitalmodel/field_development/schematics/fpso_spread.py:L63-L72] [src/digitalmodel/field_development/schematic_generator.py:L166-L172] |
| `field_name`, `figsize`, `facecolor`, render methods | Text, inches, Matplotlib color respectively | Name required; size `(12, 7)` inches; background `#f0f4f8` | [src/digitalmodel/field_development/schematics/subsea_tieback.py:L147-L178] [src/digitalmodel/field_development/schematics/platform_standalone.py:L81-L107] [src/digitalmodel/field_development/schematics/fpso_spread.py:L92-L118] |
| `fig`, `output_path`, `output_format`, `dpi`, `transparent`, renderer | Figure; destination text; `svg` or `png` after normalization; dots/inch; boolean | First three required; DPI `None` becomes 300 for PNG; transparency false; explicit figure face color keeps PNG background opaque even with `transparent=True`; SVG ignores DPI | [src/digitalmodel/field_development/schematics/renderer.py:L16-L27] [src/digitalmodel/field_development/schematics/renderer.py:L53-L74] |

The parent API reads a dictionary, requires `name`, `water_depth_m` and `output_path`, converts counts with `int`, and validates the output format; zero templates or a fixed/jacket host infer a platform, otherwise a tieback, while spread mooring requires the explicit development type. An unrecognized `development_type` silently selects the tieback branch. [src/digitalmodel/field_development/schematic_generator.py:L82-L111] [src/digitalmodel/field_development/schematic_generator.py:L133-L181]

| Output | Unit / convention | Return and evidence |
|---|---|---|
| Three schematic `render` methods | Canvas axes span 0–1 horizontally and vertically; figures hide axes | Matplotlib `Figure` [src/digitalmodel/field_development/schematics/subsea_tieback.py:L173-L178] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L266-L267] [src/digitalmodel/field_development/schematics/platform_standalone.py:L102-L107] [src/digitalmodel/field_development/schematics/platform_standalone.py:L200-L201] [src/digitalmodel/field_development/schematics/fpso_spread.py:L113-L118] [src/digitalmodel/field_development/schematics/fpso_spread.py:L212-L213] |
| Template / well positions | Dimensionless axes fractions | Lists of `(x, y)` tuples [src/digitalmodel/field_development/schematics/subsea_tieback.py:L96-L141] |
| Mooring angles | Degrees; zero points right; increasing angle feeds cosine for horizontal projection | List of `i * 360/n` [src/digitalmodel/field_development/schematics/fpso_spread.py:L81-L90] [src/digitalmodel/field_development/schematics/fpso_spread.py:L140-L149] |
| SVG / PNG | Artist coordinates transformed by Matplotlib; PNG default 300 dots/inch; tight bounding box | Destination string, parent directories created; supplied relative paths remain relative [src/digitalmodel/field_development/schematics/renderer.py:L53-L74] |
| Element helpers | Axes fractions; icon dimensions dimensionless; annotation distances km and depths m, rounded to whole units | Patch, artist list, line or text [src/digitalmodel/field_development/schematics/elements/icons.py:L15-L23] [src/digitalmodel/field_development/schematics/elements/annotations.py:L19-L73] [src/digitalmodel/field_development/schematics/elements/annotations.py:L76-L114] [src/digitalmodel/field_development/schematics/elements/seabed.py:L121-L131] |

Coordinate origin is the lower-left canvas corner, with positive x right and positive y up, through `ax.transAxes` and limits 0–1; the north arrow is an annotation at fixed canvas coordinates, with no geographic transform or phase input in these render methods. [src/digitalmodel/field_development/schematics/subsea_tieback.py:L173-L178] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L245-L251] [src/digitalmodel/field_development/schematics/elements/annotations.py:L151-L187]

## 3. Method and governing standards

Method basis is the following drawing construction; no standard compliance or licensed clause verification is claimed for these layout formulas. [src/digitalmodel/field_development/schematics/elements/seabed.py:L27-L53] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L87-L141] [src/digitalmodel/field_development/schematics/fpso_spread.py:L74-L90]

| Method | Implementation and basis |
|---|---|
| Water-column height | `compute_water_column_height` computes `clip(water_depth_m / fig_height_m, min_frac, max_frac)`, default limits 0.15 and 0.75; this is a dimensionless drawing ratio [src/digitalmodel/field_development/schematics/elements/seabed.py:L27-L53] |
| Seabed placement | Tieback uses `fig_height_m = max(1.25 * depth, 500)` and nominally clips `1-frac` to 0.18–0.60; platform uses floor 200 m and nominal range 0.18–0.65; spread uses floor 800 m and nominal range 0.15–0.55. For positive finite depths the reached ranges are 0.25–0.60, 0.25–0.65 and 0.25–0.55 respectively: the lower clamps are unreachable. Positions vary only over 200–375 m, 70–150 m and 360–600 m respectively, and saturate outside those bands; tiebacks at 400 m and 3000 m have the same seabed position [src/digitalmodel/field_development/schematics/subsea_tieback.py:L87-L94] [src/digitalmodel/field_development/schematics/platform_standalone.py:L74-L79] [src/digitalmodel/field_development/schematics/fpso_spread.py:L74-L79] |
| Template placement | Even interpolation from x=0.12 to `0.82-0.10`, or midpoint for one template; y is seabed minus 0.04 axes fraction [src/digitalmodel/field_development/schematics/subsea_tieback.py:L45-L51] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L96-L113] |
| Wells | Tieback spacing is three well radii about the template centre, below the template; platform conductors divide jacket width by `n_wells+1`; spread wells use 0.06 axes-fraction spacing [src/digitalmodel/field_development/schematics/subsea_tieback.py:L135-L141] [src/digitalmodel/field_development/schematics/platform_standalone.py:L128-L140] [src/digitalmodel/field_development/schematics/fpso_spread.py:L171-L189] |
| Flowline | `_draw_flowline` uses a degree-2 interpolating spline through three canvas control points and samples 60 points; the import occurs before the exception handler, which affects the advertised straight-line fallback [src/digitalmodel/field_development/schematics/subsea_tieback.py:L273-L308] |
| Spread angles and anchors | `compute_mooring_angles` divides a full circle by line count; render uses cosine projection, placing all anchor endpoints at seabed plus 0.01 axes fraction [src/digitalmodel/field_development/schematics/fpso_spread.py:L81-L90] [src/digitalmodel/field_development/schematics/fpso_spread.py:L140-L169] |
| Element construction | Rounded rectangle for FPSO, polygon for platform, rectangle for template, circle for well; these are drawing symbols [src/digitalmodel/field_development/schematics/elements/icons.py:L33-L166] |
| Seabed and annotations | `draw_seabed_line` creates optional solid-color water and hatched sediment rectangles; scale and depth strings round to whole km and m [src/digitalmodel/field_development/schematics/elements/seabed.py:L90-L131] [src/digitalmodel/field_development/schematics/elements/annotations.py:L49-L114] |

Citation-sidecar disposition: no standards-derived engineering constant is introduced by this pack; the listed constants control drawing dimensions and the code returns figures, positions or paths. [src/digitalmodel/field_development/schematics/elements/icons.py:L15-L23] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L45-L51] [src/digitalmodel/field_development/schematics/renderer.py:L63-L74]

## 4. Worked regression examples

V1 is a regression check: its expected adjacent gap `360/8 = 45.0` degrees repeats the implementation formula. It checks neither the start angle nor the wrap-around gap and provides no independent validation. [tests/test_field_development_schematic.py:L404-L414]

Results in this section are from the offline runs at the header code revision; named receipts are local check records, not bundled output data. [tests/test_field_development_schematic.py:L210-L285] [tests/test_field_development_schematic.py:L358-L414] [tests/test_field_development_schematic.py:L421-L442]

| ID / kind | Source and input | Expected value, unit and tolerance basis | Existing test node ID | Offline result |
|---|---|---|---|---|
| V1 regression | Equal-angle closed form; count 8, inline test input | 8 angles; adjacent gaps 45.0 degrees; absolute error less than 0.01 degrees, as asserted by the test | `tests/test_field_development_schematic.py::TestFpsoSpreadSchematic::test_mooring_line_angles_evenly_distributed` | PASS at BASE and AFTER [tests/test_field_development_schematic.py:L404-L414] |
| R1 regression | Inline layout fixture, four templates | 4 positions; every adjacent x-gap differs from the first by less than `1e-6` axes fraction; internal consistency, not an independent location benchmark | `tests/test_field_development_schematic.py::TestLayoutMath::test_template_x_positions_evenly_spaced` | PASS at BASE and AFTER [tests/test_field_development_schematic.py:L213-L223] |
| R2 regression | Inline template centre and four wells | Mean x within 0.01 axes fraction of 0.5; assertion tolerance is schematic rather than surveying accuracy | `tests/test_field_development_schematic.py::TestLayoutMath::test_well_positions_symmetric_around_template` | PASS at BASE and AFTER [tests/test_field_development_schematic.py:L272-L285] |
| R3 regression | Inline platform with two satellite wells | 1 non-null figure; no image-content or dimensional acceptance assertion | `tests/test_field_development_schematic.py::TestPlatformSchematic::test_render_with_satellite_wells` | PASS at BASE and AFTER [tests/test_field_development_schematic.py:L372-L383] |
| R4 regression | Synthetic line on a figure; temporary PNG destination | 1 existing output file, size greater than 0 bytes | `tests/test_field_development_schematic.py::TestRenderer::test_render_png_creates_file` | PASS at BASE and AFTER [tests/test_field_development_schematic.py:L424-L432] |
| R5 regression | Synthetic line on a figure; temporary SVG destination | 1 existing output file, size greater than 0 bytes | `tests/test_field_development_schematic.py::TestRenderer::test_render_svg_creates_file` | PASS at BASE and AFTER [tests/test_field_development_schematic.py:L434-L442] |

UNVERIFIED: Physical scale accuracy and independent image correctness for the three complete schematic routes remain unestablished; the render assertions principally check object or file existence, not a reference drawing; issue pending (6). [tests/test_field_development_schematic.py:L295-L351] [tests/test_field_development_schematic.py:L361-L414] [tests/test_field_development_schematic.py:L424-L449]

## 5. QA gates and reproducible checks

Gate applicability follows AQG r0, section 10, `Python calculations`, with G5 required by section 9; gate coverage here is an offline self-check, not independent clearance. (AQG r0, sections 9–11.)

| Gate | Disposition and route | Checks |
|---|---|---|
| G0 | Applicable: dictionary inputs, constructor boundaries, units and citation ranges | C01, C02, C03, C10 |
| G1 | Applicable: Python import and pytest route; SVG/PNG checks assert file existence and size; one SVG is read as text, and no PNG content is loaded, with no solver load route | C04, C05 |
| G2 | Applicable: regression angular spacing and schematic layout invariants | C06, C07, C08 |
| G3 | NOT_APPLICABLE: drawing methods contain no time integration | [src/digitalmodel/field_development/schematics/fpso_spread.py:L140-L213] [src/digitalmodel/field_development/schematics/platform_standalone.py:L102-L201] |
| G4 | NOT_APPLICABLE: symbol and line construction does not produce a diffraction mesh | [src/digitalmodel/field_development/schematics/elements/icons.py:L33-L166] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L273-L308] |
| G5 | Applicable: record pin, node IDs, output checks and explicit limits in the evidence appendix | C09 |

Check definitions below are acceptance proposals for this package; their numeric criteria are tied to the cited assertions or to explicit documentation completeness, not structural design tolerances.

| ID | Module-specific quantity and numeric pass criterion | Executable route / evidence | Gate |
|---|---|---|---|
| C01 | Parent `generate_field_schematic`: exactly 1 `ValueError` for negative depth and 1 for invalid output format, 0 unexpected exceptions; unit: exception count | `tests/field_development/test_field_development.py::TestGenerateFieldSchematic::test_negative_water_depth_raises_value_error` and `tests/field_development/test_field_development.py::TestGenerateFieldSchematic::test_invalid_format_raises_value_error`; PASS [tests/field_development/test_field_development.py:L136-L153] | G0 |
| C02 | `PlatformSchematic` with requested count 20 returns conductor count at most 8; tieback helper requested count 12 returns at most 8; unit: count | `tests/field_development/test_field_development.py::TestSchematicInstantiation::test_platform_schematic_caps_wells` and `tests/test_field_development_schematic.py::TestSubseaTiebackSchematic::test_n_wells_clamped_to_max_8`; PASS; observes silent truncation, does not certify user intent [tests/field_development/test_field_development.py:L95-L98] [tests/test_field_development_schematic.py:L345-L351] | G0 |
| C03 | Every bracketed source locator resolves at its declared pin with 0 missing files and 0 out-of-range lines; unit: citation failures | Read each cited file using `git show`, assert `1 <= start <= end <= line_count`; code pin in header, map pin in section 9; retain count and ten read-back excerpts | G0 |
| C04 | Imports and all 70 solver-free module/API tests pass; exactly 0 failures, skips or NOT_RUN cases; unit: test count | Test command below; section-4 node receipts and complete pytest result list [tests/test_field_development_schematic.py:L84-L608] [tests/field_development/test_field_development.py:L23-L184] | G1 |
| C05 | `render_figure` produces both formats with size greater than 0 bytes and rejects BMP with exactly 1 `ValueError`; unit: byte and exception counts | R4, R5 and `tests/test_field_development_schematic.py::TestRenderer::test_render_invalid_format_raises`; PASS; directory creation also exercised [tests/test_field_development_schematic.py:L424-L465] | G1 |
| C06 | `compute_mooring_angles`: 8 outputs, adjacent gap error less than 0.01 degrees against `360/8`; unit: degrees | V1; regression comparison; start angle and wrap-around gap unchecked; PASS [tests/test_field_development_schematic.py:L404-L414] | G2 |
| C07 | `compute_template_positions`: 4 positions with gap mismatch less than `1e-6` axes fraction, and 3-template positions each within 0–1 axes fraction; unit: dimensionless | R1 and `tests/test_field_development_schematic.py::TestLayoutMath::test_template_positions_within_canvas`; PASS [tests/test_field_development_schematic.py:L213-L235] | G2 |
| C08 | `compute_well_positions`: mean x error less than 0.01 axes fraction for 4 wells; `compute_water_column_height(100,500)` less than `(400,500)` and deep clamp within 0–1; tieback seabed y at 1000 m less than at 100 m; unit: dimensionless | R2, `tests/test_field_development_schematic.py::TestSeabedElements::test_compute_water_column_height_scaling`, `tests/test_field_development_schematic.py::TestSeabedElements::test_compute_water_column_clamped_to_range` and `tests/test_field_development_schematic.py::TestLayoutMath::test_seabed_y_increases_with_water_depth`; PASS; shallow/deep ordering and broad bounds only; cannot detect saturation at 400 m versus 3000 m [tests/test_field_development_schematic.py:L184-L191] [tests/test_field_development_schematic.py:L237-L246] [tests/test_field_development_schematic.py:L272-L285] | G2 |
| C09 | Evidence appendix has 6 example rows with node IDs, 6 recorded results per run, 2 complete module-run summaries and 0 unresolved citations; unit: record count | Compare section 4 against pytest PASS lists and section-10 coverage; preserve code pin, exact command form and exception reasons; independent image acceptance remains open | G5 |
| C10 | `add_depth_label` for 450 m produces text with substring `450`; no exact text or matching-label count assertion, unit: substring presence | `tests/test_field_development_schematic.py::TestAnnotationElements::test_depth_label_contains_depth_value`; PASS; integer display only [tests/test_field_development_schematic.py:L147-L152] | G0 |

Use the following generic command form with a fresh persistent temporary directory per run; the executable placeholder denotes the installed uv executable and the temporary-root placeholder denotes external scratch storage:

```text
PYTHONPATH=src <uv> run --no-sources python -m pytest \
  tests/test_field_development_schematic.py \
  tests/field_development/test_field_development.py \
  -q -rA -p no:cacheprovider --basetemp=<temporary-root>/bt-<run>
```

Do not treat the scale test as a reference check: its assertion ends in `or True` and cannot detect a broken scale factor; no detection check exists for scale fidelity. [tests/test_field_development_schematic.py:L248-L257]

## 6. Known failure modes and silent-failure sweep

sweep: 31 found (30 package sites and 1 parent-dispatch boundary site)

The count is of classified sites below, including intentional defaults and unit assumptions, rather than raw grep matches; the exception handler is the only swallowed-error construct, and no `continue`, explicit `return None`, empty return on an error path or cache write occurs in the package's Python definitions. [src/digitalmodel/field_development/schematics/subsea_tieback.py:L273-L308] [src/digitalmodel/field_development/schematics/renderer.py:L21-L74] [src/digitalmodel/field_development/schematics/elements/seabed.py:L27-L131]

Reproduce the textual sweep over the whole package, then inspect each hit and all definitions (including implicit empty results from `range`); styling margins are drawing constants rather than physical percentages:

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|clip\(|min\(|max\(|_m\b|_km\b|cache' src/digitalmodel/field_development/schematics --glob '*.py'
```

The original rows were inspected on 2026-10-18 and all rows were re-inspected on 2026-10-19 at the header pin; a check named in the table detects only its stated symptom, and `no detection check exists` denotes a missing executable guard rather than a passing gate.

| Site | Symptom / cause and evidence | Detection / follow-up |
|---|---|---|
| S01 | Importing SciPy occurs before `try`; absent SciPy raises before the advertised fallback; an `ImportError` inside the try instead silently draws a straight segment [src/digitalmodel/field_development/schematics/subsea_tieback.py:L282-L308] | no detection check exists; issue pending (1) |
| S02 | Template requests outside 1–6 silently change count [src/digitalmodel/field_development/schematics/subsea_tieback.py:L76-L76] | no detection check exists; issue pending (2) |
| S03 | Helper wells are only upper-capped; non-positive requests produce an empty range and no symbols [src/digitalmodel/field_development/schematics/subsea_tieback.py:L135-L141] | C02 detects upper truncation only; no detection check exists for non-positive helper counts; issue pending (2) |
| S04 | Tieback `render` silently clamps wells to 1–8; default 3 [src/digitalmodel/field_development/schematics/subsea_tieback.py:L147-L171] | no detection check exists for lower clamp; issue pending (2) |
| S05 | Platform conductors silently clamp to 1–8 [src/digitalmodel/field_development/schematics/platform_standalone.py:L67-L68] | C02 observes upper clamp; no detection check exists for lower clamp; issue pending (2) |
| S06 | Satellite wells default to zero and negative requests silently become zero; no upper cap prevents crowded drawings [src/digitalmodel/field_development/schematics/platform_standalone.py:L64-L70] [src/digitalmodel/field_development/schematics/platform_standalone.py:L203-L222] | no detection check exists; issue pending (2) |
| S07 | Spread mooring count silently rises to at least 3 [src/digitalmodel/field_development/schematics/fpso_spread.py:L70-L70] | no detection check exists for clamp; issue pending (2) |
| S08 | Spread well count silently rises to at least 1; fixed spacing with no upper cap can extend beyond the canvas [src/digitalmodel/field_development/schematics/fpso_spread.py:L71-L71] [src/digitalmodel/field_development/schematics/fpso_spread.py:L171-L189] | no detection check exists; issue pending (2) |
| S09 | Tieback `float(water_depth_m)` assumes metres without finite/positive validation at the constructor [src/digitalmodel/field_development/schematics/subsea_tieback.py:L77-L77] | C01 guards the parent negative-depth route only; no detection check exists for direct constructors; issue pending (2) |
| S10 | Tieback length is converted assuming km, but `x_scale_factor` compares the original argument with 1.0; numeric text can pass conversion and then raise `TypeError` [src/digitalmodel/field_development/schematics/subsea_tieback.py:L78-L80] | no detection check exists; issue pending (2) |
| S11 | Platform depth conversion assumes metres without finite/positive validation [src/digitalmodel/field_development/schematics/platform_standalone.py:L67-L67] | no detection check exists for direct constructor; issue pending (2) |
| S12 | Satellite distance assumes km and defaults to 3.0; conversion accepts no locale normalization [src/digitalmodel/field_development/schematics/platform_standalone.py:L65-L71] | no detection check exists; issue pending (2) |
| S13 | Spread depth conversion assumes metres without finite/positive validation [src/digitalmodel/field_development/schematics/fpso_spread.py:L69-L69] | no detection check exists for direct constructor; issue pending (2) |
| S14 | Water ratio assumes matching metre units; zero figure height divides by zero, and default 0.15–0.75 clamp can conceal implausible depths [src/digitalmodel/field_development/schematics/elements/seabed.py:L27-L53] | C08 covers monotonicity and broad clamp bounds only; no detection check exists for zero/non-finite input; issue pending (2) |
| S15 | Tieback notional height floor 500 m, multiplier 1.25 and secondary clamp saturate depth changes outside 200–375 m; reached y range 0.25–0.60, lower clamp unreachable [src/digitalmodel/field_development/schematics/subsea_tieback.py:L87-L94] | C08 detects shallow/deep ordering; no detection check exists for physical scale; issue pending (3) |
| S16 | Platform floor 200 m, multiplier 1.25 and secondary clamp saturate depth changes outside 70–150 m; reached y range 0.25–0.65, lower clamp unreachable [src/digitalmodel/field_development/schematics/platform_standalone.py:L74-L79] | no detection check exists; issue pending (3) |
| S17 | Spread floor 800 m, multiplier 1.25 and secondary clamp saturate depth changes outside 360–600 m; reached y range 0.25–0.55, lower clamp unreachable [src/digitalmodel/field_development/schematics/fpso_spread.py:L74-L79] | no detection check exists; issue pending (3) |
| S18 | Any unrecognized tieback host type draws a platform; TLP and SPAR share that symbol [src/digitalmodel/field_development/schematics/subsea_tieback.py:L310-L318] | no detection check exists for unknown host fallback; issue pending (2) |
| S19 | Icon width, height and radius are axes fractions, not metres; factories do not set the transform themselves [src/digitalmodel/field_development/schematics/elements/icons.py:L15-L23] [src/digitalmodel/field_development/schematics/elements/icons.py:L58-L70] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L332-L335] | no detection check exists for physical interpretation; issue pending (3) |
| S20 | Scale bar has fixed 0.12 axes-fraction length and rounds the supplied km label to a whole number; it does not calibrate the drawing [src/digitalmodel/field_development/schematics/elements/annotations.py:L19-L73] | no detection check exists; issue pending (3) |
| S21 | Depth label assumes m and rounds to a whole metre [src/digitalmodel/field_development/schematics/elements/annotations.py:L76-L114] | C10 covers an integer only; no detection check exists for fractional-unit interpretation; issue pending (3) |
| S22 | Title placement defaults to fixed canvas coordinates; string is displayed without interpretation [src/digitalmodel/field_development/schematics/elements/annotations.py:L117-L148] | no detection check exists for semantic placement; issue pending (3) |
| S23 | North arrow defaults to fixed canvas position/length and always points up; no geographic rotation input [src/digitalmodel/field_development/schematics/elements/annotations.py:L151-L187] | no detection check exists for geographic fidelity; issue pending (3) |
| S24 | Seabed extents default to 0.0–1.0, water/sediment fills default true; solid water alpha 0.35 differs from the gradient description [src/digitalmodel/field_development/schematics/elements/seabed.py:L56-L65] [src/digitalmodel/field_development/schematics/elements/seabed.py:L90-L119] | no detection check exists for gradient or physical extents; issue pending (3) |
| S25 | Template patch uses `label or ""`, so falsy labels silently become empty [src/digitalmodel/field_development/schematics/elements/icons.py:L109-L140] | no detection check exists; issue pending (2) |
| S26 | PNG DPI defaults to 300 when `None`; SVG ignores supplied DPI, and tight bounds can alter final pixel dimensions [src/digitalmodel/field_development/schematics/renderer.py:L63-L74] | C05 checks nonempty files only; no detection check exists for resolution metadata; issue pending (6) |
| S27 | Figure size defaults to inches and transparency defaults false; face color is passed explicitly to save, overriding transparent background intent: `transparent=True` still gives an opaque PNG background (alpha 1.0 at every pixel in the inspected default-color output) [src/digitalmodel/field_development/schematics/platform_standalone.py:L81-L107] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L147-L175] [src/digitalmodel/field_development/schematics/fpso_spread.py:L92-L115] [src/digitalmodel/field_development/schematics/renderer.py:L21-L27] [src/digitalmodel/field_development/schematics/renderer.py:L63-L71] | no detection check exists for dimensions/transparency semantics; issue pending (6) |
| S28 | Platform scale annotation has minimum 1.0 km, independent of fixed satellite positions; tieback scale factor is stored but layout positions stay fixed [src/digitalmodel/field_development/schematics/platform_standalone.py:L187-L187] [src/digitalmodel/field_development/schematics/platform_standalone.py:L203-L208] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L80-L80] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L103-L110] | no detection check exists; issue pending (3) |
| S29 | Side-view mooring endpoints use only cosine; `dy` is unused and every anchor y is seabed plus 0.01, so mirrored lines overlap: 8 lines give 5 distinct anchor positions [src/digitalmodel/field_development/schematics/fpso_spread.py:L140-L169] | no detection check exists; issue pending (3) |
| S30 | At high template/well counts, tieback well rows belonging to neighbouring templates overlap: spacing 0.024 gives a 0.168-wide row for 8 wells, exceeding the 0.12 template gap at 6 templates [src/digitalmodel/field_development/schematics/subsea_tieback.py:L96-L141] | no detection check exists; issue pending (3) |
| S31 | Parent dispatch boundary: unknown `development_type` silently selects a tieback rather than rejecting the value [src/digitalmodel/field_development/schematic_generator.py:L156-L181] | no detection check exists; issue pending (2) |

Additional pitfalls: the spread module's opening text says plan view while its class text and implementation describe/project a side view; the flowline-scale assertion is tautological, and the PNG DPI test checks existence without reading resolution metadata; issue pending (3) and issue pending (6). [src/digitalmodel/field_development/schematics/fpso_spread.py:L1-L10] [src/digitalmodel/field_development/schematics/fpso_spread.py:L45-L51] [src/digitalmodel/field_development/schematics/fpso_spread.py:L140-L149] [tests/test_field_development_schematic.py:L248-L257] [tests/test_field_development_schematic.py:L444-L450]

## 7. Lessons log

Append entries after this row; do not rewrite earlier entries.

| Date | Entry | Lesson and evidence | Pack change | Shared-log reference |
|---|---|---|---|---|
| 2026-10-18 | Initial source audit | A passing file-existence test does not establish scale or image correctness; an assertion containing `or True` cannot reject a wrong value [tests/test_field_development_schematic.py:L248-L257] [tests/test_field_development_schematic.py:L424-L450] | Separate closed-form angular validation from render regressions; enumerate silent clamps and missing guards | Not created; issue pending (6) |

## 8. Ownership and clearance

Review assignments for this draft: engineering calculations quality manager, with a deputy engineering calculations quality manager on an independent provider; producing role: engineering calculations documentation contributor.

Escalate ambiguous geometry, tolerance changes and missing references to the family quality manager; cross-family boundaries go to the maintainers. Clearance must be independent of the producer. (AQG r0, section 11, sign-off and independence.)

Keep status DRAFT until the quality manager resolves the cited coverage and issue-draft references; this pack assigns no clearance or approval.

## 9. Dependencies and implementation inventory

| Boundary | Dependency / consumer and evidence |
|---|---|
| Upstream configuration | Parent `generate_field_schematic` routes a dictionary to the three constructors; CLI parses arguments and invokes that API [src/digitalmodel/field_development/schematic_generator.py:L82-L125] [src/digitalmodel/field_development/schematic_generator.py:L145-L181] [scripts/generate_schematic.py:L121-L148] |
| Drawing stack | Matplotlib figures/patches, NumPy interpolation and clipping, and SciPy spline creation; declared requirements include Matplotlib, NumPy and SciPy [src/digitalmodel/field_development/schematics/subsea_tieback.py:L19-L41] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L282-L290] [pyproject.toml:L50-L52] [pyproject.toml:L72-L73] |
| Standard library | `pathlib.Path` creates destination parents; `math` converts spread angles for cosine projection [src/digitalmodel/field_development/schematics/renderer.py:L60-L74] [src/digitalmodel/field_development/schematics/fpso_spread.py:L140-L149] |
| Downstream rendering reuse | `visualization.plot_field_layout` and `onshore_layout.plot_layout` call the same renderer after drawing their own metric-coordinate layouts; these are consumers, not alternate implementations of `render_figure` [src/digitalmodel/field_development/visualization.py:L198-L205] [src/digitalmodel/field_development/onshore_layout.py:L458-L464] |
| Solver/data boundary | This package's construction methods operate on primitive inputs and drawing elements; no solver version or reference dataset is required by the listed offline tests [src/digitalmodel/field_development/schematics/subsea_tieback.py:L69-L81] [src/digitalmodel/field_development/schematics/platform_standalone.py:L59-L72] [src/digitalmodel/field_development/schematics/fpso_spread.py:L63-L72] [tests/test_field_development_schematic.py:L16-L42] |

Inventory boundary: only this package and its `elements/` subpackage; adjacent economic, terrain, catenary and workflow calculations retain their separate scope.

Implementation-map data pin is the header map revision; its declared searched-code revision is `270c8759c374b67a7f032953e7fcdb14e0224461`, distinct from this pack's code pin. [docs/registry/implementation-map.yaml@3256bab9:L1-L4]

There are **0 module map families and 0 implementation rows** whose `path` lies inside this package in the map's family `implementations` lists; consequently the required section-9 implementation inventory is empty and the ranked counted-family set is empty. [docs/registry/implementation-map.yaml@3256bab9:L345-L26502]

The map does contain four `rejected_candidates`: `draw_seabed_line` and the three `_compute_seabed_y` methods, associated with `linear-wave-kinematics` and explicitly excluded from implementation counts pending inspection; they are not family implementation rows. [docs/registry/implementation-map.yaml@3256bab9:L53340-L53367]

| A8 step | Result |
|---|---|
| Inventory and ranking | PASS: 0 in-package implementation rows, 0 module families, 0 counted families [docs/registry/implementation-map.yaml@3256bab9:L345-L26502] |
| Counted-family Python `re` grep | NOT_APPLICABLE: no module map family exists; no family signals are selected or excluded [docs/registry/implementation-map.yaml@3256bab9:L345-L26502] |
| Further second implementations | No counted-family search is applicable; no numerical miss count is asserted for an unsearched family |

Reproduce the A8 inventory by parsing the map at its named data pin, filtering each family's `implementations` by the package prefix, sorting families by descending in-package row count with stable file-order ties, and selecting at most three families; do not include `rejected_candidates`.

Supplementary whole-repository discovery includes `scripts/` and uses:

```sh
git grep -n -I -E 'compute_water_column_height|compute_mooring_angles|compute_template_positions|compute_well_positions|SubseaTiebackSchematic|FpsoSpreadSchematic|PlatformSchematic|render_figure|generate_field_schematic' <code-pin> -- '*.py' '*.md'
```

The matched parent API and CLI delegate to the listed definitions; the two metric-layout modules draw separate layouts and delegate file export, so their renderer calls are helpers, not additional file-export implementations. [src/digitalmodel/field_development/schematic_generator.py:L113-L130] [scripts/generate_schematic.py:L121-L148] [src/digitalmodel/field_development/visualization.py:L198-L205] [src/digitalmodel/field_development/onshore_layout.py:L458-L464]

Map follow-up: classify depth-ratio canvas mapping, template/well placement, spline drawing, equal-angle distribution and figure export before adding any family rows; these are the section-3 methods absent as in-package implementations, issue pending (5). [docs/registry/implementation-map.yaml@3256bab9:L345-L26502] [src/digitalmodel/field_development/schematics/elements/seabed.py:L27-L53] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L96-L141] [src/digitalmodel/field_development/schematics/subsea_tieback.py:L273-L308] [src/digitalmodel/field_development/schematics/fpso_spread.py:L81-L90] [src/digitalmodel/field_development/schematics/renderer.py:L53-L74]

## 10. Coverage and open items

Source coverage is the complete Python package at the code pin, including both initializers and the element subfolder. [src/digitalmodel/field_development/schematics/__init__.py:L1-L21] [src/digitalmodel/field_development/schematics/elements/__init__.py:L1-L44]

| Files read | Range at pin |
|---|---|
| Package initializer | [src/digitalmodel/field_development/schematics/__init__.py:L1-L21] |
| Tieback | [src/digitalmodel/field_development/schematics/subsea_tieback.py:L1-L335] |
| Platform | [src/digitalmodel/field_development/schematics/platform_standalone.py:L1-L226] |
| Spread | [src/digitalmodel/field_development/schematics/fpso_spread.py:L1-L213] |
| Renderer | [src/digitalmodel/field_development/schematics/renderer.py:L1-L74] |
| Elements initializer | [src/digitalmodel/field_development/schematics/elements/__init__.py:L1-L44] |
| Annotations | [src/digitalmodel/field_development/schematics/elements/annotations.py:L1-L187] |
| Icons | [src/digitalmodel/field_development/schematics/elements/icons.py:L1-L166] |
| Seabed | [src/digitalmodel/field_development/schematics/elements/seabed.py:L1-L131] |
| Parent API and CLI | [src/digitalmodel/field_development/schematic_generator.py:L1-L181] [scripts/generate_schematic.py:L1-L148] |
| Direct test files | [tests/test_field_development_schematic.py:L1-L608] [tests/field_development/test_field_development.py:L1-L184] |
| Module-specific documentation discovery | `docs/field-development/subsea-production-systems-mapping.md` lists this module with no standard assigned [docs/field-development/subsea-production-systems-mapping.md:L75-L85] |
| Routing | Existing beta domain entry [docs/registry/module-routing.yaml:L141-L146] |

Use the source coverage table as the package read inventory, with identifying discovery content handled below.

UNVERIFIED: A public, neutral source locator for 1 discovery document with an identifying name is unavailable in this pack; that document was read but its path and identifying content are withheld, issue pending (4).

Counts re-derived from test definitions are 51 tests in `tests/test_field_development_schematic.py` and 19 in `tests/field_development/test_field_development.py`, total 70, matching collection; tests for adjacent domain functions are outside this package's direct test inventory. [tests/test_field_development_schematic.py:L84-L608] [tests/field_development/test_field_development.py:L23-L184]

| Offline command / receipt | State tested | Run | Passed | Failed | Skipped | NOT_RUN |
|---|---|---|---|---|---|---|
| Module command in section 5; `module-base` | Header code pin before edits | 70 | 70 | 0 | 0 | 0 |
| Same module command; `module-after` | Uncommitted documentation tree, module code still at header pin | 70 | 70 | 0 | 0 | 0 |
| Same uv/pytest form with `tests/contracts tests/docs/test_digitalmodel_routing_contract.py`; `contracts-base` | Header code pin before edits | 497 | 497 | 0 | 0 | 0 |
| Same contract/routing command; `contracts-after` | Uncommitted pack and routing tree | 497 | 497 | 0 | 0 | 0 |

Both forms retain repository `addopts` unchanged, use `--no-sources`, `-q -rA -p no:cacheprovider`, and a distinct external `--basetemp`; no additional pytest extra was needed for these runs.

Open items below require maintainer issue creation from the accompanying numbered drafts before final clearance; a draft marker is not an issued issue number.

| Item | Responsibility | Tracking and limit |
|---|---|---|
| Fallback handler does not enclose SciPy import | Maintainer | issue pending (1); S01; no dependency-failure check exists |
| Constructor units, finite values, counts, unknown labels and empty helper results | Maintainer | issue pending (2); S02–S14, S18, S25, S31; partial coverage C01/C02 only |
| Drawing scale/view conventions and tautological scale assertion | Maintainer | issue pending (3); S15–S24, S28–S30; do not use schematic coordinates as physical geometry |
| Identifying discovery content and missing neutral document locator | Maintainer | issue pending (4); second unverified claim above; parent template/fixture values excluded |
| Map coverage and rejected-candidate classification | Maintainer | issue pending (5); section-3 method families must be classified before being counted |
| Independent image/geometry references | Engineering calculations quality manager | issue pending (6); first unverified claim in section 4; 6 regression examples only; no independent validated example |
| Output resolution, figure dimensions, transparency checks and durable fingerprinted appendix | Maintainer | issue pending (6); S26–S27; byte-existence checks alone are insufficient |
| Pack index, shared-log entry and independent clearance | Maintainer and family quality manager | issue pending (6); initial lesson has no shared-log ID; status remains DRAFT |

The 2026-10-19 documentation audit confirms 31 classified sweep sites (30 package sites plus the parent dispatch boundary), 10 check definitions and 6 regression examples. The package grep returned 91 matching lines, classified by definition and symptom rather than counted as separate sites. Direct probes confirm saturation bands, overlapping well rows, 5 distinct anchor projections for 8 lines and PNG alpha 1.0 throughout a default-color output requested with transparency. These probes are temporary inspection evidence, not new repository detection tests; the section-6 missing-check dispositions remain applicable.

The map inventory was parsed at `3256bab9`: 0 in-package implementation rows. At that revision the four rejected schematics candidates are at L53340–L53367; the explicit revision pin is required. Section-4 and section-5 collection resolves all 16 cited node IDs. The pack-lint executable was not found in the available repository or shared tooling; no new `pack_lint: clean` result is claimed.

No client-derived values or private sidecar entry is included; do not copy identifying example values into subsequent evidence.
