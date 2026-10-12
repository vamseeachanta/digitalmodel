# DXF body-plan reader

Phase A1 of [digitalmodel issue 2272](https://github.com/vamseeachanta/digitalmodel/issues/2272) converts selected DXF modelspace curves to `HullLineDefinition` and `HullProfile`. It accepts LINE, LWPOLYLINE, POLYLINE, ARC and SPLINE entities. Layer names are matched without case sensitivity; unsupported entities and other layers are counted as skipped.

## Installation and DWG conversion

Install `digitalmodel[drawings]` to enable DXF import/export. The optional extra requires `ezdxf>=1.3,<2`; imports are lazy, so importing the reader does not require ezdxf. An attempted read or export without it raises an actionable `ImportError`. Install `digitalmodel[curvature]` as well for the optional HullProd signature; see [curvature screening](curvature-screening.md).

DWG conversion is a separate, user-run step: use ODA File Converter to export a DWG to DXF, then inspect the resulting modelspace layers, units, origin and section geometry before selecting the input. The reader does not open DWG or invoke a converter.

## Configuration

The CLI takes a YAML mapping with exactly `reader` and `profile` keys. This synthetic example assumes five station curves in modelspace creation order:

```yaml
reader:
  body_plan_layers: [BODY_PLAN]
  station_x: [0.0, 25.0, 50.0, 75.0, 100.0]
  station_label_layer: STATIONS
  units: auto
  scale: 1.0
  centreline_y: 0.0
  baseline_z: 0.0
  mirror_side: starboard
  n_waterlines: 201
  tolerance: 0.000001
profile:
  name: synthetic_body_plan
  hull_type: custom
  length_bp: 100.0
  beam: 20.0
  draft: 8.0
  depth: 10.0
```

| Reader field | Meaning / default |
| --- | --- |
| `body_plan_layers` | Required nonempty list of unique layer names; every selected layer must contain a supported curve. |
| `half_breadth_layers`, `sheer_layers` | Optional lists; supplying either emits a warning because reconciliation is not implemented. |
| `station_x` | Optional, strictly increasing, nonnegative longitudinal positions in **full-scale metres**, one per joined curve in drawing order. |
| `station_label_layer` | Numeric TEXT/MTEXT station labels; default `STATIONS`, or `null` to disable. Ignored when `station_x` is supplied. |
| `units` | `auto` (default), `mm`, `m`, `ft` or `in`. |
| `scale` | Positive drawing-to-full-scale multiplier; default 1. |
| `centreline_y`, `baseline_z` | View origin in drawing coordinates; both default 0. |
| `mirror_side` | `port`, `starboard` or `both` (default). Each curve must remain on one side. |
| `n_waterlines` | Number of points on the common vertical grid; default 201, range 2–100001. |
| `tolerance` | Positive endpoint-joining and label-ambiguity tolerance in drawing units; default 1e-6. |

Unknown reader keys and non-finite numeric settings are rejected. At least two stations are required.

## Coordinates, station assignment and sampling

The body plan lies in the DXF XY plane. Its horizontal coordinate represents signed transverse distance and its vertical coordinate represents keel-up height. Given a drawing point `(X, Y)`, the output is:

```text
factor = metres_per_drawing_unit * scale
y_half_breadth = abs(X - centreline_y) * factor
z_keel_up = (Y - baseline_z) * factor
```

Port means negative `X - centreline_y`; starboard means positive. With `both`, individual stations may occupy either side, but a single station crossing the centreline is rejected. The output stores nonnegative half-breadths and uses longitudinal x measured from the aft perpendicular. Geometry below the configured baseline beyond tolerance is rejected.

Automatic units read the document header `$INSUNITS`: 1 = inches, 2 = feet, 4 = millimetres, 6 = metres. Unitless, missing or other codes require an explicit supported `units` override. Display-format settings do not supply a reliable geometry scale. See the official [ezdxf units documentation](https://ezdxf.readthedocs.io/en/stable/concepts/units.html).

Prefer `station_x` when positions are known. It takes precedence over labels and is never multiplied by `scale` or a unit conversion. Joined curves retain the order of their earliest source entity. Keep this order aligned with the supplied increasing station positions.

Without `station_x`, each numeric TEXT/MTEXT label is assigned to its uniquely nearest curve using insertion-point-to-curve distance. Label values are x positions **in drawing units**, converted by the same unit factor and scale as geometry. A label such as `5` means a coordinate of five drawing units; the reader does not infer station-number spacing or parse prefixes such as `Station 5`. Every curve needs exactly one numeric label and final x positions must be distinct and nonnegative. Label-derived stations are sorted by x in the resulting definition.

Ambiguous labels are rejected with handles and layers. Coincident identical stations, including repeated box sections and symmetric Wigley sections, cannot be distinguished by proximity. Supply `station_x` for these drawings, including round trips from the synthetic exporter.

Each curve must define a single-valued half-breadth as height increases. Reversed curves are oriented upward; exact consecutive duplicate points are removed. Horizontal segments with different breadths at one height, descending/folded sections, closed full sections and non-planar curves are rejected. Separate fragments join only when endpoints meet within `tolerance` and have a unique upward continuation. Ambiguous branches are rejected; overlapping vertical ranges remain separate curves.

ARC and SPLINE entities use their native ezdxf `flattening` methods. Polylines expand into virtual LINE/ARC segments, preserving bulges and using native ARC flattening rather than an intermediate cubic path approximation. The reported chord target is estimated maximum half-breadth times `1e-4`, converted to metres as `flattening_tolerance_m`. Final subdivision uses one hundredth of that target to control half-breadth interpolation error near horizontal tangents. Both are separate from fragment-joining tolerance. See [SPLINE flattening](https://ezdxf.readthedocs.io/en/stable/dxfentities/spline.html), [LWPOLYLINE bulges and virtual entities](https://ezdxf.readthedocs.io/en/stable/dxfentities/lwpolyline.html), and the alternative [ezdxf path conversion](https://ezdxf.readthedocs.io/en/stable/path.html) representation.

Offsets are linearly interpolated onto a common keel-to-top grid, restricted to each station's own vertical extent, with that station's endpoints retained. No vertical extrapolation is performed, so stations with different extents need not have the same offset count. Choose sufficient waterline resolution for downstream calculations.

The raw definition derives `length_bp` from maximum station x, `beam` from twice maximum sampled half-breadth, and **both draft and depth from the highest curve point**. This does not identify a design waterline. Supply authoritative full-scale dimensions in `profile` before computing hydrostatics. The conversion helper validates these overrides and reuses the existing `HullLineDefinition.to_hull_profile()`; it does not rescale geometry to fit dimensions. It rejects a station ending below the requested draft or beginning above the baseline with positive half-breadth. The reader can preserve partial station offsets, but this conversion guard prevents downstream hydrostatics from extrapolating missing submerged geometry. A station starting above baseline at zero half-breadth is permitted.

## Python API

```python
from digitalmodel.hydrodynamics.hull_library.line_generator.dxf_lines_reader import (
    DxfLinesConfig,
    hull_line_definition_to_profile,
    read_dxf_body_plan_with_report,
    write_body_plan_dxf,
)

config = DxfLinesConfig(
    body_plan_layers=["BODY_PLAN"],
    station_x=[0.0, 25.0, 50.0, 75.0, 100.0],
    mirror_side="starboard",
)
definition, report = read_dxf_body_plan_with_report("synthetic.dxf", config)
profile = hull_line_definition_to_profile(
    definition,
    name="synthetic_body_plan",
    hull_type="custom",
    length_bp=100.0,
    beam=20.0,
    draft=8.0,
    depth=10.0,
)
write_body_plan_dxf(profile, "synthetic_export.dxf", units="m")
```

`read_dxf_body_plan(path, config)` returns only the definition. `DxfLinesError.report` retains partial diagnostics on drawing failures. The exporter writes one starboard LWPOLYLINE per station and a TEXT label containing station x in the chosen drawing units. Its default layers are `BODY_PLAN` and `STATIONS`; export units are `m`, `mm`, `ft` or `in`.

## CLI and diagnostics

From the repository root with the package installed:

```powershell
.venv/Scripts/python.exe scripts/hull_library/dxf_to_profile.py synthetic.dxf synthetic_config.yaml --output-dir output/synthetic
```

Successful conversion writes:

- `profile.yaml`: validated HullProfile with explicit metadata and dimensions.
- `read_report.json`: entity counts, assignments and diagnostics.
- `sections.svg`: section overlay from `export_sections_svg`.
- `hydrostatics.json`: `HullHydrostatics.compute_all()` results.
- `hullprod_signature.json`: HullProd signature when the curvature dependency is available; otherwise the report records a warning.

Use a fresh or empty output directory for each conversion. The CLI refuses any nonempty output directory with exit status 1 and prints a JSON error report to stdout; existing files, including any previous `read_report.json`, are preserved. In an accepted empty output directory, handled conversion failures write `read_report.json`, print the report, and return 1. Success returns 0. Inspect `errors` before consuming outputs.

The report contains `entities_seen`, `entities_used`, `entities_skipped`; `seen_by_type`/`seen_by_layer`, `used_by_type`/`used_by_layer`, `skipped_by_type`/`skipped_by_layer`; `fragments_joined`; `stations_found`; `station_assignments` with x, handles, layers and assignment method; `flattening_tolerance_m`; resolved `units`; `errors`; and `warnings`. A fragment join counts one merge operation. Used entities include labels only when label assignment is active; explicit `station_x` makes labels skipped. Failure reports are partial, so seen need not equal used plus skipped when processing stops early.

## Boundaries

A1 does not reconcile half-breadth or sheer views, segment views automatically, recognize title blocks or scale bars, infer drawing scale, read raster images, perform OCR, or expand block INSERTs. Only explicitly selected modelspace geometry enters the body-plan conversion. Unsupported selected-layer entities are skipped, and a layer with no supported curves raises an error. DWG remains an explicit external conversion step.
