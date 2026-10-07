# Plan: digitalmodel #2272 phase A1 — DXF lines-plan reader to HullProfile

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/2272 (phase A1; other phases stay open)
**Roadmap:** workspace-hub `docs/roadmaps/drawing-to-digital-twin-roadmap.md`
**Date:** 2026-09-29 · **Status:** plan drafted by the orchestrator; dispatched to the Codex lane on the owner's standing instruction ("delegate to codex as much as possible; continue with recommended order"). `status:plan-approved` is the owner's label.
**Tier:** T2 (bounded first slice) · **Lane:** lane:codex · **Client:** N/A · **Base:** origin/main

## Goal

Read a body-plan (and optionally half-breadth and sheer) view from a DXF lines plan into the
existing `line_generator` models and a `HullProfile`, so a legacy drawing can enter the same
chain as an offset table or a parametric form: mesh, STEP/BRep, HullProd signature,
hydrostatics, diffraction. DWG enters through an explicit, user-run DWG-to-DXF conversion
(ODA File Converter); the lane does not touch DWG.

## Scope

1. **Dependency.** New optional extra `drawings = ["ezdxf>=1.3,<2"]` in `pyproject.toml`; add
   `ezdxf` to the `test` extra as well (pure Python, small). All new code imports `ezdxf`
   lazily and raises an actionable `ImportError` naming the extra.
2. **`src/digitalmodel/hydrodynamics/hull_library/line_generator/dxf_lines_reader.py`** (new,
   under 400 lines; split a `dxf_entities.py` helper if needed):
   - `DxfLinesConfig` (pydantic): `body_plan_layers: list[str]`, optional
     `half_breadth_layers`, `sheer_layers`, `station_x: list[float] | None` (x positions in
     drawing order when the drawing carries no station labels), `station_label_layer`
     (TEXT/MTEXT entities whose insertion point is nearest a curve give it its station number
     or x), `units: "auto" | "mm" | "m" | "ft" | "in"` (auto reads `$INSUNITS`), `scale: float`
     (drawing-to-full-scale multiplier, default 1.0), `centreline_y: float` and `baseline_z:
     float` (view origin in drawing coordinates), `mirror_side: "port" | "starboard" | "both"`,
     `n_waterlines: int` for resampling, `tolerance` for joining curve fragments.
   - `read_dxf_body_plan(path, config) -> HullLineDefinition`: collect LINE, LWPOLYLINE,
     POLYLINE, ARC, SPLINE (via `ezdxf` flattening with a chord tolerance) on the body-plan
     layers; join fragments whose ends coincide within `tolerance`; assign each joined curve to
     a station (label or `station_x` order); convert to (z keel-up, y half-breadth) in metres;
     resample onto the waterline grid; build `StationOffset` / `HullLineDefinition` from
     `line_parser.py`. Curves that cannot be assigned, non-monotone stations, and empty layers
     raise `DxfLinesError` with the entity handles and layer names in the message; a
     `DxfReadReport` (entities seen, used, skipped by type/layer, fragments joined, stations
     found) is returned alongside via `read_dxf_body_plan_with_report`.
   - `hull_line_definition_to_profile(defn, *, name, hull_type, length_bp, beam, draft, depth)
     -> HullProfile` (if an equivalent already exists in `line_generator`, reuse it and say so).
   - `write_body_plan_dxf(profile, path, *, layer="BODY_PLAN", label_layer="STATIONS",
     units="m")`: synthesise a body-plan DXF from a `HullProfile` (one LWPOLYLINE per station,
     TEXT label with the station x); this is both an export and the round-trip test fixture.
3. **CLI** `scripts/hull_library/dxf_to_profile.py`: DXF plus a YAML config to `HullProfile`
   YAML, a read report JSON, the sections SVG (`exporter.export_sections_svg`), hydrostatics
   summary (`HullHydrostatics.compute_all`), and, when the curvature extra is installed, the
   HullProd signature (`screen_profile`). Exit 1 with the report when any station fails.
4. **Tests** `tests/hydrodynamics/hull_library/line_generator/test_dxf_lines_reader.py` (skip
   cleanly without `ezdxf`; hullprod-dependent parts skip without it):
   - round trip, closed-form comparators: Wigley profile from
     `parametric_form.generate_profile(MonohullFormParameters(..., wigley=True))` to DXF to
     reader: every offset within 0.5 % of B/2 and the reader's `HullProfile` hydrostatic
     volume within 0.5 % of the source; same for a drillship-like transom form
     (`cb=0.72, transom_fraction=0.9, lcb_fraction=-0.10`) and the `box_profile` fixture;
   - entity coverage: a hand-built DXF with a SPLINE station, a polyline station, a station
     made of ARC + LINE fragments, a distractor layer, and mm units with `$INSUNITS=4`, reads
     to the same offsets as its polyline-only twin within the flattening tolerance;
   - failure modes: missing station label and no `station_x` raises `DxfLinesError` naming
     the curve; unknown units raise; the report counts match the fixture;
   - CLI smoke: runs on the Wigley DXF in a temp dir and writes all outputs.
5. **Docs:** `docs/domains/hull_library/dxf-lines-reader.md` (config, coordinate conventions,
   the DWG-to-DXF step, what is not read yet: half-breadth/sheer reconciliation, title blocks,
   scale bars, raster) and this plan verbatim as
   `docs/plans/2026-09-29-issue-2272-a1-dxf-lines-reader.md`.

## Out of scope

DWG reading; view segmentation and title-block/scale recognition (A2); OCR of offset tables
(A3); raster drawings (A4); structure (B1); any client drawing. The six DWG lines plans in
`docs/domains/freecad/src/hulls` are the first real cases after the owner converts them to DXF;
do not attempt them in the lane.

## Acceptance

- Round-trip tests pass at the stated tolerances; entity-coverage and failure-mode tests pass.
- Full `tests/hydrodynamics/hull_library` + `diffraction/test_quality_gates.py` green;
  existing counts unchanged.
- New modules under 400 lines, functions under 50 lines, black/ruff clean; no client, host or
  private-path identifiers.
- PR body: the round-trip table (form, max offset error as % of B/2, volume error), the
  read-report of the mixed-entity fixture, and any deviation with its reason.

## Risks

- SPLINE flattening tolerance versus offset tolerance: choose chord tolerance from B/2 times
  1e-4 and state it. Body plans usually draw port on one side and starboard on the other for
  fore and aft stations: `mirror_side` handles the convention; the reader must not assume
  which side is which without the config.
- Station assignment by label proximity can pick the wrong label on crowded plans; prefer
  `station_x` when supplied and report which method was used per station.
