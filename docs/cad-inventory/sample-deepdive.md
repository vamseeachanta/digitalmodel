# Deep-Dive: Representative Assemblies & CAM/NC Sets

**Epic:** [#1004](https://github.com/vamseeachanta/digitalmodel/issues/1004) · **Issue:** [#1008](https://github.com/vamseeachanta/digitalmodel/issues/1008) (4/5) · depends on #1005–#1007
**Method:** license-free reads only — directory structure, file naming, sizes, and **ASCII STEP parsing** (no SolidWorks seat).
**Privacy:** PUBLIC repo → operator/vendor/field names de-identified; internal drawing-number *patterns* shown to illustrate convention.

> All five samples come from the **subsea-riser equipment** domain — the densest curated CAD on the share
> (codes `CR`/`P1`/`F` in [`project-domain-map.md`](./project-domain-map.md)). They were chosen to expose the
> *repetition structure* that automation would attack.

---

## Data-quality note discovered during the dive

**~15% of the SolidWorks "files" are not models.** 8,839 of 60,036 `.sldprt`/`.sldasm` matches are `~$…`
**lock/temp files** (8,684 parts + 155 assemblies) left by an open SolidWorks session. A few `.exe` (eDrawings
self-extractors) also sit alongside. **Real SolidWorks model count ≈ 51,200.** The manifest keeps these rows
(they were on disk) but any modeling-effort estimate should exclude the `~$` prefix.

---

## Sample A — Upper Riser Assembly (URA) top-level GA · code `P1` (SLOR design)

- **File:** `…/303 URA/SW3D/<proj>-DGA-33XX - URA TOP LEVEL GA WITH SUPPORT FRAME AND CLAMPS.SLDASM` (~49 MB).
- **Structure (read from the sibling `URA TOP LEVEL …STEP`, AP214, no license):**
  - **485** assembly-usage occurrences (component placements), **1,133** product definitions, **719** solid bodies,
    19,238 faces, 562,912 points.
  - Authoring stamp inside the STEP: **`SwSTEP 2.0` / `SolidWorks 2007`**, exported **2009-03-03**.
- **Naming convention is fully systematic** — a drawing-number grammar:
  `‹proj›-DGA-NNNN` = general assembly, `‹proj›-DDL-NNXX` = detail part, `‹proj›-DFG-NNNN` = configuration/fab.
  Example detail parts in the GA: `PADEYE` (plain / slotted / hydrate-remediation variants), `SHEAVE BLOCK`,
  `VALVE MOUNT ADAPTOR`, `MAIN FRAME`, `PORCH HUB`, `ROV ACCESS PLATE`, `Y SPOOL`, `ELASTO PIPE COLLAR`,
  `FLEXIBLE JOINT STRUCTURAL DETAIL`, `SKID FRAME TOP PLATE`.
- **Parametric drivers (inferred):** padeye and frame parts recur as near-identical variants distinguished only by
  hole pattern / plate thickness / bracing — classic configuration parameters baked into separate files.
- **Drawing set:** co-located 2D `.dwg`/`.dxf` under the same `303 URA` tree (title-blocked equipment drawings).
- **CAM/toolpath:** none.

## Sample B — Buoyancy-tank compartment **family** · code `P1`

- **Files (one folder):** `302 - 1 BULKHEAD COMPARTMENT`, `…2 BULKHEAD…`, `302 - 14 COMPARTMENTS`,
  `17 COMPARTMENTS`, `18 COMPARTMENTS`, `302 - 20 COMPARTMENTS` — each a **separate `.SLDASM`** (44–51 MB each).
- **This is the automation poster child:** a single parametric buoyancy tank whose only real variable is the
  **compartment/bulkhead count** has been saved as **6+ independent hand-built assemblies**. Every downstream
  drawing, BOM and mass-property table was redone per variant.
- **Automatable as:** one configuration-driven (design-table) model → variants generated, not re-modeled.

## Sample C — Moveable Tensioner Cart · code `F` (private tensioner program)

- **Top assemblies:** `MoveableCart with 5 TC 100yr winter storm.SLDASM`,
  `MoveableCart3_Rev14 with 5 Tensionercarts.SLDASM`, `Wireline_Tensionercart_with semisub.SLDASM`.
- **Tree size:** 487 `.sldprt` + 321 `.sldasm` in the cart family (deep nested assembly).
- **Repetition signature:** the *same* cart re-saved per **load case** (`100yr winter storm`, `with semisub`) and
  per **revision** (`Rev14`) — revision + environmental-case proliferation rather than parameterization.
- **Neutral exports already present:** `…Down Stroke Assembly.STEP`, `taper stress joint.IGS`, `RiserStakUp.IGS`,
  `…SSM_shrinkwrap.stp` — i.e. someone already hand-exported STEP/IGES for exchange. A batch exporter would
  generalize this.

## Sample D — STEP as the license-free extraction path · (cross-cutting)

- A 127 MB AP214 STEP of the URA was parsed **with nothing but `grep`/`sed`** to recover assembly node counts,
  body counts and the authoring CAD/version (above). With `pythonocc`/OpenCASCADE the same files yield bounding
  boxes, mass/volume, BOM trees and tessellation — **no SolidWorks seat required.**
- Across the share there are **3,138 neutral 3D files** (STEP/IGES/Parasolid) already exportable this way.

## Sample E — CAM / NC set · (negative result, by design)

- **There is none.** `.mcam` = 0, `.cnc`/`.tap`/`.gcode` = 0, and all `.nc` are NetCDF. No toolpaths, no post-
  processed G-code, no Mastercam project anywhere on the share. **Phase-3 CAM automation has no existing data to
  seed from here** — it would start from CAD geometry, not from an NC backlog.

---

## Ranked automatable tasks (from what these samples expose)

ROI = (hours saved per occurrence) × (frequency on this share). Effort = build cost with the license-free stack
([#1006](https://github.com/vamseeachanta/digitalmodel/issues/1006)).

| # | Automatable task | Evidence in samples | ROI | Effort | License gate |
|--:|---|---|---|---|---|
| 1 | **Configuration/design-table variant generation** (collapse "N-compartment", "Rev/load-case" copies into one parametric model + generated variants) | B (1→6 tanks), C (cart × cases/revs) | **Very high** — repeats across CR/P1/F | Med (SW design tables) / High (headless re-author) | SolidWorks seat for native; or FreeCAD re-build |
| 2 | **Batch STEP/Parasolid export from the native libraries** (unlock the locked 51k SW + 184k Inventor for headless use) | C already has hand-exports; D proves value | **High** — one-time, unlocks everything downstream | Med | needs a **licensed seat once** (macro) |
| 3 | **Headless BOM / mass-property / bounding-box extraction from STEP** (auto-build part lists, weights, envelopes) | D (485 nodes, 719 solids parsed free) | **High** | Low–Med (`pythonocc`/OCC) | none (license-free) |
| 4 | **Drawing/title-block & revision harvesting from `.dwg`/`.dxf`** (index 234k legacy drawings: number, rev, title, sheet) | A drawing sets; PA hoard | **High** (huge corpus) | Low–Med (`ezdxf` + ODA) | none |
| 5 | **De-duplication & sort of the `PA` personal hoard** (416k files; content-hash, cluster, route) | PA (see #1007) | Med–High (storage + findability) | Low (hash/cluster) | none |
| 6 | **Automated GA/exploded-view & rapid-prototype STL prep** (the SLOR tree already had "STLS FOR RAPID PROTOTYPING") | A (STL export subtree) | Med | Med | seat or FreeCAD |
| 7 | **Naming-convention / drawing-number validation** (lint `‹proj›-DGA/DDL/DFG-NNNN` against a schema) | A grammar | Med | Low | none |

The full ROI ranking, phase mapping and recommended first build are in
[`automation-opportunity-report.md`](./automation-opportunity-report.md) (#1009).
