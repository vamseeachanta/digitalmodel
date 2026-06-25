# CAD/CAM Automation-Opportunity Report

**Epic:** [#1004](https://github.com/vamseeachanta/digitalmodel/issues/1004) · **Issue:** [#1009](https://github.com/vamseeachanta/digitalmodel/issues/1009) (5/5) · **synthesis of #1005–#1008**
**Audience:** the Deckhand custom-manufacturing GTM (Phase 1 office → Phase 2 CAD/CAM → Phase 3 floor/vision).
**Privacy:** PUBLIC repo — clients de-identified throughout.

---

## 1. The one-paragraph finding

The ace share holds **~525k CAD/CAM files (5.4 TB)** but the distribution is lopsided: **~86% is two un-curated
masses** (a 416k-file personal backup hoard + a 37k-file uncommitted riser library), the heaviest *clean* asset is
**OrcaFlex `.sim` (5.2 TB, already Python-scriptable)**, and **CAM/toolpath data is effectively zero**. So a
Phase-2 "CAD/CAM" offer built on *this* data is **~95% CAD, ~5% CAM** — and the fastest ROI is not native CAD
automation (Windows + paid seats) but **license-free geometry/metadata extraction** over the neutral formats and
drawings that already exist, plus collapsing the rampant **variant/revision duplication** the deep-dive exposed.

---

## 2. Ranked automation opportunities

ROI = (hours saved/occurrence) × (frequency on the share). Effort/blocker per the automation-surface gate
([#1006](https://github.com/vamseeachanta/digitalmodel/issues/1006)).

| Rank | Opportunity | Why it pays (evidence) | ROI | Effort | License gate | GTM phase |
|--:|---|---|:--:|:--:|---|:--:|
| **1** | **Headless STEP/IGES → BOM · mass · bounding-box · part-tree** extractor | #1008-D parsed a 485-node / 719-solid assembly free; 3,138 neutral files share the path | 🟢 High | 🟢 Low–Med | **none** (`pythonocc`/OCC) | **2** (entry) |
| **2** | **Drawing intelligence: title-block / number / revision harvest from `.dwg`+`.dxf`** | 237k AutoCAD files, mostly legacy; PA hoard is "Risers DWGs" | 🟢 High | 🟢 Low–Med | none (`ezdxf` + ODA conv.) | **1→2** |
| **3** | **Configuration / design-table variant generation** (collapse "N-compartment" & "per-load-case/rev" copies) | #1008-B (1 tank → 6 assemblies), #1008-C (cart × cases/revs) | 🟢 Very High | 🟡 Med–High | SW seat *or* FreeCAD re-author | **2** |
| **4** | **One-time batch STEP/Parasolid export** from the locked native libraries | unlocks 51k SW + 184k Inventor for the license-free stack above | 🟢 High (enabler) | 🟡 Med | **needs 1 licensed seat** (macro) | **2** (enabler) |
| **5** | **De-dup / cluster / route the 416k personal hoard (`PA`)** | content-hash + cluster; #1007 PA is massively duplicated | 🟡 Med–High | 🟢 Low | none | **1** |
| **6** | **OrcaFlex model/result automation** (batch run, parse, report) | 10k `.sim` / 5.2 TB; **already** in `digitalmodel.solvers.orcaflex` | 🟢 High | 🟢 Low (extend existing) | **OrcaFlex license** (have a "licensed machine") | **1→2** |
| **7** | **Naming-convention / drawing-number linting** (`-DGA/-DDL/-DFG-NNNN` schema) | #1008-A grammar | 🟡 Med | 🟢 Low | none | **2** |
| **8** | **Auto exploded-view / rapid-prototype STL prep** | #1008-A had an "STLS FOR RAPID PROTOTYPING" subtree | 🟡 Med | 🟡 Med | seat or FreeCAD | **2→3** |
| **9** | **CAM / toolpath automation** | **no data exists** — greenfield | ⚪ N/A here | 🔴 High | Mastercam seat + new data | **3** |

---

## 3. Mapping to the GTM phases

### Phase 1 — office / back-office (sell now, license-free)
- **#5 hoard de-dup/route** and **#2 drawing-register harvest** are pure document-ops: turn 237k drawings + a 416k
  hoard into a searchable, de-duplicated, revision-aware **drawing register**. This is a back-office data-hygiene
  product that *happens* to run on CAD files — a natural bridge from Phase 1 into Phase 2.
- **#6 OrcaFlex reporting** extends an asset digitalmodel already ships.

### Phase 2 — CAD/CAM (the expansion the strategy is gating on)
- **#1 headless STEP extraction** is the **lowest-risk Phase-2 beachhead**: real customer value (auto BOMs,
  weights, envelopes, clash inputs) with **zero license cost** and an in-repo home (`design_tools` + OCC).
- **#4 batch STEP export** is the **enabler** that converts the locked SolidWorks/Inventor estate into #1's feedstock.
- **#3 variant/design-table generation** is the **highest-ROI but higher-effort** prize — it directly attacks the
  duplication the deep-dive proved (one parametric model replacing 6+ hand-built variants).

### Phase 3 — shop floor / vision
- **CAM is greenfield here (#9).** There is no toolpath/NC backlog to mine, so a Phase-3 pitch cannot lean on
  existing assets on this share — it must be sold as new build on customer shop data, not ours.

---

## 4. Reusable automation-library candidates (cross-shop leverage)

These are the capabilities worth building **once** in `digitalmodel` and reusing across every shop/client:

| Library capability | Seeds from | Lives in (proposed) |
|---|---|---|
| **`neutral-cad` reader** — STEP/IGES/Parasolid → BOM, mass, bbox, tree (OCC-backed) | #1 | new `digitalmodel/cad/neutral/` (next to `visualization/design_tools`) |
| **`drawing-register`** — DWG/DXF title-block + revision parser (`ezdxf`/ODA) | #2 | `digitalmodel/cad/drawings/` |
| **`cad-dedup`** — content-hash + near-dup clustering for CAD trees | #5, #1007 caveat | `digitalmodel/cad/dedup/` |
| **`step-export` macro** — SolidWorks/Inventor → STEP batch (the one seat-gated piece) | #4 | shipped as a vendor-side macro, not headless |
| **`parametric-variants`** — design-table / FreeCAD variant driver | #3 | extend existing `design_tools` (FreeCAD/Blender) |
| **OrcaFlex batch+report** — **already exists** (`solvers/orcaflex` CLIs) | #6 | extend in place |

> The repo **already has the right scaffolding**: `solvers/orcaflex` (mature), `solvers/gmsh_meshing`, `meshio`
> dep, and a FreeCAD/Blender `design_tools` subsystem. The library play is to add a license-free `cad/` package
> beside them — not to start from scratch. *(Caveat: the existing `design_tools` "97.8% efficiency"/`Demo_User`
> claims are unvalidated pilot material — reuse the code, re-baseline the numbers.)*

---

## 5. Blockers & risks (from #1006)

- **No commercial CAD seat on this Linux host.** Every native API path (SolidWorks/Inventor/AutoCAD/Mastercam) is
  Windows + paid-seat. → Anchor the build on **license-free** paths (#1, #2, #3-via-FreeCAD) and isolate the one
  seat-gated step (#4) as a small vendor-side macro.
- **The native libraries are locked** until #4 runs — sequence #4 before #1 can cover the SW/Inventor estate.
- **Dedup is folder-level, not content-level** (#1007): `eng-partner` live (18) ≪ archive (502) — **do a content-hash
  reconciliation before deleting any `.preexisting` archive.**
- **~15% of SW "files" are `~$` lock/temp** (#1008) — exclude before any per-model effort/billing estimate.
- **OrcaFlex `.sim` runtime needs the license** — fine for in-house, but it can't be resold as headless.
- **CAM has no data** — don't scope Phase-3 CAM ROI off this share.

---

## 6. Recommended first build

**Build #1 (headless STEP → BOM/mass/bbox/tree) as a `digitalmodel/cad/neutral/` package, validated on the
the `URA` riser assembly and tensioner-cart STEP/IGES files already on the share.**

Why first: highest ROI-to-effort that is **fully license-free**, has **immediate customer-demoable output**
(auto BOM + weight + envelope from any STEP), seeds the **reusable library**, and needs **no seat and no new data**.
Pair it with a thin **#4 export macro** so the locked SolidWorks/Inventor estate can feed it — that two-step
(export-once → extract-free) is the concrete, low-risk **Phase-2 beachhead** for the manufacturing GTM.

Sequence: **#1 → #4 → #2 → #3**, with **#5/#6** runnable in parallel as Phase-1 back-office wins.
