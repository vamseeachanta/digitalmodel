# CAD Software-Ecosystem & Automation-Surface Profile

**Epic:** [#1004](https://github.com/vamseeachanta/digitalmodel/issues/1004) · **Issue:** [#1006](https://github.com/vamseeachanta/digitalmodel/issues/1006) (2/5) · depends on the inventory ([#1005](https://github.com/vamseeachanta/digitalmodel/issues/1005))
**Host:** ace-linux-1 (Linux) · **Source:** `cad-file-manifest.csv.gz` + cheap header probes

This profiles **which CAD/CAM ecosystems are actually in use**, their **version era**, their **automation surface**,
and **whether a license / runtime exists locally** — the gate for the Phase-2 CAD/CAM offer.

---

## 1. Ecosystems present (from the inventory)

| Ecosystem | Native formats | Files | Neutral-export share | Role on the share |
|---|---|--:|---|---|
| **AutoCAD** | `.dwg` (+`.dxf` exchange) | 237,325 | `.dxf` 2,624 | Legacy 2D drawing archive (equipment, schematics, P&ID-style) |
| **Autodesk Inventor** | `.ipt`, `.iam` | 184,470 | — | Almost entirely the `ri/00_inbox` flush-drive dump |
| **SolidWorks** | `.sldprt`, `.sldasm` | 60,036 | — | Dominant **curated** 3D ecosystem (subsea equipment) |
| **OrcaFlex** | `.sim`, `.dat` | 10,204 `.sim` / 29,644 `.dat` | — | Largest data mass (~5.2 TB); marine/riser/mooring analysis |
| **CATIA** (exporter) | `.catpart` 0 native; seen as STEP origin | 0 | exports to STEP | Appears only as the *origin system* of some STEP files |
| **NX / generic** | `.prt` | 256 | — | Small; ambiguous (`.prt` is NX/Pro-E/other) |
| **Neutral 3D** | STEP `.step`/`.stp`, IGES, Parasolid `.x_t` | 1,759 / 564 / 815 | — | License-free exchange geometry (~3,138 files) |
| **Mesh / 3D-print** | `.stl` | 69 | — | Minor |
| **Fusion 360** | `.f3d`, `.f3z` | **0** | — | Not present |
| **Mastercam / CAM / CNC** | `.mcam`, `.cnc`, `.tap`, `.gcode`, `.nc`(G-code) | **0** | — | **Absent.** All 10 `.nc` are NetCDF, not toolpath |

**Headline:** the share is **AutoCAD + Inventor + SolidWorks for CAD, OrcaFlex for analysis, and *no CAM at all*.**

---

## 2. Version era (cheap header detection)

### AutoCAD `.dwg` — spans ~25 years, skewed *old*
DWG stores a 6-byte version code in its first bytes. Sample of 3,000 random `.dwg`:

| Code | AutoCAD release | ~Year | Share of sample |
|---|---|---|--:|
| `AC1014` | R14 | 1997 | **32%** (most common) |
| `AC1021` | 2007 | 2007 | 26% |
| `AC1024` | 2010 | 2010 | 18% |
| `AC1012` | R13 | 1994 | 7% |
| `AC1027` | 2013 | 2013 | 6% |
| `AC1015` | 2000 | 2000 | 4% |
| `AC1018` | 2004 | 2004 | 3% |
| `AC1009` | R11/12 | ~1990 | 2% |
| `AC1032` | 2018 | 2018 | <1% |

> The DWG corpus is a **legacy archive** (median era ~R14–2007), not active drafting. Old DWGs are still fully
> readable by modern open-source tooling and are a strong fit for batch DXF/text extraction.

### STEP — mixed origin systems
Sampled STEP headers name multiple originating systems: **CATIA V5 (AP214)**, **Autodesk Inventor 10 & 2013**,
and **FreeCAD**. So STEP here is genuine cross-vendor exchange geometry — exactly the format an automation layer
can consume without any of the originating licenses.

### SolidWorks / Inventor native versions
`.sldprt`/`.ipt` are OLE compound binaries; the save-version lives in metadata streams and is **not** cheaply
greppable (no plaintext "SolidWorks 20xx" in the sampled files). Detecting it precisely needs an OLE parser
(`olefile`) or a CAD seat — deferred as low-value (the *ecosystem* is confirmed; exact release isn't load-bearing).

---

## 3. Automation surface per ecosystem

Legend — **License-gated (Windows GUI):** scripting requires a paid seat + the vendor app running on Windows.
**Headless/​license-free:** can be scripted on this Linux host with open-source tooling.

| Ecosystem | Native automation API | Headless / license-free path | Locally available *today*? |
|---|---|---|---|
| **OrcaFlex** | **`OrcFxAPI` (Python)** — first-class, documented | n/a (needs OrcaFlex license) but pure-Python | **Partial** — `OrcFxAPI` is an *optional* dep in digitalmodel (`solvers` extra) + a `solver` pytest marker for "licensed machine". Runtime needs an OrcaFlex license seat. |
| **AutoCAD** | AutoLISP / ActiveX-COM / .NET (Windows + seat) | **`ezdxf`** (read/write DXF & modern DWG-via-ODA), **ODA File Converter** for `.dwg`→`.dxf` | **No** — `ezdxf` not installed; no ODA converter; no AutoCAD. Installable, license-free. |
| **SolidWorks** | SolidWorks API (VBA/C# macros, COM) — Windows + seat | Read-only via STEP/Parasolid export → OCC/FreeCAD | **No** — Windows-only app, no seat on this host. |
| **Autodesk Inventor** | iLogic / Inventor API (COM) — Windows + seat | Read-only via STEP export → OCC/FreeCAD | **No** — Windows-only. |
| **CATIA** | CAA / VBA — Windows + seat | STEP already exported | **No** app; STEP exports usable. |
| **Neutral (STEP/IGES/Parasolid)** | n/a | **`pythonocc-core`/OCP (OpenCASCADE)**, **FreeCAD** Python, **gmsh** | **Partial** — `gmsh` installed; OCC/FreeCAD/Blender wrappers exist in-repo (below) but the libs aren't in base Python. |
| **Mesh (STL/3MF)** | n/a | **`meshio`** (already a digitalmodel dep), `trimesh`, gmsh | **Yes** (meshio dep; gmsh installed). |
| **Mastercam / CNC** | Mastercam .NET API — Windows + seat | — | **N/A — no CAM data exists to automate.** |

### What digitalmodel already ships (our in-house surface)
- **OrcaFlex automation is real and mature:** `digitalmodel.solvers.orcaflex` with CLIs `run-to-sim`,
  `orcaflex-universal`/`orcaflex-sim`, `orcaflex-convert` (format converter), plus domain modules
  `drilling_riser`, `mooring_fatigue`, `riser_fatigue`, `orcawave`. `OrcFxAPI` + `gmsh` are the declared `solvers` extra.
- **Meshing/geometry:** `gmsh-meshing` CLI, `meshio` dependency.
- **An existing open-source AI-CAD subsystem:** `src/digitalmodel/visualization/design_tools/` — `freecad_integration.py`,
  `freecad_hull.py`, `blender_integration.py`, `ai_cad_agent.py`, `manifold_check.py`, `hull_hydrostatics.py`,
  plus a `CAD_MIGRATION_PLAN.md` proposing migration off commercial CAD to FreeCAD + Blender.
  ⚠️ *Its headline claims ("97.8% efficiency gain", `Demo_User` pilot metrics) read as unvalidated pilot/marketing
  material — treat the **code** as a real seed, the **numbers** as unverified.*

---

## 4. Read-only-without-license vs scriptable-headless (the Phase-2 gate)

| Format class | Files | Without a license you can… | Phase-2 feasibility |
|---|--:|---|---|
| **OrcaFlex `.sim`/`.dat`** | ~40k | Run/parse **only with an OrcaFlex seat**, but entirely from Python | **HIGH** — best ROI: large corpus, Python API, in-house tooling already exists. License is the only gate (already on a "licensed machine" per repo). |
| **Neutral STEP/IGES/Parasolid** | 3,138 | Fully read/convert/measure **headless, license-free** (OCC/FreeCAD/gmsh) | **HIGH** — the clean license-free entry point for 3D geometry automation. |
| **AutoCAD `.dxf`** | 2,624 | Fully read/write **headless** (`ezdxf`) | **MEDIUM-HIGH** — easy; but small vs `.dwg`. |
| **AutoCAD `.dwg`** | 234,701 | Read via ODA converter → DXF, then `ezdxf` (no AutoCAD needed) | **MEDIUM** — large legacy archive; batch text/block/title-block extraction is feasible license-free. |
| **SolidWorks `.sldprt`/`.sldasm`** | 60,036 | **Nothing meaningful** without a SolidWorks seat (binary OLE). Need owner to **batch-export STEP** first. | **LOW until exported** — native API is Windows+seat; the realistic path is a one-time STEP export then headless OCC. |
| **Inventor `.ipt`/`.iam`** | 184,470 | Same as SolidWorks — need STEP export | **LOW until exported** (and most are the un-curated inbox dump). |
| **Mesh `.stl`** | 69 | Fully headless (`meshio`/`trimesh`) | trivial but tiny corpus. |
| **CAM / CNC** | 0 | — | **N/A — no data.** Phase-3 CAM automation starts from zero here. |

---

## 5. Conclusions feeding #1009

1. **No commercial CAD is installed on this host** (Linux; only `gmsh`). Every native-CAD automation path
   (SolidWorks/Inventor/AutoCAD/Mastercam APIs) requires Windows + a paid seat we don't have here.
2. **The license-free, headless-scriptable surface is: OrcaFlex (Python, license-gated runtime) + neutral
   formats (STEP/IGES/Parasolid via OCC/FreeCAD/gmsh) + DXF (ezdxf) + the existing FreeCAD/Blender design_tools.**
3. **The native SolidWorks/Inventor corpus is locked** until someone runs a one-time **STEP/Parasolid batch export**
   from a licensed seat — that export step is itself a high-leverage automation to build first.
4. **CAM is a greenfield, not a backlog** — there is no toolpath data to mine, so a Phase-3 CAM pitch can't lean
   on existing assets here.

See [`automation-opportunity-report.md`](./automation-opportunity-report.md) (#1009) for ROI ranking and phase mapping.
