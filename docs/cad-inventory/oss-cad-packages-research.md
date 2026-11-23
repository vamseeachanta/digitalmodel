# Open-Source Packages to Unleash the CAD Inventory — research findings

**Context:** supports adapter epic [#1011](https://github.com/vamseeachanta/digitalmodel/issues/1011) (esp. [#1015](https://github.com/vamseeachanta/digitalmodel/issues/1015)) and the
[CAD portability plan](./cad-portability-plan.md). **Decisive constraint: digitalmodel is MIT-licensed** — so package
*license* governs whether we can `import` it (permissive/LGPL) or must keep it at arm's length as an external CLI (GPL).

> **Method & honesty note.** Produced via the deep-research harness: 5 search angles → **23 sources fetched → 111
> claims → 25 adversarially verified (3-vote), 25 confirmed, 0 killed**. The harness's final *synthesis* agent
> returned a malformed stub, so this report is **hand-synthesized from the 25 verified claims + the verified source
> list** (cited inline). Claims marked ✅ were workflow-verified; items marked ⚠️ are domain knowledge flagged for
> hands-on confirmation in #1015.

---

## 1. License-compatibility tiers (the gate for an MIT library)

| Tier | Packages | Use in MIT digitalmodel |
|---|---|---|
| 🟢 **Permissive** (MIT/Apache/BSD) | **CadQuery** (Apache ✅), **build123d** (Apache ✅), **ezdxf** (MIT ✅), **trimesh** (MIT), **meshio** (MIT, *already a dep*), **Assimp** (BSD) | Import freely; ship as deps |
| 🟡 **Weak copyleft (LGPL)** | **pythonocc-core** (LGPL-3.0 ✅), **OCCT/OpenCASCADE** kernel (LGPL-2.1+exception), **OCP** (LGPL), **FreeCAD** (LGPL) | Safe to import without relicensing your MIT code; keep the lib replaceable |
| 🔴 **Strong copyleft (GPL)** | **LibreDWG** (GPL-3.0 ✅), **gmsh** (GPL, *already used as a tool*), **OpenSCAD** (GPL) | **Do NOT link/import** into MIT code — invoke only as a separate-process CLI (arms-length) |
| 🟡 **Free but proprietary EULA** | **ODA File Converter** (closed; external dep ✅) | External install only — never bundle/redistribute |
| 💰 **Commercial SDK** | **Datakit CrossCad/Ware** (✅ reads native SW + Inventor) | Paid; the only non-seat way to read native SW/Inventor |

---

## 2. Package matrix

| Package | License | Lang / bindings | Reads | Writes | Headless Linux | Notes |
|---|---|---|---|---|---|---|
| **OCCT (OpenCASCADE)** | LGPL-2.1+exc | C++ | STEP (AP203/214/242 read), IGES, BREP, STL | STEP (AP203/214; AP242 ⚠️ partial), IGES, STL, glTF | yes | The kernel under everything below |
| **pythonocc-core** | LGPL-3.0 ✅ | Python (SWIG) ✅, "nearly all of OCCT" ✅ | STEP/IGES/BREP/STL (data-exchange read/write ✅) | STEP/IGES/STL/glTF | yes | Most complete low-level OCCT API → best **convert engine** |
| **CadQuery** | Apache-2.0 ✅ | Python (on OCP) | STEP, BREP, DXF | **lossless STEP + DXF** ✅ | **yes** ✅ | Higher-level scripting; kernel = OCCT ✅ |
| **build123d** | Apache-2.0 ✅ | Python (on OCP) | STEP, BREP, others | STEP, STL, others | yes | Parametric B-rep ✅; pythonic API |
| **FreeCAD** | LGPL-2+ | C++ + Python (`freecadcmd`) | STEP, IGES, DXF, STL, OBJ; **Inventor via InventorLoader addon** | STEP, IGES, DXF, STL, glTF, OBJ | yes (`freecadcmd`) | Whole-app; good batch converter |
| **ezdxf** | MIT ✅ | pure Python (+opt Cython) ✅ | **DXF only** ✅ | **DXF only** ✅ | yes ✅ | **Cannot read/write DWG; not a CAD kernel** ✅ |
| **ODA File Converter** | free, proprietary ✅ | GUI/CLI (external) ✅ | **DWG↔DXF (all versions)** | DWG↔DXF | yes (xvfb) | Wrapped by ezdxf's `odafc` add-on ✅; EULA = no bundling |
| **LibreDWG** | **GPL-3.0** ✅ | C (+SWIG py) | **DWG (reader ~complete)** ✅ | DWG (**older versions only** ✅, 2-1) | yes | GPL → arms-length CLI only for MIT code |
| **libdxfrw** | GPLv2 | C++ | DXF, some DWG | DXF | yes | Used by LibreCAD; GPL caveat |
| **trimesh** | MIT | Python | STL/OBJ/PLY/**glTF/3MF** | STL/**glTF/3MF**/OBJ | yes | Best **derivative** generator (glb/3MF) |
| **meshio** | MIT | Python | many mesh fmts | many mesh fmts | yes | *Already a digitalmodel dep* |
| **gmsh** | GPL | C++ + Python API | STEP/IGES (via OCC)/mesh | mesh (MSH/VTK/…) | yes | *Already used*; GPL → keep external |
| **Assimp** | BSD-3 | C++ (+py) | many scene/mesh | **glTF**, OBJ, … | yes | Permissive glTF path (cf. Smithsonian meshsmith) |
| **OpenSCAD** | GPLv2 | CLI | STL/3MF/DXF/SVG | STL/3MF/DXF | yes | CSG; GPL → external only |
| **InventorLoader** (jmplonka) | OSS (FreeCAD addon) | Python | **Inventor .ipt/.iam (partial ⚠️)** | — | yes | Best-effort OSS Inventor read; incomplete |
| **Datakit CrossCad/Ware** | **commercial** ✅ | SDK | **native SolidWorks ✅ + Inventor ✅**, Parasolid, JT, NX, Creo | neutral fmts | yes | Paid; the only non-seat native-SW/Inventor reader |

---

## 3. The hard gaps (what no OSS package solves)

- ✅ **Native SolidWorks (`.sldprt`/`.sldasm`) and Inventor (`.ipt`/`.iam`): NO reliable OSS reader.** OCCT data-exchange
  does not read them; the only non-vendor-app options are the **commercial** Datakit CrossCad/Ware SDK (✅ verified to
  read both) or **InventorLoader** (OSS but partial ⚠️, Inventor-only). → **A one-time vendor-seat STEP export is
  unavoidable** unless a commercial SDK is bought.
- ⚠️ **Parasolid (`.x_t`/`.x_b`): no OSS reader either.** OCCT reads Parasolid only via a **commercial** add-on
  (occt3d.com Parasolid Import Component). So even the ~815 `.x_t` files on the share need the seat or a commercial
  component to read — flag for #1015.
- ✅ **STEP AP242 *write* — RESOLVED by the pilot ([pilot-tier0-results.md](./pilot-tier0-results.md)).** OCCT **7.9**
  writes a valid **`AP242_MANAGED_MODEL_BASED_3D_ENGINEERING_MIM_LF`** (ISO 10303-442) file via
  `write.step.schema=AP242DIS`. So AP242 **geometry** export is fully supported — the earlier "falls back to AP214"
  worry was too pessimistic. The genuine limit is narrower: **PMI/GD&T annotation** export is partial. → **AP242 is a
  safe archival master.**
- ✅ **DWG needs an external converter.** ezdxf does DXF only ✅; DWG requires **ODA File Converter** (free, closed) or
  **LibreDWG** (GPL → arms-length). For an MIT library, ODA-as-external-tool is the cleaner path.

---

## 4. Recommended OSS stack

### Tier 0 — license-free, importable into MIT digitalmodel (build now)
- **Geometry read + convert engine:** **pythonocc-core** (LGPL) for low-level STEP/IGES data-exchange + tessellation;
  **CadQuery**/**build123d** (Apache) for higher-level scripting. → reads the ~3,138 existing STEP/IGES files, emits
  STEP + glTF + metadata.
- **2D drawings:** **ezdxf** (MIT) for DXF read/write + title-block/entity extraction.
- **DWG → DXF:** **ODA File Converter** (free, external) via ezdxf's `odafc` add-on. *(LibreDWG only if ODA's EULA is
  unacceptable — and then as a subprocess, never imported.)*
- **Mesh / glTF / 3MF derivatives:** **trimesh** + **meshio** (MIT) (+ **Assimp**/BSD for richer glTF).
- **FEA meshing (optional):** **gmsh** — keep as the existing external tool (GPL).

### Seat-gated / commercial (the single unlock — not OSS-solvable)
- **Native SolidWorks/Inventor/Parasolid → STEP+Parasolid+PDF/DXF:** a **SolidWorks/Inventor seat batch macro** (run
  once), **or** buy **Datakit CrossCad/Ware**. This is the only way to liberate the 60k SW + 184k Inventor + 815
  Parasolid files. After this export, the entire estate is Tier-0-portable forever.

---

## 5. Verified sources (deep-research, 23 fetched; key set)

- pythonocc-core — github.com/tpaviot/pythonocc-core (primary); dev.opencascade.org/project/pythonocc
- CadQuery — pypi.org/project/cadquery (primary); build123d — dev.opencascade.org/project/build123d (primary)
- ezdxf — ezdxf.readthedocs.io/introduction + /addons/odafc (primary)
- LibreDWG — gnu.org/software/libredwg + github.com/LibreDWG/libredwg (primary); en.wikipedia.org/wiki/LibreDWG
- ODA File Converter — opendesign.com/guestfiles/oda_file_converter
- Native readers — github.com/jmplonka/InventorLoader; occt3d.com Parasolid Import Component; datakit.com SolidWorks-to-OpenCASCADE; cadexchanger.com import blog
- Mesh/derivatives — github.com/mikedh/trimesh; pypi.org/project/meshio; gmsh.info/doc; github.com/Smithsonian/dpo-meshsmith
- Licensing — dev.opencascade.org/resources/licensing; en.wikipedia.org/wiki/FreeCAD

> Next: hands-on validation of this stack on real share files (#1015) — especially **AP242 write fidelity** and
> **Parasolid read** — then scaffold the `digitalmodel/cad/` package (#1016).
