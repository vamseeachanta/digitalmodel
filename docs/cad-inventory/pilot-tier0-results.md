# Tier-0 License-Free CAD Pilot — results

**Issue:** [#1015](https://github.com/vamseeachanta/digitalmodel/issues/1015) (epic [#1011](https://github.com/vamseeachanta/digitalmodel/issues/1011)) · validates the [OSS-package research](./oss-cad-packages-research.md) + [portability plan](./cad-portability-plan.md).
**Where:** ace-linux-1, against **real** subsea-riser share files (de-identified here). **Privacy:** PUBLIC repo — neutral
file names + generic part descriptions; no client geometry/derivatives committed.

> **Verdict: the Tier-0 license-free stack is VALIDATED on real data.** pythonocc-core (OCCT 7.9) + ezdxf + trimesh
> read STEP/IGES, compute BOM-relevant metrics, export STEP **AP242** / STL / glTF / 3MF, and pass a round-trip
> fidelity check — **no CAD license, headless, 6.7 s / 310 MB for the whole batch.**

---

## Environment (reproducible)

Installed from **conda-forge** (`mamba create -n cadpilot -c conda-forge python=3.11 pythonocc-core ezdxf trimesh
numpy` + `pip install networkx lxml scipy pillow` for trimesh's glTF/3MF export):

| Package | Version | Role |
|---|---|---|
| pythonocc-core (OCCT) | **7.9.0** | STEP/IGES read, metrics, STEP/STL write |
| ezdxf | 1.4.2 | DXF read |
| trimesh (+networkx/lxml) | 4.12.2 | glTF/3MF derivatives |
| numpy | 2.4.6 | — |

Pilot script: [`pilot-tier0.py`](./pilot-tier0.py) (committed; expects de-identified samples in `./in`).

---

## Results by capability

### 1. STEP read → metrics → 4-format export → round-trip ✅

| Sample (de-id) | Solids | Faces | Bbox (mm) | Volume mass (steel) | STEP products | Round-trip |
|---|--:|--:|---|--:|--:|---|
| single riser part (AP203 in, 199 KB) | 1 | 22 | 1000×2202×778 | **7,822 kg** | 3 | **lossless** (Δbbox 0, vol err 0) |
| riser sub-assembly (454 KB) | 2 | 279 | 906×3013×813 | 2,547 kg | 11 | lossless (vol err 1e-6) |

For each, the pipeline emitted **`.master.step` (AP214) + `.stl` + `.glb` (glTF) + `.3mf`** and a **JSON sidecar**
(metrics + exports). Mesh tessellation: single part 966 v / 1,928 f, **watertight**; assembly 10,234 v / 19,747 f.

**Round-trip oracle works** — re-reading the exported STEP reproduces solid count exactly, bbox Δ = 0.0 mm, volume
relative error ≤ 1e-6. This is the validation approach the portability plan specified, now proven on real geometry.

### 2. STEP AP242 write ✅ — *corrects the research caveat*

OCCT 7.9 wrote a valid **`AP242_MANAGED_MODEL_BASED_3D_ENGINEERING_MIM_LF` (ISO 10303-442)** file via
`Interface_Static.SetCVal("write.step.schema","AP242DIS")`. **So AP242 *geometry* export is fully supported** — my
earlier "AP242 write is partial / falls back to AP214" caveat was **too pessimistic**. The genuine limit is narrower:
**PMI/GD&T annotation** export (semantic tolerances) is the partial part — and these AP203 source files carry no PMI,
so it doesn't bite here. **Recommendation: make AP242 the archival master** (downgrade to AP214 only if a consumer
can't read 242).

### 3. DXF read (ezdxf) ✅

Real plate-nesting DXF → **R2000 (AC1015), 7 layers, 3 blocks, 93 model-space entities** (63 LINE, 12 ELLIPSE,
10 CIRCLE, 8 ARC). Full entity/layer/block access with a pure-Python MIT library — the drawing-register path is clear.

### 4. IGES read ⚠️ — surface-only (confirms the lossy-for-solids caveat)

The IGES riser joint read as **0 solids / 8 faces**; volume/mass came back **negative/meaningless** because IGES
carries **un-sewn surfaces, not a solid**. IGES is fine for *shape exchange* but needs an OCC **sewing/ShapeFix**
step to recover a solid before volume/mass is valid. Treat IGES as read-only surface data; prefer STEP.

### 5. Parasolid `.x_t` read ❌ — gap confirmed

Reading the `.x_t` **failed as expected** — OCCT has no open-source Parasolid reader. Confirms the research finding:
the ~815 `.x_t` files need the **commercial** OCCT Parasolid component or a re-export. No OSS path.

### 6. Native SolidWorks / Inventor — not attempted (no OSS reader exists)

Per the research, there is no OSS native reader; untestable here without a vendor seat or the commercial CrossCad/Ware
SDK. **The one-time seat export remains the only unlock.**

---

## New nuance for the package design (#1016)

`STEPControl_Reader` **flattens** assemblies: the sub-assembly STEP has **11 `PRODUCT_DEFINITION`s** but the basic
reader surfaced only **2 merged solids**. To preserve the **assembly/BOM tree, part names, and colors** (needed for
real BOM extraction), the `digitalmodel/cad/` package must use the **XCAF reader** (`STEPCAFControl_Reader` +
`TDocStd_Document`), not the basic `STEPControl_Reader`. Flagged for #1016.

---

## Performance

Whole batch (3 geometry reads + metrics + 4 exports each + round-trip + DXF + Parasolid + AP242 test):
**6.7 s wall, 310 MB peak RSS.** Comfortably scriptable over thousands of files.

---

## Recommendation → #1016

The Tier-0 stack is proven. Scaffold `digitalmodel/cad/`:
- **`neutral/`** — OCCT **XCAF** reader (STEP/IGES) → solids, **assembly/BOM tree**, mass/bbox/COM; sewing pass for IGES.
- **`convert/`** — **STEP AP242** master writer + STL/glTF (trimesh)/3MF emitters.
- **`drawings/`** — ezdxf DXF reader (+ ODA for DWG→DXF).
- **`validate/`** — the round-trip oracle (solid-count / bbox / volume tolerances) demonstrated here.
- **License note:** pythonocc-core is **LGPL** (import-safe for MIT digitalmodel); ship via conda or the pip `OCP`
  wheel. **Still seat-gated:** native SolidWorks/Inventor/Parasolid → one-time STEP export.
