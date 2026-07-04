# Latest OSS + CAD-AI We Can Leverage — ultradeep research

**Context:** extends the [OSS-package research](./oss-cad-packages-research.md) and [pilot](./pilot-tier0-results.md) for adapter epic
[#1011](https://github.com/vamseeachanta/digitalmodel/issues/1011). Scope: the **newest (2024–2026)** open-source tooling
(GitHub) and CAD-AI models/datasets (HuggingFace/GitHub) we can leverage. **digitalmodel is MIT** → commercial-use
license is the gating filter throughout.

> **Method & honesty.** Two deep-research workflows (GitHub-OSS + HuggingFace-AI), 5 angles each. Both were heavily
> **API-rate-limited during the adversarial *verify* phase**, so most claims came back *unconfirmed (abstained), not
> refuted*. Because commercial-use licensing is the decisive factor, I **re-verified every license below myself,
> firsthand, against the GitHub License API / PyPI** — those are solid. Model *capabilities/recency* come from the
> workflow + repo descriptions and should be piloted before production reliance.

---

## Part A — OSS tooling (GitHub) worth adding

| Tool | License (verified) | What it uniquely adds | Slot in our pipeline |
|---|---|---|---|
| **cascadio** (`mikedh/cascadio`) | **MIT** ✅ | pip-installable **STEP → GLB** via OCCT, **no OCCT compile** (prebuilt wheels), feeds trimesh directly. v0.0.17. | **New, adopt** — lightweight viz/derivative path; simpler than full pythonocc for STEP→glTF |
| **manifold / manifold3d** | **Apache-2.0** ✅ | **Guaranteed watertight/manifold** mesh Boolean; now the Boolean kernel in **Blender & OpenSCAD**; pushes **3MF + glTF `EXT_mesh_manifold`** over lossy STL. v3.5.1 (Jun 2026). | **New, adopt** — robust Boolean + validates the "avoid STL, prefer 3MF/glTF" choice |
| **build123d** (`gumyr/build123d`) | **Apache-2.0** ✅ | Pure-Python (99.8%) **parametric BREP** authoring on OCCT (via OCP); Pythonic operator interface. | **Adopt** — the **variant-generation engine** (parametric model → N variants) |
| **CadQuery + OCP** | Apache-2.0 (OCP = **LGPL**) | The shared OCCT Python binding layer under CadQuery/build123d. | Already in plan; OCP is the load-bearing dep |
| FreeCAD 1.0 (Nov 2024) + InventorLoader | LGPL (addon: ⚠️ unverified) | Headless `freecadcmd`; InventorLoader *claims* native Inventor `.ipt` read — **unconfirmed**, pilot it | Possible partial Inventor read; **don't assume** |

> **Native SolidWorks/Inventor/Parasolid read:** still **no verified OSS path**. InventorLoader, `openswx`,
> `sldprt-converter` claims did **not** survive verification → the one-time **vendor-seat STEP export** remains the unlock.

## Part B — CAD-AI models & datasets (HuggingFace / GitHub)

Licenses below **verified firsthand** (GitHub License API). ✅ = commercial-OK · 🔴 = non-commercial/research-only.

| Project | License (verified) | Task | Commercial? |
|---|---|---|---|
| **UV-Net** (`AutodeskAILab/UV-Net`) | **MIT** | B-rep representation learning → face/edge/solid **embeddings**, classification, segmentation | ✅ |
| **AAGNet** (`whjdark/AAGNet`) | **MIT** | GNN **machining-feature recognition** on B-rep (semantic/instance/bottom-face) | ✅ |
| **cadrille** (`col14m/cadrille`) | **Apache-2.0** | Multimodal **point-cloud/image/text → CadQuery code**; ICLR 2026; weights on HF | ✅ |
| **CAD-Coder** (`anniedoris/CAD-Coder`) | **Apache-2.0** | VLM: **image/text → editable CadQuery**; Qwen2.5-7B base | ✅ |
| **DeepCAD** (`ChrisWu1997/DeepCAD`) | **MIT** | Generative B-rep model **+ the DeepCAD dataset** | ✅ |
| **MFCAD** (`hducg/MFCAD`) | **MIT** | STEP/B-rep dataset **labelled with machining features** | ✅ (train/benchmark) |
| **TRELLIS** (`microsoft/TRELLIS`) | **MIT** | image/text → 3D mesh generation | ✅ |
| **Hunyuan3D-2** (`Tencent-Hunyuan`) | Tencent Community License | image → 3D mesh | ⚠️ OK < 1M MAU |
| **CAD-Recode** (`filaPro/cad-recode`) | **CC BY-NC 4.0** | **point cloud → CadQuery code** (reverse-engineering) | 🔴 research-only |
| **Point2CAD** (`prs-eth/point2cad`) | CC BY-NC 4.0 | point cloud → surfaces/edges/corners | 🔴 research-only |
| **BRepNet** (`AutodeskAILab/BRepNet`) | **CC BY-NC-SA 4.0** | per-face B-rep segmentation | 🔴 research-only |
| **Fusion360 Gallery Dataset** | custom/Other | large CAD-sequence dataset | ⚠️ verify (likely NC) |
| **Zoo/KittyCAD Text-to-CAD** | hosted API | text → CAD | 💰 product, not open weights |

## Part C — how this maps to our work (highest-leverage first)

1. **Geometric similarity search + feature recognition over the estate → attacks dedup & variant-families.**
   **UV-Net (MIT)** embeddings + **AAGNet (MIT)** feature recognition can cluster the 60k SolidWorks parts and the
   416k-file hoard by *shape*, surfacing duplicate/near-duplicate families and machining features — the AI complement
   to the content-hash dedup (#1007) and the parametric-variant opportunity (#1009-#3). **Commercial-safe.**
2. **STEP → clean glTF/3MF derivatives:** add **cascadio (MIT)** + **manifold3d (Apache)** to the pilot's export
   stage → lighter, topologically-robust viz/print outputs (3MF/glTF `EXT_mesh_manifold`, not STL).
3. **Parametric variant generation:** **build123d (Apache)** as the authoring engine to collapse "N-compartment /
   per-load-case" copies into one parametric model (the #1009 top-ROI item).
4. **"Describe-a-part → editable model" assist:** **cadrille / CAD-Coder (Apache)** turn text/image into CadQuery —
   an engineer-facing accelerator; pilot for our part families.
5. **Reverse-engineering scans → CAD (research-only):** **CAD-Recode / Point2CAD (CC BY-NC)** are compelling for
   point-cloud→CAD but **cannot ship commercially** — prototype/evaluate only, or seek a commercial license.
6. **Training/benchmark corpora:** **DeepCAD + MFCAD (MIT)** for any in-house feature-recognition fine-tuning.

## Caveats

- Both research runs were rate-limited in verification; **licenses above are independently verified**, but model
  capability/recency/weight-availability claims are workflow-level and need a hands-on pilot before reliance.
- Treat all 🔴 CC-BY-NC items as **research-only** — they are tempting but legally unusable in a client pipeline
  without separate permission. Confirm the Fusion360 Gallery dataset terms before any use.
- No OSS native SolidWorks/Inventor/Parasolid reader was confirmed — the seat export stays the unlock.

> **Suggested follow-up (new sub-issue under #1011):** a CAD-AI spike — UV-Net + AAGNet (MIT) for shape-similarity
> clustering + feature recognition on a sample of the SolidWorks estate, to quantify dedup/variant leverage.
