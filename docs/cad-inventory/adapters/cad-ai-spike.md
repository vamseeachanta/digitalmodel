# CAD-AI Spike — shape-similarity + feature recognition (design)

**Issue:** [#1017](https://github.com/vamseeachanta/digitalmodel/issues/1017) (epic [#1011](https://github.com/vamseeachanta/digitalmodel/issues/1011)) · builds on the
[OSS+AI leverage research](../oss-and-ai-leverage-research.md) and the [Tier-0 pilot](../pilot-tier0-results.md).
**Status:** design + feasibility (execution pending approval). **Privacy:** PUBLIC repo — de-identified.

## Why this spike

The discovery surfaced two expensive, repetitive problems the right CAD-AI could attack — **both with
commercial-safe MIT/Apache models**:
1. **Dedup & near-duplicate detection** across the 416k-file inbox hoard ([#1007](https://github.com/vamseeachanta/digitalmodel/issues/1007)) — content-hash misses *re-saved / re-exported* copies that are geometrically identical but byte-different.
2. **Variant-family discovery** — the deep-dive ([#1008](https://github.com/vamseeachanta/digitalmodel/issues/1008)) found parts saved as N hand-built copies (compartment-count, per-load-case, per-revision). Finding these families automatically feeds the top-ROI **parametric variant generation** ([#1009](https://github.com/vamseeachanta/digitalmodel/issues/1009)).
3. (secondary) **Auto-tagging machining features** for a searchable parts register.

## Feasibility (verified)

| Model | License | Input | Pretrained weights? | Deps | Fit for our estate |
|---|---|---|---|---|---|
| **UV-Net** (`AutodeskAILab/UV-Net`) | **MIT** | STEP → DGL face-adjacency graph **via pythonocc** | ❌ none — must train on MFCAD/Fusion360 (preprocessed sets provided) | PyTorch + **DGL 0.6.1** + pythonocc | Embeddings → **shape-similarity** = the dedup/variant lever |
| **AAGNet** (`whjdark/AAGNet`) | **MIT** | STEP → attributed-adjacency graph **via pythonocc 7.5.1/OCCT 7.5.1** | ✅ yes (`/weights`) | PyTorch + DGL + py3.10 | Machining-feature recognition — ⚠️ taxonomy is *milled features*; uncertain transfer to large **weldments/structures** |

Both need STEP input (we have ~3,138 neutral files + pilot exports); native parts stay locked until the seat export.

## Approach — tiered, cheapest-first

### Tier A — OCC geometric fingerprint (NO ML, runs today in `cadpilot`)
Before paying the torch/DGL tax, test whether a **training-free** descriptor already clusters the known families.
Per STEP solid, compute (all from OpenCASCADE, mostly already in the pilot):
- volume, surface area, **bbox aspect ratios** (scale-invariant), solid/face/edge counts,
- **principal moments of inertia + radii of gyration** (rotation/scale-normalizable shape signature),
- face-type histogram (planar / cylindrical / conical / spline counts).

→ normalize → cosine / L2 nearest-neighbour → cluster (DBSCAN). **Validation oracle:** do the known families group?
- the buoyancy-tank compartment variants should land in one tight cluster,
- the padeye variants (plain / slotted / hydrate) should cluster,
- a known re-export pair (same part, STEP vs IGES) should be near-duplicate.

If Tier A separates families with usable precision/recall, **we may not need the neural nets at all** for dedup/variants.

### Tier B — learned embeddings (UV-Net, MIT) only if Tier A under-performs
Train a UV-Net encoder on **MFCAD + Fusion360 Gallery** (preprocessed sets provided), embed our STEP parts via the
pythonocc graph featurizer, and re-run the same clustering + similarity-search evaluation against Tier A. CPU
inference is fine for a few-hundred-part sample; training needs a GPU or patience.

### Tier C — feature recognition (AAGNet, MIT, pretrained)
Run pretrained AAGNet on a sample and **report honestly** whether milled-feature labels (holes/slots/pockets) are
meaningful on subsea weldments — likely low transfer; the value, if any, is on machined sub-components, not frames.

## Deliverables & go/no-go

- A similarity matrix + cluster report over a de-identified sample, with **precision/recall on the known families**.
- **Go** (build a shape-similarity index for the estate) if Tier A or B recovers known families at usable accuracy.
- Outputs map to: dedup the hoard by *shape*; auto-discover variant families → seed parametric generation; a
  "find parts like this" geometric search.

## Risks / notes

- **No UV-Net pretrained weights** → Tier B needs a training run; Tier A is the de-risk.
- **AAGNet taxonomy mismatch** (milled vs structural) — measure, don't assume.
- **Dep pinning**: DGL 0.6.1 / pythonocc 7.5.1 are older than the pilot's OCCT 7.9 — Tier B/C may need a separate
  pinned env. Tier A runs in the existing `cadpilot` env unchanged.
- **Native parts locked** → the spike runs on neutral STEP only until the seat export ([#1006](https://github.com/vamseeachanta/digitalmodel/issues/1006)/[#1012](https://github.com/vamseeachanta/digitalmodel/issues/1012)) widens coverage.
- **License guardrail:** UV-Net/AAGNet/MFCAD/DeepCAD = commercial-OK; **CAD-Recode/Point2CAD/BRepNet = CC-BY-NC, do NOT use** here.

## Recommended first step

Run **Tier A** on a de-identified sample of the existing STEP set in the `cadpilot` env (no new deps, no training) —
it directly tests the dedup/variant hypothesis at near-zero cost and tells us whether the ML tiers are even needed.
