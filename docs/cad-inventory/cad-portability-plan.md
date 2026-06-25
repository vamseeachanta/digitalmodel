# CAD-File Portability Plan — maximize tool-independence of the CAD estate

**Context:** follows the CAD/CAM discovery epic [#1004](https://github.com/vamseeachanta/digitalmodel/issues/1004) and feeds the
file-format adapter epic [#1011](https://github.com/vamseeachanta/digitalmodel/issues/1011).
**Privacy:** digitalmodel is PUBLIC — no client/path identities here; portable *artifacts* themselves stay on the share/private.

---

## 1. Objective

Make every CAD asset on the share usable by the **most tools, without the originating license, durably (archival),
and with metadata fidelity** — i.e. portability of *information*, not just geometry. The discovery ([#1005](https://github.com/vamseeachanta/digitalmodel/issues/1005)–[#1009](https://github.com/vamseeachanta/digitalmodel/issues/1009))
showed the estate is **locked inside Windows-only native formats** (60k SolidWorks, 184k Inventor, 237k AutoCAD)
with only ~3,138 neutral files already portable — and digitalmodel has **no CAD reader** today.

## 2. Target-format strategy

| Use | Recommended target | Rationale |
|---|---|---|
| **Primary 3D / archival** | **STEP AP242** (ISO 10303-242) | Vendor-neutral B-rep **+ PMI/GD&T + assembly tree + product data**; the LOTAR long-term-archival standard. Upgrade from the AP203/AP214 STEP already on the share. |
| **CAD round-trip exchange** | **Parasolid `.x_t`** (secondary) | SolidWorks / NX / Onshape / Solid Edge share the **Parasolid kernel** → near-lossless re-import when geometry must go *back* into CAD. |
| **Visualization / web / AR** | **glTF 2.0 `.glb`** | Best-supported 3D web/viewer/AR format; compact, textured, render-ready. |
| **Fabrication / 3D-print** | **3MF** (STL only as fallback) | Carries units, materials, color, multiple objects; STL loses all of it and is unitless. |
| **2D drawings** | **DXF + PDF/A** companions | DXF = portable & editable; PDF/A = fixed archival. DWG→DXF via **ODA File Converter** (no AutoCAD seat). |
| **Information / metadata** | **JSON sidecar + PNG thumbnail** | BOM tree, mass, material, bounding box, source file, revision, units — one sidecar per part/assembly. |
| **Avoid as a *write* target** | ~~IGES~~ | Legacy, surface-only, lossy for solids. **Read** existing IGES; never **emit** it. |

> Net rule: **one canonical archival master (STEP AP242) + purpose-built derivatives (Parasolid, glTF, 3MF, DXF/PDF)
> + a JSON metadata sidecar**, per asset.

## 3. Tiered pipeline

### Tier 0 — license-free, runnable now
Ingest the **existing neutral files** (STEP/IGES/Parasolid, ~3,138) → read headless with **pythonocc-core/OCP** or
**FreeCAD (headless)** → emit normalized **STEP AP242 + glTF + JSON sidecar + thumbnail**. DWG→DXF via **ODA**.
No CAD seat required.
- **Pilot target:** the SLOR `URA` and FDAS `tensioner-cart` STEP/IGES files already on the share (proven readable
  in [#1008](https://github.com/vamseeachanta/digitalmodel/issues/1008) — a 485-component / 719-solid assembly parsed with no license).

### Tier 1 — one licensed seat (the single unlock)
A **SolidWorks / Inventor batch macro** exports every native part & assembly → **STEP AP242 + Parasolid + PDF/DXF
drawings**. This is the *only* license-gated step; run it **once** and the entire native estate becomes
Tier-0-portable permanently.
- Caveat: 2007-era files need a **version-compatible seat** to open; very old files may require a migration hop.

### Tier 2 — derivatives + validation
Generate viz/print derivatives (glTF/3MF), **validate round-trip** with OCC as the oracle, content-address + dedup,
and write a portability manifest.

## 4. Fidelity validation (round-trip oracle)

For each converted asset, compare source vs portable on objective invariants and flag drift:

| Metric | Tolerance (suggested) |
|---|---|
| Solid-body count | exact match |
| Mass / volume (given density) | ≤ 0.1% |
| Bounding-box dimensions | ≤ 0.1 mm |
| Assembly component count | exact match |
| Face count | logged (informational) |

This is exactly the headless STEP-parse capability demonstrated in #1008 — no CAD seat needed to *validate*.

## 5. Storage & organization

Per asset, keep a **4-tuple**: `source` (native, on share) · `master` (STEP AP242) · `derivatives` (glTF/3MF/DXF/PDF)
· `metadata` (JSON sidecar). Content-address + de-duplicate (directly addresses the 416k-file personal-backup hoard
from [#1007](https://github.com/vamseeachanta/digitalmodel/issues/1007)). Portable artifacts inherit the source's
client sensitivity → stay on share/private; only de-identified aggregates go to public digitalmodel.

## 6. Blockers & risks

- **The one export seat** — Tier 1 needs a SolidWorks/Inventor license once; sequence it as a discrete, bounded task.
- **Old format versions** — DWG spans R11→2018 (ODA handles all); SW 2007-era files need a compatible opener.
- **Assembly external references** — native assemblies reference external parts; export must preserve the tree
  (AP242 does) or flatten deliberately.
- **PMI/GD&T capture** depends on the seat version's AP242 export support — verify before trusting PMI round-trip.

## 7. Mapping to the adapter epic (#1011) & recommended first action

| Plan element | Epic issue |
|---|---|
| License-free readers (OCC/FreeCAD/ezdxf/ODA) evaluated on real files | [#1015](https://github.com/vamseeachanta/digitalmodel/issues/1015) |
| `digitalmodel/cad/` package (neutral reader + converters + validate) | [#1016](https://github.com/vamseeachanta/digitalmodel/issues/1016) |
| Tier 1 SW/Inventor → STEP export macro | [#1012](https://github.com/vamseeachanta/digitalmodel/issues/1012) / [#1016](https://github.com/vamseeachanta/digitalmodel/issues/1016) |

**Recommended first action:** a **Tier-0 license-free pilot** — STEP AP242 + glTF + JSON sidecar over the existing
URA / tensioner-cart STEP/IGES files — to prove the portable-master pipeline and the validation oracle **before**
committing to the seat-gated Tier 1 export.
