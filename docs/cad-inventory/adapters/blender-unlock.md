# Unlocking the CAD Estate for Blender

**Issue:** adapter epic [#1011](https://github.com/vamseeachanta/digitalmodel/issues/1011) · validated 2026-06-25 (Blender on ace-linux-2, conversion on ace-linux-1).
**Question:** how to read the CAD data in Blender. **Answer:** Blender can't read CAD natively — convert to glTF
with the license-free Tier-0 pipeline ([pilot](../pilot-tier0-results.md)) first.

## What Blender reads (5.1, ace-linux-2)

Probed headless (`blender -b --python`):

| Native in Blender ✅ | NOT native ❌ (needs conversion) |
|---|---|
| **glTF/glb**, STL, OBJ, FBX, X3D, PLY, **DXF**, USD | STEP, IGES, Parasolid, native SolidWorks/Inventor, DWG |

No STEP/CAD import add-on is installed (and the good open ones are scarce) — so the path is **convert, then import**,
not "import STEP directly".

## The unlock pipeline (proven end-to-end)

```
 STEP/IGES ──(ace-linux-1: pythonocc/OCC → glTF)──▶ .glb on the share ──▶ Blender (ace-linux-2)
 native SW/Inventor ──(one-time seat export)──▶ STEP ──▶ (same as above)
 DWG ──(ODA → DXF)──▶ Blender DXF import (2D)        DXF ──▶ Blender directly (2D)
```

- **Geometry conversion stays on ace-linux-1** (where OCC/`cadpilot` lives); **Blender stays on ace-linux-2** and
  reads the resulting `.glb` from the share. No CAD license needed for the neutral path.
- **Validated:** a real STEP riser sub-assembly → glTF (a1) → **imported into Blender 5.1 on a2** (10,234 verts /
  19,732 faces, matching the source) → **headless-rendered to PNG**. Both import and render confirmed.

### Convert on a1 (already built — see `pilot-tier0.py`)
`pythonocc-core` reads STEP/IGES → `BRepMesh` tessellation → STL → `trimesh` → `.glb`/`.3mf`. (`cascadio` (MIT) is a
lighter pip-only STEP→GLB alternative.) Output `.glb` is dropped on the share.

### Import / render on a2 (headless)
```bash
blender -b --python import.py -- /path/to/part.glb     # bpy.ops.import_scene.gltf(filepath=...)
blender -b --python render.py -- part.glb out.png      # auto-camera; set cam.clip_end large (glTF is in mm)
```
> **Gotcha:** glTF from CAD is in **millimetres**, so a ~2 m part is ~2000 Blender units — set the camera
> `clip_end` to ~20× the model radius or the render comes out blank (default far-clip = 1000).

## Coverage & what's still locked

- **Neutral 3D (STEP/IGES/Parasolid-via-export) → Blender:** ✅ license-free today.
- **2D drawings (DXF):** ✅ import directly; DWG → DXF via ODA first.
- **Native SolidWorks/Inventor:** ❌ still need the one-time vendor-seat STEP export ([#1012](https://github.com/vamseeachanta/digitalmodel/issues/1012)) — no OSS native reader.
- **Scale:** to unlock the whole estate, batch-convert the neutral-STEP set on a1 → `.glb` library on the share;
  Blender (or a web glTF viewer) consumes it. Pairs naturally with the dedup (convert unique canonicals only).

## Recommendation

Add a `digitalmodel/cad/convert` batch STEP→glTF step (the pilot code generalized) that writes a `.glb` next to each
canonical CAD file; point Blender on a2 (or an `online-3d-viewer`/three.js page) at that library. This makes the
estate **viewable and editable in Blender without any CAD license** — the Phase-2 "see the model" capability.
