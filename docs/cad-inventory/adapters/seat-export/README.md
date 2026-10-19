# Seat-Export Package — unlock native SolidWorks/Inventor into the pipeline

**Issue:** adapter epic [#1011](https://github.com/vamseeachanta/digitalmodel/issues/1011) ([#1012](https://github.com/vamseeachanta/digitalmodel/issues/1012)). **State of readiness:** ready-to-run batch macros so a licensed
**SolidWorks**/**Inventor** seat can export the native estate to STEP, which then flows through the existing
**dedup → glTF → Blender → index** pipeline (validated in [pilot-tier0-results.md](../../pilot-tier0-results.md) /
[blender-unlock.md](../blender-unlock.md)). Privacy: project-specific batch lists (real paths) are kept private on the
share; the tooling here is generic.

## Why a seat is needed (verified hands-on)
There is **no open-source reader for native SolidWorks/Inventor/Parasolid**:
- FreeCAD 0.21 built-in **cannot open** `.sldprt` or `.ipt` (tested — both error).
- The FreeCAD **InventorLoader** addon can read *some* `.ipt` (parts, not assemblies) but is unproven here and
  requires installing external code — see `test_inventorloader.sh` to evaluate it before committing seat time.
- SolidWorks/Parasolid: commercial only (Datakit CrossCad/Ware) or the vendor seat.

→ A **one-time batch STEP export from a seat** is the reliable unlock; afterward everything is license-free.

## Contents
| File | What |
|---|---|
| `batch_export_solidworks.bas` | SolidWorks VBA macro — list-driven batch STEP export (AP214 default; AP242 option) |
| `batch_export_inventor.bas` | Inventor VBA macro — same via the STEP Translator AddIn |
| `gen_native_batches.py` | (runs on the file server) generates per-project batch lists from the readability index |
| `test_inventorloader.sh` | user-initiated eval of the OSS Inventor reader on the file server's FreeCAD |

## Procedure (Windows seat)
1. **Map the share** as a drive so paths resolve — `Z:\` → the share root (matches `SRC_ROOT="Z:\"` in the macros).
2. **Generate batch lists:** run the configured command below. Copy the resulting
   Windows-mapped path lists to the private location configured by `LIST_FILE`.
3. **Edit the macro CONFIG** (`LIST_FILE`, `OUT_ROOT`, `SRC_ROOT`, `STEP_AP`).
4. **TEST first** on a 5-line list; open the resulting `.step` in a viewer; then run full batches.
5. **Run in priority order** (filename prefix): the **curated 3D library** and **active project trees** first — they have
   **intact assembly references** and the real engineering value — and any **personal-backup archive last** (lower value,
   possibly unresolved references).
6. **Output** mirrors under `OUT_ROOT`; copy it to the share's `_step-export/out/`.

## Configured batch generation

The generator requires every storage location as an argument. Set these variables
in the local shell configuration; no private archive location is embedded in the
script or this document.

```bash
python docs/cad-inventory/adapters/seat-export/gen_native_batches.py \
  --index "$CAD_READABILITY_INDEX" \
  --output-dir "$CAD_BATCH_OUTPUT" \
  --source-root "$CAD_SOURCE_ROOT" \
  --windows-root "$CAD_WINDOWS_ROOT"
```

`CAD_READABILITY_INDEX` names the private TSV with `path`, `format`, `readability`
and `project` columns. `CAD_SOURCE_ROOT` is the absolute POSIX archive root used by
the index. `CAD_WINDOWS_ROOT` is its mapped drive or UNC equivalent and shall match
the macro's `SRC_ROOT`. Mapping uses path components; paths outside the source root
or containing traversal or list separators are rejected before output is written.
Unsupported formats and rows with another readability classification are skipped.

`CAD_BATCH_OUTPUT` shall name a new directory outside public Git. An existing
directory is rejected to preserve earlier runs. Batch lists retain CRLF separators,
and `_manifest.json` retains per-group counts and priority order. Filename collisions
are rejected, including collisions caused by truncation or Windows case folding.
Console output contains aggregate counts only. Importing the module writes nothing.
The command generates lists only and does not invoke a CAD application.

The input index, batch lists and manifest contain private paths or group names;
they remain private and are not de-identified publication artifacts. Validation
finishes before directory creation, but an I/O failure during writing can leave a
partial directory. Preserve it for diagnosis and select a new directory for a retry.

## Ingestion (automatic, on the file server)
New STEP under `_step-export/out/` is just new STEP files:
1. `batch_convert.py` → adds `.glb` to the library; 2. `build_cad_index.py` → flips those rows to **oss-3d readable** and
links the glb; 3. (optional) `dedup_index.py`. Or tell the agent "the seat export is on the share".

## Notes
- **Assemblies need their parts to resolve** — that's why curated/active trees go first.
- An **assembly** STEP captures the whole product; export **parts** too only if you need them individually.
- `STEP_AP=214` is the safe default; OCCT 7.9 reads/writes **AP242** (PMI) too if your seat supports exporting it.
