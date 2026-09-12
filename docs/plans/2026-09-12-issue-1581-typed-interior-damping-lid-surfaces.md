# Plan for #1581: Support typed interior and damping-lid surfaces in diffraction cases

> **Status:** draft
> **Complexity:** T3
> **Date:** 2026-09-12
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1581
> **Client:** N/A
> **Lane:** lane:codex
> **Review artifacts (target paths to be produced by the review fanout; not pre-existing evidence):** `scripts/review/results/2026-09-12-plan-1581-claude.md` | `...-codex.md` | `...-gemini.md`

---

## Resource Intelligence Summary

Six distinct sources are consulted below (issue body, four source modules, one prior plan, one
sibling-issue set). Each carries a concrete finding with a verifiable file path or issue number.

### Existing repo code

- **Found:** `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py:779-799` — `DampingLidSpec`
  carries exactly four fields (`mesh_file`, `mesh_format`, `length_units`, `damping_factor`). It has
  no surface identity, no role, no body association, no reference frame, no waterline placement and
  no panel-normal convention.
- **Found:** `input_schemas.py:983-990` — `DiffractionSpec.damping_lid` is
  `Optional[DampingLidSpec]`, i.e. **at most one lid per analysis, spec-global**, not per body. A
  multi-moonpool body cannot be expressed.
- **Gap:** no interior free-surface panel mesh can be declared anywhere. `input_schemas.py:460-472`
  (`IrregularFrequencyMethod`) offers only `none` / `interior_panels` / `control_surface`, and
  `interior_panels` is a *solver-generated* triangulation, not a user-supplied mesh.
- **Found:** `orcawave_backend.py:267-275` — interior surfaces are emitted as a **boolean pair**
  derived from that global enum:

  ```
  272	    add_interior = irreg_method is IrregularFrequencyMethod.INTERIOR_PANELS
  273	    body["BodyAddInteriorSurfacePanels"] = _bool_to_yn(add_interior)
  274	    if add_interior:
  275	        body["BodyInteriorSurfacePanelMethod"] = "Triangulation method"
  ```

  There is no path by which a supplied interior mesh file reaches the generated OrcaWave input.
- **Found (closest prior art):** the control-surface slot,
  `input_schemas.py:826-879` (`ControlSurfaceSpec`) plus `orcawave_backend.py:277-310`
  (`BodyControlSurface*` emission) plus `orcawave_backend.py:63-68`
  (`_CONTROL_SURFACE_FORMAT_MAP`). This is the only existing secondary surface with a per-body
  association, a per-slot format map, a fail-loud unsupported-format guard
  (`orcawave_backend.py:290-295`) and a "must be generatable" validator
  (`input_schemas.py:859-879`). It is the template the typed-surface contract will follow.
- **Found (second prior art):** the free-surface zone, `input_schemas.py:801-823`
  (`FreeSurfaceZoneSpec`) and `orcawave_backend.py:680-691`. It is spec-global like the damping lid
  and shares the same identity/association gaps.
- **Found:** `mesh_packaging.py:64-100` (`_iter_mesh_slots`) is the single source of truth for
  which mesh files a spec makes the solver depend on, with a four-valued `kind` discriminator
  declared at `mesh_packaging.py:54`: `body | damping_lid | control_surface | free_surface_zone`.
  Any new surface role must be added here or it will not be packaged, validated or pre-flighted.
- **Gap:** `mesh_packaging.py:303-318` appends a provenance record **only inside the conversion
  branch**. A `.gdf` lid that is solver-ready is copied at `mesh_packaging.py:279-284` and produces
  no provenance record at all, so `mesh_provenance.json` cannot today identify every applied
  secondary surface.
- **Gap:** no hashing exists anywhere in the diffraction package (gap proof below), so panel-count
  and hash provenance must be built from scratch.
- **Found:** `quality_gates.py:1-15` documents the blocking calibration and states the reason
  plainly: *"legitimate diffraction hull meshes are open at the waterline, so the watertightness
  check fails them by design; small meshes also trip the panel-count check. Such meshes score
  WARNING and must pass through."* The threshold itself is `geometry_quality.py:418-423`
  (PASS at 5/5, WARNING at >=3/5, FAIL below 3/5). **This plan will not change that calibration** —
  an interior lid or damping lid is an open patch by construction and will legitimately fail
  watertightness, so a blocking watertightness check would reject every valid secondary surface.
- **Found:** `aqwa_backend.py` contains **zero** references to `damping_lid`, `control_surface` or
  `free_surface_zone` (counts below). `AQWABackend.generate_single` (`aqwa_backend.py:140-178`) and
  `_assemble_all_decks` (`aqwa_backend.py:226-231`) therefore write a complete deck while silently
  discarding every secondary surface the spec declares.
- **Found:** `reverse_parsers.py:577-579` recognises `BodyAddInteriorSurfacePanels` on the way back
  in, and nothing else — there is no reverse mapping for any `DampingLid*` key, so the round trip is
  already lossy for the one secondary surface the schema does support.

### Standards

Not applicable. This issue defines an internal solver-input contract; no published code or standard
is transferred and no standards-derived constant is introduced, so the calc-citation contract
(`.claude/rules/calc-citation-contract.md`) does not fire.

### LLM Wiki pages consulted

No relevant wiki pages. The contract is repo-internal and the issue's boundary clause forbids client
or vessel content, so no client-sibling routing applies (`Client: N/A`).

### Documents consulted

- `docs/plans/2026-05-15-issue-609-orcawave-auxiliary-mesh-handling.md` — the auxiliary-mesh
  normalization plan. Its scope line establishes the separation this plan inherits: body-mesh
  packaging is #605, conversion is #606, runner path-resolution is #500/#605, and #609 owns the
  schema/backend/accessor contract for auxiliary assets. Its gap statement ("Body-level
  control-surface mesh filenames can be silently ignored in multi-body specs") is the same defect
  class this issue raises one level up, for surfaces that have no schema slot at all.
- Epic **[#1825](https://github.com/vamseeachanta/digitalmodel/issues/1825)** — "Diffraction results
  agree across solvers — one strict contract, three-way benchmarks". Its definition of done names
  this issue's subject explicitly: *"interior/lid surface types ... each have one declared meaning,
  enforced at parse time, preserved through reverse parsing, and identical in the AQWA and OrcaWave
  backends."* Parent initiative: vamseeachanta/workspace-hub#3601.
- Sibling issues **#1579**, **#1580**, **#1582** — the other three contract defects under the same
  epic (inertia-tensor origin, restoring stiffness, matrix units). All three are numerical-contract
  defects; #1581 is the geometry-contract defect and is the most independent of the four. See Risks.
- `tests/hydrodynamics/diffraction/test_body_level_control_surfaces.py:1-6` — the existing test
  idiom for a secondary-surface resolution rule, asserting backend emission, mesh packaging and
  preflight share one rule. The new tests will follow its shape.
- `tests/hydrodynamics/diffraction/fixtures/acceptance_610/unit_box.gdf` — an existing **generic
  synthetic** box fixture (731 bytes), header line `Unit box, 5 wetted panels (bottom + 4 sides,
  open top at waterline)`. It establishes both the fixture style and the precedent that a
  deliberately non-watertight box is a valid committed fixture. No drive-file search is performed:
  the issue's boundary clause confines this work to synthetic geometry, so no external document
  corpus is in scope — **no relevant drive files**.

### Gaps identified

1. No typed secondary-surface declaration exists — neither `interior free surface` nor
   `damping lid` can be named, given a role, or bound to a body.
2. No surface identity exists, therefore no duplicate-ID rejection is possible.
3. No unit or reference-frame agreement is checked between a secondary surface and its body.
4. No waterline-placement validation exists for a lid or interior surface.
5. No panel-normal convention is declared for any secondary surface.
6. OrcaWave emission cannot distinguish an interior irregular-frequency surface from a damping lid
   supplied as a mesh; only the spec-global lid keys exist.
7. AQWA drops secondary surfaces silently instead of failing closed.
8. `mesh_provenance.json` omits solver-ready passthrough copies entirely and carries no role, no
   identity, no hash and no damping parameter.
9. No deterministic emission ordering is defined, because multiple secondary surfaces cannot yet
   exist.
10. No generic synthetic fixture pairs a box hull with separate interior and lid meshes.

### Evidence (embedded verification)

**Issue statuses** (verified 2026-09-12T11:46:51Z via `gh issue view`):

- `#1581` — OPEN — Support typed interior and damping-lid surfaces in diffraction cases; labels
  `cat:engineering, domain:hydro, lane:codex, machine:licensed-win-1, status:needs-plan`; parent
  `vamseeachanta/digitalmodel#1825`; 0 comments.
- `#1825` — OPEN — [Epic] Diffraction results agree across solvers — one strict contract, three-way
  benchmarks; labels `cat:engineering, priority:high, domain:hydro, epic, machine:multi`.
- `#1579` — OPEN — Make diffraction inertia-tensor origin strict and consistent across backends.
- `#1580` — OPEN — Define a solver-neutral diffraction restoring-stiffness contract.
- `#1582` — OPEN — Normalize diffraction matrix units across AQWA and OrcaWave backends.

Repository state at planning time: branch `main`, HEAD
`6f808a610caf5eb9045d1ebe00a86dc2c87292ea` (2026-09-11 17:23:26 -0500).

**File existence** (checked 2026-09-12T11:46:51Z):

- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` (942 lines)
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` (727 lines)
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` (723 lines)
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/mesh_packaging.py` (291 lines)
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` (255 lines)
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/geometry_quality.py` (403 lines)
- EXISTS: `src/digitalmodel/hydrodynamics/diffraction/quality_gates.py` (176 lines)
- EXISTS: `tests/hydrodynamics/diffraction/fixtures/acceptance_610/unit_box.gdf` (731 bytes)
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_secondary_surfaces_schema.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_secondary_surfaces_orcawave.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_secondary_surfaces_aqwa_failclosed.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/test_secondary_surfaces_provenance.py`
- MISSING (new — this plan will create): `tests/hydrodynamics/diffraction/fixtures/secondary_surfaces/` (box hull, interior, lid, spec)

**Line excerpts.**

`input_schemas.py:779-799` — the whole of the only secondary-surface-with-damping model today:

```
779	class DampingLidSpec(BaseModel):
780	    """Resonance damping lid specification (for moonpool bodies)."""
781	
782	    mesh_file: str = Field(
783	        ...,
784	        description="Path to damping lid mesh file (relative to spec.yml)",
785	    )
786	    mesh_format: str = Field(
787	        default="gdf",
788	        description="Mesh file format (gdf, dat, etc.)",
789	    )
790	    length_units: str = Field(
791	        default="m",
792	        description="Length units of the lid mesh file",
793	    )
794	    damping_factor: float = Field(
795	        ...,
796	        gt=0,
797	        description="Damping factor epsilon for the lid",
798	    )
799	```

`mesh_packaging.py:48-54` — the slot discriminator that any new role must join:

```
48	class _MeshSlot(NamedTuple):
49	    """A mesh reference plus the spec object holding it (for rewrites)."""
50	
51	    label: str
52	    mesh_file: str
53	    owner: Any  # model carrying mesh_file (and possibly mesh_format)
54	    kind: str = "body"  # body | damping_lid | control_surface | free_surface_zone
```

`quality_gates.py:6-11` — the calibration this plan preserves:

```
6	- ``FAIL`` (fewer than 3 of 5 checks pass) **blocks** solve/package
7	  generation — the geometry is unusable.
8	- ``WARNING`` is reported but never blocks. Calibration note: legitimate
9	  diffraction hull meshes are open at the waterline, so the watertightness
10	  check fails them by design; small meshes also trip the panel-count check.
11	  Such meshes score WARNING and must pass through.
```

`geometry_quality.py:418-423` — the numeric threshold behind it:

```
418	        if report.passed_checks == report.total_checks:
419	            report.overall_status = "PASS"
420	        elif report.passed_checks >= 3:
421	            report.overall_status = "WARNING"
422	        else:
423	            report.overall_status = "FAIL"
```

`mesh_packaging.py:303-309` — provenance appended only on the conversion branch:

```
303	                record: dict[str, Any] = {
304	                    "reference": slot.label,
305	                    "source": str(source),
306	                    "source_format": extension.lstrip("."),
307	                    "packaged": dest.name,
308	                    "target_format": dest.suffix.lstrip("."),
309	                }
```

`orcawave_backend.py:648-660` — the single, spec-global lid section:

```
648	def _build_damping_lid_section(spec: DiffractionSpec) -> dict[str, Any]:
649	    """Build the damping lid section (for moonpool bodies)."""
650	    lid = spec.damping_lid
651	    if lid is None:
652	        return {}
653	
654	    mesh_fmt = _MESH_FORMAT_MAP.get(lid.mesh_format, "Wamit gdf")
655	    return {
656	        "DampingLidMeshFileName": Path(lid.mesh_file).name,
657	        "DampingLidMeshFormat": mesh_fmt,
658	        "DampingLidMeshLengthUnits": lid.length_units,
659	        "DampingFactorEpsilon": lid.damping_factor,
660	    }
```

**Gap proofs** (run 2026-09-12T11:46:51Z from `D:\ws\digitalmodel`):

- `Select-String -Path src/.../aqwa_backend.py -Pattern "damping_lid|control_surface|free_surface_zone"` → 0 matches.
  Programmatic count against the module source confirms `damping_lid` = 0, `control_surface` = 0,
  `free_surface_zone` = 0. Confirms the AQWA backend has no secondary-surface handling of any kind.
- `Select-String -Path src/digitalmodel/hydrodynamics/diffraction/*.py -Pattern "sha256|hashlib"` →
  *(PowerShell completed with no output)* → confirms no hashing primitive exists in the package; the
  hash provenance criterion must be built from scratch.
- `Select-String -Path src/.../reverse_parsers.py -Pattern "DampingLid|Interior|ControlSurface|FreeSurfacePanelled"`
  → 3 matches, all `BodyAddInteriorSurfacePanels` at lines 577-579 → confirms no reverse mapping for
  any lid or control-surface key.
- `[f for f in DiffractionSpec.model_fields if "surface" in f]` → `['free_surface_zone']` → confirms
  no interior-surface or secondary-surface field on the top-level spec.

**Reproduction proofs.**

The issue alleges a contract gap rather than a runtime crash, so the reproduction is a schema and
backend probe (script held in the session scratchpad; it writes nothing to the repository).

```
$ D:\ws\digitalmodel\.venv\Scripts\python.exe <scratchpad>\repro_1581.py

=== P1: unknown typed-surface block is SILENTLY DROPPED ===
  validated OK: DiffractionSpec
  hasattr secondary_surfaces: False
  model_extra: None
  fields with 'surface' in name: ['free_surface_zone']

=== P2: DampingLidSpec has no id / role / body / frame / waterline ===
  DampingLidSpec fields: ['mesh_file', 'mesh_format', 'length_units', 'damping_factor']
  BodySpec fields: ['vessel', 'position', 'attitude', 'control_surface', 'morison_elements', 'connection_parent']
  damping_lid cardinality on DiffractionSpec: typing.Optional[...DampingLidSpec]

=== P3: two lids cannot be declared ===
  rejected (single lid only): 1 validation error for DiffractionSpec

=== P4: duplicate-id / unknown-role validation does not exist ===
  lid accepted with bogus role + ft units, extras dropped:
    {'mesh_file': 'lid.gdf', 'mesh_format': 'gdf', 'length_units': 'ft', 'damping_factor': 0.2}
  body mesh units: m | lid units: ft -> no mismatch check fires

=== P5: AQWA backend silently drops the lid (no fail-closed) ===
  'damping_lid' occurrences in aqwa_backend.py source: 0
  'control_surface' occurrences: 0
  'free_surface_zone' occurrences: 0
```

A second probe resolves the one ambiguous line in P5. A naive `"lid" in text` match over the
generated AQWA deck returns True, but the matching line is `1ILID AUTO   21` — an unrelated AQWA
element-ID card, not a damping lid. The precise probe settles it:

```
$ D:\ws\digitalmodel\.venv\Scripts\python.exe <scratchpad>\repro_1581b.py

AQWA deck bytes: 3777
'moonpool_lid' in deck: False
lines matching /lid/i: ['1ILID AUTO   21']
AQWA exit status: generated WITHOUT error despite an unmappable lid

OrcaWave keys mentioning lid/Interior:
    HasResonanceDampingLid: Yes
    BodyAddInteriorSurfacePanels: Yes
    BodyInteriorSurfacePanelMethod: Triangulation method
    DampingLidMeshFileName: moonpool_lid.gdf
    DampingLidMeshFormat: Wamit gdf
    DampingLidMeshLengthUnits: m
    DampingFactorEpsilon: 0.2
```

- Reproduced at: 2026-09-12T11:46:51Z
- Failure mode observed matches issue claim: **YES — REPRODUCES.** The contract cannot declare
  separate interior or damping-lid panels. Concretely: (a) a `secondary_surfaces` block validates
  cleanly and is discarded without warning, because Pydantic v2 ignores extra keys by default and
  `model_extra` is `None`; (b) the lid is spec-global and single-valued; (c) role, identity, body
  association, frame and waterline placement have no representation; (d) a lid declared in metres
  against a body in metres is never cross-checked, and a `ft`/`m` mismatch passes; (e) the AQWA deck
  is written successfully with the lid absent from it. One finding **extends** the issue text: the
  same silent-drop applies to `control_surface` and `free_surface_zone` on the AQWA path, so the
  fail-closed guard shall cover all secondary-surface kinds, not the two new roles only.

<!-- Distinct sources consulted: 6 (issue body; four diffraction source modules; prior plan #609;
     epic #1825 + sibling issues #1579/#1580/#1582). Minimum 3 required. -->

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-09-12-issue-1581-typed-interior-damping-lid-surfaces.md` |
| Schema | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` |
| OrcaWave emission | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` |
| AQWA fail-closed guard | `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` |
| Packaging + provenance | `src/digitalmodel/hydrodynamics/diffraction/mesh_packaging.py` |
| Quality-gate labelling | `src/digitalmodel/hydrodynamics/diffraction/quality_gates.py` |
| Tests — schema | `tests/hydrodynamics/diffraction/test_secondary_surfaces_schema.py` |
| Tests — OrcaWave generation + determinism | `tests/hydrodynamics/diffraction/test_secondary_surfaces_orcawave.py` |
| Tests — AQWA fail-closed | `tests/hydrodynamics/diffraction/test_secondary_surfaces_aqwa_failclosed.py` |
| Tests — packaging, hashes, provenance | `tests/hydrodynamics/diffraction/test_secondary_surfaces_provenance.py` |
| Fixtures (generic synthetic box) | `tests/hydrodynamics/diffraction/fixtures/secondary_surfaces/` |
| Plan review — Claude | `scripts/review/results/2026-09-12-plan-1581-claude.md` |
| Plan review — Codex | `scripts/review/results/2026-09-12-plan-1581-codex.md` |
| Plan review — Gemini | `scripts/review/results/2026-09-12-plan-1581-gemini.md` |
| Docs updates | `docs/plans/README.md` (index entry) |

---

## Deliverable

A typed `secondary_surfaces` declaration on `DiffractionSpec` — each entry carrying an identity,
a role (`interior_free_surface` or `damping_lid`), a body association, units, reference frame,
waterline relationship, panel-normal convention and role-conditional damping parameters — which
validates at parse time, emits to OrcaWave in a deterministic, role-distinguishing order, fails
closed on the AQWA backend, and is recorded surface-by-surface with panel counts and content hashes
in mesh provenance.

---

## Pseudocode

**Schema (`input_schemas.py`).**

```
enum SecondarySurfaceRole:
    INTERIOR_FREE_SURFACE = "interior_free_surface"   # irregular-frequency lid
    DAMPING_LID           = "damping_lid"             # resonance damping lid

model SecondarySurfaceSpec:
    id:                       str, pattern ^[A-Za-z0-9][A-Za-z0-9_-]{0,63}$
    role:                     SecondarySurfaceRole          # unknown value -> ValidationError
    body:                     str                           # must name a body
    mesh_file:                str
    mesh_format:              MeshFormatType = AUTO
    length_units:             str = "m"
    reference_frame:          Literal["body", "global"] = "body"
    waterline_z:              float = 0.0
    panel_normal_convention:  Literal["into_fluid", "out_of_fluid"] = "into_fluid"
    damping_factor:           Optional[float]               # gt=0

    validator role_requires_damping:
        if role is DAMPING_LID  and damping_factor is None -> error "damping_lid requires damping_factor"
        if role is INTERIOR_FREE_SURFACE and damping_factor is not None
                                                -> error "damping_factor is meaningless for an interior surface"

model DiffractionSpec:
    secondary_surfaces: list[SecondarySurfaceSpec] = []

    validator secondary_surface_contract:
        body_names := [b.vessel.name for b in self.get_bodies()]   # declaration order
        seen := {}
        for s in self.secondary_surfaces:
            if s.id in seen              -> error "duplicate surface id {s.id}"
            seen[s.id] := s
            if s.body not in body_names  -> error "surface {s.id} names unknown body {s.body}"
            body := body_by_name[s.body]
            if s.length_units != body.vessel.geometry.length_units
                                         -> error "unit mismatch: surface {u1} vs body {u2}"
            if s.reference_frame == "global" and body position/attitude are non-zero
                                         -> error "global-frame surface on a displaced body is ambiguous"
            if role is DAMPING_LID and s.waterline_z != body.vessel.geometry.waterline_z
                                         -> error "a damping lid must sit at the body waterline"
            if role is INTERIOR_FREE_SURFACE and s.waterline_z > body.vessel.geometry.waterline_z
                                         -> error "an interior surface must not sit above the waterline"

    validator migrate_legacy_damping_lid:        # mirrors _migrate_irregular_frequency_fields
        if damping_lid set and secondary_surfaces non-empty -> error (mutual exclusion)
        if damping_lid set:
            emit DeprecationWarning
            if len(get_bodies()) != 1 -> error "legacy damping_lid is ambiguous for multi-body specs"
            append SecondarySurfaceSpec(id="damping_lid_legacy", role=DAMPING_LID,
                                        body=<sole body>, ...) to secondary_surfaces
```

**Deterministic ordering (`orcawave_backend.py`).**

```
_ROLE_EMISSION_RANK = {INTERIOR_FREE_SURFACE: 0, DAMPING_LID: 1}   # frozen, explicit

function secondary_surfaces_ordered(spec) -> list[SecondarySurfaceSpec]:
    body_index := {name: i for i, name in enumerate(declared body names)}  # list order, not dict order
    return sorted(spec.secondary_surfaces,
                  key = lambda s: (body_index[s.body],
                                   _ROLE_EMISSION_RANK[s.role],
                                   s.id))          # id charset is ASCII-restricted
```

**OrcaWave emission.**

```
for surface in secondary_surfaces_ordered(spec):
    if surface.role is INTERIOR_FREE_SURFACE:
        # attach to the owning body dict, alongside the existing boolean pair
        body["BodyAddInteriorSurfacePanels"]   := "Yes"
        body["BodyInteriorSurfacePanelMethod"] := "Defined by mesh file"
        body["BodyInteriorSurfaceMeshFileName"]    := basename(surface.mesh_file)
        body["BodyInteriorSurfaceMeshFormat"]      := _SECONDARY_FORMAT_MAP[ext]   # fail loud if absent
        body["BodyInteriorSurfaceMeshLengthUnits"] := surface.length_units
    else:  # DAMPING_LID
        emit DampingLidMeshFileName / MeshFormat / MeshLengthUnits / DampingFactorEpsilon
        set general section HasResonanceDampingLid := "Yes"
```

**AQWA fail-closed (`aqwa_backend.py`, at the head of `_assemble_all_decks`).**

```
function reject_unsupported_surfaces(spec):
    unsupported := []
    for surface in secondary_surfaces_ordered(spec):        # same ordering -> stable message
        unsupported.append((surface.id, surface.role, surface.body, surface.mesh_file))
    also collect: spec-level free_surface_zone, per-body control surfaces
    if unsupported:
        raise UnsupportedSurfaceError listing every entry and naming the supported backend
    # raised BEFORE any deck card is built and before any file is opened
```

**Provenance (`mesh_packaging.py`).**

```
extend _MeshSlot with: surface_id, role  (kind gains "secondary_surface")
record a provenance entry for EVERY slot, passthrough copies included:
    { reference, surface_id, role, body, source, source_format, packaged, target_format,
      length_units, reference_frame, panel_normal_convention, waterline_z,
      damping_factor, panel_count, source_sha256, packaged_sha256, solver_mapping }
write records in secondary_surfaces_ordered order, bodies first
```

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | add `SecondarySurfaceRole`, `SecondarySurfaceSpec`, `DiffractionSpec.secondary_surfaces`, the contract validator, the legacy `damping_lid` migration, and both names in `__all__` |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | add `_SECONDARY_SURFACE_FORMAT_MAP` and `_ROLE_EMISSION_RANK`, add the ordering helper, emit interior-surface keys inside `_build_body_dict`, extend `_build_damping_lid_section` to the ordered multi-lid case |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/aqwa_backend.py` | fail closed in `_assemble_all_decks` on every secondary surface it cannot map (new `UnsupportedSurfaceError`, a `ValueError` subclass) |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/mesh_packaging.py` | yield secondary-surface slots, carry `surface_id`/`role`, record provenance for passthrough copies, add panel counts and SHA-256 digests |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/quality_gates.py` | label secondary surfaces by role in the gate result; **no calibration change** |
| Create | `tests/hydrodynamics/diffraction/test_secondary_surfaces_schema.py` | schema validation suite |
| Create | `tests/hydrodynamics/diffraction/test_secondary_surfaces_orcawave.py` | OrcaWave emission, role distinction, determinism |
| Create | `tests/hydrodynamics/diffraction/test_secondary_surfaces_aqwa_failclosed.py` | AQWA fail-closed suite |
| Create | `tests/hydrodynamics/diffraction/test_secondary_surfaces_provenance.py` | packaging, panel counts, hashes, provenance |
| Create | `tests/hydrodynamics/diffraction/fixtures/secondary_surfaces/box_hull.gdf` | generic synthetic box hull (open at the waterline) |
| Create | `tests/hydrodynamics/diffraction/fixtures/secondary_surfaces/box_interior.gdf` | generic synthetic interior free-surface patch |
| Create | `tests/hydrodynamics/diffraction/fixtures/secondary_surfaces/box_lid.gdf` | generic synthetic damping-lid patch |
| Create | `tests/hydrodynamics/diffraction/fixtures/secondary_surfaces/spec_box_secondary_surfaces.yml` | spec pairing the three meshes |
| Update | `docs/plans/README.md` | index this plan |

Every fixture shall be a synthetic axis-aligned box generated from arithmetic in the test suite's
own generator, following `fixtures/acceptance_610/unit_box.gdf`. No client, vessel, project or
private mesh data shall appear in any fixture, spec, docstring or test name.

---

## TDD Test List

Tests shall be written before the corresponding implementation. All paths are under
`tests/hydrodynamics/diffraction/`.

### `test_secondary_surfaces_schema.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_unknown_role_rejected` | an unrecognised role never validates | `role: "moonpool_damper"` | `ValidationError` naming the allowed roles |
| `test_known_roles_accepted` | both declared roles validate | `interior_free_surface`, `damping_lid` | spec validates; roles round-trip through `to_yaml`/`from_yaml` |
| `test_missing_body_association_rejected` | `body` is required | entry with no `body` key | `ValidationError` "field required" |
| `test_unknown_body_name_rejected` | `body` must name a declared body | `body: "not_a_body"` | `ValueError` naming the surface id and the unknown body |
| `test_duplicate_surface_ids_rejected` | ids are unique within a spec | two entries both `id: "lid_a"` | `ValueError` "duplicate surface id 'lid_a'" |
| `test_unit_mismatch_rejected` | surface units must equal the owning body's | body `length_units: m`, surface `ft` | `ValueError` quoting both units |
| `test_frame_mismatch_rejected` | a global-frame surface on a displaced body is ambiguous | `reference_frame: global`, body `position: [10, 0, 0]` | `ValueError` naming the surface and the body offset |
| `test_damping_lid_off_waterline_rejected` | a lid must sit at the body waterline | body `waterline_z: 0.0`, lid `waterline_z: 0.5` | `ValueError` quoting both z values |
| `test_interior_surface_above_waterline_rejected` | an interior surface must not sit above the waterline | body `waterline_z: 0.0`, surface `waterline_z: 0.4` | `ValueError` quoting both z values |
| `test_interior_surface_at_or_below_waterline_accepted` | the boundary case is admissible | surface `waterline_z: 0.0` and `-0.2` | both validate |
| `test_damping_lid_requires_damping_factor` | role-conditional parameter is enforced | `role: damping_lid`, no `damping_factor` | `ValidationError` naming the field |
| `test_interior_surface_rejects_damping_factor` | the parameter is meaningless for the other role | `role: interior_free_surface`, `damping_factor: 0.2` | `ValidationError` explaining the role mismatch |
| `test_nonpositive_damping_factor_rejected` | epsilon must be positive | `damping_factor: 0.0` and `-0.1` | `ValidationError` (`gt=0`) |
| `test_surface_id_charset_enforced` | ids stay ASCII so ordering is locale-free | `id: "lid a"`, `id: "lid/ä"` | `ValidationError` quoting the pattern |
| `test_legacy_damping_lid_migrates_single_body` | the deprecated field maps into the new list | legacy `damping_lid` block, one body | one `damping_lid` surface, `DeprecationWarning` raised |
| `test_legacy_damping_lid_multibody_rejected` | the legacy field is ambiguous with >1 body | legacy block, two bodies | `ValueError` naming the ambiguity |
| `test_legacy_and_typed_mutually_exclusive` | both set is a contradiction | legacy block + `secondary_surfaces` | `ValueError` "not both" |
| `test_spec_without_secondary_surfaces_unchanged` | the field is additive | fixture specs with no surfaces | `secondary_surfaces == []`; `to_yaml` output byte-identical to the pre-change dump |

### `test_secondary_surfaces_orcawave.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_interior_surface_emits_mesh_keys_not_triangulation` | an interior mesh overrides the solver-generated triangulation | box + `interior_free_surface` | `BodyInteriorSurfaceMeshFileName == "box_interior.gdf"`, `BodyInteriorSurfacePanelMethod == "Defined by mesh file"`; the string `"Triangulation method"` is absent |
| `test_damping_lid_emits_lid_keys_and_epsilon` | lid emission is distinct from interior emission | box + `damping_lid`, `damping_factor: 0.2` | `DampingLidMeshFileName`, `DampingLidMeshFormat: Wamit gdf`, `DampingFactorEpsilon: 0.2`, `HasResonanceDampingLid: Yes` |
| `test_roles_are_distinguished_in_one_spec` | both roles on one body produce two disjoint key groups | box + interior + lid | interior keys on the body dict; lid keys in the lid section; no key appears in both |
| `test_unsupported_secondary_mesh_format_raises` | format mapping fails loud, as the control-surface map does | surface `mesh_file: "lid.xyz"` | `ValueError` naming the surface id and the supported format list |
| `test_emission_order_is_body_then_role_then_id` | the documented total order holds | 2 bodies x (interior + 2 lids) | emitted ids equal `sorted(key=(body_index, role_rank, id))` |
| `test_emission_order_invariant_under_input_shuffle` | order never depends on declaration order of the list | same six surfaces in 20 shuffled orders | one identical emitted-id sequence for all 20 |
| `test_generation_is_byte_identical_across_runs` | no dict/set/filesystem iteration leaks into output | same spec generated twice into two dirs | the two `.yml` files are byte-identical |
| `test_generation_stable_under_pythonhashseed` | hash randomisation cannot reorder output | subprocess generation at `PYTHONHASHSEED` in {0, 1, 42, "random"} | one identical SHA-256 across all four |
| `test_no_secondary_surfaces_output_unchanged` | existing golden emission does not move | `fixtures/acceptance_610/spec.yml` | generated `.yml` matches the pre-change golden byte-for-byte |
| `test_reverse_parser_round_trips_roles` | the epic's reverse-parsing clause is honoured | generated `.yml` re-read by `reverse_parsers` | each surface id, role and damping factor is recovered |

### `test_secondary_surfaces_aqwa_failclosed.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_aqwa_raises_on_damping_lid` | the current silent drop becomes a hard failure | box + `damping_lid`, `generate_single` | `UnsupportedSurfaceError` naming the surface id and role |
| `test_aqwa_raises_on_interior_surface` | same for the other role | box + `interior_free_surface` | `UnsupportedSurfaceError` naming id and role |
| `test_aqwa_raises_on_legacy_damping_lid` | legacy specs are not a bypass | legacy `damping_lid` block | `UnsupportedSurfaceError` |
| `test_aqwa_raises_on_control_surface_and_zone` | the pre-existing silent drops are covered too | spec with `control_surface` and `free_surface_zone` | `UnsupportedSurfaceError` listing both |
| `test_aqwa_error_lists_every_surface` | the message is complete, not first-failure | 3 unsupported surfaces | all 3 ids present in one message |
| `test_aqwa_error_message_order_is_deterministic` | the message uses the same total order | shuffled input, 20 repeats | one identical message for all 20 |
| `test_aqwa_writes_no_file_on_failure` | fail closed means nothing is emitted | unsupported spec, empty output dir | directory still empty after the raise |
| `test_aqwa_modular_path_also_fails_closed` | both entry points are guarded | `generate_modular` | `UnsupportedSurfaceError`; no files written |
| `test_aqwa_unaffected_without_secondary_surfaces` | no regression on supported specs | box, no surfaces | deck generated; content byte-identical to the pre-change deck |

### `test_secondary_surfaces_provenance.py`

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_packaging_copies_every_secondary_surface` | new slots reach `mesh_packaging` | box + interior + lid | all three `.gdf` files present in the package directory |
| `test_provenance_records_passthrough_copies` | the conversion-only gap is closed | three solver-ready `.gdf` inputs | `mesh_provenance.json` carries 3 records |
| `test_provenance_identifies_role_and_id` | every applied surface is identifiable | interior + lid | each record carries `surface_id`, `role`, `body` |
| `test_provenance_records_damping_parameter` | the damping parameter is preserved | lid, `damping_factor: 0.2` | lid record `damping_factor == 0.2`; interior record has none |
| `test_provenance_records_panel_counts` | panel counts are asserted, not assumed | 5-panel hull, 1-panel lid, 4-panel interior | `panel_count` equals the GDF header count for each |
| `test_provenance_panel_count_matches_parsed_geometry` | the count is a **conservation check**, not a copy of the header | fixture meshes | header panel count equals the count of panel records the mesh loader returns; a truncated fixture fails |
| `test_provenance_records_source_and_packaged_hashes` | content identity is captured | fixture meshes | `source_sha256` equals an independently computed digest; `packaged_sha256` equals it for an unconverted copy |
| `test_provenance_hash_changes_when_mesh_changes` | the hash is load-bearing, not decorative | one vertex perturbed | digest differs from the baseline |
| `test_provenance_records_solver_mapping` | the applied solver keys are traceable | interior + lid | each record names the OrcaWave keys it produced |
| `test_provenance_order_is_deterministic` | record order follows the documented key | shuffled input, 20 repeats | one identical record sequence for all 20 |
| `test_quality_gate_warns_not_blocks_for_open_lid` | the WARNING-never-blocks calibration is preserved | 1-panel lid patch (not watertight) | packaging succeeds; the warning names the lid; `MeshQualityError` is not raised |
| `test_quality_gate_still_blocks_structurally_invalid_gdf` | the calibration is preserved, not weakened | lid with a malformed GDF header | `MeshQualityError` raised, per `quality_gates.py:135-143` |

---

## Acceptance Criteria

- [ ] Schema validation rejects unknown roles, missing or unknown body association, unit mismatch,
      reference-frame mismatch, duplicate surface IDs, and invalid waterline placement — one test
      per rejection, each asserting the message names the offending surface id.
- [ ] OrcaWave generation distinguishes an interior irregular-frequency surface from a damping lid:
      disjoint key groups, verified on one spec carrying both.
- [ ] AQWA fails closed with `UnsupportedSurfaceError` on every secondary-surface kind it cannot
      map, raised before any file is written, with no output file left behind.
- [ ] Generated-artifact tests verify panel counts, role assignment, content hashes and deterministic
      emission ordering.
- [ ] **Determinism:** emission order is derived from the explicit total order
      `(body declaration index, frozen role rank, ASCII surface id)`. No ordering input is taken from
      `dict` iteration, `set` iteration, `Path.glob`, `os.listdir` or `hash()`. Proven by the
      shuffle-invariance test, the twice-generated byte-identity test, and the
      `PYTHONHASHSEED` subprocess test.
- [ ] Result provenance identifies every applied secondary surface (id, role, body, source path,
      packaged name, panel count, source and packaged SHA-256, units, frame, normal convention,
      waterline z) and every applied damping parameter.
- [ ] The `quality_gates.py` WARNING-never-blocks calibration is unchanged; a legitimately open lid
      packages at WARNING and a structurally invalid GDF still blocks.
- [ ] No regression: `PYTHONPATH=src uv run python -m pytest tests/hydrodynamics/diffraction/ -q`
      passes, with the existing golden and byte-identity suites
      (`test_orcawave_semantic_roundtrip.py`, `test_unit_box_benchmark.py`,
      `test_canonical_example.py`) green and unmodified.
- [ ] All fixtures are generic synthetic geometry. A grep for client, vessel and project identifiers
      over the diff returns empty, and `scripts/legal/legal-sanity-scan.sh` passes.
- [ ] Review artifacts posted to `scripts/review/results/`.

---

## Adversarial Review Summary

<!-- Filled in after the review pass completes. Verdicts pending. -->

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | *(pending)* | *(pending)* |
| Codex | *(pending)* | *(pending)* |
| Gemini | *(pending)* | *(pending)* |

**Overall result:** *(pending)*

Revisions made based on review:
- *(none yet — this plan has not been reviewed)*

---

## Risks and Open Questions

- **Risk — sibling contract defects and sequencing.** #1579 (inertia-tensor origin), #1580
  (restoring stiffness) and #1582 (matrix units) are the other three contract defects under epic
  [#1825](https://github.com/vamseeachanta/digitalmodel/issues/1825). **#1581 is the most
  independent of the four**: those three are *numerical* contract defects that change the meaning of
  values already flowing through `VesselInertia`, `external_stiffness`, `external_damping` and the
  result matrices, whereas #1581 is a *geometry* contract defect that adds a new declaration and
  touches no existing numeric field. It therefore does not need to wait on any sibling, and it
  should be sequenced first so the three numerical issues land against a settled geometry contract
  rather than racing it. The one shared file is `input_schemas.py`; the one shared risk is
  merge contention there, mitigated by keeping this change additive and confined to new models plus
  one new `DiffractionSpec` field.
- **Risk — AQWA fail-closed is a breaking change.** The reproduction shows an AQWA deck is written
  today for a spec carrying a lid. After this change that same spec raises. Any existing spec or
  benchmark fixture combining a lid with the AQWA path will start failing, which is the intended
  behaviour but shall be surfaced in the issue comment and the release note rather than discovered
  by a user. Mitigation: enumerate affected fixtures before implementation
  (`grep -rl damping_lid tests/ docs/`) and convert or annotate each.
- **Risk — golden and byte-identity suites.** `test_orcawave_semantic_roundtrip.py`,
  `test_unit_box_benchmark.py` and `test_canonical_example.py` pin generated output. Emission shall
  be strictly additive and gated on a non-empty `secondary_surfaces` list so that specs without
  surfaces produce byte-identical output; `test_no_secondary_surfaces_output_unchanged` is the
  guard. Per `.claude/rules/reproducibility-is-not-correctness.md`, those byte-match guards prove
  the generator deterministic and prove nothing about the emitted physics — which is why
  `test_provenance_panel_count_matches_parsed_geometry` asserts a conservation relationship (header
  count equals parsed panel count) rather than only a self-comparison.
- **Risk — watertightness calibration.** Every interior and lid patch is open by construction and
  will fail the watertightness check at `geometry_quality.py:378-383`, scoring WARNING. Tightening
  that check would reject all valid secondary surfaces. The calibration stays as
  `quality_gates.py:6-11` documents it, and two tests pin both sides of it.
- **Risk — reverse-parsing coverage.** `reverse_parsers.py` today maps only
  `BodyAddInteriorSurfacePanels` (lines 577-579) and no lid key, so the epic's "preserved through
  reverse parsing" clause is not met even for the existing lid. `test_reverse_parser_round_trips_roles`
  pulls that into scope. If the round trip proves larger than this issue, the residue shall be filed
  as a follow-on rather than silently dropped.
- **Risk — OrcaWave key names for a mesh-defined interior surface.** The emitted key names
  (`BodyInteriorSurfaceMeshFileName`, `BodyInteriorSurfacePanelMethod: "Defined by mesh file"`) are
  proposed by analogy with the control-surface keys at `orcawave_backend.py:281-299`. Per
  `.claude/rules/mechanism-before-publication.md` this is a **hypothesis until confirmed against a
  native OrcaWave exemplar**. Implementation shall first grep
  `docs/domains/orcawave/examples/` and the L00/L01/L03 native files for an
  interior-surface-by-mesh key and adopt the solver's own vocabulary; if no exemplar exists, the
  issue shall be escalated to the owner before inventing a key the solver never reads (the same
  defect class recorded at `orcawave_backend.py:583-591` for the invented `SolverPrecision` key).
- **Open:** should a single interior surface be permitted to span multiple bodies (a shared
  moonpool between two hulls)? This plan assumes one body per surface and rejects anything else.
  Flag for the approval decision.
- **Open:** should `panel_normal_convention` be validated against the mesh itself (by evaluating
  panel normals against the enclosed volume) or accepted as a declaration? This plan accepts it as a
  declaration and records it in provenance; empirical normal checking is a candidate follow-on.

---

## Complexity: T3

**T3** — systemic contract change. It modifies the canonical schema plus both solver backends plus
the shared packaging/provenance module (five source files), adds four test files and four fixtures,
introduces a persisted-artifact format change (`mesh_provenance.json` gains records and fields),
changes AQWA behaviour in a user-visible way, and carries an explicit determinism criterion that
must be proven across process boundaries. It sits under a cross-solver epic whose definition of done
requires the AQWA and OrcaWave backends to agree, so the three-provider adversarial review applies at
both the plan and the code stage.
