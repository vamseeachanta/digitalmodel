# Plan for #609: OrcaWave: normalize auxiliary mesh handling for control surfaces and QTF inputs

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/609
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-609-claude.md | scripts/review/results/2026-05-15-plan-609-codex.md

---

## Scope

Auxiliary mesh consistency scope. Body mesh packaging remains #605; conversion remains #606. This issue defines the schema/backend/runner contract for auxiliary assets. Backend/schema/accessor work can be implemented independently; packaging/preparer integration is blocked until #605/#606 provide `orcawave_asset_resolver.py` and `orcawave_mesh_preparer.py`.

## Resource Intelligence Summary

### Live issue state

Verified on 2026-05-15 via GitHub issue fetch:

- `#609` - OPEN - `OrcaWave: normalize auxiliary mesh handling for control surfaces and QTF inputs`; label: `enhancement`.
- `#500` - OPEN - `OrcaWave: mesh file pre-flight validation + auto-copy in runner`; label includes `status:plan-approved`.

### Sources consulted

- `AGENTS.md` - digitalmodel declares `PYTHONPATH=src uv run python -m pytest` as the repository test command and points source ownership at `src/digitalmodel/`.
- `docs/plans/` - repo has standalone plan files but no `docs/plans/README.md` index/template; issue #596 explicitly recorded "no `docs/plans/README.md` in this issue", so these plans follow the existing standalone-file convention.
- `src/digitalmodel/hydrodynamics/diffraction/cli.py` - current Click surface includes `convert-aqwa`, `convert-orcawave`, `compare`, `batch`, `convert-spec`, `validate-spec`, `run-orcawave`, `run-aqwa`, `batch-aqwa`, `batch-orcawave`, `plot-raos`, `mesh-build`, and benchmark commands; there is no given-mesh or doctor command yet.
- `src/digitalmodel/hydrodynamics/diffraction/spec_converter.py` - `SpecConverter.convert()` delegates directly to backends and `validate()` checks non-empty mesh strings, frequencies, headings, and positive mass only.
- `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` - runner can generate OrcaWave input, copy existing mesh files, prefer OrcFxAPI, and fall back to dry-run when no API/executable is available.
- `src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py` - existing pipeline can load/validate/prepare meshes and maps OrcaWave target format to GDF, but it is not integrated into `SpecConverter` or `OrcaWaveRunner`.
- Related issue `#500` - open plan-approved issue for mesh pre-flight validation and runner auto-copy; these future issues must not duplicate its direct scope.

### Issue-specific code findings

- `input_schemas.py` defines both `VesselSpec.control_surface` and `BodySpec.control_surface`.
- `orcawave_backend.py` currently reads `getattr(vessel, "control_surface", None)` when generating body control-surface YAML.
- `orcawave_runner.py` copies `getattr(body.vessel, "control_surface", None)` and does not inspect `BodySpec.control_surface`.
- `orcawave_backend.py` also generates damping lid and free-surface zone filename fields when present.
- Current field sites are distinct: top-level `spec.vessel.control_surface`, per-body nested `body.vessel.control_surface`, and per-body `body.control_surface`. The plan must enumerate all three to avoid ambiguous precedence.

### Gaps identified

- Body-level control-surface mesh filenames can be silently ignored in multi-body specs: the backend reads `BodySpec.control_surface` only for the quadratic-load control-surface flag but emits `BodyControlSurfaceMeshFileName` from `body.vessel.control_surface`.
- There is no documented precedence rule if both vessel-level and body-level control surfaces are set.
- Auxiliary meshes do not share a single resolver with body meshes.

### Evidence

Commands and inspections used while drafting:

```text
sed -n '1,220p' AGENTS.md
ls -la docs/plans
rg -n "Plan Index|Adversarial Review Summary|Review artifacts" docs/plans -S
sed -n '400,590p' src/digitalmodel/hydrodynamics/diffraction/cli.py
sed -n '1,180p' src/digitalmodel/hydrodynamics/diffraction/spec_converter.py
sed -n '220,560p' src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py
sed -n '1,280p' src/digitalmodel/hydrodynamics/diffraction/mesh_pipeline.py
```

Reproduction proof to capture before implementation: construct a multi-body `DiffractionSpec` with `body.control_surface.mesh_file = "body_cs.gdf"` and `body.vessel.control_surface.mesh_file = "vessel_cs.gdf"`, then run current OrcaWave backend generation. Expected current behavior is `BodyControlSurfaceMeshFileName: vessel_cs.gdf` while `body_cs.gdf` is ignored as a mesh filename. The first TDD test below must lock this failure before fixing it.

## Deliverable

Control surface, damping lid, and free-surface panelled-zone mesh references use one documented resolution/precedence rule across schema, backend, validation, and runner packaging.

## Proposed Tasks

1. Make the precedence rule explicit before implementation: for each body, `BodySpec.control_surface` overrides `body.vessel.control_surface`, which overrides top-level `spec.vessel.control_surface` only as a legacy/default fallback when the body has no nested control surface. If higher-priority and lower-priority fields reference different files, emit an `AuxiliaryMeshResolutionWarning` naming the selected path, shadowed path, body id/name, and field sources.
2. Add helper accessors for body auxiliary meshes and use them from backend, validation, and runner. The helper returns `AuxiliaryMeshResolution(asset, warnings)` with canonical asset id/type; #605/#606 integration consumes that shape once their resolver/preparer files exist.
3. Include damping lid and free-surface panelled-zone meshes in the same preflight/copy flow without changing #606 format-conversion policy.
4. Add QTF/free-surface checks only where schema metadata exists. `FreeSurfaceZoneSpec` currently lacks symmetry/global-coordinate metadata, so default behavior is an informational "metadata unavailable" finding, not an asserted warning about symmetry.
5. Document schema behavior, conflict warning behavior, and migration guidance from vessel-level defaults to body-level overrides for multi-body specs.
6. Add multi-body tests proving per-body auxiliary meshes are not ignored.

## Pseudocode

```text
resolve_control_surface(spec, body, body_index):
  candidates = [
    ("body.control_surface", body.control_surface),
    ("body.vessel.control_surface", body.vessel.control_surface),
    ("spec.vessel.control_surface", spec.vessel.control_surface),
  ]
  selected = first candidate with mesh_file
  warnings = shadowed candidate warnings where mesh_file differs from selected
  return AuxiliaryMeshResolution(asset_id=f"body_{i}_control_surface", asset=selected, warnings=warnings)

backend_body_yaml(body):
  cs = resolve_control_surface(spec, body, i)
  if cs.asset: emit BodyControlSurfaceMeshFileName = basename(cs.asset.mesh_file)
  propagate warnings to validation/manifest surfaces
```

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-05-15-issue-609-orcawave-auxiliary-mesh-handling.md` |
| Plan review - Claude (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-609-claude.md` |
| Plan review - Codex (repo-rooted at `/mnt/local-analysis/workspace-hub/digitalmodel`, non-empty completed artifact required) | `scripts/review/results/2026-05-15-plan-609-codex.md` |
| Plan review disagreement (optional if generated by fanout) | `scripts/review/results/2026-05-15-plan-609-disagreement.md` |
| Plan index | N/A - `digitalmodel/docs/plans/README.md` does not exist; follow existing standalone-plan convention |

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Modify | `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` | document/validate precedence if needed |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_backend.py` | generate YAML from canonical auxiliary accessor |
| Modify | `src/digitalmodel/hydrodynamics/diffraction/orcawave_runner.py` | package same auxiliary meshes |
| Modify/create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_asset_resolver.py` | shared resolver integration |
| Modify/create | `src/digitalmodel/hydrodynamics/diffraction/orcawave_mesh_preparer.py` | preparer integration only after #606 lands |
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_auxiliary_meshes.py` | multi-body and QTF tests |
| Modify | `docs/domains/orcawave/README.md` | schema precedence and migration guidance |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_body_level_control_surface_generates_yaml_value` | body-level field honored | multi-body spec with body and vessel control surfaces | `BodyControlSurfaceMeshFileName` equals basename of `body.control_surface.mesh_file` |
| `test_vessel_level_control_surface_applies_when_body_missing` | legacy/default behavior preserved | body without override, nested vessel-level control surface | emitted filename equals `body.vessel.control_surface.mesh_file` basename |
| `test_top_level_control_surface_is_legacy_fallback` | third field site explicit | body without body/nested control surface, top-level control surface set | emitted filename equals `spec.vessel.control_surface.mesh_file` basename and docs mark fallback |
| `test_control_surface_precedence_conflict_warns` | conflict not silent | body + nested vessel + top-level control surface with different files | body-level file emitted and `AuxiliaryMeshResolutionWarning` lists selected and shadowed paths |
| `test_runner_packages_auxiliary_meshes_after_605_606` | packaging parity | spec with aux meshes and resolver/preparer available | selected files copied/preflighted by shared resolver; skipped/deferred if dependencies absent |
| `test_free_surface_qtf_metadata_absent_reports_info` | QTF assumptions not guessed | free-surface mesh spec without symmetry metadata | informational metadata-unavailable finding, no fake symmetry warning |

## Acceptance Criteria

- [ ] Body-level control surface specs are not silently ignored.
- [ ] Vessel/body control-surface precedence is documented and tested.
- [ ] Backend YAML, runner copy logic, and validation use the same rules.
- [ ] Damping lid and free-surface mesh references are preflighted and packaged.
- [ ] Multi-body tests cover per-body auxiliary behavior.
- [ ] Packaging/preparer integration is explicitly gated on #605/#606; backend/schema/accessor behavior can land independently.
## Plan Review Gating

- [ ] Completed review artifacts under `/mnt/local-analysis/workspace-hub/digitalmodel/scripts/review/results/` exist for at least two providers and each non-empty artifact contains a `## Verdict` section; 0-byte in-progress files do not satisfy this gate.
- [ ] Any provider `MAJOR` finding requires a plan revision and re-review; the issue is commented with this plan and moved to `status:plan-review` only after no unresolved `MAJOR` findings remain.

## Adversarial Review Summary

| Provider | Verdict | Key findings |
|---|---|---|
| Claude | PENDING | Awaiting review artifact |
| Codex | PENDING | Awaiting review artifact |

**Overall result:** PENDING - do not label `status:plan-review` until artifacts exist and no unresolved `MAJOR` findings remain.

## Risks and Open Questions

- **Risk:** Auxiliary asset precedence must be applied before backend YAML and before package/run path resolution. If backend and runner call different accessors, multi-body cases can still diverge.
- **Risk:** #605/#606 own resolver/preparer mechanics and #500 owns existing runner preflight/copy behavior. #609 should add asset discovery/precedence first, then delegate packaging/conversion after those dependencies land.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** T2 complexity requires two-provider review; Claude + Codex are the selected review pair for this plan.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
