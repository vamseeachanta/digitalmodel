# Plan for #609: OrcaWave: normalize auxiliary mesh handling for control surfaces and QTF inputs

> **Status:** draft - awaiting adversarial review
> **Complexity:** T2
> **Date:** 2026-05-15
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/609
> **Review artifacts (target paths; created by completed fanout, not pre-existing evidence):** scripts/review/results/2026-05-15-plan-609-claude.md | scripts/review/results/2026-05-15-plan-609-codex.md

---

## Scope

Auxiliary mesh consistency scope. Body mesh packaging remains #605; conversion remains #606. This issue defines the schema/backend/runner contract for auxiliary assets, then routes those assets through the shared resolver/preparer from #605/#606.

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

### Gaps identified

- Body-level control surfaces can be silently ignored in multi-body specs.
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

Reproduction proofs: N/A - this is a future-work enhancement issue, not an alleged runtime regression or failing-test report. Implementation must still start with the TDD tests below.

## Deliverable

Control surface, damping lid, and free-surface panelled-zone mesh references use one documented resolution/precedence rule across schema, backend, validation, and runner packaging.

## Proposed Tasks

1. Make the precedence rule explicit before implementation: for a given `BodySpec`, `BodySpec.control_surface` overrides `BodySpec.vessel.control_surface`; if both are set and reference different files, emit a structured warning naming both paths. A vessel-level control surface remains the default for bodies that do not define a body-level override.
2. Add helper accessors for body auxiliary meshes and use them from backend, packager, validation, and runner. The helper must return the canonical asset id/type consumed by `orcawave_asset_resolver.py` and `orcawave_mesh_preparer.py`.
3. Include damping lid and free-surface panelled-zone meshes in the same preflight/copy flow without changing #606 format-conversion policy.
4. Add QTF/free-surface checks for symmetry/global-coordinate assumptions only where schema metadata exists; otherwise report an informational finding rather than guessing.
5. Document schema behavior, conflict warning behavior, and migration guidance from vessel-level defaults to body-level overrides for multi-body specs.
6. Add multi-body tests proving per-body auxiliary meshes are not ignored.

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
| Create/modify | `tests/hydrodynamics/diffraction/test_orcawave_auxiliary_meshes.py` | multi-body and QTF tests |

## TDD Test List

| Test name | What it verifies | Expected input | Expected output |
|---|---|---|---|
| `test_body_level_control_surface_generates_yaml` | body-level field honored | multi-body spec | `BodyControlSurfaceMeshFileName` present |
| `test_vessel_level_control_surface_applies_when_body_missing` | legacy/default behavior preserved | body without override, vessel-level control surface | `BodyControlSurfaceMeshFileName` present from vessel-level field |
| `test_control_surface_precedence_conflict_warns` | conflict not silent | vessel + body control surface with different files | body-level file emitted and warning lists both paths |
| `test_runner_packages_auxiliary_meshes` | packaging parity | spec with aux meshes | files copied/preflighted |
| `test_free_surface_qtf_mesh_checks` | QTF assumptions reported | free-surface mesh spec | symmetry/global-coordinate finding |

## Acceptance Criteria

- [ ] Body-level control surface specs are not silently ignored.
- [ ] Vessel/body control-surface precedence is documented and tested.
- [ ] Backend YAML, runner copy logic, and validation use the same rules.
- [ ] Damping lid and free-surface mesh references are preflighted and packaged.
- [ ] Multi-body tests cover per-body auxiliary behavior.
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
- **Risk:** #605/#606 own resolver/preparer mechanics; #609 should add asset discovery/precedence only, then delegate packaging/conversion.
- **Risk:** Licensed OrcaWave/OrcFxAPI behavior cannot be fully verified on Linux; tests requiring a license must skip cleanly and be proven on the licensed host where applicable.
- **Open:** Gemini was unavailable in this environment; use Claude + Codex as the required two-provider review set for plan-review.

## Complexity: T2

T2 justification: T2 standard multi-file change with CLI/module/test/docs impact.
