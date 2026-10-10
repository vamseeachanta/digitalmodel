# Artifact review: sr2314 R02 — verdict CHANGES-REQUIRED

**Basis:** static reading of the frozen packet only (HEAD `5bd2cc99`). Nothing was executed and no file outside the packet was inspected. Findings that depend on files outside the packet are marked *unverified*.

**Summary:** no numerical defect was found in the indexed-section, compaction or cache logic. The blocking defects are in evidence, packet completeness and the citation default.

## MAJOR

**M1. Scale evidence for finding 9 is absent from the packet.**
- `docs/plans/2026-10-10-sr2314-r02.html:28` requires measured values and exact commands retained under `docs/plans/evidence/sr2314`.
- The only evidence file in the packet is `review-disposition.md`. Line 19 asserts both runs exist but gives no wall time, RSS, error values or command lines; line 22 defers final checks to the future ("will be retained").
- The v1 criteria (≤1e-4 volume, ≤5e-4 area, <120 s, <2 GiB) and the ratio criteria (≤1.10 time, ≤1.05 RSS) are therefore not established. The "1,009,998-face" count is also unverifiable.
- Correction: commit the two JSON records from `run_case` (current and legacy) with their commands and the full-suite counts, and include them in the next packet.

**M2. The packet omits a pickle fixture that the tests deserialize.**
- `tests/naval_architecture/test_mesh_r02.py:51-55` calls `pickle.loads` on base64 blobs from `mesh_legacy_pickles.json`.
- That file is untracked in git status and is not in the packet, so the verdict cannot bind to its bytes. Unpickling is code execution on every test run.
- Correction: include the file in the packet, pin its SHA-256 in the test before `pickle.loads`, and commit the generator command for revision `4f7bfc0c`.

**M3. The `cite=True` default is known to fail in production, and the disclosure does not reach the API surface.**
- `review-disposition.md:20` states the production wiki target is absent. Every default call to `transfer_model_to_ship` and `transfer_ship_to_model` (`friction_scaling.py:251`, `:290`) will therefore raise until the page is provisioned.
- The transfer docstrings (`friction_scaling.py:270-272`) describe the failure mode but not that the dependency is currently unprovisioned. `registry.py:180` says only "must be provisioned by its owner".
- No caller inventory and no tracking issue for provisioning are in the packet.
- Correction: enumerate in-repo callers of both functions and show each passes `repo_root` or `cite=False` or is expected to fail. State the unprovisioned dependency in both docstrings. Link an owner issue for the wiki page.

## MEDIUM

**D1. `friction_scaling.__all__` is incomplete** (`friction_scaling.py:37-40`).
- It omits `Fluid`, `reynolds_number_si`, `froude_number`, `ITTC_TRANSFER_PROCEDURE` and `ITTC_TRANSFER_UNRESOLVED`. `Fluid` is imported by the module's own tests.
- If the prior module had no `__all__` (*unverified*), `from friction_scaling import *` silently loses these names.
- Correction: add them, move `__all__` below the import at line 41, and assert every prior public name in a test.

**D2. `TransferResult` defaults contradict each other** (`friction_scaling.py:177-179`).
- A default-constructed result has `cited=True`, `citations=[]` and an unresolved record whose note says "caller passed cite=False".
- `UnresolvedCitation.status` remains `"unresolved-in-registry"` (line 147), although the registry now resolves this reference; the actual cause is opt-out.
- Correction: default `unresolved_citations=()`, or validate consistency in `__post_init__`.

**D3. The plan's truth table names `CitationConfigError`, but nothing raises or tests it** (`2026-10-10-sr2314-r02.html:29`).
- `test_transfer_citation_regressions.py:50-58` covers only `CitationResolutionError`.
- That test patches `resolver.resolve_wiki_path` on the module. If `validate_citation` binds the name at import, the patch has no effect and the outcome depends on the ambient `LLM_WIKI_PATH` (*unverified*; `schema` and `resolver` are outside the packet).
- No frontmatter-mismatch case exists for the ITTC page.
- Correction: patch the symbol where it is looked up, clear the environment variable with `monkeypatch.delenv`, add a revision-mismatch case, and reconcile the plan wording.

**D4. The benchmark verdict does not read its own criteria** (`benchmark_mesh_hydrostatics.py:83-84`).
- `qualified` uses literals instead of `CRITERIA` (lines 23-25), so the emitted criteria and the applied criteria can drift.
- The ratio criteria from `review-disposition.md:19` appear in no script.
- `station_count` is hardcoded to 10 (line 75) and `test_mesh_benchmark.py:17` asserts that constant, which is tautological. Eleven distinct sections are actually evaluated, since midship 0 is not in `linspace(-45, 45, 10)`.
- Correction: derive `qualified` from `CRITERIA`, add a compare mode that takes both records and applies the ratio criteria, and report the station count from the section cache.

**D5. `load_implementation` may not be able to load the baseline** (`benchmark_mesh_hydrostatics.py:28-35`).
- The legacy file is loaded as a top-level module with no package. If the `4f7bfc0c` source uses `from .hull_fixtures import ...`, the import fails (*unverified*).
- The baseline is identified only by file hash, not by revision or extraction command.
- Correction: load it with a package parent, or run the baseline from a `git worktree` at `4f7bfc0c`, and record the revision.

**D6. The index-equivalence test barely exercises selection** (`test_mesh_r02.py:141-159`).
- The box mesh has about 12 faces whose side faces span the full x-range, so nearly every face is a candidate at every station.
- The comparator `_full_body_section` shares `_clip_keep_below` and `_cap_area_vector` with the code under test, so it checks selection and compaction only (disclosed in the disposition).
- Sorting rows and then comparing with `approx` (lines 155-156) can reorder on last-bit noise and is sensitive to segment direction.
- Correction: run the comparison on a trimmed fine Wigley mesh and a twin-body mesh, including the midship station where every cap-fan triangle is a candidate. Compare segments as order-insensitive sets.

**D7. `_check_physical_faces` is described as more than it is** (`mesh_clipping.py:177-194`).
- The floor is `eps × L²`, but cross-product rounding scales with coordinate magnitude (about `eps × |x| × L`). At a 1e6 m offset, noise-dominated triangles pass.
- `test_small_resolved_cut_is_translation_invariant` (`test_mesh_r02.py:63-69`) uses an axis-aligned box with exactly representable coordinates and no trim, so it does not probe this.
- The `repeated` branch cannot fire for generated faces, because `_clip_mixed` already drops them at `mesh_clipping.py:80`. That is a silent deletion inside a refuse-not-repair contract.
- Correction: rename it a computed-collapse guard in the plan and docstring. Add a trimmed, offset, non-representable case. Either refuse at line 80 or document the drop.

**D8. The Holtrop comparison label is positional** (`reproduce_holtrop_discrepancy.py:63`).
- `tanker_relative_to_series60_pct` is computed from `rows[1]` and `rows[0]`; a reordered or extended fixture mislabels it silently.
- `test_holtrop_reproduction.py:22,24,34` hardcodes two cases and 7.72 m/s.
- The report JSON does not name the C_T normalization (Holtrop-regression wetted surface); it appears only in a docstring, although plan line 30 requires the denominator named.
- No measured output is retained (see M1).
- Correction: select cases by `id`, and add a `ct_normalization` field.

## MINOR

- **`_common_inputs` ordering** (`friction_scaling.py:234-239`): citations resolve before allowances are validated, so a non-finite `ca_model` surfaces as a citation error. Validate inputs first.
- **`ca_model` term**: the C_A,m term is not part of the cited procedure sections, yet the ITTC citation is attached when it is declared. Flag this in `assumptions`.
- **Duplicated EN400 section string** (`friction_scaling.py:325`): it is retyped rather than shared with `resistance.ittc_1957_cf_cited`. `ittc57_cf_cited` keeps the legacy warning fallback, so the module has two different citation behaviours.
- **`TriMesh.bounds`** (`mesh_validation.py:74-76`): it reruns `np.unique` and a used-vertex copy on each of three calls per calculation. Cache it at construction.
- **Vacuous guard** (`test_transfer_citation_regressions.py:37`): `if target.exists()` lets the test pass if the fixture is missing. Assert existence.
- **Exact float equality** (`test_mesh_r02.py:104`): `== 6` on a fan-summed area is brittle.
- **Platform**: `import resource` (`benchmark_mesh_hydrostatics.py:14`) fails at import on Windows and takes the test with it.
- **Cohesion**: `_check_station` is input validation placed in `mesh_results.py:96`.
- **Facade re-exports** (*unverified*): the facade no longer re-exports `_section`, `_clip_keep_below` or `_read_only`. Confirm no existing test or caller reaches them through `mesh_hydrostatics`.

## Checked with no defect found

- **Compaction** (`mesh_sections.py:53-56`): an edge lying on the station plane has both neighbours in the candidate set, so the boundary matches the full-body cut after the `on` filter.
- **Cache identity**: the key is the exact float, and the keep side depends only on station and the fixed midship.
- **Immutability**: cached segments are bytes-backed and cannot be made writeable.
- **Call wiring**: `_base_quantities` and `_feature_quantities` argument order matches their signatures.
- **Pickle module identity**: the `__module__` reassignments resolve through the facade.

`★ Insight ─────────────────────────────────────`
- The compaction is correct because of a locality property: any edge on the plane x = s belongs to two faces that both touch s, so both are candidates. A 12-face box cannot falsify that property, which is why D6 matters.
- Fail-closed citation validation against a fixture written in the same change proves string equality only. Whether the production page exists is a separate fact, and the packet establishes that it does not.
`─────────────────────────────────────────────────`

**Next checkpoint:** resolve M1–M3 and D1–D5, rebuild the packet including the pickle fixture and benchmark records, and re-review the delta. Any changed byte voids this verdict.
