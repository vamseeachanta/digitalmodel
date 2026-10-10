# Artifact r2 review — sr2314

**Verdict: CHANGES-REQUIRED.** No numerical defect is proven in the production code. The blocking findings are in the evidence records and tests: two MAJOR, six MEDIUM, six MINOR. This is a read-only review of the frozen packet; nothing was executed, and packet integrity (`verify`) remains the orchestrator's step.

## Proven numerical defects

None. Tracing `mesh_sections.py:50-66` shows that compaction should be bit-identical to the legacy path:
- `np.unique` gives an order-preserving index map, so the `(i, j)` roles in the cut cache at `mesh_clipping.py:67-72` are unchanged.
- The fan-centre summation order is unchanged.
- Any face sharing an on-plane edge is a candidate, because selection and snapping use the same `eps`.

Recorded values check arithmetically:
- **RSS:** 879000 × 1024 = 900096000 bytes.
- **Holtrop decomposition:** C<sub>T</sub> equals viscous + wave + C<sub>A</sub> for both hulls.
- **Volume error:** 5.0e-6 matches the trapezoid estimate for nx=1000, nz=250 (about 1e-6 + 4e-6).

## MAJOR

**M1. The million-face evidence does not exercise or compare the changed path (finding 9, and finding 7 at scale).**
- **Stations sit on vertex columns.** `benchmark_mesh_hydrostatics.py:108` uses −45…45 in steps of 10 with dx = 0.1 m, and midship is at 0. No face is mixed, so `_clip_mixed` never runs on compacted indices.
- **Draft sits on a mesh row.** Draft 6.0 (`:96`, `:113`) gives zero cut triangles. `generated_physical_area_m2.count` = 1,000,000 = 4·nx·nz, and its minimum equals the source minimum to 17 digits (`mesh-benchmark-current.json:38-45`). The "generated" statistics describe unmodified source faces.
- **The cache is never hit.** `section_evaluation_count` = `unique_section_stations` = 11 in both implementations.
- **Section outputs are not recorded.** Only V and S are kept (`:125-126`); neither passes through `SectionIndex`. `compare_records` (`:43-55`) compares no outputs at all.
- **The absolute bounds are loose.** They sit 20× (volume) and about 1000× (area) above the observed errors, so they cannot detect a regression.

Correction:
1. Add at least one off-grid station set (for example −44.95…45.05) and an off-row draft with the closed-form Wigley partial-draft volume as reference.
2. Record A_M against the analytic 2BT/3 = 40 m², plus C_M, C_P and a digest of `result.to_dict()` quantities.
3. Require output equality between baseline and current in `compare_records`.
4. Either add a repeated station or state that caching is evidenced only by `test_mesh_r02.py:90`.

**M2. `generator-isolation.json` is non-discriminating.**
- All six hashes are identical, and the record names no probe, hostile binding value, command, decoy path or timestamp.
- "Probe passed" cannot be distinguished from "probe did nothing".
- No file in the packet references it, and no test exercises `generate_mesh_legacy_pickles.py`.

Correction:
1. Add a disposable-repository test that sets `GIT_DIR` and `GIT_WORK_TREE` to a decoy.
2. Record the decoy's before/after hashes, the binding values and the single enumerated output.
3. Rename the fields to say what was tampered.

## MEDIUM

**D1. The disclosure verdict precedes its object.** `validation.html:23` records "verified … conditional on its recorded digests", while the same paragraph says the manifest "will identify" the bundle. No manifest or receipt is in the packet (`:11` also cites one). Correction: produce the manifest, then issue the verdict against its digests; until then state "not yet verified".

**D2. No single environment covers the evidence.**
- The complete suite (970 passed) ran on `numpy<2.4` (`validation.html:15`).
- The fixture and benchmark are pinned to NumPy 2.4.4 (`:17`).
- The focused suite is unpinned (`:13`).

Correction:
1. Record resolved versions per run.
2. Run the focused mesh, pickle and citation tests on 2.4.4 and on the `<2.4` environment.
3. Name the `trapz` consumer that fails on 2.4 and link a follow-on issue.

**D3. `compare_records` input binding is incomplete.** `:44` omits the following, although `mesh-performance.html:2` claims "no … input … mismatches":
- the `hull_fixtures.py` digest;
- implementation roles (a self-comparison qualifies);
- the SciPy version, which drives the reference quadrature;
- the benchmark script digest;
- digests of the two compared records.

Correction: add those fields and reject equal `implementation_sha256`.

**D4. Comparison-criteria provenance is not established.**
- `sr2314-comparison-v1` is absent from the plan (`2026-10-10-sr2314-r02.html:28` lists absolute criteria only).
- Every criteria-bearing file is untracked, so "precedes the revised run" (`review-disposition.md:19`) has no commit evidence.
- `mesh-performance.html:8` confirms earlier exploratory runs existed.

Correction: either describe comparison-v1 as a no-regression guard set after exploratory measurement, or commit the criteria and re-run, citing the commit.

**D5. Fine-mesh section equivalence is tested only on vertex columns.**
- `test_mesh_r02.py:185` uses ±50, ±25 and 0 on a 100/60 m grid.
- `:127` uses x = 20 on a 1.25 m grid.
- Only the 12-face boxes reach mixed faces.
- At the 1e6 m offset the `eps/2` probes (about 5e-11) are below the coordinate ulp (about 1.2e-10), so they do not probe near-vertex behaviour.

Correction: add off-grid relatives (for example −37.3 and 11.1) to the Wigley case and to the compaction test.

**D6. Facade public surface is not bound to the baseline.** `test_mesh_r02.py:201-204` checks 11 names by `callable`. Equivalence with the public names at 4f7bfc0c (including whether `__all__` narrows `import *`) cannot be established from the packet. Correction: assert a literal baseline name list, or load the already hash-pinned baseline source and compare.

## MINOR

- **Limit operators.** Tests use `<= 400` and `<= 50` (`test_mesh_r02.py:197,200`; `test_transfer_citation_regressions.py:81,84`), while the plan and validation record state `<400` and `<50`. Align one to the other.
- **Cap-exclusion test.** `test_mesh_benchmark.py:21` passes even if caps were counted (3280 < 3598). Assert `count == 4*nx*nz`.
- **Holtrop operating range.** `holtrop-reproduction.html:15` reports Fn 0.25 and 0.30 for a C<sub>B</sub> 0.80 form without a range qualifier. Mark those rows as outside the commonly cited full-form range, pending the primary paper.
- **Stale count.** `holtrop-reproduction.html:20` states "22 passed" alongside the later 3-test statement.
- **Generator pins.** The generator pins NumPy (`:32`) but not Python or the `hull_fixtures.py` digest that supplies `box_mesh` (`:42`).
- **Caller inventory and RSS scope.** The caller inventory covers `src` and `scripts` only (`review-disposition.md:41`); extend it to `docs`, `examples` and all of `tests`. Peak RSS is a process high-water mark that includes the Python-list mesh generator, so record `ru_maxrss` before and after the timed region.

## Unsupported expectations (not defects)

- **Constructor defaults.** `TransferResult` defaults (`friction_scaling.py:181-182`) are compatibility-preserved as instructed.
- **Production wiki page.** Default `cite=True` calls raise until the owner provisions the page; this is disclosed and tracked on [#2239](https://github.com/vamseeachanta/digitalmodel/issues/2239). The draft PR body should carry it as a merge-order condition.
- **Collapse guard.** It does not establish pre-rounding collinearity; this is disclosed.
- **Speedup.** The 0.964 time ratio is single-run parity, and no speedup is claimed.
- **Not verifiable from the packet:**
  - the ITTC media URL;
  - the `holtrop_mennen.py` and `holtrop_coefficients.py` digests;
  - the baseline source bytes;
  - whether the Holtrop command's dependency closure is complete with `--with pyyaml` alone;
  - the raw plan-review and r1 verdict artifacts.

`★ Insight ─────────────────────────────────────`
- A benchmark on a structured mesh can align its cut planes with vertex rows by accident. The signature here is `generated count == 4·nx·nz` and identical minimum areas; that equality is a cheap check for vacuous clipping evidence.
- A negative-probe record needs one hash that is expected to differ, or a named decoy expected not to. Six equal hashes carry no information.
`─────────────────────────────────────────────────`
