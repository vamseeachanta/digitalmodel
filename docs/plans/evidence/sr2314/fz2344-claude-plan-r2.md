VERDICT: MAJOR

The plan's core design is sound: default EN400-only, `cite="strict"` for the dual-reference path, and no catch-all fallback. Three defects block approval.

1. **The cite-mode validation rule is underspecified and conflicts with its own test.** (`fz2344-plan.md:7`, `test_transfer_citation_regressions.py:179`)
   - The plan says the domain is "False, True or 'strict'". In Python `1 == True` and `0 == False`, so an implementation written as `cite in (False, True, "strict")` accepts `1`. The proposed test rejects `1`.
   - The plan must require identity checks: `cite is True`, `cite is False`, or `cite == "strict"` where `cite` is a `str`.
   - Today `friction_scaling.py:243,250,293,295` treat `cite` by truthiness and `== "strict"`. Nothing validates it before `_common_inputs` branches on it. `"Strict"`, `"false"`, `"off"` and `1` currently pass silently, and `"false"` and `"off"` are truthy, so they emit citations. `None` is falsy and behaves as an opt-out.
   - Validation must run first, before any branch on `cite`. The plan does not say so.

2. **Rejecting `1`, `None` and numpy bools contradicts the owner's "main-compatible defaults" requirement.** (`fz2344-plan.md:3`, `:7`)
   - Main evidently accepted any truthy or falsy `cite` value. Raising `ValueError` for `1`, `0`, `None` and `np.True_` is a behaviour break outside the stated opt-in scope.
   - The plan does not justify it against the owner constraint or document it as an intentional change.
   - Either restrict the new `ValueError` to unknown strings and keep bool-like values main-compatible, or record an explicit owner exception. The r1 disposition ("accepted") does not address this.

3. **The scope is inconsistent, and part of it is not reviewable.**
   - `fz2344-plan.md:4` says "four source/test files". `:8` lists `friction_scaling.py`, `mesh_hydrostatics.py` and three test files, which is five.
   - The `mesh_hydrostatics.py` / `test_mesh_r02.py` "Mapping optional identity fix" is unrelated to citations. It has no description, no failing-test sketch, and neither file is in the packet.
   - Without that detail or code, reviewers cannot assess the change, and it widens a fix-up PR whose draft state and main parity are the owner's priorities.
   - Either furnish the defect, test and diff in a frozen packet, or drop it from this PR.

Smaller gaps, not blocking on their own:
- **Strict/standalone warning.** The plan says strict mode raises "without a fallback warning", but `test_transfer_unconfigured_resolver_fails_closed` never asserts that no `RuntimeWarning` is emitted.
- **Default mode with both pages present.** The plan promises unresolved ITTC metadata is retained when both pages exist. `test_transfer_emits_citation_sidecar_by_default` asserts only `len(citations) == 1` and does not check `unresolved_citations`. The only default-mode test of unresolved metadata uses a missing-procedure-page fixture.
- **Main-tip parity probe.** It is described only as a local probe. The plan should name where its output is recorded under `docs/plans/evidence/sr2314/fz2344*`, and it should compare the standalone warning and the empty-sidecar case, not only the numeric fields.
