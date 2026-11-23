**Verdict: MINOR** (non-approve). No numerical or API-compatibility defect is found in the packet; nine findings concern unflagged assumptions, source qualification and report wording. Nothing was executed, per the packet instruction — the arithmetic below is hand-checked and the tests were not run.

## Findings

| # | Sev | Location | Finding |
|---|-----|----------|---------|
| F1 | MINOR (fix before merge) | `pipeline_defect_screen.py:108`, `:287-304`, `:334-338` | Metal loss in the first or last axial row raises no flag or note. |
| F2 | MINOR | `rstreng_2d.py:84-87` | New `applicability` property docstring is false for the area-weighted projection. |
| F3 | MINOR | `pipeline_defect_screen.py:209-213` | DERATE criterion says "Demand exceeds capacity" but the comparison is against factored allowables. |
| F4 | MINOR | `ffs_offering_catalog.yml` and `capabilities-added.yml` diff | Module-level `validated → live` promotion covers more than the workflow exercises. |
| F5 | MINOR | `pipeline_defect_screen.py:74-75` | Missing `grid` or `axial_positions_in` raises `KeyError`, not `ValueError`. |
| F6 | MINOR | `pipeline_defect_screen.py:190-193`, `:315`, `ffs_decision.py:401-403` | Shared-engine text still reaches the report through `str.replace` and an unguarded header. |
| F7 | MINOR | `pipeline_defect_screen.py:252-255`, `:148`, `:167` | Three report presentation defects (status wording, duplicate flags, number format). |
| F8 | MINOR | `pipeline_defect_screen.py:299`, `input.yml:19-20` | "DNV usage factor" label implies DNV provenance for an ASME B31.8 value. |
| F9 | NIT | `circumferential_defect.py:56-57`, `:291-295`; `pipeline_defect_screen.py:316`; `synthetic.yml` | Formatter drift, date-dependent report, unregistered second example. |

**F1 — truncated window.**
- `length = positions[-1] - positions[0]` treats the grid window as the defect length, and all five pressure methods depend on it.
- The README (lines 20-23) states the caller's window bounds the defect; the HTML report does not carry that assumption.
- The shipped `input.yml` has 0.150 in loss at both edge rows and governs at RSTRENG allowable/demand ≈ 1.03 (1147 psi / 1.39 against 800 psi).
- If the same loss extended to L = 12 in, RSTRENG would give about 1078 psi, a ratio of about 0.97, and the verdict would change from ACCEPT to DERATE.
- Recommended: emit a note in the result and report when either boundary row has nonzero loss, and state the window assumption in the report body. A note is sufficient; an ESCALATE flag would break the B31G reference example.

**F2 — area-weighted applicability.**
- The docstring says "Shared validity record from the underlying maximum-depth profile".
- For `projection="area_weighted"`, `result.applicability` is computed from `max(d_eff)`, the circumferentially averaged depth.
- A 0.95t pit averaged to 0.50t returns `ok=True`, so a caller passing it to `decide()` gets no ESCALATE.
- The adapter uses MAX only, so the workflow is unaffected; the defect is on the new typed surface.
- Recommended: compute the d/t check from `grid.max()/t` for both projections, or correct the docstring and add a test for the area-weighted case.

**F3 — "capacity" wording.**
- The limits quoted are failure pressure divided by the safety factor, capacity times the usage factor, and SMYS-based stress times the axial design factor.
- A demand can exceed the allowable while remaining below capacity.
- Recommended wording: "Demand exceeds allowable".

**F4 — catalog status.**
- The workflow calls `dnv_f101_single_defect` (allowable-stress format) only; `dnv_f101_psf` and `dnv_f101_interacting` are not exercised.
- The workflow uses the MAX projection only; `allowable_flaw_length` and the area-weighted projection are not exercised.
- Recommended: add a `note:` or `caveat:` naming the exercised entry points, as the `dnv-f101` row already does for #1094.

**F5 — error type.**
- `test_missing_or_mistyped_fields_are_explicit` covers only `component_id` and `smys_psi`.
- String cells such as `"0.3"` are silently coerced by `np.asarray(..., dtype=float)`.

**F6 — brittle text reuse.**
- The R2 disposition claims brittle replacement was eliminated; that holds for the footer only.
- The criterion text is still rewritten with `.replace("RSFa", …)` and `.replace("RSF", …)`.
- The ESCALATE string passes through "a Level 3 / engineering review is required" in a report that disclaims Part 5 qualification.
- `FFSReport._html_head` emits `<h1>Fitness-for-Service Assessment Report</h1>` with no structure guard equivalent to the footer's.

**F7 — report presentation.**
- A failed extent screen renders "WITHIN SCREEN LIMITS; Level-1 extent screen failed; membrane check performed", which contradicts itself.
- `collect` yields the same `B31G_DT_GT_0.80` flag four times (B31G, Modified B31G, RSTRENG, RSTRENG-2D), so the footer lists four identical items.
- The circumferential `demand_psi` is not cast to `float`, so a YAML integer prints as `100000` beside `800.000`.

**F8 — factor label.**
- The example value 0.72 is the ASME B31.8 class-1 design factor, as `dnv_rp_f101.py:94-104` states.
- Recommended label: "Usage factor applied to DNV-RP-F101 capacity (caller supplied)".

**F9 — nits.**
- The `applicability` import sits above the stdlib imports, and the `flagged(...)` call uses a hanging indent; `.claude/rules/formatter-scope.md` requires the unchanged CI lint target to be confirmed.
- The report date is `datetime.now(timezone.utc)`, so the HTML changes daily if `results/` artifacts are committed.
- `synthetic.yml` has no registry entry and no engine-level test.

## Checked and found correct

- **Reference anchors:** B31G 1182.5 psi, Modified B31G 1219 psi and DNV-RP-F101 1334 psi reproduce by hand for D = 30, t = 0.375, d = 0.150, L = 8.
- **Synthetic anchor:** 1969.2 psi reproduces, governed by segment (1, 4) with A/A0 = 0.433 and M = 1.689.
- **Decision bands:** `DecisionBands(0, 0, 0)` with infinite life reaches only ACCEPT, DERATE or ESCALATE. No flag code or note contains "RSF", so the substitution cannot corrupt flag text today.
- **Dataclass compatibility:** `NetSectionCircumferentialResult.applicability` is appended after the defaulted fields, and `RiverBottom2DResult.applicability` is a property, so positional construction is preserved.
- **Line 253 precedence:** `status += ": " + … if notes else ""` parses as intended.
- **Serialisation and escaping:** the through-wall and intact cases are JSON-safe with `allow_nan=False`. The component id, provenance, criterion and status strings are HTML-escaped.

`★ Insight ─────────────────────────────────────`
- An applicability layer catches only what the method knows about, here d/t. Whether the grid captured the whole defect is a property of the measurement window, so the adapter is the only place that can raise it (F1).
- A pass-through property inherits the semantics of whatever was fed to the inner call. `result.applicability` is correct for MAX because `d_eff` is the true maximum, and wrong for area-weighted because the inner engine never sees the raw grid (F2).
- The footer's fail-closed `partition` guard is the stronger pattern: it breaks loudly when the upstream structure changes. The `str.replace` on criterion text fails silently if `ffs_decision` rewords a sentence (F6).
`─────────────────────────────────────────────────`

## Gate status

- The verdict binds to the packet hashes, including `pipeline_defect_screen.py` at `b9a6c2dc…`. The working tree is uncommitted on HEAD `4f7bfc0c`, so any edit voids the verdict; `build-review-packet.py verify` should run before the verdict is acted on.
- The five shared files are pinned by hash but shown only as a diff, so rows outside the diff were not reviewed.
- A re-review after fixes would cover F1–F3 plus the report-text tests they touch. F4–F9 can be closed by inline patch.
- The Gemini and Codex code-stage reviews and the owner merge gate remain open. This review does not substitute for them.
