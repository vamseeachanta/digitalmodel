# Adversarial review: R02 follow-up plan (sr2314), revised

**Verdict: CHANGES-REQUIRED.** Three MAJOR defects remain, and one further MAJOR depends on evidence the packet does not carry.

The review covers only the packet bytes (sha256 `aef05c0e…cf54`, HEAD `4f7bfc0c`). No repository file was read in this session, so findings that depend on existing code are marked "not established".

## Disposition of prior major issues

| Prior issue | Status | Basis |
| --- | --- | --- |
| Source vs clip slivers | Not closed | The replacement floor is below the rounding noise of the area it tests (M1). |
| Numeric criteria | Partially closed | Benchmark limits are numeric, but equivalence and "unchanged" criteria carry no tolerance (M2, M3). |
| Cache exact keys / eps | Closed in substance | Residual inconsistency between pseudocode and text (m1). |
| Pickle | Partially closed | Fixture provenance and the read-only flag after round trip are unspecified (m2). |
| Citation missing dependencies | Partially closed | The truth table exists, but the production page is not shown to exist (M4). |

## MAJOR

**M1. The generated-face floor does not detect what it is meant to detect (`docs/plans/2026-10-10-sr2314-r02.html:8`, `:21`).**
- The criterion is area ≤ ε·L², with L the triangle's longest edge.
- A clip-generated vertex carries interpolation error of order ε·C, with C the coordinate magnitude, so a geometrically collinear triangle has a computed area of order ε·C·L.
- For L = 1e-6 m at C = 100 m, that noise is about 1e8 times the threshold. The check then refuses only exact-zero computed areas or geometry near the origin.
- The accept/refuse outcome also depends on mesh origin: translating the same hull can change it.
- The plan gives no bound on what an accepted sliver (height-to-edge ratio near 1e-15) does downstream, for example in normal normalisation or wetted-area sums.
- Required:
  - a floor that scales with coordinate magnitude (k·ε·C·L with a stated k), or an explicit statement that the check is an exact-zero guard plus proof that no consumer divides by face area;
  - a regression with the mesh translated by a large offset;
  - paired cases just above and just below the floor.
- The red state of the item-1 regression (`:7`) is also undefined: the plan does not say whether current code wrongly rejects or wrongly accepts these triangles.

**M2. The benchmark criteria cannot falsify closure of finding 9 (`:28`, `:26`).**
- Line 26 concedes the per-station scan stays O(faces), so closure rests entirely on the measurement.
- The limits (wall time < 120 s, peak RSS < 2 GiB) are absolute. No criterion relates the revised code to the 4f7bfc0c baseline, so if the baseline already meets them the test shows nothing.
- Ten stations do not exercise the faces × stations cost of a realistic grid request.
- Host, CPU, repeat count and whether mesh generation counts toward RSS are not declared.
- The mesh is not required to extend above the waterline. Without freeboard no generated faces exist, and the "generated physical mean and minimum areas" are undefined.
- Wigley wetted area has no closed form. The plan calls the comparator "analytic" (`:12`) without naming the quadrature, its tolerance, or the exclusion of cap and centreplane.
- Required: a baseline-relative criterion, a station count matching grid use, a named wetted-area comparator, a freeboard requirement and declared hardware.

**M3. Equivalence and "unchanged" claims carry no tolerance (`:24`, `:26`).**
- Compaction remaps vertices and changes summation order, so indexed and legacy sections will not match bitwise.
- "Compared with the full-face legacy path" and "unchanged analytic values" state no relative or absolute tolerance.
- The plan does not say where the legacy path lives after the refactor: retained in source, test-only, or recomputed from 4f7bfc0c.
- Required: a stated tolerance per quantity (area, segment endpoints, hydrostatic properties) and the location of the reference path.

**M4. Default-on fail-closed citation is not shown to work in production (`:10`, `:29`).**
- `cite=True` is the default and raises if either page fails to resolve.
- The plan writes no sibling wiki page, and the worktree shows only a test fixture for the ITTC page.
- Existence of the production page for ITTC-7.5-02-03-01.4 is not established. If it is absent, every default transfer call raises outside the test fixture, which contradicts "public … remain compatible" (`:5`).
- Line 29 does not say whether `cite=True` was already the default, nor whether the return shape changes.
- "Sections 2.3/2.4.1" and "revision 05" are hard-coded with no source-verification step.
- A simplified transfer that cites the full procedure needs a deviation field in the sidecar and a test for it.
- Required:
  - an empirical check of the production page;
  - a declared behaviour for installs without the wiki;
  - a statement of the before and after default and return type.

**M5. Gate order and criteria-before-launch are not established (`:3`, `:28`).**
- The session's git status shows implementation, tests, the Holtrop script and an evidence directory already in the worktree while the plan is still under review.
- Tests-before-implementation ordering and declaration of the v1 criteria before the first benchmark run therefore cannot be confirmed from the packet.
- Line 30 quotes "31–51%" as a result inside a future-tense plan.
- Required: commit timestamps or evidence-file hashes showing criteria v1 predates the measurement, and acceptance bound to committed bytes rather than the dirty tree.

## MINOR

- **m1 (`:16` vs `:26`).** The pseudocode range test omits the epsilon that the text mandates. Faces coplanar with the station plane (min = max = station) are not addressed.
- **m2 (`:27`).** The source of the "old pickle" is unstated. It should be bytes produced at 4f7bfc0c and committed as a fixture; a pickle generated by the new code is circular. NumPy does not preserve `writeable=False` through pickle, so a round-trip immutability assertion is needed.
- **m3 (`:9`).** Cached segments feed midship, features and grid outputs. No test asserts that the cached arrays are read-only or copied on hand-out.
- **m4 (`:11`, `:27`).** The line-limit list is a hard-coded allowlist. A new helper module escapes it, and `citations/registry.py` is touched but not listed. A glob over the package is recommended.
- **m5 (`:27`).** `prohaska.py` appears only in the limits list. Its origin (split from `friction_scaling.py` or new method) and its citation obligation are unstated.
- **m6 (`:12`, `:30`).** Holtrop reproduction maps to none of findings 5, 7, 9 or 10. The assertion semantics of `test_holtrop_reproduction.py` are unstated; a test that pins the discrepancy values would lock in unqualified behaviour.
- **m7 (`:27`).** "Repeated station count will equal unique station count" is ambiguous. It should read "section evaluations equal unique stations requested", with the counting hook named.
- **m8 (`:29`).** The location where `cite=False` "records its omitted sidecar" is unspecified.

`★ Insight ─────────────────────────────────────`
- A degeneracy floor has to be compared with the rounding noise of the quantity it tests. Area from differenced coordinates carries error proportional to ε·C·L, so a purely local ε·L² floor sits below that noise whenever the triangle is small relative to its distance from the origin.
- Absolute performance limits only show adequacy on one machine. Attributing an improvement to a change needs a criterion relative to the baseline revision, measured the same way.
- A fail-closed default is only as reliable as the dependency behind it. A test fixture that stands in for the production wiki page proves the resolver logic and says nothing about whether the page is there.
`─────────────────────────────────────────────────`

**Next checkpoint:** revise lines 8, 21, 24 and 26–29 for M1–M4, attach ordering evidence for M5, rebuild the frozen packet, and re-review the delta.
