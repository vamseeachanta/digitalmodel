# FFS strength methods — published-example validation (2026-06-28)

Validation-only companion to issue **#1094** (deferred published-example golden
tests). No source behaviour was changed; this adds
`tests/asset_integrity/test_ffs_published_examples.py`, which encodes the exact
**inputs** of recognised published worked examples and asserts our code
reproduces the published **outputs** within a stated tolerance.

Modules under test:
- `src/digitalmodel/asset_integrity/corroded_pipe.py` — `b31g_original`,
  `modified_b31g`, `rstreng_effective_area`.
- `src/digitalmodel/asset_integrity/dnv_rp_f101.py` — `dnv_f101_single_defect`.

## Status legend
- **PUBLISHED-VALIDATED** — asserted number is from a published worked example
  (standard appendix example or published reference implementation) *and*
  independently hand-verified here.
- **REGRESSION-ANCHOR** — no external published numeric output could be sourced
  for these exact inputs; we pin our own current output to catch drift. Not a
  claim of agreement with an external authority.

## Cases

| # | Method | Source | Inputs | Published output | Our output | Status |
|---|--------|--------|--------|------------------|-----------|--------|
| A1 | ASME B31G original (safe pressure) | ASME B31G-1991 App. A Example #1 (Kiefner & Vieth CRVL.BAS; pipenostics `b31crvl`) | D=30 in, t=0.438 in, X52 SMYS=52,000 psi, d=0.100 in, L=7.5 in, F=0.72, MAOP=910 psi | Design = Safe pressure = **1093 psi**; intermediate factor A=1.847 | 1093.25 psi; A=1.847 | **PUBLISHED-VALIDATED** |
| A2 | ASME B31G original (corroded P_f) | same example | as A1, at the published max allowed depth d=0.2490 in | max allowed depth **0.2490 in** ⇔ MAOP **910 psi** (P_f·F = MAOP) | 910.9 psi (0.1 % high) | **PUBLISHED-VALIDATED** |
| B1 | DNV-RP-F101 single defect | DNV-RP-F101 (Oct 2010) §8.2 Example 1 (pipenostics `dnvpf`) | D=812.8 mm, t=19.1 mm, d=13.4 mm, L=203.2 mm, f_u(SMTS)=530.9 MPa | **15.86626 MPa** | 15.866256 MPa | **PUBLISHED-VALIDATED** |
| B2 | DNV-RP-F101 single defect | DNV-RP-F101 (Oct 2010) §8.2 Example 2 (pipenostics `dnvpf`) | D=219.0 mm, t=14.5 mm, d=9.0 mm, L=200.0 mm, f_u(SMTS)=455.1 MPa | **34.01183 MPa** | 34.011827 MPa | **PUBLISHED-VALIDATED** |
| C1 | Modified B31G (0.85 dL) | (geometry of Example #1) | D=30 in, t=0.438 in, X52, d=0.100 in, L=7.5 in | — none sourced — | 1624.68 psi | REGRESSION-ANCHOR |
| C2 | RSTRENG effective area | (geometry of Example #1, uniform profile) | D=30 in, t=0.438 in, X52, d=0.100 in, L=7.5 in | — none sourced — | 1587.44 psi | REGRESSION-ANCHOR |

## Notes on method and units
- **Case A1** reproduces the CRVL "safe pressure": the B31G estimated failure
  pressure factored by the design factor `F`, capped at the intact design
  pressure `2·SMYS·t/D·F`. For this shallow defect the cap governs (so the
  published number is the intact design pressure, 1093 psi). Case A2 is the
  stronger check because the corroded failure-pressure formula governs.
- **Cases B1/B2** are run in SI (mm, MPa). The capacity equation is
  unit-consistent, so the `capacity_pressure_psi`-named field carries **MPa**
  for these two SI cases; the test docstrings call this out.
- The DNV cases match the published values to ~5 significant figures (rel error
  < 1e-5), confirming our `dnv_f101_single_defect` uses the standard's mean
  capacity equation `P = (2 t f_u/(D−t))(1−d/t)/(1−(d/t)/Q)` with
  `Q = sqrt(1 + 0.31 (L/√(D t))²)` exactly.

## Discrepancies found
None beyond the documented method scope. The #1094 review already noted these
methods use the standard simplified Folias factors (parabolic B31G; two-part
Modified B31G; DNV `Q`); the validated cases above agree with the published
examples within tight tolerance, so no simplification surfaced a real
disagreement. The two regression-anchored cases are flagged honestly: they are
*not* externally validated — only no published number was found for those exact
inputs.

## Sources
- ASME B31G-1991, Nonmandatory Appendix A (worked Example #1), via the Kiefner &
  Vieth `CRVL.BAS` program and the CRAN `pipenostics` package
  (`b31crvl` reference example).
- DNV-RP-F101, *Corroded Pipelines* (October 2010), Section 8.2 single-defect
  capacity equation; worked examples via `pipenostics` `dnvpf` (examples 1 & 2),
  each hand-verified against the closed-form capacity equation.
