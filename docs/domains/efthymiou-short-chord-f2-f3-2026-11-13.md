<!-- ABOUTME: Validation record for Efthymiou short-chord F2 (pinned axial) and F3 (OPB chord saddle). -->
<!-- ABOUTME: Records clause citations, independent goldens, frozen F1 results and the approved base-equation limitations. -->

# Efthymiou Short-Chord Factors F2 and F3 (2026-11-13)

## Summary

The F2 and F3 corrections are added under the approved revision of
[issue 1148](https://github.com/vamseeachanta/digitalmodel/issues/1148#issuecomment-4931446656).
The default fixed-end axial calculation retains F1. The keyword-only
`chord_ends="pinned"` selects F2 for both axial saddles. F3 corrects the OPB
chord saddle. All factors return exactly 1.0 at `alpha >= 12`.

The approved appendix-unavailable fallback is implemented. The licensed DNV
appendix was unavailable in the inspected sources; fresh verification of Eqns
5/6/7 and 11 is not established. The pinned path is an approximation: **F2
applied to fixed-end saddle form; Eqn-5/6/7 general-fixity base equations
deferred**. Fixed-end crowns (Eqns 2/4) are retained. No general-fixity parameter
C is introduced. The OPB brace expression is retained without F3, and its result
remains unverified. Method descriptions expose these limitations.

## Source of truth

- DNV-RP-C203 (2021), Appendix B, Table B-1: axial load, general fixity,
  short-chord F2 column; out-of-plane bending saddle short-chord F3 column.
- Efthymiou, M. (1988), OTC 4829.
- The repository's [F1 validation record](efthymiou-short-chord-f1-2026-06-29.md)
  records the F2/F3 expressions as previously confirmed against Appendix B.
  That existing record and the approved issue revision are the formula sources
  for this change. They do not establish new access to the licensed appendix.
  No licensed document is copied into the repository.

## Factor formulas

```text
F2 = 1 - (1.43*beta - 0.97*beta^2 - 0.03) * gamma^0.04
         * exp(-0.71 * gamma^(-1.38) * alpha^2.5)     for alpha < 12
F3 = 1 - 0.55*beta^1.8 * gamma^0.16
         * exp(-0.49 * gamma^(-0.89) * alpha^1.8)     for alpha < 12
F2 = F3 = 1.0                                       for alpha >= 12
```

The ratios are dimensionless: `beta=d/D`, `gamma=D/(2T)`, `tau=t/T`, and
`alpha=2L/D`. The default `L=2.5*D` gives `alpha=5`.

## Where the factors are applied

| Load case and term | DNV Appendix B, Table B-1 locator | Implementation |
|---|---|---|
| Fixed axial chord / brace saddle | Eqns 1/3, F1 | F1 unchanged |
| Pinned axial chord / brace saddle | General-fixity saddle, F2 | F2 on retained fixed-end bases; approximation |
| Pinned axial crowns | General-fixity Eqns 6/7 | Deferred; fixed-end Eqns 2/4 retained without correction |
| OPB chord saddle | Eqn 10, F3 | F3 applied once before the SCF floor and rounding |
| OPB brace saddle | Eqn 11, F3 | Eqn 11 unverified; legacy equation unchanged, no F3 applied |
| IPB chord / brace crown | Eqns 8/9, correction “None” | No correction; calculation unchanged |

Table 1. Implemented corrections and the limits of the approved fallback.

## Independent numerical derivation

At `beta=0.5`, `gamma=12`, `alpha=5`, 50-digit Decimal evaluation gives:

```text
F2 = 0.86499301577145575866763846384676498302142269094856
F3 = 0.91111169476715752621718690426984388422116290038120
pinned chord saddle = 5.3692554783977715817767703064982716398710517725848
pinned brace saddle = 4.9953051410576972397070048483922448395016276941110
OPB chord before F3 = 12*0.5*0.5*(1.7 - 1.05*0.5^3) = 4.70625
OPB chord after F3 = 4.2879194134979351077596358682199527801158478999190
```

These values are independently compared with binary floating-point inline
expressions at relative tolerance `1e-12`, and with three-decimal factor
values at absolute tolerance `0.0005`. SCF outputs remain rounded to three
decimals after applying the existing minimum SCF of 1.0. Rounded factors are
never used to calculate a golden SCF.

## Computed factors and SCF comparisons

All five geometries match the F1 record, with `alpha=5`. The “fixed” columns
give both the old and new default-call result: **all five numeric result
payloads and the F1 floating-point bits match unchanged main**. Method text
now names the fixity and the Table B-1 correction. The pinned path is additive,
so fixed-to-pinned values compare two model choices rather than a replacement
of fixed-end behavior.

| D / T / d / t (mm); theta (deg) | F2 | F3 | Fixed chord / brace SCF, old = new | New pinned chord / brace SCF |
|---|---|---|---|---|
| 1000 / 50 / 500 / 25; 90 | 0.907243 | 0.927260 | 4.178 / 4.062 | 4.693 / 4.563 |
| 1000 / 50 / 500 / 25; 45 | 0.907243 | 0.927260 | 2.400 / 2.413 | 2.695 / 2.530 |
| 1000 / 50 / 300 / 15; 90 | 0.934662 | 0.970997 | 2.221 / 3.334 | 2.398 / 3.601 |
| 1000 / 50 / 800 / 40; 90 | 0.896616 | 0.830493 | 5.370 / 3.839 | 6.136 / 4.387 |
| 1200 / 50 / 600 / 25; 90 | 0.864993 | 0.911112 | 4.755 / 4.423 | 5.369 / 4.995 |

Table 2. Dimensionless factor values and axial SCFs at the five reference geometries.

| D / T / d / t (mm); theta (deg) | OPB chord SCF, old → F3 | Legacy OPB brace SCF, old = new |
|---|---|---|
| 1000 / 50 / 500 / 25; 90 | 3.922 → 3.637 | 1.000 |
| 1000 / 50 / 500 / 25; 45 | 2.253 → 2.089 | 1.000 |
| 1000 / 50 / 300 / 15; 90 | 1.504 → 1.461 | 1.000 |
| 1000 / 50 / 800 / 40; 90 | 7.439 → 6.178 | 1.000 |
| 1200 / 50 / 600 / 25; 90 | 4.706 → 4.288 | 1.000 |

Table 3. Dimensionless OPB comparisons. Brace values are legacy, unverified
outputs at the existing floor; they are not Eqn-11 validation goldens.

For `D=1200 mm`, `L=7200 mm` gives `alpha=12`: F2 and F3 are exactly 1.0,
the axial chord SCF is 6.207, and the OPB chord SCF is 4.706. The same
factor recovery is verified at `alpha=20`.

## Unresolved OPB brace equation

The retained expression is
`tau^0.9 * (0.12*exp(-4*beta) + 0.011*beta^2 - 0.045)`, clamped to zero
before the final SCF floor. The approved plan flags the candidate Eqn-11 form
`tau^-0.54 * gamma^-0.05 * (0.99 - 0.47*beta + 0.08*beta^4) * SCF_chord_OPB`.
The candidate is **not freshly source-verified and is not implemented**.

The next checkpoint is licensed Table B-1 verification of Eqn 11, followed by
an independently derived brace golden and F3 wiring if confirmed. Full
general-fixity Eqns 5/6/7 likewise require source verification. A follow-up
issue is not posted because this task permits local files only; the PR-body
draft retains the discrepancy for the publishing owner.

## Test changes

The original SCF library and connection SCF tests remain unchanged. New tests
are placed in `test_short_chord_factors.py` to keep every changed module below
400 lines. The existing non-tubular SCF definitions are moved unchanged into
`_scf_details.py` and re-exported through their existing public import route.

- Before the change, the original focused gate passed 55 tests. The new tests
  then produced 28 failures and one passing IPB regression.
- After the change, all 84 focused tests pass: 40 original SCF tests, 29 new
  factor/fixity tests and 15 unchanged connection SCF tests; zero skips.
- Coverage includes exact long-chord recovery, the short side of the alpha=12
  boundary, both pinned saddles, retained crowns, invalid fixity rejection,
  public exports, oblique OPB loading and IPB alpha independence.
- The five default axial numeric payloads and F1 floating-point bits are
  compared against a snapshot captured before implementation. Full serialized
  results differ in their intentionally updated method descriptions.
- No OrcFxAPI import or licensed solver execution is required.

## Functions reviewed but NOT changed

- `short_chord_factor_f1`: executable body unchanged; companion-factor
  docstring updated.
- `efthymiou_ty_ipb`: executable body unchanged; the no-correction note is added.
- `efthymiou_k_axial`: unchanged.
- Plate, weld and Peterson SCFs: definitions unchanged; module extraction only.
- `connection_scf`: unchanged, including its F1 helper import and all its tests.
- The fatigue demonstration notebook: import route retained; no saved outputs
  or notebook edits are introduced.
