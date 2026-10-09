# Plan for #2259: Rebuild the ABS ships hull route

> **Issue:** [#2259](https://github.com/vamseeachanta/digitalmodel/issues/2259)
> **Authority:** explicit user instruction for step 2, 2026-09-29
> **Status:** revision 2 after adversarial findings; implementation authorized
> **Complexity:** T3 (standards-derived engineering calculation, routing, reports, datasets)
> **Lane:** `lane:codex`
> **Client:** N/A
> **Execution mode:** parallel-readonly discovery, single-lane implementation

## Resource intelligence summary

The implementation will use the licensed December 2017 *ABS Guidance Notes on
Cathodic Protection of Ships*. The PDF will remain in the private standards archive and
will not be committed. The repository dataset will contain table identifiers, captions,
numbers, units, and a provenance record only.

The existing `ABS_gn_ships_2018` adapter will be replaced by a new-package calculation.
The key's date is a documented misnomer. The present legacy route compounds percentage
inputs into near-unity multipliers, temperature-adjusts the selected alloy capacity, and
uses only mass-basis count for its output checks. The private five-year hull benchmark
therefore reports approximately 32% more mean demand, 49% more final demand, and 22%
more required mass than the old engine.

ABS Section 2/4.4 defines `Jc = Jb × fc` and bounds the coating breakdown factor between
zero and one. The route will convert the existing 1.0/2.05 inputs from percentages to
0.0100/0.0205 fractions. Section 2/4.5 will govern mean and maximum demand; the arithmetic
mean coating factor will be identified as a project selection because Table 4 gives no
time-development equation. It will use `cited-pending-review`, not
`client-use-with-eor-check`, until that choice and the citation target are reviewed.

Registry checks will record that the generic standards-library source is catalogued, but
no ABS-ships-specific wiki citation target or engineering registry entry exists. Citations
will identify `abs-gn-ships`, revision `2017-12`, and exact section/table locators. Result
metadata will state that the prospective wiki target does not resolve.

## Governing equations and criteria

- Initial/mean/final demand will be computed per area and condition. Percentage inputs
  will be divided by 100, `fc` will remain in `[0,1]`, and Section 2/4.5 dynamic/static
  weighting will be used. The arithmetic mean `fc` and current densities outside Table 3
  will be labeled as project choices.
- Required mass will use Section 2/7.3,
  `W = Imean × life × 8760 / (Q × u)`.
- Initial long-flush resistance will use Section 2/6.2, `R = rho / (2S)`. Its depleted
  semi-cylinder will require an actual steel-core cross-section and use Sections
  2/7.5.1-7.5.2(b), followed by the Section 2/6.2 long-flush resistance equation using
  the depleted equivalent width. Short-flush resistance will use Section 2/6.3
  `R = 0.315 rho / sqrt(A)` and retain its initial value at end of life per Section
  2/7.5.2(c).
- Current output will use Section 2/5, `I = delta_E / R`.
- The recommended count will be the maximum of mass, initial/mean/final output, and
  layout minimums. PASS will compare an independently supplied selected count against
  those requirements.
- Bilge spacing will be checked against the Section 3/5.2 limit selected by service:
  6-8 m generally or the smaller project input where the guide requires it. Layout checks
  will remain `NOT_EVALUATED` when actual spacing, selected locations, uniform
  distribution, bilge damage exposure, or service condition is not established.

## Artifact map and files to change

- `src/digitalmodel/cathodic_protection/abs_ships_tables.py` will provide cited table
  lookups returning `CitedValue`.
- `src/digitalmodel/cathodic_protection/abs_ships.py` will validate inputs and compose
  `_kernels` for demand, mass, resistance, count, and output checks.
- Existing `_kernels.py` resistance, mass, output, and count functions will be composed;
  ABS-specific depleted geometry will remain in `abs_ships.py`.
- `engine_adapter.py` will route `ABS_gn_ships_2018` to the new module and
  `ABS_gn_ships_2018_legacy` to the old solver with `DeprecationWarning`.
- `report_adapters.py` will render demand, mass/count, fresh/depleted output, citations,
  layout, governing case, PASS/FAIL, and the pending-review limitation.
- CSV datasets and `PROVENANCE.md` will be added below
  `tests/fixtures/test_vectors/cathodic_protection/datasets/abs-gn-ships/2017-12/`.
- Focused tests will cover tables, formulas, routing, legacy warning, reports, data
  provenance, hand-derived de-identified regression values, and invalid inputs.
- `CHANGELOG.md` and `docs/domains/cathodic_protection/_index.md` will be updated.
- The private comparison will be written only to the authorized benchmark follow-up.

## TDD sequence

1. Table lookup and dataset tests will fail before `abs_ships_tables.py` exists.
2. Kernel/unit tests will fail for coating phases, mass, depleted geometry, resistance,
   output counts, layout states, governing case, and PASS/FAIL.
3. Adapter tests will fail while the main key remains blocked and the legacy alias is
   absent.
4. Reporting tests will fail until every cited/derived value and limitation is rendered.
5. The de-identified hull regression will use rounded geometry and area. Every expected
   value will include a hand derivation from a cited table/equation.

## Verification

The targeted CP and reporting tests will run first. Ruff and mypy will run on touched
Python files. A public import smoke will verify the package outside pytest path injection.
The private source/new/old comparison will be read back from the private follow-up. An
adversarial plan review will precede code edits, and an adversarial code/artifact review
will precede commit.

## Risks and exclusions

- The guide's Table 4 supplies ranges but no coating time law; the benchmark calculation
  supplies a project interpretation that will remain explicit and pending engineering
  review.
- The guide's printed depleted cross-section equation is dimensionally ambiguous. The
  route will test the printed leading-pi interpretation and require an explicit core area.
- Section 2/7.5.2(b) defines the depleted long-flush shape by reference to the slender
  geometry calculation but leaves the “relevant” resistance branch open to interpretation.
  The route will use Section 2/6.2 with depleted width and retain this as a review item.
- ICCP, internal tanks, propeller/shaft specialty design, and geometric placement
  optimization are outside this issue.
