# Tank and pressure vessel shell sizing

The shell calculators and shared report adapters return arithmetic results,
input provenance, tables and interactive thickness charts. The worked examples
are synthetic and carry **NOT_EVALUATED** code qualification. A resolved source
checks metadata; it does not authenticate a numeric value or approve a design.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 00 | 2026-10-21 | Document the existing examples and separate applicability evidence so unresolved rules remain visible. |
| 01 | 2026-10-21 | Align thickness precision and source evidence so small margins and missing applicability values remain visible. |

## Worked examples

- [Tank shell analysis](one-foot-worked-example.html): a 30 m diameter tank,
  18 m liquid level, eight equal courses, specific gravity 1.0 and 1.5 mm CA.
- [Pressure vessel shell analysis](pressure-vessel-example.html): shell and head
  thickness, MAWP, hydrotest pressure and a pressure sweep with design point.
- [Inputs and unrounded results](shell-thickness-examples.json): input echo,
  course results, evidence gaps and report digests.

Both pages use the house reporting engine. Compact pages load Plotly from its
public CDN; the default generator embeds it for offline use. Chart hover data
pairs each requirement with its nominal wall and margin, including negative
margins for insufficient vessel walls. Equal heights on either side of a course
boundary keep their respective selected plates.

## Method and sources

API 650 5.6.3.2 is the equation locator supplied by issue #2958 (2026).
The issue does not identify an edition and is not a qualified standard source.
The library therefore requires the method coefficient, head offset, diameter
limit, nominal floor, material allowables and plate catalogue as explicit inputs.
No code material table or acceptance values are built in.

For each bottom-up course, the implemented equations are
`td = c D max(H-a, 0) G / Sd + CA` and
`tt = c D max(H-a, 0) / St`, using the design liquid level for both sizing cases.
A separate test fill level affects only the applied-pressure graph. The selected
plate is the first supplied plate at least as thick as the maximum of design,
test and nominal floor requirements. Shell mass uses circumference times course
height times nominal wall times the supplied density (thin-shell approximation).

With the illustrative inputs recorded in the example, independent bottom-course
arithmetic gives `4.9 * 30 * (18-0.3) / 150 + 1.5 = 18.846 mm`.
Course 7 begins at `6 * 2.25 = 13.5 m` and gives
`4.9 * 30 * (4.5-0.3) / 150 + 1.5 = 5.616 mm`.
The hand and program design values agree to three decimal places:
18.846 mm and 5.616 mm. These are arithmetic examples with assumed allowables,
not a qualification of A36 or another plate grade. All unsourced scenario inputs
are **assumed, based on previous experience**.

The vessel adapter uses the existing ASMEThicknessCalc equations: UG-27(c)(1),
UG-32(d)/(f) and UG-99. Their edition is unresolved in the inherited module.
The test factor and test allowable are caller inputs. MAWP is the smaller of
shell and head capacities. Hydrotest pressure is reported without a test-stress
acceptance assessment. Existing regression tests show that MAWP, reinforcement
and hydrotest each have one method definition and retain their hand values.

## Evidence and material inputs

Provide a Citation for each of the method, material, diameter limit (5.6.3.1)
and nominal floor (5.6.1.1). The last two use the optional OneFootBasis fields
`diameter_limit_citation` and `minimum_nominal_citation`; omitting either leaves
that check NOT_EVALUATED even if a numeric value is supplied. Method and
applicability citations must identify the same code, publisher and edition,
including when the method citation is absent. Applicability citations without
their numeric values are validated but omitted from the evidence sidecar; the
affected report rows remain NOT_EVALUATED.
Existing callers may continue to supply uncited illustrative inputs.

| Requested plate grade | Design allowable | Test allowable | Required evidence |
| --- | --- | --- | --- |
| A36 | NOT_EVALUATED | NOT_EVALUATED | Applicable API 650 material table, edition, thickness and temperature range |
| A283 C | NOT_EVALUATED | NOT_EVALUATED | Applicable API 650 material table, edition, thickness and temperature range |
| A516 70 | NOT_EVALUATED | NOT_EVALUATED | Applicable API 650 material table, edition, thickness and temperature range |

These entries identify missing inputs only; they are not a material database.
The way forward is to obtain qualified allowable values and applicability limits,
supply their citations, and review the selected plate catalogue and density.
A supplied diameter above its limit is refused with a reference to 5.6.4.

## Reproduce

With the repository's declared reporting dependencies installed:

```sh
PYTHONPATH=src uv run python examples/code_checks/shell_thickness_reports.py --output REPORT_DIRECTORY
```

Use `--compact` to reproduce the checked-in distribution. The generator records
SHA-256 digests of the rendered bytes and reads back each output. The example
tests compare regenerated compact pages and the manifest byte for byte.

## Limits and later slices

Unqualified material tables and code limits remain NOT_EVALUATED. The variable
design point method, wind girders, roof, bottom, annular plates, seismic,
settlement and full code compliance remain outside this shell sizing scope.
The course geometry and selected wall data can inform subsequent settlement
assessment, but neither linked settlement workflow is executed here.
