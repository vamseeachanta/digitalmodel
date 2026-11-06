# Design-data table layout

Calculation-report design-data tables retain the shared minimum width and use
local horizontal scrolling when the available screen width is smaller. Long
values remain in the HTML. This document provides a reproducible review example;
it does not establish that a browser or printed page renders every input without
clipping.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| A | 2026-10-19 | Document the existing width policy and add synthetic inputs for repeatable layout review. |

## Width policy and sources

The selected policy in issue #2024 is to retain the global table floor without a design-data exemption. The change
already shipped in PR #2443 (2026).
The packaged [stylesheet](../../../src/digitalmodel/reporting/assets/calc_report_format.html)
is the source for these presentation choices:

- `table { min-width: 440px }` applies to design-data tables. This is inherited
  presentation policy, not a demonstrated minimum for readable engineering data.
- `.datagrid` stacks groups in a single column. `.kv` uses `overflow-x:auto`.
- Labels remain on one line; prose values can wrap. A long unbroken token or
  label can still require local scrolling.
- The obsolete `.kv table { min-width:0 }` exemption is absent. Restoring it
  would allow these tables to shrink below the shared floor.

For available container content width `C`, the table width `T` satisfies
`T >= 440px`. If `C < T`, the scrollable content is wider than the container;
this alone is not a document-level overflow defect. These relations follow from
the packaged minimum-width rule. The scrolling mechanism follows
[CSS Overflow Module Level 3, section 3.1, `auto`](https://www.w3.org/TR/css-overflow-3/#valdef-overflow-auto):
scrolling controls depend on whether content overflows. The source was consulted
on 2026-10-19. No new pixel threshold or engineering acceptance limit is proposed.

The [print stylesheet](../../../src/digitalmodel/reporting/calc_report.py) keeps
auto table layout and wraps value cells. Its default page size is Letter; an A4
review must explicitly select A4 and override the CSS page size in print settings.
A screen scroll container does not prove that printed overflow remains visible.

The standard report engine reuses the house stylesheet. Its
[additional CSS](../../../src/digitalmodel/reporting/assets/standard_report.css)
has separate exemptions for title-block metadata, revision history and input
echo. Those are outside the calculation-report design-data decision, as are
symbol definitions (`.defs`). No global floor change is proposed here.

## Worked example: inputs, method and answer

The executable [example](../../../examples/reporting/design_data_layout.py)
constructs a complete `CalcReport` with synthetic text:

| Input | Purpose | Expected answer |
|---|---|---|
| Provenance note ending in `final qualifying phrase.` | Prose wrapping | The complete final phrase remains in the cell. |
| `ABCDEFGHIJKLMNOPQRSTUVWXYZ_` repeated eight times | Unbroken value stress case | Every repetition remains reachable on screen and visible in print. |
| `Range < lower bound & note "retained">`, unit `unit` | Entity escaping and inline units | The visible cell text equals the original text followed by a space and `unit`. |
| Separate descriptive and unbroken-input groups | Stacked layout | Groups appear one below another. |

The repetition count is illustrative stress data, not a minimum accepted string
length. The method is to serialize these inputs through the public report API,
decode each table cell independently and compare it with its supplied text. There
is no domain calculation and no derived engineering result. The expected text
comes from the literals above, not from the renderer's output.

Set `REPORT_REVIEW_DIR` to an external output directory before running:

```sh
PYTHONPATH=src uv run --no-project --with pydantic --with jinja2 \
  --with plotly==5.17.0 python examples/reporting/design_data_layout.py \
  --output "${REPORT_REVIEW_DIR:?set an external output directory}/design-data.html"
```

The Plotly version is the runtime version declared in `pyproject.toml`. The
example writes HTML only; it starts no browser or solver. It labels rendered
geometry `NOT_EVALUATED` and records the next review step in the report.

## Automated evidence and rendered acceptance

`tests/reporting/test_design_data_layout.py` checks complete long-value text,
entity decoding, units and empty values in design data, assumptions and results.
It also exercises the output command and retained floor/scroll stylesheet
contract. The existing calculation-report regression tests continue to cover
stacked groups, wrapping and print table layout. These checks establish emitted
content and stylesheet intent; they cannot measure pixels or printed geometry.

For rendered review, open the generated example and record:

1. At a narrow viewport, the actual viewport width, absence of document-level
   horizontal overflow, local scrolling within each over-wide input group, and
   access to the final character of every input. Scroll the unbroken input to
   its end rather than judging only the initially visible area.
2. In A4 print preview with A4 selected explicitly, the print settings, complete
   final words and identifier, and whether any table crosses the printable area.
3. Browser and version, screenshots or PDF evidence, and the observed result.

Until that review is recorded, A4 and narrow-screen containment remain
`NOT_EVALUATED`. If print clips an input, preserve the floor decision separately
from any subsequent print-layout fix; text-preservation tests alone cannot close
that finding.
