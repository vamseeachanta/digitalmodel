# Illustrative values against the computed static record

The prepared page retains computed static-line results only. Original response
values are removed rather than estimated. This table records what changes;
no original figure is promoted to an engineering result.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| R1 | 2026-10-14 | Static catenary lines and a traceable run record are added; unsupported illustrative response values are removed. |

| Source row or quantity | Original illustrative value | Computed replacement or removal | Reason |
| --- | --- | --- | --- |
| Matrix size | 120 cases | 120 recorded entries, 5 unique static solves, 60 entries with static lines, 60 not reproduced, 0 qualified environmental cases | Platform and sea-state labels do not establish independent response cases |
| Matrix axes | 3 platform classes, 4 concepts, 5 depths, 2 sea states | Labels retained in the native matrix; no independent environmental solves | Missing platform and numerical environmental inputs |
| Water depths | 200, 500, 1000, 1500, 2000 m | Computed static lines at these depths | Source-page depths are preserved |
| Spread angles | 120 or 90 degrees | Removed | No coupled spread geometry is solved |
| Offset-based pretension rule | Pretension selected to meet the offset band | Removed; prescribed force rule assumed for isolated statics | No environmental vessel equilibrium is solved |
| Three-line catenary cases | 30 | 30 entries referencing the five static lines | No coupled system or environmental response |
| Three-line catenary PASS / MARGIN / FAIL | 21 / 6 / 3 | Removed | Acceptance inputs and response criteria are absent |
| Three-line catenary original percentage column | 90.0% | Removed | No code-qualified cases |
| Three-line catenary dominant flag | Offset margin at deepest depth in survival | Removed | No vessel offset calculation |
| Four-line catenary cases | 30 | 30 entries referencing the same five static lines | Extra line count does not change an isolated line solve |
| Four-line catenary PASS / MARGIN / FAIL | 25 / 4 / 1 | Removed | Acceptance inputs and response criteria are absent |
| Four-line catenary original percentage column | 97.0% | Removed | No code-qualified cases |
| Four-line catenary dominant flag | Tension margin in survival | Removed | Prescribed statics are not environmental peak tension |
| Taut polyester cases | 30 | 30 entries marked not reproduced | No taut system calculation |
| Taut polyester PASS / MARGIN / FAIL | 18 / 8 / 4 | Removed | No taut system or frequency-domain response |
| Taut polyester original percentage column | 87.0% | Removed | No code-qualified cases |
| Taut polyester dominant flag | Surge period drift | Removed | No coupled natural-period or stiffness-sensitivity calculation |
| Chain-polyester hybrid cases | 30 | 30 entries marked not reproduced | No segmented hybrid model |
| Chain-polyester hybrid PASS / MARGIN / FAIL | 22 / 6 / 2 | Removed | No hybrid system or environmental response |
| Chain-polyester hybrid original percentage column | 93.0% | Removed | No code-qualified cases |
| Chain-polyester hybrid dominant flag | Anchor footprint at shallow depths | Removed | No hybrid geometry or anchor design |
| Three-line catenary stiffness | approximately 80 kN/m | Removed | Isolated force-based geometry does not calculate a restoring matrix |
| Four-line catenary stiffness | approximately 110 kN/m | Removed | No vessel restoring matrix |
| Hybrid stiffness | approximately 140 kN/m | Removed | No segmented model |
| Taut stiffness | approximately 220 kN/m | Removed | No taut restoring matrix |
| Offset band | 10.0% of depth | Removed | No qualified allowable or vessel equilibrium |
| Near-limit margin band | 10.0% | Removed | No acceptance classification |
| Stiffness sensitivity | plus or minus 20.0% | Removed | No stiffness sweep or concept ranking |
| Environmental return-period labels | operating one-year; survival hundred-year | Operating/survival labels only in record | Numerical environmental inputs are not stated or assigned |
| Delivery claims | 24-hour screening, 48-hour memo and project run | Removed | No delivery-time evidence |
| Static line resultant, horizontal force and geometry | Not given | Five native lines at the stated depths in results.json | Computed with assumed homogeneous wire and prescribed force rule |
| Empirical MBL/static-tension ratio | Not given | Computed per line; required standard factor not established | Held public example strength is not a certified component selection |

The removed quantities appear by name and reason under **Not reproduced** on the
prepared page; their original numerical values do not remain on that page.
The original page source is pinned by commit and digest in `basis.json`.
The prepared source is not published and its website tests remain open under
[the acceptance issue](https://github.com/vamseeachanta/workspace-hub/issues/2115).
