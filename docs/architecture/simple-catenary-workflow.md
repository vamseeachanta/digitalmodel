# Simple catenary workflow

The simple-command workflow now exposes one reusable single-case service while
retaining the existing analyzer as the calculation owner.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-11-16 | Document the extracted service and its unchanged presentation contract. |

## Public seam

Import `SimpleCatenaryRequest` and `analyze_simple` from
`digitalmodel.subsea.catenary_riser.workflow`. The request carries the existing
simple-command inputs: lengths in metres, coating density in kg/m³ and top
tension in newtons. Material and internal-fluid defaults remain `x65` and `oil`.
The service constructs `RiserConfiguration`, calls `SimpleCatenaryAnalyzer`
once and returns `SimpleCatenaryOutcome` with the unrounded `result` and rounded
`presentation` dictionary. Top tension takes precedence over suspended span.
Unconverged or exhausted-length results raise the existing `ValueError`.

The call graph is CLI → service → configuration and analyzer. Click parsing,
console presentation, error translation and JSON file handling remain in the
CLI; the service accepts no output path and performs no file or network access.
Synthetic fixtures capture both control modes before extraction, including the
existing JSON keys and two-decimal rounding. They establish presentation
equivalence, not an independent engineering qualification.

## Basis and exclusions

The [engineering basis](../../src/digitalmodel/subsea/catenary_riser/docs/engineering-basis.md)
and [route qualification](../../src/digitalmodel/subsea/catenary_riser/docs/route-qualification.md)
retain their existing scope: static, uniform-weight, flexible catenary geometry
from a horizontal-tangent vertex. Configuration defaults, including external
fluid and gravity, remain with the models. Preserve the raw result's provenance
and warnings when consuming it; the legacy presentation dictionary omits them.
No standards-derived constants are introduced. Future standard-based
calculations require citation sidecars and citation checks.

Weight-command extraction, the offline diffraction conversion facade's provenance
contract and native proof orchestration are separate proposed follow-ups. The
legacy angle-based workflow registry, quarantined lazy-wave route, solver
exports, engineering formulas and general engine routing remain unchanged.
