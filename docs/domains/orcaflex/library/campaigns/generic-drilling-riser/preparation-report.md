# Drilling riser analysis: preparation and analytical screens

The [preparation report](preparation-report.html) presents the existing connected-mode
case grid and analytical column screens without requiring licensed solver results
or an external acceptance-criteria file. Every solver comparison is
`NOT_EVALUATED`; no operating window is established.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 00 | 2026-10-20 | Added a preparation report so inputs and missing qualification can be inspected before solver results exist. |

## Inputs, method and answer

The generic [specification](../../templates/drilling_riser/spec.yml) supplies the
water depth and main-column length. The existing [case table](case-table.yml)
supplies the offset and current assumptions, with source fields for every value.
The grid is assumed, based on previous experience; it is not a measured site.
No additional geometry, material constant or operating limit is introduced.

For case `o5-c0`, the specified water depth is 1,020 m, the offset is 5.0% and
the current is zero. The offset hand calculation is
`X = depth * offset_percent / 100 = 1020 * 5 / 100 = 51.0 m`.
For the specified main-column length `L = 1020 m`, the straight-chord extension is
`sqrt(L² + X²) - L = sqrt(1020² + 51²) - 1020`, reported as **1.3 m**.
This is a geometric screen, not a mechanical tensioner stroke prediction.
The test docstring writes out this independent calculation; it does not use the
screen implementation to obtain the expected answer.

The analytical implementation solves the pinned constant-tension beam equation
`EI y'''' - T y'' = q`. Zero end moments and prescribed end offset are its boundary
conditions. The screen uses steady drag `q = rho Cd D U² / 2` and section stress
`T/A + |M|D/(2I)`. Sources are the existing implementations in
`src/digitalmodel/drilling_riser/riser_response.py`, `section.py`, `stackup.py`
and their adapter `generic_campaign.py`. This report introduces no numerical
acceptance criterion.

The public [MIT Structural Mechanics notes (2013)](https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/pages/lecture-notes/)
provide the independent mechanics basis: Lecture 6, equations (6.4)–(6.6),
combine constant axial force, transverse equilibrium and moment-curvature
relations to give the tensioned-beam equation. Lecture 4, equation (4.30), gives
`sigma = N/A + Mz/I`; evaluating it at the outer fibre `z = D/2` gives the
stress screen. Chord extension follows from the Pythagorean theorem, as written
out above. The lecture PDFs were accessed on 2026-10-20.

The public [Orcina line-load theory](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Linetheory,Hydrodynamicandaerodynamicloads.htm),
section Drag, supplies the transverse drag formulation (accessed 2026-10-20).
Its coefficients remain those recorded in the generic specification; the public
page does not qualify those assumed coefficients for this configuration.
The public [Orcina statics guidance](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Generaldata,Statics.htm)
explains the iterative equilibrium calculation (accessed 2026-10-20).
Both references are unversioned public WebHelp, not held-edition code criteria.

## Reproduce

With the reporting dependencies installed, from the repository root:

```sh
PYTHONPATH=src uv run python scripts/reporting/build_generic_riser_preparation_report.py \
  --output docs/domains/orcaflex/library/campaigns/generic-drilling-riser/preparation-report.html
```

An alternate prepared directory can be selected with `--campaign`. Its case table
must equal the current generic design basis. The builder checks the template,
case table, CSV and calculation fingerprints, then recomputes the analytical
screen to check every CSV value. It rejects missing or duplicate cases, changed
coordinates and nonfinite or changed numerical results. Rendering is deterministic;
the regression test compares the committed HTML with a fresh rendering.

## From preparation to qualified results

Preparation rendering does not inspect stored solver summaries. Even when a
summary exists nearby, all solver-dependent cells remain `NOT_EVALUATED`.
Use `scripts/reporting/build_generic_riser_report.py` for the results report:
it requires all matched summaries, one solver version and externally sourced
runtime criteria. The existing stack-up and operability screens remain analytical
comparators, not assembly acceptance calculations.

The connected drilling model and running-BOP model were inspected as context;
running BOP is a different operation. No vendor model is copied into this report,
and unqualified drilling-model groups are excluded. Native qualification, sourced
equipment capacities, soil representation, shielding disposition, End B moment
interpretation and confirmation of applicable API RP 16Q clauses remain open.
No licensed interface is imported, queried or executed by preparation reporting.
