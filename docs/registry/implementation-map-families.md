# Supplemental engineering families

The [implementation map](implementation-map.yaml) includes existing drift,
extreme-statistics, design-wave, sloshing, geotechnics and scour members for
review and module knowledge packs. These are bounded inventories of source
definitions, with calculations, data models and delegates identified separately.
Membership establishes neither numerical correctness nor design suitability.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-23 | Six source inventories and lookup examples were added so reviews can cite existing methods and their limits. |

## Scope and method

The earlier repository-wide snapshot retains its revision, searches, groups and
classifications. Each new family's `inventory_scope` records the revision and
exact `source_paths` inspected for this supplement. Source inspection follows
the issue's domain terms; the listed files define the coverage boundary.

For each member, the map records its repository-relative path, exact qualified
symbol, definition line, file digest and normalized AST-body digest. These are
derived from source text without importing a solver. `independent` means a
calculation body, `delegate` means an adapter or composition of existing methods,
and `data_model` means stored data or access to it. These roles do not rank
engineering quality. In particular, a stored theory choice does not implement
the corresponding solver theory.

Algorithm, vocabulary and key searches run independently over each family's
listed source files. `search_audit` counts files per signal, not implementations.
Unmapped matching definition names are retained in `rejected_candidates` with
`disposition: needs_review`; they are excluded from member counts. All six
families remain `partial`, including when every matching name is accounted for.
Dynamic dispatch and semantic completeness are not established by name searches.

The generator preserves these search boundaries on regeneration and refreshes
their revisions along with source coordinates. The initial supplemental entries
are not added retroactively to the earlier snapshot's identical-body groups.
A full regeneration can rebuild groups across all families.

## Families and limits

| Family id | Members | Coverage and unresolved engineering questions |
| --- | ---: | --- |
| `second-order-slow-drift-response` | 5 | Mean-force integration, Newman and full difference-frequency force methods, plus QTF diagonal accessors. Coupled response and finite-duration extremes are NOT_EVALUATED. |
| `extreme-statistics` | 14 | Gumbel, Weibull and Rayleigh fits or quantiles across response, current and postprocessing modules. Distribution selection and statistical qualification are NOT_EVALUATED. |
| `design-waves` | 5 | Prescribed regular-wave data, input builders, a surface-elevation evaluator and linear dispersion. Conditioned design waves, crest selection and breaking limits are NOT_EVALUATED. |
| `sloshing` | 8 | Analytical modal periods, equivalent geometry and resonance screening. Empirical constants, tolerances, nonlinear impact and CFD qualification are NOT_EVALUATED. |
| `geotechnics` | 22 | Piles, anchors, mudmat bearing, liquefaction and configuration routes; a slope-analysis delegate is identified. Soil parameters, empirical factors and compliance are NOT_EVALUATED. |
| `scour` | 5 | Sediment-motion threshold conversion, pipeline and monopile screens, armour thickness and a configuration delegate. Legacy curve attribution and design suitability are NOT_EVALUATED. |

The inventory adds 59 members; 27 additional matching names require classification
within the bounded source lists. These counts follow explicit member and
candidate rows, without extrapolating to other repository folders.

## Worked example: scour lookup

**Input:** family `scour`; retain callable calculation and delegate rows. No soil,
wave or calibration values are supplied because this example locates methods.

**Method:** read `families`, select the exact `id`, then separate rows by
`relation`. The independent hand count comes from the four public calculation
definitions in [scour.py](../../src/digitalmodel/geotechnical/scour.py) and the
single `ScourWorkflow.router` definition in
[cfg_workflows.py](../../src/digitalmodel/geotechnical/cfg_workflows.py).

```python
from pathlib import Path
import yaml

document = yaml.safe_load(
    Path("docs/registry/implementation-map.yaml").read_text()
)
family = next(f for f in document["families"] if f["id"] == "scour")
calculations = sorted(e["symbol"] for e in family["implementations"]
                      if e["relation"] == "independent")
delegates = sorted(e["symbol"] for e in family["implementations"]
                  if e["relation"] == "delegate")
print(calculations)
print(delegates)
```

**Answer:** `critical_sediment_motion`, `monopile_scour_depth`,
`pipeline_scour_depth`, `rock_armour_thickness`; delegate `ScourWorkflow.router`.
Four calculations plus one delegate gives five callable members. The threshold
converter does not predict scour depth; the delegate does not add an independent
calculation. Result records do not increase that count.

The regression `test_scour_lookup_worked_example` checks these exact sets against
the source-based hand count. No numerical scour result or design acceptance is
implied by the lookup.

## Sources and related defects

The source bodies named by the YAML are the primary evidence for membership,
roles and method descriptions. Existing citations within those bodies are
recorded source claims; their empirical values and standard attributions have
not been independently qualified by this inventory change. No numerical
coefficient, acceptance limit or new calculation is introduced.

For interpreting the statistical distinctions, the public NIST/SEMATECH
e-Handbook [Weibull section 1.3.6.6.8](https://www.itl.nist.gov/div898/handbook/eda/section3/eda3668.htm)
and [Gumbel section 1.3.6.6.16](https://www.itl.nist.gov/div898/handbook/eda/section3/eda366g.htm)
were opened on 2026-10-23. These describe distribution forms and parameters;
they do not qualify a particular fitting routine or its sampling assumptions.
The e-Handbook is an online reference without an edition year on these pages.
In particular, the zero-location river-current probability plot differs from
the location-estimating maximum-likelihood routine in the postprocessor.

Related issue 2617 concerns full-QTF variance counting. Current source uses
positive lags and excludes the constant diagonal; acceptance against independent
realizations was not rerun here. Related issue 2618 describes an earlier
fixed-location Weibull fit; current source estimates location. Both remain
`not_checked` in `other_defects` because inspection does not independently
establish issue resolution or numerical acceptance.

## Verification and further coverage

Run the implementation-map, search and family tests in `tests/docs`, and run
Ruff on the generator and changed tests. Tests require each new family, selected
reviewed members, live coordinates and digests, partial-coverage metadata, the
lookup answer and bounded regeneration behavior. They use source-based expected
sets and small hand-written search fixtures, without invoking numerical solvers.

Broader inventory reconciliation, conditioned design-wave construction,
finite-duration slow-drift extremes and independent qualification of legacy
empirical methods require further work. No defect is closed by this inventory.
