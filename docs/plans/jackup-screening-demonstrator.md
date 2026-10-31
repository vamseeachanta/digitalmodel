# Generic jack-up screening demonstrator plan

A reproducible public-data example will combine spudcan penetration, preload,
static equilibrium and an illustrative operating envelope. It will represent a
generic three-leg unit in the Gulf of Mexico. It will not establish a verified
site-specific assessment or explain an incident.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-27 | Define the demonstrator and its evidence limits before implementation. |
| 1 | 2026-10-27 | Record the circular reference and environmental extension, replace unsupported aggregate claims with synthetic particulars, and specify independent recovery and checkout checks. |
| 2 | 2026-10-28 | Replace researcher attributions with neutral source identifiers while retaining public citations and rights limitations. |

## Plan-lite

- Scope: generic penetration and static screening, circular homogeneous-soil arithmetic, frozen observation extracts and environmental sensitivities; no installation acceptance.
- Changes: document each constant and restriction by its basis, use synthetic particulars where collection traceability is unavailable, preserve digest-bound checkout bytes, and supply independent curve/recovery expectations.
- Validation: run geotechnical, input, statics, worked-example, mudmat and existing operation-envelope tests; replay frozen inputs; check source digests, file hygiene, structure and publication text directly.
- Limits: retain the field false negative, illustrative connecting segments, static recovery, unqualified ERA5/depth assumptions and marginal extrapolations; independent engineering qualification remains open.

## Scope and methods

- Place the geotechnical calculation beside the existing bearing-capacity code.
- Implement the research formulation in [Research source R2015: Spudcan penetration analyses for three case histories in sand overlying clay (2015), equations 4–14](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf), with its stated geometry and layer-thickness bounds. Distinguish analytical peak and clay branches from illustrative connecting segments.
- Compare selected numerical predictions and observations with [Research source R2019: Measured and Calculated Spudcan Penetration Profiles for Case Histories in Sand-over-Clay (2019)](https://api.research-repository.uwa.edu.au/ws/portalfiles/portal/45701479/hossaintxt.pdf). Retain disagreements; do not tune parameters to measured depths.
- Check uniform clay and the circular flat-base sand reference against independent closed forms. Use the cited 1961 circular-plate factors and 1970 bearing factors recorded in the domain documentation; do not use an equal-area square or extrapolate the layered calibration. Reject nonfinite, out-of-domain and incomplete inputs.
- Use synthetic particulars assumed, based on previous experience, when a retrievable anonymous collection provenance/reuse record is unavailable. Label dimensions, spacing, mass, preload, footing area and environmental coefficients explicitly; make no collection-statistic claim.
- Use the existing NOAA, ERA5 and GEBCO client interfaces where qualified public input is available. An observed maximum is not a return level; missing records, unauthenticated access limitations and unparseable bathymetry remain explicit gaps.
- Solve three-support vertical equilibrium, loss of contact, sliding and preload exceedance under illustrative environmental forces. Use the existing marine operation envelope API only with explicitly illustrative response inputs.

## Inputs and rights

Freeze numerical inputs and their SHA-256 digests. Keep source copies and research
collections outside the repository. Commit selected facts with citations, not
reproduced tables, source prose, specification names or copyrighted figures.
Public availability alone does not establish an unrestricted reuse licence.
The R2019 accepted manuscript states CC BY-NC-ND; use selected numerical facts
and original calculations only. Do not access standards documents or licensed
solvers. Treat the public settlement study and shelf strength curves as context
unless their numerical inputs are independently qualified for the calculation.

## Outputs

Deliver a calculator, deterministic tests, frozen synthetic and cited input data, a worked
example and a domain documentation page. An original load–penetration PNG and its
reproduction script belong outside the repository. Any public awareness text
requires the maintainers' editorial review.

## Verification

Run the focused pytest suites with bytecode and pytest caching disabled and all
temporary output outside the repository. Check digests, closed forms, published
numerical comparisons, force/moment equilibrium, invalid inputs and the example.
Run applicable repository file checks directly. Record every unavailable input,
check and remaining engineering qualification in the handoff.

## Publication limit

Issue creation is prohibited by the execution instructions. This repository plan
records the requested issue scope; the maintainers must create or select an issue
before adding a reference to the pull request.
