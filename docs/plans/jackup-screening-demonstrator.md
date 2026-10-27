# Generic jack-up screening demonstrator plan

A reproducible public-data example will combine spudcan penetration, preload,
static equilibrium and an illustrative operating envelope. It will represent a
generic three-leg unit in the Gulf of Mexico. It will not establish a verified
site-specific assessment or explain an incident.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-27 | Define the demonstrator and its evidence limits before implementation. |

## Scope and methods

- Place the geotechnical calculation beside the existing bearing-capacity code.
- Implement the research formulation in [Hu et al. (2015), equations 4–14](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf), with its stated geometry and layer-thickness bounds. Distinguish analytical peak and clay branches from illustrative connecting segments.
- Compare selected numerical predictions and observations with [Hossain et al. (2019)](https://api.research-repository.uwa.edu.au/ws/portalfiles/portal/45701479/hossaintxt.pdf). Retain disagreements; do not tune parameters to measured depths.
- Check the uniform-clay and uniform-sand limits against independent closed-form evaluations. Reject nonfinite, out-of-domain and incomplete inputs.
- Derive anonymous aggregate particulars from the supplied public specification collections. Label spacing, lightship, preload, area and environmental coefficients as illustrative assumptions with their bases.
- Use the existing NOAA, ERA5 and GEBCO client interfaces where qualified public input is available. An observed maximum is not a return level; missing records, unauthenticated access limitations and unparseable bathymetry remain explicit gaps.
- Solve three-support vertical equilibrium, loss of contact, sliding and preload exceedance under illustrative environmental forces. Use the existing marine operation envelope API only with explicitly illustrative response inputs.

## Inputs and rights

Freeze numerical inputs and their SHA-256 digests. Keep source copies and research
collections outside the repository. Commit selected facts with citations, not
reproduced tables, source prose, specification names or copyrighted figures.
Public availability alone does not establish an unrestricted reuse licence.
The Hossain accepted manuscript states CC BY-NC-ND; use selected numerical facts
and original calculations only. Do not access standards documents or licensed
solvers. Treat the public settlement study and shelf strength curves as context
unless their numerical inputs are independently qualified for the calculation.

## Outputs

Deliver a calculator, deterministic tests, frozen anonymous input data, a worked
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
