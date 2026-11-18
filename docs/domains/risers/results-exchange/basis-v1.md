# Results-exchange screening basis, version 1

This record identifies assumptions used by the exporter and its example
manifest. It supplies a versioned, repository-relative reference for enforced
factors; it does not establish qualified design inputs or standard compliance.
No retrievable public evidence for the study-specific inputs below is in hand.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-25 | Record screening assumptions separately from supported calculation references. |

## Strength screening inputs

The exporter enforces functional `C_f=1` and extreme `C_f=1.2` for this screening
exchange. Both are **assumed, based on previous experience**. The extreme factor
is not established by the repository strategy or a retrieved standard. Treating
it as a verified code factor is unsupported; engineering qualification requires
an applicable, reviewed design basis. The emitted `design_case_factor_source`
points to this section and `design_case_factor_status` records the assumption.
This version's values remain fixed; changed factors require a new basis version
and a corresponding exporter change.

| Quantity | Value or scope | Evidence status |
| --- | --- | --- |
| Basic allowable formulation | `C_f * (2/3) * SMYS` | Repository implementation in `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py`, `ApiRp2rdStrategy.C_A`; legacy WSD label only. |
| Functional and extreme factors | 1 and 1.2 | Assumed, based on previous experience; enforced screening choices. |
| Example SMYS | 450 MPa | Assumed, based on previous experience; not a material certificate. |
| Example outside diameter, nominal wall, checked wall | 273.1 mm, 25.6 mm, 19.4 mm | Assumed, based on previous experience; no qualified section or deduction basis is supplied. |
| Example extreme allowable | 360 MPa | Derived from the assumed SMYS and extreme factor; not an independent design limit. |

The README's frozen row describes retained input identity, not validated origin.
Actual manifests retain their full row and input digests; those digests establish
byte identity and do not qualify the assumptions or their applicability.

## Fatigue screening inputs

The [public FAT80 comparison](https://strathprints.strath.ac.uk/52862/),
section 3.2.1 and Figure 16, is the cited basis for range 80 MPa at two million
cycles with slope 3. This public benchmark does not establish the geometry,
material strength, extreme factor, environmental probabilities or design life.

| Quantity | Value or scope | Evidence status |
| --- | --- | --- |
| S-N continuation | One slope, no endurance cutoff | Assumed, based on previous experience; screening extrapolation of the benchmark. |
| Example SCF | 1.2 | Assumed, based on previous experience; no local stress qualification. |
| Thickness correction | Unity multiplier on stress range | Assumed, based on previous experience; no thickness qualification. |
| Mean stress and environment | No mean-stress benefit; air-equivalent damage | Assumed, based on previous experience; weld and environment treatment unqualified. |
| Annual hours | 8760 | Assumed, based on previous experience; occurrence is an annual fraction without renormalisation. |
| Example stationary duration and period | 3600 seconds, from 150 to 3750 seconds | Assumed, based on previous experience; no duration convergence evidence. |
| Example sea height, period, direction, occurrence, wave/current and offset | Values in the README manifest | Assumed, based on previous experience; no public metocean distribution or load-basis reference is supplied. |
| Selected seas | Three identifiers in the README example | Assumed, based on previous experience; their joint annual coverage is not established by a complete occurrence table. |
| Contact tolerance and fibre sampling | README examples | Extraction choices; convergence remains unverified. |

Native readback, stress-section reconciliation, convergence and independent
engineering review remain prerequisites for qualified use. Reciprocal benchmark
damage is not qualified service life.
