# Seawater resistivity from practical salinity and temperature

The surface-pressure PSS-78 inverse provides conductivity in S/m and its
reciprocal, resistivity in ohm.m. Inputs outside the cited validity range raise
`ValueError`. The chart data are computational inputs for later review; no report
page or site-specific seawater range is established here.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| A | 2026-10-26 | Add the cited relation, chart CSV and numerical checks so resistivity can be evaluated from salinity and temperature. |
| B | 2026-10-26 | Remove personal names from citations while retaining organizations and publication details. |

## Sources and method

- UNESCO (1981), *The Practical Salinity Scale 1978 and the International Equation
  of State of Seawater 1980*, Technical Papers in Marine Science 36. This original
  reference is identified in the TEOS-10 notes, p. 2; its original PDF was not
  accessible. The implementation uses the same PSS-78 relation as reproduced in
  the accessible computational reference below, rather than a different relation.
- UNESCO (1983), *Algorithms for computation of fundamental
  properties of seawater*, Technical Papers in Marine Science 44,
  [open institutional copy](https://repository.oceanbestpractices.org/handle/11329/109).
  Printed pp. 6-8, equations (1)-(4), define the conductivity ratios, coefficients,
  temperature correction and validity range. Printed p. 9 describes numerical
  inversion and gives the check table; printed p. 13 gives additional tabulated
  salinities. These correspond to PDF pages 10-13 and 17. The scanned pages were
  inspected directly; the repository's text extraction contains no usable text.
- TEOS-10 (2011), [*Notes on the GSW function gsw_SP_from_C*](https://www.teos-10.org/pubs/gsw/pdf/SP_from_C.pdf),
  p. 1: standard conductivity at practical salinity 35, IPTS-68 temperature
  15 degrees C and zero sea pressure is 42.914 mS/cm, or 4.2914 S/m. The notes cite
  IEEE (1980), *Determination of the Concentration of Potassium
  Chloride Solution Having the Same Electrical Conductivity, at 15 C and Infinite
  Frequency, as Standard Seawater of Salinity 35.0000 (Chlorinity 19.37394)*,
  IEEE Journal of Oceanic Engineering 5, pp. 22-23. The absolute conductivity
  reference is taken from the accessible TEOS-10 notes, not independently measured.

The source responses used above were retrieved successfully on 2026-10-26 UTC.
Retrieval addresses, UTC times, HTTP statuses and response bytes are retained in
scratch evidence. The original UNESCO catalogue returned HTTP 403, and candidate
PDF locations returned HTTP 404; neither is used as numerical evidence.

The module is `digitalmodel.cathodic_protection.seawater_resistivity`:

```python
from digitalmodel.cathodic_protection.seawater_resistivity import (
    seawater_conductivity_s_m,
    seawater_resistivity_ohm_m,
)

conductivity = seawater_conductivity_s_m(35, 15)
resistivity = seawater_resistivity_ohm_m(35, 15)
```

At surface pressure, the pressure ratio is unity and `R = R_t * r_t(t)`.
The implementation bisects the polynomial in `sqrt(R_t)` and multiplies the
resulting ratio by the standard conductivity. Bisection replaces the sample
Newton iteration on printed pp. 9 and 12; it changes the numerical solver only.
The bracket and iteration count are numerical choices, not physical constants.
Resistivity follows from the SI identity `rho = 1 / conductivity`.

## Validity and temperature scale

The enforced working bounds are practical salinity **2 through 42**, and
temperature **-2 through 35 degrees C on IPTS-68**, following UNESCO (1983),
printed pp. 7-8. Endpoints are included; no extrapolation is permitted. The source
also writes strict salinity inequalities beside the polynomial. Endpoint checks
exercise its stated working bounds, without implying validity beyond them.

Practical salinity is dimensionless. `salinity_psu` is conventional API notation;
it is not a mass fraction or Absolute Salinity. Temperatures on ITS-90 must be
converted before calling this API. The TEOS-10 notes, Appendix E, describe this
distinction. Pressure is fixed at zero sea pressure, corresponding to one standard
atmosphere, as defined on UNESCO printed p. 6. No subsurface prediction is made.

## Chart data and reproduction

[`data/seawater-resistivity.csv`](data/seawater-resistivity.csv) contains one
salinity column and six resistivity curves. The requested illustration samples
salinities 30 through 40 at integer intervals, at IPTS-68 temperatures 5, 10, 15,
20, 25 and 30 degrees C. These are requested plotting choices within the cited
domain, not a measured environmental envelope. Column names give temperatures
in degrees C and resistivity in ohm.m. Values retain eight decimal places for
computational reuse, without claiming corresponding physical accuracy.

The CSV can be regenerated from the repository root in a Python environment
with the package's CP import dependencies (`pydantic` and `scipy`):

```bash
PYTHONPATH=src uv run --no-project --with pydantic --with scipy python - <<'PY'
import csv
from pathlib import Path
from digitalmodel.cathodic_protection.seawater_resistivity import (
    seawater_resistivity_ohm_m,
)

temperatures = (5, 10, 15, 20, 25, 30)
path = Path("docs/domains/cathodic_protection/data/seawater-resistivity.csv")
with path.open("w", newline="", encoding="utf-8") as stream:
    writer = csv.writer(stream, lineterminator="\n")
    writer.writerow(["salinity_psu", *(
        f"resistivity_{t}_c_ohm_m" for t in temperatures
    )])
    for salinity in range(30, 41):
        writer.writerow([salinity, *(
            f"{seawater_resistivity_ohm_m(salinity, t):.8f}"
            for t in temperatures
        )])
PY
```

## Comparison with the existing hand table

[`literature.md`](literature.md), Design Data / Salinity, lists conductivity and
resistivity at numerical salinities 0.1, 1, 10, 25 and 31, labels salinity as ppt,
and explicitly leaves temperature unknown. The first two rows lie outside the
new relation's validity range. The remaining rows are compared as numerical
practical salinities for illustration only; the original salinity scale is
unconfirmed. The file is unchanged.

The comparison below assumes IPTS-68 temperatures of 15 and 25 degrees C to
expose the missing temperature basis. Difference means
`100 * (calculated resistivity / tabulated resistivity - 1)`.
Source values are retained at their printed precision; calculated values and
differences are rounded for display.

| Numerical salinity | Hand table, ohm.cm | Calculated at 15 degrees C, ohm.cm | Difference, % | Calculated at 25 degrees C, ohm.cm | Difference, % |
|---|---|---|---|---|---|
| 10 | 58.75 | 73.0 | +24.2 | 58.7 | 0.0 |
| 25 | 25.47 | 31.5 | +23.8 | 25.5 | 0.0 |
| 31 | 21.00 | 26.0 | +23.7 | 21.0 | 0.0 |

At the assumed 25 degrees C the unrounded differences are within 0.1%; the bound
is rounded upward. This agreement does not establish the hand table's missing
temperature or convert ppt to practical salinity. The printed conductivity and
resistivity columns also satisfy the independent unit identity
`rho [ohm.cm] = 1000 / C [mS/cm]` to their rounding precision.

## Numerical checks and review status

`tests/cathodic_protection/test_seawater_resistivity.py` checks the standard
reference, the in-domain surface rows of UNESCO printed p. 13, and the printed
p. 9 check factors after reducing the depth examples to their surface ratios.
It does not treat depth conductivities as surface values. Table comparisons
account for the rounding of the published salinities and ratios.

A separate Decimal evaluation of the published forward equations checks the
inverse across the validity bounds and every CSV cell. It uses independently
transcribed coefficients and does not obtain expected salinities from the
inverse under test. Additional checks reject non-finite and out-of-range inputs
and compare the existing hand table without treating it as validation evidence.

Numerical checks passed. Independent technical review is pending; these values
must not be incorporated into a reviewed report or its value manifest until
that review passes. No report page is built here.
