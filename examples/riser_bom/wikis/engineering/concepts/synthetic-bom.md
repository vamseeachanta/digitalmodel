---
code_id: SYNTHETIC-BOM
publisher: Synthetic fixture
revision: "1"
---
# Synthetic BOM arithmetic

The quantity schedule and rates below are invented solely to exercise arithmetic.
No procurement record, indexed curve or normalised real cost basis was used.
TEST is a fictional currency and has no exchange rate.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Define invented quantities and prime-number rates for reproducible tests. |

Table 1. Synthetic basis revision.

## Invented arithmetic

The installed length is 25 m, joint length is 10 m, coated length is 25 m,
buoyancy module count is 4, ancillary count is 2 and installation duration is
1.5 days. These are test inputs, not engineering recommendations.

Rates are the first five primes: 2 TEST/joint, 3 TEST/m of coating,
5 TEST/module, 7 TEST/item and 11 TEST/day.
The least whole joint count covering 25 m is ceil(25/10) = 3.
The independent sum is 3*2 + 25*3 + 4*5 + 2*7 + 1.5*11 = 131.5 TEST.
Changing the count to 6 modules increases cost by (6-4)*5 = 10.0 TEST.

The quantity-times-unit-rate method follows OpenStax, *Prealgebra 2e* (2020),
section 5.6, Unit Rate and Unit Price:
[public textbook section](https://openstax.org/books/prealgebra-2e/pages/5-6-ratios-and-rate).
The textbook supplies the arithmetic method, not the invented rates or quantities.
Whole-joint upward rounding is the example's declared procurement convention.
