# Hydrostatic weight moment balance

This runnable teaching example checks a publicly cited weight addition and
removal case against independent moment arithmetic. Supplemental synthetic
weights demonstrate addition, removal, a vertical shift, and inverse add/remove
operations. Neither case represents measured loading data.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-27 | Added synthetic moment balances and an executable example to cover removal, vertical shifting, and inverse operations. |
| 2 | 2026-10-27 | Added a cited worked case and independent arithmetic to establish numerical input provenance while retaining the synthetic coverage. |

Table 1. Revision history of the instructional example.

## Basis

The weight moment equation is given in USNA, *EN400 Principles of Ship
Performance*, Summer 2020, section 3.2.2.1, "Weighted Average Technique",
page 3-10. The cited inputs below come from Example 3.2, page 3-12 (PDF page 107),
in the [public course notes](https://www.usna.edu/NAOE/_files/documents/Courses/EN400/EN400_Course_Notes%2C_Summer_2020.pdf).
That edition was opened and its worked case checked on 2026-10-27. The tests
run without downloading the source.

Use long tons (LT) for weight and feet (ft) for vertical height above one fixed
datum. KG means vertical centre of gravity above that datum. Assume rigid,
concentrated weights and a positive remaining total weight.

## Cited worked case

Example 3.2 supplies an initial weight of 4092 LT at KG 18.9 ft, an addition of
200 LT at 10 ft, and a removal of 75 LT at 20 ft. These published numerical
inputs are retained at their stated precision. Applying the cited weight moment
balance gives the following independent hand calculation:

```text
W_new = 4092 - 75 + 200 = 4217 LT
M_new = 4092 × 18.9 - 75 × 20 + 200 × 10 = 77838.8 LT ft
KG_new = 77838.8 / 4217 ft, which rounds to 18.5 ft
```

The source displays its moment as 77839 LT ft and its KG as 18.5 ft. The code
retains the unrounded moment rather than using that rounded intermediate as an
exact expectation. The test checks weight, moment, and KG independently, using
the exact ratio `389194 / 21085 ft` for KG and also checking agreement with the
source's displayed 18.5 ft answer.

```python
from digitalmodel.naval_architecture.hydrostatics import (
    vertical_cg_after_weight_change,
)

# USNA EN400, Summer 2020, section 3.2.2.1, Example 3.2, page 3-12.
reference_initial_weight_lt = 4092.0
reference_initial_kg_ft = 18.9
reference_removed_lt = 75.0
reference_removed_height_ft = 20.0
reference_added_lt = 200.0
reference_added_height_ft = 10.0

reference_remaining_lt = reference_initial_weight_lt - reference_removed_lt
reference_remaining_kg_ft = vertical_cg_after_weight_change(
    reference_initial_weight_lt,
    reference_initial_kg_ft,
    -reference_removed_lt,
    reference_removed_height_ft,
)
reference_weight_lt = reference_remaining_lt + reference_added_lt
reference_kg_ft = vertical_cg_after_weight_change(
    reference_remaining_lt,
    reference_remaining_kg_ft,
    reference_added_lt,
    reference_added_height_ft,
)
reference_moment_lt_ft = reference_weight_lt * reference_kg_ft

print(f"Cited case: W={reference_weight_lt:.1f} LT, "
      f"M={reference_moment_lt_ft:.1f} LT ft, KG={reference_kg_ft:.1f} ft")
```

## Supplemental synthetic moment balance

The following components are chosen solely for instructional arithmetic; their
numerical values are not attributed to the cited source:

| Component | Weight (LT) | Height (ft) | Weight × height (LT ft) |
| --- | ---: | ---: | ---: |
| Lower component | 800 | 10 | 8000 |
| Upper component | 200 | 30 | 6000 |
| Total | 1000 | — | 14000 |

Table 2. Synthetic starting components; each moment is the displayed weight times height.

The starting moment sum is `M = 800 × 10 + 200 × 30 = 14000 LT ft`, so
`W = 800 + 200 = 1000 LT` and `KG = M / W = 14 ft`.

For a signed weight change `w` at height `z`, conservation of the weight moment
about the datum gives

```text
W_new = W + w
W_new × KG_new = W × KG + w × z
KG_new = (W × KG + w × z) / (W + w)
```

An addition has positive `w`; a removal has negative `w` and uses the weight's
original height. Each case below starts from the same synthetic initial state:

| Operation | Independent component moment sum (LT ft) | New total weight (LT) | New KG (ft) |
| --- | --- | ---: | --- |
| Add 100 LT at 30 ft | `800 × 10 + (200 + 100) × 30 = 17000` | 1100 | `17000 / 1100 = 170 / 11` |
| Remove the upper 200 LT at 30 ft | `800 × 10 = 8000` | 800 | `8000 / 800 = 10` |
| Shift the upper 200 LT from 30 ft to 5 ft | `800 × 10 + 200 × 5 = 9000` | 1000 | `9000 / 1000 = 9` |

Table 3. Independent instructional arithmetic for each operation.

For the vertical shift, the moment change is `200 × (5 - 30) = -5000 LT ft`,
while total weight is unchanged. The existing function can implement this as
removal at the old height and addition at the new height, using the intermediate
800 LT at KG 10 ft. That intermediate total must remain positive.

The inverse of the addition removes the same 100 LT at the same 30 ft height:
`W = 1100 - 100 = 1000 LT`, `M = 17000 - 100 × 30 = 14000 LT ft`, and
`KG = 14000 / 1000 = 14 ft`. Conversely, adding the removed 200 LT back at 30 ft
gives `W = 800 + 200 = 1000 LT` and
`KG = (8000 + 200 × 30) / 1000 = 14 ft`. Both recover the initial state.

## Runnable synthetic example

Use an environment with the package dependencies available. From the repository
root, save either Python block outside the checkout, set `EXAMPLE_FILE` to its
saved path, and run `PYTHONPATH=src python "$EXAMPLE_FILE"`. The snippet tests
execute both exact blocks and compare the results with their independent moment
sums above.

```python
from math import isclose, ulp

from digitalmodel.naval_architecture.hydrostatics import (
    vertical_cg_after_weight_change,
)

# Synthetic inputs from Table 2; no measured or reference-example data.
initial_weight_lt = 1000.0
initial_kg_ft = 14.0
added_load_lt = 100.0
upper_load_lt = 200.0
upper_height_ft = 30.0
shift_height_ft = 5.0

added_weight_lt = initial_weight_lt + added_load_lt
added_kg_ft = vertical_cg_after_weight_change(
    initial_weight_lt, initial_kg_ft, added_load_lt, upper_height_ft
)

removed_weight_lt = initial_weight_lt - upper_load_lt
removed_kg_ft = vertical_cg_after_weight_change(
    initial_weight_lt, initial_kg_ft, -upper_load_lt, upper_height_ft
)

# Move the upper component: remove at its old height, add at its new height.
shifted_weight_lt = removed_weight_lt + upper_load_lt
shifted_kg_ft = vertical_cg_after_weight_change(
    removed_weight_lt, removed_kg_ft, upper_load_lt, shift_height_ft
)

recovered_after_add_weight_lt = added_weight_lt - added_load_lt
recovered_after_add_kg_ft = vertical_cg_after_weight_change(
    added_weight_lt, added_kg_ft, -added_load_lt, upper_height_ft
)

recovered_after_remove_weight_lt = removed_weight_lt + upper_load_lt
recovered_after_remove_kg_ft = vertical_cg_after_weight_change(
    removed_weight_lt, removed_kg_ft, upper_load_lt, upper_height_ft
)

# Four ulps cover rounding in these short arithmetic sequences.
for weight_lt, kg_ft in (
    (recovered_after_add_weight_lt, recovered_after_add_kg_ft),
    (recovered_after_remove_weight_lt, recovered_after_remove_kg_ft),
):
    assert weight_lt == initial_weight_lt
    assert isclose(kg_ft, initial_kg_ft, rel_tol=0.0, abs_tol=4 * ulp(initial_kg_ft))

# Display rounding only; all calculations and assertions use unrounded values.
for label, weight_lt, kg_ft in (
    ("Added", added_weight_lt, added_kg_ft),
    ("Removed", removed_weight_lt, removed_kg_ft),
    ("Shifted", shifted_weight_lt, shifted_kg_ft),
    (
        "Recovered after add",
        recovered_after_add_weight_lt,
        recovered_after_add_kg_ft,
    ),
    (
        "Recovered after remove",
        recovered_after_remove_weight_lt,
        recovered_after_remove_kg_ft,
    ),
):
    print(f"{label}: W={weight_lt:.1f} LT, KG={kg_ft:.1f} ft")
```

## Limits and remaining scope

This example checks weight and vertical moment accounting only. It does not
establish stability, free-surface corrections, equilibrium draft, trim, or a
qualified loading condition. The function returns KG only; the example tracks
total weight explicitly. No input-validation behaviour is changed.

The parent issue remains open. Broader provenance mapping, reference-corpus
fixtures, resistance, manoeuvring, propulsion, and hull-strength validation remain
outside this bounded example. The cited case establishes agreement for one
published loading calculation, not validation of those broader capabilities.
