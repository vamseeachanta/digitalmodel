# Hydrostatic weight moment balance

This runnable teaching example uses explicitly synthetic weights to demonstrate
addition, removal, a vertical shift, and inverse add/remove operations. All
values and expected results are independent instructional arithmetic, not
transcribed reference examples or measured loading data.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-27 | Added synthetic moment balances and an executable example to cover removal, vertical shifting, and inverse operations. |

Table 1. Initial issue of the instructional example.

## Basis and moment balance

The method context is USNA EN400 Chapters 2–3, already identified in the
[hydrostatics module](../../src/digitalmodel/naval_architecture/hydrostatics.py).
That reference supplies method context only; none of the numerical values below
is attributed to it. No external corpus is needed to run this example.

Use long tons (LT) for weight and feet (ft) for vertical height above one fixed
datum. KG means vertical centre of gravity above that datum. Assume rigid,
concentrated weights and a positive remaining total weight. The starting
components are chosen solely for this example:

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

## Runnable example

Use an environment with the package dependencies available. From the repository
root, save this Python block outside the checkout, set `EXAMPLE_FILE` to its saved
path, and run `PYTHONPATH=src python "$EXAMPLE_FILE"`. The snippet test executes
this exact block and compares the results with the independent moment sums above.

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
outside this bounded example.
