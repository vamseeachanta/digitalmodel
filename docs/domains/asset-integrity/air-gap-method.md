# Supplied-data air-gap arithmetic

## Summary

The API calculates geometric clearance and compares it with a supplied minimum.
It reports the first sampled breach of that minimum. It does not establish a
complete fixed-platform assessment or select design-basis criteria.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-28 | Define supplied-data subtraction and sampled comparisons with independent arithmetic checks. |

## API and datum contract

`digitalmodel.asset_integrity.air_gap` supplies immutable typed records.
`Elevation(value_m, datum)` describes an elevation positive upward, in metres.
`AirGapBasis(deck_underside, still_water, crest_height_m, required_clearance_m)`
holds the fixed basis. Both elevations require exactly matching, nonblank datum
labels; no datum conversion or label normalization is performed. Matching labels
are a caller assertion, not independent evidence of survey compatibility.
Crest height is measured upward from the supplied still-water elevation.

`calculate_air_gap(basis, subsidence_m)` uses supplied downward deck movement:

```text
clearance = deck underside - subsidence - still water - crest height
margin = clearance - required clearance
meets_required_clearance = clearance >= required clearance
```

All quantities are in metres. Subsidence, crest height and required clearance
must be nonnegative; elevations may be negative. Missing, nonnumeric and
nonfinite quantities are rejected, including nonfinite derived results.
Equality meets the supplied minimum. Translating both elevations by the same
offset leaves the subtraction unchanged in exact arithmetic; results use binary
floating-point arithmetic and carry its rounding limits.

`evaluate_sampled_air_gap(basis, samples)` accepts a nonempty sequence of
`SubsidenceSample(date_label, subsidence_m)` records. Each annual date label is
an integer paired with its value. Labels must strictly increase; duplicates and
unordered inputs raise `ValueError`. Inputs are never sorted. Subsidence values
need not increase. The result contains ordered `samples` and
`first_sampled_breach`, the first supplied label with clearance below the minimum,
or `None` if no supplied sample breaches it.

## Independent synthetic hand calculation

The example is defined solely for subtraction checks: deck underside D = 10 m,
still water W = 2 m, crest C = 3 m and required clearance = 1 m, with one
shared synthetic datum. The table evaluates D - S - W - C and then subtracts
the supplied minimum; none of these numbers is an environmental or code criterion.

| Annual label | Supplied S (m) | Clearance calculation (m) | Margin calculation (m) | Meets supplied minimum |
| --- | --- | --- | --- | --- |
| 2000 | 0 | 10 - 0 - 2 - 3 = 5 | 5 - 1 = 4 | Yes |
| 2001 | 2 | 10 - 2 - 2 - 3 = 3 | 3 - 1 = 2 | Yes |
| 2002 | 4 | 10 - 4 - 2 - 3 = 1 | 1 - 1 = 0 | Yes |
| 2003 | 5 | 10 - 5 - 2 - 3 = 0 | 0 - 1 = -1 | No |

The first sampled breach is labelled 2003. A continuous crossing date is not
inferred. The tests assert these independently written values and use synthetic
offsets and boundary probes solely to check input and coordinate contracts.

## Supplied-data limitations

Deck elevation, still water, crest height, subsidence and minimum clearance
require an independently established basis. Labels identify samples only: no
calendar-year duration convention, interpolation, forecast rate or subsidence
model is introduced. No uncertainty allowance is inferred. A result above the
supplied minimum establishes only this geometric comparison.

Wave-in-deck loads, environmental selection, code criteria, raising or remediation
advice and economic decisions remain outside this API. The offering catalog and
public capability status remain unchanged. Issue #3443 remains open for those
independently sourced parts and broader platform qualification.
