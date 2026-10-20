# Metocean threshold exposure statistics

## Summary

`digitalmodel.data_systems.metocean_statistics.hours_below_threshold` adapts
in-memory metocean records to the optional `metocean-stats` backend. It reports
observed hours strictly below each supplied threshold, grouped by UTC sample year.
It provides the exposure-counting part of issue #384.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-18 | Added a sourced counting adapter to introduce statistical backend integration. |

## API mapping

Install the `metocean` extra: `pip install 'digitalmodel[metocean]'`.
The extra pins the backend to the tested published release, `metocean-stats==1.2.0`.
The adapter imports the backend only when called. The test extra also includes
the backend so the direct-call regression tests run in the test environment.

| Adapter input or output | Backend equivalent |
| --- | --- |
| Iterable of records with `timestamp` and the selected field | In-memory pandas DataFrame with a UTC DatetimeIndex |
| `parameter` | `var` |
| `thresholds` | `thr_arr` |
| Nested mapping: threshold, year, hours | Rows of `stats.general.nb_hours_below_threshold`, with columns in ascending year order |

A record stream from `MetoceanClient.query_metocean` can be passed directly if its
selected field is complete and its timestamps are regular. Explicitly select the
field name emitted by the provider; no field aliases or unit conversions occur.
The adapter retains timestamps and selected values in memory, so callers must
bound the requested time range.

```python
from digitalmodel.data_systems.metocean_statistics import hours_below_threshold

# records: an iterable with datetime timestamps and numeric observations
# limits: caller-supplied thresholds in the same units as wave_height
exposure = hours_below_threshold(
    records,
    parameter="wave_height",
    thresholds=limits,
)
```

## Counting basis

For regular spacing, the backend computes

`H(year, threshold) = interval_hours * count(value < threshold in year)`.

Counting observations satisfying a condition follows the frequency definition in
[NIST/SEMATECH e-Handbook of Statistical Methods (2012), section 1.3.3.14](https://www.itl.nist.gov/div898/handbook/eda/section3/histogra.htm).
Multiplication by the represented interval converts that count to observed
exposure. The time conversion is defined in
[BIPM SI Brochure, ninth edition (2019), section 4, table 8](https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf):
an hour is 3600 seconds. The adapter delegates counting and unit conversion to
[the backend function](https://github.com/MET-OM/metocean-stats/blob/3f99e50bf08cbf0ec47370c0a8eee349b0226b85/metocean_stats/stats/general.py).
No empirical coefficients or default engineering acceptance limits are introduced.

Every observation represents one complete sample interval, including the final
observation. Each interval is assigned wholly to the UTC year of its sample
timestamp, without splitting at a year boundary. This matches the backend;
it is not elapsed time between the first and last observation. Naive datetimes
are interpreted as UTC; aware datetimes are converted to UTC. Mixing the two
forms is rejected. Threshold equality is excluded.

At least two observations are needed to infer an interval. Duplicate, reversed,
irregular or missing timestamps and missing, nonnumeric or nonfinite values are
rejected. Missing observations are not dropped or interpolated. Provider sentinel
values must be resolved by the caller before analysis. Thresholds must be finite,
distinct and nonempty. Values and thresholds have caller-defined matching units.

## Test basis

The tests contain independent hand calculations, alongside a direct backend
comparison. Synthetic inputs are test data, not design criteria:

- Half-hour samples `[1, 2]` in one year and `[3, 1]` in the next give one sample
  below threshold `2` in each year: `1 * 0.5 = 0.5` hours per year.
- At threshold `3`, the same data give `2 * 0.5 = 1.0` hours and
  `1 * 0.5 = 0.5` hours respectively.
- Two-hour samples `[1, 2, 4, 3]` give `3 * 2 = 6.0` hours below threshold `4`.
- Zero and complete exposure, UTC year grouping, invalid data and optional
  dependency failures are covered separately.

## Limits and later slices

Hours below a threshold do not establish a contiguous operating window. No
minimum operation duration, waiting time or seasonal operability is computed.
Partial years remain partial; the result is not scaled to a full year. There is
no quality qualification, uncertainty estimate or engineering acceptance result.

Later slices can add duration-qualified weather windows, exposure-aware extreme
value analysis, joint models, environmental contours, real observation examples
and reports. Integration with the unified data acquisition interface in the
separate data repository remains follow-up work; this adapter consumes the local
procurement record shape.
