# GEBCO elevation response admission

Missing elevation evidence must remain distinguishable from finite zero and
land. The response parser rejects missing, malformed and nonfinite elevations
while preserving finite elevation values and the existing depth conversion.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-27 | Record the bounded parser change and offline test plan. |

## Plan-lite

1. Bound the change to text response admission in `GEBCOClient.get_elevation`.
   Keep the endpoint, WMS parameters and finite depth conversion unchanged.
2. Parse candidate elevation tokens as floats. Continue past malformed lines;
   raise `ValueError` immediately for a parsed nonfinite value, or after all
   lines if no elevation is parsed. Let `get_depth` and `query_by_date`
   propagate the error rather than replacing missing evidence with zero.
3. Replace only HTTP transport in offline tests, retaining the real parser.
   Cover missing and malformed text, signed NaNs and infinities, float
   overflow, finite zero, land, negative elevations and date-query propagation.
   Check finite depths against the independent hand calculations beside the
   synthetic test inputs, then run the neighboring regression tests.

## Offline test command

Run from the repository root with `TMPDIR` set to an external temporary
directory and `uv` resolving to the available executable:

```sh
PYTHONPATH=src UV_NO_BUILD=1 RUN_LIVE_METOCEAN=0 uv run --no-project \
  --with pytest --with pytest-asyncio --with requests --with numpy \
  --with pyyaml --with scipy python -m pytest \
  tests/data_systems/test_gebco_elevation_contract.py \
  tests/data_systems/test_cache_manager.py \
  tests/data_systems/test_config_loader.py \
  tests/workflows/integration/test_metocean_streaming.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/gebco-regression"
```

## Limits

Synthetic responses establish parser behavior. Live service compatibility,
dataset-version integration, coordinate validation and interpretation of finite
numeric NoData markers require separate work.
