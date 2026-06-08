# Wall Thickness — Multi-Code Quick Check (DNV + API)

A minimal, deterministic "quick calc" example: runs a single representative
pipeline case through **two design codes** (DNV-ST-F101 and API RP 1111) and
emits a short text verdict plus a self-contained HTML report.

This is the EXECUTE artifact for a Deckhand "quick calc" path (deckhand#170).
A downstream allowlist references exactly one stable command (below) and scrapes
the printed `ARTIFACT:` line for the report attachment.

It wraps the existing `digitalmodel` wall-thickness engine
(`digitalmodel.structural.analysis.wall_thickness` and
`...wall_thickness_comparison.compare_codes`) — no engineering is reimplemented.

## Representative case (all inputs are echoed in the report)

| Quantity            | Value                              |
|---------------------|------------------------------------|
| Line                | 12-inch export line (OD 323.9 mm)  |
| Wall thickness      | 28.575 mm (1.125 in)               |
| Corrosion allowance | 3 mm                               |
| Material            | X65 (SMYS 448 MPa, SMTS 531 MPa)   |
| Internal pressure   | 150 bar (15.0 MPa)                 |
| Water depth         | 1500 m (external ≈ 15.08 MPa)      |
| Codes               | DNV-ST-F101 (2021), API RP 1111    |

Result: both codes **PASS**; the check is governed by DNV propagation buckling
at utilisation ≈ 0.719 (DNV is the more conservative code here).

## Stable command (offline, deterministic — use this in the allowlist)

```bash
PYTHONPATH=src uv run python \
    examples/structural/wall_thickness_quickcheck/quick_check.py --from-cache
```

- `--from-cache` (default-safe for CI): loads the committed JSON fixture at
  `data/quickcheck_cache.json`. **No engine call, no network.** Deterministic.
- The final stdout line is `ARTIFACT: <absolute path to the HTML report>`.
- Default report path:
  `examples/structural/wall_thickness_quickcheck/output/wall_thickness_quickcheck.html`
  (override with `--output <path>`).

## Refreshing the cache (when the engine changes)

```bash
PYTHONPATH=src uv run python \
    examples/structural/wall_thickness_quickcheck/quick_check.py --compute
```

`--compute` runs the live engine and rewrites `data/quickcheck_cache.json`.
The test `tests/test_wall_thickness_quickcheck_example.py::test_cache_matches_fresh_compute`
guards against a stale fixture.

## Test

```bash
PYTHONPATH=src uv run pytest tests/test_wall_thickness_quickcheck_example.py -q
```
