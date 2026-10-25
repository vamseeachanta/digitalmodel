# Synthetic river profile worked example

This mathematical demonstration calls `power_law_profile` and
`power_law_depth_averaged` with synthetic inputs. It is not a measured river
and does not qualify the full module. The parent report issue remains open.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-25 | Added the standalone linear-profile demonstration and independent hand integral to supply a worked example. |

## Hand calculation

S1 is the explicitly synthetic prescription: depth H = 4.0 m, surface speed
u_s = 2.0 m/s, exponent_m = 1.0 (dimensionless), and sample heights above bed
z = (0.0, 2.0, 4.0) m. These selected inputs are the source of the numerical
constants in the builder and readback tests; no external data is needed.

C1 is the independent hand calculation:

- Substitute S1 into u(z) = u_s (z/H)^(1/exponent_m):
  u(z) = (2.0 m/s)(z / (4.0 m)) = z / (2.0 s).
- The velocities at the selected heights are (0.0, 1.0, 2.0) m/s.
- Integrate the triangle: area = (4.0 m)(2.0 m/s)/2 = 4.0 m²/s.
- Divide area by depth: mean = (4.0 m²/s)/(4.0 m) = 1.0 m/s.

The triangle factor is the geometric area formula, independent of the
depth-average helper. Rendered helper values are checked against C1, rather
than against values obtained from the implementation under test.

## Build and check

Run from the repository root in an environment with the module dependencies
(NumPy and SciPy) and test dependencies (pytest and PyYAML) available:

```sh
PYTHONPATH=src uv run --no-project --with pytest python docs/reports/modules/river_currents/build_worked_example.py --output-dir "$TMPDIR/worked-example"
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/hydrodynamics/river_currents -q -rA -p no:cacheprovider
```

For an isolated environment lacking those dependencies, add
`--with numpy --with scipy --with pyyaml` to the `uv run` commands.
Dependency resolution may need network access; the builder itself uses no
network, external input data, licensed software or large computation. Once
dependencies are cached, `uv run --offline` can execute the same commands.

The required `--output-dir` receives only `worked-example.html`. The builder
does not execute on import, update shared indexes or include a build timestamp
or local paths. Generated HTML stays outside version control. Tests launch the
builder as a subprocess from a temporary directory, read back values and units,
compare rebuild bytes within and across output directories, check import side
effects and ensure every delivered file is not ignored.

## Limits and follow-up

- bank effects: NOT_EVALUATED
- force calibration: NOT_EVALUATED
- extreme-value fitting: NOT_EVALUATED

Natural-river applicability and engineering design suitability remain
unevaluated. Full-module review, the complete report and shared index entries
remain follow-up work.
