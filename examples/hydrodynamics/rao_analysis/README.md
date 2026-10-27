# Synthetic RAO comparison

`basic_usage.py` passes analytic curves through `RAOAnalysis.add_trace_to_fig`
into an in-memory Plotly figure. The overlay illustrates comparison, including
how a prescribed amplitude reduction appears; it establishes no physical
shielding qualification. No measured data or native result files are used.

## Analytic basis

All numerical curve parameters are deliberately chosen synthetic inputs,
defined here rather than taken from a physical model or publication. Let
period `T` be in seconds and define amplitude ratios in metres per metre:

```text
A(T) = 1 / (1 + (T / 8 - 1)^2)
B(T) = (3/4) A(T)
T = 4, 8, 12, 16 seconds
```

The reference period is eight seconds, the numerator is unity, the offset is
unity, and the exponent squares the offset. The comparison is a prescribed
three-quarter multiple. The samples give these independent hand values:

| Period (s) | T/8 - 1 | A (m/m) | B (m/m) |
| --- | --- | --- | --- |
| 4 | -1/2 | 4/5 | 3/5 |
| 8 | 0 | 1 | 3/4 |
| 12 | 1/2 | 4/5 | 3/5 |
| 16 | 1 | 1/2 | 3/8 |

For example, at four seconds the denominator is `1 + (-1/2)^2 = 5/4`,
so `A = 4/5` and `B = (3/4)(4/5) = 3/5`. Tests compare every plotted
point with these fractions, independently of the curve implementation.
Straight lines join the samples; interpolation is only a visual aid.

## Run

From the repository root, in an environment with the existing RAO module
dependencies installed, choose an explicit destination outside the repository:

```sh
PYTHONPATH=src python examples/hydrodynamics/rao_analysis/basic_usage.py \
  --output "$TMPDIR/synthetic-rao.html"
```

The destination's parent directory must already exist. The entrypoint writes
one standalone HTML file with embedded Plotly JavaScript and opens no browser.
No default output is selected. Importing the example or calling `build_figure()`
writes nothing; the returned figure can be inspected directly.

The focused tests use pytest, NumPy, SciPy, pandas, Plotly and the existing
`assetutilities` dependency. With the sibling source checkout supplied by the
test configuration, the minimal extras used for this example are:

```sh
PYTHONPATH=src uv run --no-project --with pytest \
  --with 'numpy<2' --with 'scipy<2' --with 'pandas<3' \
  --with 'plotly==5.17.0' --with pyyaml --with 'deepdiff<8' \
  --with jinja2 --with loguru --with ruamel.yaml --with 'webcolors<2' \
  python -m pytest \
  tests/hydrodynamics/rao_analysis/test_synthetic_plot_example.py \
  tests/unit/hydrodynamics/test_rao_analysis.py \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/rao-example-tests"
```

The dependency bounds follow the repository runtime dependency declarations;
they are software versions, separate from the synthetic curve inputs.

## Limits

The prescribed scaling contains no geometry, wave heading, phase, diffraction,
radiation or interaction calculation. These curves cannot quantify shielding
or qualify a design. Physical seastate RAO comparisons remain separate work
under issue #16 and require a suitable data basis and engineering assessment.
