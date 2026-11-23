# Analytic oscillator comparison

`basic_usage.py` passes analytic curves through `RAOAnalysis.add_trace_to_fig`
into an in-memory Plotly figure. The overlay compares a damped oscillator
response with its static reference; it establishes no physical shielding
qualification. No measured data or native result files are used.

## Analytic basis

The parameter source is [Lamar University, Differential Equations,
Mechanical Vibrations, Examples 5 and 6](https://tutorial.math.lamar.edu/Classes/DE/Vibrations.aspx).
Example 5 gives mass `m = 3 kg`, stiffness `k = 75 N/m`, forcing amplitude
`F0 = 10 N` and forcing angular frequency `omega_f = 5 rad/s`. Example 6
adds damping `c = 90 N s/m` and obtains the steady response
`u_p(t) = (1/45) sin(5t) m`. These are published oscillator inputs, not
wave or structure properties.

For the same oscillator, harmonic substitution into the source's equation
`m u'' + c u' + k u = F0 cos(omega t)` gives the following derived magnitude.
Divide displacement amplitude by static deflection `F0/k = 2/15 m`:

```text
D(omega) = |F0 / (k - m omega^2 + i c omega)| / (F0/k)
         = 75 / sqrt((75 - 3 omega^2)^2 + (90 omega)^2)
S(omega) = 1
```

`S` is the static reference, not a second dynamic solution. No arbitrary
amplitude multiplier or reference period is used. The ordinate is displacement
per static deflection in metres per metre, not displacement per incident wave
amplitude. The abscissa is angular frequency in radians per second; zero denotes
the static limit, not a finite-period oscillation.

The evaluation grid uses the static limit and the first three integer multiples
of the published forcing frequency: `omega = 0, 5, 10, 15 rad/s`. This grid is
an illustration choice derived from `omega_f`, not measured data or additional
frequencies claimed to be worked in the source. The following independent hand
values follow from the real and imaginary parts of the dynamic stiffness:

| Angular frequency (rad/s) | k - m omega^2 (N/m) | c omega (N/m) | S (m/m) | D (m/m) |
| --- | --- | --- | --- | --- |
| 0 | 75 | 0 | 1 | 1 |
| 5 | 0 | 450 | 1 | 1/6 |
| 10 | -225 | 900 | 1 | 1/(3 sqrt(17)) |
| 15 | -600 | 1350 | 1 | 1/(2 sqrt(97)) |

At the published forcing frequency, the amplitude is `10/450 = 1/45 m`,
matching Example 6. Dividing by `2/15 m` gives `1/6`. At `10 rad/s`, the
stiffness modulus is `225 sqrt(17) N/m`, giving `75/(225 sqrt(17))`.
Tests compare every plotted point with the hand values independently of the
curve implementation. Straight lines join the samples as a visual aid.

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
they are software versions, separate from the cited oscillator inputs.

## Limits

The oscillator magnitude contains no geometry, wave heading, phase, diffraction,
radiation or interaction calculation. The source's transient response and initial
conditions are excluded from this steady response illustration. These curves
cannot quantify shielding or qualify a design. Physical seastate RAO comparisons
remain separate work under issue #16 and require a suitable data basis and
engineering assessment.
