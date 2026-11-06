# Spectral fatigue frequency units

The `fatigue.spectral_fatigue` API uses Hz moments of a one-sided stress PSD.
Removing its extra angular-to-cyclic conversion corrects the rates and damage
from Hz inputs. The structural API retains angular moments. This note qualifies
frequency units, synthetic narrow-band arithmetic and the Dirlik coefficient
definition. Broad-band regression covers a rectangular spectrum; historical
results and general broad-band estimator accuracy remain unqualified.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-28 | Define both moment contracts, record independent arithmetic tests and identify stored-result consumers after correcting cyclic rates. |
| 2 | 2026-11-06 | Correct the Dirlik irregularity coefficients, add independent damage regressions, regenerate the spectral atlas and clarify example verdicts and publication provenance. |

## Moment and rate contracts

For nonnegative frequency `f` in Hz, the one-sided PSD `S_f(f)` includes the full
stress variance: `M_k = integral(f**k * S_f(f) df)`, `k = 0, 1, 2, 4`.
With stress in MPa, `S_f` is MPa²/Hz and `M_k` is MPa² Hz**k.
The Hz model returns `nu_0 = sqrt(M_2/M_0)` and `nu_p = sqrt(M_4/M_2)` in cycles/s.

The derivation follows the change of variable `omega = 2*pi*f`:
`S_omega(omega) d(omega) = S_f(f) df`, so
`m_k_angular = integral(omega**k * S_omega(omega) d(omega)) = (2*pi)**k * M_k`.
Consequently `sqrt(m_2_angular/m_0_angular)/(2*pi) = sqrt(M_2/M_0)`, with the
same cancellation for the peak-rate ratio. The structural
`fatigue.frequency_domain` API integrates `(2*pi*f)**k*S_f(f)` against `df`
and retains this division. Its `lambda1` and `lambda2` are angular frequencies.
The moment objects from these APIs must not be interchanged.

Direct angular inputs to the Hz model require explicit conversion:
`M_k = m_k_angular/(2*pi)**k`. Within the Hz API, both rates and every damage
rate using them increase by `2*pi` relative to the previous implementation for
unchanged Hz moments; annual damage follows the same factor. Equivalent ranges
and dimensionless corrections retain their existing definitions. The `duration`
argument remains accepted and does not change rate fields; accumulated damage
is `damage_rate * exposure_seconds`.

## Independent arithmetic and quadrature bound

The synthetic probe is `S_f(f)=1` on `a=99/100` through `b=101/100` Hz and zero
elsewhere, with no extra endpoint samples outside the integration interval.
Antiderivatives give `M_k=(b**(k+1)-a**(k+1))/(k+1)` independently of the code.
Exact rational arithmetic supplies the expected moments before conversion to
floating point. The rate oracles are their square-root ratios above.

For `N=32` uniform subintervals and `h=(b-a)/N`, the composite trapezoidal rule
has remainder bounded by `E_k=(b-a)*h**2*max|g_k''(f)|/12`, where `g_k=f**k`.
Twice differentiating gives `g_k''=k*(k-1)*f**(k-2)`: the bound vanishes for
orders zero and one and reaches its maximum at `b` for the other tested orders.
For a rate `sqrt(M_i/M_j)` with positive lower moment bounds, propagation gives

```text
sqrt((M_i-E_i)/(M_j+E_j)) <= numerical rate <= sqrt((M_i+E_i)/(M_j-E_j)).
```

The tests add a floating-point allowance `gamma_n=n*u/(1-n*u)`,
`u=ulp(1)/2`, `n=8*N+32`, covering grid construction, powers, summation, unit
conversion and ratios. Moment allowances scale with the interval width times
the maximum integrand. The intervals are narrow arithmetic bounds; they cannot
conceal an extra factor of `2*pi`. Both exact and integrated angular moments
are checked against these independent Hz expectations.

The arbitrary synthetic scale `c=7/2` uses `S_c(f)=S_f(f/c)/c` on `[c*a,c*b]`.
Substitution gives `M_k(c)=c**k*M_k`, unchanged variance and rates multiplied by
`c`. The quadrature bound includes the PSD height `1/c` at this scale.

For the independent single-slope curve `N(R)=A/R**2`, choose `A=1` and
`log10(A)=0`. A Rayleigh amplitude has second moment `2*M_0`; stress range is
twice amplitude, hence `E[R**2]=8*M_0` and
`damage_rate=sqrt(M_2/M_0)*8*M_0/A`. Tests check this absolute rate, reported
peak rate, frequency scaling and duration independence. Annualization checks
retain the existing API definition of a year: `365.25*24*3600` seconds.
All numbers in these probes are synthetic choices or the displayed hand
calculations, rather than material properties or engineering acceptance factors.

## Dirlik coefficient regression

Dirlik's mixture coefficients use the irregularity factor
`gamma=M_2/sqrt(M_0*M_4)`, rather than the spectral width
`epsilon=sqrt(1-gamma**2)`. The reported `bandwidth_parameter` remains epsilon;
Wirsching-Light retains its own width-based correction. The coefficient and
range-damage expressions follow equations (8) and (9) of
[Metals 11 (2021), 1333](https://doi.org/10.3390/met11091333).
The publisher's [open PDF](https://mdpi-res.com/d_attachment/metals/metals-11-01333/article_deploy/metals-11-01333.pdf)
was read on 2026-11-06; its equations and Table 1 support this regression basis.

Tests compare Dirlik with the Rayleigh narrow-band limit on the rectangle
above, using both exact and integrated moments. Broad-band tests use the
unit-area rectangle on [1,5], normalized to unit variance and unit angular
second moment before conversion to Hz. The absolute expectations come from
independent substitution into equations (8) and (9), documented in
`tests/fatigue/test_benasciutti_tovo.py`; that file separately checks the Table 1
damage ratios at slopes 3, 5 and 7. The new tests exercise the production
Dirlik estimator against those expectations, so an independent denominator
can no longer conceal its coefficient defect. This verifies the formula for
these inputs and does not qualify all broad-band spectra.

## Constructor and consumer inventory

Repository Python constructors and imports were inspected before changing the
rate formulas. The findings are:

- `fatigue/spectral_fatigue.py::compute_spectral_moments` already integrates Hz
  moments. Its exported aliases in `fatigue/__init__.py` keep this contract.
- `tests/fatigue/test_benasciutti_tovo.py` and
  `examples/domains/fatigue/benasciutti_tovo_example.py` contain declared angular
  benchmark inputs. Their constructors now divide each angular moment by
  `(2*pi)**k`; published correction ratios and independent absolute benchmark
  expectations are preserved. The spectral-line test also converts its angular
  frequency explicitly. Other synthetic moment-ratio and invalid-input fixtures
  declare no angular rate anchor and remain valid as Hz moments.
- `tests/fatigue/test_spectral_fatigue.py` constructs synthetic positive-moment
  fixtures without an angular contract or absolute-rate oracle; no migration
  is required. Structural constructors in
  `tests/structural/fatigue/test_frequency_domain.py` retain their angular model.
- `orcawave/wave_spectrum.py` defines a separate wave-moment model; it is not
  passed to either stress-moment model and is unchanged.
- `spectral_fatigue/workflow.py` integrates Hz PSDs and writes per-state damage,
  annual damage and `peak_rate_Hz` to CSV, then derives reciprocal life, margin
  and screening status. `rao_spectral_fatigue/workflow.py` already converts
  angular wave PSDs and axes to Hz before delegating. No input conversion is
  required in either workflow.
- Stored-result consumers include the spectral and RAO workflow result folders
  and registry tests in
  `tests/workflows/test_durable_workflows.py`. The direct-PSD keel assertion
  now expects failure, as recorded in the current notes in
  `spectral_fatigue/docs/agent-pack.md` and `rao_spectral_fatigue/docs/agent-pack.md`
  method notes. Both direct-PSD locations fail screening with the corrected Hz
  rates. Correcting the Dirlik coefficients restores a passing keel verdict in
  the RAO example, so that assertion now expects a pass; its splash zone still
  fails. Both examples are governed by the splash zone. Reports built from those outputs inherit their
  basis. A unit fix alone does not qualify a screening status. The separate
  `report_pack` example uses the legacy FATG path and is unaffected.
- `parametric/generate.py::_spectral_fatigue_annual_damage` already converts wave
  spectra to Hz. Its response feeds `atlases/spectral_fatigue`,
  `parametric/query.py::_handle_annual_damage` and the spectral atlas-query
  workflow. `parametric/refresh.py::SOURCE_FILES` fingerprints the changed
  spectral module, so existing atlas bases become stale. The spectral atlas
  grid, surrogate, validation and provenance are regenerated with
  `python scripts/parametric/build_atlas.py spectral-fatigue`, and the default
  pointer selects the corrected Hz basis. Regression tests compare the stored
  grid and holdout results against a fresh build with numerical tolerances,
  verify the retained source and artifact digests, and exercise the shipped
  query with output readback. Earlier atlas versions remain available by ID;
  their results and separate report outputs require recalculation before reuse.
  Manifest `code_version` and inventory `recorded_code_version` identify the
  build-time HEAD, which precedes publication and can have uncommitted source
  changes. They do not identify the commit containing the generated atlas.
  Publication is resolved by matching the recorded source and artifact digests
  against reachable commits (`first-matching-artifact-publication`), including
  after a squash merge. Retained source bytes reconstruct the content
  fingerprint; changing fingerprinted files requires another atlas build.
- The independent `structural/spectral_fatigue.py` helper and structural
  `frequency_domain.py` consumers (`analysis.py`, `scatter_fatigue.py`) retain
  their existing contracts; agreement of their damage estimators is not claimed.

## Remaining evidence gaps

Frequency-unit tests do not qualify empirical broad-band corrections. Their
scale invariance is checked separately. The Dirlik irregularity substitution
is corrected and checked against the narrow-band limit and independent
rectangular-spectrum arithmetic; other coefficient guards and empirical
assumptions remain unchanged. The structural
narrow-band estimator also uses a representative range rather than the Rayleigh
range integral tested here, so only its frequency convention is cross-checked.

External direct constructors cannot be inventoried from this repository.
Historical results, workflow statuses and superseded atlas predictions need
recalculation and independent review before reuse. The regenerated spectral
atlas remains a screening model; its interpolation check does not qualify the
Dirlik estimator or its fixed stress gain for a design application. S-N tables
are unchanged, no design factors are selected and no solver is run. The lesson
from this change is to pair dimensionless correction tests with independent
absolute-rate anchors at every frequency-unit boundary.
