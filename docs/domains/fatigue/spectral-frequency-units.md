# Spectral fatigue frequency units

The `fatigue.spectral_fatigue` API uses Hz moments of a one-sided stress PSD.
Removing its extra angular-to-cyclic conversion corrects the rates and damage
from Hz inputs. The structural API retains angular moments. This note qualifies
frequency units, synthetic narrow-band arithmetic and the Dirlik irregularity
coefficients against independent broad-band arithmetic. Historical results and
general broad-band estimator accuracy remain unqualified.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-28 | Define both moment contracts, record independent arithmetic tests and identify stored-result consumers after correcting cyclic rates. |
| 2 | 2026-11-06 | Correct Dirlik irregularity coefficients, add independent limiting and broad-band regressions, refresh the atlas and clarify its publication basis. |
| 3 | 2026-11-06 | Mark the retained intermediate atlas as superseded and record the structural Dirlik evidence gap and follow-up criteria. |
| 4 | 2026-11-06 | Guard both supersession fields with a regression and clarify that the loader does not enforce the advisory disposition. |

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
`M_k = m_k_angular/(2*pi)**k`. For the frequency-unit correction alone,
both rates and every damage rate using them increase by `2*pi` for unchanged Hz moments; annual damage
follows the same factor. The subsequent Dirlik coefficient correction also
changes its dimensionless mixture, so its combined damage change is not a
uniform `2*pi` factor. Equivalent ranges and dimensionless corrections retain their existing definitions. The `duration`
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
  `tests/workflows/test_durable_workflows.py`. The two defect-dependent detail
  pass assertions were rechecked after both corrections. Both direct-PSD
  example locations fail; the RAO example has a passing keel joint (margin
  7.6) and a failing splash zone (margin 0.9). These outcomes are recorded in
  the updated
  `spectral_fatigue/docs/agent-pack.md` and `rao_spectral_fatigue/docs/agent-pack.md`
  dependency notes. The registry regressions exercise the engine and CSV
  outputs in a temporary source copy, keeping generated results external.
  Reports built from those outputs inherit their basis. A unit fix alone does not qualify a screening status. The separate
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
- The independent `structural/spectral_fatigue.py` helper and structural
  `frequency_domain.py` consumers (`analysis.py`, `scatter_fatigue.py`) retain
  their existing contracts; agreement of their damage estimators is not claimed.

## Dirlik coefficients and independent evidence

Dirlik's mixture uses `gamma = M_2/sqrt(M_0*M_4)`, the irregularity factor,
in `D1`, `R`, `D2` and `Q`. The spectral width `sqrt(1-gamma**2)` remains
reported as bandwidth and remains the Wirsching-Light input; it cannot replace
`gamma` in the Dirlik mixture. The narrow-band limit has `gamma` tending to
one and width tending to zero.

[Metals 11 (2021), 1333](https://doi.org/10.3390/met11091333), Equations (8)-(9)
and Table 1, supplies the coefficient convention and a band-limited PSD
benchmark. The publisher's [open PDF](https://mdpi-res.com/d_attachment/metals/metals-11-01333/article_deploy/metals-11-01333.pdf)
was read for this correction. The existing independent hand calculations in
`tests/fatigue/test_benasciutti_tovo.py` now anchor production Dirlik damage
for slopes 3, 5 and 7, in addition to testing the published BT/Dirlik ratios.
A unit-area rectangular PSD on `[1-h,1+h]` Hz gives exact moments
`M0=M1=1`, `M2=1+h**2/3`, `M4=1+2*h**2+h**4/5`.
The Dirlik/narrow-band damage ratio converges to one as `h` decreases through
0.1, 0.01 and 0.001, with a numerical probe tolerance of 0.1 % at 0.01.
All six new regressions fail with the spectral-width substitution.

The regenerated atlas contains 60 grid points and 14 holdout points; its maximum
holdout interpolation error is 0.2 % against the configured 10.0 % threshold.
These interpolation figures concern the implemented response, not empirical
accuracy against measured fatigue. No historical atlas files are overwritten.

### Superseded retained basis

The provenance record for atlas `692aaf26f51c` is **superseded for calculation
use** by `cebf7823125c`. The intermediate atlas used spectral width in place
of the irregularity factor in Dirlik's coefficients; it therefore retains
known-defective damage predictions. Its files and record are retained for
traceability, and the atlas remains loadable by explicit ID. The default
pointer selects `cebf7823125c`.

The `superseded_by` and `superseded_reason` fields are advisory metadata.
The atlas loader and query path do not read these fields; loading an explicit
ID does not emit a supersession warning or reject the retained basis.
`test_superseded_spectral_atlas_records_advisory_disposition` guards both
fields and the presence of the corrected successor's provenance record.

The historical record's `VERIFIED` status certifies reconstruction of its source
fingerprint and artifact digests, not the estimator's engineering validity.
It does not override this superseded disposition. Predictions from
`692aaf26f51c` shall be recalculated with the corrected coefficient basis and
independently reviewed before reuse; loading the retained ID does not perform
that recalculation. The corrected default also requires a drift check after
integration because the registry and refresh module are fingerprinted sources.

The manifest's `code_version` and the provenance record's `recorded_code_version`
are the build-time HEAD, which can precede publication of the generated files
and need not contain working-tree source changes. Publication is resolved by
digest: `first-matching-artifact-publication` finds the first reachable commit
whose source bytes and atlas artifacts match every recorded digest. The content
fingerprint binds the numerical source basis; `code_version` alone does not.
After integration with main, rerun the drift gate and regenerate if that basis
changes, then rerun the grid, shipped-query and retained-provenance regressions.

## Remaining evidence gaps

These checks qualify the coefficient substitution and bounded analytical
benchmarks, rather than general empirical broad-band accuracy. The structural
narrow-band estimator also uses a representative range rather than the Rayleigh
range integral tested here, so only its frequency convention is cross-checked.

External direct constructors cannot be inventoried from this repository.
Historical results, workflow statuses and superseded atlas predictions need
recalculation and independent review before reuse. The regenerated spectral
atlas remains a screening model; its interpolation check does not qualify the
Dirlik estimator or its fixed stress gain for a design application. S-N tables
are unchanged, no design factors are selected and no solver is run. The lesson
from this change is to pair dimensionless correction tests with independent
absolute-rate anchors at every frequency-unit boundary. Narrow-band limit
regressions also distinguish irregularity from its complementary spectral width.

### Structural Dirlik follow-up

`structural/fatigue/frequency_domain.py::DirlikMethod` retains a separate
coefficient defect: its mixture uses `q` and expressions for `R1` and `D2`
that differ from Equations (8)-(9) cited above. Its damage predictions remain
unqualified by the spectral API correction. Follow-up issue pending: the
structural coefficient correction will require independent narrow-band-limit
and converted angular `c=5` rectangular-PSD anchors for slopes 3, 5 and 7,
matching the acceptance cases in `tests/fatigue/test_spectral_frequency_units.py`
and `tests/fatigue/test_benasciutti_tovo.py`. The structural angular-moment
contract will be preserved. The existing structural tests do not establish
agreement with these independent Dirlik anchors.
