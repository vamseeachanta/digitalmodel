Frozen plan SHA-256: 3e4bb50343145a0e6e3c960419df85dc78433d1b0a533aeaaf7b6c6d5684f49c

VERDICT: MINOR

The core derivation is correct. The remaining findings are gaps in input validation, tolerances and test definition that should be fixed in the plan before implementation starts.

**Findings**

1. **The plan doesn't check that the QTF is Hermitian, and the unique-pair sum ignores the lower triangle.** `plan:26-30` assumes a Hermitian QTF. The new `i<j` sum reads only `Q_ij` for i<j, but `drift_forces.py:199-202` accepts any two matrices. If a QTF fails that assumption, or is stored lower-triangle-only, the function returns a wrong or zero variance and gives no warning. The plan should add a check that `Q_ji ≈ conj(Q_ij)` and raise an error when it fails, or define how a non-Hermitian input is reduced. It should also add a test for each case.

2. **The plan claims uniform grids are supported but adds no check, and an existing test breaks that rule.** `plan:34` restricts support to uniform grids. `drift_forces.py:207` takes `dω` from the first interval only. `tests/orcawave/test_drift_forces.py:90` already calls the function on a nonuniform grid `[0.3,0.5,0.8,1.0]`. The plan should either reject nonuniform grids, and update or replace that test, or document the current behaviour as unsupported. It should also deal with `np.interp` silently holding S constant outside `omega` (`drift_forces.py:205`).

3. **The QTF convention is assumed, not tied to a source.** The Monte Carlo history (`plan:39`) is "independent" only in how it sums terms. It uses the same `a_i a_j Q_ij` normalization it is meant to check. The plan cites the Orcina page (`plan:11`) but not the equation behind `F = ΣΣ a_i a_j Re[Q_ij e^{i(...)}]`. That is the convention where the diagonal gives `Σ a_i² Q_ii = Σ 2 S_i dω Q_ii`. A test should tie the diagonal to `compute_mean_drift_force` (`drift_forces.py:117`, `2·D·S`). That would catch a QTF defined with a different factor, such as ½.

4. **The 1% tolerance for Newman vs full agreement is too loose.** `plan:41`: on the same grid with `Q_ij = ½(D_i+D_j)`, the two discrete calculations should agree to round-off. A 1% tolerance would hide endpoint-weight or off-by-one-lag errors of order 1/n. The plan should state an rtol near 1e-10 when the grids are identical.

5. **The JONSWAP test can't tell whether the diagonal was excluded.** `plan:17-19`: from the baseline ratio, the diagonal adds about 0.6% of the variance (16U+8D = 2.006·8U, so D ≈ 0.006U). A faulty fix that just halves the full double sum is off by only about 0.3% and passes the 1% test at `plan:43`. Only the unit tests at `plan:40` catch it. The plan should say so, so the ensemble test isn't presented as diagonal coverage.

6. **The oscillator test has no defined sampling confidence.** `plan:44-46` gives a 5% tolerance but doesn't specify:
   - the number of records or a confidence interval;
   - the time step relative to the natural period;
   - a minimum ratio of `ζω_n` to `dω`, which the oscillator bandwidth needs so it covers several frequency lines;
   - that the response mean is removed, since H(0) applies a static offset;
   - that averaging runs over a whole force period of `2π/dω` after the transient is discarded.

   If the damping is light, a few lines dominate and a 5% check against one fixed seed carries no statistical meaning. The plan should use the same CI rule as `plan:42-43`.

7. **The plan has no step for telling users about the change.** `plan:47-48` lists callers. It doesn't say the std returned by `full_qtf_slowly_varying` falls by about 1/√2, or that Newman bin 0 changes. Those changes reach `DriftForceResult.sv_*_std_kn` users. The plan should add a changelog or release note and a docstring "behaviour change" note, and should search docs, examples and notebooks, not only Python imports.

8. **Possible NumPy compatibility problem in the touched file.** `drift_forces.py:117-119` uses `np.trapz`, which NumPy 2.x deprecates. The plan should state the pinned NumPy version, or note this as a known issue left out of scope.

**Checks performed**

- **Pair variance.** For a Hermitian pair, the two ordered terms combine to `2a_ia_j Re(Q_ij e^{iθ})`. The variance is `2a_i²a_j²|Q|²`, which equals `8S_iS_jdω²|Q|²` (`plan:26-29`). Correct.
- **Size of the old error.** The old code computes `8Σ_all`, which equals `16Σ_{i<j} + 8Σ_diag`. That matches the measured ratio of 2.006. The diagonal is constant in time, so it adds to the mean, not the variance.
- **Phase independence.** Phase averages give zero cross-covariance between different unordered pairs. Rayleigh-amplitude sampling gives the same off-diagonal result.
- **Newman spectrum.** `8·Σ·dω` per lag (`drift_forces.py:176`) times dω, summed over k≥1, equals the unique-pair variance. Zeroing lag 0 removes only the diagonal. The continuum limit, `8∫T²S(ω)S(ω+μ)dω`, matches `plan:33`.
- **Monte Carlo setup.**
  - dω = 1.024/2048 = 5e-4 rad/s.
  - Record length = 2π/dω = 12566.37 s.
  - 8192 samples give a Nyquist frequency of 2.048 rad/s, above the maximum difference frequency of 1.024 rad/s.
  - Differences sit on exact DFT bins, so each record's time variance is unbiased for the ensemble value.
  - A CI half-width of 0.30% is well below 1%.
- **Effect on existing tests.** `test_drift_forces.py:105` (diagonal-only, `>=0`) still passes when the result is 0. Tests at lines 64-83 are unaffected by zeroing bin 0.
