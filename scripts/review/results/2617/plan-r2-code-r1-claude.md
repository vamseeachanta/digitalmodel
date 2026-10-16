Review target: plan-r2-code-r1.packet.md; raw-byte hashes in plan-r2-code-r1.manifest.json.

VERDICT: MINOR

**Findings**

1. `src/digitalmodel/orcawave/drift_forces.py:223-226` reads only the upper triangle, `qtf_mag2[i, j]` for `j>i`. The `|Q_ij|` derivation holds only for a Hermitian matrix. For a general matrix the ordered pair amplitude is `|Q_ij + conj(Q_ji)|`.
   - A lower-triangle-only QTF returns exactly 0.0 with no warning.
   - A non-Hermitian matrix is silently half-read.
   - Plan lines 34-36 and report lines 77-78 call this a "limitation", but nothing enforces it. The old code's failure was loud by comparison.
   - A Hermiticity check, or the symmetric `|Q_ij + conj(Q_ji)|/2` form, is cheap. No test covers a non-Hermitian or triangular input.

2. The oscillator "validation" (`tests/orcawave/test_drift_force_variance.py:82-120, 174-180`) is weak.
   - The single beat frequency (0.1 rad/s, at the natural frequency) tests only one linear filter gain.
   - The prediction is the pair formula fed a hand-built `10*|H|` QTF. It tests the filter identity, not response-statistics accumulation.
   - With one pair there is no phase dependence. `np.ptp < 1e-10` therefore confirms invariance, not "independent random-phase records", as plan lines 49-51 imply.
   - There is no response check with several pairs sharing a difference frequency, where cross-terms matter. The ensemble-mean cancellation is asserted in prose (report lines 16-17) but never tested, because the narrow-band test only checks the mean.

3. The Monte Carlo reference is itself a fixed-amplitude, random-phase ensemble, and the sea is not Gaussian.
   - A Gaussian sea has Rayleigh-random amplitudes, which is a different ensemble on a coarse grid.
   - Report line 9 claims qualification of "the documented numerical cases". That scope is acceptable, but the ensemble definition belongs in the docstring and in the report conclusion, not just in a comment.
   - The narrow-band test cannot detect a diagonal-handling error. The report concedes this at lines 37-38, and only the sparse tests guard it.

4. `newman_approximation` and `full_qtf_slowly_varying` take `d_omega` from different grids (`omega` at line 167, `freqs` at line 212). A mismatched `omega` and `qtf.frequencies_rad_s` is interpolated silently, and no test covers this. Newman's zero-lag bin changed from positive to zero, which is a behavior change for consumers. The report flags it, but external consumers are not inventoried (report line 75).

5. `test_narrow_band_monte_carlo_force_variance` is `@pytest.mark.slow`.
   - The packet does not show the marker registered. Under `--strict-markers` the test would error.
   - If slow tests are deselected by default, the headline 1% criterion is not exercised in routine CI.

6. Several claims cannot be verified from the packet.
   - These are the caller audit and the `__init__.py` re-export (report lines 66-68), "seven failures and one pass" (line 80), "357 tests pass" (line 82), and the review results in README line 8.
   - `src/digitalmodel/orcawave/__init__.py` and `scripts/review/results/2617/` are not in the packet.
   - Report line 38 and the metrics JSON `baseline.source_sha256` are assertions only.

7. `compute_mean_drift_force` uses `np.trapz`, so the mean-force convention test (test line 154) matches only because the end-point spectrum values are zero. The NumPy<2 pin in the plan covers the deprecation. The test does not establish the convention for nonzero edge values.

**Checks performed (verified from the packet)**
- Pair variance: two ordered terms give amplitude `2 a_i a_j |Q|`, and variance is `amplitude²/2 = 8 S_i S_j dw² |Q|²`. This matches the code at line 226.
- Unique pairs have zero phase-ensemble covariance, including pairs that share a component.
- The old bichromatic value 312.5 reproduces by hand (`0.08 × 3906.25`). The corrected value 50 matches the 12.5 complex-pair case.
- Newman: `sum(S_sv)*dw = 8 dw² Σ_{k≥1}…` equals the full-QTF pair sum when grids coincide. Dropping `k=0` removes the diagonal. Full end-point bin weights are right, and the trapezoid would halve the last bin.
- The FFT reference has independent code, with `n=8192` and `T = 2π/dw`. Nyquist is 2.048 rad/s against a maximum difference frequency of 1.024 rad/s. A full-period `np.var` is unbiased, since the mean is the constant diagonal.
- The CI is computed over record variances, not correlated samples. The half-width of 0.298% is below 1%, and the 0.105% error is consistent with it.
- Oscillator parameters are consistent. `exp(-40) ≈ 4e-18` for a 64-cycle discard, the force variance of 50.0000000000001 is within 1e-12 relative, and the 256 samples per cycle are exactly commensurate.
- The plan's future-tense register holds, and the plan matches the code on factor eight, the zero-lag bin and the unchanged signatures.

