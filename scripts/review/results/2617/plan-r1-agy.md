Frozen plan SHA-256: 3e4bb50343145a0e6e3c960419df85dc78433d1b0a533aeaaf7b6c6d5684f49c

VERDICT: MINOR

### Findings

1. **Non-physical zero-variance in existing unit test after diagonal exclusion (`tests/orcawave/test_drift_forces.py:89-105`, `docs/plans/issue-2617-qtf-variance.html:29-30`, `docs/plans/issue-2617-qtf-variance.html:47`)**
   - In `TestFullQTF.test_full_qtf_positive_std`, the fixture constructs a strictly diagonal QTF matrix (`real_part = np.eye(n) * 10.0`, `imag_part = np.zeros((n, n))`) and asserts `sigma >= 0` (`tests/orcawave/test_drift_forces.py:93-105`).
   - Once `full_qtf_slowly_varying` restricts summation to $i < j$ (`docs/plans/issue-2617-qtf-variance.html:29`), this test will silently evaluate `sigma == 0.0` and still pass `assert sigma >= 0`, failing to test non-zero positive standard deviation as its name (`test_full_qtf_positive_std`) and original intent imply. The plan does not mention updating or augmenting `tests/orcawave/test_drift_forces.py` (`docs/plans/issue-2617-qtf-variance.html:53-57`).

2. **Unspecified single-element frequency grid behavior (`src/digitalmodel/orcawave/drift_forces.py:169,207`, `docs/plans/issue-2617-qtf-variance.html:40`)**
   - Both `newman_approximation` (`drift_forces.py:169`) and `full_qtf_slowly_varying` (`drift_forces.py:207`) use fallback `d_omega = ... if len(...) > 1 else 1.0`.
   - For a single wave component (`n = 1`), `i < j` is empty and `k = 0` is zeroed out, so both return zero regardless of `d_omega`. However, the plan (`docs/plans/issue-2617-qtf-variance.html:34,40`) should explicitly state whether `n < 2` or non-uniform/non-increasing `omega`/`freqs` grids are validated (e.g., raising `ValueError` vs. returning zero for `n == 1`).

3. **Oscillator frequency-domain vs. time-domain comparison specification (`docs/plans/issue-2617-qtf-variance.html:44-46`)**
   - The plan states that the linear surge oscillator time-domain ODE response will be compared against `full_qtf_slowly_varying` called with "a QTF weighted by the oscillator transfer magnitude at each difference frequency" (`docs/plans/issue-2617-qtf-variance.html:44-45`).
   - Because `full_qtf_slowly_varying` takes a `QTFMatrix` with `real_part` and `imag_part`, weighting $Q_{ij}$ by $|H(\omega_i - \omega_j)|$ (or complex $H(\omega_j - \omega_i)$) yields identical $|Q^{\text{resp}}_{ij}|^2 = |H(\mu_{ij})|^2 |Q_{ij}|^2$, which is mathematically valid for variance. However, in the time-domain ODE driving force, if complex $Q_{ij}$ has non-zero imaginary part or if the ODE is integrated over a finite record with step $\Delta t$, numerical integration damping/phase error and resonant bandwidth resolution ($\Delta\omega \ll \zeta \omega_n$) must be satisfied so that discretizing the narrow resonant peak does not breach the 5% tolerance. The plan does not specify the oscillator natural frequency $\omega_n$ and damping ratio $\zeta$ relative to $\Delta\omega$.

---

### Checks Performed

- **QTF normalization & unique-pair derivation (`docs/plans/issue-2617-qtf-variance.html:26-29`, `src/digitalmodel/orcawave/drift_forces.py:188-217`)**:
  - Verified that for wave elevation $\eta(t) = \sum_i a_i \cos(\omega_i t + \phi_i)$ with $a_i = \sqrt{2 S_i \Delta\omega}$ and Hermitian $Q_{ji} = Q_{ij}^*$, each unordered pair $i < j$ contributes $F_{ij}(t) = 2 a_i a_j |Q_{ij}| \cos((\omega_j - \omega_i)t + \phi_j - \phi_i - \arg Q_{ij})$.
  - Verified that the variance of $A \cos(\Delta\omega t + \theta)$ with uniform independent phases is $A^2 / 2 = \frac{1}{2}(2 a_i a_j |Q_{ij}|)^2 = 2 a_i^2 a_j^2 |Q_{ij}|^2 = 8 S_i S_j \Delta\omega^2 |Q_{ij}|^2$.
  - Confirmed that summing $8 S_i S_j \Delta\omega^2 |Q_{ij}|^2$ over all $(i, j)$ in `drift_forces.py:212-215` double-counts every off-diagonal pair ($i < j$ and $i > j$) and erroneously includes the $i = j$ diagonal, explaining the ~2.006x ratio in the reproduction (`docs/plans/issue-2617-qtf-variance.html:17-19`).
- **Diagonal / zero-frequency treatment (`docs/plans/issue-2617-qtf-variance.html:29-33`, `src/digitalmodel/orcawave/drift_forces.py:170-177`)**:
  - Verified that $i = j$ ($\mu = 0$) gives constant mean drift $\sum_i a_i^2 Q_{ii} = 2 \sum_i S_i Q_{ii} \Delta\omega$, which has zero time-fluctuation variance.
  - Verified that in `newman_approximation` (`drift_forces.py:171-176`), for $k > 0$, the inner loop sums $i \in [0, n - k - 1]$ with $j = i + k$ (i.e. $i < j$ at lag $\mu_k = k \Delta\omega$) and multiplies by $8 \Delta\omega$, so setting `sv_spectrum[0] = 0.0` makes `sum(sv_spectrum) * d_omega` exactly equal to the $i < j$ double sum $8 \sum_{i < j} Q_{ij}^2 S_i S_j \Delta\omega^2$.
- **Monte Carlo independence & sampling confidence (`docs/plans/issue-2617-qtf-variance.html:14-20, 38-43`)**:
  - Checked period and grid parameters: $\Delta\omega = (1.224 - 0.2)/2048 = 0.0005\text{ rad/s}$, repeat period $T = 2\pi / \Delta\omega = 4000\pi \approx 12566.370614\text{ s}$, matching line 16.
  - Checked confidence interval math: with $N = 2048$ independent records, a relative 95% CI half-width of $\sim 0.30\%$ (`0.00298`, line 20) is well within the $< 1\%$ threshold (`docs/plans/issue-2617-qtf-variance.html:43`).
- **Compatibility & acceptance criteria (`docs/plans/issue-2617-qtf-variance.html:30-31, 47-48`)**:
  - Confirmed return types and signatures of `full_qtf_slowly_varying` (`float`) and `newman_approximation` (`np.ndarray` of length `len(omega)`) are preserved, maintaining API compatibility with existing callers and tests in `tests/orcawave/test_drift_forces.py`.
