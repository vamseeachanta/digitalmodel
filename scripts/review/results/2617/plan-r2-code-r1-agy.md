Review target: plan-r2-code-r1.packet.md; raw-byte hashes in plan-r2-code-r1.manifest.json.

VERDICT: APPROVE

### Checks Performed

1. **QTF Normalization & Pair-Sum Symmetry**
   - Verified `compute_mean_drift_force` (`src/digitalmodel/orcawave/drift_forces.py:112-121`) integrates $2 D(\omega) S(\omega)\,d\omega = \sum_i D(\omega_i) a_i^2$ for $a_i = \sqrt{2 S_i \Delta\omega}$, matching `test_mean_drift_qtf_normalization_matches_wave_amplitudes` (`tests/orcawave/test_drift_force_variance.py:154-160`).
   - Verified `full_qtf_slowly_varying` (`src/digitalmodel/orcawave/drift_forces.py:214-228`) sums `qtf_mag2[i, j] * s_interp[i] * s_interp[j]` strictly over `i < j` with prefactor `8.0 * d_omega**2`. Under $F(t) = \operatorname{Re}\sum_{i,j} Q_{ij} a_i a_j e^{i[(\omega_i-\omega_j)t + \phi_i - \phi_j]}$ and Hermitian $Q_{ji} = Q_{ij}^*$, each unordered pair $\{i,j\}$ ($i<j$) forms a single real sinusoid of amplitude $2 a_i a_j |Q_{ij}|$ and phase-ensemble variance $\frac{1}{2}(2 a_i a_j |Q_{ij}|)^2 = 8 S_i S_j \Delta\omega^2 |Q_{ij}|^2$. Distinct unordered pairs (including pairs sharing one frequency index) have uncorrelated phase differences on $[0, 2\pi)$.
   - Verified exact bichromatic variances for both real ($Q_{12}=10$, $\sigma^2=50\text{ kN}^2$) and complex Hermitian ($Q_{12}=3+4i$, $\sigma^2=12.5\text{ kN}^2$) cases in `tests/orcawave/test_drift_force_variance.py:123-131` and `docs/verification/issue-2617-qtf-variance.html:46-49`.

2. **Diagonal / Zero-Frequency Treatment & Newman Consistency**
   - Verified `full_qtf_slowly_varying` (`src/digitalmodel/orcawave/drift_forces.py:223-225`) excludes `i == j` mean-load terms, and `test_diagonal_qtf_has_mean_load_but_zero_fluctuation` (`tests/orcawave/test_drift_force_variance.py:134-138`) checks both single-component (`count=1`) and multi-component diagonal (`count=4`) QTFs return `0.0`.
   - Verified `newman_approximation` (`src/digitalmodel/orcawave/drift_forces.py:168-177`) initializes `sv_spectrum = np.zeros(n)` and accumulates lags `k in range(1, n)`, leaving `sv_spectrum[0] == 0.0` so that `newman.sum() * dw` equals `full_qtf_slowly_varying(qtf_newman, ...)**2` with full endpoint-bin weight (`tests/orcawave/test_drift_force_variance.py:141-151`, `docs/verification/issue-2617-metrics.json:9-10`).

3. **Monte Carlo Independence, Unaliased Grid & Sampling Confidence**
   - Verified `narrow_band_reference` (`tests/orcawave/test_drift_force_variance.py:43-79`) constructs the time-domain complex wave envelope via inverse DFT (`np.fft.ifft(..., n=8192, axis=1) * 8192`) and evaluates $F(t) = Q|z(t)|^2$ directly in time without calling any double-sum formula.
   - Verified exact bin alignment and anti-aliasing: $\omega = \text{linspace}(0.2, 1.224, 2049)\text{ rad/s}$ gives $\Delta\omega = 0.0005\text{ rad/s}$, period $T = 2\pi/\Delta\omega = 12566.370614\text{ s}$, maximum difference frequency $\Delta\omega_{\max} = 1.024\text{ rad/s}$, and Nyquist frequency $\pi / (T/8192) = 2.048\text{ rad/s} > \Delta\omega_{\max}$ (`tests/orcawave/test_drift_force_variance.py:48-62`, `docs/verification/issue-2617-qtf-variance.html:30-34`).
   - Verified confidence interval calculation (`tests/orcawave/test_drift_force_variance.py:74-76`, `docs/verification/issue-2617-metrics.json:4-8`): 2048 independent phase records (`32 * 64`, seed `2617`) yield relative 95% CI half-width `0.0029835655 < 0.01`, and full-QTF variance (`24.952483 kN^2`) agrees with Monte Carlo (`24.926308 kN^2`) to `0.105% < 1%`.

4. **Linear Surge Oscillator Validation**
   - Verified `surge_oscillator_reference` (`tests/orcawave/test_drift_force_variance.py:82-120`, `docs/verification/issue-2617-qtf-variance.html:51-63`): for $\omega=[0.7, 0.8]\text{ rad/s}$ ($\mu=0.1\text{ rad/s}$), $a=[1.0, 0.5]\text{ m}$, $Q=10\text{ kN/m}^2$, and $m=10000, c=200, k=100$ ($\omega_n=0.1\text{ rad/s}, \zeta=0.1$), $|H(0.1)| = 1/(200 \times 0.1) = 0.05\text{ m/kN}$, giving analytical steady-state displacement standard deviation $10 \times 0.05 / \sqrt{2} = 0.3535533905932738\text{ m}$.
   - Verified transient decay and windowing: discarding 64 beat cycles attenuates homogeneous transients by $e^{-\zeta\omega_n (64 \cdot 2\pi/\Delta\omega)} = e^{-1.28\pi \cdot 10} = e^{-40.212} \approx 3.45 \times 10^{-18} < 4 \times 10^{-18}$, and measuring 128 integer cycles at 256 samples/cycle yields `0.3535356427801949 m` (`0.00502%` error vs `5%` threshold, phase spread `< 1e-10 m` in `tests/orcawave/test_drift_force_variance.py:174-180` and `docs/verification/issue-2617-metrics.json:12-25`).

5. **Compatibility, Caller Audit & Artifact Consistency**
   - Verified function signatures, return types, and docstrings in `src/digitalmodel/orcawave/drift_forces.py:128-229`, fixture update in `tests/orcawave/test_drift_forces.py:89-105`, index entry in `docs/plans/README.md:8`, plan in `docs/plans/issue-2617-qtf-variance.html:1-73`, verification report in `docs/verification/issue-2617-qtf-variance.html:1-88`, and metrics in `docs/verification/issue-2617-metrics.json:1-40` are mutually consistent.

