# Review packet: 2617 plan review

Generated 2026-10-16T15:33:40+00:00 · git HEAD `c5e4ac3390942e326243093a9a6175716f0e7459`
Digest domain: raw bytes as stored on disk (line endings included).
Line numbers are prefixed; cite findings as `path:line`. The packet is the complete review target.

## docs/plans/issue-2617-qtf-variance.html

sha256 `3e4bb50343145a0e6e3c960419df85dc78433d1b0a533aeaaf7b6c6d5684f49c` · 6419 bytes · 65 lines

```
   1 | <!doctype html>
   2 | <html lang="en"><meta charset="utf-8"><title>Issue 2617: QTF variance plan</title>
   3 | <style>body{font:16px/1.5 sans-serif;max-width:960px;margin:40px auto;padding:0 20px}code{background:#eee}td,th{border:1px solid #bbb;padding:8px}table{border-collapse:collapse}</style>
   4 | <h1>Issue 2617: difference-frequency force variance</h1>
   5 | <p>The change will address <a href="https://github.com/vamseeachanta/digitalmodel/issues/2617">issue 2617</a>
   6 | on a fresh worktree from <code>c5e4ac33</code>. Complexity will be T2; execution will use one writer and parallel read-only plan/code reviews.
   7 | The user request will authorize implementation, one public draft PR and its issue summary; merge and deployment will remain outside scope.</p>
   8 | <h2>Resource intelligence and reproduction</h2>
   9 | <p>The implementation will be checked against <code>src/digitalmodel/orcawave/drift_forces.py</code>,
  10 | the existing tests in <code>tests/orcawave/test_drift_forces.py</code>, the issue criteria, and
  11 | <a href="https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltheory,Wavedriftandsumfrequencyloads.htm">Orcina's second-order load theory</a>.
  12 | The derivation will distinguish conjugate-paired terms and the constant diagonal from fluctuation variance.</p>
  13 | <p>The pre-change runtime evidence will be retained in the verification report:</p>
  14 | <pre>seed=2617; narrow-band JONSWAP Hs=2 m, Tp=2*pi/0.8 s, gamma=10
  15 | omega=linspace(0.2,1.224,2049) rad/s; Q=10 kN/m^2
  16 | 2048 random-phase records; 8192 samples/record; 12566.370614 s/record
  17 | repository full-QTF variance = 50.005839732175204 kN^2
  18 | Monte Carlo force variance   = 24.926308458002424 kN^2
  19 | repository / Monte Carlo    = 2.0061470320175374
  20 | 95% CI relative half-width  = 0.0029835655035290666</pre>
  21 | <p>Registry discovery will remain bounded to the hub's standards-transfer ledger, code registry and online-resource registry.
  22 | No standards-derived coefficients will be introduced. The available online registry will supply context for Orcina L07;
  23 | no licensed/client files will be consumed. The absent sibling wiki catalogs and claim helper will be recorded as coverage gaps;
  24 | the isolated worktree will avoid shared implementation paths.</p>
  25 | <h2>Proposed calculation and scope</h2>
  26 | <p>For fixed-amplitude random-phase waves, <code>a_i=sqrt(2 S_i dw)</code> and a Hermitian difference-frequency QTF,
  27 | the two ordered terms will combine into a real sinusoid of amplitude <code>2 a_i a_j |Q_ij|</code>.
  28 | Its phase-ensemble variance will be <code>2 a_i^2 a_j^2 |Q_ij|^2 = 8 S_i S_j dw^2 |Q_ij|^2</code>.
  29 | Summation will therefore visit only <code>i&lt;j</code>. The diagonal will contribute mean load, not slowly-varying variance.</p>
  30 | <p><code>full_qtf_slowly_varying</code> will use the unique-pair sum, retain its scalar standard-deviation API and document the derivation.
  31 | <code>newman_approximation</code> will retain its array API and positive-lag values, with its zero-lag bin set to zero to exclude mean load.
  32 | Its documentation will specify <code>mu=k*dw</code> and discrete variance <code>sum(S_sv)*dw</code>, including full endpoint-bin weights.
  33 | This discrete interpretation will be explicit; the positive-frequency continuum limit will remain the usual factor-eight convolution.</p>
  34 | <p>Uniform increasing frequency grids and Hermitian full QTF matrices will remain the stated supported physical case.
  35 | Nonuniform quadrature, directional seas, QTF interpolation and nonlinear/mooring qualification will remain outside this correction.</p>
  36 | <h2>TDD and acceptance</h2>
  37 | <ol>
  38 | <li>A reproducible synthetic reference harness will call the actual public APIs before any production edit.
  39 | The recorded baseline will precede implementation; random-phase force histories will be formed independently from wave amplitudes.</li>
  40 | <li>Failing tests will cover a bichromatic known variance (including a complex Hermitian QTF), a single component/diagonal-only zero fluctuation,
  41 | and Newman/full variance agreement within 1%. These will fail against the old implementation.</li>
  42 | <li>A narrow-band JONSWAP ensemble will use fixed seed 2617 and independent records. The 95% confidence half-width of the mean
  43 | time-history variance will be below 1%; full-QTF and Monte Carlo variances will agree within 1%.</li>
  44 | <li>A linear surge oscillator will use synthetic mass, stiffness and damping. A time-domain ODE response will be compared to the repository full-QTF
  45 | standard deviation using a QTF weighted by the oscillator transfer magnitude at each difference frequency.
  46 | Transients will be discarded, and standard deviations will agree within 5%.</li>
  47 | <li>The existing drift, spectrum, motion-statistics and relevant package tests will be run. Every tracked reference/import/call to both functions
  48 | will be inspected, and the PR will list every full-QTF caller plus the package re-export and new tests.</li>
  49 | <li>Adversarial plan and code reviews will use frozen SHA-256 packets with verification before verdict acceptance.
  50 | Any unavailable provider will be recorded without treating it as approval. No-MAJOR findings will precede publication.</li>
  51 | </ol>
  52 | <h2>Artifacts and retention</h2>
  53 | <table><tr><th>Purpose</th><th>Proposed artifact</th></tr>
  54 | <tr><td>Implementation</td><td>src/digitalmodel/orcawave/drift_forces.py</td></tr>
  55 | <tr><td>Regression/reference harness</td><td>tests/orcawave/test_drift_force_variance.py</td></tr>
  56 | <tr><td>Calculation evidence</td><td>docs/verification/issue-2617-qtf-variance.html</td></tr>
  57 | <tr><td>Frozen review evidence</td><td>scripts/review/results/2617/</td></tr></table>
  58 | <p>Only synthetic inputs, compact metrics and reproducible generation code will be retained in Git. Full time arrays and QTF matrices will be generated
  59 | in memory and discarded. Input units and criteria will be pinned in the harness/report; no external dataset ownership will be implied.</p>
  60 | <h2>Review, publication and closeout</h2>
  61 | <p>Claude and Gemini/Agy reviews will be requested alongside the main-session Codex adversarial check at both stages.
  62 | Changed reviewed bytes will invalidate the corresponding verdict. Review transcripts will record actual provider availability.
  63 | The public outgoing bundle will be checked for secrets, client data and source rights. The cleanup audit will classify retained worktree/evidence as expected.
  64 | The verified changes will be committed and pushed without force or hook bypass; one draft PR will reference the issue and carry the result and caller audit.</p>
  65 | </html>
```

## src/digitalmodel/orcawave/drift_forces.py

sha256 `b33d7cc8f677d4f61a42f2f5d9b96c95c57943d3d600c58baf721548b4872fa6` · 10079 bytes · 298 lines

```
   1 | """
   2 | ABOUTME: Mean drift and slowly-varying drift force calculations for OrcaWave.
   3 | Implements Newman's approximation for QTF, full QTF double-sum integration,
   4 | mean drift force coefficient handling, and wind/current drift estimation.
   5 | """
   6 | 
   7 | from typing import Dict, List, Optional, Tuple
   8 | 
   9 | import numpy as np
  10 | from pydantic import BaseModel, Field
  11 | 
  12 | 
  13 | # ---------------------------------------------------------------------------
  14 | # Pydantic models
  15 | # ---------------------------------------------------------------------------
  16 | 
  17 | class MeanDriftCoefficients(BaseModel):
  18 |     """Frequency-dependent mean drift force coefficients for one heading."""
  19 | 
  20 |     heading_deg: float = Field(..., description="Wave heading in degrees")
  21 |     frequencies_rad_s: List[float] = Field(..., description="Frequency vector")
  22 |     surge_kn_m2: List[float] = Field(
  23 |         ...,
  24 |         description="Mean drift force coefficient in surge (kN/m^2)",
  25 |     )
  26 |     sway_kn_m2: List[float] = Field(
  27 |         ...,
  28 |         description="Mean drift force coefficient in sway (kN/m^2)",
  29 |     )
  30 |     yaw_knm_m2: List[float] = Field(
  31 |         ...,
  32 |         description="Mean drift moment coefficient in yaw (kN·m/m^2)",
  33 |     )
  34 | 
  35 | 
  36 | class QTFMatrix(BaseModel):
  37 |     """Quadratic Transfer Function matrix for one DOF and heading."""
  38 | 
  39 |     dof: str = Field(..., description="Degree of freedom (surge, sway, yaw)")
  40 |     heading_deg: float
  41 |     frequencies_rad_s: List[float] = Field(
  42 |         ...,
  43 |         description="Frequency vector (same for both axes)",
  44 |     )
  45 |     real_part: List[List[float]] = Field(
  46 |         ...,
  47 |         description="Real part of QTF matrix [n_freq x n_freq]",
  48 |     )
  49 |     imag_part: List[List[float]] = Field(
  50 |         ...,
  51 |         description="Imaginary part of QTF matrix [n_freq x n_freq]",
  52 |     )
  53 | 
  54 | 
  55 | class DriftForceResult(BaseModel):
  56 |     """Mean and slowly-varying drift force results."""
  57 | 
  58 |     heading_deg: float
  59 |     mean_surge_kn: float = Field(..., description="Mean drift force in surge (kN)")
  60 |     mean_sway_kn: float = Field(..., description="Mean drift force in sway (kN)")
  61 |     mean_yaw_knm: float = Field(..., description="Mean drift moment in yaw (kN·m)")
  62 |     sv_surge_std_kn: float = Field(
  63 |         default=0.0,
  64 |         description="Standard deviation of slowly-varying surge force (kN)",
  65 |     )
  66 |     sv_sway_std_kn: float = Field(
  67 |         default=0.0,
  68 |         description="Standard deviation of slowly-varying sway force (kN)",
  69 |     )
  70 | 
  71 | 
  72 | class WindCurrentDrift(BaseModel):
  73 |     """Steady drift force from wind and current."""
  74 | 
  75 |     wind_speed_m_s: float
  76 |     current_speed_m_s: float
  77 |     heading_deg: float
  78 |     wind_force_surge_kn: float
  79 |     wind_force_sway_kn: float
  80 |     wind_moment_yaw_knm: float
  81 |     current_force_surge_kn: float
  82 |     current_force_sway_kn: float
  83 |     current_moment_yaw_knm: float
  84 |     total_surge_kn: float
  85 |     total_sway_kn: float
  86 |     total_yaw_knm: float
  87 | 
  88 | 
  89 | # ---------------------------------------------------------------------------
  90 | # Mean drift forces
  91 | # ---------------------------------------------------------------------------
  92 | 
  93 | def compute_mean_drift_force(
  94 |     drift_coeffs: MeanDriftCoefficients,
  95 |     wave_spectrum: np.ndarray,
  96 |     omega: np.ndarray,
  97 | ) -> Tuple[float, float, float]:
  98 |     """Compute mean drift forces from coefficients and wave spectrum.
  99 | 
 100 |     F_drift = integral( 2 * D(omega) * S(omega) d_omega )
 101 | 
 102 |     where D(omega) is the mean drift coefficient and S(omega) the wave spectrum.
 103 | 
 104 |     Args:
 105 |         drift_coeffs: Frequency-dependent drift coefficients for one heading.
 106 |         wave_spectrum: Wave spectral density S(omega).
 107 |         omega: Frequency vector (rad/s).
 108 | 
 109 |     Returns:
 110 |         (surge_kN, sway_kN, yaw_kNm) mean drift forces.
 111 |     """
 112 |     freqs = np.array(drift_coeffs.frequencies_rad_s)
 113 |     d_surge = np.interp(omega, freqs, drift_coeffs.surge_kn_m2)
 114 |     d_sway = np.interp(omega, freqs, drift_coeffs.sway_kn_m2)
 115 |     d_yaw = np.interp(omega, freqs, drift_coeffs.yaw_knm_m2)
 116 | 
 117 |     f_surge = float(np.trapz(2.0 * d_surge * wave_spectrum, omega))
 118 |     f_sway = float(np.trapz(2.0 * d_sway * wave_spectrum, omega))
 119 |     m_yaw = float(np.trapz(2.0 * d_yaw * wave_spectrum, omega))
 120 | 
 121 |     return f_surge, f_sway, m_yaw
 122 | 
 123 | 
 124 | # ---------------------------------------------------------------------------
 125 | # Newman's approximation
 126 | # ---------------------------------------------------------------------------
 127 | 
 128 | def newman_approximation(
 129 |     drift_coeffs: MeanDriftCoefficients,
 130 |     wave_spectrum: np.ndarray,
 131 |     omega: np.ndarray,
 132 |     dof: str = "surge",
 133 | ) -> np.ndarray:
 134 |     """Newman's approximation for the slowly-varying QTF.
 135 | 
 136 |     QTF_ij ~ 0.5 * (D(omega_i) + D(omega_j))
 137 | 
 138 |     where D(omega) is the diagonal (mean drift) coefficient.
 139 | 
 140 |     Args:
 141 |         drift_coeffs: Mean drift coefficients.
 142 |         wave_spectrum: Wave spectral density.
 143 |         omega: Frequency vector.
 144 |         dof: Degree of freedom ('surge', 'sway', or 'yaw').
 145 | 
 146 |     Returns:
 147 |         Slowly-varying force time series standard deviation spectrum.
 148 |     """
 149 |     freqs = np.array(drift_coeffs.frequencies_rad_s)
 150 | 
 151 |     if dof == "surge":
 152 |         d_vals = np.array(drift_coeffs.surge_kn_m2)
 153 |     elif dof == "sway":
 154 |         d_vals = np.array(drift_coeffs.sway_kn_m2)
 155 |     else:
 156 |         d_vals = np.array(drift_coeffs.yaw_knm_m2)
 157 | 
 158 |     d_interp = np.interp(omega, freqs, d_vals)
 159 | 
 160 |     n = len(omega)
 161 |     # Build Newman QTF matrix
 162 |     qtf_newman = np.zeros((n, n))
 163 |     for i in range(n):
 164 |         for j in range(n):
 165 |             qtf_newman[i, j] = 0.5 * (d_interp[i] + d_interp[j])
 166 | 
 167 |     # Slowly-varying force spectrum (difference-frequency)
 168 |     # S_sv(mu) = 8 * integral( QTF(omega, omega+mu)^2 * S(omega) * S(omega+mu) d_omega )
 169 |     d_omega = omega[1] - omega[0] if len(omega) > 1 else 1.0
 170 |     sv_spectrum = np.zeros(n)
 171 |     for k in range(n):
 172 |         total = 0.0
 173 |         for i in range(n - k):
 174 |             j = i + k
 175 |             total += qtf_newman[i, j] ** 2 * wave_spectrum[i] * wave_spectrum[j]
 176 |         sv_spectrum[k] = 8.0 * total * d_omega
 177 | 
 178 |     return sv_spectrum
 179 | 
 180 | 
 181 | def full_qtf_slowly_varying(
 182 |     qtf: QTFMatrix,
 183 |     wave_spectrum: np.ndarray,
 184 |     omega: np.ndarray,
 185 | ) -> float:
 186 |     """Compute slowly-varying drift force std using full QTF integration.
 187 | 
 188 |     sigma_sv^2 = 8 * double_sum( |QTF(omega_i, omega_j)|^2
 189 |                                   * S(omega_i) * S(omega_j) * d_omega^2 )
 190 | 
 191 |     Args:
 192 |         qtf: Full QTF matrix.
 193 |         wave_spectrum: Wave spectral density.
 194 |         omega: Frequency vector.
 195 | 
 196 |     Returns:
 197 |         Standard deviation of slowly-varying drift force.
 198 |     """
 199 |     freqs = np.array(qtf.frequencies_rad_s)
 200 |     qr = np.array(qtf.real_part)
 201 |     qi = np.array(qtf.imag_part)
 202 |     qtf_mag2 = qr**2 + qi**2
 203 | 
 204 |     # Interpolate wave spectrum to QTF frequency grid
 205 |     s_interp = np.interp(freqs, omega, wave_spectrum)
 206 | 
 207 |     d_omega = freqs[1] - freqs[0] if len(freqs) > 1 else 1.0
 208 | 
 209 |     # Double sum
 210 |     n = len(freqs)
 211 |     variance = 0.0
 212 |     for i in range(n):
 213 |         for j in range(n):
 214 |             variance += qtf_mag2[i, j] * s_interp[i] * s_interp[j]
 215 |     variance *= 8.0 * d_omega**2
 216 | 
 217 |     return float(np.sqrt(max(variance, 0.0)))
 218 | 
 219 | 
 220 | # ---------------------------------------------------------------------------
 221 | # Wind and current drift
 222 | # ---------------------------------------------------------------------------
 223 | 
 224 | def compute_wind_current_drift(
 225 |     wind_speed_m_s: float,
 226 |     current_speed_m_s: float,
 227 |     heading_deg: float,
 228 |     projected_wind_area_m2: float = 3000.0,
 229 |     projected_current_area_m2: float = 1500.0,
 230 |     vessel_length_m: float = 250.0,
 231 |     cx_wind: float = 1.0,
 232 |     cy_wind: float = 0.8,
 233 |     cx_current: float = 0.08,
 234 |     cy_current: float = 1.2,
 235 |     rho_air: float = 1.225,
 236 |     rho_water: float = 1025.0,
 237 | ) -> WindCurrentDrift:
 238 |     """Estimate steady drift forces from wind and current.
 239 | 
 240 |     Uses standard drag-type formulations:
 241 |         F = 0.5 * rho * C * A * V^2
 242 | 
 243 |     Args:
 244 |         wind_speed_m_s: Wind speed (m/s).
 245 |         current_speed_m_s: Current speed (m/s).
 246 |         heading_deg: Environmental heading (deg, 0=head, 90=beam).
 247 |         projected_wind_area_m2: Projected above-water area (m^2).
 248 |         projected_current_area_m2: Projected below-water area (m^2).
 249 |         vessel_length_m: Vessel length for yaw moment arm.
 250 |         cx_wind: Wind drag coefficient in surge direction.
 251 |         cy_wind: Wind drag coefficient in sway direction.
 252 |         cx_current: Current drag coefficient in surge direction.
 253 |         cy_current: Current drag coefficient in sway direction.
 254 |         rho_air: Air density (kg/m^3).
 255 |         rho_water: Water density (kg/m^3).
 256 | 
 257 |     Returns:
 258 |         WindCurrentDrift with all force components.
 259 |     """
 260 |     theta = np.deg2rad(heading_deg)
 261 |     cos_t = np.cos(theta)
 262 |     sin_t = np.sin(theta)
 263 | 
 264 |     # Wind forces
 265 |     q_wind = 0.5 * rho_air * wind_speed_m_s**2
 266 |     fw_surge = q_wind * cx_wind * projected_wind_area_m2 * abs(cos_t) * np.sign(cos_t)
 267 |     fw_sway = q_wind * cy_wind * projected_wind_area_m2 * abs(sin_t) * np.sign(sin_t)
 268 |     # Yaw moment: assume moment arm ~ 0.1 * vessel length
 269 |     mw_yaw = fw_sway * 0.1 * vessel_length_m
 270 | 
 271 |     # Current forces
 272 |     q_curr = 0.5 * rho_water * current_speed_m_s**2
 273 |     fc_surge = q_curr * cx_current * projected_current_area_m2 * abs(cos_t) * np.sign(cos_t)
 274 |     fc_sway = q_curr * cy_current * projected_current_area_m2 * abs(sin_t) * np.sign(sin_t)
 275 |     mc_yaw = fc_sway * 0.1 * vessel_length_m
 276 | 
 277 |     # Convert to kN
 278 |     fw_surge_kn = float(fw_surge / 1000.0)
 279 |     fw_sway_kn = float(fw_sway / 1000.0)
 280 |     mw_yaw_knm = float(mw_yaw / 1000.0)
 281 |     fc_surge_kn = float(fc_surge / 1000.0)
 282 |     fc_sway_kn = float(fc_sway / 1000.0)
 283 |     mc_yaw_knm = float(mc_yaw / 1000.0)
 284 | 
 285 |     return WindCurrentDrift(
 286 |         wind_speed_m_s=wind_speed_m_s,
 287 |         current_speed_m_s=current_speed_m_s,
 288 |         heading_deg=heading_deg,
 289 |         wind_force_surge_kn=fw_surge_kn,
 290 |         wind_force_sway_kn=fw_sway_kn,
 291 |         wind_moment_yaw_knm=mw_yaw_knm,
 292 |         current_force_surge_kn=fc_surge_kn,
 293 |         current_force_sway_kn=fc_sway_kn,
 294 |         current_moment_yaw_knm=mc_yaw_knm,
 295 |         total_surge_kn=fw_surge_kn + fc_surge_kn,
 296 |         total_sway_kn=fw_sway_kn + fc_sway_kn,
 297 |         total_yaw_knm=mw_yaw_knm + mc_yaw_knm,
 298 |     )
```

## tests/orcawave/test_drift_forces.py

sha256 `45a2e0336c1c19bfc737b89087697a4207924a4196c9cee6128b61f79852e3ec` · 4845 bytes · 141 lines

```
   1 | """Tests for orcawave.drift_forces module."""
   2 | 
   3 | import numpy as np
   4 | import pytest
   5 | 
   6 | from digitalmodel.orcawave.drift_forces import (
   7 |     MeanDriftCoefficients,
   8 |     QTFMatrix,
   9 |     compute_mean_drift_force,
  10 |     compute_wind_current_drift,
  11 |     full_qtf_slowly_varying,
  12 |     newman_approximation,
  13 | )
  14 | from digitalmodel.orcawave.wave_spectrum import pierson_moskowitz
  15 | 
  16 | 
  17 | def _make_drift_coeffs() -> MeanDriftCoefficients:
  18 |     """Helper: create realistic drift coefficients."""
  19 |     freqs = [0.3, 0.4, 0.5, 0.6, 0.8, 1.0, 1.2, 1.5]
  20 |     # Typical mean drift: increases with frequency squared (short waves)
  21 |     surge = [10 * f**2 for f in freqs]
  22 |     sway = [8 * f**2 for f in freqs]
  23 |     yaw = [50 * f**2 for f in freqs]
  24 |     return MeanDriftCoefficients(
  25 |         heading_deg=0.0,
  26 |         frequencies_rad_s=freqs,
  27 |         surge_kn_m2=surge,
  28 |         sway_kn_m2=sway,
  29 |         yaw_knm_m2=yaw,
  30 |     )
  31 | 
  32 | 
  33 | class TestMeanDriftForce:
  34 |     """Test mean drift force computation."""
  35 | 
  36 |     def test_positive_drift(self):
  37 |         coeffs = _make_drift_coeffs()
  38 |         omega = np.linspace(0.1, 2.0, 200)
  39 |         s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
  40 |         surge, sway, yaw = compute_mean_drift_force(coeffs, s_wave, omega)
  41 |         assert surge > 0  # positive drift coefficients -> positive force
  42 |         assert sway > 0
  43 | 
  44 |     def test_zero_spectrum_zero_drift(self):
  45 |         coeffs = _make_drift_coeffs()
  46 |         omega = np.linspace(0.1, 2.0, 200)
  47 |         s_wave = np.zeros_like(omega)
  48 |         surge, sway, yaw = compute_mean_drift_force(coeffs, s_wave, omega)
  49 |         np.testing.assert_allclose(surge, 0.0, atol=1e-15)
  50 | 
  51 |     def test_larger_hs_larger_drift(self):
  52 |         coeffs = _make_drift_coeffs()
  53 |         omega = np.linspace(0.1, 2.0, 200)
  54 |         s1 = pierson_moskowitz(omega, hs=2.0, tp=10.0)
  55 |         s2 = pierson_moskowitz(omega, hs=5.0, tp=10.0)
  56 |         surge1, _, _ = compute_mean_drift_force(coeffs, s1, omega)
  57 |         surge2, _, _ = compute_mean_drift_force(coeffs, s2, omega)
  58 |         assert surge2 > surge1
  59 | 
  60 | 
  61 | class TestNewmanApproximation:
  62 |     """Test Newman's approximation for slowly-varying forces."""
  63 | 
  64 |     def test_sv_spectrum_positive(self):
  65 |         coeffs = _make_drift_coeffs()
  66 |         omega = np.linspace(0.1, 2.0, 50)
  67 |         s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
  68 |         sv = newman_approximation(coeffs, s_wave, omega, dof="surge")
  69 |         assert np.all(sv >= 0)
  70 | 
  71 |     def test_sv_spectrum_length(self):
  72 |         coeffs = _make_drift_coeffs()
  73 |         omega = np.linspace(0.1, 2.0, 50)
  74 |         s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
  75 |         sv = newman_approximation(coeffs, s_wave, omega, dof="surge")
  76 |         assert len(sv) == len(omega)
  77 | 
  78 |     def test_sv_zero_spectrum(self):
  79 |         coeffs = _make_drift_coeffs()
  80 |         omega = np.linspace(0.1, 2.0, 50)
  81 |         s_wave = np.zeros_like(omega)
  82 |         sv = newman_approximation(coeffs, s_wave, omega, dof="surge")
  83 |         np.testing.assert_allclose(sv, 0.0, atol=1e-15)
  84 | 
  85 | 
  86 | class TestFullQTF:
  87 |     """Test full QTF slowly-varying computation."""
  88 | 
  89 |     def test_full_qtf_positive_std(self):
  90 |         freqs = [0.3, 0.5, 0.8, 1.0]
  91 |         n = len(freqs)
  92 |         # Create simple diagonal-dominant QTF
  93 |         real_part = np.eye(n) * 10.0
  94 |         imag_part = np.zeros((n, n))
  95 |         qtf = QTFMatrix(
  96 |             dof="surge",
  97 |             heading_deg=0.0,
  98 |             frequencies_rad_s=freqs,
  99 |             real_part=real_part.tolist(),
 100 |             imag_part=imag_part.tolist(),
 101 |         )
 102 |         omega = np.array(freqs)
 103 |         s_wave = pierson_moskowitz(omega, hs=3.0, tp=10.0)
 104 |         sigma = full_qtf_slowly_varying(qtf, s_wave, omega)
 105 |         assert sigma >= 0
 106 | 
 107 | 
 108 | class TestWindCurrentDrift:
 109 |     """Test wind and current drift estimation."""
 110 | 
 111 |     def test_head_wind(self):
 112 |         result = compute_wind_current_drift(
 113 |             wind_speed_m_s=20.0,
 114 |             current_speed_m_s=1.0,
 115 |             heading_deg=0.0,
 116 |         )
 117 |         assert result.wind_force_surge_kn != 0
 118 |         assert result.total_surge_kn != 0
 119 | 
 120 |     def test_beam_wind_sway(self):
 121 |         result = compute_wind_current_drift(
 122 |             wind_speed_m_s=20.0,
 123 |             current_speed_m_s=0.0,
 124 |             heading_deg=90.0,
 125 |         )
 126 |         # At 90 deg, sway should be dominant
 127 |         assert abs(result.wind_force_sway_kn) > abs(result.wind_force_surge_kn)
 128 | 
 129 |     def test_zero_wind_zero_force(self):
 130 |         result = compute_wind_current_drift(
 131 |             wind_speed_m_s=0.0,
 132 |             current_speed_m_s=0.0,
 133 |             heading_deg=0.0,
 134 |         )
 135 |         np.testing.assert_allclose(result.total_surge_kn, 0.0)
 136 |         np.testing.assert_allclose(result.total_sway_kn, 0.0)
 137 | 
 138 |     def test_higher_wind_higher_force(self):
 139 |         r1 = compute_wind_current_drift(10.0, 0.0, 0.0)
 140 |         r2 = compute_wind_current_drift(30.0, 0.0, 0.0)
 141 |         assert abs(r2.wind_force_surge_kn) > abs(r1.wind_force_surge_kn)
```
