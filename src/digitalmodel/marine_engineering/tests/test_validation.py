"""Quick validation test for wave spectra implementation."""

import sys
sys.path.insert(0, 'D:/workspace-hub/digitalmodel/src')

from digitalmodel.marine_engineering.wave_spectra import (
    WaveSpectrumParameters,
    JONSWAPSpectrum,
    PiersonMoskowitzSpectrum
)
import numpy as np

print('=== JONSWAP Spectrum Test ===')
params = WaveSpectrumParameters(Hs=3.0, Tp=10.0, gamma=3.3)
spectrum = JONSWAPSpectrum(params)
print('OK: Spectrum created successfully')

S = spectrum.compute_spectrum()
print(f'OK: Spectrum computed: {len(S)} points, max={np.max(S):.4f} m^2s/rad')

Hs = spectrum.significant_wave_height()
error_pct = abs(Hs-3.0)/3.0*100
print(f'OK: Hs recovery: {Hs:.3f} m (input: 3.0 m, error: {error_pct:.2f}%)')

Tz = spectrum.zero_crossing_period()
print(f'OK: Tz: {Tz:.3f} s (expected ~7.1s for Tp=10s)')

bw = spectrum.spectral_bandwidth()
print(f'OK: Bandwidth: {bw:.3f}')

print('')
print('=== Pierson-Moskowitz Spectrum Test ===')
pm = PiersonMoskowitzSpectrum(params)
S_pm = pm.compute_spectrum()
print(f'OK: P-M Spectrum computed: {len(S_pm)} points')

Hs_pm = pm.significant_wave_height()
error_pm = abs(Hs_pm-3.0)/3.0*100
print(f'OK: P-M Hs recovery: {Hs_pm:.3f} m (error: {error_pm:.2f}%)')

print('')
print('=== Spectral Statistics Test ===')
stats = spectrum.get_spectral_statistics()
print(f'OK: Hs = {stats["Hs"]:.3f} m')
print(f'OK: Tz = {stats["Tz"]:.3f} s')
print(f'OK: Tm = {stats["Tm"]:.3f} s')
print(f'OK: Bandwidth = {stats["bandwidth"]:.3f}')

print('')
print('=== Validation Summary ===')
print(f'JONSWAP Hs error: {error_pct:.2f}% (target: < 2%)')
print(f'P-M Hs error: {error_pm:.2f}% (target: < 2%)')

if error_pct < 2.0 and error_pm < 2.0:
    print('PASS: All spectrum validations successful')
    sys.exit(0)
else:
    print('FAIL: Hs recovery errors exceed threshold')
    sys.exit(1)
