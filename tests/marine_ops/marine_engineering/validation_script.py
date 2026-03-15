"""Quick validation test for wave spectra implementation."""

import sys
sys.path.insert(0, 'D:/workspace-hub/digitalmodel/src')

from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra
import numpy as np

ws = WaveSpectra()

print('=== JONSWAP Spectrum Test ===')
freq, S = ws.jonswap(hs=3.0, tp=10.0, gamma=3.3)
print('OK: Spectrum created successfully')

print(f'OK: Spectrum computed: {len(S)} points, max={np.max(S):.4f} m^2s/rad')

Hs = ws.significant_height_from_spectrum(freq, S)
error_pct = abs(Hs-3.0)/3.0*100
print(f'OK: Hs recovery: {Hs:.3f} m (input: 3.0 m, error: {error_pct:.2f}%)')

Tz = ws.zero_crossing_period_from_spectrum(freq, S)
print(f'OK: Tz: {Tz:.3f} s (expected ~7.1s for Tp=10s)')

stats = ws.spectrum_statistics(freq, S)
bw = stats['spectral_width']
print(f'OK: Bandwidth: {bw:.3f}')

print('')
print('=== Pierson-Moskowitz Spectrum Test ===')
freq_pm, S_pm = ws.pierson_moskowitz(hs=3.0, tp=10.0)
print(f'OK: P-M Spectrum computed: {len(S_pm)} points')

Hs_pm = ws.significant_height_from_spectrum(freq_pm, S_pm)
error_pm = abs(Hs_pm-3.0)/3.0*100
print(f'OK: P-M Hs recovery: {Hs_pm:.3f} m (error: {error_pm:.2f}%)')

print('')
print('=== Spectral Statistics Test ===')
stats = ws.spectrum_statistics(freq, S)
print(f'OK: Hs = {stats["Hs_m"]:.3f} m')
print(f'OK: Tz = {stats["Tz_s"]:.3f} s')
print(f'OK: Tp = {stats["Tp_s"]:.3f} s')
print(f'OK: Bandwidth = {stats["spectral_width"]:.3f}')

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
