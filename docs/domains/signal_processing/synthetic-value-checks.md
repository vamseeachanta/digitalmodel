# Synthetic signal value checks

## Summary

A synthetic single tone checks Fourier amplitude, frequency resolution and
spectral energy against hand calculations. Additional tests check digital
filter gain, command output contracts, compatibility adapters and error
recovery. Production code is unchanged. Strict expected failures retain the
correct averaging and command-output expectations where the current code
does not satisfy them. These checks establish numerical behaviour for the
chosen inputs; they do not qualify field measurements or fatigue designs.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-22 | Added synthetic hand calculations and executable checks to expose numerical and interface gaps. |

## Inputs, method and answer

The worked example is [signal_value_example.py](../examples/signal_value_example.py).
Run it with `PYTHONPATH=src python docs/domains/examples/signal_value_example.py`
in an environment containing NumPy, pandas and SciPy. It writes no files.

| Input | Chosen value |
|---|---|
| Signal | `x[n] = 4 sin(2 pi 16 n/256)` |
| Sample rate | 256.0 Hz |
| Record length | 256 samples, with no repeated endpoint |
| Record duration | 1.0 s |
| Window | Rectangular |
| Detrending for FFT | None |

The [NumPy reference manual, edition 1.26, Discrete Fourier Transform](https://numpy.org/doc/1.26/reference/routines.fft.html)
defines the unnormalized forward transform. Expanding the sine as two
complex exponentials gives two geometric sums. Orthogonality makes all
coefficients zero except `X[16] = -512j` and `X[240] = 512j`.
The one-sided amplitude is `2 |X[16]| / N = 4`, and `df = fs/N = 1/T = 1 Hz`.
Floating-point sine generation and Fourier summation use the requested
absolute tolerance of `1e-9`, with relative tolerance disabled.

The [SciPy user guide, edition 1.14.1 (2024), Spectral Analysis](https://docs.scipy.org/doc/scipy-1.14.1/tutorial/signal.html#spectral-analysis)
sets out Parseval's relation and density normalization. The time-domain
mean square is `A²/2 = 8`; the full Fourier sum is
`(512² + 512²)/256² = 8`. For an interior, positive-frequency tone, doubling
the retained positive-frequency power recovers that full sum. This derivation
does not extend the same doubling to DC or Nyquist bins.

| Output | Hand answer |
|---|---|
| Peak frequency | 16.0 Hz |
| One-sided amplitude | 4.0 |
| Frequency spacing | 1.0 Hz |
| Mean square | 8.0 |
| Integrated one-sided PSD | 8.0 |

The PSD check uses a frequency-bin sum times `df`, including both end bins
with their documented one-sided weights. With a rectangular window, the
single occupied bin has density 8.0. With a periodic Hann window of length
256, `sum(w²) = 3N/8 = 96`. Multiplication by the Hann window distributes
the tone into coefficients of magnitude 128, 256 and 128 at 15, 16 and
17 Hz. The one-sided density `2 |Xw|² / (fs sum(w²))` is therefore
`4/3, 16/3, 4/3`; the area is again 8.0. A four-second input repeats these
identical windows without noise. No reference PSD implementation supplies
the expected bins.

## Filter checks

The [open DSP textbook, hardcover edition 1997, chapter 20](https://www.dspguide.com/CH20.PDF),
page 335, defines the cutoff convention by amplitude 0.707. The
[edition page](https://www.dspguide.com/editions.htm) identifies the hardcover
edition. The [SciPy reference manual, edition 1.14.1, butter](https://docs.scipy.org/doc/scipy-1.14.1/reference/generated/scipy.signal.butter.html)
states the exact Butterworth cutoff gain `1/sqrt(2)`.

For a normalized second-order lowpass with denominator `s² + a s + 1`,
the squared magnitude denominator is `1 + (a²-2)r² + r⁴`.
Maximal flatness at zero, as described for the Butterworth response in
chapter 20, removes the quadratic term: `a²=2`. Stability requires positive
`a`, giving `a=sqrt(2)` and squared magnitude denominator `1+r⁴`. The
[SciPy user guide IIR Filter section](https://docs.scipy.org/doc/scipy-1.14.1/tutorial/signal.html#iir-filter)
gives the bilinear substitution. On the unit circle it yields
`r = tan(pi f/fs)/tan(pi fc/fs)`. At `fs=256 Hz, fc=32 Hz`, the cutoff
gain is exactly `1/sqrt(2)` and DC gain is exactly 1. At 1 Hz, `r < 0.030`,
so the gain differs from 1 by less than `4.1e-7`. The test uses `1e-6`
for that passband comparison and `1e-12` for DC and cutoff roundoff.
Those are derived numerical test allowances, not equipment acceptance limits.

For the second-order 16..64 Hz bandstop and a 32 Hz tone at the same
sampling rate, the guide's lowpass-to-bandstop substitution gives a
normalized stop ratio exceeding 12. The single-pass gain is below `1/144`.
The [filtfilt reference, edition 1.14.1](https://docs.scipy.org/doc/scipy-1.14.1/reference/generated/scipy.signal.filtfilt.html)
describes forward-backward filtering; its steady-state amplitude gain is
the square of the single-pass gain, below `1/20736`. After discarding four
seconds at each end of the synthetic record, the test requires an amplitude
reduction exceeding 1000.0 times. The looser bound accommodates end transients
and rounded intermediate arithmetic. Requested cutoffs at or above
`fs/2 = 128 Hz` must raise `ValueError`.

## Coverage and interpretation

| Slice | New checks | Limit |
|---|---|---|
| 1 | Raw legacy FFT bins, complex tone coefficient, amplitude, Parseval, peak and window flags | Raw power is unnormalized; the one-sided amplitude is derived explicitly. |
| 2 | Variant raw FFT, reconstructed signal and matching hand power | Both legacy averages have the same divisor defect. |
| 3 | Every PSD bin, PSD area, rectangular window average, filter gain, bandstop and Nyquist refusal | No noisy measurements or filter-family sweep. |
| 4 | CLI FFT, PSD and filter numeric file contracts; all four commands reject missing files and nonnumeric input | Three valid-output tests currently fail strictly as expected. |
| 5 | Injected adapter counter boundary, hand damage, migration text, typed error reports and decorator fallback | Default rainflow counting accuracy is NOT_EVALUATED. |

The adapter test injects an independent hand table for
`[0,2,0,2,0,2,0,2,0]`: eight half cycles, each of range 2 and midpoint 1,
total weight 4. It checks conversion and delegation, not the default
algorithm. An artificial curve uses `N=1000 S**(-2)` to give allowable
cycles 250 at range 2 and damage `4/250=0.016`; it represents no material.
The equation is the local compatibility API's explicitly stated law.
Typed error tests use synthetic failures and require the reported type,
message and fallback to survive wrapping. No solver file or licensed
library is used by the new tests or example.

## Findings and way forward

The legacy window average divides by `number_of_windows + 1`. With two
identical windows the expected peak raw power is 262144.0, but the result is
174762.7, which is 33.3 % low. The strict tests retain the hand answer.
The variant also emits pandas chained-assignment warnings when setting peak
flags. The test explicitly requires those existing warnings under pandas 2.2;
behaviour under pandas 3 is not established.

The FFT CLI iterates DataFrame column names as though they were peak records
and expects plural spectrum column names. The PSD CLI similarly reads
`frequencies` while the analyzer returns `frequency`. The filter CLI calls
`TimeSeriesProcessor.filter_signal`, which is absent. Their strict tests
require successful file creation and the hand values above; production fixes
remain separate. Maintainers should repair these interfaces and averaging
before removing their expected-failure markers.

Rainflow numerical output through the CLI and default adapters is
NOT_EVALUATED. The [existing exposure assessment](../fatigue/rainflow-path-exposure-2026-09-11.md)
links the counting-defect issue and identifies `sigproc_astm` and `calm_buoy`
as defective paths. The CLI and adapters use the former. A licensed ASTM
worked example was not accessed, and no ASTM compliance claim is made.
The hand-counted boundary fixture above remains independent of that algorithm.

## Test locations

- `tests/signal_processing/time_series/test_component_values.py`
- `tests/signal_processing/signal_analysis/test_spectral_filter_values.py`
- `tests/signal_processing/signal_analysis/test_cli_values.py`
- `tests/signal_processing/signal_analysis/test_adapter_error_values.py`

The three existing time-series test files exercise mocked engine wrappers,
rather than these numerical component methods. The existing spectral tests
already check frequency detection and general interfaces; the new files add
the complete analytic bin distributions, exact energy and derived filter gains.
The executable example is exercised by the new spectral tests. The public API
inventory and the complete check results accompany the pull request.
