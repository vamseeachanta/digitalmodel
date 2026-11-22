# Square-cylinder force-history summary

## Plan-lite

Scope: postprocess supplied, uniformly sampled drag and lift coefficient
histories over an explicit half-open averaging interval. Return mean drag,
fluctuating lift RMS, shedding frequency, Strouhal number, sample count and
FFT-bin spacing using the existing spectral API.

Acceptance criteria:

- Independent integer-cycle hand calculations establish mean Cd = 2,
  fluctuating lift RMS = 3/sqrt(2), shedding frequency = 1 Hz and
  St = 1 * 2 / 4 for the synthetic eight-second record sampled at 32 Hz.
- The independently calculated FFT-bin spacing is 32/256 Hz. Interval tests
  exclude the stop bound and earlier samples; a lift offset cannot set the
  shedding frequency through the DC bin.
- Regression cases cover Nyquist, competing tones, constant lift and invalid
  histories, reference scales and interval bounds. Constant lift gives zero
  fluctuating RMS and no identified shedding frequency or Strouhal number.
- The summary tests, existing spectral tests and all five executable example
  steps pass. Synthetic inputs establish postprocessing behavior only.

Exclusions: no solver campaign, solver execution, experimental dataset or
literature comparison. No experimental coefficient or literature agreement
is established. Frequencies are discrete FFT bins without sub-bin estimation;
record duration and sampling must resolve shedding. Rectangular-window
spectral leakage and the four-selected-sample software minimum remain limits.
