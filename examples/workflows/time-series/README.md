# Time Series

Computes an FFT spectrum from a deterministic synthetic signal with a 5 second peak period.

Run:

```bash
uv run python -m digitalmodel examples/workflows/time-series/input.yml
```

Expected outputs are written to `examples/workflows/time-series/results/`, including `input_signal_fft.csv` and the resolved result configuration `input.yml`.
