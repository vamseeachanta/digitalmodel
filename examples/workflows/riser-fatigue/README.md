# Riser Fatigue (Wave + VIV)

Combined wave and vortex-induced-vibration (VIV) fatigue screening for a Steel
Catenary Riser (SCR). For each riser segment the workflow accumulates
Palmgren-Miner damage from two independent contributions and sums them
(DNV-RP-C203 bilinear S-N + DNV-OS-F201 DFF):

- **Wave fatigue** — a long-term hot-spot stress-range histogram (cycles per
  stress bin) with an SCF and the DNV thickness correction, evaluated through
  the licence-free touchdown-zone fatigue core.
- **VIV fatigue** — narrow-band lock-in cases, each a (stress range, frequency,
  annual exposure) triple; cycles = frequency x exposure-seconds x design life.

The governing (worst) segment drives the top-level fatigue life, DFF margin and
pass/fail verdict.

This template is **offline-analytical and licence-free**. It consumes a stress
response (real or synthetic) and reimplements no S-N math. The OrcaFlex /
Shear7 / VIVANA front-ends that *produce* the wave histogram and VIV stress
amplitudes are a deferred licensed follow-on (see issue #810); wire them in
front of this workflow when a licence is available.

Run:

```bash
uv run python -m digitalmodel examples/workflows/riser-fatigue/input.yml
```

Expected outputs are written to `examples/workflows/riser-fatigue/results/`:
the resolved result configuration `input.yml`, the per-bin damage CSV
`input_riser_fatigue.csv`, and the per-segment summary `input_riser_fatigue_summary.csv`.
