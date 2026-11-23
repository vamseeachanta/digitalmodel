# Liquefaction Triggering Screening

Screens a saturated soil profile for **earthquake-induced liquefaction triggering** using the Seed-Idriss simplified procedure (NCEER / Youd et al. 2001).

Per layer it compares the earthquake-induced cyclic stress ratio (CSR) to the soil's cyclic resistance ratio (CRR):

```
CSR    = 0.65 · (a_max/g) · (σv / σv') · rd
CRR7.5 = 1/(34 − N) + N/135 + 50/(10N + 45)² − 1/200     [N = (N1)60cs]
FS     = (CRR7.5 / CSR) · MSF · K_σ
```

with the depth stress-reduction factor `rd` (Liao & Whitman 1986), the magnitude scaling factor `MSF = 174/Mw^2.56` (Idriss, ≈1.0 at Mw 7.5), and the overburden factor `K_σ` (1.0 for screening). Clean sand with `(N1)60cs ≥ 30` is treated as non-liquefiable. A layer liquefies when `FS` is below the required factor of safety; the workflow reports the per-layer factor of safety, the governing (lowest-FS) layer, and a top-level `screening_status`.

The example screens a saturated sand profile under PGA 0.25 g / Mw 7.0: the shallow loose sand (N = 8, FS ≈ 0.45) and the medium layer liquefy, while the deeper dense sand (N = 28, FS ≈ 1.50) is acceptable → `fail`, governed by the 3 m layer.

Run with `uv run python -m digitalmodel examples/workflows/liquefaction/input.yml`.

Reference: Youd et al. (2001) "Liquefaction Resistance of Soils," *J. Geotech. Geoenviron. Eng.* (NCEER/NSF workshops); Seed & Idriss (1971).
