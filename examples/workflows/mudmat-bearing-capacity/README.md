# Mudmat Bearing-Capacity Screening

Screens a subsea **mudmat** (shallow rectangular foundation) for vertical bearing capacity and base sliding, per the general bearing-capacity equation with shape, depth and load-eccentricity (effective-area) corrections.

`q_ult = c·Nc·sc·dc + p0'·Nq·sq·dq + 0.5·γ'·B_eff·Nγ·sγ·dγ`

Bearing-capacity factors are Brinch Hansen (1970) / DNV-RP-C212:
`Nq = e^(π·tanφ)·tan²(45+φ/2)`, `Nc = (Nq−1)·cotφ` (φ→0: `Nc = 2+π = 5.14`), `Nγ = 1.5·(Nq−1)·tanφ`.

Two soil conditions: **undrained** (φ=0, total stress, `c = su`) and **drained** (effective stress, φ and c'). A moment reduces the effective width via Meyerhof's effective area (`B_eff = B − 2·M/V`). Base sliding resistance is `su·A'` (undrained) or `V·tanδ + c'·A'` (drained).

The workflow applies a factor of safety to the vertical and sliding capacities, computes the utilisations, names the governing check, and emits a top-level `screening_status` (pass/fail). The example drives a soft-clay mudmat to a bearing-governed failure (utilisation ≈ 1.10).

Run with `uv run python -m digitalmodel examples/workflows/mudmat-bearing-capacity/input.yml`.

Reference: DNV-RP-C212 (Offshore soil mechanics and geotechnical engineering); Brinch Hansen J. (1970), *A revised and extended formula for bearing capacity*.
