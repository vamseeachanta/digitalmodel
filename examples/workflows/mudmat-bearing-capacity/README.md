# Mudmat Bearing-Capacity Screening

Screens a subsea **mudmat** (shallow rectangular foundation) for vertical bearing capacity and base sliding, per the general bearing-capacity equation with shape, depth and load-eccentricity (effective-area) corrections.

`q_ult = c·Nc·sc·dc + p0'·Nq·sq·dq + 0.5·γ'·B_eff·Nγ·sγ·dγ`

Bearing-capacity factors are Brinch Hansen (1970) / DNV-RP-C212:
`Nq = e^(π·tanφ)·tan²(45+φ/2)`, `Nc = (Nq−1)·cotφ` (φ→0: `Nc = 2+π = 5.14`), `Nγ = 1.5·(Nq−1)·tanφ`.

Two soil conditions: **undrained** (φ=0, total stress, `c = su`) and **drained** (effective stress, φ and c'). Set `loads.eccentricity_axis` to `B` (default) or `L` to select the direction of load displacement, perpendicular to the moment's rotation axis. With `e = abs(M)/V`, Meyerhof's effective area reduces the selected dimension by `2e`. The input dimensions retain `B <= L`; returned effective dimensions retain their axis identities, while bearing factors use the shorter effective dimension as their width. Base sliding resistance is `su·A'` (undrained) or `V·tanδ + c'·A'` (drained).

For a 3 m × 4 m mat with V = 800 kN and M = 400 kN·m, `B` gives `(3 − 1) × 4 = 8 m²`; `L` gives `3 × (4 − 1) = 9 m²`. The calculator accepts the same selection through `eccentricity_axis="L"`. A single moment represents one eccentricity direction; simultaneous biaxial eccentricity is not evaluated.

The workflow applies a factor of safety to the vertical and sliding capacities, computes the utilisations, names the governing check, and emits a top-level `screening_status` (pass/fail). The example drives a soft-clay mudmat to a bearing-governed failure (utilisation ≈ 1.10).

Run with `uv run python -m digitalmodel examples/workflows/mudmat-bearing-capacity/input.yml`.

Reference: DNV-RP-C212 (Offshore soil mechanics and geotechnical engineering); Brinch Hansen J. (1970), *A revised and extended formula for bearing capacity*.
