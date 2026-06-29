# Circumferential / combined-loading metal-loss FFS — validation

**Module:** `src/digitalmodel/asset_integrity/circumferential_defect.py`
**Tests:** `tests/asset_integrity/test_circumferential_defect.py` (17 tests, all green)
**Issue:** digitalmodel #1094 extension
**Date:** 2026-06-29

This module extends the existing corroded-pipe / longitudinal-LTA FFS work
(`corroded_pipe.py`, `dnv_rp_f101.py`) with the assessments that govern a
*circumferentially* extensive or axially-loaded metal-loss flaw. It imports the
DNV-RP-F101 single-defect primitive rather than duplicating it.

## What is implemented vs. stubbed

| # | Method | Status | Golden |
|---|--------|--------|--------|
| 1 | API 579-1 Part 5 longitudinal LTA RSF | **Implemented (fully verified)** | RSF = 0.8907 |
| 2 | API 579-1 Part 5 circumferential extent — net-section membrane RSF | **Implemented (verified)** | RSF_circ = 0.9001 |
| 3 | DNV-RP-F101 Sec. 3.7.4 combined loading (H1) | **STUB / NotImplemented** | see below |

---

## 1. API 579-1/ASME FFS-1 Part 5 longitudinal LTA — RSF (FULLY VERIFIED)

Level-1/Level-2 cylindrical-shell metal-loss screen.

- Shell parameter: `lambda = 1.285 * s / sqrt(D * Tc)`
- Folias factor (Part 5 **cylinder**, longitudinal): `M_t = sqrt( sum_{i=0..10} a_i * lambda^i )`, valid `lambda <= 20`.
- Remaining-thickness ratio: `Rt = (tmm - FCA) / Tc`
- **RSF = Rt / (1 - (1/M_t)*(1 - Rt))**
- Accept if `RSF >= RSF_a` (default 0.90); else reduce MAWP: `MAWP_r = MAWP * RSF/RSF_a`.

### Part 5 cylinder Folias polynomial coefficients (a0..a10)

```
a0 =  1.0010
a1 = -0.014195
a2 =  0.29090
a3 = -0.096420
a4 =  0.020890
a5 = -0.0030540
a6 =  2.9570e-4
a7 = -1.8462e-5
a8 =  7.1553e-7
a9 = -1.5631e-8
a10 = 1.4656e-10
```

Source: API 579-1/ASME FFS-1 Part 5 cylinder (longitudinal flaw) Mt polynomial.
Independently reproduced the supplied golden to 4 decimals.

### Golden (D=39 in, Tc=0.5, s=6, tmm=0.30, FCA=0 → Rt=0.60)

| Quantity | Value |
|----------|-------|
| lambda | 1.746 |
| M_t | 1.2255 |
| Rt | 0.60 |
| **RSF** | **0.8907** → FAIL (< 0.90) |
| MAWP_r factor (RSF/0.90) | 0.9897 |

---

## 2. API 579-1 Part 5 circumferential extent — NET-SECTION membrane (VERIFIED)

> **IMPORTANT:** this is a **net-section / screening** solution for the axial
> (longitudinal-stress) capacity of a circumferentially extensive flaw. It is
> **NOT** a "circumferential Folias" and deliberately does **not** reuse the
> longitudinal `M_t` above. Labelled "net-section membrane RSF" in code + docs.

- Level-1 circumferential extent screen: `c <= 2*s*(E_L/E_C)` (E_L, E_C = weld joint efficiencies; 1.0 seamless).
- **RSF_circ = 1 - (d/t) * (c / (pi * Dm))**
- Allowable axial membrane stress = `RSF_circ * sigma_flow`.

### Golden (d/t = 0.40, 90° arc: c = 31.0 in, Dm = 39.5)

`RSF_circ = 1 - 0.40 * (31.0 / (pi * 39.5)) = ` **0.9001**.
(90° arc check: `pi*Dm/4 = 31.02 ≈ c`.)

---

## 3. DNV-RP-F101 Sec. 3.7.4 combined loading — STUB (no fabricated H1)

Internal pressure + superimposed **longitudinal compressive** stress reduces the
pure-pressure single-defect capacity by a factor `H1 < 1`:

```
p_corr,comp = [pure-pressure capacity] * H1
```

where the pure-pressure base reuses
`dnv_rp_f101.dnv_f101_single_defect` (= `2*t*f_u/(D-t) * (1-d/t)/(1-(d/t)/Q)`).

### Why this is stubbed

The exact closed form of `H1` is given in DNV-RP-F101 Sec. 3.7.4. The published
editions accessible during this work typeset that equation **as an image**, so
the extracted text layer is corrupt/garbled (e.g. `H1 = (1 + .../(ξ·SMTS·A_r)) /
(1 - γ_m·.../(...(1-γ_d(d/t)*)))` could not be read unambiguously). The symbol
set was confirmed (ξ = usage factor for longitudinal stress; θ = c/(π·D);
A_r = corroded-area ratio; structure
`p_corr,comp = γ_m·2t·SMTS·(1-γ_d(d/t)*) / [(D-t)(1-γ_d(d/t)*/Q)] · H1`), but the
`H1` fraction itself was **not** obtained verbatim.

Per the task instruction, `H1` is **not approximated or guessed**. The
combined-loading entry point `dnv_f101_combined_loading(...)` computes the real
pure-pressure base and then raises `NotImplementedError` (with the base capacity
in the message), rather than ship a fabricated `H1`.

### Validation anchor recorded for a future exact implementation

From the DNV Sesam *RP-F101* user manual worked example (Part A, "Combined
loading" sheet). **Not used as a passing test** because `H1` is not implemented:

| Input | Value |
|-------|-------|
| sigma_L (compressive longitudinal stress) | -190.00 MPa |
| c (circumferential length of corroded region) | 50.00 mm |
| theta = c/(pi*D) | 0.02 |
| d/t (base case) | 0.250 |
| gamma_m | 0.85 |
| gamma_d | 1.275 |
| f_u | 495.26 MPa |
| **p_corr (pure pressure)** | **171.6 bar** |
| **p_corr,comp (combined)** | **163.31 bar** |

Implied `H1 = 163.31 / 171.6 = 0.9517`. A single worked point is **insufficient**
to uniquely validate a multi-parameter closed form; an exact transcription of
the Sec. 3.7.4 `H1` equation from a clean copy of DNV-RP-F101 is required before
this path is implemented.

### Sources consulted

- DNV Sesam *RP-F101* user manual (worked example; references "Sec. 3.7.4" without reproducing the equation) — https://sesam.dnv.com/download/userdocumentation/rp-f101-user-manual.pdf
- BSEE TAP report 265AH (BG/DNV background to RP-F101; Sec. 3.3 combined loading — equation is an image, text layer garbled) — https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program//265ah.pdf
- ISOPE 1999 "Introduction and Background to DNV RP-F101" (background; no clean H1) — http://publications.isope.org/proceedings/ISOPE/ISOPE%201999/papers/I99v2p117.pdf

---

## Conventions

- Two `# ABOUTME:` lines on the module + test file.
- Result dataclasses (`Part5LongitudinalRSFResult`, `NetSectionCircumferentialResult`) follow the `CorrodedPipeResult` / `DNVF101Result` pattern (`method`, `details`, `code_reference`).
- `code_reference` from `digitalmodel.codes` (`API_579.label`, `DNV_RP_F101.label`).
- No pandas (numpy / stdlib `math` only — pandas FutureWarning is a CI error).
- `black --check` + `flake8 --select=E9,F63,F7,F82,F401,F811,F841` clean.
- Run: `PYTHONPATH=$PWD/src .venv/bin/python -m pytest tests/asset_integrity/test_circumferential_defect.py -p no:cacheprovider -o addopts="" -q`
