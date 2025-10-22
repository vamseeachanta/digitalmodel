# Critical Damping Ratios at RAO Peak Periods

## Overview

Critical damping ratios were extracted from the **“APPROXIMATE PERCENTAGE CRITICAL DAMPING”** sections of every LIS file under `D:\1522\ctr9\fatigue_full\rev_a08\base_files\aqwa_to_ofx\input`. For each vessel, draft, and degree of freedom (DOF), the RAO dataset was scanned to locate the peak RMS response over headings, and the nearest critical-damping entry was recorded. All raw results live in `critical_damping_at_peak_periods.csv`, which now includes the vessel identifier and LNG load percentage for every record.

## Peak-Period Summary

| Vessel | Case ID | LNG Load % | DOF | Peak Frequency (rad/s) | Peak Period (s) | ζ<sub>min</sub> | ζ<sub>max</sub> |
| --- | --- | --- | --- | --- | --- | --- | --- |
| FST1 | fst_pair_l015 | 15 | heave | 0.18 | 35.0 | 0.195 | 0.201 |
| FST1 | fst_pair_l015 | 15 | pitch | 0.18 | 35.0 | 0.043 | 0.079 |
| FST1 | fst_pair_l015 | 15 | roll | 0.18 | 35.0 | 0.002 | 0.003 |
| FST2 | fst_pair_l095 | 95 | heave | 0.18 | 35.0 | 0.157 | 0.182 |
| FST2 | fst_pair_l095 | 95 | pitch | 0.18 | 35.0 | 0.055 | 0.117 |
| FST2 | fst_pair_l095 | 95 | roll | 0.18 | 35.0 | 0.000 | 0.001 |
| LNGC | lngc_125km3 | 0 | heave | 0.18 | 35.0 | 0.165 | 0.188 |
| LNGC | lngc_125km3 | 0 | pitch | 0.18 | 35.0 | 0.042 | 0.105 |
| LNGC | lngc_125km3 | 0 | roll | 0.18 | 35.0 | 0.000 | 0.001 |
| LNGC | lngc_125km3 | 100 | heave | 0.18 | 35.0 | 0.169 | 0.213 |
| LNGC | lngc_125km3 | 100 | pitch | 0.18 | 35.0 | 0.038 | 0.062 |
| LNGC | lngc_125km3 | 100 | roll | 0.18 | 35.0 | 0.002 | 0.003 |
| LNGC | lngc_180km3 | 0 | heave | 0.18 | 35.0 | 0.163 | 0.188 |
| LNGC | lngc_180km3 | 0 | pitch | 0.18 | 35.0 | 0.042 | 0.104 |
| LNGC | lngc_180km3 | 0 | roll | 0.18 | 35.0 | 0.000 | 0.001 |
| LNGC | lngc_180km3 | 100 | heave | 0.18 | 35.0 | 0.167 | 0.212 |
| LNGC | lngc_180km3 | 100 | pitch | 0.18 | 35.0 | 0.037 | 0.063 |
| LNGC | lngc_180km3 | 100 | roll | 0.18 | 35.0 | 0.002 | 0.003 |

### Heave DOF

| Vessel | Case ID | LNG Load % | ζ<sub>min</sub> | ζ<sub>max</sub> |
| --- | --- | --- | --- | --- |
| FST1 | fst_pair_l015 | 15 | 0.195 | 0.201 |
| FST2 | fst_pair_l095 | 95 | 0.157 | 0.182 |
| LNGC | lngc_125km3 | 0 | 0.165 | 0.188 |
| LNGC | lngc_125km3 | 100 | 0.169 | 0.213 |
| LNGC | lngc_180km3 | 0 | 0.163 | 0.188 |
| LNGC | lngc_180km3 | 100 | 0.167 | 0.212 |

### Pitch DOF

| Vessel | Case ID | LNG Load % | ζ<sub>min</sub> | ζ<sub>max</sub> |
| --- | --- | --- | --- | --- |
| FST1 | fst_pair_l015 | 15 | 0.043 | 0.079 |
| FST2 | fst_pair_l095 | 95 | 0.055 | 0.117 |
| LNGC | lngc_125km3 | 0 | 0.042 | 0.105 |
| LNGC | lngc_125km3 | 100 | 0.038 | 0.062 |
| LNGC | lngc_180km3 | 0 | 0.042 | 0.104 |
| LNGC | lngc_180km3 | 100 | 0.037 | 0.063 |

### Roll DOF

| Vessel | Case ID | LNG Load % | ζ<sub>min</sub> | ζ<sub>max</sub> |
| --- | --- | --- | --- | --- |
| FST1 | fst_pair_l015 | 15 | 0.002 | 0.003 |
| FST2 | fst_pair_l095 | 95 | 0.000 | 0.001 |
| LNGC | lngc_125km3 | 0 | 0.000 | 0.001 |
| LNGC | lngc_125km3 | 100 | 0.002 | 0.003 |
| LNGC | lngc_180km3 | 0 | 0.000 | 0.001 |
| LNGC | lngc_180km3 | 100 | 0.002 | 0.003 |

## Project-Specific Response (Periods < 12 s)

Peak damping magnitudes for short-period motions (< 12 s) were extracted from the same LIS sections. The full dataset is stored in `critical_damping_under_12s_summary.csv`.

### Heave DOF (< 12 s)

| Vessel | Case ID | LNG Load % | Period (s) | Frequency (rad/s) | % Critical | ζ |
| --- | --- | --- | --- | --- | --- | --- |
| FST1 | fst_pair_l015 | 15 | 1.0 | 0.18 | 20.1 | 0.201 |
| FST2 | fst_pair_l095 | 95 | 4.0 | 0.33 | 32.7 | 0.327 |
| LNGC | lngc_125km3 | 0 | 11.0 | 0.68 | 156.4 | 1.564 |
| LNGC | lngc_125km3 | 100 | 11.0 | 0.68 | 58.3 | 0.583 |
| LNGC | lngc_180km3 | 0 | 10.0 | 0.63 | 60.3 | 0.603 |
| LNGC | lngc_180km3 | 100 | 11.0 | 0.68 | 41.6 | 0.416 |

### Pitch DOF (< 12 s)

| Vessel | Case ID | LNG Load % | Period (s) | Frequency (rad/s) | % Critical | ζ |
| --- | --- | --- | --- | --- | --- | --- |
| FST1 | fst_pair_l015 | 15 | 1.0 | 0.18 | 7.9 | 0.079 |
| FST2 | fst_pair_l095 | 95 | 8.0 | 0.53 | 24.4 | 0.244 |
| LNGC | lngc_125km3 | 0 | 11.0 | 0.68 | 70.0 | 0.700 |
| LNGC | lngc_125km3 | 100 | 9.0 | 0.58 | 35.1 | 0.351 |
| LNGC | lngc_180km3 | 0 | 11.0 | 0.68 | 91.1 | 0.911 |
| LNGC | lngc_180km3 | 100 | 11.0 | 0.68 | 52.6 | 0.526 |

### Roll DOF (< 12 s)

| Vessel | Case ID | LNG Load % | Period (s) | Frequency (rad/s) | % Critical | ζ |
| --- | --- | --- | --- | --- | --- | --- |
| FST1 | fst_pair_l015 | 15 | 1.0 | 0.18 | 0.3 | 0.003 |
| FST2 | fst_pair_l095 | 95 | 11.0 | 0.68 | 7.5 | 0.075 |
| LNGC | lngc_125km3 | 0 | 11.0 | 0.68 | 30.2 | 0.302 |
| LNGC | lngc_125km3 | 100 | 10.0 | 0.63 | 44.5 | 0.445 |
| LNGC | lngc_180km3 | 0 | 11.0 | 0.68 | 21.3 | 0.213 |
| LNGC | lngc_180km3 | 100 | 10.0 | 0.63 | 37.6 | 0.376 |

### Literature Context

- Ship-shaped hull surveys and ITTC/OCIMF guidance typically cite **heave/pitch damping ≈ 0.5–2 %** (ζ = 0.005–0.02) and **roll damping ≈ 1–5 %** (ζ = 0.01–0.05), with 10 % considered high even for stabilised roll systems.
- Our AQWA-derived results exactly mirror the `.LIS` values: **FST cases already reach ~0.2–0.33 ζ** in heave/pitch within short periods, and **LNGC cases rise to 0.4–0.9 ζ**, indicating much stronger viscous tuning than standard empirical expectations.
- Roll damping in the AQWA decks spans **0.003–0.45 ζ**, significantly above the survey ranges, so these coefficients should be treated as project-specific calibration rather than generic hull defaults.

## Key Observations

- All vessels exhibit RAO peak response at **0.18 rad/s (≈35 s)** for heave, roll, and pitch.
- **FST1 (15 % LNG)** maintains ζ between 0.002 and 0.201 across DOFs; **FST2 (95 % LNG)** slightly increases pitch damping to ~0.12.
- **LNG carriers** show the widest damping envelope: up to **ζ ≈ 0.21** for heave and **≈0.10** for pitch at 100 % load, while roll remains ≤0.003 across all configurations.
- Zero-load LNGC cases retain ζ ranges similar to the FSTs in pitched response but reach zero damping for roll at multiple drafts.

> **Data provenance:** `critical_damping_at_peak_periods.csv` (this directory) contains the comprehensive row-level output, including vessel, LNG load, draft/direction tags, and the matched critical-damping percentages.
