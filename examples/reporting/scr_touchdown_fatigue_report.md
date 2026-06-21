# SCR/SLWR Touchdown-Zone Fatigue Report

**Standard:** DNV-RP-C203 (2021) + DNV-OS-F201 (DFF)
**Critical location:** Touchdown Zone (TDZ)

## Inputs

| Parameter | Value |
|---|---|
| Outer diameter | 323.9 mm |
| Wall thickness | 25.4 mm |
| Material | API 5L X65 |
| S-N detail category | F1 (seawater_cp) |
| SCF | 1.15 |
| Thickness correction | t_ref=25 mm, k=0.25 |
| Histogram period | 1.00 yr |
| Design life | 25 yr |
| Design Fatigue Factor (DFF) | 10.0 |

## Stress-range histogram and per-bin damage

| Nominal ΔS (MPa) | Corrected ΔS (MPa) | Cycles n | Allowable N | Damage n/N |
|---:|---:|---:|---:|---:|
| 20.0 | 23.1 | 2,000,000 | 2.23e+07 | 8.9802e-02 |
| 40.0 | 46.2 | 800,000 | 2.02e+06 | 3.9585e-01 |
| 60.0 | 69.3 | 200,000 | 5.99e+05 | 3.3400e-01 |
| 80.0 | 92.4 | 40,000 | 2.53e+05 | 1.5834e-01 |
| 100.0 | 115.5 | 5,000 | 1.29e+05 | 3.8658e-02 |

## Results

| Quantity | Value |
|---|---|
| Damage per histogram period | 1.0167e+00 |
| Annual damage | 1.0167e+00 |
| Damage over design life (25 yr) | 2.5416e+01 |
| Unfactored fatigue life (D=1) | 1.0 yr |
| Allowable damage (1/DFF) | 1.0000e-01 |
| Usage factor (D_life · DFF) | 254.164 |
| **Verdict** | **FAIL** |

## Deferred (follow-on)

- OrcaFlex time-domain stress extraction + rainflow at the TDZ to generate the histogram consumed by this workflow (issue #810).
