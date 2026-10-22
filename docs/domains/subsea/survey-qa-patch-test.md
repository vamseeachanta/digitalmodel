# Synthetic MBES patch-test analysis

## Title and revision block

| Field | Value |
|---|---|
| Document number | S0000-RPT-001-00 |
| Revision | 00 |
| Title | Synthetic MBES patch-test analysis |
| Project | Synthetic example |
| Client | Not applicable |
| Date | 2026-10-22 |

### Revision history

| Rev | Date | Description | Prepared | Checked | Approved |
|---|---|---|---|---|---|
| 00 | 2026-10-22 | Initial synthetic method and worked calculations |  |  |  |

## 1. Summary

A synthetic multibeam patch-test campaign recovers the injected roll 0.5 deg, pitch 1.0 deg, yaw 1.5 deg and latency 0.08 s. All estimates satisfy the chosen regression tolerances of 0.1 deg and 0.01 s. The corrected matched overlap is WITHIN_SCREEN against an explicitly supplied S-44 TVU ceiling. This is a synthetic consistency analysis; no survey-order certification is inferred.

## 2. Revision history

```text
Revision | Date       | What changed and why
00       | 2026-10-22 | Added synthetic geometry and hand calculations to demonstrate patch testing.
```

## 3. Scope and data basis

Eight synthetic lines share constant injected biases. Separate flat, sloping and point-target patches isolate each parameter. All values below are chosen inputs, except the identified S-44 table coefficients. No real survey data are used.

- Depth datum 50 m; along-track slope 0.2 (depth increases with X).
- Point target at X=0, Y=0, shoaling 5 m on the sloping patch.
- Beam offsets -30 to +30 m at 10 m spacing; yaw tracks Y=-30/+30 m.
- Speeds 2/4 m/s; pings at 1 s intervals for 40 s; reference starts X=-40/+40 m.
- Sensor lever arm (0,0,0) m; constant sound velocity 1500 m/s at depths 0/100 m.
- Exactly paired timestamps; supplied synchronization tolerance 0 s.
- Independent Gaussian depth noise, standard deviation 0.002 m; seed 7 plus line index.
- Chosen overlap XY matching tolerance 0.1 m; matched subset only.
- Special Order coefficients a=0.25 m and b=0.0075, read from S-44 6.1.0 Table 1.

## 4. Geometry and method

Ground X runs forward along the first line, Y to starboard, Z down. Reciprocal direction reverses both horizontal vessel axes. Positive roll moves starboard returns deeper, pitch moves nadir returns forward, yaw moves starboard returns aft. Positive latency advances recorded position. Sensor lever arms set each rotation origin.

```text
Plan view (Z down)       X forward --->
Y=-30: parallel line    ----------------->
Y=  0: feature                          *
Y=+30: parallel line    ----------------->
Reciprocal slope/flat   <-----------------
Cross section: starboard b>0; depth D>0 below sensor.
```

The footprint vector is q=(0,b,D). Forward injection is T_latency Ry Rx Rz q. Its inverse is applied as latency, pitch, roll, yaw. This rotation convention is declared for the example, not assumed for an acquisition system.

Roll: fit measured cross-track slopes m+ and m- by least squares. After pitch correction m+=tan(r), m-=-tan(r); r=atan((m+-m-)/2). Hand example: slopes +0.01/-0.01 give r=atan(0.01).

Pitch: after latency correction, nadir X_obs=X_true+direction*D_obs*tan(p). At equal observed depth, reciprocal separation dx=2*D_obs*tan(p); p=atan(dx/(2*D_obs)). Hand example: D=50 m and dx=2 m give p=atan(0.02). Using true instead of observed depth gives the usual small-angle approximation.

Latency: at equal depth on the same heading, dx=direction*(v_fast-v_slow)*tau. Hand example: a 0.16 m shift divided by 2 m/s gives 0.08 s.

Yaw: after earlier corrections, associated target returns have dx=-delta_b*sin(y), dy=delta_b*cos(y) in the vessel frame; y=atan(-dx/dy). Hand example: dx=-2 m, dy=sqrt(40^2-2^2) m gives y=asin(0.05). Track separation would require asin; the estimator uses observed relative offsets.

Least squares: minimize sum((y-a-b*x)^2); the normal equations yield b=sum((x-xbar)*(y-ybar))/sum((x-xbar)^2). These equations are derived from the declared matrices and line geometries, rather than copied instrument formulas.

## 5. Results

Standard-order coordinate RMS is 2.1 mm; reverse-order RMS is 31.3 mm on the first roll line. These truth-based diagnostics are used only for the synthetic example. Estimation uses measured returns, never truth coordinates. Tables use one decimal place.

The overlap compares absolute paired depth differences against sqrt(a^2+(b*d)^2), with d the mean pair depth. This individual-sounding ceiling is used as a conservative consistency screen, not an IHO overlap rule. When table inputs are omitted, the same overlap is NOT_EVALUATED.

### Bias recovery

| Parameter | Unit | Injected | Estimated | Difference | Test tolerance |
|---|---|---|---|---|---|
| roll_deg | deg | 0.5 | 0.5 | 0.0 | 0.1 |
| pitch_deg | deg | 1.0 | 1.0 | 0.0 | 0.1 |
| yaw_deg | deg | 1.5 | 1.5 | 0.0 | 0.1 |
| latency_s | ms | 80.0 | 78.1 | -1.9 | 10.0 |

Source: `estimates.csv`

### Corrected overlap consistency

| Screen | Matched | Unmatched first | Unmatched second | Max difference mm | Min TVU ceiling mm | Worst utilisation percent |
|---|---|---|---|---|---|---|
| WITHIN_SCREEN | 287 | 0 | 0 | 8.5 | 450.7 | 1.9 |

Source: `overlap.csv`

## 6. Sources and traceability

- NOAA Field Procedures Manual (2020), section 1.5.6.2, pp. 26-27: line selection and pitch, roll, heading sequence. https://nauticalcharts.noaa.gov/publications/docs/standards-and-requirements/fpm/field_procedures_manual_2020.pdf
- IHO S-44, edition 6.1.0 (October 2022), section 3.2.3 and Table 1, p. 18: TVU equation and Special Order coefficients. https://iho.int/uploads/user/pubs/standards/s-44/S-44_Edition_6.1.0.pdf

OpenStax, Precalculus 2e (2021), section 5.4 supplies the trigonometric ratios used in the geometry derivations: https://openstax.org/books/precalculus-2e/pages/5-4-right-triangle-trigonometry

OpenStax, Introductory Statistics 2e (2023), section 12.3 supplies the least-squares criterion; the normal-equation rearrangement is derived here: https://openstax.org/books/introductory-statistics-2e/pages/12-3-the-regression-equation

All four sources were opened from their publishers on 2026-10-22. The attempted 6.2.0 URL returned HTML, so it supplies no coefficients. The NOAA reference treats timing as a separate gross diagnostic; latency-first is the chosen synthetic sequence here. Public URLs are retained in the provenance manifest. Wiki-based citation records are not fabricated; the citation sidecar is empty.

## 7. Conclusions and limits

The synthetic recovery and consistency screen satisfy the stated test criteria. Use the module to exercise sign conventions and parameter coupling before considering measured-data work. Real calibration requires qualified sensor geometry, sound-velocity treatment, timing, target association and an uncertainty budget.

Only two parallel headings, constant velocity, known planar suitability, prescribed beam footprints and one explicitly associated point are supported. The SVP is validated metadata; no acoustic travel times are generated. No raw survey importer, refraction, motion history, datum reduction or full S-44 order assessment is included. Estimates have no confidence intervals. A user supplies the applicable order, edition, a, b and table reference; missing evidence remains NOT_EVALUATED. The nearest-point overlap matcher is intended for small synthetic datasets.
