# Procedure for Strut Foundation Fatigue Evaluation with Rainflow Counting

## 1. Define Environmental Load Cases

- **Wave Load Cases**: 18 cases with fixed Hs = 0.5 m (MetOcean Table 9)
- **Wind Load Cases**: 16 cases with fixed wind speed = 10 m/s (MetOcean Table 10)
- **Fatigue Conditions** (MetOcean Table 11): 81 combined wind-wave conditions with varying:
  - Wind speed
  - Wave height (Hs)
  - Occurrence (%)

## 2. Time Domain Analysis

- Run 34 simulations (18 wave + 16 wind) separately
- Each simulation is **3 hours** long
- Extract **strut load time histories** for all **8 mooring struts**

## 3. Rainflow Counting

- Apply rainflow counting to each strut's time history
- Extract:
  - **Load ranges**: Delta F
  - **Cycle counts**: n
- Result: 272 arrays (34 conditions x 8 struts)
- Time duration for rainflow counting: 200 seconds (to be confirmed)

## 4. Scaling Strut Loads for Fatigue Conditions

### Objective

Scale rainflow load ranges from time domain analysis to match the wind speeds and wave heights in the 81 fatigue conditions.

### Assumptions

- Wind loads scale with the square of wind speed
- Wave loads scale linearly with significant wave height (Hs)

### Process

**Step 1:** Separate wind and wave rainflow results

**Step 2:** For each fatigue condition:
- Identify wind speed V_wind and wave height H_s
- Scale wind-induced load ranges:

  Delta F_scaled,wind = Delta F_wind x (V_wind / 10 m/s)^2

- Scale wave-induced load ranges:

  Delta F_scaled,wave = Delta F_wave x (H_s / 0.5 m)

**Step 3:** Map both scaled load ranges into common bins
- Example: 0-50, 50-100, ..., 950-1000 kN

**Step 4:** Sum the cycle counts from wind and wave for each bin:

  n_combined(Delta F) = n_wind(Delta F) + n_wave(Delta F)

This is calculated per strut for each fatigue condition.

## 5. Weighting by Annual Occurrence

- Use occurrence percentages from fatigue wave and wind table (MetOcean Table 11)
- Weight each rainflow result:

  n_weighted(Delta F) = n_combined(Delta F) x (Annual Occurrence % / 100)

## 6. FEA Modeling

- One FEA model used for both wind and wave loads
- Apply a **unit strut load of 4000 kN**
- Extract **principal stresses** from high-stress concentration areas
- Result: Stress per strut per unit load:

  sigma_unit = sigma_FEA / 4000 kN

**Note:** Steps 7 through 9 are repeated for each critical stress location (currently about 5 locations) and all 8 struts.

**Question:** Is there a way to merge all 8 struts to one load range vs cycles array?

## 7. Stress-Range Mapping

Convert scaled load ranges to stress ranges (per strut):

  Delta sigma_j = SCF x Delta F_scaled,j x sigma_unit

where:
- SCF = Stress Concentration Factor
- j = stress range index

## 8. Fatigue Damage Calculation

### S-N Curve Parameters (ABS "E" in Air)

| Parameter | Value |
|-----------|-------|
| A | 1.04 x 10^12 |
| m | 3 |
| C | 1.48 x 10^11 |
| r | 5 |

### Cycles to Failure

For each stress range j (where j = 1, 2, ..., J for the i-th fatigue condition):

If N <= 10^6:
  N_j = A x (Delta sigma_j)^(-m)

If N > 10^6:
  N_j = C x (Delta sigma_j)^(-r)

### Damage Accumulation

For each fatigue condition i:

  D_i = Sum(j=1 to J) [n_j / N_j]

Total damage across all 81 fatigue conditions:

  D_total = Sum(i=1 to 81) D_i

### Annual Damage

Calculate annual damage based on rainflow counting duration:

  D_annual = D_total x (31,536,000 seconds/year / 200 seconds)

## 9. Fatigue Life Estimation

### Apply Miner's Rule

  Fatigue Life = 1 / D_annual (years)

### Design Verification

Evaluate per strut and compare against design life:

  Fatigue Life >= FDF x Design Life

where FDF is the Fatigue Design Factor.