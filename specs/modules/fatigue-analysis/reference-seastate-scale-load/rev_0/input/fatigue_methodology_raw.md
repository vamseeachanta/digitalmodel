**Procedure for Strut Foundation Fatigue Evaluation with rainflow counting**

**🔹 1. Define Environmental Load Cases**

* **Wave Load Cases**: 18 cases with fixed Hs = 0.5 m (MetOcean Table 9).
* **Wind Load Cases**: 16 cases with fixed wind speed = 10 m/s (MetOcean Table 10).
* **Fatigue Conditions (MetOcean Table 11)**: 81 combined wind-wave conditions with varying:
  + Wind speed
  + Wave height (Hs)
  + Occurrence (%)

**🔹 2. Time Domain Analysis**

* Run 34 simulations (18 wave + 16 wind) separately.
* Each simulation is **3 hours** long.
* Extract **strut load time histories** for all **8 mooring struts**.

**🔹 3. Rainflow Counting**

* Apply rainflow counting to each strut’s time history.
* Extract:
  + **Load ranges** (ΔF)
  + **Cycle counts** (n)
* Result: 272 arrays (34 conditions × 8 struts).
* What is the time duration used for the rain flow counting? 200seconds?

**🔹 4. Scaling Strut Loads for Fatigue Conditions ✅**

**🔸 Objective:**

Scale rainflow load ranges from time domain analysis to match the wind speeds and wave heights in the 81 fatigue conditions.

**🔸 Assumptions:**

* **Wind loads scale with the square of wind speed**.
* **Wave loads scale linearly with significant wave height (Hs)**.

**🔸 Process:**

1. **Separate wind and wave rainflow results**.
2. For each fatigue condition:
   * Identify wind speed *Vwind*​ and wave height *HS.*
   * Scale wind-induced load ranges:

$$∆F\_{scaled, wind}=∆F\_{wind}×(\frac{V\_{wind}}{10m/s})^{2}$$

* + Scale wave-induced load ranges:

$$∆F\_{scaled, wave}=∆F\_{wave}×\frac{H\_{S}}{0.5m}$$

1. **Map both scaled load ranges into common bins** (e.g., 0–50, 50–100, ..., 950–1000 kN).
2. **Sum the cycle counts** from wind and wave for each bin:

$n\_{combined}\left(∆F\right)=n\_{wind}\left(∆F\right)+n\_{wave}(∆F)$ **per strut** for each fatigue condition.

**🔹 5. Weighting by Annual Occurrence**

* Use occurrence percentages from fatigue wave and wind table (MetOcean Table 11).
* Weight each rainflow result:

$$n\_{weighted}(∆F)=n\_{combined}(∆F)×\frac{Annual Occurance (\%)}{100}$$

**🔹 6. FEA Modeling**

* One FEA model used for both wind and wave loads.
* Apply a **unit strut load of 4000 kN**.
* Extract **principal stresses** from high-stress concentration areas.
* Result: Stress per strut per unit load:

$$σ\_{unit}=\frac{σ\_{FEA}}{4000kN}$$

Steps 7 through 9 are repeated for each critical stress location (currently about 5 locations) and all 8 struts.

Is there a way to merge all 8 struts to one load range vs cycles array?

**🔹 7. Stress-Range Mapping**

* Convert scaled load ranges to stress ranges:​

$∆σ\_{j}=SCF×∆F\_{scaled}\_{j}×σ\_{unit}$ *per strut*

**🔹 8. Fatigue Damage Calculation**

* Use ABS “E” in Air S-N curve:

$$A=1.04×10^{12}$$

$$m=3$$

$$C=1.48×10^{11}$$

$$r=5$$

* For each stress range (j=1, 2, …, J(total stress intervals for *i*th fatigue condition)):

$$N\_{j}=\left\{\begin{array}{c}A∙\left(∆σ\_{j}\right)^{-m}, N\leq 10^{6}\\C∙\left(∆σ\_{j}\right)^{-r}, N>10^{6}\end{array}\right.$$

$$D\_{i}=\sum\_{j=1}^{J}\frac{n\_{j}}{N\_{j}}$$

* Sum damage across all 81 fatigue conditions (i=1, 2, …, 81):

$D\_{total}=\sum\_{i=1}^{81}D\_{i}$ ​

* Annual damage is calculated based on the duration of the rainflow counting.

$$D\_{annual}=D\_{total}×\frac{31536000s/year}{200s}$$

**🔹 9. Fatigue Life Estimation**

* Use Miner’s Rule:

  $Fatigue Life= \frac{1}{D\_{annual}}$ ​​

* Evaluate **per strut** and compare against design life

$$Fatigue Life\geq FDF×Design Life$$