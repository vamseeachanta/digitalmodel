# Fatigue Analysis Tutorial - For Young Engineers

## Welcome! 👋

This tutorial will help you understand fatigue analysis for offshore structures. We'll go from basic concepts to running your first analysis.

## Chapter 1: What is Fatigue?

### The Paper Clip Example
Try this: Take a paper clip and bend it back and forth. After several bends, it breaks! That's fatigue failure - repeated stress causes damage over time, even if each stress is small.

### In Offshore Structures
Ocean waves create cyclic loading on mooring structures:
- Wave goes up → Tension increases
- Wave goes down → Tension decreases  
- Repeat millions of times → Fatigue damage

## Chapter 2: Key Concepts

### 1. Stress Range
The difference between maximum and minimum stress in a cycle.
```
Stress Range = σ_max - σ_min

Example:
Max tension: 100 MPa
Min tension: 20 MPa
Stress Range = 100 - 20 = 80 MPa
```

### 2. Rainflow Counting
Imagine rain flowing down a pagoda roof - that's how we count stress cycles!
- Converts irregular stress history into simple cycles
- Each cycle has: stress range + number of occurrences
- Output: Histogram of stress ranges vs cycle counts

### 3. S-N Curve (Wöhler Curve)
Shows relationship between Stress (S) and Number of cycles to failure (N).

```
High Stress → Few cycles to failure (like bending paper clip hard)
Low Stress → Many cycles to failure (gentle bending)

Our equation: log₁₀(N) = 12.164 - 3.0 × log₁₀(S)
```

### 4. Fatigue Limit
Below certain stress (52.64 MPa for our steel), material lasts forever!
Like gently wiggling the paper clip - it won't break.

### 5. Miner's Rule
How we add up damage from different stress levels:
```
Total Damage = Σ(actual cycles / allowable cycles)

Example:
Stress A: 1,000 cycles actual / 10,000 allowable = 0.1 damage
Stress B: 2,000 cycles actual / 100,000 allowable = 0.02 damage
Total = 0.12 damage (12% of life consumed)

When Total Damage = 1.0 → Failure!
```

## Chapter 3: Real-World Factors

### Stress Concentration Factor (SCF)
Holes, welds, and geometry changes amplify stress:
```
Actual Stress = Nominal Stress × SCF

Think of it like this:
- Smooth bar: SCF = 1.0 (no amplification)
- Bar with hole: SCF = 3.0 (3x stress at hole edge!)
- Our critical location (loc02): SCF = 2.0
```

### Thickness Effect
Thicker plates are stronger against fatigue:
```
TCF = (thickness / 22mm)^0.25

Examples:
- 18mm plate: TCF = 0.95 (5% weaker)
- 50mm plate: TCF = 1.23 (23% stronger)
```

### Safety Factor
We design for 5x longer than needed:
```
Design Life = Required Life × Safety Factor
25 years required × 5 = 125 years design target
```

## Chapter 4: Running Your First Analysis

### Step 1: Understand the Files

**Input File Structure:**
```csv
stress range (Mpa),Cycles_Annual
10.0,5000000    # 5 million cycles at 10 MPa
20.0,2000000    # 2 million cycles at 20 MPa
30.0,500000     # 500k cycles at 30 MPa
```

**What each line means:**
- First column: Stress level
- Second column: How many times per year

### Step 2: Run the Analysis

```bash
# Navigate to the module
cd specs/modules/fatigue-analysis/stress_rainflow_to_damage

# Run test (quick check)
python run_damage_analysis.py --config input/damage_analysis_config_test.yml

# Check the output
ls output/test/damage_results/
```

### Step 3: Understand the Output

**Damage Rate File:**
```csv
stress_range_mpa,cycles_annual,stress_corrected_mpa,cycles_to_failure,damage_per_bin,damage_rate_per_year
10.0,5000000,11.5,1.45e10,3.44e-4,3.44e-4
```

**Reading this:**
- 10 MPa stress → 11.5 MPa with SCF
- Can handle 14.5 billion cycles before failure
- 5 million cycles causes 0.000344 damage
- At this rate: 1/0.000344 = 2,907 years to failure!

## Chapter 5: Practical Exercise

### Exercise 1: Calculate Damage Manually

Given:
- Stress range: 40 MPa
- Annual cycles: 1,000,000
- SCF: 1.15
- No thickness correction

Steps:
1. Apply SCF: 40 × 1.15 = 46 MPa
2. Calculate N: log(N) = 12.164 - 3.0 × log(46)
   - log(46) = 1.663
   - log(N) = 12.164 - 3.0 × 1.663 = 7.175
   - N = 10^7.175 = 14,962,357 cycles
3. Damage = 1,000,000 / 14,962,357 = 0.0668
4. Life = 1 / 0.0668 = 15 years

### Exercise 2: Interpret Results

Look at this summary line:
```
loc02,25.0,2.0,1.032,14132800,0.0,inf,inf
```

Questions:
1. What's the thickness? → 25mm
2. What's the SCF? → 2.0 (highest!)
3. Why infinite life? → Stress below 52.64 MPa limit
4. Should we worry? → No, but monitor since highest SCF

## Chapter 6: Common Pitfalls

### 1. Unit Confusion
- Always use MPa for stress
- Cycles are counts (no units)
- Check input file headers!

### 2. Missing SCF
- Forgetting SCF underestimates damage
- Our loc02 has SCF=2.0 - doubles the stress!

### 3. Wrong S-N Curve
- Different curves for different conditions
- We use ABS E (in-air)
- Seawater would be more conservative

### 4. Ignoring Fatigue Limit
- Stress below 52.64 MPa = infinite life
- But corrosion can eliminate fatigue limit!

## Chapter 7: Python Code Walkthrough

### Core Calculation Function
```python
def calculate_damage(stress, cycles, scf, thickness):
    # Step 1: Apply SCF
    stress_actual = stress * scf
    
    # Step 2: Thickness correction
    tcf = (thickness / 22) ** 0.25
    stress_for_sn = stress_actual / tcf
    
    # Step 3: Check fatigue limit
    if stress_for_sn < 52.64:
        return 0  # Infinite life!
    
    # Step 4: Calculate cycles to failure
    log_n = 12.164 - 3.0 * math.log10(stress_for_sn)
    n_failure = 10 ** log_n
    
    # Step 5: Calculate damage
    damage = cycles / n_failure
    
    return damage
```

## Chapter 8: Career Development Tips

### Skills You're Learning
1. **Fatigue Analysis** - Critical for offshore, aerospace, automotive
2. **Python Programming** - Data processing and automation
3. **Data Visualization** - Communicating results effectively
4. **Standards Compliance** - Working with ABS, DNV, API codes

### Next Steps
1. Read ABS Fatigue Guide Chapter 1-2
2. Learn about crack propagation (Paris Law)
3. Study frequency domain fatigue analysis
4. Explore reliability-based design

### Interview Questions You Can Now Answer
1. "What is an S-N curve?" ✓
2. "Explain Miner's rule" ✓
3. "What is SCF and why does it matter?" ✓
4. "How do you calculate fatigue life?" ✓

## Quick Reference Card

### Key Numbers to Remember
- Fatigue limit: 52.64 MPa
- S-N curve: log(N) = 12.164 - 3.0×log(S)
- Reference thickness: 22mm
- Design factor: 5.0
- Design life: 25 years

### Command Cheat Sheet
```bash
# Run analysis
python run_damage_analysis.py --config [config_file]

# Check results
cat output/[folder]/reports/damage_analysis_summary.csv

# View plot
open output/[folder]/visualizations/[filename].png
```

## Congratulations! 🎉

You've learned the fundamentals of fatigue analysis! You can now:
- Understand stress rainflow data
- Calculate fatigue damage
- Run automated analysis
- Interpret results

Remember: Every expert was once a beginner. Keep learning, stay curious, and don't hesitate to ask questions!

---
*Tutorial Version: 1.0*
*Created for: Entry-level Engineers*
*Prerequisites: Basic engineering mechanics*

## Additional Resources
- Book: "Metal Fatigue in Engineering" by Stephens et al.
- Online: eFatigue.com (free fatigue calculators)
- Course: Coursera - "Fatigue of Materials"
- Software: Practice with Python and Excel

Good luck with your engineering journey! 🚀