# Ship Cathodic Protection Requirements per ABS Guidelines

## Executive Summary

This document provides a comprehensive checklist of critical design data required for ship cathodic protection (CP) system design according to ABS (American Bureau of Shipping) Guidance Notes on Cathodic Protection of Ships (December 2017).

## 1. Vessel Particulars (Mandatory)

### 1.1 Basic Vessel Data
- **Vessel Type**: Cargo, tanker, container, passenger, etc.
- **Overall Length (LOA)**: $L_{OA}$ in meters
- **Breadth (Beam)**: $B$ in meters
- **Draft Conditions**:
  - Design draft: $T_d$ 
  - Loaded draft: $T_l$
  - Ballast draft: $T_b$
- **Light Waterline Position**: LWL
- **Hull Material**: Carbon manganese steel, low-alloy steel
- **Construction Drawings**: Including frame locations

### 1.2 Surface Area Calculations

The total protected area is calculated as:

$$A_{total} = A_{hull} + A_{appendages} + A_{internal}$$

Where:
- $A_{hull}$ = External underwater hull surface area (m²)
- $A_{appendages}$ = Sum of all appendage areas (m²)
- $A_{internal}$ = Internal tank surfaces requiring protection (m²)

#### Appendage Areas Include:
- Rudder surface area: $A_r$
- Propeller surface area: $A_p$ 
- Shaft surface area: $A_s$
- Stabilizer surface area: $A_{st}$
- Sea chest areas: $A_{sc}$
- Thruster tunnel areas: $A_t$
- Other underwater openings: $A_o$

## 2. Environmental Conditions (Mandatory)

### 2.1 Water Characteristics
- **Seawater Resistivity**: $\rho_{sw}$ (Ω·m)
  - Typical ocean: 0.20-0.30 Ω·m
  - Brackish water: 0.50-2.00 Ω·m
  - Fresh water: &gt;10 Ω·m
- **Water Temperature Range**: $T_{min}$ to $T_{max}$ (°C)
- **Salinity**: S (‰) - typically 35‰ for ocean water
- **pH Range**: 5-10 for zinc anodes
- **Dissolved Oxygen**: DO (mg/L)

### 2.2 Service Conditions

Current density requirements based on vessel speed $V$:

| Speed Range | Condition | Current Density $J_b$ (mA/m²) |
|------------|-----------|-------------------------------|
| $V \leq 1$ m/s | Static/moored | 100-250 |
| $1 &lt; V &lt; 10$ m/s | Normal operation | 220-350 |
| $V \geq 10$ m/s | High speed | 350-500 |
| Ice conditions | Arctic service | 500-750 |

## 3. Coating Specifications (Mandatory)

### 3.1 Coating System Parameters

The coating breakdown factor over time is expressed as:

$$f_c(t) = f_{c0} + k_d \cdot t$$

Where:
- $f_{c0}$ = Initial coating breakdown factor (typically 0.01-0.02)
- $k_d$ = Annual deterioration rate (%/year)
  - Low durability: 0.03/year
  - Medium durability: 0.015/year
  - High durability: 0.005-0.01/year
- $t$ = Time in years

### 3.2 Mean Coating Breakdown Factor

For design purposes, the mean coating breakdown factor:

$$f_{c,mean} = f_{c0} + \frac{k_d \cdot T_{design}}{2}$$

## 4. Design Current Density Requirements

### 4.1 Current Demand Calculation

The total current demand is calculated as:

$$I_{total} = \sum_{i} (A_i \times f_{c,i} \times J_{b,i})$$

Where:
- $A_i$ = Surface area of component $i$ (m²)
- $f_{c,i}$ = Coating breakdown factor for component $i$
- $J_{b,i}$ = Bare steel current density for component $i$ (mA/m²)

### 4.2 Design Current Requirements

- **Maximum Current Demand**: 
  $$I_{max} = I_{total} \times SF$$
  where $SF$ = Safety factor (typically 1.25)

- **Mean Current Demand**:
  $$I_{mean} = \frac{I_{initial} + I_{final}}{2}$$

## 5. Cathodic Protection Potential Criteria

### 5.1 Protection Potentials (vs. Ag/AgCl/Seawater Reference)

| Material | Minimum Potential $E_{min}$ (V) | Maximum Potential $E_{max}$ (V) |
|----------|----------------------------------|----------------------------------|
| Carbon/Low-alloy steel (aerobic) | -0.80 | -1.10 |
| Carbon/Low-alloy steel (anaerobic) | -0.90 | -1.10 |
| High-strength steel (&gt;690 MPa) | -0.80 | -0.95 |
| Stainless steel | Varies | Varies |
| Copper alloys | -0.45 | -0.60 |

## 6. Anode System Design

### 6.1 Galvanic Anode Weight Calculation

The minimum total anode weight required:

$$W_{total} = \frac{I_{mean} \times T_{design} \times 8760}{Q \times u}$$

Where:
- $W_{total}$ = Total anode weight (kg)
- $I_{mean}$ = Mean current demand (A)
- $T_{design}$ = Design life (years)
- $Q$ = Anode current capacity (A·h/kg)
  - Aluminum alloys: ~2700 A·h/kg
  - Zinc alloys: ~780 A·h/kg
- $u$ = Utilization factor (0.7-0.95)
- 8760 = Hours per year

### 6.2 Anode Resistance Calculations

#### For Slender Stand-off Anodes:

$$R_a = \frac{\rho}{2\pi L}\left[\ln\left(\frac{4L}{r}\right) - 1\right]$$

Where:
- $R_a$ = Anode resistance (Ω)
- $\rho$ = Seawater resistivity (Ω·m)
- $L$ = Anode length (m)
- $r$ = Equivalent anode radius (m)

#### For Flush-Mounted Anodes:

$$R_a = \frac{\rho}{2S}$$

Where:
- $S$ = Exposed anode surface area (m²)

### 6.3 Number of Anodes Required

The number of anodes $N$ is determined by:

$$N = \max\left(\frac{W_{total}}{W_{single}}, \frac{I_{max}}{I_{anode}}\right)$$

Where:
- $W_{single}$ = Weight of single anode (kg)
- $I_{anode}$ = Current output per anode (A)

The current output per anode:

$$I_{anode} = \frac{E_{driving}}{R_a + R_{structure}}$$

Where:
- $E_{driving}$ = Driving voltage (V) = $|E_{anode} - E_{protection}|$
- $R_{structure}$ = Structure resistance (Ω)

## 7. Impressed Current System Design

### 7.1 Transformer Rectifier Sizing

For ICCP systems, the power supply rating:

$$P_{TR} = V_{max} \times I_{max} \times SF$$

Where:
- $P_{TR}$ = Transformer rectifier power (W)
- $V_{max}$ = Maximum operating voltage (V)
  - Platinized titanium: 8V
  - Platinized niobium: 50V
  - Platinized tantalum: 100V
- $SF$ = Safety factor (typically 1.25)

### 7.2 Current Distribution

The throwing power or current spread from an anode:

$$L_{spread} = K \times \sqrt{\frac{\rho \times t}{J_p}}$$

Where:
- $L_{spread}$ = Effective current spread (m)
- $K$ = Empirical constant (~2.5)
- $t$ = Coating thickness (mm)
- $J_p$ = Protection current density (A/m²)

## 8. Special Considerations

### 8.1 Ice-Class Vessels

For ice-going vessels, modified parameters:
- Current density: $J_b = 500-750$ mA/m²
- Anode recession: Maximum 25mm below hull
- Impact protection factor: 1.5× standard thickness

### 8.2 High-Speed Vessels (&gt;25 knots)

Enhanced requirements:
- Coating breakdown acceleration factor: $k_{hs} = 1.5$
- Current density multiplier: $\alpha_v = 1.2-1.5$
- Modified current demand:
  $$I_{high-speed} = I_{standard} \times \alpha_v$$

### 8.3 Propeller Protection

Propeller current density requirements:

$$J_{prop} = J_{base} \times \left(1 + 0.1 \times \frac{RPM}{100}\right)$$

Where RPM is the propeller rotation speed.

## 9. System Verification

### 9.1 Protection Level Verification

The protection current density provided:

$$J_{provided} = \frac{I_{total}}{A_{total} \times f_c}$$

Must satisfy: $J_{provided} \geq J_{required}$

### 9.2 Anode Life Verification

Actual anode life:

$$T_{actual} = \frac{W_{total} \times Q \times u}{I_{mean} \times 8760}$$

Must satisfy: $T_{actual} \geq T_{design}$

## 10. Documentation Requirements

### 10.1 Design Documentation Checklist
- [ ] CP system general arrangement drawings
- [ ] Detailed calculation report with all formulas
- [ ] Material specifications and certificates
- [ ] Installation procedures and welding specifications
- [ ] Commissioning test procedures
- [ ] Operation and maintenance manual
- [ ] Potential monitoring locations

### 10.2 ABS Submission Requirements
- [ ] Design basis memorandum
- [ ] Current demand calculations
- [ ] Anode distribution plan
- [ ] Protection potential criteria justification
- [ ] Environmental condition assessment
- [ ] Coating specification and breakdown analysis

## 11. Quality Assurance

### 11.1 Design Review Criteria

Minimum acceptance criteria:
- Current capacity: $\frac{I_{available}}{I_{required}} \geq 1.0$
- Anode life: $\frac{T_{actual}}{T_{design}} \geq 1.0$
- Distribution: Maximum 40m between anodes
- Potential: $E_{min} \leq E_{structure} \leq E_{max}$

### 11.2 Commissioning Tests

Required measurements:
- Structure-to-electrolyte potential at multiple points
- Current output from each anode/zone
- Coating resistance verification
- Electrical continuity tests

## References

1. ABS Guidance Notes on Cathodic Protection of Ships, December 2017
2. NACE SP0169 - Control of External Corrosion on Underground or Submerged Metallic Piping Systems
3. DNV-RP-B401 - Cathodic Protection Design
4. ISO 15589-2 - Petroleum, petrochemical and natural gas industries — Cathodic protection of pipeline transportation systems

## Appendix A: Common Anode Properties

| Anode Type | Alloy | Potential (V vs Ag/AgCl) | Capacity (A·h/kg) | Consumption Rate (kg/A·yr) |
|------------|-------|---------------------------|-------------------|---------------------------|
| Aluminum | Al-Zn-In | -1.05 to -1.10 | 2500-2700 | 3.2-3.5 |
| Zinc | Zn (MIL-A-18001) | -1.00 to -1.05 | 780 | 11.2 |
| Magnesium | Mg-Al-Zn | -1.50 to -1.55 | 1230 | 7.1 |

## Appendix B: Typical Current Densities

| Environment | Structure Type | Current Density (mA/m²) |
|-------------|---------------|------------------------|
| Seawater | Bare steel (static) | 100-150 |
| Seawater | Bare steel (flowing) | 150-500 |
| Seawater | Well-coated steel | 5-20 |
| Seawater | Damaged coating | 50-100 |
| Mud | Bare steel | 20-50 |
| Mud | Coated steel | 2-10 |

---

*Document prepared in accordance with ABS Guidance Notes on Cathodic Protection of Ships (December 2017) and industry best practices.*