# Cathodic Protection Engineering

### Standards-Based CP Design — Pipelines, Vessel Hulls, and Offshore Structures

---

## Why Cathodic Protection

Steel infrastructure placed in seawater corrodes. Without intervention, free corrosion
proceeds until the structure fails. Cathodic protection (CP) shifts the steel potential
to the immune region on the Pourbaix diagram, eliminating the electrochemical driving
force for corrosion.

CP is the primary method used across the entire offshore industry:

- **Submarine pipelines** — export lines, flowlines, jumpers, and FLETs
- **Vessel hulls** — FPSOs, FSOs, drillships, AHTs, and service vessels
- **Fixed offshore platforms** — jacket structures, conductors, piles, and risers
- **Subsea equipment** — riser bases, foundations, manifolds, and pipeline end terminations

Getting the design right matters. Under-protection leads to accelerated corrosion and
inspection-driven intervention. Over-protection wastes capital on anodes that are never
consumed, adds unnecessary weight, and creates hydrogen embrittlement risk on
high-strength steels.

---

## What A&CE Delivers

A&CE provides cathodic protection engineering across the design lifecycle:

- **CP design calculations** — current demand, anode mass, anode count, spacing, and
  design life verification per recognised international standards
- **Anode material and geometry selection** — aluminium, zinc, and magnesium alloys;
  bracelet, flush-mounted, stand-off, and retrofittable configurations
- **CP design reports** — calculation packages formatted for client review, class
  submission, and regulatory approval
- **Anode schedules** — fabrication-ready anode lists with geometry, mass, quantity,
  and location
- **Inspection and monitoring plans** — potential survey criteria, close-interval
  potential survey (CIPS) specifications, and re-assessment intervals
- **CP review and audit** — independent review of third-party CP designs and
  assessment of existing CP systems for life extension

CP engineering is supported by a validated Python calculation engine with full
test coverage, traceable to standards, and capable of rapid parametric sensitivity
studies that would take days to run manually in spreadsheets.

---

## Standards Coverage

A&CE CP calculations are implemented against the following recognised standards.

| Standard | Edition | Scope |
|---|---|---|
| **DNV-RP-F103** | October 2010 | Cathodic protection of submarine pipelines by galvanic anodes |
| **ABS GN Ships** | December 2017 | Cathodic protection of ships and floating structures |
| **ABS GN Offshore** | December 2018 | Cathodic protection of offshore structures |
| **DNV-RP-B401** | 2011 (reference) | Cathodic protection design for offshore structures |

DNV-RP-F103 (2010) and ABS GN Ships (2017) are fully implemented and validated
against worked examples. ABS GN Offshore (2018) is implemented for submerged-zone
jacket structures. DNV-RP-B401 provides the industry reference for offshore platform
SACP design.

---

## Structure Types

### Submarine Pipelines

Pipeline CP design uses the DNV-RP-F103 methodology. The key variables are burial
condition (non-buried, buried, or intermittent), internal fluid temperature, coating
type and quality, and pipeline length. The calculation determines current demand at
initial, mean, and final coating breakdown conditions, sizes total anode mass from
the charge balance, and checks attenuation length to confirm adequate current
distribution along the line.

Coating types covered: 3LPE (three-layer polyethylene/FBE), FBE (fusion-bonded
epoxy), bare, and asphalt enamel. Anode geometries: Al-Zn-In bracelet anodes in
standard and tapered profiles.

### Ship and Vessel Hulls

Vessel hull CP design uses the ABS GN Ships guidance note method. The ABS method
applies a compound-growth coating breakdown model (distinct from the linear DNV
pipeline model) and accounts for design life, seawater temperature, hull wetted area,
and the proportion of bare vs. coated steel. Anode resistance is calculated for
long-flush and short-flush geometries, giving the individual anode current output
and a check on whether the design current demand is satisfied.

The temperature-adjusted aluminium anode electrochemical capacity is computed at the
design seawater temperature (typically 10–30 °C), giving accurate mass and count
results for both tropical and temperate deployments.

### Fixed Offshore Platforms

Jacket structure CP design uses the ABS GN Offshore 2018 method. The calculation
accounts for climatic region (tropical, sub-tropical, temperate, or arctic), water
depth band (0–30 m, 30–100 m, 100–300 m, and >300 m), environmental zone
(submerged or saline mud), and a depth-band-dependent coating breakdown model
that differs from the pipeline approach.

Current densities are selected from the standard's Table 1, coating breakdown
factors are computed at initial, mean, and final conditions, and anode mass and
count are determined from the mean current demand charge balance.

---

## What the Calculation Produces

Every CP design calculation returns a full set of engineering outputs:

| Output | Symbol | Unit |
|---|---|---|
| Current density (initial / mean / final) | i_ci, i_cm, i_cf | mA/m² or A/m² |
| Coating breakdown factor (initial / mean / final) | f_ci, f_cm, f_cf | — |
| Current demand (initial / mean / final) | I_ci, I_cm, I_cf | A |
| Total charge over design life | Q_total | Ah |
| Aluminium anode electrochemical capacity | acc | Ah/kg |
| Total anode mass required | M | kg |
| Anode count (for selected anode geometry) | N | — |
| Anode resistance (long-flush, short-flush, or stand-off) | R_a | Ω |
| Anode current output per unit | I_a | A |
| Longitudinal resistance per unit length (pipelines) | R_Me/m | Ω/m |
| Attenuation length (pipelines) | La | m |
| Design life verification | — | pass/fail |

Sensitivity studies — coating quality variants, temperature ranges, design life
options, and anode geometry comparisons — are run programmatically, enabling rapid
evaluation of multiple design options within a single analysis cycle.

---

## Worked Examples

The following examples demonstrate complete end-to-end CP calculations using the
A&CE calculation engine. All inputs, calculation steps, and output values are shown
with standard references.

### Example 1 — Submarine Pipeline, DNV-RP-F103:2010

**Structure:** 323.9 mm OD submarine pipeline, 10,000 m, 3LPE-coated, non-buried,
60 °C internal fluid temperature, aluminium bracelet anodes (300 kg each),
25-year design life.

| Output | Value | Unit |
|---|---|---|
| Pipeline outer surface area | 10,175.6 | m² |
| Current density i_cm (Table 5-1, >50–80 °C) | 0.060 | A/m² |
| Coating factor (mean) f_cm | 0.001375 | — |
| Mean current demand I_cm | 0.839 | A |
| Final current demand I_cf | 1.068 | A |
| Total charge Q_total | 183,848 | Ah |
| Total anode mass required | 114.9 | kg |
| Anode count (300 kg, 1.10 contingency) | 1 | — |
| Attenuation length La | 272 | m |

The 3LPE coating breakdown factors (a = 0.001, b = 0.00003) are an order of
magnitude lower than FBE, which drives the low current demand for a 10 km line.
The attenuation analysis confirms the designer must verify adequate potential at
both pipeline extremities — a single mid-pipe anode may not be sufficient for a
10 km line despite meeting the mass requirement.

Full calculation: `docs/domains/cathodic_protection/examples/example-01-pipeline-dnv-f103-2010.md`

---

### Example 2 — Ship Hull, ABS GN Ships (Dec 2017)

**Structure:** Floating storage terminal hull, 10,778 m² wetted area, 100% coated,
high-durability coating (α = β = 1.0%/yr), 5-year design life, seawater at 14 °C,
aluminium long-flush anodes (29 kg each).

| Output | Value | Unit |
|---|---|---|
| Coating factor (mean) fcm | 1.031 | — |
| Coating factor (final) fcf | 1.051 | — |
| Aluminium anode capacity at 14 °C | 2,162 | Ah/kg |
| Mean current demand I_cm | 149.9 | A |
| Total anode mass required | 3,679 | kg |
| Anode count (29 kg each, 5-year) | 127 | — |
| Anode resistance (long-flush, 1.00 m) | 0.289 | Ω |
| Current output per anode | ~1.00 | A |

**Design life sensitivity** — the same hull protected for 25 years requires 707
anodes (5.6× more), driven almost entirely by the longer duration in the anode
mass formula rather than by a large increase in mean current demand (149.9 A
rising to 166.8 A). This non-linear sensitivity to design life is a key input to
dry-docking interval decisions.

Full calculation: `docs/domains/cathodic_protection/examples/example-02-ship-hull-abs-gn-ships-2017.md`

---

### Example 3 — Fixed Offshore Jacket, ABS GN Offshore 2018

**Structure:** Jacket structure, 2,000 m² submerged surface, 50 m water depth,
tropical region, aluminium flush anodes (100 kg each), 25-year design life,
seawater at 25 °C.

| Output | Value | Unit |
|---|---|---|
| Initial current density i_ci (Table 1) | 120.0 | mA/m² |
| Mean current density i_cm | 60.0 | mA/m² |
| Final current density i_cf | 80.0 | mA/m² |
| Coating factor (mean) f_cm | 0.11 | — |
| Coating factor (final) f_cf | 0.21 | — |
| Mean current demand I_cm | 13.200 | A |
| Final current demand I_cf | 33.600 | A |
| Aluminium anode capacity at 25 °C | 1,865 | Ah/kg |
| Total anode mass required | ~1,937 | kg |
| Anode count (100 kg each) | 20 | — |

The final current demand (33.6 A) is 2.5× the mean (13.2 A). This large ratio
is driven by the coating factor rising from 0.11 to 0.21 and the current density
rising from 60 to 80 mA/m² at end-of-life — a combined effect that characterises
jacket structures more severely than pipelines or vessels.

Full calculation: `docs/domains/cathodic_protection/examples/example-03-platform-abs-offshore-2018.md`

---

## Integration with Fitness for Service

CP (cathodic protection) and FFS (fitness for service) are the two sides of
corrosion management:

| | Cathodic Protection | Fitness for Service |
|---|---|---|
| **Focus** | Prevention — stop corrosion occurring | Assessment — evaluate damage already present |
| **When applied** | Design stage and inspection-driven reassessment | When corrosion, erosion, or defects are found |
| **Question answered** | "How do we protect this structure?" | "Is this corroded structure still fit for service?" |
| **Standards** | DNV-RP-F103, ABS GN Ships, ABS GN Offshore | API 579-1/ASME FFS-1, BS 7910, DNV-RP-C203 |
| **Output** | Anode schedule, CP design report | Remaining life, fitness-for-service verdict |

Together, CP and FFS provide the complete corrosion management lifecycle — from
the original design decision to protect a structure through to the in-service
assessment of damage that occurs despite protection.

Where CP systems are degraded or absent, FFS assessment quantifies the impact of
the resulting corrosion on structural capacity and remaining life. The combination
of both capabilities in a single engineering team enables efficient integrity
management without handoffs between disciplines.

See also: FFS assessment capability — `docs/marketing/` (Fitness for Service brochure)

---

## Typical Deliverables

| Deliverable | Description |
|---|---|
| **CP Design Calculation** | Standards-traceable calculation package: current demand, anode mass, count, spacing, attenuation, and design life check |
| **Anode Schedule** | Fabrication-ready list with anode type, geometry, net mass, gross mass, quantity, and installation locations |
| **CP Design Basis** | Document setting out the environmental parameters, standards, coating assumptions, and design criteria |
| **Inspection and Monitoring Plan** | Defines survey intervals, reference electrodes, acceptance potentials, and re-assessment triggers |
| **CP Audit Report** | Independent review of an existing CP design or operating system, with gap assessment and recommendations |
| **Life Extension Study** | Assessment of CP system adequacy beyond the original design life, including remaining anode mass survey interpretation |

---

## Technical Specifications

| Specification | Detail |
|---|---|
| **Calculation engine** | Python, `CathodicProtection` class with calculation router |
| **Standards implemented** | DNV-RP-F103:2010, ABS GN Ships Dec 2017, ABS GN Offshore Dec 2018 |
| **Structure types** | Submarine pipelines, vessel hulls, fixed offshore platforms (submerged zone) |
| **Anode materials** | Aluminium (Al-Zn-In), zinc, temperature-corrected electrochemical capacity |
| **Anode geometries** | Bracelet (pipelines), long-flush, short-flush, stand-off (vessels and platforms) |
| **Sensitivity analysis** | Design life, coating quality, seawater temperature, water depth — all parametric |
| **Test coverage** | Full unit test suite with variants across coating quality, temperature, and design life |
| **Calculation traceability** | Every formula and table reference cited in code with standard clause and edition |

---

## Contact

| | |
|---|---|
| **Email** | [contact@aceengineer.com] |
| **Web** | [www.aceengineer.com] |
| **Technical queries** | Available to discuss project scope and standard applicability |

---

<sub>A&CE Cathodic Protection Engineering — standards-based CP design for pipelines,
vessels, and offshore structures.</sub>
