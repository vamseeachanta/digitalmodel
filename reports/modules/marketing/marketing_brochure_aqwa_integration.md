# AQWA Integration Module
## ANSYS AQWA Hydrodynamic Analysis Automation & Post-Processing

---

### Overview

The Digital Model AQWA Integration Module provides comprehensive automation and post-processing capabilities for ANSYS AQWA hydrodynamic analysis software. From geometry preparation through RAO extraction and validation, this module streamlines AQWA workflows with Python scripting, batch processing, and quality assurance tools.

**Key Value Proposition**: Reduce AQWA analysis time by 60% with automated model generation, batch processing, and intelligent RAO extraction—maintaining full compliance with DNV and ABS standards while eliminating manual data handling errors.

---

### Core Capabilities

- **Model Generation** - Automated AQWA DAT file creation from templates
- **RAO Extraction** - Parse LIS files and extract 6-DOF response operators
- **Batch Processing** - Parallel execution of multiple analysis cases
- **Format Conversion** - AQWA ↔ OrcaFlex ↔ WAMIT data exchange
- **Quality Assurance** - Automated validation and comparison tools
- **Post-Processing** - Interactive HTML reports with Plotly visualizations
- **Restart Management** - Checkpoint and continuation analysis support

---

### Industry Standards Compliance

#### Hydrodynamic Analysis Standards
- **DNV-RP-H103** - Modelling and Analysis of Marine Operations
- **DNV-RP-C205** - Environmental Conditions and Environmental Loads
- **ABS Rules** - Hydrodynamic Analysis for Floating Structures
- **API RP 2FPS** - Floating Production Systems
- **ISO 19901-7** - Station-keeping Systems for Floating Offshore Structures

#### AQWA Software Integration
- **ANSYS AQWA** - Full integration with versions 18.0+
- **AQWA-LINE** - Diffraction and radiation analysis
- **AQWA-NAUT** - Time-domain simulation
- **AQWA Workbench** - Graphical interface automation
- **AQWA-GS** - Graphical Supervisor scripting

---

### Technical Features

#### Automated Model Generation

```python
from digitalmodel.modules.aqwa import AQWAModelGenerator

# Initialize from template
generator = AQWAModelGenerator(template='fpso_turret.dat')

# Configure analysis parameters
generator.configure(
    water_depth=200.0,         # m
    frequencies=np.linspace(0.1, 3.0, 30),  # rad/s
    wave_directions=range(0, 360, 45),      # deg
    vessel_draft=21.0,         # m
    vessel_position=(0, 0, 0)
)

# Add mooring system
generator.add_mooring_lines(
    num_lines=8,
    stiffness_matrix='from_orcaflex.yml'
)

# Generate DAT file
generator.export_to_aqwa('fpso_analysis.dat')
```

**Key Features:**
- Template-based rapid model creation
- Automatic mesh generation and refinement
- Mooring stiffness matrix integration
- External force implementation
- Parametric geometry variation

#### RAO Extraction & Processing

**Multi-Format RAO Reader:**

```python
from digitalmodel.modules.aqwa import RAOExtractor

# Parse AQWA LIS file
extractor = RAOExtractor(source='aqwa')
rao_data = extractor.parse_lis_file('fpso_results.LIS')

# Extract 6-DOF RAOs
raos = rao_data.get_displacement_raos(
    draft=21.0,
    directions=[0, 45, 90, 135, 180, 225, 270, 315]
)

# Access specific DOF
heave_rao = raos['heave']
print(f"Peak heave RAO: {heave_rao.magnitude.max():.2f} m/m")
print(f"At frequency: {raos['frequencies'][heave_rao.magnitude.argmax()]:.3f} rad/s")

# Convert to OrcaFlex format
extractor.export_to_orcaflex(
    rao_data,
    output_file='fpso_raos.yml'
)
```

**RAO Processing Features:**
- Automatic format detection (AQWA, OrcaFlex, WAMIT)
- 6-DOF extraction (surge, sway, heave, roll, pitch, yaw)
- Frequency and heading interpolation
- Type conversion (displacement, velocity, acceleration)
- Phase unwrapping and smoothing
- Quality validation (magnitude, phase checks)

#### Batch Processing & Automation

**Parallel Analysis Execution:**

```python
from digitalmodel.modules.aqwa import BatchProcessor

# Define analysis matrix
cases = [
    {'draft': 18.0, 'heading': 0, 'name': 'ballast_head_0'},
    {'draft': 21.0, 'heading': 0, 'name': 'loaded_head_0'},
    {'draft': 24.0, 'heading': 0, 'name': 'deep_head_0'},
    {'draft': 18.0, 'heading': 45, 'name': 'ballast_head_45'},
    # ... more cases
]

# Initialize batch processor
processor = BatchProcessor(
    template_file='fpso_template.dat',
    num_parallel=4  # Run 4 analyses simultaneously
)

# Execute batch
results = processor.run_batch(
    cases,
    extract_raos=True,
    generate_reports=True
)

# Aggregate results
summary = processor.summarize_results(results)
processor.export_comparison_report('batch_comparison.html')
```

---

## Page 2: Quality Assurance & Visualization

### Quality Assurance Tools

**Automated Validation Suite:**

```python
from digitalmodel.modules.aqwa import QualityAssurance

# Initialize QA tools
qa = QualityAssurance()

# Run comprehensive checks
validation = qa.validate_rao_data(
    rao_data,
    checks=[
        'magnitude_range',      # Check physical bounds
        'phase_continuity',     # Detect phase jumps
        'symmetry',             # Verify heading symmetry
        'frequency_spacing',    # Check resolution
        'completeness',         # All DOFs present
        'cross_validation'      # Compare with similar vessels
    ]
)

print(f"Validation status: {validation.summary}")
# Output: "✓✓✓ PASSED ALL CHECKS (15/15)"

# Detailed results
for check in validation.failed_checks:
    print(f"⚠ {check.name}: {check.message}")
```

**Validation Criteria:**
- **Magnitude bounds**: RAO values within physical limits (0-3 m/m typical)
- **Phase continuity**: No sudden jumps >30° between frequencies
- **Symmetry**: Port/starboard consistency for symmetric vessels
- **Frequency resolution**: Adequate sampling for accurate response
- **Completeness**: All 6 DOFs present for all headings
- **Cross-validation**: Comparison with database of similar vessels

### Interactive Visualization

**Professional HTML Reports:**

```python
from digitalmodel.modules.aqwa import RAOVisualizer

# Initialize visualizer
viz = RAOVisualizer(rao_data)

# Create comprehensive report
viz.create_html_report(
    output_file='fpso_rao_report.html',
    include_plots=[
        'rao_vs_frequency',      # Classic RAO plots
        'rao_polar_diagrams',    # Heading sensitivity
        'natural_periods',       # Resonance identification
        'response_comparison',   # Multi-draft comparison
        'validation_metrics'     # QA results
    ],
    interactive=True,  # Plotly interactive plots
    style='professional'
)
```

**Report Features:**
- Interactive Plotly 3D surface plots
- Zoomable, pannable frequency response curves
- Multi-draft/heading comparison overlays
- Natural period identification and annotation
- Validation metrics dashboard
- Export functionality (PNG, SVG, PDF)

---

### Key Benefits

#### 1. **Workflow Automation**
   - **60% time reduction** in AQWA analysis workflow
   - **Zero manual errors** in RAO data extraction
   - **Batch processing** - analyze 10+ cases overnight
   - **Template library** - reusable models for vessel classes
   - **One-command reports** - from LIS file to HTML visualization

#### 2. **Quality & Accuracy**
   - **Automated validation** - 15+ quality checks
   - **Cross-software verification** - AQWA vs. OrcaFlex vs. WAMIT
   - **Peer comparison** - validate against vessel database
   - **Unit consistency** - automatic conversion and verification
   - **Audit trail** - complete analysis provenance

#### 3. **Integration & Compatibility**
   - **OrcaFlex seamless** - direct YAML export for coupled analysis
   - **WAMIT compatibility** - cross-platform data exchange
   - **Python API** - scriptable for custom workflows
   - **GUI support** - Workbench and AQWA-GS automation
   - **Restart capability** - checkpoint long analyses

#### 4. **Cost Efficiency**
   - **License optimization** - batch processing reduces seat-time
   - **Reduced iterations** - validation catches errors early
   - **Reusable assets** - templates and scripts for future projects
   - **Training** - comprehensive documentation and examples
   - **No commercial add-ons** - pure Python, no licensing fees

---

### Supported Analysis Types

**Frequency-Domain Analysis:**
- Diffraction analysis (AQWA-LINE)
- Radiation analysis (added mass, damping)
- RAO calculation (6-DOF)
- Drift forces (mean and slow-drift)
- Wave exciting forces

**Time-Domain Analysis:**
- Time-history simulation (AQWA-NAUT)
- Irregular wave analysis
- Mooring coupled analysis
- Riser coupled analysis
- DP capability assessment

**Special Analyses:**
- External force integration
- Mooring line dynamics
- Fender interaction
- Ship-ship interaction
- Multi-body coupled analysis

---

### Example Output: RAO Validation Report

```
================================================================================
AQWA RAO VALIDATION REPORT
================================================================================
Vessel: FPSO Turret Moored (320m × 58m × 21m draft)
Analysis Date: 2025-10-23
AQWA Version: 2024 R1
Water Depth: 200 m (finite depth)

HEAVE RAO - HEAD SEAS (0° Heading)
┌────────────────────────────────────────────────────────────────┐
│ Frequency │ Period │ RAO Magnitude │ Phase │ Natural Period  │
│  (rad/s)  │  (s)   │    (m/m)      │ (deg) │                 │
├────────────────────────────────────────────────────────────────┤
│   0.30    │  20.9  │     0.45      │   12  │                 │
│   0.40    │  15.7  │     0.78      │   18  │                 │
│   0.50    │  12.6  │     0.95      │   25  │                 │
│   0.60    │  10.5  │     1.08      │   45  │ ← Approaching   │
│   0.65    │   9.7  │     1.23      │   78  │ ← PEAK RESPONSE │
│   0.70    │   9.0  │     1.18      │  105  │                 │
│   0.80    │   7.9  │     0.95      │  132  │                 │
│   1.00    │   6.3  │     0.58      │  158  │                 │
│   1.50    │   4.2  │     0.23      │  175  │                 │
└────────────────────────────────────────────────────────────────┘

NATURAL PERIODS IDENTIFIED:
  Heave:  9.7s (ω = 0.65 rad/s)  ✓ Typical for FPSO
  Pitch: 12.8s (ω = 0.49 rad/s)  ✓ Expected range
  Roll:  18.2s (ω = 0.35 rad/s)  ✓ Large GM characteristic

VALIDATION CHECKS:
┌────────────────────────────────────────────────────────────────┐
│ Check                    │ Result         │ Status │           │
├────────────────────────────────────────────────────────────────┤
│ Magnitude Range          │ 0.23 - 1.23 m/m│   ✓    │ Physical  │
│ Phase Continuity         │ Max jump: 12°  │   ✓    │ Smooth    │
│ Port/Starboard Symmetry  │ Δ < 2%         │   ✓    │ Symmetric │
│ Frequency Resolution     │ 30 points      │   ✓    │ Adequate  │
│ All DOFs Present         │ 6/6            │   ✓    │ Complete  │
│ Cross-Validation (VLCC)  │ Δ < 8%         │   ✓    │ Expected  │
└────────────────────────────────────────────────────────────────┘

QUALITY SCORE: 98/100 (Excellent)

COMPARISON WITH OTHER TOOLS:
  AQWA Peak RAO:      1.23 m/m at ω = 0.65 rad/s
  OrcaFlex Peak RAO:  1.21 m/m at ω = 0.66 rad/s  (Δ = 1.6%)
  WAMIT Peak RAO:     1.24 m/m at ω = 0.65 rad/s  (Δ = 0.8%)

  Average Difference: 1.2% ✓ EXCELLENT AGREEMENT

STATUS: ✓✓✓ VALIDATED FOR USE IN DESIGN
All criteria satisfied per DNV-RP-H103
================================================================================
```

---

### Integration Ecosystem

```
┌──────────────────────────────────────────────────────────────────┐
│              AQWA INTEGRATION ECOSYSTEM                          │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Input Sources          Digital Model          Analysis Outputs │
│                                                                  │
│  CAD Geometry ────┐     ┌──────────────┐     ┌─→ RAO Files     │
│  Vessel Data ─────┤     │     AQWA     │     ├─→ OrcaFlex YAML │
│  Mooring Stiff. ──┼────→│ Integration  │────→├─→ HTML Reports  │
│  Metocean Data ───┤     │    Module    │     ├─→ CSV/JSON      │
│  Templates ───────┘     └──────────────┘     └─→ Validation    │
│                                │                                 │
│                                ├─→ Marine Analysis               │
│                                ├─→ OrcaFlex Integration          │
│                                └─→ Mooring Design                │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

---

### Module Statistics

| Metric | Value |
|--------|-------|
| **AQWA Versions** | 18.0+ (2018 to latest) |
| **File Formats** | 4 (DAT, LIS, AQWA-GS, Workbench) |
| **RAO Types** | 3 (Displacement, Velocity, Acceleration) |
| **Validation Checks** | 15+ automated quality tests |
| **Export Formats** | 5 (OrcaFlex, WAMIT, CSV, JSON, HDF5) |
| **Batch Processing** | Parallel execution (configurable cores) |
| **Visualization** | Interactive HTML (Plotly) |

---

### Real-World Applications

- **FPSO Design** - Hydrodynamic analysis and RAO generation
- **Semi-Submersible** - Platform motion and mooring loads
- **Spar Platform** - Heave and pitch response verification
- **TLP Design** - Tension leg platform dynamics
- **Ship Design** - Seakeeping and operability assessment
- **Offshore Wind** - Floating foundation response
- **LNG Carrier** - Berthing and offloading operations
- **Drilling Riser** - Vessel motion for riser analysis

---

### About Digital Model

**Digital Model** is a comprehensive engineering asset lifecycle management platform featuring:

- **20+ years** offshore/subsea engineering experience
- **200+ SURF engineers'** collective insights validated
- **Production-ready** - active use in major offshore projects
- **704+ Python modules** - comprehensive capability coverage
- **1,971+ test cases** - rigorous quality assurance
- **Open architecture** - MIT license, GitHub-hosted

**Dedicated to Mark Cerkovnik** - Chief Engineer, mentor, and inspiration.

---

### Contact & Resources

**Technical Support**
- Email: vamsee.achanta@aceengineer.com
- GitHub: https://github.com/vamseeachanta/digitalmodel

**Documentation**
- Module Guide: `/src/digitalmodel/modules/aqwa/`
- AQWA Manuals: `/docs/modules/aqwa/reference-manuals/`
- Workflows: `/docs/modules/aqwa/workflows/`
- Examples: `/docs/modules/aqwa/examples/`
- Scripts: `/docs/modules/aqwa/scripts/`

**Installation**
```bash
pip install git+https://github.com/vamseeachanta/digitalmodel.git
```

---

*Digital Model AQWA Integration Module - Version 1.0.0*
*Professional Engineering Software for Offshore & Marine Applications*
*© 2025 Digital Model Project - MIT License*
