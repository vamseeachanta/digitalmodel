# Hydrodynamic Coefficient Visualization Charts

## Overview

This directory contains comprehensive visualizations of hydrodynamic coefficients extracted from marine engineering analysis data. The charts illustrate frequency-dependent added mass and damping coefficients for all six degrees of freedom (DOF).

**Generated:** 2025-10-03

## Chart Gallery

### 1. Frequency Response Curves
**File:** `frequency_response_curves.png`

Shows added mass (solid red) and damping (dashed blue) coefficients as functions of wave frequency for all six DOF:
- Surge (1,1)
- Sway (2,2)
- Heave (3,3)
- Roll (4,4)
- Pitch (5,5)
- Yaw (6,6)

**Key Insights:**
- Added mass decreases with increasing frequency
- Damping exhibits peaks near resonance frequencies
- Each DOF has distinct frequency response characteristics

---

### 2. Critical Damping Ratios
**File:** `critical_damping_ratios.png`

Bar chart showing damping ratio (ζ) for each degree of freedom at reference frequency (ω ≈ 1.0 rad/s).

**Color Coding:**
- 🔴 Red: ζ < 0.05 (Highly underdamped)
- 🟡 Yellow: 0.05 ≤ ζ < 0.1 (Underdamped)
- 🟢 Green: ζ ≥ 0.1 (Well damped)

**Typical Values:**
- ζ < 0.05: Potential for resonance amplification
- ζ = 1.0: Critical damping (optimal for motion control)
- ζ > 1.0: Overdamped (sluggish response)

---

### 3. Coupling Coefficient Network
**File:** `coupling_network.png`

Network diagram visualizing coupling between different degrees of freedom through added mass coefficients.

**Features:**
- **Nodes:** Six DOF arranged in hexagonal layout
- **Edges:** Coupling strength shown by line thickness
- **Labels:** Coupling values displayed on edges

**Typical Couplings:**
- Surge-Pitch (0,4): Longitudinal motion coupling
- Sway-Roll (1,3): Lateral motion coupling
- Heave-Pitch (2,4): Vertical-rotational coupling

---

### 4. Natural Period Analysis
**File:** `natural_periods.png`

Shows estimated natural periods for each DOF as functions of wave frequency.

**Applications:**
- Identify resonance zones
- Design mooring systems
- Optimize station-keeping performance
- Assess seakeeping characteristics

---

### 5. Added Mass Matrix Animation
**File:** `added_mass_animation.gif`

Animated heatmap showing evolution of 6×6 added mass matrix across all wave frequencies.

**Animation Details:**
- Frame rate: 5 fps
- Duration: ~17 seconds (84 frequencies at 4-frame interval)
- Color scale: Yellow-Orange-Red (YlOrRd)

**Usage:**
View in web browser or image viewer supporting GIF animations.

---

### 6. Damping Coefficient Animation
**File:** `damping_animation.gif`

Animated heatmap showing evolution of 6×6 damping coefficient matrix across all wave frequencies.

**Animation Details:**
- Frame rate: 5 fps
- Duration: ~17 seconds
- Color scale: Blue gradient

**Key Observations:**
- Damping peaks at intermediate frequencies
- Strong diagonal dominance
- Frequency-dependent coupling patterns

---

## Individual Heatmaps

Static heatmaps are generated at key frequencies:

### Low Frequency (ω ≈ 0.1 rad/s, T ≈ 63 s)
- `added_mass_heatmap_omega_0.1000.png`
- `damping_heatmap_omega_0.1000.png`

**Characteristics:**
- Maximum added mass values
- Low damping coefficients
- Long wave period response

### Medium Frequency (ω ≈ 1.57 rad/s, T ≈ 4 s)
- `added_mass_heatmap_omega_1.5675.png`
- `damping_heatmap_omega_1.5675.png`

**Characteristics:**
- Moderate added mass
- Peak damping values
- Typical operational sea states

### High Frequency (ω ≈ 3.0 rad/s, T ≈ 2 s)
- `added_mass_heatmap_omega_3.0000.png`
- `damping_heatmap_omega_3.0000.png`

**Characteristics:**
- Minimum added mass
- Reduced damping
- Short wave period response

---

## Data Interpretation

### Degrees of Freedom (DOF)

| DOF | Motion Type | Index |
|-----|-------------|-------|
| Surge | Longitudinal translation | 1 |
| Sway | Lateral translation | 2 |
| Heave | Vertical translation | 3 |
| Roll | Rotation about longitudinal axis | 4 |
| Pitch | Rotation about lateral axis | 5 |
| Yaw | Rotation about vertical axis | 6 |

### Matrix Notation

- **A_ij**: Added mass coefficient (i-th force due to j-th acceleration)
- **B_ij**: Damping coefficient (i-th force due to j-th velocity)

**Symmetry:**
- Diagonal terms (A_ii, B_ii): Direct coefficients
- Off-diagonal terms (A_ij, B_ij): Coupling coefficients
- Matrices are typically symmetric (A_ij = A_ji)

---

## Applications

### 1. Motion Response Analysis
Use frequency response curves to:
- Predict vessel motions in irregular seas
- Calculate Response Amplitude Operators (RAOs)
- Assess comfort and operational limits

### 2. Mooring System Design
Use damping ratios and natural periods to:
- Size mooring lines and anchors
- Optimize mooring configuration
- Evaluate station-keeping performance

### 3. Seakeeping Performance
Use coupling networks to:
- Identify critical motion couplings
- Design motion reduction systems
- Optimize hull form

### 4. Digital Twin Integration
Export coefficient data for:
- Real-time motion prediction
- Control system tuning
- Operational decision support

---

## Technical Details

### Data Source
- **Frequency Range:** 0.1 - 3.0 rad/s (84 points)
- **Period Range:** 2.1 - 62.8 seconds
- **Matrix Size:** 6×6 for each frequency
- **Total Data Points:** 6,048 coefficients

### Visualization Tools
- **Matplotlib:** Static charts and animations
- **Seaborn:** Heatmap styling
- **NumPy:** Numerical computations
- **Pandas:** Data management

### File Formats
- **PNG:** High-resolution static images (150 dpi)
- **GIF:** Animated visualizations (100 dpi)
- **CSV:** Raw coefficient data (see `data/marine_engineering/hydrodynamic/`)

---

## Usage Examples

### Python API
```python
from scripts.extract_hydro_coefficients import HydrodynamicCoefficientExtractor

# Initialize extractor
extractor = HydrodynamicCoefficientExtractor(
    excel_path='path/to/coefficients.xlsx',
    output_dir='data/marine_engineering/hydrodynamic'
)

# Extract and visualize
extractor.extract_from_excel(sheet_name='Damping')
extractor.plot_frequency_response_curves()
extractor.plot_critical_damping_ratios()
extractor.generate_html_report()
```

### Command Line
```bash
# Run complete extraction pipeline
python scripts/run_hydro_extraction.py

# Run with custom Excel file
python scripts/extract_hydro_coefficients.py --excel path/to/file.xlsx --sheet Damping

# View examples
python scripts/example_hydro_usage.py
```

---

## Chart Quality Guidelines

### Resolution
- **Screen viewing:** Default PNG at 150 dpi
- **Presentations:** Use PNG files directly
- **Publications:** Re-generate at 300 dpi if needed

### Color Schemes
- **Added Mass:** Yellow-Orange-Red (warm colors)
- **Damping:** Blue gradient (cool colors)
- **Frequency Response:** Red (added mass) + Blue (damping)

### Accessibility
- High contrast color schemes
- Clear axis labels and titles
- Annotated values on heatmaps
- Grid lines for readability

---

## References

### Marine Engineering Standards
- **DNV-RP-C205:** Environmental Conditions and Environmental Loads
- **API RP 2SK:** Design and Analysis of Stationkeeping Systems for Floating Structures
- **ISO 19901-7:** Stationkeeping Systems for Floating Offshore Structures

### Hydrodynamic Theory
- Newman, J.N. (2018). *Marine Hydrodynamics*. MIT Press.
- Faltinsen, O.M. (1990). *Sea Loads on Ships and Offshore Structures*. Cambridge University Press.

### Software Documentation
- AQWA User Manual (ANSYS)
- WAMIT Hydrodynamic Analysis
- OrcaFlex Hydrodynamics

---

## Support

For questions or issues with coefficient extraction:

1. Check the HTML report: `docs/hydro_coefficients_extraction_report.html`
2. Review example scripts: `scripts/example_hydro_usage.py`
3. Examine raw data: `data/marine_engineering/hydrodynamic/`
4. Consult main documentation: `README.md`

---

## Version History

**v1.0.0** (2025-10-03)
- Initial release
- Complete extraction pipeline
- Six visualization chart types
- Interactive HTML reporting
- Animated heatmaps

---

**Generated by Digital Model Marine Engineering Suite**
*Advancing offshore engineering through data-driven visualization*
