# CALM Buoy Operability Analysis - Implementation Proposal

## Overview

Based on the completed 1-year return period implementation, this document proposes the next steps to enable comprehensive operability analysis for CALM Buoy systems using OrcaFlex.

---

## Current State

### ✅ What We Have:

1. **Return Period Data Structure**:
   - `Operating` - Generic daily operations (Hs=2.0-2.5m)
   - `Design (1-year)` - Operability limit (Hs=2.5m, Tp=7.5s) ⭐ NEW
   - `Design (10-year)` - Design verification (Hs=5.0m, Tp=11.0s) ⭐ NEW
   - `Extreme (100-year)` - Survival (Hs=8.5m, Tp=14.0s)

2. **Load Cases**:
   - 4 basic load cases (2 operating + 2 extreme)
   - Limited directional coverage (0°, 45°, 90°)
   - Condition references: "operating" and "extreme"

3. **Key Mappings**:
   - Full bidirectional human-friendly ↔ technical key mapping
   - All return period parameters mapped

### ❌ What's Missing for Operability Analysis:

1. **Comprehensive Directional Coverage**:
   - Current: 3 directions (0°, 45°, 90°)
   - Needed: 12-24 directions (30° or 15° spacing)

2. **Return Period Mapping**:
   - Load cases don't explicitly reference return periods
   - No mechanism to select "design_1yr_conditions" for operability

3. **Operability Analysis Workflow**:
   - No automated generation of 360° analysis
   - No weather downtime calculations
   - No operability envelope outputs

---

## Proposal: Operability Analysis Implementation

### Phase 1: Load Case Condition Mapping (IMMEDIATE)

**Goal**: Enable load cases to reference specific return periods

**Changes Required**:

#### 1.1 Update Load Case Structure

**Current**:
```yaml
Load Cases:
  - Name: "operating_head_seas"
    Condition: "operating"  # References "Operating" section
    Wave Direction: 0
```

**Proposed**:
```yaml
Load Cases:
  - Name: "operability_000deg"
    Condition: "design_1yr"  # References "Design (1-year)" section
    Wave Direction: 0
    Current Direction: 0
    Wind Direction: 0
    Purpose: "Operability analysis per API RP 2SK"
```

#### 1.2 Update Condition Resolution Logic

Add to `generate_calm_buoy_project.py`:

```python
def _resolve_metocean_condition(self, condition_name: str) -> Dict[str, Any]:
    """
    Resolve load case condition to metocean parameters.

    Args:
        condition_name: "operating", "design_1yr", "design_10yr", or "extreme"

    Returns:
        Dictionary of metocean parameters
    """
    condition_map = {
        'operating': 'human_input.site.metocean_override.operating_conditions',
        'design_1yr': 'human_input.site.metocean_override.design_1yr_conditions',
        'design_10yr': 'human_input.site.metocean_override.design_10yr_conditions',
        'extreme': 'human_input.site.metocean_override.extreme_conditions',
    }

    if condition_name not in condition_map:
        raise ValueError(f"Unknown condition: {condition_name}")

    return self._get_value(condition_map[condition_name])
```

---

### Phase 2: Operability Load Case Generator (HIGH PRIORITY)

**Goal**: Automatically generate 12-24 load cases for 360° analysis

**Implementation**:

```python
def generate_operability_load_cases(
    self,
    directions: int = 12,
    return_period: str = "design_1yr"
) -> List[Dict[str, Any]]:
    """
    Generate operability analysis load cases for 360° coverage.

    Args:
        directions: Number of directions (12, 24, 36)
        return_period: "design_1yr", "design_10yr", etc.

    Returns:
        List of load case dictionaries
    """
    angle_step = 360 / directions
    load_cases = []

    for i in range(directions):
        angle = int(i * angle_step)
        load_cases.append({
            'name': f'operability_{angle:03d}deg',
            'condition': return_period,
            'wave_direction': angle,
            'current_direction': angle,  # Aligned with waves
            'wind_direction': angle,     # Aligned with waves
            'purpose': f'Operability analysis - {angle}° heading'
        })

    return load_cases
```

**Usage**:
```yaml
Analysis Settings:
  Operability Analysis:
    Enabled: Yes
    Return Period: "design_1yr"  # Use 1-year for operability
    Directions: 12  # 30° spacing (standard)
    # Or: 24 for 15° spacing (detailed)
```

---

### Phase 3: OrcaFlex Module Updates (MEDIUM PRIORITY)

**Goal**: Update environment modules to use return period data

#### 3.1 Environment Module Template

Update `specs/modules/orcaflex/modular-input-file/output/_03c_waves.yml`:

**Current**:
```yaml
Environment:
  WaveHeight: 2.5  # Hardcoded
  WavePeriod: 8.0  # Hardcoded
```

**Proposed**:
```yaml
Environment:
  # Metocean parameters populated from return period section
  WaveHeight: {{wave_hs}}  # From design_1yr_conditions.hs_1yr
  WavePeriod: {{wave_tp}}   # From design_1yr_conditions.tp_1yr
  WaveDirection: {{wave_dir}}  # From load case

  CurrentSpeed: {{current_speed}}  # From design_1yr_conditions.current_speed_surface_1yr
  CurrentDirection: {{current_dir}}  # From load case

  WindSpeed: {{wind_speed}}  # From design_1yr_conditions.wind_speed_1yr
  WindDirection: {{wind_dir}}  # From load case
```

#### 3.2 Template Variable Substitution

Add to generator:
```python
def _populate_environment_module(
    self,
    template_path: Path,
    condition: Dict[str, Any],
    directions: Dict[str, float]
) -> str:
    """
    Populate environment module with return period data.

    Args:
        template_path: Path to module template
        condition: Metocean condition dictionary
        directions: Wave/current/wind directions

    Returns:
        Populated module content
    """
    with open(template_path, 'r') as f:
        template = f.read()

    substitutions = {
        'wave_hs': condition['hs_1yr'],
        'wave_tp': condition['tp_1yr'],
        'wave_dir': directions['wave_direction'],
        'current_speed': condition['current_speed_surface_1yr'],
        'current_dir': directions['current_direction'],
        'wind_speed': condition['wind_speed_1yr'],
        'wind_dir': directions['wind_direction'],
    }

    for key, value in substitutions.items():
        template = template.replace(f'{{{{{key}}}}}', str(value))

    return template
```

---

### Phase 4: Operability Results Post-Processing (FUTURE)

**Goal**: Generate operability envelopes and weather downtime statistics

**Outputs**:

1. **Operability Envelope Plot**:
   - X-axis: Heading (0-360°)
   - Y-axis: Maximum mooring tension
   - Limit line: MBL/SF threshold
   - Identify operable sectors

2. **Weather Downtime Calculation**:
   - Based on wave scatter diagram
   - Percentage of time Hs > operability limit
   - Monthly/seasonal breakdown

3. **Critical Headings Report**:
   - Identify worst-case directions
   - Mooring line utilization ratios
   - Clearance margins

**Implementation**:
```python
class OperabilityAnalyzer:
    """Analyze operability results from OrcaFlex simulations."""

    def generate_operability_envelope(
        self,
        results: List[Dict],
        mbl: float,
        sf: float
    ) -> Path:
        """Generate interactive Plotly operability envelope plot."""
        pass

    def calculate_weather_downtime(
        self,
        wave_scatter: pd.DataFrame,
        operability_limit_hs: float
    ) -> Dict[str, float]:
        """Calculate weather downtime statistics."""
        pass

    def generate_critical_headings_report(
        self,
        results: List[Dict]
    ) -> pd.DataFrame:
        """Identify critical headings and maximum responses."""
        pass
```

---

## Implementation Priority

### 🔴 IMMEDIATE (Next Step):

**Task**: Update load case condition mapping

**Files to Modify**:
1. `scripts/generate_calm_buoy_project.py`
   - Add `_resolve_metocean_condition()` method
   - Update load case processing to use return periods

2. `examples/modules/calm_buoy/north_sea_calm_project_human.yml`
   - Update load cases to use "design_1yr" condition
   - Add example operability load cases

**Expected Outcome**:
- Load cases can reference "design_1yr", "design_10yr", "extreme"
- Generation script correctly extracts return period data
- Test generation passes with new condition references

**Time Estimate**: 2-3 hours

---

### 🟡 HIGH PRIORITY:

**Task**: Implement operability load case generator

**Files to Create/Modify**:
1. `scripts/generate_calm_buoy_project.py`
   - Add `generate_operability_load_cases()` method
   - Add CLI option: `--operability-directions 12`

2. Update YAML schema to support operability settings

**Expected Outcome**:
- Automatic generation of 12/24/36 directional load cases
- User can specify number of directions
- Load cases properly distributed around 360°

**Time Estimate**: 4-6 hours

---

### 🟢 MEDIUM PRIORITY:

**Task**: Update OrcaFlex module templates

**Files to Modify**:
1. Environment modules (_03c_waves.yml, _03d_current.yml, _03e_wind.yml)
   - Add template variable placeholders
   - Update generator to populate variables

**Expected Outcome**:
- Environment modules use return period data
- Each load case gets correct metocean conditions
- Generated OrcaFlex files ready for operability analysis

**Time Estimate**: 6-8 hours

---

### 🔵 FUTURE:

**Task**: Operability results post-processing

**Files to Create**:
1. `scripts/analyze_operability_results.py`
2. `src/digitalmodel/modules/orcaflex/operability_analysis.py`

**Expected Outcome**:
- Automated operability envelope generation
- Weather downtime calculations
- Interactive HTML reports with Plotly

**Time Estimate**: 2-3 days

---

## Example: Operability Analysis Workflow

### Step 1: Configure Project (User)

```yaml
Human Input:
  Site Conditions:
    Metocean Conditions:
      Design (1-year):
        Significant Wave Height (Hs): 2.5  # m
        Peak Period (Tp): 7.5  # s
        Wind Speed (1-hour mean): 20  # m/s
        Surface Current: 1.4  # m/s

  Analysis Settings:
    Operability Analysis:
      Enabled: Yes
      Return Period: "design_1yr"
      Directions: 12  # 30° spacing
```

### Step 2: Generate Project

```bash
python scripts/generate_calm_buoy_project.py \
  --config examples/modules/calm_buoy/north_sea_calm_project_human.yml \
  --output projects/NSE_OPERABILITY \
  --operability-directions 12 \
  --validate
```

**Generates**:
- 12 OrcaFlex load cases (0°, 30°, 60°, ..., 330°)
- Each uses 1-year return period data (Hs=2.5m)
- All environment modules populated correctly

### Step 3: Run OrcaFlex Simulations

```bash
# User runs OrcaFlex batch analysis
# 12 simulations × 1 hour each = 12 hours runtime
```

### Step 4: Post-Process Results (Future)

```bash
python scripts/analyze_operability_results.py \
  --project projects/NSE_OPERABILITY \
  --output-report reports/operability_analysis.html
```

**Generates**:
- Interactive operability envelope plot
- Weather downtime statistics
- Critical headings report
- Mooring utilization summary

---

## Benefits

### For Engineers:
- ✅ **Automated Operability Analysis**: No manual load case creation
- ✅ **Research-Based Data**: NORA10/NORSOK/DNV-GL validated values
- ✅ **API RP 2SK Compliance**: 1-year return period per standard
- ✅ **Comprehensive Coverage**: 360° analysis, all critical headings

### For Projects:
- ✅ **Weather Downtime Estimates**: Quantified operability limits
- ✅ **Design Optimization**: Identify critical headings early
- ✅ **Regulatory Compliance**: Documented analysis per standards
- ✅ **Reduced Risk**: Comprehensive pre-installation analysis

### For System:
- ✅ **Flexible Architecture**: Supports 1yr/10yr/100yr analysis
- ✅ **Scalable**: 12/24/36/72 directions easily configured
- ✅ **Maintainable**: Clear separation of concerns
- ✅ **Extensible**: Ready for fatigue, damaged conditions, sensitivity

---

## Success Criteria

### Phase 1 Complete When:
- [x] 1-year return period data implemented
- [ ] Load cases reference return periods ("design_1yr")
- [ ] Generation script resolves conditions correctly
- [ ] Test generation passes with new structure

### Phase 2 Complete When:
- [ ] Operability load case generator working
- [ ] 12/24/36 directions auto-generated
- [ ] CLI option `--operability-directions` functional
- [ ] Documentation updated

### Phase 3 Complete When:
- [ ] Environment modules use template variables
- [ ] Each load case gets correct metocean data
- [ ] Generated OrcaFlex files validated
- [ ] Example operability project complete

### Phase 4 Complete When:
- [ ] Post-processing scripts operational
- [ ] Operability envelopes auto-generated
- [ ] Weather downtime calculated
- [ ] HTML reports with interactive plots

---

## Questions for Stakeholder

1. **Directional Spacing**:
   - 12 directions (30° spacing) - Standard operability
   - 24 directions (15° spacing) - Detailed analysis
   - Which is preferred for typical projects?

2. **Return Period Selection**:
   - Should operability analysis default to 1-year?
   - Or allow user to select (1yr/10yr)?

3. **Damaged Conditions**:
   - Should operability analysis include damaged mooring (1-line out)?
   - If yes, which return period? (typically 10-year)

4. **Priority**:
   - Should we proceed with Phase 1 immediately?
   - Or wait for feedback on full proposal?

---

**Created**: 2025-11-11
**Status**: ✅ Proposal ready for review
**Next Action**: Stakeholder approval to proceed with Phase 1
