# digitalmodel - Repository Skills

> Repository-specific Claude Code skills for marine/offshore engineering analysis.
>
> Location: `digitalmodel/.claude/skills/`

## Overview

This collection provides **10 specialized skills** for marine and offshore engineering workflows. These skills are automatically activated when Claude Code determines they're relevant to the current task.

## Available Skills

| Skill | Description |
|-------|-------------|
| [fatigue-analysis](fatigue-analysis/SKILL.md) | Perform fatigue analysis using S-N curves and damage accumulation (DNV, API, BS, ABS standards) |
| [structural-analysis](structural-analysis/SKILL.md) | Perform structural analysis (stress, buckling, capacity checks) per design codes |
| [mooring-design](mooring-design/SKILL.md) | Design and analyze mooring systems (CALM, SALM, catenary) with safety factors |
| [orcaflex-modeling](orcaflex-modeling/SKILL.md) | Setup and run OrcaFlex hydrodynamic simulations with universal runner |
| [orcaflex-post-processing](orcaflex-post-processing/SKILL.md) | Post-process OrcaFlex results (summary stats, range graphs, time series, visualization) |
| [signal-analysis](signal-analysis/SKILL.md) | Signal processing, rainflow counting (ASTM E1049-85), FFT/PSD spectral analysis |
| [catenary-riser](catenary-riser/SKILL.md) | Catenary and lazy wave riser static analysis with OrcaFlex model generation |
| [viv-analysis](viv-analysis/SKILL.md) | Vortex-induced vibration assessment for risers and tubular members |
| [aqwa-analysis](aqwa-analysis/SKILL.md) | AQWA hydrodynamic software integration for RAO and coefficient extraction |
| [hydrodynamics](hydrodynamics/SKILL.md) | Hydrodynamic coefficients, wave spectra, and OCIMF environmental loading |

## Skill Categories

### Structural & Fatigue Analysis

- **fatigue-analysis**: 221 S-N curves from 17 international standards (DNV, API, BS, ABS, etc.)
- **structural-analysis**: Von Mises stress, plate buckling, capacity verification
- **signal-analysis**: Rainflow cycle counting, FFT/PSD, time series conditioning

### Riser & Pipe Analysis

- **catenary-riser**: Simple catenary and lazy wave riser static configuration
- **viv-analysis**: VIV susceptibility, natural frequencies, safety factor evaluation

### Mooring Systems

- **mooring-design**: CALM/SALM buoy design, catenary analysis, safety factor calculations

### Hydrodynamics

- **hydrodynamics**: 6×6 coefficient matrices, wave spectra (JONSWAP, PM), OCIMF loading
- **aqwa-analysis**: RAO extraction, added mass/damping, AQWA file processing

### OrcaFlex Integration

- **orcaflex-modeling**: Model setup, static/dynamic analysis, batch processing, universal runner
- **orcaflex-post-processing**: OPP summary, linked statistics, range graphs, time series, HTML reports

## Usage

### Automatic Activation

Skills activate automatically based on their description:

```
User: "Run fatigue analysis on the riser using DNV S-N curves"
Claude: [Activates fatigue-analysis skill]

User: "Post-process the OrcaFlex simulation results"
Claude: [Activates orcaflex-post-processing skill]

User: "Extract RAOs from AQWA output file"
Claude: [Activates aqwa-analysis skill]

User: "Calculate rainflow cycles from stress time history"
Claude: [Activates signal-analysis skill]
```

### Manual Reference

Reference skills directly in prompts:

```
"Using the orcaflex-modeling skill, setup a batch run for all YAML files"
"Apply the structural-analysis skill to verify the capacity of this plate"
"Use the catenary-riser skill to calculate lazy wave configuration"
```

## Directory Structure

```
digitalmodel/.claude/skills/
├── README.md                    # This file
├── fatigue-analysis/
│   └── SKILL.md
├── structural-analysis/
│   └── SKILL.md
├── mooring-design/
│   └── SKILL.md
├── orcaflex-modeling/
│   └── SKILL.md
├── orcaflex-post-processing/
│   └── SKILL.md
├── signal-analysis/
│   └── SKILL.md
├── catenary-riser/
│   └── SKILL.md
├── viv-analysis/
│   └── SKILL.md
├── aqwa-analysis/
│   └── SKILL.md
└── hydrodynamics/
    └── SKILL.md
```

## Integration with Global Skills

These repository-specific skills complement the global skills in `~/.claude/skills/`:

- **Global**: General development, document handling, reporting
- **Repository-specific**: Domain-specific marine/offshore engineering

## Best Practices & Guidelines

### [Interactive Plotting Best Practices](interactive-plotting-best-practices.md)

Comprehensive guidelines for creating effective interactive HTML visualizations:

**General Principles (All Plots)**:
- **Clear Visual Hierarchy**: Bold labels, appropriate font sizes
- **Subplot Organization**: Multi-panel layouts with consistent formatting
- **Interactive Elements**: Hover info, dropdown menus, export capabilities
- **Data Export**: Always include CSV data alongside HTML plots

**Displacement RAO Plots Specifically**:
- **Y-Axis Range Management**: Minimum 0.2 range to avoid noise from small RAO values

**Key Lessons**:
- Use bold formatting for subplot titles and axis labels with units (all plots)
- Font sizes: titles 16px, axis labels 14px minimum (all plots)
- Export data as CSV for reproducibility and further analysis (all plots)
- Set minimum y-axis range (≥ 0.2) for **displacement RAO plots only** to prevent noise
- Other plot types use context-appropriate ranges (time series, stress, fatigue)

## Related Documentation

- [digitalmodel Module Documentation](../../docs/)
- [OrcaFlex Module README](../../src/digitalmodel/modules/orcaflex/README.md)
- [Fatigue Module Documentation](../../src/digitalmodel/modules/fatigue/)
- [Workspace Hub Skills](../../../.claude/skills/README.md)
- [HTML Reporting Standards](../../docs/modules/standards/HTML_REPORTING_STANDARDS.md)

---

*Repository-specific skills for digitalmodel marine/offshore engineering workflows*
