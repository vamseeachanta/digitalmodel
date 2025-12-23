# digitalmodel - Repository Skills

> Repository-specific Claude Code skills for marine/offshore engineering analysis.
>
> Location: `digitalmodel/.claude/skills/`

## Overview

This collection provides **5 specialized skills** for marine and offshore engineering workflows. These skills are automatically activated when Claude Code determines they're relevant to the current task.

## Available Skills

| Skill | Description |
|-------|-------------|
| [fatigue-analysis](fatigue-analysis/SKILL.md) | Perform fatigue analysis using S-N curves and damage accumulation (DNV, API, BS, ABS standards) |
| [mooring-design](mooring-design/SKILL.md) | Design and analyze mooring systems (CALM, SALM, catenary) with safety factors |
| [structural-analysis](structural-analysis/SKILL.md) | Perform structural analysis (stress, buckling, capacity checks) per design codes |
| [orcaflex-modeling](orcaflex-modeling/SKILL.md) | Setup and run OrcaFlex hydrodynamic simulations with universal runner |
| [orcaflex-post-processing](orcaflex-post-processing/SKILL.md) | Post-process OrcaFlex results (summary stats, range graphs, time series, visualization) |

## Skill Categories

### Structural & Fatigue Analysis

- **fatigue-analysis**: 221 S-N curves from 17 international standards (DNV, API, BS, ABS, etc.)
- **structural-analysis**: Von Mises stress, plate buckling, capacity verification

### Mooring Systems

- **mooring-design**: CALM/SALM buoy design, catenary analysis, safety factor calculations

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
```

### Manual Reference

Reference skills directly in prompts:

```
"Using the orcaflex-modeling skill, setup a batch run for all YAML files"
"Apply the structural-analysis skill to verify the capacity of this plate"
```

## Directory Structure

```
digitalmodel/.claude/skills/
├── README.md                    # This file
├── fatigue-analysis/
│   └── SKILL.md
├── mooring-design/
│   └── SKILL.md
├── structural-analysis/
│   └── SKILL.md
├── orcaflex-modeling/
│   └── SKILL.md
└── orcaflex-post-processing/
    └── SKILL.md
```

## Integration with Global Skills

These repository-specific skills complement the global skills in `~/.claude/skills/`:

- **Global**: General development, document handling, reporting
- **Repository-specific**: Domain-specific marine/offshore engineering

## Related Documentation

- [digitalmodel Module Documentation](../../docs/)
- [OrcaFlex Module README](../../src/digitalmodel/modules/orcaflex/README.md)
- [Fatigue Module Documentation](../../src/digitalmodel/modules/fatigue/)
- [Workspace Hub Skills](../../../.claude/skills/README.md)

---

*Repository-specific skills for digitalmodel marine/offshore engineering workflows*
