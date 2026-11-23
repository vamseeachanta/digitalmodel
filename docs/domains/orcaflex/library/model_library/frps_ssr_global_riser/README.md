# FrPS / SSR Free-Standing Riser — Global Dynamic Model (Input Scaffold)

> **Status: INPUT SCAFFOLD — not solved, no results.**
> This directory contains the *model-input definition* for the global dynamic
> analysis the FDAS riser-buckling screening campaign concluded it needs. It is
> **not a run** and produces **no engineering results**. A licensed solver
> (OrcaFlex / Flexcom / Abaqus-Aqua) is required to execute it and is almost
> certainly **not installed or licensed** in the environment where this was
> scaffolded (cf. workspace-hub #2849, "solver-license availability").

## What this model is

A parameterized OrcaFlex model of a **free-standing, dry-tree riser** landed on
a surface SSR / wellhead. It is the engineering follow-on to the FDAS screening
campaign (`github.com/vamseeachanta/llm-wiki-fdas` issues **#2–#7**), whose
conservative hand-screens (unrestrained Euler #2/#3, survival-pushdown stress
#4, tendon tension #7) all fail — and whose verdict pivots on inputs the screens
cannot manufacture. The global-model scope is **FDAS issue #2**; this scaffold
mirrors that scope as reviewable YAML.

### Geometry
- Concentric **16 in / 11.75 in / 7 in** strings.
- **810 ft (246.888 m)** bare-pipe "slick" section carrying the dry tree.
- Centralizers at **~30 ft (9.144 m)** spacing.
- Set-down stops, surface wellhead landing.
- Water depths **6,000 / 8,000 / 10,000 ft** (1828.8 / 2438.4 / 3048.0 m).

### Top boundary (per FDAS #2, Jun-4 Chuck↔Roy exchange)
Axially sliding on UHMW pads (μ < 0.1) but **restrained against rotation** (no
articulation) — modeled as guided / rotation-fixed with stroke limits at the
set-down stops. Baseline assumes the **7-in tubing is always installed and
pre-tensioned** when the tree is on the SSR; the *no-tubing* case is carried as
a robustness/abnormal sensitivity, not the baseline.

## Files

| File | Purpose |
|---|---|
| `spec.yml` | Human-authored, parameterized model definition (geometry, strings, slick section, restraints, boundaries). Consumed by the `modular_generator`. Section properties are **placeholders** pending the section-basis decision. |
| `inputs/parameters.yml` | Parametric matrix: section-basis variants, demand/set-down variants, water-depth cases, restraint-stiffness placeholders, metocean/load-case matrix, analysis-type toggles, sensitivity cases, tooling/license note. |
| `README.md` | This file. |

> `modular/` (the OrcaFlex-loadable `master.yml` + `includes/*.yml`) is
> **generator output and is intentionally not committed**. Generate it on a
> licensed solver host (see below) after the two open inputs are resolved.

## The two OPEN inputs that must be set before build

Both are unresolved in the screening campaign and are carried as explicit
**variants** in `inputs/parameters.yml` — they are deliberate engineering
decisions, not values to guess:

1. **Section basis** (`section_basis`, FDAS #3) —
   **82.51 in²** (two-casing, tubing not structural) **vs ~97 in²**
   (three-string, 7-in WT 0.50→0.75 in). This sets the buckling demand the
   screens flagged. Pick one variant; derive EA/EI for the concentric stack
   and write it into `spec.yml` line types before build. *Do not average.*

2. **Roy demand / set-down definition** (`demand.setdown_definition`, FDAS #4/#7) —
   Roy's "5–10 ft pushdown": does it **net the slip-joint stroke**?
   Variants `gross_pushdown` vs `net_of_stroke` bound it. The preferred
   resolution per #2 is to derive imposed set-down + heave from the **coupled
   vessel/riser RAO response**, not a quoted figure; the variants bound it
   until that coupled run exists.

Additional values marked `~` / `TBD` / `PLACEHOLDER` (restraint stiffnesses,
metocean magnitudes, S-N class, RAOs) are **not results** — they are slots for
project data and calibration.

## Analyses the model is structured to support

Each closes a specific open screening item (`inputs/parameters.yml → analyses`):

| Analysis | Closes | Output |
|---|---|---|
| **Static** | #4/#7 | set-down, stop reaction, tendon tension, effective-tension profile |
| **Modal/eigenvalue** | #3/#5 | mode shapes/frequencies, restraint efficiency, VIV lock-in screen |
| **Dynamic time-domain** | storm combos | max tension/stress, snap loads, dynamic stop reaction |
| **Fatigue** | #4/#5 | VIV + wave stress-range spectra → S-N life |

## Hydrodynamics / RAOs

Vessel RAOs (and QTFs) are to be sourced from the **digitalmodel OrcaWave /
diffraction capability** for the actual SSR hull and linked into the vessel
type in `spec.yml`. The placeholder vessel type is **not populated** and must
not be used as-is.

## How to generate the OrcaFlex model (on a licensed host)

```bash
# 1. Resolve the two open inputs in inputs/parameters.yml
#    (set section_basis.selected and demand.setdown_definition; derive EA/EI).
# 2. Generate the OrcaFlex-loadable include tree:
uv run python -m digitalmodel.solvers.orcaflex.modular_generator generate \
    --input spec.yml --output modular/
```

This follows the same convention as the other entries under
`docs/domains/orcaflex/library/model_library/` (e.g. `b01_drilling_riser`).

## What is scaffolded vs. what is needed

**Scaffolded (this commit):**
- Geometry, concentric strings, slick section, boundaries/restraint structure.
- Parametric matrix: section-basis & demand variants, water-depth cases,
  metocean/load-case matrix, analysis toggles, sensitivity (no-tubing) case.
- Honest placeholders for every value the screening could not settle.

**Still needed (NOT in this commit):**
- A **licensed solver** (OrcaFlex primary; Flexcom / Abaqus-Aqua alternatives)
  on a host where it is installed — none assumed here.
- Resolution of the **two open inputs** (section basis; Roy demand definition).
- Project **metocean** data, **restraint-stiffness calibration**, **S-N class**,
  and **OrcaWave-sourced RAOs/QTFs**.
- The generated `modular/` tree, then static → modal → dynamic → fatigue runs.

## Provenance

- Screening campaign: `llm-wiki-fdas` #3–#7.
- Global-model scope: `llm-wiki-fdas` #2.
- Basis page: `llm-wiki-fdas pages/frps-fsr-global-riser-model.md` (PR #9, unmerged).
- No raw vendor files are committed; off-repo raw data should be referenced by
  sha256 pointer per the house method.
