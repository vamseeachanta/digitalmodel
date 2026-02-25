# OrcaFlex Modular Example Parameter Reference

Comprehensive reference for all `inputs/parameters.yml` keys found across the 49 modular
OrcaFlex example categories (A01–Z09, covering 62 individual example variants). Each entry
lists the parameter key, observed values, engineering units, a brief description, and the
OrcFxAPI attribute path used when applying the value to a model object.

---

## Parameter Categories

1. [Environment Parameters](#environment-parameters)
2. [Metocean / Sea-State Parameters](#metocean--sea-state-parameters)
3. [Simulation Staging Parameters](#simulation-staging-parameters)
4. [Numerical Parameters](#numerical-parameters)
5. [Cross-Example Comparison Table](#cross-example-comparison-table)
6. [Example Category Reference](#example-category-reference)

---

## Environment Parameters

Keys that define the physical environment — fluid properties, current, and wind.

| Parameter | Type | Units | OrcFxAPI Path | Observed Values | Description |
|-----------|------|-------|---------------|-----------------|-------------|
| `water_depth` | float | m | `model.general.WaterDepth` | 28–6560 | Still water depth below mean water level. Absent in some examples where it is defined directly inside the model YAML rather than as a parametric override (see notes). |
| `water_density` | float | t/m³ or lb/ft³ | `model.environment.Density` | 1.024, 1.025, 1025, 64 | Fluid density. **Most examples** use 1.025 t/m³ (SI seawater). A05/A06 use ≈64 (FPS units: lb/ft³ seawater). K06 uses 1025 (kg/m³, non-standard). Z02 uses 0.00128 (air density — in-air edge-case model). |
| `current_speed` | float | m/s | `model.environment.RefCurrentSpeed` | 0–2 | Uniform (depth-averaged) current speed at the reference depth. Set to 0 in still-water examples. Absent when the model uses a depth-profile current defined in the master YAML. |
| `current_direction` | float | deg | `model.environment.RefCurrentDirection` | 150, 180 | Direction FROM which the current comes (met. convention). Present only in C05 and C09; most examples embed direction in the model. |
| `wind_speed` | float | m/s | `model.environment.WindSpeed` | 0–35 | Uniform wind speed at reference height (10 m default). Set to 0 when wind is not modelled. Absent when the master YAML handles wind directly or when no wind is relevant. |

---

## Metocean / Sea-State Parameters

Keys that define wave loading. Many examples set `hs: 0` (current-only or static analysis).

| Parameter | Type | Units | OrcFxAPI Path | Observed Values | Description |
|-----------|------|-------|---------------|-----------------|-------------|
| `hs` | float | m | `model.environment.HsAtOrigin` (random) or `WaveHeight` (regular) | 0–90 | Significant wave height. **Zero-wave examples**: B06, C03, C06, C08, D02, D03, E01-E04, F01-F04, F07, G04, H02, J01, K02, K03, M01, Z02. A05 (spar) uses Hs=90 (FPS units, ≈27m SI). |
| `tp` | float | s | `model.environment.TpAtOrigin` (random) or `WavePeriod` (regular) | 4.5–17.5 | Peak spectral period (random wave) or regular wave period. Absent when `hs` is absent or zero. |
| `wave_direction` | float | deg | `model.environment.WaveDirection` | 0–360 | Wave propagation direction in degrees (met. convention: direction FROM). Present in all examples; 180° is the most common default (head seas). |

---

## Simulation Staging Parameters

OrcaFlex divides a simulation into sequential stages (e.g., build-up, storm, decay). The
`stage_durations` list directly sets the duration of each stage.

| Parameter | Type | Units | OrcFxAPI Path | Observed Values | Description |
|-----------|------|-------|---------------|-----------------|-------------|
| `stage_durations` | list[float] | s | `model.general.StageDuration[i]` | 1–8 stages; 1–3200 s each | List of stage durations in seconds. Length determines the number of simulation stages. The first stage is typically a build-up/ramp stage; subsequent stages are storm or operational periods. |
| `time_step` | float | s | `model.general.InnerTimeStep` | 0.01, 0.1 | Explicit time step override. Present only in C05 (0.1 s) and C09 (0.01 s); all other examples rely on the model's default inner time step. |

### Stage Count Patterns by Analysis Type

| Stage count | Typical use | Examples |
|-------------|-------------|---------|
| 1 | Long quasi-static, pipeline thermal | M01 (lateral buckling 550 s, walking 3200 s), Pipeline spanning |
| 2 | Standard: build-up + storm | A01, A02, B01, C10, D04, F01, G04, H01, H03, I01, K01, L01 |
| 3 | Ramp + event + decay | D02, F01 (trapped water), F07 |
| 4 | Multi-phase installation | A04, D02, D03 |
| 5 | Complex operation sequence | J01, D03 |
| 8 | Multi-stage pipelay with events | E05 |

---

## Numerical Parameters

| Parameter | Type | Units | OrcFxAPI Path | Observed Values | Description |
|-----------|------|-------|---------------|-----------------|-------------|
| `time_step` | float | s | `model.general.InnerTimeStep` | 0.01, 0.1 | See Simulation Staging section. Repeated here for completeness. |

---

## Cross-Example Comparison Table

Marks which parameters are present (✓), absent (–), or set to zero (0) across
representative examples. An asterisk (*) indicates a noteworthy non-default value.

| Parameter | A01 | A05* | B01 | C05 | E08 | G04 | K01 | L01 | M01 | Z02* |
|-----------|-----|------|-----|-----|-----|-----|-----|-----|-----|------|
| `water_depth` | 100 | 2950–6560 | 1020 | 220 | ? | 500 | – | 100 | – | 100 |
| `water_density` | 1.025 | 64* | 1.025 | 1.025 | 1.025 | 1.024 | 1.025 | 1.025 | 1.025 | 0.00128* |
| `current_speed` | 0.7 | 1 | 0 | 1.0 | ? | 0 | 0 | 0 | – | 0 |
| `current_direction` | – | – | – | 150* | – | – | – | – | – | – |
| `wind_speed` | 0 | – | – | 35* | – | – | 15* | – | 0 | 1 |
| `hs` | 6 | 25 | 1.5 | 10 | ? | 0 | 6 | 7 | 0 | 0 |
| `tp` | 7 | 18 | 8 | 13 | ? | 8 | 10 | 8 | 8 | 8 |
| `wave_direction` | 180 | 180 | 180 | 180 | ? | 180 | 180 | 180 | 180 | 180 |
| `stage_durations` count | 2 | 2 | 2 | 2 | ? | 2 | 2 | 2 | 1–2 | 2 |
| `time_step` | – | – | – | 0.1* | – | – | – | – | – | – |

Legend: `–` = parameter absent (model default used); `*` = noteworthy value; `?` = E08 uses
YAML batch config — parameters injected at runtime.

### Notable Observations

1. **Units mixed across examples** — A05 and A06 use FPS units (`water_density ≈ 64`
   lb/ft³, `hs` in feet). These must not be mixed with SI examples in a multi-case batch.
2. **Z02 is an in-air model** — `water_density: 0.00128` is the density of air. This is an
   edge-case test; physics results are meaningless in a seawater context.
3. **`water_depth` absent from L04/L05** — these semi-sub examples define WD inside the
   model rather than as a parameter override.
4. **`hs: 0` does not mean no sea state** — some examples with `hs: 0` still model current
   and wind. Zero-Hs is a "current-only" load case, not a zero-load case.
5. **`stage_durations` drives simulation cost** — M01 pipeline walking uses `[3200, 3200]`
   (total 6400 s) and is one of the most computationally expensive examples.

---

## Example Category Reference

Full map of example codes to model types, analysis category, and notable parameter features.

| Code | Model Type | Analysis | Notable parameters |
|------|-----------|----------|--------------------|
| **A01** | Flexible riser (cat/lw/pw/sw) | Time-domain riser | 4 variants; wave_direction varies (90/105/180/270) |
| **A02** | Lazy/pliant/steep S riser | Time-domain riser | current_speed=1 m/s; tp=8–12 s |
| **A03** | Deep riser | Time-domain riser | No water_depth or current_speed in params |
| **A04** | Deep water riser 200m | Riser, Hs=14.2 m | 4-stage; large Hs and Tp |
| **A05** | Riser + semi/spar/FPSO | Deep water (FPS units) | water_depth 2950–6560 ft; density≈64 lb/ft³ |
| **A06** | VIV (SHEAR7) | VIV, deep water | FPS units; Hs=0 (current-driven VIV) |
| **B01** | Drilling riser | Drilling ops 1020m | wave_direction=180; small Hs=1.5m |
| **B06** | Drilling riser shallow | No waves | 234m WD; no Hs/Tp (current-only) |
| **C03** | Mooring/multi-body | High current 2 m/s | Long stage_durations=[13, 500] |
| **C05** | Jack-up platform | Full environment | time_step=0.1; wind+current+waves; long duration |
| **C06** | CALM buoy | Mooring + buoy | No Hs/Tp; wind+current only |
| **C07** | Semi-sub mooring | Deepwater 500m | Large Hs=13m; current_speed=1.125 |
| **C08** | Shallow water structure | Static/near-static | 40m WD; no current/wind/waves |
| **C09** | Platform | Fine time step | time_step=0.01; small Hs=1.5m |
| **C10** | Multiple statics | YAML sweep | 100m WD; multi-static with wave+current |
| **D02** | Manifold lowering | 4-stage installation | 137m WD; no waves (static-dominated) |
| **D03** | Subsea installation | 5-stage | 300m WD; no waves |
| **D04** | Subsea | 2-stage | 120m WD; small Hs=3m |
| **E01** | Pipelay S-lay | Stinger explicit/simple | No Hs/Tp; current + direction |
| **E02** | Pipelay | Current-only | 100m WD; no wave |
| **E03** | Pipelay | Current | 100m WD; current |
| **E04** | Pipelay | Single current | 100m WD |
| **E05** | Pipelay (complex) | 8-stage ops | 8 stages; Hs=0.75m; complex sequence |
| **E08** | Lay table (YAML batch) | Batch automation | Parameters injected from lay-table config |
| **F01** | Subsea installation | 3 variants | Lowered cone / manifold / trapped water |
| **F02** | Subsea | No waves | 100m WD; current-free |
| **F03** | Heave-compensated winch | Crane + vessel | 100m WD; vessel motion |
| **F04** | Subsea | Wave dir 270° | 100m WD |
| **F06** | Subsea | Small waves | Hs=2m, Tp=7s |
| **F07** | Floating (no WD) | Wind + current | No water_depth; wave_direction=45° |
| **G04** | Deepwater mooring | 500m WD | No waves (current-only) |
| **H01** | Shallow ship ops | 28m WD | Hs=8m, Tp=10s; wave_direction=350° (oblique) |
| **H02** | Ship operations | 80m WD | No current/wave in params |
| **H03** | Ship operations | Harsh | Hs=13.2m; current=1.67 m/s |
| **I01** | VIV (OrcaFlex) | Native VIV | High current=2 m/s; Hs=2m |
| **J01** | Jack-up / riser | 5-stage | 250m WD; 5 stages; no waves |
| **K01** | FOWT spar (5MW) | Offshore wind | Wind=15 m/s; no WD param; Hs=6m |
| **K02** | Wind turbine | External wind file | No WD/Hs/Tp; external .bts wind |
| **K03** | FOWT | Wind-dominated | Wind=17.1 m/s; no waves |
| **K06** | FPV (solar array) | Floating PV | water_density=1025 kg/m³ (non-standard); Hs=0.5m |
| **L01** | Default vessel | Vessel motions | Standard environment; 100m WD |
| **L02** | Vessel (FPSO) | Wind + current | wind_speed=15; no waves |
| **L03** | Semi-sub | Current + small wave | wave_direction=135° (oblique) |
| **L04** | Semi-sub | No WD in params | wind_speed=0; Hs=5m |
| **L05** | Semi-sub | Same as L04 | Identical params to L04 |
| **M01** | Seabed pipeline | Lateral buckling / walking | Long durations (550–3200 s); no current |
| **Pipeline spanning** | Pipeline | Lay to seabed | 500s; current=wind=0 |
| **Z02** | Edge case | In-air model | water_density=0.00128 (air); validate unusual physics |

---

*Generated from audit of 62 `inputs/parameters.yml` files across 49 modular example
categories (A01–Z09). See `catalog/complete_catalog.md` for source model metadata and
download provenance.*
