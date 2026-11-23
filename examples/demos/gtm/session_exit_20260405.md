# GTM Feature Work — Session Exit Document
## Date: 2026-04-05

## What Was Done

### 1. Full GTM Issue Review
Reviewed all 18 open GTM-labeled issues and 10 domain:gtm issues across workspace-hub.

### 2. Demo Prioritization
Analyzed readiness, impact, and engineering risk for all 4 remaining demos.
Established build order: **Demo 3 -> Demo 5 -> Demo 1 -> Demo 4**

### 3. Design Decisions Captured (user-confirmed)
| Decision | Choice |
|----------|--------|
| Splash zone method | Simplified screening (OrcaFlex governs) |
| Cable weight | From existing vessel catalogs/models |
| Go/No-Go thresholds | Per DNV code utilisation factors |
| Hero chart layout | Two side-by-side heatmaps |
| Seabed bearing | Soft clay default + sensitivity chart |
| DAF | Applied where needed; OrcaFlex governs |
| Reference sea state | Hs sweep (1.0, 1.5, 2.0, 2.5, 3.0m) |

### 4. GitHub Issues Updated
- **#1800** (umbrella) — updated with build status table, design decisions, build order
- **#1871** (Demo 1 Freespan) — updated with existing free_span module reference
- **#1872** (Demo 3 Mudmat) — updated with refined requirements, 180 cases (Hs sweep), 6 charts
- **#1873** (Demo 4 Pipelay) — updated with catenary solver status
- **#1874** (Demo 5 Jumper) — updated with Demo 3 code reuse plan

### 5. New GitHub Issues Created
- **#1904** — INVENTORY: Complete OrcaFlex/OrcaWave model and Excel workbook catalog
- **#1905** — DATA: Rigid jumper OrcaFlex model + input workbook for model generation

### 6. Model Inventory Audit (key finding)

**workspace-hub has 3,524 OrcaFlex YML models:**
- 1,043 mooring | 765 examples | 701 library | 532 installation
- 119 risers | 106 pipeline | 88 templates | 83 jumper

**/mnt/ace has the legacy project data:**
- 3,697 domain Excel workbooks (238 jumper-specific)
- 18 OrcaFlex-related Excel workbooks
- 9 .sim binary models
- Key jumper project: 3824 containment riser (extensive stackup calcs)

**Gap identified**: No clean rigid jumper input workbook exists for model generation.
The 238 Excel files are project-specific post-processing, not model-building inputs.
Issue #1905 tracks creating one.

## Requirements Doc
Written to: `digitalmodel/examples/demos/gtm/demo_03_requirements.md`

## Next Steps (for next session)
1. Build Demo 3 (Mudmat Installation) — all requirements defined, #1872
2. The OrcaFlex/OrcaWave inventory (#1904) is a Gemini/research task
3. Jumper input workbook (#1905) needs Vamsee's domain input on which 3824 Excel to use as template
