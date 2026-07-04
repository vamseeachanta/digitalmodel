# Real riser-inspection fixtures (anonymized)

Anonymized excerpts from a real 2017 baseline UT inspection campaign over a deepwater
marine drilling-riser fleet (29 joints; main tube 21.25 in OD × 0.875 in nominal WT,
X-80). All operator, contractor, rig, well, personnel and project identifiers, scan
timestamps and joint serials have been removed or replaced with synthetic IDs
(`RJ-1xx`); grids carry no header metadata at all. Issue: #1293.

## Wall-thickness C-scan grids

Raw CSV, **no header/index**: rows = axial stations, columns = 64 circumferential
stations, values = absolute wall thickness in **mm**. Parse with
`GridParser.from_csv(path, input_units="mm")`.

| File | Scan | Axial pitch | Character |
|---|---|---|---|
| `ut_grid_RJ-101-boxend_ds8.csv` | full joint length, downsampled 8× axially | 32 mm | severe — the fleet's worst scan (register min life 3.5 yr, 0.25 mm/yr) |
| `ut_grid_RJ-101-boxend_worstzone_fullres.csv` | 2 m window centered on the deepest loss | 4 mm | severe, full resolution — preserves real pit morphology |
| `ut_grid_RJ-102-boxend_ds8.csv` | full length, downsampled 8× | 32 mm | moderate (register min life 8.7 yr) |
| `ut_grid_RJ-103-pinend_ds8.csv` | full length, downsampled 8× | 32 mm | mild (register min life 31.5 yr) |

Observed thickness range across the grids ≈ 15.0–25.1 mm against 22.225 mm nominal
(deepest isolated pits ≈ 32% loss). Empty cells are unmeasured stations (NaN).

## Registers

- `gml_results_register.csv` — the campaign's per-scan API 579 GML results across all
  four components (main / choke / kill / mud-boost; 142 scans): minimum life (yr),
  corrosion rate (mm/yr), measured average WT (mm), **minimum required WT** (mm,
  collapse-limit allowable — constant per component), averaging length (mm).
- `flaw_register.csv` — weld flaw-register schema with the three populated example
  rows (length/depth in mm; type = ext/emb/int; string zones).

## Provenance & anonymization gate

Source data is private and stays off-repo; the extraction script lives outside the
repository. Values are unmodified measurements (only rounded to 0.001 mm); axial
downsampling where noted. Do not add fixtures here without stripping identifiers and
running a name sweep first.
