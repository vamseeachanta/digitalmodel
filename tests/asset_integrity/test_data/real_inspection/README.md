# Real riser-inspection fixtures (anonymized)

Anonymized excerpts from a real ultrasonic (UT) wall-thickness inspection of steel
riser sections: about 540 mm [21 in] outside diameter by about 22 mm [0.9 in]
nominal wall, both rounded here, grade X80. The exact nominal dimensions used by
the calculations are set in the code that reads these files. The asset, its owner,
its location, the inspection date and the inspection company are not named. All
operator, contractor, rig, well, personnel and project identifiers, scan
timestamps and serials have been removed or replaced with synthetic IDs; the grids
carry no header metadata at all, and their file names carry no source identifier.

## Wall-thickness C-scan grids

Raw CSV, **no header/index**: rows = axial stations, columns = 64 circumferential
stations, values = absolute wall thickness in **mm**. Parse with
`GridParser.from_csv(path, input_units="mm")`.

| File | Scan | Axial pitch | Character |
|---|---|---|---|
| `ut_grid_scan-a_ds8.csv` | full scan length, downsampled 8x axially | 32 mm | severe: the shortest register life of the excerpts |
| `ut_grid_scan-a_window_fullres.csv` | window of about 2 m (rounded) centred on the deepest loss of scan A | 4 mm | severe, full resolution: keeps the measured pit morphology |
| `ut_grid_scan-b_ds8.csv` | full scan length, downsampled 8x | 32 mm | moderate |
| `ut_grid_scan-c_ds8.csv` | full scan length, downsampled 8x | 32 mm | mild |

Readings run from about 15 mm to about 25 mm, rounded, against the nominal wall;
the deepest isolated pits are about 32 % loss. Empty cells are unmeasured
stations (NaN).

## Registers

- `gml_results_register.csv`: per-scan API 579 general metal loss results from the
  same inspection, one row per scan, keyed by synthetic joint ID (J01 to J28),
  component (Line A is the main tube; Lines B, C and D are auxiliary lines) and
  scan location (End 1 and End 2 are the two ends of a joint): minimum life
  (yr), corrosion rate (mm/yr), measured average wall (mm), **minimum required
  wall** (mm, collapse-limit allowable, constant per component) and averaging
  length (mm).
- `flaw_register.csv`: weld flaw-register schema with three populated example
  rows (length and depth in mm; type = ext/emb/int; string zones).

## Provenance and anonymization gate

Source data is private and stays off-repo; the extraction script lives outside the
repository. Values are unmodified measurements (only rounded to 0.001 mm); axial
downsampling where noted. Do not add fixtures here without stripping identifiers and
running a name sweep first.
