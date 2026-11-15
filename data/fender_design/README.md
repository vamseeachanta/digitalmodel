# Fender design database

Public tables behind the fender design module
(`digitalmodel.hydrodynamics.fender_design`, with `berthing_energy`), the universal
workbook and the generic method report in `docs/reports/modules/fender_design/`.

| File | Holds |
|---|---|
| `database_schema.yaml` | Column, unit and meaning of every field of the fender database |
| `catalogue_index.csv` | One row per manufacturer catalogue read: title, edition, URL, fender types and row counts, date read. No catalogue values |
| `sources.csv` | The method source register (`fender_design.SOURCES`): document, edition, clause, verification level, where it was read |
| `method_matrix.csv` | What each method uses for velocity, Cm, Ce, Cs, Cc, design factor and fender capacity |
| `illustrative_fenders.csv` | Fictional fender rows (`GEN-*`) in the selection layout, for tests and the generic example |
| `illustrative_curves.csv` | A fictional normalised load-deflection curve (shape only) |

## Rules

- Rated fender values are manufacturers' published values. They are held in a private
  vendor table with a citation per row (catalogue, page or table, value method, date read)
  and are not committed here. `scripts/fender_design/import_vendor_tables.py --in <dir>
  --out <dir>` converts that table into `fender_database.csv` (this schema) and
  `fender_catalogue.csv` (the layout `berthing_energy.load_fender_catalogue` reads); point
  `--out` outside the repository. `catalogue_index.csv` records what was read.
- Rated columns hold only values a catalogue prints as ratings; graph reads are kept apart
  in the vendor table and are not imported as ratings.
- Rows on different test bases are not directly comparable: filter on `perf_basis` (RPD,
  CV, GEA).
- Per-metre rows (extruded and arch fenders) stay in the database but are left out of the
  selection catalogue, which compares one fender unit with the energy per fender.
- Every method value in the module names its source in `sources.csv`. Values marked
  `secondary` were read in a manufacturer's design manual or a paper that reproduces the
  standard and should be checked against the standard before use as a design basis.
- When `fender_design.SOURCES` changes, regenerate `sources.csv`; a test checks the two
  agree.
