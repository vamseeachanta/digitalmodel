# Data systems

Data acquisition, catalogue validation and migration utilities.

## Engineering catalogues

The [engineering databases plan-lite](../../../docs/plans/engineering-databases-plan-lite.md)
defines the fields, units, citation requirements and population batches for four
catalogues. Production files are empty; test values are invented fixtures.

| Catalogue | Directory | Loader in `engineering_databases` |
| --- | --- | --- |
| Line pipe | `data/line_pipe` | `load_line_pipe(directory)` |
| Flexible pipe | `data/flexible_pipe` | `load_flexible_pipe(directory)` |
| Structure properties | `data/riser_database/structure_properties` | `load_structure_properties(directory)` |
| Soil stiffness | `data/soil_stiffness` | `load_soil_stiffness(directory)` |

Table 1. Catalogue entry points; directories are repository-relative.

Each loader checks its JSON schema, exact unit labels, row citations and
engineering relationships. Synthetic records require `allow_synthetic=True`.
Validation establishes consistency; source rights and engineering suitability
require independent review. Data directories must be supplied explicitly and are
not bundled into the installed wheel.

The [migration utilities](data/README.md) cover existing file conversion tools.
