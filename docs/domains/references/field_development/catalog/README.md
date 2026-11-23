# Field Development Case Study Catalog

A YAML-driven reference library of real field development projects for use in
NPV modelling, engineering benchmarks, and production engineering studies.

## Structure

```
catalog/
  catalog_schema.yaml       Schema definition for all field YAML files
  catalog_index.yaml        Master index with lookup tables by basin / type / operator
  README.md                 This file
  solveig-phase2.yaml       Aker BP Solveig Phase 2 (NCS subsea tieback)
  johan-sverdrup-phase1.yaml  Equinor Johan Sverdrup Phase 1 (NCS fixed platform)
  mad-dog-phase2.yaml       BP Mad Dog Phase 2 / Argos (GoM semi)
  jack-st-malo.yaml         Chevron Jack / St. Malo (GoM SPAR)
  liza-phase2.yaml          Liza Phase 2 (Guyana FPSO)
  vito.yaml                 Shell Vito (GoM semi, cost-reduction benchmark)
```

## Schema Quick Reference

Each field YAML contains:

| Field | Required | Description |
|---|---|---|
| `id` | Yes | Unique kebab-case slug |
| `name` | Yes | Human-readable project name |
| `operator` | Yes | Operating company |
| `basin` | Yes | Basin code (NCS, GoM, Guyana-Suriname, ...) |
| `country` | Yes | Host country |
| `water_depth_m` | Yes | Water depth in metres |
| `field_type` | Yes | deepwater / shallow_water / onshore |
| `development_type` | Yes | FPSO / TLP / SPAR / semi_submersible / fixed_platform / subsea_tieback |
| `production_start_year` | Yes | First production year |
| `reserves_mmboe` | Yes | Recoverable reserves in MMboe |
| `capex_bn_usd` | Yes | Development CAPEX in billion USD |
| `capex_confidence` | No | disclosed / proxy / estimate |
| `npv_assumptions` | No | Oil price, discount rate, NPV basis |
| `notes` | No | 2-5 sentence summary |
| `tags` | No | Free-form filter tags |

Full schema: `catalog_schema.yaml`.

## Using the Catalog

### Load all entries (Python)

```python
import yaml
from pathlib import Path

catalog_dir = Path("digitalmodel/docs/modules/references/field_development/catalog")

with open(catalog_dir / "catalog_index.yaml") as f:
    index = yaml.safe_load(f)

fields = {}
for entry in index["entries"]:
    with open(catalog_dir / entry["file"]) as f:
        fields[entry["id"]] = yaml.safe_load(f)
```

### Filter by basin

```python
gom_ids = index["by_basin"]["GoM"]
gom_fields = {k: v for k, v in fields.items() if k in gom_ids}
```

### Filter by development type

```python
fpso_ids = index["by_development_type"]["FPSO"]
```

## Adding a New Entry

1. Create `<field-id>.yaml` using `catalog_schema.yaml` as the template.
2. Add a summary row to `catalog_index.yaml` under `entries:`.
3. Add the id to the relevant `by_basin`, `by_development_type`, and
   `by_field_type` lookup lists.
4. Increment `total_entries`.
5. Validate: `python3 -c "import yaml; yaml.safe_load(open('<field-id>.yaml'))"`

## Data Quality Notes

- `capex_confidence: disclosed` — figure taken directly from a public operator
  filing or press release.
- `capex_confidence: proxy` — derived from comparable project benchmarks.
- `capex_confidence: estimate` — third-party analyst or industry estimate.
- `npv_bn_usd: null` — NPV not published at project level; assumptions are
  provided for modelling reference only.
- All data is sourced from public information only.

## Related Work Items

- WRK-191 — initial catalog creation
- WRK-080 — NPV blog post (uses catalog as reference)
- WRK-081 — NPV calculator defaults
- WRK-190 — NCS production data module
