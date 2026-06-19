# Vessel Database

Public-sourced vessel data to feed **diffraction analysis** (OrcaWave/AQWA) and
**installation-envelope / operability analysis** (`marine_ops/installation`).

Supersedes the never-executed Phase-1 outputs proposed in
`docs/domains/data/DATA_PROCUREMENT_STRATEGY.md`.

## Hard rule — flag, don't fake

These are engineering inputs. **Never fabricate a numeric value.** If a field
is not in a citable public source, mark it:

- `gap` — not found in any public source
- `estimated:<basis>` — derived from a documented empirical relation (e.g.
  `estimated:kxx=0.34*beam`), with the relation named in `provenance`
- a real value **with** a `citation` (source name + URL + access date)

A record with an un-cited hard number is a defect.

## Layout

```
data/vessels/
  raw/           # one file per (scope, layer, source) as collected, with metadata sidecar
  processed/     # curated, schema-conformant, deduped records ready for analysis
  sources/       # per-scope source catalogs (what's public, where, paywalled?)
  SCHEMA.yaml    # the record schema (single source of truth for fields)
  README.md      # this file
```

## Scopes

| Scope | Covers | Primary analysis driver |
|---|---|---|
| `install` | HLV / crane vessels / derrick barges / pipelay (S/J/reel) / OCV | Installation envelope (crane curves, deck, motions) |
| `floating` | FPSO / FPU / semisub / spar / drillship / FSRU | Diffraction → RAOs (moored floaters) |
| `support` | AHTS / PSV / CTV / SOV·W2W + metocean criteria | Operability limits |
| `tanker` | VLCC / Suezmax / Aframax / LNGC / LPGC | Extends OCIMF coefficient corpus |

## Data layers (per vessel record)

1. **Particulars** — LOA, LBP, beam, depth, draft(s), displacement, block coeff.
2. **Mass properties** — LCG, VCG, TCG, radii of gyration kxx/kyy/kzz, loading conditions.
   *(gyradii almost never public → `estimated:` with named relation, or `gap`)*
3. **RAOs** — published model-test or computed motion RAOs; benchmark hulls.
   *(scarce for real named vessels → prefer published benchmark datasets + cite)*
4. **Crane & deck** — crane SWL-vs-radius curves, boom/tip height, deck area/strength.
   *(install scope; HLV brochures publish these)*
5. **Wind/current coefficients** — OCIMF extension (tanker/gas scope).

## Provenance contract

Every record carries `citations[]` binding each non-trivial field to a source.
See `SCHEMA.yaml`. Mirrors the calc-citation discipline used elsewhere in the repo.

## Consuming the data (`marine_ops.vessel_db`)

`raw/*.json` is the curated source of truth. Promote it to analysis-ready CSVs:

```
uv run python -m digitalmodel.marine_ops.vessel_db.promote
```

writes under `processed/`:

| File | Use |
|---|---|
| `particulars.csv` | principal dims + mass props; **radii of gyration filled by documented estimate** (`kxx_basis`/`kyy_basis`/`kzz_basis` say `cited` or `estimated:<relation>`) — diffraction mass setup |
| `crane_summary.csv` | installation crane SWL anchors + deck data |
| `rao_datasets.csv` | index of RAO proxy/benchmark datasets |
| `metocean_criteria.csv` | regional metocean design criteria |
| `PROVENANCE_REPORT.md` | provenance integrity check + gyradii estimation log |

Programmatic access:

```python
from digitalmodel.marine_ops.vessel_db import (
    iter_records, load_crane_curves, validate_provenance, estimate_gyradii,
)

assert validate_provenance() == []          # no un-cited hard numbers
curves = load_crane_curves()                # {name: installation.CraneCurve}
curves["SSCV Sleipnir"].capacity_at_radius(48.0)
```

`load_crane_curves()` returns `marine_ops.installation.CraneCurve` objects ready
for `crane_tip_motion` / `go_no_go`. Gyradii estimation relations live in
`gyradii.py` (roll `0.35·B`, pitch `0.25·Lpp`, yaw `0.26·Lpp`; non-ship forms
carry a caveat).
