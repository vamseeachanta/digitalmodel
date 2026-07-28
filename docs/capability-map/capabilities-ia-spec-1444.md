# Capabilities page — information-architecture spec (issue #1444)

> GENERATED tables (source of truth: `capabilities-sections.yml`, `capabilities-clusters.yml`, `capabilities-added.yml`). Regenerate with:
> `.venv/bin/python scripts/capabilities/build_capabilities_inventory.py`
> Presentation is owned by the capabilities revamp lane (PR #1389 coordination note) — this spec is its input. The rendered page moved to aceengineer.com/capabilities (PR #1573); the census is committed here, not scraped.

Sections in the census: **22** · clusters: **7** · PDF coverage gaps: **0** · unlinked explorers: **0**

## Cluster taxonomy

### Structures & Fitness-for-Service (`structures-ffs`)
*Strength, fatigue and remaining-life decisions for plates, panels and aging assets — field measurement to code verdict, with the validation table anchoring every engine to a published golden case.*

- [`#structural`](../api/capabilities/index.html#structural) — Ship structural strength
- [`#fatigue`](../api/capabilities/index.html#fatigue) — Fatigue & fracture — S-N life and crack growth
- [`#ffs`](../api/capabilities/index.html#ffs) — Fitness-for-service
- [`#cathodic`](../api/capabilities/index.html#cathodic) — Cathodic protection
- [`#validation`](../api/capabilities/index.html#validation) — Validated against published references

### Pipelines & Risers (`pipelines-risers`)
*Wall sizing, code checks and vortex-induced-vibration screening for flowlines, pipelines and riser systems across 10+ design codes.*

- [`#risers`](../api/capabilities/index.html#risers) — Risers & pipelines
- [`#wall-thickness`](../api/capabilities/index.html#wall-thickness) — Wall thickness — sizing & code checks
- [`#viv`](../api/capabilities/index.html#viv) — Vortex-induced vibration — screening, frequency & fatigue

### Moorings, Anchors & Subsea (`moorings-stationkeeping`)
*Stationkeeping design and subsea hardware — mooring strength and fatigue, anchor holding capacity, and foundation geotechnics.*

- [`#subsea`](../api/capabilities/index.html#subsea) — Subsea
- [`#geotechnical`](../api/capabilities/index.html#geotechnical) — Geotechnical — pile, anchor & foundation capacity

### Hydrodynamics & Naval Architecture (`hydro-naval`)
*Vessel and floating-body behaviour — diffraction, seakeeping, CFD, manoeuvring and ship-form performance from hull lines to RAOs.*

- [`#hydro`](../api/capabilities/index.html#hydro) — Hydrodynamics & diffraction
- [`#cfd`](../api/capabilities/index.html#cfd) — Computational fluid dynamics (CFD)
- [`#manoeuvring`](../api/capabilities/index.html#manoeuvring) — Manoeuvring & station-keeping
- [`#naval-architecture`](../api/capabilities/index.html#naval-architecture) — Naval architecture — stability, resistance & hull strength

### Wells, Drilling & Production (`wells-drilling-production`)
*The well axis end-to-end — casing and drilling engineering, pore pressure, artificial lift, production chemistry and flow assurance.*

- [`#well`](../api/capabilities/index.html#well) — Well construction — casing & tubulars
- [`#drilling-engineering`](../api/capabilities/index.html#drilling-engineering) — Drilling engineering — pore pressure, hydraulics & well control
- [`#artificial-lift`](../api/capabilities/index.html#artificial-lift) — Artificial lift — rod-pump diagnostics
- [`#production-engineering`](../api/capabilities/index.html#production-engineering) — Production engineering — nodal analysis & well deliverability
- [`#corrosion-production`](../api/capabilities/index.html#corrosion-production) — Corrosion & production chemistry

### Field Development & Economics (`field-dev-economics`)
*Concept-to-cashflow screening — field development options, floating wind TOTEX/LCOE and NPV levers over real project structures.*

- [`#field-development`](../api/capabilities/index.html#field-development) — Field development — concept screening, cost & economics
- [`#wind`](../api/capabilities/index.html#wind) — Floating wind

### Installation & Marine Operations (`installation-ops`)
*Getting hardware to the seabed safely — installability screening, lifting/lowering dynamics and weather-windowed marine operations.*

- [`#installation`](../api/capabilities/index.html#installation) — Installation

## Reference index (citable front doors)

| Section | Cluster | Live explorer(s) | 1-pager PDF | Added |
|---|---|---|---|---|
| [`#ffs`](../api/capabilities/index.html#ffs) | structures-ffs | `docs/api/ffs/riser-joint-acceptance-explorer.html` | `docs/api/capabilities/pdf/sec-ffs.pdf` | unknown |
| [`#structural`](../api/capabilities/index.html#structural) | structures-ffs | *gap* | `docs/api/capabilities/pdf/sec-structural.pdf` | unknown |
| [`#fatigue`](../api/capabilities/index.html#fatigue) | structures-ffs | *gap* | `docs/api/capabilities/pdf/sec-fatigue.pdf` | 2026-07-04 (#1396) |
| [`#hydro`](../api/capabilities/index.html#hydro) | hydro-naval | `docs/api/hydro/ocimf-coefficient-explorer.html` | `docs/api/capabilities/pdf/sec-hydro.pdf` | unknown |
| [`#cfd`](../api/capabilities/index.html#cfd) | hydro-naval | `docs/api/structural/sloshing-explorer.html` | `docs/api/capabilities/pdf/sec-cfd.pdf` | 2026-07-06 (#1442) |
| [`#risers`](../api/capabilities/index.html#risers) | pipelines-risers | `docs/api/drilling/drilling-riser-operability-explorer.html` | `docs/api/capabilities/pdf/sec-risers.pdf` | unknown |
| [`#wall-thickness`](../api/capabilities/index.html#wall-thickness) | pipelines-risers | `docs/api/structural/wall-thickness-explorer.html` | `docs/api/capabilities/pdf/sec-wall-thickness.pdf` | 2026-07-04 (#1389) |
| [`#subsea`](../api/capabilities/index.html#subsea) | moorings-stationkeeping | *gap* | `docs/api/capabilities/pdf/sec-subsea.pdf` | unknown |
| [`#viv`](../api/capabilities/index.html#viv) | pipelines-risers | `docs/api/structural/viv-explorer.html` | `docs/api/capabilities/pdf/sec-viv.pdf` | 2026-07-04 (#1396) |
| [`#installation`](../api/capabilities/index.html#installation) | installation-ops | *gap* | `docs/api/capabilities/pdf/sec-installation.pdf` | unknown |
| [`#wind`](../api/capabilities/index.html#wind) | field-dev-economics | *gap* | `docs/api/capabilities/pdf/sec-wind.pdf` | unknown |
| [`#field-development`](../api/capabilities/index.html#field-development) | field-dev-economics | `docs/api/structural/field-economics-explorer.html` | `docs/api/capabilities/pdf/sec-field-development.pdf` | 2026-07-04 (#1396) |
| [`#manoeuvring`](../api/capabilities/index.html#manoeuvring) | hydro-naval | `docs/api/hydro/rudder-maneuvering-explorer.html` | `docs/api/capabilities/pdf/sec-manoeuvring.pdf` | unknown |
| [`#naval-architecture`](../api/capabilities/index.html#naval-architecture) | hydro-naval | `docs/api/structural/ship-resistance-explorer.html` | `docs/api/capabilities/pdf/sec-naval-architecture.pdf` | 2026-07-04 (#1394) |
| [`#geotechnical`](../api/capabilities/index.html#geotechnical) | moorings-stationkeeping | `docs/api/structural/anchor-holding-explorer.html` | `docs/api/capabilities/pdf/sec-geotechnical.pdf` | 2026-07-04 (#1394) |
| [`#artificial-lift`](../api/capabilities/index.html#artificial-lift) | wells-drilling-production | *gap* | `docs/api/capabilities/pdf/sec-artificial-lift.pdf` | unknown |
| [`#production-engineering`](../api/capabilities/index.html#production-engineering) | wells-drilling-production | `docs/api/structural/ipr-explorer.html` | `docs/api/capabilities/pdf/sec-production-engineering.pdf` | 2026-07-04 (#1396) |
| [`#well`](../api/capabilities/index.html#well) | wells-drilling-production | `docs/api/well/casing-design-explorer.html` | `docs/api/capabilities/pdf/sec-well.pdf` | unknown |
| [`#drilling-engineering`](../api/capabilities/index.html#drilling-engineering) | wells-drilling-production | `docs/api/structural/pore-pressure-explorer.html` | `docs/api/capabilities/pdf/sec-drilling-engineering.pdf` | 2026-07-04 (#1396) |
| [`#cathodic`](../api/capabilities/index.html#cathodic) | structures-ffs | `docs/api/structural/cathodic-protection-explorer.html` | `docs/api/capabilities/pdf/sec-cathodic.pdf` | 2026-07-04 (#1389) |
| [`#corrosion-production`](../api/capabilities/index.html#corrosion-production) | wells-drilling-production | `docs/api/corrosion/galvanic-compatibility-explorer.html`<br>`docs/api/production/scale-si-explorer.html` | `docs/api/capabilities/pdf/sec-corrosion-production.pdf` | unknown |
| [`#validation`](../api/capabilities/index.html#validation) | structures-ffs | *gap* | `docs/api/capabilities/pdf/sec-validation.pdf` | unknown |

**PDF gap set (0):** 

**Unlinked explorers:** none

## Recently added (strip content model)

Display contract: top-N below (N from `capabilities-added.yml:recent_n`), newest first; entries without PR evidence stay off the strip (honest `unknown`, never a fabricated date — repo history was truncated by the 2026-07 git slim, so recency is explicit metadata).

- `#cfd` — 2026-07-06 (PR #1442)
- `#fatigue` — 2026-07-04 (PR #1396)
- `#wall-thickness` — 2026-07-04 (PR #1389)
- `#viv` — 2026-07-04 (PR #1396)
- `#field-development` — 2026-07-04 (PR #1396)
- `#naval-architecture` — 2026-07-04 (PR #1394)
- `#geotechnical` — 2026-07-04 (PR #1394)
- `#production-engineering` — 2026-07-04 (PR #1396)

## Anchor-stability contract

The revamp MUST preserve every anchor below (external links already cite them). Enforcement pattern: route manifest + link-graph CI gate (see worldenergydata #850).

```
ffs structural fatigue hydro cfd risers wall-thickness subsea viv installation wind field-development manoeuvring naval-architecture geotechnical artificial-lift production-engineering well drilling-engineering cathodic corrosion-production validation
```
