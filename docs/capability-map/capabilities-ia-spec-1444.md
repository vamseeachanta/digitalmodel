# Capabilities page — information-architecture spec (issue #1444)

> GENERATED tables (source of truth: `capabilities-sections.yml`, `capabilities-clusters.yml`, `capabilities-added.yml`). Regenerate with:
> `.venv/bin/python scripts/capabilities/build_capabilities_inventory.py`
> Presentation lives at [https://www.aceengineer.com/capabilities/](https://www.aceengineer.com/capabilities/) (C10, #1573) — this spec describes the IA, and does not edit the rendered surface.

Sections declared: **22** · clusters: **7** · PDF coverage gaps: **0** · unlinked explorers: **1**

## Cluster taxonomy

### Structures & Fitness-for-Service (`structures-ffs`)
*Strength, fatigue and remaining-life decisions for plates, panels and aging assets — field measurement to code verdict, with the validation table anchoring every engine to a published golden case.*

- [`#structural`](https://www.aceengineer.com/capabilities/#structural) — Ship structural strength
- [`#fatigue`](https://www.aceengineer.com/capabilities/#fatigue) — Fatigue & fracture — S-N life and crack growth
- [`#ffs`](https://www.aceengineer.com/capabilities/#ffs) — Fitness-for-service
- [`#cathodic`](https://www.aceengineer.com/capabilities/#cathodic) — Cathodic protection
- [`#validation`](https://www.aceengineer.com/capabilities/#validation) — Validated against published references

### Pipelines & Risers (`pipelines-risers`)
*Wall sizing, code checks and vortex-induced-vibration screening for flowlines, pipelines and riser systems across 10+ design codes.*

- [`#risers`](https://www.aceengineer.com/capabilities/#risers) — Risers & pipelines
- [`#wall-thickness`](https://www.aceengineer.com/capabilities/#wall-thickness) — Wall thickness — sizing & code checks
- [`#viv`](https://www.aceengineer.com/capabilities/#viv) — Vortex-induced vibration — screening, frequency & fatigue

### Moorings, Anchors & Subsea (`moorings-stationkeeping`)
*Stationkeeping design and subsea hardware — mooring strength and fatigue, anchor holding capacity, and foundation geotechnics.*

- [`#subsea`](https://www.aceengineer.com/capabilities/#subsea) — Subsea
- [`#geotechnical`](https://www.aceengineer.com/capabilities/#geotechnical) — Geotechnical — pile, anchor & foundation capacity

### Hydrodynamics & Naval Architecture (`hydro-naval`)
*Vessel and floating-body behaviour — diffraction, seakeeping, CFD, manoeuvring and ship-form performance from hull lines to RAOs.*

- [`#hydro`](https://www.aceengineer.com/capabilities/#hydro) — Hydrodynamics & diffraction
- [`#cfd`](https://www.aceengineer.com/capabilities/#cfd) — Computational fluid dynamics (CFD)
- [`#manoeuvring`](https://www.aceengineer.com/capabilities/#manoeuvring) — Manoeuvring & station-keeping
- [`#naval-architecture`](https://www.aceengineer.com/capabilities/#naval-architecture) — Naval architecture — stability, resistance & hull strength

### Wells, Drilling & Production (`wells-drilling-production`)
*The well axis end-to-end — casing and drilling engineering, pore pressure, artificial lift, production chemistry and flow assurance.*

- [`#well`](https://www.aceengineer.com/capabilities/#well) — Well construction — casing & tubulars
- [`#drilling-engineering`](https://www.aceengineer.com/capabilities/#drilling-engineering) — Drilling engineering — pore pressure, hydraulics & well control
- [`#artificial-lift`](https://www.aceengineer.com/capabilities/#artificial-lift) — Artificial lift — rod-pump diagnostics
- [`#production-engineering`](https://www.aceengineer.com/capabilities/#production-engineering) — Production engineering — nodal analysis & well deliverability
- [`#corrosion-production`](https://www.aceengineer.com/capabilities/#corrosion-production) — Corrosion & production chemistry

### Field Development & Economics (`field-dev-economics`)
*Concept-to-cashflow screening — field development options, floating wind TOTEX/LCOE and NPV levers over real project structures.*

- [`#field-development`](https://www.aceengineer.com/capabilities/#field-development) — Field development — concept screening, cost & economics
- [`#wind`](https://www.aceengineer.com/capabilities/#wind) — Floating wind

### Installation & Marine Operations (`installation-ops`)
*Getting hardware to the seabed safely — installability screening, lifting/lowering dynamics and weather-windowed marine operations.*

- [`#installation`](https://www.aceengineer.com/capabilities/#installation) — Installation

## Reference index (citable front doors)

| Section | Cluster | Live explorer(s) | 1-pager PDF | Added |
|---|---|---|---|---|
| [`#ffs`](https://www.aceengineer.com/capabilities/#ffs) | structures-ffs | `docs/api/ffs/riser-joint-acceptance-explorer.html` | `docs/api/capabilities/pdf/sec-ffs.pdf` | unknown |
| [`#structural`](https://www.aceengineer.com/capabilities/#structural) | structures-ffs | *gap* | `docs/api/capabilities/pdf/sec-structural.pdf` | unknown |
| [`#fatigue`](https://www.aceengineer.com/capabilities/#fatigue) | structures-ffs | *gap* | `docs/api/capabilities/pdf/sec-fatigue.pdf` | 2026-07-04 (#1396) |
| [`#hydro`](https://www.aceengineer.com/capabilities/#hydro) | hydro-naval | `docs/api/hydro/ocimf-coefficient-explorer.html` | `docs/api/capabilities/pdf/sec-hydro.pdf` | unknown |
| [`#cfd`](https://www.aceengineer.com/capabilities/#cfd) | hydro-naval | `docs/api/structural/sloshing-explorer.html` | `docs/api/capabilities/pdf/sec-cfd.pdf` | 2026-07-06 (#1442) |
| [`#risers`](https://www.aceengineer.com/capabilities/#risers) | pipelines-risers | `docs/api/drilling/drilling-riser-operability-explorer.html` | `docs/api/capabilities/pdf/sec-risers.pdf` | unknown |
| [`#wall-thickness`](https://www.aceengineer.com/capabilities/#wall-thickness) | pipelines-risers | `docs/api/structural/wall-thickness-explorer.html` | `docs/api/capabilities/pdf/sec-wall-thickness.pdf` | 2026-07-04 (#1389) |
| [`#subsea`](https://www.aceengineer.com/capabilities/#subsea) | moorings-stationkeeping | *gap* | `docs/api/capabilities/pdf/sec-subsea.pdf` | unknown |
| [`#viv`](https://www.aceengineer.com/capabilities/#viv) | pipelines-risers | `docs/api/structural/viv-explorer.html` | `docs/api/capabilities/pdf/sec-viv.pdf` | 2026-07-04 (#1396) |
| [`#installation`](https://www.aceengineer.com/capabilities/#installation) | installation-ops | *gap* | `docs/api/capabilities/pdf/sec-installation.pdf` | unknown |
| [`#wind`](https://www.aceengineer.com/capabilities/#wind) | field-dev-economics | *gap* | `docs/api/capabilities/pdf/sec-wind.pdf` | unknown |
| [`#field-development`](https://www.aceengineer.com/capabilities/#field-development) | field-dev-economics | `docs/api/structural/field-economics-explorer.html` | `docs/api/capabilities/pdf/sec-field-development.pdf` | 2026-07-04 (#1396) |
| [`#manoeuvring`](https://www.aceengineer.com/capabilities/#manoeuvring) | hydro-naval | `docs/api/hydro/rudder-maneuvering-explorer.html` | `docs/api/capabilities/pdf/sec-manoeuvring.pdf` | unknown |
| [`#naval-architecture`](https://www.aceengineer.com/capabilities/#naval-architecture) | hydro-naval | `docs/api/structural/ship-resistance-explorer.html` | `docs/api/capabilities/pdf/sec-naval-architecture.pdf` | 2026-07-04 (#1394) |
| [`#geotechnical`](https://www.aceengineer.com/capabilities/#geotechnical) | moorings-stationkeeping | `docs/api/structural/anchor-holding-explorer.html` | `docs/api/capabilities/pdf/sec-geotechnical.pdf` | 2026-07-04 (#1394) |
| [`#artificial-lift`](https://www.aceengineer.com/capabilities/#artificial-lift) | wells-drilling-production | *gap* | `docs/api/capabilities/pdf/sec-artificial-lift.pdf` | unknown |
| [`#production-engineering`](https://www.aceengineer.com/capabilities/#production-engineering) | wells-drilling-production | `docs/api/structural/ipr-explorer.html` | `docs/api/capabilities/pdf/sec-production-engineering.pdf` | 2026-07-04 (#1396) |
| [`#well`](https://www.aceengineer.com/capabilities/#well) | wells-drilling-production | `docs/api/well/casing-design-explorer.html` | `docs/api/capabilities/pdf/sec-well.pdf` | unknown |
| [`#drilling-engineering`](https://www.aceengineer.com/capabilities/#drilling-engineering) | wells-drilling-production | `docs/api/structural/pore-pressure-explorer.html` | `docs/api/capabilities/pdf/sec-drilling-engineering.pdf` | 2026-07-04 (#1396) |
| [`#cathodic`](https://www.aceengineer.com/capabilities/#cathodic) | structures-ffs | `docs/api/structural/cathodic-protection-explorer.html` | `docs/api/capabilities/pdf/sec-cathodic.pdf` | 2026-07-04 (#1389) |
| [`#corrosion-production`](https://www.aceengineer.com/capabilities/#corrosion-production) | wells-drilling-production | `docs/api/corrosion/galvanic-compatibility-explorer.html`<br>`docs/api/production/scale-si-explorer.html` | `docs/api/capabilities/pdf/sec-corrosion-production.pdf` | unknown |
| [`#validation`](https://www.aceengineer.com/capabilities/#validation) | structures-ffs | *gap* | `docs/api/capabilities/pdf/sec-validation.pdf` | unknown |

**PDF gap set (0):** 

**Unlinked explorers:** `docs/api/structural/wall-thickness-3d-explorer.html`

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
