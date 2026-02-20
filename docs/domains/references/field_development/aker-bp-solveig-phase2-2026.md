# Aker BP — Solveig Phase 2 Field Development

> Reference: subsea tieback architecture, North Sea, 2026

## Source

OGJ, 2 February 2026 — "Aker BP begins oil production at Solvieg in the North Sea"
URL: https://www.ogj.com/drilling-production/production-operations/news/55354630/aker-bp-begins-oil-production-at-solvieg-in-the-north-sea

Visual: `aker-bp-solveig-phase2-2026.avif` (field development schematic / aerial)

## Key Facts

| Parameter          | Value                                      |
|--------------------|--------------------------------------------|
| Field              | Solveig Phase 2                            |
| Location           | North Sea, ~15 km south of Edvard Grieg    |
| Operator           | Aker BP ASA (65%)                          |
| Partners           | OMV Norge AS (20%), Harbour Energy (15%)   |
| Production start   | 2 February 2026                            |
| Recoverable resources | ~39 MMboe                               |
| Development type   | Subsea tieback to Edvard Grieg platform    |
| Wells              | 3 (new and existing reservoir segments)    |

## Key Contractors

- **TechnipFMC** — subsea systems
- **Moreld Apply** — platform modifications
- **Odfjell Drilling / Halliburton** — drilling operations

## Field Development Notes

- Fifth Aker BP-operated project sanctioned in 2022 to reach production
- Subsea tieback leverages existing Edvard Grieg infrastructure (capital-efficient)
- Designed to extend plateau production using available platform processing capacity
- Targets both new and existing reservoir segments — hybrid exploration/development approach

## Why This Example Matters (WRK-080 / visual library)

- Real-world subsea tieback with publicly disclosed CAPEX proxy (5 wells, tieback ~15 km)
- 39 MMboe recoverable → good range for NPV sensitivity illustrations
- Aker BP publishes detailed project economics; useful benchmark for deepwater NPV blog post
- The field schematic image shows:
  - Subsea template / manifold layout
  - Flowline routing to host platform
  - Well slot arrangement
  - Surface facility (Edvard Grieg semi)
- Visual style reference for field development diagrams targeted at the aceengineer blog

## Data Requirement — NCS Production Data (equivalent to BSEE)

We need field-level production data for Solveig and similar NCS fields, equivalent to what BSEE provides for GoM.

**Source**: Norwegian Petroleum Directorate (NPD) / Sodir open data portal
- Factpages API: `factpages.npd.no` — field production, well, discovery, facility data
- Monthly production per field (oil, gas, NGL, condensate, water injection)
- Well completion, core, and test data
- All open-access, no authentication required (equivalent access level to BSEE eWell/production)

**Fields of interest** (Edvard Grieg area and comparable subsea tiebacks):
- Solveig (Aker BP operated, tieback to Edvard Grieg)
- Edvard Grieg (host platform, Aker BP)
- Ivar Aasen (nearby, Aker BP)
- Ula / Tambar (older NCS tiebacks, good decline curve data)
- Johan Sverdrup (Equinor, largest NCS field — benchmark for platform-based economics)

**Tracked by**: WRK-190 — NCS production data module in worldenergydata

## Related WRK

- **WRK-080** Phase 2: "NPV Analysis for Deepwater Field Development with Python" — use Solveig as the case study backbone (39 MMboe, tieback CAPEX, plateau extension economics)
- **WRK-081**: NPV calculator — Solveig parameters make a realistic default scenario
- **WRK-164**: Production engineering — subsea well test data quality context (3-well tieback with Edvard Grieg test facilities)
- **WRK-190**: NCS production data module — acquire NPD/Sodir data for Solveig and peer fields
