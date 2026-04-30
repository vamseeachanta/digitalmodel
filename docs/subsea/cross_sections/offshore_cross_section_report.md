# Offshore Cable, Umbilical, and Pipeline Cross-Section Report

Generated deterministically from the bundled `digitalmodel.subsea.cross_sections` YAML fixtures. No timestamps, host paths, or derived mechanical capacities are emitted.

## Regeneration

```bash
PYTHONPATH=src uv run python -m digitalmodel.subsea.cross_sections.cli --output-dir docs/subsea/cross_sections --format all
```

## Fixture Inventory

| Fixture ID | Name | Family | Duty |
|---|---|---|---|
| `66kv-inter-array-cable` | Representative 66 kV inter-array cable cross-section | `offshore_wind_inter_array_cable` | `static` |
| `220kv-hvac-export-cable` | Representative 220 kV HVAC export cable cross-section | `offshore_wind_hvac_export_cable` | `static` |
| `power-optical-hybrid-umbilical` | Representative power and optical hybrid umbilical | `power_optical_hybrid_umbilical` | `static` |
| `concrete-coated-pipeline` | Representative concrete-coated rigid pipeline | `rigid_pipeline_flowline` | `static` |
| `steel-tube-electro-hydraulic-umbilical` | Representative steel-tube electro-hydraulic umbilical | `steel_tube_electro_hydraulic_umbilical` | `dynamic` |

## Cross-Family Comparison

| Fixture ID | Name | Family | Duty | Overall OD / envelope | Radial layer count | Packed component count | Key ratings | Primary source IDs | Caveat tags |
|---|---|---|---|---|---:|---:|---|---|---|
| `66kv-inter-array-cable` | Representative 66 kV inter-array cable cross-section | `offshore_wind_inter_array_cable` | `static` | 130 mm | 4 | 0 | voltage: 66 kV | `wiki-cross-section-recon-2026-04-26`, `project-assumption-2514`, `prysmian-66kv-example`, `guide-floating-offshore-wind-66kv` | `cable-duty`, `flexible-pipe-deferred` |
| `220kv-hvac-export-cable` | Representative 220 kV HVAC export cable cross-section | `offshore_wind_hvac_export_cable` | `static` | 205 mm | 4 | 0 | voltage: 220 kV | `wiki-cross-section-recon-2026-04-26`, `project-assumption-2514`, `guide-floating-offshore-wind-220kv` | `cable-duty`, `flexible-pipe-deferred` |
| `power-optical-hybrid-umbilical` | Representative power and optical hybrid umbilical | `power_optical_hybrid_umbilical` | `static` | component max 24 mm (bundle envelope not specified) | 0 | 2 | voltage: 33 kV; medium voltage power cores voltage: 33 kV | `wiki-cross-section-recon-2026-04-26`, `project-assumption-2514`, `prysmian-power-optical-umbilical` | `umbilical-schematic`, `flexible-pipe-deferred` |
| `concrete-coated-pipeline` | Representative concrete-coated rigid pipeline | `rigid_pipeline_flowline` | `static` | 430 mm | 4 | 0 | pressure: 15 MPa | `wiki-cross-section-recon-2026-04-26`, `project-assumption-2514`, `dnv-st-f101-registry`, `vallourec-coating-reference`, `octal-concrete-weight-coating-reference` | `pipeline-route-coating`, `flexible-pipe-deferred` |
| `steel-tube-electro-hydraulic-umbilical` | Representative steel-tube electro-hydraulic umbilical | `steel_tube_electro_hydraulic_umbilical` | `dynamic` | component max 18 mm (bundle envelope not specified) | 0 | 3 | pressure: 345 bar; hydraulic steel tube bundle pressure: 345 bar; electrical power quad voltage: 3.3 kV | `wiki-cross-section-recon-2026-04-26`, `project-assumption-2514`, `sut-umbilical-reference` | `umbilical-schematic`, `flexible-pipe-deferred` |

## Cross-Section Visuals

### Representative 66 kV inter-array cable cross-section (`66kv-inter-array-cable`)

<svg id="visual-66kv-inter-array-cable" role="img" aria-label="radial layer annulus schematic" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 240"><rect width="520" height="240" fill="white"/><circle cx="120" cy="120" r="48.00" fill="#e45756" stroke="#243447" stroke-width="1"><title>outer jacket</title></circle><circle cx="120" cy="120" r="42.46" fill="#54a24b" stroke="#243447" stroke-width="1"><title>metallic screen and armour</title></circle><circle cx="120" cy="120" r="27.69" fill="#f58518" stroke="#243447" stroke-width="1"><title>xlpe insulation</title></circle><circle cx="120" cy="120" r="12.92" fill="#4c78a8" stroke="#243447" stroke-width="1"><title>conductor core</title></circle><text x="250" y="20" font-size="10"><tspan font-weight="700">1.</tspan> conductor core (copper)</text><text x="250" y="34" font-size="10"><tspan font-weight="700">2.</tspan> xlpe insulation (XLPE)</text><text x="250" y="48" font-size="10"><tspan font-weight="700">3.</tspan> metallic screen and armour (steel wire)</text><text x="250" y="62" font-size="10"><tspan font-weight="700">4.</tspan> outer jacket (polyethylene)</text></svg>

### Representative 220 kV HVAC export cable cross-section (`220kv-hvac-export-cable`)

<svg id="visual-220kv-hvac-export-cable" role="img" aria-label="radial layer annulus schematic" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 240"><rect width="520" height="240" fill="white"/><circle cx="120" cy="120" r="48.00" fill="#e45756" stroke="#243447" stroke-width="1"><title>armour and jacket</title></circle><circle cx="120" cy="120" r="36.53" fill="#54a24b" stroke="#243447" stroke-width="1"><title>metallic screen and water barrier</title></circle><circle cx="120" cy="120" r="29.97" fill="#f58518" stroke="#243447" stroke-width="1"><title>xlpe insulation</title></circle><circle cx="120" cy="120" r="13.58" fill="#4c78a8" stroke="#243447" stroke-width="1"><title>conductor core</title></circle><text x="250" y="20" font-size="10"><tspan font-weight="700">1.</tspan> conductor core (copper)</text><text x="250" y="34" font-size="10"><tspan font-weight="700">2.</tspan> xlpe insulation (XLPE)</text><text x="250" y="48" font-size="10"><tspan font-weight="700">3.</tspan> metallic screen and water barrier (lead alloy and copper wire)</text><text x="250" y="62" font-size="10"><tspan font-weight="700">4.</tspan> armour and jacket (steel armour and polyethylene)</text></svg>

### Representative power and optical hybrid umbilical (`power-optical-hybrid-umbilical`)

<svg id="visual-power-optical-hybrid-umbilical" role="img" aria-label="not-to-scale schematic packed-component bundle" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 240"><rect width="520" height="240" fill="white"/><circle cx="120" cy="120" r="92" fill="#f7f7f7" stroke="#243447" stroke-width="2"><title>not-to-scale schematic bundle envelope</title></circle><text x="32" y="224" font-size="10" font-weight="700">not-to-scale schematic</text><circle cx="120.00" cy="58.00" r="7.00" fill="#4c78a8" stroke="#243447" stroke-width="1"><title>optical fibre package; count=1</title></circle><circle cx="120.00" cy="182.00" r="7.00" fill="#f58518" stroke="#243447" stroke-width="1"><title>medium voltage power cores; count=3</title></circle><text x="250" y="20" font-size="10"><tspan font-weight="700">1x</tspan> optical fibre package (fiber_optic)</text><text x="250" y="34" font-size="10"><tspan font-weight="700">3x</tspan> medium voltage power cores (power)</text></svg>

### Representative concrete-coated rigid pipeline (`concrete-coated-pipeline`)

<svg id="visual-concrete-coated-pipeline" role="img" aria-label="radial layer annulus schematic" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 240"><rect width="520" height="240" fill="white"/><circle cx="120" cy="120" r="48.00" fill="#e45756" stroke="#243447" stroke-width="1"><title>concrete weight coating</title></circle><circle cx="120" cy="120" r="36.84" fill="#54a24b" stroke="#243447" stroke-width="1"><title>corrosion coating</title></circle><circle cx="120" cy="120" r="36.17" fill="#f58518" stroke="#243447" stroke-width="1"><title>line pipe wall</title></circle><circle cx="120" cy="120" r="33.49" fill="#4c78a8" stroke="#243447" stroke-width="1"><title>bore</title></circle><text x="250" y="20" font-size="10"><tspan font-weight="700">1.</tspan> bore (produced fluid envelope)</text><text x="250" y="34" font-size="10"><tspan font-weight="700">2.</tspan> line pipe wall (API 5L steel)</text><text x="250" y="48" font-size="10"><tspan font-weight="700">3.</tspan> corrosion coating (fusion bonded epoxy)</text><text x="250" y="62" font-size="10"><tspan font-weight="700">4.</tspan> concrete weight coating (reinforced concrete)</text></svg>

### Representative steel-tube electro-hydraulic umbilical (`steel-tube-electro-hydraulic-umbilical`)

<svg id="visual-steel-tube-electro-hydraulic-umbilical" role="img" aria-label="not-to-scale schematic packed-component bundle" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 240"><rect width="520" height="240" fill="white"/><circle cx="120" cy="120" r="92" fill="#f7f7f7" stroke="#243447" stroke-width="2"><title>not-to-scale schematic bundle envelope</title></circle><text x="32" y="224" font-size="10" font-weight="700">not-to-scale schematic</text><circle cx="120.00" cy="58.00" r="7.00" fill="#4c78a8" stroke="#243447" stroke-width="1"><title>electrical power quad; count=2</title></circle><circle cx="173.69" cy="151.00" r="7.00" fill="#f58518" stroke="#243447" stroke-width="1"><title>fibre optic element; count=1</title></circle><circle cx="66.31" cy="151.00" r="7.00" fill="#54a24b" stroke="#243447" stroke-width="1"><title>hydraulic steel tube bundle; count=4</title></circle><text x="250" y="20" font-size="10"><tspan font-weight="700">2x</tspan> electrical power quad (electrical)</text><text x="250" y="34" font-size="10"><tspan font-weight="700">1x</tspan> fibre optic element (fiber_optic)</text><text x="250" y="48" font-size="10"><tspan font-weight="700">4x</tspan> hydraulic steel tube bundle (hydraulic)</text></svg>

## Provenance

| Source ID | Type | Citation | URL/path | Note | Derived from |
|---|---|---|---|---|---|
| `dnv-st-f101-registry` | `standard` | DNV-ST-F101 Submarine Pipeline Systems registry entry | data/design-codes/code-registry.yaml | not specified | not applicable |
| `guide-floating-offshore-wind-220kv` | `wiki` | Guide to Floating Offshore Wind 220 kV export cable values noted in recon page | knowledge/wikis/marine-engineering/wiki/sources/offshore-cable-umbilical-cross-section-recon-2026-04-26.md | not specified | not applicable |
| `guide-floating-offshore-wind-66kv` | `wiki` | Guide to Floating Offshore Wind 66 kV values noted in recon page | knowledge/wikis/marine-engineering/wiki/sources/offshore-cable-umbilical-cross-section-recon-2026-04-26.md | not specified | not applicable |
| `octal-concrete-weight-coating-reference` | `vendor_catalogue` | Octal concrete weight coating source noted in recon page | knowledge/wikis/marine-engineering/wiki/sources/offshore-cable-umbilical-cross-section-recon-2026-04-26.md | not specified | not applicable |
| `project-assumption-2514` | `project_assumption` | Issue | not specified | Representative layer dimensions are examples for schema validation, not design defaults. | not applicable |
| `prysmian-66kv-example` | `vendor_catalogue` | Prysmian 66 kV cable example noted in recon page | knowledge/wikis/marine-engineering/wiki/sources/offshore-cable-umbilical-cross-section-recon-2026-04-26.md | not specified | not applicable |
| `prysmian-power-optical-umbilical` | `vendor_catalogue` | Prysmian power/optical umbilical source noted in recon page | knowledge/wikis/marine-engineering/wiki/sources/offshore-cable-umbilical-cross-section-recon-2026-04-26.md | not specified | not applicable |
| `sut-umbilical-reference` | `wiki` | SUT umbilical reference noted in recon page | knowledge/wikis/marine-engineering/wiki/sources/offshore-cable-umbilical-cross-section-recon-2026-04-26.md | not specified | not applicable |
| `vallourec-coating-reference` | `vendor_catalogue` | Vallourec coating source noted in recon page | knowledge/wikis/marine-engineering/wiki/sources/offshore-cable-umbilical-cross-section-recon-2026-04-26.md | not specified | not applicable |
| `wiki-cross-section-recon-2026-04-26` | `wiki` | Offshore cable/umbilical cross-section reconnaissance, 2026-04-26 | knowledge/wikis/marine-engineering/wiki/sources/offshore-cable-umbilical-cross-section-recon-2026-04-26.md | not specified | not applicable |

## Engineering Caveats

- `cable-duty`: static/export/inter-array cable fixtures are examples only; dynamic cable duty and fatigue assessment require project-specific mechanics.
- `umbilical-schematic`: packed umbilical visuals are not-to-scale schematic arrangements, not optimized manufacturing layouts.
- `pipeline-route-coating`: concrete-coated pipeline dimensions are route- and project-specific; no stability, collapse, or code capacity is derived here.
- `flexible-pipe-deferred`: flexible pipe/riser mechanics are deferred to #2516; this report does not model carcass, pressure armor, tensile armor, bending, fatigue, or dynamic riser behavior.

## Deferred Scope and Follow-Ups

- #2516 owns flexible pipe/riser mechanics and dynamic behavior.
- Vendor dimensions, source catalogue completeness, mechanical capacities, thermal ratings, weights, fatigue, and OrcaFlex export remain out of scope for this deterministic reporting pass.
