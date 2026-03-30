# digitalmodel

Engineering calculation library for offshore, subsea, and marine analysis. Single source of truth from standard clause to validated code.

**Dedicated to Mark Cerkovnik** -- Chief Engineer, mentor, and inspiration.

## What This Is

A Python package of engineering calculations where every function traces to an international standard. Library-first: importable calculations with CLI convenience wrappers.

- 30 engineering disciplines (structural, subsea, hydrodynamics, cathodic protection, asset integrity, geotechnical, marine ops, and more)
- 7,355 public functions across 1,077 source files
- 42 standards fully implemented, 455 in capability map
- Dual traceability: code docstrings cite standard clauses + YAML manifests for CI validation

## Installation

```bash
pip install digitalmodel
```

Requires Python 3.10+. See [pyproject.toml](pyproject.toml) for full dependency list.

## Quick Start

```python
from digitalmodel.structural.fatigue import sn_curves
from digitalmodel.subsea.on_bottom_stability import dnv_rp_f109
from digitalmodel.cathodic_protection import dnv_rp_b401
```

## Key Modules

| Module | What It Does | Standards | Tests |
|--------|-------------|-----------|-------|
| structural/fatigue | S-N curves, rainflow counting, spectral fatigue | DNV-RP-C203, API RP 2A, BS 7608 (221 curves, 17 standards) | 808 |
| cathodic_protection | Anode sizing, coating breakdown, current demand | DNV-RP-B401, API RP 1632, ISO 15589-2 | 72 |
| subsea/on_bottom_stability | Pipeline stability on seabed | DNV-RP-F109 | 20 |
| structural/analysis | Wall thickness, member capacity | ASME B31.4, DNV-ST-F101, Eurocode 3 | -- |
| asset_integrity | Fitness-for-service assessment | API 579 Level 1/2/3, BS 7910 | -- |
| hydrodynamics | Wave spectra, RAO processing, diffraction | (825 functions, standards mapping in progress) | -- |
| solvers/orcaflex | OrcaFlex model generation and post-processing | -- | -- |
| power | Generator controls, microgrid EMS, protection relays | IEEE 1547, NFPA 110, IEEE C37.112 | 210+ |
| gis | CRS transforms, spatial queries, multi-format I/O | -- | -- |

For the complete module catalog with maturity levels, capabilities, and gaps: see [specs/module-registry.yaml](specs/module-registry.yaml).

## Development Roadmap

See [ROADMAP.md](ROADMAP.md) for tiered development priorities driven by client project demand.

Current Tier 1 priorities:
- OrcaFlex subsea structural analysis -- advancing to production-grade workflow
- Cathodic protection maturity -- expanding test coverage and standard coverage

## Documentation

- [ROADMAP.md](ROADMAP.md) -- Development priorities and tech debt
- [docs/vision/CALCULATIONS-VISION.md](docs/vision/CALCULATIONS-VISION.md) -- Ecosystem vision, current state, gap register
- [specs/module-registry.yaml](specs/module-registry.yaml) -- Machine-readable module catalog (1,634 lines)
- [specs/data-needs.yaml](specs/data-needs.yaml) -- Data dependency lifecycle tracker
- [CHANGELOG.md](CHANGELOG.md) -- Release history

## CLI Tools

digitalmodel registers 27 CLI entry points. Run `digitalmodel --help` for the full list.

## Testing

```bash
cd digitalmodel
python -m pytest tests/ -x --tb=short -q
```

## Contributing

Lead developer: Vamsee Achanta (vamsee.achanta@aceengineer.com)

## License

MIT -- see [LICENSE](LICENSE)
