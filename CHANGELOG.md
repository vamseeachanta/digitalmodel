# Changelog

All notable changes to the Digital Model project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [Unreleased]

- cathodic_protection: propose a repo-owned structure/benchmark report roadmap with explicit privacy, review-only document control and owner-approval gates ([#2281](https://github.com/vamseeachanta/digitalmodel/issues/2281)); portfolio implementation remains pending.
- cathodic_protection: rebuild `ABS_gn_ships_2018` on shared kernels with fractional ABS coating breakdown, depleted long-flush geometry, mass/output/layout checks, and cited December 2017 guide values; the old solver remains as deprecated `ABS_gn_ships_2018_legacy`. Owner decision 2026-10-01 accepts the arithmetic-mean coating treatment, depleted long-flush resistance reading, and explicit project dynamic bare-steel current-density input; the rebuilt route is now `client-use-with-eor-check` ([#2259](https://github.com/vamseeachanta/digitalmodel/issues/2259)).

### Added

- cathodic_protection: terminal `DNV_RP_F103_anode_bank` route calculates pipeline-plus-structure demand, grouped stand-off-anode resistance, conservative F103 terminal attenuation, protected length, far-end potential and governing PASS/FAIL status.

### Changed

- cathodic_protection: `DNV_RP_B401_offshore` now accepts a mutually exclusive `inputs.components[]` extension for hybrid and free-standing risers, with component-local zones, coatings, environment and life; explicit electrical continuity and family allocations; stand-off, flush and bracelet anodes; and component, family and overall demand/mass/count/output checks and reports ([#2261](https://github.com/vamseeachanta/digitalmodel/issues/2261)).
- cathodic_protection: the `DNV_RP_B401_offshore` route now supports concrete-embedded reinforcement zones and named seawater/sediment anode families, with cited per-family electrochemistry, mass/count/current-output checks, and family plus overall governing cases ([#2262](https://github.com/vamseeachanta/digitalmodel/issues/2262)).
- cathodic_protection: add B401 riser-base/foundation/mudmat/hatch-cover compositions, sequential temporary and wet-storage consumption, retrofit additional-anode sizing, and explicit fail-preserving accepted-output-shortfall records ([#2263](https://github.com/vamseeachanta/digitalmodel/issues/2263)).
- cathodic_protection: record evidence class/source without lifting experimental gates; include 200 ohm-m in the EN middle band, require measured graphite mass for life, and expose evidence in provisional report text ([#2264](https://github.com/vamseeachanta/digitalmodel/issues/2264)).

- cathodic_protection: `ABS_gn_ships_2018` now raises `ExperimentalModelError` unless `inputs.design_data.experimental: true` (Boolean). Opt-in preserves the calculation and sets `status.use_status` to `experimental-known-understatement`; reports warn that mean demand is about a third low and final demand about half low per the 2026-09-27 benchmark ([#2259](https://github.com/vamseeachanta/digitalmodel/issues/2259), [#1852](https://github.com/vamseeachanta/digitalmodel/issues/1852)). `ABS_gn_offshore_2018` is unchanged.

- cathodic_protection: DNV-RP-F103 default edition is now 2019 (was 2010). Results for fluids above 25 °C and for FBE coatings increase; pass edition='2010' to reproduce earlier results.
- cathodic_protection: new engine key `DNV_RP_F103` runs `design_data.edition` (default 2019); `DNV_RP_F103_2010` is now a deprecated alias pinned to edition 2010 (DeprecationWarning; a conflicting `design_data.edition` raises) so existing YAMLs reproduce their earlier results.
- cathodic_protection: client use approved on the DNV-RP-B401 offshore and DNV-RP-F103 bracelet routes only, subject to an engineer-of-record check of every deliverable; every route's `status` block now carries `use_status` (`client-use-with-eor-check` / `legacy-uncited-independent-check-required` for the ABS and `*_legacy` routes) and the CP anode-design report states it (owner decision 2026-09-27, #2206).

### Fixed

- cathodic_protection: the `DNV_RP_F103` route maps `pipeline.field_joint_coating` for the resolved edition, so 2019 runs accept the DNVGL-RP-F102 (2011) Table A-2 ids (e.g. `3A`, `2B(1)`, `5A/B/C(1)`) and May 2021 names; the new `pipeline.field_joint_infill` selects the 3A FBE row (none 0.10/0.010, 4E(2) PU 0.03/0.003) and is required for it (#2256).

## [2.1.0] - 2026-03-26

### Phase 1 GSD Sprint -- New Calculation Modules

Three new calculation modules shipped with full test coverage and traceability manifests.

### Added

#### On-Bottom Stability (DNV-RP-F109)
- 5 calculation functions for pipeline stability on seabed
- Covers absolute stability, generalized lateral stability, vertical stability
- 20 document-verified tests against DNV-RP-F109 worked examples
- YAML manifest with clause-level traceability
- **Source:** `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py`

#### ASME B31.4 Wall Thickness
- CodeStrategy pattern: burst, collapse, propagation buckling checks for liquid pipelines
- Registered in CODE_REGISTRY alongside existing DNV-ST-F101 strategy
- YAML manifest tracing each check to ASME B31.4 clause numbers
- **Source:** `src/digitalmodel/structural/analysis/wall_thickness_codes/asme_b31_4.py`

#### Spectral Scatter Fatigue (DNV-RP-C203)
- scatter_fatigue_damage function for sea-state scatter diagram fatigue analysis
- SeaStateEntry and ScatterFatigueResult dataclasses
- Injectable wave spectrum function (JONSWAP default, decoupled from hydrodynamics)
- YAML manifest tracing to DNV-RP-C203 Appendix C/D
- **Source:** `src/digitalmodel/structural/fatigue/scatter_fatigue.py`

#### Module Manifest Schema
- Pydantic-based ModuleManifest schema for per-module manifest.yaml validation
- CI validation script (validate_manifests.py) for repo-wide manifest discovery
- **Source:** `src/digitalmodel/specs/manifest_schema.py`

#### Integration
- module-registry.yaml updated with 3 new module entries
- All 3 manifests validated against Pydantic schema
- Cross-module test pass with 90.5% coverage (all modules above 80%)

---

## [2.0.0] - 2025-10-03

### Phase 1 Complete - Foundation Release

Major milestone release establishing core analytical infrastructure for offshore and marine engineering.

### Added

#### Fatigue Analysis Module
- **S-N Curve Database**: 221 S-N curves from 17 international standards
  - DNV (81 curves): Editions from 1984-2012
  - BS 7608 (79 curves): British Standard 1993, 2014
  - ABS (24 curves): American Bureau of Shipping 2020
  - BP (25 curves): Industry standard 2008
  - Norsok (15 curves): Norwegian standard 1998
  - Bureau Veritas (14 curves): Classification society 2020
  - API (2 curves): American Petroleum Institute 1994
  - Titanium (4 curves): Specialized materials
- **S-N Curve Plotter** (`src/digitalmodel/fatigue/sn_curve_plotter.py`)
  - Log-log and linear-log plotting capabilities
  - Stress concentration factor (SCF) application
  - Fatigue limit handling for multi-slope curves
  - Multi-curve comparison plots
  - Reference curve highlighting
  - Export to PNG, SVG, PDF formats
- **Data Formats**: Structured CSV and JSON formats with complete metadata
- **Documentation**: Comprehensive README for fatigue database (`data/fatigue/README.md`)

#### Marine Analysis Module
- **RAO Data Processor** (`src/digitalmodel/modules/marine_analysis/rao_processor.py`)
  - Multi-format data import (AQWA, OrcaFlex, experimental)
  - RAOData container class with metadata tracking
  - User-friendly error handling with RAOImportError
- **RAO Validators** (`src/digitalmodel/modules/marine_analysis/rao_validators.py`)
  - Physical constraint validation (frequency > 0, heading 0-360°)
  - Data completeness verification (all 6 DOFs)
  - Symmetry validation (port/starboard, fore/aft)
  - Phase continuity analysis
  - Amplitude anomaly detection
  - ValidationReport with detailed warnings/errors
- **RAO Interpolator** (`src/digitalmodel/modules/marine_analysis/rao_interpolator.py`)
  - Cubic spline interpolation for frequencies
  - Angular interpolation for headings with wrapping
  - Grid standardization capabilities
  - Quality metrics tracking
- **Format Readers**
  - AQWA Reader (`aqwa_reader.py`): .lis file parser for displacement RAOs
  - OrcaFlex Reader (`orcaflex_reader.py`): Vessel type data import
  - Enhanced Parser (`aqwa_enhanced_parser.py`): Advanced AQWA parsing

#### Mooring Analysis Module
- **Mooring Base** (`src/digitalmodel/modules/mooring/mooring.py`)
  - Configuration-driven analysis framework
  - Router pattern for extensible analysis types
  - YAML/JSON configuration support
  - Logging and reporting infrastructure
- **OrcaFlex Integration** (`orcaflex.py`)
  - Mooring line modeling support
  - Environmental condition setup
  - Static and dynamic analysis foundation

#### Examples
- `examples/fatigue/plot_sn_curves_examples.py`: 8 comprehensive examples
- `examples/fatigue/complete_fatigue_analysis.py`: Full workflow demonstration
- `examples/fatigue/plot_sn_curves_cli.py`: Command-line interface

#### Documentation
- **Phase 1 Implementation Report** (`docs/phase1-implementation-report.md`)
  - Complete implementation summary
  - Module descriptions and architecture
  - Validation results
  - Known issues and workarounds
  - Phase 2 roadmap
- **Phase 1 API Reference** (`docs/phase1-api-reference.md`)
  - Complete API documentation
  - Function signatures and parameters
  - Return types and exceptions
  - Code examples for all APIs
- **Executive Summary** (`docs/EXECUTIVE_SUMMARY.md`)
- **Fatigue Curve Implementation Review** (`docs/fatigue_curve_implementation_review.md`)

#### Tests
- `tests/fatigue/test_fatigue_migration.py`: Migration validation
- `tests/domains/fatigue_analysis/test_fatigue_analysis_sn.py`: S-N curve tests
- `tests/test_fatigue_basic.py`: Basic functionality tests
- Comprehensive test coverage: 85%+

### Changed

#### Project Structure
- Reorganized files into proper directory structure
  - Source code: `src/digitalmodel/`
  - Data: `data/`
  - Examples: `examples/`
  - Tests: `tests/`
  - Documentation: `docs/`
- Updated README.md with Phase 1 features and comprehensive documentation
- Enhanced project organization following best practices

#### Code Quality
- Implemented comprehensive input validation
- Added detailed error messages with suggestions
- Improved logging throughout codebase
- Enhanced docstrings with examples

### Fixed
- Overall damage calculation with explicit type handling
- Rainflow analysis enhancements
- File organization issues (removed root directory clutter)

### Performance
- S-N curve plotting: <0.5s for 20 curves, <20 MB memory
- RAO import (AQWA .lis): <1s for 100 KB file, <50 MB memory
- RAO interpolation: <2s for 1000 points, <100 MB memory

### Validation
- S-N curve plotting accuracy: 99.9% (target: 99%)
- RAO interpolation error: <2% (target: <5%)
- Phase preservation: <1° (target: <2°)
- Data validation coverage: 95% (target: 90%)

### Known Issues
1. **Fatigue Module**
   - Plotting >20 curves results in cluttered legends
   - Custom S-N curve addition requires manual CSV editing

2. **Marine Analysis Module**
   - Phase unwrapping not automatic (requires manual review)
   - Limited AQWA format support (displacement RAOs only)

3. **Mooring Module**
   - Limited mooring type coverage (foundation only)
   - Static analysis only (dynamic integration incomplete)

See `docs/phase1-implementation-report.md` Section 6 for detailed workarounds.

### Security
- No hardcoded secrets or credentials
- Secure file path handling
- Input validation at all entry points

### Migration Notes
- This is a major release establishing new core infrastructure
- All new APIs are considered stable for Phase 1
- Configuration format is backward compatible with existing projects

---

## [1.x.x] - Previous Versions

### Historical Development (Pre-Phase 1)

Prior versions focused on:
- Basic engineering calculations
- OrcaFlex integration foundations
- Stress analysis capabilities
- Time-series analysis tools
- Hydrodynamics calculations

See git history for detailed commit logs: https://github.com/vamseeachanta/digitalmodel/commits

---

## Future

### Planned Enhancements

#### Marine Analysis
- WAMIT output file support
- Force/moment RAO extraction
- Automatic phase unwrapping
- Multi-body RAO handling

#### Mooring Module
- Spread mooring systems
- Line fatigue assessment
- Optimization capabilities

#### Cross-Module Integration
- Mooring-Fatigue coupling
- Unified reporting system

---

## Version Numbering

This project uses Semantic Versioning (MAJOR.MINOR.PATCH):
- **MAJOR**: Incompatible API changes or major new features
- **MINOR**: New functionality, backward compatible
- **PATCH**: Bug fixes, backward compatible

**Current Version:** 2.0.0 (Phase 1 Complete)

---

## Links

- **Repository**: https://github.com/vamseeachanta/digitalmodel
- **Issues**: https://github.com/vamseeachanta/digitalmodel/issues
- **Documentation**: [docs/](docs/)
- **Examples**: [examples/](examples/)

---

## Contributors

**Lead Developer:** Vamsee Achanta ([email removed])

**Dedication:** Mark Cerkovnik - Chief Engineer, mentor, and inspiration

**Acknowledgments:**
- 200+ SURF engineers for collective insights
- Industry standards organizations (DNV, API, ABS, BS, Norsok, Bureau Veritas)
- Open-source community

---

**Changelog Maintained By:** Digital Model Development Team
**Last Updated:** March 26, 2026
