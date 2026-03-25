---
title: "Workspace Capability Report"
generated: 2026-02-14
repos_scanned: 24
capabilities_found: 42
domains_active: 7
---

# Workspace Capability Report

> Generated on 2026-02-14 | 24 repos scanned | 42 capabilities mapped | 7 domains active

## Executive Summary

### Key Metrics

| Metric | Value |
|--------|-------|
| Repositories Scanned | 24 |
| Capability Areas | 42 |
| Active Domains | 7 |
| Average Maturity | 2.3 / 4.0 |

### Maturity Distribution

| Level | Count | Percentage | Bar |
|-------|------:|------------|-----|
| Mature (4) | 3 | 13% | `###...............` |
| Established (3) | 5 | 21% | `####..............` |
| Emerging (2) | 10 | 42% | `########..........` |
| Nascent (1) | 6 | 25% | `#####.............` |

### Top Strengths

1. **Offshore Engineering Analysis** (digitalmodel, maturity 4) -- Production-grade OrcaFlex/OrcaWave/AQWA with 12-case WAMIT validation, modular model generation, 157 skills, 7 CI workflows
2. **Data Infrastructure & Processing** (pdf-large-reader + worldenergydata, maturity 4) -- 215 tests at 93.6% coverage for large PDF streaming; global energy data aggregation with BSEE extraction and vessel hull classification
3. **Business Automation** (aceengineer-admin, maturity 3) -- Invoice generation (85% time reduction), automated tax prep (Form 1120/K-1), 14 skills, 6 automation scripts

### Critical Gaps

1. **Knowledge domain entirely nascent** -- All 3 KNOW repos are placeholders (maturity 1), no structured knowledge capture system exists
2. **50% of repos lack test coverage** -- 12 repos have zero tests; engineering repos doris, saipem, seanation carry delivery risk
3. **Weak cross-repo integration** -- assetutilities (shared library) consumed by only 1 of 23 repos; OrcaFlex integration duplicated across digitalmodel, acma-projects, saipem

## Full Capability Table

| Repo | Domain | Purpose | Maturity | Tests | Skills | CI/CD |
|------|--------|---------|:--------:|:-----:|:------:|:-----:|
| **digitalmodel** | ENG | Digital twin modeling -- OrcaFlex/OrcaWave/AQWA, WAMIT validation | 4 | 1971+ files | 157 | 7 workflows |
| **worldenergydata** | DATA | Global energy data aggregation, vessel hull classification, BSEE extraction | 4 | Yes (hypothesis) | 38 | 4 workflows |
| **pdf-large-reader** | INFRA | Memory-efficient PDF processing for 100MB+ files, streaming architecture | 4 | 215 (93.6%) | 2 | -- |
| **aceengineer-admin** | BIZ | Invoice automation (85% time save), tax prep, Form 1120/K-1 filing | 3 | Yes (80% target) | 14 | -- |
| **aceengineer-website** | WEB | AI-native engineering portfolio, static site, GitHub Pages/Vercel | 3 | Jest | 4 | Vercel |
| **acma-projects** | ENG | Client engineering project portfolio, OrcaFlex license management | 3 | -- | 6 | -- |
| **assetutilities** | INFRA | Shared Python utilities -- web scraping, data processing, office automation | 3 | Yes | 13 | -- |
| **pyproject-starter** | INFRA | Python package template (pyproject.toml, uv, pre-commit, hatchling) | 3 | Template | 5 | -- |
| **assethold** | FIN | Stock analysis -- insider trading, institutional holders, options, technical indicators | 2 | Yes (pytest) | 6 | -- |
| **achantas-data** | DATA | Personal/family data management (finance, health, travel, printer setup) | 2 | -- | -- | -- |
| **client_projects** | BIZ | Client engagement tracking -- energy, pipeline, ML, legacy submodules | 2 | -- | -- | -- |
| **doris** | ENG | Subsea pipeline design, structural analysis, DNV/API code compliance | 2 | -- | -- | -- |
| **frontierdeepwater** | ENG | Deepwater exploration project framework, regulatory compliance | 2 | -- | -- | -- |
| **OGManufacturing** | ENG | AI-powered CAD/CAM for O&G subsea equipment manufacturing | 2 | Basic | 5 | -- |
| **rock-oil-field** | ENG | Oil field engineering, contract navigation, admin documentation | 2 | Coverage file | Yes | -- |
| **sabithaandkrishnaestates** | FIN | Real estate investment tracking, corporate formation, tax filing | 2 | -- | 5 | -- |
| **saipem** | ENG | Offshore subsea installation -- umbilical analysis, cathodic protection | 2 | -- | -- | -- |
| **seanation** | ENG | Offshore drilling -- coiled tubing, reel handling, deepwater tech | 2 | -- | -- | -- |
| **achantas-media** | KNOW | Media/asset storage (placeholder, RAG-to-skills feedback completed) | 1 | -- | -- | -- |
| **ai-native-traditional-eng** | KNOW | Engineering knowledge repository (placeholder, empty) | 1 | -- | -- | -- |
| **hobbies** | KNOW | Personal knowledge management (arts, music theory) | 1 | -- | -- | -- |
| **investments** | FIN | Investment tracking (placeholder, .gitignore only) | 1 | -- | -- | -- |
| **sd-work** | INFRA | Software dev workflow optimization (placeholder) | 1 | -- | -- | -- |
| **teamresumes** | BIZ | Team resume documentation (placeholder) | 1 | -- | -- | -- |

## Domain Breakdowns

### Engineering Analysis (`ENG`)

**Repos**: digitalmodel, acma-projects, doris, frontierdeepwater, OGManufacturing, rock-oil-field, saipem, seanation, worldenergydata (partial)

| Capability Area | Maturity | Lead Repo | Notes |
|----------------|:--------:|-----------|-------|
| Hydrodynamic diffraction/radiation | 4 | digitalmodel | OrcaWave, AQWA, WAMIT validation (11/12 pass) |
| OrcaFlex modular model generation | 4 | digitalmodel | Pipeline, riser, mooring builders; 51-model benchmark |
| Riser analysis (catenary, lazy wave, pliant wave, steep wave) | 4 | digitalmodel | All 4 converge, <5.1% deviation |
| Fatigue analysis (S-N curves) | 3 | digitalmodel | 221 curves from 17 standards |
| Structural analysis | 3 | digitalmodel | Wall thickness, stress, capacity validation |
| Mooring design | 3 | digitalmodel | Chain database (R3-R5), router pattern |
| VIV analysis | 3 | digitalmodel | Vortex-induced vibration assessment |
| CAD/mesh generation | 3 | digitalmodel | GMSH, FreeCAD automation, BEMRosetta |
| Subsea pipeline design | 2 | doris | MATLAB/Excel-based, DNV/API codes, no Python impl |
| Offshore drilling | 2 | seanation | CT drilling, reel operations, Excel/VBA |
| Manufacturing automation | 2 | OGManufacturing | AI-powered CAD/CAM concept, partial implementation |
| Deepwater exploration | 2 | frontierdeepwater | Project framework, empty src/ |

### Data & Analytics (`DATA`)

**Repos**: worldenergydata, achantas-data

| Capability Area | Maturity | Lead Repo | Notes |
|----------------|:--------:|-----------|-------|
| Energy market data aggregation | 4 | worldenergydata | EIA, IEA, BSEE, SODIR; scrapy + selenium |
| Vessel fleet & hull classification | 4 | worldenergydata | 2,187 rigs, 163 historical deepwater; hull geometry |
| BSEE offshore production data | 4 | worldenergydata | Binary datasets (~300MB), parallel refresh |
| Metocean data | 3 | worldenergydata | Fetch, statistics, visualization skills |
| Economic analysis | 3 | worldenergydata | NPV, sensitivity, production forecasting |
| Personal data management | 2 | achantas-data | Finance, health, travel -- file-based |

### Infrastructure & DevOps (`INFRA`)

**Repos**: assetutilities, pdf-large-reader, pyproject-starter, sd-work

| Capability Area | Maturity | Lead Repo | Notes |
|----------------|:--------:|-----------|-------|
| Large PDF processing | 4 | pdf-large-reader | 93.6% coverage, streaming, AI fallback |
| Web scraping | 3 | assetutilities | Scrapy, Selenium, Playwright, undetected-chromedriver |
| Office automation | 3 | assetutilities | Excel, Word, PDF utilities |
| Python project scaffolding | 3 | pyproject-starter | Modern PEP 517/518, uv, pre-commit |
| Shared data processing | 3 | assetutilities | pandas/numpy ETL, unit tracking (pint) |
| Dev workflow optimization | 1 | sd-work | Placeholder |

### Business Operations (`BIZ`)

**Repos**: aceengineer-admin, assethold, client_projects, teamresumes

| Capability Area | Maturity | Lead Repo | Notes |
|----------------|:--------:|-----------|-------|
| Invoice automation | 3 | aceengineer-admin | 85% time reduction, python-docx/ReportLab |
| Tax preparation | 3 | aceengineer-admin | Form 1120, K-1, franchise tax automation |
| Contract management | 3 | aceengineer-admin | OrcaFlex agreements, compliance docs |
| Client project tracking | 2 | client_projects | Legacy submodules, consolidation in progress |
| Team capability docs | 1 | teamresumes | Placeholder |

### Web & Frontend (`WEB`)

**Repos**: aceengineer-website

| Capability Area | Maturity | Lead Repo | Notes |
|----------------|:--------:|-----------|-------|
| Engineering portfolio | 3 | aceengineer-website | Bootstrap 3.x, SEO, Vercel hosting |
| Technical transparency | 3 | aceengineer-website | Diffraction/dynacard capabilities showcase |

### Financial & Investments (`FIN`)

**Repos**: assethold, investments, sabithaandkrishnaestates

| Capability Area | Maturity | Lead Repo | Notes |
|----------------|:--------:|-----------|-------|
| Stock analysis | 2 | assethold | Dash/Flask UI, yfinance/Finnhub, options |
| Real estate tracking | 2 | sabithaandkrishnaestates | CRE, corporate formation, tax filing |
| Investment portfolio | 1 | investments | Empty placeholder |

### Knowledge & Documentation (`KNOW`)

**Repos**: achantas-media, ai-native-traditional-eng, hobbies

| Capability Area | Maturity | Lead Repo | Notes |
|----------------|:--------:|-----------|-------|
| Engineering knowledge base | 1 | ai-native-traditional-eng | Empty -- no content |
| Media collection | 1 | achantas-media | RAG-to-skills feedback loop completed, no content |
| Personal learning | 1 | hobbies | Arts, music theory -- file-based |

## Cross-Domain Dependencies

| Source | Target | Dependency | Repos Involved |
|--------|--------|------------|----------------|
| worldenergydata | assetutilities | Git submodule for shared data processing | worldenergydata -> assetutilities |
| digitalmodel <-> worldenergydata | -- | Tier 2 (eng reference) <-> Tier 1 (collection data) governance | Data residency policy |
| aceengineer-admin -> acma-projects | -- | Invoice/tax automation for consulting tracked in acma-projects | Business ops cluster |
| pyproject-starter -> All Python repos | -- | Template for standardized pyproject.toml, uv, pytest | 16 Python repos |
| digitalmodel + acma-projects + saipem | -- | Engineering consulting cluster (all use OrcaFlex) | Shared solver licensing |
| worldenergydata -> digitalmodel | -- | Vessel hull models feed RAO mapping in hydrodynamic library | Hull geometry pipeline |
| assethold -> aceengineer-admin | -- | Investment returns feed into tax preparation/filing | Financial reporting |

## Gap Analysis

### Covered Objectives

- Offshore engineering digital twin modeling (OrcaFlex, OrcaWave, AQWA)
- Cross-solver validation benchmarking (WAMIT, AQWA vs OrcaWave)
- Energy market data aggregation and analysis
- Business automation (invoicing, tax, contracts)
- Large document processing (PDF streaming)
- Python project standardization

### In Progress

- Vessel hull classification and fleet expansion (WRK-135)
- Manufacturing automation (OGManufacturing -- AI-powered CAD/CAM)
- Stock analysis platform (assethold -- Dash UI, institutional tracking)
- Knowledge indexing (aceengineer-admin Phase 1)

### Gaps

- **Structured knowledge management** -- No system for capturing engineering decisions, lessons learned, or institutional knowledge across repos
- **Cross-repo CI/CD** -- Only 3 repos have GitHub Actions; no unified pipeline or quality gate
- **Shared OrcaFlex SDK** -- OrcaFlex integration duplicated in digitalmodel, acma-projects, saipem with no shared library
- **API layer** -- No FastAPI/REST endpoints exposing engineering capabilities programmatically (worldenergydata has schema but no deployed API)
- **Monitoring/observability** -- No unified health dashboard, error tracking, or usage metrics across repos

### At Risk

- **doris, saipem, seanation** -- Active engineering consulting repos with zero tests and no CI/CD; delivery risk for client projects
- **6 nascent repos** -- Maintenance overhead with no value delivery; should be archived or activated within 30 days

## Recommendations

| # | Priority | Recommendation | Approach | Target |
|---|----------|---------------|----------|--------|
| 1 | High | Archive 6 nascent repos or set 30-day activation deadline | Review with stakeholder; archive via `git archive` + README tombstone | 2026-03-15 |
| 2 | High | Add pytest + CI to doris, saipem, seanation | Use pyproject-starter template; add GitHub Actions ci.yml | 2026-03-31 |
| 3 | Medium | Extract shared OrcaFlex utilities to assetutilities | Identify common patterns (model loading, post-processing, RAO extraction) | 2026-04-30 |
| 4 | Medium | Consolidate duplicated skills into workspace-hub shared library | Audit 157 + 38 + 14 + 13 skills for overlap; deduplicate | 2026-04-15 |
| 5 | Medium | Deploy unified CI/CD pipeline across all active repos | GitHub Actions reusable workflows in workspace-hub/.github/ | 2026-04-30 |
| 6 | Low | Build engineering knowledge base in ai-native-traditional-eng | Capture decision records, solver gotchas, domain patterns | Ongoing |
| 7 | Low | Create API layer for engineering capabilities | FastAPI endpoints wrapping digitalmodel + worldenergydata | 2026-06-30 |

## Appendix: Per-Repo Profiles

### digitalmodel (ENG, Maturity 4)
- **Tech**: Python 3.11+, OrcaFlex (OrcFxAPI), OrcaWave, AQWA, Plotly, pytest, gmsh
- **Skills**: 157 across engineering, CAD, workflows, meta
- **Tests**: 1,971+ test files, 80% coverage target, 7 CI workflows
- **CLI**: 16 commands (orcaflex-universal, diffraction, aqwa, mooring-analysis, etc.)
- **Recent**: WRK-134 WAMIT validation (11/12 pass), modular generator enhancements, wind turbine templates

### worldenergydata (DATA, Maturity 4)
- **Tech**: Python 3.11+, pandas, numpy, scipy, Plotly, scrapy, selenium, SQLAlchemy, FastAPI
- **Skills**: 38 (BSEE, metocean, economic analysis, vessel fleet, web scraper)
- **Tests**: Comprehensive with hypothesis property testing
- **CLI**: `worldenergydata` unified orchestrator
- **Recent**: WRK-135 vessel hull models, WRK-120 BSEE parallel refresh, drilling rig fleet (2,187 rigs)

### pdf-large-reader (INFRA, Maturity 4)
- **Tech**: Python 3.8+, PyMuPDF, Pillow, Claude AI fallback
- **Skills**: 2
- **Tests**: 215 tests (170 unit + 45 integration), 93.58% coverage
- **CLI**: `pdf-large-reader`
- **Recent**: Table extraction fixes, robust extraction documentation

### aceengineer-admin (BIZ, Maturity 3)
- **Tech**: Python 3.11+, pandas, python-docx, ReportLab, Click
- **Skills**: 14
- **Tests**: pytest with 80% coverage target
- **Scripts**: 6 automation scripts
- **Recent**: OrcaFlex agreement drafting, knowledge indexing Phase 1

### aceengineer-website (WEB, Maturity 3)
- **Tech**: HTML5, CSS3, Bootstrap 3.x, JavaScript, jQuery, Vercel
- **Skills**: 4
- **Tests**: Jest
- **Recent**: Lighthouse optimization, diffraction/dynacard capability showcase

### acma-projects (ENG, Maturity 3)
- **Tech**: Documentation-driven, OrcaFlex license management
- **Skills**: 6
- **Recent**: OrcaFlex license admin, host migration

### assetutilities (INFRA, Maturity 3)
- **Tech**: Python 3.9+, scrapy, selenium, pandas, numpy, Plotly, python-docx
- **Skills**: 13
- **CLI**: assetutils-devtools, assetutils-modernize, assetutils-propagate
- **Recent**: Hardware docs, engineering unit tracking, cross-repo sync

### pyproject-starter (INFRA, Maturity 3)
- **Tech**: Python 3.8+, Hatchling, uv, pytest, black, mypy, ruff
- **Skills**: 5
- **Recent**: Inheritance model migration

### assethold (FIN, Maturity 2)
- **Tech**: Python 3.9+, Dash/Flask, Plotly, yfinance, Finnhub
- **Skills**: 6
- **Tests**: pytest
- **Recent**: Insider trading analysis, caching layer, timezone fixes

### Remaining 15 repos: Maturity 1-2, limited implementation, see full table above.

---
*Generated by repo-capability-map v1.0.0 | Claude Opus 4.6*
