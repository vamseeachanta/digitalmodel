# OCIMF MEG3 / MEG4 Coefficient Corpus — Data Routing

Map of where the real OCIMF wind and current coefficient data lives across the
ecosystem, what schema it uses, and how digitalmodel consumes it.

## TL;DR

The real OCIMF MEG3/MEG4 coefficient corpus is **off-repo** at
`/mnt/ace/acma-codes/OCIMF/` and `/mnt/ace/acma-codes/OCIMF (MEG 4)/`. It is
NOT in this repository, and should not be committed here (vendor-licensed
standard, correctly routed off-repo per workspace-hub's
`feedback_offrepo_intel_routing` rule). The in-repo sample CSV is synthetic
test data, not the real corpus.

## Canonical sources

| Artifact | Location | Role |
|---|---|---|
| MEG3 (2008) full PDF | `/mnt/ace/acma-codes/OCIMF/OCIMF - 2008 - Mooring Equipment Guidelines.pdf` | Canonical published 3rd-edition standard |
| MEG4 (2018) full PDF | `/mnt/ace/acma-codes/OCIMF (MEG 4)/Mooring Equipment Guidelines - MEG4.pdf` | Canonical published 4th-edition standard |
| **Digitized lookup (master)** | **`/mnt/ace/acma-codes/OCIMF/OCIMF Coef.xlsx`** | **17 sheets, 3 data + 14 chart; covers Annex A A5-A19** |
| Alternative WPD digitization | `/mnt/ace/acma-codes/OCIMF 3rd ed/Digitizer/` | WebPlotDigitizer extraction for A5-A9 only — cross-validation reference |
| Figure PDF extracts (15 files) | `/mnt/ace/acma-codes/OCIMF/Figures/` | Per-figure extracts of the published curves |
| Project-bundled MEG4 extracts | `/mnt/ace/acma-projects/B1522/ctr-7/_data/ocimf/` | 8 PDFs covering A13-A21 + worked examples |
| MEG4 justification commentary | `/mnt/ace/acma-codes/OCIMF (MEG 4)/Mooring Equipment Guidelines (MEG4) justification.pdf` | Update commentary for MEG3 → MEG4 transition |
| LNG-specific wind coefficients (separate file, not in master) | `/mnt/ace/acma-codes/OCIMF 3rd ed/old & other/LNGC Coeficients.xlsx` | Supplementary LNG-focused coefficients |
| Tandem mooring guideline | `/mnt/ace/acma-codes/OCIMF/OCIMF-Tandem Mooring and Offloading Guidelines for Conventional Tankers at FPSO Facilities.pdf` | Specialized guideline (not coefficient data) |

## In-repo references

| Artifact | Path | Role |
|---|---|---|
| Synthetic sample CSV | `docs/domains/charts/phase2/validation/temp_ocimf_db.csv` | 118-row VLCC test fixture (NOT real corpus) — see companion `temp_ocimf_db.csv.README` |
| Interactive HTML explorer | `docs/domains/charts/phase2/ocimf/ocimf_coefficient_explorer.html` | Visualizes the real corpus, 15 interactive Plotly figures |
| Cross-project prompt template | `docs/domains/charts/phase2/ocimf/PROMPT_TEMPLATE.md` | Reusable LLM prompt for retrieving coefficients in another project |
| Build script | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | Generates the HTML explorer from the real corpus |
| OCIMF module | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `OCIMFDatabase`, `EnvironmentalForces` — currently uses synthetic sample |
| OCIMF module (duplicate) | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | Twin of the above; differs by single import. Consolidation TBD. |
| Hydrodynamics OCIMF | `src/digitalmodel/hydrodynamics/ocimf_loading.py` | Alternative MEG4 implementation with simplified hardcoded coefficients |
| Validator | `src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py` | Generic CSV validator |
| Extraction script | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | Reads an Excel workbook (Windows path hardcoded; non-portable) |
| Tests | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | Uses synthetic sample fixture |
| Citation registry | `src/digitalmodel/citations/registry.py` | DNV-OS-E301 pilot; OCIMF-MEG4 wiring pending #563 |

## Schema mismatch summary

| Axis | `OCIMFDatabase` schema (current) | `OCIMF Coef.xlsx` schema (real) |
|---|---|---|
| Primary | `heading` (deg) | ✅ same |
| Secondary | `displacement` (tonnes) | `WD/T ratio` (water-depth / draft) |
| Vessel taxonomy | `vessel_type` column | Sheet-block partition (Tanker Loaded / Tanker Ballast 40%T / Gas Carrier Prismatic / Gas Carrier Spherical) |
| Coefficient set | `CXw, CYw, CMw, CXc, CYc, CMc` per row | Per-figure: tankers carry CURRENT only (A5-A14), gas carriers carry WIND only (A17-A19) |
| Vessel geometry | `vessel_name, loa, beam, draft` columns | NOT in coefficient corpus — caller-supplied |

This mismatch is the scope of the `OCIMFExcelAdapter` ingestion path tracked at
[digitalmodel#563](https://github.com/vamseeachanta/digitalmodel/issues/563).

## Coverage gaps

- **Figure A15 absent** from the digitization (sheet naming jumps Data 10a-14a → Data 16a-19a)
- **Tanker wind coefficients NOT in this corpus** — separately published in OCIMF
  1994 "Prediction of Wind and Current Loads on VLCCs", which is NOT digitized
  in the ecosystem
- **Gas carrier current coefficients NOT in this corpus** — MEG4 recommends
  using tanker current tables with gas-carrier-specific reference area
- **Vessel classes other than tankers and gas carriers** — outside OCIMF MEG
  scope; need vessel-specific wind-tunnel data
- **Loading states other than loaded and ballast-40%-T** — interpolate or use
  the closest digitized condition

## Citation contract

Per `.claude/rules/calc-citation-contract.md`:
- Any standards-derived constant from OCIMF MEG3/MEG4 emitted by a calc module
  must produce a `Citation` sidecar binding to a wiki standards page.
- Target wiki pages: `wiki/standards/ocimf-meg3.md` and
  `wiki/standards/ocimf-meg4.md` — both tracked at
  [workspace-hub#2284](https://github.com/vamseeachanta/workspace-hub/issues/2284).
- Precedent: `src/digitalmodel/orcaflex/mooring_design.py::check_mbl_with_safety_factor`
  (DNV-OS-E301 pilot, workspace-hub#2685).

## Related issues

| Issue | Scope |
|---|---|
| [digitalmodel#556](https://github.com/vamseeachanta/digitalmodel/issues/556) | Coefficient interpolation produces unphysical values from synthetic data |
| [digitalmodel#557](https://github.com/vamseeachanta/digitalmodel/issues/557) | Boundary-warnings not firing on out-of-range queries |
| [digitalmodel#561](https://github.com/vamseeachanta/digitalmodel/issues/561) | Combined environmental forces test premise |
| [digitalmodel#563](https://github.com/vamseeachanta/digitalmodel/issues/563) | OCIMF database performance + real-corpus ingestion (the `OCIMFExcelAdapter` scope) |
| [digitalmodel#564](https://github.com/vamseeachanta/digitalmodel/issues/564) | Environmental forces total |
| [workspace-hub#2284](https://github.com/vamseeachanta/workspace-hub/issues/2284) | Wiki promotion of MEG3/MEG4 (provides citation target pages) |
| [workspace-hub#2278](https://github.com/vamseeachanta/workspace-hub/issues/2278) | MEG fragments reconcile (closed 2026-05-20) |
| [workspace-hub#2625](https://github.com/vamseeachanta/workspace-hub/issues/2625) | Marine-engineering domain regressions umbrella |
