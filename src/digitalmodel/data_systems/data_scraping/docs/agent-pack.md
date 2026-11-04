# Data scraping knowledge pack

| Header | Value |
| --- | --- |
| Module | data_systems/data_scraping |
| Repository | digitalmodel |
| Package path | `src/digitalmodel/data_systems/data_scraping` |
| Pack revision | 0 |
| Code revision | `df9787d346c226aad513cab9d248f7be841b37d5` |
| Implementation-map document pin | `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` on `codex/implmap-work` |
| Implementation-map data revision | `270c8759c374b67a7f032953e7fcdb14e0224461` |
| Analysis family | EC; Python calculations gate set for data pipelines |
| Quality role | Calculations quality manager; data-pipeline coverage |
| Status | DRAFT |

The code pin governs code, tests and documentation citations; citations to `docs/registry/implementation-map.yaml` use the separate map document pin above, whose `revision` records its data snapshot. [docs/registry/implementation-map.yaml:L1-L3]

## 1. Purpose and scope

The package collects HTML tables, standardizes vessel and equipment records, extracts equipment PDF tables, writes CSV and metadata, and scores data completeness. [src/digitalmodel/data_systems/data_scraping/__init__.py:L1-L10] [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L183-L226] [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L41-L94] [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L48-L121]

The enclosing data-systems domain is beta; the existing registry routes its tests to `tests/data_systems/`. [docs/registry/module-routing.yaml:L112-L122]

Scope includes every Python file in this package, including its `examples`, `scrapers` and `validators` subfolders; the exported API contains the base, vessel, production-unit, drilling-rig and pipelay scrapers plus `DataValidator`. [src/digitalmodel/data_systems/data_scraping/__init__.py:L15-L31]

The default production-unit and drilling-rig endpoints are templates; the production-unit fallback parser never populates its row dictionary, and pipelay scraping requires a URL. [src/digitalmodel/data_systems/data_scraping/scrapers/fpso_scraper.py:L37-L102] [src/digitalmodel/data_systems/data_scraping/scrapers/drilling_rig_scraper.py:L41-L78] [src/digitalmodel/data_systems/data_scraping/scrapers/pipelay_scraper.py:L84-L92]

The data path has no physical force calculation: `OCIMFValidator` checks CSV schema, completeness, ranges, heading coverage and duplicate keys before scoring the result. [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L65-L95]

## 2. Inputs and outputs

Input ranges below describe implemented acceptance or absence of checking, rather than engineering approval.

| Input | Unit | Implemented range / default | Source and evidence |
| --- | --- | --- | --- |
| `BaseScraper.base_url`, `fetch_page.url` | text URL | Relative URLs are joined to the base; non-GET methods take the POST branch. | [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L119-L181] |
| `rate_limit`, `timeout`, `max_retries` | s, s, attempts | Defaults 2.0, 30 and 3; no constructor range check. | [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L34-L66] |
| `output_dir` | directory | Default `data`; `save_csv` appends domain/subdomain/filename. | [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L52-L56] [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L201-L226] |
| `rig_type`, custom source | category, URL | Annotation names deepwater, jackup and both; default both, with no runtime enumeration check. | [src/digitalmodel/data_systems/data_scraping/scrapers/drilling_rig_scraper.py:L20-L39] |
| `year_built` text | year | First four-digit match beginning 19 or 20; missing/unmatched values return None. | [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L66-L88] |
| Dimension text | m after parsing | Commas removed; feet marker converted using `Q_`; other numbers treated as metres and rounded. | [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L90-L118] |
| Equipment dimensions, weights, capacities | suffix-dependent; kg for weight | Named dimensions retain their unit suffix; unnamed products use metre fields; weights require a recognized mass token; capacity units are retained. | [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L99-L218] |
| Fender specification dictionary | m, %, kN, kJ, bar; density unspecified | Field suffixes assign units without converting input text; zero numeric values fail truth tests. | [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L73-L131] |
| PDF catalog and output directory | PDF file; directory | Default output `data/equipment/processed`; specification tables selected by keywords or numeric-column count. | [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L19-L27] [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L118-L158] |
| `DataValidator` frame, required fields, unique key | cells, column labels | Required fields and unique key default None; configuration defaults to an empty mapping. | [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L19-L53] |
| OCIMF CSV | heading degrees; dimensions m; coefficients dimensionless labels | Default `data/mooring/raw/ocimf`; requires type and heading; absent coefficient columns create warnings. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L19-L45] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L97-L113] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L188-L209] |
| Dynamic page settings | s, rows | Headless true; load timeout 30 s, table wait 10 s, minimum table size 1 row; a CSS-selected table returns before minimum-row filtering. | [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L33-L58] [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L176-L237] |

| Output | Unit / format | Behaviour | Evidence |
| --- | --- | --- | --- |
| `save_csv` | CSV; original column units | Writes without a DataFrame index and returns the path. | [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L201-L226] |
| `save_metadata` | JSON | Records dataset stem, dates, source URL, collection method and optional extra fields; suffix is `_metadata.json`. | [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L228-L276] |
| `save_vessel_data` | CSV plus JSON | Standardizes, adds type, cleans, drops unnamed/duplicate rows, then writes; its returned path names the cleaned data, whereas callers return their original frame. | [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L179-L205] [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L241-L262] [src/digitalmodel/data_systems/data_scraping/scrapers/fpso_scraper.py:L173-L186] |
| `DataValidator.validate_dataframe` | Boolean, counts, score points out of 100 | Empty inputs return an invalid result; populated results can include missing fractions and type issues. | [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L65-L121] |
| Text / interactive quality report | text / HTML | Text summarizes the returned result; interactive output uses Plotly and a CDN script. | [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L175-L200] [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L332-L335] |
| `PDFExtractor` frame | source column units / assumed suffixes | Adds PDF source, page and table indices; concatenated catalogs also carry catalog source. | [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L69-L85] [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L258-L263] |
| `OCIMFValidator.validate_dataset` | errors, warnings, statistics, score points | A caught exception sets valid false; quality score may be absent on that path. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L57-L95] |
| Fender combined output | CSV / DataFrame | Concatenates sources and drops duplicate product/manufacturer pairs; filename uses the scrape date. | [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L373-L395] |

No coordinate origin, phase convention or directional load transformation is supplied by the CSV validator; heading checking uses sorted unique numeric headings and their intervals. [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L188-L209]

## 3. Method and governing standards

| Method | Implementing definition | Basis and limit |
| --- | --- | --- |
| HTTP retry / throttling | `BaseScraper._create_session`, `_respect_rate_limit`, `fetch_page` | Retry statuses and allowed methods are explicit; request exceptions log and return None. [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L86-L102] [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L110-L181] |
| HTML table selection | `FPSOScraper.scrape_custom_url`, `DrillingRigScraper.scrape_custom_url`, `PipelayVesselScraper.scrape_custom_url` | Production-unit and pipelay routes take the largest table; drilling-rig routes concatenate all tables. [src/digitalmodel/data_systems/data_scraping/scrapers/fpso_scraper.py:L128-L145] [src/digitalmodel/data_systems/data_scraping/scrapers/drilling_rig_scraper.py:L92-L108] [src/digitalmodel/data_systems/data_scraping/scrapers/pipelay_scraper.py:L53-L66] |
| Dynamic table selection | `DynamicScraper.scrape_dynamic` | Uses rendered HTML and optional CSS selection; otherwise retains tables meeting the row threshold and chooses the largest. [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L193-L237] |
| Vessel normalization | `VesselScraper.parse_year`, `parse_dimension`, `standardize_columns`, `validate_vessel_data` | Regex extraction, column aliases and row filtering; deadweight is aliased to displacement without a physical conversion. [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L66-L205] |
| Equipment parsing | `EquipmentScraper.parse_dimensions`, `parse_weight`, `parse_capacity` | Regex-based number/unit recognition; mass conversion constants are internal code values, with no cited reference beside them. [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L99-L218] |
| Fender extraction | `FenderScraper.classify_fender_type`, `parse_fender_specifications`, `_extract_numeric_value` | Keyword classification and first-number extraction; extraction does not inspect a unit token. [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L53-L131] [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L397-L415] |
| PDF extraction | `PDFExtractor.extract_tables_from_pdf`, `find_specification_tables`, `parse_fender_specifications`, `extract_from_catalog`, `process_directory` | First row becomes the header; empty rows/columns are removed; numeric coercion can produce missing values; errors can leave partial output. [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L54-L94] [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L131-L201] [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L217-L308] |
| General quality score | `DataValidator.validate_dataframe`, `_check_missing_data`, `_check_data_types` | Starts at 100 points, deducts schema/missing/duplicate/type penalties, floors at zero and rejects scores below 60; fractions average only columns having missing cells. [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L65-L163] |
| Coefficient dataset validation | `OCIMFValidator.__init__`, `validate_dataset`, `_validate_columns`, `_validate_completeness`, `_validate_coefficient_ranges`, `_validate_dimensions`, `_validate_headings`, `_validate_duplicates`, `_calculate_quality_score`, `validate_all`, `generate_report`; CLI `main` | Internal schema/range/coverage rules and weighted completeness/error/warning score; no wind or current load is solved. [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L19-L260] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L262-L415] |

The package README lists coefficient scraping as future work, while the validator already provides generic CSV checks. [src/digitalmodel/data_systems/data_scraping/README.md:L383-L391] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L65-L89]

The separate corpus guide names OCIMF, *Mooring Equipment Guidelines*, third edition (2008) and fourth edition (2018), Annex A; it distinguishes synthetic repository fixtures from the licensed corpus and describes a schema mismatch. Clause text not checked against the licensed copy; no tabulated coefficient is reproduced here. [docs/data/OCIMF_CORPUS_README.md:L46-L57] [docs/data/OCIMF_CORPUS_README.md:L76-L90]

UNVERIFIED: the validator's screening ranges and heading-interval rule are attributable to a particular OCIMF edition, clause or table; its code supplies no such attribution or `Citation` sidecar. This is a provenance gap, rather than evidence of conflicting editions; issue pending (5). [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L30-L45] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L201-L209]

## 4. Validated worked examples

Accepted examples: 0 validated; 0 regression. The example script contains seven live-source demonstrations with printed outcomes; the module tests also print data or failure messages without asserted expected values. They do not supply independently sourced expected values or stable golden outputs. [src/digitalmodel/data_systems/data_scraping/examples/scrape_vessels_example.py:L20-L235] [tests/data_systems/data_scraping/test_direct_manufacturers.py:L14-L139] [tests/data_systems/data_scraping/test_fender_scraper.py:L18-L176] [tests/data_systems/data_scraping/test_pdf_extraction.py:L13-L180] [tests/data_systems/data_scraping/test_selenium.py:L18-L102]

The eleven demonstration test nodes and their offline NOT_RUN results are inventoried in section 10; none is promoted to a worked example. Fixture-based tests with explicit expected values remain issue pending (1). [tests/data_systems/data_scraping/test_pdf_extraction.py:L13-L24] [tests/data_systems/data_scraping/test_direct_manufacturers.py:L10-L14]

## 5. QA gates

AQG r0, section 10, Python calculations supplies G0 inputs/units/citations, G1 imports/tests and G2 hand/published checks; sections 7–10 make G3 and G4 NOT_APPLICABLE to that family and G5 applicable to every family. The following checks specify this pack's review criteria rather than certify operational data (AQG r0, sections 9–11).

| Gate | Applicability | Check IDs and route |
| --- | --- | --- |
| G0 | Applicable | C01–C03: instantiate the HTTP configuration and exercise parser/schema logic using synthetic values; no page request. [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L34-L66] [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L65-L121] |
| G1 | Applicable | C07 imports the six named public classes without requesting a licensed interface; C08 targets the module test directory explicitly. [src/digitalmodel/data_systems/data_scraping/__init__.py:L15-L31] [pytest.ini:L15-L18] |
| G2 | Applicable | C04–C05 check dimensional identity and scaling by hand; neither qualifies the source dataset or validates all conversion factors. [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L103-L118] [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L159-L178] |
| G3 | NOT_APPLICABLE | No time integration or dynamic solver output is produced by the data-validation path. [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L48-L121] |
| G4 | NOT_APPLICABLE | No diffraction mesh or hydrostatics is produced by the data-validation path. [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L65-L95] |
| G5 | Applicable | C06 checks CSV/metadata agreement; C09 requires complete citation, sweep, node and issue evidence before review (AQG r0, section 9). [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L220-L276] |

| ID | Gate | Executable check and numeric pass criterion | Evidence / recorded result |
| --- | --- | --- | --- |
| C01 | G0 | Construct `BaseScraper` with a synthetic URL; rate limit = 2.0 s, timeout = 30 s, retries = 3 attempts; zero configuration mismatches. | PASS in the offline hand-check record; constructor only. [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L34-L66] |
| C02 | G0 | `VesselScraper.parse_year('Built 2018')` = 2018 years; `parse_year('unknown')` = None; 0 mismatches across 2 cases. | PASS; this checks the implemented parser, not an external chronology. [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L76-L88] |
| C03 | G0 | `DataValidator.validate_dataframe` on two complete numeric rows with required fields returns valid true and 100 score points; an empty frame returns valid false and 0 score points; 0 mismatches. | PASS; synthetic schema/empty-input regression check. [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L65-L121] |
| C04 | G2 | `parse_dimension('3 m')` = 3.0 m; doubling metre or foot input doubles the returned value within 0.01 m, allowing output rounding; 0 mismatches. | PASS; hand identity/scaling only, with no independent foot conversion benchmark. [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L103-L118] |
| C05 | G2 | `EquipmentScraper.parse_weight('2 kg')` = 2.0 kg; `parse_weight('4 kg')` = twice that result with 0.0 kg difference; 0 mismatches. | PASS; hand identity/scaling only, without a catalog or licensed-standard value. [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L159-L178] |
| C06 | G5 | Write a two-row synthetic frame using `save_csv`; `read_csv` row count and `save_metadata` extra `total_records` both = 2 rows; 0 count differences; record SHA-256. | PASS; files and digest retained in the external evidence record. [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L220-L276] |
| C07 | G1 | Import `BaseScraper`, `VesselScraper`, `DataValidator`, `EquipmentScraper`, `FenderScraper`, `OCIMFValidator`; 6 class imports, 0 licensed-interface imports. | PASS; no request, browser or solver started. [src/digitalmodel/data_systems/data_scraping/__init__.py:L15-L31] [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L8-L17] [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L8-L16] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L9-L17] |
| C08 | G1 | `PYTHONPATH=src uv run --no-sources python -m pytest tests/data_systems/data_scraping -q -rA -p no:cacheprovider --basetemp=<external-directory>`; require 11 collected nodes, 0 failures and 0 collection errors. | FAIL at BASE and AFTER: 0 collected, 4 collection errors, 11 test bodies NOT_RUN; `ModuleNotFoundError: No module named 'scrapers'`; existing import defect, #1900 and issue pending (1). [tests/data_systems/data_scraping/test_direct_manufacturers.py:L7-L11] [tests/data_systems/data_scraping/test_fender_scraper.py:L11-L15] [tests/data_systems/data_scraping/test_pdf_extraction.py:L7-L10] [tests/data_systems/data_scraping/test_selenium.py:L11-L15] |
| C09 | G5 | Resolve every pinned citation, resolve all 11 listed test definitions by AST, account for every sweep row and open item, and run pack lint; require 0 broken citations, 0 missing definitions, 0 untracked findings and 0 lint failures (counts). | Final evidence recorded in the change description; these document checks do not clear the failed module test gate. [tests/data_systems/data_scraping/test_pdf_extraction.py:L13-L180] |

## 6. Known failure modes and pitfalls

sweep: 183 found

The count is a conservative location inventory: it includes exception handlers, empty returns, skipped records, conversions, unit assumptions, policy defaults and bookkeeping zeros; a location with multiple syntactic triggers appears once. Each row below records its disposition and an existing detection check or the absence of one, rather than treating all counters as defects. [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L65-L66] [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L103-L118] [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L65-L121]

Sweep command (followed by AST inspection of each package Python file and manual review of row filtering, unit suffixes and empty-output writes):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|return pd.DataFrame|= 0(\.0)?\b|1970|0\.05|float\(|int\(|coerce|dropna|drop_duplicates|_m\b|_kg\b|_kn\b|_kj\b|_bar\b' src/digitalmodel/data_systems/data_scraping --glob '*.py'
```

| Finding | Symptom / cause | Detection check | Tracking and date |
| --- | --- | --- | --- |
| Imported package names | Explicit module-test targeting fails at imports; the bare `scrapers` dependency also appears in shipped CLI code. | C08; #1900 records unresolved bare imports; issue pending (1) covers assertion-based offline tests. | #1900; issue pending (1); inspection 2026-11-04. [src/digitalmodel/data_systems/data_scraping/cli.py:L13-L16] [tests/data_systems/data_scraping/test_fender_scraper.py:L11-L15] |
| Traversal exclusion | The module demonstration folder remains in `norecursedirs`; the closed repository visibility issue #1977 concerns traversal, rather than repaired module imports. | C08 explicitly targets this folder; no detection check exists for asserted scraping outcomes. | #1977; issue pending (1); inspection 2026-11-04. [pytest.ini:L4-L18] |
| Metadata/documentation drift | README describes `.metadata.json`, but `save_metadata` writes `_metadata.json`; callers supplying an already nested output directory get the domain/raw components appended again. | C06 checks row provenance; no detection check exists for README consistency. | issue pending (6); inspection 2026-11-04. [src/digitalmodel/data_systems/data_scraping/README.md:L228-L240] [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L220-L223] [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L271-L276] [src/digitalmodel/data_systems/data_scraping/examples/scrape_vessels_example.py:L47-L51] |
| No-input extraction | The batch script divides by the discovered PDF count before its no-data return. | no detection check exists. | issue pending (2); inspection 2026-11-04. [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L73-L79] |
| Cleanup path | `BaseScraper` supplies context-manager cleanup, but no `close` method; equipment/dynamic `close` delegates only when present, and dynamic context exit inherits HTTP-only cleanup. | no detection check exists. | issue pending (2); inspection 2026-11-04. [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L287-L294] [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L469-L472] [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L239-L256] |
| Empty output after cleaning | `save_vessel_data` does not reject a frame emptied by name filtering before writing it. | no detection check exists. | issue pending (2); inspection 2026-11-04. [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L195-L205] [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L241-L260] |

| Sweep location | Trigger and symptom / disposition | Detection check | Tracking |
| --- | --- | --- | --- |
| S001: [src/digitalmodel/data_systems/data_scraping/cli.py:L95-L95] | Initial zero counter/state; bookkeeping rather than an imputed measurement. | no detection check exists. | issue pending (2). |
| S002: [src/digitalmodel/data_systems/data_scraping/cli.py:L96-L96] | Initial zero counter/state; bookkeeping rather than an imputed measurement. | no detection check exists. | issue pending (2). |
| S003: [src/digitalmodel/data_systems/data_scraping/cli.py:L101-L101] | batch failed source omitted. | no detection check exists. | issue pending (2). |
| S004: [src/digitalmodel/data_systems/data_scraping/cli.py:L111-L111] | batch failed source omitted. | no detection check exists. | issue pending (2). |
| S005: [src/digitalmodel/data_systems/data_scraping/cli.py:L121-L121] | batch failed source omitted. | no detection check exists. | issue pending (2). |
| S006: [src/digitalmodel/data_systems/data_scraping/examples/scrape_vessels_example.py:L157-L157] | failed batch source omitted. | no detection check exists. | issue pending (2). |
| S007: [src/digitalmodel/data_systems/data_scraping/examples/scrape_vessels_example.py:L224-L226] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S008: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L47-L47] | Initial zero counter/state; bookkeeping rather than an imputed measurement. | no detection check exists. | issue pending (2). |
| S009: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L48-L48] | Initial zero counter/state; bookkeeping rather than an imputed measurement. | no detection check exists. | issue pending (2). |
| S010: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L57-L57] | empty extraction omitted. | no detection check exists. | issue pending (2). |
| S011: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L65-L68] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S012: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L68-L68] | Loop branch continues without adding the current record/result. | no detection check exists. | issue pending (2). |
| S013: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L74-L74] | zero-input summary division. | no detection check exists. | issue pending (2). |
| S014: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L75-L75] | zero-input summary division. | no detection check exists. | issue pending (2). |
| S015: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L79-L79] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S016: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L111-L111] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (2). |
| S017: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L112-L112] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (2). |
| S018: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L155-L155] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (2). |
| S019: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L170-L170] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (2). |
| S020: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L176-L178] | Exception is reported and exits with status 1; explicit failure, retained as a sweep control. | no detection check exists. | issue pending (2). |
| S021: [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L179-L183] | Exception is reported and exits with status 1; explicit failure, retained as a sweep control. | no detection check exists. | issue pending (2). |
| S022: [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L65-L65] | Initial zero counter/state; bookkeeping rather than an imputed measurement. | C01 (configuration only; no timing assertion). | issue pending (2). |
| S023: [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L66-L66] | Initial zero counter/state; bookkeeping rather than an imputed measurement. | C01 (configuration only; no timing assertion). | issue pending (2). |
| S024: [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L179-L181] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S025: [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L181-L181] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S026: [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L197-L199] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S027: [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L199-L199] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S028: [src/digitalmodel/data_systems/data_scraping/scrapers/drilling_rig_scraper.py:L64-L64] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S029: [src/digitalmodel/data_systems/data_scraping/scrapers/drilling_rig_scraper.py:L78-L78] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S030: [src/digitalmodel/data_systems/data_scraping/scrapers/drilling_rig_scraper.py:L94-L94] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S031: [src/digitalmodel/data_systems/data_scraping/scrapers/drilling_rig_scraper.py:L100-L100] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S032: [src/digitalmodel/data_systems/data_scraping/scrapers/drilling_rig_scraper.py:L153-L153] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S033: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L63-L63] | Already initialized driver returns early; reuse control, no record loss. | no detection check exists. | issue pending (2). |
| S034: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L98-L100] | Driver initialization exception is logged and re-raised; explicit failure control. | no detection check exists. | issue pending (2). |
| S035: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L135-L136] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S036: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L144-L146] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S037: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L146-L146] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S038: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L147-L149] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S039: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L149-L149] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S040: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L164-L164] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S041: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L172-L174] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S042: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L174-L174] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S043: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L215-L217] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S044: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L217-L217] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S045: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L224-L224] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S046: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L231-L231] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S047: [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L247-L248] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S048: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L60-L60] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (3). |
| S049: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L84-L84] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (3). |
| S050: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L110-L110] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (3). |
| S051: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L127-L127] | numeric conversion; unit suffix retained; parser coercion or summary conversion can reject, round or truncate input. | no detection check exists. | issue pending (3). |
| S052: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L132-L132] | numeric conversion; unlabelled dimensions assumed metres; parser coercion or summary conversion can reject, round or truncate input. | no detection check exists. | issue pending (3). |
| S053: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L154-L154] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | C05 (kg identity/scaling only). | issue pending (3). |
| S054: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L161-L161] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | C05 (kg identity/scaling only). | issue pending (3). |
| S055: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L164-L164] | numeric conversion; comma removal; parser coercion or summary conversion can reject, round or truncate input. | C05 (kg identity/scaling only). | issue pending (3). |
| S056: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L167-L167] | mass conversion constants. | C05 (kg identity/scaling only). | issue pending (3). |
| S057: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L178-L178] | unknown mass unit fallback. | C05 (kg identity/scaling only). | issue pending (3). |
| S058: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L191-L191] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (3). |
| S059: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L207-L207] | numeric conversion; load separator removal; parser coercion or summary conversion can reject, round or truncate input. | no detection check exists. | issue pending (3). |
| S060: [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L215-L215] | numeric conversion; generic load separator removal; parser coercion or summary conversion can reject, round or truncate input. | no detection check exists. | issue pending (3). |
| S061: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L90-L90] | diameter unit assumption and zero omission; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S062: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L96-L96] | length unit assumption and zero omission; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S063: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L102-L102] | deflection unit assumption and zero omission; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S064: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L108-L108] | force unit assumption and zero omission; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S065: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L114-L114] | energy unit assumption and zero omission; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S066: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L120-L120] | pressure unit assumption and zero omission; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S067: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L126-L126] | density unit assumption and zero omission; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S068: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L152-L152] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S069: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L159-L159] | small static result triggers fallback. | no detection check exists. | issue pending (2). |
| S070: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L179-L179] | small table threshold. | no detection check exists. | issue pending (2). |
| S071: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L228-L228] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S072: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L247-L247] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S073: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L273-L273] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S074: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L292-L292] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S075: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L298-L298] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S076: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L349-L349] | empty source selection replaced by default. | no detection check exists. | issue pending (2). |
| S077: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L364-L364] | Loop branch continues without adding the current record/result. | no detection check exists. | issue pending (2). |
| S078: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L369-L371] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S079: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L371-L371] | Loop branch continues without adding the current record/result. | no detection check exists. | issue pending (2). |
| S080: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L376-L376] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S081: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L384-L384] | coercion or row removal. | no detection check exists. | issue pending (2). |
| S082: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L408-L408] | empty return; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S083: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L411-L411] | first-number parser discards sign and separators; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S084: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L413-L413] | numeric conversion; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S085: [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L415-L415] | empty return; unit token is not converted and numeric zero is omitted by the consumer. | no detection check exists. | issue pending (3). |
| S086: [src/digitalmodel/data_systems/data_scraping/scrapers/fpso_scraper.py:L56-L56] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S087: [src/digitalmodel/data_systems/data_scraping/scrapers/fpso_scraper.py:L102-L102] | Fallback parser row dictionary is never populated; returns None for every page. | no detection check exists. | issue pending (2). |
| S088: [src/digitalmodel/data_systems/data_scraping/scrapers/fpso_scraper.py:L132-L132] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S089: [src/digitalmodel/data_systems/data_scraping/scrapers/fpso_scraper.py:L139-L139] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S090: [src/digitalmodel/data_systems/data_scraping/scrapers/fpso_scraper.py:L189-L189] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S091: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L65-L65] | small table omitted. | no detection check exists. | issue pending (2). |
| S092: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L66-L66] | Loop branch continues without adding the current record/result. | no detection check exists. | issue pending (2). |
| S093: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L72-L72] | missing header manufactured. | no detection check exists. | issue pending (2). |
| S094: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L76-L76] | coercion or row removal. | no detection check exists. | issue pending (2). |
| S095: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L77-L77] | coercion or row removal. | no detection check exists. | issue pending (2). |
| S096: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L88-L89] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S097: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L113-L114] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S098: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L134-L134] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S099: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L148-L148] | Successful keyword-selected table is appended before continue; no record loss. | no detection check exists. | issue pending (2). |
| S100: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L150-L150] | Successful keyword-selected table is appended before continue; no record loss. | no detection check exists. | issue pending (2). |
| S101: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L154-L154] | numeric-column heuristic. | no detection check exists. | issue pending (2). |
| S102: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L173-L173] | unit labels renamed without conversion; original unit/invalid token is lost in renamed/coerced output. | no detection check exists. | issue pending (3). |
| S103: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L199-L199] | coercion or row removal; original unit/invalid token is lost in renamed/coerced output. | no detection check exists. | issue pending (3). |
| S104: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L221-L221] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S105: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L251-L251] | Duplicate-column suffix counter starts at zero; bookkeeping control. | no detection check exists. | issue pending (2). |
| S106: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L290-L292] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S107: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L292-L292] | Loop branch continues without adding the current record/result. | no detection check exists. | issue pending (2). |
| S108: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L296-L296] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S109: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L320-L320] | zero structure counters returned with error. | no detection check exists. | issue pending (2). |
| S110: [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L344-L345] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (2). |
| S111: [src/digitalmodel/data_systems/data_scraping/scrapers/pipelay_scraper.py:L57-L57] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S112: [src/digitalmodel/data_systems/data_scraping/scrapers/pipelay_scraper.py:L63-L63] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S113: [src/digitalmodel/data_systems/data_scraping/scrapers/pipelay_scraper.py:L92-L92] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S114: [src/digitalmodel/data_systems/data_scraping/scrapers/pipelay_scraper.py:L110-L110] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (2). |
| S115: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L56-L56] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | no detection check exists. | issue pending (3). |
| S116: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L77-L77] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | C02 (ordinary year/missing case only). | issue pending (3). |
| S117: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L84-L84] | numeric conversion; parser coercion or summary conversion can reject, round or truncate input. | C02 (ordinary year/missing case only). | issue pending (3). |
| S118: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L85-L86] | Exception intercepted; subsequent status/return needs explicit scrutiny. | C02 (ordinary year/missing case only). | issue pending (3). |
| S119: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L88-L88] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | C02 (ordinary year/missing case only). | issue pending (3). |
| S120: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L101-L101] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | C04 (ordinary units/scaling only). | issue pending (3). |
| S121: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L106-L106] | first-number unit parsing. | C04 (ordinary units/scaling only). | issue pending (3). |
| S122: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L108-L108] | numeric conversion; parser coercion or summary conversion can reject, round or truncate input. | C04 (ordinary units/scaling only). | issue pending (3). |
| S123: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L111-L111] | feet detection; otherwise metres. | C04 (ordinary units/scaling only). | issue pending (3). |
| S124: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L115-L116] | Exception intercepted; subsequent status/return needs explicit scrutiny. | C04 (ordinary units/scaling only). | issue pending (3). |
| S125: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L118-L118] | Empty/sentinel return; caller must distinguish missing input, no match and extraction failure. | C04 (ordinary units/scaling only). | issue pending (3). |
| S126: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L147-L147] | Deadweight alias becomes displacement without checking the physical quantity. | no detection check exists. | issue pending (3). |
| S127: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L201-L201] | Keeps first duplicate identifier; later differing records are discarded. | no detection check exists. | issue pending (3). |
| S128: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L255-L255] | Year extrema cast to integers; internal summary conversion. | no detection check exists. | issue pending (3). |
| S129: [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L256-L256] | Year extrema cast to integers; internal summary conversion. | no detection check exists. | issue pending (3). |
| S130: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L27-L27] | Missing or unreadable configuration becomes an empty mapping; configured rules are not consumed by validation. | no detection check exists. | issue pending (2). |
| S131: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L44-L46] | Missing or unreadable configuration becomes an empty mapping; configured rules are not consumed by validation. | no detection check exists. | issue pending (2). |
| S132: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L46-L46] | Missing or unreadable configuration becomes an empty mapping; configured rules are not consumed by validation. | no detection check exists. | issue pending (2). |
| S133: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L70-L70] | quality score initial assumption. | C03 (complete/empty cases only). | issue pending (4). |
| S134: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L77-L77] | Initial zero counter/state; bookkeeping rather than an imputed measurement. | C03 (complete/empty cases only). | issue pending (4). |
| S135: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L86-L86] | missing schema penalty. | C03 (complete/empty cases only). | issue pending (4). |
| S136: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L93-L93] | Average includes only columns with missing cells; adding complete columns cannot improve the missing penalty. | no detection check exists. | issue pending (4). |
| S137: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L96-L96] | missing-data policy threshold. | no detection check exists. | issue pending (4). |
| S138: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L105-L105] | duplicate penalty. | no detection check exists. | issue pending (4). |
| S139: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L112-L112] | type penalty. | no detection check exists. | issue pending (4). |
| S140: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L115-L115] | quality floor. | C03 (complete/empty cases only). | issue pending (4). |
| S141: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L118-L118] | quality acceptance threshold. | C03 (complete/empty cases only). | issue pending (4). |
| S142: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L159-L159] | Numeric coercion counts nulls as nonnumeric unless the entire column is null. | no detection check exists. | issue pending (4). |
| S143: [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L274-L274] | Report parses a count from its own formatted issue text; internal conversion, format-coupled. | no detection check exists. | issue pending (4). |
| S144: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L31-L31] | coefficient screening bounds without standard clause. | no detection check exists. | issue pending (5). |
| S145: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L41-L41] | dimension bounds in metres. | no detection check exists. | issue pending (5). |
| S146: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L91-L93] | Exception intercepted; subsequent status/return needs explicit scrutiny. | no detection check exists. | issue pending (4). |
| S147: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L122-L122] | empty input division. | no detection check exists. | issue pending (4). |
| S148: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L140-L140] | empty-cell division. | no detection check exists. | issue pending (4). |
| S149: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L147-L147] | Missing/all-null coefficient or dimension skips the range check; missing coefficient generates only a warning elsewhere. | no detection check exists. | issue pending (4). |
| S150: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L149-L149] | coercion or row removal. | no detection check exists. | issue pending (4). |
| S151: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L151-L151] | Missing/all-null coefficient or dimension skips the range check; missing coefficient generates only a warning elsewhere. | no detection check exists. | issue pending (4). |
| S152: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L163-L163] | Non-null series statistics cast to float; singleton standard deviation can remain non-finite. | no detection check exists. | issue pending (4). |
| S153: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L164-L164] | Non-null series statistics cast to float; singleton standard deviation can remain non-finite. | no detection check exists. | issue pending (4). |
| S154: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L165-L165] | Non-null series statistics cast to float; singleton standard deviation can remain non-finite. | no detection check exists. | issue pending (4). |
| S155: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L166-L166] | Non-null series statistics cast to float; singleton standard deviation can remain non-finite. | no detection check exists. | issue pending (4). |
| S156: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L173-L173] | Missing/all-null coefficient or dimension skips the range check; missing coefficient generates only a warning elsewhere. | no detection check exists. | issue pending (4). |
| S157: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L175-L175] | coercion or row removal. | no detection check exists. | issue pending (4). |
| S158: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L177-L177] | Missing/all-null coefficient or dimension skips the range check; missing coefficient generates only a warning elsewhere. | no detection check exists. | issue pending (4). |
| S159: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L190-L190] | Heading absent: coverage check returns without calculating coverage. | no detection check exists. | issue pending (4). |
| S160: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L191-L191] | Heading absent: coverage check returns without calculating coverage. | no detection check exists. | issue pending (4). |
| S161: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L193-L193] | coercion or row removal. | no detection check exists. | issue pending (4). |
| S162: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L196-L196] | Fractional heading is truncated in the integer summary; only first ten headings shown in unique_values. | no detection check exists. | issue pending (4). |
| S163: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L197-L197] | Fractional heading is truncated in the integer summary; only first ten headings shown in unique_values. | no detection check exists. | issue pending (4). |
| S164: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L198-L198] | Fractional heading is truncated in the integer summary; only first ten headings shown in unique_values. | no detection check exists. | issue pending (4). |
| S165: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L206-L206] | heading median heuristic. | no detection check exists. | issue pending (5). |
| S166: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L223-L223] | Fewer than two key columns: duplicate check returns without checking rows. | no detection check exists. | issue pending (4). |
| S167: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L224-L224] | Fewer than two key columns: duplicate check returns without checking rows. | no detection check exists. | issue pending (4). |
| S168: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L244-L244] | Quality accumulator starts at zero; explicit scoring policy. | no detection check exists. | issue pending (4). |
| S169: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L249-L249] | numeric conversion; completeness weight and truncation; parser coercion or summary conversion can reject, round or truncate input. | no detection check exists. | issue pending (4). |
| S170: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L252-L252] | missing error list defaults empty. | no detection check exists. | issue pending (4). |
| S171: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L256-L256] | warning count default and penalty. | no detection check exists. | issue pending (4). |
| S172: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L257-L257] | warning score floor. | no detection check exists. | issue pending (4). |
| S173: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L260-L260] | quality score ceiling. | no detection check exists. | issue pending (4). |
| S174: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L273-L273] | No CSV files: empty result mapping returned; no dataset failure status. | no detection check exists. | issue pending (4). |
| S175: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L291-L291] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (4). |
| S176: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L294-L294] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (4). |
| S177: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L295-L295] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (4). |
| S178: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L342-L342] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (4). |
| S179: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L345-L345] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (4). |
| S180: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L346-L346] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (4). |
| S181: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L347-L347] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (4). |
| S182: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L407-L407] | Absent result key displayed as zero; unavailable and measured zero become indistinguishable. | no detection check exists. | issue pending (4). |
| S183: [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L410-L410] | all() over empty results succeeds and returns success status. | no detection check exists. | issue pending (4). |

The sweep records local observations, not historical incidents; unresolved detection gaps remain attached to the numbered drafts above.

## 7. Lessons log

| Date | Entry ID | Lesson | Evidence | Pack change | Shared log |
| --- | --- | --- | --- | --- | --- |
| 2026-11-04 | data-scraping-001 | Printed success/failure messages do not establish an asserted offline reference. | [tests/data_systems/data_scraping/test_fender_scraper.py:L18-L84] | Section 4 admits no worked examples; section 10 inventories the demonstration nodes. | Pending, issue pending (1). |
| 2026-11-04 | data-scraping-002 | An empty validation inventory satisfies `all()` unless dataset count is checked separately. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L400-L415] | Sections 5–6 distinguish document evidence from the failed operational test gate. | Pending, issue pending (4). |

## 8. Ownership

Pack review assignments: calculations quality manager for the EC gate set; deputy calculations quality manager as deputy; Python data-pipeline maintainers as producing role. Escalation of a check, input ambiguity or standards attribution goes to the calculations quality manager; cross-family scope goes to the maintainers (AQG r0, section 11).

Status remains DRAFT pending independent review; this record grants no engineering-data qualification (AQG r0, section 11).

## 9. Dependencies and implementation inventory

The base path imports requests, urllib3 retry handling and pandas; HTML/equipment routes use BeautifulSoup, vessel-unit conversion uses `digitalmodel.units.Q_`, general validation uses YAML and Plotly, PDF extraction imports pdfplumber, and dynamic scraping imports Selenium and webdriver-manager. [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L12-L21] [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L8-L13] [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L8-L12] [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L7-L13] [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L8-L13] [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L13-L22]

The dynamic driver initialization fixes a browser binary location and a driver version in code; neither browser execution nor current website compatibility is established by the offline record. [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L87-L100]

CSV and metadata consumers are demonstrated in the package example, and the corpus guide routes coefficient datasets to separate environmental-loading modules; importing those calculation routes is not needed for this pack's checks. [src/digitalmodel/data_systems/data_scraping/examples/scrape_vessels_example.py:L59-L73] [docs/data/OCIMF_CORPUS_README.md:L60-L73]

### Counted map family

Exactly one map family has an implementation row inside the package: `wind-and-current-force-routes`, with 1 in-package row and 135 total rows. It is the sole counted family; no ranking tie or subpackage exclusion arises. [docs/registry/implementation-map.yaml:L15400-L15405] [docs/registry/implementation-map.yaml:L15744-L15753] [docs/registry/implementation-map.yaml:L16782-L16791]

All rows of that family are listed below in map order, including callers, exports and tests as classified by the map; listing a row does not endorse its classification. The four discovery files with identifying names are referenced by stable family ordinal and exact map record instead of repeating their paths; codenaming remains issue pending (7). [docs/registry/implementation-map.yaml:L15400-L15438]

| Family ordinal | Recorded path | Recorded symbol | Recorded source line | Map record at map document pin |
| --- | --- | --- | --- | --- |
| 1 | `Identifying path withheld; issue pending (7)` | `calc_splash_zone` | 312 | [docs/registry/implementation-map.yaml:L15400-L15409] |
| 2 | `Identifying path withheld; issue pending (7)` | `calc_phase_3_splash` | 375 | [docs/registry/implementation-map.yaml:L15410-L15419] |
| 3 | `Identifying path withheld; issue pending (7)` | `<module>` | 51 | [docs/registry/implementation-map.yaml:L15420-L15428] |
| 4 | `Identifying path withheld; issue pending (7)` | `_good_case` | 80 | [docs/registry/implementation-map.yaml:L15429-L15438] |
| 5 | `examples/domains/fpso/fpso_mooring_analysis.py` | `calculate_environmental_forces` | 194 | [docs/registry/implementation-map.yaml:L15439-L15448] |
| 6 | `examples/domains/fpso/fpso_mooring_analysis.py` | `main` | 497 | [docs/registry/implementation-map.yaml:L15449-L15460] |
| 7 | `examples/domains/ocimf/ocimf_demo.py` | `main` | 24 | [docs/registry/implementation-map.yaml:L15461-L15470] |
| 8 | `examples/domains/workflows/01_spread_mooring_8point.py` | `calculate_environmental_forces` | 123 | [docs/registry/implementation-map.yaml:L15471-L15480] |
| 9 | `examples/domains/workflows/03_calm_buoy.py` | `analyze_calm_buoy` | 53 | [docs/registry/implementation-map.yaml:L15481-L15490] |
| 10 | `examples/domains/workflows/05_jackup_installation.py` | `analyze_jackup_mooring` | 37 | [docs/registry/implementation-map.yaml:L15491-L15500] |
| 11 | `examples/domains/workflows/06_floating_wind_turbine.py` | `analyze_floating_wind_mooring` | 37 | [docs/registry/implementation-map.yaml:L15501-L15510] |
| 12 | `examples/domains/workflows/07_wave_energy_converter.py` | `analyze_wec_mooring` | 36 | [docs/registry/implementation-map.yaml:L15511-L15520] |
| 13 | `examples/domains/workflows/08_aquaculture_mooring.py` | `analyze_aquaculture_mooring` | 35 | [docs/registry/implementation-map.yaml:L15521-L15530] |
| 14 | `examples/domains/workflows/09_spm_terminal.py` | `analyze_spm_terminal` | 34 | [docs/registry/implementation-map.yaml:L15531-L15540] |
| 15 | `examples/domains/workflows/10_dp_backup_mooring.py` | `analyze_dp_backup_mooring` | 36 | [docs/registry/implementation-map.yaml:L15541-L15550] |
| 16 | `scripts/capabilities/build_onepagers.py` | `<module>` | 36 | [docs/registry/implementation-map.yaml:L15551-L15559] |
| 17 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `<module>` | 25 | [docs/registry/implementation-map.yaml:L15560-L15568] |
| 18 | `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `extract_ocimf_database` | 220 | [docs/registry/implementation-map.yaml:L15569-L15578] |
| 19 | `scripts/python/digitalmodel/legacy/generate_integration_charts.py` | `<module>` | 40 | [docs/registry/implementation-map.yaml:L15579-L15587] |
| 20 | `scripts/python/digitalmodel/legacy/generate_ocimf_report.py` | `main` | 16 | [docs/registry/implementation-map.yaml:L15588-L15597] |
| 21 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler._setup_test_data` | 112 | [docs/registry/implementation-map.yaml:L15598-L15607] |
| 22 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | 196 | [docs/registry/implementation-map.yaml:L15608-L15617] |
| 23 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.run_all` | 499 | [docs/registry/implementation-map.yaml:L15618-L15629] |
| 24 | `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `main` | 518 | [docs/registry/implementation-map.yaml:L15630-L15639] |
| 25 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | 104 | [docs/registry/implementation-map.yaml:L15640-L15649] |
| 26 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | 853 | [docs/registry/implementation-map.yaml:L15650-L15659] |
| 27 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.export_json_summary` | 1191 | [docs/registry/implementation-map.yaml:L15660-L15669] |
| 28 | `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.run_complete_validation` | 1216 | [docs/registry/implementation-map.yaml:L15670-L15684] |
| 29 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `<module>` | 22 | [docs/registry/implementation-map.yaml:L15685-L15693] |
| 30 | `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `main` | 371 | [docs/registry/implementation-map.yaml:L15694-L15703] |
| 31 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `make_polar_overlay` | 406 | [docs/registry/implementation-map.yaml:L15704-L15713] |
| 32 | `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `parse_args` | 802 | [docs/registry/implementation-map.yaml:L15714-L15723] |
| 33 | `src/digitalmodel/ansys/pressure_vessel.py` | `PressureVesselGenerator.generate_wind_load` | 704 | [docs/registry/implementation-map.yaml:L15724-L15733] |
| 34 | `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` | `SyntheticRopeDatabaseClient._generate_rope_spec` | 171 | [docs/registry/implementation-map.yaml:L15734-L15743] |
| 35 | `src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py` | `main` | 367 | [docs/registry/implementation-map.yaml:L15744-L15753] |
| 36 | `src/digitalmodel/drilling_riser/drift_off.py` | `environmental_force_n` | 97 | [docs/registry/implementation-map.yaml:L15754-L15763] |
| 37 | `src/digitalmodel/drilling_riser/envelope.py` | `CurrentProfile.drag_load_n_per_m` | 108 | [docs/registry/implementation-map.yaml:L15764-L15773] |
| 38 | `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `as_analysed_sections` | 248 | [docs/registry/implementation-map.yaml:L15774-L15783] |
| 39 | `src/digitalmodel/dynmoor_mooring/workflow.py` | `router` | 101 | [docs/registry/implementation-map.yaml:L15784-L15793] |
| 40 | `src/digitalmodel/engine.py` | `engine` | 90 | [docs/registry/implementation-map.yaml:L15794-L15806] |
| 41 | `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_wind_cmd` | 105 | [docs/registry/implementation-map.yaml:L15807-L15816] |
| 42 | `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_current_cmd` | 163 | [docs/registry/implementation-map.yaml:L15817-L15826] |
| 43 | `src/digitalmodel/hydrodynamics/cli.py` | `combined_env_cmd` | 221 | [docs/registry/implementation-map.yaml:L15827-L15836] |
| 44 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_total_viv_fatigue` | 99 | [docs/registry/implementation-map.yaml:L15837-L15846] |
| 45 | `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.tabulate_viv_results` | 114 | [docs/registry/implementation-map.yaml:L15847-L15856] |
| 46 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_total_viv_fatigue` | 176 | [docs/registry/implementation-map.yaml:L15857-L15866] |
| 47 | `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.tabulate_viv_results` | 200 | [docs/registry/implementation-map.yaml:L15867-L15876] |
| 48 | `src/digitalmodel/infrastructure/utils/ETL_components.py` | `ETL_components.transform_viv_current_reformat_1` | 746 | [docs/registry/implementation-map.yaml:L15877-L15886] |
| 49 | `src/digitalmodel/installation/installation_pamphlet/calculations.py` | `compute_splashzone_envelope` | 456 | [docs/registry/implementation-map.yaml:L15887-L15896] |
| 50 | `src/digitalmodel/installation/jumper_installation/calculations.py` | `calc_phase_3_splash` | 163 | [docs/registry/implementation-map.yaml:L15897-L15906] |
| 51 | `src/digitalmodel/marine_ops/marine_analysis/__main__.py` | `main` | 27 | [docs/registry/implementation-map.yaml:L15907-L15916] |
| 52 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | 306 | [docs/registry/implementation-map.yaml:L15917-L15926] |
| 53 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | 628 | [docs/registry/implementation-map.yaml:L15927-L15936] |
| 54 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `create_sample_database` | 861 | [docs/registry/implementation-map.yaml:L15937-L15946] |
| 55 | `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py` | `OCIMFWorkflow.router` | 48 | [docs/registry/implementation-map.yaml:L15947-L15956] |
| 56 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `<module>` | 25 | [docs/registry/implementation-map.yaml:L15957-L15965] |
| 57 | `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `extract_ocimf_database` | 220 | [docs/registry/implementation-map.yaml:L15966-L15975] |
| 58 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler._setup_test_data` | 110 | [docs/registry/implementation-map.yaml:L15976-L15987] |
| 59 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_ocimf_lookup` | 188 | [docs/registry/implementation-map.yaml:L15988-L15997] |
| 60 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.run_all` | 494 | [docs/registry/implementation-map.yaml:L15998-L16009] |
| 61 | `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `main` | 513 | [docs/registry/implementation-map.yaml:L16010-L16019] |
| 62 | `src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` | `main` | 644 | [docs/registry/implementation-map.yaml:L16020-L16029] |
| 63 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` | 104 | [docs/registry/implementation-map.yaml:L16030-L16039] |
| 64 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_statistics_to_csv` | 853 | [docs/registry/implementation-map.yaml:L16040-L16049] |
| 65 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.export_json_summary` | 1191 | [docs/registry/implementation-map.yaml:L16050-L16059] |
| 66 | `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.run_complete_validation` | 1216 | [docs/registry/implementation-map.yaml:L16060-L16074] |
| 67 | `src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py` | `<module>` | 40 | [docs/registry/implementation-map.yaml:L16075-L16083] |
| 68 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_output_dir` | 25 | [docs/registry/implementation-map.yaml:L16084-L16093] |
| 69 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_cli_output_dir` | 34 | [docs/registry/implementation-map.yaml:L16094-L16103] |
| 70 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `load_data` | 55 | [docs/registry/implementation-map.yaml:L16104-L16113] |
| 71 | `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `main` | 448 | [docs/registry/implementation-map.yaml:L16114-L16126] |
| 72 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` | 320 | [docs/registry/implementation-map.yaml:L16127-L16136] |
| 73 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `EnvironmentalForces.calculate_total_forces` | 642 | [docs/registry/implementation-map.yaml:L16137-L16146] |
| 74 | `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `create_sample_database` | 875 | [docs/registry/implementation-map.yaml:L16147-L16156] |
| 75 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._environmental_forces` | 77 | [docs/registry/implementation-map.yaml:L16157-L16168] |
| 76 | `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._ocimf_database_path` | 103 | [docs/registry/implementation-map.yaml:L16169-L16178] |
| 77 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `_resolve_arrow_direction_in_body_frame` | 39 | [docs/registry/implementation-map.yaml:L16179-L16188] |
| 78 | `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `polar_force_overlay` | 273 | [docs/registry/implementation-map.yaml:L16189-L16198] |
| 79 | `src/digitalmodel/naval_architecture/maneuvering_envelope.py` | `threshold_speed_for_steerage` | 211 | [docs/registry/implementation-map.yaml:L16199-L16208] |
| 80 | `src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py` | `run_maneuvering_envelope_sweep` | 233 | [docs/registry/implementation-map.yaml:L16209-L16218] |
| 81 | `src/digitalmodel/orcaflex/installation_analysis.py` | `SplashZoneInput.calculate_splash_zone_loads` | 245 | [docs/registry/implementation-map.yaml:L16219-L16228] |
| 82 | `src/digitalmodel/orcawave/drift_forces.py` | `compute_wind_current_drift` | 224 | [docs/registry/implementation-map.yaml:L16229-L16238] |
| 83 | `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator._extract_configurations` | 104 | [docs/registry/implementation-map.yaml:L16239-L16248] |
| 84 | `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder.build` | 171 | [docs/registry/implementation-map.yaml:L16249-L16258] |
| 85 | `src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py` | `extract_loads` | 12 | [docs/registry/implementation-map.yaml:L16259-L16268] |
| 86 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `calculate_dual_chute_drag` | 81 | [docs/registry/implementation-map.yaml:L16269-L16280] |
| 87 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `assess_all_load_cases` | 131 | [docs/registry/implementation-map.yaml:L16281-L16293] |
| 88 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `simulate_deployment_transient` | 189 | [docs/registry/implementation-map.yaml:L16294-L16304] |
| 89 | `src/digitalmodel/structural/parachute/chute_assessment.py` | `export_results_yaml` | 296 | [docs/registry/implementation-map.yaml:L16305-L16314] |
| 90 | `src/digitalmodel/structural/parachute/parachute_drag.py` | `calculate_drag_force` | 37 | [docs/registry/implementation-map.yaml:L16315-L16324] |
| 91 | `src/digitalmodel/subsea/mooring_analysis/cli.py` | `design_verification` | 145 | [docs/registry/implementation-map.yaml:L16325-L16334] |
| 92 | `src/digitalmodel/subsea/mooring_analysis/designer.py` | `MooringDesigner.calculate_environmental_loads` | 60 | [docs/registry/implementation-map.yaml:L16335-L16344] |
| 93 | `src/digitalmodel/subsea/mooring_analysis/dynmoor.py` | `build_wind_model.force` | 1094 | [docs/registry/implementation-map.yaml:L16345-L16354] |
| 94 | `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_phase_demand` | 194 | [docs/registry/implementation-map.yaml:L16355-L16364] |
| 95 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `hydrodynamic_loads` | 171 | [docs/registry/implementation-map.yaml:L16365-L16374] |
| 96 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `drag_force_per_meter` | 214 | [docs/registry/implementation-map.yaml:L16375-L16384] |
| 97 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lateral_stability_check` | 291 | [docs/registry/implementation-map.yaml:L16385-L16396] |
| 98 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `check_lateral_stability` | 363 | [docs/registry/implementation-map.yaml:L16397-L16408] |
| 99 | `src/digitalmodel/subsea/pipeline/integrity_workflows.py` | `run_on_bottom_stability` | 7 | [docs/registry/implementation-map.yaml:L16409-L16418] |
| 100 | `src/digitalmodel/subsea/viv_analysis/screening.py` | `VIVScreening.screen_with_current_profile` | 278 | [docs/registry/implementation-map.yaml:L16419-L16428] |
| 101 | `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py` | `viv_analysis` | 52 | [docs/registry/implementation-map.yaml:L16429-L16438] |
| 102 | `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py` | `VortexSheddingAnalyzer.analyze_with_profile` | 160 | [docs/registry/implementation-map.yaml:L16439-L16448] |
| 103 | `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_mooring_design_template` | 306 | [docs/registry/implementation-map.yaml:L16449-L16458] |
| 104 | `src/digitalmodel/workflows/workflow_automation/workflows.py` | `MooringSystemDesignWorkflow.create` | 168 | [docs/registry/implementation-map.yaml:L16459-L16468] |
| 105 | `tests/contracts/test_force_units_boundary_contract.py` | `<module>` | 52 | [docs/registry/implementation-map.yaml:L16469-L16477] |
| 106 | `tests/legal/test_published_pages_have_no_internal_paths.py` | `_load_producer` | 147 | [docs/registry/implementation-map.yaml:L16478-L16487] |
| 107 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `sample_database_path` | 30 | [docs/registry/implementation-map.yaml:L16488-L16497] |
| 108 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `ocimf_db` | 50 | [docs/registry/implementation-map.yaml:L16498-L16507] |
| 109 | `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `env_forces` | 56 | [docs/registry/implementation-map.yaml:L16508-L16517] |
| 110 | `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` | 32 | [docs/registry/implementation-map.yaml:L16518-L16528] |
| 111 | `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py` | `TestOCIMFMooringIntegration.test_combined_environmental_forces` | 105 | [docs/registry/implementation-map.yaml:L16529-L16538] |
| 112 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `ocimf_database` | 32 | [docs/registry/implementation-map.yaml:L16539-L16549] |
| 113 | `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `pytest_benchmark_update_json` | 364 | [docs/registry/implementation-map.yaml:L16550-L16559] |
| 114 | `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py` | `test_refactored_polar_signatures_match_fixture` | 72 | [docs/registry/implementation-map.yaml:L16560-L16569] |
| 115 | `tests/solvers/orcaflex/reporting/fixture_helpers.py` | `_build_loads` | 161 | [docs/registry/implementation-map.yaml:L16570-L16579] |
| 116 | `tests/structural/parachute/test_chute_assessment.py` | `TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc` | 136 | [docs/registry/implementation-map.yaml:L16580-L16589] |
| 117 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_200_mph` | 70 | [docs/registry/implementation-map.yaml:L16590-L16600] |
| 118 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_250_mph` | 82 | [docs/registry/implementation-map.yaml:L16601-L16611] |
| 119 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_force_scales_with_v_squared` | 98 | [docs/registry/implementation-map.yaml:L16612-L16622] |
| 120 | `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_result_has_newtons` | 105 | [docs/registry/implementation-map.yaml:L16623-L16633] |
| 121 | `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons` | 173 | [docs/registry/implementation-map.yaml:L16634-L16644] |
| 122 | `tests/structural/parachute/test_parachute_drag.py` | `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs` | 188 | [docs/registry/implementation-map.yaml:L16645-L16655] |
| 123 | `tests/subsea/catenary_riser/conftest.py` | `catenary_environment` | 18 | [docs/registry/implementation-map.yaml:L16656-L16665] |
| 124 | `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py` | `TestLiftForce.test_known_lift_force` | 103 | [docs/registry/implementation-map.yaml:L16666-L16675] |
| 125 | `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestDragForce.test_scenario_1` | 72 | [docs/registry/implementation-map.yaml:L16676-L16685] |
| 126 | `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestLiftForce.test_scenario_1` | 82 | [docs/registry/implementation-map.yaml:L16686-L16695] |
| 127 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_circular_mock` | 145 | [docs/registry/implementation-map.yaml:L16696-L16705] |
| 128 | `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_rectangular_mock` | 170 | [docs/registry/implementation-map.yaml:L16706-L16715] |
| 129 | `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_drag_force_per_meter` | 54 | [docs/registry/implementation-map.yaml:L16716-L16725] |
| 130 | `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_lift_force_per_meter` | 68 | [docs/registry/implementation-map.yaml:L16726-L16735] |
| 131 | `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` | 56 | [docs/registry/implementation-map.yaml:L16736-L16747] |
| 132 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `hydrodynamic_force_per_meter` | 53 | [docs/registry/implementation-map.yaml:L16748-L16758] |
| 133 | `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `lift_force_per_meter` | 90 | [docs/registry/implementation-map.yaml:L16759-L16769] |
| 134 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `lift_force_per_meter` | 237 | [docs/registry/implementation-map.yaml:L16770-L16781] |
| 135 | `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `inertia_force_per_meter` | 268 | [docs/registry/implementation-map.yaml:L16782-L16791] |

### Exact-signal package search

Search recipe: load `search_signals` from the pinned map; for each pattern, omit it only when `re.search(pattern, 'data_systems/data_scraping', re.I)` matches; otherwise search every package `.py` file with case-sensitive Python `re.finditer`. Assign each matching line to the smallest enclosing AST function/class; assign other lines to one module definition. No pattern matched the package path, so all three patterns were retained.

```python
'(?:0\\.5|1\\s*/\\s*2).*(?:rho|density).*(?:cd|drag|area)|(?:wind|current|drag|lift)_forces?|calculate_(?:wind|current)|OCIMF|current_profile|wind_height_coefficient'
'OCIMF|wind.load|current.load|wind.force|current.force|drag.coefficient|current.profile'
'Cxw|Cyw|CXw|CYw|CXc|CYc|drag_coefficient|lift_coefficient|wind_speed|current_velocity'
```

| Matching definition | Classification | Evidence at code pin |
| --- | --- | --- |
| `<module>` | Module docstring only. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L1-L419] |
| `OCIMFValidator` | Validation/data/report helper, listed in section 3; computes no wind/current force. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L16-L364] |
| `OCIMFValidator.__init__` | Validation/data/report helper, listed in section 3; computes no wind/current force. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L19-L45] |
| `OCIMFValidator.validate_dataset` | Validation/data/report helper, listed in section 3; computes no wind/current force. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L47-L95] |
| `OCIMFValidator.validate_all` | Validation/data/report helper, listed in section 3; computes no wind/current force. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L262-L311] |
| `OCIMFValidator.generate_report` | Validation/data/report helper, listed in section 3; computes no wind/current force. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L313-L364] |
| `main` | Listed CLI wrapper; invokes validation and report generation. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L367-L415] |
| `OCIMFValidator._validate_columns` | Validation/data/report helper, listed in section 3; computes no wind/current force. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L97-L113] |
| `OCIMFValidator._validate_completeness` | Validation/data/report helper, listed in section 3; computes no wind/current force. | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L115-L141] |

Search result: 9 definitions, 0 further second implementations; A8 passes the inventory/search criterion. This result concerns duplicate detection, not correctness of the validator's screening assumptions. [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L19-L95] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L367-L415]

### Repository search and map follow-up

The same three patterns were searched across all tracked Python files, including `scripts/`, yielding 342 matching files at the code pin; hits outside this package are map follow-up, outside the counted package result. The repository contains coefficient extractors and environmental force calculations in separate routes. [scripts/python/digitalmodel/legacy/extract_ocimf_database.py:L30-L41] [src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py:L560-L628] [src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py:L577-L645]

The map's in-package `main` row describes force evaluation, but this function launches CSV validation and reporting; classification requires correction. Pipeline families for HTTP retries/table extraction, numeric/unit normalization, PDF table extraction, dataset scoring and provenance export have no in-package implementation rows in this map snapshot. They remain uncounted until mapped; issue pending (7). [docs/registry/implementation-map.yaml:L15744-L15753] [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L86-L102] [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L183-L276] [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L66-L118] [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L41-L94] [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L65-L121]

## 10. Coverage and open items

### Reading inventory

Every Python file below was read at the code pin; package folders contain scrapers, validators and one example script. [src/digitalmodel/data_systems/data_scraping/__init__.py:L7-L10] [src/digitalmodel/data_systems/data_scraping/README.md:L31-L46]

| File read | Pinned coverage |
| --- | --- |
| `__init__.py` | [src/digitalmodel/data_systems/data_scraping/__init__.py:L1-L31] |
| `cli.py` | [src/digitalmodel/data_systems/data_scraping/cli.py:L1-L286] |
| `examples/scrape_vessels_example.py` | [src/digitalmodel/data_systems/data_scraping/examples/scrape_vessels_example.py:L1-L235] |
| `process_all_fender_pdfs.py` | [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L1-L183] |
| `scrapers/__init__.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/__init__.py:L1-L23] |
| `scrapers/base_scraper.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L1-L294] |
| `scrapers/drilling_rig_scraper.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/drilling_rig_scraper.py:L1-L186] |
| `scrapers/dynamic_scraper.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/dynamic_scraper.py:L1-L256] |
| `scrapers/equipment_scraper.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/equipment_scraper.py:L1-L472] |
| `scrapers/fender_scraper.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L1-L415] |
| `scrapers/fpso_scraper.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/fpso_scraper.py:L1-L213] |
| `scrapers/pdf_extractor.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L1-L347] |
| `scrapers/pipelay_scraper.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/pipelay_scraper.py:L1-L110] |
| `scrapers/vessel_scraper.py` | [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L1-L262] |
| `validators/__init__.py` | [src/digitalmodel/data_systems/data_scraping/validators/__init__.py:L1-L5] |
| `validators/data_validator.py` | [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L1-L352] |
| `validators/validate_ocimf_data.py` | [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L1-L419] |
| `src/digitalmodel/data_systems/data_scraping/README.md` | [src/digitalmodel/data_systems/data_scraping/README.md:L1-L403] |
| `docs/domains/data_systems/data_scraping/README.md` | [docs/domains/data_systems/data_scraping/README.md:L1-L17] |
| `docs/data/OCIMF_CORPUS_README.md` | [docs/data/OCIMF_CORPUS_README.md:L1-L122] |
| `docs/registry/implementation-map.md` | [docs/registry/implementation-map.md:L1-L71] |
| `docs/plans/2026-08-05-issue-1977-norecursedirs-bare-basenames.html` | [docs/plans/2026-08-05-issue-1977-norecursedirs-bare-basenames.html:L1-L225] |

Documentation discovery used `rg -n -i 'data.scraping|data_scraping' docs --glob '*.md' --glob '*.rst' --glob '*.html'`; it found the four external documentation files above. The domain README has placeholder overview/API text; the package README's referenced strategy document and installation requirements file do not occur at their cited locations. Documentation reconciliation remains issue pending (6). [docs/domains/data_systems/data_scraping/README.md:L5-L17] [src/digitalmodel/data_systems/data_scraping/README.md:L19-L23] [src/digitalmodel/data_systems/data_scraping/README.md:L394-L399]

### Module test inventory and execution record

`PYTHONPATH=src uv run --no-sources python -m pytest tests/data_systems/data_scraping -q -rA -p no:cacheprovider --basetemp=<external-directory>` was used at the code pin, without an addopts override. BASE and AFTER collection, identical: 0 tests run, 0 passed, 0 failed test bodies, 0 skipped, 4 collection errors; 11 declared bodies NOT_RUN because `ModuleNotFoundError: No module named 'scrapers'` prevents collection. This command explicitly targets the folder excluded from recursive discovery. [pytest.ini:L15-L18] [tests/data_systems/data_scraping/test_direct_manufacturers.py:L7-L11] [tests/data_systems/data_scraping/test_selenium.py:L11-L15]

| Existing test node ID | Offline outcome | Source definition |
| --- | --- | --- |
| `tests/data_systems/data_scraping/test_direct_manufacturers.py::test_pacific_marine` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_direct_manufacturers.py:L14-L65] |
| `tests/data_systems/data_scraping/test_direct_manufacturers.py::test_marine_fenders_intl` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_direct_manufacturers.py:L68-L101] |
| `tests/data_systems/data_scraping/test_direct_manufacturers.py::test_trelleborg` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_direct_manufacturers.py:L104-L139] |
| `tests/data_systems/data_scraping/test_fender_scraper.py::test_nauticexpo_fenders` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_fender_scraper.py:L18-L84] |
| `tests/data_systems/data_scraping/test_fender_scraper.py::test_marine_fenders_intl` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_fender_scraper.py:L87-L125] |
| `tests/data_systems/data_scraping/test_fender_scraper.py::test_combined_scraping` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_fender_scraper.py:L128-L176] |
| `tests/data_systems/data_scraping/test_pdf_extraction.py::test_single_pdf` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_pdf_extraction.py:L13-L79] |
| `tests/data_systems/data_scraping/test_pdf_extraction.py::test_design_manual` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_pdf_extraction.py:L82-L123] |
| `tests/data_systems/data_scraping/test_pdf_extraction.py::test_multiple_pdfs` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_pdf_extraction.py:L126-L180] |
| `tests/data_systems/data_scraping/test_selenium.py::test_selenium_fpso` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_selenium.py:L18-L63] |
| `tests/data_systems/data_scraping/test_selenium.py::test_selenium_pipelay` | NOT_RUN: baseline collection error, bare `scrapers` import. | [tests/data_systems/data_scraping/test_selenium.py:L66-L102] |

The three PDF demonstrations need local catalog fixtures and can return before doing work when files are absent; the remaining demonstrations reach live websites, with browser use explicit or a possible fallback. Those behaviours were read, not exercised after the import failure. [tests/data_systems/data_scraping/test_pdf_extraction.py:L19-L24] [tests/data_systems/data_scraping/test_pdf_extraction.py:L88-L92] [tests/data_systems/data_scraping/test_pdf_extraction.py:L132-L148] [tests/data_systems/data_scraping/test_direct_manufacturers.py:L20-L32] [tests/data_systems/data_scraping/test_selenium.py:L24-L35] [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L158-L170]

### Open items

These are maintainer follow-ups; draft numbers identify the accompanying issue-draft list until issue numbers are assigned.

| Item | Coverage gap / required action | Tracking |
| --- | --- | --- |
| Offline examples and test harness | Repair package imports; use synthetic HTML/PDF fixtures, explicit assertions, missing-fixture status and independent expected results before admitting worked examples; record the shared lesson. | #1900; #1977 for traversal history; issue pending (1). [tests/data_systems/data_scraping/test_pdf_extraction.py:L7-L24] [tests/data_systems/data_scraping/test_fender_scraper.py:L39-L84] |
| Error and completeness contracts | Distinguish empty, partial and failed extraction; reject empty post-cleaning publication; fix no-input batch arithmetic and resource cleanup; cover all section-6 rows assigned to this item. | issue pending (2). [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L88-L94] [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L241-L247] [src/digitalmodel/data_systems/data_scraping/process_all_fender_pdfs.py:L73-L79] |
| Numeric and unit preservation | Cover separator/sign parsing, zero-valued fields, deadweight/displacement distinctions, schema unit conversions and numeric coercion. | issue pending (3). [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L103-L118] [src/digitalmodel/data_systems/data_scraping/scrapers/vessel_scraper.py:L147-L148] [src/digitalmodel/data_systems/data_scraping/scrapers/fender_scraper.py:L119-L129] [src/digitalmodel/data_systems/data_scraping/scrapers/pdf_extractor.py:L173-L199] |
| Dataset quality semantics | Define missing-column averaging, warning validity, finite/empty statistics, heading precision/coverage and nonempty validation inventory. | issue pending (4). [src/digitalmodel/data_systems/data_scraping/validators/data_validator.py:L88-L119] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L145-L209] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L400-L415] |
| Standards provenance | UNVERIFIED: edition/clause/table provenance of screening ranges and heading-interval policy, plus absence of a corresponding citation sidecar; resolve against an authorized source before using these as engineering acceptance limits. Clause text not checked against the licensed copy. | issue pending (5). [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L30-L45] [src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py:L201-L209] |
| Documentation and provenance | Reconcile stale API/setup references, metadata suffix, output directory nesting and source/unit evidence; no private values or sidecar entries are asserted here. | issue pending (6). [src/digitalmodel/data_systems/data_scraping/README.md:L19-L46] [src/digitalmodel/data_systems/data_scraping/README.md:L228-L240] [src/digitalmodel/data_systems/data_scraping/examples/scrape_vessels_example.py:L47-L51] [src/digitalmodel/data_systems/data_scraping/scrapers/base_scraper.py:L220-L223] |
| Map family coverage and source identities | Correct the validator CLI classification, add pipeline method families, review repository hits outside the package, and codename four discovery files with identifying names before including their paths in public pack prose. | issue pending (7). [docs/registry/implementation-map.yaml:L15400-L15438] [docs/registry/implementation-map.yaml:L15744-L15753] |
