# Browser module knowledge pack

| Header | Value |
| --- | --- |
| Module | solvers/orcaflex/browser |
| Repository | digitalmodel |
| Path | src/digitalmodel/solvers/orcaflex/browser |
| Pack revision | 0 |
| Code revision | b47e42fbe7208c69b28dd436aa91fb129bc385f7 |
| Analysis family | OrcaFlex riser and mooring; results browsing |
| Quality manager role | Riser and mooring quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The Flask backend discovers local result folders, reads CSV tables, provides Excel-derived filter configuration, and selects a file configuration with the largest absolute strut-force value. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L50-L82]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L183-L286]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L562-L622]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L776]`

The root route serves the sixth dashboard variant; `/v5` and two debug routes serve other HTML pages. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L30-L48]`

The routing registry assigns beta maturity to the enclosing `solvers` domain, rather than recording an independent browser maturity. [docs/registry/module-routing.yaml:L477-L483]

Scope includes all three Python files and the backend-facing route of the served dashboard; independent qualification of the other dashboard variants is an open item in section 10. `[src/digitalmodel/solvers/orcaflex/browser/__init__.py:L1-L1]` `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L20-L40]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L30-L48]`

## 2. Inputs and outputs

### Inputs

| Name | Unit | Range or constraint | Default | Source and evidence |
| --- | --- | --- | --- | --- |
| `PARENT_DIR`, `BASE_DIR` | filesystem locator | Existing directories; model suffixes are discovered as names only | Placeholder roots | `get_subfolders` lists model basenames without loading a model. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L23-L28]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L50-L79]` |
| `subfolder` | text | Required by both CSV routes and the maximum-force route | Absent request rejected | Query argument appended to the result directory. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L183-L200]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L290-L305]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L778-L790]` |
| CSV files | source column units | pandas-inferred dtypes; maximum scan accepts `float64` and `int64` force columns | First matching or fallback file | `pd.read_csv` receives no explicit units, decimal or thousands options. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L226-L256]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L675]` |
| `vesselType`, `tide`, `heading`, `envType` | text; heading token | Filename scoring rather than an input schema | Category, tide, heading and environment literals embedded in the function | Category-specific loading and capacity defaults are filters, not a design basis. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L290-L328]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L339-L373]` |
| Collation workbooks | metadata, not physical measurements | Lowercase or uppercase discovery glob; optional `inputs`, `r_inputs`, `case_matrix` sheets | Empty structures or fallback configuration | Reader looks below `postproc/collated`. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L23-L40]` `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L105-L136]` |
| Host and debug options | address; boolean | Explicit CLI selections | Loopback; false; port 5000 | `run_server` warns on exposed debug mode. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L987-L1011]` |

### Outputs

| Name | Unit | Range or convention | Default or error behaviour | Evidence |
| --- | --- | --- | --- | --- |
| `/api/files` response | size in bytes; other fields text | CSV names, paths and filename-derived parameters | Missing directory: HTTP 404; other exception: HTTP 500 | `get_files` constructs one metadata entry per discovered CSV. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L84-L181]` |
| `/api/load_csv` response | source units | Arrays grouped as lines, struts, jacket or raw numeric columns | First column treated as time if no recognised time column | Duration is the last time value; short arrays use a fixed `dt`. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L234-L288]` |
| `/api/data` response | source units; summary index in rows | Summary numeric categories or time-series arrays | Object/string summary columns skipped | Summary time is `range(len(df))`, not physical elapsed time. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L399-L474]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L537-L555]` |
| `/api/max_strut_force` response | source force units; processing time in seconds | Largest absolute eligible component; file and configuration metadata | Zero-only files produce no result; no aggregate result: HTTP 404 | Magnitude selection is not a vector resultant. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L634-L685]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L951-L985]` |
| Excel configuration routes | metadata text | Options, filename patterns, case matrix, refresh timestamp | Partial, empty or default values can be returned after errors | Cache and endpoint fallbacks require the cautions in section 6. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L274-L338]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L562-L622]` |

### Public definition inventory

| Public symbol | Source range at the code pin |
| --- | --- |
| `ExcelCollationReader` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L20-L339]` |
| `ExcelCollationReader.find_collation_files` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L30-L40]` |
| `ExcelCollationReader.parse_filename_pattern` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L42-L103]` |
| `ExcelCollationReader.parse_excel_config` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L105-L136]` |
| `ExcelCollationReader.get_vessel_types` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L224-L272]` |
| `ExcelCollationReader.get_configuration_for_vessel` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L274-L316]` |
| `ExcelCollationReader.refresh_cache` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L318-L327]` |
| `ExcelCollationReader.get_all_configurations` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L329-L339]` |
| `get_excel_config` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L364-L378]` |
| `index` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L31-L33]` |
| `index_v5` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L36-L38]` |
| `debug` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L41-L43]` |
| `debug_enhanced` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L46-L48]` |
| `get_subfolders` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L51-L82]` |
| `get_files` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L85-L181]` |
| `load_csv` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L184-L288]` |
| `get_data` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L291-L555]` |
| `test` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L558-L560]` |
| `get_excel_configuration` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L563-L570]` |
| `get_vessel_types` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L573-L583]` |
| `get_vessel_configuration` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L586-L601]` |
| `refresh_configuration` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L604-L611]` |
| `get_file_patterns` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L614-L622]` |
| `process_single_strut_file` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L776]` |
| `get_max_strut_force_config` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L779-L985]` |
| `run_server` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L987-L997]` |

UNVERIFIED: the physical force unit of an arbitrary CSV is not established by this reader; the served page labels the selected maximum as N without converting the input. Issue pending (2). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L685]` [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v6-clean.html:L704-L713]

UNVERIFIED: coordinate origin, axis direction, force sign and phase conventions require an upstream dataset contract; the time-series reader copies arrays without those declarations. Issue pending (2). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L465-L547]`

## 3. Method and governing standards

| Method | Implementation and limits | Governing basis |
| --- | --- | --- |
| Workbook configuration | Filename token extraction, optional sheet parsing, merging and a time-limited in-memory cache | Application rules; no engineering equation or standards-derived constant is implemented in these methods. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L42-L103]` `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L105-L222]` `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L274-L338]` |
| CSV selection | `load_csv` tests literal filters; `get_data` scores filename fragments, preferring non-summary CSVs | Application heuristics; unmatched requests can select a different file. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L199-L235]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L329-L379]` |
| Result categorisation | Summary dtypes and column/filename substrings select response categories | No physical reanalysis; summary indexes and time-series columns have different meanings. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L417-L547]` |
| Maximum force | `process_single_strut_file` takes absolute component maxima; `get_max_strut_force_config` reduces positive results from workers | No interpolation, vector norm, resistance check or solver launch in this path. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L776]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L827-L849]` |

AQG r0, sections 3–10, supplies the workflow gates below; it does not supply physical acceptance criteria for the browser's filename heuristics. (AQG r0, sections 3–10.)

## 4. Validated worked examples

Validated engineering examples: 0; no independent expected force, solver response or benchmark dataset is supplied by the launch assertions below. The examples are regression checks of launch settings, using extracted production syntax and test doubles rather than a running server. `[tests/contracts/test_development_server_settings.py:L23-L44]` `[tests/contracts/test_development_server_settings.py:L74-L113]`

| Regression case | Input and expected value | Tolerance and basis | Existing test node ID | Offline result at code revision |
| --- | --- | --- | --- | --- |
| Default bind | No override; loopback address | Exact equality; 0 assertion failures (count) | `tests/contracts/test_development_server_settings.py::test_default_host_is_loopback[browser]` | PASS at BASE and AFTER. `[tests/contracts/test_development_server_settings.py:L74-L76]` |
| Explicit bind A | Explicit wildcard address retained | Exact equality; 0 assertion failures (count) | `tests/contracts/test_development_server_settings.py::test_host_requires_explicit_selection[0.0.0.0-browser]` | PASS at BASE and AFTER. `[tests/contracts/test_development_server_settings.py:L79-L82]` |
| Explicit bind B | Explicit documentation address retained | Exact equality; 0 assertion failures (count) | `tests/contracts/test_development_server_settings.py::test_host_requires_explicit_selection[192.0.2.10-browser]` | PASS at BASE and AFTER. `[tests/contracts/test_development_server_settings.py:L79-L82]` |
| Default debug | No override; false | Exact equality; 0 assertion failures (count) | `tests/contracts/test_development_server_settings.py::test_debug_is_off_by_default[browser]` | PASS at BASE and AFTER. `[tests/contracts/test_development_server_settings.py:L85-L87]` |
| Explicit debug | True and false retained | Exact equality; 0 assertion failures (count) | `tests/contracts/test_development_server_settings.py::test_debug_requires_explicit_selection[browser]` | PASS at BASE and AFTER. `[tests/contracts/test_development_server_settings.py:L90-L93]` |
| CLI default | No flags; loopback and false | Exact equality; 0 assertion failures (count) | `tests/contracts/test_development_server_settings.py::test_browser_command_line_selects_host_and_debug[arguments0-127.0.0.1-False]` | PASS at BASE and AFTER. `[tests/contracts/test_development_server_settings.py:L96-L113]` |
| CLI override | Explicit host and debug flags retained | Exact equality; 0 assertion failures (count) | `tests/contracts/test_development_server_settings.py::test_browser_command_line_selects_host_and_debug[arguments1-192.0.2.10-True]` | PASS at BASE and AFTER. `[tests/contracts/test_development_server_settings.py:L96-L113]` |
| Exposed debug A | Wildcard debug emits RuntimeWarning | Exact equality; 0 assertion failures (count) | `tests/contracts/test_development_server_settings.py::test_browser_warns_when_debug_is_exposed[0.0.0.0]` | PASS at BASE and AFTER. `[tests/contracts/test_development_server_settings.py:L138-L143]` |
| Exposed debug B | Documentation-address debug emits RuntimeWarning | Exact equality; 0 assertion failures (count) | `tests/contracts/test_development_server_settings.py::test_browser_warns_when_debug_is_exposed[192.0.2.10]` | PASS at BASE and AFTER. `[tests/contracts/test_development_server_settings.py:L138-L143]` |

Execution record: `PYTHONPATH=src uv run --no-sources python -m pytest tests/contracts/ tests/docs/test_digitalmodel_routing_contract.py -q -rA -p no:cacheprovider --basetemp=<scratch>`; both runs use the pinned code, the repository addopts unchanged, and external scratch output. Counts are recorded in the change description; the nine browser cases above are part of that command. `[tests/contracts/test_development_server_settings.py:L17-L44]` `[tests/contracts/test_development_server_settings.py:L74-L143]`

## 5. QA gates

The family gate set is G0, G1, G2, G3 and G5; G4 is conditional on new diffraction-derived vessel data. A browser result does not attest that any upstream model passed these gates. (AQG r0, sections 3–10.)

The following are executable review procedures, not claims that the browser implements an analysis acceptance system. Downstream gate records and synthetic ingestion tests remain an open item, issue pending (5). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L399-L415]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L951-L978]`

| Gate | Applicability | Check ID and executable procedure | Numeric pass criterion and unit | Evidence and result |
| --- | --- | --- | --- | --- |
| G0 | Applicable to result selection and configuration | B0-LAUNCH: run all nine browser launch nodes in section 4 | 9 passed tests; 0 assertion failures (count) | PASS; source-only tests do not attest ingestion. `[tests/contracts/test_development_server_settings.py:L74-L143]` |
| G0 | Applicable to input provenance | B0-INPUT: compare each selected CSV and workbook with its upstream manifest, column schema, units and fingerprint; require explicit selection rather than an unrelated fallback | 0 unmatched files, 0 undeclared columns, 0 unit mismatches (count) | NOT_RUN: no qualified manifest or dataset supplied; current fallback branches are documented below. Issue pending (2). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L199-L235]` `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L105-L136]` |
| G0 | Applicable to pack completeness | B0-SWEEP: rerun the exact sweep below and reconcile every candidate against a row or an explicit exclusion | 0 undocumented failure paths (count) | Source review recorded below; does not constitute an ingestion test. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L133-L204]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L568-L622]` |
| G1 | Applicable upstream; browser has no model writer/load route | B1-LOAD: trace the selected result basename to the originating model and inspect its target-version load record; the program-specific route is OrcaFlex target-version demo loading where qualified, otherwise licensed `Model.LoadData` | 0 load errors; 0 load warnings; 0 version mismatches (count) | NOT_RUN: licensed program and originating model evidence absent; no solver interface was used for this pack. (AQG r0, section 5.) `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L657-L691]` |
| G2 | Applicable upstream; maximum absolute component is not statics QA | B2-STATIC: trace the selected `force_column` and model to a converged statics/component record; compare its displayed value with the same source column and units | 0 untraced components (count); 0 difference in source force units between displayed and source value | NOT_RUN: statics, independent hand reference and qualified result fixture absent; the browser only reduces existing columns. Issue pending (5). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L634-L685]` (AQG r0, section 6.) |
| G3 | Applicable upstream; arrays alone do not attest a dynamic solve | B3-DYNAMIC: compare each displayed response array length with `time`; check finite values and the originating short-run record, using the production time step | 0 length mismatches (samples), 0 nonfinite values (count); 1 s run reaches its stop time | NOT_RUN: originating dynamics record and fixture absent; summary indexes must not be interpreted as seconds. Issue pending (5). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L460-L474]` (AQG r0, section 7.) |
| G4 | NOT_APPLICABLE to browser calculations; conditional upstream record required for a new diffraction source | B4-RECORD: when a selected dataset incorporates a new diffraction run, inspect the upstream G4 record identifier | 0 missing applicable records (count) | No mesh or diffraction calculation in the inspected maximum-force path; no such dataset qualified here. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L776]` (AQG r0, sections 8 and 10.) |
| G5 | Applicable to any report using browser results | B5-TRACE: compare every exported or reported response with a signed gate record and the recomputed source fingerprint | 0 untraced responses; 0 missing signatures (count) | NOT_RUN: no report or signed source record supplied; endpoint metadata lacks this evidence. Issue pending (5). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L409-L415]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L951-L978]` (AQG r0, section 9.) |

## 6. Known failure modes and pitfalls

sweep: 62 found

The count covers separately located degradations, fallbacks and unit assumptions, rather than unique root causes; related locations share draft issues. The Python and HTML sweeps are text-only. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L133-L204]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L568-L622]`

Reproduce the candidate searches from the pinned tree, then inspect the enclosing definitions (case-sensitive unless the command says otherwise):

```sh
rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|\bint\(|\bfloat\(' src/digitalmodel/solvers/orcaflex/browser --glob '*.py'
rg -ni 'cache|default|fallback|assum|unit|seconds|percent' src/digitalmodel/solvers/orcaflex/browser --glob '*.py'
rg -n 'catch|continue|return null|return \[\]|return \{\}|parseFloat|parseInt|Math.random|Time \(s\)|Force \(|designLimit|tideFactor|utilization|1970' src/digitalmodel/solvers/orcaflex/browser --glob '*.html'
```

| ID | Symptom and cause at the pin | Detection check | Tracking |
| --- | --- | --- | --- |
| S01 | Missing collation directory returns an empty discovery list. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L35-L40]` | no detection check exists | issue pending (1) |
| S02 | Only all-lowercase and all-uppercase globs are searched; mixed-case names can be omitted. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L35-L37]` | no detection check exists | issue pending (1) |
| S03 | Unrecognised filename fields remain UNKNOWN or None. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L50-L59]` | no detection check exists | issue pending (1) |
| S04 | Two digit-token conversions become percentage text without a loading-range check. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L70-L75]` | no detection check exists | issue pending (1) |
| S05 | Capacity suffix and loading labels are inferred from filename tokens, not workbook units. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L83-L95]` | no detection check exists | issue pending (1) |
| S06 | Absent optional sheets retain empty structures without a missing-sheet status. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L119-L131]` | no detection check exists | issue pending (1) |
| S07 | Workbook parse exception logs an error and returns partial configuration. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L133-L136]` | no detection check exists | issue pending (1) |
| S08 | Unexpected inputs schema or caught iteration error returns empty or partial parameter options. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L149-L166]` | no detection check exists | issue pending (1) |
| S09 | Unsupported path cells and caught errors leave incomplete filename patterns. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L174-L187]` | no detection check exists | issue pending (1) |
| S10 | NaN entries and empty rows are omitted; caught errors return partial cases. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L193-L204]` | no detection check exists | issue pending (1) |
| S11 | No recognised discovery type produces fabricated default options. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L250-L263]` | no detection check exists | issue pending (1) |
| S12 | No matching metadata produces a truthy dictionary of empty options. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L274-L316]` | no detection check exists | issue pending (1) |
| S13 | Refresh caches empty configurations and assigns a refresh timestamp. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L318-L325]` | no detection check exists | issue pending (1) |
| S14 | Timedelta.seconds ignores whole days when deciding cache age. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L329-L333]` | no detection check exists | issue pending (1) |
| S15 | Fallback configuration embeds demonstration options without source provenance. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L343-L361]` | no detection check exists | issue pending (1) |
| S16 | Empty outer config or a caught exception returns defaults; empty per-type dictionaries can bypass the outer emptiness test. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L364-L378]` | no detection check exists | issue pending (1) |
| S17 | Basename falls back to the first filename token or the first matching parent name. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L149-L162]` | no detection check exists | issue pending (2) |
| S18 | No exact CSV match falls back to the first discovered file. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L226-L230]` | no detection check exists | issue pending (2) |
| S19 | No recognised time field treats the first column as time. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L248-L256]` | no detection check exists | issue pending (2) |
| S20 | Only selected numeric dtypes enter raw_data; empty time gives zero duration and a short time array gives fixed dt. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L273-L283]` | no detection check exists | issue pending (2) |
| S21 | Request defaults and loading-code mapping can choose a configuration not explicitly requested; some category filters are read without being scored. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L294-L328]` | no detection check exists | issue pending (2) |
| S22 | Zero initial score, weighted filename scoring and ties can return an unrelated first CSV. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L339-L373]` | no detection check exists | issue pending (2) |
| S23 | Object/string columns are skipped, including numbers read as text. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L426-L428]` | no detection check exists | issue pending (2) |
| S24 | Summary row indexes become time and row count becomes duration. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L460-L463]` | no detection check exists | issue pending (2) |
| S25 | First column can stand for time while column exclusion recognises only fixed time names. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L467-L479]` | no detection check exists | issue pending (2) |
| S26 | Fallback categories exclude other dtypes; duration is last time, not elapsed interval; single-sample dt is fixed. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L537-L547]` | no detection check exists | issue pending (2) |
| S27 | Excel-config endpoint returns default JSON after an exception without an error status. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L565-L570]` | no detection check exists | issue pending (1) |
| S28 | Empty types or endpoint errors return default type options. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L575-L583]` | no detection check exists | issue pending (1) |
| S29 | Empty vessel configuration may select defaults; caught errors return an empty dictionary. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L588-L601]` | no detection check exists | issue pending (1) |
| S30 | Missing patterns or caught endpoint errors return an empty pattern list. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L616-L622]` | no detection check exists | issue pending (1) |
| S31 | Zero initial maximum and numeric-dtype filtering omit text, nonfinite and zero-only force evidence. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L634-L649]` | no detection check exists | issue pending (3) |
| S32 | Time-series reduction drops force sign by taking an absolute maximum. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L670-L676]` | no detection check exists | issue pending (3) |
| S33 | Positive-only output condition suppresses all-zero results; float conversion serialises a numeric reduction rather than parsing external text. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L677-L683]` | no detection check exists | issue pending (3) |
| S34 | Loading tokens map to paired defaults and a summary forces a category without validating a source manifest. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L701-L745]` | no detection check exists | issue pending (3) |
| S35 | Heading normalisation strips zero padding and supplies a zero token. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L729-L756]` | no detection check exists | issue pending (3) |
| S36 | No positive force or a caught read/reduction error returns None. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L770-L776]` | no detection check exists | issue pending (3) |
| S37 | A missing strut set silently substitutes jacket files for the scan. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L806-L814]` | no detection check exists | issue pending (3) |
| S38 | Worker failures are logged and omitted from the aggregate; zero initial reduction can conceal incomplete coverage. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L829-L849]` | no detection check exists | issue pending (3) |
| S39 | An unmatched sim_basename does not enter the following configuration-matching branch. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L859-L879]` | no detection check exists | issue pending (3) |
| S40 | Related-file fallback matches loading, tide and heading without an environment equality requirement. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L879-L915]` | no detection check exists | issue pending (3) |
| S41 | Response summary can supply a default heading token for an unspecified heading. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L969-L976]` | no detection check exists | issue pending (3) |
| S42 | Synthetic force history uses tide and heading constants plus random noise. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser.html:L481-L526] | no detection check exists | issue pending (4) |
| S43 | Utilisation and status labels use an assumed design limit and fixed thresholds. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser.html:L732-L752] | no detection check exists | issue pending (4) |
| S44 | Insight calculations reuse an assumed limit and parse heading text without a numeric contract. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser.html:L778-L826] | no detection check exists | issue pending (4) |
| S45 | Synthetic line, strut and jacket histories use fixed factors, random noise and heading text conversions. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v2.html:L638-L748] | no detection check exists | issue pending (4) |
| S46 | Utilisation uses an example breaking load and fixed status thresholds. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v2.html:L1051-L1070] | no detection check exists | issue pending (4) |
| S47 | Synthetic chart data use random terms, fixed tide factors and force/time labels. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v3.html:L269-L375] | no detection check exists | issue pending (4) |
| S48 | Chart labels assume seconds without a source unit declaration. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v4.html:L516-L525] | no detection check exists | issue pending (2) |
| S49 | Vessel-configuration error is logged only to the console. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L539-L541] | no detection check exists | issue pending (4) |
| S50 | Excel-configuration error is logged only to the console. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L585-L587] | no detection check exists | issue pending (4) |
| S51 | Refresh error is logged and loading proceeds. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L621-L626] | no detection check exists | issue pending (4) |
| S52 | Maximum label assumes N while chart axis uses another force label. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L753-L753] | no detection check exists | issue pending (2) |
| S53 | Missing strut number defaults to zero before filtering it out. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L1067-L1074] | no detection check exists | issue pending (4) |
| S54 | Axes assume seconds and kN without conversion. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L1122-L1127] | no detection check exists | issue pending (2) |
| S55 | Selected force label assumes N without a source unit contract. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v6-clean.html:L710-L710] | no detection check exists | issue pending (2) |
| S56 | Digit extraction partitions strut channels by fixed numeric bounds. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v6-clean.html:L856-L866] | no detection check exists | issue pending (4) |
| S57 | Force channels with unmatched strut digits can disappear from the partition. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v6-clean.html:L913-L921] | no detection check exists | issue pending (4) |
| S58 | Absolute comparison against an initial negative infinity prevents finite maxima from being selected. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v6-clean.html:L975-L1000] | no detection check exists | issue pending (4) |
| S59 | Chart hover and x-axis assume seconds even for a summary index. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v6-clean.html:L1020-L1029] | no detection check exists | issue pending (2) |
| S60 | Missing processing-time text becomes zero before parseFloat; text units are removed without a numeric schema. [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard-enhanced.html:L597-L597] | no detection check exists | issue pending (4) |
| S61 | A failed performance sample is logged and omitted from results. [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard-enhanced.html:L601-L603] | no detection check exists | issue pending (4) |
| S62 | Debug plot labels an unqualified time array in seconds. [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard.html:L275-L279] | no detection check exists | issue pending (2) |

Explicit error paths are not counted as silent findings: backend exceptions in `get_subfolders`, `get_files`, `load_csv`, `get_data`, `refresh_configuration` and the maximum endpoint return HTTP 500. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L81-L82]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L180-L181]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L287-L288]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L551-L555]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L609-L611]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L981-L985]`

Other HTML catches report errors in visible status or debug logs: enhanced-debug lines 326, 342, 368, 393, 415, 455, 516 and 574; debug lines 105, 124, 181 and 289; original line 608; second variant line 843; fourth variant lines 272, 304, 350 and 450; fifth variant lines 505, 609, 684 and 798; sixth variant lines 633, 651, 752 and 805. These are explicit error reporting, not successful empty data. [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard-enhanced.html:L326-L329] [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard-enhanced.html:L342-L575] [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard.html:L105-L290] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser.html:L608-L612] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v2.html:L843-L849] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v4.html:L272-L451] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L505-L507] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L609-L800] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v6-clean.html:L633-L807]

Additional defects: the server's bare `excel_reader` import is incompatible with a normal package-relative import without path adjustment; #1900 records unresolved shipped imports, including this module name. No direct backend-import test exists in the discovered browser coverage. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L10-L15]` `[tests/contracts/test_development_server_settings.py:L23-L44]`

Directory containment is not checked before joining a request subfolder to the base directory; no detection check exists, issue pending (7). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L183-L200]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L290-L305]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L778-L790]`

The maximum endpoint can create up to 20 workers; this is an implementation limit, not an instruction to use that many processes during review. No module test enforces a configurable resource cap, issue pending (3). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L827-L838]`

## 7. Lessons log

| Date | Change reference | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-11-08 | Initial pack revision | Keep launch regression separate from result-ingestion qualification | The launch tests extract a function and replace the network launch object. `[tests/contracts/test_development_server_settings.py:L23-L44]` | Sections 4–6 distinguish coverage from absent controls | Not assigned; issue pending (5) |

## 8. Ownership

Required review roles: riser and mooring quality manager; deputy riser and mooring quality manager, to be assigned by the maintainers, issue pending (5). Engineering interpretation, missing references and tolerance changes require that family review before clearance. (AQG r0, section 11.)

The status remains DRAFT; the producer does not supply independent clearance. (AQG r0, section 11.)

## 9. Dependencies and implementation inventory

### Runtime and interface dependencies

The backend uses pandas, Flask, Flask-CORS, the Excel reader and process-pool reduction; the workbook reader uses pandas and openpyxl. The Python source contains no solver-binding import or model solve in the inspected backend path. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L7-L28]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L776]` `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L6-L14]`

The served page loads Plotly through a CDN; the original page's README describes Plotly and Papa Parse and proposes future results-analysis integration. Those are dependency statements and a plan, not a qualified integration with the sibling results-analysis package. [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v6-clean.html:L6-L7] [src/digitalmodel/solvers/orcaflex/browser/README.md:L92-L106]

UNVERIFIED: supported OrcaFlex result-format and solver versions are not established by the CSV reading or filename matching logic. Issue pending (5). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L199-L235]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L675]`

Every source-text OrcaFlex reference is inventoried below as a textual product or dependency mention; no entry implies a solver run:

| File | Every matching source line or range | Interpretation |
| --- | --- | --- |
| `DEBUG_GUIDE.md` | [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L1-L1] [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L48-L48] [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L57-L57] [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L188-L188] [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L209-L209] [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L213-L213] [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L224-L224] [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L227-L227] [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L230-L230] | Product text, local page references or documented future dependency; text inspected only |
| `README.md` | [src/digitalmodel/solvers/orcaflex/browser/README.md:L1-L1] [src/digitalmodel/solvers/orcaflex/browser/README.md:L5-L5] [src/digitalmodel/solvers/orcaflex/browser/README.md:L10-L10] [src/digitalmodel/solvers/orcaflex/browser/README.md:L42-L42] [src/digitalmodel/solvers/orcaflex/browser/README.md:L104-L104] [src/digitalmodel/solvers/orcaflex/browser/README.md:L105-L105] [src/digitalmodel/solvers/orcaflex/browser/README.md:L111-L111] [src/digitalmodel/solvers/orcaflex/browser/README.md:L112-L112] [src/digitalmodel/solvers/orcaflex/browser/README.md:L119-L119] | Product text, local page references or documented future dependency; text inspected only |
| `__init__.py` | `[src/digitalmodel/solvers/orcaflex/browser/__init__.py:L1-L1]` | Product text, local page references or documented future dependency; text inspected only |
| `debug-dashboard-enhanced.html` | [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard-enhanced.html:L6-L6] [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard-enhanced.html:L130-L130] | Product text, local page references or documented future dependency; text inspected only |
| `debug-dashboard.html` | [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard.html:L6-L6] [src/digitalmodel/solvers/orcaflex/browser/debug-dashboard.html:L65-L65] | Product text, local page references or documented future dependency; text inspected only |
| `excel_reader.py` | `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L2-L2]` | Product text, local page references or documented future dependency; text inspected only |
| `orcaflex-data-browser-v2.html` | [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v2.html:L6-L6] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v2.html:L331-L331] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v2.html:L332-L332] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v2.html:L522-L522] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v2.html:L528-L528] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v2.html:L530-L530] | Product text, local page references or documented future dependency; text inspected only |
| `orcaflex-data-browser-v3.html` | [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v3.html:L6-L6] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v3.html:L110-L110] | Product text, local page references or documented future dependency; text inspected only |
| `orcaflex-data-browser-v4.html` | [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v4.html:L6-L6] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v4.html:L169-L169] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v4.html:L170-L170] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v4.html:L258-L258] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v4.html:L273-L273] | Product text, local page references or documented future dependency; text inspected only |
| `orcaflex-data-browser-v5-enhanced.html` | [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L6-L6] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v5-enhanced.html:L287-L287] | Product text, local page references or documented future dependency; text inspected only |
| `orcaflex-data-browser-v6-clean.html` | [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser-v6-clean.html:L6-L6] | Product text, local page references or documented future dependency; text inspected only |
| `orcaflex-data-browser.html` | [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser.html:L6-L6] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser.html:L352-L352] [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser.html:L911-L911] | Product text, local page references or documented future dependency; text inspected only |
| `orcaflex_data_server.py` | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L3-L3]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L23-L23]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L33-L33]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L38-L38]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L1007-L1007]` | Product text, local page references or documented future dependency; text inspected only |
| `test-dashboard.html` | [src/digitalmodel/solvers/orcaflex/browser/test-dashboard.html:L4-L4] | Product text, local page references or documented future dependency; text inspected only |

### Map revision, ranking and complete inventory

Map data revision: `270c8759c374b67a7f032953e7fcdb14e0224461`; map reference revision: `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4` on `origin/codex/implmap-work`. The two in-package row identities and the search signals also agree with the map at the code pin; external records differ, so the external inventory uses the named map-reference revision. [docs/registry/implementation-map.yaml:L1-L4]

The only family with browser-package rows is `orcaflex-text-model-writers`, with 2 in-package rows; it is therefore the only counted family. Its complete map inventory has 440 rows: the 2 package rows below and 438 external records individually indexed below by source-record citation, avoiding propagation of identifying discovery names. [docs/registry/implementation-map.yaml:L6863-L6882]

| In-package implementation | Map classification | Definition at code pin | Map evidence |
| --- | --- | --- | --- |
| `load_csv` | writer; independent | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L183-L288]` | [docs/registry/implementation-map.yaml:L6863-L6872] |
| `get_data` | writer; independent | `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L290-L555]` | [docs/registry/implementation-map.yaml:L6873-L6882] |

The map's writer classification is retained for inventory completeness, although both functions read CSVs and return JSON rather than emit a solver model; correction is issue pending (6). `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L234-L286]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L378-L415]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L549-L555]`

### Exact counted-family search

Read `search_signals` from that map revision as Python `re`, case-sensitive. Omit only the vocabulary pattern `orcaflex|OrcaFlex|includefile|modular_generator`, because it matches the package path below `src/digitalmodel/` case-insensitively; retain the algorithm and constants/key patterns without alteration. [docs/registry/implementation-map.yaml:L4941-L4955]

Reproduction logic, with `map_document` loaded from the named map revision and Python sources loaded from the code pin:

```python
family = next(f for f in map_document['families']
              if f['id'] == 'orcaflex-text-model-writers')
package = 'solvers/orcaflex/browser'
signals = [s for s in family['search_signals']
           if not re.search(s['pattern'], package, re.IGNORECASE)]
for path, source in pinned_package_python_sources:
    for line_number, line in enumerate(source.splitlines(), 1):
        for signal in signals:
            if re.search(signal['pattern'], line):
                record_enclosing_definition(path, line_number)
```

| Hit definition | Hit lines | Classification |
| --- | --- | --- |
| `get_subfolders` | 66, 70 | Helper: lists existing model filenames; no model-writing algorithm. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L66-L79]` |
| `get_files` | 155 | Helper: obtains parent model basenames for CSV metadata. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L147-L177]` |

Further second implementations found: 0, against an allowed total of at most 1. The two grep-hit definitions are helpers, not missing model writers; the 2 map-listed CSV definitions remain inventoried despite the classification gap. `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L66-L79]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L147-L177]` [docs/registry/implementation-map.yaml:L6863-L6882]

### Repository-wide follow-up search

The whole repository, including `scripts/`, was searched for the named workbook, CSV-selection and maximum-force definitions; the concrete backend implementations are listed in section 3 and the public definition inventory. Similar names in tests are not additional production implementations. `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L20-L40]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L183-L235]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L776]`

```sh
rg -n 'process_single_strut_file|ExcelCollationReader|def get_max_strut_force|def parse_filename_pattern|def load_csv\(' . --glob '*.py' --glob '!**/.venv/**'
```

Map follow-up: workbook discovery/configuration, filename-based CSV selection, numeric categorisation, absolute component maxima and the HTML demonstration analytics lack their own browser-package family rows. They are not counted until the map includes in-package rows; they remain coverage items in issue pending (6). `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L30-L222]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L183-L547]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L624-L776]` [src/digitalmodel/solvers/orcaflex/browser/orcaflex-data-browser.html:L481-L526]

### External map-record inventory

IM denotes the published repository implementation map at reference revision `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, data revision `270c8759c374b67a7f032953e7fcdb14e0224461`, family `orcaflex-text-model-writers`. Each ordinal identifies one implementation record of the counted family in map-file order; the two in-package records are listed above. The reference is the identifier for an external implementation here, rather than a duplicated discovery filename. [docs/registry/implementation-map.yaml:L6863-L6882]

| Map ordinal | External implementation record |
| --- | --- |
| 1 | IM record 1, lines 4940–4949 |
| 2 | IM record 2, lines 4950–4958 |
| 3 | IM record 3, lines 4959–4968 |
| 4 | IM record 4, lines 4969–4978 |
| 5 | IM record 5, lines 4979–4988 |
| 6 | IM record 6, lines 4989–4998 |
| 7 | IM record 7, lines 4999–5008 |
| 8 | IM record 8, lines 5009–5020 |
| 9 | IM record 9, lines 5021–5030 |
| 10 | IM record 10, lines 5031–5040 |
| 11 | IM record 11, lines 5041–5050 |
| 12 | IM record 12, lines 5051–5060 |
| 13 | IM record 13, lines 5061–5070 |
| 14 | IM record 14, lines 5071–5080 |
| 15 | IM record 15, lines 5081–5090 |
| 16 | IM record 16, lines 5091–5100 |
| 17 | IM record 17, lines 5101–5110 |
| 18 | IM record 18, lines 5111–5120 |
| 19 | IM record 19, lines 5121–5132 |
| 20 | IM record 20, lines 5133–5142 |
| 21 | IM record 21, lines 5143–5151 |
| 22 | IM record 22, lines 5152–5161 |
| 23 | IM record 23, lines 5162–5171 |
| 24 | IM record 24, lines 5172–5181 |
| 25 | IM record 25, lines 5182–5191 |
| 26 | IM record 26, lines 5192–5201 |
| 27 | IM record 27, lines 5202–5211 |
| 28 | IM record 28, lines 5212–5223 |
| 29 | IM record 29, lines 5224–5233 |
| 30 | IM record 30, lines 5234–5243 |
| 31 | IM record 31, lines 5244–5253 |
| 32 | IM record 32, lines 5254–5263 |
| 33 | IM record 33, lines 5264–5273 |
| 34 | IM record 34, lines 5274–5283 |
| 35 | IM record 35, lines 5284–5293 |
| 36 | IM record 36, lines 5294–5303 |
| 37 | IM record 37, lines 5304–5313 |
| 38 | IM record 38, lines 5314–5323 |
| 39 | IM record 39, lines 5324–5333 |
| 40 | IM record 40, lines 5334–5343 |
| 41 | IM record 41, lines 5344–5353 |
| 42 | IM record 42, lines 5354–5363 |
| 43 | IM record 43, lines 5364–5372 |
| 44 | IM record 44, lines 5373–5382 |
| 45 | IM record 45, lines 5383–5392 |
| 46 | IM record 46, lines 5393–5402 |
| 47 | IM record 47, lines 5403–5415 |
| 48 | IM record 48, lines 5416–5425 |
| 49 | IM record 49, lines 5426–5437 |
| 50 | IM record 50, lines 5438–5447 |
| 51 | IM record 51, lines 5448–5457 |
| 52 | IM record 52, lines 5458–5467 |
| 53 | IM record 53, lines 5468–5477 |
| 54 | IM record 54, lines 5478–5487 |
| 55 | IM record 55, lines 5488–5497 |
| 56 | IM record 56, lines 5498–5507 |
| 57 | IM record 57, lines 5508–5519 |
| 58 | IM record 58, lines 5520–5531 |
| 59 | IM record 59, lines 5532–5540 |
| 60 | IM record 60, lines 5541–5550 |
| 61 | IM record 61, lines 5551–5560 |
| 62 | IM record 62, lines 5561–5570 |
| 63 | IM record 63, lines 5571–5580 |
| 64 | IM record 64, lines 5581–5593 |
| 65 | IM record 65, lines 5594–5603 |
| 66 | IM record 66, lines 5604–5615 |
| 67 | IM record 67, lines 5616–5628 |
| 68 | IM record 68, lines 5629–5638 |
| 69 | IM record 69, lines 5639–5648 |
| 70 | IM record 70, lines 5649–5658 |
| 71 | IM record 71, lines 5659–5667 |
| 72 | IM record 72, lines 5668–5677 |
| 73 | IM record 73, lines 5678–5687 |
| 74 | IM record 74, lines 5688–5697 |
| 75 | IM record 75, lines 5698–5707 |
| 76 | IM record 76, lines 5708–5717 |
| 77 | IM record 77, lines 5718–5727 |
| 78 | IM record 78, lines 5728–5737 |
| 79 | IM record 79, lines 5738–5749 |
| 80 | IM record 80, lines 5750–5759 |
| 81 | IM record 81, lines 5760–5769 |
| 82 | IM record 82, lines 5770–5781 |
| 83 | IM record 83, lines 5782–5791 |
| 84 | IM record 84, lines 5792–5801 |
| 85 | IM record 85, lines 5802–5811 |
| 86 | IM record 86, lines 5812–5823 |
| 87 | IM record 87, lines 5824–5833 |
| 88 | IM record 88, lines 5834–5843 |
| 89 | IM record 89, lines 5844–5853 |
| 90 | IM record 90, lines 5854–5863 |
| 91 | IM record 91, lines 5864–5872 |
| 92 | IM record 92, lines 5873–5882 |
| 93 | IM record 93, lines 5883–5891 |
| 94 | IM record 94, lines 5892–5901 |
| 95 | IM record 95, lines 5902–5911 |
| 96 | IM record 96, lines 5912–5921 |
| 97 | IM record 97, lines 5922–5933 |
| 98 | IM record 98, lines 5934–5943 |
| 99 | IM record 99, lines 5944–5953 |
| 100 | IM record 100, lines 5954–5965 |
| 101 | IM record 101, lines 5966–5975 |
| 102 | IM record 102, lines 5976–5985 |
| 103 | IM record 103, lines 5986–5994 |
| 104 | IM record 104, lines 5995–6004 |
| 105 | IM record 105, lines 6005–6014 |
| 106 | IM record 106, lines 6015–6024 |
| 107 | IM record 107, lines 6025–6034 |
| 108 | IM record 108, lines 6035–6044 |
| 109 | IM record 109, lines 6045–6054 |
| 110 | IM record 110, lines 6055–6064 |
| 111 | IM record 111, lines 6065–6074 |
| 112 | IM record 112, lines 6075–6084 |
| 113 | IM record 113, lines 6085–6096 |
| 114 | IM record 114, lines 6097–6109 |
| 115 | IM record 115, lines 6110–6119 |
| 116 | IM record 116, lines 6120–6129 |
| 117 | IM record 117, lines 6130–6141 |
| 118 | IM record 118, lines 6142–6153 |
| 119 | IM record 119, lines 6154–6163 |
| 120 | IM record 120, lines 6164–6173 |
| 121 | IM record 121, lines 6174–6183 |
| 122 | IM record 122, lines 6184–6193 |
| 123 | IM record 123, lines 6194–6203 |
| 124 | IM record 124, lines 6204–6213 |
| 125 | IM record 125, lines 6214–6223 |
| 126 | IM record 126, lines 6224–6233 |
| 127 | IM record 127, lines 6234–6243 |
| 128 | IM record 128, lines 6244–6253 |
| 129 | IM record 129, lines 6254–6263 |
| 130 | IM record 130, lines 6264–6273 |
| 131 | IM record 131, lines 6274–6283 |
| 132 | IM record 132, lines 6284–6293 |
| 133 | IM record 133, lines 6294–6303 |
| 134 | IM record 134, lines 6304–6313 |
| 135 | IM record 135, lines 6314–6323 |
| 136 | IM record 136, lines 6324–6333 |
| 137 | IM record 137, lines 6334–6343 |
| 138 | IM record 138, lines 6344–6353 |
| 139 | IM record 139, lines 6354–6363 |
| 140 | IM record 140, lines 6364–6373 |
| 141 | IM record 141, lines 6374–6383 |
| 142 | IM record 142, lines 6384–6393 |
| 143 | IM record 143, lines 6394–6403 |
| 144 | IM record 144, lines 6404–6413 |
| 145 | IM record 145, lines 6414–6423 |
| 146 | IM record 146, lines 6424–6436 |
| 147 | IM record 147, lines 6437–6448 |
| 148 | IM record 148, lines 6449–6458 |
| 149 | IM record 149, lines 6459–6468 |
| 150 | IM record 150, lines 6469–6478 |
| 151 | IM record 151, lines 6479–6490 |
| 152 | IM record 152, lines 6491–6500 |
| 153 | IM record 153, lines 6501–6510 |
| 154 | IM record 154, lines 6511–6520 |
| 155 | IM record 155, lines 6521–6530 |
| 156 | IM record 156, lines 6531–6540 |
| 157 | IM record 157, lines 6541–6550 |
| 158 | IM record 158, lines 6551–6560 |
| 159 | IM record 159, lines 6561–6570 |
| 160 | IM record 160, lines 6571–6580 |
| 161 | IM record 161, lines 6581–6590 |
| 162 | IM record 162, lines 6591–6600 |
| 163 | IM record 163, lines 6601–6610 |
| 164 | IM record 164, lines 6611–6620 |
| 165 | IM record 165, lines 6621–6630 |
| 166 | IM record 166, lines 6631–6640 |
| 167 | IM record 167, lines 6641–6650 |
| 168 | IM record 168, lines 6651–6659 |
| 169 | IM record 169, lines 6660–6672 |
| 170 | IM record 170, lines 6673–6682 |
| 171 | IM record 171, lines 6683–6692 |
| 172 | IM record 172, lines 6693–6704 |
| 173 | IM record 173, lines 6705–6714 |
| 174 | IM record 174, lines 6715–6724 |
| 175 | IM record 175, lines 6725–6734 |
| 176 | IM record 176, lines 6735–6744 |
| 177 | IM record 177, lines 6745–6754 |
| 178 | IM record 178, lines 6755–6764 |
| 179 | IM record 179, lines 6765–6774 |
| 180 | IM record 180, lines 6775–6784 |
| 181 | IM record 181, lines 6785–6794 |
| 182 | IM record 182, lines 6795–6804 |
| 183 | IM record 183, lines 6805–6814 |
| 184 | IM record 184, lines 6815–6824 |
| 185 | IM record 185, lines 6825–6834 |
| 186 | IM record 186, lines 6835–6844 |
| 187 | IM record 187, lines 6845–6854 |
| 190 | IM record 190, lines 6875–6884 |
| 191 | IM record 191, lines 6885–6894 |
| 192 | IM record 192, lines 6895–6904 |
| 193 | IM record 193, lines 6905–6914 |
| 194 | IM record 194, lines 6915–6927 |
| 195 | IM record 195, lines 6928–6937 |
| 196 | IM record 196, lines 6938–6947 |
| 197 | IM record 197, lines 6948–6957 |
| 198 | IM record 198, lines 6958–6967 |
| 199 | IM record 199, lines 6968–6977 |
| 200 | IM record 200, lines 6978–6987 |
| 201 | IM record 201, lines 6988–6997 |
| 202 | IM record 202, lines 6998–7007 |
| 203 | IM record 203, lines 7008–7017 |
| 204 | IM record 204, lines 7018–7031 |
| 205 | IM record 205, lines 7032–7041 |
| 206 | IM record 206, lines 7042–7051 |
| 207 | IM record 207, lines 7052–7061 |
| 208 | IM record 208, lines 7062–7071 |
| 209 | IM record 209, lines 7072–7081 |
| 210 | IM record 210, lines 7082–7091 |
| 211 | IM record 211, lines 7092–7101 |
| 212 | IM record 212, lines 7102–7111 |
| 213 | IM record 213, lines 7112–7121 |
| 214 | IM record 214, lines 7122–7131 |
| 215 | IM record 215, lines 7132–7141 |
| 216 | IM record 216, lines 7142–7153 |
| 217 | IM record 217, lines 7154–7163 |
| 218 | IM record 218, lines 7164–7176 |
| 219 | IM record 219, lines 7177–7186 |
| 220 | IM record 220, lines 7187–7196 |
| 221 | IM record 221, lines 7197–7207 |
| 222 | IM record 222, lines 7208–7217 |
| 223 | IM record 223, lines 7218–7227 |
| 224 | IM record 224, lines 7228–7237 |
| 225 | IM record 225, lines 7238–7247 |
| 226 | IM record 226, lines 7248–7257 |
| 227 | IM record 227, lines 7258–7269 |
| 228 | IM record 228, lines 7270–7279 |
| 229 | IM record 229, lines 7280–7289 |
| 230 | IM record 230, lines 7290–7299 |
| 231 | IM record 231, lines 7300–7309 |
| 232 | IM record 232, lines 7310–7319 |
| 233 | IM record 233, lines 7320–7329 |
| 234 | IM record 234, lines 7330–7339 |
| 235 | IM record 235, lines 7340–7349 |
| 236 | IM record 236, lines 7350–7359 |
| 237 | IM record 237, lines 7360–7369 |
| 238 | IM record 238, lines 7370–7379 |
| 239 | IM record 239, lines 7380–7389 |
| 240 | IM record 240, lines 7390–7399 |
| 241 | IM record 241, lines 7400–7409 |
| 242 | IM record 242, lines 7410–7419 |
| 243 | IM record 243, lines 7420–7429 |
| 244 | IM record 244, lines 7430–7439 |
| 245 | IM record 245, lines 7440–7449 |
| 246 | IM record 246, lines 7450–7459 |
| 247 | IM record 247, lines 7460–7469 |
| 248 | IM record 248, lines 7470–7479 |
| 249 | IM record 249, lines 7480–7489 |
| 250 | IM record 250, lines 7490–7499 |
| 251 | IM record 251, lines 7500–7509 |
| 252 | IM record 252, lines 7510–7519 |
| 253 | IM record 253, lines 7520–7529 |
| 254 | IM record 254, lines 7530–7539 |
| 255 | IM record 255, lines 7540–7551 |
| 256 | IM record 256, lines 7552–7561 |
| 257 | IM record 257, lines 7562–7571 |
| 258 | IM record 258, lines 7572–7581 |
| 259 | IM record 259, lines 7582–7591 |
| 260 | IM record 260, lines 7592–7603 |
| 261 | IM record 261, lines 7604–7613 |
| 262 | IM record 262, lines 7614–7626 |
| 263 | IM record 263, lines 7627–7636 |
| 264 | IM record 264, lines 7637–7646 |
| 265 | IM record 265, lines 7647–7656 |
| 266 | IM record 266, lines 7657–7666 |
| 267 | IM record 267, lines 7667–7676 |
| 268 | IM record 268, lines 7677–7686 |
| 269 | IM record 269, lines 7687–7696 |
| 270 | IM record 270, lines 7697–7706 |
| 271 | IM record 271, lines 7707–7716 |
| 272 | IM record 272, lines 7717–7729 |
| 273 | IM record 273, lines 7730–7739 |
| 274 | IM record 274, lines 7740–7749 |
| 275 | IM record 275, lines 7750–7762 |
| 276 | IM record 276, lines 7763–7774 |
| 277 | IM record 277, lines 7775–7784 |
| 278 | IM record 278, lines 7785–7794 |
| 279 | IM record 279, lines 7795–7804 |
| 280 | IM record 280, lines 7805–7816 |
| 281 | IM record 281, lines 7817–7826 |
| 282 | IM record 282, lines 7827–7836 |
| 283 | IM record 283, lines 7837–7846 |
| 284 | IM record 284, lines 7847–7856 |
| 285 | IM record 285, lines 7857–7866 |
| 286 | IM record 286, lines 7867–7878 |
| 287 | IM record 287, lines 7879–7888 |
| 288 | IM record 288, lines 7889–7898 |
| 289 | IM record 289, lines 7899–7908 |
| 290 | IM record 290, lines 7909–7921 |
| 291 | IM record 291, lines 7922–7931 |
| 292 | IM record 292, lines 7932–7941 |
| 293 | IM record 293, lines 7942–7951 |
| 294 | IM record 294, lines 7952–7961 |
| 295 | IM record 295, lines 7962–7971 |
| 296 | IM record 296, lines 7972–7981 |
| 297 | IM record 297, lines 7982–7991 |
| 298 | IM record 298, lines 7992–8001 |
| 299 | IM record 299, lines 8002–8011 |
| 300 | IM record 300, lines 8012–8021 |
| 301 | IM record 301, lines 8022–8031 |
| 302 | IM record 302, lines 8032–8043 |
| 303 | IM record 303, lines 8044–8053 |
| 304 | IM record 304, lines 8054–8063 |
| 305 | IM record 305, lines 8064–8073 |
| 306 | IM record 306, lines 8074–8083 |
| 307 | IM record 307, lines 8084–8093 |
| 308 | IM record 308, lines 8094–8103 |
| 309 | IM record 309, lines 8104–8113 |
| 310 | IM record 310, lines 8114–8123 |
| 311 | IM record 311, lines 8124–8133 |
| 312 | IM record 312, lines 8134–8143 |
| 313 | IM record 313, lines 8144–8155 |
| 314 | IM record 314, lines 8156–8165 |
| 315 | IM record 315, lines 8166–8175 |
| 316 | IM record 316, lines 8176–8185 |
| 317 | IM record 317, lines 8186–8195 |
| 318 | IM record 318, lines 8196–8205 |
| 319 | IM record 319, lines 8206–8215 |
| 320 | IM record 320, lines 8216–8227 |
| 321 | IM record 321, lines 8228–8237 |
| 322 | IM record 322, lines 8238–8247 |
| 323 | IM record 323, lines 8248–8257 |
| 324 | IM record 324, lines 8258–8267 |
| 325 | IM record 325, lines 8268–8279 |
| 326 | IM record 326, lines 8280–8290 |
| 327 | IM record 327, lines 8291–8300 |
| 328 | IM record 328, lines 8301–8310 |
| 329 | IM record 329, lines 8311–8323 |
| 330 | IM record 330, lines 8324–8333 |
| 331 | IM record 331, lines 8334–8343 |
| 332 | IM record 332, lines 8344–8353 |
| 333 | IM record 333, lines 8354–8363 |
| 334 | IM record 334, lines 8364–8373 |
| 335 | IM record 335, lines 8374–8383 |
| 336 | IM record 336, lines 8384–8393 |
| 337 | IM record 337, lines 8394–8403 |
| 338 | IM record 338, lines 8404–8413 |
| 339 | IM record 339, lines 8414–8423 |
| 340 | IM record 340, lines 8424–8435 |
| 341 | IM record 341, lines 8436–8445 |
| 342 | IM record 342, lines 8446–8457 |
| 343 | IM record 343, lines 8458–8470 |
| 344 | IM record 344, lines 8471–8480 |
| 345 | IM record 345, lines 8481–8492 |
| 346 | IM record 346, lines 8493–8505 |
| 347 | IM record 347, lines 8506–8517 |
| 348 | IM record 348, lines 8518–8527 |
| 349 | IM record 349, lines 8528–8537 |
| 350 | IM record 350, lines 8538–8547 |
| 351 | IM record 351, lines 8548–8560 |
| 352 | IM record 352, lines 8561–8572 |
| 353 | IM record 353, lines 8573–8582 |
| 354 | IM record 354, lines 8583–8596 |
| 355 | IM record 355, lines 8597–8608 |
| 356 | IM record 356, lines 8609–8619 |
| 357 | IM record 357, lines 8620–8632 |
| 358 | IM record 358, lines 8633–8647 |
| 359 | IM record 359, lines 8648–8657 |
| 360 | IM record 360, lines 8658–8667 |
| 361 | IM record 361, lines 8668–8677 |
| 362 | IM record 362, lines 8678–8687 |
| 363 | IM record 363, lines 8688–8701 |
| 364 | IM record 364, lines 8702–8714 |
| 365 | IM record 365, lines 8715–8724 |
| 366 | IM record 366, lines 8725–8739 |
| 367 | IM record 367, lines 8740–8749 |
| 368 | IM record 368, lines 8750–8759 |
| 369 | IM record 369, lines 8760–8773 |
| 370 | IM record 370, lines 8774–8783 |
| 371 | IM record 371, lines 8784–8793 |
| 372 | IM record 372, lines 8794–8803 |
| 373 | IM record 373, lines 8804–8813 |
| 374 | IM record 374, lines 8814–8823 |
| 375 | IM record 375, lines 8824–8833 |
| 376 | IM record 376, lines 8834–8843 |
| 377 | IM record 377, lines 8844–8856 |
| 378 | IM record 378, lines 8857–8866 |
| 379 | IM record 379, lines 8867–8876 |
| 380 | IM record 380, lines 8877–8886 |
| 381 | IM record 381, lines 8887–8896 |
| 382 | IM record 382, lines 8897–8908 |
| 383 | IM record 383, lines 8909–8918 |
| 384 | IM record 384, lines 8919–8928 |
| 385 | IM record 385, lines 8929–8938 |
| 386 | IM record 386, lines 8939–8948 |
| 387 | IM record 387, lines 8949–8958 |
| 388 | IM record 388, lines 8959–8970 |
| 389 | IM record 389, lines 8971–8980 |
| 390 | IM record 390, lines 8981–8990 |
| 391 | IM record 391, lines 8991–9000 |
| 392 | IM record 392, lines 9001–9010 |
| 393 | IM record 393, lines 9011–9020 |
| 394 | IM record 394, lines 9021–9030 |
| 395 | IM record 395, lines 9031–9040 |
| 396 | IM record 396, lines 9041–9050 |
| 397 | IM record 397, lines 9051–9060 |
| 398 | IM record 398, lines 9061–9070 |
| 399 | IM record 399, lines 9071–9080 |
| 400 | IM record 400, lines 9081–9090 |
| 401 | IM record 401, lines 9091–9100 |
| 402 | IM record 402, lines 9101–9110 |
| 403 | IM record 403, lines 9111–9120 |
| 404 | IM record 404, lines 9121–9130 |
| 405 | IM record 405, lines 9131–9140 |
| 406 | IM record 406, lines 9141–9152 |
| 407 | IM record 407, lines 9153–9162 |
| 408 | IM record 408, lines 9163–9172 |
| 409 | IM record 409, lines 9173–9182 |
| 410 | IM record 410, lines 9183–9192 |
| 411 | IM record 411, lines 9193–9202 |
| 412 | IM record 412, lines 9203–9212 |
| 413 | IM record 413, lines 9213–9222 |
| 414 | IM record 414, lines 9223–9232 |
| 415 | IM record 415, lines 9233–9242 |
| 416 | IM record 416, lines 9243–9252 |
| 417 | IM record 417, lines 9253–9262 |
| 418 | IM record 418, lines 9263–9272 |
| 419 | IM record 419, lines 9273–9282 |
| 420 | IM record 420, lines 9283–9292 |
| 421 | IM record 421, lines 9293–9302 |
| 422 | IM record 422, lines 9303–9312 |
| 423 | IM record 423, lines 9313–9322 |
| 424 | IM record 424, lines 9323–9332 |
| 425 | IM record 425, lines 9333–9342 |
| 426 | IM record 426, lines 9343–9352 |
| 427 | IM record 427, lines 9353–9362 |
| 428 | IM record 428, lines 9363–9372 |
| 429 | IM record 429, lines 9373–9382 |
| 430 | IM record 430, lines 9383–9392 |
| 431 | IM record 431, lines 9393–9402 |
| 432 | IM record 432, lines 9403–9412 |
| 433 | IM record 433, lines 9413–9422 |
| 434 | IM record 434, lines 9423–9432 |
| 435 | IM record 435, lines 9433–9442 |
| 436 | IM record 436, lines 9443–9452 |
| 437 | IM record 437, lines 9453–9462 |
| 438 | IM record 438, lines 9463–9472 |
| 439 | IM record 439, lines 9473–9482 |
| 440 | IM record 440, lines 9483–9493 |

## 10. Coverage and open items

Full Python and Markdown reading: `__init__.py`, `excel_reader.py`, `orcaflex_data_server.py`, `README.md`, `DEBUG_GUIDE.md` in the package, and the moved-module stub `docs/domains/solvers/orcaflex_browser/README.md`; the stub contains TODO usage/API text rather than further engineering guidance. `[src/digitalmodel/solvers/orcaflex/browser/__init__.py:L1-L1]` `[src/digitalmodel/solvers/orcaflex/browser/excel_reader.py:L20-L384]` `[src/digitalmodel/solvers/orcaflex/browser/orcaflex_data_server.py:L30-L1011]` [src/digitalmodel/solvers/orcaflex/browser/README.md:L1-L129] [src/digitalmodel/solvers/orcaflex/browser/DEBUG_GUIDE.md:L1-L266] [docs/domains/solvers/orcaflex_browser/README.md:L1-L17]

HTML coverage consists of text searches and numerical/error-handler excerpts for the original page, variants 2–6, both debug pages and the small test page. The test page draws a fixed chart through a button; it is not a pytest case or an independent engineering benchmark. [src/digitalmodel/solvers/orcaflex/browser/test-dashboard.html:L12-L25]

The direct browser test evidence is the nine launch nodes of section 4; they do not import the backend module or inspect its CSV/Excel results. `[tests/contracts/test_development_server_settings.py:L23-L44]` `[tests/contracts/test_development_server_settings.py:L74-L143]`

| Open item | Responsible role | Tracking |
| --- | --- | --- |
| Make all partial/default workbook results and cache age explicit; section-6 rows linked to this cause | Browser maintainer | issue pending (1) |
| UNVERIFIED force units; UNVERIFIED coordinate origin, axes, sign and phase; exact selection, time metadata and dtype preservation | Browser maintainer | issue pending (2) |
| Complete per-file maximum evidence, signed source provenance, related-file matching and configurable worker budget | Browser maintainer | issue pending (3) |
| Finite maximum annotations, deterministic demonstration qualification, error-state tests and complete HTML workflow audit | Browser maintainer | issue pending (4) |
| Independent engineering example, ingestion and direct package-import tests; upstream gate/fingerprint evidence; UNVERIFIED supported solver/result versions; assign deputy and shared-log reference | Riser and mooring quality manager with browser maintainer | issue pending (5) |
| Correct the map's writer classifications and add the missing browser method families; external algorithms remain map follow-up | Map maintainer | issue pending (6) |
| Reject paths outside the configured result root | Browser maintainer | issue pending (7) |
| Resolve the bare sibling-module import before package import can be qualified | Packaging maintainer | #1900 |

No client-derived numeric result or private sidecar entry is included; if private evidence is subsequently required, sidecar entry: to be created under issue pending (5). Review and qualify those inputs separately before applying B0-INPUT or B5-TRACE. (AQG r0, sections 4 and 9.)
