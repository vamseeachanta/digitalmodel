# Reservoir module knowledge pack

| Header | Value |
| --- | --- |
| Module | marine_ops/reservoir |
| Repository | digitalmodel |
| Package | `src/digitalmodel/marine_ops/reservoir` |
| Pack revision | 0 |
| Code revision | `c099a5e1317e6bed63d876252d87cbb12c774372` |
| Analysis family | Engineering calculations (EC) |
| Quality role | Engineering calculations quality manager |
| Status | DRAFT |
| Offline evidence date | 2026-10-18 |

## 1. Purpose and scope

The public facade exports property calculators, material balance, well-test analysis, depletion, decline forecasting and log/correlation analysis. [src/digitalmodel/marine_ops/reservoir/__init__.py:L17-L73]

The domain registry marks marine operations beta; the separate flat reservoir domain has its own route, so its volumetrics and plotting functions are comparison implementations rather than this package's public API. [docs/registry/module-routing.yaml:L185-L190] [docs/registry/module-routing.yaml:L326-L331] [src/digitalmodel/reservoir/volumetrics.py:L40-L64]

Depletion is a uniform tank pressure update, not a spatial flow solution: the grid is initialized but the loop updates a scalar pressure, and property maps in characterization are constant mean grids. [src/digitalmodel/marine_ops/reservoir/modeling.py:L330-L415] [src/digitalmodel/marine_ops/reservoir/modeling.py:L639-L688]

Hyperbolic decline retains fixed guesses and non-exponential fit quality is a placeholder; these paths cannot establish calibrated forecasts. [src/digitalmodel/marine_ops/reservoir/modeling.py:L475-L504]

The package scope includes all five Python files and the local README; no nested Python package occurs in the declared module layout. [src/digitalmodel/marine_ops/reservoir/README.md:L15-L25]

## 2. Inputs and outputs

### Inputs


| Input | Unit / convention | Allowed range or check | Default | Source | Evidence |
| --- | --- | --- | --- | --- | --- |
| RockProperties | porosity and net_to_gross: fraction; permeability: mD | porosity and net_to_gross 0 to 1; permeability >=0 | net_to_gross 1; thickness/compressibility absent; grain_density 2.65 g/cm³; exponents 2 | caller | [src/digitalmodel/marine_ops/reservoir/properties.py:L27-L60] |
| RockProperties geometry | thickness ft; compressibility 1/psi; densities g/cm³ | no further range check in constructor | thickness None; compressibility None | caller | [src/digitalmodel/marine_ops/reservoir/properties.py:L27-L60] |
| FluidProperties | oil_density interpreted as °API by conversion; gas density specific gravity; viscosities cp; Bo res bbl/STB; Bg res ft³/scf | constructor supplies no validation | water density/viscosity 1; Bo/Bg 1; solution_gor 0 scf/STB; water compressibility 3e-6 1/psi | caller | [src/digitalmodel/marine_ops/reservoir/properties.py:L89-L142] |
| PVTProperties | pressure psi; temperature °F; z dimensionless | no range guard; initial pressure copies pressure | z 1; optional expansion override | caller | [src/digitalmodel/marine_ops/reservoir/properties.py:L146-L185] |
| ReservoirProperties | area acres; depth ft; saturations fraction | only total saturation warning outside 0.99 to 1.01 | Sw 0.3; missing So/Sg inferred; PVT 2000 psi and 180 °F | caller | [src/digitalmodel/marine_ops/reservoir/properties.py:L189-L228] |
| WellData / ProductionData | x/y, MD/TVD, radius ft; oil/water STB/day; gas Mscf/day; pressure psi | no coordinate or rate range guard | TVD=MD; skin 0; radius 0.25 ft; all rates 0 | caller | [src/digitalmodel/marine_ops/reservoir/modeling.py:L25-L86] |
| LogCurve | depth ft; units string per curve; same-length arrays | length equality only; sentinel equality mask | null -999.25 | caller | [src/digitalmodel/marine_ops/reservoir/analysis.py:L33-L89] |
| LogAnalysis | GR, RHOB g/cm³, NPHI fraction, RT ohm-m | required curves depend on operation | facies 5; depth step 0.5 ft; Rw 0.1 ohm-m; porosity cutoff 0.1; Sw cutoff 0.5 | caller | [src/digitalmodel/marine_ops/reservoir/analysis.py:L160-L395] |
| Characterization logs | DEPTH ft; POROSITY/SW/VSH fractions; PERMEABILITY mD | well must exist; missing columns get defaults | porosity cutoff 0.1; saturation cutoff 0.5; depth spacing fallback 0.5 ft | DataFrame | [src/digitalmodel/marine_ops/reservoir/modeling.py:L592-L637] |
| Migration frames | UWI, Depth, GR, RT, RHOB, NPHI; UWI/TOP/BASE | required columns checked before plotting | optional Formation; output path absent | DataFrames | [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L22-L132] |

UNVERIFIED: the caller's horizontal coordinate reference system, origin and depth datum are unspecified; the API provides x/y and depth in ft but no coordinate metadata (issue pending (10)). [src/digitalmodel/marine_ops/reservoir/modeling.py:L25-L59]

No phase convention is consumed by these property, production and log calculations; flattened depth is original depth minus the selected datum, while plotted depth is inverted. [src/digitalmodel/marine_ops/reservoir/analysis.py:L503-L523] [src/digitalmodel/marine_ops/reservoir/analysis.py:L663-L672]

### Outputs


| Output | Unit / representation | Missing-data behavior | Evidence |
| --- | --- | --- | --- |
| pore / hydrocarbon pore volume | acre-ft; scalar | None when area or thickness is missing | [src/digitalmodel/marine_ops/reservoir/properties.py:L230-L253] |
| OOIP / OGIP | STB / scf | None when volume or phase is unavailable | [src/digitalmodel/marine_ops/reservoir/properties.py:L255-L288] |
| cumulative material balance | oil STB; water STB; gas Mscf | empty arrays without history; scalars with data | [src/digitalmodel/marine_ops/reservoir/modeling.py:L128-L158] |
| tank_material_balance | pressure drop psi; expansion factors dimensionless; estimate STB intended | ooip_estimate None for no oil or nonpositive expansion | [src/digitalmodel/marine_ops/reservoir/modeling.py:L160-L223] |
| buildup analysis | permeability mD; skin, efficiency and R² dimensionless; intercept psi | raises for fewer than three points | [src/digitalmodel/marine_ops/reservoir/modeling.py:L254-L325] |
| depletion / forecast | time days; pressure psi; oil STB/day; cumulative and EUR STB | depletion uses assumed values; forecast requires parameters | [src/digitalmodel/marine_ops/reservoir/modeling.py:L368-L415] [src/digitalmodel/marine_ops/reservoir/modeling.py:L517-L554] |
| net pay / gridded maps | thickness/depth ft; fraction properties; arrays and dictionaries | defaults, sentinels or missing-data error dictionaries | [src/digitalmodel/marine_ops/reservoir/modeling.py:L592-L688] [src/digitalmodel/marine_ops/reservoir/analysis.py:L343-L395] [src/digitalmodel/marine_ops/reservoir/analysis.py:L820-L963] |
| summary / analysis report / migration | nested dictionaries, Figure, or text; optional image/text writes | integrated failures may be returned as error dictionaries | [src/digitalmodel/marine_ops/reservoir/modeling.py:L728-L814] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1144-L1199] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L144-L164] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L470-L570] |

## 3. Method and governing standards

The following formulas describe the implementation; they establish neither a standard edition nor empirical applicability. [src/digitalmodel/marine_ops/reservoir/properties.py:L291-L405] [src/digitalmodel/marine_ops/reservoir/modeling.py:L439-L554]


| Definition / method | Implementation and limits | Evidence |
| --- | --- | --- |
| RockProperties.calculate_bulk_density / calculate_formation_factor | density=(1-porosity)×grain_density+porosity×water_density; formation factor=porosity^(-m), with coefficient assumed unity | [src/digitalmodel/marine_ops/reservoir/properties.py:L62-L87] |
| FluidProperties / PVTProperties | API-to-specific-gravity algebra; simple initial/current pressure gas expansion with current z and unit initial z | [src/digitalmodel/marine_ops/reservoir/properties.py:L119-L142] [src/digitalmodel/marine_ops/reservoir/properties.py:L167-L185] |
| ReservoirProperties.calculate_pore_volume / calculate_ooip / calculate_ogip | area×thickness×porosity×net_to_gross; hydrocarbon pore volume split by phase and divided by formation volume factor | [src/digitalmodel/marine_ops/reservoir/properties.py:L230-L288] |
| calculate_porosity / calculate_permeability | RMS or arithmetic average with clipping; simplified Kozeny-Carman, Timur and Coates branches with defaults and permeability floor | [src/digitalmodel/marine_ops/reservoir/properties.py:L291-L352] |
| calculate_water_saturation | Archie and simplified Waxman-Smits/Simandoux branches, followed by clipping | [src/digitalmodel/marine_ops/reservoir/properties.py:L355-L405] |
| MaterialBalance.calculate_cumulative_production / tank_material_balance | trapezoidal integration against integer elapsed days; simplified withdrawal/expansion estimate; gas scale mismatch listed in section 6 | [src/digitalmodel/marine_ops/reservoir/modeling.py:L128-L223] |
| WellTestAnalysis.analyze_buildup_test | Horner transform; middle 15% to 85% linear fit; assumed preproduction time/rate and compressibilities; not an independent reference | [src/digitalmodel/marine_ops/reservoir/modeling.py:L254-L325] |
| ReservoirSimulation.run_depletion_simulation | fixed 30-day tank steps; pressure decrement from production and compressibility; hard pressure production cutoff | [src/digitalmodel/marine_ops/reservoir/modeling.py:L368-L415] |
| ProductionForecast.arps_decline_analysis / forecast_production | exponential log-linear and harmonic reciprocal fits; hyperbolic fixed guesses; linspace samples integrated as 30-day rectangles | [src/digitalmodel/marine_ops/reservoir/modeling.py:L439-L554] |
| LogCurve / LogAnalysis | linear interpolation; standardization and KMeans facies; density-neutron porosity; separate Archie implementation; sample-count net pay | [src/digitalmodel/marine_ops/reservoir/analysis.py:L51-L89] [src/digitalmodel/marine_ops/reservoir/analysis.py:L160-L395] |
| StratigraphicAnalysis / WellCorrelation / ReservoirMapping | datum subtraction; linear griddata mapping with edge NaNs; plotting helpers; names sorted lexically for common marker choice | [src/digitalmodel/marine_ops/reservoir/analysis.py:L451-L548] [src/digitalmodel/marine_ops/reservoir/analysis.py:L820-L963] |
| ReservoirCharacterization.calculate_net_pay / generate_property_maps | cutoff masks and mean depth increment; grids filled with well means rather than spatial interpolation | [src/digitalmodel/marine_ops/reservoir/modeling.py:L592-L688] |
| PerformanceAnalysis.calculate_eur_by_analogy / CoreAnalysis.correlate_core_to_logs | mean-rate duration cumulative analogue; statistics and linear log/core interpolation with Pearson correlation and RMSE | [src/digitalmodel/marine_ops/reservoir/analysis.py:L984-L1092] |
| legacy_migration | adapts DataFrames to analysis objects and falls back to a second plotting implementation | [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L22-L164] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L167-L381] |

UNVERIFIED: published editions, equation provenance, empirical validity ranges and calculation Citation sidecars for the named correlations have not been established; independent benchmark coverage remains an open item (issue pending (10)). [src/digitalmodel/marine_ops/reservoir/properties.py:L330-L405] [src/digitalmodel/marine_ops/reservoir/modeling.py:L475-L504]

No licensed standard clause is asserted here; the README describes capabilities without a numbered governing standard, and its kriging claim differs from the mean-grid implementation. [src/digitalmodel/marine_ops/reservoir/README.md:L204-L229] [src/digitalmodel/marine_ops/reservoir/modeling.py:L666-L688]

## 4. Validated worked examples

Validated cases require an independent expected result; the exact linear-fit identity below checks only the goodness-of-fit helper, not a reservoir correlation.


| Case | Source / inputs | Expected value and tolerance | Test node ID | Offline result | Evidence |
| --- | --- | --- | --- | --- | --- |
| V1 — closed-form linear fit | For y=2x the residual sum of squares is zero, so R²=1; four dimensionless points | R² 1; absolute tolerance 1e-10, as asserted | `tests/marine_ops/reservoir/test_modeling.py::TestWellTestAnalysis::test_calculate_r_squared_perfect_fit` | PASS at BASE and AFTER | [tests/marine_ops/reservoir/test_modeling.py:L466-L474] |
| R1 — no production (regression) | empty production history; current pressure equals initial | 0 psi pressure drop; 0 cumulative oil/gas/water; OOIP estimate None; pytest.approx defaults | `tests/marine_ops/reservoir/test_modeling.py::TestMaterialBalance::test_tank_material_balance_no_production` | PASS at BASE and AFTER | [tests/marine_ops/reservoir/test_modeling.py:L347-L356] |
| R2 — pressure depletion (regression) | fixture initial 5000 psi, current 4500 psi | 500 psi pressure drop; pytest.approx defaults | `tests/marine_ops/reservoir/test_modeling.py::TestMaterialBalance::test_tank_material_balance_pressure_drop` | PASS at BASE and AFTER | [tests/marine_ops/reservoir/test_modeling.py:L326-L333] |
| R3 — initial grid (regression) | 3 by 3 grid, initialized reservoir pressure | every cell 5000 psi; pytest.approx defaults | `tests/marine_ops/reservoir/test_modeling.py::TestReservoirSimulation::test_setup_grid_initial_pressure` | PASS at BASE and AFTER | [tests/marine_ops/reservoir/test_modeling.py:L532-L537] |
| R4 — legacy pore volume (regression candidate) | 640 acres × 50 ft × 0.2 porosity × 0.8 net-to-gross | 5120 acre-ft; one decimal-place comparison, not independent empirical evidence | `tests/test_reservoir_framework.py::TestReservoirProperties::test_pore_volume_calculation` | SKIP at BASE and AFTER: Reservoir modules not available | [tests/test_reservoir_framework.py:L145-L188] |

The full demonstration script imports facade names that are not exported, so it is a discovery lead rather than an additional tested worked case; the basic-usage script is a TODO, not a numerical example. [examples/domains/reservoir/reservoir_analysis_examples.py:L22-L38] [src/digitalmodel/marine_ops/reservoir/__init__.py:L17-L73] [examples/marine_ops/reservoir/basic_usage.py:L1-L2]

Offline record at `c099a5e1317e6bed63d876252d87cbb12c774372`: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest tests/marine_ops/reservoir tests/reservoir tests/test_reservoir_framework.py -q -rA -p no:cacheprovider --basetemp=<scratch>`; no addopts override; 157 collected, 127 passed, 0 failed, 30 skipped, 0 NOT_RUN; 104 passing tests exercise modeling directly, and 23 passing tests exercise the separate flat reservoir package. AFTER repeats 127 passed, 0 failed, 30 skipped and 0 NOT_RUN with the same command form on the uncommitted documentation tree; module code remains at the code pin. The 30 framework skips follow its caught import failure and existing per-test skip, not a missing optional extra. [tests/test_reservoir_framework.py:L21-L57] [src/digitalmodel/reservoir/__init__.py:L1-L1]

## 5. QA gates

The gate applicability is AQG r0, section 10, Python calculations, with G5 from section 9.


| Gate | Applicability / route | Checks |
| --- | --- | --- |
| G0 | APPLICABLE — input bounds, units and missing data; correlation provenance remains open | C01, C02, C06, C07, C08 |
| G1 | APPLICABLE — Python imports and offline tests; no external program input is produced | C03 |
| G2 | APPLICABLE — independent linear-fit identity and pressure arithmetic; no calibrated field benchmark claimed | C04, C05 |
| G3 | NOT_APPLICABLE — no dynamic solver model is produced by the tank calculation | none |
| G4 | NOT_APPLICABLE — no diffraction bodies or meshes are produced | none |
| G5 | APPLICABLE — report structure and explicit error evidence; signed appendix remains open | C09, C10 |

| Check | Gate | Function / quantity | Numeric pass criterion and unit | Evidence produced | Source |
| --- | --- | --- | --- | --- | --- |
| C01 | G0 | Construct RockProperties with invalid porosity, permeability and net-to-gross | 3 of 3 invalid inputs raise ValueError; count; dimensionless fractions and mD | scratch self-check | [src/digitalmodel/marine_ops/reservoir/properties.py:L53-L60] |
| C02 | G0 | Scalar porosity calculation with shale_correction=False; water-saturation bounds | mean of 0.2 and 0.18 equals 0.19 within 1e-12 fraction; Archie output in [0,1] fraction | scratch self-check | [src/digitalmodel/marine_ops/reservoir/properties.py:L291-L315] [src/digitalmodel/marine_ops/reservoir/properties.py:L355-L405] |
| C03 | G1 | Run all discovered reservoir test files in section 4 | 0 failed tests and 0 collection errors; 30 skips retained as coverage gap; count | pytest summaries | [tests/test_reservoir_framework.py:L21-L57] [tests/marine_ops/reservoir/test_modeling.py:L23-L38] |
| C04 | G2 | WellTestAnalysis._calculate_r_squared on y=2x | absolute R² error <=1e-10 dimensionless; V1 node | pytest PASS | [tests/marine_ops/reservoir/test_modeling.py:L466-L474] |
| C05 | G2 | MaterialBalance.tank_material_balance pressure drop | 500 psi within pytest.approx default relative 1e-6 or absolute 1e-12 psi; R2 node | pytest PASS | [tests/marine_ops/reservoir/test_modeling.py:L326-L333] |
| C06 | G0 | ReservoirCharacterization.calculate_net_pay complement | hydrocarbon_saturation = 1-average_water_saturation within 1e-10 fraction; existing node | pytest PASS | [tests/marine_ops/reservoir/test_modeling.py:L888-L897] |
| C07 | G0 | MaterialBalance empty history result | 0 cumulative oil/gas/water and 0 psi drop, OOIP estimate None; R1 node | pytest PASS | [tests/marine_ops/reservoir/test_modeling.py:L347-L356] |
| C08 | G0 | Buildup default viscosity; grid missing initial pressure; constant-y fit | permeability >0 mD; every grid cell 3000 psi; constant-y R² exactly 0; existing nodes | pytest PASS | [tests/marine_ops/reservoir/test_modeling.py:L486-L510] [tests/marine_ops/reservoir/test_modeling.py:L621-L628] |
| C09 | G5 | ReservoirModel.export_summary keys | 4 of 4 sections present: model_info, reservoir_properties, reserves, wells; count | pytest PASS | [tests/marine_ops/reservoir/test_modeling.py:L1112-L1119] |
| C10 | G5 | ReservoirModel.run_integrated_analysis with one insufficient well test | 1 of 1 failed-well entries contains error; count | pytest PASS | [tests/marine_ops/reservoir/test_modeling.py:L1093-L1102] |

C06 uses `tests/marine_ops/reservoir/test_modeling.py::TestReservoirCharacterization::test_hydrocarbon_saturation_complement`; C08 uses the three `TestWellTestAnalysis` / `TestReservoirSimulation` tests named in its cited ranges; C09 and C10 use their cited existing test definitions. These checks detect the specified behaviors only; they do not validate the physical defaults or convert an error dictionary to a successful calculation. [tests/marine_ops/reservoir/test_modeling.py:L486-L510] [tests/marine_ops/reservoir/test_modeling.py:L621-L628] [tests/marine_ops/reservoir/test_modeling.py:L888-L897] [tests/marine_ops/reservoir/test_modeling.py:L1093-L1102] [tests/marine_ops/reservoir/test_modeling.py:L1112-L1119]

## 6. Known failure modes and pitfalls


sweep: 196 found

The count enumerates default, fallback, masked-record and error-conversion sites, including intentional guarded behavior; it is not a count of confirmed independent defects. The following exact discovery commands were run, with every match classified against its enclosing definition:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|\b(?:int|float)\(|\b(?:0(?:\.0)?|1970)\b|kwargs.get|\.get\(|\bor [0-9]|[Aa]ssum|[Dd]efault|[Pp]laceholder|clip|maximum|minimum|null_value|fill_value|to_csv|save|write' src/digitalmodel/marine_ops/reservoir --glob '*.py'
rg -n 'except|continue|return|int\(|float\(|1970|cache|unit|assum|default' src/digitalmodel/marine_ops/reservoir --glob '*.py'
```


| Site | Symptom / cause | Detection in section 5 | Tracking / date |
| --- | --- | --- | --- |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L44-L44] | Rock defaults supply net-to-gross, matrix density or exponents without site calibration. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L47-L47] | Rock defaults supply net-to-gross, matrix density or exponents without site calibration. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L50-L50] | Rock defaults supply net-to-gross, matrix density or exponents without site calibration. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L51-L51] | Rock defaults supply net-to-gross, matrix density or exponents without site calibration. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L72-L72] | Bulk-density water density or formation-factor unity coefficient is assumed. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L86-L86] | Bulk-density water density or formation-factor unity coefficient is assumed. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L109-L109] | Fluid property defaults supply unmeasured density, viscosity, formation volume factors, solution gas or compressibility. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L112-L112] | Fluid property defaults supply unmeasured density, viscosity, formation volume factors, solution gas or compressibility. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L113-L113] | Fluid property defaults supply unmeasured density, viscosity, formation volume factors, solution gas or compressibility. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L114-L114] | Fluid property defaults supply unmeasured density, viscosity, formation volume factors, solution gas or compressibility. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L115-L115] | Fluid property defaults supply unmeasured density, viscosity, formation volume factors, solution gas or compressibility. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L117-L117] | Fluid property defaults supply unmeasured density, viscosity, formation volume factors, solution gas or compressibility. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L126-L126] | Missing oil API input propagates None rather than a density result. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L140-L140] | Missing oil API input propagates None rather than a density result. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L163-L163] | Default z, initial-pressure substitution, initial z assumption, PVT, saturation or inferred zero gas can hide missing inputs. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L169-L169] | Default z, initial-pressure substitution, initial z assumption, PVT, saturation or inferred zero gas can hide missing inputs. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L184-L184] | Default z, initial-pressure substitution, initial z assumption, PVT, saturation or inferred zero gas can hide missing inputs. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L207-L207] | Default z, initial-pressure substitution, initial z assumption, PVT, saturation or inferred zero gas can hide missing inputs. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L210-L210] | Default z, initial-pressure substitution, initial z assumption, PVT, saturation or inferred zero gas can hide missing inputs. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L219-L219] | Default z, initial-pressure substitution, initial z assumption, PVT, saturation or inferred zero gas can hide missing inputs. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L227-L227] | Saturation sum only warns; individual invalid phase fractions may persist. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L237-L237] | Unavailable volume or absent phase returns None; callers must distinguish absence from zero. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L250-L250] | Unavailable volume or absent phase returns None; callers must distinguish absence from zero. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L263-L263] | Unavailable volume or absent phase returns None; callers must distinguish absence from zero. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L280-L280] | Unavailable volume or absent phase returns None; callers must distinguish absence from zero. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L266-L266] | Volumetric conversion assumes field units; no input unit metadata is enforced. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L283-L283] | Volumetric conversion assumes field units; no input unit metadata is enforced. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L315-L315] | Porosity or saturation clipping can conceal out-of-range input or raw result. | C02 checks scalar mean/bounds only; no detection check exists for raw invalid values | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L405-L405] | Porosity or saturation clipping can conceal out-of-range input or raw result. | C02 checks scalar mean/bounds only; no detection check exists for raw invalid values | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L336-L336] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L341-L341] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L346-L346] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L352-L352] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L376-L376] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L377-L377] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L378-L378] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L385-L385] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L388-L388] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L392-L392] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L393-L393] | Correlation default or empirical simplification/floor affects the result without applicability validation. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L412-L412] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L429-L429] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L430-L430] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L431-L431] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L436-L436] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L437-L437] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L442-L442] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L443-L443] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L459-L459] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L476-L476] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L477-L477] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L478-L478] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L483-L483] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L484-L484] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L489-L489] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L490-L490] | Quick-setup lithology, saturation, exponent, fluid or PVT assumption; filtered kwargs may duplicate explicit arguments. | no detection check exists | issue pending (1); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L52-L52] | Well defaults assume zero skin, fixed radius or TVD equal to MD. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L53-L53] | Well defaults assume zero skin, fixed radius or TVD equal to MD. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L58-L58] | Well defaults assume zero skin, fixed radius or TVD equal to MD. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L79-L79] | Absent production rates default to zero in STB/day or Mscf/day. | no detection check exists | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L80-L80] | Absent production rates default to zero in STB/day or Mscf/day. | no detection check exists | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L81-L81] | Absent production rates default to zero in STB/day or Mscf/day. | no detection check exists | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L135-L135] | No history returns empty arrays; populated history returns scalar totals. | C07 | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L147-L147] | Elapsed-time integration uses whole days, losing sub-day intervals. | no detection check exists | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L186-L186] | Zero or missing rock compressibility silently selects an assumed compressibility. | no detection check exists | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L194-L194] | Zero or missing rock compressibility silently selects an assumed compressibility. | no detection check exists | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L192-L192] | Mscf cumulative gas is multiplied by Bg in reservoir ft³/scf and added to reservoir oil barrels without scale/volume conversion. | no detection check exists | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L199-L199] | Nonpositive expansion or absent cumulative oil sets OOIP estimate to None. | C07 covers no-production branch only | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L201-L201] | Nonpositive expansion or absent cumulative oil sets OOIP estimate to None. | C07 covers no-production branch only | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L220-L220] | Empty cumulative input becomes 0.0; missing history is numerically indistinguishable from zero withdrawal. | C07 | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L269-L269] | Buildup fixes preproduction duration, fit window, thickness, flow rate or compressibility; nonpositive first pressure substitutes intercept. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L273-L273] | Buildup fixes preproduction duration, fit window, thickness, flow rate or compressibility; nonpositive first pressure substitutes intercept. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L274-L274] | Buildup fixes preproduction duration, fit window, thickness, flow rate or compressibility; nonpositive first pressure substitutes intercept. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L293-L293] | Buildup fixes preproduction duration, fit window, thickness, flow rate or compressibility; nonpositive first pressure substitutes intercept. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L294-L294] | Buildup fixes preproduction duration, fit window, thickness, flow rate or compressibility; nonpositive first pressure substitutes intercept. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L304-L304] | Buildup fixes preproduction duration, fit window, thickness, flow rate or compressibility; nonpositive first pressure substitutes intercept. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L305-L305] | Buildup fixes preproduction duration, fit window, thickness, flow rate or compressibility; nonpositive first pressure substitutes intercept. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L308-L308] | Buildup fixes preproduction duration, fit window, thickness, flow rate or compressibility; nonpositive first pressure substitutes intercept. | no detection check exists | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L290-L290] | Buildup viscosity defaults to unity; constant pressure R² returns zero. | C08 | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L325-L325] | Buildup viscosity defaults to unity; constant pressure R² returns zero. | C08 | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L342-L342] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L358-L358] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L365-L365] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L382-L382] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L388-L388] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L392-L392] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L397-L397] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L398-L398] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L406-L406] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L414-L414] | Depletion uses fixed grid/step and missing-or-zero pressure, gas fraction, volume, compressibility, cutoff or OOIP assumptions; pressure still declines after production becomes zero. | C08 covers pressure initialization only; no detection check exists for depletion balance | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L456-L456] | Decline analysis truncates elapsed time to days and silently filters nonpositive oil rates. | no detection check exists | issue pending (5); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L457-L457] | Decline analysis truncates elapsed time to days and silently filters nonpositive oil rates. | no detection check exists | issue pending (5); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L473-L473] | Decline exponent zero is the exponential branch; hyperbolic guesses are not fitted and non-exponential R² is fixed. | no detection check exists for calibration | issue pending (5); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L480-L480] | Decline exponent zero is the exponential branch; hyperbolic guesses are not fitted and non-exponential R² is fixed. | no detection check exists for calibration | issue pending (5); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L481-L481] | Decline exponent zero is the exponential branch; hyperbolic guesses are not fitted and non-exponential R² is fixed. | no detection check exists for calibration | issue pending (5); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L504-L504] | Decline exponent zero is the exponential branch; hyperbolic guesses are not fitted and non-exponential R² is fixed. | no detection check exists for calibration | issue pending (5); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L534-L534] | Linspace spacing differs from the 30-day cumulative multiplier; short forecasts can have zero samples. | no detection check exists | issue pending (5); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L547-L547] | Linspace spacing differs from the 30-day cumulative multiplier; short forecasts can have zero samples. | no detection check exists | issue pending (5); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L592-L592] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L593-L593] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L611-L611] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L612-L612] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L613-L613] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L617-L617] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L621-L621] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L625-L625] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L626-L626] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L627-L627] | Net-pay cutoff, missing-column or depth-step defaults can suppress pay or fabricate properties; zero gross thickness returns zero ratio. | C06 covers complement only; no detection check exists for missing columns | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L647-L647] | Insufficient mapping wells warn and return empty mapping. | no detection check exists in section 5 | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L663-L663] | Unlogged wells inherit reservoir properties or assumed thickness; mapping uses a fixed grid of mean values. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L664-L664] | Unlogged wells inherit reservoir properties or assumed thickness; mapping uses a fixed grid of mean values. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L674-L674] | Unlogged wells inherit reservoir properties or assumed thickness; mapping uses a fixed grid of mean values. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L766-L766] | Integrated well-test exceptions are returned as error dictionaries. | C10 | issue pending (4); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/modeling.py:L777-L777] | Material-balance exception becomes an error dictionary without failing the entire integrated result. | no detection check exists | issue pending (3); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L49-L49] | Equality sentinel masking leaves NaN valid; insufficient or out-of-range interpolation returns sentinel, possibly cast to target dtype. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L58-L58] | Equality sentinel masking leaves NaN valid; insufficient or out-of-range interpolation returns sentinel, possibly cast to target dtype. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L80-L80] | Equality sentinel masking leaves NaN valid; insufficient or out-of-range interpolation returns sentinel, possibly cast to target dtype. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L87-L87] | Equality sentinel masking leaves NaN valid; insufficient or out-of-range interpolation returns sentinel, possibly cast to target dtype. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L194-L194] | Facies and porosity use fixed spacing, hard-coded null sentinel/densities and clipped porosity; custom sentinel and overlap endpoints need checks. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L206-L206] | Facies and porosity use fixed spacing, hard-coded null sentinel/densities and clipped porosity; custom sentinel and overlap endpoints need checks. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L251-L251] | Facies and porosity use fixed spacing, hard-coded null sentinel/densities and clipped porosity; custom sentinel and overlap endpoints need checks. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L258-L258] | Facies and porosity use fixed spacing, hard-coded null sentinel/densities and clipped porosity; custom sentinel and overlap endpoints need checks. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L259-L259] | Facies and porosity use fixed spacing, hard-coded null sentinel/densities and clipped porosity; custom sentinel and overlap endpoints need checks. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L268-L268] | Facies and porosity use fixed spacing, hard-coded null sentinel/densities and clipped porosity; custom sentinel and overlap endpoints need checks. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L281-L281] | Archie log path assumes Rw/exponents/spacing, masks nonpositive input and clips saturation; coefficients are independent of scalar property helper. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L309-L309] | Archie log path assumes Rw/exponents/spacing, masks nonpositive input and clips saturation; coefficients are independent of scalar property helper. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L314-L314] | Archie log path assumes Rw/exponents/spacing, masks nonpositive input and clips saturation; coefficients are independent of scalar property helper. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L318-L318] | Archie log path assumes Rw/exponents/spacing, masks nonpositive input and clips saturation; coefficients are independent of scalar property helper. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L319-L319] | Archie log path assumes Rw/exponents/spacing, masks nonpositive input and clips saturation; coefficients are independent of scalar property helper. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L320-L320] | Archie log path assumes Rw/exponents/spacing, masks nonpositive input and clips saturation; coefficients are independent of scalar property helper. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L323-L323] | Archie log path assumes Rw/exponents/spacing, masks nonpositive input and clips saturation; coefficients are independent of scalar property helper. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L329-L329] | Archie log path assumes Rw/exponents/spacing, masks nonpositive input and clips saturation; coefficients are independent of scalar property helper. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L343-L343] | Log net pay assumes cutoffs and spacing; arrays are combined without depth realignment, and no pay returns zero porosity/unit saturation. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L367-L367] | Log net pay assumes cutoffs and spacing; arrays are combined without depth realignment, and no pay returns zero porosity/unit saturation. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L378-L378] | Log net pay assumes cutoffs and spacing; arrays are combined without depth realignment, and no pay returns zero porosity/unit saturation. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L385-L385] | Log net pay assumes cutoffs and spacing; arrays are combined without depth realignment, and no pay returns zero porosity/unit saturation. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L386-L386] | Log net pay assumes cutoffs and spacing; arrays are combined without depth realignment, and no pay returns zero porosity/unit saturation. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L491-L491] | Flattening uses mean/minimum/zero fallback datum; no common markers returns empty list. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L501-L501] | Flattening uses mean/minimum/zero fallback datum; no common markers returns empty list. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L530-L530] | Flattening uses mean/minimum/zero fallback datum; no common markers returns empty list. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L535-L535] | Flattening uses mean/minimum/zero fallback datum; no common markers returns empty list. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L545-L545] | Common markers are sorted by name rather than by shallowest depth despite the caller comment. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L651-L651] | Facies failure leaves a warning/blank track; missing datum defaults to zero; no valid GR/RT silently omits track. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L661-L661] | Facies failure leaves a warning/blank track; missing datum defaults to zero; no valid GR/RT silently omits track. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L685-L685] | Facies failure leaves a warning/blank track; missing datum defaults to zero; no valid GR/RT silently omits track. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L705-L705] | Facies failure leaves a warning/blank track; missing datum defaults to zero; no valid GR/RT silently omits track. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L710-L710] | Facies failure leaves a warning/blank track; missing datum defaults to zero; no valid GR/RT silently omits track. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L756-L756] | Density-neutron plot assumes quartz/water density without caller metadata. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L843-L843] | Unknown structural wells are silently omitted; structure/property mapping extends bounds and returns NaN outside the convex hull. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L857-L857] | Unknown structural wells are silently omitted; structure/property mapping extends bounds and returns NaN outside the convex hull. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L858-L858] | Unknown structural wells are silently omitted; structure/property mapping extends bounds and returns NaN outside the convex hull. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L943-L943] | Unknown structural wells are silently omitted; structure/property mapping extends bounds and returns NaN outside the convex hull. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L944-L944] | Unknown structural wells are silently omitted; structure/property mapping extends bounds and returns NaN outside the convex hull. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1000-L1000] | Analogue selection silently excludes unknown or empty wells; cumulative estimate uses average-rate duration and ignores target_well. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1002-L1002] | Analogue selection silently excludes unknown or empty wells; cumulative estimate uses average-rate duration and ignores target_well. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1005-L1005] | Analogue selection silently excludes unknown or empty wells; cumulative estimate uses average-rate duration and ignores target_well. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1070-L1070] | Core interpolation inserts NaN, drops invalid pairs and returns an error dictionary for insufficient data. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1078-L1078] | Core interpolation inserts NaN, drops invalid pairs and returns an error dictionary for insufficient data. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1091-L1091] | Core interpolation inserts NaN, drops invalid pairs and returns an error dictionary for insufficient data. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1141-L1141] | Net-pay exceptions yield warnings or per-well error dictionaries; exported type is hard-coded Conventional. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1168-L1168] | Net-pay exceptions yield warnings or per-well error dictionaries; exported type is hard-coded Conventional. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1190-L1190] | Net-pay exceptions yield warnings or per-well error dictionaries; exported type is hard-coded Conventional. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L78-L78] | Migration skips missing well logs, synthesizes formation names, or falls back to separate plotting logic after exceptions; fallback skips unselected groups. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L125-L125] | Migration skips missing well logs, synthesizes formation names, or falls back to separate plotting logic after exceptions; fallback skips unselected groups. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L158-L158] | Migration skips missing well logs, synthesizes formation names, or falls back to separate plotting logic after exceptions; fallback skips unselected groups. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L235-L235] | Migration skips missing well logs, synthesizes formation names, or falls back to separate plotting logic after exceptions; fallback skips unselected groups. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L179-L179] | Fallback thickness is signed while modern layer thickness is absolute; data limits become fallback tops/bases and density uses a different matrix assumption. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L243-L243] | Fallback thickness is signed while modern layer thickness is absolute; data limits become fallback tops/bases and density uses a different matrix assumption. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L307-L307] | Fallback thickness is signed while modern layer thickness is absolute; data limits become fallback tops/bases and density uses a different matrix assumption. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L406-L406] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L410-L410] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L433-L433] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L435-L435] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L502-L502] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L503-L503] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L504-L504] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L523-L523] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L524-L524] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L525-L525] | Discovery/reporting substitutes zero counts or empty lists for absent well/formation/curve metadata. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/properties.py:L294-L315] | The photoelectric_factor argument is accepted but is not consumed by the porosity branches. | no detection check exists | issue pending (2); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L117-L117] | Layer thickness takes an absolute difference, concealing reversed top/base ordering. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L174-L174] | Facies uses only available requested curves; missing requested channels can be omitted when two remain. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L183-L183] | Empty valid-depth curves do not contribute to common-range selection. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L270-L276] | The in-memory porosity result can store empty arrays when curve ranges do not overlap; no empty-result guard precedes the write. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L331-L338] | The in-memory saturation result can store empty depth/value arrays after nonoverlap; subsequent consumers require nonempty data. | no detection check exists | issue pending (6); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L585-L585] | Plot layout defaults hspace to zero; this is a display assumption, not missing engineering data. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L586-L586] | Plot layout defaults wspace to zero; this is a display assumption, not missing engineering data. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L930-L930] | Property mapping silently omits entries absent from well_locations if caller mutation leaves dictionaries inconsistent. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1076-L1076] | Core/log pair masking drops NaN pairs before correlation; retained-pair count must be checked. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1174-L1174] | Comprehensive results are cached in memory even when well entries contain error dictionaries; this is not a successful well calculation. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/analysis.py:L1196-L1196] | A report exported before analysis can include empty detailed_results. | no detection check exists | issue pending (7); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L219-L219] | Fallback plot layout defaults hspace to zero; display-only assumption. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L220-L220] | Fallback plot layout defaults wspace to zero; display-only assumption. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L398-L398] | Empty log input leaves an empty discovery dictionary; report output treats it as absent log data. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L399-L399] | Empty formation input leaves an empty discovery dictionary; report output treats it as absent top data. | no detection check exists | issue pending (8); 2026-10-18 |
| [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L510-L510] | Report output substitutes an empty facies-class list when that metadata is absent. | no detection check exists | issue pending (8); 2026-10-18 |

The `int()` calls select fit-window indices and forecast sample counts, and `_cumulative_value` converts numeric arrays; none parses external numeric text. No disk result-cache writer or epoch fallback occurs in these paths; in-memory petrophysical/result stores are listed above, and migration writes figures/text. [src/digitalmodel/marine_ops/reservoir/modeling.py:L215-L223] [src/digitalmodel/marine_ops/reservoir/modeling.py:L273-L274] [src/digitalmodel/marine_ops/reservoir/modeling.py:L534-L547] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L151-L164] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L563-L570]

Additional confirmed pitfalls: the README imports missing facade helpers; the helpers themselves can forward pressure/temperature twice, and the direct modeling test provides a NumPy trapz compatibility alias, which can conceal a runtime incompatibility outside that test. [src/digitalmodel/marine_ops/reservoir/README.md:L29-L43] [src/digitalmodel/marine_ops/reservoir/__init__.py:L17-L73] [src/digitalmodel/marine_ops/reservoir/properties.py:L441-L445] [src/digitalmodel/marine_ops/reservoir/properties.py:L488-L492] [tests/marine_ops/reservoir/test_modeling.py:L19-L22]


| Incident / lead | Disposition | Detection | Evidence |
| --- | --- | --- | --- |
| #566 (closed) | No-production contract now expects a degenerate result, not an exception | C07 | [tests/marine_ops/reservoir/test_modeling.py:L347-L356] [docs/plans/2026-05-05-issue-566-marine-ops-r1-r5-r7-r8-batched.md:L43-L52] |
| #1743 (open) | Production-profile enhancement lead; does not validate the existing fixed hyperbolic fit | no detection check exists for calibrated forecasts | [src/digitalmodel/marine_ops/reservoir/modeling.py:L475-L504] |
| #1746 (open) | Integration enhancement lead; not evidence of an implemented end-to-end forecast/economics contract | no detection check exists for the requested integrated workflow | [src/digitalmodel/marine_ops/reservoir/modeling.py:L728-L780] |
| issue pending (1) | 30 framework tests skip because expected flat API exports are absent | C03 records 30 skips; no successful framework coverage | [tests/test_reservoir_framework.py:L21-L57] [src/digitalmodel/reservoir/__init__.py:L1-L1] |

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change | Shared-log entry |
| --- | --- | --- | --- | --- | --- |
| 2026-10-18 | Initial pack | A successful modeling test does not establish working facade examples or execution of the legacy framework | [src/digitalmodel/marine_ops/reservoir/__init__.py:L17-L73] [tests/test_reservoir_framework.py:L21-L57] | Record skip and API gaps separately | No entry assigned |

## 8. Ownership

- Family quality manager: engineering calculations quality manager.
- Deputy role: engineering calculations deputy quality manager; appointment requires maintainer confirmation.
- Producing role: engineering calculations contributors.
- Method, tolerance and classification disputes: family quality manager; cross-family scope or missing appointment: maintainers.
- These are review responsibilities for this draft, not an assertion of completed independent review.

## 9. Dependencies and implementation inventory

The facade imports properties, modeling and analysis eagerly; analysis imports NumPy, Pandas, Matplotlib, SciPy and scikit-learn, while migration consumes the local analysis and well-data objects. [src/digitalmodel/marine_ops/reservoir/__init__.py:L17-L45] [src/digitalmodel/marine_ops/reservoir/analysis.py:L16-L29] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L11-L19]

The demonstration script is a downstream caller but requests missing facade exports; the plain flat package tests import a different reservoir package. No external solver version is a dependency of these numerical paths. [examples/domains/reservoir/reservoir_analysis_examples.py:L22-L38] [tests/reservoir/test_reservoir.py:L23-L38] [src/digitalmodel/marine_ops/reservoir/modeling.py:L14-L22]

### Counted map family

Implementation-map ref `origin/codex/implmap-work` was read at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`; its data revision is `270c8759c374b67a7f032953e7fcdb14e0224461`. The identity/order/relation of all 392 rows and all three search signals of the selected family agree with the copy at the code pin; the following locators use the code-pin copy so each resolves at the header revision. File/body digests and coordinates are snapshot metadata, not a claim of unchanged code. [docs/registry/implementation-map.yaml:L1-L3] [docs/registry/implementation-map.yaml:L16783-L16801]


| Rank | Family | In-package rows | Whole-family rows | Counted |
| --- | --- | --- | --- | --- |
| 1 | sn-curves-and-miner-damage | 7 | 392 | yes — only map family with package rows |

The map matches `log_a` inside `log_analysis`; the seven in-package rows below manipulate logs, markers, core correlation or delegated net pay rather than stress-life curves or Miner damage. Preserve the inventory while correcting the classification in the map follow-up (issue pending (9)). [src/digitalmodel/marine_ops/reservoir/analysis.py:L414-L436] [src/digitalmodel/marine_ops/reservoir/analysis.py:L451-L523] [src/digitalmodel/marine_ops/reservoir/analysis.py:L803-L811] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1046-L1092] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1119-L1176]

### Full inventory for the selected family

Every row is retained in map order, including definitions outside the package; outside-package names are represented by exact map locators.

| Map row | Definition | Map relation | Inventory evidence |
| --- | --- | --- | --- |
| 1 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L16790-L16799] |
| 2 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L16800-L16809] |
| 3 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L16810-L16819] |
| 4 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L16820-L16829] |
| 5 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L16830-L16843] |
| 6 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16844-L16855] |
| 7 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16856-L16865] |
| 8 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16866-L16875] |
| 9 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16876-L16885] |
| 10 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16886-L16895] |
| 11 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16896-L16905] |
| 12 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16906-L16921] |
| 13 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16922-L16931] |
| 14 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16932-L16941] |
| 15 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16942-L16951] |
| 16 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16952-L16961] |
| 17 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16962-L16971] |
| 18 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16972-L16981] |
| 19 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16982-L16991] |
| 20 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L16992-L17001] |
| 21 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17002-L17011] |
| 22 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17012-L17021] |
| 23 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17022-L17031] |
| 24 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17032-L17041] |
| 25 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17042-L17051] |
| 26 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17052-L17061] |
| 27 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17062-L17071] |
| 28 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17072-L17088] |
| 29 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17089-L17098] |
| 30 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17099-L17108] |
| 31 | Outside package; path and qualified symbol at locator | example | [docs/registry/implementation-map.yaml:L17109-L17121] |
| 32 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17122-L17130] |
| 33 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17131-L17139] |
| 34 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17140-L17149] |
| 35 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17150-L17159] |
| 36 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17160-L17169] |
| 37 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17170-L17179] |
| 38 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17180-L17189] |
| 39 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17190-L17199] |
| 40 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17200-L17211] |
| 41 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17212-L17222] |
| 42 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17223-L17232] |
| 43 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17233-L17244] |
| 44 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17245-L17256] |
| 45 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17257-L17266] |
| 46 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17267-L17276] |
| 47 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17277-L17286] |
| 48 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17287-L17296] |
| 49 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17297-L17306] |
| 50 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17307-L17315] |
| 51 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17316-L17325] |
| 52 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17326-L17335] |
| 53 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17336-L17347] |
| 54 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17348-L17357] |
| 55 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17358-L17367] |
| 56 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17368-L17377] |
| 57 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17378-L17387] |
| 58 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17388-L17399] |
| 59 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L17400-L17417] |
| 60 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17418-L17430] |
| 61 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17431-L17440] |
| 62 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17441-L17450] |
| 63 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17451-L17464] |
| 64 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17465-L17473] |
| 65 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17474-L17483] |
| 66 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17484-L17493] |
| 67 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17494-L17503] |
| 68 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17504-L17513] |
| 69 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17514-L17523] |
| 70 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17524-L17533] |
| 71 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17534-L17543] |
| 72 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17544-L17553] |
| 73 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17554-L17563] |
| 74 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17564-L17575] |
| 75 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17576-L17585] |
| 76 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17586-L17597] |
| 77 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17598-L17607] |
| 78 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17608-L17619] |
| 79 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17620-L17629] |
| 80 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17630-L17642] |
| 81 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17643-L17652] |
| 82 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L17653-L17664] |
| 83 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L17665-L17675] |
| 84 | Outside package; path and qualified symbol at locator | wrapper | [docs/registry/implementation-map.yaml:L17676-L17687] |
| 85 | Outside package; path and qualified symbol at locator | wrapper | [docs/registry/implementation-map.yaml:L17688-L17699] |
| 86 | Outside package; path and qualified symbol at locator | wrapper | [docs/registry/implementation-map.yaml:L17700-L17711] |
| 87 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17712-L17721] |
| 88 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L17722-L17732] |
| 89 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17733-L17742] |
| 90 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17743-L17752] |
| 91 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17753-L17762] |
| 92 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17763-L17772] |
| 93 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17773-L17784] |
| 94 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17785-L17795] |
| 95 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17796-L17806] |
| 96 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L17807-L17817] |
| 97 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17818-L17827] |
| 98 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17828-L17837] |
| 99 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17838-L17848] |
| 100 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L17849-L17859] |
| 101 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17860-L17869] |
| 102 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17870-L17880] |
| 103 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17881-L17890] |
| 104 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17891-L17901] |
| 105 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L17902-L17912] |
| 106 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17913-L17922] |
| 107 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17923-L17932] |
| 108 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17933-L17942] |
| 109 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L17943-L17954] |
| 110 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17955-L17964] |
| 111 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L17965-L17974] |
| 112 | Outside package; path and qualified symbol at locator | wrapper | [docs/registry/implementation-map.yaml:L17975-L17986] |
| 113 | Outside package; path and qualified symbol at locator | wrapper | [docs/registry/implementation-map.yaml:L17987-L17999] |
| 114 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18000-L18009] |
| 115 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18010-L18019] |
| 116 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18020-L18030] |
| 117 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18031-L18040] |
| 118 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18041-L18050] |
| 119 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18051-L18060] |
| 120 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L18061-L18078] |
| 121 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18079-L18088] |
| 122 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18089-L18098] |
| 123 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18099-L18108] |
| 124 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18109-L18120] |
| 125 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18121-L18130] |
| 126 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L18131-L18143] |
| 127 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18144-L18153] |
| 128 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18154-L18163] |
| 129 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18164-L18176] |
| 130 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18177-L18186] |
| 131 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18187-L18196] |
| 132 | Outside package; path and qualified symbol at locator | wrapper | [docs/registry/implementation-map.yaml:L18197-L18208] |
| 133 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18209-L18220] |
| 134 | Outside package; path and qualified symbol at locator | wrapper | [docs/registry/implementation-map.yaml:L18221-L18232] |
| 135 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18233-L18242] |
| 136 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18243-L18252] |
| 137 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18253-L18262] |
| 138 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18263-L18272] |
| 139 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18273-L18282] |
| 140 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18283-L18292] |
| 141 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18293-L18302] |
| 142 | `src/digitalmodel/marine_ops/reservoir/analysis.py` / `StratigraphicAnalysis.add_well_analysis` | independent | [docs/registry/implementation-map.yaml:L18303-L18312] |
| 143 | `src/digitalmodel/marine_ops/reservoir/analysis.py` / `StratigraphicAnalysis.set_well_order` | independent | [docs/registry/implementation-map.yaml:L18313-L18322] |
| 144 | `src/digitalmodel/marine_ops/reservoir/analysis.py` / `StratigraphicAnalysis.create_flattened_section` | independent | [docs/registry/implementation-map.yaml:L18323-L18332] |
| 145 | `src/digitalmodel/marine_ops/reservoir/analysis.py` / `WellCorrelation.add_well_data` | independent | [docs/registry/implementation-map.yaml:L18333-L18342] |
| 146 | `src/digitalmodel/marine_ops/reservoir/analysis.py` / `CoreAnalysis.correlate_core_to_logs` | independent | [docs/registry/implementation-map.yaml:L18343-L18352] |
| 147 | `src/digitalmodel/marine_ops/reservoir/analysis.py` / `ReservoirAnalysis.add_well_analysis` | independent | [docs/registry/implementation-map.yaml:L18353-L18362] |
| 148 | `src/digitalmodel/marine_ops/reservoir/analysis.py` / `ReservoirAnalysis.run_comprehensive_analysis` | independent | [docs/registry/implementation-map.yaml:L18363-L18372] |
| 149 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L18373-L18386] |
| 150 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18387-L18397] |
| 151 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18398-L18407] |
| 152 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18408-L18417] |
| 153 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18418-L18428] |
| 154 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18429-L18438] |
| 155 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18439-L18448] |
| 156 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18449-L18457] |
| 157 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L18458-L18469] |
| 158 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18470-L18479] |
| 159 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L18480-L18491] |
| 160 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18492-L18502] |
| 161 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18503-L18512] |
| 162 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18513-L18522] |
| 163 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18523-L18532] |
| 164 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18533-L18542] |
| 165 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18543-L18552] |
| 166 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18553-L18562] |
| 167 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18563-L18572] |
| 168 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18573-L18582] |
| 169 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18583-L18592] |
| 170 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18593-L18608] |
| 171 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18609-L18620] |
| 172 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18621-L18631] |
| 173 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18632-L18642] |
| 174 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18643-L18652] |
| 175 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18653-L18662] |
| 176 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18663-L18672] |
| 177 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18673-L18682] |
| 178 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18683-L18692] |
| 179 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18693-L18702] |
| 180 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18703-L18715] |
| 181 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18716-L18725] |
| 182 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18726-L18735] |
| 183 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18736-L18745] |
| 184 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18746-L18755] |
| 185 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18756-L18765] |
| 186 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18766-L18775] |
| 187 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18776-L18785] |
| 188 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18786-L18795] |
| 189 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18796-L18805] |
| 190 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18806-L18815] |
| 191 | Outside package; path and qualified symbol at locator | copy | [docs/registry/implementation-map.yaml:L18816-L18825] |
| 192 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18826-L18837] |
| 193 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18838-L18847] |
| 194 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18848-L18857] |
| 195 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L18858-L18869] |
| 196 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18870-L18879] |
| 197 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L18880-L18893] |
| 198 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18894-L18903] |
| 199 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18904-L18912] |
| 200 | Outside package; path and qualified symbol at locator | wrapper | [docs/registry/implementation-map.yaml:L18913-L18923] |
| 201 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18924-L18933] |
| 202 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18934-L18943] |
| 203 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18944-L18955] |
| 204 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18956-L18965] |
| 205 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18966-L18975] |
| 206 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18976-L18985] |
| 207 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18986-L18995] |
| 208 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L18996-L19005] |
| 209 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19006-L19015] |
| 210 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19016-L19024] |
| 211 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19025-L19036] |
| 212 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19037-L19046] |
| 213 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19047-L19056] |
| 214 | Outside package; path and qualified symbol at locator | wrapper | [docs/registry/implementation-map.yaml:L19057-L19070] |
| 215 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19071-L19080] |
| 216 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19081-L19090] |
| 217 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19091-L19100] |
| 218 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19101-L19110] |
| 219 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19111-L19120] |
| 220 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19121-L19130] |
| 221 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19131-L19140] |
| 222 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19141-L19150] |
| 223 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19151-L19159] |
| 224 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19160-L19169] |
| 225 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19170-L19179] |
| 226 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19180-L19189] |
| 227 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19190-L19199] |
| 228 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19200-L19209] |
| 229 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19210-L19219] |
| 230 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19220-L19229] |
| 231 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19230-L19239] |
| 232 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19240-L19248] |
| 233 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L19249-L19260] |
| 234 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19261-L19270] |
| 235 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L19271-L19282] |
| 236 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19283-L19292] |
| 237 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19293-L19302] |
| 238 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19303-L19312] |
| 239 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19313-L19324] |
| 240 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19325-L19334] |
| 241 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19335-L19343] |
| 242 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19344-L19353] |
| 243 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19354-L19363] |
| 244 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19364-L19373] |
| 245 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19374-L19383] |
| 246 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19384-L19393] |
| 247 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19394-L19403] |
| 248 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19404-L19413] |
| 249 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19414-L19423] |
| 250 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19424-L19433] |
| 251 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19434-L19443] |
| 252 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19444-L19453] |
| 253 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19454-L19463] |
| 254 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19464-L19473] |
| 255 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19474-L19483] |
| 256 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L19484-L19496] |
| 257 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L19497-L19509] |
| 258 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L19510-L19522] |
| 259 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19523-L19532] |
| 260 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19533-L19544] |
| 261 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19545-L19554] |
| 262 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19555-L19564] |
| 263 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19565-L19574] |
| 264 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19575-L19584] |
| 265 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19585-L19593] |
| 266 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19594-L19603] |
| 267 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19604-L19613] |
| 268 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19614-L19623] |
| 269 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19624-L19635] |
| 270 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19636-L19645] |
| 271 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19646-L19655] |
| 272 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19656-L19664] |
| 273 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19665-L19674] |
| 274 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19675-L19684] |
| 275 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19685-L19694] |
| 276 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19695-L19703] |
| 277 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L19704-L19715] |
| 278 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19716-L19725] |
| 279 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19726-L19735] |
| 280 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19736-L19745] |
| 281 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19746-L19755] |
| 282 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19756-L19767] |
| 283 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19768-L19777] |
| 284 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19778-L19787] |
| 285 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19788-L19797] |
| 286 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19798-L19807] |
| 287 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19808-L19817] |
| 288 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19818-L19827] |
| 289 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19828-L19837] |
| 290 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L19838-L19849] |
| 291 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19850-L19859] |
| 292 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19860-L19871] |
| 293 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L19872-L19883] |
| 294 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19884-L19893] |
| 295 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19894-L19903] |
| 296 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19904-L19916] |
| 297 | Outside package; path and qualified symbol at locator | caller | [docs/registry/implementation-map.yaml:L19917-L19928] |
| 298 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19929-L19938] |
| 299 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19939-L19952] |
| 300 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19953-L19963] |
| 301 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19964-L19973] |
| 302 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L19974-L19983] |
| 303 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L19984-L19993] |
| 304 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L19994-L20005] |
| 305 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20006-L20015] |
| 306 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20016-L20025] |
| 307 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20026-L20037] |
| 308 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20038-L20047] |
| 309 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20048-L20057] |
| 310 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20058-L20071] |
| 311 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20072-L20081] |
| 312 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20082-L20091] |
| 313 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20092-L20101] |
| 314 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20102-L20111] |
| 315 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20112-L20121] |
| 316 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20122-L20132] |
| 317 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20133-L20142] |
| 318 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20143-L20152] |
| 319 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20153-L20164] |
| 320 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20165-L20174] |
| 321 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20175-L20185] |
| 322 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20186-L20195] |
| 323 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20196-L20208] |
| 324 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20209-L20218] |
| 325 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20219-L20228] |
| 326 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20229-L20239] |
| 327 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20240-L20250] |
| 328 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20251-L20260] |
| 329 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20261-L20270] |
| 330 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20271-L20280] |
| 331 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20281-L20290] |
| 332 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20291-L20300] |
| 333 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20301-L20310] |
| 334 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20311-L20320] |
| 335 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20321-L20330] |
| 336 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20331-L20340] |
| 337 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20341-L20349] |
| 338 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20350-L20359] |
| 339 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20360-L20371] |
| 340 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20372-L20381] |
| 341 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20382-L20391] |
| 342 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20392-L20401] |
| 343 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20402-L20413] |
| 344 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20414-L20428] |
| 345 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20429-L20438] |
| 346 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20439-L20448] |
| 347 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20449-L20458] |
| 348 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20459-L20468] |
| 349 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20469-L20478] |
| 350 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20479-L20488] |
| 351 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20489-L20498] |
| 352 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20499-L20508] |
| 353 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20509-L20518] |
| 354 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20519-L20528] |
| 355 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20529-L20538] |
| 356 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20539-L20550] |
| 357 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20551-L20560] |
| 358 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20561-L20570] |
| 359 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20571-L20580] |
| 360 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20581-L20592] |
| 361 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20593-L20602] |
| 362 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20603-L20612] |
| 363 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20613-L20622] |
| 364 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20623-L20632] |
| 365 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20633-L20642] |
| 366 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20643-L20652] |
| 367 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20653-L20662] |
| 368 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20663-L20672] |
| 369 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20673-L20682] |
| 370 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20683-L20692] |
| 371 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20693-L20703] |
| 372 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20704-L20714] |
| 373 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20715-L20724] |
| 374 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20725-L20734] |
| 375 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20735-L20744] |
| 376 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20745-L20754] |
| 377 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20755-L20764] |
| 378 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20765-L20776] |
| 379 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20777-L20786] |
| 380 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20787-L20796] |
| 381 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20797-L20807] |
| 382 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20808-L20817] |
| 383 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20818-L20829] |
| 384 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20830-L20840] |
| 385 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20841-L20851] |
| 386 | Outside package; path and qualified symbol at locator | test_helper | [docs/registry/implementation-map.yaml:L20852-L20862] |
| 387 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L20863-L20873] |
| 388 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L20874-L20883] |
| 389 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L20884-L20893] |
| 390 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L20894-L20904] |
| 391 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L20905-L20914] |
| 392 | Outside package; path and qualified symbol at locator | independent | [docs/registry/implementation-map.yaml:L20915-L20924] |

### Exact search and hit classification

The counted search applies Python `re.search` case-sensitively to each package Python line at the code pin, assigning hits to the smallest enclosing function/class; module statements would use one module definition. Each signal is first compared case-insensitively against `marine_ops/reservoir`; no signal matches that package path, so none is omitted.

```text
algorithm: rainflow|miner|cycles_to_failure|log_a|log10.*(?:cycles|stress)|(?:stress|stress_range|delta_sigma).*\*\*.*\bm\b|damage_sum|thickness_correction|endurance_limit|sn_curve|SNcurve|sn_data|stress_life
vocabulary: S.N.curve|fatigue.damage|C203|BS.?7608|RP.?2A|SNcurve|SN.?Curve
constants_or_keys: thickness_correction|endurance_limit|n_cycles|sn_curve|stress_ranges
```

| Enclosing definition | Hit lines | Classification | Code evidence |
| --- | --- | --- | --- |
| `StratigraphicAnalysis.__init__` | 410 | Helper/data/export/wrapper; no stress-life or damage computation | [src/digitalmodel/marine_ops/reservoir/analysis.py:L410-L410] |
| `StratigraphicAnalysis.add_well_analysis` | 414, 419, 421 | Inventoried definition; unrelated log-processing match | [src/digitalmodel/marine_ops/reservoir/analysis.py:L414-L414] [src/digitalmodel/marine_ops/reservoir/analysis.py:L419-L419] [src/digitalmodel/marine_ops/reservoir/analysis.py:L421-L421] |
| `StratigraphicAnalysis.set_well_order` | 433 | Inventoried definition; unrelated log-processing match | [src/digitalmodel/marine_ops/reservoir/analysis.py:L433-L433] |
| `StratigraphicAnalysis.create_flattened_section` | 494, 496, 507, 510 | Inventoried definition; unrelated log-processing match | [src/digitalmodel/marine_ops/reservoir/analysis.py:L494-L494] [src/digitalmodel/marine_ops/reservoir/analysis.py:L496-L496] [src/digitalmodel/marine_ops/reservoir/analysis.py:L507-L507] [src/digitalmodel/marine_ops/reservoir/analysis.py:L510-L510] |
| `StratigraphicAnalysis.plot_stratigraphic_section` | 616, 625, 648, 650, 654, 655 | Helper/data/export/wrapper; no stress-life or damage computation | [src/digitalmodel/marine_ops/reservoir/analysis.py:L616-L616] [src/digitalmodel/marine_ops/reservoir/analysis.py:L625-L625] [src/digitalmodel/marine_ops/reservoir/analysis.py:L648-L648] [src/digitalmodel/marine_ops/reservoir/analysis.py:L650-L650] [src/digitalmodel/marine_ops/reservoir/analysis.py:L654-L654] [src/digitalmodel/marine_ops/reservoir/analysis.py:L655-L655] |
| `WellCorrelation.__init__` | 800 | Helper/data/export/wrapper; no stress-life or damage computation | [src/digitalmodel/marine_ops/reservoir/analysis.py:L800-L800] |
| `WellCorrelation.add_well_data` | 803, 808, 811 | Inventoried definition; unrelated log-processing match | [src/digitalmodel/marine_ops/reservoir/analysis.py:L803-L803] [src/digitalmodel/marine_ops/reservoir/analysis.py:L808-L808] [src/digitalmodel/marine_ops/reservoir/analysis.py:L811-L811] |
| `CoreAnalysis.correlate_core_to_logs` | 1046, 1051, 1062, 1063 | Inventoried definition; unrelated log-processing match | [src/digitalmodel/marine_ops/reservoir/analysis.py:L1046-L1046] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1051-L1051] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1062-L1062] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1063-L1063] |
| `ReservoirAnalysis.__init__` | 1108 | Helper/data/export/wrapper; no stress-life or damage computation | [src/digitalmodel/marine_ops/reservoir/analysis.py:L1108-L1108] |
| `ReservoirAnalysis.add_well_analysis` | 1119, 1124, 1127, 1128, 1132 | Inventoried definition; unrelated log-processing match | [src/digitalmodel/marine_ops/reservoir/analysis.py:L1119-L1119] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1124-L1124] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1127-L1127] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1128-L1128] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1132-L1132] |
| `ReservoirAnalysis.run_comprehensive_analysis` | 1161, 1163, 1166 | Inventoried definition; unrelated log-processing match | [src/digitalmodel/marine_ops/reservoir/analysis.py:L1161-L1161] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1163-L1163] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1166-L1166] |
| `ReservoirAnalysis.export_analysis_report` | 1191 | Helper/data/export/wrapper; no stress-life or damage computation | [src/digitalmodel/marine_ops/reservoir/analysis.py:L1191-L1191] |
| `migrate_legacy_stratigraphic_plot` | 81, 86, 90, 94, 98, 107, 117 | Helper/data/export/wrapper; no stress-life or damage computation | [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L81-L81] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L86-L86] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L90-L90] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L94-L94] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L98-L98] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L107-L107] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L117-L117] |

The search has 40 matching lines in 13 definitions, all from the algorithm signal; vocabulary and constants-or-keys have zero hits. Seven definitions are inventoried, and the six additional definitions initialize data, plot/delegate analysis, export data or migrate logs; further unlisted family computations: 0, within the limit of 1. [src/digitalmodel/marine_ops/reservoir/analysis.py:L410-L421] [src/digitalmodel/marine_ops/reservoir/analysis.py:L616-L655] [src/digitalmodel/marine_ops/reservoir/analysis.py:L800-L811] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1108-L1142] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1189-L1199] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L81-L117]

### Whole-repository discovery and map follow-up

Repository-wide Python discovery included `src/`, `scripts/`, tests, examples and documentation Python snippets; this additional reservoir-method search is separate from the counted fatigue-family search.

```bash
rg -n -i 'reservoir|decline|material.balance|net.pay|porosity|water.saturation' . --glob '*.py' --glob '!.venv/**' --glob '!src/digitalmodel.egg-info/**'
rg -n 'def (ooip_stb|arps|exponential|hyperbolic|harmonic|.*decline|.*net_pay|.*porosity|.*water_saturation)' src scripts --glob '*.py'
```

`properties.calculate_porosity` and `LogAnalysis.calculate_porosity_from_logs`, the two water-saturation methods, and the two net-pay methods compute separate results rather than delegating. The flat volumetrics function independently computes OOIP, and the flat stratigraphic renderer and migration fallback independently render related plots. These are map gaps outside the counted family, not additional fatigue implementations. [src/digitalmodel/marine_ops/reservoir/properties.py:L291-L405] [src/digitalmodel/marine_ops/reservoir/analysis.py:L233-L395] [src/digitalmodel/marine_ops/reservoir/modeling.py:L592-L637] [src/digitalmodel/reservoir/volumetrics.py:L40-L64] [src/digitalmodel/reservoir/stratigraphic.py:L1-L34] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L167-L381]

Map follow-up: add reservoir volumetrics, rock/fluid/PVT correlations, material balance, buildup, tank depletion, decline forecasting, petrophysical log analysis, net-pay calculations, mapping/core correlation, analogue EUR and stratigraphic plotting with in-package rows; remove or reclassify the seven unrelated fatigue rows. Those families begin to count only after the map carries in-package implementations (issue pending (9)). [src/digitalmodel/marine_ops/reservoir/properties.py:L27-L405] [src/digitalmodel/marine_ops/reservoir/modeling.py:L89-L688] [src/digitalmodel/marine_ops/reservoir/analysis.py:L125-L1092]

## 10. Coverage and open items

### Read coverage

All package Python files were read: `__init__.py` (75 lines), `properties.py` (500), `modeling.py` (814), `analysis.py` (1199), and `legacy_migration.py` (579); total 3167 lines. The local README, the two module-naming documentation files below, flat reservoir documentation, example documentation and both example scripts were also inspected. [src/digitalmodel/marine_ops/reservoir/__init__.py:L1-L75] [src/digitalmodel/marine_ops/reservoir/properties.py:L1-L500] [src/digitalmodel/marine_ops/reservoir/modeling.py:L1-L814] [src/digitalmodel/marine_ops/reservoir/analysis.py:L1-L1199] [src/digitalmodel/marine_ops/reservoir/legacy_migration.py:L1-L579]

- `docs/registry/implementation-map.md`: reservoir appears in the fatigue-family module inventory. [docs/registry/implementation-map.md:L24-L24]
- `docs/plans/2026-05-05-issue-566-marine-ops-r1-r5-r7-r8-batched.md`: records the no-production contract incident. [docs/plans/2026-05-05-issue-566-marine-ops-r1-r5-r7-r8-batched.md:L43-L52]
- `docs/domains/reservoir/reservoir.md`: discovery prose and external links, not a benchmark for this implementation. [docs/domains/reservoir/reservoir.md:L1-L17]
- `examples/domains/reservoir/README.md` and the local README: capability and example leads, not validation evidence. [examples/domains/reservoir/README.md:L1-L17] [src/digitalmodel/marine_ops/reservoir/README.md:L139-L156]
- `tests/marine_ops/reservoir/test_modeling.py`, `tests/test_reservoir_framework.py`, and all three Python test modules in `tests/reservoir/`: collected in the offline command in section 4. [tests/marine_ops/reservoir/test_modeling.py:L1-L38] [tests/test_reservoir_framework.py:L21-L57] [tests/reservoir/test_reservoir.py:L1-L17] [tests/reservoir/test_volumetrics.py:L1-L16] [tests/reservoir/test_stratigraphic.py:L1-L30]

### Open items


| Item | Remaining work / limit | Tracking |
| --- | --- | --- |
| Public API, examples and skipped framework | Repair absent facade exports, duplicate quick-setup keywords, obsolete examples and 30 skipped framework tests; preserve independent expectations | issue pending (1) |
| Property validation | Validate missing/zero/nonfinite values, phase fractions, units, empirical floors and default coefficients; expose missing outputs explicitly | issue pending (2) |
| Material balance | Reconcile Mscf/scf and barrel/cubic-foot conversion, sub-day integration, shape contract, fallback compressibility and NumPy trapz runtime dependence | issue pending (3) |
| Buildup and depletion | Replace hard-coded physical assumptions with qualified inputs; account for production cutoff and time labels in pressure balance | issue pending (4) |
| Forecast calibration and integration | Fit hyperbolic parameters, replace placeholder R², align integration with sample spacing and handle short durations | issue pending (5); #1743 |
| Log and net-pay integrity | Respect custom sentinels, finite masks and depth alignment; declare spacing and missing-column policy | issue pending (6) |
| Mapping, stratigraphy, core and analogues | Check omissions and error dictionaries; distinguish uniform mean maps from interpolation; define analogue integration and datum choice | issue pending (7) |
| Legacy migration | Expose fallback status and output disposition; cover omitted wells, default metadata and density/depth differences | issue pending (8) |
| Map completeness | Correct fatigue false positives and inventory every reservoir family named in section 3 | issue pending (9) |
| Independent evidence and conventions | UNVERIFIED items: coordinate reference/origin/depth datum; correlation editions, applicability, benchmarks and Citation sidecars; maintainer confirmation of deputy appointment and signed QA evidence | issue pending (10) |
| Integration scope | End-to-end forecast/economics interface remains outside the current container result contract | #1746 |

All open items are assigned to the maintainers for triage and the engineering calculations quality manager for technical disposition; draft numbers require issue creation before final clearance.
