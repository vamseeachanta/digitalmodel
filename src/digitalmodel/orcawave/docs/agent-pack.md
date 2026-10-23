# OrcaWave top-level module knowledge pack

| Header | Value |
| --- | --- |
| Module | orcawave, top-level |
| Repository | digitalmodel |
| Package | `src/digitalmodel/orcawave/` |
| Pack revision | 0 |
| Code revision | `7aad603be1c6136106dbb2381ce5096b050654f8` |
| Analysis family | diffraction; registry code `DF` |
| Quality manager role | diffraction quality manager |
| Status | DRAFT; independent clearance pending |
| Evidence date | 2026-10-17 |

Citations use the code revision above, except implementation-map citations, which use the separate data pin in section 9. Test results below are observations of the offline executions recorded in section 4.

## 1. Purpose and scope

The public API covers RAO conversion and interpolation, coefficient storage and conversion, GDF panels, spectra, response statistics, drift loads and preliminary hull data; the top-level initializer imports these seven calculation modules. [src/digitalmodel/orcawave/__init__.py:L14-L102]

This pack covers the eight directly contained Python files; `reporting/` keeps a separate scope, and its presence is recorded as a dependency in section 9. The initializer describes reporting separately from the calculation utilities. [src/digitalmodel/orcawave/__init__.py:L1-L11]

The routing registry records maturity as beta and routes tests to `tests/orcawave/`; this is a registry classification, not evidence of engineering qualification. [docs/registry/module-routing.yaml:L249-L254]

Representative RAOs are generated using simplified transfer functions for preliminary screening only; they are not measured or diffraction-solved data. [src/digitalmodel/orcawave/vessel_database.py:L211-L243]

No solver result or program-version qualification is established by the synthetic tests: their reference inputs are in-memory arrays and mocks. [tests/orcawave/test_drift_force_variance.py:L1-L6] [tests/orcawave/conftest.py:L1-L5]

## 2. Inputs and outputs

### Inputs

| Input | Unit and shape | Range/default and source |
| --- | --- | --- |
| `RAOEntry`, `RAOTable` | Period s; heading and phase deg; amplitude unit string, default m/m; heading-by-period matrices | Entry period > 0; table arrays have no shape validator in these definitions. [src/digitalmodel/orcawave/rao_processing.py:L20-L43] |
| `read_rao_csv(text)` | CSV: DOF, Unit, period/heading header; amplitude/phase cells | Missing metadata defaults to Unknown and m/m; missing cell phase defaults to zero; float conversions require Python numeric syntax. [src/digitalmodel/orcawave/rao_processing.py:L95-L150] |
| `HydroMatrix6x6`, `ExcitationForceVector` | Frequency rad/s; nested matrices; amplitude and phase vectors | Frequency >= 0; six-component shape is documented but not validated here. [src/digitalmodel/orcawave/hydro_coefficients.py:L19-L39] |
| `HydroCoefficients` | Mass kg, CoG m, frequency rad/s | Mass and CoG default to zero; coefficient lists default empty; restoring matrix defaults None. [src/digitalmodel/orcawave/hydro_coefficients.py:L42-L71] |
| `PanelQuad`, `GDFMesh` | Four vertices; each documented as x,y,z; characteristic length and gravity in GDF header | Exactly four outer vertices; gravity defaults 9.81; both symmetry flags default 1; coordinate lengths and units are not validated. [src/digitalmodel/orcawave/panel_mesh.py:L19-L27] [src/digitalmodel/orcawave/panel_mesh.py:L62-L70] |
| `read_gdf(text)` | Header; ULEN/GRAVITY; ISX/ISY; panel count; 12 coordinates per panel line | Raw float/int parsing; missing gravity defaults 9.81; missing second symmetry flag defaults 0; no ULEN scaling is applied to vertices. [src/digitalmodel/orcawave/panel_mesh.py:L77-L130] |
| `SpectrumParameters` / spectrum functions | Hs m; peak period s; frequency rad/s; S in m²·s/rad | Hs and Tp > 0 in model; JONSWAP default; gamma and second-mode defaults are code assumptions, not licensed standard values. [src/digitalmodel/orcawave/wave_spectrum.py:L21-L51] |
| `generate_spectrum` | Frequency interval rad/s and bin count | Defaults 0.05 to 3.0 rad/s, 200 bins; no frequency-order validator. [src/digitalmodel/orcawave/wave_spectrum.py:L335-L352] |
| Response arrays / `ScatterCell` | Aligned RAO, S and omega arrays; Hs m, Tp s, occurrence probability dimensionless | Cell Hs/Tp > 0, individual probability in [0,1]; total probability is not validated. [src/digitalmodel/orcawave/motion_statistics.py:L51-L61] [src/digitalmodel/orcawave/motion_statistics.py:L97-L116] [src/digitalmodel/orcawave/motion_statistics.py:L241-L263] |
| `motion_sickness_incidence` | Vertical acceleration m/s², exposure h, dominant frequency Hz | Defaults exposure 2 h and frequency 0.15 Hz; no argument-domain validation. [src/digitalmodel/orcawave/motion_statistics.py:L290-L331] |
| Drift coefficients / QTF | Omega rad/s; surge/sway kN/m², yaw kN·m/m²; complete Hermitian QTF | Uniform increasing grid required by algorithm, not enforced; both matrix triangles expected. [src/digitalmodel/orcawave/drift_forces.py:L17-L51] [src/digitalmodel/orcawave/drift_forces.py:L180-L228] |
| Wind/current estimation | Speeds m/s; areas m²; length m; densities kg/m³; drag coefficients dimensionless | Defaults for areas, densities and coefficients are embedded in the function signature; callers must supply an explicit analysis basis. [src/digitalmodel/orcawave/drift_forces.py:L235-L268] |
| Preliminary hull parameters | Dimensions and GM m; displacement tonnes; waterplane m² | Positive dimension/displacement fields; waterplane defaults zero; block coefficient constrained to [0.3,1.0]. [src/digitalmodel/orcawave/vessel_database.py:L19-L44] |

### Outputs and conventions

| Output | Unit / convention | Limit |
| --- | --- | --- |
| RAO CSV and interpolation | Amplitude retains table unit; phase deg; complex representation uses positive imaginary sine | Interpolation clamps period and heading rather than extrapolating or wrapping heading. [src/digitalmodel/orcawave/rao_processing.py:L62-L92] [src/digitalmodel/orcawave/rao_processing.py:L173-L211] [src/digitalmodel/orcawave/rao_processing.py:L318-L343] |
| WAMIT-labelled coefficient text | Rows contain omega and one-based i,j with unchanged values | Added-mass text claims nondimensionalisation, but the exporter emits raw values and has no characteristic-length conversion; external-format compatibility remains unqualified. [src/digitalmodel/orcawave/hydro_coefficients.py:L162-L202] |
| Hydrostatic restoring | C33 force/length; C44/C55 moment/radian under SI input | Only heave, roll and pitch diagonal terms are populated. [src/digitalmodel/orcawave/hydro_coefficients.py:L239-L265] |
| GDF text, panel metrics and summary | Raw coordinates; area described as m², volume as m³; symmetry multiplies area/volume | Summary volume takes an absolute value and normals use a near-waterplane heuristic; neither establishes signed closed-surface validity. [src/digitalmodel/orcawave/panel_mesh.py:L133-L153] [src/digitalmodel/orcawave/panel_mesh.py:L211-L217] [src/digitalmodel/orcawave/panel_mesh.py:L259-L286] [src/digitalmodel/orcawave/panel_mesh.py:L327-L350] |
| Spectral moments / response statistics | Moments integrate rad/s arrays; response units follow supplied RAO | Narrow-band Rayleigh is assumed; the significant-response field says double amplitude but implementation uses twice RMS. [src/digitalmodel/orcawave/wave_spectrum.py:L68-L102] [src/digitalmodel/orcawave/motion_statistics.py:L131-L174] |
| Drift results | Mean force kN, yaw moment kN·m; SV spectrum on difference-frequency grid | Full-QTF returns standard deviation; Newman bins require full endpoint weights, not trapezoidal integration. [src/digitalmodel/orcawave/drift_forces.py:L93-L121] [src/digitalmodel/orcawave/drift_forces.py:L145-L177] [src/digitalmodel/orcawave/drift_forces.py:L180-L228] |
| Wind/current results | kN and kN·m; heading 0=head, 90=beam | Yaw uses an assumed arm proportional to length; no reference point is carried. [src/digitalmodel/orcawave/drift_forces.py:L257-L309] |
| Generated preliminary hull/RAOs | Waterline half-breadth offsets m; heave/surge m/m; roll/pitch deg/m | Hull x spans 0 to length; no transform connects this origin to imported RAO or matrix reference points. [src/digitalmodel/orcawave/vessel_database.py:L47-L87] [src/digitalmodel/orcawave/vessel_database.py:L343-L381] |

UNVERIFIED U1: a shared origin, body/global axes, wave-direction convention and phase lead/lag convention across imported formats are not established; input qualification remains open in section 10, issue pending (1). [src/digitalmodel/orcawave/rao_processing.py:L20-L43] [src/digitalmodel/orcawave/hydro_coefficients.py:L42-L71]

## 3. Method and governing standards

The inventory labels below enumerate calculating definitions, including definitions found by the section-9 map grep; listing a method does not qualify it as a reference calculation.

| Method | Definition and implemented behaviour |
| --- | --- |
| RAO transforms | `amplitude_phase_to_complex`: amplitude times complex cosine/sine; `complex_to_amplitude_phase`: magnitude and argument. [src/digitalmodel/orcawave/rao_processing.py:L62-L92] |
| RAO interpolation / combination / comparison | `interpolate_rao`: complex bilinear interpolation; `combine_raos_multi_body`: first complex array plus weighted second; `compare_raos`: common-grid absolute and RMS amplitude errors with wrapped phase error. [src/digitalmodel/orcawave/rao_processing.py:L153-L315] |
| Coefficient interpolation | `interpolate_matrix_at_frequency`: clamped linear matrix interpolation; `interpolate_excitation_at_frequency`: heading tolerance and complex force interpolation. [src/digitalmodel/orcawave/hydro_coefficients.py:L78-L166] |
| Restoring stiffness | `create_hydrostatic_restoring`: density/gravity/waterplane heave term and displacement/GM roll and pitch terms. [src/digitalmodel/orcawave/hydro_coefficients.py:L239-L265] |
| Mesh geometry | `_triangle_area_and_normal`: cross-product area; `compute_panel_quality`: two-triangle area/normal, edge ratio, diagonal skewness; `compute_mesh_summary`: waterplane selection, volume estimate, symmetry and suggestions; `compute_waterplane_area` and `compute_displaced_volume` delegate to summary. Do not use its volume as an independent hydrostatic reference; section 6 records the defect. [src/digitalmodel/orcawave/panel_mesh.py:L160-L350] |
| Spectral moments / periods | `compute_spectral_moments`: trapezoidal moments; `spectral_periods`: derived periods and bandwidth with denominator floors. [src/digitalmodel/orcawave/wave_spectrum.py:L68-L102] |
| Single-mode spectra | `pierson_moskowitz`, `jonswap`, `issc_spectrum`: implemented spectral expressions; JONSWAP renormalises to requested variance; `bretschneider` delegates to Pierson-Moskowitz. These are code descriptions, not standards-qualified formulae. [src/digitalmodel/orcawave/wave_spectrum.py:L109-L230] |
| Two-mode spectra | `ochi_hubble` and nested `_component`: gamma-function components summed; `heuristic_two_peak`: explicitly synthetic sea/swell partition; former named-model API raises. [src/digitalmodel/orcawave/wave_spectrum.py:L233-L328] |
| Spectrum generation | `generate_spectrum`: normalised type selection, synthetic second-mode fallbacks, computed Hs and peak period. [src/digitalmodel/orcawave/wave_spectrum.py:L335-L390] |
| Response / short-term | `compute_response_spectrum`: squared RAO times wave spectrum and moments; `short_term_statistics`: RMS, significant response, zero crossing and duration-dependent maxima. [src/digitalmodel/orcawave/motion_statistics.py:L97-L174] |
| Exceedance / long-term | `rayleigh_exceedance`, `rayleigh_quantile`: exponential exceedance and inverse; `long_term_extreme`: probability-weighted mixture solved by fixed-iteration bisection. [src/digitalmodel/orcawave/motion_statistics.py:L181-L283] |
| Motion sickness | `motion_sickness_incidence`: piecewise frequency weighting and an acceleration/exposure power expression; output is capped. [src/digitalmodel/orcawave/motion_statistics.py:L290-L339] |
| Mean / slow drift | `compute_mean_drift_force`: spectral integral; `newman_approximation`: arithmetic diagonal QTF approximation and lag-bin sum; `full_qtf_slowly_varying`: upper-triangle variance excluding constant diagonal load. [src/digitalmodel/orcawave/drift_forces.py:L93-L228] |
| Wind/current | `compute_wind_current_drift`: drag pressure times area and directional coefficient, then assumed yaw arm and SI-to-kilo conversion. [src/digitalmodel/orcawave/drift_forces.py:L235-L309] |
| Preliminary database methods | `_generate_ship_rao`: capped oscillator amplitude; `get_representative_raos`: fixed grid, assumed natural periods/damping and heading scales; `generate_parametric_hull`: heuristic waterline and bilge sizing. [src/digitalmodel/orcawave/vessel_database.py:L211-L398] |

| Standard/reference lead | Located use and qualification |
| --- | --- |
| ISO 2631-1 | Source names the standard and Annex D, but supplies no edition or clause title; the docstring expression differs from the implemented power expression. Clause text not checked against the licensed copy. UNVERIFIED U2: edition, exact governing clause and compliance of the simplification; issue pending (4). [src/digitalmodel/orcawave/motion_statistics.py:L290-L339] |
| DNV-RP-C205 | No use was found in the eight direct Python files; references in discovery documentation are not evidence that these algorithms implement a clause. UNVERIFIED U3: governing edition and clause for this package; issue pending (4). [src/digitalmodel/orcawave/__init__.py:L1-L11] |
| Torsethaugen and Haver (2004) | Source names a simplified partition; no external publication was opened, so this is a code attribution rather than independent validation. [src/digitalmodel/orcawave/wave_spectrum.py:L282-L328] |
| Orcina second-order load theory | Source and synthetic tests state the complete-Hermitian convention and exclusion of mean-load diagonals; the finite-component closed forms below validate that convention, not solver agreement. [src/digitalmodel/orcawave/drift_forces.py:L185-L228] [tests/orcawave/test_drift_force_variance.py:L1-L6] |
| Calculation citation sidecars | UNVERIFIED U4: standards-derived results have no runtime `Citation` sidecar in the inspected MSI and spectral calculation return paths; traceable standards qualification remains open, issue pending (4). [src/digitalmodel/orcawave/motion_statistics.py:L324-L339] [src/digitalmodel/orcawave/wave_spectrum.py:L374-L390] |


## 4. Validated worked examples and regressions

Validated cases below use elementary closed forms evaluated independently of the production expression, not solver output or a golden snapshot. The cited test files contain their synthetic inputs. [tests/orcawave/test_panel_mesh.py:L18-L27] [tests/orcawave/test_rao_processing.py:L55-L58] [tests/orcawave/test_drift_force_variance.py:L199-L204]

| ID / class | Input and independent expected quantity | Tolerance / basis | Existing test node | Offline result |
| --- | --- | --- | --- | --- |
| V1 / validated | Unit square: area = side × side = 1 m² | 0.01 m² absolute, test tolerance | `tests/orcawave/test_panel_mesh.py::TestPanelQuality::test_unit_square_area` | PASS at BASE and AFTER. [tests/orcawave/test_panel_mesh.py:L60-L63] |
| V2 / validated | Complex unit amplitude at 90 deg: real = 0, imaginary = 1 m/m | 1e-10 m/m absolute, test tolerance | `tests/orcawave/test_rao_processing.py::TestAmplitudePhaseConversion::test_complex_at_90_degrees` | PASS at BASE and AFTER. [tests/orcawave/test_rao_processing.py:L55-L58] |
| V3 / validated | Synthetic independent wave amplitudes 1 m and 0.5 m, diagonal coefficient 10 kN/m²: mean = 10 × (1² + 0.5²) = 12.5 kN | 1e-12 relative, closed finite-component load identity | `tests/orcawave/test_drift_force_variance.py::test_mean_drift_qtf_normalization_matches_wave_amplitudes` | PASS at BASE and AFTER. [tests/orcawave/test_drift_force_variance.py:L199-L204] |
| R1 / regression | Added-mass text roundtrip, five synthetic frequencies; original diagonal recovered | 1e-4 relative; format roundtrip only | `tests/orcawave/test_hydro_coefficients.py::TestWAMITFormat::test_wamit_roundtrip` | PASS at BASE and AFTER. [tests/orcawave/test_hydro_coefficients.py:L106-L114] |
| R2 / regression | JONSWAP requested Hs = 4 m recovered by integrating its generated spectrum | 5.0 % relative; internal renormalisation means no independent spectrum validation | `tests/orcawave/test_wave_spectrum.py::TestJONSWAP::test_hs_preservation` | PASS at BASE and AFTER. [tests/orcawave/test_wave_spectrum.py:L48-L54] |
| R3 / regression | Identical complex RAOs combined at zero coupling produce twice the amplitude | 1e-10 m/m absolute; synthetic superposition identity | `tests/orcawave/test_rao_processing.py::TestCombineAndCompare::test_combine_independent` | PASS at BASE and AFTER. [tests/orcawave/test_rao_processing.py:L123-L131] |

### Execution record

The command form is `PYTHONPATH=src uv run --no-sources python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, using the available full-path uv executable, a dedicated scratch environment, one test process and no addopts override. The code pin for both runs is `7aad603be1c6136106dbb2381ce5096b050654f8`; AFTER denotes the uncommitted documentation tree, with calculation source unchanged.

| Paths | Stage | Run | Passed | Failed | Skipped | NOT_RUN |
| --- | --- | --- | --- | --- | --- | --- |
| `tests/orcawave` | BASE | 358 | 358 | 0 | 0 | 0 |
| `tests/orcawave` | AFTER | 358 | 358 | 0 | 0 | 0 |
| `tests/contracts tests/docs/test_digitalmodel_routing_contract.py` | BASE | 494 | 494 | 0 | 0 | 0 |
| `tests/contracts tests/docs/test_digitalmodel_routing_contract.py` | AFTER | 494 | 494 | 0 | 0 | 0 |

No failing or skipped package test node was reported. The 96 direct-calculation cases and 262 adjacent/schema cases are counted from individual node outcomes, not a roadmap estimate.

Native solver cases: NOT_RUN, because native loading and hydrostatics require a solver binary or licensed interface; no such import, query or run is part of this pack. Synthetic tests use mocks rather than establish native loading. [tests/orcawave/conftest.py:L1-L5]

## 5. QA gates

This section supersedes the baseline gate table. The other source inventories and
line references in this pack describe the historical audit snapshot; consult
[engineering-citations.md](engineering-citations.md) for the corrected APIs,
equations, assumptions and migration limits.

G0 uses the C0 nodes below. G1 uses explicit native NOT_RUN records. G2/G3 are
NOT_APPLICABLE to this frequency-domain pack. G4 uses the C4 nodes. G5 asserts
the `qa_gates.build_trace` output for supplied offline quantities. None of these
synthetic inputs establishes native loading, solver convergence, native CoB/GM
agreement, irregular-frequency freedom or a qualified engineering report.
Native consumers call `require_native_gate`, which raises without solver access.

Every row has one collected pytest node in
`tests/orcawave/test_pack_gates.py`, named `test_` plus the check identifier with
hyphens replaced by underscores. Native nodes assert deferred status and loud
failure; they do not claim that the native criterion passed. C4-VOLUME is a
passing numerical regression after the volume fix; no known offline defect is
hidden behind a non-strict xfail. The narrower C0-MAP scope is the direct-file
package producer map; wider historical discovery is retained as snapshot data.

| Check | Producing package function / output | Executable assertion | Qualification |
| --- | --- | --- | --- |
| C0-CSV | rao_processing.read_rao_csv | 2 periods, 2 headings; encoded amplitudes within 1e-6 | offline |
| C0-GDF | panel_mesh.read_gdf / write_gdf | 6 panels, ULEN=1, signed volume=8 m3 | offline |
| C0-COEFF | hydro_coefficients.from_native_added_mass | one SI record, omega=2pi/10, A33=8200000 kg; no WAMIT claim | offline |
| C0-MAP | qa_gates.implementation_map | direct-file AST function map contains all ten named core producers | offline |
| C1-OW | qa_gates.native_gate_record / require_native_gate | status NOT_RUN; consumer request raises | native NOT_RUN |
| C1-AQWA | qa_gates.native_gate_record / require_native_gate | status NOT_RUN; consumer request raises | native NOT_RUN |
| C4-AREA | panel_mesh.compute_panel_quality | box bottom area=4 m2 within .01 | offline |
| C4-ASPECT | panel_mesh.compute_panel_quality | square aspect=1 within .01 | offline |
| C4-SKEW | panel_mesh.compute_panel_quality | square skew=0 within .01 | offline |
| C4-VOLUME | panel_mesh.compute_displaced_volume | outward box=8 m3 within .5%; regression now passes | offline |
| C4-BALANCE | qa_gates.check_balance | rho*V=8200 kg; reject >1% mass error | offline |
| C4-FRAME | qa_gates.compute_buoyancy_centroid / hydro_coefficients.create_hydrostatic_restoring | declared body frame, box CoB=[0,1,-1] m, translated CoB=[3,-1,-1] m; GM=2,100 m recovered from restoring | offline; native CoB/GM comparison NOT_RUN |
| C4-MESH | qa_gates.check_mesh_resolution | max edge=2 m, aspect<=3, edge<=lambda/6, 6 panels<=declared limit; reject excess | offline |
| C4-WATERLINE | qa_gates.check_waterline | closed lidless boundary and 4 m2 area, draft=2 m; reject missing edge or >.01 m draft error | offline |
| C4-NORMALS | qa_gates.check_normals | positive 8 m3 volume, nonzero area, opposite shared edges; reject reversed or inconsistent orientation | offline; full closed mesh only |
| C4-IRREGULAR | qa_gates.native_gate_record / require_native_gate | status NOT_RUN; require_native_gate raises for missing native lid/pressure/spike evidence | native NOT_RUN |
| C4-HYDRO | qa_gates.check_hydrodynamics | added-mass and radiation-damping reciprocity<=2% using diagonal/coupling normalization, nonnegative damping, longest supplied-period heave=1±2% across all headings; reject excessive asymmetry/negative damping/bad heave | offline |
| C4-WIND | drift_forces.compute_wind_current_drift | beam wind588 kN, current922.5 kN, yaw37762.5 kN m within rel1e-9 | offline |
| C4-HULL | vessel_database.generate_parametric_hull | rectangular midsection half breadth=15 m within1% | offline |
| C5-TRACE | qa_gates.build_trace | SHA256, quantity producer/value/unit, gate/disposition/maintainer signature present; required-quantity coverage=100%; incomplete coverage or missing gate rejected | offline; native/report qualification NOT_RUN |

The canonical offline pack-report required set is `offline_pack_report_v1`:
`displaced_volume_m3`, `waterplane_area_m2`, `centre_of_buoyancy_m`, `gm_t_m`,
`gm_l_m`, `longest_period_heave_m_per_m`, `wind_force_sway_kn`,
`current_force_sway_kn`, and `total_yaw_knm`. Pass this entire set as
`required_quantities` to `build_trace` for a pack report; extra traced quantities
cannot replace any required quantity. The smaller C5-TRACE volume fixture tests
one producer and is not a complete pack report. Native/report qualification
remains NOT_RUN.

C4-HYDRO uses `abs(Mij-Mji) / max(sqrt(abs(Mii*Mjj)), abs(Mij), abs(Mji))`
for both matrix families, with a 2% limit. A zero denominator denotes a zero
pair with zero error. This scale admits small opposite-sign numerical noise
beside large diagonals while retaining the 1% pass and 3% rejection cases.

## 6. Known failure modes and pitfalls

sweep: 92 found

This count denotes reviewed fallback, conversion, unit and heuristic sites, not 92 independently confirmed defects.

Scope is the direct Python files only. The exact discovery commands were:

```bash
rg -n 'except|continue|return None|return \[\]|return \{\}|break|default|= 0(\.0)?\b|1970|0\.05|float\(|int\(|1e-|maximum|minimum|clip|\.get\(|kg|rad|deg|metre|tonne|seawater|normal|assum|cache' src/digitalmodel/orcawave/*.py
rg -n 'except|continue|return None|return \[\]|return \{\}|1970|cache|write|save' src/digitalmodel/orcawave/*.py
```

The table distinguishes risky fallbacks from arithmetic accumulators and unit assumptions. Repeated expressions at one site are individually named in the row; a shared draft groups their cause. No exception handler, `return None`, error-path empty-literal return, epoch default or cache-write path was found in this scope; list/dict model defaults and query emptiness are covered below. [src/digitalmodel/orcawave/__init__.py:L14-L102]

| Sweep ID | Site and symptom / classification | Existing detection / disposition |
| --- | --- | --- |
| S01 | Entry positivity differs from unconstrained table periods; m/m, degree and matrix-shape conventions assumed. [src/digitalmodel/orcawave/rao_processing.py:L23-L43] | no detection check exists; issue pending (1). |
| S02 | Missing DOF label becomes Unknown. [src/digitalmodel/orcawave/rao_processing.py:L117-L117] | no detection check exists; issue pending (1). |
| S03 | Missing CSV unit becomes m/m. [src/digitalmodel/orcawave/rao_processing.py:L118-L118] | no detection check exists; issue pending (1). |
| S04 | External heading float conversion has no thousands/decimal-separator policy. [src/digitalmodel/orcawave/rao_processing.py:L121-L121] | no detection check exists; issue pending (1). |
| S05 | Blank or empty-period CSV records are skipped by continue; period float conversion is unguarded. [src/digitalmodel/orcawave/rao_processing.py:L131-L134] | no detection check exists; issue pending (1). |
| S06 | External amplitude float conversion; missing phase becomes zero, otherwise external phase float conversion; short rows can create ragged matrices. [src/digitalmodel/orcawave/rao_processing.py:L135-L138] | no detection check exists; issue pending (1). |
| S07 | Period and heading clamp silently; duplicate-axis denominators floored, circular heading not handled. [src/digitalmodel/orcawave/rao_processing.py:L176-L190] | no detection check exists; issue pending (1). |
| S08 | Coupling defaults zero; grid and coupling-range assumptions not validated. [src/digitalmodel/orcawave/rao_processing.py:L214-L252] | no detection check exists; issue pending (6). |
| S09 | Disjoint tables return NaN metrics rather than refusal. [src/digitalmodel/orcawave/rao_processing.py:L268-L280] | no detection check exists; issue pending (1). |
| S10 | Dictionary index construction overwrites duplicate period/heading keys. [src/digitalmodel/orcawave/rao_processing.py:L287-L291] | no detection check exists; issue pending (1). |
| S11 | CSV amplitude and phase precision truncates values on export. [src/digitalmodel/orcawave/rao_processing.py:L337-L343] | C0-CSV covers nominal values, not precision loss; issue pending (1). |
| S12 | Frequency rad/s and six-by-six/six-vector conventions; actual nested shapes unchecked. [src/digitalmodel/orcawave/hydro_coefficients.py:L19-L39] | no detection check exists; issue pending (3). |
| S13 | Mass defaults zero kg; CoG defaults three zero coordinates. [src/digitalmodel/orcawave/hydro_coefficients.py:L45-L50] | no detection check exists; issue pending (3). |
| S14 | Frequency, added-mass, radiation-damping and excitation lists each default empty; restoring defaults None. [src/digitalmodel/orcawave/hydro_coefficients.py:L51-L71] | no detection check exists; issue pending (3). |
| S15 | Boundary targets return endpoint matrix; duplicate-frequency interpolation fraction defaults zero; order assumed. [src/digitalmodel/orcawave/hydro_coefficients.py:L95-L106] | no detection check exists; issue pending (3). |
| S16 | Excitation heading tolerance is fixed; out-of-band frequencies clamp to endpoint vectors. [src/digitalmodel/orcawave/hydro_coefficients.py:L125-L143] | no detection check exists; issue pending (3). |
| S17 | Density is assigned but unused; raw added-mass rows omit elements below threshold; nondimensionalisation claim is not implemented. [src/digitalmodel/orcawave/hydro_coefficients.py:L162-L182] | C0-COEFF nominal roundtrip only; issue pending (3). |
| S18 | Damping export omits below-threshold values; raw values have no dimensional conversion. [src/digitalmodel/orcawave/hydro_coefficients.py:L185-L202] | no detection check exists; issue pending (3). |
| S19 | Short WAMIT rows are skipped by continue. [src/digitalmodel/orcawave/hydro_coefficients.py:L215-L218] | no detection check exists; issue pending (3). |
| S20 | External omega float conversion assumes Python numeric syntax. [src/digitalmodel/orcawave/hydro_coefficients.py:L219-L219] | no detection check exists; issue pending (3). |
| S21 | External row and column int conversions assume integer text and subtract one. [src/digitalmodel/orcawave/hydro_coefficients.py:L220-L221] | no detection check exists; issue pending (3). |
| S22 | External coefficient float conversion assumes Python numeric syntax. [src/digitalmodel/orcawave/hydro_coefficients.py:L222-L222] | no detection check exists; issue pending (3). |
| S23 | Unspecified matrix entries are filled with zero. [src/digitalmodel/orcawave/hydro_coefficients.py:L223-L226] | no detection check exists; issue pending (3). |
| S24 | Out-of-range matrix indices silently discard their value. [src/digitalmodel/orcawave/hydro_coefficients.py:L225-L226] | no detection check exists; issue pending (3). |
| S25 | Empty or entirely skipped input produces empty matrix list. [src/digitalmodel/orcawave/hydro_coefficients.py:L214-L236] | no detection check exists; issue pending (3). |
| S26 | Restoring density/gravity are assumed defaults; other matrix entries remain zero by design. [src/digitalmodel/orcawave/hydro_coefficients.py:L239-L265] | C0-COEFF does not qualify design-basis inputs; issue pending (3). |
| S27 | Only outer vertex count is constrained; coordinate dimensions and units are assumed. [src/digitalmodel/orcawave/panel_mesh.py:L19-L27] | no detection check exists; issue pending (1). |
| S28 | normal_consistent defaults True without a geometric test. [src/digitalmodel/orcawave/panel_mesh.py:L40-L44] | no detection check exists; issue pending (2). |
| S29 | ULEN and gravity default; ISX and ISY each default active symmetry; panels default empty. [src/digitalmodel/orcawave/panel_mesh.py:L65-L70] | no detection check exists; issue pending (1). |
| S30 | External ULEN/gravity float conversions; absent gravity silently supplied. [src/digitalmodel/orcawave/panel_mesh.py:L99-L100] | no detection check exists; issue pending (1). |
| S31 | External ISX/ISY int conversions; absent ISY silently supplied. [src/digitalmodel/orcawave/panel_mesh.py:L103-L104] | no detection check exists; issue pending (1). |
| S32 | External panel-count int conversion; negative count is not refused. [src/digitalmodel/orcawave/panel_mesh.py:L106-L106] | no detection check exists; issue pending (1). |
| S33 | Truncated GDF exits via break without comparing parsed count to declared count. [src/digitalmodel/orcawave/panel_mesh.py:L109-L112] | no detection check exists; issue pending (1). |
| S34 | External coordinate float conversion; records with fewer than 12 numbers ignored; extra numbers truncated. [src/digitalmodel/orcawave/panel_mesh.py:L112-L121] | no detection check exists; issue pending (1). |
| S35 | GDF serialisation rounds header and vertex precision; ULEN is not applied to geometry. [src/digitalmodel/orcawave/panel_mesh.py:L142-L153] | no detection check exists; issue pending (1). |
| S36 | Zero-area triangle normal denominator is floored rather than rejected. [src/digitalmodel/orcawave/panel_mesh.py:L166-L171] | no detection check exists; issue pending (2). |
| S37 | Area-weighted panel-normal denominator is floored for degeneracy. [src/digitalmodel/orcawave/panel_mesh.py:L186-L192] | no detection check exists; issue pending (2). |
| S38 | Zero minimum edge replaced by tiny number; a fully collapsed panel can yield zero aspect ratio. [src/digitalmodel/orcawave/panel_mesh.py:L194-L203] | no detection check exists; issue pending (2). |
| S39 | Diagonal skewness denominator is floored; degenerate or nonplanar shapes not rejected. [src/digitalmodel/orcawave/panel_mesh.py:L205-L209] | no detection check exists; issue pending (2). |
| S40 | Normals default consistent except near-waterplane vertical sign heuristic. [src/digitalmodel/orcawave/panel_mesh.py:L211-L217] | no detection check exists; issue pending (2). |
| S41 | Empty mesh yields zero areas/volume/ratios and no bad-normal count. [src/digitalmodel/orcawave/panel_mesh.py:L242-L253] | C4-AREA does not test emptiness; no detection check exists; issue pending (2). |
| S42 | Waterplane area accumulator starts zero; only near-horizontal panels selected; open-hull waterline absent. [src/digitalmodel/orcawave/panel_mesh.py:L259-L264] | no detection check exists; issue pending (2). |
| S43 | Volume accumulator starts zero; vertical-only integral divided by three, then absolute value hides orientation. [src/digitalmodel/orcawave/panel_mesh.py:L266-L275] | C4-VOLUME (known failure); issue pending (2). |
| S44 | Any truthy symmetry flag doubles quantities; no flag-domain or duplicate-geometry check. [src/digitalmodel/orcawave/panel_mesh.py:L277-L284] | no detection check exists; issue pending (2). |
| S45 | Aspect/skewness/panel-count advisory thresholds are fixed; they do not enforce AQG mesh limits. [src/digitalmodel/orcawave/panel_mesh.py:L288-L311] | C4-MESH (NOT_RUN); issue pending (2). |
| S46 | Spectrum type, enhancement and optional swell defaults are assumptions rather than qualified input provenance. [src/digitalmodel/orcawave/wave_spectrum.py:L21-L37] | no detection check exists; issue pending (4). |
| S47 | Tz, T01 and bandwidth denominators floored separately; invalid moments can still give plausible periods. [src/digitalmodel/orcawave/wave_spectrum.py:L98-L102] | no detection check exists; issue pending (4). |
| S48 | Pierson-Moskowitz initialises zero spectrum; nonpositive-frequency bins remain zero. [src/digitalmodel/orcawave/wave_spectrum.py:L127-L137] | no detection check exists; issue pending (4). |
| S49 | JONSWAP has fixed width/normalisation constants; nonpositive exponent branch supplies zero; small/nonpositive integral bypasses renormalisation. [src/digitalmodel/orcawave/wave_spectrum.py:L161-L182] | no detection check exists; issue pending (4). |
| S50 | ISSC initialises zero spectrum and masks nonpositive frequencies; units and grid monotonicity assumed. [src/digitalmodel/orcawave/wave_spectrum.py:L225-L230] | no detection check exists; issue pending (4). |
| S51 | Each Ochi-Hubble component initialises zero spectrum and masks nonpositive frequencies. [src/digitalmodel/orcawave/wave_spectrum.py:L266-L279] | no detection check exists; issue pending (4). |
| S52 | Torsethaugen assigns a fixed small secondary component in either partition, with fixed enhancement values. [src/digitalmodel/orcawave/wave_spectrum.py:L301-L318] | no detection check exists; issue pending (4). |
| S53 | Torsethaugen skips normalisation if computed variance is too small or nonpositive. [src/digitalmodel/orcawave/wave_spectrum.py:L322-L328] | no detection check exists; issue pending (4). |
| S54 | Fixed frequency interval/bin defaults; empty/descending/zero-frequency grids not validated. [src/digitalmodel/orcawave/wave_spectrum.py:L335-L352] | no detection check exists; issue pending (4). |
| S55 | ISSC route estimates zero-crossing period using a fixed peak-period ratio. [src/digitalmodel/orcawave/wave_spectrum.py:L361-L363] | no detection check exists; issue pending (4). |
| S56 | Missing second-mode height, period and enhancement each receive synthetic fallback values. [src/digitalmodel/orcawave/wave_spectrum.py:L364-L368] | no detection check exists; issue pending (4). |
| S57 | Computed variance floored at zero; nonpositive peak frequency substitutes input Tp. [src/digitalmodel/orcawave/wave_spectrum.py:L374-L379] | no detection check exists; issue pending (4). |
| S58 | Response frequency units and narrow-band/double-amplitude labels are assumptions; significant output is twice RMS. [src/digitalmodel/orcawave/motion_statistics.py:L18-L47] | no detection check exists; issue pending (4). |
| S59 | Individual scatter probability constrained, but sum not checked; contribution list defaults empty. [src/digitalmodel/orcawave/motion_statistics.py:L51-L75] | no detection check exists; issue pending (4). |
| S60 | MSI frequency_weighted defaults True without weighting evidence. [src/digitalmodel/orcawave/motion_statistics.py:L78-L90] | no detection check exists; issue pending (4). |
| S61 | m0, m2 and m4 are each floored, concealing nonpositive moment input. [src/digitalmodel/orcawave/motion_statistics.py:L146-L148] | no detection check exists; issue pending (4). |
| S62 | Cycle count and logarithm floored; no maximum returns sigma fallback; expected-maximum constant is embedded. [src/digitalmodel/orcawave/motion_statistics.py:L150-L165] | no detection check exists; issue pending (4). |
| S63 | Nonpositive RMS returns zero exceedance, including zero threshold. [src/digitalmodel/orcawave/motion_statistics.py:L193-L195] | no detection check exists; issue pending (4). |
| S64 | Invalid probability or RMS returns zero quantile instead of rejecting. [src/digitalmodel/orcawave/motion_statistics.py:L208-L210] | no detection check exists; issue pending (4). |
| S65 | Return period and sea-state duration defaults; time conversion and target-probability assumptions unvalidated. [src/digitalmodel/orcawave/motion_statistics.py:L222-L241] | no detection check exists; issue pending (4). |
| S66 | Long-term variance is clipped at zero before square root. [src/digitalmodel/orcawave/motion_statistics.py:L247-L250] | no detection check exists; issue pending (4). |
| S67 | Bisection bounds and 100 iterations are fixed; zero sigmas use fallback upper bound and denominator floor; no convergence residual checked. [src/digitalmodel/orcawave/motion_statistics.py:L265-L277] | no detection check exists; issue pending (4). |
| S68 | MSI exposure and frequency defaults; piecewise weighting assumed; invalid domains not rejected. [src/digitalmodel/orcawave/motion_statistics.py:L290-L320] | no detection check exists; issue pending (4). |
| S69 | MSI high-frequency branch, power expression and upper cap can hide qualification limits; weighted acceleration returned under RMS label. [src/digitalmodel/orcawave/motion_statistics.py:L321-L338] | no detection check exists; issue pending (4). |
| S70 | Mean/QTF arrays assume aligned rad/s grids and force/moment coefficient units. [src/digitalmodel/orcawave/drift_forces.py:L17-L51] | no detection check exists; #2624. |
| S71 | Uncomputed surge and sway slowly-varying standard deviations each default zero. [src/digitalmodel/orcawave/drift_forces.py:L62-L69] | no detection check exists; issue pending (6). |
| S72 | Mean-drift interpolation silently holds coefficient endpoints outside their grid. [src/digitalmodel/orcawave/drift_forces.py:L112-L119] | no detection check exists; issue pending (6). |
| S73 | Unknown DOF falls through to yaw rather than refusal. [src/digitalmodel/orcawave/drift_forces.py:L153-L160] | no detection check exists; issue pending (6). |
| S74 | Newman single-point spacing defaults to one; total initialised zero; zero-lag bin deliberately zero. [src/digitalmodel/orcawave/drift_forces.py:L164-L177] | no invalid-grid detection check exists; #2624. |
| S75 | Spectrum interpolated to QTF grid with endpoint holding; single-point spacing defaults one. [src/digitalmodel/orcawave/drift_forces.py:L209-L214] | no detection check exists; #2624. |
| S76 | Variance starts zero; only upper triangle used; negative variance floored before square root. [src/digitalmodel/orcawave/drift_forces.py:L221-L228] | no invalid-QTF detection check exists; #2624. |
| S77 | Wind/current areas, length, drag coefficients and both densities have embedded defaults. [src/digitalmodel/orcawave/drift_forces.py:L239-L247] | no detection check exists; issue pending (6). |
| S78 | Environment heading convention, density and projected-area units assumed. [src/digitalmodel/orcawave/drift_forces.py:L257-L268] | no detection check exists; issue pending (6). |
| S79 | Wind and current yaw arms each assume a fraction of length without moment reference metadata. [src/digitalmodel/orcawave/drift_forces.py:L276-L286] | no detection check exists; issue pending (6). |
| S80 | All force/moment outputs divided by SI-to-kilo factor; input unit contract not checked. [src/digitalmodel/orcawave/drift_forces.py:L288-L294] | C4-WIND covers zero only; issue pending (6). |
| S81 | Database units and block coefficient assumed; waterplane defaults zero; natural-period dictionary defaults empty. [src/digitalmodel/orcawave/vessel_database.py:L27-L44] | no detection check exists; issue pending (5). |
| S82 | Representative headings fixed; translation/rotation unit assumptions differ; optional surge array empty. [src/digitalmodel/orcawave/vessel_database.py:L47-L70] | no detection check exists; issue pending (5). |
| S83 | Bilge radius defaults zero; bow type, section count and empty offset list supplied. [src/digitalmodel/orcawave/vessel_database.py:L74-L87] | no detection check exists; issue pending (5). |
| S84 | Embedded preliminary parameters have no external data provenance in the definitions. [src/digitalmodel/orcawave/vessel_database.py:L94-L165] | no detection check exists; issue pending (5). |
| S85 | Returns shared database objects; unknown type query returns empty list. [src/digitalmodel/orcawave/vessel_database.py:L189-L204] | Existing empty-query regression, not provenance/mutation check; issue pending (5). |
| S86 | Oscillator damping defaults to five percent; period floor and amplitude cap applied. [src/digitalmodel/orcawave/vessel_database.py:L211-L235] | no detection check exists; issue pending (5). |
| S87 | First matching database item selected; heave, roll and pitch natural periods each have fallback. [src/digitalmodel/orcawave/vessel_database.py:L251-L263] | no detection check exists; issue pending (5). |
| S88 | Fixed period/heading arrays, four heading scale arrays, type damping and fallback damping are heuristic. [src/digitalmodel/orcawave/vessel_database.py:L258-L281] | no detection check exists; issue pending (5). |
| S89 | Four motion-response calls use fixed amplitudes and period multipliers. [src/digitalmodel/orcawave/vessel_database.py:L288-L303] | no detection check exists; issue pending (5). |
| S90 | Ship/barge waterline taper fractions and powers are fixed assumptions. [src/digitalmodel/orcawave/vessel_database.py:L343-L369] | C4-HULL covers midsection only; issue pending (5). |
| S91 | Pontoon breadth is heuristic; unsupported types silently use generic shape. [src/digitalmodel/orcawave/vessel_database.py:L371-L379] | no detection check exists; issue pending (5). |
| S92 | Geometry floats convert computed arrays, not external text; bilge starts zero with fixed type-dependent caps. [src/digitalmodel/orcawave/vessel_database.py:L381-L387] | no detection check exists; issue pending (5). |


### Incident and issue leads

| Issue | Confirmed scope at this pin | Detection / disposition |
| --- | --- | --- |
| #2617 | Historic slow-drift double-counting concern: current code excludes diagonal loads and counts unordered off-diagonal pairs once. [src/digitalmodel/orcawave/drift_forces.py:L185-L228] | Existing independent finite-component, phase-ensemble and oscillator tests address the concern; PASS status comes from the section-4 execution record. [tests/orcawave/test_drift_force_variance.py:L168-L237] |
| #2624 | Uniform grid and complete-Hermitian input assumptions are documented but not validated. [src/digitalmodel/orcawave/drift_forces.py:L180-L228] | No invalid-grid or triangular-input detection check exists in the inspected drift test files; ordinary complete-input tests do not close the issue. [tests/orcawave/test_drift_forces.py:L82-L103] [tests/orcawave/test_drift_force_variance.py:L168-L237] |
| #2543 | Outside-package JONSWAP normalisation defect lead; local JONSWAP multiplies the normalisation term and then renormalises variance. [src/digitalmodel/orcawave/wave_spectrum.py:L172-L182] | R2 checks local integrated height, not the affected outside-package paths; map annotations marked not checked remain unqualified. |
| #1709 | Coverage planning lead, not an incident; actual top-level calculation and registry checks are recorded here without adopting roadmap counts. [docs/registry/module-routing.yaml:L249-L254] | Package test execution is recorded in section 4; percent line coverage was not measured. |
| #1797 / #487 | Domain/research and towing requirements are leads, not evidence of a defect in this top-level package. [src/digitalmodel/orcawave/__init__.py:L1-L11] | No incident-specific detection check exists for those broader requirements in this scope; they are not counted as confirmed incidents. |

## 7. Lessons log

| Date | Record | Lesson | Evidence | Pack change / shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-10-17 | Initial pack | Positive box volume is not an independent volume calculation. | [tests/orcawave/test_panel_mesh.py:L131-L135] [src/digitalmodel/orcawave/panel_mesh.py:L266-L275] | C4-VOLUME and draft 2; no shared-log entry established. |
| 2026-10-17 | Initial pack | A raw-value roundtrip does not establish external coefficient scaling. | [tests/orcawave/test_hydro_coefficients.py:L104-L112] [src/digitalmodel/orcawave/hydro_coefficients.py:L162-L182] | WAMIT qualification limit and draft 3; no shared-log entry established. |

## 8. Ownership

The designated review role for this pack is the diffraction quality manager. Independent second-provider acceptance is pending; this authored pack assigns no clearance.

UNVERIFIED U5: the named deputy assignment and production support roster are not established by the inspected registry; maintainer confirmation is required, issue pending (7). The proposed deputy role is an independent diffraction reviewer; the producing responsibility is engineering documentation, and criterion or input ambiguity goes to the diffraction quality manager. [docs/registry/module-routing.yaml:L249-L254]

## 9. Dependencies and implementation inventory

### Direct calculation imports and adjacent packages

NumPy and Pydantic are used by the directly contained calculation modules; the Ochi-Hubble component imports SciPy's gamma function locally. [src/digitalmodel/orcawave/wave_spectrum.py:L11-L14] [src/digitalmodel/orcawave/wave_spectrum.py:L264-L279]

`reporting/` is excluded from the top-level scope; it builds reports from result files and has separate configuration and section builders. The solver and conversion packages below remain separate dependencies, not solver qualification by this pack. [src/digitalmodel/orcawave/reporting/builder.py:L53-L76]

| Adjacent package / interface | Scope relationship |
| --- | --- |
| `src/digitalmodel/orcawave/reporting/` | Report builder, configuration and section builders; not part of the direct-file silent-failure sweep. [src/digitalmodel/orcawave/reporting/builder.py:L1-L40] |
| `src/digitalmodel/hydrodynamics/diffraction/` | Distinct solver-neutral input, backend and postprocessing route described by the canonical CLI documentation. [docs/domains/orcawave/README.md:L29-L33] |
| `src/digitalmodel/solvers/orcawave/` | Separate solver setup area; its import is not needed to use the direct calculation utilities. [src/digitalmodel/orcawave/__init__.py:L14-L102] |
| `tests/workflows/orcawave/`, `tests/solvers/orcawave/` | Distinct workflow/setup tests; they are not substituted for the package's named registry test directory. [docs/registry/module-routing.yaml:L249-L254] |

### Every direct-source named-program occurrence

These occurrences are documentation or format labels, read as text; none establishes an installed program/version or a solver run.

| Program | Direct-source occurrences and dependency meaning |
| --- | --- |
| OrcaWave | Package and reporting description. [src/digitalmodel/orcawave/__init__.py:L1-L4] |
| OrcaWave | Drift workflow description. [src/digitalmodel/orcawave/drift_forces.py:L1-L4] |
| OrcaWave | Coefficient results description. [src/digitalmodel/orcawave/hydro_coefficients.py:L1-L4] |
| OrcaWave | Response-statistics workflow description. [src/digitalmodel/orcawave/motion_statistics.py:L1-L5] |
| OrcaWave | Mesh description and default GDF header. [src/digitalmodel/orcawave/panel_mesh.py:L1-L5] [src/digitalmodel/orcawave/panel_mesh.py:L62-L70] |
| OrcaWave | RAO description, CSV read label and CSV write label. [src/digitalmodel/orcawave/rao_processing.py:L1-L5] [src/digitalmodel/orcawave/rao_processing.py:L95-L106] [src/digitalmodel/orcawave/rao_processing.py:L318-L325] |
| OrcaWave | Preliminary database description. [src/digitalmodel/orcawave/vessel_database.py:L1-L6] |
| OrcaWave | Spectrum library and workflow descriptions. [src/digitalmodel/orcawave/wave_spectrum.py:L1-L8] |
| AQWA | Coefficient module claims format-conversion helpers; implemented exports in the direct file are WAMIT-labelled text. No native AQWA deck-generation capability is established here. [src/digitalmodel/orcawave/hydro_coefficients.py:L1-L4] [src/digitalmodel/orcawave/hydro_coefficients.py:L162-L236] |

### Map pin, counted families and definition search

The implementation map is read at containing commit `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, with data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; it is not part of this branch's code pin. All citations to `docs/registry/implementation-map.yaml` in this pack resolve at that containing commit. Inventory source-line numbers describe the map data revision, not relocated code at the code pin.

| Rank | Family | In-scope rows | All mapped rows | Counted |
| --- | --- | --- | --- | --- |
| 1 | wave-spectra | 8 | 141 | Yes. [docs/registry/implementation-map.yaml:L13593-L13599] |
| 2 | wind-and-current-force-routes | 1 | 135 | Yes. [docs/registry/implementation-map.yaml:L15393-L15399] |

No other map family has a direct-package implementation row. Counts exclude `reporting/`, which has its own scope. The exact Python string literals for the `re` patterns below are used case-sensitively; no signal matches the path `orcawave` under the case-insensitive path test, so no signal is excluded.

**wave-spectra**

```python
'jonswap|pierson.moskowitz|bretschneider|ochi.hubble|torsethaugen|gaussian.swell|\\bISSC\\b|spectral_moment|normalize_to_significant|spectral_density|gamma\\s*\\*\\*|S_PM|S_pm|np\\.exp|math\\.exp|spectral_density|spectrum\\s*\\*|sigma\\s*='
'spectrum|spectral.density|wave.spectra|JONSWAP'
'0\\.07|0\\.09|0\\.287|peak_enhancement'
```

**wind-and-current-force-routes**

```python
'(?:0\\.5|1\\s*/\\s*2).*(?:rho|density).*(?:cd|drag|area)|(?:wind|current|drag|lift)_forces?|calculate_(?:wind|current)|OCIMF|current_profile|wind_height_coefficient'
'OCIMF|wind.load|current.load|wind.force|current.force|drag.coefficient|current.profile'
'Cxw|Cyw|CXw|CYw|CXc|CYc|drag_coefficient|lift_coefficient|wind_speed|current_velocity'
```


| Counted-family hit classification | Result |
| --- | --- |
| wave-spectra | 23 enclosing definitions. Eight map rows are listed in the inventory; `compute_spectral_moments` is additionally listed in section 3. The outer Ochi-Hubble function composes its listed nested component. Response/drift statistics calculate distinct results and are explicitly listed in section 3. Model classes, exports and module docstrings are helpers. Further unlisted own implementations: 0. [src/digitalmodel/orcawave/wave_spectrum.py:L68-L390] [src/digitalmodel/orcawave/motion_statistics.py:L18-L283] [src/digitalmodel/orcawave/drift_forces.py:L93-L228] |
| wind-and-current-force-routes | Two enclosing definitions: `WindCurrentDrift` holds data, and `compute_wind_current_drift` computes the listed result. Further unlisted own implementations: 0. [src/digitalmodel/orcawave/drift_forces.py:L72-L87] [src/digitalmodel/orcawave/drift_forces.py:L235-L309] |

A8 self-check: inventory covers all mapped rows of the two module families; counted-family further misses total 0, meeting the limit of at most 1. This classification is subject to independent review, not a clearance claim.

The whole-repository search included `scripts/`: 5,650 tracked Python files were searched with the same patterns; wave signals matched 494 files, including 60 script files, and wind/current signals matched 295 files, including 23 script files. These are text-hit counts, not implementation counts.

Map follow-up: further outside-package own algorithms are `OCIMFLoading.wind_load` and `current_load`, depth-integrated `vessel_forces.current_load`, and stress/Hz spectral moments; map-family assignment remains a follow-up rather than an in-package miss, issue pending (8). [src/digitalmodel/hydrodynamics/ocimf_loading.py:L28-L182] [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L47-L82] [src/digitalmodel/fatigue/spectral_fatigue.py:L116-L143]

Uncounted result families in section 3 include RAO interpolation/comparison, coefficient/restoring conversion, panel geometry, short/long-term statistics, MSI, mean/slow drift, and preliminary hull/representative-RAO generation. They require map rows before entering counted-family review; issue pending (8). [src/digitalmodel/orcawave/__init__.py:L14-L102]

### Mapped inventory: wave-spectra

All 141 map implementation rows are inventoried below; the map data revision is `270c8759c374b67a7f032953e7fcdb14e0224461` and its containing commit is `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`. [docs/registry/implementation-map.yaml:L13593-L13595]

| Path | Mapped symbols and source line at map data revision | Map inventory evidence |
| --- | --- | --- |
| `docs/domains/orcaflex/examples/raw/K01/PythonController.py` | `Actuator.output` (L56) | [docs/registry/implementation-map.yaml:L13600-L13611] |
| `docs/domains/orcaflex/examples/raw/K02/BladedControllerWrapper.py` | `Actuator.output` (L61) | [docs/registry/implementation-map.yaml:L13612-L13623] |
| `examples/domains/fpso/fpso_mooring_analysis.py` | `define_environmental_conditions` (L137); `generate_visualizations` (L347); `main` (L497) | [docs/registry/implementation-map.yaml:L13624-L13662] |
| `examples/prototypes/orcaflex-agent/generate_test.py` | `main` (L35) | [docs/registry/implementation-map.yaml:L13663-L13674] |
| `scripts/_seed_eq_mock.py` | `mock_time_series` (L33) | [docs/registry/implementation-map.yaml:L13675-L13686] |
| `scripts/benchmark/extract_pdf_plots.py` | `<module>` (L38) | [docs/registry/implementation-map.yaml:L13687-L13697] |
| `scripts/benchmark/validate_owd_vs_spec.py` | `<module>` (L61) | [docs/registry/implementation-map.yaml:L13698-L13708] |
| `scripts/extract_property_inventory.py` | `build_dormancy_rules` (L179) | [docs/registry/implementation-map.yaml:L13709-L13720] |
| `scripts/generate_env_files.py` | `create_wave_file` (L38); `main` (L142) | [docs/registry/implementation-map.yaml:L13721-L13746] |
| `scripts/ingest_orcina_help.py` | `<module>` (L21) | [docs/registry/implementation-map.yaml:L13747-L13757] |
| `scripts/python/digitalmodel/legacy/19_-_DistanceBetweenTwoVessels.py` | `<module>` (L4) | [docs/registry/implementation-map.yaml:L13758-L13768] |
| `scripts/python/digitalmodel/legacy/26_-_LinkedStatistics.py` | `<module>` (L3) | [docs/registry/implementation-map.yaml:L13769-L13779] |
| `scripts/python/digitalmodel/legacy/27_-_RainflowHalfCycles.py` | `<module>` (L3) | [docs/registry/implementation-map.yaml:L13780-L13790] |
| `scripts/python/digitalmodel/legacy/29_-_ExtremeStatisticsRayleigh.py` | `<module>` (L3) | [docs/registry/implementation-map.yaml:L13791-L13801] |
| `scripts/python/digitalmodel/legacy/30_-_ExtremeStatisticsLikelihood.py` | `<module>` (L3) | [docs/registry/implementation-map.yaml:L13802-L13812] |
| `scripts/python/digitalmodel/legacy/31_-_ExtremeStatisticsAggregation.py` | `<module>` (L5) | [docs/registry/implementation-map.yaml:L13813-L13823] |
| `scripts/python/digitalmodel/legacy/32_-_FrequencyDomainResults.py` | `<module>` (L3) | [docs/registry/implementation-map.yaml:L13824-L13834] |
| `scripts/python/digitalmodel/legacy/33_-_FrequencyDomainMPM.py` | `<module>` (L3) | [docs/registry/implementation-map.yaml:L13835-L13845] |
| `scripts/python/digitalmodel/legacy/34_-_FrequencyDomainRangeGraph.py` | `<module>` (L3) | [docs/registry/implementation-map.yaml:L13846-L13856] |
| `scripts/python/digitalmodel/legacy/35_-_FrequencyDomainSpectralDensitySpectralResponse.py` | `<module>` (L4) | [docs/registry/implementation-map.yaml:L13857-L13867] |
| `scripts/python/digitalmodel/legacy/36_-_FrequencyDomainTimeHistorySynthesis.py` | `<module>` (L4) | [docs/registry/implementation-map.yaml:L13868-L13878] |
| `scripts/python/digitalmodel/legacy/analyze_marine_excel.py` | `analyze_excel_file` (L13) | [docs/registry/implementation-map.yaml:L13879-L13890] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler.profile_wave_spectrum` (L121); `PerformanceProfiler.profile_wave_spectrum.run_spectrum` (L146) | [docs/registry/implementation-map.yaml:L13891-L13914] |
| `scripts/python/digitalmodel/orcawave/mcp_orcawave.py` | `freq_analyze` (L36) | [docs/registry/implementation-map.yaml:L13915-L13926] |
| `src/digitalmodel/drilling_riser/global_model/build.py` | `_environment_and_simulation` (L430) | [docs/registry/implementation-map.yaml:L13927-L13938] |
| `src/digitalmodel/fatigue/scf_library.py` | `short_chord_factor_f1` (L108) | [docs/registry/implementation-map.yaml:L13939-L13950] |
| `src/digitalmodel/hydrodynamics/cli.py` | `spectrum_cmd` (L34) | [docs/registry/implementation-map.yaml:L13951-L13964] |
| `src/digitalmodel/hydrodynamics/seakeeping.py` | `operability_analysis` (L129) | [docs/registry/implementation-map.yaml:L13965-L13976] |
| `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra._normalize_to_significant_height` (L29); `WaveSpectra.jonswap` (L46); `WaveSpectra.pierson_moskowitz` (L98); `WaveSpectra.bretschneider` (L139); `WaveSpectra.issc` (L176); `WaveSpectra.generate_spectrum` (L215); `WaveSpectra.significant_height_from_spectrum` (L288); `WaveSpectra.zero_crossing_period_from_spectrum` (L308); `WaveSpectra.spectrum_statistics` (L348) | [docs/registry/implementation-map.yaml:L13977-L14096] |
| `src/digitalmodel/hydrodynamics/wave_spectrum/workflow.py` | `router` (L16); `_spectrum_name` (L78) | [docs/registry/implementation-map.yaml:L14097-L14122] |
| `src/digitalmodel/infrastructure/base_solvers/marine/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` (L207); `OrcaflexModelComponents.RayleighDampingCoefficients` (L307) | [docs/registry/implementation-map.yaml:L14123-L14146] |
| `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `_complex_rao` (L28); `crane_tip_raos` (L45); `crane_tip_significant_motion` (L110); `crane_tip_significant_velocity` (L153); `_jonswap_spectrum` (L171) | [docs/registry/implementation-map.yaml:L14147-L14213] |
| `src/digitalmodel/marine_ops/marine_analysis/analysis/excel_analyzer.py` | `analyze_excel_file` (L13) | [docs/registry/implementation-map.yaml:L14214-L14225] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler.profile_wave_spectrum` (L119); `PerformanceProfiler.profile_wave_spectrum.run_spectrum` (L139) | [docs/registry/implementation-map.yaml:L14226-L14249] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._wave_spectrum` (L55) | [docs/registry/implementation-map.yaml:L14218-L14231] |
| `src/digitalmodel/marine_ops/operation_envelope.py` | `operation_envelope` (L168); `_significant_response` (L283) | [docs/registry/implementation-map.yaml:L14262-L14287] |
| `src/digitalmodel/motion_forecast/wave_forecast.py` | `synthesize_directional_forecast` (L111) | [docs/registry/implementation-map.yaml:L14288-L14301] |
| `src/digitalmodel/motion_forecast/wave_source.py` | `jonswap_spectrum` (L21); `synthesize_forecast` (L49) | [docs/registry/implementation-map.yaml:L14302-L14328] |
| `src/digitalmodel/orcaflex/environment.py` | `WaveSpectrumParams.tz` (L138); `WaveSpectrumParams.spectral_density` (L146); `WaveSpectrumParams.to_orcaflex_dict` (L176) | [docs/registry/implementation-map.yaml:L14329-L14368] |
| `src/digitalmodel/orcawave/motion_statistics.py` | `compute_response_spectrum` (L97) | [docs/registry/implementation-map.yaml:L14369-L14380] |
| `src/digitalmodel/orcawave/wave_spectrum.py` | `pierson_moskowitz` (L109); `jonswap` (L140); `bretschneider` (L187); `issc_spectrum` (L207); `ochi_hubble._component` (L266); `torsethaugen` (L282); `generate_spectrum` (L335) | [docs/registry/implementation-map.yaml:L14381-L14477] |
| `src/digitalmodel/parametric/generate.py` | `_spectral_fatigue_annual_damage` (L208); `_esp_pump_utilisation` (L405) | [docs/registry/implementation-map.yaml:L14478-L14501] |
| `src/digitalmodel/rao_spectral_fatigue/workflow.py` | `_stress_psd_from_rao` (L84) | [docs/registry/implementation-map.yaml:L14502-L14513] |
| `src/digitalmodel/solvers/benchmark/solvers.py` | `run_orcaflex` (L49) | [docs/registry/implementation-map.yaml:L14514-L14525] |
| `src/digitalmodel/solvers/fea_model/environment_components.py` | `Environment.get_wave_waveTrains` (L89); `Environment.getJONSWAPGamma` (L100); `Environment.RayleighDampingCoefficients` (L193) | [docs/registry/implementation-map.yaml:L14526-L14563] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder._build_wave_trains` (L251); `EnvironmentBuilder._order_wave_train` (L367) | [docs/registry/implementation-map.yaml:L14564-L14591] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/schema/campaign.py` | `CampaignSpec.from_legacy_config` (L233) | [docs/registry/implementation-map.yaml:L14592-L14605] |
| `src/digitalmodel/solvers/orcaflex/orcaflex_model_components.py` | `OrcaflexModelComponents.wave` (L248); `OrcaflexModelComponents.RayleighDampingCoefficients` (L394) | [docs/registry/implementation-map.yaml:L14606-L14629] |
| `src/digitalmodel/spectral_fatigue/workflow.py` | `_evaluate_location` (L79) | [docs/registry/implementation-map.yaml:L14630-L14641] |
| `src/digitalmodel/structural/fatigue/analysis.py` | `FatigueAnalysisEngine.analyze_psd` (L421); `FatigueAnalysisEngine._perform_frequency_validation` (L786) | [docs/registry/implementation-map.yaml:L14642-L14667] |
| `src/digitalmodel/structural/fatigue/frequency_domain.py` | `NarrowBandMethod.calculate_damage_rate` (L191); `DirlikMethod.calculate_damage_rate` (L240); `TovoBenasciuttiMethod.calculate_damage_rate` (L382); `SingleMomentMethod.calculate_damage_rate` (L481) | [docs/registry/implementation-map.yaml:L14668-L14715] |
| `src/digitalmodel/structural/fatigue/scatter_fatigue.py` | `_resolve_wave_spectrum_func` (L281); `_resolve_wave_spectrum_func._jonswap_wrapper` (L305) | [docs/registry/implementation-map.yaml:L14716-L14739] |
| `src/digitalmodel/subsea/catenary_riser/legacy/orcaflexModel.py` | `orcaflexModel` (L13); `wave` (L410) | [docs/registry/implementation-map.yaml:L14740-L14763] |
| `src/digitalmodel/subsea/pipeline/free_span/wave_velocity.py` | `jonswap_gamma` (L69); `jonswap_spectrum` (L93); `compute_wave_velocity` (L269) | [docs/registry/implementation-map.yaml:L14764-L14806] |
| `src/digitalmodel/vessel_seakeeping/workflow.py` | `_spectrum_type` (L106) | [docs/registry/implementation-map.yaml:L14807-L14818] |
| `src/digitalmodel/workflows/installation_full_report.py` | `_design` (L116) | [docs/registry/implementation-map.yaml:L14819-L14830] |
| `src/digitalmodel/workflows/installation_report_sections.py` | `design_data` (L19) | [docs/registry/implementation-map.yaml:L14831-L14842] |
| `src/digitalmodel/workflows/installation_seastates.py` | `_configure` (L82); `_verify` (L109); `_change_payload` (L244) | [docs/registry/implementation-map.yaml:L14843-L14878] |
| `src/digitalmodel/workflows/workflow_automation/workflows.py` | `VesselResponseWorkflow.create` (L384) | [docs/registry/implementation-map.yaml:L14879-L14892] |
| `tests/hydrodynamics/rao_analysis/conftest.py` | `wave_spectrum` (L30) | [docs/registry/implementation-map.yaml:L14893-L14906] |
| `tests/hydrodynamics/test_seakeeping.py` | `jonswap_spectrum` (L43) | [docs/registry/implementation-map.yaml:L14907-L14920] |
| `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` (L32) | [docs/registry/implementation-map.yaml:L14921-L14932] |
| `tests/marine_ops/marine_engineering/integration/test_wave_dynamics_integration.py` | `TestWaveDynamicsIntegration.test_jonswap_generates_valid_spectrum` (L50); `TestWaveDynamicsIntegration.test_zero_crossing_period_relationship` (L95); `TestWaveDynamicsIntegration.test_wave_elevation_time_series_generation` (L134); `TestWaveDynamicsIntegration.test_wave_energy_distribution` (L240) | [docs/registry/implementation-map.yaml:L14933-L14980] |
| `tests/marine_ops/marine_engineering/legacy/test_validation.py` | `test_jonswap_hs_recovery` (L28); `test_pierson_moskowitz_hs_recovery` (L67) | [docs/registry/implementation-map.yaml:L14981-L15004] |
| `tests/marine_ops/marine_engineering/legacy/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` (L105); `TestJONSWAPSpectrum.test_hs_from_m0_relationship` (L121); `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` (L309); `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` (L349); `TestPublishedBenchmarks.test_jonswap_vs_published_curves` (L438) | [docs/registry/implementation-map.yaml:L15005-L15064] |
| `tests/marine_ops/marine_engineering/test_wave_spectra.py` | `TestJONSWAPSpectrum.test_spectral_moments_accuracy` (L105); `TestJONSWAPSpectrum.test_hs_from_m0_relationship` (L121); `TestIrregularWaveSynthesis.test_wave_synthesis_statistics` (L309); `TestIrregularWaveSynthesis.test_wave_synthesis_gaussian_distribution` (L349); `TestPublishedBenchmarks.test_jonswap_vs_published_curves` (L438) | [docs/registry/implementation-map.yaml:L15065-L15124] |
| `tests/orcaflex/test_model_default_tokens.py` | `wave_order_findings` (L385) | [docs/registry/implementation-map.yaml:L15125-L15136] |
| `tests/solvers/orcaflex/modular_generator/integration/test_campaign_integration.py` | `_write_floating_campaign` (L30) | [docs/registry/implementation-map.yaml:L15137-L15148] |
| `tests/solvers/orcaflex/modular_generator/test_campaign_generator.py` | `_make_campaign_data` (L13) | [docs/registry/implementation-map.yaml:L15149-L15160] |
| `tests/structural/fatigue/test_frequency_domain.py` | `TestSpectralMomentsCalculation.test_white_noise_m0_equals_level_times_bandwidth` (L215) | [docs/registry/implementation-map.yaml:L15161-L15172] |
| `tests/structural/fatigue/test_scatter_fatigue.py` | `_jonswap_hz` (L42) | [docs/registry/implementation-map.yaml:L15173-L15184] |
| `tests/unit/hydrodynamics/test_rao_analysis.py` | `simple_wave_params` (L37); `TestWaveSpectra.test_jonswap_peak_near_tp` (L191) | [docs/registry/implementation-map.yaml:L15185-L15210] |
| `tests/unit/hydrodynamics/test_wave_spectra_extended.py` | `TestPiersonMoskowitzExtended.test_pm_peak_near_tp` (L155); `TestBretschneiderExtended.test_bretschneider_peak_near_tp` (L202); `TestZeroCrossingPeriodFromSpectrum.test_tz_pm_approximately_0711_tp` (L393); `TestPeakFrequencyFromSpectrum.test_peak_frequency_near_2pi_over_tp` (L421) | [docs/registry/implementation-map.yaml:L15211-L15258] |
| `tests/workflows/hydrodynamics_assertions.py` | `_assert_wave_spectra` (L51) | [docs/registry/implementation-map.yaml:L15259-L15270] |
| `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` (L56); `test_spectral_fatigue_workflow_matches_narrow_band_closed_form` (L1876); `test_rao_spectral_fatigue_constant_rao_matches_fixed_gain_screening` (L1935); `test_vessel_seakeeping_significant_amplitude_matches_library` (L2005); `test_esp_pump_hydraulics_tdh_and_sizing_closed_form` (L2335) | [docs/registry/implementation-map.yaml:L15271-L15342] |
| `src/digitalmodel/fatigue/fatg.py` | `fatg_significant_stress_range` (L150); `fatg_significant_stress_range.ordinate` (L172) | [docs/registry/implementation-map.yaml:L15343-L15377] |

### Mapped inventory: wind-and-current-force-routes

All 135 map implementation rows are inventoried below; the map data revision is `270c8759c374b67a7f032953e7fcdb14e0224461` and its containing commit is `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`. [docs/registry/implementation-map.yaml:L15393-L15395]

| Path | Mapped symbols and source line at map data revision | Map inventory evidence |
| --- | --- | --- |
| Discovery file with identifying name 1 | implementation row 1 at source line 312 | [docs/registry/implementation-map.yaml:L15400-L15409] |
| Discovery file with identifying name 2 | implementation row 1 at source line 375 | [docs/registry/implementation-map.yaml:L15410-L15419] |
| Discovery file with identifying name 3 | implementation row 1 at source line 51 | [docs/registry/implementation-map.yaml:L15420-L15428] |
| Discovery file with identifying name 4 | implementation row 1 at source line 80 | [docs/registry/implementation-map.yaml:L15429-L15438] |
| `examples/domains/fpso/fpso_mooring_analysis.py` | `calculate_environmental_forces` (L194); `main` (L497) | [docs/registry/implementation-map.yaml:L15439-L15460] |
| `examples/domains/ocimf/ocimf_demo.py` | `main` (L24) | [docs/registry/implementation-map.yaml:L15461-L15470] |
| `examples/domains/workflows/01_spread_mooring_8point.py` | `calculate_environmental_forces` (L123) | [docs/registry/implementation-map.yaml:L15471-L15480] |
| `examples/domains/workflows/03_calm_buoy.py` | `analyze_calm_buoy` (L53) | [docs/registry/implementation-map.yaml:L15481-L15490] |
| `examples/domains/workflows/05_jackup_installation.py` | `analyze_jackup_mooring` (L37) | [docs/registry/implementation-map.yaml:L15491-L15500] |
| `examples/domains/workflows/06_floating_wind_turbine.py` | `analyze_floating_wind_mooring` (L37) | [docs/registry/implementation-map.yaml:L15501-L15510] |
| `examples/domains/workflows/07_wave_energy_converter.py` | `analyze_wec_mooring` (L36) | [docs/registry/implementation-map.yaml:L15511-L15520] |
| `examples/domains/workflows/08_aquaculture_mooring.py` | `analyze_aquaculture_mooring` (L35) | [docs/registry/implementation-map.yaml:L15521-L15530] |
| `examples/domains/workflows/09_spm_terminal.py` | `analyze_spm_terminal` (L34) | [docs/registry/implementation-map.yaml:L15531-L15540] |
| `examples/domains/workflows/10_dp_backup_mooring.py` | `analyze_dp_backup_mooring` (L36) | [docs/registry/implementation-map.yaml:L15541-L15550] |
| `scripts/capabilities/build_onepagers.py` | `<module>` (L36) | [docs/registry/implementation-map.yaml:L15551-L15559] |
| `scripts/python/digitalmodel/legacy/extract_ocimf_database.py` | `<module>` (L25); `extract_ocimf_database` (L220) | [docs/registry/implementation-map.yaml:L15560-L15578] |
| `scripts/python/digitalmodel/legacy/generate_integration_charts.py` | `<module>` (L40) | [docs/registry/implementation-map.yaml:L15579-L15587] |
| `scripts/python/digitalmodel/legacy/generate_ocimf_report.py` | `main` (L16) | [docs/registry/implementation-map.yaml:L15588-L15597] |
| `scripts/python/digitalmodel/legacy/profile_marine_modules.py` | `PerformanceProfiler._setup_test_data` (L112); `PerformanceProfiler.profile_ocimf_lookup` (L196); `PerformanceProfiler.run_all` (L499); `main` (L518) | [docs/registry/implementation-map.yaml:L15598-L15639] |
| `scripts/python/digitalmodel/legacy/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` (L104); `Phase2Validator.export_statistics_to_csv` (L853); `Phase2Validator.export_json_summary` (L1191); `Phase2Validator.run_complete_validation` (L1216) | [docs/registry/implementation-map.yaml:L15640-L15684] |
| `scripts/python/digitalmodel/legacy/visualize_ocimf_data.py` | `<module>` (L22); `main` (L371) | [docs/registry/implementation-map.yaml:L15685-L15703] |
| `scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py` | `make_polar_overlay` (L406); `parse_args` (L802) | [docs/registry/implementation-map.yaml:L15704-L15723] |
| `src/digitalmodel/ansys/pressure_vessel.py` | `PressureVesselGenerator.generate_wind_load` (L704) | [docs/registry/implementation-map.yaml:L15724-L15733] |
| `src/digitalmodel/data_systems/data_procurement/mooring/database_clients/synthetic_rope_db_client.py` | `SyntheticRopeDatabaseClient._generate_rope_spec` (L171) | [docs/registry/implementation-map.yaml:L15734-L15743] |
| `src/digitalmodel/data_systems/data_scraping/validators/validate_ocimf_data.py` | `main` (L367) | [docs/registry/implementation-map.yaml:L15744-L15753] |
| `src/digitalmodel/drilling_riser/drift_off.py` | `environmental_force_n` (L97) | [docs/registry/implementation-map.yaml:L15754-L15763] |
| `src/digitalmodel/drilling_riser/envelope.py` | `CurrentProfile.drag_load_n_per_m` (L108) | [docs/registry/implementation-map.yaml:L15764-L15773] |
| `src/digitalmodel/drilling_riser/global_model/orcaflex_run.py` | `as_analysed_sections` (L248) | [docs/registry/implementation-map.yaml:L15774-L15783] |
| `src/digitalmodel/dynmoor_mooring/workflow.py` | `router` (L101) | [docs/registry/implementation-map.yaml:L15784-L15793] |
| `src/digitalmodel/engine.py` | `engine` (L90) | [docs/registry/implementation-map.yaml:L15794-L15806] |
| `src/digitalmodel/hydrodynamics/cli.py` | `ocimf_wind_cmd` (L105); `ocimf_current_cmd` (L163); `combined_env_cmd` (L221) | [docs/registry/implementation-map.yaml:L15807-L15836] |
| `src/digitalmodel/infrastructure/base_solvers/fatigue/fatigue_analysis_components.py` | `FatigueAnalysisComponents.get_total_viv_fatigue` (L99); `FatigueAnalysisComponents.tabulate_viv_results` (L114) | [docs/registry/implementation-map.yaml:L15837-L15856] |
| `src/digitalmodel/infrastructure/base_solvers/viv/viv_fatigue_analysis_components.py` | `VIVFatigueAnalysisComponents.get_total_viv_fatigue` (L176); `VIVFatigueAnalysisComponents.tabulate_viv_results` (L200) | [docs/registry/implementation-map.yaml:L15857-L15876] |
| `src/digitalmodel/infrastructure/utils/ETL_components.py` | `ETL_components.transform_viv_current_reformat_1` (L746) | [docs/registry/implementation-map.yaml:L15877-L15886] |
| `src/digitalmodel/installation/installation_pamphlet/calculations.py` | `compute_splashzone_envelope` (L456) | [docs/registry/implementation-map.yaml:L15887-L15896] |
| `src/digitalmodel/installation/jumper_installation/calculations.py` | `calc_phase_3_splash` (L163) | [docs/registry/implementation-map.yaml:L15897-L15906] |
| `src/digitalmodel/marine_ops/marine_analysis/__main__.py` | `main` (L27) | [docs/registry/implementation-map.yaml:L15907-L15916] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` (L306); `EnvironmentalForces.calculate_total_forces` (L628); `create_sample_database` (L861) | [docs/registry/implementation-map.yaml:L15917-L15946] |
| `src/digitalmodel/marine_ops/marine_analysis/environmental_loading/workflow.py` | `OCIMFWorkflow.router` (L48) | [docs/registry/implementation-map.yaml:L15947-L15956] |
| `src/digitalmodel/marine_ops/marine_analysis/extraction/extract_ocimf.py` | `<module>` (L25); `extract_ocimf_database` (L220) | [docs/registry/implementation-map.yaml:L15957-L15975] |
| `src/digitalmodel/marine_ops/marine_analysis/profiling/profile_modules.py` | `PerformanceProfiler._setup_test_data` (L110); `PerformanceProfiler.profile_ocimf_lookup` (L188); `PerformanceProfiler.run_all` (L494); `main` (L513) | [docs/registry/implementation-map.yaml:L15976-L16019] |
| `src/digitalmodel/marine_ops/marine_analysis/reporting/ocimf_interactive_report.py` | `main` (L644) | [docs/registry/implementation-map.yaml:L16020-L16029] |
| `src/digitalmodel/marine_ops/marine_analysis/validation/validate_phase2.py` | `Phase2Validator.generate_excel_reference_data` (L104); `Phase2Validator.export_statistics_to_csv` (L853); `Phase2Validator.export_json_summary` (L1191); `Phase2Validator.run_complete_validation` (L1216) | [docs/registry/implementation-map.yaml:L16030-L16074] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/integration_charts.py` | `<module>` (L40) | [docs/registry/implementation-map.yaml:L16075-L16083] |
| `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py` | `_output_dir` (L25); `_cli_output_dir` (L34); `load_data` (L55); `main` (L448) | [docs/registry/implementation-map.yaml:L16084-L16126] |
| `src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py` | `OCIMFDatabase.get_coefficients` (L320); `EnvironmentalForces.calculate_total_forces` (L642); `create_sample_database` (L878) | [docs/registry/implementation-map.yaml:L16117-L16126] |
| `src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/fpso_full_workflow.py` | `FPSOMooringFullWorkflow._environmental_forces` (L78); `FPSOMooringFullWorkflow._ocimf_database` (L104) | [docs/registry/implementation-map.yaml:L16127-L16149] |
| `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py` | `_resolve_arrow_direction_in_body_frame` (L39); `polar_force_overlay` (L273) | [docs/registry/implementation-map.yaml:L16179-L16198] |
| `src/digitalmodel/naval_architecture/maneuvering_envelope.py` | `threshold_speed_for_steerage` (L211) | [docs/registry/implementation-map.yaml:L16199-L16208] |
| `src/digitalmodel/naval_architecture/maneuvering_envelope_workflow.py` | `run_maneuvering_envelope_sweep` (L233) | [docs/registry/implementation-map.yaml:L16209-L16218] |
| `src/digitalmodel/orcaflex/installation_analysis.py` | `SplashZoneInput.calculate_splash_zone_loads` (L245) | [docs/registry/implementation-map.yaml:L16219-L16228] |
| `src/digitalmodel/orcawave/drift_forces.py` | `compute_wind_current_drift` (L224) | [docs/registry/implementation-map.yaml:L16229-L16238] |
| `src/digitalmodel/solvers/orcaflex/examples_integration/integrator.py` | `KnowledgeIntegrator._extract_configurations` (L104) | [docs/registry/implementation-map.yaml:L16239-L16248] |
| `src/digitalmodel/solvers/orcaflex/modular_generator/builders/environment_builder.py` | `EnvironmentBuilder.build` (L171) | [docs/registry/implementation-map.yaml:L16249-L16258] |
| `src/digitalmodel/solvers/orcaflex/reporting/extractors/loads_extractor.py` | `extract_loads` (L12) | [docs/registry/implementation-map.yaml:L16259-L16268] |
| `src/digitalmodel/structural/parachute/chute_assessment.py` | `calculate_dual_chute_drag` (L81); `assess_all_load_cases` (L131); `simulate_deployment_transient` (L189); `export_results_yaml` (L296) | [docs/registry/implementation-map.yaml:L16269-L16314] |
| `src/digitalmodel/structural/parachute/parachute_drag.py` | `calculate_drag_force` (L37) | [docs/registry/implementation-map.yaml:L16315-L16324] |
| `src/digitalmodel/subsea/mooring_analysis/cli.py` | `design_verification` (L145) | [docs/registry/implementation-map.yaml:L16325-L16334] |
| `src/digitalmodel/subsea/mooring_analysis/designer.py` | `MooringDesigner.calculate_environmental_loads` (L60) | [docs/registry/implementation-map.yaml:L16335-L16344] |
| `src/digitalmodel/subsea/mooring_analysis/dynmoor.py` | `build_wind_model.force` (L1094) | [docs/registry/implementation-map.yaml:L16345-L16354] |
| `src/digitalmodel/subsea/on_bottom_stability/e305_simplified.py` | `_phase_demand` (L194) | [docs/registry/implementation-map.yaml:L16355-L16364] |
| `src/digitalmodel/subsea/on_bottom_stability/simplified.py` | `hydrodynamic_loads` (L171); `drag_force_per_meter` (L214); `lateral_stability_check` (L291); `check_lateral_stability` (L363); `lift_force_per_meter` (L237); `inertia_force_per_meter` (L268) | [docs/registry/implementation-map.yaml:L16365-L16791] |
| `src/digitalmodel/subsea/pipeline/integrity_workflows.py` | `run_on_bottom_stability` (L7) | [docs/registry/implementation-map.yaml:L16409-L16418] |
| `src/digitalmodel/subsea/viv_analysis/screening.py` | `VIVScreening.screen_with_current_profile` (L278) | [docs/registry/implementation-map.yaml:L16419-L16428] |
| `src/digitalmodel/subsea/viv_analysis/viv_analysis_legacy.py` | `viv_analysis` (L52) | [docs/registry/implementation-map.yaml:L16429-L16438] |
| `src/digitalmodel/subsea/viv_analysis/vortex_shedding.py` | `VortexSheddingAnalyzer.analyze_with_profile` (L160) | [docs/registry/implementation-map.yaml:L16439-L16448] |
| `src/digitalmodel/workflows/workflow_automation/templates.py` | `create_mooring_design_template` (L306) | [docs/registry/implementation-map.yaml:L16449-L16458] |
| `src/digitalmodel/workflows/workflow_automation/workflows.py` | `MooringSystemDesignWorkflow.create` (L168) | [docs/registry/implementation-map.yaml:L16459-L16468] |
| `tests/contracts/test_force_units_boundary_contract.py` | `<module>` (L52) | [docs/registry/implementation-map.yaml:L16469-L16477] |
| `tests/legal/test_published_pages_have_no_internal_paths.py` | `_load_producer` (L147) | [docs/registry/implementation-map.yaml:L16478-L16487] |
| `tests/marine_ops/marine_engineering/environmental_loading/test_ocimf.py` | `sample_database_path` (L30); `ocimf_db` (L50); `env_forces` (L56) | [docs/registry/implementation-map.yaml:L16488-L16517] |
| `tests/marine_ops/marine_engineering/integration/integration_example.py` | `main` (L32) | [docs/registry/implementation-map.yaml:L16518-L16528] |
| `tests/marine_ops/marine_engineering/integration/test_ocimf_mooring_integration.py` | `TestOCIMFMooringIntegration.test_combined_environmental_forces` (L105) | [docs/registry/implementation-map.yaml:L16529-L16538] |
| `tests/marine_ops/marine_engineering/test_marine_eng_perf.py` | `ocimf_database` (L32); `pytest_benchmark_update_json` (L364) | [docs/registry/implementation-map.yaml:L16539-L16559] |
| `tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py` | `test_refactored_polar_signatures_match_fixture` (L72) | [docs/registry/implementation-map.yaml:L16560-L16569] |
| `tests/solvers/orcaflex/reporting/fixture_helpers.py` | `_build_loads` (L161) | [docs/registry/implementation-map.yaml:L16570-L16579] |
| `tests/structural/parachute/test_chute_assessment.py` | `TestAssessAllLoadCases.test_governing_force_within_1_percent_of_hand_calc` (L136) | [docs/registry/implementation-map.yaml:L16580-L16589] |
| `tests/structural/parachute/test_parachute_drag.py` | `TestDragForce.test_drag_200_mph` (L70); `TestDragForce.test_drag_250_mph` (L82); `TestDragForce.test_force_scales_with_v_squared` (L98); `TestDragForce.test_result_has_newtons` (L105); `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_newtons` (L173); `TestChatGPTCrossValidation.test_steady_state_250mph_matches_chatgpt_lbs` (L188) | [docs/registry/implementation-map.yaml:L16590-L16655] |
| `tests/subsea/catenary_riser/conftest.py` | `catenary_environment` (L18) | [docs/registry/implementation-map.yaml:L16656-L16665] |
| `tests/subsea/on_bottom_stability/test_dnv_rp_f109.py` | `TestLiftForce.test_known_lift_force` (L103) | [docs/registry/implementation-map.yaml:L16666-L16675] |
| `tests/subsea/pipeline/test_on_bottom_stability.py` | `TestDragForce.test_scenario_1` (L72); `TestLiftForce.test_scenario_1` (L82) | [docs/registry/implementation-map.yaml:L16676-L16695] |
| `tests/test_automation/fixtures/engineering_fixtures.py` | `get_dnv_circular_mock` (L145); `get_dnv_rectangular_mock` (L170) | [docs/registry/implementation-map.yaml:L16696-L16715] |
| `tests/test_on_bottom_stability.py` | `TestHydrodynamicLoads.test_drag_force_per_meter` (L54); `TestHydrodynamicLoads.test_lift_force_per_meter` (L68) | [docs/registry/implementation-map.yaml:L16716-L16735] |
| `tests/workflows/test_durable_workflows.py` | `test_workflow_registry` (L56) | [docs/registry/implementation-map.yaml:L16736-L16747] |
| `src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py` | `hydrodynamic_force_per_meter` (L53); `lift_force_per_meter` (L90) | [docs/registry/implementation-map.yaml:L16748-L16769] |

4 discovery files with identifying names will require codenaming; issue pending (8). [docs/registry/implementation-map.yaml:L15400-L16791]


## 10. Coverage and open items

### Read and execution coverage

All eight direct source files were read at the code pin: initializer, `drift_forces.py`, `hydro_coefficients.py`, `motion_statistics.py`, `panel_mesh.py`, `rao_processing.py`, `vessel_database.py` and `wave_spectrum.py`. Their public exports enumerate the seven implementation modules. [src/digitalmodel/orcawave/__init__.py:L14-L102]

The eight directly relevant test files contain 96 executed cases, including 11 slow-drift variance cases; the whole registered directory contains 358 executed cases, with reporting/schema tests included in that broader execution rather than in the direct-file method audit. Test outcomes are recorded in section 4. [tests/orcawave/test_drift_force_variance.py:L168-L237] [docs/registry/module-routing.yaml:L249-L254]

| Test file | Executed cases at BASE | Outcome |
| --- | --- | --- |
| `tests/orcawave/test_drift_forces.py` | 11 | PASS |
| `tests/orcawave/test_drift_force_variance.py` | 11 | PASS |
| `tests/orcawave/test_hydro_coefficients.py` | 10 | PASS |
| `tests/orcawave/test_motion_statistics.py` | 13 | PASS |
| `tests/orcawave/test_panel_mesh.py` | 11 | PASS |
| `tests/orcawave/test_rao_processing.py` | 12 | PASS |
| `tests/orcawave/test_vessel_database.py` | 14 | PASS |
| `tests/orcawave/test_wave_spectrum.py` | 14 | PASS |

The remaining 262 cases in the registered directory cover adjacent reporting and schema behaviour. The current inventory count replaces the unconfirmed earlier count; it does not assert a line-coverage percentage.

Canonical documentation describes the separate diffraction CLI, while the intended-API pages explicitly mark older snippets as unimplemented. Their examples are not treated as callable direct-package API. [docs/domains/orcawave/README.md:L29-L33] [docs/domains/orcawave/README.md:L198-L205] [docs/domains/orcawave/intended-api/README.md:L1-L13]

Documentation discovery found 147 Markdown/reStructuredText files under `docs/` naming the module/program. Seven were read fully, two in relevant ranges, and 138 retain deferred semantic coverage. This is an explicit coverage limit, not an assertion that every discovered page supports a method here; issue pending (7).

### Open-item register

The maintainer is responsible for assigning each follow-up; draft references are provisional until issue numbers are created. Independent acceptance checks the resulting issue numbers before clearance.

| Item | Evidence gap and required disposition | Tracking |
| --- | --- | --- |
| Input validation and U1 | Shared axes, origins, phase lead/lag, format units, sparse/ragged arrays and malformed/locale numeric text remain unqualified; add rejection and convention tests. [src/digitalmodel/orcawave/rao_processing.py:L117-L150] [src/digitalmodel/orcawave/panel_mesh.py:L95-L130] | issue pending (1) |
| Mesh reference | Correct volume/sign/normals, waterline reconstruction, scaling and degenerate-geometry detection; existing positivity assertion does not qualify C4. [src/digitalmodel/orcawave/panel_mesh.py:L166-L284] | issue pending (2) |
| Coefficient conversion | Qualify dimensions and external WAMIT frequency/scaling semantics against an independent source; preserve raw-vs-qualified distinction. [src/digitalmodel/orcawave/hydro_coefficients.py:L162-L236] | issue pending (3) |
| Standards and U2–U4 | ISO edition/clause/compliance, DNV applicability and runtime citation sidecars remain unestablished; clause text not checked against the licensed copy; no licensed numeric data reproduced. Statistical/spectral domain and amplitude-definition defects require independent tests. [src/digitalmodel/orcawave/motion_statistics.py:L131-L174] [src/digitalmodel/orcawave/motion_statistics.py:L290-L339] | issue pending (4) |
| Preliminary data | Qualify provenance and prevent silent synthetic/default response data becoming analysis inputs. [src/digitalmodel/orcawave/vessel_database.py:L211-L398] | issue pending (5) |
| Force/RAO assumptions | Require explicit wind/current basis, moment origins and valid DOFs/grids/coupling weights. [src/digitalmodel/orcawave/drift_forces.py:L153-L177] [src/digitalmodel/orcawave/drift_forces.py:L235-L309] [src/digitalmodel/orcawave/rao_processing.py:L214-L252] | issue pending (6) |
| Drift input validation | Refuse nonuniform or incomplete/non-Hermitian QTFs; no refusal regression exists at the pin. [src/digitalmodel/orcawave/drift_forces.py:L180-L228] | #2624 |
| Native acceptance and convergence | Complete C1 and production C4 checks with qualified meshes, explicit program versions and native evidence; no solver acceptance claimed. [src/digitalmodel/orcawave/panel_mesh.py:L62-L70] | #610, #612; issue pending (7) for pack/evidence integration |
| Ownership and U5 / adjacent scope | Confirm deputy role, finish deferred documentary reads and separate reporting qualification; no generated QA appendix or shared-log record is established. [docs/registry/module-routing.yaml:L249-L254] | issue pending (7); #1709 for coverage work |
| Registry baseline | Strict duplicate-key parsing fails on the existing solver-domain `submodules` keys at the code pin; this change adds neither duplicate and leaves their bytes unchanged. Normal parsed comparison confirms the OrcaWave addition only. [docs/registry/module-routing.yaml:L303-L317] | issue pending (9) |
| Map follow-up / neutral identifiers | Add unmapped result families and outside-package algorithms; replace six identifying discovery filenames and one filename containing internal wording with neutral IDs before publishing their exact locations. [src/digitalmodel/hydrodynamics/river_currents/vessel_forces.py:L47-L82] [src/digitalmodel/fatigue/spectral_fatigue.py:L116-L143] | issue pending (8) |

No client-derived values or private source files are consumed by this pack. No private-sidecar entry is asserted or created.

### Discovered documentation inventory

| Status | Discovered paths and evidence |
| --- | --- |
| Semantic review deferred; issue pending (7) | `docs/README.md` [docs/README.md:L26-L26]; `docs/RELOCATED-DATA.md` [docs/RELOCATED-DATA.md:L47-L47]; `docs/api/cfd/cases/wave_excited_body/README.md` [docs/api/cfd/cases/wave_excited_body/README.md:L116-L116]; `docs/archive/ARCHIVE-MOVE-PLAN-2026-10-08.md` [docs/archive/ARCHIVE-MOVE-PLAN-2026-10-08.md:L85-L85]; `docs/cad-inventory/ecosystem-and-automation-surface.md` [docs/cad-inventory/ecosystem-and-automation-surface.md:L81-L81] |
| Semantic review deferred; issue pending (7) | `docs/capability-map/capability-report-2026-02-14.md` [docs/capability-map/capability-report-2026-02-14.md:L35-L35]; `docs/domains/README.md` [docs/domains/README.md:L12-L12]; `docs/domains/SPEC_AUDIT_REPORT.md` [docs/domains/SPEC_AUDIT_REPORT.md:L4-L4]; `docs/domains/_index.md` [docs/domains/_index.md:L12-L12]; `docs/domains/aqwa/AQWA_TESTING_SUMMARY.md` [docs/domains/aqwa/AQWA_TESTING_SUMMARY.md:L254-L254] |
| Semantic review deferred; issue pending (7) | `docs/domains/aqwa/README.md` [docs/domains/aqwa/README.md:L16-L16]; `docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md` [docs/domains/design-tools/STL_TO_GDF_CONVERSION_GUIDE.md:L1-L1]; `docs/domains/diffraction/AQWA_PARSER.md` [docs/domains/diffraction/AQWA_PARSER.md:L205-L205]; `docs/domains/diffraction/CLI_GUIDE.md` [docs/domains/diffraction/CLI_GUIDE.md:L8-L8]; `docs/domains/diffraction/ORCAWAVE_CONVERTER_GUIDE.md` [docs/domains/diffraction/ORCAWAVE_CONVERTER_GUIDE.md:L1-L1] |
| Semantic review deferred; issue pending (7) | `docs/domains/diffraction/PHASE_2_COMPLETION.md` [docs/domains/diffraction/PHASE_2_COMPLETION.md:L409-L409]; `docs/domains/diffraction/examples/ORCAWAVE_TEST_RESULTS.md` [docs/domains/diffraction/examples/ORCAWAVE_TEST_RESULTS.md:L1-L1]; `docs/domains/diffraction/examples/README.md` [docs/domains/diffraction/examples/README.md:L301-L301]; `docs/domains/diffraction/tutorials/01_getting_started.md` [docs/domains/diffraction/tutorials/01_getting_started.md:L8-L8]; `docs/domains/diffraction/tutorials/02_aqwa_conversion_example.md` [docs/domains/diffraction/tutorials/02_aqwa_conversion_example.md:L346-L346] |
| Semantic review deferred; issue pending (7) | `docs/domains/diffraction/tutorials/03_batch_processing.md` [docs/domains/diffraction/tutorials/03_batch_processing.md:L33-L33]; `docs/domains/hull_library/curvature-signature-table.md` [docs/domains/hull_library/curvature-signature-table.md:L70-L70]; `docs/domains/hull_library/hullprod-curvature-screening-evaluation.md` [docs/domains/hull_library/hullprod-curvature-screening-evaluation.md:L43-L43]; `docs/domains/infrastructure/SKILL_VERSIONING.md` [docs/domains/infrastructure/SKILL_VERSIONING.md:L235-L235]; `docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md` [docs/domains/infrastructure/SKILL_VERSIONING_IMPLEMENTATION.md:L225-L225] |
| Semantic review deferred; issue pending (7) | `docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md` [docs/domains/infrastructure/SKILL_VERSIONING_QUICK_REF.md:L160-L160]; `docs/domains/infrastructure/cache_implementation_summary.md` [docs/domains/infrastructure/cache_implementation_summary.md:L251-L251]; `docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md` [docs/domains/mooring/MOORING_ANALYSIS_IMPLEMENTATION.md:L396-L396]; `docs/domains/orcaflex/MANUALS_AND_REFERENCES.md` [docs/domains/orcaflex/MANUALS_AND_REFERENCES.md:L1-L1]; `docs/domains/orcaflex/ORCAFLEX_MODEL_PREPARATION_GUIDE.md` [docs/domains/orcaflex/ORCAFLEX_MODEL_PREPARATION_GUIDE.md:L18-L18] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md` [docs/domains/orcaflex/ORCAFLEX_MODULE_COMPLETION.md:L291-L291]; `docs/domains/orcaflex/SECTION_FIDELITY_ANALYSIS.md` [docs/domains/orcaflex/SECTION_FIDELITY_ANALYSIS.md:L7-L7]; `docs/domains/orcaflex/SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md` [docs/domains/orcaflex/SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md:L57-L57]; `docs/domains/orcaflex/SPEC_AUDIT_SNAPSHOT.md` [docs/domains/orcaflex/SPEC_AUDIT_SNAPSHOT.md:L16-L16]; `docs/domains/orcaflex/aqwa/to_orcaflex/from_aqwa_to_orcaflex.md` [docs/domains/orcaflex/aqwa/to_orcaflex/from_aqwa_to_orcaflex.md:L88-L88] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcaflex/aqwa/to_orcawave/from_aqwa_to_orcawave.md` [docs/domains/orcaflex/aqwa/to_orcawave/from_aqwa_to_orcawave.md:L2-L2]; `docs/domains/orcaflex/property_routing/routing_map.md` [docs/domains/orcaflex/property_routing/routing_map.md:L106-L106]; `docs/domains/orcaflex/property_routing/walkthrough_examples.md` [docs/domains/orcaflex/property_routing/walkthrough_examples.md:L234-L234]; `docs/domains/orcaflex/reference/INDEX.md` [docs/domains/orcaflex/reference/INDEX.md:L35-L35]; `docs/domains/orcaflex/reference/vessel-types-conventions.md` [docs/domains/orcaflex/reference/vessel-types-conventions.md:L99-L99] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcaflex/support/naval_architecture/passing_ship.md` [docs/domains/orcaflex/support/naval_architecture/passing_ship.md:L3-L3]; `docs/domains/orcaflex/support/orcaflex_vs_orcawave/orcaflex_vs_orcawave.md` [docs/domains/orcaflex/support/orcaflex_vs_orcawave/orcaflex_vs_orcawave.md:L5-L5]; `docs/domains/orcaflex/support/orcawave/examples.md` [docs/domains/orcaflex/support/orcawave/examples.md:L1-L1]; `docs/domains/orcawave/DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md` [docs/domains/orcawave/DIFFRACTION_CAPABILITIES_EXPANSION_PLAN.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/AGENT_PROMPT.md` [docs/domains/orcawave/L01_aqwa_benchmark/AGENT_PROMPT.md:L12-L12] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcawave/L01_aqwa_benchmark/API_INVESTIGATION_SUMMARY.md` [docs/domains/orcawave/L01_aqwa_benchmark/API_INVESTIGATION_SUMMARY.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md` [docs/domains/orcawave/L01_aqwa_benchmark/COMPARISON_SUMMARY.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/EXECUTION_TEST_RESULTS.md` [docs/domains/orcawave/L01_aqwa_benchmark/EXECUTION_TEST_RESULTS.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/INTERPOLATION_COMPARISON_SUMMARY.md` [docs/domains/orcawave/L01_aqwa_benchmark/INTERPOLATION_COMPARISON_SUMMARY.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/MODEL_UPDATE_SUMMARY.md` [docs/domains/orcawave/L01_aqwa_benchmark/MODEL_UPDATE_SUMMARY.md:L1-L1] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcawave/L01_aqwa_benchmark/ORCAWAVE_GUI_EXECUTION_RESULTS.md` [docs/domains/orcawave/L01_aqwa_benchmark/ORCAWAVE_GUI_EXECUTION_RESULTS.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/QUICK_REFERENCE_API_FIXES.md` [docs/domains/orcawave/L01_aqwa_benchmark/QUICK_REFERENCE_API_FIXES.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/README_ORCAWAVE_EXECUTION.md` [docs/domains/orcawave/L01_aqwa_benchmark/README_ORCAWAVE_EXECUTION.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/ROOT_CAUSE_ANALYSIS.md` [docs/domains/orcawave/L01_aqwa_benchmark/ROOT_CAUSE_ANALYSIS.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/SESSION_STATUS.md` [docs/domains/orcawave/L01_aqwa_benchmark/SESSION_STATUS.md:L1-L1] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcawave/L01_aqwa_benchmark/SESSION_SUMMARY.md` [docs/domains/orcawave/L01_aqwa_benchmark/SESSION_SUMMARY.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/THREAD_USAGE_SUMMARY.md` [docs/domains/orcawave/L01_aqwa_benchmark/THREAD_USAGE_SUMMARY.md:L1-L1]; `docs/domains/orcawave/L01_aqwa_benchmark/way_forward.md` [docs/domains/orcawave/L01_aqwa_benchmark/way_forward.md:L3-L3]; `docs/domains/orcawave/LICENSED_E2E_ACCEPTANCE.md` [docs/domains/orcawave/LICENSED_E2E_ACCEPTANCE.md:L3-L3]; `docs/domains/orcawave/examples/L01_default_vessel/API_EXECUTION_SUMMARY.md` [docs/domains/orcawave/examples/L01_default_vessel/API_EXECUTION_SUMMARY.md:L1-L1] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcawave/examples/L01_default_vessel/LICENSE_TEST_RESULTS.md` [docs/domains/orcawave/examples/L01_default_vessel/LICENSE_TEST_RESULTS.md:L1-L1]; `docs/domains/orcawave/examples/ORCAFLEX_MODELS.md` [docs/domains/orcawave/examples/ORCAFLEX_MODELS.md:L3-L3]; `docs/domains/orcawave/examples/PARAMETER_REFERENCE.md` [docs/domains/orcawave/examples/PARAMETER_REFERENCE.md:L1-L1]; `docs/domains/orcawave/examples/qa/QA_REPORT.md` [docs/domains/orcawave/examples/qa/QA_REPORT.md:L1-L1]; `docs/domains/orcawave/intended-api/aqwa-benchmark.md` [docs/domains/orcawave/intended-api/aqwa-benchmark.md:L1-L1] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcawave/intended-api/damping-sweep.md` [docs/domains/orcawave/intended-api/damping-sweep.md:L1-L1]; `docs/domains/orcawave/intended-api/multi-body.md` [docs/domains/orcawave/intended-api/multi-body.md:L1-L1]; `docs/domains/orcawave/intended-api/to-orcaflex.md` [docs/domains/orcawave/intended-api/to-orcaflex.md:L1-L1]; `docs/domains/orcawave/notes/post/orcaflex_import.md` [docs/domains/orcawave/notes/post/orcaflex_import.md:L3-L3]; `docs/domains/orcawave/notes/post/owr.md` [docs/domains/orcawave/notes/post/owr.md:L4-L4] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcawave/notes/pre_mesh/mesh.md` [docs/domains/orcawave/notes/pre_mesh/mesh.md:L9-L9]; `docs/domains/orcawave/notes/pre_model/file_extensions.md` [docs/domains/orcawave/notes/pre_model/file_extensions.md:L1-L1]; `docs/domains/orcawave/notes/run_time.md` [docs/domains/orcawave/notes/run_time.md:L25-L25]; `docs/domains/orcawave/reference/data-bodies.md` [docs/domains/orcawave/reference/data-bodies.md:L17-L17]; `docs/domains/orcawave/reference/data-calculation-output.md` [docs/domains/orcawave/reference/data-calculation-output.md:L10-L10] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcawave/reference/data-constraints.md` [docs/domains/orcawave/reference/data-constraints.md:L27-L27]; `docs/domains/orcawave/reference/data-environment.md` [docs/domains/orcawave/reference/data-environment.md:L40-L40]; `docs/domains/orcawave/reference/data-inertia.md` [docs/domains/orcawave/reference/data-inertia.md:L49-L49]; `docs/domains/orcawave/reference/data-model.md` [docs/domains/orcawave/reference/data-model.md:L11-L11]; `docs/domains/orcawave/reference/data-morison.md` [docs/domains/orcawave/reference/data-morison.md:L16-L16] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcawave/reference/data-qtfs.md` [docs/domains/orcawave/reference/data-qtfs.md:L16-L16]; `docs/domains/orcawave/reference/data-spring-dampers.md` [docs/domains/orcawave/reference/data-spring-dampers.md:L61-L61]; `docs/domains/orcawave/reference/orcina-l07-wave-drift-load-analysis.md` [docs/domains/orcawave/reference/orcina-l07-wave-drift-load-analysis.md:L11-L11]; `docs/domains/orcawave/reference/text-data-files.md` [docs/domains/orcawave/reference/text-data-files.md:L40-L40]; `docs/domains/orcawave/support/scripts/scripts.md` [docs/domains/orcawave/support/scripts/scripts.md:L9-L9] |
| Semantic review deferred; issue pending (7) | `docs/domains/orcfxapi/reference/python-ref-model.md` [docs/domains/orcfxapi/reference/python-ref-model.md:L285-L285]; `docs/domains/project-docs/CLEANUP_SUMMARY.md` [docs/domains/project-docs/CLEANUP_SUMMARY.md:L3-L3]; `docs/domains/project-docs/FULL_CLEANUP_REPORT.md` [docs/domains/project-docs/FULL_CLEANUP_REPORT.md:L35-L35]; `docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md` [docs/domains/project-docs/MODULE_IMPLEMENTATION_PHASE4.md:L105-L105]; `docs/domains/project-docs/MODULE_IMPLEMENTATION_RECOMMENDATIONS.md` [docs/domains/project-docs/MODULE_IMPLEMENTATION_RECOMMENDATIONS.md:L29-L29] |
| Semantic review deferred; issue pending (7) | `docs/domains/project-docs/MODULE_PORTFOLIO_2026.md` [docs/domains/project-docs/MODULE_PORTFOLIO_2026.md:L33-L33]; `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md` [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE2.md:L30-L30]; `docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md` [docs/domains/project-docs/MODULE_RECOMMENDATIONS_PHASE3.md:L42-L42]; `docs/domains/project-docs/MODULE_SURVEY.md` [docs/domains/project-docs/MODULE_SURVEY.md:L17-L17]; `docs/domains/standards/RAO_PHASE_CONVENTIONS.md` [docs/domains/standards/RAO_PHASE_CONVENTIONS.md:L14-L14] |
| Semantic review deferred; issue pending (7) | `docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md` [docs/domains/testing/CLI_TESTING_FRAMEWORK_COMPLETE.md:L396-L396]; `docs/maps/digitalmodel-operator-map.md` [docs/maps/digitalmodel-operator-map.md:L9-L9]; `docs/plans/2026-05-05-issue-272-engineering-standards-citation-audit.md` [docs/plans/2026-05-05-issue-272-engineering-standards-citation-audit.md:L23-L23]; `docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md` [docs/plans/2026-05-05-issue-465-capytaine-ship-hull-benchmarking.md:L12-L12]; `docs/plans/2026-05-05-issue-470-openfoam-seakeeping.md` [docs/plans/2026-05-05-issue-470-openfoam-seakeeping.md:L11-L11] |
| Semantic review deferred; issue pending (7) | `docs/plans/2026-05-05-issue-483-curves-py-decomposition.md` [docs/plans/2026-05-05-issue-483-curves-py-decomposition.md:L27-L27]; `docs/plans/2026-05-05-issue-507-orcaflex-orcina-help-ingestion.md` [docs/plans/2026-05-05-issue-507-orcaflex-orcina-help-ingestion.md:L9-L9]; `docs/plans/2026-05-15-issue-605-self-contained-orcawave-packages.md` [docs/plans/2026-05-15-issue-605-self-contained-orcawave-packages.md:L1-L1]; `docs/plans/2026-05-15-issue-606-orcawave-meshpipeline-integration.md` [docs/plans/2026-05-15-issue-606-orcawave-meshpipeline-integration.md:L1-L1]; `docs/plans/2026-05-15-issue-607-orcawave-given-mesh-cli.md` [docs/plans/2026-05-15-issue-607-orcawave-given-mesh-cli.md:L1-L1] |
| Semantic review deferred; issue pending (7) | `docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md` [docs/plans/2026-05-15-issue-608-orcawave-mesh-quality-gates.md:L1-L1]; `docs/plans/2026-05-15-issue-609-orcawave-auxiliary-mesh-handling.md` [docs/plans/2026-05-15-issue-609-orcawave-auxiliary-mesh-handling.md:L1-L1]; `docs/plans/2026-05-15-issue-610-orcawave-arbitrary-mesh-acceptance-test.md` [docs/plans/2026-05-15-issue-610-orcawave-arbitrary-mesh-acceptance-test.md:L1-L1]; `docs/plans/2026-05-15-issue-611-orcawave-result-contract.md` [docs/plans/2026-05-15-issue-611-orcawave-result-contract.md:L1-L1]; `docs/plans/2026-05-15-issue-612-orcawave-convergence-sensitivity-workflow.md` [docs/plans/2026-05-15-issue-612-orcawave-convergence-sensitivity-workflow.md:L1-L1] |
| Semantic review deferred; issue pending (7) | `docs/plans/2026-05-15-issue-613-orcawave-doctor-command.md` [docs/plans/2026-05-15-issue-613-orcawave-doctor-command.md:L1-L1]; `docs/plans/2026-05-15-issue-614-orcawave-docs-canonical-workflow.md` [docs/plans/2026-05-15-issue-614-orcawave-docs-canonical-workflow.md:L1-L1]; `docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md` [docs/plans/2026-05-20-issue-616-ocimf-polar-vessel-force-overlay.md:L22-L22]; `docs/plans/2026-06-26-issue-1038-ocr-unlimited-ocr-evaluation.md` [docs/plans/2026-06-26-issue-1038-ocr-unlimited-ocr-evaluation.md:L46-L46]; `docs/plans/2026-06-29-cfd-capability-assessment.md` [docs/plans/2026-06-29-cfd-capability-assessment.md:L46-L46] |
| Semantic review deferred; issue pending (7) | `docs/plans/2026-06-29-cfd-environment-spec.md` [docs/plans/2026-06-29-cfd-environment-spec.md:L10-L10]; `docs/plans/2026-07-13-issue-1571-aqwa-triangles-frequency-v261.md` [docs/plans/2026-07-13-issue-1571-aqwa-triangles-frequency-v261.md:L31-L31]; `docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md` [docs/plans/2026-08-03-issue-1633-ship-benchmark-verdict.md:L21-L21]; 1 discovery document with internal wording (issue pending (8)); `docs/plans/2026-09-11-issue-2089-ghs-canary.md` [docs/plans/2026-09-11-issue-2089-ghs-canary.md:L17-L17] |
| Semantic review deferred; issue pending (7) | `docs/plans/evidence/2026-08-03-plan-1633-r1-claude.md` [docs/plans/evidence/2026-08-03-plan-1633-r1-claude.md:L15-L15]; `docs/plans/evidence/2026-08-03-plan-1633-r2-codex-evidence.md` [docs/plans/evidence/2026-08-03-plan-1633-r2-codex-evidence.md:L7-L7]; `docs/plans/evidence/2026-08-03-plan-1633-r2-codex-physics.md` [docs/plans/evidence/2026-08-03-plan-1633-r2-codex-physics.md:L6-L6]; `docs/plans/evidence/2026-08-03-plan-1633-r2-codex-testdesign.md` [docs/plans/evidence/2026-08-03-plan-1633-r2-codex-testdesign.md:L10-L10]; `docs/plans/evidence/2026-08-10-issue-1983-triage.md` [docs/plans/evidence/2026-08-10-issue-1983-triage.md:L116-L116] |
| Semantic review deferred; issue pending (7) | `docs/plans/evidence/2026-09-12-issue-1633-claude-continuation.md` [docs/plans/evidence/2026-09-12-issue-1633-claude-continuation.md:L55-L55]; `docs/plans/evidence/2026-09-12-issue-2094-claude-r2.md` [docs/plans/evidence/2026-09-12-issue-2094-claude-r2.md:L33-L33]; `docs/plans/parametric-refresh/adding-an-atlas.md` [docs/plans/parametric-refresh/adding-an-atlas.md:L18-L18]; `docs/plans/parametric-refresh/atlas-spec.md` [docs/plans/parametric-refresh/atlas-spec.md:L34-L34]; `docs/plans/parametric-refresh/licensed-libraries.md` [docs/plans/parametric-refresh/licensed-libraries.md:L8-L8] |
| Semantic review deferred; issue pending (7) | `docs/plans/parametrics-preparedness.md` [docs/plans/parametrics-preparedness.md:L4-L4]; `docs/postprocess/results-workbook.md` [docs/postprocess/results-workbook.md:L4-L4]; `docs/registry/implementation-map.md` [docs/registry/implementation-map.md:L19-L19]; `docs/reports/2026-10-09-compute-program/README.md` [docs/reports/2026-10-09-compute-program/README.md:L10-L10]; `docs/research/hydrodynamics-industry-practice.md` [docs/research/hydrodynamics-industry-practice.md:L19-L19] |
| Semantic review deferred; issue pending (7) | `docs/spikes/2026-09-25-hullprod-curvature-screening/README.md` [docs/spikes/2026-09-25-hullprod-curvature-screening/README.md:L37-L37] |
| Reviewed lines 1–180 | `docs/domains/diffraction/README.md` [docs/domains/diffraction/README.md:L3-L3]; `docs/domains/orcawave/PHASE_1_COMPLETION.md` [docs/domains/orcawave/PHASE_1_COMPLETION.md:L14-L14] |
| Reviewed full text | `docs/domains/orcawave/README.md` [docs/domains/orcawave/README.md:L1-L1]; `docs/domains/orcawave/intended-api/README.md` [docs/domains/orcawave/intended-api/README.md:L1-L1]; `docs/domains/orcawave/intended-api/analysis.md` [docs/domains/orcawave/intended-api/analysis.md:L1-L1]; `docs/domains/orcawave/intended-api/mesh-generation.md` [docs/domains/orcawave/intended-api/mesh-generation.md:L1-L1]; `docs/domains/orcawave/intended-api/qtf-analysis.md` [docs/domains/orcawave/intended-api/qtf-analysis.md:L1-L1] |
| Reviewed full text | `docs/domains/orcawave/reference/INDEX.md` [docs/domains/orcawave/reference/INDEX.md:L1-L1]; `docs/domains/orcawave/reference/data-wave-spectrum.md` [docs/domains/orcawave/reference/data-wave-spectrum.md:L12-L12] |

2 discovery files with identifying names and 1 document with internal wording are withheld from this inventory; issue pending (8).
