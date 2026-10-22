# Vessel seakeeping module knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/vessel_seakeeping` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` (`origin/main` at inspection) |
| Revision / status | 0 / DRAFT; synthetic spectral arithmetic, engineering qualification incomplete |
| QA family | EC; operability screening |

The workflow combines each DOF's motion RAO with a wave spectrum and weighted
sea-state scatter. Significant single amplitudes determine per-DOF operability;
all DOFs must meet the required percentage. Nonfinite inputs, frequency-grid
handling and underflow require further guards before engineering acceptance.

### Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-22 | Record the spectral contract and add the previously absent module test folder with independent examples. |

## 1. Purpose and scope

The facade is descriptive only; `workflow.router` is the sole public local
function. The hydrodynamics engine and spectrum generator supply the calculation.
[src/digitalmodel/vessel_seakeeping/__init__.py:L1-L8]
[src/digitalmodel/vessel_seakeeping/workflow.py:L29-L56]
At the pin there is no `tests/vessel_seakeeping/` folder. The new folder fills the
workflow gap; existing `tests/hydrodynamics/test_seakeeping.py` covers the engine.
The pack preserves production code, including the documented input gaps.

## 2. Inputs and outputs

Inventory keys are `file_stem.symbol`; imported standard-library helpers are not
workflow API definitions. All configuration units must be supplied consistently;
`unit` is only a label and performs no conversion.

| Public function / class | Input, units, validation and defaults | Output contract | Evidence |
|---|---|---|---|
| `workflow.router` | cfg mapping with vessel_seakeeping settings; required operability, frequency grid, scatter and DOFs below; defaults for vessel label, spectrum, gamma and output directory | Mutates cfg['vessel_seakeeping'] and cfg['screening_status'], writes two CSVs, returns same cfg; validation/file/numerical exceptions propagate | [src/digitalmodel/vessel_seakeeping/workflow.py:L29-L103] |

| Configuration input | Units, validation and defaults | Evidence |
|---|---|---|
| vessel | str conversion, default 'vessel'; descriptive label | [src/digitalmodel/vessel_seakeeping/workflow.py:L30-L31] |
| spectrum_type | stripped/lowercased jonswap, pm or bretschneider; defaults jonswap, unknown raises | [src/digitalmodel/vessel_seakeeping/workflow.py:L106-L112] |
| gamma | float conversion; JONSWAP peakedness default at locator, NOT_EVALUATED numerical provenance; positivity/finiteness unchecked; unused for other spectra | [src/digitalmodel/vessel_seakeeping/workflow.py:L32-L33] [src/digitalmodel/vessel_seakeeping/workflow.py:L49-L56] |
| required_operability_pct | Required > zero, NaN refused, percentage; positive infinity and values above a full percentage are accepted; no upper bound | [src/digitalmodel/vessel_seakeeping/workflow.py:L34-L38] [src/digitalmodel/vessel_seakeeping/workflow.py:L183-L190] |
| rao_frequency_rad_s | Required list with at least two float-convertible entries, strictly increasing and first > zero; rad/s; no explicit finite check; nonuniform grids accepted but downstream regenerated uniformly | [src/digitalmodel/vessel_seakeeping/workflow.py:L115-L130] |
| sea_states | Nonempty list; hs (m) and tp (s) required > zero and not NaN; probability defaults identity weight, must ≥ zero, total > zero; weights need not sum to unity; nonfinite weights/positive-infinite hs or tp not refused locally | [src/digitalmodel/vessel_seakeeping/workflow.py:L133-L149] [src/digitalmodel/vessel_seakeeping/workflow.py:L183-L190] |
| dofs | Nonempty list of mappings; name nonblank string; rao_amplitude list matching frequency count and nonnegative; no finite check or duplicate-name refusal | [src/digitalmodel/vessel_seakeeping/workflow.py:L152-L180] |
| dof.criterion / unit | Required criterion > zero, NaN refused but positive infinity accepted; criterion in response amplitude unit; unit defaults empty string; translational RAOs typically m/m, rotational RAOs response angle/m | [src/digitalmodel/vessel_seakeeping/workflow.py:L163-L190] [src/digitalmodel/hydrodynamics/seakeeping.py:L40-L45] |
| output_dir / configuration paths | Defaults 'results'; relative to _config_dir_path, else _config_file_path parent, else current directory; prefix from filename stem, else basename, else package name | [src/digitalmodel/vessel_seakeeping/workflow.py:L193-L221] |

| Output | Units / meaning / limits | Evidence |
|---|---|---|
| Detailed CSV | dof, unit, hs (m), tp (s), probability (input weight), significant_amplitude (response unit), criterion (same unit), operable (boolean); rows for each DOF and sea state; engine m0 not retained | [src/digitalmodel/vessel_seakeeping/workflow.py:L58-L70] |
| DOF summaries / summary CSV | dof, unit, criterion, operability_pct, required_operability_pct, passes; all percentages weighted by occurrence; amplitude ≤ criterion is operable | [src/digitalmodel/vessel_seakeeping/workflow.py:L71-L79] [src/digitalmodel/hydrodynamics/seakeeping.py:L208-L227] |
| Top-level summary | governing_dof = first minimum operability; governing_operability_pct; response_transfer string; screening_status pass iff every summary passes; same status copied into cfg | [src/digitalmodel/vessel_seakeeping/workflow.py:L82-L103] |
| Output paths / retained inputs | results_csv and summary_csv strings, repository-relative when possible, absolute otherwise; existing files overwritten, directories created. Frequency/RAO/scatter/gamma inputs omitted from replaced result mapping | [src/digitalmodel/vessel_seakeeping/workflow.py:L88-L103] [src/digitalmodel/vessel_seakeeping/workflow.py:L224-L239] |

## 3. Method and governing standards

Public web sources were opened on 2026-10-22. Live web help supplies no numbered
edition; its access date is the inspected version. Standards named in source
comments have not been opened and imply no compliance qualification.

| Method / constant | Equation or algorithm | Source / status |
|---|---|---|
| Response transfer | S_response(omega)=RAO_amplitude² S_wave(omega) | [Orcina, Spectral response analysis, Spectral response calculation details](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Spectralresponseanalysis.htm), RAO=sqrt(response PSD/wave PSD), algebraically rearranged; source calculation differs, only the transfer identity is used. [src/digitalmodel/hydrodynamics/seakeeping.py:L32-L55] |
| modified fully developed spectrum / Bretschneider shape | Q(omega)=(5/16) Hs² omega_p⁴ omega^(−5) exp[−(5/4)(omega_p/omega)⁴], omega_p=2pi/Tp | [Orcina, Waves: Wave spectra, ISSC spectrum](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Waves,Wavespectra.htm), frequency form changed to angular frequency with S_omega d_omega=S_f df. Coefficients 5/16 and 5/4 from that public equation. The package's modified fully developed spectrum path is this two-parameter modified form. [src/digitalmodel/hydrodynamics/wave_spectra.py:L100-L176] |
| JONSWAP shape | modified fully developed spectrum shape × gamma^b, b=exp[−(omega−omega_p)²/(2 sigma² omega_p²)]; sigma switches at peak | Same public page, JONSWAP spectrum; default gamma and two sigma values NOT_EVALUATED because that page treats them as inputs; default values not restated. [src/digitalmodel/hydrodynamics/wave_spectra.py:L48-L98] |
| Gravity in shape construction | g in alpha cancels g² in modified fully developed spectrum/JONSWAP; not present in Bretschneider expression | Fixed g numerical value NOT_EVALUATED, not restated. [src/digitalmodel/hydrodynamics/wave_spectra.py:L27-L29] [src/digitalmodel/hydrodynamics/wave_spectra.py:L82-L98] [src/digitalmodel/hydrodynamics/wave_spectra.py:L133-L139] |
| Discrete spectrum normalisation | target area Hs²/16; scale Q by target/trapezoidal area; return zero spectrum when target/current area nonpositive | Repository algorithm [src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46]. Target follows integration of the public two-parameter spectrum formula over positive frequencies (substitution t=(omega_p/omega)⁴ yields Hs²/16); rescaling a truncated grid is an implementation choice, UNVERIFIED as a physical bandwidth approximation |
| m0 and significant amplitude | m0=integral(S_response d_omega), trapezoidal implementation; significant single amplitude = 2sqrt(m0) by the engine's stipulated convention | [src/digitalmodel/hydrodynamics/seakeeping.py:L58-L105]; physical narrow-band/Rayleigh applicability NOT_EVALUATED; no opened statistical derivation or motion acceptance rule |
| Scatter weighting | operable iff amplitude ≤ criterion; percentage = 100 sum(operable weights)/sum(weights); each DOF evaluated independently | Arithmetic definition [src/digitalmodel/hydrodynamics/seakeeping.py:L208-L231]; criteria and required operability are caller inputs, not prescribed limits |
| Aggregate result | every DOF's percentage ≥ required percentage; governing is minimum DOF | [src/digitalmodel/vessel_seakeeping/workflow.py:L71-L102]; per-DOF occurrence screening, not joint all-DOF sea-state operability |

NOT_EVALUATED: DNV-RP-C205 edition/clause applicability, PNA motions basis,
statistical single-amplitude approximation, defaults and physical criteria. No
value is reproduced from a licensed document or memory.

## 4. Validated worked examples and regression examples

The source-cited two-parameter spectrum formula above, with Tp=2pi s and Hs=2 m, on
omega=[1/2,1,3/2] rad/s gives raw ordinates:

`q0=40 exp(−20)`, `q1=(5/4) exp(−5/4)`,
`q2=(40/243) exp(−20/81)`.

Trapezoidal raw area is `A=q0/4+q1/2+q2/4`. Multiplication by
`(Hs²/16)/A` makes the discrete area exactly `Hs²/16`. A flat unit RAO
leaves that spectrum unchanged; the stipulated amplitude is `2sqrt(Hs²/16)=Hs/2`.
For Hs=4 and 6 m the raw ordinates scale by four and nine; the same cancellation
gives m0=1 and 9/4 m². With JONSWAP gamma explicitly unity the peak multiplier
is unity everywhere. This is a hand-worked discrete-area identity, not evidence
that this sparse grid resolves a physical motion spectrum.

| Example | Inputs → hand answer | Test node ID |
|---|---|---|
| Unit RAO, modified fully developed spectrum | Single state Hs=2 m, Tp=2pi s, grid above, RAO=[1,1,1], criterion 1.5 m, required 90%: m0=1/4 m² → amplitude 1.0 m → operability 100.0%, pass | `tests/vessel_seakeeping/test_workflow.py::test_unit_rao_pm_hs2` |
| Unit RAO, Bretschneider | Same grid/period, Hs=4 m: m0=1 m² → amplitude 2.0 m; criterion 1.5 m → operability 0.0%, fail | `tests/vessel_seakeeping/test_workflow.py::test_unit_rao_bretschneider_hs4` |
| Unit RAO, JONSWAP | Same grid/period, gamma=1, Hs=6 m: m0=9/4 m² → amplitude 3.0 m; criterion 3.5 m → operability 100.0%, pass | `tests/vessel_seakeeping/test_workflow.py::test_unit_rao_jonswap_hs6` |
| Weighted scatter | Hs=2 and 4 m, weights 3 and 1, flat unit RAO, criterion 1.5 m: amplitudes 1 and 2 m, only first operable → 100×3/(3+1)=75.0%; required 90% → fail | `tests/vessel_seakeeping/test_workflow.py::test_scatter_weighting_literal_percentage` |
| Two DOFs | Hs=2 m; flat RAO 1 m/m and 2 deg/m → amplitudes 1 m and 2 deg. Criteria 1.5 in each response unit → 100.0% and 0.0%, aggregate fail | `tests/vessel_seakeeping/test_workflow.py::test_all_dofs_must_pass` |

The test docstring repeats the full area derivation. Criteria, weights, baseline
percentages and RAOs are synthetic test inputs, not sourced acceptance limits.

## 5. QA gates and executable checks

| Gate | Executable check | Limit |
|---|---|---|
| Unit transfer / spectral area | `tests/vessel_seakeeping/test_workflow.py::test_unit_rao_pm_hs2` | Normalised discrete identity |
| Weighted operability | `tests/vessel_seakeeping/test_workflow.py::test_scatter_weighting_literal_percentage` | Independent weighted percentage |
| Aggregate all DOFs | `tests/vessel_seakeeping/test_workflow.py::test_all_dofs_must_pass` | Per-DOF threshold aggregation |
| Missing RAO fails closed | `tests/vessel_seakeeping/test_workflow.py::test_missing_rao_fails_closed` | Raises before writing or returning a pass |
| Nonfinite criterion case | `tests/vessel_seakeeping/test_workflow.py::test_nan_criterion_fails_closed` | NaN only; positive infinity remains an open defect |
| Inventory and evidence | `tests/docs/test_marine_agent_packs.py::test_new_pack_contracts` | Static AST / bounded locators |
| Cited IDs collect | `tests/docs/test_marine_agent_packs.py::test_all_new_pack_node_ids_collect` | All sections and gates |

Run pytest on `tests/vessel_seakeeping/test_workflow.py` and
`tests/hydrodynamics/test_seakeeping.py` with NumPy/SciPy and an external basetemp.
No measured RAO, joint operability or design acceptance is claimed.

## 6. Known failure modes and pitfalls

| Evidence | Defect / limit | Follow-up |
|---|---|---|
| [src/digitalmodel/vessel_seakeeping/workflow.py:L183-L190] | Positive infinity accepted for criterion; finite response can then pass an unusable infinite criterion | Reject all nonfinite physical scalars; NaN refusal alone is insufficient |
| [src/digitalmodel/vessel_seakeeping/workflow.py:L34-L38] | Required percentage has no full-percentage upper bound | Validate percentage range |
| [src/digitalmodel/vessel_seakeeping/workflow.py:L115-L130] [src/digitalmodel/hydrodynamics/seakeeping.py:L185-L209] | Nonuniform RAO grid accepted, spectrum rebuilt uniformly from endpoints/count; amplitudes paired by index without interpolation | Require uniform grid or interpolate consistently |
| [src/digitalmodel/vessel_seakeeping/workflow.py:L123-L130] [src/digitalmodel/vessel_seakeeping/workflow.py:L163-L175] | Frequency and amplitude finiteness not checked; NaN can evade comparison-only guards | Validate every array element |
| [src/digitalmodel/vessel_seakeeping/workflow.py:L133-L149] | Infinite/NaN probabilities not explicitly rejected; positive-infinite hs/tp accepted locally | Validate finite scatter inputs and total |
| [src/digitalmodel/vessel_seakeeping/workflow.py:L32-L33] | Unvalidated gamma may produce invalid spectrum arithmetic | Validate peakedness and source its range |
| [src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46] | Raw spectral area underflow returns zero spectrum, which can yield zero motion and a passing criterion | Refuse unresolved/underflowed spectra |
| [src/digitalmodel/hydrodynamics/wave_spectra.py:L31-L46] | A truncated spectrum is rescaled to full target area, concealing omitted bandwidth | Establish frequency coverage and convergence independently |
| [src/digitalmodel/vessel_seakeeping/workflow.py:L152-L180] | Duplicate DOF names accepted, units are labels only | Require unique names and declared RAO/criterion units |
| [src/digitalmodel/vessel_seakeeping/workflow.py:L71-L102] | Every marginal DOF percentage passing does not establish joint all-DOF sea-state operability | Define whether marginal or joint criterion is intended |
| [src/digitalmodel/vessel_seakeeping/workflow.py:L88-L103] [src/digitalmodel/vessel_seakeeping/workflow.py:L224-L239] | Input provenance lost on replacement; output overwritten; outside-root paths rendered absolute | Retain input manifest and publish sanitised paths |

## 7. Lessons log

A unit RAO provides an exact area identity across all normalised spectra, but
cannot prove spectral shape accuracy. Nonconstant RAOs require matched grids and
bandwidth evidence. Testing NaN refusal must not imply all nonfinite values are
refused: the positive-infinity criterion gap remains explicit.

## 8. Ownership

Maintainers own the public contract and tests. A hydrodynamics reviewer must
qualify RAO frames/units, spectrum/scatter basis, frequency coverage, criteria
and the single-amplitude statistic before engineering acceptance. This draft
appoints no individual and claims no engineering approval.

## 9. Dependencies and other implementations

The router lazily calls the hydrodynamics seakeeping engine, which constructs
WaveSpectra; NumPy, standard-library math/csv/pathlib provide numerical arrays
and I/O. These are dependencies, not public definitions added by the facade.
[src/digitalmodel/vessel_seakeeping/workflow.py:L42-L56]
[src/digitalmodel/hydrodynamics/seakeeping.py:L22-L29]
[src/digitalmodel/hydrodynamics/seakeeping.py:L172-L206]
The library API supports a criteria dictionary but uses its first value; the
router avoids that ambiguity by calling once per DOF with a one-entry dictionary.
[src/digitalmodel/hydrodynamics/seakeeping.py:L172-L174]

## 10. Coverage and open items

The sole public function is inventoried. Seven new module tests cover three
single-state literal amplitudes, weighted scatter, multiple DOFs and two
fail-closed cases. The former absence of a dedicated test folder is addressed.
The existing engine tests remain required. Positive-infinity refusal, grid
alignment, spectral underflow, statistical qualification and joint operability
remain open; no production code is changed to mask these gaps.
