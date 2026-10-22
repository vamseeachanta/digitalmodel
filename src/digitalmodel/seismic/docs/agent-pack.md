# Seismic module knowledge pack

| Header | Value |
| --- | --- |
| Package | `src/digitalmodel/seismic` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Revision / status | 0 / DRAFT |
| Analysis / QA family | Acceleration preprocessing / EC |

## 1. Purpose and scope

The package reads one acceleration component, removes a baseline, optionally
filters it, integrates acceleration twice and renders the resulting histories.
It is a domain-neutral numeric tool. It provides no seismic design qualification
or code-compliance verdict. Despite the `response_spectrum` name, there is no
oscillator equation solver or response spectrum in this code pin.
[src/digitalmodel/seismic/response_spectrum/calculations.py:L1-L6]

### Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-22 | Record the pinned code contract and independent arithmetic anchors. |

The report work associated with pull request #3254 is outside this pack.
Evidence locators refer to the code pin; new tests are the reviewable evidence
added with this pack. Public definitions in all Python files, including public
methods and properties, are listed below. Re-exports in the package and reader
`__all__` refer to the same definitions; the workflow export is `router`.
[src/digitalmodel/seismic/__init__.py:L3-L5]
[src/digitalmodel/seismic/readers/__init__.py:L3-L5]
[src/digitalmodel/seismic/response_spectrum/__init__.py:L3-L5]

## 2. Inputs and outputs

Names are qualified relative to the package. Imported third-party objects are
dependencies, not definitions of this package. No constructor validates the
mutable arrays stored inside a frozen dataclass.

| API | Inputs, units and validation | Defaults | Output contract | Evidence |
| --- | --- | --- | --- | --- |
| `baseline.pre_event_mean` | Acceleration array, m/s²; integer samples must be positive and no larger than array size | samples required | Corrected array and removed mean, m/s² | [src/digitalmodel/seismic/baseline.py:L8-L21] |
| `baseline.polynomial_detrend` | Time in s, acceleration in m/s²; equal sizes, order nonnegative, size greater than order | order required | Residual array and descending polynomial coefficients, with powers of seconds in coefficient units | [src/digitalmodel/seismic/baseline.py:L24-L42] |
| `readers.ascii.AccelerogramRecord` | time_s, acceleration_m_s2 arrays; original unit label, source_path and source_digest; constructor has no content checks | all fields required | Frozen record containing mutable arrays; digest is a source-byte identifier | [src/digitalmodel/seismic/readers/ascii.py:L16-L24] |
| `readers.ascii.AccelerogramRecord.npts` | record time array | none | Integer time-array size | [src/digitalmodel/seismic/readers/ascii.py:L26-L28] |
| `readers.ascii.AccelerogramRecord.dt_s` | record time array; at least two samples | none | First time difference, s; uniformity is not rechecked | [src/digitalmodel/seismic/readers/ascii.py:L30-L34] |
| `readers.ascii.AccelerogramRecord.pga_m_s2` | acceleration array | none | Maximum absolute raw acceleration, m/s² | [src/digitalmodel/seismic/readers/ascii.py:L36-L38] |
| `readers.ascii.AccelerogramRecord.pga_g` | raw PGA in SI | implementation G0_M_S2 conversion | Raw PGA divided by standard acceleration of free fall, g | [src/digitalmodel/seismic/readers/ascii.py:L40-L42] |
| `readers.ascii.AccelerogramRecord.acceleration_g` | acceleration array in SI | same conversion | Converted array in g | [src/digitalmodel/seismic/readers/ascii.py:L44-L46] |
| `readers.ascii.read_ascii_accelerogram` | UTF-8 path; one acceleration column plus dt or two time/acceleration columns; uniform positive step, at least two samples, supported unit alias | units=None: explicit argument then header then m_s2; dt_s=None: header for single column; two columns use their time | Record in SI, original normalised unit label, path and SHA-256 of exact bytes; malformed input raises | [src/digitalmodel/seismic/readers/ascii.py:L49-L72] |
| `response_spectrum.calculations.KinematicResult` | record, DataFrame, summary dict, processing-chain list; no constructor validation | method=kinematic_cumtrapz | Frozen container with mutable payloads | [src/digitalmodel/seismic/response_spectrum/calculations.py:L24-L32] |
| `response_spectrum.calculations.compute_kinematics` | Record; baseline dict: none, pre_event_mean or polynomial/poly; filter dict: enabled, band_hz, order | None or empty baseline selects pre_event_mean with min(10,npts) samples; polynomial order 1; filter absent disables; filter order 4 | KinematicResult: six-column time series, original-record summary and two processing steps; zero initial velocity/displacement | [src/digitalmodel/seismic/response_spectrum/calculations.py:L35-L64] |
| `response_spectrum.render.render_report` | KinematicResult; keyword data_source required, may be None but then provenance gate raises; optional path and label | output_path=None; generated_label=None uses fixed label | HTML string when no path, otherwise written Path via report assembler; data_source required for successful render | [src/digitalmodel/seismic/response_spectrum/render.py:L29-L53] |
| `response_spectrum.workflow.router` | Mutable config with response_spectrum.record.file; optional units, dt_s, baseline, filter, output_dir, generated_label | paths relative to config directory or working directory; output_dir=results; stem from config filename then basename then response_spectrum | Writes stem_timeseries.csv and stem_report.html; mutates and returns same cfg, replaces settings with result payload; screening_status=pass means execution completed | [src/digitalmodel/seismic/response_spectrum/workflow.py:L16-L47] |

CSV/DataFrame columns: time_s (s), acceleration_m_s2 (raw),
corrected_acceleration_m_s2, filtered_acceleration_m_s2 (all m/s²), velocity_m_s
(m/s), displacement_m (m). Summary keys are npts, dt_s, duration_s, pga_m_s2,
pga_g. The peaks describe the original record, not the processed history.
[src/digitalmodel/seismic/response_spectrum/calculations.py:L49-L64]
[src/digitalmodel/seismic/response_spectrum/calculations.py:L125-L132]

| Implementation setting | Contract and basis |
| --- | --- |
| G0_M_S2 | 9.80665 m/s²; standard acceleration of free fall, NIST SP 811 (2008), Appendix B.8; defined at [src/digitalmodel/seismic/readers/ascii.py:L13-L13] |
| Uniform-time tolerance | rtol=1e-6, atol=1e-12 s: implementation choices, not sampling acceptance criteria; [src/digitalmodel/seismic/readers/ascii.py:L120-L125] |
| Baseline/filter defaults | min(10,npts), polynomial order 1, filter order 4, cap 0.99 times Nyquist: implementation choices with no qualified engineering basis; [src/digitalmodel/seismic/response_spectrum/calculations.py:L71-L96] [src/digitalmodel/seismic/response_spectrum/calculations.py:L104-L121] |
| CSV formatting | Ten significant digits, an output serialization choice; [src/digitalmodel/seismic/response_spectrum/workflow.py:L78-L80] |

## 3. Methods and sources

Sources below were opened as public documents. Software manual versions specify
the documented algorithms; they do not assert installed dependency versions.

| Method / equation | Source and status | Implementation evidence |
| --- | --- | --- |
| a_SI=a_g G0; a_SI=a_cm/100 | [NIST SP 811, 2008 edition, Appendix B.8, acceleration entries](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8); conversion only | [src/digitalmodel/seismic/readers/ascii.py:L144-L150] |
| offset=sum(a_i)/k; a_corrected=a-offset; PGA=max(abs(a)) | [NumPy 2.2, mean, Notes](https://numpy.org/doc/2.2/reference/generated/numpy.mean.html) and [max, Returns](https://numpy.org/doc/2.2/reference/generated/numpy.max.html); arithmetic independently worked below; baseline window suitability NOT_EVALUATED | [src/digitalmodel/seismic/baseline.py:L15-L21] [src/digitalmodel/seismic/readers/ascii.py:L36-L46] |
| Least-squares polynomial trend and residual | [NumPy 2.2 manual, polyfit, Notes](https://numpy.org/doc/2.2/reference/generated/numpy.polyfit.html); coefficients minimise squared residuals | [src/digitalmodel/seismic/baseline.py:L32-L42] |
| v_i=v_(i-1)+(t_i-t_(i-1))(a_i+a_(i-1))/2; repeat for displacement | [SciPy 1.15.3 manual, cumulative_trapezoid, Parameters and Notes](https://docs.scipy.org/doc/scipy-1.15.3/reference/generated/scipy.integrate.cumulative_trapezoid.html); both initial values set to zero, an implementation assumption | [src/digitalmodel/seismic/response_spectrum/calculations.py:L45-L48] |
| f_s=1/dt; f_N=f_s/2; f_high=min(requested,0.99 f_N) | [SciPy 1.15.3 manual, butter, Parameters Wn/fs](https://docs.scipy.org/doc/scipy-1.15.3/reference/generated/scipy.signal.butter.html); Nyquist definition; cap 0.99 and order 4 are implementation choices; filter-response qualification NOT_EVALUATED | [src/digitalmodel/seismic/response_spectrum/calculations.py:L99-L122] |
| SDOF pulse / spectral acceleration | NOT_EVALUATED: no equation-of-motion integration scheme is implemented; kinematics cannot serve as oscillator spectra | [src/digitalmodel/seismic/response_spectrum/calculations.py:L1-L6] |

## 4. Worked examples

All inputs are synthetic. Expected results are independent arithmetic, not fresh
calls used as their own oracle. Test tolerances cover floating point roundoff.

| Inputs and method | Hand-calculated answer | Executable evidence |
| --- | --- | --- |
| t=[0,1,2] s; a=[0,100,-200] cm/s²; divide by 100 | a=[0,1,-2] m/s²; dt=1 s; npts=3; PGA=2 m/s² | `tests/seismic/test_agent_pack.py::test_centimetre_conversion` |
| a=[2,4,8] m/s²; first two samples form baseline | offset=(2+4)/2=3; corrected=[-1,1,5] m/s² | `tests/seismic/test_agent_pack.py::test_mean_subtraction` |
| t=[0,1,2] s; a=[1,3,5] m/s²; fit order 1 | slope=(5-1)/(2-0)=2; intercept=1; coefficients=[2,1]; residual=[0,0,0] | `tests/seismic/test_agent_pack.py::test_linear_detrend` |
| t=[0,1,2] s; a=[0,2,2] m/s²; baseline none, filter disabled | velocity increments (0+2)/2=1 and (2+2)/2=2: v=[0,1,3] m/s. Displacement increments (0+1)/2=0.5 and (1+3)/2=2: d=[0,0.5,2.5] m | `tests/seismic/test_agent_pack.py::test_two_trapezoids` |

The last result is the double discrete trapezoidal integral. No continuous pulse
response or oscillator value is inferred from it; spectrum values remain
NOT_EVALUATED.

## 5. QA gates

Run `python -m pytest` followed by these node IDs, with the repository source on
the Python import path. These are numeric and contract gates, not design gates.

- Reader rejects nonuniform sampling: `tests/seismic/test_readers.py::test_rejects_nonuniform_two_column_time_step`.
- Single-column dt is required: `tests/seismic/test_readers.py::test_requires_dt_for_single_column_without_header`.
- Empty baseline window is rejected: `tests/seismic/test_baseline.py::test_pre_event_mean_rejects_empty_window`.
- Missing report provenance is rejected: `tests/seismic/test_agent_pack.py::test_render_requires_provenance`.
- Direct workflow writes declared files: `tests/seismic/test_agent_pack.py::test_direct_router_contract`.
- Pack/API and locator agreement: `tests/docs/test_numeric_agent_packs.py::test_new_pack_contracts`.
- All examples and gates collect: `tests/docs/test_numeric_agent_packs.py::test_new_pack_node_ids`.

## 6. Failure modes and boundaries

| Finding | Evidence | Required follow-up |
| --- | --- | --- |
| Missing units silently means m/s²; an empty explicit unit string falls back to header/default | [src/digitalmodel/seismic/readers/ascii.py:L60-L65] | Require callers to record explicit source units. |
| Finite acceleration is unchecked; NaN/Inf can reach peaks, fits and integration | [src/digitalmodel/seismic/readers/ascii.py:L85-L88] [src/digitalmodel/seismic/readers/ascii.py:L144-L150] | Add finite-value validation in separate numerical work. |
| Frozen records do not validate array dimensions, equal lengths or uniform time when constructed directly | [src/digitalmodel/seismic/readers/ascii.py:L16-L38] | Validate direct construction before integration. |
| Baseline defaults may remove actual motion; no pre-event interval is identified | [src/digitalmodel/seismic/response_spectrum/calculations.py:L71-L84] | Require a justified baseline window. |
| Polynomial fit does not check finite values, distinct time or conditioning | [src/digitalmodel/seismic/baseline.py:L32-L42] | Establish numerical-conditioning gates. |
| Missing/empty band disables filtering; requested high edge is silently capped | [src/digitalmodel/seismic/response_spectrum/calculations.py:L104-L121] | Inspect effective band and justify filtering. |
| Zero initial motion is assumed; summary PGA remains raw | [src/digitalmodel/seismic/response_spectrum/calculations.py:L47-L63] [src/digitalmodel/seismic/response_spectrum/calculations.py:L125-L132] | Declare initial conditions and distinguish processed peaks. |
| pass is unconditional after successful file output; settings are replaced | [src/digitalmodel/seismic/response_spectrum/workflow.py:L43-L47] | Never treat status as engineering acceptance; retain original configuration. |
| External output paths can be returned as absolute strings | [src/digitalmodel/seismic/response_spectrum/workflow.py:L111-L116] | Sanitize any subsequently published payload; report source identifier itself is sanitised. |

## 7. Ownership and changes

The maintainers review this code contract and any proposed numerical changes.
Independent method qualification remains required before engineering use. This
pack changes no numerical code, workflow status or acceptance policy.

## 8. Dependencies and operation

NumPy supports arrays/fits; pandas stores histories; SciPy integrates;
`signal_processing.signal_analysis.filters.frequency.FrequencyFilter` supplies
optional filtering. Plotly, the reporting assembler and assumption ledger
render provenance-bearing HTML. No licensed solver is required by the direct
numeric entry points.
[src/digitalmodel/seismic/response_spectrum/calculations.py:L13-L21]
[src/digitalmodel/seismic/response_spectrum/render.py:L9-L24]

Use `read_ascii_accelerogram`, then `compute_kinematics` with explicit settings;
use `router` only when file outputs are wanted. ASCII header keys include units
and dt_s; dt, delta_t and time_step_s alias dt_s. Numeric rows must be consistently
one or two columns.
[src/digitalmodel/seismic/readers/ascii.py:L75-L125]

## 9. Registry and evidence coverage

Routing is registered under seismic with QA family EC in
`docs/registry/module-routing.yaml`; the matching pack link is recorded in the
implementation map's `agent_packs` index. That index is documentation routing,
not a claim of membership in the map's existing physics families.
The AST helper checks all package Python definitions and every evidence locator;
node collection checks sections 4 and 5. General-engine integration is separate
from the direct numeric contract.

## 10. Open items and limits

NOT_EVALUATED: oscillator spectra, pulse response, baseline selection, filter
engineering suitability, initial-state reconstruction, structural acceptance,
field measurement qualification and multi-component combination. Resolve the
section-6 findings in separate changes. Existing module tests remain the broader
regression suite; literal examples establish only the stated arithmetic.
