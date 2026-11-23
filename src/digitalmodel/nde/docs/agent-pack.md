# NDE module knowledge pack

| Header | Value |
|---|---|
| Package | `src/digitalmodel/nde` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` (`origin/main` at preparation) |
| Revision / status | 2 / DRAFT; engineering qualification UNVERIFIED |
| Analysis family | EC (engineering calculation; proposed for these APIs) |

Pinned knowledge for isolated sampled-pulse timing and the optional acoustic-scattering loader. Synthetic receiver traces test timing arithmetic; mocked dataset items establish wrapper behavior only. No ultrasonic inspection qualification or real dataset execution is claimed.

### Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-22 | Record pinned interfaces, independent arithmetic, executable gates and evidence gaps for review. |
| 2 | 2026-10-25 | Correct the numeric-literal ledger to describe pulse timing and bookkeeping without an unrelated IPR qualification. |

## 1. Purpose and scope

Includes pulse_timing and well_acoustic/acoustic_scattering_loader and their package exports. A sampled unique peak is not a first-arrival picker. The loader does not solve a wave equation or convert dataset frames into a receiver trace. No material wave speed, path length, detection threshold or flaw-acceptance limit is supplied.

## 2. Inputs and outputs

Locally defined public classes/functions/properties and literal `__all__` exports are inventoried below. Constructor defaults and required arguments are preserved; backend imports are dependencies, not locally owned functions.

| Public symbol | Inputs and defaults (omitted default means required) | Units and validation | Output contract | Code-pin evidence |
|---|---|---|---|---|
| `pulse_timing.pulse_peak_time` | `time_s: Sequence[float], pressure: Sequence[float]` | Seconds relative to caller emission reference; pressure in any consistent signed unit. Equal nonempty finite arrays; times strictly increasing; nonzero unique absolute maximum required, else ValueError. | float timestamp in seconds of unique absolute sample peak; sample resolution only | [src/digitalmodel/nde/pulse_timing.py:L9-L51] |
| `well_acoustic.__init__.AcousticScatteringLoader` | Re-export; defining-symbol contract in this table (constants below) | No extra validation/defaults/unit conversion | Same object as imported definition | [src/digitalmodel/nde/well_acoustic/__init__.py:L31-L35] |
| `well_acoustic.__init__.WELL_AVAILABLE` | Re-export; defining-symbol contract in this table (constants below) | No extra validation/defaults/unit conversion | Same object as imported definition | [src/digitalmodel/nde/well_acoustic/__init__.py:L31-L35] |
| `well_acoustic.__init__.THE_WELL_ATTRIBUTION` | Re-export; defining-symbol contract in this table (constants below) | No extra validation/defaults/unit conversion | Same object as imported definition | [src/digitalmodel/nde/well_acoustic/__init__.py:L31-L35] |
| `well_acoustic.acoustic_scattering_loader.AcousticScatteringLoader` | `self, variant: str='basic'` | Variant string and requested item count, no physical units inferred. Constructor checks variant membership; stream_sample checks dependency availability only. n_steps type/sign not validated. | service instance; public operations listed separately below | [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L76-L149] |
| `well_acoustic.acoustic_scattering_loader.AcousticScatteringLoader.stream_sample` | `self, n_steps: int=10` | Variant string and requested item count, no physical units inferred. Constructor checks variant membership; stream_sample checks dependency availability only. n_steps type/sign not validated. | dict: frames list (unmodified backend items), variant, dataset_name, n_steps actual count, attribution; RuntimeError if unavailable | [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L106-L149] |

| Constant or result field | Contract / default / units / validation | Evidence |
|---|---|---|
| WELL_AVAILABLE | Boolean from optional import; False on ImportError only. Backend execution can still fail when True. | [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L41-L47] |
| THE_WELL_ATTRIBUTION | String preserving The Well dataset attribution, CC BY 4.0 and the public source link; retained in returned sample. | [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L54-L57] |
| VARIANT_DATASET_MAP / VALID_VARIANTS | basic -> acoustic_scattering; maze -> acoustic_scattering_maze; inclusions -> acoustic_scattering_inclusions; VALID_VARIANTS is a frozen set of keys. Mutable mapping has no runtime integrity check. | [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L64-L70] |
| stream_sample result | frames is a list of backend iteration items, no shape/units/schema check; n_steps is actual list length, which can be below request on exhausted data. variant and dataset_name are strings, attribution is preserved. No receiver timestamp is inferred. | [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L132-L149] |

The exported backend `WellDataset` name in the implementation module is an optional third-party alias (or None), not a locally defined class. No claim about its signature is made by the local function inventory.

## 3. Methods and public sources

Public pages opened 2026-10-22; records retained externally with address, time and HTTP status. [OpenStax, *College Physics 2e* (2022), §2.3, Eq. 2.5](https://openstax.org/books/college-physics-2e/pages/2-3-time-velocity-and-speed) gives elapsed time as final minus initial time and constant-speed travel as distance divided by speed. [The Well upstream README, usage and streaming sections](https://github.com/PolymathicAI/the_well/blob/master/README.md) and [API, Dataset section](https://github.com/PolymathicAI/the_well/blob/master/docs/api.md) document a map-style dataset with configuration for storage and dataset identity. [Acoustic-scattering maze dataset card](https://huggingface.co/datasets/polymathic-ai/acoustic_scattering_maze/blob/main/README.md) declares CC BY 4.0; [upstream maze description, Equation and About the data](https://github.com/PolymathicAI/the_well/blob/master/datasets/acoustic_scattering_maze/README.md) describes simulated pressure/velocity fields and material properties. These mutable upstream pages are access-date evidence, not a pinned backend compatibility guarantee. The upstream software LICENSE is BSD-3-Clause, distinct from dataset licensing; this pack redistributes no dataset data.

| Operation | Method / equation / constants | Qualification |
|---|---|---|
| pulse_peak_time | argmax(abs(pressure)) indexes the supplied timestamps; nonzero and unique maximum required | Exact sampled convention in code; no empirical coefficient. Travel-time comparison t_peak=t_emit+distance/speed uses OpenStax §2.3. Real receiver/noise/echo applicability UNVERIFIED. |
| stream_sample | Construct backend for selected dataset and train split; append iteration items until length ≥ requested count or iterator exhaustion | Wrapper bookkeeping; default 10 is a count setting, NOT_EVALUATED as a physically meaningful sampling choice. No time interpolation, wave propagation or data download qualification. |
| dataset PDE | Public maze source describes variable-coefficient acoustic pressure/velocity evolution | NOT_EVALUATED by this wrapper: no PDE solver is implemented and no dataset timescale is converted to SI. Caller must establish a receiver/channel and physical units. |
| backend compatibility | Code uses dataset_name and split; current public README uses well_base_path, well_dataset_name and well_split_name | UNVERIFIED installed-version compatibility; upstream API mismatch is an open integration defect, not tested by mocks. |
| attribution / variants | Code label CC BY 4.0 agrees with the opened maze dataset card | Data provenance for basic/inclusions is NOT_EVALUATED independently; public upstream lists acoustic_scattering_discontinuous rather than the basic mapping name. Do not silently rename the requested dataset. |

### Implementation numeric-literal ledger

This ledger records implementation values, not accepted engineering limits. The constant-speed pulse-timing relation is sourced above; its worked-example inputs remain synthetic fixture choices. The literals `0` and `1` support trace validation and indexing; the default `10` is a loader item count, NOT_EVALUATED as a physically meaningful sampling choice. Every use is located below; no external source is inferred from a variable name.

| File | Literal and source line(s) | Status |
|---|---|---|
| `src/digitalmodel/nde/pulse_timing.py` | `0` [src/digitalmodel/nde/pulse_timing.py:L47-L47]; `1` [src/digitalmodel/nde/pulse_timing.py:L42-L42], [src/digitalmodel/nde/pulse_timing.py:L49-L49] | NOT_EVALUATED unless sourced above; implementation evidence only |
| `src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py` | `10` [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L106-L106] | NOT_EVALUATED unless sourced above; implementation evidence only |

## 4. Worked examples with independent expected values

These are synthetic nondispersive isolated pulses with an exactly sampled unique absolute peak. Distances and speeds are fixture choices, not material constants. The public constant-speed relation determines expected time; arbitrary sample amplitudes only identify that peak.

| Inputs | Hand method and answer | Executable test node ID | Limit |
|---|---|---|---|
| distance=6 m, speed=2 m/s, emission=0 s; times [0,1,2,3,4] s, pressure [0,1,2,4,1] in arbitrary consistent units | 0+6/2=3 s; largest magnitude 4 occurs at t=3 s. | `tests/nde/test_agent_pack_values.py::test_peak_travel_time_literal` | Timing arithmetic; no inspection benchmark. |
| distance=6 m, speed=2 m/s, emission=1 s; times [1,1.5,3,4,5.5] s, pressure [0,1,2,4,1] | 1+6/2=4 s; irregular sampling must retain timestamps rather than return index. | `tests/nde/test_agent_pack_values.py::test_peak_emission_offset_literal` | Caller supplies emission reference. |
| distance=4 m, speed=2 m/s, emission=0 s; times [0,1,2,3] s, pressure [0,2,-4,1] | 4/2=2 s; absolute magnitude of -4 exceeds 2, so negative polarity retains t=2 s. | `tests/nde/test_agent_pack_values.py::test_negative_peak_literal` | Absolute peak, not largest signed value. |

## 5. QA gates as executable checks

| Gate | Test node ID | Evidence limit |
|---|---|---|
| Synthetic timing | `tests/nde/test_agent_pack_values.py::test_peak_travel_time_literal`; `tests/nde/test_agent_pack_values.py::test_peak_emission_offset_literal`; `tests/nde/test_agent_pack_values.py::test_negative_peak_literal` | Literal seconds from section 4. |
| Invalid traces | `tests/nde/test_pulse_timing.py::test_invalid_or_ambiguous_trace_rejected` | Empty/mismatched/nonfinite/unordered/zero/tied traces raise. |
| Mocked stream | `tests/nde/test_nde.py::TestStreamSample::test_stream_sample_mocked` | Result structure and item cap only; no live backend API test. |
| Public inventory | `tests/docs/test_production_nde_agent_packs.py::test_pack_public_functions_and_locators[nde]` | Local public names and symbol-specific locators. |
| Negative check | `tests/docs/test_production_nde_agent_packs.py::test_removed_public_function_is_detected[nde]`; `tests/docs/test_production_nde_agent_packs.py::test_out_of_bounds_and_wrong_symbol_locators_are_detected` | Removed function / invalid locator must be detected. |
| Registry | `tests/docs/test_production_nde_agent_packs.py::test_registry_pack_paths_and_ten_sections` | Existing paths and ten ordered sections. |

Collection includes sections 4 and 5 through `tests/docs/test_production_nde_agent_packs.py::test_pack_cited_nodes_collect[nde]`. Reproduce domain tests with `uv run --no-project --with pytest --with pytest-asyncio python -m pytest tests/nde -q -rA -p no:cacheprovider --basetemp=<scratch>/bt-nde`. No backend installation, dataset download or licensed solver is required by these tests.

## 6. Failure modes and known defects

| File and code-pin lines | Actual failure mode / implication | Follow-up |
|---|---|---|
| [src/digitalmodel/nde/pulse_timing.py:L9-L51] | Unique sampled peak need not be first arrival; caller must isolate and baseline-correct pulse. Negative origins and single samples are accepted. No speed/distance/schema is inferred. | Qualify receiver extraction, units, noise and pulse window independently. |
| [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L41-L47] | ImportError marks dependency unavailable; other import failures propagate. Successful import proves neither dataset availability nor compatible API. | Add separately scoped versioned integration evidence. |
| [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L64-L70] | Basic maps to acoustic_scattering, whereas opened upstream inventory uses acoustic_scattering_discontinuous. | Confirm supported data release and canonical names. |
| [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L132-L149] | Constructor keyword names differ from public current API; storage location omitted; actual dataset iteration items may be windows rather than individual frames. | Test a pinned public backend version and explicitly define item schema. |
| [src/digitalmodel/nde/well_acoustic/acoustic_scattering_loader.py:L135-L140] | n_steps≤0 still appends one item from nonempty data; noninteger/type values not validated. Exhaustion silently returns fewer items. | Validate requested count and define truncation/exhaustion semantics. |

## 7. Ownership and review

Package maintainers own timing and loader contracts. A reviewer must establish data-release identity, dataset attribution, physical units, receiver extraction and an inspection-specific reference before results support an NDE decision. This pack does not assign individuals or claim inspection approval.

## 8. Dependencies and boundaries

pulse_timing uses the standard library only. Loader import is optional on the_well; stream_sample requires it and may access data through that backend. Tests mock the dependency; no large optional backend is installed or executed here. CC BY 4.0 attribution must accompany dataset derivatives; software licensing is separate. Do not interpret backend arrays as seconds or dimensional pressures without dataset metadata.

## 9. Coverage and limitations

Existing tests cover package imports, variants, attribution, missing dependency, mocked iteration and pulse validation. New literal tests cover travel time, emission offset and negative polarity. New structural tests cover public definitions/re-exports, locator bounds, row deletion, test citation collection and registry entries. Real dataset iteration, units, simulation validity and field inspection accuracy remain UNVERIFIED. A mocked stream is not a real-data validation fixture.

## 10. Open items and way forward

- Pin and exercise a supported backend release with an openly available sample; correct the constructor/name/schema mismatch in separate code work.
- Require positive integer counts or explicitly define nonpositive/exhaustion behavior; retain source attribution in derived material.
- Verify basic/inclusions dataset provenance and release mappings; only maze licensing was independently checked here.
- Establish receiver extraction, dimensional scaling, baseline treatment and physical pulse reference before using peak timing with a dataset or real signal.
- Consolidate the AST fallback with the shared checker when available; keep unverified integration evidence visible.
