# Residual module knowledge pack

| Header | Value |
| --- | --- |
| Package | `src/digitalmodel/residual` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Revision / status | 0 / DRAFT |
| Analysis / QA family | Bounded empirical correction / EC |

## 1. Purpose and scope

The package fits a bounded affine estimate from paired measured and predicted
values and provides forecast-skill metrics. It is domain-neutral and empirical;
its fitted parameters and implementation bounds are not standard values. The
consumer must enforce `max(corrected, raw)` when larger demand tightens a verdict:
that verdict policy does not live in this module.
[src/digitalmodel/residual/__init__.py:L3-L17]

### Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-22 | Record empirical fit contracts, independent examples and escalation limits. |

## 2. Inputs and outputs

The table lists every public definition and public method/property in all three
Python files, qualified relative to the package. `__all__` re-exports these
definitions plus constants listed separately. Measured/predicted, bias, noise,
output bounds and RMSE must share caller-selected units; scale and correlations
are dimensionless. No unit conversion is performed.
[src/digitalmodel/residual/__init__.py:L19-L42]

| API | Inputs, units and validation | Defaults | Output contract | Evidence |
| --- | --- | --- | --- | --- |
| `model.Correction` | value float in response units; status and reason strings; constructor does not validate | status=ACTIVE, reason empty | Frozen plain value/status record; never a cited standard value | [src/digitalmodel/residual/model.py:L38-L49] |
| `model.Correction.escalated` | status string | none | True unless status equals active | [src/digitalmodel/residual/model.py:L51-L53] |
| `model.ResidualModel` | scale, bias, lo/hi, n_pairs, skill_before/after, domain_lo/hi, scale_min/max, bias_max, status, note; constructor unchecked | scale=1, bias=0, lo=0, hi=inf, n_pairs=0, skills=NaN, domain=[-inf,inf], implementation bounds, status=escalate:not_fitted, empirical note | Frozen fit/fallback container; use fit rather than direct active construction | [src/digitalmodel/residual/model.py:L56-L80] |
| `model.ResidualModel.active` | model status | none | Boolean status equality with active | [src/digitalmodel/residual/model.py:L82-L84] |
| `model.ResidualModel.identity` | reason string; caller output bounds | reason=not_fitted, lo=0, hi=inf | Nonactive identity fallback; apply returns raw value without clipping | [src/digitalmodel/residual/model.py:L87-L90] |
| `model.ResidualModel.fit` | measured/predicted numeric arrays with equal shapes; removes nonfinite pairs, checks count and nonzero predicted variance; checks fitted bounds | lo=0, hi=inf, min_pairs=8, scale_bounds=(0.5,2), bias_max=1 | Active least-squares model or escalated model; domain is retained predicted min/max; skill fields are training RMSE; shape mismatch raises ValueError | [src/digitalmodel/residual/model.py:L92-L149] |
| `model.ResidualModel.apply` | One prediction, caller response unit | fitted domain and output bounds | Correction: active affine result clipped to lo/hi, or raw value and escalation status if nonactive, nonfinite or outside domain | [src/digitalmodel/residual/model.py:L152-L165] |
| `model.ResidualModel.provenance` | model | none | Plain dict with data_driven/not_a_standard/not_cited flags, fit/bounds/domain/status and training RMSE; no source-data digest | [src/digitalmodel/residual/model.py:L168-L189] |
| `skill.rmse` | Numeric arrays; equal shape required | empty input gives NaN | sqrt(mean squared error), caller units; nonfinite values propagate | [src/digitalmodel/residual/skill.py:L13-L21] |
| `skill.correlation` | Numeric arrays | fewer than two values or constant input gives NaN | Pearson coefficient via NumPy, dimensionless; no explicit shape/finite check | [src/digitalmodel/residual/skill.py:L24-L30] |
| `skill.skill_gain` | RMSE before and after in common unit | none | before-after, same unit; positive is reduction | [src/digitalmodel/residual/skill.py:L33-L35] |
| `skill.is_significant` | before/after RMSE, noise_sigma in same unit; noise must be finite and nonnegative | none | Strict gain>noise_sigma, otherwise False for invalid noise; heuristic, not statistical significance testing | [src/digitalmodel/residual/skill.py:L38-L47] |

| Exported constant | Pinned value | Basis |
| --- | --- | --- |
| MIN_PAIRS | 8 pairs | Implementation choice, not a standard sample-size requirement; [src/digitalmodel/residual/model.py:L24-L29] |
| SCALE_MIN | 0.5 | Implementation choice, not a standard scale limit; [src/digitalmodel/residual/model.py:L24-L29] |
| SCALE_MAX | 2.0 | Implementation choice, not a standard scale limit; [src/digitalmodel/residual/model.py:L24-L29] |
| BIAS_MAX | 1.0 in caller response units | Implementation choice, not a universal physical bound; [src/digitalmodel/residual/model.py:L24-L29] |
| ACTIVE | active | Implementation status string; [src/digitalmodel/residual/model.py:L31-L31] |

## 3. Methods and sources

| Method / equation | Public source or status | Code evidence |
| --- | --- | --- |
| Least squares: minimise sum((m_i-(s p_i+b))²) | [NumPy 2.2 manual, polyfit, Notes](https://numpy.org/doc/2.2/reference/generated/numpy.polyfit.html), polynomial degree 1 | [src/digitalmodel/residual/model.py:L113-L129] |
| corrected=min(max(s p+b,lo),hi) | [NumPy 2.2, clip, Notes](https://numpy.org/doc/2.2/reference/generated/numpy.clip.html) and affine arithmetic; independent worked example below; bounds and 1e-9 floating-edge allowance are implementation choices, engineering suitability NOT_EVALUATED | [src/digitalmodel/residual/model.py:L128-L139] [src/digitalmodel/residual/model.py:L164-L165] |
| RMSE=sqrt(sum((a_i-b_i)²)/n); gain=RMSE_before-RMSE_after | [NumPy 2.2, mean, Notes](https://numpy.org/doc/2.2/reference/generated/numpy.mean.html) for sum/n applied to squared errors, then take the square root; independently derived below. Gain subtraction is an implementation definition; external acceptance calibration NOT_EVALUATED | [src/digitalmodel/residual/skill.py:L13-L21] [src/digitalmodel/residual/skill.py:L33-L35] |
| r=cov(a,b)/sqrt(var(a)var(b)) | [NumPy 2.2 manual, corrcoef, Notes](https://numpy.org/doc/2.2/reference/generated/numpy.corrcoef.html); coefficient only | [src/digitalmodel/residual/skill.py:L24-L30] |
| significant iff gain>noise_sigma | Implementation heuristic; NOT_EVALUATED as a formal statistical test; no confidence level or distributional derivation provided | [src/digitalmodel/residual/skill.py:L38-L47] |
| Insufficient finite pairs, invalid fit, out-of-domain query escalate | Implementation safety policy, not a cited standard; minimum/bounds from Section 2 | [src/digitalmodel/residual/model.py:L113-L149] [src/digitalmodel/residual/model.py:L152-L165] |

There is no standard-based acceptance calculation in this package. Every numeric
bound listed here is read from the pinned implementation, not supplied from
memory. Qualification of physical bounds, data sufficiency and noise floor is
NOT_EVALUATED.

## 4. Worked examples

All examples use synthetic dimensionally consistent scalar values. Numeric
tolerances in tests allow floating point roundoff only.

| Inputs and independent method | Hand-calculated answer | Executable evidence |
| --- | --- | --- |
| p=[1,2,3,4,5,6,7,8], m=[2,3.5,5,6.5,8,9.5,11,12.5]; lo=0, hi=10 | slope=(12.5-2)/(8-1)=1.5; intercept=2-1.5=0.5. Query 4 gives 6.5; query 8 gives min(12.5,10)=10. Query 9 exceeds training maximum 8, so escalates with raw value 9 | `tests/residual/test_agent_pack.py::test_affine_fit_and_clip` |
| a=[1,2], b=[4,5]; before=3, after=1; proportional series [1,2,3] and [2,4,6] | errors [-3,-3], RMSE=sqrt((9+9)/2)=3; gain=3-1=2; noise=2 fails strict comparison, noise=1 passes; Pearson r=1 from proportional centered arrays | `tests/residual/test_agent_pack.py::test_rmse_and_correlation` |
| For every count 0..7, try m=1.5p+0.5; query 4; default minimum 8 | count<8 escalates; apply returns raw 4, never candidate affine answer 6.5 and never active status | `tests/residual/test_agent_pack.py::test_insufficient_pairs_never_corrects` |
| Eight pairs m=3p, p=1..8 | fitted scale 3 exceeds implementation maximum 2; query 4 escalates with raw 4, never affine 12 | `tests/residual/test_agent_pack.py::test_implementation_bounds` |

Escalation returns a `Correction` object holding the raw prediction. It never
returns an active corrected estimate in the insufficient-pair case; the raw
fallback is not evidence of a valid correction.

## 5. QA gates

Execute with `python -m pytest` followed by these node IDs:

- Every insufficient-pair count: `tests/residual/test_agent_pack.py::test_insufficient_pairs_never_corrects`.
- Fit bound escalation: `tests/residual/test_residual_model.py::test_fit_out_of_bounds_escalates_and_applies_identity`.
- Domain gate: `tests/residual/test_residual_model.py::test_out_of_domain_query_escalates`.
- Explicit empirical provenance: `tests/residual/test_residual_model.py::test_provenance_marks_data_driven_not_cited`.
- No citation-layer dependency: `tests/residual/test_residual_model.py::test_residual_package_imports_no_citations`.
- Independent fit, clip and metric anchors: section-4 node IDs.
- AST and locator agreement: `tests/docs/test_numeric_agent_packs.py::test_new_pack_contracts`.
- All examples/gates collect: `tests/docs/test_numeric_agent_packs.py::test_new_pack_node_ids`.

## 6. Failure modes and boundaries

| Finding | Evidence | Required follow-up |
| --- | --- | --- |
| Direct constructor can set active with arbitrary parameters and an unbounded training domain | [src/digitalmodel/residual/model.py:L64-L84] | Use fit; assess constructor validation separately. |
| Nonfinite training pairs are discarded; sufficiently many remaining pairs can still activate | [src/digitalmodel/residual/model.py:L113-L123] | Record rejected counts and define missing-data policy. |
| min_pairs, scale_bounds, bias_max and lo/hi are unchecked caller settings; reversed or nonphysical bounds can reach clipping | [src/digitalmodel/residual/model.py:L98-L103] [src/digitalmodel/residual/model.py:L128-L133] | Validate caller limits, finite ranges and minimum count. |
| Fit is not required to improve training RMSE or exceed a noise floor | [src/digitalmodel/residual/model.py:L127-L149] | Gate improvement in consumers and use independent held-out data. |
| Fallback apply returns raw prediction without clipping; raw NaN/Inf remains nonfinite with escalation | [src/digitalmodel/residual/model.py:L156-L163] | Require status handling before downstream calculations. |
| Only scalar predicted min/max gates the operating domain; no multidimensional operating context is stored | [src/digitalmodel/residual/model.py:L125-L126] [src/digitalmodel/residual/model.py:L161-L163] | Add domain-specific context in the consumer. |
| Provenance reports fit values but does not identify source pairs or their digest | [src/digitalmodel/residual/model.py:L174-L189] | Retain dataset provenance separately. |
| Correlation lacks equal-shape and finite-value validation; empty RMSE yields NaN | [src/digitalmodel/residual/skill.py:L13-L30] | Validate paired numeric arrays and undefined metric states. |
| is_significant validates noise only; negative/nonfinite RMSE inputs are not rejected | [src/digitalmodel/residual/skill.py:L38-L47] | Define valid RMSE inputs; do not interpret the heuristic as a p-value. |
| Output correction can reduce demand; max policy is only a consumer responsibility | [src/digitalmodel/residual/__init__.py:L11-L17] [src/digitalmodel/residual/model.py:L164-L165] | Confirm the consumer applies max(corrected, raw) when larger demand tightens the verdict. |

## 7. Ownership and changes

The maintainers review empirical model contracts. Consumers own physical bounds,
operating-domain qualification and a verdict policy that cannot relax limits. No fit, metric,
constant or consumer policy is changed by this pack.

## 8. Dependencies and operation

NumPy supplies polynomial least squares, clipping and array metrics. Python
dataclasses and math supply the status-bearing records. The package does not
import a physical domain or citation layer; evidence for the latter is an AST
test in section 5. Fit on representative paired data, inspect status/provenance,
then inspect each `Correction.escalated` before using its value. An inactive
model and an out-of-domain query both require escalation handling.
[src/digitalmodel/residual/model.py:L14-L22]
[src/digitalmodel/residual/model.py:L152-L189]

## 9. Registry and evidence coverage

The residual entry in `docs/registry/module-routing.yaml` links this pack with
QA family EC. The implementation map's `agent_packs` index holds a matching
documentation link without claiming membership in unrelated physics families.
The AST helper checks all three source files and their public definitions;
collection checks every test citation in sections 4 and 5.

## 10. Open items and limits

NOT_EVALUATED: physical justification for minimum count and parameter bounds,
multidimensional operating-domain checks, independent generalisation, uncertainty
on fitted parameters, formal forecast-skill significance and consumer verdict
qualification. The implementation constants remain implementation choices.
Section-6 findings require separate numerical or consumer changes; this pack
documents them without widening the model's authority.
