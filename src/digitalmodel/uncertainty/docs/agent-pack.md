# Uncertainty module knowledge pack

| Header | Value |
| --- | --- |
| Package | `src/digitalmodel/uncertainty` |
| Code pin | `3554b14fde67f44b1c52b81b6c72106282eaff29` |
| Revision / status | 0 / DRAFT |
| Analysis / QA family | Monte Carlo numerical harness / EC |

## 1. Purpose and scope

The package samples marginal priors, dispatches an evaluator row by row and
summarises scalar outputs. It is a domain-neutral numeric harness. Sampled
variation does not establish physical model validity, probability of failure or
engineering compliance. This pack documents code, without extending the report
work in pull request #3259.
[src/digitalmodel/uncertainty/__init__.py:L1-L20]

### Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-22 | Record public contracts, statistical anchors and limits at the code pin. |

## 2. Inputs and outputs

All distribution and response units are caller-selected. There is no unit
conversion or dimensional compatibility check. Names are relative to the package.
All public definitions and public methods in its Python files are listed;
`__all__` re-exports these same definitions. Imported library classes and typing
aliases are dependencies, not package-defined functions or classes.
[src/digitalmodel/uncertainty/__init__.py:L22-L34]

| API | Inputs, units and validation | Defaults | Output contract | Evidence |
| --- | --- | --- | --- | --- |
| `distributions.Prior` | Nonblank name; dist uniform, normal, lognormal or triangular; numeric parameters finite and distribution-specific; no extra keys | Optional parameters None; std/std_log/median positive when present | Pydantic prior; uniform low<high; normal mean and std; lognormal exactly one of mean_log/median and std_log; triangular low<=mode<=high and low<high | [src/digitalmodel/uncertainty/distributions.py:L13-L95] |
| `distributions.Prior.from_dict` | Mapping with prior fields; same validation | none | Validated Prior | [src/digitalmodel/uncertainty/distributions.py:L97-L99] |
| `distributions.Prior.frozen` | Validated prior | none | SciPy frozen distribution: uniform loc=low, scale=high-low; normal loc=mean, scale=std; lognormal s=std_log, scale=exp(mean_log) or median; triangular c=(mode-low)/(high-low) | [src/digitalmodel/uncertainty/distributions.py:L101-L111] |
| `distributions.Prior.ppf` | Scalar/array u, dimensionless; quantile range delegated to SciPy | none | Scalar/array in prior units; endpoints may be infinite; invalid quantiles may be NaN | [src/digitalmodel/uncertainty/distributions.py:L113-L115] |
| `doe.sample_lhs` | Nonempty uniquely named priors, n_samples positive, seed explicit | no defaults; SciPy default scrambling | n_samples by number-of-priors DataFrame, named columns in prior order; one point per probability stratum per marginal before inverse CDF | [src/digitalmodel/uncertainty/doe.py:L17-L28] |
| `doe.sample_random` | Same prior and count checks, seed explicit | random state seed+column index | DataFrame of independent marginal draws; no dependence model specified | [src/digitalmodel/uncertainty/doe.py:L31-L41] |
| `evaluator.Evaluator` | Protocol for an object with run(sample) | none | Structural typing contract; no runtime validation until run_matrix | [src/digitalmodel/uncertainty/evaluator.py:L11-L13] |
| `evaluator.Evaluator.run` | Dict of named sample scalars, caller units | implementation supplied by caller | Dict of named scalar outputs in caller units | [src/digitalmodel/uncertainty/evaluator.py:L12-L13] |
| `evaluator.run_matrix` | Callable or object with run; samples DataFrame; output must be mapping of finite Real scalars | sequential evaluation; output keys colliding with inputs prefixed output_ | Inputs plus outputs DataFrame; evaluator exceptions become RuntimeError with row index; invalid output mapping/scalar raises | [src/digitalmodel/uncertainty/evaluator.py:L25-L53] |
| `analytics.tornado` | DataFrame, input column names, output name; each standard deviation nonzero | population std, ddof=0; least-squares rcond=None | DataFrame input, coefficient, abs_rank sorted by absolute standardised regression coefficient; dimensionless | [src/digitalmodel/uncertainty/analytics.py:L16-L29] |
| `analytics.spearman_ranks` | DataFrame and named input/output columns; ties, constant columns and missing data delegated to SciPy | SciPy spearmanr defaults | DataFrame input, coefficient, pvalue; coefficients dimensionless, no significance verdict | [src/digitalmodel/uncertainty/analytics.py:L32-L40] |
| `analytics.percentile_summary` | DataFrame and scalar output; percentile sequence, ranges delegated to NumPy | percentiles=(10,50,90); NumPy linear method | Dict p10/p50/p90 or requested labels to floats in output units | [src/digitalmodel/uncertainty/analytics.py:L43-L52] |
| `analytics.fan` | DataFrame and ordered step column names; percentile sequence | (10,50,90) | DataFrame with step name and percentile columns; step has no inferred physical unit | [src/digitalmodel/uncertainty/analytics.py:L55-L65] |
| `config.DoeConfig` | method lhs/random, n_samples integer >0, seed integer; extra keys forbidden | all required | Validated design specification | [src/digitalmodel/uncertainty/config.py:L21-L26] |
| `config.EvaluatorConfig` | Exactly one nonblank builtin or dotted_path; extra keys forbidden | both None individually; empty combination rejected | Validated dispatch description; does not load callable | [src/digitalmodel/uncertainty/config.py:L29-L39] |
| `config.ScreeningConfig` | Nonblank metric, operator <=/< />=/>, finite threshold in metric units | all required | Validated screening description; does not evaluate threshold | [src/digitalmodel/uncertainty/config.py:L42-L47] |
| `config.OutputsConfig` | Nonblank primary name and optional ScreeningConfig; extra keys forbidden | screening=None | Validated output description | [src/digitalmodel/uncertainty/config.py:L50-L54] |
| `config.UncertaintyStudy` | At least one Prior, DoeConfig, EvaluatorConfig, OutputsConfig; extra keys forbidden | all required | Typed study; duplicate prior names are rejected later by sampler | [src/digitalmodel/uncertainty/config.py:L57-L63] |
| `config.load_study` | YAML path; safe_load then model validation | none | UncertaintyStudy; file/YAML/validation exceptions propagate | [src/digitalmodel/uncertainty/config.py:L66-L70] |

## 3. Methods and sources

Public manual versions below are source editions, not a claim of a dependency
lock. Parameters remain caller inputs; no prior is a standard material value.

| Equation / method | Public source and status | Code evidence |
| --- | --- | --- |
| Uniform inverse CDF low+(high-low)u | [SciPy 1.15.3, uniform, Notes](https://docs.scipy.org/doc/scipy-1.15.3/reference/generated/scipy.stats.uniform.html) | [src/digitalmodel/uncertainty/distributions.py:L103-L104] |
| Normal X=mean+std Z | [SciPy 1.15.3, norm, Notes](https://docs.scipy.org/doc/scipy-1.15.3/reference/generated/scipy.stats.norm.html); std is standard deviation | [src/digitalmodel/uncertainty/distributions.py:L105-L106] |
| Lognormal X=exp(mean_log+std_log Z); median=exp(mean_log) | [SciPy 1.15.3, lognorm, Notes](https://docs.scipy.org/doc/scipy-1.15.3/reference/generated/scipy.stats.lognorm.html); logarithmic quantities must use a consistent reference unit | [src/digitalmodel/uncertainty/distributions.py:L107-L109] |
| Triangular c=(mode-low)/(high-low), loc=low, scale=high-low | [SciPy 1.15.3, triang, Notes](https://docs.scipy.org/doc/scipy-1.15.3/reference/generated/scipy.stats.triang.html) | [src/digitalmodel/uncertainty/distributions.py:L110-L111] |
| One point per [j/n,(j+1)/n), each dimension | [SciPy 1.15.3, LatinHypercube, Notes and References 1](https://docs.scipy.org/doc/scipy-1.15.3/reference/generated/scipy.stats.qmc.LatinHypercube.html), citing *A Comparison of Three Methods for Selecting Values of Input Variables in the Analysis of Output from a Computer Code*, Technometrics (1979). Original-paper text NOT_EVALUATED; property explicitly documented by the public manual | [src/digitalmodel/uncertainty/doe.py:L17-L28] |
| Marginal random variates; seed offset by index | Distribution rvs methods in the four SciPy 1.15.3 references above; seed+index is an implementation reproducibility choice, not proof of statistical independence | [src/digitalmodel/uncertainty/doe.py:L31-L41] |
| z=(x-mean)/sqrt(mean((x-mean)²)); solve min norm(X beta-y) | Arithmetic population moments and [NumPy 2.2, linalg.lstsq, Parameters and Returns](https://numpy.org/doc/2.2/reference/generated/numpy.linalg.lstsq.html); ddof=0 and sequential absolute ranks are implementation choices | [src/digitalmodel/uncertainty/analytics.py:L8-L29] |
| Spearman coefficient = Pearson correlation of ranks; without ties rho=1-6 sum(d²)/(n(n²-1)) | [SciPy 1.15.3, spearmanr, statistic and Notes](https://docs.scipy.org/doc/scipy-1.15.3/reference/generated/scipy.stats.spearmanr.html) and [NumPy 2.2, corrcoef, Notes](https://numpy.org/doc/2.2/reference/generated/numpy.corrcoef.html); historical reference: [*The Proof and Measurement of Association between Two Things* (1904)](https://doi.org/10.2307/1412159). Original text NOT_EVALUATED: public resolver returned a browser challenge. The no-tie identity is derived independently from centered ranks below | [src/digitalmodel/uncertainty/analytics.py:L32-L40] |
| Linear percentile index h=(N-1)q, interpolate adjacent sorted values; fan repeats per step | [NumPy 2.2, percentile, Parameters and Notes](https://numpy.org/doc/2.2/reference/generated/numpy.percentile.html) and [quantile, Notes, default linear method](https://numpy.org/doc/2.2/reference/generated/numpy.quantile.html); P10/P50/P90 are implementation summary choices, not reliability targets | [src/digitalmodel/uncertainty/analytics.py:L43-L65] |
| Screening comparison / dependence modelling | NOT_EVALUATED: config declares operator/threshold but this package has no runner that resolves those settings or correlated-prior method | [src/digitalmodel/uncertainty/config.py:L42-L70] |

## 4. Worked examples

All values are synthetic and unit-neutral; choose one consistent scalar unit
for each input/output. Seeds are explicit reproducibility inputs.

| Inputs / independent method | Hand-calculated answer | Executable evidence |
| --- | --- | --- |
| Uniform [2,6], u=[0,0.25,0.5,0.75,1]; x=2+4u | [2,3,4,5,6] | `tests/uncertainty/test_agent_pack.py::test_uniform_quantiles` |
| Two uniform [0,1] marginals, n=4, seed=7; strata [0,0.25), [0.25,0.5), [0.5,0.75), [0.75,1) | Each column has sorted stratum indices [0,1,2,3], counts [1,1,1,1]; no seed-specific jitter is used as an oracle | `tests/uncertainty/test_agent_pack.py::test_lhs_four_strata` |
| Pairs (1,1),(2,3),(3,2),(4,5),(5,4), no ties; ranks equal values | d=[0,-1,1,-1,1], sum(d²)=4, rho=1-24/120=0.8. Independently, centered ranks x=[-2,-1,0,1,2], y=[-2,0,-1,2,1]; sum(xy)=8 and sum(x²)=sum(y²)=10, hence 8/sqrt(100)=0.8 | `tests/uncertainty/test_agent_pack.py::test_spearman_five_pairs` |
| Sorted a=[0,10,20,30,40], b=a+10; h=4q | h=[0.4,2,3.6], so a P10/P50/P90=[4,20,36]; b=[14,30,46] | `tests/uncertainty/test_agent_pack.py::test_percentiles_and_fan` |
| x=[1,2,3]; evaluator y=2x+1; standardise both | y=[3,5,7]; z_y=z_x, least-squares beta=1, abs_rank=1 | `tests/uncertainty/test_agent_pack.py::test_evaluator_and_tornado` |

For the no-tie identity, sum of squared centered ranks is n(n²-1)/12;
sum(d²)=2 sum(x²)-2 sum(xy). Substitute into rho=sum(xy)/sum(x²)
to obtain the factor 6. This finite-sum identity is an independent derivation,
not a quoted value from the inaccessible original paper. Small-sample p-values
are not established by the coefficient example.

## 5. QA gates

Execute with `python -m pytest` and these node IDs:

- Literal examples: the five section-4 node IDs.
- Unique marginal coverage: `tests/uncertainty/test_agent_pack.py::test_lhs_four_strata`.
- Prior rejects irrelevant parameters: `tests/uncertainty/test_doe.py::test_prior_rejects_distribution_irrelevant_params`.
- Evaluation failure aborts: `tests/uncertainty/test_evaluator.py::test_run_matrix_fails_closed_with_row_index_when_evaluator_raises`.
- Nonfinite/scalar outputs rejected: `tests/uncertainty/test_evaluator.py::test_run_matrix_rejects_non_scalar_or_non_finite_outputs`.
- Exactly one dispatch description: `tests/uncertainty/test_config.py::test_evaluator_config_requires_exactly_one_dispatch_path`.
- Pack names and locators: `tests/docs/test_numeric_agent_packs.py::test_new_pack_contracts`.
- Example and gate collection: `tests/docs/test_numeric_agent_packs.py::test_new_pack_node_ids`.

## 6. Failure modes and boundaries

| Finding | Evidence | Required follow-up |
| --- | --- | --- |
| Quantile endpoints may be infinite for unbounded priors; invalid u is not locally rejected | [src/digitalmodel/uncertainty/distributions.py:L113-L115] | Require explicit quantile-domain policy. |
| Lognormal exp(mean_log) can overflow despite finite input fields | [src/digitalmodel/uncertainty/distributions.py:L107-L109] | Validate representable distribution scales. |
| No correlated priors; seed+index alone is not an independence assessment | [src/digitalmodel/uncertainty/doe.py:L22-L27] [src/digitalmodel/uncertainty/doe.py:L36-L40] | Qualify joint distributions separately. |
| Input cells need not be finite scalar numbers; empty sample matrix loses columns | [src/digitalmodel/uncertainty/evaluator.py:L34-L53] | Validate input matrix contract. |
| output_ collision handling can overwrite an existing input or another output; output schemas can vary by row | [src/digitalmodel/uncertainty/evaluator.py:L44-L53] | Reserve a collision-free output namespace and validate schemas. |
| Boolean output is a Real scalar and is accepted | [src/digitalmodel/uncertainty/evaluator.py:L45-L51] | Declare whether logical outputs are intended. |
| Tornado uses no intercept after centering; collinearity is not rejected and equal magnitudes still get distinct ranks | [src/digitalmodel/uncertainty/analytics.py:L18-L29] | Establish rank/conditioning and tie policy. |
| Constant/NaN data can yield undefined rank coefficients or summaries; no missing-data policy is enforced | [src/digitalmodel/uncertainty/analytics.py:L32-L65] | Validate complete finite columns and report undefined results. |
| Screening and dotted-path fields are descriptions only, not automatic execution or acceptance | [src/digitalmodel/uncertainty/config.py:L29-L70] | Call evaluators explicitly; independently implement any screening policy. |
| Duplicate YAML keys can be replaced before schema validation; study does not reject duplicate prior names | [src/digitalmodel/uncertainty/config.py:L60-L70] | Require strict YAML and cross-field name checks. |

## 7. Ownership and changes

The maintainers review sampler and summary contracts. Callers supply the prior
data basis, units, evaluator validity and acceptance criteria. This pack makes
no change to sampling, dispatch, configuration or numerical implementation.

## 8. Dependencies and operation

SciPy supplies distributions, qmc and rank statistics; NumPy supplies moments,
least squares and percentiles; pandas supplies named matrices. Pydantic validates
the typed front door; PyYAML reads it. `load_study` does not itself run a study.
An explicit program must select `sample_lhs`/`sample_random`, pass the resulting
matrix to `run_matrix`, and request summaries. No solver is required by these
generic functions.
[src/digitalmodel/uncertainty/distributions.py:L3-L8]
[src/digitalmodel/uncertainty/config.py:L3-L16]
[src/digitalmodel/uncertainty/evaluator.py:L16-L22]

## 9. Registry and evidence coverage

The uncertainty routing entry has an agent_pack path and QA family EC in
`docs/registry/module-routing.yaml`. The implementation map's `agent_packs`
index supplies the matching documentation link without adding unreviewed
mathematical-family membership. The AST check covers all six Python files;
the collection check covers every section-4 and section-5 test citation.

## 10. Open items and limits

NOT_EVALUATED: source-data suitability, physical validation, dependence between
priors, convergence in distribution tails, finite-sample confidence intervals,
small-sample rank p-values and screening acceptance. The 1979 and 1904 original
papers were not available as readable original text; the public software
references and explicit arithmetic support the limited statistical anchors.
Separate follow-ups are needed for section-6 validation gaps. The existing
roundtrip test is a synthetic distribution-moment regression, not a qualified
application result.
