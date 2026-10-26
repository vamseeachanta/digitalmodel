# Cathodic protection analysis — integrated surveys

The `ECDA` calculation combines CIS, DCVG, pipeline current mapping (PCM) and
ACVG observations by pipeline chainage. It produces an assessment list with
the evidence and decision rule for each location, plus an interactive chart.
The worked example uses only synthetic data. It predicts neither metal loss
nor remaining strength and does not authorize excavation.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-26 | Initial configurable survey correlation and example for assessment planning. |

## Inputs and demonstration defaults

Every threshold below is an input: **user to confirm against the governing code**.
The defaults demonstrate the software and carry no claim of code compliance.
Use metres for chainage and a common registered chainage datum across surveys.
Non-finite values and negative chainages are rejected.

| Input under `inputs.ecda.criteria` | Default | Meaning |
| --- | --- | --- |
| `alignment_tolerance_m` | 5 m | Maximum inclusive span of one aligned group. |
| `cis_potential` | `off` | Use OFF potential; missing OFF stays unknown. Select `on` explicitly to use ON. |
| `cis_protection_criterion_V` | -0.85 V vs CSE | Less negative readings are positive anomaly evidence. |
| `cis_overprotection_limit_V` | -1.2 V vs CSE | Equal or more negative readings are also anomaly evidence. |
| `dcvg_min_ir_percentage` | 35 % | IR percentage at or above this input is positive evidence. |
| `pcm.loss_unit` | `dB/m` | Signed loss per metre; `mA/m` is also accepted. |
| `pcm.loss_threshold` | 0.1 dB/m | Loss at or above this input flags a segment. Confirm units when changing the loss unit. |
| `acvg.unit` | `relative` | Instrument-relative readings or `dB_uV` (dB microvolts). No conversion is inferred. |
| `acvg.moderate_min` | 20 | Inclusive lower edge of moderate severity in the declared unit. |
| `acvg.severe_min` | 40 | Inclusive lower edge of severe severity in the declared unit. |
| `acvg.very_severe_min` | 60 | Inclusive lower edge of very severe severity in the declared unit. |
| `acvg_min_severity` | `severe` | Minimum ACVG severity contributing positive evidence. |
| `high_priority_min_surveys` | 2 | Required number of distinct positive survey types, from 2 through 4. |
| `require_cis_for_high` | `true` | Require positive CIS evidence for high priority. |

CIS and DCVG use the existing `CISSurveyPoint` and `DCVGIndication` fields.
DCVG correlation uses the supplied IR percentage and the configured threshold;
it does not inherit a prefilled severity label. PCM readings have `distance_m`
and `signal_current_mA`. ACVG has `distance_m`, `reading`, `unit`, and optional
`direction` (`upstream` or `downstream`). ACVG direction is evidence only;
it does not change the severity. Values below `moderate_min` are `minor`;
the other classes are `moderate`, `severe` and `very_severe`. Bands must
strictly increase, and each ACVG reading must match the configured unit.

## PCM calculation and alignment

PCM sorts readings by ascending chainage and rejects duplicate chainages and
zero or negative currents. A segment from currents `I_start` to `I_end` over
length `L` has signed amplitude loss:

- `dB/m`: `20 * (log10(I_start) - log10(I_end)) / L`.
- `mA/m`: `(I_start - I_end) / L`.

A current gain stays negative and does not flag a non-negative loss threshold.
The interpretation assumes a transmitter arrangement where current decays along
increasing chainage. Check survey direction, branches and transmitter position
before interpreting a segment. Zero or one reading provides no segment evidence.
Each segment is located at its midpoint for correlation; both endpoints,
currents, computed loss, threshold and flag remain in the evidence. A long segment
does not establish the position of a localized defect along its entire length.

All CIS points, DCVG indications, PCM segment midpoints and ACVG indications are
sorted together. The earliest unused observation anchors a group. Observations
within the inclusive alignment tolerance of that anchor join it; the group never
extends transitively. At a tolerance of 5 m, observations at 0 and 5 m align;
an observation at 10 m starts a new group. Each observation is used once. A normal
observation may anchor the group. The exported offset records the difference
between each observation and the group's chainage. Multiple observations of one
survey remain visible but contribute at most one positive vote.

This bounded grouping can separate nearby observations on opposite sides of a
group boundary. Confirm registration and tolerance against survey resolution.
Absence of an indication does not prove sound coating. No interpolation, nearest
sample extrapolation, missing-as-normal substitution or coverage inference occurs.

## Decision rules and ranking

Only groups containing positive evidence become anomalies. Rules run in this order:

1. `cis_disagrees`: any protected CIS evidence in a group with positive evidence
   produces **conflicting signals**, including mixed protected/anomalous CIS.
2. `corroborated_anomaly`: enough distinct positive survey types, satisfying the
   configured CIS requirement, produces **high priority**.
3. `isolated_anomaly`: every other positive group requires **further assessment**.

Ranking follows high priority, further assessment, then conflicting signals.
Within each class, more positive survey types rank first, followed by earlier
chainage. Missing OFF potentials are unknown and cannot create a protection
conflict. Conflicting locations still require investigation; their rank expresses
the demonstration criteria rather than an assurance of lower physical risk.

## Worked example and YAML workflow

The input is [ecda_synthetic.yml](../../../examples/cathodic_protection/ecda_synthetic.yml).
It uses `basename: cathodic_protection` and `inputs.calculation_type: ECDA`
through the existing engine adapter. A direct adapter invocation also avoids
unrelated engine initialization:

```python
from pathlib import Path
import yaml
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

cfg = yaml.safe_load(Path("examples/cathodic_protection/ecda_synthetic.yml").read_text())
cfg["Analysis"] = {"result_folder": "results/ecda"}
result = run_cathodic_protection(cfg)["results"]
```

The result folder is optional. When supplied, the workflow writes
`ecda_dig_list.csv` and a self-contained `ecda_chainage.html`. Without it,
results remain in memory. `results.ecda` contains serializable criteria,
survey evidence and anomalies. The status is `ASSESSMENT` with
`engineering-validation-required`, since completion is not a design PASS.

| Dig rank | Chainage [m] | Class | Evidence and rule |
| --- | --- | --- | --- |
| 1 | 10.0 | high priority | All four surveys contribute positive evidence; `corroborated_anomaly`. |
| 2 | 200.0 | further assessment | Isolated CIS anomaly; `isolated_anomaly`. |
| 3 | 300.0 | further assessment | Isolated ACVG indication; `isolated_anomaly`. |
| 4 | 100.0 | conflicting signals | Protected CIS with DCVG and ACVG indications; `cis_disagrees`. |

The first PCM segment loses 2.0 dB/m over 10.0 m. Its midpoint is 12.0 m,
within the example's 5.0 m alignment tolerance of the 10.0 m anchor.
The chart stacks all four raw surveys on a shared chainage axis, shows the PCM
segment loss on a second vertical axis, and marks anomalies with colored guides
and dig-rank annotations. Hover retains the survey evidence; pan and zoom follow
the shared chainage axis. The HTML includes a summary, revision history,
criteria table, ranked dig evidence and way forward.

For Python use, `correlate_surveys(...)` accepts the four model lists and an
`ECDAConfig`. `ECDAResult.to_dataframe()` and `.to_csv(path)` produce the same
ranked columns. Survey evidence and criteria use JSON cells so repeated
observations and missing values survive CSV export. Empty inputs return an empty
DataFrame with stable columns and an empty four-panel chart. Raw CSV evidence
retains numerical precision for auditing.

## Checks and way forward

Synthetic tests cover PCM formulas, threshold equality, ACVG units and severity
boundaries, configurable rules, unknown readings, non-transitive grouping,
duplicate survey votes, deterministic ranking, CSV evidence and YAML dispatch.
The optional phrase check reads `ECDA_POST_DENYLIST`; when unset it skips.
It reports only a text index on a match and never copies phrases into output.

Before field use, the maintainer should confirm the governing criteria, survey
datum, instrument units, transmitter arrangement and completeness of the evidence,
then review the dig list and investigate conflicting signals.
