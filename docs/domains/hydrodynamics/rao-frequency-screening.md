# Diffraction RAO frequency screening

Raw RAO rows can be annotated before publication with an explicit, dataset-specific
lower angular-frequency bound. Values, phases, headings, order and row count are
retained. This screen neither repairs diffraction results nor establishes their
physical validity. Its motivating criterion is the Recommendation section of
[issue 1548](https://github.com/vamseeachanta/digitalmodel/issues/1548) (2026).

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Document the offline frequency screen so suspect native rows remain visible with their criterion and source. |

## Method and inputs

The input CSV requires a positive finite angular-frequency column in rad/s.
Its default name is `frequency_rad_s`; `--frequency-column` selects another name.
Hz and period columns require an explicit conversion upstream. No implicit unit
conversion, amplitude clipping or heading exception is applied.

The lower bound and its source are supplied together. There is no default bound:

- `frequency < minimum_frequency_rad_s`: `low_freq_artifact`.
- `frequency >= minimum_frequency_rad_s`: `screening_range`.
- No supplied policy: `NOT_EVALUATED`.

These comparisons implement the case-specific range recommended in issue 1548
(2026), Recommendation section. They introduce no empirical coefficients or
amplitude acceptance limits. `screening_range` means only that the row meets the
supplied frequency bound; mesh convergence, units, damping and cross-solver
agreement remain separate checks. The issue's bound shall not be applied to an
unrelated dataset without an independently documented basis.

Every output row carries `frequency_screening_status`, `frequency_screening_basis`
and `minimum_screening_frequency_rad_s`. The `reliability` and `reliability_basis`
columns retain earlier findings and sources; new defect labels are appended with
semicolons without duplication. An in-range screen never clears an earlier flag.
The dedicated frequency columns describe the current screen even when an older
finding remains. Consumers shall inspect the dedicated status and retained
findings together; no row is qualified for engineering use by this screen alone.

## Worked example

For a dataset to which the documented criterion applies, the lower bound is
**0.5 rad/s** (issue 1548, 2026, Recommendation). The following amplitudes and
headings are synthetic inputs solely to demonstrate preservation; they are not
measured data or qualified solver results:

```csv
frequency_rad_s,heading_deg,heave,sway
0.35,180,8.0,4.0
0.5,90,1.0,0.0
0.8,0,0.8,0.0
```

Run from the repository with the package available:

```sh
python -m digitalmodel.hydrodynamics.rao_tabulation.reliability \
  input.csv annotated.csv \
  --minimum-frequency-rad-s 0.5 \
  --basis 'https://github.com/vamseeachanta/digitalmodel/issues/1548'
```

Independent hand comparison gives:

| Frequency (rad/s) | Comparison with bound (rad/s) | Frequency screening status |
| --- | --- | --- |
| 0.35 | 0.35 < 0.5 | `low_freq_artifact` |
| 0.5 | 0.5 = 0.5 | `screening_range` |
| 0.8 | 0.8 > 0.5 | `screening_range` |

Table 1. Expected frequency labels; all input values remain unchanged.

The executable tests repeat these direct comparisons independently of the
implementation. Invalid frequencies, incomplete rows and malformed headers are
rejected before an output file is created. An existing output is never replaced,
including when the output aliases the input. Without a policy, the CLI still
retains the rows and explicitly marks the frequency check `NOT_EVALUATED`.

## Scope and remaining evidence

This tool annotates native sample rows; it does not annotate interpolated data.
Consumers interpolating RAOs shall propagate flags from every contributing
sample. A query above the bound may still depend on a flagged sample below it;
classifying only the query frequency would conceal that dependency. No existing
publisher or operability consumer is automatically wired to this tool.

The numerical cause reported in the issue remains unconfirmed. A resolved
low-frequency solution requires reviewed mesh-convergence evidence, appropriate
irregular-frequency treatment and an independently checked frequency grid.
Solver execution, replacement of published datasets and certification of the
remaining frequency range are not established by the offline tests.
