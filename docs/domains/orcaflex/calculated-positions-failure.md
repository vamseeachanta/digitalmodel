# Calculated-positions statics failure handling

A calculated-positions analysis succeeds only when both statics solves complete
and pass the existing model-state gate. A failed second solve is recorded as an
error and returns `False` from `run_static_analysis`. The sequential workflow
raises `RuntimeError`, giving an uncaught command invocation a non-zero status.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-22 | Document failure propagation and a reproducible stub example to prevent false success. |

## Method and interface

The sequence is: initial statics, state check, apply calculated positions, save
calculated-position input, second statics, then another state check. Exceptions
from either solve, either state check, or the intervening input save are logged
with the input filename and traceback. The direct method returns `(model, False)`;
`process_fea` and `perform_simulations` stop with an exception on that flag.
Interrupts and process-exit requests propagate unchanged.

The calculated-position input is saved before the second solve. That file may
therefore exist after failure; its presence does not establish convergence.
Dynamics, final `.sim` and `.dat` saves, target iteration, and the aggregate
configuration save are not reached after failure. The sequential workflow stops
at the first failed input; later inputs are not attempted.

The success interface remains `(model, True)` after both solves complete. The
existing overwrite and calculated-position options are retained.

## Worked example

This example is a software event trace with synthetic solver outcomes. It makes
no engineering convergence claim and introduces no physical constants or limits.

Inputs:

```yaml
file_management:
  input_files:
    yml: [case.yml]
orcaflex:
  analysis:
    static: true
    simulation: true
    save_sim: true
    save_dat: true
    iterate:
      flag: false
    UseCalculatedPositions_cfg:
      flag: false
      clean_StaleCalculatedPositions: false
  iterate:
    overwrite_data: false
    UseCalculatedPositions:
      SetLinesToUserSpecifiedStartingShape: false
      UseStaticLineEndOrientations: false
```

The stub returns `InStaticState` from the initial solve, then raises
`RuntimeError("second solve failed")` on the calculated-positions solve.
The independent hand trace is:

| Step | Event | Consequence |
| --- | --- | --- |
| Load | Read `case.yml` | Initial solve is attempted. |
| Initial solve | Return `InStaticState` | State gate passes. |
| Positions | Call `UseCalculatedPositions()` | Calculated input is prepared. |
| Input save | Save `case_calculated_all.yml` | Input exists before the second solve. |
| Second solve | Raise `second solve failed` | No success flag can be set. |
| Report | Log the exception and return `(model, False)` | Sequential workflow raises `Static analysis failed for case.yml`. |

Answer: unsuccessful statics, one error record containing the original failure,
and a non-zero status when the workflow exception is uncaught. There are no
dynamics or final result-save calls. If the second call instead returns
`InStaticState`, the result is `(model, True)` and the configured dynamics and
final saves proceed. A second call returning `Reset` is also rejected by the
existing state gate even if the call raises no exception.

Executable cases, including a subprocess exit check:

```bash
uv run --no-project --with pytest --with loguru --with numpy --with scipy \
  --with pydantic --with pyyaml python -m pytest \
  tests/solvers/orcaflex/test_calculated_positions_failure.py \
  -q -rA -p no:cacheprovider
```

The tests load the complete analysis module with import-only dependency stubs and
a scripted solver stub. The real state gate and workflow methods are exercised;
the native solver binding is neither imported nor called.

## Sources and limits

- Issue #2548 defines the failed-second-solve regression and recorded-error requirement.
- `src/digitalmodel/solvers/orcaflex/orcaflex_custom_analysis.py` defines the
  two-solve sequence and the existing direct-call success-flag interface.
- `src/digitalmodel/solvers/orcaflex/run_state.py` defines the existing state
  gate; its acceptance states are unchanged.
- The independent expected event sequences are written in
  `tests/solvers/orcaflex/test_calculated_positions_failure.py` and above.

No engineering equations, coefficients, numeric acceptance limits, or published
engineering example values are used. Native convergence and separate parallel
execution paths are outside this example's evidence. A caller that catches the
workflow exception controls its own exit status; direct callers must inspect the
returned flag before using results.
