# Effective CFD time-control receipts

Diagnostic edits need evidence that the selected solver consumes the changed
setting. The standalone `effective_time_controls` module preserves supplied
dictionary evidence and checks active readbacks before classifying a trial.
It supports OpenCFD OpenFOAM v2312 `interFoam` with default `localEuler` or `Euler`.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Add effective-setting receipts to reject inactive diagnostic edits. |
| 1 | 2026-10-25 | Require complete active-control evidence in both snapshots to prevent execution claims from incomplete readbacks. |

## Active controls

| Default time scheme | Condition | Consumed control location | Response metric |
| --- | --- | --- | --- |
| `localEuler` | Local stepping | `system/fvSolution/PIMPLE/{maxCo,maxAlphaCo,maxDeltaT}` | `rDeltaT`, inverse seconds |
| `Euler` | `adjustTimeStep yes` | `system/controlDict/{maxCo,maxAlphaCo,maxDeltaT}` | `deltaT`, seconds |
| `Euler` | `adjustTimeStep no` | `system/controlDict/deltaT`; Courant and maximum-step caps are inactive | `deltaT`, seconds |

Table 1. Locations follow the pinned source implementation, rather than dictionary presence.

For local stepping, `maxAlphaCo` is inactive when it is greater than or equal to
`maxCo`. A local `controlDict/maxCo` edit is rejected even if the edited value is
readable there. Global adaptive `deltaT` edits and local pseudo-time `deltaT`
edits are `NOT_EVALUATED`; their interactions are outside this contract.
Omitted active values remain `NOT_EVALUATED` even when the solver has defaults.
Missing or invalid readbacks for any required active control in either snapshot
keep the receipt `NOT_EVALUATED` when no rejection applies, including controls
outside the requested edits. Established inactive controls and the unsupported
`deltaT` cases described above are excluded from this evidence requirement.
The global adjustment switch must be normalized to `yes` or `no` by the caller.

## Evidence collection and use

Import from `digitalmodel.solvers.openfoam.effective_time_controls`. The module
uses only the Python standard library; the package initializer has additional
numerical dependencies.

1. Observe the actual distribution, release and executable and construct a
   `SolverIdentity`. Keep the requested identity separately.
2. Before and after the diagnostic, call `capture_snapshot` with original text
   for `system/controlDict`, `system/fvSchemes` and `system/fvSolution`, plus any
   included fragments that need preserving. Supply resolved leaf readbacks from
   those top-level dictionaries in `resolved`, keyed by full relative paths such
   as `system/fvSolution/PIMPLE/maxCo`. Expand includes and macros before querying;
   querying a formerly included fragment is insufficient.
3. Supply unchanged trial input bytes in `input_files`, and the original restart
   source bytes in `restart_files`. Use a nonempty inventory for each. Dictionary
   edits belong in `dictionaries`, rather than the unchanged input inventory.
   Restart evidence describes the original source, rather than newly written
   solver output. Capture copies dictionaries and readbacks and hashes inputs
   and restart files with SHA-256. Dictionary text is preserved verbatim with
   its UTF-8 SHA-256 digest.
4. Call `resolve_time_controls` for a preflight view. Each control includes its
   consumed path, value, status, reason and source reference. This view alone
   makes no execution claim.
5. Collect paired observations from comparable trial stages. Local observations
   must use the same cells and order. Construct `TimeStepResponse` with the
   appropriate metric, both snapshots' `sha256()` values, a required
   `minimum_change`, and a nonempty `criterion_source` describing its basis.
   No acceptance threshold is supplied by this module.
6. Call `verify_experiment` with the requested identity, both snapshots, the
   intended full leaf paths and values in `edits`, and the response. Preserve the
   returned JSON-compatible receipt with the diagnostic record. A missing
   response cannot establish execution.

For each requested edit, the final consumed value must match the request exactly
and differ from the previous consumed value. Unrequested resolved settings,
input inventories and restart-source inventories must remain unchanged. Every
required check must pass before `executed` becomes true. Receipt status is:

- `REJECTED` if any check establishes an inactive edit, mismatch or other failure.
- `NOT_EVALUATED` if evidence or a supported contract is missing and no check fails.
- `EXECUTED` when all effective-control and response checks pass.

Response arithmetic is the maximum absolute difference of paired observations.
It must be strictly greater than the caller's threshold; equality is rejected.
This arithmetic measures an observed change. It does not establish causality,
predict the expected magnitude, qualify numerical accuracy, or certify solver
completion. Readbacks and observations are supplied by the caller; a hash binds
the evidence but does not authenticate where it came from. The caller is
responsible for complete inventories and truthful resolved readbacks.

## Source basis

All references below are OpenCFD OpenFOAM v2312, released in 2023. The public
source pages were read for this implementation; no solver default or fitted
numerical limit was copied into the receipt checks.

- [localEulerDdt.C, lines 39–43](https://api.openfoam.com/2312/localEulerDdt_8C_source.html#l00039)
  selects local stepping from the default time scheme.
- [interFoam.C, lines 81–103](https://api.openfoam.com/2312/interFoam_8C_source.html#l00081)
  selects the local or global time-step update.
- [VoF/setRDeltaT.H, lines 4–79](https://api.openfoam.com/2312/multiphase_2VoF_2setRDeltaT_8H_source.html#l00004)
  reads local controls from PIMPLE and applies the interface-cap condition.
- [readTimeControls.H, lines 35–42](https://api.openfoam.com/2312/readTimeControls_8H_source.html#l00035)
  reads the global adjustment switch, Courant cap and maximum time step.
- [VoF/alphaCourantNo.H, lines 34–37](https://api.openfoam.com/2312/VoF_2alphaCourantNo_8H_source.html#l00034)
  reads the global interface Courant cap from `controlDict`.
- [VoF/setDeltaT.H, lines 36–53](https://api.openfoam.com/2312/applications_2solvers_2multiphase_2VoF_2setDeltaT_8H_source.html#l00036)
  applies global caps only when adaptive stepping is enabled.

## Tests and later work

The regression suite uses synthetic settings and independent hand calculations
written in its docstrings. It covers inert local edits, inactive fixed global
caps, resolved-parent precedence over changed fragments, effective local/global
edits, response thresholds, invalid observations, identity mismatches,
incomplete untargeted active readbacks, inactive-control exclusions, rejection
precedence and preserved dictionaries/input/restart evidence.

Native dictionary querying, version discovery, automatic trial integration,
function-object controls and monitoring/process exception handling remain later
work. Other solvers, distributions and releases remain `NOT_EVALUATED`.
No licensed solver is imported, queried or started by these checks.
