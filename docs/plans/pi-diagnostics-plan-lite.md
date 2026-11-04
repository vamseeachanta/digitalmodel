# Plan-lite: productivity index from tests, skin and pre-stimulation triage (#3830)

## Scope

Add `src/digitalmodel/production_engineering/pi_diagnostics.py` beside
`gigo_detector.py` and `ipr_models.py`, with tests in
`tests/production_engineering/test_pi_diagnostics.py`:

1. `pi_from_test(q, pr, pwf, pwf_source, pwf_error_psi=None)`: J = q/(Pr - Pwf)
   with a +/- Pwf error band. A calculated Pwf requires a stated error; a
   measured Pwf defaults to zero error. An error not less than the drawdown
   leaves the upper band unbounded.
2. Darcy radial PI for oil in field units with the constant 141.2, skin from
   the ratio of tested to zero-skin PI, the skin pressure drop, flow
   efficiency and J2/J1 = (ln(re/rw) - 0.75 + S1)/(ln(re/rw) - 0.75 + S2).
3. `pre_stimulation_triage(current, previous, rock_fluid, thresholds)`: Pwf source,
   depletion (rate against PI), mobility (water cut and GOR), comparability
   (stabilisation, flow regime, choke, lift setting and Pwf), then skin. It stops at
   the first step that finds a cause and returns the cause, step and reason.
   Required `TriageThresholds` fields: `pi_change_fraction`,
   `rate_drop_fraction`, `water_cut_change`, `gor_change_fraction`,
   `choke_change_64ths`, `reservoir_pressure_drop_psi`, `pwf_change_psi`.
   Separate historical PI bands establish a change; the current band is also
   inverted to skin, with zero in that band leaving skin attribution unresolved.

`gigo_detector.py`, `ipr_models.py` and the package `__init__.py` are not
changed; the new module is imported directly. Wiring the triage into
`gigo_detector._diagnose` is left for a follow-up.

## Acceptance

- All tests in `tests/production_engineering/test_pi_diagnostics.py` pass.
  The expected values are hand-checked in the module docstring of the test
  file; a 100 psi error at 300 psi drawdown gives -25 % / +50 % on J.
- `tests/docs/test_production_nde_agent_packs.py` passes: section 2 of
  `src/digitalmodel/production_engineering/docs/agent-pack.md` lists every
  public symbol of `pi_diagnostics` with exact line locators, and the
  revision history records the addition.
- The rest of `tests/production_engineering/` passes as it does at the base.

## Failures the test loop may fix

The loop runs
`python -m pytest tests/production_engineering tests/docs/test_production_nde_agent_packs.py -q -p no:cacheprovider`
and may fix only these:

1. Any failing test in `tests/production_engineering/test_pi_diagnostics.py`:
   fix `pi_diagnostics.py`, not the test. If the hand-check arithmetic in a
   test docstring or assertion is itself wrong, stop and report BLOCKED with
   the test id and the arithmetic; do not change the expected value.
2. `test_pack_public_functions_and_locators[production_engineering]` and
   `test_pack_cited_nodes_collect[production_engineering]` reporting
   `missing function pi_diagnostics.*` or a wrong locator: add one row per
   public `pi_diagnostics` symbol to section 2 of the agent pack, in the
   existing row format (inputs and defaults, units and validation, output
   contract, `[path:Lstart-Lend]` locator taken from the final file), and a
   revision 3 row dated with the run date: "Add `pi_diagnostics.py` (#3830);
   no other module changed." Add the equations to section 3 only as a list
   pointing at the module docstring. Do not change the code pin line.
3. A failure that also fails at the base commit of this branch: do not fix
   it; record its node id in PR.md under "Pre-existing failures".

## Limits

- Single-phase oil above the bubble point; vertical well, circular drainage
  area, pseudo-steady state; no rate-dependent (non-Darcy) skin.
- Every triage tolerance is a required caller input, including the minimum
  reservoir-pressure decline and the allowed Pwf change in psi. The module
  supplies no field screening criterion. Depletion requires pressure-decline
  evidence and comparable operating conditions; either zero rate returns
  insufficient data before skin inversion. Only stated Pwf error is propagated;
  the supplied rock/fluid inputs and ideal PI are held fixed.
- The module cites Petroleum Production Systems, second edition,
  section 1.3.1, equations (1-6) and (1-8), and section 1.4, Example 1-1,
  equation (1-10), for the equation basis. The tests remain synthetic hand
  checks; qualification against a published worked example remains open
  under #2011.
