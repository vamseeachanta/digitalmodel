# Relative point-response arithmetic

## Summary

`digitalmodel.marine_ops.w2w_gangway` provides an arithmetic building block:
subtract the access-point complex response from the receiving-point complex
response at a single, explicitly matched frequency and heading. It retains the
complex phase and the declared units. Standards qualification and the full
operability table remain open.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-26 | Added the arithmetic contract and independent examples to define the limited calculation scope. |

## Input contract

`compute_relative_rao(access_point_response, receiving_point_response,
access_basis=..., receiving_basis=...)` accepts mappings from component names to
complex responses. `ResponseBasis` declares frequency, heading, their units,
heading convention, coordinate convention, harmonic phase convention, common
incident-wave phase reference and a unit label for every response component.
For transfer responses the labels include wave normalization, such as `m/m` or
`rad/m`. The phase reference declaration includes the incident-wave normalization.
Both sides must describe the same incident wave and phase origin.

Responses **must already be resolved at the two physical points**, expressed
along common axes. The caller applies rigid-body lever arms, coordinate
transformations, unit conversions and spatial wave-phase shifts before calling.
Labels are caller declarations; the helper cannot establish their physical truth.
There are no implicit zero components or default conventions. A stationary side
supplies explicit complex zeros for every declared component.

The helper rejects any basis mismatch, differing component sets, empty labels
or nonfinite inputs. Matching is exact: there is no tolerance, interpolation,
heading wrapping or conversion between equivalent labels. Each call handles one
frequency/heading sample; grids require explicit matching and iteration outside
this helper. The result carries `responses` and the unchanged `basis`.

## Independent hand calculations

These synthetic inputs are chosen to illustrate complex arithmetic; they are
neither measured data nor acceptance limits. The tests use the following hand
calculations independently of the helper:

- Cancellation: each complex component minus itself is zero.
- Stationary access: receiving minus zero retains the receiving response.
  Stationary receiving: zero minus access reverses its sign and phase.
- Phase opposition: access `3 + 4i` has magnitude `5`; receiving `-6 - 8i`
  has magnitude `10`. Subtraction gives `-9 - 12i`, magnitude `15`.
  Subtracting magnitudes first would give a different result.
- Lever arm: the small-angle rigid-body identity
  `u_point = u_origin + theta cross r` gives vertical response
  `z = heave - rx*pitch + ry*roll` in common right-handed axes, with rotations
  in radians and offsets in metres. This is also the identity documented by
  [`vertical_motion_at`](../../../src/digitalmodel/motion_forecast/reconstruct.py).
  For access, `h = 1 + 2i`, `pitch = 0.1 + 0.2i`, `roll = 0.3 - 0.1i`
  and `r = (2, 3, 4)` give `z_A = 1.7 + 1.3i`.
  For receiving, `h = 2 - i`, `pitch = -0.2 + 0.1i`, `roll = 0.1 + 0.2i`
  and `r = (-1, 2, 3)` give `z_R = 2 - 0.5i`.
  The caller supplies these resolved point responses; subtraction yields
  `z_R - z_A = 0.3 - 1.8i`. The vertical component has no `rz` contribution.

## Validation and remaining scope

Before the addition, the adjacent suites contained and passed five derived-motion
cases and twenty-two analytic reconstruction cases. After the addition, those
counts remain unchanged; thirty-three new relative-response cases also pass.
The regression examples cover complex phase, units, both stationary sides,
pre-resolved point offsets and explicit basis rejection.

This helper supplies no certification thresholds, standards tables, persistence
model, solver adapter or operability decision. Standards qualification and the
full operability table require separate inputs and assessment. Shared phase
reference, wave normalization and point resolution must be established before
using the arithmetic in an engineering analysis.
