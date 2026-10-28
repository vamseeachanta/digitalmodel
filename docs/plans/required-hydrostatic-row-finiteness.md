# Required hydrostatic-row finiteness

The scope of issue #3284 covered here is rejection of non-finite
values in the three required `HydroRow` properties. The implementation and
existing regression tests are recorded below; optional-property policies and
other stability-screening findings remain outside this scope.

## Plan-lite

1. Check `draft_m`, `displacement_t` and `km_m` with `math.isfinite` before
   the existing positivity checks. Reject NaN and either infinity with a
   field-specific `ValueError`; retain the positive-value requirement.
2. Exercise the rejection and control matrix with synthetic inputs:

   | Cases | Count | Expected result |
   | --- | ---: | --- |
   | Three required fields with NaN, positive infinity and negative infinity | 9 | Field-specific finite-value error |
   | Three required fields with zero and minus one | 6 | Existing positivity error |
   | Two exact nodes and one midpoint, each queried by displacement and draft | 3 | Finite supplied or interpolated properties |

   Each finite-control case exercises both lookup directions. For the synthetic
   rows (draft m, displacement t, KM m) `(1, 10, 3)` and `(2, 20, 5)`, the
   midpoint at 15 t gives draft 1.5 m and KM 4 m by hand interpolation.
3. Preserve the policies for `lcb_m`, `lcf_m`, `mct_t_m_per_cm` and
   `tpc_t_per_cm`, including absent optional properties. Preserve trim, GZ,
   interpolation and assessment policies; introduce no engineering constants
   or acceptance thresholds.

## Validation

Run the [core tests](../../tests/naval_architecture/test_vessel_stability_screening.py)
and [workflow tests](../../tests/naval_architecture/test_vessel_stability_screening_workflow.py).
The expected matrix is 53 core cases and 17 workflow cases, including the
18 required-property rejection and finite-control cases above. The inputs
are synthetic controls, not a physical hydrostatics validation.
