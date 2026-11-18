# Plan-lite: serial axial stiffness of segmented mooring lines

## Bounded property

Expose `MooringLine.axial_stiffness` as `1 / sum(L_i / EA_i)` for segments
carrying the same axial force. Length is in m, EA is in kN, and the returned
small-strain material stiffness is in kN/m.

## Validation rules

On every property access, reject an empty segment list and any segment whose
length or EA is nonpositive or nonfinite with `ValueError`. Check every current
segment so mutation after construction is validated. Existing total length,
weakest-segment capacity and material-template construction retain their behavior.

## Independent test basis

Use synthetic arithmetic inputs only. Under a common 100 kN force, independently
calculated extensions of 0.025, 0.200 and 0.025 m total 0.250 m, giving 400 kN/m.
Check unequal segments, the single-segment case, invariance when splitting an
identical-material segment, and existing length and capacity properties.
Reject zero, negative, NaN and both infinite signs for length and EA in every
segment position. These cases are covered by the 35 tests in
`tests/subsea/mooring_analysis/test_series_stiffness.py`; rerun them and the
complete mooring module suite before merge, reporting environment limits.

## Limits

This property covers linear material extension in series. Geometric catenary
stiffness, nonlinear response, environmental loading, design factors and solver
generation remain outside its scope. Synthetic examples establish the stated
arithmetic and validation behavior; they do not qualify measured line response.
The broader mooring study in #203 remains open.
