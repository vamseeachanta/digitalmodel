# Critical-ratio coefficient scaling

Governing basis: [damping-basis.md](damping-basis.md) E5; this note records the regression cases only.

Public test inputs are neutral illustrative examples.

For rotational motion the absolute coefficient is in N m s/rad; translational
use requires N s/m throughout. The caller resolves the degree of freedom and
units before calling. The existing method rounds additions to one decimal
place in the coefficient's units. A lower target returns zero; no removal is
computed.

The existing diffraction unit converters supply angular frequency, cycle
frequency and period conversions. A coefficient per degree/second is pi/180
times the coefficient per radian/second, so equal
physical angular rates produce equal torque. This coefficient transformation
uses the linear factor in `degrees_to_radians`; it is not a conversion of a
torque into an angle.

Current critical percentage must be positive. Absolute coefficient and target
percentage must be nonnegative. All three inputs must be finite. The method
rejects an implicit target between zero and one unless percent is explicit.
The frequency converters themselves do not validate the physical domain;
their caller must supply finite, positive periods and frequencies.

No standard or clause is stated in the source for this ratio-scaling method.
OCIMF is named for separate environmental-loading content: clause not stated
in source. No clause, tabulated constant or standard compliance is inferred.

Input gaps include the provenance and governing condition of measured or
solver-derived critical percentages and absolute coefficients, the choice of
target, and conflicting rotational/translational unit labels in some source
expressions. These remain caller-supplied inputs without inferred defaults.

The tests import `AqwaRAOs` and the existing diffraction conversion module
through their normal package paths. They exercise the bound
`get_additional_damping` method and unit converters without copying their
implementations. They do not verify
workflow routing, solver execution, resonance selection, or the physical
validity of supplied coefficients. Environmental coefficients, component
tables, geometry, polynomial fits and formatted solver records remain outside
this regression scope. No production implementation is added or changed.
