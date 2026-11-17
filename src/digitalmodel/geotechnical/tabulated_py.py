"""Adapt supplied normalized lateral load-transfer ordinates to an SI curve.

Method: y_i = reference_displacement_m * displacement_ratios[i], and
p_i = reference_resistance_kn_m * resistance_ratios[i]. The existing
``py_curves.Curve`` supplies piecewise-linear interpolation, odd reflection,
and constant resistance beyond the last knot. Decreasing branches and zero
residual resistance are permitted. This is a monotonic backbone representation,
not a cyclic hysteresis or pile-equilibrium solver.

A legacy engineering workbook identifies API RP 2GEO; clause not stated in
source. This adapter selects no standard-table coefficients and makes no
standard-conformance claim. Use ``py_curves.soft_clay_py`` for the existing
continuous soft-clay model, which can differ from rounded tabulated ordinates.

Inputs and units: reference displacement [m], reference lateral resistance
[kN/m], and two dimensionless ordinate arrays. All are required. The existing
``digitalmodel.units.ureg`` provides conversions at the caller's unit boundary;
force per unit pile length must remain distinct from total force or stress.

Input gaps: source clauses, ultimate resistance derivation, characteristic
displacement derivation, and cyclic resistance selection are not established.
No empirical values are inferred from soil descriptions. Supplied ratios and
reference values require an independent engineering basis. Solver-file layout,
soil interpolation, axial capacity derivation, and installation-age applicability
remain outside this adapter. Existing axial table scaling is in
``py_curves.clay_tz``; its stress output is in kPa.
"""

from dataclasses import dataclass
import math

from digitalmodel.geotechnical.py_curves import Curve


@dataclass(frozen=True)
class TabulatedPYInputs:
    """Required scales and normalized p-y ordinates, without empirical defaults.

    References must be positive and finite. Ratios must be finite, nonnegative
    paired arrays beginning at the origin; displacement ratios must increase
    strictly. Caller-owned arrays are copied to immutable tuples.
    """

    reference_displacement_m: float
    reference_resistance_kn_m: float
    displacement_ratios: tuple[float, ...]
    resistance_ratios: tuple[float, ...]

    def __post_init__(self) -> None:
        for name in ("reference_displacement_m", "reference_resistance_kn_m"):
            value = getattr(self, name)
            if isinstance(value, bool) or not isinstance(value, (int, float)):
                raise ValueError(f"{name} must be a positive finite number")
            try:
                finite = math.isfinite(value)
            except OverflowError as error:
                raise ValueError(f"{name} must be a positive finite number") from error
            if not finite or value <= 0:
                raise ValueError(f"{name} must be a positive finite number")
        # Validate factors before multiplication; invalid signs cannot cancel.
        checked = Curve(self.displacement_ratios, self.resistance_ratios, "kN/m", True)
        object.__setattr__(self, "displacement_ratios", checked.displacements_m)
        object.__setattr__(self, "resistance_ratios", checked.resistances)


def tabulated_py_curve(inputs: TabulatedPYInputs) -> Curve:
    """Scale y and p ordinates into metres and kN/m, retaining supplied knots.

    y_i = y_ref * r_yi; p_i = p_ref * r_pi. API RP 2GEO is named in
    a legacy engineering workbook; clause not stated in source. No table,
    ultimate resistance, or cyclic degradation law is selected here.
    """
    return Curve(
        tuple(inputs.reference_displacement_m * r for r in inputs.displacement_ratios),
        tuple(inputs.reference_resistance_kn_m * r for r in inputs.resistance_ratios),
        "kN/m",
        True,
    )
