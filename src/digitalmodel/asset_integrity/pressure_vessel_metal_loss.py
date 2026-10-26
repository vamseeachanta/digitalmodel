# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Uniform external metal loss in an idealised closed cylindrical shell.

Primary basis: first-principles static force equilibrium. For a longitudinal
diametral cut of length L, pressure p acts on projected area 2*r*L and the
two walls carry 2*t*L*sigma_h. Equating forces gives sigma_h = p*r/t.
For a closed-end transverse cut, pressure thrust p*pi*r**2 balances the
thin-wall membrane force sigma_l*2*pi*r*t, giving sigma_l = p*r/(2*t).
The axial wall area neglects terms of order t**2: both stresses are thin-shell
approximations, away from ends and discontinuities. Units: mm and MPa (N/mm2).

Metal loss is an ASSUMED uniform reduction from nominal thickness:
t_remaining = t_nominal - loss. The inner radius stays fixed, representing
external loss. Pressure is internal minus external pressure, nonnegative.
There are no other axial forces, moments, thermal loads or local defects.

The reference thickness is the result of our implementation, delegated to
ASMEThicknessCalc.ug27_cylindrical_shell with zero corrosion allowance.
Its formula is neither reproduced nor independently qualified here. The signed
thickness difference is remaining minus reference thickness; it is geometric
arithmetic, with no acceptance verdict or assessment level. Acceptance criteria,
material/temperature qualification, future allowance and inspection uncertainty
remain data gaps. No standard document is used as a source.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.ansys.pressure_vessel import ASMEThicknessCalc


@dataclass(frozen=True)
class ShellInputs:
    """Explicit shell inputs; example values are ASSUMED, not material data."""

    pressure_mpa: float
    inner_radius_mm: float
    nominal_thickness_mm: float
    assumed_allowable_stress_mpa: float
    joint_efficiency: float


@dataclass(frozen=True)
class ShellLossResult:
    """Membrane demands and a thickness comparison without a verdict."""

    metal_loss_mm: float
    remaining_thickness_mm: float
    hoop_stress_mpa: float
    longitudinal_stress_mpa: float
    implementation_required_thickness_mm: float
    thickness_difference_mm: float


def _number(name: str, value: float, *, positive: bool = False) -> None:
    """Reject nonnumeric, nonfinite and physically invalid scalar inputs."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or value < 0 or (positive and value == 0):
        raise ValueError(f"{name} must be finite and nonnegative; positive if dimensional")


def evaluate_shell_loss(inputs: ShellInputs, metal_loss_mm: float) -> ShellLossResult:
    """Calculate one loss case using the force balances in the module docstring.

    The caller establishes applicability of the thin-shell approximation.
    An absent wall is rejected; a thickness deficit is returned as a signed
    value so it can be plotted without implying a service decision.
    """
    _number("pressure_mpa", inputs.pressure_mpa)
    for name in ("inner_radius_mm", "nominal_thickness_mm",
                 "assumed_allowable_stress_mpa", "joint_efficiency"):
        _number(name, getattr(inputs, name), positive=True)
    if inputs.joint_efficiency > 1:
        raise ValueError("joint_efficiency must be at most 1")
    _number("metal_loss_mm", metal_loss_mm)
    remaining = inputs.nominal_thickness_mm - metal_loss_mm
    if remaining <= 0:
        raise ValueError("metal loss consumes the whole wall")
    try:
        required = ASMEThicknessCalc().ug27_cylindrical_shell(
            pressure_mpa=inputs.pressure_mpa,
            inner_radius_mm=inputs.inner_radius_mm,
            allowable_stress_mpa=inputs.assumed_allowable_stress_mpa,
            joint_efficiency=inputs.joint_efficiency,
            corrosion_allowance_mm=0.0,
        )["t_required_mm"]
    except ZeroDivisionError as exc:
        raise ValueError("thickness implementation is undefined for these inputs") from exc
    if not math.isfinite(required) or required < 0:
        raise ValueError("thickness implementation returned an invalid thickness")
    hoop = inputs.pressure_mpa * inputs.inner_radius_mm / remaining
    if not math.isfinite(hoop):
        raise ValueError("membrane stress is nonfinite for these inputs")
    return ShellLossResult(
        metal_loss_mm=metal_loss_mm,
        remaining_thickness_mm=remaining,
        hoop_stress_mpa=hoop,
        longitudinal_stress_mpa=hoop / 2,
        implementation_required_thickness_mm=required,
        thickness_difference_mm=remaining - required,
    )
