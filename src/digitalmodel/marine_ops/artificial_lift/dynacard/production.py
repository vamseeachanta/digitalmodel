# ABOUTME: Production corrections for rod-pump slippage and fluid shrinkage.
# ABOUTME: Implements the house Patterson equation without assumed inputs.


def patterson_slippage_bpd(
    plunger_diameter_in: float,
    differential_pressure_psi: float,
    clearance_in: float,
    fluid_viscosity_cp: float,
    plunger_length_in: float,
    strokes_per_minute: float,
) -> float:
    """Return plunger/barrel slippage as a continuous-operation BPD rate.

    The equation and coefficients are transcribed from cell B17 of the house
    ``Pump_Slippage_Calculator_SPM_PattersonEq.xls`` workbook:

    ``452 * D * dP * C**1.52 / (mu * L) * (0.14 * SPM + 1)``

    Inputs use the oilfield units named by the workbook: inches, psi,
    centipoise, and strokes per minute.
    """
    values = (
        plunger_diameter_in,
        differential_pressure_psi,
        clearance_in,
        fluid_viscosity_cp,
        plunger_length_in,
        strokes_per_minute,
    )
    if any(value <= 0.0 for value in values):
        raise ValueError("Patterson slippage inputs must all be positive")
    return (
        452.0
        * plunger_diameter_in
        * differential_pressure_psi
        * clearance_in**1.52
        / (fluid_viscosity_cp * plunger_length_in)
        * (0.14 * strokes_per_minute + 1.0)
    )
