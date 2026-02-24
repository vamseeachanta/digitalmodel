"""FFS Level 1 screening — minimum measured thickness vs code-required minimum.

Implements API 579-1/ASME FFS-1 Level 1 acceptance criterion:

  t_mm >= t_min  →  ACCEPT
  t_mm <  t_min  →  FAIL_LEVEL_1

The minimum required thickness t_min is calculated per the applicable
design code:

+------------------+-----------------------------------------------------------+
| Code             | Equation                                                  |
+==================+===========================================================+
| B31.8            | t = P * D / (2 * SMYS * F * E_joint * T_derating)        |
|                  | F = 0.72 (Class 1, Div 1), E_joint = 1.0, T = 1.0       |
+------------------+-----------------------------------------------------------+
| B31.4            | t = P * D / (2 * SMYS * F * E_joint)                     |
|                  | F = 0.72, E_joint = 1.0 (typical liquid transmission)    |
+------------------+-----------------------------------------------------------+
| ASME_VIII_DIV1   | t = P * R / (S * E - 0.6 * P)   [UG-27(c)(1)]           |
|                  | R = OD / 2 (conservative thin-wall approximation)        |
|                  | S = allowable stress, E = weld joint efficiency = 1.0    |
+------------------+-----------------------------------------------------------+

References:
  ASME B31.8-2022 §841.1.1
  ASME B31.4-2022 §403.2.1
  ASME BPVC Section VIII Div 1-2023 UG-27(c)(1)
"""

from __future__ import annotations

_SUPPORTED_CODES = {"B31.8", "B31.4", "ASME_VIII_DIV1"}

# Default design factors (conservative — override via constructor kwargs)
_B31_DESIGN_FACTOR = 0.72
_ASME8_WELD_JOINT_EFF = 1.0


class Level1Screener:
    """Evaluate Level 1 FFS acceptance per a specified design code.

    Args:
        design_code: One of ``'B31.8'``, ``'B31.4'``, ``'ASME_VIII_DIV1'``.
        nominal_od_in: Pipe / vessel outside diameter (inches).
        design_pressure_psi: Internal design / operating pressure (psi).
        smys_psi: Specified minimum yield strength (psi).
            Required for B31.4 and B31.8.
        allowable_stress_psi: Allowable design stress (psi).
            Required for ASME_VIII_DIV1.  If not supplied, defaults to
            SMYS / 3.5 (conservative ASME rule-of-thumb).
        b31_design_factor: Override the B31.4 / B31.8 design factor F
            (default 0.72).
        weld_joint_efficiency: ASME VIII weld joint efficiency E (default 1.0).

    Raises:
        ValueError: If design_code is not in the supported set.
    """

    def __init__(
        self,
        design_code: str,
        nominal_od_in: float,
        design_pressure_psi: float,
        *,
        smys_psi: float | None = None,
        allowable_stress_psi: float | None = None,
        b31_design_factor: float = _B31_DESIGN_FACTOR,
        weld_joint_efficiency: float = _ASME8_WELD_JOINT_EFF,
    ) -> None:
        normalised_code = design_code.strip().upper().replace("-", "_")
        # Accept "ASME VIII DIV1" etc. by normalising spaces
        normalised_code = normalised_code.replace(" ", "_")
        # Re-map bare "B31_8" → "B31.8" style
        normalised_code = normalised_code.replace("B31_8", "B31.8").replace(
            "B31_4", "B31.4"
        )
        if normalised_code not in _SUPPORTED_CODES:
            raise ValueError(
                f"design_code '{design_code}' is not supported.  "
                f"Choose from: {sorted(_SUPPORTED_CODES)}"
            )

        self._code = normalised_code
        self._od = nominal_od_in
        self._pressure = design_pressure_psi
        self._smys = smys_psi
        self._b31_df = b31_design_factor
        self._weld_eff = weld_joint_efficiency

        if allowable_stress_psi is not None:
            self._s_allow = allowable_stress_psi
        elif smys_psi is not None:
            # Conservative default: S = SMYS / 3.5 (approx. ASME low-alloy)
            self._s_allow = smys_psi / 3.5
        else:
            self._s_allow = None

    # ------------------------------------------------------------------
    # Public interface
    # ------------------------------------------------------------------

    def t_min(self) -> float:
        """Calculate the code-required minimum wall thickness (inches)."""
        if self._code == "B31.8":
            return self._t_min_b31(
                temperature_derating=1.0,
                longitudinal_joint_factor=1.0,
            )
        if self._code == "B31.4":
            return self._t_min_b31(
                temperature_derating=None,
                longitudinal_joint_factor=1.0,
            )
        if self._code == "ASME_VIII_DIV1":
            return self._t_min_asme8_ug27()
        raise AssertionError(f"Unhandled code: {self._code}")  # should not reach

    def evaluate(self, tmm_in: float) -> dict:
        """Compare minimum measured thickness to code-required minimum.

        Args:
            tmm_in: Minimum measured wall thickness from inspection (inches).

        Returns:
            dict with keys:
                verdict (str): ``'ACCEPT'`` or ``'FAIL_LEVEL_1'``
                t_min_in (float): Code-required minimum thickness (inches)
                t_mm_in (float): Input minimum measured thickness (inches)
                margin_in (float): t_mm - t_min (positive = acceptable margin)
                design_code (str): Design code used
        """
        t_min_val = self.t_min()
        margin = tmm_in - t_min_val
        verdict = "ACCEPT" if tmm_in >= t_min_val else "FAIL_LEVEL_1"
        return {
            "verdict": verdict,
            "t_min_in": t_min_val,
            "t_mm_in": tmm_in,
            "margin_in": margin,
            "design_code": self._code,
        }

    # ------------------------------------------------------------------
    # Private helpers — one per design code
    # ------------------------------------------------------------------

    def _t_min_b31(
        self,
        temperature_derating: float | None,
        longitudinal_joint_factor: float,
    ) -> float:
        """B31.4 / B31.8 minimum thickness.

        B31.8 §841.1.1: t = P * D / (2 * S * F * E * T)
        B31.4 §403.2.1: t = P * D / (2 * S * F * E)
          where S = SMYS, F = design factor, E = weld joint efficiency,
          T = temperature derating factor (B31.8 only, = 1.0 for ≤ 250 °F).
        """
        if self._smys is None:
            raise ValueError(
                f"smys_psi must be provided for design_code='{self._code}'."
            )
        denominator = 2.0 * self._smys * self._b31_df * longitudinal_joint_factor
        if temperature_derating is not None:
            denominator *= temperature_derating
        return (self._pressure * self._od) / denominator

    def _t_min_asme8_ug27(self) -> float:
        """ASME VIII Div 1 UG-27(c)(1) minimum thickness.

        t = P * R / (S * E - 0.6 * P)
          where R = OD / 2 (thin-wall conservative approximation),
          S = allowable stress, E = weld joint efficiency.
        """
        if self._s_allow is None:
            raise ValueError(
                "allowable_stress_psi or smys_psi must be provided for "
                "design_code='ASME_VIII_DIV1'."
            )
        r = self._od / 2.0
        denominator = self._s_allow * self._weld_eff - 0.6 * self._pressure
        if denominator <= 0:
            raise ValueError(
                "Pressure exceeds allowable limit for ASME VIII Div 1 "
                "(S*E - 0.6*P <= 0).  Check inputs."
            )
        return (self._pressure * r) / denominator
