# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Module for calculating catenary curve properties."""

import math


class CatenaryCalculator:
    """Class for calculating catenary curve properties."""

    def __init__(self):
        pass

    def calculate(self, data):
        """
        Calculate catenary properties based on the input data.

        Returns:
            dict: Updated data dictionary with computed values
        """
        if (
            data.get("F") is not None
            and data.get("w") is not None
            and data.get("d") is not None
        ):
            return self._calculate_from_force(data)
        elif data.get("q") is not None and data.get("d") is not None:
            return self._calculate_from_angle(data)
        elif data.get("X") is not None:
            return self._calculate_from_x(data)
        else:
            raise ValueError(
                "Insufficient or invalid input parameters for calculation."
            )

    def _calculate_from_force(self, data):
        """Calculate catenary properties using force parameters."""
        force = data["F"]
        weight = data["w"]
        distance = data["d"]

        # Governing vertex-catenary equations: engineering-basis.md E1-E2.
        # F is total top tension (N); w is effective weight (N/m).
        if not all(math.isfinite(v) and v > 0 for v in (force, weight, distance)):
            raise ValueError("F, w and d must be finite and positive")
        t_horizontal = force - weight * distance
        if t_horizontal <= 0:
            raise ValueError("F must exceed w*d for a suspended catenary")
        length = math.sqrt(distance * (2 * force / weight - distance))
        a = t_horizontal / weight
        # E1 half-angle identity avoids loss of a small distance/a.
        horiz_dist = 2 * a * math.asinh(math.sqrt(distance / a / 2))
        data.update({"S": length, "X": horiz_dist,
                     "THorizontal": t_horizontal, "b": weight / t_horizontal})
        return data

    def _calculate_from_x(self, data):
        """Calculate catenary properties using X parameter."""
        raise NotImplementedError("Calculation based on X is not implemented yet.")

    def _calculate_from_angle(self, data):
        """Calculate catenary properties using angle parameters."""
        angle = data["q"]
        distance = data["d"]

        if not math.isfinite(angle) or not 0 < angle < 90:
            raise ValueError("q must be finite and between 0 and 90 degrees from vertical")
        if not math.isfinite(distance) or distance <= 0:
            raise ValueError("d must be finite and positive")
        # E1-E3: sin(q)=a/(a+d). The half-angle identity avoids cancellation
        # in 1-sin(q) near a horizontal hang-off. No physical cutoff is inferred.
        q = math.radians(angle)
        bend_radius = distance * math.sin(q) / (2 * math.sin(math.radians(90 - angle) / 2)**2)
        length = math.sqrt(distance * (distance + 2 * bend_radius))
        horiz_dist = 2 * bend_radius * math.asinh(math.sqrt(distance / bend_radius / 2))

        data.update({"S": length, "X": horiz_dist, "BendRadius": bend_radius})
        return data
