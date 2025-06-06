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
        if (data.get("F") is not None and data.get("w") is not None
            and data.get("d") is not None):
            return self._calculate_from_force(data)
        elif data.get("X") is not None:
            return self._calculate_from_x(data)
        elif data.get("q") is not None and data.get("d") is not None:
            return self._calculate_from_angle(data)
        else:
            raise ValueError("Insufficient or invalid input parameters for calculation.")

    def _calculate_from_force(self, data):
        """Calculate catenary properties using force parameters."""
        force = data["F"]
        weight = data["w"]
        distance = data["d"]

        length = distance * (2 * force / weight - distance)
        horiz_dist = (((force / weight) - distance) * math.log(
            (length + (force / weight)) / ((force / weight) - distance)))
        t_horiz = force * horiz_dist / math.sqrt(length**2 + horiz_dist**2)
        t_horizontal = force * horiz_dist / math.sqrt(length**2 + horiz_dist**2)
        shape_param = weight * 9.81 / t_horizontal

        data.update({
            "S": length,
            "X": horiz_dist,
            "THorizontal": t_horiz,
            "THorizontal": t_horizontal,
            "b": shape_param
        })
        return data

    def _calculate_from_x(self, data):
        """Calculate catenary properties using X parameter."""
        raise NotImplementedError(
            "Calculation based on X is not implemented yet.")

    def _calculate_from_angle(self, data):
        """Calculate catenary properties using angle parameters."""
        angle = data["q"]
        distance = data["d"]

        angle_comp = 90 - angle
        tan_q = math.tan(math.radians(angle_comp))
        cos_q = math.cos(math.radians(angle_comp))
        one_minus_cosq = 1 - cos_q

        if abs(one_minus_cosq) < 1e-12:
            raise ValueError("Invalid input: denominator too small in calculation.")

        bend_radius = distance * cos_q / one_minus_cosq
        length = bend_radius * tan_q
        horiz_dist = bend_radius * math.asinh(tan_q)

        data.update({
            "S": length,
            "X": horiz_dist,
            "BendRadius": bend_radius
        })
        return data
