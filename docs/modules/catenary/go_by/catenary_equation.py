import math

def catenaryEquation(data):
    """
    Calculates catenary properties based on the given input dictionary.

    Args:
        data (dict): Input parameters. Possible keys:
            - F (float): Force (horizontal tension component)
            - w (float): Weight per unit length
            - d (float): Vertical distance or distance between supports
            - X (float, optional): Horizontal distance (Not implemented)
            - q (float, optional): Angle in degrees (alternative input)

    Returns:
        dict: Original data dict updated with computed values.
    """
    # Validate inputs
    if not isinstance(data, dict):
        raise ValueError("Input must be a dictionary.")

    # Case 1: Calculation based on F (horizontal tension)
    if data.get("F") is not None and data.get("w") is not None and data.get("d") is not None:
        F = data["F"]
        w = data["w"]
        d = data["d"]

        S = d * (2 * F / w - d)
        # Horizontal Distance.
        X = (((F / w) - d) * math.log(
            (S + (F / w)) / ((F / w) - d)))
        # Weight of the suspended chain
        W = w * S
        # Normalized horizontal tension component
        THorizontal = F * X / math.sqrt(S**2 + X**2)
        # Catenary shape parameter
        b = w * 9.81 / THorizontal

        data.update({
            "S": S,
            "X": X,
            "W": W,
            "THorizontal": THorizontal,
            "b": b
        })
        return data

    # Case 2: Calculation based on X (not implemented)
    elif data.get("X") is not None:
        raise NotImplementedError("Calculation based on X is not implemented yet.")

    # Case 3: Calculation based on angle q
    elif data.get("q") is not None and data.get("d") is not None:
        q = data["q"]
        d = data["d"]

        angle_comp = 90 - q
        tanq = math.tan(math.radians(angle_comp))
        cosq = math.cos(math.radians(angle_comp))
        one_minus_cosq = 1 - cosq
        if abs(one_minus_cosq) < 1e-12:
            raise ValueError("Invalid input: denominator too small in bend radius calculation.")

        BendRadius = d * cosq / one_minus_cosq
        S = BendRadius * tanq
        X = BendRadius * math.asinh(tanq)

        data.update({
            "S": S,
            "X": X,
            "BendRadius": BendRadius
        })
        return data

    else:
        raise ValueError("Insufficient or invalid input parameters for catenary calculation.")