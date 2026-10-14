"""Solver-neutral equilibrium screens on explicitly supplied evidence.

These are arithmetic QA checks, not stability or class-rule acceptance.
Tolerances are required inputs: the caller shall derive them from the model's
coordinate precision, mass budget and force convergence criterion. No universal
engineering tolerance or implicit unit conversion is assumed.
"""
from math import hypot, isfinite


def _number(value, name, *, positive=False):
    value = float(value)
    if not isfinite(value) or (positive and value <= 0):
        raise ValueError(f"{name} must be finite" + (" and positive" if positive else ""))
    return value


def _tolerance(value, name):
    value = _number(value, name)
    if value < 0:
        raise ValueError(f"{name} must be non-negative")
    return value


def _vector(values, name):
    if len(values) != 3:
        raise ValueError(f"{name} must contain three components")
    return [_number(v, name) for v in values]


def check_cog_cob(centre_of_gravity, centre_of_buoyancy, tolerance_m):
    """Compare horizontal CoG/CoB separation with inclusive ``tolerance_m``.

    Coordinates shall be metres in the same gravity-aligned frame at the
    assessed attitude. For a freely floating body without external moments,
    weight and buoyancy act on the same vertical line. Vertical separation is
    therefore deliberately not an error; this check does not establish GM.
    Set the absolute tolerance from coordinate/mesh precision. External moments
    or a restrained body require a different equilibrium model.
    """
    cog = _vector(centre_of_gravity, "centre_of_gravity")
    cob = _vector(centre_of_buoyancy, "centre_of_buoyancy")
    tolerance = _tolerance(tolerance_m, "tolerance_m")
    separation = hypot(cog[0] - cob[0], cog[1] - cob[1])
    if separation > tolerance:
        return [f"Horizontal CoG-CoB separation {separation:.6g} m exceeds {tolerance:.6g} m"]
    return []


def check_mass_displacement(mass_kg, displacement_m3, water_density_kg_m3,
                            relative_tolerance):
    """Compare mass with displaced-water mass for free flotation.

    Acceptance is abs(mass - density * volume) <= relative_tolerance *
    density * volume, inclusive. Specify relative tolerance from the mass and
    hydrostatic discretization budgets; zero requests exact arithmetic equality.
    The reference is displaced mass, not the larger of the two masses, so an
    erroneous input mass cannot relax its own criterion. SI inputs are required.
    """
    mass = _number(mass_kg, "mass_kg", positive=True)
    volume = _number(displacement_m3, "displacement_m3", positive=True)
    density = _number(water_density_kg_m3, "water_density_kg_m3", positive=True)
    tolerance = _tolerance(relative_tolerance, "relative_tolerance")
    displaced = _number(density * volume, "displaced mass", positive=True)
    limit = _number(tolerance * displaced, "mass tolerance")
    if abs(mass - displaced) > limit:
        return [f"Mass {mass:.6g} kg differs from displaced mass {displaced:.6g} kg by more than {limit:.6g} kg"]
    return []


def check_force_balance(input_forces_n, output_forces_n, relative_tolerance,
                        absolute_tolerance_n):
    """Compare three signed force components, without magnitude cancellation.

    Each component shall satisfy abs(output - input) <= absolute_tolerance_n +
    relative_tolerance * abs(input), inclusive. Both vectors shall refer to the
    same load case, coordinates and sign convention, in N. Reaction forces
    shall be sign-reversed by the caller. Set the relative tolerance from the
    force-convergence criterion and the absolute tolerance from the resolution
    of near-zero components. Moments, mixed units and complex RAOs are excluded.
    """
    supplied = _vector(input_forces_n, "input_forces_n")
    returned = _vector(output_forces_n, "output_forces_n")
    relative = _tolerance(relative_tolerance, "relative_tolerance")
    absolute = _tolerance(absolute_tolerance_n, "absolute_tolerance_n")
    issues = []
    for axis, expected, actual in zip("xyz", supplied, returned):
        limit = _number(absolute + relative * abs(expected), "force tolerance")
        if abs(actual - expected) > limit:
            issues.append(f"Force {axis}: input {expected:.6g} N, output {actual:.6g} N; difference exceeds {limit:.6g} N")
    return issues
