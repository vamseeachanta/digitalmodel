"""Physical input domains shared by the scalar stability surfaces.

Zero bounds below are geometric/sign definitions, not empirical standard limits.
Signed velocity and acceleration are permitted; stability demands are peaks.
"""

import math
from numbers import Real


def _number(name, value, *, positive=False, signed=False):
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite")
    if positive and value <= 0:
        raise ValueError(f"{name} must be positive")
    if not signed and value < 0:
        raise ValueError(f"{name} must be nonnegative")


def _loads(rho, diameter, velocity, acceleration, cd, cm, cl):
    _number("water density", rho, positive=True)
    _number("diameter", diameter, positive=True)
    _number("velocity", velocity, signed=True)
    _number("acceleration", acceleration, signed=True)
    for name, value in (
        ("drag coefficient", cd),
        ("inertia coefficient", cm),
        ("lift coefficient", cl),
    ):
        _number(name, value)


def _geometry(od, wall, coat, steel, coating, contents, water):
    _number("steel diameter", od, positive=True)
    _number("wall thickness", wall, positive=True)
    _number("coating thickness", coat)
    if 2 * wall >= od:  # Positive bore; concentric-circle geometry.
        raise ValueError("wall thickness must leave a positive bore")
    _number("steel density", steel, positive=True)
    _number("water density", water, positive=True)
    _number("coating density", coating)
    _number("contents density", contents)


def _stability(weight, horizontal, lift, friction, passive, factor):
    # Buoyant weights remain finite signed results; assessment returns failure.
    _number("submerged weight", weight, signed=True)
    _number("horizontal force", horizontal)
    _number("lift force", lift)
    _number("friction coefficient", friction)
    _number("passive resistance", passive)
    _number("safety factor", factor, positive=True)
