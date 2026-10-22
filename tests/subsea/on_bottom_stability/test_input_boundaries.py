"""Slice 4: finite physical domains and directional negative controls.

Numerical fixtures are synthetic assumptions. Zero is the boundary of positive
geometry and nonnegative density/coefficient domains, not a standard limit.
"""

import math
import inspect

import pytest

from digitalmodel.subsea.on_bottom_stability import dnv_rp_f109 as f109
from digitalmodel.subsea.on_bottom_stability import simplified
from digitalmodel.subsea.on_bottom_stability.e305_simplified import Case, evaluate_case


@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf, True])
@pytest.mark.parametrize(
    "function,args,index",
    [
        (f109.hydrodynamic_force_per_meter, [1000, 1, 2, 0.5, 2, 3], 2),
        (f109.lift_force_per_meter, [1000, 1, 2, 0.5], 3),
        (f109.submerged_weight_per_meter, [1, 0.1, 8, 3, 0.1, 1, 1], 4),
        (f109.absolute_stability_check, [200, 40, 10, 0.5, 2], 0),
        (f109.generalized_stability_check, [200, 40, 10, 0.5, 10, 2], 4),
        (simplified.hydrodynamic_loads, [1, 2, 0.5, 1000, 2, 3, 0.5], 2),
        (simplified.drag_force_per_meter, [2, 1, 1000, 2], 0),
        (simplified.lift_force_per_meter, [2, 1, 1000, 0.5], 1),
        (simplified.inertia_force_per_meter, [0.5, 1, 1000, 3], 2),
        (simplified.submerged_weight, [1, 0.1, 0.1, 8, 3, 1, 1], 0),
        (simplified.check_lateral_stability, [0.2, 0.04, 0.01, 0.5, 2], 3),
        (simplified.check_vertical_stability, [0.2, 0.01, 2], 1),
    ],
)
def test_nonfinite_or_boolean_inputs_rejected(function, args, index, bad):
    args = args.copy()
    args[index] = bad
    with pytest.raises(ValueError):
        function(*args)


@pytest.mark.parametrize(
    "args",
    [
        (0, 1, 1, 1, 1, 1),
        (1000, -1, 1, 1, 1, 1),
        (1000, 1, 1, 1, -1, 1),
        (1000, 1, 1, 1, 1, -1),
    ],
)
def test_nonphysical_load_inputs_rejected(args):
    with pytest.raises(ValueError):
        f109.hydrodynamic_force_per_meter(*args)


@pytest.mark.parametrize(
    "wall,coat", [(-0.1, 0), (0, 0), (0.5, 0), (0.6, 0), (0.1, -0.1)]
)
def test_invalid_annular_geometry_rejected(wall, coat):
    with pytest.raises(ValueError):
        f109.submerged_weight_per_meter(1, wall, 8, 3, coat, 1, 1)


@pytest.mark.parametrize(
    "args", [(200, 40, -1, 0.5, 2), (200, 40, 10, -0.5, 2), (200, 40, 10, 0.5, 0)]
)
def test_nonphysical_stability_inputs_rejected(args):
    with pytest.raises(ValueError):
        f109.absolute_stability_check(*args)


def test_signed_horizontal_load_must_not_produce_negative_required_weight():
    """Hand balance: gamma*abs(-40)/mu+L=2*40/.5+10=170 N/m."""
    assert (
        simplified.lateral_stability_check(200, -40, 10, 0.5, 2).required_weight_n_per_m
        == 170
    )


@pytest.mark.parametrize(
    "function,names",
    [
        (f109.hydrodynamic_force_per_meter, ("C_D", "C_M")),
        (f109.lift_force_per_meter, ("C_L",)),
        (f109.absolute_stability_check, ("gamma_SC",)),
        (f109.generalized_stability_check, ("gamma_SC",)),
        (simplified.hydrodynamic_loads, ("cl",)),
        (simplified.lateral_stability_check, ("safety_factor",)),
        (simplified.check_lateral_stability, ("safety_factor",)),
        (simplified.check_vertical_stability, ("safety_factor",)),
    ],
)
def test_unverified_table_values_are_required_inputs(function, names):
    for name in names:
        assert (
            inspect.signature(function).parameters[name].default
            is inspect.Parameter.empty
        )


@pytest.mark.parametrize("field", ["weight", "horizontal", "lift"])
def test_kn_wrapper_validates_before_unit_conversion(field):
    values = {"weight": 0.2, "horizontal": 0.04, "lift": 0.01}
    values[field] = True
    with pytest.raises(ValueError):
        simplified.check_lateral_stability(*values.values(), 0.5, 2)


@pytest.mark.parametrize("weight", [math.nan, math.inf, -math.inf, True])
def test_e305_available_weight_must_be_a_finite_number(weight):
    c = Case("extreme_wave", 0, 1, 8, 1, 1, 1, 0, 0.5, 0.5, 1, 1, "wave_velocity")
    with pytest.raises(ValueError):
        evaluate_case(c, diameter_m=1, rho_water_kg_m3=1, available_weight_n_m=weight)


def test_absolute_rejects_lost_contact_even_with_a_small_supplied_factor():
    """Assumed W=10<L=20: no unrestrained static contact at any multiplier."""
    assert not f109.absolute_stability_check(10, 1, 20, 0.5, 0.1).is_stable
