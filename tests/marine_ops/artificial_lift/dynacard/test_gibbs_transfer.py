"""Independent cases for the coupled rod solution (issue #1857).

Basis: Applied Mechanics of Solids (online edition read 2026-10-19), section 4.3.3, longitudinal
motion of a thin rod. The companion force equation follows by differentiating
U and using tensile F = -EA U_x with x downward and displacement upward.
All numbers below define synthetic boundary conditions, not material data.
The 1e-12 absolute numerical tolerance is a roundoff budget: for the 64-sample
cases, 64 * float64 epsilon * 70 lb is below 1e-12 lb. The 70 lb bound exceeds
the largest independently computed force in these cases. This is not a field
accuracy threshold.
"""

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.exceptions import PhysicsError
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    CardData,
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.physics import (
    DynacardPhysicsSolver,
)


def quarter_wave_context(samples=64):
    """Hand case: A=1 in², EA=1440 lb, a=720 in/s, L=180 in.

    At 1 Hz, q=omega*L/a=pi/2. Set u=2 cos(t), F=20+12 sin(t).
    Then U_L=-F_ac/(EA*k)=-3/pi sin(t), F_L=EA*k*U=8*pi cos(t).
    With density=144 and fluid density=0 lb/ft³, weight=15 lb,
    so mean bottom tension=20-15=5 lb. The stroke is 6/pi inches.
    """
    t = np.arange(samples) * (2 * np.pi / samples)
    return DynacardAnalysisContext(
        api14="SYNTHETIC",
        surface_card=CardData(
            position=(2 * np.cos(t)).tolist(), load=(20 + 12 * np.sin(t)).tolist()
        ),
        rod_string=[
            RodSection(
                diameter=2 / np.sqrt(np.pi),
                length=15,
                modulus_of_elasticity=1440,
                density=144,
                speed_of_sound=60,
                damping_factor=0,
            )
        ],
        pump=PumpProperties(diameter=1, depth=15),
        spm=60,
        fluid_density=0,
    ), t


def solve(context):
    with pytest.warns(UserWarning, match="comparison only"):
        return DynacardPhysicsSolver(context).solve_wave_equation().downhole_card


@pytest.mark.parametrize("samples", [63, 64])
def test_quarter_wave_hand_solution(samples):
    """The hand solution in quarter_wave_context checks both axes and DC."""
    ctx, t = quarter_wave_context(samples)
    card = solve(ctx)
    expected = -3 / np.pi * np.sin(t)
    np.testing.assert_allclose(card.position, expected - expected.min(), atol=1e-12)
    np.testing.assert_allclose(card.load, 5 + 8 * np.pi * np.cos(t), atol=1e-12)
    assert np.ptp(card.position) < np.ptp(ctx.surface_card.position)


def test_rippled_load_changes_shape():
    """q3=3*pi/2 gives cos(q3)=0: the surface load ripple cancels.

    With no displacement at harmonic 3, its pump force is zero. The remaining
    force is cos(t), orthogonal to both sin(t) and sin(3t) at these sample times.
    The correlation ceiling is the explicit regression requirement of #1857.
    """
    ctx, t = quarter_wave_context()
    ctx.surface_card.load = (20 + 12 * np.sin(t) + 3 * np.sin(3 * t)).tolist()
    card = solve(ctx)
    np.testing.assert_allclose(card.load, 5 + 8 * np.pi * np.cos(t), atol=1e-12)
    assert abs(np.corrcoef(ctx.surface_card.load, card.load)[0, 1]) < 0.99


def test_damped_complex_hand_solution():
    """Choose gamma*L=alpha+i*beta, alpha=ln(2), beta=pi/2.

    cosh(gamma*L)=0.75i; sinh(gamma*L)=1.25i, exactly from exp(ln 2).
    Let K=EA/L=8. For U=cos(t), F_ac=sin(t), the bottom Fourier
    force amplitude is 0.75+1.25*K*beta - i*1.25*K*alpha.
    The displacement amplitude is -1.25*alpha/(K*d)
    + i*(0.75+1.25*beta/(K*d)), where d=alpha²+beta².
    Solve gamma²=(-omega²+i*omega*c)/a² for the input a and c.
    """
    ctx, t = quarter_wave_context()
    alpha, beta, omega = np.log(2), np.pi / 2, 2 * np.pi
    section = ctx.rod_string[0]
    section.speed_of_sound = omega * section.length / np.sqrt(beta**2 - alpha**2)
    section.damping_factor = 2 * omega * alpha * beta / (beta**2 - alpha**2)
    ctx.surface_card.position = np.cos(t).tolist()
    ctx.surface_card.load = (20 + np.sin(t)).tolist()
    card = solve(ctx)
    k, d = 8, alpha**2 + beta**2
    expected_u = -1.25 * alpha / (k * d) * np.cos(t) - (
        0.75 + 1.25 * beta / (k * d)
    ) * np.sin(t)
    expected_f = 5 + (0.75 + 1.25 * k * beta) * np.cos(t) + 1.25 * k * alpha * np.sin(t)
    np.testing.assert_allclose(card.position, expected_u - expected_u.min(), atol=1e-12)
    np.testing.assert_allclose(card.load, expected_f, atol=1e-12)
    section.damping_factor = 0
    undamped = solve(ctx)
    assert not np.allclose(card.load, undamped.load)
    assert not np.allclose(card.position, undamped.position)


def test_taper_interface_hand_solution():
    """Two quarter-wave sections, same a and L, EA2=2*EA1.

    M(q=pi/2)=[[0,-1/(EA*k)],[EA*k,0]]. Thus M2*M1 has diagonal
    [-EA1/EA2,-EA2/EA1]=[-1/2,-2]. For U=2cos(t), F=20+12sin(t),
    the final AC position is -cos(t) and AC force is -24sin(t).
    Static weights are 15 and 30 lb, giving mean force 20-45=-25 lb.
    """
    ctx, t = quarter_wave_context()
    second = ctx.rod_string[0].model_copy(deep=True)
    second.diameter *= np.sqrt(2)
    ctx.rod_string.append(second)
    card = solve(ctx)
    expected_u = -np.cos(t)
    np.testing.assert_allclose(card.position, expected_u - expected_u.min(), atol=1e-12)
    np.testing.assert_allclose(card.load, -25 - 24 * np.sin(t), atol=1e-12)


def test_section_subdivision_preserves_solution():
    """exp(B*L/2)²=exp(B*L): an interface with unchanged EA has no reflection."""
    ctx, _ = quarter_wave_context()
    whole = solve(ctx)
    half = ctx.rod_string[0].model_copy(deep=True)
    half.length /= 2
    ctx.rod_string = [half, half.model_copy(deep=True)]
    split = solve(ctx)
    np.testing.assert_allclose(split.position, whole.position, atol=1e-12)
    np.testing.assert_allclose(split.load, whole.load, atol=1e-12)


def test_static_buoyancy_and_elastic_limit():
    """Constant F=20, weight=15: bottom force=5, no oscillatory motion.

    Doubling fluid density from zero to the rod density removes all net weight,
    so the bottom tension becomes the prescribed 20 lb without attenuation.
    """
    ctx, t = quarter_wave_context()
    ctx.surface_card.position = np.zeros_like(t).tolist()
    ctx.surface_card.load = np.full_like(t, 20).tolist()
    card = solve(ctx)
    np.testing.assert_allclose(card.position, 0, atol=1e-12)
    np.testing.assert_allclose(card.load, 5, atol=1e-12)
    ctx.fluid_density = 144
    np.testing.assert_allclose(solve(ctx).load, 20, atol=1e-12)


@pytest.mark.parametrize(
    "field,value",
    [
        ("diameter", 0),
        ("length", -1),
        ("modulus_of_elasticity", 0),
        ("speed_of_sound", 0),
        ("density", float("nan")),
        ("damping_factor", -1),
    ],
)
def test_invalid_section_rejected(field, value):
    ctx, _ = quarter_wave_context()
    setattr(ctx.rod_string[0], field, value)
    with pytest.raises(PhysicsError):
        DynacardPhysicsSolver(ctx).solve_wave_equation()


@pytest.mark.parametrize("invalid", ["empty_string", "spm", "fluid", "lengths", "nan"])
def test_invalid_boundary_rejected(invalid):
    ctx, _ = quarter_wave_context()
    if invalid == "empty_string":
        ctx.rod_string = []
    elif invalid == "spm":
        ctx.spm = 0
    elif invalid == "fluid":
        ctx.fluid_density = -1
    elif invalid == "lengths":
        ctx.surface_card.load.pop()
    else:
        ctx.surface_card.load[0] = float("nan")
    with pytest.raises(PhysicsError):
        DynacardPhysicsSolver(ctx).solve_wave_equation()


def test_overflow_raises_without_returning_a_card():
    """An unbounded decay rate must fail, not produce a finite-looking card.

    The maximum representable rate is a numerical stress input, not an
    engineering acceptance limit or material property.
    """
    ctx, _ = quarter_wave_context()
    ctx.rod_string[0].damping_factor = np.finfo(float).max
    solver = DynacardPhysicsSolver(ctx)
    with pytest.raises(PhysicsError, match="non-finite"):
        solver.solve_wave_equation()
    assert solver.results.downhole_card is None


def test_nyquist_component_uses_real_projection():
    """At harmonic 32, q=16*pi: C=1, S=0; alternating load is unchanged.

    The constant displacement has no AC contribution. The 15 lb static weight
    gives the same 5 lb mean as the other quarter-wave hand cases.
    """
    ctx, t = quarter_wave_context()
    alternating = (-1.0) ** np.arange(len(t))
    ctx.surface_card.position = np.zeros_like(t).tolist()
    ctx.surface_card.load = (20 + alternating).tolist()
    np.testing.assert_allclose(solve(ctx).load, 5 + alternating, atol=1e-12)
