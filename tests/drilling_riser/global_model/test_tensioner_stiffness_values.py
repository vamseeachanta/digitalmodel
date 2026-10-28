"""Independent force-derivative checks using arbitrary synthetic arithmetic inputs.

These values are not a tensioner design. Source for all expected stiffnesses:
let a line's ring-to-sheave vector after ring displacement x be
r(x) = (a*cos(phi) - x, a*sin(phi), h), with constant force magnitude T.
Then F_x(x) = T*r_x/|r| and the restoring tangent stiffness at x = 0 is
k_x = -dF_x/dx = (T/L)*(1 - u_x**2), where u_x = a*cos(phi)/L.

Choose four intact lines spaced by 90 degrees, vertical force 400 N, h = 4 m
and radial difference a = 3 m. Thus L = sqrt(3**2 + 4**2) = 5 m and each
line carries T = (400/4)/(4/5) = 125 N. The two x-aligned lines each give
(125/5)*(1 - (3/5)**2) = 16 N/m; the two y-aligned lines each give 25 N/m.
Their sum is 2*16 + 2*25 = 82 N/m, independently of the implementation.

The relative tolerance 1e-12 allows floating-point trigonometry and summation
roundoff; it is not an engineering uncertainty allowance. No absolute
fallback tolerance is used. These checks cover intact, equally spaced lines
with positive vertical separation and constant line-force magnitude only.
"""

import pytest

from digitalmodel.drilling_riser.global_model.hand_checks import (
    tensioner_lateral_stiffness_n_m,
)
from digitalmodel.drilling_riser.global_model.spec import (
    Contents,
    FlexJoint,
    GlobalEnvironment,
    LineSection,
    RiserGlobalModelSpec,
    TensionRing,
    Tensioners,
)


def _synthetic_spec(
    *,
    first_azimuth_deg: float = 0.0,
    ring_z_m: float = 0.0,
    radial_difference_m: float = 3.0,
    total_vertical_force_n: float = 400.0,
) -> RiserGlobalModelSpec:
    """Construct the minimum topology through the validating model interface.

    The tensioner inputs come from the module's hand calculation. Other inputs
    are arbitrary synthetic placeholders unused by this stiffness check:
    one 1 m section per required line, unit positive stiffness/inertia/density,
    and zero mass, volume, bore and hydrodynamic coefficients. The ring attach
    radius is 1 m; sheave radius adds the chosen radial difference. Upper and
    lower pivots are 1 m above/below the ring, and the datum is another 1 m
    below. A 10 m water depth accommodates these translated geometries.
    """
    def section(name: str) -> LineSection:
        return LineSection(
            name=name,
            length_m=1.0,
            segment_length_m=1.0,
            mass_per_m_kg=0.0,
            displaced_volume_per_m_m3=0.0,
            bore_id_m=0.0,
            ei_nm2=1.0,
            ea_n=1.0,
            drag_diameter_m=0.0,
            cd_normal=0.0,
            ca_normal=0.0,
        )

    return RiserGlobalModelSpec(
        name="synthetic-stiffness-arithmetic",
        environment=GlobalEnvironment(water_depth_m=10.0, water_density_kg_m3=1.0),
        contents=Contents(density_kg_m3=0.0, pressure_ref_z_m=0.0),
        upper_flex_joint=FlexJoint(
            pivot_z_m=ring_z_m + 1.0, rotational_stiffness_nm_per_rad=1.0,
        ),
        lower_flex_joint=FlexJoint(
            pivot_z_m=ring_z_m - 1.0, rotational_stiffness_nm_per_rad=1.0,
        ),
        tension_ring=TensionRing(
            mass_kg=0.0,
            volume_m3=0.0,
            moments_of_inertia_kgm2=(1.0, 1.0, 1.0),
            z_static_m=ring_z_m,
        ),
        tensioners=Tensioners(
            representation="lines",
            count=4,
            sheave_radius_m=1.0 + radial_difference_m,
            sheave_z_m=ring_z_m + 4.0,
            ring_attach_radius_m=1.0,
            total_vertical_tension_n=total_vertical_force_n,
            first_azimuth_deg=first_azimuth_deg,
            failed_count=0,
        ),
        inner_barrel=[section("synthetic-inner")],
        riser=[section("synthetic-riser")],
        stack=[section("synthetic-stack")],
        wellhead_datum_z_m=ring_z_m - 2.0,
    )


def test_four_line_stiffness_matches_force_derivative():
    """Two contributions of 16 N/m and two of 25 N/m give 82 N/m above."""
    stiffness = tensioner_lateral_stiffness_n_m(_synthetic_spec())
    assert stiffness == pytest.approx(82.0, rel=1e-12, abs=0.0)


def test_stiffness_rotation_translation_and_load_scaling():
    """Independent consequences of the force derivative in the module docstring.

    At 45 degrees the four cos(phi)**2 values are each 1/2, still summing to
    2: (125/5)*(4 - (3/5)**2*2) = 82 N/m. An arbitrary common elevation
    shift of -7 m leaves h = 4 m, giving 82 N/m. Doubling the vertical force
    to 800 N doubles each line force and tangent stiffness: 164 N/m.
    """
    rotated = _synthetic_spec(first_azimuth_deg=45.0)
    translated = _synthetic_spec(ring_z_m=-7.0)
    doubled = _synthetic_spec(total_vertical_force_n=800.0)

    assert tensioner_lateral_stiffness_n_m(rotated) == pytest.approx(
        82.0, rel=1e-12, abs=0.0,
    )
    assert tensioner_lateral_stiffness_n_m(translated) == pytest.approx(
        82.0, rel=1e-12, abs=0.0,
    )
    assert tensioner_lateral_stiffness_n_m(doubled) == pytest.approx(
        164.0, rel=1e-12, abs=0.0,
    )


def test_vertical_line_limit_matches_total_force_over_height():
    """At zero radial difference u_x = 0: sum(T/L) = 400/4 = 100 N/m."""
    vertical = _synthetic_spec(radial_difference_m=0.0)
    assert tensioner_lateral_stiffness_n_m(vertical) == pytest.approx(
        100.0, rel=1e-12, abs=0.0,
    )
