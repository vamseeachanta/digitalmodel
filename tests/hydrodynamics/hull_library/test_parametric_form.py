"""Parametric monohull checks with analytical and consumer comparators.

Closed-form comparators are a rectangular prism, a separable parabolic Wigley
surface, and the half-circle section; target tests integrate actual offsets.
"""

from time import perf_counter

import numpy as np
import pytest
from scipy.integrate import simpson

from digitalmodel.hydrodynamics.hull_library.curvature_screen import (
    hullprod_available,
    screen_profile,
)
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)
from digitalmodel.hydrodynamics.hull_library.parametric_form import (
    FormReport,
    MonohullFormParameters,
    form_report,
    generate_profile,
    midship_area,
    midship_section,
    sectional_area_curve,
    station_offsets,
    sweep_forms,
)
from digitalmodel.hydrodynamics.hull_library.parametric_hull import (
    ParametricRange,
    form_space_profiles,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile, HullType
from digitalmodel.visualization.design_tools.hull_hydrostatics import HullHydrostatics

needs_hullprod = pytest.mark.skipif(
    not hullprod_available(), reason="optional hullprod extra not installed"
)


def parameters(**changes):
    values = {"length_bp": 100.0, "beam": 20.0, "draft": 10.0, "depth": 14.0}
    values.update(changes)
    return MonohullFormParameters(**values)


def test_box_closed_form():
    """Closed-form comparator: rectangular prism has Cb=Cp=Cm=Cwp=1."""
    params = parameters(box=True, cb=1.0, bilge_radius_fraction=0.0)
    profile = generate_profile(params)
    report = form_report(params)
    for coefficient in (report.cb, report.cp, report.cm, report.cwp):
        assert coefficient == pytest.approx(1.0, abs=1e-6)
    assert report.displaced_volume == pytest.approx(20000.0, rel=1e-6)
    assert HullHydrostatics(profile).compute_displaced_volume() == pytest.approx(
        20000.0
    )


def test_wigley_closed_form():
    """Closed-form comparator: separable parabolas give Cb=(2/3)^2=4/9."""
    params = parameters(wigley=True, cb=4 / 9, bilge_radius_fraction=0.0)
    report = form_report(params)
    assert report.cb == pytest.approx(4 / 9, rel=0.01)
    assert report.cm == pytest.approx(2 / 3, rel=0.01)
    assert report.lcb_fraction == pytest.approx(0.0, abs=0.002)


def test_semicircle_midship_closed_form():
    """Closed-form comparator: half-circle area/(beam*draft) equals pi/4."""
    params = parameters(cb=0.55, bilge_radius_fraction=1.0)
    assert midship_area(params) / (params.beam * params.draft) == pytest.approx(
        np.pi / 4, abs=1e-6
    )
    z = np.linspace(0.0, params.draft, 10001)
    y = midship_section(params, z)
    assert 2 * simpson(y, x=z) == pytest.approx(midship_area(params), rel=1e-5)
    assert midship_section(params, params.draft) == pytest.approx(params.beam / 2)


@pytest.mark.parametrize("cb", [0.55, 0.70, 0.85])
@pytest.mark.parametrize("lcb", [-0.02, 0.0, 0.02])
def test_target_grid(cb, lcb):
    """Numerical comparator: Simpson-integrated generated station offsets."""
    params = parameters(cb=cb, lcb_fraction=lcb)
    profile = generate_profile(params)
    report = form_report(params)
    observed = form_report(profile)
    assert report.cb == pytest.approx(cb, rel=0.005)
    assert report.lcb_fraction == pytest.approx(lcb, abs=0.002)
    assert observed.displaced_volume == pytest.approx(report.displaced_volume)
    assert profile.block_coefficient == pytest.approx(report.cb)
    assert report.hydrostatic_volume == pytest.approx(report.displaced_volume, rel=0.01)
    assert report.cp == pytest.approx(report.cb / report.cm)
    assert report.targets["cb"] == cb
    assert report.targets["lcb_fraction"] == lcb
    assert observed.targets is None


def test_sac_integral_and_positive_forward_centroid():
    """Numerical comparator: independent dense Simpson integral of the SAC."""
    params = parameters(cb=0.70, lcb_fraction=0.02)
    x = np.linspace(0.0, params.length_bp, 4001)
    area = sectional_area_curve(params, x)
    volume = simpson(area, x=x)
    lcb = simpson(x * area, x=x) / volume / params.length_bp - 0.5
    assert volume == pytest.approx(params.cb * 20000, rel=1e-5)
    assert lcb == pytest.approx(params.lcb_fraction, abs=1e-5)
    assert np.isfinite(area).all() and np.min(area) >= 0


@pytest.mark.parametrize(
    "changes,match",
    [
        ({"cb": 0.96}, "cb"),
        ({"cb": 0.9, "bilge_radius_fraction": 1.0}, "cb"),
        ({"lcb_fraction": 0.45}, "lcb_fraction"),
        ({"depth": 5.0}, "depth"),
        ({"length_bp": float("inf")}, "length_bp"),
        ({"beam": float("nan")}, "beam"),
        ({"transom_fraction": 1.0}, "transom_fraction"),
        ({"n_stations": 4}, "n_stations"),
        ({"n_stations": 40}, "n_stations"),
        ({"n_waterlines": 4}, "n_waterlines"),
        ({"deadrise_deg": 31.0}, "deadrise_deg"),
        ({"flare_deg": -16.0}, "flare_deg"),
    ],
)
def test_invalid_or_unreachable_parameters(changes, match):
    with pytest.raises(ValueError, match=match):
        generate_profile(parameters(**changes))


def test_station_geometry_and_end_regularization():
    params = parameters()
    profile = generate_profile(params)
    assert len(profile.stations) == params.n_stations
    assert profile.hull_type == HullType.SHIP
    assert profile.source == "parametric_form"
    for station in profile.stations:
        offsets = np.asarray(station.waterline_offsets)
        assert len(offsets) == params.n_waterlines
        assert np.isfinite(offsets).all() and offsets.min() >= 0
        assert np.all(np.diff(offsets[:, 0]) > 0)
        assert np.all(np.diff(offsets[:, 1]) >= -1e-12)
    middle = np.asarray(station_offsets(params, params.length_bp / 2))
    assert middle[-1, 1] == pytest.approx(params.beam / 2)
    for x in (0.0, params.length_bp):
        assert station_offsets(params, x)[-1][1] == pytest.approx(
            0.005 * params.beam / 2
        )
    restored = HullProfile.from_yaml_dict(profile.to_yaml_dict())
    assert restored == profile


def test_mesh_and_hydrostatic_consumers():
    """Consumer comparator: panel extents and independent trapezoidal volume."""
    params = parameters()
    profile = generate_profile(params)
    mesh = HullMeshGenerator().generate(
        profile, MeshGeneratorConfig(target_panels=2000)
    )
    extents = np.ptp(mesh.vertices, axis=0)
    assert extents == pytest.approx(
        [params.length_bp, params.beam / 2, params.draft], rel=0.01
    )
    volume = HullHydrostatics(profile).compute_displaced_volume()
    assert volume == pytest.approx(form_report(profile).displaced_volume, rel=0.01)


def test_sweep_reports_and_iterator():
    params = parameters()
    ranges = {
        "cb": ParametricRange(min=0.65, max=0.75, steps=2),
        "lcb_fraction": ParametricRange(min=-0.01, max=0.01, steps=2),
    }
    started = perf_counter()
    rows = sweep_forms(params, ranges, screen=False)
    elapsed = perf_counter() - started
    assert elapsed < 5.0
    assert len(rows) == 4
    assert all(isinstance(row["report"], FormReport) for row in rows)
    assert {tuple(sorted(row["parameters"].items())) for row in rows} == {
        (("cb", cb), ("lcb_fraction", lcb))
        for cb in (0.65, 0.75)
        for lcb in (-0.01, 0.01)
    }
    assert all(row.get("signature") is None for row in rows)
    profiles = list(form_space_profiles(params, ranges))
    assert len(profiles) == len({name for name, _ in profiles}) == 4
    assert all(name == profile.name for name, profile in profiles)
    assert params.cb == 0.7 and params.lcb_fraction == 0.0


def test_unknown_sweep_parameter_is_rejected():
    with pytest.raises(ValueError, match="unknown_parameter"):
        sweep_forms(
            parameters(), {"unknown_parameter": ParametricRange(min=1, max=2, steps=2)}
        )


@needs_hullprod
def test_brep_export_consumer(tmp_path):
    from digitalmodel.hydrodynamics.hull_library.hull_surface_brep import (
        profile_to_step,
    )

    path = profile_to_step(generate_profile(parameters()), tmp_path / "generated.step")
    assert path.is_file() and path.stat().st_size > 1000


@needs_hullprod
def test_full_midbody_has_more_developable_area():
    """Regression comparator: fuller generated form at equal panel density."""
    config = MeshGeneratorConfig(target_panels=7225)
    full = screen_profile(
        generate_profile(parameters(cb=0.85, parallel_midbody_fraction=0.5)), config
    ).signature
    fine = screen_profile(
        generate_profile(parameters(cb=0.55, parallel_midbody_fraction=0.1)), config
    ).signature
    assert full.a_flat + full.a_single > fine.a_flat + fine.a_single
    assert 0 <= full.a_saddle <= 1 and 0 <= fine.a_saddle <= 1


@needs_hullprod
def test_rounder_bilge_increases_single_curvature():
    """Regression comparator: larger circular bilge has more cylindrical area."""
    config = MeshGeneratorConfig(target_panels=7225)
    signatures = [
        screen_profile(
            generate_profile(
                parameters(
                    cb=0.85, parallel_midbody_fraction=0.6, bilge_radius_fraction=radius
                )
            ),
            config,
        ).signature
        for radius in (0.1, 0.6)
    ]
    assert signatures[1].a_single > signatures[0].a_single


@needs_hullprod
def test_screened_sweep_carries_signature():
    rows = sweep_forms(
        parameters(),
        {},
        screen=True,
        mesh_config=MeshGeneratorConfig(target_panels=600),
    )
    assert len(rows) == 1
    assert rows[0]["signature"].lref == 100.0


def test_pointed_ends_have_no_pinched_station_after_floor():
    """Geometry regression: endpoint regularization must remain continuous inward."""
    profile = generate_profile(parameters(cb=0.55))
    widths = np.array([s.waterline_offsets[-1][1] for s in profile.stations])
    assert np.all(np.diff(widths[:21]) >= -1e-10)
    assert np.all(np.diff(widths[20:]) <= 1e-10)
    assert widths.min() >= 0.05 - 1e-10


def test_transom_width_area_and_target():
    params = parameters(cb=0.72, transom_fraction=0.9, lcb_fraction=-0.10)
    offsets = np.asarray(station_offsets(params, 0.0))
    assert offsets[-1, 1] == pytest.approx(9.0)
    assert 2 * simpson(offsets[:, 1], x=offsets[:, 0]) == pytest.approx(
        0.81 * midship_area(params), rel=0.001
    )
    report = form_report(params)
    assert report.cb == pytest.approx(0.72, rel=0.005)
    assert report.lcb_fraction == pytest.approx(-0.10, abs=0.002)


def test_sweep_without_optional_dependency(monkeypatch):
    from digitalmodel.hydrodynamics.hull_library import curvature_screen

    monkeypatch.setattr(curvature_screen, "hullprod_available", lambda: False)

    def forbidden(*args, **kwargs):
        raise AssertionError("optional screener must not be called")

    monkeypatch.setattr(curvature_screen, "screen_profile", forbidden)
    rows = sweep_forms(parameters(), {}, screen=True)
    assert len(rows) == 1 and rows[0]["signature"] is None


@pytest.mark.parametrize(
    "changes", [{"deadrise_deg": 10}, {"flare_deg": 10}, {"flare_deg": -2}]
)
def test_angled_section_area_and_volume(changes):
    """Numerical comparator: fine section quadrature versus analytic line/arc area."""
    params = parameters(**changes)
    z = np.linspace(0, params.draft, 20001)
    assert 2 * simpson(midship_section(params, z), x=z) == pytest.approx(
        midship_area(params), rel=1e-5
    )
    report = form_report(params)
    assert report.cb == pytest.approx(params.cb, rel=0.005)
    assert report.hydrostatic_volume == pytest.approx(report.displaced_volume, rel=0.01)
    if params.flare_deg < 0:
        assert midship_section(params, params.draft - 0.1) > midship_section(
            params, params.draft
        )


def test_unresolved_transom_rejects_inadequate_waterline_grid():
    with pytest.raises(ValueError, match="transom_fraction.*n_waterlines"):
        generate_profile(parameters(transom_fraction=0.001))


def test_unresolved_transom_requires_more_stations():
    """Numerical comparator: narrow SAC transitions must not miss the Cb target."""
    with pytest.raises(ValueError, match="n_stations"):
        generate_profile(parameters(cb=0.55, transom_fraction=0.6))


def test_refined_transom_attains_targets():
    """Numerical comparator: resolve the narrow transition before accepting it."""
    params = parameters(cb=0.55, transom_fraction=0.6, n_stations=161)
    report = form_report(params)
    assert report.cb == pytest.approx(params.cb, rel=0.005)
    assert report.lcb_fraction == pytest.approx(params.lcb_fraction, abs=0.002)
    assert report.hydrostatic_volume == pytest.approx(report.displaced_volume, rel=0.01)


def test_unresolved_circle_requires_more_waterlines():
    """Consumer comparator: five circle samples disagree with trapezoidal volume."""
    with pytest.raises(ValueError, match="n_waterlines"):
        generate_profile(parameters(cb=0.55, bilge_radius_fraction=1.0, n_waterlines=5))


def test_close_sweep_values_have_distinct_variation_ids():
    ranges = {"cb": ParametricRange(min=0.7, max=0.700001, steps=2)}
    profiles = list(form_space_profiles(parameters(), ranges))
    assert len({name for name, _ in profiles}) == 2
    assert all(name == profile.name for name, profile in profiles)
