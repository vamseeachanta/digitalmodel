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
    screen_panel_mesh,
    screen_profile,
    screen_trimesh,
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


def test_station_geometry_and_closed_ends():
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
        assert np.asarray(station_offsets(params, x))[:, 1] == pytest.approx(0)
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


def test_pointed_ends_close_monotonically():
    """Geometry regression: closure is zero at the tips and monotone inward."""
    profile = generate_profile(parameters(cb=0.55))
    widths = np.array([s.waterline_offsets[-1][1] for s in profile.stations])
    assert np.all(np.diff(widths[:21]) >= -1e-10)
    assert np.all(np.diff(widths[20:]) <= 1e-10)
    assert widths[[0, -1]] == pytest.approx(0)


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


@pytest.mark.parametrize("count", [9, 21, 41, 161, 1001])
def test_station_grid_spacing_limits(count):
    profile = generate_profile(
        parameters(n_stations=count, box=True, cb=1, bilge_radius_fraction=0)
    )
    spacing = np.diff([s.x_position for s in profile.stations])
    ratios = spacing[1:] / spacing[:-1]
    assert min(spacing[0], spacing[-1]) >= 100 / (4 * count)
    assert np.max(np.maximum(ratios, 1 / ratios)) <= 2
    assert spacing[0] < spacing[len(spacing) // 2]


@pytest.mark.parametrize("cb", [0.55, 0.70, 0.85])
@pytest.mark.parametrize("angle", [5, 25, 60])
def test_end_waterline_tangency_and_targets(cb, angle):
    params = parameters(cb=cb, entrance_angle_deg=angle, run_angle_deg=angle)
    step = params.length_bp * 1e-8
    slope = np.tan(np.deg2rad(angle))
    for x in (step, params.length_bp - step):
        assert station_offsets(params, x)[-1][1] / step == pytest.approx(
            slope, rel=0.005
        )
    report = form_report(params)
    assert report.cb == pytest.approx(cb, rel=0.005)
    assert report.lcb_fraction == pytest.approx(0, abs=0.002)


@pytest.mark.parametrize("field", ["entrance_angle_deg", "run_angle_deg"])
@pytest.mark.parametrize("angle", [4.9, 60.1, float("nan"), float("inf")])
def test_invalid_end_angles(field, angle):
    with pytest.raises(ValueError, match=field):
        parameters(**{field: angle})


def test_wigley_exact_offsets_and_angles():
    params = parameters(wigley=True, cb=4 / 9, bilge_radius_fraction=0, draft=8)
    profile = generate_profile(params)
    for station in profile.stations:
        z, y = np.asarray(station.waterline_offsets).T
        expected = (
            10 * (1 - (2 * station.x_position / 100 - 1) ** 2) * (1 - (1 - z / 8) ** 2)
        )
        assert y == pytest.approx(expected, abs=1e-12)
    expected_angle = np.rad2deg(np.arctan(2 * params.beam / params.length_bp))
    assert params.entrance_angle_deg == pytest.approx(expected_angle)
    assert params.run_angle_deg == pytest.approx(expected_angle)
    with pytest.raises(ValueError, match="wigley.*angle"):
        parameters(
            wigley=True, cb=4 / 9, bilge_radius_fraction=0, entrance_angle_deg=40
        )


def test_zero_breadth_ends_mesh_consumer():
    profile = generate_profile(
        parameters(wigley=True, cb=4 / 9, bilge_radius_fraction=0, draft=8)
    )
    assert all(
        y == 0
        for s in (profile.stations[0], profile.stations[-1])
        for _, y in s.waterline_offsets
    )
    mesh = HullMeshGenerator().generate(
        profile, MeshGeneratorConfig(target_panels=8000)
    )
    assert mesh.n_panels == 7225
    assert np.isfinite(mesh.vertices).all()
    assert np.all(mesh.panel_areas > 0)


def _analytic_wigley(length, breadth, draft, nx=425, nz=17):
    trimesh = pytest.importorskip("trimesh")
    x, z = np.meshgrid(
        np.linspace(-length / 2, length / 2, nx + 1),
        np.linspace(-draft, 0, nz + 1),
        indexing="ij",
    )
    y = breadth / 2 * (1 - (2 * x / length) ** 2) * (1 - (z / draft) ** 2)
    vertices = np.column_stack((x.ravel(), y.ravel(), z.ravel()))
    faces, row = [], nz + 1
    for i in range(nx):
        for j in range(nz):
            a, b, c, d = (
                i * row + j,
                i * row + j + 1,
                (i + 1) * row + j,
                (i + 1) * row + j + 1,
            )
            faces.extend(
                ([a, c, b], [b, c, d]) if (i + j) % 2 else ([a, c, d], [a, d, b])
            )
    return trimesh.Trimesh(vertices, np.asarray(faces), process=False)


@needs_hullprod
@pytest.mark.parametrize("metric", ["signature", "end_share"])
@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason=(
        "Shortest split: I_D=7.914447 vs triangle 6.599658 (19.9%); "
        "end share=0.360858 > 0.20; residual generated-mesh sensitivity (issue 2253)"
    ),
)
def test_wigley_mesh_signature_and_end_share(metric):
    params = parameters(wigley=True, cb=4 / 9, bilge_radius_fraction=0, draft=8)
    mesh = HullMeshGenerator().generate(
        generate_profile(params), MeshGeneratorConfig(target_panels=8000)
    )
    assert mesh.n_panels == 7225
    result = screen_panel_mesh(mesh, lref=100, keep_fields=True, expand_symmetry=False)
    analytic = screen_trimesh(_analytic_wigley(100, 20, 8), lref=100).signature
    if metric == "signature":
        assert result.signature.I_D == pytest.approx(analytic.I_D, rel=0.10)
        return
    fields = result.fields
    valid = fields.valid & np.isfinite(fields.K)
    ends = (fields.vertices[:, 0] < 5) | (fields.vertices[:, 0] > 95)
    share = np.abs(fields.K[valid & ends]).sum() / np.abs(fields.K[valid]).sum()
    assert share < 0.20


@needs_hullprod
@pytest.mark.parametrize("beam,draft,expected", [(10, 6.25, 4.31), (20, 8, 7.119738)])
def test_wigley_brep_signature(beam, draft, expected):
    params = parameters(
        wigley=True, cb=4 / 9, bilge_radius_fraction=0, beam=beam, draft=draft
    )
    signature = screen_profile(
        generate_profile(params), representation="brep"
    ).signature
    assert signature.status == "valid"
    assert signature.I_D == pytest.approx(expected, rel=0.05)


@pytest.mark.parametrize("explicit_none", [False, True])
def test_wigley_sweep_rederives_default_angles(explicit_none):
    angles = (
        {"entrance_angle_deg": None, "run_angle_deg": None} if explicit_none else {}
    )
    base = parameters(wigley=True, cb=4 / 9, bilge_radius_fraction=0, **angles)
    rows = sweep_forms(base, {"beam": ParametricRange(min=10, max=20, steps=2)})
    for row in rows:
        targets = row["report"].targets
        expected = np.rad2deg(np.arctan(2 * targets["beam"] / 100))
        assert targets["entrance_angle_deg"] == pytest.approx(expected)
        assert targets["run_angle_deg"] == pytest.approx(expected)


@pytest.mark.parametrize("explicit_angle", [None, 20])
def test_fullness_sweep_angle_defaults_and_overrides(explicit_angle):
    base = parameters(entrance_angle_deg=explicit_angle)
    rows = sweep_forms(base, {"bow_fullness": ParametricRange(min=2, max=4, steps=2)})
    for row in rows:
        fresh = parameters(
            bow_fullness=row["parameters"]["bow_fullness"],
            entrance_angle_deg=explicit_angle,
        )
        assert row["report"].targets["entrance_angle_deg"] == fresh.entrance_angle_deg


def test_transom_end_tangency():
    params = parameters(
        cb=0.72,
        transom_fraction=0.9,
        lcb_fraction=-0.10,
        run_angle_deg=25,
        entrance_angle_deg=40,
    )
    step = 1e-6
    start = station_offsets(params, 0)[-1][1]
    assert start == pytest.approx(9)
    assert (station_offsets(params, step)[-1][1] - start) / step == pytest.approx(
        np.tan(np.deg2rad(25)), rel=0.005
    )
    report = form_report(params)
    assert report.cb == pytest.approx(0.72, rel=0.005)
    assert report.lcb_fraction == pytest.approx(-0.10, abs=0.002)
