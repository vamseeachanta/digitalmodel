"""Wave extent tests using synthetic inputs and independent geometry.

Basis: Ship wakes: Kelvin or Mach angle? (2013), arXiv:1304.2653,
pp. 1–2: lambda = 2*pi*U**2/g and sin(alpha) = 1/3. Hence
tan(alpha) = 1/sqrt(8). All wake counts and dimensions below are synthetic
caller inputs, not acceptance recommendations. pytest.approx is used only
for floating point arithmetic, not as an engineering acceptance tolerance.
"""

import json
import math
import runpy
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.hull_case import (
    HullCaseConfig,
    build_hull_case,
    derive_hull_case,
)
from digitalmodel.solvers.openfoam.hull_case_physics import CellBudgetError
from digitalmodel.solvers.openfoam.hull_domain import (
    build_hull_domain,
    refinement_boxes,
)
from digitalmodel.solvers.openfoam.hull_free_surface import GRAVITY
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest
from digitalmodel.solvers.openfoam.hull_wave_extent import (
    WaveExtentError,
    derive_wave_extent,
)


@pytest.fixture
def hull(manifest_dict):
    return HullManifest.from_dict(manifest_dict)


def extent(hull, speed=1.0, count=2.0, **domain_options):
    domain = build_hull_domain(hull, **domain_options)
    boxes = refinement_boxes(hull, domain)
    return derive_wave_extent(hull, domain, boxes, speed, count)


def config(hull, stl_file, **changes):
    return HullCaseConfig(
        manifest=hull,
        stl_path=stl_file,
        velocity=1.0,
        ranks=8,
        free_surface_wake_wavelengths=2.0,
        **changes,
    )


def test_hand_calculated_wake_and_kelvin_width(hull):
    """At U=1, lambda=2*pi/g; N=2 gives aft reach 4*pi/g.

    The synthetic hull stern is -0.05, bow 6.15, half beam 0.45 m.
    Aft half width = 0.45 + (6.20 + 4*pi/g)/sqrt(8) m.
    These are substitutions into the cited equations, independent of the API.
    """
    wave = extent(hull)
    reach = 4 * math.pi / GRAVITY
    assert wave.wake_length_m == pytest.approx(reach)
    assert wave.x_aft_m == pytest.approx(-0.05 - reach)
    assert wave.half_width_aft_m == pytest.approx(0.45 + (6.20 + reach) / math.sqrt(8))
    assert wave.kelvin_half_angle_deg == pytest.approx(math.degrees(math.asin(1 / 3)))


def test_same_hull_different_speeds_move_extent_quadratically(hull):
    slow, fast = extent(hull), extent(hull, speed=2.0)
    assert fast.wake_length_m == pytest.approx(4 * slow.wake_length_m)
    assert fast.x_aft_m < slow.x_aft_m
    assert fast.half_width_aft_m > slow.half_width_aft_m


@pytest.mark.parametrize("bad", [0.0, -1.0, float("nan"), float("inf")])
def test_invalid_required_extent_is_refused(hull, bad):
    with pytest.raises(WaveExtentError, match="wake_wavelengths"):
        extent(hull, count=bad)


@pytest.mark.parametrize("bad", [0.0, -1.0, float("nan"), float("inf")])
def test_invalid_speed_is_refused(hull, bad):
    with pytest.raises(WaveExtentError, match="velocity"):
        extent(hull, speed=bad)


def test_omitted_requirement_is_explicitly_not_evaluated(hull, stl_file, tmp_path):
    cfg = config(hull, stl_file).replace(free_surface_wake_wavelengths=None)
    case = build_hull_case(cfg, tmp_path)
    fs = json.loads((case / "case_provenance.json").read_text())["mesh"]["free_surface"]
    assert fs["extent"]["status"] == "NOT_EVALUATED"
    assert fs["extent"]["required_wake_wavelengths"] is None
    assert "required input" in fs["extent"]["reason"]
    assert not list((case / "constant" / "triSurface").glob("waveExtent*.stl"))


@pytest.mark.parametrize(
    "options, binding",
    [
        ({"downstream_lpp": 0.3}, "downstream_lpp"),
        ({"lateral_lpp": 0.4}, "lateral_lpp"),
    ],
)
def test_domain_shortfall_refuses_before_any_write(
    hull, stl_file, tmp_path, options, binding
):
    cfg = config(hull, stl_file).replace(free_surface_wake_wavelengths=4.0, **options)
    with pytest.raises(WaveExtentError, match=binding):
        build_hull_case(cfg, tmp_path / "refused")
    assert not (tmp_path / "refused").exists()


def test_required_region_is_added_at_every_stage_and_recorded(hull, stl_file, tmp_path):
    cfg = config(hull, stl_file)
    case = build_hull_case(cfg, tmp_path)
    derivation = derive_hull_case(cfg)
    fs = json.loads((case / "case_provenance.json").read_text())["mesh"]["free_surface"]
    assert fs["extent"]["status"] == "PASS"
    assert fs["extent"]["achieved_wake_wavelengths"] >= 2.0
    assert fs["cells_per_wavelength"] >= fs["required_cells_per_wavelength"]
    for stage in range(1, len(derivation.boxes) + 1):
        text = (case / "system" / f"topoSetDict.{stage}").read_text()
        assert "source  boxToCell" in text
        assert "source  surfaceToCell" in text
        assert "action  add" in text
        assert f'file    "constant/triSurface/waveExtent{stage}.stl";' in text
        assert "includeCut true;" in text
        assert "includeInside true;" in text
        assert "@" not in text
        surface = case / "constant" / "triSurface" / f"waveExtent{stage}.stl"
        vertices = [
            tuple(map(float, line.split()[1:]))
            for line in surface.read_text().splitlines()
            if line.strip().startswith("vertex ")
        ]
        emitted_reach = hull.bbox_min_m[0] - min(v[0] for v in vertices)
        assert fs["extent"]["achieved_wake_wavelengths"] == pytest.approx(
            emitted_reach / fs["wavelength_m"]
        )
        assert len(vertices) == 36  # Twelve triangles on a closed quadrilateral prism.
        edges = {}
        for i in range(0, len(vertices), 3):
            tri = vertices[i : i + 3]
            for a, b in zip(tri, tri[1:] + tri[:1]):
                edges[(a, b)] = edges.get((a, b), 0) + 1
        assert all(edges.get((b, a)) == n for (a, b), n in edges.items())
        # A trapezoid widens aft; its forward side remains at half beam.
        forward = [v for v in vertices if v[0] == max(p[0] for p in vertices)]
        assert min(v[1] for v in forward) == pytest.approx(-hull.half_beam_m)
        assert min(v[1] for v in vertices) < min(v[1] for v in forward)
        assert min(v[2] for v in vertices) == pytest.approx(
            derivation.boxes[stage - 1][0][2]
        )
        assert max(v[2] for v in vertices) == pytest.approx(
            derivation.boxes[stage - 1][1][2]
        )


def test_wave_region_cost_is_visible_and_a_cap_is_still_enforced(hull, stl_file):
    cfg = config(hull, stl_file)
    legacy = derive_hull_case(cfg.replace(free_surface_wake_wavelengths=None))
    wave = derive_hull_case(cfg)
    assert wave.budget.estimated_cells > legacy.budget.estimated_cells
    with pytest.raises(CellBudgetError):
        derive_hull_case(cfg.replace(max_global_cells=legacy.budget.estimated_cells))


def test_translated_hull_uses_actual_stern_and_bow(manifest_dict):
    """Synthetic translation by 1 m must translate x, preserving the reach."""
    original = HullManifest.from_dict(manifest_dict)
    for key in ("bbox_min_m", "bbox_max_m"):
        manifest_dict[key][0] += 1.0
    moved = extent(HullManifest.from_dict(manifest_dict))
    base = extent(original)
    assert moved.x_aft_m == pytest.approx(base.x_aft_m + 1.0)
    assert moved.half_width_aft_m == pytest.approx(base.half_width_aft_m)


def test_outside_point_is_inside_domain_beyond_wave_prism(hull, stl_file, tmp_path):
    """The surface selector needs an exterior seed strictly inside the mesh."""
    import re

    cfg = config(hull, stl_file)
    domain = derive_hull_case(cfg).domain
    case = build_hull_case(cfg, tmp_path)
    text = (case / "system" / "topoSetDict.1").read_text()
    point = tuple(
        map(float, re.search(r"outsidePoints \(\(([^)]+)\)\)", text)[1].split())
    )
    assert hull.bbox_max_m[0] < point[0] < domain.x_inlet
    assert domain.y_side < point[1] < domain.y_centreplane
    assert domain.z_levels[0] < point[2] < domain.z_levels[-1]


def test_emitted_wake_changes_at_two_speeds(hull, stl_file, tmp_path):
    """At speeds 1 and 2, the emitted aft reach ratio must be 4 (U squared)."""
    aft = []
    for speed in (1.0, 2.0):
        cfg = config(hull, stl_file).replace(velocity=speed, name=f"speed{speed}")
        case = build_hull_case(cfg, tmp_path)
        text = (case / "constant" / "triSurface" / "waveExtent6.stl").read_text()
        xs = [
            float(line.split()[1])
            for line in text.splitlines()
            if line.strip().startswith("vertex ")
        ]
        aft.append(hull.bbox_min_m[0] - min(xs))
    assert aft[1] == pytest.approx(4 * aft[0])


@pytest.mark.parametrize("speed, count", [(1e-300, 2.0), (1e308, 2.0), (2.0, 1e308)])
def test_numeric_range_is_refused(hull, speed, count):
    with pytest.raises(WaveExtentError):
        extent(hull, speed=speed, count=count)


def test_achieved_reach_uses_emitted_coordinates(hull):
    """A positive request below the coordinate precision still covers its count.

    The synthetic tiny count exercises outward rounding; there is no physical
    acceptance limit here. The wavelength and emitted stern-to-aft distance
    independently determine how many wavelengths are actually covered.
    """
    wave = extent(hull, count=1e-20)
    achieved = (hull.bbox_min_m[0] - wave.x_aft_m) / wave.wavelength_m
    assert achieved >= 1e-20
    assert wave.achieved_wake_wavelengths == achieved


def test_worked_example_matches_hand_calculation():
    """Same explicit substitutions as the hand calculation above, at both U."""
    script = Path(__file__).resolve().parents[3] / "examples/openfoam/wave_extent.py"
    rows = runpy.run_path(str(script))["example_results"]()
    for row, speed in zip(rows, (1.0, 2.0)):
        reach = 4 * math.pi * speed**2 / GRAVITY
        assert row["wake_length_m"] == pytest.approx(reach)
        assert row["aft_half_width_m"] == pytest.approx(
            0.45 + (6.20 + reach) / math.sqrt(8)
        )
