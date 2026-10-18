"""
ABOUTME: The free-surface HORIZONTAL resolution requirement (#2023). The grid
ladder for this lane was sized at one speed and then run at four, and the
in-plane criterion it has to satisfy moves with the SQUARE of the speed while
the mesh does not move at all.

WHAT THESE TESTS PIN, MEASURED. On the full-scale hull below, at the coarse
level (background cell Lpp/6.3 = 25.11 m, six topoSet/refineMesh stages that
halve x and y only, so 0.392 m in the free-surface plane) the achieved
resolution is 85 cells per linear deep-water wavelength at 14 kn -- and 73, 62
and 52 at 13, 12 and 11 kn. The project's acceptance criterion is 80. A coarse
sweep therefore fails the criterion at EVERY solve below 14 kn.

Nothing in the solve reports that. An under-resolved wave train is damped by
numerical diffusion, the residuals converge, and the wave-making component of
resistance comes out low. So the check has to happen at BUILD time, and it has
to refuse -- the same posture as ``CellBudgetError``, which raises before
anything is written rather than letting snappyHexMesh truncate and report
success.
"""

from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Any, Dict

import pytest

from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig, build_hull_case
from digitalmodel.solvers.openfoam.hull_domain import (
    REFINEMENT_STAGES,
    block_divisions,
    build_hull_domain,
)
from digitalmodel.solvers.openfoam.hull_free_surface import (
    DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH,
    GRAVITY,
    FreeSurfaceResolutionError,
    deep_water_wavelength,
    free_surface_divisions,
    free_surface_resolution,
)
from digitalmodel.solvers.openfoam.hull_case_physics import derive_cell_budget
from digitalmodel.solvers.openfoam.hull_domain import refinement_boxes
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest

#: The speeds the sweep actually runs, with the published wavelengths. These
#: are the numbers the acceptance criterion is scored against; they are stated
#: here so a change to the wavelength relation cannot pass silently.
SPEEDS_M_S = {14: 7.2022, 13: 6.6878, 12: 6.1733, 11: 5.6589}
WAVELENGTHS_M = {14: 33.23, 13: 28.65, 12: 24.41, 11: 20.52}

#: The full-scale hull the sweep runs on. A size class, not a vessel: what
#: matters to these tests is that Lpp/6.3 puts the in-plane cell at 0.39 m,
#: which is where the 14 kn ladder sits right on the criterion.
FULL_SCALE: Dict[str, Any] = {
    "source_file": "hull.stl",
    "source_sha256": "0" * 64,
    "units_in": "m",
    "scale_to_m": 1.0,
    "orientation": {"x": "forward", "y": "port", "z": "up"},
    "origin": "aft_perpendicular_keel",
    "lpp_m": 158.20,
    "beam_m": 27.004,
    "draft_m": 10.40,
    "wetted_surface_m2": 6216.8,
    "displacement_m3": 0.80 * 158.20 * 27.004 * 10.40,
    "watertight": True,
    "n_triangles": 500000,
    "bbox_min_m": [-1.0, -13.502, 0.0],
    "bbox_max_m": [160.2, 13.502, 14.59],
}


@pytest.fixture
def full_scale() -> HullManifest:
    return HullManifest.from_dict(json.loads(json.dumps(FULL_SCALE)))


@pytest.fixture
def domain(full_scale: HullManifest):
    return build_hull_domain(full_scale)


def _config(manifest: HullManifest, stl: Path, velocity: float, **kw) -> HullCaseConfig:
    return HullCaseConfig(
        manifest=manifest,
        stl_path=stl,
        velocity=velocity,
        ranks=8,
        name="fs_case",
        **kw,
    )


# --------------------------------------------------------------------------- #
#  The wavelength itself
# --------------------------------------------------------------------------- #

def test_wavelength_is_the_linear_deep_water_relation() -> None:
    """lambda = 2 pi V^2 / g, against the published table."""
    for knots, velocity in SPEEDS_M_S.items():
        assert deep_water_wavelength(velocity) == pytest.approx(
            WAVELENGTHS_M[knots], rel=1e-3
        ), knots


def test_wavelength_falls_with_the_square_of_the_speed() -> None:
    """The reason a ladder sized at one speed fails at another.

    Two knots slower is not a small perturbation: 11 kn asks for a wavelength
    38% shorter than 14 kn, on a mesh that has not changed.
    """
    assert deep_water_wavelength(2.0) == pytest.approx(4.0 * deep_water_wavelength(1.0))
    assert deep_water_wavelength(1.0) == pytest.approx(2.0 * math.pi / GRAVITY)


def test_a_non_positive_speed_is_refused() -> None:
    for bad in (0.0, -1.0):
        with pytest.raises(FreeSurfaceResolutionError):
            deep_water_wavelength(bad)


# --------------------------------------------------------------------------- #
#  The measured defect
# --------------------------------------------------------------------------- #

def test_the_in_plane_cell_is_the_background_cell_halved_once_per_stage(
    domain,
) -> None:
    """``directions (tan1 tan2)`` with tan1 = (1 0 0), tan2 = (0 1 0).

    Every topoSet/refineMesh stage halves x and y and leaves z alone, so the
    free-surface plane resolution is a property of nx/ny, not of the vertical
    band. Scoring it against the background cell -- or against the band cell
    height -- reads like a working derivation and is off by a factor of 64.
    """
    div = block_divisions(domain)
    res = free_surface_resolution(domain, div, SPEEDS_M_S[14])
    coarsest = max(domain.length / div["nx"], domain.width / div["ny"])
    assert res.cell_size == pytest.approx(coarsest / 2**REFINEMENT_STAGES)
    assert res.cell_size == pytest.approx(0.392, abs=5e-4)


def test_the_fourteen_knot_ladder_fails_the_criterion_below_fourteen_knots(
    domain,
) -> None:
    """The defect this lane exists to catch, in the numbers that measured it."""
    div = block_divisions(domain)
    achieved = {
        knots: free_surface_resolution(domain, div, v).cells_per_wavelength
        for knots, v in SPEEDS_M_S.items()
    }
    assert achieved[14] == pytest.approx(85, abs=1)
    assert achieved[13] == pytest.approx(73, abs=1)
    assert achieved[12] == pytest.approx(62, abs=1)
    assert achieved[11] == pytest.approx(52, abs=1)

    required = DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH
    assert achieved[14] >= required
    for knots in (13, 12, 11):
        assert achieved[knots] < required, knots


def test_the_default_criterion_is_eighty_cells_per_wavelength() -> None:
    """Stated by the analysis plan, not invented here."""
    assert DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH == 80


# --------------------------------------------------------------------------- #
#  The refinement that meets it
# --------------------------------------------------------------------------- #

def test_refinement_raises_the_in_plane_counts_until_the_criterion_is_met(
    domain,
) -> None:
    base = block_divisions(domain)
    for velocity in SPEEDS_M_S.values():
        div = free_surface_divisions(domain, base, velocity)
        res = free_surface_resolution(domain, div, velocity)
        assert res.cells_per_wavelength >= DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH
        assert div["nx"] >= base["nx"] and div["ny"] >= base["ny"]


def test_the_criterion_is_met_in_BOTH_in_plane_directions(domain) -> None:
    """A Kelvin pattern is not one-dimensional.

    The transverse waves run along x and the divergent system carries the same
    wavelength across y, so scoring dx alone passes a mesh that is coarse in
    the direction half the wave energy travels in.
    """
    velocity = SPEEDS_M_S[11]
    div = free_surface_divisions(domain, block_divisions(domain), velocity)
    lam = deep_water_wavelength(velocity)
    for extent, count in ((domain.length, div["nx"]), (domain.width, div["ny"])):
        cell = extent / count / 2**REFINEMENT_STAGES
        assert lam / cell >= DEFAULT_FREE_SURFACE_CELLS_PER_WAVELENGTH


def test_a_ladder_that_already_clears_the_criterion_is_left_alone(domain) -> None:
    """No cost is spent where the physics does not ask for it."""
    base = block_divisions(domain)
    assert free_surface_divisions(domain, base, SPEEDS_M_S[14]) == base


def test_the_vertical_band_is_not_touched_by_the_in_plane_requirement(domain) -> None:
    """This is the in-plane analogue of ``FREE_SURFACE_CELL_FRACTION``, not a
    replacement for it. The stages refine x and y only, so a vertical count
    changed here would be a cost with no effect on the criterion."""
    base = block_divisions(domain)
    div = free_surface_divisions(domain, base, SPEEDS_M_S[11])
    for key in ("nza", "nzb", "nzc", "nzd"):
        assert div[key] == base[key], key


def test_a_non_positive_criterion_is_refused(domain) -> None:
    for bad in (0.0, -80.0):
        with pytest.raises(FreeSurfaceResolutionError):
            free_surface_divisions(
                domain,
                block_divisions(domain),
                SPEEDS_M_S[13],
                cells_per_wavelength=bad,
            )


# --------------------------------------------------------------------------- #
#  The cost, which is load-bearing for the programme schedule
# --------------------------------------------------------------------------- #

def test_meeting_the_criterion_costs_cells_and_the_cost_is_visible(
    full_scale, domain
) -> None:
    """The estimate must MOVE, and it must move by the in-plane factor only.

    Refining nx and ny costs the square of the linear factor, not the cube: the
    vertical staging is untouched. A cost model that assumed the cube would
    over-state the coarse level by 10% and an implementation that refined the
    background cell instead of nx/ny would actually cost it.
    """
    boxes = refinement_boxes(full_scale, domain)
    base = block_divisions(domain)
    div = free_surface_divisions(domain, base, SPEEDS_M_S[13])

    before = derive_cell_budget(domain, boxes, base, ranks=8).estimated_cells
    after = derive_cell_budget(domain, boxes, div, ranks=8).estimated_cells
    assert after > before

    in_plane = (div["nx"] / base["nx"]) * (div["ny"] / base["ny"])
    assert after / before == pytest.approx(in_plane, rel=0.02)


# --------------------------------------------------------------------------- #
#  The build gate
# --------------------------------------------------------------------------- #

def test_the_emitted_case_meets_the_criterion_at_every_swept_speed(
    full_scale, stl_file, tmp_path
) -> None:
    for knots, velocity in SPEEDS_M_S.items():
        case = build_hull_case(
            _config(full_scale, stl_file, velocity).replace(name=f"kn{knots}"),
            tmp_path,
        )
        prov = json.loads((case / "case_provenance.json").read_text())
        fs = prov["mesh"]["free_surface"]
        assert fs["cells_per_wavelength"] >= fs["required_cells_per_wavelength"]


def test_provenance_records_the_achieved_resolution(
    full_scale, stl_file, tmp_path
) -> None:
    case = build_hull_case(_config(full_scale, stl_file, SPEEDS_M_S[13]), tmp_path)
    fs = json.loads((case / "case_provenance.json").read_text())["mesh"]["free_surface"]
    assert fs["wavelength_m"] == pytest.approx(WAVELENGTHS_M[13], rel=1e-3)
    assert fs["velocity_m_s"] == pytest.approx(SPEEDS_M_S[13])
    assert fs["required_cells_per_wavelength"] == 80
    assert fs["cells_per_wavelength"] >= 80
    assert fs["in_plane_cell_size_m"] > 0
    assert fs["refinement_stages"] == REFINEMENT_STAGES


def test_a_stated_background_cell_that_cannot_meet_the_criterion_fails_the_BUILD(
    full_scale, stl_file, tmp_path
) -> None:
    """A caller-stated ``base_cell_size`` is the caller's grid, not ours.

    Silently refining it would meet the criterion and destroy the systematic
    refinement ratio a grid-convergence triplet is read from -- the coarse and
    medium levels would no longer differ by the factor the study assumes. So a
    stated grid that cannot resolve the wave is REFUSED, and the refusal names
    both numbers.
    """
    coarse = full_scale.lpp_m / 6.3
    with pytest.raises(FreeSurfaceResolutionError) as excinfo:
        build_hull_case(
            _config(full_scale, stl_file, SPEEDS_M_S[13], base_cell_size=coarse),
            tmp_path / "aborted",
        )
    message = str(excinfo.value)
    assert "73" in message, "the achieved cells/wavelength is not named"
    assert "80" in message, "the required cells/wavelength is not named"
    assert "base_cell_size" in message, "the knob to turn is not named"


def test_nothing_is_written_when_the_free_surface_check_fails(
    full_scale, stl_file, tmp_path
) -> None:
    """Refuse before emitting, exactly as the cell-budget gate does."""
    out = tmp_path / "aborted"
    config = _config(
        full_scale, stl_file, SPEEDS_M_S[13], base_cell_size=full_scale.lpp_m / 6.3
    )
    with pytest.raises(FreeSurfaceResolutionError):
        build_hull_case(config, out)
    assert not (out / config.name).exists()


def test_a_stated_background_cell_that_meets_the_criterion_is_honoured(
    full_scale, stl_file, tmp_path
) -> None:
    """The refusal is about resolution, not about stating a cell size."""
    fine = full_scale.lpp_m / 6.3 / 2.0
    case = build_hull_case(
        _config(full_scale, stl_file, SPEEDS_M_S[11], base_cell_size=fine), tmp_path
    )
    prov = json.loads((case / "case_provenance.json").read_text())
    assert prov["domain"]["base_cell_size_m"] == pytest.approx(fine)
    assert prov["mesh"]["free_surface"]["cells_per_wavelength"] >= 80


def test_the_criterion_can_be_stood_down_but_never_silently(
    full_scale, stl_file, tmp_path
) -> None:
    """Disabling it is a recorded decision, not a default."""
    config = _config(
        full_scale,
        stl_file,
        SPEEDS_M_S[11],
        base_cell_size=full_scale.lpp_m / 6.3,
        free_surface_cells_per_wavelength=None,
    )
    case = build_hull_case(config, tmp_path)
    fs = json.loads((case / "case_provenance.json").read_text())["mesh"]["free_surface"]
    assert fs["required_cells_per_wavelength"] is None
    assert fs["enforced"] is False
    assert fs["cells_per_wavelength"] == pytest.approx(52, abs=1)


def test_a_tighter_criterion_costs_more_cells(full_scale, stl_file, tmp_path) -> None:
    def cells(name: str, criterion: float) -> int:
        case = build_hull_case(
            _config(
                full_scale,
                stl_file,
                SPEEDS_M_S[13],
                free_surface_cells_per_wavelength=criterion,
            ).replace(name=name),
            tmp_path,
        )
        return json.loads((case / "case_provenance.json").read_text())["mesh"][
            "estimated_cells"
        ]

    assert cells("tight", 120.0) > cells("plan", 80.0)
