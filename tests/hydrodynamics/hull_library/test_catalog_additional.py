from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.hydrodynamics.hull_library.catalog import (
    HullCatalog,
    SeaStateDefinition,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)
from digitalmodel.hydrodynamics.models import RAOData


def _profile(name: str = "synthetic_box") -> HullProfile:
    return HullProfile(
        name=name,
        hull_type=HullType.BARGE,
        stations=[
            HullStation(
                x_position=0.0,
                waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
            ),
            HullStation(
                x_position=50.0,
                waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
            ),
        ],
        length_bp=50.0,
        beam=10.0,
        draft=4.0,
        depth=6.0,
        source="test",
    )


def _rao_with_direction_bias() -> RAOData:
    frequencies = np.linspace(0.1, 1.0, 10)
    directions = np.array([0.0, 90.0, 180.0])
    amps = np.zeros((len(frequencies), len(directions), 6))
    phases = np.zeros_like(amps)
    # Distinct heave levels per direction so nearest-direction choice is testable.
    amps[:, 0, 2] = 0.5
    amps[:, 1, 2] = 1.0
    amps[:, 2, 2] = 2.0
    return RAOData(
        frequencies=frequencies,
        directions=directions,
        amplitudes=amps,
        phases=phases,
        vessel_name="synthetic",
    )


def test_load_profiles_skips_invalid_yaml_and_keeps_valid(tmp_path):
    good = _profile("valid_hull")
    good.save_yaml(tmp_path / "valid.yaml")
    (tmp_path / "invalid.yaml").write_text("not: [valid")

    cat = HullCatalog(profiles_dir=tmp_path)
    assert cat.list_hulls() == ["valid_hull"]


def test_interpolate_rao_uses_zero_outside_frequency_range():
    rao_f = np.array([0.5, 1.0])
    rao_a = np.array([1.0, 2.0])
    target = np.array([0.1, 0.5, 0.75, 1.0, 2.0])

    out = HullCatalog._interpolate_rao(rao_f, rao_a, target)
    assert out[0] == 0.0
    assert out[-1] == 0.0
    assert out[1] == pytest.approx(1.0)
    assert out[3] == pytest.approx(2.0)


def test_compute_motions_selects_nearest_heading_direction():
    cat = HullCatalog()
    cat.register_hull(_profile())
    rao = _rao_with_direction_bias()
    sea_95 = SeaStateDefinition(significant_height=3.0, peak_period=10.0, heading=95.0)
    sea_175 = SeaStateDefinition(significant_height=3.0, peak_period=10.0, heading=175.0)

    resp_95 = cat.compute_motions("synthetic_box", sea_95, rao)
    resp_175 = cat.compute_motions("synthetic_box", sea_175, rao)

    assert resp_175.significant_values["heave_sig"] > resp_95.significant_values["heave_sig"]


def test_compute_accelerations_rotation_terms_increase_response():
    cat = HullCatalog()
    cat.register_hull(_profile())

    frequencies = np.linspace(0.1, 1.0, 10)
    directions = np.array([0.0])
    amps = np.zeros((len(frequencies), len(directions), 6))
    phases = np.zeros_like(amps)
    # Rotational RAOs only, so acceleration depends on lever arm.
    amps[:, 0, 3] = 0.2  # roll
    amps[:, 0, 4] = 0.2  # pitch
    rao = RAOData(frequencies=frequencies, directions=directions, amplitudes=amps, phases=phases)
    sea = SeaStateDefinition(significant_height=2.0, peak_period=8.0)

    near = cat.compute_accelerations("synthetic_box", sea, rao, point=(0.0, 0.0, 0.0))
    far = cat.compute_accelerations("synthetic_box", sea, rao, point=(20.0, 10.0, 10.0))

    assert far["vertical"] > near["vertical"]
    assert far["lateral"] >= near["lateral"]


def test_generate_spectrum_rejects_unknown_type():
    cat = HullCatalog()
    cat.register_hull(_profile())
    with pytest.raises(ValueError):
        cat.compute_motions(
            "synthetic_box",
            SeaStateDefinition(
                significant_height=2.0,
                peak_period=8.0,
                spectrum_type="not-a-spectrum",
            ),
            _rao_with_direction_bias(),
        )

