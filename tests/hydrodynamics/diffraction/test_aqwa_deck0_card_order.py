"""Deck 0 card ordering, which AQWA enforces and rejects the deck over.

AQWA requires the RESTART card to follow immediately after the OPTIONS list
that carries REST. With OPTIONS AHD1 emitted between the two, AQWA 2026 R1
stops in the preliminary data check with

    **** INPUT DATA ERROR  :LINE 14 HAVING INCLUDED A REST OPTION
         A RESTART CARD MUST FOLLOW (COL 1)

and produces no hydrodynamic results, while still exiting with status 0.
That exit status is why this is pinned by a test rather than left to the
runner to notice.
"""

from __future__ import annotations

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec


def _spec(**solver_options) -> DiffractionSpec:
    options = {
        "remove_irregular_frequencies": False,
        "qtf_calculation": False,
        "load_rao_method": "both",
        "precision": "double",
    }
    options.update(solver_options)
    return DiffractionSpec.model_validate({
        "version": "1.0",
        "analysis_type": "diffraction",
        "vessel": {
            "name": "CardOrder",
            "type": "barge",
            "geometry": {
                "mesh_file": "unit_box.gdf",
                "mesh_format": "gdf",
                "symmetry": "none",
                "reference_point": [0.0, 0.0, 0.0],
                "waterline_z": 0.0,
                "length_units": "m",
            },
            "inertia": {
                "mass": 1025.0,
                "centre_of_gravity": [0.0, 0.0, -0.5],
                "radii_of_gyration": [0.4, 0.4, 0.4],
            },
        },
        "environment": {
            "water_depth": 100.0,
            "water_density": 1025.0,
            "gravity": 9.80665,
        },
        "frequencies": {"input_type": "frequency", "values": [0.3, 0.6]},
        "wave_headings": {"values": [0.0, 90.0], "symmetry": False},
        "solver_options": options,
        "outputs": {"formats": ["csv"], "components": ["raos"]},
        "metadata": {"project": "card_order", "author": "test"},
    })


def _indices(cards: list[str]) -> tuple[int, int]:
    rest = next(i for i, c in enumerate(cards)
                if c.startswith("OPTIONS") and "REST" in c)
    restart = next(i for i, c in enumerate(cards) if c.startswith("RESTART"))
    return rest, restart


@pytest.mark.parametrize("output_ah1", [False, True])
def test_restart_immediately_follows_the_rest_option(output_ah1):
    cards = AQWABackend().build_deck0(_spec(output_ah1=output_ah1))
    rest, restart = _indices(cards)
    between = cards[rest + 1:restart]
    assert between == [], (
        f"AQWA requires RESTART to follow the REST option directly; "
        f"found {between!r} in between"
    )


def test_ah1_option_is_still_emitted():
    cards = AQWABackend().build_deck0(_spec(output_ah1=True))
    assert any(c.strip() == "OPTIONS AHD1" for c in cards)


def test_ah1_option_absent_when_not_requested():
    cards = AQWABackend().build_deck0(_spec(output_ah1=False))
    assert not any("AHD1" in c for c in cards)


def test_ah1_option_precedes_the_terminating_options_card():
    """END terminates the OPTIONS list, so AHD1 has to come before it."""
    cards = AQWABackend().build_deck0(_spec(output_ah1=True))
    ah1 = next(i for i, c in enumerate(cards) if "AHD1" in c)
    end = next(i for i, c in enumerate(cards)
               if c.startswith("OPTIONS") and c.rstrip().endswith("END"))
    assert ah1 < end


def test_deck0_still_opens_with_job_and_title():
    cards = AQWABackend().build_deck0(_spec(output_ah1=True))
    body = [c for c in cards if not c.startswith("*")]
    assert body[0] == "JOB AQWA  LINE"
    assert body[1].startswith("TITLE")
