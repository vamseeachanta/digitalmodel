"""Vessel roll and pitch must not be dropped silently (#3986).

The riser and S-lay vessel builders emit only ``InitialHeading`` from
``orientation[2]``; ``orientation[0]`` and ``orientation[1]`` were accepted and
discarded, so a heeled or trimmed vessel was generated level. Until the native
heel and trim data names are confirmed on a licensed load, non-zero values are
rejected at validation.
"""

from __future__ import annotations

import pytest
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex.modular_generator.schema.equipment import Vessel
from digitalmodel.solvers.orcaflex.modular_generator.schema.riser import RiserVessel

_PROPS = {
    "loa": 100, "beam": 20, "draft": 5, "gmt": 2,
    "cog": [50, 0, 5], "gyration_radii": [8, 25, 25],
}


@pytest.mark.parametrize("orientation", [[2.0, 0, 0], [0, -1.5, 0], [0.5, 0.5, 90]])
def test_riser_vessel_roll_or_pitch_rejected(orientation):
    with pytest.raises(ValidationError, match="roll and pitch"):
        RiserVessel(name="FPSO", orientation=orientation)


@pytest.mark.parametrize("orientation", [[2.0, 0, 0], [0, -1.5, 0]])
def test_slay_vessel_roll_or_pitch_rejected(orientation):
    with pytest.raises(ValidationError, match="roll and pitch"):
        Vessel(name="Lay", properties=_PROPS, orientation=orientation)


def test_heading_only_orientation_accepted():
    assert RiserVessel(name="FPSO", orientation=[0, 0, 270]).orientation == [0, 0, 270]
    assert Vessel(name="Lay", properties=_PROPS, orientation=[0, 0, 180]).orientation[2] == 180
