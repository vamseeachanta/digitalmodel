"""Synthetic choices are test inputs, not operational limits (issue #2955)."""

import pytest

from digitalmodel.subsea.survey_qa import (
    Acquisition,
    Line,
    Offsets,
    Seabed,
    generate_swath,
)


@pytest.fixture
def survey():
    def make(
        *,
        bed=None,
        direction=1,
        speed=2.0,
        track_y=0.0,
        offsets=None,
        noise=0.0,
        seed=7,
        beams=(-20.0, -10.0, 0.0, 10.0, 20.0),
    ):
        # Independent grid: x = -20 + 2t forward, 20 - 2t reciprocal.
        times = tuple(float(i) for i in range(21))
        start = -20.0 if direction == 1 else 20.0
        acquisition = Acquisition(
            sensor_offsets_m=(0.0, 0.0, 0.0),
            svp=((0.0, 1500.0), (100.0, 1500.0)),
            sonar_times_s=times,
            navigation_times_s=times,
            sync_tolerance_s=0.0,
        )
        return generate_swath(
            bed or Seabed(50.0, 0.0, 0.0),
            Line(start, track_y, direction, speed),
            acquisition,
            beams,
            offsets or Offsets(),
            noise_std_m=noise,
            seed=seed,
        )

    return make
