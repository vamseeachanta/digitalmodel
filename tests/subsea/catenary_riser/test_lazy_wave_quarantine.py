"""Quarantine contract for the non-functional lazy-wave analyzer.

`subsea.catenary_riser.LazyWaveAnalyzer` never solved a lazy-wave catenary. It returned
hardcoded fractions of water depth (sag = 0.35 x depth, hog = 0.20 x depth), hardcoded
tension ratios (0.4 / 0.6 of baseline top tension) and hardcoded bend angles
(45 deg / 30 deg, commented "Rough estimate"). The buoyancy weight was computed and then
never read, so **buoyancy configuration had no effect whatsoever on the result**.

Its previous tests passed because they asserted properties true by construction --
`sag_bend_depth > hog_bend_depth` holds for any positive water depth given 0.35 > 0.20,
and `0 <= buoyancy_utilization <= 1` was guaranteed by a `min(1.0, ...)`. They would have
passed with the physics deleted, because in effect it was.

Why this matters beyond tidiness: an optimiser searching buoyancy configurations against
this analyzer sees a perfectly flat objective. It returns whatever it started with and
reports convergence. That is worse than a crash -- it is a confident wrong answer, and it
is exactly what the SLWR buoyancy work would have run into.

The real solver is `digitalmodel.marine_ops.marine_analysis.catenary.LazyWaveSolver`.
"""

import pytest

from digitalmodel.subsea.catenary_riser import (
    PRODUCTION_OIL,
    STEEL_API_5L_X65,
    BuoyancyModule,
    LazyWaveAnalyzer,
    LazyWaveConfiguration,
    RiserConfiguration,
)


@pytest.fixture
def basic_riser():
    return RiserConfiguration(
        name="TestRiser",
        outer_diameter=0.508,
        wall_thickness=0.025,
        length=1500.0,
        material=STEEL_API_5L_X65,
        internal_fluid=PRODUCTION_OIL,
        water_depth=1000.0,
        horizontal_offset=500.0,
    )


def _config(riser, *, length, outer_diameter, density):
    return LazyWaveConfiguration(
        riser=riser,
        buoyancy_modules=[
            BuoyancyModule(
                name="Buoy",
                length=length,
                outer_diameter=outer_diameter,
                buoyancy_material_density=density,
                start_length=300.0,
            )
        ],
    )


def test_analyzer_refuses_rather_than_fabricating(basic_riser):
    """It must fail loudly, not return numbers it did not compute.

    A caller who gets a populated result object has no way to know the geometry was
    invented. Raising is the only honest behaviour until a real solve exists here.
    """
    config = _config(basic_riser, length=200.0, outer_diameter=1.5, density=500)

    with pytest.raises(NotImplementedError) as exc:
        LazyWaveAnalyzer().analyze(config)

    # The error must route the caller somewhere useful, not just refuse.
    assert "LazyWaveSolver" in str(exc.value)


def test_buoyancy_invariance_is_no_longer_reachable(basic_riser):
    """Regression guard on the defect itself.

    Previously these three wildly different buoyancy configurations produced
    bit-identical output. If `analyze` is ever reimplemented, this test must be rewritten
    to assert that the results actually DIFFER -- not deleted. Until then, the quarantine
    is what makes the invariance unreachable.
    """
    configs = [
        _config(basic_riser, length=50.0, outer_diameter=0.35, density=900),
        _config(basic_riser, length=300.0, outer_diameter=0.65, density=450),
        _config(basic_riser, length=800.0, outer_diameter=1.50, density=200),
    ]

    for config in configs:
        with pytest.raises(NotImplementedError):
            LazyWaveAnalyzer().analyze(config)
