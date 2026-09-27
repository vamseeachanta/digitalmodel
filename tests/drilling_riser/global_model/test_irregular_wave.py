"""JONSWAP irregular sea in the riser global model (W4 3 h and fatigue sea states)."""

from __future__ import annotations

from pathlib import Path

import pytest
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex import orcaflex_api

from .conftest import synthetic_spec


def _with(**extra):
    from digitalmodel.drilling_riser.global_model.spec import RiserGlobalModelSpec

    d = synthetic_spec().model_dump()
    d.update(extra)
    return RiserGlobalModelSpec.model_validate(d)


IRR = {"hs_m": 4.2, "tp_s": 9.0, "gamma": 2.0, "direction_deg": 180.0, "seed": 12345}


def test_irregular_wave_emits_a_jonswap_train_with_tp_gamma_and_seed():
    from digitalmodel.drilling_riser.global_model.build import build_generic_spec

    env = build_generic_spec(_with(irregular_wave=IRR))["environment"]
    train = env["raw_properties"]["WaveTrains"][0]
    assert train["WaveType"] == "JONSWAP"
    assert train["WaveJONSWAPParameters"] == "Partially specified"
    assert (train["WaveHs"], train["WaveTp"], train["WaveGamma"], train["WaveSeed"]) == (4.2, 9.0, 2.0, 12345)
    assert "WaveTz" not in train
    assert env["raw_properties"]["UserSpecifiedRandomWaveSeeds"] == "Yes"
    assert env["waves"]["type"] == "jonswap"


def test_regular_and_irregular_waves_are_exclusive():
    with pytest.raises(ValidationError):
        _with(irregular_wave=IRR, regular_wave={"height_m": 3.0, "period_s": 9.0})


@pytest.mark.parametrize("bad", [{"gamma": 0.9}, {"gamma": 7.5}, {"hs_m": 0}, {"tp_s": -1}])
def test_irregular_wave_rejects_invalid_values(bad):
    with pytest.raises(ValidationError):
        _with(irregular_wave={**IRR, **bad})


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_irregular_wave_loads_in_orcaflex_as_specified(tmp_path: Path):
    from digitalmodel.drilling_riser.global_model.build import write_model

    ofx = orcaflex_api.api()
    m = ofx.Model(str(write_model(_with(irregular_wave=IRR), tmp_path) / "master.yml"))
    env = m.environment
    assert env.WaveType == "JONSWAP"
    assert env.WaveHs == pytest.approx(4.2)
    assert env.WaveTp == pytest.approx(9.0)
    assert env.WaveGamma == pytest.approx(2.0)
    assert env.WaveSeed == 12345
    assert env.WaveDirection == pytest.approx(180.0)
