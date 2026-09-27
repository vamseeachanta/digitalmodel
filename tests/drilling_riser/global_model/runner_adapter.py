"""Test adapter for the parallel runner: the synthetic riser, statics or a short regular-wave run."""

from __future__ import annotations

from pathlib import Path

from .conftest import synthetic_spec
from .test_nonlinear_vessel_foundation import _vessel_motion


class SyntheticRiserAdapter:
    def build(self, case: dict, model_dir: Path) -> Path:
        from digitalmodel.drilling_riser.global_model.build import write_model
        from digitalmodel.drilling_riser.global_model.spec import Dynamics, RegularWave, RiserGlobalModelSpec

        p = case.get("params", {})
        if p.get("break_build"):
            raise ValueError("deliberately broken case")
        d = synthetic_spec().model_dump()
        if case["analysis"] == "dynamics":
            d["vessel_motion"] = _vessel_motion().model_dump()
            d["regular_wave"] = RegularWave(height_m=p["wave_height_m"], period_s=9.0).model_dump()
            d["dynamics"] = Dynamics(time_step_s=0.1, build_up_s=9.0, duration_s=18.0).model_dump()
        spec = RiserGlobalModelSpec.model_validate(d)
        return write_model(spec, model_dir) / "master.yml"

    def extract(self, model, case: dict) -> dict:
        from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

        spec = synthetic_spec()
        if case["analysis"] == "dynamics":
            return orun.governing_responses(model, spec)
        return orun.static_responses(model, spec)


ADAPTER = SyntheticRiserAdapter()
