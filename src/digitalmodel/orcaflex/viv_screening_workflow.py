# ABOUTME: Engine router making the pure DNV-RP-C205 viv_screening calc callable under
# ABOUTME: basename "viv_parametric_screening" (digitalmodel#1505). kind: files, byte-stable.
"""Engine route for the synthetic VIV parametric screening pilot.

#1505 wires the pure, analytical, redistribution-free ``orcaflex/viv_screening.py``
(``VIVScreeningInput`` / ``BeamProperties`` / ``viv_screening`` /
``estimate_response_amplitude`` -- DNV-RP-C205 §9, DNV-RP-F105 §4-5, Blevins 1990)
into the engine under the NEW basename ``viv_parametric_screening``. It is modelled
EXACTLY on ``structural/buckling_workflow.BucklingParametricWorkflow`` -- the
byte-stable determinism-golden precedent:

* the output dir is resolved via :func:`_result_dir` = ``Analysis.result_folder`` if
  present else ``cfg["_config_dir_path"]/results``, which ``configure_embed`` rebases
  onto the injected throwaway ``root_folder`` on the #3307 embed path, so
  ``run_workflow._run_once`` writes only into the sandbox it ``rmtree``s (side-effect
  free) and ``extract_result(..., root=tmp)`` still finds the files for the golden;
* ``write_viv_outputs(..., timestamp=None)`` omits any wall-clock ``generated_at`` so
  ``results.json`` is byte-stable -- the property the reference golden relies on;
* a DNV-RP-C205 / DNV-RP-F105 ``citations`` sidecar is attached to the native payload.

The pure ``viv_screening.py`` module is consumed UNCHANGED. This adapter does NOT use
``workflows/parametric_run.py`` (whose ``_resolve_output_path`` hardcodes
``REPO_ROOT/results/parametric_run/...`` and is NOT embed-aware). The "parametric"
sweep is produced by EXTERNAL ``run_workflow("viv-parametric-screening", params=...)``
calls, one per synthetic variant.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import pandas as pd

from digitalmodel.orcaflex.viv_screening import (
    BeamProperties,
    VIVScreeningInput,
    estimate_response_amplitude,
    viv_screening,
)

STANDARD = "DNV-RP-C205"
SECONDARY_STANDARD = "DNV-RP-F105"
MODULE_NAME = "digitalmodel.orcaflex.viv_screening_workflow"

# Fatigue-proxy exponent m: an S-N-slope-flavoured SCREENING surrogate exponent on the
# A/D amplitude ratio. NOT a certified fatigue life -- see the metrics `quality` note.
M_EXP = 3.0

# The public standards cited by the pure calc; surfaced as a provenance-liftable
# `citations` sidecar and retained verbatim in results.json (redistribution-free).
CITATIONS = [
    {"code_id": "DNV-RP-C205", "publisher": "DNV", "revision": "2019", "section": "9"},
    {"code_id": "DNV-RP-F105", "publisher": "DNV", "revision": "2017", "section": "4-5"},
]


class VIVParametricScreeningWorkflow:
    """Engine adapter for the VIV screening calc (basename ``viv_parametric_screening``)."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        params = cfg.get("viv_parametric_screening", {}) or {}
        vin = VIVScreeningInput(**(params.get("input") or {}))
        beam = BeamProperties(**(params.get("beam") or {}))
        n_modes = int(params.get("n_modes", 10))

        res = viv_screening(vin, beam, n_modes=n_modes)

        # Critical mode drives A/D; fall back to mode 1 when screening passes (no lock-in).
        crit_idx = (res.critical_mode or 1) - 1
        crit = res.details[crit_idx]
        a_d = estimate_response_amplitude(
            crit["reduced_velocity"], vin.outer_diameter, vin.stability_parameter
        )
        fatigue_proxy = round(crit["vortex_shedding_freq_Hz"] * (a_d ** M_EXP), 6)

        out_dir = _result_dir(cfg)
        # timestamp=None => no generated_at => byte-stable results.json (golden).
        written = write_viv_outputs(
            vin, beam, res, a_d, fatigue_proxy, out_dir, timestamp=None
        )

        cfg[cfg["basename"]] = {
            "standard": STANDARD,
            "screening_pass": res.screening_pass,
            "lock_in": not res.screening_pass,
            "critical_mode": res.critical_mode,
            "critical_vr": res.critical_vr,
            "a_d_ratio": a_d,
            "fatigue_proxy": fatigue_proxy,
            "result_folder": str(out_dir),
            "outputs": {key: str(path) for key, path in written.items()},
        }
        return cfg


def _screening_payload(
    vin: VIVScreeningInput,
    beam: BeamProperties,
    res,
    a_d: float,
    fatigue_proxy: float,
    n_modes: int,
    timestamp: str | None,
) -> dict[str, Any]:
    """Assemble the native, byte-stable results payload (no wall-clock field)."""
    payload: dict[str, Any] = {
        "meta": {
            "standard": STANDARD,
            "secondary_standard": SECONDARY_STANDARD,
            "module": MODULE_NAME,
            "n_modes": n_modes,
            "units": {"length": "m", "velocity": "m/s", "frequency": "Hz"},
            "generated_by": "viv_parametric_screening",
            "preliminary": False,
        },
        "screening": {
            "screening_pass": res.screening_pass,
            "lock_in": not res.screening_pass,
            "critical_mode": res.critical_mode,
            "critical_vr": res.critical_vr,
            "a_d_ratio": a_d,
            "fatigue_proxy": fatigue_proxy,
            "stability_parameter_Ks": round(vin.stability_parameter, 3),
        },
        "input": {
            "outer_diameter": round(vin.outer_diameter, 4),
            "current_speed": round(vin.current_speed, 4),
            "reynolds_number": round(vin.reynolds_number, 0),
            "strouhal_number": round(vin.st, 3),
            "vortex_shedding_freq_Hz": round(vin.vortex_shedding_frequency, 4),
        },
        "beam": {
            "length": round(beam.length, 4),
            "outer_diameter": round(beam.outer_diameter, 4),
            "inner_diameter": round(beam.inner_diameter, 4),
            "mass_per_length": round(beam.mass_per_length, 4),
            "natural_frequency_1_Hz": round(beam.natural_frequency(1), 4),
        },
        "modes": res.details,
        "citations": CITATIONS,
    }
    if timestamp is not None:
        payload["meta"]["generated_at"] = timestamp
    return payload


def write_viv_outputs(
    vin: VIVScreeningInput,
    beam: BeamProperties,
    res,
    a_d: float,
    fatigue_proxy: float,
    out_dir: str | Path,
    timestamp: str | None = None,
) -> dict[str, Path]:
    """Write results.json + cases.csv (per-mode table). Returns the written paths.

    ``timestamp=None`` (the golden path) omits ``meta.generated_at`` so results.json is
    byte-stable. Every numeric is already rounded (pydantic model rounding +
    :func:`estimate_response_amplitude`), so the content hash is cross-machine stable.
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    payload = _screening_payload(
        vin, beam, res, a_d, fatigue_proxy, res.modes_checked, timestamp
    )

    json_path = out_dir / "results.json"
    json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    csv_path = out_dir / "cases.csv"
    df = pd.DataFrame(res.details)
    df.to_csv(csv_path, index=False)

    return {"results_json": json_path, "cases_csv": csv_path}


def _result_dir(cfg: dict[str, Any]) -> Path:
    """Engine-resolved result folder (embed-aware, exactly like BucklingParametricWorkflow).

    On the durable CLI path this is ``<config dir>/results``; on the #3307 embed path it
    is the injected ``root_folder/results`` (configure_embed rebases both
    ``Analysis.result_folder`` and ``_config_dir_path`` onto the injected root).
    """
    analysis = cfg.get("Analysis", {}) or {}
    result_folder = analysis.get("result_folder")
    if result_folder:
        return Path(result_folder)
    config_dir = cfg.get("_config_dir_path")
    return (Path(config_dir) if config_dir else Path.cwd()) / "results"
