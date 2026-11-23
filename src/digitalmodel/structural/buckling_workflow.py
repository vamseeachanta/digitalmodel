# ABOUTME: Engine router making the DNV-RP-C201 buckling_parametric sweep callable
# ABOUTME: under basename "buckling_parametric" (workspace-hub#3285-OWNED). kind: files.
"""Engine route for the parametric plate-buckling sweep.

#3285 OWNS-CREATES this route: the scope-named ``structural/buckling_parametric.py``
producer (PR #1044) was a pure-Python parametric layer with no engine basename and
no registry row. This adapter gives it the basename ``buckling_parametric`` so it is
callable from the durable CLI path AND from
``digitalmodel.workflow_api.run_workflow("buckling-parametric", ...)`` via a bare
in-repo id, and writes ``results.json`` (+ ``cases.csv``) into the engine-resolved
result folder so the registry ``result: {kind: files}`` descriptor can content-hash it.

``write_outputs(..., timestamp=None)`` deliberately omits ``meta.generated_at`` so the
emitted ``results.json`` is byte-stable -- the property the reference golden relies on.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

from digitalmodel.structural.buckling_parametric import (
    DEFAULT_SHIP_PLATE_SWEEP,
    BucklingSweepConfig,
    run_sweep,
    utility_curves,
    write_outputs,
)


class BucklingParametricWorkflow:
    """Engine adapter for the buckling parametric sweep (basename ``buckling_parametric``)."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        sweep = _build_sweep(cfg.get("buckling_parametric", {}))
        rows = run_sweep(sweep)
        curves = utility_curves(sweep)

        out_dir = _result_dir(cfg)
        # timestamp=None => no meta.generated_at => byte-stable results.json (golden).
        written = write_outputs(
            rows, curves, out_dir, gamma_m=sweep.gamma_m, timestamp=None
        )

        # Thin in-memory summary on cfg[basename] for downstream/report consumers;
        # the authoritative result surface for this row is kind: files (results.json).
        cfg[cfg["basename"]] = {
            "n_cases": len(rows),
            "standard": "DNV-RP-C201",
            "result_folder": str(out_dir),
            "outputs": {key: str(path) for key, path in written.items()},
        }
        return cfg


def _build_sweep(sweep_cfg: dict[str, Any]) -> BucklingSweepConfig:
    """Build a sweep config from cfg, defaulting to the full ship-plate sweep."""
    sweep = sweep_cfg.get("sweep") if sweep_cfg else None
    if not sweep:
        return DEFAULT_SHIP_PLATE_SWEEP
    return BucklingSweepConfig(
        thicknesses=[float(t) for t in sweep["thicknesses"]],
        widths=[float(w) for w in sweep["widths"]],
        plate_lengths=[float(L) for L in sweep["plate_lengths"]],
        load_cases=[tuple(float(v) for v in lc) for lc in sweep["load_cases"]],
        grades=list(sweep["grades"]),
        gamma_m=float(sweep.get("gamma_m", 1.15)),
    )


def _result_dir(cfg: dict[str, Any]) -> Path:
    """Engine-resolved result folder.

    On the durable CLI path this is ``<config dir>/results``; on the #3307 embed
    path it is the injected ``root_folder/results`` (configure_embed rebases both
    ``Analysis.result_folder`` and ``_config_dir_path`` to the injected root).
    """
    analysis = cfg.get("Analysis", {}) or {}
    result_folder = analysis.get("result_folder")
    if result_folder:
        return Path(result_folder)
    config_dir = cfg.get("_config_dir_path")
    return (Path(config_dir) if config_dir else Path.cwd()) / "results"
