"""Engine router for OpenFOAM CFD case build + solve (digitalmodel #1161).

This is the thin engine-facing handler that makes a CFD config resolvable
through the main digitalmodel engine (``basename: openfoam``). It does NOT
author any dict files or re-implement case generation — it delegates entirely
to :class:`OpenFOAMCaseBuilder` (case authoring, license-free) and
:class:`OpenFOAMRunner` (fail-closed subprocess execution).

It mirrors the ANSYS/OrcaWave/AQWA router contract so the same fixed
``uv run python -m digitalmodel <input>`` lane command can drive a CFD solve.

Input contract::

    basename: openfoam            # (or "cfd")
    openfoam:
      operation: run_openfoam     # build_case | run_openfoam (default)
      case_type: current_loading  # any CaseType value
      name: my_case               # case directory name
      output_directory: out       # relative to Analysis.result_folder
      solver: simpleFoam          # optional override; else read from controlDict
      mesh_utility: blockMesh
      run_snappy: false
      to_vtk: true
      dry_run: false

Fail-closed: if a real solve was requested but no OpenFOAM is on PATH, the
runner reports DRY_RUN and this router raises, so the licensed-run lane can
never record a false finish (same guard the ANSYS router uses).
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

_SUPPORTED_OPERATIONS = ("run_openfoam", "build_case")


class OpenFOAMWorkflow:
    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        settings = cfg.setdefault("openfoam", {})
        operation = settings.get("operation", "run_openfoam")
        if operation not in _SUPPORTED_OPERATIONS:
            raise ValueError(
                f"Unsupported openfoam operation: {operation!r}. "
                f"Supported operations: {', '.join(_SUPPORTED_OPERATIONS)}"
            )

        case_dir = self._build_case(cfg, settings)
        settings["case_dir"] = str(case_dir)

        if operation == "build_case":
            settings["run_status"] = "built"
            return cfg

        return self._run_openfoam(cfg, settings, case_dir)

    # ------------------------------------------------------------------ #
    #  build (license-free) — delegate to OpenFOAMCaseBuilder            #
    # ------------------------------------------------------------------ #
    def _build_case(self, cfg: dict[str, Any], settings: dict[str, Any]) -> Path:
        # Lazy import: keep the engine import graph light.
        from .case_builder import OpenFOAMCaseBuilder
        from .models import CaseType, OpenFOAMCase

        case_type_raw = settings.get("case_type")
        if case_type_raw is None:
            raise ValueError("openfoam.case_type is required")
        try:
            case_type = CaseType(case_type_raw)
        except ValueError as exc:
            valid = ", ".join(ct.value for ct in CaseType)
            raise ValueError(
                f"Unknown openfoam.case_type {case_type_raw!r}. Valid: {valid}"
            ) from exc

        name = settings.get("name") or f"{case_type.value}_case"
        case = OpenFOAMCase.for_case_type(case_type, name)

        # Optional solver override (otherwise the case-type default is used).
        solver_override = settings.get("solver")
        if solver_override:
            case.solver_config.solver_name = solver_override

        parent_dir = self._resolve_output_dir(cfg, settings)
        parent_dir.mkdir(parents=True, exist_ok=True)
        return OpenFOAMCaseBuilder(case).build(parent_dir)

    # ------------------------------------------------------------------ #
    #  run (fail-closed) — delegate to OpenFOAMRunner                    #
    # ------------------------------------------------------------------ #
    def _run_openfoam(
        self, cfg: dict[str, Any], settings: dict[str, Any], case_dir: Path
    ) -> dict[str, Any]:
        from .runner import OpenFOAMRunConfig, OpenFOAMRunner

        requested_dry_run = bool(settings.get("dry_run", False))
        run_cfg = OpenFOAMRunConfig(
            solver=settings.get("solver"),
            mesh_utility=settings.get("mesh_utility", "blockMesh"),
            run_snappy=bool(settings.get("run_snappy", False)),
            to_vtk=bool(settings.get("to_vtk", True)),
            timeout_seconds=int(settings.get("timeout_seconds", 7200)),
            dry_run=requested_dry_run,
        )
        result = OpenFOAMRunner(run_cfg).run(case_dir)

        status = str(getattr(result.status, "value", result.status)).lower()
        settings["run_status"] = status
        settings["solver"] = result.solver
        settings["outputs"] = {
            "case_dir": str(result.case_dir),
            "vtk_dir": str(result.vtk_dir) if result.vtk_dir else None,
            "stages": [
                {"name": s.name, "return_code": s.return_code}
                for s in result.stages
            ],
        }

        if status == "failed":
            raise RuntimeError(
                f"OpenFOAM solve failed: {result.error_message or 'unknown error'}"
            )
        # A silent dry-run fallback (no OpenFOAM on PATH) must NOT read as success
        # for a real solve request — the licensed-run lane would record a false
        # finish. Mirrors the ANSYS router's fail-closed guard.
        if status == "dry_run" and not requested_dry_run:
            raise RuntimeError(
                "OpenFOAM solver unavailable: run fell back to dry-run "
                f"({result.error_message or 'no OpenFOAM on PATH'}). "
                "Run on a host with OpenFOAM installed."
            )
        return cfg

    # ------------------------------------------------------------------ #
    #  helpers                                                            #
    # ------------------------------------------------------------------ #
    @staticmethod
    def _resolve_output_dir(cfg: dict[str, Any], settings: dict[str, Any]) -> Path:
        result_folder = cfg.get("Analysis", {}).get("result_folder")
        base_dir = Path(result_folder) if result_folder else Path.cwd()
        output = settings.get("output_directory")
        if output is None:
            return base_dir.resolve()
        output_dir = Path(output)
        if output_dir.is_absolute():
            return output_dir
        return (base_dir / output_dir).resolve()
