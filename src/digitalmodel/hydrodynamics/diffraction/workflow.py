from __future__ import annotations

from pathlib import Path
from typing import Any

from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

_SUPPORTED_OPERATIONS = ("convert_spec", "run_orcawave", "run_aqwa")

# Solve operation -> (runner module, convenience fn, human label). Both runners
# share the same signature and fail-closed contract, so the lane's fixed
# ``python -m digitalmodel <input>`` command drives either solver (#900/#939).
_SOLVERS = {
    "run_orcawave": ("orcawave_runner", "run_orcawave", "OrcaWave"),
    "run_aqwa": ("aqwa_runner", "run_aqwa", "AQWA"),
}


class DiffractionWorkflow:
    """Engine router for diffraction-domain workflows.

    ``convert_spec`` (default, offline) converts a canonical spec into solver
    input decks. ``run_orcawave`` / ``run_aqwa`` (require an OrcaWave / AQWA
    license) convert and solve end-to-end from a single input file, so the
    licensed-run lane's fixed ``python -m digitalmodel <input>`` command can
    drive a solve (issues #900 / #939).
    """

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        settings = cfg.setdefault("diffraction", {})
        operation = settings.get("operation", "convert_spec")
        if operation == "convert_spec":
            return self._convert_spec(cfg, settings)
        if operation in _SOLVERS:
            return self._run_solver(cfg, settings, operation)
        raise ValueError(
            f"Unsupported diffraction operation: {operation!r}. "
            f"Supported operations: {', '.join(_SUPPORTED_OPERATIONS)}"
        )

    def _convert_spec(
        self, cfg: dict[str, Any], settings: dict[str, Any]
    ) -> dict[str, Any]:
        spec_path = self._resolve_spec_path(cfg, settings)
        output_dir = self._resolve_output_dir(cfg, settings)
        solver = settings.get("solver", "all").lower()
        fmt = settings.get("format", "single").lower()

        converter = SpecConverter(spec_path)
        issues = converter.validate()
        if issues:
            joined = "; ".join(issues)
            raise ValueError(f"Invalid diffraction spec: {joined}")

        outputs = self._convert(converter, solver, fmt, output_dir)
        settings["spec_path"] = str(spec_path)
        settings["output_directory"] = str(output_dir)
        settings["outputs"] = {
            name: str(path) for name, path in sorted(outputs.items())
        }
        settings["validation_issues"] = issues
        return cfg

    def _run_solver(
        self, cfg: dict[str, Any], settings: dict[str, Any], operation: str
    ) -> dict[str, Any]:
        import importlib

        # Lazy import: only pull the OrcFxAPI/AQWA-adjacent runner on a solve request.
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec

        module_name, func_name, label = _SOLVERS[operation]
        runner_module = importlib.import_module(
            f"digitalmodel.hydrodynamics.diffraction.{module_name}"
        )
        run_solve = getattr(runner_module, func_name)  # patch-friendly at call time

        spec_path = self._resolve_spec_path(cfg, settings)
        output_dir = self._resolve_output_dir(cfg, settings)
        requested_dry_run = bool(settings.get("dry_run", False))

        spec = DiffractionSpec.from_yaml(spec_path)
        result = run_solve(
            spec,
            output_dir=output_dir,
            dry_run=requested_dry_run,
            timeout_seconds=int(settings.get("timeout_seconds", 7200)),
            spec_path=spec_path,
        )

        status = str(getattr(result.status, "value", result.status)).lower()
        settings["spec_path"] = str(spec_path)
        settings["output_directory"] = str(getattr(result, "output_dir", output_dir))
        settings["run_status"] = status
        settings["outputs"] = {
            "input_file": str(getattr(result, "input_file", "") or ""),
            "modular_files": [
                str(p) for p in getattr(result, "modular_files", []) or []
            ],
            "mesh_files": [str(p) for p in getattr(result, "mesh_files", []) or []],
        }
        if status == "failed":
            raise RuntimeError(
                f"{label} solve failed: "
                f"{getattr(result, 'error_message', None) or 'unknown error'}"
            )
        # A silent dry-run fallback (no executable/license) must NOT read as success
        # for a real solve request — the licensed-run lane would record a false finish.
        if status == "dry_run" and not requested_dry_run:
            raise RuntimeError(
                f"{label} solver unavailable: run fell back to dry-run "
                f"(no {label} executable/license found). Run on a licensed host."
            )
        return cfg

    @staticmethod
    def _resolve_spec_path(cfg: dict[str, Any], settings: dict[str, Any]) -> Path:
        spec = settings.get("spec")
        if spec is None:
            raise ValueError("diffraction.spec is required")

        spec_path = Path(spec)
        if spec_path.is_absolute():
            return spec_path

        config_dir = cfg.get("_config_dir_path")
        if config_dir:
            return (Path(config_dir) / spec_path).resolve()
        return spec_path.resolve()

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

    @staticmethod
    def _convert(
        converter: SpecConverter, solver: str, fmt: str, output_dir: Path
    ) -> dict[str, Path]:
        if solver == "all":
            return converter.convert_all(format=fmt, output_dir=output_dir)

        path = converter.convert(solver=solver, format=fmt, output_dir=output_dir)
        return {solver: path}
