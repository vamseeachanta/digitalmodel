"""Engine router for ANSYS Mechanical APDL solves (digitalmodel #940).

``run_ansys`` (requires an ANSYS/MAPDL license) executes a prepared APDL script
from a single input file, so the licensed-run lane's fixed
``python -m digitalmodel <input>`` command can drive an ANSYS solve. Fail-closed:
a silent dry-run fallback (no executable/license) on a real solve request raises,
so the lane cannot record a false finish.

Input contract::

    basename: ansys
    ansys:
      operation: run_ansys
      script: model.inp        # prepared APDL script (relative to the config dir)
      output_directory: out
      dry_run: false
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

_SUPPORTED_OPERATIONS = ("run_ansys",)


class AnsysWorkflow:
    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        settings = cfg.setdefault("ansys", {})
        operation = settings.get("operation", "run_ansys")
        if operation == "run_ansys":
            return self._run_ansys(cfg, settings)
        raise ValueError(
            f"Unsupported ansys operation: {operation!r}. "
            f"Supported operations: {', '.join(_SUPPORTED_OPERATIONS)}"
        )

    def _run_ansys(
        self, cfg: dict[str, Any], settings: dict[str, Any]
    ) -> dict[str, Any]:
        # Lazy import: only pull the MAPDL-adjacent runner on a solve request.
        from digitalmodel.ansys.runner import run_ansys

        script_path = self._resolve_script_path(cfg, settings)
        output_dir = self._resolve_output_dir(cfg, settings)
        requested_dry_run = bool(settings.get("dry_run", False))

        result = run_ansys(
            script_path,
            output_dir=output_dir,
            dry_run=requested_dry_run,
            timeout_seconds=int(settings.get("timeout_seconds", 7200)),
            executable_path=settings.get("executable_path"),
        )

        status = str(getattr(result.status, "value", result.status)).lower()
        settings["script_path"] = str(script_path)
        settings["output_directory"] = str(getattr(result, "output_dir", output_dir))
        settings["run_status"] = status
        settings["outputs"] = {
            "log_file": str(getattr(result, "log_file", "") or ""),
            "result_files": [str(p) for p in getattr(result, "result_files", []) or []],
            "return_code": getattr(result, "return_code", None),
        }
        if status == "failed":
            raise RuntimeError(
                f"ANSYS solve failed: "
                f"{getattr(result, 'error_message', None) or 'unknown error'}"
            )
        # A silent dry-run fallback (no executable/license) must NOT read as success
        # for a real solve request — the licensed-run lane would record a false finish.
        if status == "dry_run" and not requested_dry_run:
            raise RuntimeError(
                "ANSYS solver unavailable: run fell back to dry-run "
                "(no ANSYS/MAPDL executable/license found). Run on a licensed host."
            )
        return cfg

    @staticmethod
    def _resolve_script_path(cfg: dict[str, Any], settings: dict[str, Any]) -> Path:
        script = settings.get("script")
        if script is None:
            raise ValueError("ansys.script is required")
        script_path = Path(script)
        if script_path.is_absolute():
            return script_path
        config_dir = cfg.get("_config_dir_path")
        if config_dir:
            return (Path(config_dir) / script_path).resolve()
        return script_path.resolve()

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
