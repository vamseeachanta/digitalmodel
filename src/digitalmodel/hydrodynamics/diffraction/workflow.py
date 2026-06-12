from __future__ import annotations

from pathlib import Path
from typing import Any

from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter


class DiffractionWorkflow:
    """Engine router for offline diffraction-domain preparation workflows."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        settings = cfg.setdefault("diffraction", {})
        operation = settings.get("operation", "convert_spec")
        if operation != "convert_spec":
            raise ValueError(
                f"Unsupported diffraction operation: {operation!r}. "
                "Supported operations: convert_spec"
            )

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
