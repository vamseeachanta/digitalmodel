"""UV-workflow router for stress-strain material curves."""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any

import numpy as np

from digitalmodel.structural.stress.stress_strain import (
    MaterialModel,
    MaterialProperties,
    StressStrainAnalyzer,
)


REPO_ROOT = Path(__file__).resolve().parents[4]


def router(cfg: dict) -> dict:
    settings = cfg.get("stress_strain") or {}
    material_data = settings.get("material") or {}
    material = _material_properties(settings, material_data)
    model = _material_model(settings.get("model", MaterialModel.RAMBERG_OSGOOD.value))
    strains = _strain_grid(settings)
    parameters = _model_parameters(model, settings, material_data, material)

    analyzer = StressStrainAnalyzer(material=material)
    curve = analyzer.generate_curve(model, np.array(strains), **parameters)
    stresses = _origin_normalised_stresses(curve.strains, curve.stresses)

    csv_path = _curve_csv_path(cfg, settings)
    _write_curve_csv(csv_path, curve.strains, stresses)

    cfg["stress_strain"] = {
        "model": model.value,
        "material": material_data.get("name"),
        "unit_system": material_data.get("unit_system"),
        "points": len(curve.strains),
        "elastic_modulus": material.elastic_modulus,
        "yield_strength": material.yield_strength,
        "curve_csv": _display_path(csv_path),
    }
    return cfg


def _material_properties(settings: dict, material_data: dict) -> MaterialProperties:
    return MaterialProperties(
        elastic_modulus=_required_float(settings, material_data, "elastic_modulus"),
        yield_strength=_required_float(settings, material_data, "yield_strength"),
        ultimate_strength=_optional_float(settings, material_data, "ultimate_strength"),
        poisson_ratio=_optional_float(settings, material_data, "poisson_ratio", 0.3),
        density=_optional_float(settings, material_data, "density"),
        ramberg_osgood_k=_optional_float(settings, material_data, "ramberg_osgood_k"),
        ramberg_osgood_n=_optional_float(settings, material_data, "ramberg_osgood_n"),
        proportional_limit=_optional_float(settings, material_data, "proportional_limit"),
        fracture_strain=_optional_float(settings, material_data, "fracture_strain"),
        strain_hardening_exponent=_optional_float(
            settings, material_data, "strain_hardening_exponent"
        ),
    )


def _material_model(model_name: str) -> MaterialModel:
    value = str(model_name).strip().lower().replace("-", "_")
    model = MaterialModel(value)
    if model not in {
        MaterialModel.LINEAR_ELASTIC,
        MaterialModel.RAMBERG_OSGOOD,
        MaterialModel.BILINEAR,
    }:
        raise ValueError(f"Unsupported stress_strain model: {value}")
    return model


def _strain_grid(settings: dict) -> list[float]:
    grid = settings.get("strain_grid", settings.get("strains"))
    if isinstance(grid, dict):
        start = float(grid["start"])
        stop = float(grid["stop"])
        num = int(grid["num"])
        if num < 2:
            raise ValueError("stress_strain strain_grid.num must be at least 2")
        step = (stop - start) / (num - 1)
        strains = [start + index * step for index in range(num)]
    elif isinstance(grid, list):
        strains = [float(value) for value in grid]
    else:
        raise ValueError("stress_strain requires strain_grid as a list or mapping")

    if any(strain < 0 for strain in strains):
        raise ValueError("stress_strain strain values must be non-negative")
    if any(strains[index] <= strains[index - 1] for index in range(1, len(strains))):
        raise ValueError("stress_strain strain values must be strictly increasing")
    return strains


def _model_parameters(
    model: MaterialModel,
    settings: dict,
    material_data: dict,
    material: MaterialProperties,
) -> dict[str, float]:
    parameters = settings.get("parameters") or {}
    base = {"elastic_modulus": material.elastic_modulus}
    if model == MaterialModel.LINEAR_ELASTIC:
        return base
    if model == MaterialModel.RAMBERG_OSGOOD:
        return {
            **base,
            "yield_strength": material.yield_strength,
            "k": _lookup_float(
                parameters, material_data, settings, name="ramberg_osgood_k"
            ),
            "n": _lookup_float(
                parameters, material_data, settings, name="ramberg_osgood_n"
            ),
        }
    return {
        **base,
        "yield_strength": material.yield_strength,
        "hardening_modulus": _lookup_optional_float(
            parameters, material_data, settings, name="hardening_modulus"
        ),
    }


def _origin_normalised_stresses(strains: list[float], stresses: list[float]) -> list[float]:
    normalised = [float(stress) for stress in stresses]
    if strains and abs(float(strains[0])) <= 1.0e-15:
        normalised[0] = 0.0
    return normalised


def _curve_csv_path(cfg: dict, settings: dict) -> Path:
    cfg_dir = _config_dir(cfg)
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = cfg_dir / output_dir
    stem = _input_stem(cfg)
    return output_dir / f"{stem}_stress_strain.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "stress_strain"))


def _write_curve_csv(path: Path, strains: list[float], stresses: list[float]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["strain", "stress"])
        writer.writerows(zip(strains, stresses))


def _required_float(settings: dict, material_data: dict, name: str) -> float:
    value = _lookup_optional_float(material_data, settings, {}, name=name)
    if value is None:
        raise ValueError(f"stress_strain material.{name} is required")
    return value


def _optional_float(
    settings: dict,
    material_data: dict,
    name: str,
    default: float | None = None,
) -> float | None:
    value = _lookup_optional_float(material_data, settings, {}, name=name)
    return default if value is None else value


def _lookup_float(*sources: dict[str, Any], name: str) -> float:
    value = _lookup_optional_float(*sources, name=name)
    if value is None:
        raise ValueError(f"stress_strain {name} is required")
    return value


def _lookup_optional_float(*sources: dict[str, Any], name: str) -> float | None:
    aliases = {
        "ramberg_osgood_k": ("ramberg_osgood_k", "k"),
        "ramberg_osgood_n": ("ramberg_osgood_n", "n"),
    }.get(name, (name,))
    for source in sources:
        for alias in aliases:
            if source.get(alias) is not None:
                return float(source[alias])
    return None


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
