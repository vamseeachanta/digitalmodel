"""UV-workflow router for taper stress joint membrane sizing.

This is a minimal offline API RP 2RD-style screening calculation for a tapered
riser stress joint. It evaluates true wall tension and von Mises membrane stress
at each taper station; it does not depend on OrcaFlex or dynamic analysis.
"""

from __future__ import annotations

import csv
import math
from pathlib import Path
from typing import Any


REPO_ROOT = Path(__file__).resolve().parents[3]


def router(cfg: dict) -> dict:
    settings = cfg.get("tsj_sizing") or {}
    effective_tension_n = _effective_tension_n(settings)
    internal_pressure_pa = _pressure_pa(
        settings,
        ("internal_pressure_Pa", "internal_pressure_pa"),
        ("internal_pressure_MPa", "internal_pressure_mpa"),
        0.0,
    )
    external_pressure_pa = _pressure_pa(
        settings,
        ("external_pressure_Pa", "external_pressure_pa"),
        ("external_pressure_MPa", "external_pressure_mpa"),
        0.0,
    )
    smys_pa = _required_positive_float(settings, ("SMYS_Pa", "smys_Pa", "SMYS", "smys"))
    design_factor = _required_positive_float(settings, ("design_factor",))
    allowable_stress_pa = design_factor * smys_pa
    sections = _sections(settings)

    rows = [
        _section_result(
            section,
            effective_tension_n,
            internal_pressure_pa,
            external_pressure_pa,
            allowable_stress_pa,
        )
        for section in sections
    ]
    csv_path = _results_csv_path(cfg, settings)
    _write_results_csv(csv_path, rows)

    governing = max(rows, key=lambda row: float(row["utilisation"]))
    cfg["tsj_sizing"] = {
        "effective_tension_N": effective_tension_n,
        "internal_pressure_Pa": internal_pressure_pa,
        "external_pressure_Pa": external_pressure_pa,
        "SMYS_Pa": smys_pa,
        "design_factor": design_factor,
        "allowable_stress_Pa": allowable_stress_pa,
        "sections": len(rows),
        "max_utilisation": float(governing["utilisation"]),
        "governing_section": {
            "position_m": governing["position_m"],
            "OD_mm": governing["OD_mm"],
            "WT_mm": governing["WT_mm"],
            "utilisation": governing["utilisation"],
        },
        "passes": float(governing["utilisation"]) <= 1.0,
        "results_csv": _display_path(csv_path),
    }
    return cfg


def _section_result(
    section: dict[str, float],
    effective_tension_n: float,
    internal_pressure_pa: float,
    external_pressure_pa: float,
    allowable_stress_pa: float,
) -> dict[str, float]:
    od_m = section["OD_mm"] / 1000.0
    wt_m = section["WT_mm"] / 1000.0
    id_m = od_m - 2.0 * wt_m
    area_m2 = math.pi / 4.0 * (od_m**2 - id_m**2)
    inner_area_m2 = math.pi / 4.0 * id_m**2
    outer_area_m2 = math.pi / 4.0 * od_m**2
    inertia_m4 = math.pi / 64.0 * (od_m**4 - id_m**4)
    section_modulus_m3 = inertia_m4 / (od_m / 2.0)
    true_wall_tension_n = (
        effective_tension_n
        + internal_pressure_pa * inner_area_m2
        - external_pressure_pa * outer_area_m2
    )
    axial_stress_pa = true_wall_tension_n / area_m2
    hoop_stress_pa = (internal_pressure_pa - external_pressure_pa) * od_m / (2.0 * wt_m)
    vm_stress_pa = math.sqrt(
        axial_stress_pa**2 - axial_stress_pa * hoop_stress_pa + hoop_stress_pa**2
    )

    return {
        "position_m": section["position_m"],
        "OD_mm": section["OD_mm"],
        "WT_mm": section["WT_mm"],
        "area_m2": area_m2,
        "section_modulus_m3": section_modulus_m3,
        "true_wall_tension_N": true_wall_tension_n,
        "axial_stress_Pa": axial_stress_pa,
        "hoop_stress_Pa": hoop_stress_pa,
        "vm_stress_Pa": vm_stress_pa,
        "utilisation": vm_stress_pa / allowable_stress_pa,
    }


def _sections(settings: dict[str, Any]) -> list[dict[str, float]]:
    raw_sections = settings.get("taper")
    if not isinstance(raw_sections, list) or not raw_sections:
        raise ValueError("tsj_sizing taper must be a non-empty list")

    sections = []
    for index, raw_section in enumerate(raw_sections):
        if not isinstance(raw_section, dict):
            raise ValueError("tsj_sizing taper sections must be mappings")
        position_m = _required_positive_or_zero_float(raw_section, ("position_m",))
        od_mm = _required_positive_float(
            raw_section,
            ("outer_diameter_mm", "OD_mm", "od_mm"),
        )
        wt_mm = _required_positive_float(
            raw_section,
            ("wall_thickness_mm", "WT_mm", "wt_mm"),
        )
        if od_mm <= 2.0 * wt_mm:
            raise ValueError(
                f"tsj_sizing taper[{index}] wall_thickness_mm leaves no bore"
            )
        sections.append({"position_m": position_m, "OD_mm": od_mm, "WT_mm": wt_mm})

    if any(
        sections[index]["position_m"] <= sections[index - 1]["position_m"]
        for index in range(1, len(sections))
    ):
        raise ValueError("tsj_sizing taper positions must be strictly increasing")
    return sections


def _effective_tension_n(settings: dict[str, Any]) -> float:
    value = _first_value(settings, ("effective_tension_N", "effective_tension_n"))
    if value is not None:
        value = float(value)
    else:
        value = _first_value(
            settings,
            ("effective_tension_kN", "effective_tension_kn"),
        )
        if value is None:
            raise ValueError("tsj_sizing effective_tension_N is required")
        value = float(value) * 1000.0
    if value <= 0.0:
        raise ValueError("tsj_sizing effective_tension_N must be positive")
    return value


def _pressure_pa(
    settings: dict[str, Any],
    pa_aliases: tuple[str, ...],
    mpa_aliases: tuple[str, ...],
    default: float,
) -> float:
    value = _first_value(settings, pa_aliases)
    if value is not None:
        pressure = float(value)
    else:
        value = _first_value(settings, mpa_aliases)
        if value is None:
            return default
        pressure = float(value) * 1.0e6
    if pressure < 0.0:
        raise ValueError(f"tsj_sizing {pa_aliases[0]} must be non-negative")
    return pressure


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    stem = _input_stem(cfg)
    return output_dir / f"{stem}_tsj.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "tsj_sizing"))


def _write_results_csv(path: Path, rows: list[dict[str, float]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def _required_positive_float(
    values: dict[str, Any],
    aliases: tuple[str, ...],
) -> float:
    value = _first_value(values, aliases)
    if value is None:
        raise ValueError(f"tsj_sizing {aliases[0]} is required")
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"tsj_sizing {aliases[0]} must be positive")
    return value


def _required_positive_or_zero_float(
    values: dict[str, Any],
    aliases: tuple[str, ...],
) -> float:
    value = _first_value(values, aliases)
    if value is None:
        raise ValueError(f"tsj_sizing {aliases[0]} is required")
    value = float(value)
    if value < 0.0:
        raise ValueError(f"tsj_sizing {aliases[0]} must be non-negative")
    return value


def _first_value(values: dict[str, Any], aliases: tuple[str, ...]) -> Any:
    for alias in aliases:
        if values.get(alias) is not None:
            return values[alias]
    return None


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
