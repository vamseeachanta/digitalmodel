# ABOUTME: Routed workflow (basename alt_fuel_ship_sizing) composing LH2
# ABOUTME: fuel-chain sizing, wind-assist, trade study and ITC 69 tonnage.
"""Durable workflow: alternative-fuel (LH2) ship-design sizing toolkit.

Concept-design screening only. Composes four calculators:

1. LH2 fuel-chain sizing (tank volume from endurance + powering + fuel-cell
   efficiency chain; boil-off from tank heat leak; net/gross volumes),
2. wind-assist integration (user thrust matrix over a route wind rose),
3. endurance / fuel-cost trade study (speed x wind-assist sweep), and
4. ITC 69 gross/net tonnage estimate.

Shaft-power demand is an input (bare-hull resistance/propulsion, weight
groups and stability are separate lanes and feed this workflow, they are
not reimplemented here).

Config schema (YAML basename ``alt_fuel_ship_sizing``)::

    basename: alt_fuel_ship_sizing
    alt_fuel_ship_sizing:
      route:
        distance_nm: 4000.0
        service_speed_kn: 12.0
      powering:
        shaft_power_kw: 5000.0        # at service speed, incl. margins (input lane)
        hotel_load_kw: 500.0
        # speed_power_curve: [{speed_kn: 10, shaft_power_kw: 3000}, ...]  # optional
        # power_speed_exponent: 3.0   # cube-law fallback exponent
      efficiency:
        fuel_cell_lhv: 0.50
        electric_drivetrain: 0.95
      fuel:                            # optional; defaults = published LH2 values
        margin_fraction: 0.10
        # density_kg_per_m3: 70.85
        # lhv_mj_per_kg: 119.96
        # heat_of_vaporization_kj_per_kg: 446.0
        # storage_temperature_k: 20.28
      tank:
        ullage_fraction: 0.08
        length_to_diameter: 5.0
        insulation_heat_flux_w_per_m2: 1.5   # or u_value_w_per_m2_k (+ ambient_temperature_k)
        bog_handling: consumed         # consumed | lost
      wind_assist:                     # optional
        enabled: true
        propulsive_efficiency: 0.70
        thrust_matrix:
          tws_mps: [4.0, 8.0, 12.0, 16.0]
          twa_deg: [0.0, 45.0, 90.0, 135.0, 180.0]
          thrust_kn:                   # rows = tws, columns = twa
            - [0.0, 5.0, 10.0, 8.0, 2.0]
            - ...
        wind_rose:
          - {tws_mps: 8.0, twa_deg: 60.0, probability: 0.25}
          - ...
      trade_study:                     # optional
        speeds_kn: [10.0, 12.0, 14.0]
        wind_assist: [false, true]
      tonnage:                         # optional; ITC 69 Annex I Regs 3-4
        total_enclosed_volume_m3: 60000.0
        cargo_volume_m3: 47000.0
        moulded_draught_m: 10.0
        moulded_depth_m: 15.0
        passengers_in_cabins_up_to_8_berths: 0
        passengers_other: 0
      economics:                       # optional
        fuel_price_per_kg: 6.0
      output_dir: results

Outputs (CSV, under ``output_dir``): ``<stem>_fuel_chain_summary.csv``;
``<stem>_wind_assist_bins.csv`` when wind-assist is enabled;
``<stem>_trade_study.csv`` when a trade study is requested (drops straight
into ``report_pack`` ``results.tables``); ``<stem>_tonnage.csv`` when
tonnage inputs are given. The enriched config carries the same results.
"""

from __future__ import annotations

import csv
from dataclasses import asdict
from pathlib import Path
from typing import Any

from digitalmodel.alt_fuel_ship_sizing.lh2_fuel_chain import (
    FuelProperties,
    TankParameters,
    size_fuel_chain,
)
from digitalmodel.alt_fuel_ship_sizing.tonnage import tonnage
from digitalmodel.alt_fuel_ship_sizing.trade_study import (
    TradeStudyInputs,
    WindAssistInputs,
    run_trade_study,
)
from digitalmodel.alt_fuel_ship_sizing.wind_assist import (
    ThrustMatrix,
    WindRoseBin,
    wind_assist_saving,
)

REPO_ROOT = Path(__file__).resolve().parents[3]
_LABEL = "alt_fuel_ship_sizing"


def router(cfg: dict) -> dict:
    settings = cfg.get(_LABEL) or {}
    if not isinstance(settings, dict) or not settings:
        raise ValueError(f"{_LABEL} mapping is required")

    route = _mapping(settings, "route")
    distance_nm = _positive(route, "distance_nm", "route.distance_nm")
    speed_kn = _positive(route, "service_speed_kn", "route.service_speed_kn")

    powering = _mapping(settings, "powering")
    shaft_power_kw = _positive(powering, "shaft_power_kw", "powering.shaft_power_kw")
    hotel_load_kw = _non_negative(powering, "hotel_load_kw", "powering.hotel_load_kw")

    efficiency = _mapping(settings, "efficiency")
    eta_fc = _positive(efficiency, "fuel_cell_lhv", "efficiency.fuel_cell_lhv")
    eta_el = _positive(
        efficiency, "electric_drivetrain", "efficiency.electric_drivetrain"
    )

    fuel_settings = settings.get("fuel") or {}
    fuel = _fuel_properties(fuel_settings)
    margin = float(fuel_settings.get("margin_fraction", 0.10))

    tank = _tank_parameters(_mapping(settings, "tank"))

    wind_settings = settings.get("wind_assist") or {}
    wind_enabled = bool(wind_settings.get("enabled", bool(wind_settings)))
    wind_inputs = _wind_inputs(wind_settings) if wind_enabled else None

    economics = settings.get("economics") or {}
    fuel_price = economics.get("fuel_price_per_kg")
    fuel_price = float(fuel_price) if fuel_price is not None else None

    # --- design point: wind assist first, then the fuel chain -------------
    wind_result = None
    effective_shaft_power = shaft_power_kw
    if wind_inputs is not None:
        wind_result = wind_assist_saving(
            wind_inputs.matrix,
            wind_inputs.wind_rose,
            ship_speed_kn=speed_kn,
            required_shaft_power_kw=shaft_power_kw,
            propulsive_efficiency=wind_inputs.propulsive_efficiency,
        )
        effective_shaft_power = wind_result.effective_shaft_power_kw

    chain = size_fuel_chain(
        distance_nm=distance_nm,
        speed_kn=speed_kn,
        shaft_power_kw=effective_shaft_power,
        hotel_load_kw=hotel_load_kw,
        eta_electric_drivetrain=eta_el,
        eta_fuel_cell_lhv=eta_fc,
        tank=tank,
        fuel=fuel,
        fuel_margin_fraction=margin,
    )

    summary: dict[str, Any] = {
        "distance_nm": distance_nm,
        "service_speed_kn": speed_kn,
        "shaft_power_kw": shaft_power_kw,
        "wind_assist_enabled": wind_inputs is not None,
        "wind_power_saving_kw": (
            wind_result.expected_power_saving_kw if wind_result else 0.0
        ),
        "wind_saving_fraction": wind_result.saving_fraction if wind_result else 0.0,
        "effective_shaft_power_kw": effective_shaft_power,
        "endurance_hours": chain.endurance_hours,
        "electrical_power_kw": chain.electrical_power_kw,
        "fuel_lhv_power_kw": chain.fuel_lhv_power_kw,
        "fuel_mass_flow_kg_per_h": chain.fuel_mass_flow_kg_per_h,
        "voyage_fuel_kg": chain.voyage_fuel_kg,
        "bog_lost_kg": chain.bog_lost_kg,
        "required_fuel_mass_kg": chain.required_fuel_mass_kg,
        "net_tank_volume_m3": chain.net_tank_volume_m3,
        "gross_tank_volume_m3": chain.gross_tank_volume_m3,
        "tank_diameter_m": chain.tank.diameter_m,
        "tank_length_m": chain.tank.length_m,
        "tank_surface_area_m2": chain.tank.surface_area_m2,
        "tank_heat_leak_w": chain.boil_off.heat_leak_w,
        "bog_kg_per_day": chain.boil_off.bog_kg_per_day,
        "bog_rate_percent_per_day": chain.boil_off.bog_rate_percent_per_day,
        "bog_handling": chain.bog_handling,
        "bog_exceeds_consumption": chain.bog_exceeds_consumption,
    }
    if fuel_price is not None:
        summary["fuel_cost"] = fuel_price * chain.required_fuel_mass_kg

    result: dict[str, Any] = {
        "method": "alt_fuel_lh2_concept_sizing",
        "posture": "concept-design screening (public math, uncalibrated)",
        "summary": summary,
        "warnings": list(chain.warnings),
    }

    csv_paths: dict[str, Path] = {}
    csv_paths["fuel_chain_summary_csv"] = _output_path(
        cfg, settings, "fuel_chain_summary.csv"
    )
    _write_csv(csv_paths["fuel_chain_summary_csv"], [summary])

    if wind_result is not None:
        bin_rows = [asdict(item) for item in wind_result.bins]
        csv_paths["wind_assist_bins_csv"] = _output_path(
            cfg, settings, "wind_assist_bins.csv"
        )
        _write_csv(csv_paths["wind_assist_bins_csv"], bin_rows)
        result["wind_assist"] = {
            "expected_power_saving_kw": wind_result.expected_power_saving_kw,
            "saving_fraction": wind_result.saving_fraction,
            "effective_shaft_power_kw": wind_result.effective_shaft_power_kw,
            "bins": bin_rows,
        }

    trade_settings = settings.get("trade_study") or {}
    if trade_settings:
        rows = run_trade_study(
            TradeStudyInputs(
                distance_nm=distance_nm,
                reference_speed_kn=speed_kn,
                reference_shaft_power_kw=shaft_power_kw,
                hotel_load_kw=hotel_load_kw,
                eta_electric_drivetrain=eta_el,
                eta_fuel_cell_lhv=eta_fc,
                tank=tank,
                fuel=fuel,
                fuel_margin_fraction=margin,
                power_speed_exponent=float(
                    powering.get("power_speed_exponent", 3.0)
                ),
                speed_power_curve=_speed_power_curve(powering),
                wind=wind_inputs,
                fuel_price_per_kg=fuel_price,
            ),
            speeds_kn=_float_list(
                trade_settings, "speeds_kn", "trade_study.speeds_kn"
            ),
            wind_assist_options=_bool_list(
                trade_settings, "wind_assist", "trade_study.wind_assist"
            ),
        )
        csv_paths["trade_study_csv"] = _output_path(cfg, settings, "trade_study.csv")
        _write_csv(csv_paths["trade_study_csv"], rows)
        result["trade_study"] = rows

    tonnage_settings = settings.get("tonnage") or {}
    if tonnage_settings:
        ton = tonnage(
            total_enclosed_volume_m3=_positive(
                tonnage_settings,
                "total_enclosed_volume_m3",
                "tonnage.total_enclosed_volume_m3",
            ),
            cargo_volume_m3=_positive(
                tonnage_settings, "cargo_volume_m3", "tonnage.cargo_volume_m3"
            ),
            moulded_draught_m=_positive(
                tonnage_settings, "moulded_draught_m", "tonnage.moulded_draught_m"
            ),
            moulded_depth_m=_positive(
                tonnage_settings, "moulded_depth_m", "tonnage.moulded_depth_m"
            ),
            passengers_in_cabins_up_to_8_berths=int(
                tonnage_settings.get("passengers_in_cabins_up_to_8_berths", 0)
            ),
            passengers_other=int(tonnage_settings.get("passengers_other", 0)),
        )
        ton_row = asdict(ton)
        csv_paths["tonnage_csv"] = _output_path(cfg, settings, "tonnage.csv")
        _write_csv(csv_paths["tonnage_csv"], [ton_row])
        result["tonnage"] = ton_row

    result.update(
        {name: _display_path(path) for name, path in csv_paths.items()}
    )
    cfg[_LABEL] = result
    return cfg


# --- input parsing ---------------------------------------------------------


def _mapping(settings: dict[str, Any], name: str) -> dict[str, Any]:
    value = settings.get(name)
    if not isinstance(value, dict) or not value:
        raise ValueError(f"{_LABEL} {name} mapping is required")
    return value


def _positive(source: dict[str, Any], name: str, label: str | None = None) -> float:
    label = label or name
    value = source.get(name)
    if value is None:
        raise ValueError(f"{_LABEL} {label} is required")
    value = float(value)
    if value <= 0.0:
        raise ValueError(f"{_LABEL} {label} must be positive")
    return value


def _non_negative(
    source: dict[str, Any], name: str, label: str | None = None
) -> float:
    label = label or name
    value = source.get(name)
    if value is None:
        raise ValueError(f"{_LABEL} {label} is required")
    value = float(value)
    if value < 0.0:
        raise ValueError(f"{_LABEL} {label} must be non-negative")
    return value


def _float_list(source: dict[str, Any], name: str, label: str) -> list[float]:
    value = source.get(name)
    if not isinstance(value, list) or not value:
        raise ValueError(f"{_LABEL} {label} must be a non-empty list")
    return [float(v) for v in value]


def _bool_list(source: dict[str, Any], name: str, label: str) -> list[bool]:
    value = source.get(name)
    if not isinstance(value, list) or not value:
        raise ValueError(f"{_LABEL} {label} must be a non-empty list")
    return [bool(v) for v in value]


def _fuel_properties(fuel_settings: dict[str, Any]) -> FuelProperties:
    defaults = FuelProperties()
    return FuelProperties(
        density_kg_per_m3=float(
            fuel_settings.get("density_kg_per_m3", defaults.density_kg_per_m3)
        ),
        lhv_mj_per_kg=float(
            fuel_settings.get("lhv_mj_per_kg", defaults.lhv_mj_per_kg)
        ),
        heat_of_vaporization_kj_per_kg=float(
            fuel_settings.get(
                "heat_of_vaporization_kj_per_kg",
                defaults.heat_of_vaporization_kj_per_kg,
            )
        ),
        storage_temperature_k=float(
            fuel_settings.get(
                "storage_temperature_k", defaults.storage_temperature_k
            )
        ),
    )


def _tank_parameters(tank_settings: dict[str, Any]) -> TankParameters:
    heat_flux = tank_settings.get("insulation_heat_flux_w_per_m2")
    u_value = tank_settings.get("u_value_w_per_m2_k")
    return TankParameters(
        ullage_fraction=float(tank_settings.get("ullage_fraction", 0.08)),
        length_to_diameter=float(tank_settings.get("length_to_diameter", 5.0)),
        insulation_heat_flux_w_per_m2=(
            float(heat_flux) if heat_flux is not None else None
        ),
        u_value_w_per_m2_k=float(u_value) if u_value is not None else None,
        ambient_temperature_k=float(
            tank_settings.get("ambient_temperature_k", 293.15)
        ),
        bog_handling=str(tank_settings.get("bog_handling", "consumed")),
    )


def _wind_inputs(wind_settings: dict[str, Any]) -> WindAssistInputs:
    matrix_settings = _mapping(wind_settings, "thrust_matrix")
    matrix = ThrustMatrix.from_lists(
        _float_list(matrix_settings, "tws_mps", "wind_assist.thrust_matrix.tws_mps"),
        _float_list(matrix_settings, "twa_deg", "wind_assist.thrust_matrix.twa_deg"),
        matrix_settings.get("thrust_kn") or [],
    )
    rose_raw = wind_settings.get("wind_rose")
    if not isinstance(rose_raw, list) or not rose_raw:
        raise ValueError(f"{_LABEL} wind_assist.wind_rose must be a non-empty list")
    rose = [
        WindRoseBin(
            tws_mps=_non_negative(item, "tws_mps", f"wind_rose[{i}].tws_mps"),
            twa_deg=float(item.get("twa_deg", 0.0)),
            probability=_non_negative(
                item, "probability", f"wind_rose[{i}].probability"
            ),
        )
        for i, item in enumerate(rose_raw)
    ]
    return WindAssistInputs(
        matrix=matrix,
        wind_rose=rose,
        propulsive_efficiency=_positive(
            wind_settings, "propulsive_efficiency", "wind_assist.propulsive_efficiency"
        ),
    )


def _speed_power_curve(
    powering: dict[str, Any],
) -> list[tuple[float, float]] | None:
    curve = powering.get("speed_power_curve")
    if curve is None:
        return None
    if not isinstance(curve, list) or not curve:
        raise ValueError(f"{_LABEL} powering.speed_power_curve must be a list")
    points: list[tuple[float, float]] = []
    for i, item in enumerate(curve):
        if not isinstance(item, dict):
            raise ValueError(f"{_LABEL} speed_power_curve[{i}] must be a mapping")
        points.append(
            (
                _positive(item, "speed_kn", f"speed_power_curve[{i}].speed_kn"),
                _positive(
                    item, "shaft_power_kw", f"speed_power_curve[{i}].shaft_power_kw"
                ),
            )
        )
    return points


# --- output plumbing (sibling-module pattern) ------------------------------


def _output_path(cfg: dict, settings: dict[str, Any], suffix: str) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir / f"{_input_stem(cfg)}_{suffix}"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", _LABEL))


def _write_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    if not rows:
        raise ValueError(f"{_LABEL} cannot write empty results")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
