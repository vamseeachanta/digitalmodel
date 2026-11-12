"""IEA 15 MW / VolturnUS-S spec for the OrcaFlex modular generator.

Builds a ``ProjectInputSpec`` dictionary of model type ``fowt`` from the two
sourced files in ``docs/domains/orcaflex/wind/fowt-volturnus/``:
``basis.yml`` (system mass, geometry, mooring, environment) and
``generator_data.yml`` (turbine geometry, WAMIT hydrostatics and radiation,
HydroDyn quadratic damping). Units are converted to OrcaFlex units
(te, kN, m, s).

Conventions and simplifications, all stated in the returned ``source_note``:

* Rigid representation: hull mass properties are the whole-system values of
  ``basis.yml`` (platform, tower and RNA, adopted tower-top RNA reading).
* WAMIT ``.1`` finite-frequency entries are held for 11, 33, 55, 15 and 66;
  22, 44 and 24 are completed from the hull's three-fold symmetry
  (A22=A11, A44=A55, A24=-A15). The zero- and infinite-frequency matrices
  held in full show these relations to 0.1 % (tested).
* OrcaFlex other-damping is diagonal; the HydroDyn surge-pitch coupling
  entries of ``AddBQuad`` are not represented.
* Seabed contact stiffness and line hydrodynamic settings that are model
  settings rather than reference-design values are labelled as such.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any

import yaml

ROOT = Path(__file__).resolve().parents[3]
FOLDER = ROOT / "docs" / "domains" / "orcaflex" / "wind" / "fowt-volturnus"
ORCAFLEX_GRAVITY = 9.80665  # OrcaFlex default; the generated General section does not override it


def load_sources(folder: Path = FOLDER) -> tuple[dict, dict]:
    basis = yaml.safe_load((folder / "basis.yml").read_text(encoding="utf-8"))
    data = yaml.safe_load((folder / "generator_data.yml").read_text(encoding="utf-8"))
    return basis, data


def _v(node: dict) -> Any:
    return node["value"]


def _expand(row: list[float]) -> tuple[list[list[float]], list[list[float]]]:
    """6x6 added mass and damping (non-dimensional) from one radiation row."""
    _, a11, a33, a55, a15, a66, b11, b33, b55, b15, b66 = row
    a = [[0.0] * 6 for _ in range(6)]
    b = [[0.0] * 6 for _ in range(6)]
    for m, d11, d33, d55, d15, d66 in ((a, a11, a33, a55, a15, a66),
                                        (b, b11, b33, b55, b15, b66)):
        m[0][0] = m[1][1] = d11
        m[2][2] = d33
        m[3][3] = m[4][4] = d55
        m[5][5] = d66
        m[0][4] = m[4][0] = d15
        m[1][3] = m[3][1] = -d15
    return a, b


def _dimensional(matrix: list[list[float]], rho: float, factor: float = 1.0) -> list[list[float]]:
    """WAMIT non-dimensional to dimensional with L = 1 m (rho in te/m3)."""
    return [[rho * factor * value for value in row] for row in matrix]


def radiation_entries(data: dict, rho: float) -> list[dict]:
    hydro = data["hydrodynamics"]
    if _v(hydro["wamit_length_scale"]) != 1.0:
        raise ValueError("conversion assumes WAMITULEN = 1 m")
    entries = [{
        "period": 0.0,
        "added_mass": _dimensional(_v(hydro["added_mass_infinite_frequency_nondimensional"]), rho),
        "damping": [[0.0] * 6 for _ in range(6)],
    }]
    for row in hydro["radiation_by_period_nondimensional"]["value"]:
        period = row[0]
        omega = 2.0 * math.pi / period
        a, b = _expand(row)
        entries.append({"period": period,
                        "added_mass": _dimensional(a, rho),
                        "damping": _dimensional(b, rho, omega)})
    return entries


def hull_length(basis: dict) -> float:
    """Fore-aft extent of the column footprint (m)."""
    g = basis["geometry"]
    radius = _v(g["outer_column_radius"])
    return radius * 1.5 + _v(g["outer_column_diameter"])


def build_spec(basis: dict, data: dict, *, thrust_kN: float | None = None,
               wind_speed: float | None = None, name: str = "iea15_volturnus_s_lc0_statics",
               stages: list[float] | None = None, time_step: float = 0.1) -> dict:
    """Return a ProjectInputSpec dictionary for the rigid IEA 15 MW FOWT."""
    env = basis["environment"]
    system = basis["system"]
    mooring = basis["mooring"]
    turbine = data["turbine"]
    hydro = data["hydrodynamics"]
    rho = _v(env["water_density"]) / 1000.0
    g = ORCAFLEX_GRAVITY
    hst = _v(hydro["hydrostatic_stiffness_nondimensional"])
    block = [[rho * g * hst[i][j] for j in (2, 3, 4)] for i in (2, 3, 4)]
    quad = _v(hydro["additional_quadratic_damping"])
    nominal = _v(mooring["nominal_diameter"])
    fowt: dict[str, Any] = {
        "representation": "rigid",
        "gravity": g,
        "hull": {
            "name": "VolturnUS-S",
            "length": hull_length(basis),
            "mass": _v(system["mass"]) / 1000.0,
            "centre_of_mass": list(_v(system["center_of_gravity"])),
            "inertia_about_centre_of_mass": [[x / 1000.0 for x in row]
                                             for row in _v(system["inertia_cg"])],
            "displaced_volume": _v(hydro["reference_volume"]),
            "centre_of_buoyancy": list(_v(basis["published"]["center_of_buoyancy"])),
            "hydrostatic_stiffness_excluding_gravity": block,
            "radiation": radiation_entries(data, rho),
            "other_quadratic_damping": [quad[i][i] / 1000.0 for i in range(6)],
        },
        "mooring_line_types": [{
            "name": "Chain R3 185mm",
            "outer_diameter": _v(mooring["volume_equivalent_diameter"]),
            "mass_per_length": _v(mooring["mass_per_length"]) / 1000.0,
            "axial_stiffness": _v(mooring["axial_stiffness"]) / 1000.0,
            "minimum_breaking_load": _v(mooring["minimum_breaking_load"]) / 1000.0,
            "normal_drag_coefficient": 2.0,
            "axial_drag_coefficient": 0.4,
            "normal_added_mass_coefficient": 0.82,
            "axial_added_mass_coefficient": 0.27,
        }],
        "mooring_lines": [
            {"name": f"Mooring{k + 1}", "line_type": "Chain R3 185mm",
             "fairlead": list(fair), "anchor_xy": [anchor[0], anchor[1]],
             "anchor_height_above_seabed": anchor[2] + _v(env["water_depth"]),
             "length": _v(mooring["unstretched_length"])}
            for k, (fair, anchor) in enumerate(zip(_v(mooring["fairleads"]),
                                                   _v(mooring["anchors"])))
        ],
        "source_note": (
            "basis.yml and generator_data.yml (IEA-15-240-RWT at "
            f"{data['reference_commit'][:7]}; Allen et al. 2020 NREL/TP-5000-76773). "
            "Hull mass properties are whole-system values (rigid representation). "
            "WAMIT 22, 44, 24 completed by three-fold symmetry; AddBQuad off-diagonals "
            "dropped (OrcaFlex other damping is diagonal). Mooring Cd 2.0/0.4 and Ca "
            "0.82/0.27 from the MoorDyn input LINE TYPES at the same revision. "
            f"Nominal chain diameter {nominal} m is not used for hydrodynamics."
        ),
    }
    if thrust_kN is not None:
        fowt["rotor_thrust"] = {
            "thrust": thrust_kN,
            "hub_position": [_v(turbine["hub_overhang"]), 0.0, _v(turbine["hub_height"])],
            "shaft_tilt": _v(turbine["shaft_tilt"]),
            "wind_speed": wind_speed,
        }
    return {
        "metadata": {
            "name": name,
            "description": "IEA 15 MW reference turbine on the UMaine VolturnUS-S "
                           "semi-submersible; rigid floating wind model",
            "structure": "floating_wind_turbine",
            "operation": "in-place",
            "project": "public reference design",
        },
        "environment": {
            "water": {"depth": _v(env["water_depth"]), "density": rho},
            # Seabed contact stiffness is a model setting (Orcina K03 example
            # value), not a reference-design value.
            "seabed": {"stiffness": {"normal": 100, "shear": 100}},
        },
        "fowt": fowt,
        "simulation": {"time_step": time_step, "stages": stages or [10, 100]},
    }


def thrust_at(data: dict, wind_speed: float) -> float:
    """Steady rotor thrust (N) by linear interpolation of the WISDEM table."""
    rows = data["rotor_performance"]["value"]
    for lower, upper in zip(rows, rows[1:]):
        if lower[0] <= wind_speed <= upper[0]:
            f = (wind_speed - lower[0]) / (upper[0] - lower[0])
            return lower[3] + f * (upper[3] - lower[3])
    raise ValueError(f"wind speed {wind_speed} m/s outside the rotor performance table")


def _sea_state(hs: float, tp: float, gamma: float, direction: float = 0.0) -> dict:
    """JONSWAP train given by Hs, Tp and gamma.

    Tp and gamma go through the raw wave-train layer so that the environment
    builder emits ``WaveTp`` (not ``WaveTz``) and keeps gamma = 1.0, which the
    spec-level gamma field (gamma > 1) does not accept.
    """
    return {
        "waves": {"type": "jonswap", "height": hs, "period": tp, "direction": direction},
        "raw_properties": {"WaveTrains": [{
            "Name": "Wave 1", "WaveJONSWAPParameters": "Partially specified",
            "WaveTp": tp, "WaveGamma": gamma}]},
    }


def load_case_specs(basis: dict, data: dict, equilibrium: dict) -> dict[str, dict]:
    """Load-case matrix for the licensed runs.

    ``equilibrium`` holds the still-water position from the licence-free
    checks: ``surge`` (m), ``heave`` (m) and ``pitch`` (deg). Decay cases
    start from that position plus the stated offset and are excluded from
    statics so the offset is held at the start of dynamics.
    """
    env = basis["environment"]
    rated = _v(basis["turbine"]["rated_rotor_thrust"]) / 1000.0
    op, ext = env["operating"], env["extreme"]
    op_wind = _v(op["wind_speed"])
    cases: dict[str, dict] = {}
    cases["LC0_statics"] = build_spec(basis, data, name="LC0_statics", stages=[10, 100])
    decays = {
        "LC1_surge_decay": ([10.0, 0.0, 0.0], [0.0, 0.0, 0.0], 1200),
        "LC2_heave_decay": ([0.0, 0.0, -2.0], [0.0, 0.0, 0.0], 300),
        "LC3_pitch_decay": ([0.0, 0.0, 0.0], [0.0, 5.0, 0.0], 400),
    }
    for name, (offset, rotation, duration) in decays.items():
        spec = build_spec(basis, data, name=name, stages=[1, duration])
        hull = spec["fowt"]["hull"]
        hull["initial_position"] = [equilibrium["surge"] + offset[0], 0.0,
                                    equilibrium["heave"] + offset[2]]
        hull["initial_orientation"] = [0.0, equilibrium["pitch"] + rotation[1], 0.0]
        hull["included_in_statics"] = "None"
        cases[name] = spec
    cases["LC4a_rated_thrust_statics"] = build_spec(
        basis, data, name="LC4a_rated_thrust_statics", stages=[10, 100],
        thrust_kN=rated, wind_speed=_v(basis["turbine"]["rated_wind_speed"]))
    operating = build_spec(basis, data, name="LC4_operating", stages=[100, 3600],
                           thrust_kN=thrust_at(data, op_wind) / 1000.0, wind_speed=op_wind)
    operating["environment"].update(_sea_state(
        _v(op["significant_wave_height"]), _v(op["peak_period"]), _v(op["jonswap_gamma"])))
    operating["environment"]["wind"] = {"speed": op_wind, "direction": 0.0}
    operating["fowt"]["hull"]["include_wave_load_first_order"] = True
    cases["LC4_operating"] = operating
    parked = build_spec(basis, data, name="LC5_parked_extreme", stages=[100, 3600])
    parked["environment"].update(_sea_state(
        _v(ext["significant_wave_height"]), _v(ext["peak_period"]), _v(ext["jonswap_gamma"])))
    parked["environment"]["wind"] = {"speed": _v(ext["wind_speed"]), "direction": 0.0}
    parked["fowt"]["hull"]["include_wave_load_first_order"] = True
    cases["LC5_parked_extreme"] = parked
    return cases
