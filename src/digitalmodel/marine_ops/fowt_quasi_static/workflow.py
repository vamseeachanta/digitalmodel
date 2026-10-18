# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reproducible public-reference hydrodynamic and quasi-static mooring workflow."""
from __future__ import annotations

from itertools import product
import ast
from hashlib import sha256
from importlib.metadata import version
import json
from pathlib import Path

import numpy as np
import yaml

from .basis import aggregate
from .geometry import analytical_volume
from .hydrodynamics import coefficient_arrays, solve_hydrodynamics
from .mooring import equilibrate, forces, line_results, mooring_system, stiffness
from .response import mass_matrix, natural_periods, response_operators, spectrum, wave_statistics


def load_basis(root: Path, input_path: Path | None = None) -> dict:
    """Load attributed data and apply the explicit case-input analysis settings."""
    root = root.resolve()
    input_path = input_path or root/"examples/workflows/fowt-volturnus-mooring/input.yml"
    config = yaml.safe_load(input_path.read_text())
    basis_path = (root/config["basis"]).resolve()
    if root not in basis_path.parents:
        raise ValueError("Basis must be contained in the source root")
    basis: dict = yaml.safe_load(basis_path.read_text())
    basis["analysis"].update(config["analysis"])
    return basis


def interpolate_hydrodynamics(data: dict, omega: np.ndarray) -> dict:
    """Interpolate coefficients before solving the dense impedance equation."""
    if omega[0] < data["omega"][0] or omega[-1] > data["omega"][-1]:
        raise ValueError("Hydrodynamic extrapolation is not permitted")
    result = {"omega": omega}
    for key in ("added", "radiation", "excitation"):
        values = data[key]
        columns = values.reshape(len(data["omega"]), -1).T
        interpolated = [np.interp(omega, data["omega"], col.real)
                        + 1j*np.interp(omega, data["omega"], col.imag) for col in columns]
        array = np.asarray(interpolated).T.reshape((len(omega), *values.shape[1:]))
        result[key] = array if np.iscomplexobj(values) else array.real
    return result


def constraint_reaction(residual: np.ndarray, mass: float, cg: np.ndarray,
                         volume: float, cob: np.ndarray, rho: float, gravity: float) -> list:
    """Balance residual loads, weight and buoyancy at fixed draft and attitude."""
    weight, buoyancy = np.array([0., 0., -mass*gravity]), np.array([0., 0., volume*rho*gravity])
    total = residual + np.r_[weight+buoyancy, np.cross(cg, weight)+np.cross(cob, buoyancy)]
    reaction: list = (-total[2:]).tolist()
    return reaction


def frequency_grid() -> np.ndarray:
    """Return the analysis grid, including low-frequency period-search points."""
    return np.unique(np.round(np.r_[np.geomspace(0.02, 0.05, 6),
                                   np.arange(0.05, 0.501, 0.01),
                                   np.arange(0.55, 2.001, 0.05)], 8))


def source_digest(path: Path) -> dict:
    """Bind raw executed bytes and parsed calculation syntax separately."""
    raw = path.read_bytes()
    syntax = ast.dump(ast.parse(raw), include_attributes=False).encode()
    return {"raw": sha256(raw).hexdigest(), "syntax": sha256(syntax).hexdigest()}


def executed_code_digests() -> dict:
    """Return exact-byte digests of the panel calculation's local dependencies."""
    return {name: source_digest(Path(__file__).parent/name)["raw"]
            for name in ("basis.py", "geometry.py", "hydrodynamics.py")}


def bem_inputs(basis: dict, step: float) -> dict:
    """Describe all retained BEM inputs, ordered settings and numerical versions."""
    physical = {key: value for key, value in basis.items() if key != "analysis"}
    canonical = json.dumps(physical, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()
    return {"basis_sha256": sha256(canonical).hexdigest(), "panel_size": step,
            "omega": frequency_grid().tolist(), "headings": basis["analysis"]["headings"]["value"],
            "engine_syntax_sha256": {name: source_digest(Path(__file__).parent/name)["syntax"]
                                      for name in ("basis.py", "geometry.py", "hydrodynamics.py")},
            "versions": {name: version(name) for name in
                         ("capytaine", "numpy", "scipy", "shapely", "xarray")}}


def _output_digests(scratch: Path, name: str) -> dict:
    return {f"{name}{suffix}": sha256((scratch/f"{name}{suffix}").read_bytes()).hexdigest()
            for suffix in (".npz", "-hydrostatics.json")}


def verify_retained_panels(basis: dict, scratch: Path) -> dict:
    """Reject stale inputs, incomplete metadata or altered retained coefficients."""
    records = {}
    for size, name in ((2.5, "coarse"), (2.0, "fine")):
        path = scratch/f"{name}-provenance.json"
        if not path.is_file():
            raise ValueError("BEM input provenance is missing")
        record = json.loads(path.read_text())
        if record.get("inputs") != bem_inputs(basis, size):
            raise ValueError("BEM input provenance differs from the current calculation")
        executed = record.get("executed_code_sha256", {})
        if set(executed) != set(executed_code_digests()) or any(
                len(digest) != 64 or any(c not in "0123456789abcdef" for c in digest)
                for digest in executed.values()):
            raise ValueError("BEM input provenance lacks raw executed-code digests")
        if record.get("outputs") != _output_digests(scratch, name):
            raise ValueError("BEM output digest mismatch")
        records[name] = record
    return records


def panel_runs(basis: dict, scratch: Path) -> dict:
    """Compute and retain two panel resolutions, with untruncated frequency axes."""
    results = {}
    for size, name in ((2.5, "coarse"), (2.0, "fine")):
        signature = bem_inputs(basis, size)
        executed = executed_code_digests()
        dataset, hydro = solve_hydrodynamics(basis, size, frequency_grid(),
                                             np.asarray(basis["analysis"]["headings"]["value"]))
        a, b, excitation = coefficient_arrays(dataset)
        if not all(np.isfinite(array).all() for array in (a, b, excitation)):
            raise ValueError("Nonfinite hydrodynamic coefficients")
        np.savez(scratch/f"{name}.npz", omega=frequency_grid(), added=a,
                 radiation=b, excitation=excitation, stiffness=hydro["stiffness"])
        (scratch/f"{name}-hydrostatics.json").write_text(json.dumps(hydro, indent=2))
        if bem_inputs(basis, size) != signature:
            raise ValueError("BEM inputs changed during the calculation")
        metadata = {"inputs": signature, "executed_code_sha256": executed,
                    "outputs": _output_digests(scratch, name)}
        (scratch/f"{name}-provenance.json").write_text(json.dumps(metadata, sort_keys=True, indent=2))
        results[name] = hydro
    return results


def _spectral_response(omega: np.ndarray, operators: np.ndarray, sea: dict,
                        duration: float) -> list:
    mask = omega >= 0.05
    omega, operators = omega[mask], operators[mask]
    density = spectrum(omega, sea["significant_wave_height"]["value"],
                       sea["peak_period"]["value"], sea["jonswap_gamma"]["value"])
    statistics = []
    for axis in (0, 1):
        statistics.append(wave_statistics(omega, operators[:, axis], density, duration))
    return statistics


def _design_tensions(system: object, mean: np.ndarray, statistics: list,
                      basis: dict, damaged: bool) -> tuple:
    candidates = []
    for sign_x, sign_y in product((-1, 1), repeat=2):
        offset = mean.copy()
        offset[:2] += np.array([sign_x, sign_y])*[s["maximum"] for s in statistics]
        lines = line_results(system, offset, basis, damaged)
        governing = max(lines, key=lambda line: line["utilisation"])
        candidates.append((governing["utilisation"], offset, lines, governing))
    _, offset, lines, governing = max(candidates, key=lambda item: item[0])
    return offset, lines, governing


def _case(basis: dict, data: dict, hydro: dict, heading_index: int,
           sea_name: str, removed: int | None, length_change: float = 0,
           reading: str | None = None) -> dict:
    heading = basis["analysis"]["headings"]["value"][heading_index]
    system = mooring_system(basis, removed, length_change)
    equilibrium = equilibrate(system, basis, heading)
    damaged = removed is not None
    zero_stiffness = stiffness(system, np.zeros(6)) if not damaged else equilibrium["stiffness"]
    total_stiffness = np.array(hydro["stiffness"])+zero_stiffness
    reading = reading or basis["system"]["adopted_reading"]
    mass = mass_matrix(basis, reading)
    operators = response_operators(data["omega"], data["added"], data["radiation"],
                                   data["excitation"], mass, total_stiffness,
                                   basis["analysis"]["additional_damping"]["value"])
    statistics = _spectral_response(data["omega"], operators[:, heading_index],
                                     basis["environment"][sea_name], basis["analysis"]["duration"]["value"])
    design, lines, governing = _design_tensions(system, equilibrium["offset"], statistics, basis, damaged)
    mean_forces = forces(system, equilibrium["offset"])
    assembly = aggregate(basis, reading)
    env = basis["environment"]
    reaction = constraint_reaction(equilibrium["residual"], assembly["mass"], assembly["cg"],
                                   hydro["volume"], np.array(hydro["center_of_buoyancy"]),
                                   env["water_density"]["value"], env["gravity"]["value"])
    return {"heading": heading, "sea": sea_name, "state": "damaged" if damaged else "intact",
            "removed_line": None if removed is None else removed+1,
            "mean": equilibrium["offset"].tolist(), "design": design.tolist(),
            "statistics": statistics, "lines": lines, "governing": governing,
            "constraint_reaction": reaction,
            "horizontal_residual": float(np.linalg.norm(equilibrium["residual"][:2])),
            "mean_mooring_vertical": float(mean_forces[2]),
            "length_change": length_change}


def load_cases(basis: dict, data: dict, hydro: dict, reading: str | None = None) -> list:
    """Evaluate heading/sea envelopes intact and after removing the loaded line."""
    cases = []
    for heading in range(3):
        for sea in ("operating", "extreme"):
            intact = _case(basis, data, hydro, heading, sea, None, reading=reading)
            removed = intact["governing"]["line"]-1
            damaged = _case(basis, data, hydro, heading, sea, removed, reading=reading)
            cases.extend([intact, damaged])
    return cases


def convergence(coarse: dict, fine: dict) -> list:
    """Compare common-frequency values at each finer-grid quantity peak."""
    rows = []
    for label, key, axis in (("Heave added mass", "added", 2), ("Pitch added mass", "added", 4)):
        values = fine[key][:, axis, axis]
        index = int(np.argmax(abs(values)))
        reference = values[index]
        difference = 100*abs(coarse[key][index, axis, axis]-reference)/abs(reference)
        rows.append({"quantity": label, "omega": float(fine["omega"][index]),
                     "fine": float(reference), "difference_percent": float(difference)})
    for label, axis in (("Heave excitation", 2), ("Pitch excitation", 4)):
        values = abs(fine["excitation"][:, :, axis])
        peak_index: tuple = np.unravel_index(np.argmax(values), values.shape)
        reference = values[peak_index]
        difference = 100*abs(abs(coarse["excitation"][peak_index[0], peak_index[1], axis])-reference)/reference
        rows.append({"quantity": label, "omega": float(fine["omega"][peak_index[0]]),
                     "heading": float(peak_index[1]*30), "fine": float(reference),
                     "difference_percent": float(difference)})
    return rows


def summarise(root: Path, scratch: Path) -> dict:
    """Consume retained panel outputs and calculate all response and line results."""
    basis = load_basis(root)
    provenance = verify_retained_panels(basis, scratch)
    fine = dict(np.load(scratch/"fine.npz"))
    coarse = dict(np.load(scratch/"coarse.npz"))
    hydro = json.loads((scratch/"fine-hydrostatics.json").read_text())
    if abs(hydro["volume"]/basis["published"]["displacement"]["value"]-1) > 0.05:
        raise ValueError("Computed displacement differs from the published value by more than 5%")
    if len(fine["omega"]) != len(fine["added"]) or not all(np.isfinite(a).all() for a in fine.values()):
        raise ValueError("Incomplete retained panel solution")
    system = mooring_system(basis)
    k0 = stiffness(system, np.zeros(6))
    total = np.array(hydro["stiffness"])+k0
    response, sensitivity, periods = {}, [], {}
    dense = interpolate_hydrodynamics(fine, np.unique(np.r_[fine["omega"][fine["omega"] >= 0.05],
                                                          np.linspace(0.05, 2, 10001)]))
    for reading in ("at_cg", "at_tower_top"):
        mass = mass_matrix(basis, reading)
        periods[reading] = natural_periods(fine["omega"], fine["added"], mass, total).tolist()
        for damping in (0.02, 0.05, 0.10):
            rao = response_operators(dense["omega"], dense["added"], dense["radiation"],
                                     dense["excitation"], mass, total, damping)
            sensitivity.append({"reading": reading, "damping": damping,
                                 "surge_peak": float(abs(rao[:, :, 0]).max()),
                                 "heave_peak": float(abs(rao[:, :, 2]).max()),
                                 "pitch_peak": float(abs(rao[:, :, 4]).max())})
            if damping == 0.05:
                response[reading] = rao
    adopted = response["at_tower_top"]
    scale = abs(adopted[:, 0, :]).max()
    symmetry = float(abs(adopted[:, 0, [1, 3, 5]]).max()/scale)
    if symmetry >= 1e-6:
        raise ValueError("Reflection-symmetric RAO response exceeds the stated tolerance")
    cases = load_cases(basis, dense, hydro)
    comparison = _case_inertia_sensitivity(basis, dense, hydro, cases)
    equilibrium = aggregate(basis, basis["system"]["adopted_reading"])
    selected = _plot_indices(adopted)
    return {"hydrostatics": hydro, "coarse_hydrostatics": json.loads((scratch/"coarse-hydrostatics.json").read_text()),
            "bem_provenance": provenance,
            "analytical_volume": analytical_volume(basis), "convergence": convergence(coarse, fine),
            "periods": periods, "sensitivity": sensitivity, "cases": cases,
            "omega": dense["omega"][selected].tolist(), "rao_amplitude": abs(adopted[selected]).tolist(),
            "rao_phase": np.angle(adopted[selected]).tolist(), "symmetry_ratio": symmetry,
            "response_solution_points": len(dense["omega"]),
            "inertia_case_sensitivity": comparison,
            "projection_sensitivity": _projection_sensitivity(basis, dense, total, adopted),
            "hydrostatic_validation": _hydrostatic_validation(basis, hydro),
            "system_mass": equilibrium["mass"], "system_cg": equilibrium["cg"].tolist(),
            "mooring_stiffness": k0.tolist(), "versions": {"Capytaine": "3.0.0", "MoorPy": "1.3.0"}}


def _plot_indices(operators: np.ndarray) -> np.ndarray:
    """Retain a compact plotting grid plus each local governing peak."""
    indices = list(np.linspace(0, len(operators)-1, 320, dtype=int))
    for heading in range(3):
        for axis in range(6):
            peak = int(abs(operators[:, heading, axis]).argmax())
            indices.extend(range(max(0, peak-2), min(len(operators), peak+3)))
    return np.unique(indices)


def _case_inertia_sensitivity(basis: dict, data: dict, hydro: dict, cases: list) -> dict:
    """Compare inertia interpretations with identical surviving lines and lengths."""
    alternative = []
    for case in cases:
        removed = None if case["removed_line"] is None else case["removed_line"]-1
        index = basis["analysis"]["headings"]["value"].index(case["heading"])
        final = case.get("solution", case)
        alternative.append(_case(basis, data, hydro, index, case["sea"], removed,
                                  final["length_change"], "at_cg"))
    result: dict = {}
    for label, entries in (("adopted", cases), ("alternative", alternative)):
        result[label] = {state: max(c.get("solution", c)["governing"]["utilisation"]
                                    for c in entries if c["state"] == state)
                         for state in ("intact", "damaged")}
    result["acceptance_changes"] = any((result["adopted"][s] <= 1) !=
                                        (result["alternative"][s] <= 1)
                                        for s in ("intact", "damaged"))
    result["alternative_cases"] = alternative
    return result


def _projection_sensitivity(basis: dict, data: dict, total: np.ndarray,
                             projected: np.ndarray) -> dict:
    """Quantify the response effect of the explicitly assumed reflection projection."""
    full = response_operators(data["omega"], data["added"], data["radiation"], data["excitation"],
                              mass_matrix(basis, basis["system"]["adopted_reading"], False),
                              total, basis["analysis"]["additional_damping"]["value"])
    return {f"{name}_peak_difference_percent": float(100*(abs(full[:, :, axis]).max()/
             abs(projected[:, :, axis]).max()-1)) for axis, name in ((0, "surge"), (2, "heave"), (4, "pitch"))}


def _hydrostatic_validation(basis: dict, hydro: dict) -> list:
    """Remove gravitational restoring for comparison with published geometric terms."""
    data = aggregate(basis, basis["system"]["adopted_reading"])
    gravity = basis["environment"]["gravity"]["value"]
    computed = np.array(hydro["stiffness"]).diagonal().copy()
    computed[[3, 4]] += data["mass"]*gravity*data["cg"][2]
    published = basis["published"]["hydrostatic_stiffness"]["value"]
    return [{"quantity": name, "computed": float(computed[i]), "published": published[i],
             "unit": "N/m" if i == 2 else "N m/rad"}
            for i, name in ((2, "Heave stiffness"), (3, "Roll geometric stiffness"), (4, "Pitch geometric stiffness"))]
