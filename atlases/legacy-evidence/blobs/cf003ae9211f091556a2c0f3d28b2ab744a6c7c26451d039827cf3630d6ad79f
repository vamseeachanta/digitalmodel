"""Headless FPSO spread-mooring workflow for the durable workflow registry."""
from __future__ import annotations

import math
from dataclasses import asdict
from pathlib import Path
from typing import Any
import numpy as np

from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra
from digitalmodel.marine_ops.marine_engineering.environmental_loading import (
    EnvironmentalConditions,
    EnvironmentalForces,
    OCIMFDatabase,
    VesselGeometry,
    create_sample_database,
)
from digitalmodel.marine_ops.marine_engineering.mooring_analysis import (
    CatenaryInput,
    CatenarySolver,
)
from .component_database import ChainGrade, ChainProperties, ComponentDatabase, LinkType
from .fpso_full_catenary import baseline_bounds
from .fpso_full_output import (
    json_safe,
    print_self_check,
    result_folder,
    write_outputs,
)


class FPSOMooringFullWorkflow:
    """Run the demo FPSO mooring analysis from durable workflow YAML."""

    def router(self, cfg: dict) -> dict:
        vessel = cfg["vessel"]
        environment = cfg["environment"]
        mooring = cfg["mooring"]

        spectrum = self._wave_spectrum(environment)
        forces = self._environmental_forces(cfg, vessel, environment, spectrum)
        chain = self._chain_properties(mooring)
        line_model = self._line_model(vessel, mooring, chain)
        equilibrium, line_tensions = self._static_equilibrium(forces, line_model)

        result = self._result(
            cfg, forces, spectrum, chain, equilibrium, line_tensions
        )
        cfg["fpso_mooring_full"] = result
        write_outputs(cfg, result, result_folder(cfg))
        print_self_check(cfg, result)
        return cfg

    def _wave_spectrum(self, environment: dict) -> dict:
        generator = WaveSpectra()
        frequencies, density = generator.jonswap(
            hs=float(environment["Hs"]),
            tp=float(environment["Tp"]),
            gamma=float(environment.get("gamma", 3.3)),
            freq_min=0.1,
            freq_max=2.0,
            n_points=100,
        )
        m0 = generator.spectral_moment(frequencies, density, n=0)
        m2 = generator.spectral_moment(frequencies, density, n=2)
        return {
            "frequencies": frequencies,
            "density": density,
            "m0": m0,
            "Hs_check": 4.0 * math.sqrt(m0),
            "Tz": 2.0 * math.pi * math.sqrt(m0 / m2),
            "peak_frequency": generator.peak_frequency_from_spectrum(
                frequencies, density
            ),
        }

    def _environmental_forces(
        self,
        cfg: dict,
        vessel: dict,
        environment: dict,
        spectrum: dict,
    ) -> dict:
        database = OCIMFDatabase(str(self._ocimf_database_path(cfg)))
        calculator = EnvironmentalForces(database)
        conditions = EnvironmentalConditions(
            wind_speed=float(environment["wind_speed"]),
            wind_direction=float(environment["wind_heading"]),
            current_speed=float(environment["current_speed"]),
            current_direction=float(environment["current_heading"]),
        )
        geometry = self._vessel_geometry(vessel)
        steady = calculator.calculate_total_forces(
            conditions,
            geometry,
            float(vessel["displacement"]),
        )
        wave = self._wave_drift_force(environment, spectrum)
        total_fx = steady.total_fx + wave["force_N"] * wave["unit_x"]
        total_fy = steady.total_fy + wave["force_N"] * wave["unit_y"]
        return self._force_output(steady, wave, total_fx, total_fy)

    def _ocimf_database_path(self, cfg: dict) -> Path:
        path = result_folder(cfg) / "ocimf_coefficients_sample.csv"
        if not path.exists():
            path.parent.mkdir(parents=True, exist_ok=True)
            create_sample_database(
                str(path),
                num_vessels=5,
                num_headings=13,
                num_displacements=3,
            )
        return path

    def _vessel_geometry(self, vessel: dict) -> VesselGeometry:
        loa = float(vessel["loa"])
        beam = float(vessel["beam"])
        draft = float(vessel["draft"])
        freeboard = float(vessel.get("freeboard", 10.0))
        return VesselGeometry(
            loa=loa,
            beam=beam,
            draft=draft,
            frontal_area_wind=beam * freeboard,
            lateral_area_wind=loa * freeboard,
            frontal_area_current=beam * draft,
            lateral_area_current=loa * draft,
        )

    def _wave_drift_force(self, environment: dict, spectrum: dict) -> dict:
        direction = float(environment.get("wave_heading", environment["wind_heading"]))
        coefficient = float(environment.get("wave_drift_coefficient_N_per_m2", 5e5))
        force = coefficient * float(spectrum["m0"])
        radians = math.radians(direction)
        return {
            "force_N": force,
            "heading_deg": direction,
            "coefficient_N_per_m2": coefficient,
            "unit_x": math.cos(radians),
            "unit_y": math.sin(radians),
        }

    def _force_output(
        self, steady, wave: dict, total_fx: float, total_fy: float
    ) -> dict:
        total_force = math.hypot(total_fx, total_fy)
        return {
            "wind_fx_N": steady.wind_fx,
            "wind_fy_N": steady.wind_fy,
            "current_fx_N": steady.current_fx,
            "current_fy_N": steady.current_fy,
            "wave_drift_force_N": wave["force_N"],
            "wave_heading_deg": wave["heading_deg"],
            "total_fx_N": total_fx,
            "total_fy_N": total_fy,
            "total_force_N": total_force,
            "total_heading_deg": math.degrees(math.atan2(total_fy, total_fx)),
            "coefficients": asdict(steady.coefficients),
        }

    def _chain_properties(self, mooring: dict) -> ChainProperties:
        diameter = float(mooring["chain_diameter"])
        grade = str(mooring.get("chain_grade", "R4"))
        link_type = str(mooring.get("chain_link_type", "Studless"))
        try:
            return ComponentDatabase().get_chain(diameter, grade, link_type)
        except (FileNotFoundError, ValueError):
            chain_link = (
                LinkType.STUDLESS
                if link_type.lower() == "studless"
                else LinkType.STUD_LINK
            )
            return ChainProperties.from_excel_formula(
                diameter,
                ChainGrade[grade.upper()],
                chain_link,
            )

    def _line_model(self, vessel: dict, mooring: dict, chain: ChainProperties) -> dict:
        water_depth = float(mooring["water_depth"])
        line_length = float(mooring["line_length"])
        pretension = float(mooring["pretension"])
        fairlead_radius = float(mooring.get("fairlead_radius", vessel["loa"] / 2.0))
        return {
            "n_lines": int(mooring["n_lines"]),
            "line_length": line_length,
            "water_depth": water_depth,
            "pretension": pretension,
            "fairlead_radius": fairlead_radius,
            "baseline_span": self._baseline_span(
                chain, water_depth, line_length, pretension
            ),
            "weight_per_length": chain.weight_water * 9.81,
            "ea_stiffness": float(chain.stiffness) * 1000.0,
            "mbl_N": float(chain.mbl) * 1000.0,
        }

    def _baseline_span(
        self,
        chain: ChainProperties,
        water_depth: float,
        line_length: float,
        pretension: float,
    ) -> float:
        solver = CatenarySolver()
        weight = chain.weight_water * 9.81
        max_span = math.sqrt(max(line_length**2 - water_depth**2, 1.0)) * 0.98
        low, high = baseline_bounds(
            solver,
            self._solve_line,
            chain,
            water_depth,
            line_length,
            pretension,
            max_span,
        )
        for _ in range(50):
            mid = 0.5 * (low + high)
            tension = self._solve_line(
                solver, line_length, mid, water_depth, weight, chain
            )
            if tension["horizontal_tension_N"] < pretension:
                low = mid
            else:
                high = mid
        return 0.5 * (low + high)

    def _static_equilibrium(
        self, forces: dict, line_model: dict
    ) -> tuple[dict, list[dict]]:
        force_mag = float(forces["total_force_N"])
        direction = math.radians(float(forces["total_heading_deg"]))
        unit = np.array([math.cos(direction), math.sin(direction)])
        high = self._equilibrium_bracket(force_mag, unit, line_model)
        low = 0.0
        for _ in range(50):
            mid = 0.5 * (low + high)
            balance = self._restoring_projection(mid, unit, line_model) + force_mag
            if balance > 0.0:
                low = mid
            else:
                high = mid
        offset = 0.5 * (low + high)
        lines = self._line_tensions(offset * unit, line_model)
        restoring = self._restoring_force(offset * unit, line_model, lines)
        equilibrium = {
            "offset_m": offset,
            "offset_x_m": offset * float(unit[0]),
            "offset_y_m": offset * float(unit[1]),
            "restoring_fx_N": float(restoring[0]),
            "restoring_fy_N": float(restoring[1]),
        }
        return equilibrium, lines

    def _equilibrium_bracket(
        self,
        force_mag: float,
        unit: np.ndarray,
        line_model: dict,
    ) -> float:
        high = 1.0
        for _ in range(12):
            balance = self._restoring_projection(high, unit, line_model) + force_mag
            if balance <= 0.0:
                return high
            high *= 2.0
        return high

    def _restoring_projection(
        self,
        offset: float,
        unit: np.ndarray,
        line_model: dict,
    ) -> float:
        try:
            lines = self._line_tensions(offset * unit, line_model)
        except ValueError:
            return float("-inf")
        restoring = self._restoring_force(offset * unit, line_model, lines)
        return float(np.dot(restoring, unit))

    def _line_tensions(self, offset: np.ndarray, line_model: dict) -> list[dict]:
        solver = CatenarySolver()
        tensions = []
        for index in range(line_model["n_lines"]):
            angle_deg = index * 360.0 / line_model["n_lines"]
            state = self._line_state(solver, index + 1, angle_deg, offset, line_model)
            tensions.append(state)
        return tensions

    def _line_state(
        self,
        solver: CatenarySolver,
        line_number: int,
        angle_deg: float,
        offset: np.ndarray,
        line_model: dict,
    ) -> dict:
        angle = math.radians(angle_deg)
        radial = np.array([math.cos(angle), math.sin(angle)])
        fairlead = offset + line_model["fairlead_radius"] * radial
        anchor_radius = line_model["fairlead_radius"] + line_model["baseline_span"]
        anchor = anchor_radius * radial
        span = float(np.linalg.norm(anchor - fairlead))
        result = self._solve_line(
            solver,
            line_model["line_length"],
            span,
            line_model["water_depth"],
            line_model["weight_per_length"],
            line_model,
        )
        return {
            "line_id": f"Line_{line_number}",
            "heading_deg": angle_deg,
            "horizontal_span_m": span,
            "top_tension_N": result["top_tension_N"],
            "horizontal_tension_N": result["horizontal_tension_N"],
            "anchor_tension_N": result["anchor_tension_N"],
            "safety_factor": line_model["mbl_N"] / result["top_tension_N"],
        }

    def _solve_line(
        self,
        solver: CatenarySolver,
        line_length: float,
        span: float,
        water_depth: float,
        weight: float,
        source: Any,
    ) -> dict:
        stiffness = source["ea_stiffness"] if isinstance(source, dict) else (
            source.stiffness * 1000.0
        )
        solved = solver.solve(
            CatenaryInput(
                length=line_length,
                horizontal_span=span,
                vertical_span=water_depth,
                weight_per_length=weight,
                ea_stiffness=float(stiffness),
                water_depth=water_depth,
            )
        )
        return {
            "top_tension_N": solved.total_tension_fairlead,
            "horizontal_tension_N": solved.horizontal_tension,
            "anchor_tension_N": solved.total_tension_anchor,
        }

    def _restoring_force(
        self,
        offset: np.ndarray,
        line_model: dict,
        lines: list[dict],
    ) -> np.ndarray:
        restoring = np.array([0.0, 0.0])
        for line in lines:
            angle = math.radians(line["heading_deg"])
            radial = np.array([math.cos(angle), math.sin(angle)])
            fairlead = offset + line_model["fairlead_radius"] * radial
            anchor_radius = line_model["fairlead_radius"] + line_model["baseline_span"]
            direction = anchor_radius * radial - fairlead
            restoring += (
                line["horizontal_tension_N"] * direction / np.linalg.norm(direction)
            )
        return restoring

    def _result(
        self,
        cfg: dict,
        forces: dict,
        spectrum: dict,
        chain: ChainProperties,
        equilibrium: dict,
        line_tensions: list[dict],
    ) -> dict:
        max_tension = max(line["top_tension_N"] for line in line_tensions)
        max_line = max(line_tensions, key=lambda item: item["top_tension_N"])
        return json_safe({
            "inputs": {key: dict(cfg[key]) for key in (
                "vessel", "environment", "mooring"
            )},
            "wave_spectrum": {key: spectrum[key] for key in (
                "m0", "Hs_check", "Tz", "peak_frequency"
            )},
            "environmental_forces": forces,
            "static_equilibrium": equilibrium,
            "line_tensions": line_tensions,
            "chain": asdict(chain),
            "summary": {
                "n_lines": len(line_tensions),
                "pretension_N": float(cfg["mooring"]["pretension"]),
                "max_line_tension_N": max_tension,
                "max_line_tension_MN": max_tension / 1.0e6,
                "max_tension_line": max_line["line_id"],
                "min_safety_factor": min(
                    line["safety_factor"] for line in line_tensions),
            },
        })
