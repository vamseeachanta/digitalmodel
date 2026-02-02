"""Reverse parsers: read AQWA .dat and OrcaWave .yml into DiffractionSpec (WRK-063).

These parsers read solver-specific input files and produce canonical
DiffractionSpec objects. Combined with the forward backends (WRK-058, WRK-059),
this enables full cross-conversion:

    AQWA .dat  -> AQWAInputParser  -> DiffractionSpec -> to_yaml()  -> spec.yml
    OrcaWave .yml -> OrcaWaveInputParser -> DiffractionSpec -> to_yaml() -> spec.yml

And therefore:
    AQWA .dat -> spec.yml -> OrcaWave .yml
    OrcaWave .yml -> spec.yml -> AQWA .dat
"""

from __future__ import annotations

import math
import re
from pathlib import Path
from typing import Any, Optional

import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    AnalysisType,
    DiffractionSpec,
    EnvironmentSpec,
    FrequencyInputType,
    FrequencySpec,
    MetadataSpec,
    OutputSpec,
    SolverOptions,
    VesselGeometry,
    VesselInertia,
    VesselSpec,
    WaveHeadingSpec,
)


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# AQWA uses a large depth to represent infinite / deep water
_AQWA_DEEP_WATER_THRESHOLD = 5000.0

# Regex for matching floats in AQWA .dat lines
_FLOAT_PATTERN = re.compile(r"[-+]?\d*\.?\d+(?:[Ee][-+]?\d+)?")

# AQWA deck header pattern
_DECK_PATTERN = re.compile(r"DECK\s+(\d+)", re.IGNORECASE)


# ---------------------------------------------------------------------------
# AQWAInputParser
# ---------------------------------------------------------------------------


class AQWAInputParser:
    """Parse AQWA .dat input file into DiffractionSpec.

    This is the reverse of AQWABackend: it reads AQWA deck/card format
    and extracts the information needed to populate a canonical spec.

    Supports single-body models. Multi-body parsing is deferred.

    Example::

        parser = AQWAInputParser()
        spec = parser.parse(Path("analysis.dat"))
        spec.to_yaml("spec.yml")
    """

    def parse(self, dat_path: Path) -> DiffractionSpec:
        """Read AQWA .dat and extract sections into canonical spec.

        Parameters
        ----------
        dat_path : Path
            Path to the AQWA .dat input file.

        Returns
        -------
        DiffractionSpec
            Canonical specification parsed from the .dat file.
        """
        dat_path = Path(dat_path)
        decks = self._read_decks(dat_path)

        metadata = self._parse_deck0(decks.get(0, []))
        environment = self._parse_environment(decks.get(5, []))
        frequencies = self._parse_frequencies(decks.get(6, []))
        headings = self._parse_headings(decks.get(6, []))
        mass = self._parse_mass(decks.get(3, []))
        inertia_tensor = self._parse_inertia(decks.get(4, []))
        vessel_name = self._parse_vessel_name(decks.get(0, []), metadata)
        solver_options = self._parse_solver_options(decks)

        # Build the vessel spec
        vessel_inertia = VesselInertia(
            mass=mass,
            centre_of_gravity=[0.0, 0.0, 0.0],
            inertia_tensor=inertia_tensor,
        )

        vessel_geometry = VesselGeometry(
            mesh_file="mesh.gdf",
            mesh_format="auto",
            symmetry="none",
        )

        vessel = VesselSpec(
            name=vessel_name,
            geometry=vessel_geometry,
            inertia=vessel_inertia,
        )

        return DiffractionSpec(
            version="1.0",
            analysis_type=AnalysisType.DIFFRACTION,
            vessel=vessel,
            environment=environment,
            frequencies=frequencies,
            wave_headings=headings,
            solver_options=solver_options,
            outputs=OutputSpec(),
            metadata=metadata,
        )

    # -----------------------------------------------------------------
    # Deck reading
    # -----------------------------------------------------------------

    def _read_decks(self, dat_path: Path) -> dict[int, list[str]]:
        """Split a .dat file into deck-keyed line lists."""
        decks: dict[int, list[str]] = {}
        current_deck: Optional[int] = None

        with open(dat_path, "r", encoding="utf-8", errors="ignore") as f:
            for raw_line in f:
                line = raw_line.rstrip("\n")
                deck_match = _DECK_PATTERN.search(line)
                if deck_match:
                    current_deck = int(deck_match.group(1))
                    decks.setdefault(current_deck, [])
                    continue
                if current_deck is not None:
                    decks[current_deck].append(line)

        return decks

    # -----------------------------------------------------------------
    # Deck 0 — JOB / metadata
    # -----------------------------------------------------------------

    def _parse_deck0(self, lines: list[str]) -> MetadataSpec:
        """Extract metadata from deck 0 (JOB, TITLE)."""
        project = None
        for line in lines:
            upper = line.upper().strip()
            if upper.startswith("TITLE"):
                # Title text comes after the TITLE keyword
                title_text = line[len("TITLE"):].strip()
                if title_text:
                    project = title_text
                break

        return MetadataSpec(project=project)

    def _parse_vessel_name(
        self, deck0_lines: list[str], metadata: MetadataSpec
    ) -> str:
        """Derive a vessel name from available metadata."""
        if metadata.project:
            return metadata.project
        return "AQWA_Vessel"

    def _parse_solver_options(
        self, decks: dict[int, list[str]]
    ) -> SolverOptions:
        """Extract solver options from OPTIONS cards."""
        remove_irreg = False
        qtf = False

        for lines in decks.values():
            for line in lines:
                upper = line.upper()
                if "OPTIONS" in upper:
                    if "LHFR" in upper:
                        remove_irreg = True
                    if "MQTF" in upper:
                        qtf = True

        return SolverOptions(
            remove_irregular_frequencies=remove_irreg,
            qtf_calculation=qtf,
        )

    # -----------------------------------------------------------------
    # Deck 5 — Environment (GLOB)
    # -----------------------------------------------------------------

    def _parse_environment(self, lines: list[str]) -> EnvironmentSpec:
        """Extract water depth, density, and gravity from deck 5 GLOB cards."""
        water_depth: float | str = 100.0
        water_density: float = 1025.0
        gravity: float = 9.80665

        for line in lines:
            upper = line.upper().strip()
            if "DPTH" in upper:
                floats = _FLOAT_PATTERN.findall(line)
                if floats:
                    depth_val = float(floats[-1])
                    if depth_val >= _AQWA_DEEP_WATER_THRESHOLD:
                        water_depth = "infinite"
                    else:
                        water_depth = depth_val
            elif "DENS" in upper:
                floats = _FLOAT_PATTERN.findall(line)
                if floats:
                    water_density = float(floats[-1])
            elif "ACCG" in upper:
                floats = _FLOAT_PATTERN.findall(line)
                if floats:
                    gravity = float(floats[-1])

        return EnvironmentSpec(
            water_depth=water_depth,
            water_density=water_density,
            gravity=gravity,
        )

    # -----------------------------------------------------------------
    # Deck 6 — Frequencies (HRTZ)
    # -----------------------------------------------------------------

    def _parse_frequencies(self, lines: list[str]) -> FrequencySpec:
        """Extract frequencies from HRTZ cards in deck 6.

        AQWA stores frequencies in Hz. The canonical spec uses rad/s.
        Conversion: omega = 2 * pi * f
        """
        freq_hz_list: list[float] = []

        for line in lines:
            upper = line.upper()
            if "HRTZ" in upper:
                floats = _FLOAT_PATTERN.findall(line)
                if floats:
                    freq_hz = float(floats[-1])
                    if freq_hz > 0:
                        freq_hz_list.append(freq_hz)

        # Convert Hz to rad/s
        freq_rad_s = [f * 2.0 * math.pi for f in freq_hz_list]

        if not freq_rad_s:
            freq_rad_s = [0.1]  # fallback

        return FrequencySpec(
            input_type=FrequencyInputType.FREQUENCY,
            values=freq_rad_s,
        )

    # -----------------------------------------------------------------
    # Deck 6 — Headings (DIRN)
    # -----------------------------------------------------------------

    def _parse_headings(self, lines: list[str]) -> WaveHeadingSpec:
        """Extract wave headings from DIRN cards in deck 6.

        AQWA stores headings in degrees. The canonical spec also uses degrees.
        """
        heading_list: list[float] = []

        for line in lines:
            upper = line.upper()
            if "DIRN" in upper:
                floats = _FLOAT_PATTERN.findall(line)
                if floats:
                    heading_deg = float(floats[-1])
                    heading_list.append(heading_deg)

        if not heading_list:
            heading_list = [0.0]  # fallback

        return WaveHeadingSpec(values=heading_list)

    # -----------------------------------------------------------------
    # Deck 3 — Mass (MATE)
    # -----------------------------------------------------------------

    def _parse_mass(self, lines: list[str]) -> float:
        """Extract vessel mass from deck 3 MATE cards.

        The mass line format is:
            <struct_id>         <ref_node>  <mass_value>
        """
        total_mass = 0.0

        for line in lines:
            stripped = line.strip()
            if not stripped or stripped.startswith("*"):
                continue
            upper = stripped.upper()
            if "MATE" in upper or "END" in upper or "DECK" in upper:
                continue
            # Parse float values from mass lines
            floats = _FLOAT_PATTERN.findall(line)
            if len(floats) >= 1:
                try:
                    mass_val = float(floats[-1])
                    if mass_val > 0:
                        total_mass += mass_val
                except ValueError:
                    continue

        return total_mass if total_mass > 0 else 1.0

    # -----------------------------------------------------------------
    # Deck 4 — Inertia (GEOM / PMAS)
    # -----------------------------------------------------------------

    def _parse_inertia(self, lines: list[str]) -> dict[str, float]:
        """Extract inertia tensor from deck 4 GEOM/PMAS cards.

        The PMAS line format:
            <struct_id>PMAS     <ref_node> <Ixx> <Ixy> <Ixz> <Iyy> <Iyz> <Izz>
        """
        ixx = 0.0
        ixy = 0.0
        ixz = 0.0
        iyy = 0.0
        iyz = 0.0
        izz = 0.0

        for line in lines:
            upper = line.upper()
            if "PMAS" in upper:
                floats = _FLOAT_PATTERN.findall(line)
                # The PMAS line has: struct_id, ref_node, Ixx, Ixy, Ixz, Iyy, Iyz, Izz
                # We need the last 6 floats
                if len(floats) >= 6:
                    vals = [float(v) for v in floats[-6:]]
                    ixx += vals[0]
                    ixy += vals[1]
                    ixz += vals[2]
                    iyy += vals[3]
                    iyz += vals[4]
                    izz += vals[5]

        return {
            "Ixx": ixx,
            "Iyy": iyy,
            "Izz": izz,
            "Ixy": ixy,
            "Ixz": ixz,
            "Iyz": iyz,
        }


# ---------------------------------------------------------------------------
# OrcaWaveInputParser
# ---------------------------------------------------------------------------


class OrcaWaveInputParser:
    """Parse OrcaWave .yml project file into DiffractionSpec.

    This is the reverse of OrcaWaveBackend: it reads OrcaWave YAML
    format and maps the fields to a canonical DiffractionSpec.

    Supports single-body and multi-body models.

    Example::

        parser = OrcaWaveInputParser()
        spec = parser.parse(Path("project.yml"))
        spec.to_yaml("spec.yml")
    """

    def parse(self, yml_path: Path) -> DiffractionSpec:
        """Read OrcaWave YAML and extract into canonical spec.

        Parameters
        ----------
        yml_path : Path
            Path to the OrcaWave .yml project file.

        Returns
        -------
        DiffractionSpec
            Canonical specification parsed from the .yml file.
        """
        yml_path = Path(yml_path)

        with open(yml_path, "r", encoding="utf-8") as f:
            data = yaml.safe_load(f)

        environment = self._parse_environment(data)
        frequencies = self._parse_frequencies(data)
        headings = self._parse_headings(data)
        solver_options = self._parse_solver_options(data)

        bodies = data.get("Bodies", [])

        if len(bodies) == 1:
            vessel = self._parse_single_body(bodies[0])
            return DiffractionSpec(
                version="1.0",
                analysis_type=AnalysisType.DIFFRACTION,
                vessel=vessel,
                environment=environment,
                frequencies=frequencies,
                wave_headings=headings,
                solver_options=solver_options,
                outputs=OutputSpec(),
                metadata=MetadataSpec(),
            )
        elif len(bodies) > 1:
            from digitalmodel.hydrodynamics.diffraction.input_schemas import BodySpec

            body_specs = [self._parse_body_spec(b) for b in bodies]
            return DiffractionSpec(
                version="1.0",
                analysis_type=AnalysisType.DIFFRACTION,
                bodies=body_specs,
                environment=environment,
                frequencies=frequencies,
                wave_headings=headings,
                solver_options=solver_options,
                outputs=OutputSpec(),
                metadata=MetadataSpec(),
            )
        else:
            # No bodies found; create a placeholder
            vessel = VesselSpec(
                name="Unknown",
                geometry=VesselGeometry(mesh_file="mesh.gdf"),
                inertia=VesselInertia(
                    mass=1.0,
                    centre_of_gravity=[0.0, 0.0, 0.0],
                    radii_of_gyration=[1.0, 1.0, 1.0],
                ),
            )
            return DiffractionSpec(
                version="1.0",
                analysis_type=AnalysisType.DIFFRACTION,
                vessel=vessel,
                environment=environment,
                frequencies=frequencies,
                wave_headings=headings,
                solver_options=solver_options,
                outputs=OutputSpec(),
                metadata=MetadataSpec(),
            )

    # -----------------------------------------------------------------
    # Environment
    # -----------------------------------------------------------------

    def _parse_environment(self, data: dict[str, Any]) -> EnvironmentSpec:
        """Extract environment from OrcaWave YAML data."""
        water_depth_raw = data.get("WaterDepth", 100.0)

        if isinstance(water_depth_raw, str) and water_depth_raw.lower() in (
            "infinity",
            "inf",
        ):
            water_depth: float | str = "infinite"
        else:
            water_depth = float(water_depth_raw)

        # OrcaWave SI: density in t/m^3 -> spec uses kg/m^3
        water_density_raw = data.get("WaterDensity", 1.025)
        water_density = float(water_density_raw) * 1000.0

        return EnvironmentSpec(
            water_depth=water_depth,
            water_density=water_density,
        )

    # -----------------------------------------------------------------
    # Frequencies
    # -----------------------------------------------------------------

    def _parse_frequencies(self, data: dict[str, Any]) -> FrequencySpec:
        """Extract frequencies from OrcaWave YAML data.

        OrcaWave typically stores periods in seconds. The canonical spec
        can store either. We convert periods to rad/s frequencies for
        consistency.
        """
        referred_by = data.get("WavesReferredToBy", "period (s)")
        values = data.get("PeriodOrFrequency", [])

        if not values:
            return FrequencySpec(
                input_type=FrequencyInputType.FREQUENCY,
                values=[0.1],
            )

        if "period" in referred_by.lower():
            # Convert periods (s) to frequencies (rad/s)
            freq_rad_s = [
                2.0 * math.pi / float(t) for t in values if float(t) > 0
            ]
            return FrequencySpec(
                input_type=FrequencyInputType.FREQUENCY,
                values=freq_rad_s,
            )
        else:
            # Already frequencies
            return FrequencySpec(
                input_type=FrequencyInputType.FREQUENCY,
                values=[float(v) for v in values],
            )

    # -----------------------------------------------------------------
    # Headings
    # -----------------------------------------------------------------

    def _parse_headings(self, data: dict[str, Any]) -> WaveHeadingSpec:
        """Extract wave headings from OrcaWave YAML data."""
        headings = data.get("WaveHeading", [0.0])
        return WaveHeadingSpec(
            values=[float(h) for h in headings],
        )

    # -----------------------------------------------------------------
    # Solver options
    # -----------------------------------------------------------------

    def _parse_solver_options(self, data: dict[str, Any]) -> SolverOptions:
        """Extract solver options from OrcaWave YAML data."""
        qtf_pressure = data.get("QuadraticLoadPressureIntegration", False)
        # YAML 1.1 may parse Yes/No as True/False
        qtf_enabled = qtf_pressure is True or (
            isinstance(qtf_pressure, str) and qtf_pressure.lower() == "yes"
        )

        return SolverOptions(
            remove_irregular_frequencies=True,
            qtf_calculation=qtf_enabled,
        )

    # -----------------------------------------------------------------
    # Body parsing
    # -----------------------------------------------------------------

    def _parse_single_body(self, body_data: dict[str, Any]) -> VesselSpec:
        """Parse a single OrcaWave body dict into a VesselSpec."""
        name = body_data.get("BodyName", "Unknown")
        mesh_file = body_data.get("BodyMeshFileName", "mesh.gdf")
        mesh_format = self._reverse_mesh_format(
            body_data.get("BodyMeshFormat", "Auto")
        )
        mesh_symmetry = self._reverse_symmetry(
            body_data.get("BodyMeshSymmetry", "None")
        )
        length_units = body_data.get("BodyMeshLengthUnits", "m")

        geometry = VesselGeometry(
            mesh_file=mesh_file,
            mesh_format=mesh_format,
            symmetry=mesh_symmetry,
            length_units=length_units,
        )

        # Mass: OrcaWave SI uses tonnes -> spec uses kg
        mass_tonnes = float(body_data.get("BodyMass", 0.0))
        mass_kg = mass_tonnes * 1000.0

        cog = body_data.get("BodyCentreOfMass", [0.0, 0.0, 0.0])

        # Inertia tensor
        inertia_tensor = self._parse_body_inertia(body_data, mass_tonnes)

        inertia = VesselInertia(
            mass=mass_kg,
            centre_of_gravity=[float(c) for c in cog],
            inertia_tensor=inertia_tensor,
        )

        # External stiffness
        external_stiffness = self._parse_6x6_matrix(body_data, "Stiffness")
        external_damping = self._parse_6x6_matrix(body_data, "Damping")

        # Fixed DOFs
        fixed_dofs = self._parse_fixed_dofs(body_data)

        return VesselSpec(
            name=name,
            geometry=geometry,
            inertia=inertia,
            external_stiffness=external_stiffness,
            external_damping=external_damping,
            fixed_dofs=fixed_dofs if fixed_dofs else None,
        )

    def _parse_body_spec(self, body_data: dict[str, Any]) -> Any:
        """Parse a body into a BodySpec (for multi-body)."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import BodySpec

        vessel = self._parse_single_body(body_data)
        position = body_data.get("BodyMeshPosition", [0.0, 0.0, 0.0])
        attitude = body_data.get("BodyMeshAttitude", [0.0, 0.0, 0.0])
        connection = body_data.get("BodyConnectionParent", "Free")

        return BodySpec(
            vessel=vessel,
            position=[float(p) for p in position],
            attitude=[float(a) for a in attitude],
            connection_parent=connection if connection != "Free" else None,
        )

    def _parse_body_inertia(
        self, body_data: dict[str, Any], mass_tonnes: float
    ) -> dict[str, float]:
        """Extract inertia tensor from body data.

        OrcaWave stores inertia in t.m^2 for SI. The spec uses kg.m^2.
        Conversion: multiply by 1000.
        """
        inertia_key = (
            "BodyInertiaTensorRx, "
            "BodyInertiaTensorRy, "
            "BodyInertiaTensorRz"
        )

        tensor_data = body_data.get(inertia_key)

        if tensor_data is not None and isinstance(tensor_data, list):
            # 3x3 matrix
            if len(tensor_data) >= 3:
                row0 = tensor_data[0]
                row1 = tensor_data[1]
                row2 = tensor_data[2]

                # Convert t.m^2 to kg.m^2
                ixx = float(row0[0]) * 1000.0 if len(row0) > 0 else 0.0
                ixy = float(row0[1]) * 1000.0 if len(row0) > 1 else 0.0
                ixz = float(row0[2]) * 1000.0 if len(row0) > 2 else 0.0
                iyy = float(row1[1]) * 1000.0 if len(row1) > 1 else 0.0
                iyz = float(row1[2]) * 1000.0 if len(row1) > 2 else 0.0
                izz = float(row2[2]) * 1000.0 if len(row2) > 2 else 0.0

                return {
                    "Ixx": ixx,
                    "Iyy": iyy,
                    "Izz": izz,
                    "Ixy": ixy,
                    "Ixz": ixz,
                    "Iyz": iyz,
                }

        # Fallback: use mass with unit radii of gyration
        mass_kg = mass_tonnes * 1000.0
        return {
            "Ixx": mass_kg,
            "Iyy": mass_kg,
            "Izz": mass_kg,
            "Ixy": 0.0,
            "Ixz": 0.0,
            "Iyz": 0.0,
        }

    def _parse_6x6_matrix(
        self, body_data: dict[str, Any], matrix_type: str
    ) -> Optional[list[list[float]]]:
        """Extract a 6x6 matrix (stiffness or damping) from body data."""
        # Search for the composite key
        for key in body_data:
            if matrix_type in key and isinstance(body_data[key], list):
                matrix = body_data[key]
                if len(matrix) == 6 and all(
                    isinstance(row, list) and len(row) == 6
                    for row in matrix
                ):
                    # Check if it's all zeros
                    all_zero = all(
                        all(v == 0 for v in row) for row in matrix
                    )
                    if all_zero:
                        return None
                    return [[float(v) for v in row] for row in matrix]
        return None

    def _parse_fixed_dofs(
        self, body_data: dict[str, Any]
    ) -> list[str]:
        """Extract fixed DOFs from body data."""
        dof_map = {
            "BodyFixedDOFx": "surge",
            "BodyFixedDOFy": "sway",
            "BodyFixedDOFz": "heave",
            "BodyFixedDOFRx": "roll",
            "BodyFixedDOFRy": "pitch",
            "BodyFixedDOFRz": "yaw",
        }
        fixed = []
        for key, dof_name in dof_map.items():
            val = body_data.get(key, False)
            # YAML 1.1 maps Yes/No to True/False
            if val is True or (isinstance(val, str) and val.lower() == "yes"):
                fixed.append(dof_name)
        return fixed

    # -----------------------------------------------------------------
    # Reverse mapping helpers
    # -----------------------------------------------------------------

    @staticmethod
    def _reverse_mesh_format(orcawave_fmt: str) -> str:
        """Map OrcaWave mesh format string back to spec enum value."""
        fmt_map = {
            "wamit gdf": "gdf",
            "wamit dat": "dat",
            "stl": "stl",
            "gmsh": "msh",
            "obj": "obj",
            "auto": "auto",
        }
        return fmt_map.get(orcawave_fmt.lower(), "auto")

    @staticmethod
    def _reverse_symmetry(orcawave_sym: str) -> str:
        """Map OrcaWave symmetry string back to spec enum value."""
        sym_map = {
            "none": "none",
            "xz plane": "xz",
            "yz plane": "yz",
            "xz and yz planes": "xz+yz",
        }
        return sym_map.get(orcawave_sym.lower(), "none")


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "AQWAInputParser",
    "OrcaWaveInputParser",
]
