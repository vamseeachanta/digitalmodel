#!/usr/bin/env python3
"""
WAMIT Reference Data Loader

Loads WAMIT reference data from .owd files (via OrcFxAPI) or manually-curated
YAML files and converts to DiffractionResults for benchmark comparison.

WAMIT non-dimensional conventions:
- Added mass: A_ij / (rho * L^k)  where k depends on DOF pairing
- Damping: B_ij / (rho * omega * L^k)
- L = characteristic length (typically displaced volume^(1/3) or beam/2)
- k=3 for linear-linear, k=4 for linear-rotational, k=5 for rotational-rotational

References:
- WAMIT User Manual v7, Chapter 4 (Output Files)
- Lee & Newman (2006) — non-dimensionalization conventions
"""

from __future__ import annotations

import logging
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, List, Optional

import numpy as np
import yaml

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    AddedMassSet,
    DampingSet,
    DiffractionResults,
    DOF,
    FrequencyData,
    HeadingData,
    HydrodynamicMatrix,
    RAOComponent,
    RAOSet,
)

logger = logging.getLogger(__name__)

# DOF names expected in YAML files, mapped to DOF enum
_DOF_NAMES = {
    "surge": DOF.SURGE,
    "sway": DOF.SWAY,
    "heave": DOF.HEAVE,
    "roll": DOF.ROLL,
    "pitch": DOF.PITCH,
    "yaw": DOF.YAW,
}

# Exponent k for WAMIT non-dimensionalization: A_ij / (rho * L^k)
# Row DOF i, Column DOF j. Linear DOFs (1-3) contribute 1, rotational (4-6) contribute 2.
# k = sum of contributions.
_NONDIM_EXPONENT = np.array(
    [
        [3, 3, 3, 4, 4, 4],
        [3, 3, 3, 4, 4, 4],
        [3, 3, 3, 4, 4, 4],
        [4, 4, 4, 5, 5, 5],
        [4, 4, 4, 5, 5, 5],
        [4, 4, 4, 5, 5, 5],
    ],
    dtype=int,
)


class WamitReferenceLoader:
    """Load WAMIT reference data for validation benchmarks.

    Supports two data sources:
    1. .owd files — OrcaWave diffraction files that may contain embedded
       WAMIT comparison data (requires OrcFxAPI).
    2. YAML files — manually curated reference data digitized from WAMIT
       publications or validation reports.

    Both paths produce DiffractionResults objects compatible with the
    benchmark_runner.py comparison framework.
    """

    @classmethod
    def from_owd(
        cls,
        owd_path: Path,
        vessel_name: str,
        water_depth: float,
    ) -> DiffractionResults:
        """Extract WAMIT comparison data from an .owd file via OrcFxAPI.

        This is the primary path when Phase 0 probe confirms that the .owd
        file contains external/comparison data sets.

        Args:
            owd_path: Path to the .owd OrcaWave diffraction file.
            vessel_name: Vessel identifier for the results.
            water_depth: Water depth in metres (use float('inf') for deep water).

        Returns:
            DiffractionResults populated with WAMIT data extracted from .owd.

        Raises:
            ImportError: If OrcFxAPI is not available.
            FileNotFoundError: If owd_path does not exist.
            ValueError: If no WAMIT comparison data found in the file.
        """
        owd_path = Path(owd_path)
        if not owd_path.exists():
            raise FileNotFoundError(f"OWD file not found: {owd_path}")

        try:
            import OrcFxAPI  # noqa: F811
        except ImportError as exc:
            raise ImportError(
                "OrcFxAPI is required to load .owd files. "
                "Install via: pip install OrcFxAPI"
            ) from exc

        # Load the OrcaWave model
        model = OrcFxAPI.DiffractionModel(str(owd_path))

        # Attempt to find comparison/external data sets
        # OrcaWave .owd files can contain "ExternalResults" or similar
        # objects that hold digitized comparison data from other solvers
        frequencies_rad_s = []
        headings_deg = []
        rao_data: Dict[str, Dict[str, Any]] = {}
        added_mass_data: List[Dict[str, Any]] = []
        damping_data: List[Dict[str, Any]] = []

        # Try to extract external RAO data from the model
        try:
            ext_results = model.externalResultsObjects
            if not ext_results:
                raise ValueError(
                    f"No external/comparison results found in {owd_path}. "
                    "Use from_yaml() with manually digitized data instead."
                )

            # Extract frequency and heading arrays from first external result
            ext = ext_results[0]
            frequencies_rad_s = list(ext.frequencies)
            headings_deg = list(ext.headings)

            # Extract RAO data for each DOF
            for dof_name, dof_enum in _DOF_NAMES.items():
                dof_idx = dof_enum.value  # 1-based
                magnitudes = []
                phases = []
                for f_idx in range(len(frequencies_rad_s)):
                    mag_row = []
                    phase_row = []
                    for h_idx in range(len(headings_deg)):
                        mag_row.append(
                            ext.RAOAmplitude(dof_idx, f_idx, h_idx)
                        )
                        phase_row.append(
                            ext.RAOPhase(dof_idx, f_idx, h_idx)
                        )
                    magnitudes.append(mag_row)
                    phases.append(phase_row)

                rao_data[dof_name] = {
                    "magnitude": magnitudes,
                    "phase": phases,
                }

        except AttributeError:
            raise ValueError(
                f"OrcaWave model at {owd_path} does not support "
                "external results extraction. Use from_yaml() instead."
            )

        return cls._build_results(
            vessel_name=vessel_name,
            water_depth=water_depth,
            frequencies_rad_s=frequencies_rad_s,
            headings_deg=headings_deg,
            rao_data=rao_data,
            added_mass_data=added_mass_data if added_mass_data else None,
            damping_data=damping_data if damping_data else None,
            source=str(owd_path),
        )

    @classmethod
    def from_yaml(cls, path: Path) -> DiffractionResults:
        """Load WAMIT reference data from a manually-curated YAML file.

        Expected YAML format::

            vessel_name: Pyramid_ZC08
            solver: WAMIT v7.3
            water_depth: infinite  # or numeric value in metres
            frequencies_rad_s: [0.2, 0.4, 0.6, ...]
            headings_deg: [0.0]
            raos:
              heave:
                magnitude: [[1.0, ...]]  # [nfreq x nheading]
                phase: [[0.0, ...]]       # degrees
              surge: ...
            added_mass:   # optional
              - frequency: 0.2
                matrix: [[...6x6...]]
            damping:      # optional
              - frequency: 0.2
                matrix: [[...6x6...]]

        Args:
            path: Path to YAML file.

        Returns:
            DiffractionResults populated from YAML data.

        Raises:
            FileNotFoundError: If path does not exist.
            ValueError: If required fields are missing or malformed.
        """
        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(f"YAML file not found: {path}")

        with open(path, "r", encoding="utf-8") as f:
            data = yaml.safe_load(f)

        if not isinstance(data, dict):
            raise ValueError(f"Expected YAML mapping at top level, got {type(data).__name__}")

        # Required fields
        vessel_name = data.get("vessel_name")
        if not vessel_name:
            raise ValueError("Missing required field: vessel_name")

        frequencies_raw = data.get("frequencies_rad_s")
        if not frequencies_raw:
            raise ValueError("Missing required field: frequencies_rad_s")

        headings_raw = data.get("headings_deg")
        if not headings_raw:
            raise ValueError("Missing required field: headings_deg")

        rao_data_raw = data.get("raos")
        if not rao_data_raw:
            raise ValueError("Missing required field: raos")

        # Parse water depth
        water_depth_raw = data.get("water_depth", "infinite")
        water_depth = _parse_water_depth(water_depth_raw)

        # Validate RAO data structure
        frequencies_rad_s = [float(f) for f in frequencies_raw]
        headings_deg = [float(h) for h in headings_raw]
        nfreq = len(frequencies_rad_s)
        nhead = len(headings_deg)

        rao_data: Dict[str, Dict[str, Any]] = {}
        for dof_name, dof_block in rao_data_raw.items():
            if dof_name not in _DOF_NAMES:
                logger.warning("Ignoring unknown DOF '%s' in YAML", dof_name)
                continue

            if not isinstance(dof_block, dict):
                raise ValueError(
                    f"RAO data for '{dof_name}' must be a mapping with "
                    "'magnitude' and 'phase' keys"
                )

            magnitude = dof_block.get("magnitude")
            phase = dof_block.get("phase")
            if magnitude is None:
                raise ValueError(f"Missing 'magnitude' for RAO DOF '{dof_name}'")
            if phase is None:
                raise ValueError(f"Missing 'phase' for RAO DOF '{dof_name}'")

            mag_arr = np.array(magnitude, dtype=np.float64)
            phase_arr = np.array(phase, dtype=np.float64)

            if mag_arr.shape != (nfreq, nhead):
                raise ValueError(
                    f"RAO '{dof_name}' magnitude shape {mag_arr.shape} "
                    f"does not match expected ({nfreq}, {nhead})"
                )
            if phase_arr.shape != (nfreq, nhead):
                raise ValueError(
                    f"RAO '{dof_name}' phase shape {phase_arr.shape} "
                    f"does not match expected ({nfreq}, {nhead})"
                )

            rao_data[dof_name] = {
                "magnitude": mag_arr.tolist(),
                "phase": phase_arr.tolist(),
            }

        # Parse optional added mass and damping
        added_mass_data = data.get("added_mass")
        damping_data = data.get("damping")

        if added_mass_data is not None:
            _validate_matrix_entries(added_mass_data, "added_mass")
        if damping_data is not None:
            _validate_matrix_entries(damping_data, "damping")

        return cls._build_results(
            vessel_name=vessel_name,
            water_depth=water_depth,
            frequencies_rad_s=frequencies_rad_s,
            headings_deg=headings_deg,
            rao_data=rao_data,
            added_mass_data=added_mass_data,
            damping_data=damping_data,
            source=str(path),
        )

    @staticmethod
    def _redimensionalize_wamit(
        nondim_value: float,
        rho: float,
        characteristic_length: float,
        omega: float,
        coefficient_type: str,
    ) -> float:
        """Convert WAMIT non-dimensional coefficient to SI units.

        WAMIT non-dimensionalizes hydrodynamic coefficients as:
        - Added mass:  A_bar = A / (rho * L^k)
        - Damping:     B_bar = B / (rho * omega * L^k)

        where L is the characteristic length and k is an exponent that
        depends on the DOF coupling:
        - k=3 for linear-linear (e.g. surge-surge)
        - k=4 for linear-rotational (e.g. surge-roll)
        - k=5 for rotational-rotational (e.g. roll-roll)

        Args:
            nondim_value: Non-dimensional WAMIT coefficient.
            rho: Fluid density (kg/m^3), typically 1025.0 for seawater.
            characteristic_length: Reference length (m). Typically the
                cube root of displaced volume, or half-beam.
            omega: Wave circular frequency (rad/s). Only used for damping.
            coefficient_type: One of 'added_mass_linear',
                'added_mass_rotational', 'added_mass_coupled',
                'damping_linear', 'damping_rotational', 'damping_coupled'.

        Returns:
            Dimensional coefficient in SI units.

        Raises:
            ValueError: If coefficient_type is not recognized.
        """
        L = characteristic_length  # noqa: N806

        type_to_exponent = {
            "added_mass_linear": 3,
            "added_mass_coupled": 4,
            "added_mass_rotational": 5,
            "damping_linear": 3,
            "damping_coupled": 4,
            "damping_rotational": 5,
        }

        if coefficient_type not in type_to_exponent:
            raise ValueError(
                f"Unknown coefficient_type '{coefficient_type}'. "
                f"Expected one of: {list(type_to_exponent.keys())}"
            )

        k = type_to_exponent[coefficient_type]

        if coefficient_type.startswith("added_mass"):
            return nondim_value * rho * (L ** k)
        else:
            # Damping: B = B_bar * rho * omega * L^k
            return nondim_value * rho * omega * (L ** k)

    @staticmethod
    def redimensionalize_matrix(
        nondim_matrix: np.ndarray,
        rho: float,
        characteristic_length: float,
        omega: float,
        matrix_type: str,
    ) -> np.ndarray:
        """Convert a full 6x6 WAMIT non-dimensional matrix to SI.

        Applies the correct exponent for each (i, j) coupling pair based
        on the _NONDIM_EXPONENT table.

        Args:
            nondim_matrix: 6x6 non-dimensional WAMIT matrix.
            rho: Fluid density (kg/m^3).
            characteristic_length: Reference length (m).
            omega: Wave frequency (rad/s). Used only for damping.
            matrix_type: Either 'added_mass' or 'damping'.

        Returns:
            6x6 dimensional matrix in SI units.
        """
        L = characteristic_length  # noqa: N806
        dim_matrix = np.zeros((6, 6), dtype=np.float64)

        for i in range(6):
            for j in range(6):
                k = int(_NONDIM_EXPONENT[i, j])
                if matrix_type == "added_mass":
                    dim_matrix[i, j] = nondim_matrix[i, j] * rho * (L ** k)
                elif matrix_type == "damping":
                    dim_matrix[i, j] = nondim_matrix[i, j] * rho * omega * (L ** k)
                else:
                    raise ValueError(
                        f"matrix_type must be 'added_mass' or 'damping', "
                        f"got '{matrix_type}'"
                    )

        return dim_matrix

    @staticmethod
    def _build_results(
        vessel_name: str,
        water_depth: float,
        frequencies_rad_s: list,
        headings_deg: list,
        rao_data: dict,
        added_mass_data: Optional[list],
        damping_data: Optional[list],
        source: str,
    ) -> DiffractionResults:
        """Build DiffractionResults from parsed data.

        Constructs the full schema-compliant result object with RAOSet,
        AddedMassSet, and DampingSet. DOFs not present in rao_data are
        filled with zero arrays.

        Args:
            vessel_name: Vessel identifier.
            water_depth: Water depth in metres (inf for deep water).
            frequencies_rad_s: List of circular frequencies.
            headings_deg: List of wave headings in degrees.
            rao_data: Dict mapping DOF name to {magnitude, phase} lists.
            added_mass_data: Optional list of {frequency, matrix} dicts.
            damping_data: Optional list of {frequency, matrix} dicts.
            source: Source identifier string (file path or description).

        Returns:
            Fully populated DiffractionResults.
        """
        freq_arr = np.array(frequencies_rad_s, dtype=np.float64)
        head_arr = np.array(headings_deg, dtype=np.float64)
        nfreq = len(frequencies_rad_s)
        nhead = len(headings_deg)
        now = datetime.now().isoformat()

        # Build FrequencyData and HeadingData
        freq_data = FrequencyData(
            values=freq_arr,
            periods=2.0 * np.pi / freq_arr,
            count=nfreq,
            min_freq=float(freq_arr.min()),
            max_freq=float(freq_arr.max()),
        )
        heading_data = HeadingData(
            values=head_arr,
            count=nhead,
            min_heading=float(head_arr.min()),
            max_heading=float(head_arr.max()),
        )

        # Build RAO components for each DOF
        components: Dict[str, RAOComponent] = {}
        for dof_name, dof_enum in _DOF_NAMES.items():
            if dof_name in rao_data:
                mag = np.array(rao_data[dof_name]["magnitude"], dtype=np.float64)
                phase = np.array(rao_data[dof_name]["phase"], dtype=np.float64)
            else:
                # Fill missing DOFs with zeros
                mag = np.zeros((nfreq, nhead), dtype=np.float64)
                phase = np.zeros((nfreq, nhead), dtype=np.float64)

            components[dof_name] = RAOComponent(
                dof=dof_enum,
                magnitude=mag,
                phase=phase,
                frequencies=FrequencyData(
                    values=freq_arr.copy(),
                    periods=2.0 * np.pi / freq_arr,
                    count=nfreq,
                    min_freq=float(freq_arr.min()),
                    max_freq=float(freq_arr.max()),
                ),
                headings=HeadingData(
                    values=head_arr.copy(),
                    count=nhead,
                    min_heading=float(head_arr.min()),
                    max_heading=float(head_arr.max()),
                ),
                unit="",
            )

        rao_set = RAOSet(
            vessel_name=vessel_name,
            analysis_tool="WAMIT",
            water_depth=water_depth,
            surge=components["surge"],
            sway=components["sway"],
            heave=components["heave"],
            roll=components["roll"],
            pitch=components["pitch"],
            yaw=components["yaw"],
            created_date=now,
            source_file=source,
        )

        # Build added mass matrices
        am_matrices = _build_matrix_list(added_mass_data, "added_mass", freq_arr)
        added_mass_set = AddedMassSet(
            vessel_name=vessel_name,
            analysis_tool="WAMIT",
            water_depth=water_depth,
            matrices=am_matrices,
            frequencies=FrequencyData(
                values=freq_arr.copy(),
                periods=2.0 * np.pi / freq_arr,
                count=nfreq,
                min_freq=float(freq_arr.min()),
                max_freq=float(freq_arr.max()),
            ),
            created_date=now,
            source_file=source,
        )

        # Build damping matrices
        damp_matrices = _build_matrix_list(damping_data, "damping", freq_arr)
        damping_set = DampingSet(
            vessel_name=vessel_name,
            analysis_tool="WAMIT",
            water_depth=water_depth,
            matrices=damp_matrices,
            frequencies=FrequencyData(
                values=freq_arr.copy(),
                periods=2.0 * np.pi / freq_arr,
                count=nfreq,
                min_freq=float(freq_arr.min()),
                max_freq=float(freq_arr.max()),
            ),
            created_date=now,
            source_file=source,
        )

        return DiffractionResults(
            vessel_name=vessel_name,
            analysis_tool="WAMIT",
            water_depth=water_depth,
            raos=rao_set,
            added_mass=added_mass_set,
            damping=damping_set,
            created_date=now,
            source_files=[source],
            phase_convention="unknown",
            unit_system="SI",
        )


# ---------------------------------------------------------------------------
# Module-level helpers
# ---------------------------------------------------------------------------


def _parse_water_depth(raw: Any) -> float:
    """Parse water depth from YAML value.

    Accepts numeric values or string 'infinite'/'inf'.

    Returns:
        Water depth in metres, or float('inf') for deep water.
    """
    if isinstance(raw, (int, float)):
        return float(raw)
    if isinstance(raw, str):
        normalized = raw.strip().lower()
        if normalized in ("infinite", "inf", "deep"):
            return float("inf")
        try:
            return float(normalized)
        except ValueError:
            raise ValueError(
                f"Cannot parse water_depth '{raw}'. "
                "Use a numeric value or 'infinite'."
            )
    raise ValueError(f"Unexpected water_depth type: {type(raw).__name__}")


def _validate_matrix_entries(entries: list, label: str) -> None:
    """Validate structure of added_mass or damping matrix entries from YAML."""
    if not isinstance(entries, list):
        raise ValueError(f"'{label}' must be a list of {{frequency, matrix}} entries")

    for i, entry in enumerate(entries):
        if not isinstance(entry, dict):
            raise ValueError(f"'{label}[{i}]' must be a mapping")
        if "frequency" not in entry:
            raise ValueError(f"'{label}[{i}]' missing 'frequency' key")
        if "matrix" not in entry:
            raise ValueError(f"'{label}[{i}]' missing 'matrix' key")
        mat = np.array(entry["matrix"], dtype=np.float64)
        if mat.shape != (6, 6):
            raise ValueError(
                f"'{label}[{i}]' matrix shape {mat.shape} is not (6, 6)"
            )


def _build_matrix_list(
    data: Optional[list],
    matrix_type: str,
    freq_arr: np.ndarray,
) -> List[HydrodynamicMatrix]:
    """Build list of HydrodynamicMatrix from parsed data or zeros.

    If data is None, returns zero matrices at each frequency in freq_arr.
    """
    if data is not None:
        matrices = []
        for entry in data:
            mat = np.array(entry["matrix"], dtype=np.float64)
            matrices.append(
                HydrodynamicMatrix(
                    matrix=mat,
                    frequency=float(entry["frequency"]),
                    matrix_type=matrix_type,
                    units=_default_units(matrix_type),
                )
            )
        return matrices

    # No data provided: fill with zero matrices
    return [
        HydrodynamicMatrix(
            matrix=np.zeros((6, 6), dtype=np.float64),
            frequency=float(f),
            matrix_type=matrix_type,
            units=_default_units(matrix_type),
        )
        for f in freq_arr
    ]


def _default_units(matrix_type: str) -> Dict[str, str]:
    """Default SI unit labels for matrix types."""
    if matrix_type == "added_mass":
        return {
            "linear": "kg",
            "coupled": "kg.m",
            "rotational": "kg.m^2",
        }
    return {
        "linear": "N.s/m",
        "coupled": "N.m.s/rad",
        "rotational": "N.m.s/rad",
    }
