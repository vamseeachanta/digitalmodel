"""OrcaFlex YAML format validator.

Validates generated YAML files match OrcaFlex's expected structure
without requiring an OrcaFlex license or OrcFxAPI.

Rules are derived from empirical testing documented in
docs/domains/orcaflex/CRITICAL_YAML_FORMAT_FINDINGS.md
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any

import yaml


class Severity(Enum):
    ERROR = "error"      # Will definitely fail to load
    WARNING = "warning"  # May cause issues
    INFO = "info"        # Suggestion for improvement


@dataclass
class ValidationIssue:
    severity: Severity
    message: str
    file: str = ""
    line: int | None = None
    property: str = ""

    def __str__(self) -> str:
        parts = [f"[{self.severity.value.upper()}]"]
        if self.file:
            parts.append(f"({self.file})")
        if self.property:
            parts.append(f"'{self.property}':")
        parts.append(self.message)
        return " ".join(parts)


@dataclass
class ValidationResult:
    valid: bool
    issues: list[ValidationIssue] = field(default_factory=list)
    format_detected: str = ""  # "flat" or "include-based" or "hybrid" or "unknown"

    @property
    def errors(self) -> list[ValidationIssue]:
        return [i for i in self.issues if i.severity == Severity.ERROR]

    @property
    def warnings(self) -> list[ValidationIssue]:
        return [i for i in self.issues if i.severity == Severity.WARNING]

    def add(self, severity: Severity, message: str, *,
            file: str = "", line: int | None = None, prop: str = "") -> None:
        self.issues.append(ValidationIssue(severity, message, file=file, line=line, property=prop))
        if severity == Severity.ERROR:
            self.valid = False


# ---------------------------------------------------------------------------
# Known-good schemas
# ---------------------------------------------------------------------------

VALID_TOP_LEVEL_SECTIONS = {
    "General", "Environment", "VesselTypes", "Vessels",
    "LineTypes", "Lines", "6DBuoys", "3DBuoys",
    "Shapes", "Constraints", "Links", "Winches",
    "ClumpTypes", "WingTypes", "BrowserGroups",
    "MorisonElementTypes", "SupportTypes",
    "DragChainTypes", "FlexJointTypes",
    "AttachedBuoys", "LineContents",
    "SolidTypes", "CodeChecks", "TurbineTypes", "Turbines",
    "VariableData", "FrictionCoefficients", "Groups",
}

# Valid top-level keys in include-file fragments (no section wrapper)
VALID_FRAGMENT_TOP_LEVEL = {
    "BaseFile", "includefile",
    # Properties that appear bare at top level inside fragments:
    "StageDuration", "StaticsMinDamping", "DynamicsSolutionMethod",
    "ImplicitConstantTimeStep", "TargetLogSampleInterval", "LogPrecision",
    "StartTime", "FirstStage", "RampStartTime", "RampFinishTime",
    "TimeHistoryImportFrom", "TimeHistoryImportTo",
    "RestartStateRecordingPeriodicCount", "RestartStateRecordingTest",
    # New: declarations for named objects
    "New",
    # Wave fragment properties (inside Environment context)
    "KinematicStretchingMethod", "UserSpecifiedRandomWaveSeeds",
    "WaveFrequencySpectrumDiscretisationMethod", "WaveTrains",
    # Current / wind properties (inside Environment context)
    "RefCurrentSpeed", "RefCurrentDirection",
    "CurrentDepth", "CurrentFactor", "CurrentRotation",
    "WindSpeed", "WindDirection", "WindType",
}

# Properties that are known INVALID (commonly generated incorrectly)
INVALID_PROPERTIES = {
    "ImplicitVariableMaxTimeStep",  # Doesn't exist in OrcaFlex
    "CategoryType",                 # Not a valid LineType property
    "NumMooringLines",              # Not a valid OrcaFlex variable
}

# Wave properties that must live inside WaveTrains list items,
# never at the Environment level directly.
WAVE_TRAIN_ONLY_PROPERTIES = {
    "WaveType", "WaveHs", "WaveHeight", "WaveTz", "WaveTp",
    "WaveDirection", "WaveOrigin", "WaveTimeOrigin",
    "WaveJONSWAPParameters", "WaveGamma",
    "WaveNumberOfComponents", "WaveNumberOfSpectralDirections",
    "WaveSpectrumMinRelFrequency", "WaveSpectrumMaxRelFrequency",
    "WaveSpectrumMaxComponentFrequencyRange",
}

KNOWN_GENERAL_PROPS = {
    "UnitsSystem", "StageDuration", "StaticsMinDamping",
    "DynamicsSolutionMethod", "ImplicitConstantTimeStep",
    "TargetLogSampleInterval", "LogPrecision",
    "StartTime", "FirstStage", "RampStartTime", "RampFinishTime",
    "TimeHistoryImportFrom", "TimeHistoryImportTo",
    "RestartStateRecordingPeriodicCount", "RestartStateRecordingTest",
    "StaticsStep1", "StaticsMinConvergenceSteps",
}

KNOWN_ENVIRONMENT_PROPS = {
    "WaterDepth", "Density", "WaterSurfaceZ", "SeabedOriginDepth",
    "SeabedSlope", "SeabedSlopeDirection", "SeabedType", "SeabedModel",
    "SeabedNormalStiffness", "SeabedShearStiffness", "SeabedOrigin",
    "WaveTrains", "RefCurrentSpeed", "RefCurrentDirection",
    "CurrentDepth", "CurrentFactor", "CurrentRotation",
    "WindSpeed", "WindDirection", "WindType",
    "KinematicStretchingMethod", "KinematicViscosity",
    "UserSpecifiedRandomWaveSeeds",
    "WaveFrequencySpectrumDiscretisationMethod",
    "includefile",
}


class OrcaFlexYAMLValidator:
    """Validates OrcaFlex YAML files for structural correctness."""

    def validate_file(self, path: Path) -> ValidationResult:
        """Validate a single YAML file.

        Args:
            path: Path to the YAML file.

        Returns:
            ValidationResult with any issues found.
        """
        result = ValidationResult(valid=True)
        filename = path.name

        if not path.exists():
            result.add(Severity.ERROR, f"File does not exist: {path}", file=filename)
            return result

        if not path.suffix.lower() in (".yml", ".yaml"):
            result.add(Severity.WARNING, "File does not have .yml or .yaml extension", file=filename)

        try:
            raw_text = path.read_text(encoding="utf-8-sig")
        except Exception as exc:
            result.add(Severity.ERROR, f"Cannot read file: {exc}", file=filename)
            return result

        if not raw_text.strip():
            result.add(Severity.ERROR, "File is empty", file=filename)
            return result

        try:
            data = yaml.safe_load(raw_text)
        except yaml.YAMLError as exc:
            result.add(Severity.ERROR, f"YAML parse error: {exc}", file=filename)
            return result

        if data is None:
            result.add(Severity.WARNING, "File parses to null/empty", file=filename)
            return result

        fmt = self._detect_format(data)
        result.format_detected = fmt

        if fmt == "flat":
            self._validate_flat_format(data, result, filename)
        elif fmt == "include-based":
            self._validate_include_format(data, result, filename, base_dir=path.parent)
        elif fmt == "hybrid":
            result.add(
                Severity.ERROR,
                "Hybrid format detected (list with non-include items). "
                "OrcaFlex requires either pure flat YAML (dict) or pure include-based (list of includefile entries).",
                file=filename,
            )
            # Still validate individual items for more diagnostics
            self._validate_include_format(data, result, filename, base_dir=path.parent)
        elif fmt == "fragment":
            self._validate_fragment(data, result, filename)
        else:
            result.add(Severity.ERROR, f"Unrecognized YAML structure (type: {type(data).__name__})", file=filename)

        return result

    def validate_directory(self, path: Path) -> ValidationResult:
        """Validate all YAML files in a directory (master + includes).

        Args:
            path: Directory containing YAML files.

        Returns:
            Aggregated ValidationResult.
        """
        result = ValidationResult(valid=True)

        if not path.is_dir():
            result.add(Severity.ERROR, f"Not a directory: {path}")
            return result

        yml_files = sorted(path.glob("*.yml")) + sorted(path.glob("*.yaml"))

        if not yml_files:
            result.add(Severity.WARNING, f"No YAML files found in {path}")
            return result

        for yml_file in yml_files:
            file_result = self.validate_file(yml_file)
            result.issues.extend(file_result.issues)
            if not file_result.valid:
                result.valid = False

        return result

    # ------------------------------------------------------------------
    # Format detection
    # ------------------------------------------------------------------

    def _detect_format(self, data: Any) -> str:
        """Detect whether file is flat, include-based, fragment, or hybrid.

        Returns:
            One of: "flat", "include-based", "hybrid", "fragment", "unknown".
        """
        if isinstance(data, list):
            all_includes = all(
                isinstance(item, dict) and "includefile" in item
                for item in data
                if isinstance(item, dict)
            )
            # Also check that every item is a dict
            all_dicts = all(isinstance(item, dict) for item in data)
            if all_dicts and all_includes and len(data) > 0:
                return "include-based"
            return "hybrid"
        elif isinstance(data, dict):
            # Check if it has any recognized top-level sections -> flat
            has_sections = any(k in VALID_TOP_LEVEL_SECTIONS for k in data)
            if has_sections:
                return "flat"
            # Otherwise it's a fragment file (e.g. stages, wave params)
            return "fragment"
        return "unknown"

    # ------------------------------------------------------------------
    # Flat format validation
    # ------------------------------------------------------------------

    def _validate_flat_format(self, data: dict, result: ValidationResult, filename: str) -> None:
        """Validate flat YAML format (top-level dict with named sections)."""
        for key in data:
            if key in VALID_TOP_LEVEL_SECTIONS or key == "BaseFile":
                continue
            # Check if it looks like a named object declaration (New: pattern)
            if key == "New":
                continue
            result.issues.append(ValidationIssue(
                Severity.WARNING,
                f"Unknown top-level section: '{key}'. "
                f"Valid sections include: General, Environment, VesselTypes, Lines, etc.",
                file=filename, property=key,
            ))

        if "Environment" in data and isinstance(data["Environment"], dict):
            self._validate_environment(data["Environment"], result, filename)

        if "General" in data and isinstance(data["General"], dict):
            self._validate_general(data["General"], result, filename)

        if "VesselTypes" in data:
            self._validate_vessel_types(data["VesselTypes"], result, filename)

        if "Vessels" in data:
            self._validate_vessels(data, result, filename)

        if "LineTypes" in data:
            self._validate_line_types(data["LineTypes"], result, filename)

        if "Lines" in data:
            self._validate_lines(data, result, filename)

        # Check for common singular-key mistakes
        self._check_singular_section_keys(data, result, filename)

    # ------------------------------------------------------------------
    # Include-based format validation
    # ------------------------------------------------------------------

    def _validate_include_format(
        self, data: list, result: ValidationResult, filename: str, *, base_dir: Path | None = None,
    ) -> None:
        """Validate include-based format (list of includefile entries)."""
        for idx, item in enumerate(data):
            if not isinstance(item, dict):
                result.add(
                    Severity.ERROR,
                    f"Item {idx} in include list is not a dict: {type(item).__name__}",
                    file=filename,
                )
                continue

            if "includefile" not in item:
                result.add(
                    Severity.ERROR,
                    f"Item {idx} in include list has no 'includefile' key. "
                    f"Keys found: {list(item.keys())}",
                    file=filename,
                )
                continue

            include_path = item["includefile"]
            if not isinstance(include_path, str):
                result.add(
                    Severity.ERROR,
                    f"includefile value must be a string, got {type(include_path).__name__}",
                    file=filename,
                )
                continue

            # Check that the referenced file exists (relative to base_dir)
            if base_dir is not None:
                resolved = base_dir / include_path
                if not resolved.exists():
                    result.add(
                        Severity.ERROR,
                        f"Include file not found: '{include_path}' (resolved: {resolved})",
                        file=filename, prop="includefile",
                    )

    # ------------------------------------------------------------------
    # Fragment validation (modular files without section wrappers)
    # ------------------------------------------------------------------

    def _validate_fragment(self, data: dict, result: ValidationResult, filename: str) -> None:
        """Validate a fragment file (bare properties, not wrapped in a section)."""
        self._check_invalid_properties_recursive(data, result, filename)

        # Check for wave properties at top level (valid only inside WaveTrains list)
        for prop in WAVE_TRAIN_ONLY_PROPERTIES:
            if prop in data:
                result.add(
                    Severity.ERROR,
                    f"Wave property '{prop}' must be inside a WaveTrains list item, "
                    f"not at the top level of a fragment",
                    file=filename, prop=prop,
                )

        # Check for common singular-key mistakes (same as flat format checks)
        self._check_singular_section_keys(data, result, filename)

    # ------------------------------------------------------------------
    # Section-specific validation
    # ------------------------------------------------------------------

    def _validate_environment(self, env: dict, result: ValidationResult, filename: str) -> None:
        """Validate Environment section properties."""
        # Check for globally invalid properties
        self._check_invalid_properties(env, result, filename)

        # Check for wave properties at Environment level (must be in WaveTrains)
        for prop in WAVE_TRAIN_ONLY_PROPERTIES:
            if prop in env:
                result.add(
                    Severity.ERROR,
                    f"Wave property '{prop}' must be inside WaveTrains list, "
                    f"not at Environment level",
                    file=filename, prop=prop,
                )

        # Validate WaveTrains structure if present
        if "WaveTrains" in env:
            self._validate_wave_trains(env["WaveTrains"], result, filename)

        # Validate WaterDepth is numeric
        if "WaterDepth" in env:
            wd = env["WaterDepth"]
            if wd is not None and not isinstance(wd, (int, float)):
                result.add(
                    Severity.ERROR,
                    f"WaterDepth must be numeric, got {type(wd).__name__}: {wd}",
                    file=filename, prop="WaterDepth",
                )

        # Check for include-based environment (list of includes under Environment)
        if isinstance(env, dict):
            for key, val in env.items():
                if isinstance(val, list) and all(
                    isinstance(item, dict) and "includefile" in item for item in val if isinstance(item, dict)
                ):
                    # This is valid: Environment with nested include files
                    pass

    def _validate_general(self, gen: dict, result: ValidationResult, filename: str) -> None:
        """Validate General section properties."""
        self._check_invalid_properties(gen, result, filename)

        # StageDuration should be a list of positive numbers
        if "StageDuration" in gen:
            sd = gen["StageDuration"]
            if isinstance(sd, list):
                for idx, val in enumerate(sd):
                    if isinstance(val, (int, float)) and val < 0:
                        result.add(
                            Severity.WARNING,
                            f"StageDuration[{idx}] is negative ({val}). "
                            f"OrcaFlex expects positive values.",
                            file=filename, prop="StageDuration",
                        )
            elif sd is not None:
                result.add(
                    Severity.WARNING,
                    f"StageDuration should be a list, got {type(sd).__name__}",
                    file=filename, prop="StageDuration",
                )

    def _validate_wave_trains(self, wave_trains: Any, result: ValidationResult, filename: str) -> None:
        """Validate WaveTrains structure."""
        if not isinstance(wave_trains, list):
            result.add(
                Severity.ERROR,
                f"WaveTrains must be a list, got {type(wave_trains).__name__}",
                file=filename, prop="WaveTrains",
            )
            return

        for idx, train in enumerate(wave_trains):
            if not isinstance(train, dict):
                result.add(
                    Severity.ERROR,
                    f"WaveTrains[{idx}] must be a dict, got {type(train).__name__}",
                    file=filename, prop="WaveTrains",
                )
                continue

            if "Name" not in train:
                result.add(
                    Severity.WARNING,
                    f"WaveTrains[{idx}] is missing 'Name' property",
                    file=filename, prop="WaveTrains",
                )

    def _validate_vessel_types(self, vessel_types: Any, result: ValidationResult, filename: str) -> None:
        """Validate VesselTypes section structure."""
        if not isinstance(vessel_types, list):
            result.add(
                Severity.ERROR,
                f"VesselTypes must be a list, got {type(vessel_types).__name__}",
                file=filename, prop="VesselTypes",
            )
            return

        for idx, vt in enumerate(vessel_types):
            if not isinstance(vt, dict):
                result.add(
                    Severity.ERROR,
                    f"VesselTypes[{idx}] must be a dict",
                    file=filename, prop="VesselTypes",
                )
                continue

            if "Name" not in vt:
                result.add(
                    Severity.WARNING,
                    f"VesselTypes[{idx}] is missing 'Name'",
                    file=filename, prop="VesselTypes",
                )

            # Check for inline vessel type definition (common mistake)
            if "VesselType" in vt and isinstance(vt.get("VesselType"), dict):
                result.add(
                    Severity.ERROR,
                    f"VesselTypes[{idx}] has nested 'VesselType' dict. "
                    f"VesselType and Vessel definitions must be separate.",
                    file=filename, prop="VesselType",
                )

    def _validate_vessels(self, data: dict, result: ValidationResult, filename: str) -> None:
        """Validate Vessels section and cross-reference VesselTypes."""
        vessels = data.get("Vessels")
        if not isinstance(vessels, list):
            result.add(
                Severity.ERROR,
                f"Vessels must be a list, got {type(vessels).__name__}",
                file=filename, prop="Vessels",
            )
            return

        # Collect known vessel type names for cross-referencing
        known_types: set[str] = set()
        vt_section = data.get("VesselTypes")
        if isinstance(vt_section, list):
            for vt in vt_section:
                if isinstance(vt, dict) and "Name" in vt:
                    known_types.add(vt["Name"])

        for idx, vessel in enumerate(vessels):
            if not isinstance(vessel, dict):
                result.add(
                    Severity.ERROR,
                    f"Vessels[{idx}] must be a dict",
                    file=filename, prop="Vessels",
                )
                continue

            if "Name" not in vessel:
                result.add(
                    Severity.WARNING,
                    f"Vessels[{idx}] is missing 'Name'",
                    file=filename, prop="Vessels",
                )

            # VesselType reference should be a string, not a dict
            vt_ref = vessel.get("VesselType")
            if isinstance(vt_ref, dict):
                result.add(
                    Severity.ERROR,
                    f"Vessels[{idx}] 'VesselType' must be a string reference, not an inline dict. "
                    f"Define vessel types in VesselTypes section separately.",
                    file=filename, prop="VesselType",
                )
            elif isinstance(vt_ref, str) and known_types and vt_ref not in known_types:
                result.add(
                    Severity.WARNING,
                    f"Vessels[{idx}] references VesselType '{vt_ref}' which is not defined in this file. "
                    f"This is fine if it's defined in an include file.",
                    file=filename, prop="VesselType",
                )

    def _validate_line_types(self, line_types: Any, result: ValidationResult, filename: str) -> None:
        """Validate LineTypes section structure."""
        if not isinstance(line_types, list):
            result.add(
                Severity.ERROR,
                f"LineTypes must be a list, got {type(line_types).__name__}",
                file=filename, prop="LineTypes",
            )
            return

        for idx, lt in enumerate(line_types):
            if not isinstance(lt, dict):
                result.add(
                    Severity.ERROR,
                    f"LineTypes[{idx}] must be a dict",
                    file=filename, prop="LineTypes",
                )
                continue

            if "Name" not in lt:
                result.add(
                    Severity.WARNING,
                    f"LineTypes[{idx}] is missing 'Name'",
                    file=filename, prop="LineTypes",
                )

            self._check_invalid_properties(lt, result, filename)

    def _validate_lines(self, data: dict, result: ValidationResult, filename: str) -> None:
        """Validate Lines section and cross-reference LineTypes."""
        lines = data.get("Lines")
        if not isinstance(lines, list):
            result.add(
                Severity.ERROR,
                f"Lines must be a list, got {type(lines).__name__}",
                file=filename, prop="Lines",
            )
            return

        # Collect known object names for connection validation
        known_objects: set[str] = set()
        for section in ("Vessels", "6DBuoys", "3DBuoys"):
            sec = data.get(section)
            if isinstance(sec, list):
                for item in sec:
                    if isinstance(item, dict) and "Name" in item:
                        known_objects.add(item["Name"])

        known_line_types: set[str] = set()
        lt_section = data.get("LineTypes")
        if isinstance(lt_section, list):
            for lt in lt_section:
                if isinstance(lt, dict) and "Name" in lt:
                    known_line_types.add(lt["Name"])

        for idx, line in enumerate(lines):
            if not isinstance(line, dict):
                result.add(
                    Severity.ERROR,
                    f"Lines[{idx}] must be a dict",
                    file=filename, prop="Lines",
                )
                continue

            if "Name" not in line:
                result.add(
                    Severity.WARNING,
                    f"Lines[{idx}] is missing 'Name'",
                    file=filename, prop="Lines",
                )

            # Check LineType reference
            lt_ref = line.get("LineType")
            if lt_ref is not None:
                lt_names: list[str] = []
                if isinstance(lt_ref, str):
                    lt_names = [lt_ref]
                elif isinstance(lt_ref, list):
                    lt_names = [n for n in lt_ref if isinstance(n, str)]

                if known_line_types:
                    for name in lt_names:
                        if name not in known_line_types:
                            result.add(
                                Severity.WARNING,
                                f"Lines[{idx}] references LineType '{name}' which is not defined in this file",
                                file=filename, prop="LineType",
                            )

            # Validate Connection format
            conn = line.get("Connection")
            if conn is not None and not isinstance(conn, (list, str)):
                result.add(
                    Severity.WARNING,
                    f"Lines[{idx}] 'Connection' should be a list [EndA, EndB] or string",
                    file=filename, prop="Connection",
                )

    # ------------------------------------------------------------------
    # Utility methods
    # ------------------------------------------------------------------

    def _check_singular_section_keys(self, data: dict, result: ValidationResult, filename: str) -> None:
        """Check for common singular-key mistakes (Vessel vs Vessels, etc.)."""
        _SINGULAR_TO_PLURAL = {
            "Vessel": "Vessels",
            "LineType": "LineTypes",
            "VesselType": "VesselTypes",
            "Line": "Lines",
        }
        for singular, plural in _SINGULAR_TO_PLURAL.items():
            if singular in data and plural not in data:
                result.add(
                    Severity.ERROR,
                    f"Found '{singular}:' — OrcaFlex expects '{plural}:' (plural) as the top-level section",
                    file=filename, prop=singular,
                )

    def _check_invalid_properties(self, section: dict, result: ValidationResult, filename: str) -> None:
        """Check a dict for known-invalid property names."""
        for prop in INVALID_PROPERTIES:
            if prop in section:
                result.add(
                    Severity.ERROR,
                    f"Invalid property '{prop}' — this property does not exist in OrcaFlex",
                    file=filename, prop=prop,
                )

    def _check_invalid_properties_recursive(self, data: Any, result: ValidationResult, filename: str) -> None:
        """Recursively check for invalid properties in nested dicts."""
        if isinstance(data, dict):
            self._check_invalid_properties(data, result, filename)
            for value in data.values():
                self._check_invalid_properties_recursive(value, result, filename)
        elif isinstance(data, list):
            for item in data:
                self._check_invalid_properties_recursive(item, result, filename)


def validate_orcaflex_yaml(path: Path) -> ValidationResult:
    """Convenience function to validate an OrcaFlex YAML file or directory.

    Args:
        path: Path to a YAML file or directory of YAML files.

    Returns:
        ValidationResult with issues found.
    """
    validator = OrcaFlexYAMLValidator()
    if path.is_dir():
        return validator.validate_directory(path)
    return validator.validate_file(path)
