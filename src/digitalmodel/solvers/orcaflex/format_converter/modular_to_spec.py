"""Convert modular OrcaFlex format to spec.yml (best-effort).

Reverse extraction from OrcaFlex sections back to simplified
spec format. Uses an EXTRACTION_MAP to pull known fields.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml

from .modular_to_single import ModularToSingleConverter
from .protocols import ConversionReport


# Maps OrcaFlex data paths to spec.yml paths.
# Format: (orcaflex_section, orcaflex_key) -> spec_path (dot-notation)
EXTRACTION_MAP: list[tuple[tuple[str, str], str]] = [
    # Environment -> environment
    (("Environment", "WaterDepth"), "environment.water.depth"),
    (("Environment", "Density"), "environment.water.density"),
    (("Environment", "SeabedSlope"), "environment.seabed.slope"),
    (("Environment", "SeabedNormalStiffness"), "environment.seabed.stiffness.normal"),
    (("Environment", "SeabedShearStiffness"), "environment.seabed.stiffness.shear"),
    # Waves — primary train extracted via _populate_wave_trains() below.
    # Legacy single-train extraction kept for confidence scoring only.
    (("_WaveTrain0", "WaveHeight"), "environment.waves.height"),
    (("_WaveTrain0", "WavePeriod"), "environment.waves.period"),
    (("_WaveTrain0", "WaveDirection"), "environment.waves.direction"),
    (("_WaveTrain0", "WaveType"), "environment.waves.type"),
    # Current
    (("Environment", "RefCurrentSpeed"), "environment.current.speed"),
    (("Environment", "RefCurrentDirection"), "environment.current.direction"),
    # Wind
    (("Environment", "WindSpeed"), "environment.wind.speed"),
    (("Environment", "WindDirection"), "environment.wind.direction"),
    # General -> simulation
    (("General", "ImplicitConstantTimeStep"), "simulation.time_step"),
    (("General", "StageDuration"), "simulation.stages"),
    (("General", "NorthDirection"), "simulation.north_direction"),
]

# Total number of extractable fields (for confidence calculation)
_TOTAL_EXTRACTABLE = len(EXTRACTION_MAP)


class ModularToSpecConverter:
    """Convert modular OrcaFlex format to spec.yml (best-effort).

    Not all OrcaFlex data maps to the simplified spec format.
    The converter reports confidence based on how many fields
    were successfully extracted.
    """

    def convert(
        self, source: Path | None = None, target: Path | None = None
    ) -> ConversionReport:
        """Convert modular format to spec.

        Args:
            source: Path to master.yml or modular directory.
            target: Path for output spec.yml.
        """
        if source is None:
            raise ValueError("No source path provided")

        src = Path(source)
        tgt = Path(target) if target else src.parent / "extracted_spec.yml"

        # Merge modular files into single dict
        m2s = ModularToSingleConverter()
        if src.is_dir():
            master = src / "master.yml"
        else:
            master = src
        merged = m2s.merge_to_dict(master)

        if not merged:
            return ConversionReport(
                success=False,
                source_format="modular",
                target_format="spec",
                source_path=src,
                target_path=tgt,
                warnings=["No data found in modular files"],
            )

        # Prepare flat data with WaveTrain extraction
        flat: dict[str, Any] = dict(merged)
        env = flat.get("Environment", {})
        if isinstance(env, dict) and "WaveTrains" in env and env["WaveTrains"]:
            wt = env["WaveTrains"]
            if isinstance(wt, list) and len(wt) > 0 and isinstance(wt[0], dict):
                flat["_WaveTrain0"] = wt[0]

        # Extract fields using EXTRACTION_MAP
        spec: dict[str, Any] = {}
        extracted_count = 0
        unmapped_sections: list[str] = []
        warnings: list[str] = []

        for (section, key), spec_path in EXTRACTION_MAP:
            value = None
            if section in flat and isinstance(flat[section], dict):
                value = flat[section].get(key)

            if value is not None:
                self._set_nested(spec, spec_path, value)
                extracted_count += 1

        # Populate wave trains list when multiple trains are present
        self._populate_wave_trains(flat, spec)

        # Add metadata placeholder
        spec.setdefault("metadata", {})
        spec["metadata"].setdefault("name", "extracted_model")
        spec["metadata"].setdefault(
            "description", "Auto-extracted from OrcaFlex modular format"
        )
        spec["metadata"].setdefault("structure", "unknown")
        spec["metadata"].setdefault("operation", "unknown")

        # Extract pipeline info if available
        lines = merged.get("Lines", [])
        if isinstance(lines, list) and lines:
            first_line = lines[0] if isinstance(lines[0], dict) else {}
            if "Name" in first_line:
                spec.setdefault("pipeline", {})
                spec["pipeline"]["name"] = first_line["Name"]

        # Track unmapped OrcaFlex sections
        known_sections = {"General", "Environment", "VariableData"}
        for section_key in merged:
            if section_key not in known_sections:
                if section_key not in {"_WaveTrain0"}:
                    unmapped_sections.append(section_key)

        # Calculate confidence
        confidence = (
            extracted_count / _TOTAL_EXTRACTABLE if _TOTAL_EXTRACTABLE > 0 else 0.0
        )

        # Write spec with confidence header
        header = (
            f"# Auto-extracted from OrcaFlex modular format\n"
            f"# Confidence: {confidence:.2f} ({extracted_count}/{_TOTAL_EXTRACTABLE} fields)\n"
            f"# Review before use\n"
        )
        content = yaml.dump(
            spec,
            default_flow_style=False,
            allow_unicode=True,
            sort_keys=False,
        )
        tgt.parent.mkdir(parents=True, exist_ok=True)
        tgt.write_text(f"{header}\n{content}")

        return ConversionReport(
            success=True,
            source_format="modular",
            target_format="spec",
            source_path=src,
            target_path=tgt,
            unmapped_sections=unmapped_sections,
            confidence=round(confidence, 2),
            warnings=warnings,
        )

    def supported_formats(self) -> tuple[str, str]:
        return ("modular", "spec")

    # Reverse mapping from OrcaFlex wave type strings to spec type keys
    _WAVE_TYPE_REVERSE: dict[str, str] = {
        "Dean stream": "dean_stream",
        "Airy": "airy",
        "Stokes' 5th": "stokes_5th",
        "Cnoidal": "cnoidal",
        "JONSWAP": "jonswap",
        "Pierson-Moskowitz": "pierson_moskowitz",
        "User defined spectrum": "user_defined",
    }

    def _populate_wave_trains(
        self, flat: dict[str, Any], spec: dict[str, Any]
    ) -> None:
        """Extract wave trains from OrcaFlex WaveTrains list into spec format.

        When multiple wave trains are present, populates
        ``spec["environment"]["waves"]["trains"]`` with all trains.
        For a single train the flat-field extraction from EXTRACTION_MAP
        is sufficient (backward compat), but we still populate trains
        for consistency.
        """
        env_data = flat.get("Environment", {})
        if not isinstance(env_data, dict):
            return
        raw_trains = env_data.get("WaveTrains", [])
        if not isinstance(raw_trains, list) or not raw_trains:
            return

        trains: list[dict[str, Any]] = []
        for i, wt in enumerate(raw_trains):
            if not isinstance(wt, dict):
                continue
            train: dict[str, Any] = {}
            if "Name" in wt:
                train["name"] = wt["Name"]
            raw_type = wt.get("WaveType", "Airy")
            train["type"] = self._WAVE_TYPE_REVERSE.get(raw_type, raw_type)
            if "WaveDirection" in wt:
                train["direction"] = wt["WaveDirection"]
            # Height/period depend on wave type
            if "WaveHeight" in wt:
                train["height"] = wt["WaveHeight"]
            elif "WaveHs" in wt:
                train["height"] = wt["WaveHs"]
            if "WavePeriod" in wt:
                train["period"] = wt["WavePeriod"]
            elif "WaveTz" in wt:
                train["period"] = wt["WaveTz"]
            if "WaveGamma" in wt:
                train["gamma"] = wt["WaveGamma"]
            trains.append(train)

        if len(trains) > 1:
            # Multi-train: use trains list format
            spec.setdefault("environment", {}).setdefault("waves", {})["trains"] = trains
            # Remove flat fields that were extracted by EXTRACTION_MAP
            waves = spec.get("environment", {}).get("waves", {})
            for key in ("type", "height", "period", "direction"):
                waves.pop(key, None)
        elif len(trains) == 1:
            # Single train: flat fields from EXTRACTION_MAP already cover it.
            # Merge any fields the EXTRACTION_MAP missed (e.g. name, gamma).
            waves = spec.setdefault("environment", {}).setdefault("waves", {})
            for key in ("name", "gamma"):
                if key in trains[0] and key not in waves:
                    waves[key] = trains[0][key]

    @staticmethod
    def _set_nested(d: dict, path: str, value: Any) -> None:
        """Set a value in a nested dict using dot-notation path."""
        keys = path.split(".")
        for key in keys[:-1]:
            d = d.setdefault(key, {})
        d[keys[-1]] = value
