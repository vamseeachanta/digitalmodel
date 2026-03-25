"""Convert single monolithic OrcaFlex YAML to modular format.

Promoted from scripts/conversion/yaml_to_include.py.
Uses canonical OrcaFlexDumper from yaml_utils.
"""

from __future__ import annotations

from datetime import datetime
from pathlib import Path
from typing import Any

import yaml

from ..yaml_utils import OrcaFlexDumper, orcaflex_dump, orcaflex_load
from .protocols import ConversionReport
from .section_mapping import INPUT_PARAMETERS, SECTION_MAPPING


class SingleToModularConverter:
    """Convert a single monolithic OrcaFlex YAML to modular format.

    Output structure:
        output_dir/
            master.yml              # Include directives + header
            includes/               # Section-per-file
                01_general.yml
                03_environment.yml
                ...
            inputs/
                parameters.yml      # Extracted input parameters
    """

    def __init__(self, source_path: Path, output_dir: Path | None = None):
        self.source_path = Path(source_path)
        self.output_dir = (
            Path(output_dir)
            if output_dir
            else self.source_path.parent / self.source_path.stem
        )

    def convert(
        self, source: Path | None = None, target: Path | None = None
    ) -> ConversionReport:
        """Execute the conversion.

        Args:
            source: Override source path (for FormatConverter protocol).
            target: Override target path (for FormatConverter protocol).

        Returns:
            ConversionReport with conversion results.
        """
        src = Path(source) if source else self.source_path
        tgt = Path(target) if target else self.output_dir

        warnings: list[str] = []

        # Create directory structure
        tgt.mkdir(parents=True, exist_ok=True)
        includes_dir = tgt / "includes"
        includes_dir.mkdir(exist_ok=True)
        inputs_dir = tgt / "inputs"
        inputs_dir.mkdir(exist_ok=True)

        # Load source
        data, header_lines = orcaflex_load(src)
        if not data:
            return ConversionReport(
                success=False,
                source_format="single",
                target_format="modular",
                source_path=src,
                target_path=tgt,
                warnings=["Source file is empty or invalid"],
            )

        # Extract input parameters
        input_params = self._extract_input_parameters(data)

        # Group data by target filename
        file_groups = self._group_by_filename(data)

        # Generate include files
        include_files: list[str] = []
        for filename, section_data in file_groups.items():
            self._write_include_file(
                includes_dir / filename,
                section_data,
                src.name,
            )
            include_files.append(filename)

        include_files.sort()

        # Generate master.yml
        self._write_master_file(
            tgt / "master.yml",
            header_lines,
            include_files,
            input_params,
        )

        # Generate parameters.yml
        if input_params:
            self._write_parameters_file(
                inputs_dir / "parameters.yml",
                input_params,
                src.name,
            )

        return ConversionReport(
            success=True,
            source_format="single",
            target_format="modular",
            source_path=src,
            target_path=tgt,
            warnings=warnings,
        )

    def supported_formats(self) -> tuple[str, str]:
        return ("single", "modular")

    def _extract_input_parameters(self, data: dict) -> dict[str, Any]:
        """Extract key parameters for parametric analysis."""
        inputs: dict[str, Any] = {}

        for section, params in INPUT_PARAMETERS.items():
            if section in data:
                section_data = data[section]
                if not isinstance(section_data, dict):
                    continue
                for yaml_key, input_key in params.items():
                    if yaml_key in section_data:
                        value = section_data[yaml_key]
                        if value is not None and value != "~":
                            inputs[input_key] = value

        # Extract wave parameters from WaveTrains
        if "Environment" in data:
            env = data["Environment"]
            if isinstance(env, dict) and "WaveTrains" in env and env["WaveTrains"]:
                wave = env["WaveTrains"][0]
                if isinstance(wave, dict):
                    if "WaveHeight" in wave:
                        inputs["hs"] = wave["WaveHeight"]
                    if "WavePeriod" in wave:
                        inputs["tp"] = wave["WavePeriod"]
                    if "WaveDirection" in wave:
                        inputs["wave_direction"] = wave["WaveDirection"]

        return inputs

    def _group_by_filename(self, data: dict) -> dict[str, dict]:
        """Group flat OrcaFlex data by target include filename.

        Multiple OrcaFlex sections can map to the same file
        (e.g., 6DBuoys, Buoys, 3DBuoys all map to 08_buoys.yml).
        This method accumulates them correctly.
        """
        file_groups: dict[str, dict] = {}

        for key, value in data.items():
            if key in SECTION_MAPPING:
                filename = SECTION_MAPPING[key]
            else:
                # Unknown sections go to general
                filename = "01_general.yml"

            if filename not in file_groups:
                file_groups[filename] = {}
            file_groups[filename][key] = value

        return file_groups

    def _write_include_file(
        self, path: Path, section_data: dict, source_name: str
    ) -> None:
        """Write a single include file."""
        # Use first key as section label for the comment header
        first_key = next(iter(section_data), "Unknown")
        header = (
            f"# {first_key} Configuration\n"
            f"# Generated from: {source_name}\n"
            f"# Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}"
        )
        orcaflex_dump(section_data, path, header=header)

    def _write_master_file(
        self,
        path: Path,
        header_lines: list[str],
        include_files: list[str],
        input_params: dict,
    ) -> None:
        """Write the master.yml file."""
        header = (
            "\n".join(header_lines)
            if header_lines
            else "%YAML 1.1\n# Type: Model\n---"
        )

        lines = [header]

        # Input parameters as comments
        if input_params:
            lines.append("# Input Parameters (for parametric analysis)")
            lines.append("# See inputs/parameters.yml for extracted values:")
            for key, value in input_params.items():
                lines.append(f"#   {key}: {value}")
            lines.append("#")

        # Include directives
        lines.append("# Include files")
        for inc_file in include_files:
            lines.append(f"- includefile: includes/{inc_file}")
        lines.append("")

        path.write_text("\n".join(lines))

    def _write_parameters_file(
        self, path: Path, input_params: dict, source_name: str
    ) -> None:
        """Write the parameters.yml file."""
        header = (
            f"# Input Parameters for Parametric Analysis\n"
            f"# Source: {source_name}\n"
            f"# Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n"
        )
        content = yaml.dump(
            input_params,
            Dumper=OrcaFlexDumper,
            default_flow_style=False,
            sort_keys=False,
        )
        path.write_text(f"{header}\n{content}")
