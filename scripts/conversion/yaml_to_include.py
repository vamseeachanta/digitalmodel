#!/usr/bin/env python3
"""
OrcaFlex YAML to Include-Based Format Converter (backward-compat wrapper).

This module delegates to the canonical implementations:
- OrcaFlexDumper from digitalmodel.modules.orcaflex.yaml_utils
- SingleToModularConverter from digitalmodel.modules.orcaflex.format_converter
- SECTION_MAPPING / INPUT_PARAMETERS from format_converter.section_mapping

Legacy callers can continue using OrcaFlexYAMLConverter and batch_convert.
"""

import argparse
from pathlib import Path
from typing import List

from digitalmodel.modules.orcaflex.yaml_utils import OrcaFlexDumper
from digitalmodel.modules.orcaflex.format_converter.section_mapping import (
    INPUT_PARAMETERS,
    SECTION_MAPPING,
)
from digitalmodel.modules.orcaflex.format_converter.single_to_modular import (
    SingleToModularConverter,
)


class OrcaFlexYAMLConverter:
    """Backward-compat wrapper around SingleToModularConverter.

    Preserves the original API where convert() returns the output Path.
    """

    def __init__(self, source_path: str, output_dir: str | None = None):
        src = Path(source_path)
        out = Path(output_dir) if output_dir else src.parent / src.stem
        self._converter = SingleToModularConverter(src, out)
        self.source_path = src
        self.output_dir = out

    def convert(self) -> Path:
        """Execute the conversion, returning the output directory."""
        report = self._converter.convert()
        return report.target_path


def batch_convert(source_dir: str, output_base: str, pattern: str = "*.yml") -> List[Path]:
    """Batch convert multiple YAML files."""
    source_path = Path(source_dir)
    output_base_path = Path(output_base)

    converted = []
    for yml_file in source_path.rglob(pattern):
        if 'includes' in str(yml_file) or 'master.yml' in str(yml_file):
            continue

        rel_path = yml_file.relative_to(source_path)
        output_dir = output_base_path / rel_path.parent / yml_file.stem

        try:
            converter = OrcaFlexYAMLConverter(str(yml_file), str(output_dir))
            result = converter.convert()
            converted.append(result)
            print(f"Converted: {yml_file.name} -> {result}")
        except Exception as e:
            print(f"Error converting {yml_file}: {e}")

    return converted


def main():
    parser = argparse.ArgumentParser(
        description='Convert OrcaFlex YAML files to include-based format'
    )
    parser.add_argument('source', help='Source YAML file or directory')
    parser.add_argument('-o', '--output', help='Output directory (default: source_name/)')
    parser.add_argument('-b', '--batch', action='store_true',
                        help='Batch convert all YAML files in directory')
    parser.add_argument('-p', '--pattern', default='*.yml',
                        help='File pattern for batch mode (default: *.yml)')

    args = parser.parse_args()

    if args.batch:
        output_base = args.output or args.source
        converted = batch_convert(args.source, output_base, args.pattern)
        print(f"\nConverted {len(converted)} files")
    else:
        converter = OrcaFlexYAMLConverter(args.source, args.output)
        result = converter.convert()
        print(f"Converted to: {result}")


if __name__ == '__main__':
    main()
