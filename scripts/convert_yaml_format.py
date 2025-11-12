#!/usr/bin/env python3
"""
ABOUTME: Convert YAML between technical and human-friendly formats
ABOUTME: Bidirectional conversion utility for CALM Buoy configuration files
"""

import sys
import argparse
from pathlib import Path
import yaml
import io

# Configure stdout for UTF-8
if sys.platform == 'win32':
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

# Add scripts to path
sys.path.insert(0, str(Path(__file__).parent))
from yaml_key_mapper import YAMLKeyMapper


def convert_to_human_friendly(input_file: Path, output_file: Path):
    """Convert technical YAML to human-friendly format."""
    print(f"📖 Loading technical YAML: {input_file}")

    with open(input_file, 'r', encoding='utf-8') as f:
        config = yaml.safe_load(f)

    mapper = YAMLKeyMapper()
    human_config = mapper.convert_dict_to_human(config)

    print(f"✍️  Writing human-friendly YAML: {output_file}")

    with open(output_file, 'w', encoding='utf-8') as f:
        # Write header
        f.write("# " + "═" * 79 + "\n")
        f.write("# CALM BUOY PROJECT CONFIGURATION (Human-Friendly Format)\n")
        f.write("# " + "═" * 79 + "\n")
        f.write("#\n")
        f.write("# This file uses human-readable keys for intuitive editing.\n")
        f.write("# Units are specified in comments for clarity.\n")
        f.write("#\n")
        f.write("# " + "═" * 79 + "\n\n")

        # Write YAML with human-friendly keys
        yaml.dump(human_config, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

    print("✅ Conversion complete!")


def convert_to_technical(input_file: Path, output_file: Path):
    """Convert human-friendly YAML to technical format."""
    print(f"📖 Loading human-friendly YAML: {input_file}")

    with open(input_file, 'r', encoding='utf-8') as f:
        config = yaml.safe_load(f)

    mapper = YAMLKeyMapper()
    technical_config = mapper.convert_dict_to_technical(config)

    print(f"✍️  Writing technical YAML: {output_file}")

    with open(output_file, 'w', encoding='utf-8') as f:
        yaml.dump(technical_config, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

    print("✅ Conversion complete!")


def main():
    parser = argparse.ArgumentParser(
        description="Convert YAML between technical and human-friendly formats"
    )
    parser.add_argument('input', type=Path, help='Input YAML file')
    parser.add_argument('output', type=Path, help='Output YAML file')
    parser.add_argument(
        '--to',
        choices=['human', 'technical'],
        required=True,
        help='Target format'
    )

    args = parser.parse_args()

    if not args.input.exists():
        print(f"❌ Error: Input file not found: {args.input}")
        return 1

    if args.to == 'human':
        convert_to_human_friendly(args.input, args.output)
    else:
        convert_to_technical(args.input, args.output)

    return 0


if __name__ == "__main__":
    sys.exit(main())
