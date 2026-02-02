"""Canonical OrcaFlex YAML utilities.

Provides the single authoritative OrcaFlexDumper and convenience functions
for loading/dumping OrcaFlex-format YAML files.

Replaces duplicate implementations in:
- scripts/conversion/yaml_to_include.py
- src/digitalmodel/modules/orcaflex/library_generator.py
"""

from pathlib import Path
from typing import Any

import yaml


class OrcaFlexDumper(yaml.SafeDumper):
    """Custom YAML dumper for OrcaFlex format.

    Handles:
    - None -> ~ (OrcaFlex null representation)
    - bool -> Yes/No (OrcaFlex boolean format)
    - sort_keys=False (preserve insertion order)
    - width=1000 (avoid unwanted line wrapping)
    """
    pass


def _represent_none(dumper: yaml.Dumper, data: Any) -> yaml.ScalarNode:
    """Represent None as ~ (OrcaFlex format)."""
    return dumper.represent_scalar('tag:yaml.org,2002:null', '~')


def _represent_bool(dumper: yaml.Dumper, data: Any) -> yaml.ScalarNode:
    """Represent bool as Yes/No (OrcaFlex format)."""
    return dumper.represent_scalar('tag:yaml.org,2002:bool', 'Yes' if data else 'No')


OrcaFlexDumper.add_representer(type(None), _represent_none)
OrcaFlexDumper.add_representer(bool, _represent_bool)


def orcaflex_dump(data: dict, path: Path, header: str | None = None) -> None:
    """Dump data to an OrcaFlex-format YAML file.

    Args:
        data: Dictionary to write.
        path: Output file path.
        header: Optional header text (lines starting with # or %YAML).
    """
    path.parent.mkdir(parents=True, exist_ok=True)
    content = yaml.dump(
        data,
        Dumper=OrcaFlexDumper,
        default_flow_style=False,
        allow_unicode=True,
        sort_keys=False,
        width=1000,
    )
    with open(path, 'w') as f:
        if header:
            f.write(header.rstrip('\n') + '\n')
            if not header.endswith('---'):
                f.write('---\n')
        f.write(content)


def orcaflex_load(path: Path) -> tuple[dict, list[str]]:
    """Load an OrcaFlex-format YAML file, preserving header comments.

    Args:
        path: Path to YAML file.

    Returns:
        Tuple of (parsed data dict, list of header comment lines).
    """
    header_lines: list[str] = []
    yaml_lines: list[str] = []
    in_header = True

    with open(path, encoding='utf-8-sig') as f:
        for line in f:
            stripped = line.rstrip('\n')
            if in_header:
                if stripped.startswith(('%YAML', '#', '---')) or stripped == '':
                    header_lines.append(stripped)
                    if stripped == '---':
                        in_header = False
                    continue
                else:
                    in_header = False
            yaml_lines.append(line)

    data = yaml.safe_load('\n'.join(yaml_lines)) or {}
    return data, header_lines
