"""YAML/JSON fixture loading and dumping for subsea cross-sections."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from ruamel.yaml import YAML

from digitalmodel.subsea.cross_sections.schema import CrossSectionDefinition


def _read_data(path: Path) -> dict[str, Any]:
    if path.suffix.lower() == ".json":
        return json.loads(path.read_text())
    yaml = YAML(typ="safe")
    data = yaml.safe_load(path.read_text())
    if not isinstance(data, dict):
        raise ValueError(f"fixture {path} must contain a mapping")
    return data


def load_cross_section_fixture(path: str | Path) -> CrossSectionDefinition:
    """Load a YAML or JSON fixture into a validated cross-section definition."""

    fixture_path = Path(path)
    return CrossSectionDefinition.model_validate(_read_data(fixture_path))


def dump_cross_section_fixture(
    definition: CrossSectionDefinition | dict[str, Any], path: str | Path
) -> None:
    """Dump a validated definition to stable YAML or JSON."""

    parsed = (
        definition
        if isinstance(definition, CrossSectionDefinition)
        else CrossSectionDefinition.model_validate(definition)
    )
    output_path = Path(path)
    data = parsed.to_serializable_dict()
    if output_path.suffix.lower() == ".json":
        output_path.write_text(json.dumps(data, indent=2, sort_keys=True, ensure_ascii=False) + "\n")
        return
    yaml = YAML()
    yaml.default_flow_style = False
    yaml.indent(mapping=2, sequence=4, offset=2)
    with output_path.open("w", encoding="utf-8") as stream:
        yaml.dump(data, stream)
