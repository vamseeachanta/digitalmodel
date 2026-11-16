# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Read effective domain commands as data; never interpret configuration text."""

import argparse
import sys
from collections.abc import Mapping
from pathlib import Path
from typing import Any, NoReturn

import yaml

SUFFIXES = {
    "quality-gates-by-domain": " tests/riser_fatigue/ tests/riser_bom/",
    "full-matrix-sweep": " tests/riser_bom/",
}


class UniqueKeyLoader(yaml.SafeLoader):
    """Reject ambiguous mappings instead of accepting the last duplicate."""

    def construct_mapping(self, node: yaml.MappingNode, deep: bool = False) -> dict[Any, Any]:
        """Construct a mapping only when its keys are unique."""
        keys = set()
        for key_node, _ in node.value:
            key = self.construct_object(key_node, deep=deep)
            try:
                duplicate = key in keys
                keys.add(key)
            except TypeError as exc:
                raise yaml.constructor.ConstructorError("invalid mapping key") from exc
            if duplicate:
                raise yaml.constructor.ConstructorError("duplicate mapping key")
        return super().construct_mapping(node, deep=deep)


def load_gates(path: Path) -> Any:
    """Load safe YAML with duplicate keys rejected at every mapping level."""
    with path.open(encoding="utf-8") as handle:
        return yaml.load(handle, Loader=UniqueKeyLoader)


def effective_command(document: object, gate: str, workflow: str) -> str:
    """Return the configured bytes plus the existing workflow-specific suffix."""
    if workflow not in SUFFIXES or not isinstance(document, Mapping):
        raise ValueError("invalid workflow or document")
    gates = document.get("gates")
    if not isinstance(gates, Mapping) or not isinstance(gates.get(gate), Mapping):
        raise ValueError("invalid gate mapping")
    command = gates[gate].get("command")
    if not isinstance(command, str) or not command.strip():
        raise ValueError("command must be a nonempty string")
    if any(character in command for character in "\n\r\v\f\x1c\x1d\x1e\x85\u2028\u2029\0"):
        raise ValueError("command must occupy one output line")
    return command + (SUFFIXES[workflow] if gate == "tests-subsea" else "")


class CommandParser(argparse.ArgumentParser):
    """Keep invalid argument diagnostics bounded and independent of input."""

    def error(self, message: str) -> NoReturn:
        """Report argument errors through the same command failure boundary."""
        raise ValueError("invalid arguments")


def main(argv: list[str] | None = None) -> int:
    """Print only a command on success, or a bounded diagnostic on failure."""
    parser = CommandParser(description=__doc__)
    parser.add_argument("--gate-file", type=Path, required=True)
    parser.add_argument("--gate", required=True)
    parser.add_argument("--workflow", required=True)
    try:
        args = parser.parse_args(argv)
        command = effective_command(load_gates(args.gate_file), args.gate, args.workflow)
    except (OSError, UnicodeError, yaml.YAMLError, ValueError):
        print("domain gate command: invalid configuration or selection", file=sys.stderr)
        return 1
    print(command)
    return 0


if __name__ == "__main__":
    sys.exit(main())
