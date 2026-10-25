# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check that YAML formatting preserves parsed values and unique mapping keys."""

import argparse
from pathlib import Path

import yaml


class _UniqueKeyLoader(yaml.SafeLoader):
    pass


def _mapping(loader: yaml.SafeLoader, node: yaml.MappingNode) -> dict[object, object]:
    result: dict[object, object] = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node)
        if key in result:
            raise ValueError("duplicate YAML key")
        result[key] = loader.construct_object(value_node)
    return result


_UniqueKeyLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _mapping)


def main() -> None:
    """Compare YAML inputs without rewriting either file."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("before", type=Path)
    parser.add_argument("after", type=Path)
    args = parser.parse_args()
    try:
        before = yaml.load(args.before.read_text(encoding="utf-8"), Loader=_UniqueKeyLoader)
        after = yaml.load(args.after.read_text(encoding="utf-8"), Loader=_UniqueKeyLoader)
    except (OSError, UnicodeError, ValueError, TypeError, yaml.YAMLError):
        parser.exit(1, "YAML input is invalid or unreadable\n")
    if before != after:
        parser.exit(1, "YAML values differ\n")
    print("YAML parsed values are unchanged")


if __name__ == "__main__":
    main()
