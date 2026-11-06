# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Explain advisory family test selection without changing CI or running tests."""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path, PureWindowsPath

if __package__:
    from . import detect_touched_domains as detector
else:
    import detect_touched_domains as detector

ROOT = Path(__file__).resolve().parents[2]


def relative_path(path: str) -> str:
    """Accept normalized repository paths with an optional leading dot slash."""
    if not isinstance(path, str):
        raise ValueError("path must be a string")
    normalized = path.removeprefix("./")
    if (
        not normalized or PureWindowsPath(normalized).drive
        or "\\" in normalized or any(ord(char) < 32 for char in normalized)
        or any(part in {"", ".", ".."} for part in normalized.split("/"))
    ):
        raise ValueError("path must be normalized and repository-relative")
    return normalized


def unique_object(pairs: list[tuple]) -> dict:
    """Reject duplicate inventory keys instead of silently dropping homes."""
    result: dict = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate inventory key")
        result[key] = value
    return result


def validate_family_map(inventory: dict) -> None:
    """Check the version-one inventory fields consumed by this selector."""
    if not isinstance(inventory, dict) or type(inventory.get("schema_version")) is not int or inventory["schema_version"] != 1:
        raise ValueError("expected inventory schema_version 1")
    families = inventory.get("families")
    if (
        not isinstance(families, list) or not families
        or any(not isinstance(name, str) or not name for name in families)
        or len(set(families)) != len(families) or "ENGINE" not in families
    ):
        raise ValueError("expected unique families including ENGINE")
    for field in ("module_to_family", "test_file_to_family"):
        homes = inventory.get(field)
        if not isinstance(homes, dict) or not homes:
            raise ValueError(f"expected nonempty {field}")
        for path, family in homes.items():
            if not isinstance(family, str) or family not in families:
                raise ValueError(f"unknown family in {field}")
            if field == "module_to_family":
                if not isinstance(path, str) or not (
                    path == "digitalmodel" or path.startswith("digitalmodel.")
                ):
                    raise ValueError("invalid module name")
                # The inventory derives names from file paths, including legacy
                # filenames that are not valid import identifiers.
                relative_path(path.replace(".", "/"))
            elif relative_path(path) != path or not path.endswith(".py"):
                raise ValueError("invalid test file path")
    deps = inventory.get("family_to_engine_deps")
    if not isinstance(deps, dict) or set(deps) != set(families) - {"ENGINE"}:
        raise ValueError("expected dependencies for each application family")
    for modules in deps.values():
        if not isinstance(modules, list) or any(
            not isinstance(module, str)
            or inventory["module_to_family"].get(module) != "ENGINE"
            for module in modules
        ):
            raise ValueError("dependency must name a recorded ENGINE module")


def load_family_map(path: Path) -> dict:
    """Read a recorded map without generating, importing or updating anything."""
    inventory = json.loads(path.read_text(encoding="utf-8"), object_pairs_hook=unique_object)
    validate_family_map(inventory)
    return inventory


def module_for_path(path: str) -> str | None:
    """Translate a Python source path, treating package initializers exactly."""
    if not path.startswith("src/digitalmodel/") or not path.endswith(".py"):
        return None
    module = path.removeprefix("src/").removesuffix(".py").replace("/", ".")
    return module.removesuffix(".__init__")


def select_tests(changed_paths: list[str], inventory: dict) -> dict:
    """Union exact homes and measured ENGINE dependents; explain unknown paths."""
    validate_family_map(inventory)
    selected: set[str] = set()
    reasons = []
    unmapped = []
    for path in sorted({relative_path(path) for path in changed_paths}):
        family = inventory["test_file_to_family"].get(path)
        module = module_for_path(path)
        if family is not None:
            names = {family}
            rule = "test-home"
        elif module in inventory["module_to_family"]:
            family = inventory["module_to_family"][module]
            names = {family}
            rule = "module-home"
            if family == "ENGINE":
                names.update(
                    name for name, dependencies in inventory["family_to_engine_deps"].items()
                    if module in dependencies
                )
                rule = "engine-dependents"
        else:
            names = set(inventory["families"])
            rule = "unmapped"
            unmapped.append(path)
        selected.update(names)
        reasons.append({"path": path, "rule": rule, "families": sorted(names)})
    # Exact files are valid pytest roots. Collapsing a mixed directory would
    # silently select other families or include files absent from this map.
    return {
        "families": sorted(selected),
        "test_roots": sorted(
            path for path, family in inventory["test_file_to_family"].items()
            if family in selected
        ),
        "reasons": reasons,
        "unmapped_paths": unmapped,
    }


def validate_revisions(base: str | None, head: str | None) -> None:
    """Prevent revision endpoints from being interpreted as Git options."""
    if bool(base) != bool(head) or any(
        ref and ref.startswith("-") for ref in (base, head)
    ):
        raise ValueError("revision endpoints must be paired and cannot begin with '-'")


def explain(
    changed_paths: list[str], inventory: dict, domains: list[detector.Domain],
    base: str | None = None, head: str | None = None,
) -> dict:
    """Compare with the active detector, including both revisions' record pins."""
    validate_revisions(base, head)
    selection = select_tests(changed_paths, inventory)
    pins: dict[str, set[str]] = {}
    if base and head:
        for ref in dict.fromkeys((base, head)):
            for path, names in detector.git_pinned_domains(ref, domains).items():
                pins.setdefault(path, set()).update(names)
    active = detector.touched_domains(
        [reason["path"] for reason in selection["reasons"]], domains,
        base, head, pinned_domains=pins,
    )
    roots = sorted({root for domain in active for root in domain.roots})
    active_files = {
        path for path in inventory["test_file_to_family"]
        if any(detector.path_matches_root(path, root) for root in roots)
    }
    family_files = set(selection["test_roots"])
    return {
        "advisory": True,
        **selection,
        "domain_comparison": {
            "domains": [domain.name for domain in active],
            "test_roots": roots,
            "family_only_test_files": sorted(family_files - active_files),
            "domain_only_test_files": sorted(active_files - family_files),
            "basis": "Recorded test files only; collection parity is not established.",
        },
    }


def main(argv: list[str] | None = None) -> int:
    """Print a JSON dry run for revision endpoints or explicit changed paths."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--families-file", type=Path, default=ROOT / "config/ci/module-families.json")
    parser.add_argument("--domains-file", type=Path, default=ROOT / "tests/DOMAINS.md")
    parser.add_argument("--base", help="Base revision endpoint (git diff, not merge-base).")
    parser.add_argument("--head", help="Head revision endpoint.")
    parser.add_argument("--changed-path", action="append", dest="paths", help="Repository-relative path; repeat for multiple paths.")
    args = parser.parse_args(argv)
    if bool(args.base) != bool(args.head) or bool(args.paths) == bool(args.base):
        parser.error("provide --base and --head, or one or more --changed-path arguments")
    try:
        validate_revisions(args.base, args.head)
    except ValueError as error:
        parser.error(str(error))
    try:
        inventory = load_family_map(args.families_file)
    except (OSError, ValueError):
        print("family inventory unavailable or invalid; supply --families-file", file=sys.stderr)
        return 2
    try:
        changed = args.paths if args.paths is not None else detector.git_changed_files(args.base, args.head)
        report = explain(changed, inventory, detector.parse_domains(args.domains_file), args.base, args.head)
    except (OSError, ValueError, subprocess.CalledProcessError):
        print("could not read diff or compare domain selection", file=sys.stderr)
        return 2
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
