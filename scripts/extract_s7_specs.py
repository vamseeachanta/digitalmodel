#!/usr/bin/env python
"""Batch spec.yml extraction from monolithic OrcaFlex YAML directories.

Walks all ``monolithic/`` directories under ``docs/modules/orcaflex/`` and
extracts spec.yml files using the existing MonolithicExtractor.  Each
extracted spec is validated against the ProjectInputSpec schema and saved
as a sibling to the ``monolithic/`` directory.

Usage::

    uv run python scripts/extract_s7_specs.py
    uv run python scripts/extract_s7_specs.py --dry-run
    uv run python scripts/extract_s7_specs.py --force
    uv run python scripts/extract_s7_specs.py --docs-root docs/modules/orcaflex
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path
from typing import Any

import yaml
from pydantic import ValidationError

from digitalmodel.solvers.orcaflex.modular_generator.extractor import (
    MonolithicExtractor,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import (
    ProjectInputSpec,
)

logger = logging.getLogger(__name__)

# Structure categories inferred from ancestor directory names.
_STRUCTURE_KEYWORDS: dict[str, str] = {
    "jumper": "jumper",
    "installation": "installation",
    "mooring": "mooring",
    "training": "training",
    "regional": "regional",
    "vessel_raos": "vessel_raos",
    "reference": "reference",
}


def infer_structure(path: Path) -> str:
    """Infer a structure category from the directory path.

    Scans ancestor directory names for known keywords and returns the
    first match.  Falls back to ``"generic"`` when no keyword matches.

    Args:
        path: Any path whose ancestor names may contain category hints.

    Returns:
        A short structure label such as ``"jumper"`` or ``"generic"``.
    """
    parts = path.parts
    for part in parts:
        lower_part = part.lower()
        for keyword, category in _STRUCTURE_KEYWORDS.items():
            if lower_part == keyword:
                return category
    return "generic"


def find_monolithic_dirs(docs_root: Path) -> list[Path]:
    """Recursively find all ``monolithic/`` directories under *docs_root*.

    Args:
        docs_root: The root directory to search (typically
            ``docs/modules/orcaflex``).

    Returns:
        Sorted list of absolute paths to ``monolithic/`` directories that
        contain at least one ``.yml`` file.
    """
    monolithic_dirs: list[Path] = []
    for mono_dir in sorted(docs_root.rglob("monolithic")):
        if not mono_dir.is_dir():
            continue
        yml_files = sorted(mono_dir.glob("*.yml"))
        if yml_files:
            monolithic_dirs.append(mono_dir)
    return monolithic_dirs


def pick_representative_yml(mono_dir: Path) -> Path | None:
    """Select the first ``.yml`` file (sorted alphabetically) from *mono_dir*.

    For multi-file directories (e.g. installation steps) the first file
    alphabetically is used as the base spec.

    Args:
        mono_dir: Path to a ``monolithic/`` directory.

    Returns:
        Path to the chosen ``.yml`` file, or ``None`` if none found.
    """
    yml_files = sorted(mono_dir.glob("*.yml"))
    if not yml_files:
        return None
    return yml_files[0]


def extract_spec(yml_path: Path) -> dict[str, Any]:
    """Run MonolithicExtractor on *yml_path* and return the spec dict.

    Args:
        yml_path: Path to a monolithic OrcaFlex ``.yml`` file.

    Returns:
        Dictionary compatible with ``ProjectInputSpec``.

    Raises:
        Exception: Propagates any error from the extractor or YAML parser.
    """
    extractor = MonolithicExtractor(yml_path)
    return extractor.extract()


def override_metadata(
    spec_dict: dict[str, Any],
    parent_dir: Path,
) -> None:
    """Override metadata fields in-place with library conventions.

    Sets:
    - ``metadata.project`` to ``"Reference Library"``
    - ``metadata.structure`` inferred from the parent path
    - ``metadata.name`` to the parent directory name

    Args:
        spec_dict: The extracted spec dictionary (modified in-place).
        parent_dir: The directory that contains ``monolithic/`` (one
            level up).
    """
    meta = spec_dict.setdefault("metadata", {})
    meta["project"] = "Reference Library"
    meta["structure"] = infer_structure(parent_dir)
    meta["name"] = parent_dir.name


def validate_spec(spec_dict: dict[str, Any]) -> ProjectInputSpec | None:
    """Validate *spec_dict* against the ProjectInputSpec schema.

    Args:
        spec_dict: Dictionary to validate.

    Returns:
        The validated ``ProjectInputSpec`` instance, or ``None`` if
        validation fails (error is logged).
    """
    try:
        return ProjectInputSpec(**spec_dict)
    except ValidationError as exc:
        logger.warning("Schema validation failed: %s", exc)
        return None


def save_spec(spec_dict: dict[str, Any], output_path: Path) -> None:
    """Write *spec_dict* to *output_path* as YAML.

    Args:
        spec_dict: The spec dictionary to serialize.
        output_path: Destination file path.
    """
    with open(output_path, "w", encoding="utf-8") as fh:
        yaml.dump(
            spec_dict,
            fh,
            default_flow_style=False,
            sort_keys=False,
            allow_unicode=True,
        )


def process_monolithic_dir(
    mono_dir: Path,
    *,
    dry_run: bool = False,
    force: bool = False,
) -> str:
    """Process a single ``monolithic/`` directory.

    Args:
        mono_dir: Path to the ``monolithic/`` directory.
        dry_run: If ``True``, report what would happen but do not write.
        force: If ``True``, overwrite an existing ``spec.yml``.

    Returns:
        One of ``"extracted"``, ``"skipped"``, or ``"failed"`` indicating
        the outcome.
    """
    parent_dir = mono_dir.parent
    output_path = parent_dir / "spec.yml"

    # Skip if spec.yml already exists and force is not set.
    if output_path.exists() and not force:
        logger.info(
            "SKIP  %s (spec.yml exists, use --force to overwrite)",
            parent_dir.name,
        )
        return "skipped"

    # Pick representative YAML file.
    yml_path = pick_representative_yml(mono_dir)
    if yml_path is None:
        logger.warning("FAIL  %s (no .yml files in monolithic/)", parent_dir.name)
        return "failed"

    yml_count = len(list(mono_dir.glob("*.yml")))
    multi_note = f" ({yml_count} yml files, using first)" if yml_count > 1 else ""

    if dry_run:
        logger.info(
            "DRY   %s -> %s%s",
            yml_path.name,
            output_path.relative_to(output_path.parent.parent.parent),
            multi_note,
        )
        return "extracted"

    # Extract spec from monolithic YAML.
    try:
        spec_dict = extract_spec(yml_path)
    except Exception as exc:
        logger.error("FAIL  %s: extraction error: %s", parent_dir.name, exc)
        return "failed"

    # Override metadata with library conventions.
    override_metadata(spec_dict, parent_dir)

    # Validate against ProjectInputSpec.
    validated = validate_spec(spec_dict)
    if validated is None:
        logger.warning(
            "WARN  %s: validation failed, saving raw dict anyway",
            parent_dir.name,
        )

    # Save spec.yml.
    try:
        save_spec(spec_dict, output_path)
    except Exception as exc:
        logger.error("FAIL  %s: save error: %s", parent_dir.name, exc)
        return "failed"

    status = "OK   " if validated is not None else "WARN "
    logger.info(
        "%s %s -> spec.yml%s",
        status,
        yml_path.name,
        multi_note,
    )
    return "extracted"


def resolve_docs_root(cli_value: str | None) -> Path:
    """Resolve the docs root directory.

    If *cli_value* is provided, use it directly.  Otherwise auto-detect
    relative to the script location (assumes the script is at
    ``<repo>/scripts/extract_s7_specs.py``).

    Args:
        cli_value: Explicit path from ``--docs-root``, or ``None``.

    Returns:
        Absolute path to the ``docs/modules/orcaflex`` directory.

    Raises:
        SystemExit: If the resolved path does not exist.
    """
    if cli_value is not None:
        root = Path(cli_value)
    else:
        repo_root = Path(__file__).resolve().parent.parent
        root = repo_root / "docs" / "modules" / "orcaflex"

    root = root.resolve()
    if not root.is_dir():
        logger.error("Docs root not found: %s", root)
        sys.exit(1)
    return root


def build_parser() -> argparse.ArgumentParser:
    """Build the CLI argument parser."""
    parser = argparse.ArgumentParser(
        description=(
            "Batch-extract spec.yml files from monolithic OrcaFlex YAML "
            "directories."
        ),
    )
    parser.add_argument(
        "--docs-root",
        default=None,
        help=(
            "Path to docs/modules/orcaflex/ directory. "
            "Default: auto-detect from script location."
        ),
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be extracted without writing files.",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Overwrite existing spec.yml files (default: skip if exists).",
    )
    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Enable debug-level logging.",
    )
    return parser


def main() -> int:
    """Entry point for the batch extraction script.

    Returns:
        Exit code: 0 on success, 1 if any extraction failed.
    """
    parser = build_parser()
    args = parser.parse_args()

    # Configure logging.
    log_level = logging.DEBUG if args.verbose else logging.INFO
    logging.basicConfig(
        level=log_level,
        format="%(message)s",
        stream=sys.stdout,
    )

    docs_root = resolve_docs_root(args.docs_root)
    logger.info("Docs root: %s", docs_root)
    if args.dry_run:
        logger.info("Mode: DRY RUN (no files will be written)\n")
    elif args.force:
        logger.info("Mode: FORCE (existing spec.yml will be overwritten)\n")
    else:
        logger.info("Mode: default (skip existing spec.yml)\n")

    # Find all monolithic directories.
    mono_dirs = find_monolithic_dirs(docs_root)
    logger.info("Found %d monolithic directories with .yml files\n", len(mono_dirs))

    if not mono_dirs:
        logger.info("Nothing to do.")
        return 0

    # Process each monolithic directory.
    extracted = 0
    skipped = 0
    failed = 0
    failed_names: list[str] = []

    for mono_dir in mono_dirs:
        result = process_monolithic_dir(
            mono_dir,
            dry_run=args.dry_run,
            force=args.force,
        )
        if result == "extracted":
            extracted += 1
        elif result == "skipped":
            skipped += 1
        elif result == "failed":
            failed += 1
            failed_names.append(mono_dir.parent.name)

    # Print summary.
    logger.info("")
    logger.info("=" * 50)
    logger.info("SUMMARY")
    logger.info("=" * 50)
    logger.info("  Monolithic dirs found : %d", len(mono_dirs))
    logger.info("  Specs extracted       : %d", extracted)
    logger.info("  Specs skipped         : %d", skipped)
    logger.info("  Specs failed          : %d", failed)
    if failed_names:
        logger.info("")
        logger.info("Failed models:")
        for name in failed_names:
            logger.info("  - %s", name)

    return 1 if failed > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
