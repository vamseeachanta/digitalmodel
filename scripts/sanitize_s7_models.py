#!/usr/bin/env python
"""Sanitize and organize OrcaFlex models from rock-oil-field/s7/ into the
digitalmodel library.

Walks the s7 directory, deduplicates by content hash, applies client-name
sanitization, strips metadata headers, and writes sanitized .yml files into
an organized category-based folder structure under docs/modules/orcaflex/.

Usage:
    uv run python scripts/sanitize_s7_models.py
    uv run python scripts/sanitize_s7_models.py --dry-run
    uv run python scripts/sanitize_s7_models.py --skip-dat
    uv run python scripts/sanitize_s7_models.py --s7-root /path/to/s7 --output-root /path/to/out
"""
from __future__ import annotations

import argparse
import hashlib
import json
import logging
import re
import sys
import time
from dataclasses import asdict, dataclass, field
from pathlib import Path

try:
    import OrcFxAPI

    HAS_ORCFX = True
except ImportError:
    HAS_ORCFX = False

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEFAULT_S7_ROOT = Path(r"D:\workspace-hub\rock-oil-field\s7")
DEFAULT_OUTPUT_ROOT = Path("docs/modules/orcaflex")

# Longest-first ordering is enforced at runtime; this dict is the canonical
# source of all sanitization replacements.
SANITIZATION_MAP: dict[str, str] = {
    # Client project names
    "Ballymore": "deepwater_field_a",
    "ballymore": "deepwater_field_a",
    # Vessel names
    "Candies": "installation_vessel_01",
    "WyatteCandies": "iv01_type",
    "WyattCandies": "iv01_type",
    "Seven Arctic": "installation_vessel_02",
    "7arctic": "region_a",
    "7seas": "region_b",
    "Seven Seas": "region_b",
    "SevenSeas": "region_b",
    # Project names
    "Shell Perdido": "deepwater_field_b",
    "Perdido South": "deepwater_field_b",
    "shell_perdido_south": "deepwater_field_b",
    "Perdido": "deepwater_field_b",
    "Talos Venice": "deepwater_field_c",
    "talos_venice": "deepwater_field_c",
    "BP_MD2_FJR": "regional_design_01",
    "BP_MD2": "regional_design_01",
    "ONGC": "operator_a",
    "Limerock": "pipeline_route_a",
    "limerock": "pipeline_route_a",
    # User/machine identifiers (remove entirely)
    "SS7A2365": "",
    "USNGL23LR3": "",
    "ss7a2365": "",
}

# Pre-sorted by key length descending to avoid partial-match corruption.
_SORTED_SANITIZATION_PAIRS: list[tuple[str, str]] = sorted(
    SANITIZATION_MAP.items(), key=lambda kv: len(kv[0]), reverse=True
)

# Source-to-target category mapping.  Keys are path fragments relative to
# s7-root using forward slashes.
CATEGORY_MAP: dict[str, str] = {
    # ballymore jumper models
    "ballymore/Jumper_Manifold to PLET": "jumper/manifold_to_plet",
    "ballymore/Jumper_PLET to PLEM": "jumper/plet_to_plem",
    "ballymore/sut_mm/01-SZ-Xdeg": "jumper/sut_mm",
    "ballymore/sut_mm/02-DZ-30deg_no_AHC": "jumper/sut_mm",
    "ballymore/sut_mm/03-DZ-00deg_AHC": "jumper/sut_mm",
    "ballymore/sut_mm/04-RES": "jumper/sut_mm/resonance",
    # installation
    "shell_perdido_south/anl/manifold": "installation/manifold/region_a",
    "shell_perdido_south/mudmat_tool/rev2": "installation/mudmat",
    "shell_perdido_south/mudmat_tool/rev3": "installation/mudmat",
    "talos_venice/infield": "installation/pipeline/route_a",
    "talos_venice/limerock": "installation/pipeline/route_b",
    # mooring
    "analysis_general/moorings": "mooring/reference",
    "analysis_general/_ref/mooring": "mooring/reference",
    # training
    "analysis_general/train/CraneMaster": "training/crane_master",
    "analysis_general/train/hulls": "training/hulls",
    "analysis_general/train/Node feeding": "training/node_feeding",
    "analysis_general/train/jumper": "training/jumper_training",
    "analysis_general/train/s-lay": "training/s_lay",
    "analysis_general/train/structure": "training/structure",
    "analysis_general/train/Riser Design": "training/riser_design",
    "analysis_general/train/SCR Design April 2015": "training/scr_design",
    "analysis_general/train/Reeling": "training/reeling",
    "analysis_general/train/Pipeline Design Workshop": "training/pipeline_design",
    # RAOs
    "ballymore/WyattCandies-Case1-2-seastateRAOs": "vessel_raos/iv01",
    "talos_venice/raos": "vessel_raos/route_a",
    # regional / general
    "BP_MD2_FJR": "regional",
    "analysis_general/_ref": "reference",
    "analysis_general/umbilical": "installation/umbilical",
    # Catch-all for unmapped subdirectories
    "ballymore/sut_mm/qa": "jumper/sut_mm",
    "ballymore/sut_mm/data": "jumper/sut_mm",
    "ballymore/sut_mm": "jumper/sut_mm",
    "analysis_general": "reference",
    "shell_perdido_south/anl": "installation/manifold/region_a",
}

# Pre-sorted by specificity (longest prefix first) so the most specific
# category wins during lookup.
_SORTED_CATEGORY_PAIRS: list[tuple[str, str]] = sorted(
    CATEGORY_MAP.items(), key=lambda kv: len(kv[0]), reverse=True
)

# Directories to skip entirely (relative to s7-root, forward slashes).
EXCLUSIONS: list[str] = [
    "shell_perdido_south/mudmat_tool/rev1",
    "ballymore/sut_mm/mudmat_tool",
    "ballymore/sut_mm/Ballymore Graphics",
    "ballymore/weather",
    "analysis_general/ssRAOs_toolkit",
    "analysis_general/cad",
    "analysis_general/py",
    "analysis_general/orcina",
    "analysis_general/eca",
    "analysis_general/f101",
    "analysis_general/flow_assurance",
    "analysis_general/geotech",
    "analysis_general/foundations",
    "analysis_general/lifting",
    "analysis_general/eng_process",
    "analysis_general/pipelines",
    "analysis_general/i7",
    "md2",
    "shell_perdido_south/data",
    "shell_perdido_south/_ref",
    "shell_perdido_south/anl/go_by",
    "talos_venice/report",
    "talos_venice/stormgeo",
    "talos_venice/infield/data",
    "talos_venice/infield/report",
    "talos_venice/infield/calculatedpositions",
    "talos_venice/infield/calculatedpositions_qa",
    "talos_venice/infield/uth",
]

# YAML metadata lines that OrcFxAPI.SaveData() embeds at file top.
_HEADER_LINE_RE = re.compile(r"^(User|Machine|File)\s*:.*$", re.MULTILINE)

# Heuristic: OrcaFlex YAML files contain a `---` separator or known
# section names.
_ORCAFLEX_SECTION_NAMES = {
    "General",
    "Environment",
    "LineTypes",
    "Lines",
    "Vessels",
    "VesselTypes",
    "3DBuoys",
    "6DBuoys",
    "Links",
    "Shapes",
    "Constraints",
    "WaveTrains",
    "DiffractionData",
    "ClumpTypes",
    "Groups",
    "Winches",
}

logger = logging.getLogger("sanitize_s7")


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class AuditEntry:
    source_path: str
    target_path: str
    sha256: str
    file_size_bytes: int
    transformations: list[str] = field(default_factory=list)
    category: str = ""
    status: str = "ok"
    error: str = ""


@dataclass
class RunStats:
    total_files_found: int = 0
    yml_processed: int = 0
    dat_processed: int = 0
    dupes_skipped: int = 0
    excluded_skipped: int = 0
    non_orcaflex_skipped: int = 0
    errors: int = 0
    categories_used: set[str] = field(default_factory=set)
    start_time: float = 0.0
    end_time: float = 0.0


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def sha256_of_file(path: Path) -> str:
    """Return hex digest of file contents."""
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 16), b""):
            h.update(chunk)
    return h.hexdigest()


def is_excluded(rel_path: str) -> bool:
    """Check whether *rel_path* (forward-slash, relative to s7-root) falls
    under any excluded directory prefix."""
    rel_lower = rel_path.lower()
    for exc in EXCLUSIONS:
        exc_lower = exc.lower().replace("\\", "/")
        if rel_lower.startswith(exc_lower + "/") or rel_lower == exc_lower:
            return True
    return False


def resolve_category(rel_path: str) -> str | None:
    """Return the target category for a file's relative directory, or None
    if no mapping matches."""
    rel_norm = rel_path.replace("\\", "/")
    for prefix, category in _SORTED_CATEGORY_PAIRS:
        prefix_norm = prefix.replace("\\", "/")
        if rel_norm.startswith(prefix_norm + "/") or rel_norm == prefix_norm:
            return category
    return None


def looks_like_orcaflex_yml(path: Path) -> bool:
    """Heuristic check: does this .yml file appear to be an OrcaFlex model?"""
    try:
        with open(path, "r", encoding="latin-1", errors="replace") as f:
            head = f.read(8192)
    except OSError:
        return False

    if "---" in head:
        return True

    for section_name in _ORCAFLEX_SECTION_NAMES:
        if re.search(rf"^{section_name}\s*:", head, re.MULTILINE):
            return True

    return False


def sanitize_text(text: str) -> tuple[str, list[str]]:
    """Apply all sanitization replacements and strip metadata headers.

    Returns (sanitized_text, list_of_transformation_descriptions).
    """
    transformations: list[str] = []

    # Apply replacements longest-first.
    for old, new in _SORTED_SANITIZATION_PAIRS:
        if old in text:
            count = text.count(old)
            if new:
                text = text.replace(old, new)
                transformations.append(f"replaced '{old}' -> '{new}' ({count}x)")
            else:
                text = text.replace(old, "")
                transformations.append(f"removed '{old}' ({count}x)")

    # Strip header metadata lines.
    stripped, n = _HEADER_LINE_RE.subn("", text)
    if n:
        transformations.append(f"removed {n} metadata header line(s)")
        text = stripped

    # Collapse any resulting blank-line runs at the very start of file.
    text = text.lstrip("\n")

    return text, transformations


def sanitize_object_names(model: "OrcFxAPI.Model") -> list[str]:
    """Rename objects inside a loaded OrcFxAPI model using the sanitization
    map.  Returns list of transformation descriptions."""
    transformations: list[str] = []

    for obj in model.objects:
        original_name = obj.Name
        new_name = original_name
        for old, new in _SORTED_SANITIZATION_PAIRS:
            if old in new_name:
                new_name = new_name.replace(old, new)
        if new_name != original_name:
            obj.Name = new_name
            transformations.append(
                f"object renamed: '{original_name}' -> '{new_name}'"
            )

    return transformations


def target_yml_path(
    output_root: Path, category: str, source_name: str
) -> Path:
    """Build the target path:
    <output_root>/<category>/monolithic/<sanitized_filename>.yml
    """
    # Sanitize the filename itself.
    sanitized_name = source_name
    for old, new in _SORTED_SANITIZATION_PAIRS:
        if old in sanitized_name:
            sanitized_name = sanitized_name.replace(old, new)

    # Ensure .yml extension.
    stem = Path(sanitized_name).stem
    return output_root / category / "monolithic" / f"{stem}.yml"


# ---------------------------------------------------------------------------
# Processing
# ---------------------------------------------------------------------------

def process_yml_file(
    source: Path,
    output_root: Path,
    category: str,
    dry_run: bool,
) -> AuditEntry:
    """Text-based sanitization of a .yml file."""
    file_hash = sha256_of_file(source)
    file_size = source.stat().st_size
    target = target_yml_path(output_root, category, source.name)

    entry = AuditEntry(
        source_path=str(source),
        target_path=str(target),
        sha256=file_hash,
        file_size_bytes=file_size,
        category=category,
    )

    try:
        with open(source, "r", encoding="latin-1", errors="replace") as f:
            raw_text = f.read()

        sanitized, transforms = sanitize_text(raw_text)
        entry.transformations = transforms

        if not dry_run:
            target.parent.mkdir(parents=True, exist_ok=True)
            with open(target, "w", encoding="latin-1", newline="") as f:
                f.write(sanitized)

        entry.status = "ok"
    except Exception as exc:
        entry.status = "error"
        entry.error = str(exc)
        logger.error("Error processing %s: %s", source, exc)

    return entry


def process_dat_file(
    source: Path,
    output_root: Path,
    category: str,
    dry_run: bool,
) -> AuditEntry:
    """Load .dat via OrcFxAPI, sanitize object names, save as .yml, then
    apply text-based sanitization on the result."""
    file_hash = sha256_of_file(source)
    file_size = source.stat().st_size
    target = target_yml_path(output_root, category, source.name)

    entry = AuditEntry(
        source_path=str(source),
        target_path=str(target),
        sha256=file_hash,
        file_size_bytes=file_size,
        category=category,
    )

    if not HAS_ORCFX:
        entry.status = "skipped"
        entry.error = "OrcFxAPI not installed"
        return entry

    try:
        model = OrcFxAPI.Model(str(source))
        obj_transforms = sanitize_object_names(model)
        entry.transformations.extend(obj_transforms)

        if not dry_run:
            target.parent.mkdir(parents=True, exist_ok=True)
            model.SaveData(str(target))

            # Second pass: text-based sanitization on the saved .yml to
            # catch embedded strings that object-rename did not cover.
            with open(target, "r", encoding="latin-1", errors="replace") as f:
                raw_text = f.read()

            sanitized, text_transforms = sanitize_text(raw_text)
            entry.transformations.extend(text_transforms)

            with open(target, "w", encoding="latin-1", newline="") as f:
                f.write(sanitized)

        entry.status = "ok"
    except Exception as exc:
        entry.status = "error"
        entry.error = str(exc)
        logger.error("Error processing %s: %s", source, exc)

    return entry


# ---------------------------------------------------------------------------
# Discovery
# ---------------------------------------------------------------------------

def discover_model_files(
    s7_root: Path,
) -> tuple[list[Path], list[Path], int]:
    """Walk s7_root and return (yml_files, dat_files, excluded_count).

    Applies exclusion filtering and the OrcaFlex-heuristic check for .yml
    files.
    """
    yml_files: list[Path] = []
    dat_files: list[Path] = []
    excluded_count = 0

    for path in sorted(s7_root.rglob("*")):
        if not path.is_file():
            continue
        suffix = path.suffix.lower()
        if suffix not in (".dat", ".yml"):
            continue

        rel = path.relative_to(s7_root).as_posix()

        if is_excluded(rel):
            excluded_count += 1
            continue

        if suffix == ".yml":
            if not looks_like_orcaflex_yml(path):
                excluded_count += 1
                continue
            yml_files.append(path)
        else:
            dat_files.append(path)

    return yml_files, dat_files, excluded_count


# ---------------------------------------------------------------------------
# Main orchestration
# ---------------------------------------------------------------------------

def run(args: argparse.Namespace) -> int:
    """Main entry point.  Returns exit code (0 = success)."""
    s7_root = Path(args.s7_root).resolve()
    output_root = Path(args.output_root).resolve()
    dry_run: bool = args.dry_run
    skip_dat: bool = args.skip_dat

    if not s7_root.is_dir():
        logger.error("S7 root directory does not exist: %s", s7_root)
        return 1

    stats = RunStats(start_time=time.time())
    audit_log: list[AuditEntry] = []
    seen_hashes: set[str] = set()

    logger.info("Discovering model files in %s ...", s7_root)
    yml_files, dat_files, excluded_count = discover_model_files(s7_root)
    stats.excluded_skipped = excluded_count
    stats.total_files_found = len(yml_files) + len(dat_files)

    logger.info(
        "Found %d .yml and %d .dat files (%d excluded)",
        len(yml_files),
        len(dat_files),
        excluded_count,
    )

    # --- Phase 1: .yml files (text-based sanitization) ---
    logger.info("--- Phase 1: Sanitizing .yml files ---")
    for source in yml_files:
        file_hash = sha256_of_file(source)
        if file_hash in seen_hashes:
            stats.dupes_skipped += 1
            logger.debug("Duplicate skipped: %s", source)
            continue
        seen_hashes.add(file_hash)

        rel_dir = source.parent.relative_to(s7_root).as_posix()
        category = resolve_category(rel_dir)
        if category is None:
            category = "uncategorized"
            logger.warning("No category mapping for: %s", rel_dir)

        stats.categories_used.add(category)
        entry = process_yml_file(source, output_root, category, dry_run)
        audit_log.append(entry)

        if entry.status == "error":
            stats.errors += 1
        else:
            stats.yml_processed += 1

    # --- Phase 2: .dat files (OrcFxAPI conversion + text sanitization) ---
    if skip_dat:
        logger.info("--- Phase 2: Skipped (.dat conversion disabled) ---")
    elif not HAS_ORCFX:
        logger.warning(
            "--- Phase 2: Skipped (OrcFxAPI not installed) ---"
        )
    else:
        logger.info("--- Phase 2: Converting .dat files via OrcFxAPI ---")
        for source in dat_files:
            file_hash = sha256_of_file(source)
            if file_hash in seen_hashes:
                stats.dupes_skipped += 1
                logger.debug("Duplicate skipped: %s", source)
                continue
            seen_hashes.add(file_hash)

            rel_dir = source.parent.relative_to(s7_root).as_posix()
            category = resolve_category(rel_dir)
            if category is None:
                category = "uncategorized"
                logger.warning("No category mapping for: %s", rel_dir)

            stats.categories_used.add(category)
            entry = process_dat_file(source, output_root, category, dry_run)
            audit_log.append(entry)

            if entry.status == "error":
                stats.errors += 1
            elif entry.status == "skipped":
                pass
            else:
                stats.dat_processed += 1

    stats.end_time = time.time()

    # --- Write audit log ---
    audit_path = output_root / "sanitization_audit.json"
    audit_data = {
        "summary": {
            "total_files_found": stats.total_files_found,
            "yml_processed": stats.yml_processed,
            "dat_processed": stats.dat_processed,
            "dupes_skipped": stats.dupes_skipped,
            "excluded_skipped": stats.excluded_skipped,
            "non_orcaflex_skipped": stats.non_orcaflex_skipped,
            "errors": stats.errors,
            "categories_used": sorted(stats.categories_used),
            "elapsed_seconds": round(stats.end_time - stats.start_time, 2),
            "dry_run": dry_run,
            "orcfx_available": HAS_ORCFX,
        },
        "entries": [asdict(e) for e in audit_log],
    }

    if not dry_run:
        audit_path.parent.mkdir(parents=True, exist_ok=True)
        with open(audit_path, "w", encoding="utf-8") as f:
            json.dump(audit_data, f, indent=2)
        logger.info("Audit log written to %s", audit_path)
    else:
        logger.info("DRY RUN: audit log not written (would be %s)", audit_path)

    # --- Print summary ---
    _print_summary(stats, dry_run)

    return 0 if stats.errors == 0 else 1


def _print_summary(stats: RunStats, dry_run: bool) -> None:
    elapsed = stats.end_time - stats.start_time
    prefix = "[DRY RUN] " if dry_run else ""
    print(f"\n{'=' * 60}")
    print(f"{prefix}Sanitization Summary")
    print(f"{'=' * 60}")
    print(f"  Total model files found : {stats.total_files_found}")
    print(f"  .yml files processed    : {stats.yml_processed}")
    print(f"  .dat files processed    : {stats.dat_processed}")
    print(f"  Duplicates skipped      : {stats.dupes_skipped}")
    print(f"  Excluded dirs skipped   : {stats.excluded_skipped}")
    print(f"  Non-OrcaFlex .yml skip  : {stats.non_orcaflex_skipped}")
    print(f"  Errors                  : {stats.errors}")
    print(f"  Categories used         : {len(stats.categories_used)}")
    for cat in sorted(stats.categories_used):
        print(f"    - {cat}")
    print(f"  Elapsed time            : {elapsed:.1f}s")
    print(f"  OrcFxAPI available      : {HAS_ORCFX}")
    print(f"{'=' * 60}\n")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Sanitize and organize OrcaFlex models from s7/ into "
        "the digitalmodel library.",
    )
    parser.add_argument(
        "--s7-root",
        type=str,
        default=str(DEFAULT_S7_ROOT),
        help="Path to the rock-oil-field/s7/ directory "
        f"(default: {DEFAULT_S7_ROOT})",
    )
    parser.add_argument(
        "--output-root",
        type=str,
        default=str(DEFAULT_OUTPUT_ROOT),
        help="Output directory for sanitized models "
        f"(default: {DEFAULT_OUTPUT_ROOT})",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Discover and log transformations without writing files",
    )
    parser.add_argument(
        "--skip-dat",
        action="store_true",
        help="Skip .dat file conversion (useful without OrcFxAPI)",
    )
    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Enable debug logging",
    )
    return parser.parse_args(argv)


if __name__ == "__main__":
    args = parse_args()

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)-8s %(message)s",
        datefmt="%H:%M:%S",
    )

    sys.exit(run(args))
