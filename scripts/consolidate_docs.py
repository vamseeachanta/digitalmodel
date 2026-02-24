#!/usr/bin/env python3
"""Consolidate docs/eng/ into docs/modules/.

Migrates all files from the legacy docs/eng/ directory structure into
docs/modules/ using explicit directory mappings and pattern-based routing
for ref/ and root-level files.

Usage:
    python scripts/consolidate_docs.py --dry-run   # Preview changes
    python scripts/consolidate_docs.py              # Execute migration
"""

import argparse
import logging
import re
import shutil
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Directory mappings: eng/<key> -> modules/<value>
# ---------------------------------------------------------------------------
DIR_MAP = {
    # Overlapping (merge into existing)
    "cathodic_protection": "cathodic_protection",
    "metocean": "metocean",
    "pipelines": "pipelines",
    "reservoir": "reservoir",
    "signal_processing": "signal_processing",
    "structural": "structural",
    "subsea": "subsea",
    "viv": "viv",
    "wind": "wind",
    # Non-overlapping
    "abaqus": "ansys",
    "accidents": "standards/accidents",
    "animation": "visualization",
    "casing_design": "drilling/casing_design",
    "co2_storage": "production",
    "connectors": "subsea/connectors",
    "data_driven": "data_systems",
    "decomm": "offshore_installation",
    "energy": "references",
    "environment": "metocean",
    "fea": "fem",
    "ffs": "structural/ffs",
    "field_development": "references/field_development",
    "fluid_machanics": "hydrodynamics",
    "geotech": "references/geotech",
    "geothermal": "references/geothermal",
    "hpht": "drilling/hpht",
    "hydrogen": "references/hydrogen",
    "interfaces": "structural",
    "intervention": "interventions",
    "manufacturing": "references/manufacturing",
    "materials": "structural/materials",
    "moorings": "mooring",
    "navigation": "marine_ops",
    "offshore_installation": "offshore_installation",
    "process": "references/process",
    "rigid_jumper": "risers",
    "simulation": "references",
    "trees": "subsea",
    "umbilicals": "umbilical",
}

# ---------------------------------------------------------------------------
# Pattern-based routing for docs/eng/ref/ files (first match wins)
# ---------------------------------------------------------------------------
REF_PATTERNS = [
    # Drilling, well construction, MWD, casing
    (
        r"(?i)(drill|well|MWD|casing|cement|mud|liner|directional|ISCWSA"
        r"|overburden|pore|fracture|wellbore|wellpath|tortuosity|collision"
        r"|multi.?well|ranging|geoscience|bit)",
        "drilling/references",
    ),
    # Wind, FOWT, turbine
    (r"(?i)(wind|fowt|fwjip|turbine)", "wind/references"),
    # Structural, FFS, FAD, buckling, FEA, Schmidt
    (
        r"(?i)(structural|ffs|fad|buckling|FEA|schmidt|nordlock|bolt|lug)",
        "structural/references",
    ),
    # Standards (API, DNV, BS, BSEE, Macondo, BOEM)
    (r"(?i)(API5|API579|BS7910|DNV|BSEE|Macondo|BOEM|OOC|JSCE)", "standards"),
    # Fatigue, ECA, GoM fatigue
    (
        r"(?i)(fatigue|ECA|flaw|crackwise|FlawCheck)",
        "fatigue/references",
    ),
    # Reservoir, petrophysics, well testing
    (
        r"(?i)(reservoir|petrophysic|net.?pay|SPE-|WPC-"
        r"|production.?casing|completions|stimulation)",
        "reservoir/references",
    ),
    # Data engineering, SQL, analytics
    (
        r"(?i)(data|SQL|analytics|ISA|SCADA|workflow|algorithm)",
        "data_systems/references",
    ),
    # Marine ops, vessels, tankers, positioning, sloshing
    (
        r"(?i)(vessel|tanker|euronav|dynamic.?positioning|sloshing"
        r"|offshore.*wind.*bourbon|lifting|SD-1)",
        "marine_ops/references",
    ),
    # Signal processing, DSP, vibration, seismic
    (
        r"(?i)(signal|radar|vibration|seismic|spectral|phase|hilbert|siesmic)",
        "signal_processing/references",
    ),
    # Visualization (D3, charts, VIZ)
    (r"(?i)(viz|d3|visual|chart|interactive.?data)", "visualization/references"),
    # Artificial lift, ESP, SRP
    (
        r"(?i)(artificial.?lift|ESP|SRP|rotaflex|predator|TOKU|pump)",
        "artificial_lift/references",
    ),
    # Subsea production, intervention
    (r"(?i)(subsea|intervention|process.*subsea)", "subsea/references"),
    # Welding, pipe joining
    (r"(?i)(weld|zaplok|pipe.?join)", "welding/references"),
    # Mooring, FLNG
    (r"(?i)(moor|FLNG|mooring)", "mooring/references"),
    # VIV
    (r"(?i)(VIV)", "viv/references"),
    # Cathodic protection, corrosion
    (r"(?i)(cathodic|corrosion)", "cathodic_protection/references"),
    # Non-engineering (SDEV, FIN, CLD, MAN, HA, COVID, GIT, ML, MATH)
    (
        r"(?i)(REF-SDEV|REF-FIN|REF-CLD|REF-MAN|REF-MNG|REF-HA|REF-ML"
        r"|REF-MATH|REF-GIT|REF-IT|REF-ECO|COVID|montessori|harvard"
        r"|leadership|nudging|proctor|presentation|treating|transform"
        r"|startup|manage|goal|surviving|machine.?learning|vector.*linear"
        r"|javascript|html|AWS|cookbook|pytest|doe_intro)",
        "references/misc",
    ),
    # Carbon capture, CO2, energy transition
    (r"(?i)(carbon|co2|direct.?air|carbonomics|energy.*outlook|WEO)", "references"),
    # O&G general (ORRI, interest definitions, oil tools)
    (r"(?i)(ORRI|interest.?def|oil.?tools|O&G)", "references"),
    # Drones
    (r"(?i)(drone)", "references"),
    # Design of experiments
    (r"(?i)(design.*experiment|Forbes_DoE|doe_intro)", "references"),
    # Catch-all
    (r".*", "references"),
]

# ---------------------------------------------------------------------------
# Pattern-based routing for root-level docs/eng/ files (first match wins)
# ---------------------------------------------------------------------------
ROOT_PATTERNS = [
    (r"(?i)(drill|rig|well|offshore_drill)", "drilling"),
    (r"(?i)(fatigue)", "fatigue"),
    (r"(?i)(riser|catenary|DualGradient)", "risers"),
    (r"(?i)(metocean|wave|wait.*weather)", "metocean"),
    (r"(?i)(wind|turbine|material_demand.*solar)", "wind"),
    (r"(?i)(install|pipelay|tow|vessel)", "offshore_installation"),
    (r"(?i)(fea|structural|civil|JSCE|hyperbolic)", "structural"),
    (r"(?i)(intervention)", "interventions"),
    (r"(?i)(process_eng)", "references/process"),
    (r"(?i)(production|decline_curve|spe\.md)", "production"),
    (r"(?i)(manufacturing|data_driven_manuf)", "references/manufacturing"),
    (r"(?i)(reservoir|REF-ENG-O&G-Reservoir)", "reservoir"),
    (r"(?i)(emission|fuel|hydrogen|flare)", "references"),
    (r"(?i)(ndt|sonatest)", "nde"),
    (r"(?i)(fmea|imca)", "risk"),
    (
        r"(?i)(bsee|BSEE|deepwater.*asgard|Deepwater_Asgard|dhsg)",
        "standards",
    ),
    (r"(?i)(digital.?twin|ditigal_twin)", "digitaltwin"),
    (
        r"(?i)(engineering_general|engineering_analysis|engineering_data"
        r"|engineering_draw|engineering_driven|paraview)",
        "references",
    ),
    (r"(?i)(ag_drone|drone)", "references"),
    (r"(?i)(jones_act)", "marine_ops"),
    (r"(?i)(ml_for_forwarder|forest)", "references"),
    (r"(?i)(O&G.*Guide|oil.*gas.*guide)", "guides"),
    (r".*", "references"),
]


def classify_ref_file(filename: str) -> str:
    """Determine target subdir for a ref/ file based on filename patterns."""
    for pattern, target in REF_PATTERNS:
        if re.search(pattern, filename):
            return target
    return "references"


def classify_root_file(filename: str) -> str:
    """Determine target subdir for a root eng/ file."""
    for pattern, target in ROOT_PATTERNS:
        if re.search(pattern, filename):
            return target
    return "references"


def setup_logging(log_file: Path) -> logging.Logger:
    """Configure logger with both file and stdout handlers."""
    logger = logging.getLogger("consolidate_docs")
    logger.setLevel(logging.INFO)

    formatter = logging.Formatter(
        "%(asctime)s | %(message)s", datefmt="%Y-%m-%d %H:%M:%S"
    )

    fh = logging.FileHandler(str(log_file), mode="w", encoding="utf-8")
    fh.setFormatter(formatter)
    logger.addHandler(fh)

    sh = logging.StreamHandler(sys.stdout)
    sh.setFormatter(formatter)
    logger.addHandler(sh)

    return logger


def process_directory_mapped_files(
    eng_dir: Path,
    modules_dir: Path,
    dry_run: bool,
    logger: logging.Logger,
) -> tuple[int, int, int]:
    """Process files that have explicit directory mappings.

    Returns:
        Tuple of (moved, collisions, errors) counts.
    """
    moved = 0
    collisions = 0
    errors = 0

    for eng_subdir, modules_subdir in DIR_MAP.items():
        src_dir = eng_dir / eng_subdir
        if not src_dir.exists():
            continue

        dst_dir = modules_dir / modules_subdir
        logger.info(f"\n--- {eng_subdir}/ -> {modules_subdir}/ ---")

        for src_file in sorted(src_dir.rglob("*")):
            if not src_file.is_file():
                continue

            # Preserve subdirectory structure within the eng subdir
            rel = src_file.relative_to(src_dir)
            dst_file = dst_dir / rel

            try:
                if dst_file.exists():
                    collisions += 1
                    new_name = f"eng_{dst_file.name}"
                    dst_file = dst_file.parent / new_name
                    logger.info(f"  COLLISION: {rel} -> renamed to {new_name}")

                if not dry_run:
                    dst_file.parent.mkdir(parents=True, exist_ok=True)
                    shutil.move(str(src_file), str(dst_file))

                prefix = "[DRY RUN] " if dry_run else ""
                logger.info(
                    f"  {prefix}MOVE: {src_file.relative_to(eng_dir)}"
                    f" -> modules/{modules_subdir}/{rel}"
                )
                moved += 1
            except Exception as e:
                logger.error(f"  ERROR moving {src_file}: {e}")
                errors += 1

    return moved, collisions, errors


def process_ref_files(
    eng_dir: Path,
    modules_dir: Path,
    dry_run: bool,
    logger: logging.Logger,
) -> tuple[int, int, int]:
    """Process ref/ files using pattern-based routing.

    Returns:
        Tuple of (moved, collisions, errors) counts.
    """
    moved = 0
    collisions = 0
    errors = 0

    ref_dir = eng_dir / "ref"
    if not ref_dir.exists():
        return moved, collisions, errors

    logger.info("\n--- ref/ (pattern-based distribution) ---")

    for src_file in sorted(ref_dir.rglob("*")):
        if not src_file.is_file():
            continue

        target_subdir = classify_ref_file(src_file.name)
        dst_file = modules_dir / target_subdir / src_file.name

        try:
            if dst_file.exists():
                collisions += 1
                new_name = f"eng_{dst_file.name}"
                dst_file = dst_file.parent / new_name
                logger.info(
                    f"  COLLISION: {src_file.name} -> renamed to {new_name}"
                )

            if not dry_run:
                dst_file.parent.mkdir(parents=True, exist_ok=True)
                shutil.move(str(src_file), str(dst_file))

            prefix = "[DRY RUN] " if dry_run else ""
            logger.info(
                f"  {prefix}MOVE: ref/{src_file.name}"
                f" -> modules/{target_subdir}/{src_file.name}"
            )
            moved += 1
        except Exception as e:
            logger.error(f"  ERROR moving {src_file}: {e}")
            errors += 1

    return moved, collisions, errors


def process_root_files(
    eng_dir: Path,
    modules_dir: Path,
    dry_run: bool,
    logger: logging.Logger,
) -> tuple[int, int, int]:
    """Process root-level eng/ files using pattern-based routing.

    Returns:
        Tuple of (moved, collisions, errors) counts.
    """
    moved = 0
    collisions = 0
    errors = 0

    logger.info("\n--- Root-level eng/ files (pattern-based distribution) ---")

    for src_file in sorted(eng_dir.iterdir()):
        if not src_file.is_file():
            continue

        target_subdir = classify_root_file(src_file.name)
        dst_file = modules_dir / target_subdir / src_file.name

        try:
            if dst_file.exists():
                collisions += 1
                new_name = f"eng_{dst_file.name}"
                dst_file = dst_file.parent / new_name
                logger.info(
                    f"  COLLISION: {src_file.name} -> renamed to {new_name}"
                )

            if not dry_run:
                dst_file.parent.mkdir(parents=True, exist_ok=True)
                shutil.move(str(src_file), str(dst_file))

            prefix = "[DRY RUN] " if dry_run else ""
            logger.info(
                f"  {prefix}MOVE: {src_file.name}"
                f" -> modules/{target_subdir}/{src_file.name}"
            )
            moved += 1
        except Exception as e:
            logger.error(f"  ERROR moving {src_file}: {e}")
            errors += 1

    return moved, collisions, errors


def report_remaining(
    eng_dir: Path, logger: logging.Logger
) -> None:
    """Check for any files still remaining in docs/eng/ after migration."""
    remaining_files = [f for f in eng_dir.rglob("*") if f.is_file()]
    if remaining_files:
        logger.warning(
            f"  WARNING: {len(remaining_files)} files still in docs/eng/"
        )
        for f in remaining_files:
            logger.warning(f"    {f.relative_to(eng_dir)}")
    else:
        logger.info("  docs/eng/ is now empty (safe to delete)")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Consolidate docs/eng/ into docs/modules/"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Log moves without executing",
    )
    args = parser.parse_args()

    # Setup paths
    project_root = Path(__file__).resolve().parent.parent
    eng_dir = project_root / "docs" / "eng"
    modules_dir = project_root / "docs" / "modules"
    log_file = Path(__file__).resolve().parent / "consolidate_docs_log.txt"

    # Setup logging
    logger = setup_logging(log_file)

    if not eng_dir.exists():
        logger.error(f"Source directory not found: {eng_dir}")
        sys.exit(1)

    mode_label = "[DRY RUN] " if args.dry_run else ""
    logger.info(f"{mode_label}Starting docs/eng/ -> docs/modules/ consolidation")
    logger.info(f"Source: {eng_dir}")
    logger.info(f"Target: {modules_dir}")
    logger.info("=" * 80)

    # 1. Process directory-mapped files
    dir_moved, dir_collisions, dir_errors = process_directory_mapped_files(
        eng_dir, modules_dir, args.dry_run, logger
    )

    # 2. Process ref/ files
    ref_moved, ref_collisions, ref_errors = process_ref_files(
        eng_dir, modules_dir, args.dry_run, logger
    )

    # 3. Process root-level eng/ files
    root_moved, root_collisions, root_errors = process_root_files(
        eng_dir, modules_dir, args.dry_run, logger
    )

    # Totals
    moved = dir_moved + ref_moved + root_moved
    collisions = dir_collisions + ref_collisions + root_collisions
    errors = dir_errors + ref_errors + root_errors

    # Summary
    logger.info("\n" + "=" * 80)
    logger.info(f"{mode_label}SUMMARY")
    logger.info(f"  Files moved: {moved}")
    logger.info(f"  Collisions (renamed): {collisions}")
    logger.info(f"  Errors: {errors}")

    if not args.dry_run:
        report_remaining(eng_dir, logger)

    logger.info(f"Log saved to: {log_file}")


if __name__ == "__main__":
    main()
