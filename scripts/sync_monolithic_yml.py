# ABOUTME: Converts .dat files to .yml for library models and syncs to monolithic directories
# ABOUTME: Ensures .yml files exist alongside .dat files and in library monolithic folders

"""
Sync Monolithic YML Files for Library Models

Loads each .dat file using OrcFxAPI, saves as .yml, and copies to library
monolithic directories for offline reference (no OrcaFlex license needed).

The 5 library models:
  - A01 Catenary riser
  - A01 Lazy wave riser
  - A01 Pliant wave riser
  - A01 Steep wave riser
  - A02 Lazy S detailed

Output locations for each model:
  1. docs/domains/orcaflex/examples/raw/<group>/<name>.yml  (alongside .dat)
  2. docs/domains/orcaflex/library/model_library/<slug>/monolithic/<name>.yml
  3. docs/domains/orcaflex/library/tier2_fast/<slug>/monolithic/<name>.yml (if dir exists)

Usage:
    uv run python scripts/sync_monolithic_yml.py
    uv run python scripts/sync_monolithic_yml.py --dry-run
    uv run python scripts/sync_monolithic_yml.py --force
"""

import argparse
import logging
import shutil
import sys
from pathlib import Path

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
logger = logging.getLogger(__name__)

PROJECT_ROOT = Path(__file__).resolve().parent.parent
RAW_DIR = PROJECT_ROOT / "docs" / "modules" / "orcaflex" / "examples" / "raw"
MODEL_LIBRARY_DIR = PROJECT_ROOT / "docs" / "modules" / "orcaflex" / "library" / "model_library"
TIER2_FAST_DIR = PROJECT_ROOT / "docs" / "modules" / "orcaflex" / "library" / "tier2_fast"

# Map: (group_folder, dat_stem) -> library_slug
LIBRARY_MODELS = {
    ("A01", "A01 Catenary riser"): "a01_catenary_riser",
    ("A01", "A01 Lazy wave riser"): "a01_lazy_wave_riser",
    ("A01", "A01 Pliant wave riser"): "a01_pliant_wave_riser",
    ("A01", "A01 Steep wave riser"): "a01_steep_wave_riser",
    ("A02", "A02 Lazy S detailed"): "a02_lazy_s_detailed",
}


def check_orcaflex() -> bool:
    """Verify OrcFxAPI is importable and licensed."""
    try:
        import OrcFxAPI

        model = OrcFxAPI.Model()
        del model
        return True
    except ImportError:
        logger.error("OrcFxAPI is not installed.")
        return False
    except Exception as e:
        logger.error(f"OrcFxAPI license check failed: {e}")
        return False


def convert_dat_to_yml(dat_path: Path, yml_path: Path) -> bool:
    """Load a .dat file and save as .yml using OrcFxAPI."""
    import OrcFxAPI

    try:
        logger.info(f"  Loading: {dat_path.name}")
        model = OrcFxAPI.Model(str(dat_path))
        yml_path.parent.mkdir(parents=True, exist_ok=True)
        model.SaveData(str(yml_path))
        logger.info(f"  Saved:   {yml_path}")
        return True
    except Exception as e:
        logger.error(f"  FAILED converting {dat_path.name}: {e}")
        return False


def copy_file(src: Path, dst: Path) -> bool:
    """Copy a file, creating parent directories as needed."""
    try:
        dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src, dst)
        logger.info(f"  Copied:  {dst}")
        return True
    except Exception as e:
        logger.error(f"  FAILED copying to {dst}: {e}")
        return False


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Sync monolithic .yml files for the 5 library riser models",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be done without executing",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Regenerate .yml even if it already exists",
    )
    args = parser.parse_args()

    if not args.dry_run and not check_orcaflex():
        return 1

    success_count = 0
    fail_count = 0

    for (group, stem), slug in LIBRARY_MODELS.items():
        logger.info(f"\n{'='*60}")
        logger.info(f"Processing: {stem}")
        logger.info(f"{'='*60}")

        dat_path = RAW_DIR / group / f"{stem}.dat"
        if not dat_path.exists():
            logger.error(f"  .dat file not found: {dat_path}")
            fail_count += 1
            continue

        # --- Step 1: Generate .yml alongside .dat ---
        raw_yml = RAW_DIR / group / f"{stem}.yml"

        if args.dry_run:
            action = "REGENERATE" if raw_yml.exists() else "CREATE"
            if not args.force and raw_yml.exists():
                action = "SKIP (exists)"
            logger.info(f"  [DRY-RUN] {action}: {raw_yml}")
        else:
            if args.force or not raw_yml.exists():
                if not convert_dat_to_yml(dat_path, raw_yml):
                    fail_count += 1
                    continue
            else:
                logger.info(f"  Exists (use --force to regenerate): {raw_yml}")

        # Source yml for copying (use the one we just generated, or existing)
        source_yml = raw_yml

        # --- Step 2: Copy .dat and .yml to model_library monolithic ---
        ml_mono_dir = MODEL_LIBRARY_DIR / slug / "monolithic"
        ml_dat = ml_mono_dir / f"{stem}.dat"
        ml_yml = ml_mono_dir / f"{stem}.yml"

        if args.dry_run:
            logger.info(f"  [DRY-RUN] COPY .dat -> {ml_dat}")
            logger.info(f"  [DRY-RUN] COPY .yml -> {ml_yml}")
        else:
            copy_file(dat_path, ml_dat)
            copy_file(source_yml, ml_yml)

        # --- Step 3: Copy .dat and .yml to tier2_fast monolithic (if parent exists) ---
        t2f_parent = TIER2_FAST_DIR / slug
        if t2f_parent.exists():
            t2f_mono_dir = t2f_parent / "monolithic"
            t2f_dat = t2f_mono_dir / f"{stem}.dat"
            t2f_yml = t2f_mono_dir / f"{stem}.yml"

            if args.dry_run:
                logger.info(f"  [DRY-RUN] COPY .dat -> {t2f_dat}")
                logger.info(f"  [DRY-RUN] COPY .yml -> {t2f_yml}")
            else:
                copy_file(dat_path, t2f_dat)
                copy_file(source_yml, t2f_yml)
        else:
            logger.info(f"  tier2_fast/{slug}/ does not exist, skipping tier2_fast copy")

        success_count += 1

    logger.info(f"\n{'='*60}")
    logger.info("SUMMARY")
    logger.info(f"{'='*60}")
    logger.info(f"Models processed successfully: {success_count}/{len(LIBRARY_MODELS)}")
    if fail_count:
        logger.info(f"Models failed: {fail_count}")
    logger.info("Done.")

    return 0 if fail_count == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
