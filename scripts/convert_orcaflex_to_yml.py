# ABOUTME: Converts OrcaFlex .dat and .sim files to .yml format
# ABOUTME: Requires OrcaFlex license via OrcFxAPI

"""
OrcaFlex to YAML Converter

Converts OrcaFlex binary files (.dat, .sim) to human-readable YAML format.
Requires OrcaFlex license and OrcFxAPI Python package.

Usage:
    python scripts/convert_orcaflex_to_yml.py --input-dir <path>
    python scripts/convert_orcaflex_to_yml.py --input-dir <path> --output-dir <path>
    python scripts/convert_orcaflex_to_yml.py --dry-run
"""

import argparse
import json
import logging
import os
import sys
from datetime import datetime
from pathlib import Path
from typing import Optional

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
logger = logging.getLogger(__name__)

# Default paths
DEFAULT_INPUT_DIR = Path(__file__).parent.parent / "data" / "orcaflex_examples"
DEFAULT_OUTPUT_DIR = Path(__file__).parent.parent / "docs" / "modules" / "orcaflex" / "examples" / "raw"

# File extensions to convert
CONVERTIBLE_EXTENSIONS = {".dat", ".sim"}


def check_orcaflex_available() -> bool:
    """Check if OrcFxAPI is available and licensed."""
    try:
        import OrcFxAPI
        # Try to create a model to verify license
        model = OrcFxAPI.Model()
        del model
        return True
    except ImportError:
        logger.error("OrcFxAPI not installed. Install with: pip install OrcFxAPI")
        return False
    except Exception as e:
        logger.error(f"OrcFxAPI available but license check failed: {e}")
        return False


class OrcaFlexConverter:
    """Converts OrcaFlex files to YAML format."""

    def __init__(
        self,
        input_dir: Path,
        output_dir: Path,
        dry_run: bool = False,
        skip_existing: bool = True,
        also_save_dat: bool = True,
    ):
        self.input_dir = Path(input_dir)
        self.output_dir = Path(output_dir)
        self.dry_run = dry_run
        self.skip_existing = skip_existing
        self.also_save_dat = also_save_dat
        self.stats = {
            "total_files": 0,
            "successful_conversions": 0,
            "skipped_existing": 0,
            "failed_conversions": 0,
            "errors": [],
        }

    def find_convertible_files(self) -> list[Path]:
        """Find all .dat and .sim files in input directory."""
        files = []
        for ext in CONVERTIBLE_EXTENSIONS:
            files.extend(self.input_dir.rglob(f"*{ext}"))

        # Sort by name for consistent ordering
        files.sort(key=lambda p: p.name)
        return files

    def get_output_path(self, input_file: Path, extension: str) -> Path:
        """Generate output path for converted file."""
        # Extract example ID (e.g., A01, C06) from path
        parts = input_file.parts
        example_id = None

        for part in parts:
            # Look for pattern like "A01", "C06", etc.
            if len(part) >= 3 and part[0].isalpha() and part[1:3].isdigit():
                example_id = part[:3].upper()
                break
            # Also check URL-encoded paths
            if "%20" in part or " " in part:
                clean_part = part.replace("%20", " ")
                if len(clean_part) >= 3 and clean_part[0].isalpha():
                    example_id = clean_part[:3].upper()
                    break

        if example_id:
            # Create subdirectory for example
            output_subdir = self.output_dir / example_id
        else:
            # Fallback to flat structure
            output_subdir = self.output_dir

        output_subdir.mkdir(parents=True, exist_ok=True)

        # Use original filename with new extension
        output_name = input_file.stem + extension
        return output_subdir / output_name

    def convert_file(self, input_file: Path) -> dict:
        """Convert a single OrcaFlex file to YAML (and optionally .dat)."""
        import OrcFxAPI

        result = {
            "input": str(input_file),
            "success": False,
            "outputs": [],
            "error": None,
        }

        yml_output = self.get_output_path(input_file, ".yml")
        dat_output = self.get_output_path(input_file, ".dat")

        # Check if already exists
        if self.skip_existing and yml_output.exists():
            result["success"] = True
            result["skipped"] = True
            result["outputs"] = [str(yml_output)]
            self.stats["skipped_existing"] += 1
            logger.info(f"Skipped (exists): {yml_output.name}")
            return result

        if self.dry_run:
            logger.info(f"[DRY-RUN] Would convert: {input_file.name} -> {yml_output.name}")
            result["success"] = True
            result["outputs"] = [str(yml_output)]
            return result

        try:
            # Load the model
            logger.info(f"Loading: {input_file.name}")
            model = OrcFxAPI.Model(str(input_file))

            # Save as YAML
            model.SaveData(str(yml_output))
            result["outputs"].append(str(yml_output))
            logger.info(f"Saved: {yml_output.name}")

            # Also save as .dat if requested and input was .sim
            if self.also_save_dat and input_file.suffix.lower() == ".sim":
                if not dat_output.exists():
                    model.SaveData(str(dat_output))
                    result["outputs"].append(str(dat_output))
                    logger.info(f"Saved: {dat_output.name}")

            result["success"] = True
            self.stats["successful_conversions"] += 1

        except OrcFxAPI.DLLError as e:
            error_msg = str(e)
            result["error"] = error_msg
            self.stats["failed_conversions"] += 1
            self.stats["errors"].append({
                "file": str(input_file),
                "error": error_msg
            })
            logger.error(f"Failed: {input_file.name} - {error_msg[:100]}...")

        except Exception as e:
            error_msg = str(e)
            result["error"] = error_msg
            self.stats["failed_conversions"] += 1
            self.stats["errors"].append({
                "file": str(input_file),
                "error": error_msg
            })
            logger.error(f"Failed: {input_file.name} - {error_msg}")

        return result

    def run(self) -> dict:
        """Run the conversion process."""
        logger.info(f"OrcaFlex to YAML Converter")
        logger.info(f"Input directory: {self.input_dir}")
        logger.info(f"Output directory: {self.output_dir}")
        logger.info(f"Dry run: {self.dry_run}")
        logger.info(f"Skip existing: {self.skip_existing}")

        # Find files to convert
        files = self.find_convertible_files()
        self.stats["total_files"] = len(files)
        logger.info(f"Found {len(files)} files to convert")

        if not files:
            logger.warning("No convertible files found")
            return self.stats

        # Create output directory
        if not self.dry_run:
            self.output_dir.mkdir(parents=True, exist_ok=True)

        # Convert each file
        start_time = datetime.now()

        for i, file_path in enumerate(files, 1):
            logger.info(f"\n[{i}/{len(files)}] Processing: {file_path.name}")
            self.convert_file(file_path)

        end_time = datetime.now()
        processing_time = (end_time - start_time).total_seconds()

        # Save conversion report
        if not self.dry_run:
            report = {
                "timestamp": datetime.now().isoformat(),
                "statistics": {
                    **self.stats,
                    "processing_time": processing_time,
                },
                "input_directory": str(self.input_dir),
                "output_directory": str(self.output_dir),
            }

            report_path = self.output_dir / "conversion_report.json"
            with open(report_path, "w") as f:
                json.dump(report, f, indent=2)
            logger.info(f"\nReport saved: {report_path}")

        # Print summary
        logger.info(f"\n{'='*60}")
        logger.info("CONVERSION SUMMARY")
        logger.info(f"{'='*60}")
        logger.info(f"Total files found: {self.stats['total_files']}")
        logger.info(f"Successful conversions: {self.stats['successful_conversions']}")
        logger.info(f"Skipped (existing): {self.stats['skipped_existing']}")
        logger.info(f"Failed conversions: {self.stats['failed_conversions']}")
        logger.info(f"Processing time: {processing_time:.2f} seconds")

        if self.stats["errors"]:
            logger.warning(f"\nFailed files:")
            for error in self.stats["errors"]:
                logger.warning(f"  - {Path(error['file']).name}")

        return self.stats


def sync_examples(source_dir: Path, target_dir: Path, dry_run: bool = False) -> dict:
    """Sync converted examples to the raw examples directory."""
    logger.info(f"\nSyncing examples from {source_dir} to {target_dir}")

    stats = {"copied": 0, "skipped": 0}

    # Find all .yml files in source
    yml_files = list(source_dir.rglob("*.yml"))

    for yml_file in yml_files:
        # Determine target path
        relative_path = yml_file.relative_to(source_dir)
        target_path = target_dir / relative_path

        if target_path.exists():
            stats["skipped"] += 1
            continue

        if dry_run:
            logger.info(f"[DRY-RUN] Would copy: {yml_file.name}")
            stats["copied"] += 1
        else:
            target_path.parent.mkdir(parents=True, exist_ok=True)
            import shutil
            shutil.copy2(yml_file, target_path)
            logger.info(f"Copied: {yml_file.name}")
            stats["copied"] += 1

    logger.info(f"Sync complete: {stats['copied']} copied, {stats['skipped']} skipped")
    return stats


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Convert OrcaFlex .dat/.sim files to YAML format",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Convert all files in default input directory
  python convert_orcaflex_to_yml.py

  # Convert specific directory
  python convert_orcaflex_to_yml.py --input-dir path/to/models

  # Preview without converting
  python convert_orcaflex_to_yml.py --dry-run

  # Force reconvert existing files
  python convert_orcaflex_to_yml.py --no-skip-existing

Requirements:
  - OrcaFlex license (checked via OrcFxAPI)
  - OrcFxAPI Python package
        """,
    )

    parser.add_argument(
        "--input-dir",
        type=Path,
        default=DEFAULT_INPUT_DIR,
        help=f"Input directory with .dat/.sim files (default: {DEFAULT_INPUT_DIR})",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help=f"Output directory for .yml files (default: {DEFAULT_OUTPUT_DIR})",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Preview conversions without executing",
    )
    parser.add_argument(
        "--no-skip-existing",
        action="store_true",
        help="Reconvert files even if output exists",
    )
    parser.add_argument(
        "--no-dat",
        action="store_true",
        help="Don't save .dat files (only .yml)",
    )
    parser.add_argument(
        "--check-only",
        action="store_true",
        help="Only check OrcaFlex availability, don't convert",
    )

    args = parser.parse_args()

    # Check OrcaFlex availability
    if not check_orcaflex_available():
        if args.check_only:
            logger.error("OrcaFlex is NOT available")
            return 1
        logger.error("Cannot proceed without OrcaFlex license")
        return 1

    if args.check_only:
        logger.info("OrcaFlex is available and licensed")
        return 0

    # Run converter
    converter = OrcaFlexConverter(
        input_dir=args.input_dir,
        output_dir=args.output_dir,
        dry_run=args.dry_run,
        skip_existing=not args.no_skip_existing,
        also_save_dat=not args.no_dat,
    )

    try:
        stats = converter.run()
        return 0 if stats["failed_conversions"] == 0 else 1
    except KeyboardInterrupt:
        logger.info("\nOperation cancelled by user")
        return 1
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
