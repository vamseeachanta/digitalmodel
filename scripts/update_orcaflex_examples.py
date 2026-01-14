# ABOUTME: Downloads and updates OrcaFlex examples from Orcina website
# ABOUTME: Extracts .yml and .py files, optionally keeps binary .dat/.sim for conversion

"""
OrcaFlex Examples Updater

Downloads official OrcaFlex examples from Orcina website and extracts
files for use as reference examples.

Usage:
    python scripts/update_orcaflex_examples.py
    python scripts/update_orcaflex_examples.py --categories a c k
    python scripts/update_orcaflex_examples.py --dry-run
    python scripts/update_orcaflex_examples.py --keep-binaries  # For conversion workflow

Workflow for YAML conversion:
    1. python scripts/update_orcaflex_examples.py --keep-binaries
    2. python scripts/convert_orcaflex_to_yml.py
    3. python scripts/update_orcaflex_examples.py  # To clean up binaries
"""

import argparse
import logging
import os
import re
import shutil
import subprocess
import sys
import zipfile
from pathlib import Path
from typing import Optional
from urllib.parse import unquote

import requests
from bs4 import BeautifulSoup

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
logger = logging.getLogger(__name__)

# Constants
ORCINA_EXAMPLES_URL = "https://www.orcina.com/resources/examples/"
DEFAULT_OUTPUT_DIR = Path(__file__).parent.parent / "data" / "orcaflex_examples"

# Example categories from Orcina website
CATEGORIES = {
    "a": ("A_production_risers", "Production Risers"),
    "b": ("B_drilling_risers", "Drilling Risers"),
    "c": ("C_moorings", "Moorings"),
    "d": ("D_riser_installation", "Riser Installation"),
    "e": ("E_pipelay_recovery", "Pipelay and Recovery"),
    "f": ("F_payload_handling", "Payload Handling"),
    "g": ("G_deployment", "Deployment"),
    "h": ("H_offloading_systems", "Offloading Systems"),
    "i": ("I_towed_systems", "Towed Systems"),
    "j": ("J_defence", "Defence"),
    "k": ("K_renewables", "Renewables"),
    "l": ("L_diffraction", "Diffraction"),
    "m": ("M_pipelines", "Pipelines"),
    "z": ("Z_miscellaneous", "Miscellaneous"),
}

# File extensions to keep (text-readable)
KEEP_EXTENSIONS = {".yml", ".yaml", ".py", ".txt", ".md", ".json", ".csv"}

# File extensions to remove (binary)
REMOVE_EXTENSIONS = {".dat", ".sim", ".owr", ".owd", ".wrk", ".gdf", ".dll", ".bts", ".x"}


class OrcaFlexExamplesUpdater:
    """Downloads and processes OrcaFlex examples from Orcina website."""

    def __init__(
        self,
        output_dir: Path,
        categories: Optional[list[str]] = None,
        dry_run: bool = False,
        keep_zips: bool = False,
        keep_pdfs: bool = True,
        keep_binaries: bool = False,
    ):
        self.output_dir = Path(output_dir)
        self.categories = categories or list(CATEGORIES.keys())
        self.dry_run = dry_run
        self.keep_zips = keep_zips
        self.keep_pdfs = keep_pdfs
        self.keep_binaries = keep_binaries
        self.session = requests.Session()
        self.session.headers.update(
            {
                "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) OrcaFlexExamplesUpdater/1.0"
            }
        )

    def get_zip_urls(self, category_key: str) -> list[str]:
        """Fetch ZIP file URLs from a category page."""
        url = f"{ORCINA_EXAMPLES_URL}?key={category_key}"
        logger.info(f"Fetching category {category_key.upper()} from {url}")

        try:
            response = self.session.get(url, timeout=30)
            response.raise_for_status()
        except requests.RequestException as e:
            logger.error(f"Failed to fetch {url}: {e}")
            return []

        soup = BeautifulSoup(response.text, "html.parser")
        zip_urls = []

        for link in soup.find_all("a", href=True):
            href = link["href"]
            if href.endswith(".zip"):
                zip_urls.append(href)

        logger.info(f"Found {len(zip_urls)} ZIP files in category {category_key.upper()}")
        return zip_urls

    def download_file(self, url: str, dest_path: Path) -> bool:
        """Download a file from URL to destination path."""
        if self.dry_run:
            logger.info(f"[DRY-RUN] Would download: {url}")
            return True

        try:
            response = self.session.get(url, stream=True, timeout=120)
            response.raise_for_status()

            dest_path.parent.mkdir(parents=True, exist_ok=True)

            with open(dest_path, "wb") as f:
                for chunk in response.iter_content(chunk_size=8192):
                    f.write(chunk)

            logger.info(f"Downloaded: {dest_path.name}")
            return True

        except requests.RequestException as e:
            logger.error(f"Failed to download {url}: {e}")
            return False

    def extract_zip(self, zip_path: Path, extract_dir: Path) -> bool:
        """Extract a ZIP file to destination directory."""
        if self.dry_run:
            logger.info(f"[DRY-RUN] Would extract: {zip_path.name}")
            return True

        try:
            with zipfile.ZipFile(zip_path, "r") as zf:
                zf.extractall(extract_dir)
            logger.info(f"Extracted: {zip_path.name}")
            return True
        except zipfile.BadZipFile as e:
            logger.error(f"Failed to extract {zip_path}: {e}")
            return False

    def clean_extracted_files(self, directory: Path) -> dict[str, int]:
        """Remove binary files and keep only text-readable files."""
        if self.dry_run:
            logger.info(f"[DRY-RUN] Would clean: {directory}")
            return {"removed": 0, "kept": 0}

        if self.keep_binaries:
            logger.info("Keeping binary files (.dat, .sim) for conversion workflow")
            return {"removed": 0, "kept": 0}

        stats = {"removed": 0, "kept": 0}

        for file_path in directory.rglob("*"):
            if not file_path.is_file():
                continue

            ext = file_path.suffix.lower()

            # Remove binary files
            if ext in REMOVE_EXTENSIONS:
                file_path.unlink()
                stats["removed"] += 1
            # Optionally remove PDFs
            elif ext == ".pdf" and not self.keep_pdfs:
                file_path.unlink()
                stats["removed"] += 1
            else:
                stats["kept"] += 1

        return stats

    def organize_files(self, category_dir: Path) -> dict[str, list[Path]]:
        """Organize extracted files into yml_models and python_scripts folders."""
        if self.dry_run:
            return {"yml": [], "py": []}

        yml_dir = self.output_dir / "yml_models"
        py_dir = self.output_dir / "python_scripts"
        yml_dir.mkdir(exist_ok=True)
        py_dir.mkdir(exist_ok=True)

        organized = {"yml": [], "py": []}

        # Get category prefix from directory name (e.g., "A" from "A_production_risers")
        category_prefix = category_dir.name.split("_")[0]

        for file_path in category_dir.rglob("*"):
            if not file_path.is_file():
                continue

            ext = file_path.suffix.lower()

            if ext in {".yml", ".yaml"}:
                # Create clean filename with category prefix
                new_name = f"{category_prefix}_{self._clean_filename(file_path.stem)}.yml"
                dest = yml_dir / new_name
                if not dest.exists():
                    shutil.copy2(file_path, dest)
                    organized["yml"].append(dest)

            elif ext == ".py":
                # Create clean filename with category prefix
                new_name = f"{category_prefix}_{self._clean_filename(file_path.stem)}.py"
                dest = py_dir / new_name
                if not dest.exists():
                    shutil.copy2(file_path, dest)
                    organized["py"].append(dest)

        return organized

    def _clean_filename(self, name: str) -> str:
        """Clean filename by removing special characters."""
        # Decode URL encoding
        name = unquote(name)
        # Remove example number prefix if already present (e.g., "L06 ")
        name = re.sub(r"^[A-Z]\d+\s*", "", name)
        # Replace spaces and special chars with underscores
        name = re.sub(r"[^\w\-]", "_", name)
        # Remove multiple underscores
        name = re.sub(r"_+", "_", name)
        # Remove leading/trailing underscores
        name = name.strip("_")
        return name

    def process_category(self, category_key: str) -> dict:
        """Process a single category: download, extract, clean, organize."""
        if category_key not in CATEGORIES:
            logger.warning(f"Unknown category: {category_key}")
            return {"status": "skipped", "reason": "unknown category"}

        dir_name, description = CATEGORIES[category_key]
        category_dir = self.output_dir / dir_name
        category_dir.mkdir(parents=True, exist_ok=True)

        logger.info(f"\n{'='*60}")
        logger.info(f"Processing Category {category_key.upper()}: {description}")
        logger.info(f"{'='*60}")

        # Get ZIP URLs
        zip_urls = self.get_zip_urls(category_key)
        if not zip_urls:
            return {"status": "empty", "files": 0}

        results = {
            "status": "success",
            "downloaded": 0,
            "extracted": 0,
            "yml_files": [],
            "py_files": [],
        }

        for url in zip_urls:
            # Download ZIP
            filename = unquote(url.split("/")[-1])
            zip_path = category_dir / filename

            if self.download_file(url, zip_path):
                results["downloaded"] += 1

                # Extract ZIP
                extract_dir = category_dir / zip_path.stem
                if self.extract_zip(zip_path, extract_dir):
                    results["extracted"] += 1

                # Remove ZIP if not keeping
                if not self.keep_zips and not self.dry_run:
                    zip_path.unlink()

        # Clean binary files
        if not self.dry_run:
            clean_stats = self.clean_extracted_files(category_dir)
            logger.info(
                f"Cleaned: removed {clean_stats['removed']} binary files, "
                f"kept {clean_stats['kept']} text files"
            )

        # Organize files
        organized = self.organize_files(category_dir)
        results["yml_files"] = [str(p) for p in organized["yml"]]
        results["py_files"] = [str(p) for p in organized["py"]]

        return results

    def run(self) -> dict:
        """Run the full update process for all selected categories."""
        logger.info(f"OrcaFlex Examples Updater")
        logger.info(f"Output directory: {self.output_dir}")
        logger.info(f"Categories: {', '.join(c.upper() for c in self.categories)}")
        logger.info(f"Dry run: {self.dry_run}")

        # Create output directory
        if not self.dry_run:
            self.output_dir.mkdir(parents=True, exist_ok=True)

        all_results = {}
        total_yml = 0
        total_py = 0

        for category_key in self.categories:
            results = self.process_category(category_key)
            all_results[category_key] = results
            total_yml += len(results.get("yml_files", []))
            total_py += len(results.get("py_files", []))

        # Summary
        logger.info(f"\n{'='*60}")
        logger.info("SUMMARY")
        logger.info(f"{'='*60}")
        logger.info(f"Categories processed: {len(self.categories)}")
        logger.info(f"Total YML files: {total_yml}")
        logger.info(f"Total Python scripts: {total_py}")
        logger.info(f"Output directory: {self.output_dir}")

        return all_results


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Download and update OrcaFlex examples from Orcina website",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Categories:
  a - Production Risers      h - Offloading Systems
  b - Drilling Risers        i - Towed Systems
  c - Moorings               j - Defence
  d - Riser Installation     k - Renewables
  e - Pipelay and Recovery   l - Diffraction
  f - Payload Handling       m - Pipelines
  g - Deployment             z - Miscellaneous

Examples:
  python update_orcaflex_examples.py                    # Update all categories
  python update_orcaflex_examples.py --categories a c k # Update specific categories
  python update_orcaflex_examples.py --dry-run          # Preview without downloading
        """,
    )

    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help=f"Output directory (default: {DEFAULT_OUTPUT_DIR})",
    )
    parser.add_argument(
        "--categories",
        nargs="+",
        choices=list(CATEGORIES.keys()),
        help="Specific categories to update (default: all)",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Preview actions without downloading",
    )
    parser.add_argument(
        "--keep-zips",
        action="store_true",
        help="Keep ZIP files after extraction",
    )
    parser.add_argument(
        "--no-pdfs",
        action="store_true",
        help="Remove PDF documentation files",
    )
    parser.add_argument(
        "--keep-binaries",
        action="store_true",
        help="Keep binary .dat/.sim files for YAML conversion workflow",
    )

    args = parser.parse_args()

    updater = OrcaFlexExamplesUpdater(
        output_dir=args.output_dir,
        categories=args.categories,
        dry_run=args.dry_run,
        keep_zips=args.keep_zips,
        keep_pdfs=not args.no_pdfs,
        keep_binaries=args.keep_binaries,
    )

    try:
        results = updater.run()
        return 0
    except KeyboardInterrupt:
        logger.info("\nOperation cancelled by user")
        return 1
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
