"""
Complete OrcaFlex Example Downloader

Downloads ALL examples from the Orcina portal across all letter categories.
The examples are organized by letter keys: a, b, c, d, e, f, g, h, i, j, k, l, z
"""

import os
import re
import time
import json
import hashlib
import logging
import zipfile
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from urllib.parse import urljoin, urlparse
from datetime import datetime

import requests
from bs4 import BeautifulSoup
from tqdm import tqdm

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class CompleteOrcaflexDownloader:
    """Downloads ALL OrcaFlex examples from Orcina portal across all letter categories."""
    
    # All letter keys available on the Orcina examples page
    LETTER_KEYS = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'z']
    
    def __init__(self, base_dir: str = "docs/domains/orcaflex/examples"):
        """Initialize the complete downloader."""
        self.base_url = "https://www.orcina.com/resources/examples/"
        self.base_dir = Path(base_dir)
        self.raw_dir = self.base_dir / "raw"
        self.zip_dir = self.base_dir / "downloads"
        self.metadata_dir = self.base_dir / "metadata"
        
        # Create directories
        self.raw_dir.mkdir(parents=True, exist_ok=True)
        self.zip_dir.mkdir(parents=True, exist_ok=True)
        self.metadata_dir.mkdir(parents=True, exist_ok=True)
        
        # Session configuration
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        })
        
        # Rate limiting
        self.rate_limit_delay = 1.0
        self.last_request_time = 0
        
        # Manifest
        self.manifest_file = self.metadata_dir / "complete_manifest.json"
        self.manifest = self.load_manifest()
    
    def load_manifest(self) -> Dict:
        """Load or create download manifest."""
        if self.manifest_file.exists():
            with open(self.manifest_file, 'r') as f:
                return json.load(f)
        return {
            "last_updated": None,
            "letter_categories": {},
            "zip_files": {},
            "examples": {},
            "statistics": {}
        }
    
    def save_manifest(self):
        """Save download manifest."""
        self.manifest["last_updated"] = datetime.now().isoformat()
        with open(self.manifest_file, 'w') as f:
            json.dump(self.manifest, f, indent=2)
    
    def _rate_limit(self):
        """Enforce rate limiting."""
        current_time = time.time()
        time_since_last = current_time - self.last_request_time
        if time_since_last < self.rate_limit_delay:
            time.sleep(self.rate_limit_delay - time_since_last)
        self.last_request_time = time.time()
    
    def parse_letter_page(self, letter: str) -> List[Dict]:
        """
        Parse a specific letter category page to find all ZIP files.
        
        Args:
            letter: Letter key (a-l, z)
            
        Returns:
            List of ZIP file information dictionaries
        """
        url = f"{self.base_url}?key={letter}"
        logger.info(f"Parsing category '{letter.upper()}': {url}")
        
        self._rate_limit()
        try:
            response = self.session.get(url, timeout=30)
            response.raise_for_status()
        except requests.RequestException as e:
            logger.error(f"Failed to fetch page for letter '{letter}': {e}")
            return []
        
        soup = BeautifulSoup(response.text, 'html.parser')
        zip_files = []
        
        # Find all links to ZIP files
        for link in soup.find_all('a', href=True):
            href = link.get('href', '')
            text = link.get_text(strip=True)
            
            if href.endswith('.zip'):
                # Extract category info from URL
                # Pattern: /wp-content/uploads/examples/{letter}/{code}/{filename}.zip
                category_match = re.search(r'/([a-z])/([a-z]\d+)/', href.lower())
                
                if category_match:
                    category_letter = category_match.group(1).upper()
                    category_code = category_match.group(2).upper()
                else:
                    # Fallback to letter from query
                    category_letter = letter.upper()
                    category_code = f"{letter.upper()}XX"
                
                # Extract file size from text
                size_match = re.search(r'\((\d+(?:\.\d+)?)\s*([MG]b)\)', text, re.IGNORECASE)
                if size_match:
                    size_value = float(size_match.group(1))
                    size_unit = size_match.group(2).upper()
                    size_mb = size_value if 'M' in size_unit else size_value * 1024
                else:
                    size_mb = 0
                
                # Get filename
                filename = os.path.basename(urlparse(href).path)
                
                # Clean description
                description = filename.replace('.zip', '')
                description = re.sub(r'^[A-Z]\d+\s*', '', description).strip()
                
                zip_info = {
                    'url': urljoin(self.base_url, href),
                    'filename': filename,
                    'display_text': text,
                    'category_letter': category_letter,
                    'category_code': category_code,
                    'size_mb': size_mb,
                    'description': description,
                    'source_letter': letter.upper()
                }
                
                zip_files.append(zip_info)
                logger.info(f"  Found: {category_code} - {description} ({size_mb:.1f}MB)")
        
        # Update manifest with letter category info
        self.manifest['letter_categories'][letter] = {
            'url': url,
            'zip_count': len(zip_files),
            'last_checked': datetime.now().isoformat()
        }
        
        return zip_files
    
    def download_zip_file(self, zip_info: Dict) -> Optional[Path]:
        """Download a single ZIP file."""
        zip_path = self.zip_dir / zip_info['filename']
        
        # Check if already downloaded
        if zip_path.exists():
            # Verify size if we have it
            if zip_info['size_mb'] > 0:
                actual_size_mb = zip_path.stat().st_size / (1024 * 1024)
                if abs(actual_size_mb - zip_info['size_mb']) < 1:  # Within 1MB tolerance
                    logger.info(f"  Already downloaded: {zip_info['filename']}")
                    return zip_path
            else:
                logger.info(f"  Already downloaded: {zip_info['filename']}")
                return zip_path
        
        logger.info(f"  Downloading: {zip_info['filename']} ({zip_info['size_mb']:.1f}MB)")
        
        try:
            self._rate_limit()
            response = self.session.get(zip_info['url'], stream=True, timeout=60)
            response.raise_for_status()
            
            # Download with progress bar
            total_size = int(response.headers.get('content-length', 0))
            
            with open(zip_path, 'wb') as f:
                with tqdm(total=total_size, unit='B', unit_scale=True, 
                         desc=f"    {zip_info['category_code']}") as pbar:
                    for chunk in response.iter_content(chunk_size=8192):
                        if chunk:
                            f.write(chunk)
                            pbar.update(len(chunk))
            
            # Calculate checksum
            with open(zip_path, 'rb') as f:
                checksum = hashlib.md5(f.read()).hexdigest()
            
            # Update manifest
            self.manifest['zip_files'][zip_info['filename']] = {
                'url': zip_info['url'],
                'category_code': zip_info['category_code'],
                'category_letter': zip_info['category_letter'],
                'source_letter': zip_info['source_letter'],
                'description': zip_info['description'],
                'size_mb': zip_info['size_mb'],
                'checksum': checksum,
                'downloaded': datetime.now().isoformat(),
                'path': str(zip_path.relative_to(self.base_dir))
            }
            self.save_manifest()
            
            logger.info(f"  ✅ Downloaded: {zip_info['filename']}")
            return zip_path
            
        except Exception as e:
            logger.error(f"  ❌ Failed to download {zip_info['filename']}: {e}")
            if zip_path.exists():
                zip_path.unlink()
            return None
    
    def extract_zip_file(self, zip_path: Path, zip_info: Dict) -> List[str]:
        """Extract OrcaFlex files from ZIP archive."""
        extracted_files = []
        category_dir = self.raw_dir / zip_info['category_code']
        category_dir.mkdir(parents=True, exist_ok=True)
        
        try:
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                file_list = zip_ref.namelist()
                
                # Extract OrcaFlex files
                orcaflex_extensions = ['.dat', '.sim', '.yml', '.yaml']
                
                for file_name in file_list:
                    if any(file_name.lower().endswith(ext) for ext in orcaflex_extensions):
                        base_name = os.path.basename(file_name)
                        if not base_name:  # Skip directories
                            continue
                        
                        target_path = category_dir / base_name
                        
                        with zip_ref.open(file_name) as source:
                            with open(target_path, 'wb') as target:
                                target.write(source.read())
                        
                        extracted_files.append(str(target_path))
                        
                        # Update manifest
                        self.manifest['examples'][base_name] = {
                            'category': zip_info['category_code'],
                            'category_letter': zip_info['category_letter'],
                            'zip_file': zip_info['filename'],
                            'original_path': file_name,
                            'extracted_path': str(target_path.relative_to(self.base_dir)),
                            'extracted': datetime.now().isoformat()
                        }
                
                # Also extract documentation
                doc_extensions = ['.pdf', '.docx', '.txt', '.md']
                doc_dir = category_dir / 'docs'
                
                for file_name in file_list:
                    if any(file_name.lower().endswith(ext) for ext in doc_extensions):
                        doc_dir.mkdir(exist_ok=True)
                        base_name = os.path.basename(file_name)
                        if base_name:
                            target_path = doc_dir / base_name
                            with zip_ref.open(file_name) as source:
                                with open(target_path, 'wb') as target:
                                    target.write(source.read())
                
                logger.info(f"  Extracted {len(extracted_files)} OrcaFlex files")
                
        except Exception as e:
            logger.error(f"  Failed to extract {zip_path}: {e}")
        
        self.save_manifest()
        return extracted_files
    
    def download_all_categories(self, letters: Optional[List[str]] = None, 
                              skip_large: bool = False,
                              max_size_mb: float = 100) -> Dict:
        """
        Download examples from all letter categories.
        
        Args:
            letters: Specific letters to download (None for all)
            skip_large: Skip files larger than max_size_mb
            max_size_mb: Maximum file size in MB when skip_large is True
            
        Returns:
            Dictionary with download statistics
        """
        letters = letters or self.LETTER_KEYS
        
        logger.info("="*70)
        logger.info("COMPLETE ORCAFLEX EXAMPLES DOWNLOAD")
        logger.info("="*70)
        logger.info(f"Categories to process: {', '.join(letters).upper()}")
        
        all_zip_files = []
        
        # Step 1: Parse all letter pages
        logger.info("\n" + "="*70)
        logger.info("STEP 1: Discovering all available examples")
        logger.info("="*70)
        
        for letter in letters:
            logger.info(f"\nCategory '{letter.upper()}':")
            zip_files = self.parse_letter_page(letter)
            all_zip_files.extend(zip_files)
            time.sleep(0.5)  # Be nice to the server
        
        # Remove duplicates (same filename)
        unique_zips = {}
        for zf in all_zip_files:
            if zf['filename'] not in unique_zips:
                unique_zips[zf['filename']] = zf
        
        all_zip_files = list(unique_zips.values())
        
        # Calculate statistics
        total_size_mb = sum(zf['size_mb'] for zf in all_zip_files)
        
        logger.info("\n" + "="*70)
        logger.info("DISCOVERY SUMMARY")
        logger.info("="*70)
        logger.info(f"Total unique ZIP files found: {len(all_zip_files)}")
        logger.info(f"Total download size: {total_size_mb:.1f}MB ({total_size_mb/1024:.2f}GB)")
        
        # Filter if requested
        if skip_large:
            original_count = len(all_zip_files)
            all_zip_files = [zf for zf in all_zip_files if zf['size_mb'] <= max_size_mb]
            if len(all_zip_files) < original_count:
                skipped = original_count - len(all_zip_files)
                new_total = sum(zf['size_mb'] for zf in all_zip_files)
                logger.info(f"Skipping {skipped} files larger than {max_size_mb}MB")
                logger.info(f"Reduced download size: {new_total:.1f}MB")
        
        # Sort by size (smallest first)
        all_zip_files.sort(key=lambda x: x['size_mb'] if x['size_mb'] > 0 else float('inf'))
        
        # Step 2: Download and extract
        logger.info("\n" + "="*70)
        logger.info("STEP 2: Downloading and extracting files")
        logger.info("="*70)
        
        stats = {
            'total_zips': len(all_zip_files),
            'successful_downloads': 0,
            'failed_downloads': 0,
            'total_extracted': 0,
            'total_size_mb': 0,
            'by_category': {}
        }
        
        for i, zip_info in enumerate(all_zip_files, 1):
            logger.info(f"\n[{i}/{len(all_zip_files)}] {zip_info['category_code']} - {zip_info['description']}")
            
            # Download
            zip_path = self.download_zip_file(zip_info)
            
            if zip_path:
                stats['successful_downloads'] += 1
                stats['total_size_mb'] += zip_info['size_mb']
                
                # Extract
                extracted = self.extract_zip_file(zip_path, zip_info)
                stats['total_extracted'] += len(extracted)
                
                # Track by category letter
                letter = zip_info['source_letter']
                if letter not in stats['by_category']:
                    stats['by_category'][letter] = {'zips': 0, 'files': 0}
                stats['by_category'][letter]['zips'] += 1
                stats['by_category'][letter]['files'] += len(extracted)
            else:
                stats['failed_downloads'] += 1
        
        # Update manifest statistics
        self.manifest['statistics'] = stats
        self.save_manifest()
        
        return stats
    
    def generate_complete_catalog(self) -> str:
        """Generate a comprehensive catalog of all downloaded examples."""
        catalog = []
        catalog.append("# Complete OrcaFlex Examples Catalog")
        catalog.append("")
        catalog.append(f"Generated: {datetime.now().isoformat()}")
        catalog.append("")
        
        # Statistics
        if 'statistics' in self.manifest:
            stats = self.manifest['statistics']
            catalog.append("## Download Statistics")
            catalog.append(f"- Total ZIP files: {stats.get('total_zips', 0)}")
            catalog.append(f"- Successfully downloaded: {stats.get('successful_downloads', 0)}")
            catalog.append(f"- Total OrcaFlex files: {stats.get('total_extracted', 0)}")
            catalog.append(f"- Total size: {stats.get('total_size_mb', 0):.1f}MB")
            catalog.append("")
            
            if 'by_category' in stats:
                catalog.append("## By Letter Category")
                for letter in sorted(stats['by_category'].keys()):
                    cat_stats = stats['by_category'][letter]
                    catalog.append(f"- **{letter}**: {cat_stats['zips']} ZIPs, {cat_stats['files']} files")
                catalog.append("")
        
        # Group examples by category code
        by_category = {}
        for example_name, example_info in self.manifest['examples'].items():
            category = example_info['category']
            if category not in by_category:
                by_category[category] = {
                    'letter': example_info.get('category_letter', category[0]),
                    'files': []
                }
            by_category[category]['files'].append(example_name)
        
        # Generate catalog by category
        catalog.append("## Examples by Category")
        catalog.append("")
        
        current_letter = None
        for category in sorted(by_category.keys()):
            cat_info = by_category[category]
            letter = cat_info['letter']
            
            # Add letter header if changed
            if letter != current_letter:
                catalog.append(f"\n### Category {letter}")
                catalog.append("")
                current_letter = letter
            
            # Find ZIP info
            zip_info = None
            for zip_name, zip_data in self.manifest['zip_files'].items():
                if zip_data.get('category_code') == category:
                    zip_info = zip_data
                    break
            
            catalog.append(f"#### {category} - {zip_info['description'] if zip_info else 'Unknown'}")
            
            if zip_info:
                catalog.append(f"- Size: {zip_info['size_mb']:.1f}MB")
            
            catalog.append("- Files:")
            for example in sorted(cat_info['files']):
                catalog.append(f"  - {example}")
            catalog.append("")
        
        catalog_text = "\n".join(catalog)
        
        # Save catalog
        catalog_file = self.base_dir / "catalog" / "complete_catalog.md"
        catalog_file.parent.mkdir(exist_ok=True)
        with open(catalog_file, 'w') as f:
            f.write(catalog_text)
        
        logger.info(f"Catalog saved to: {catalog_file}")
        return catalog_text


def main():
    """Main function to download all OrcaFlex examples."""
    import argparse
    
    parser = argparse.ArgumentParser(description="Download ALL OrcaFlex examples from Orcina portal")
    parser.add_argument(
        '--letters',
        nargs='+',
        help='Specific letters to download (default: all)',
        choices=['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'z']
    )
    parser.add_argument(
        '--skip-large',
        action='store_true',
        help='Skip files larger than max-size'
    )
    parser.add_argument(
        '--max-size',
        type=float,
        default=100,
        help='Maximum file size in MB when using --skip-large (default: 100)'
    )
    
    args = parser.parse_args()
    
    downloader = CompleteOrcaflexDownloader()
    
    # Download from specified or all categories
    stats = downloader.download_all_categories(
        letters=args.letters,
        skip_large=args.skip_large,
        max_size_mb=args.max_size
    )
    
    # Generate complete catalog
    logger.info("\n" + "="*70)
    logger.info("STEP 3: Generating complete catalog")
    logger.info("="*70)
    
    catalog = downloader.generate_complete_catalog()
    
    # Final summary
    logger.info("\n" + "="*70)
    logger.info("DOWNLOAD COMPLETE - FINAL SUMMARY")
    logger.info("="*70)
    logger.info(f"✅ Successful downloads: {stats['successful_downloads']}")
    logger.info(f"❌ Failed downloads: {stats['failed_downloads']}")
    logger.info(f"📁 Total OrcaFlex files: {stats['total_extracted']}")
    logger.info(f"💾 Total size: {stats['total_size_mb']:.1f}MB")
    
    if stats['by_category']:
        logger.info("\nBy letter category:")
        for letter in sorted(stats['by_category'].keys()):
            cat_stats = stats['by_category'][letter]
            logger.info(f"  {letter}: {cat_stats['zips']} ZIPs, {cat_stats['files']} files")
    
    # Check what file types we have
    raw_dir = Path("docs/domains/orcaflex/examples/raw")
    if raw_dir.exists():
        dat_files = list(raw_dir.glob("**/*.dat"))
        sim_files = list(raw_dir.glob("**/*.sim"))
        yml_files = list(raw_dir.glob("**/*.yml")) + list(raw_dir.glob("**/*.yaml"))
        
        logger.info(f"\nFile types in repository:")
        logger.info(f"  .dat files: {len(dat_files)}")
        logger.info(f"  .sim files: {len(sim_files)}")
        logger.info(f"  .yml/.yaml files: {len(yml_files)}")
    
    logger.info("\n✅ All OrcaFlex examples have been successfully downloaded!")


if __name__ == "__main__":
    main()