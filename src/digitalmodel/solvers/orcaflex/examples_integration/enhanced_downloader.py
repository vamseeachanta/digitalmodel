"""
Enhanced OrcaFlex Example Downloader

Downloads and extracts example ZIP files from the Orcina portal.
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
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class EnhancedOrcaflexDownloader:
    """Downloads and extracts OrcaFlex examples from Orcina portal ZIP files."""
    
    def __init__(self, base_dir: str = "docs/domains/orcaflex/examples"):
        """
        Initialize the enhanced downloader.
        
        Args:
            base_dir: Base directory for storing downloaded examples
        """
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
        self.manifest_file = self.metadata_dir / "download_manifest.json"
        self.manifest = self.load_manifest()
    
    def load_manifest(self) -> Dict:
        """Load or create download manifest."""
        if self.manifest_file.exists():
            with open(self.manifest_file, 'r') as f:
                return json.load(f)
        return {
            "last_updated": None,
            "zip_files": {},
            "examples": {},
            "categories": {}
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
    
    def parse_examples_page(self) -> List[Dict]:
        """
        Parse the main examples page to find all ZIP files.
        
        Returns:
            List of ZIP file information dictionaries
        """
        logger.info("Parsing Orcina examples page...")
        
        self._rate_limit()
        response = self.session.get(self.base_url, timeout=30)
        response.raise_for_status()
        
        soup = BeautifulSoup(response.text, 'html.parser')
        zip_files = []
        
        # Find all links to ZIP files
        for link in soup.find_all('a', href=True):
            href = link.get('href', '')
            text = link.get_text(strip=True)
            
            if href.endswith('.zip'):
                # Extract category from URL path
                # Example: /wp-content/uploads/examples/a/a01/A01 Catenary and wave systems.zip
                path_parts = href.split('/')
                category_match = re.search(r'/([a-z])/([a-z]\d+)/', href)
                
                if category_match:
                    category_letter = category_match.group(1).upper()
                    category_code = category_match.group(2).upper()
                else:
                    category_letter = 'Unknown'
                    category_code = 'Unknown'
                
                # Extract file size from text if available
                size_match = re.search(r'\((\d+)Mb\)', text)
                size_mb = int(size_match.group(1)) if size_match else 0
                
                # Get filename from URL
                filename = os.path.basename(urlparse(href).path)
                
                zip_info = {
                    'url': urljoin(self.base_url, href),
                    'filename': filename,
                    'display_text': text,
                    'category_letter': category_letter,
                    'category_code': category_code,
                    'size_mb': size_mb,
                    'description': filename.replace('.zip', '').replace(category_code, '').strip()
                }
                
                zip_files.append(zip_info)
                logger.info(f"Found ZIP: {category_code} - {zip_info['description']} ({size_mb}MB)")
        
        # Also check for example panels or divs with more structure
        example_panels = soup.find_all('div', class_='example-panel')
        for panel in example_panels:
            # Try to extract more structured information
            title = panel.find(['h2', 'h3', 'h4'])
            if title:
                category_name = title.get_text(strip=True)
                # Store category information
                if category_name not in self.manifest['categories']:
                    self.manifest['categories'][category_name] = {
                        'found': datetime.now().isoformat()
                    }
        
        self.save_manifest()
        return zip_files
    
    def download_zip_file(self, zip_info: Dict) -> Optional[Path]:
        """
        Download a single ZIP file.
        
        Args:
            zip_info: Dictionary with ZIP file information
            
        Returns:
            Path to downloaded file or None if failed
        """
        zip_path = self.zip_dir / zip_info['filename']
        
        # Check if already downloaded
        if zip_path.exists():
            logger.info(f"Already downloaded: {zip_info['filename']}")
            return zip_path
        
        logger.info(f"Downloading: {zip_info['filename']} ({zip_info['size_mb']}MB)")
        
        try:
            self._rate_limit()
            response = self.session.get(zip_info['url'], stream=True, timeout=60)
            response.raise_for_status()
            
            # Download with progress bar
            total_size = int(response.headers.get('content-length', 0))
            
            with open(zip_path, 'wb') as f:
                with tqdm(total=total_size, unit='B', unit_scale=True, desc=zip_info['filename']) as pbar:
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
                'description': zip_info['description'],
                'size_mb': zip_info['size_mb'],
                'checksum': checksum,
                'downloaded': datetime.now().isoformat(),
                'path': str(zip_path.relative_to(self.base_dir))
            }
            self.save_manifest()
            
            logger.info(f"Successfully downloaded: {zip_info['filename']}")
            return zip_path
            
        except Exception as e:
            logger.error(f"Failed to download {zip_info['filename']}: {e}")
            if zip_path.exists():
                zip_path.unlink()  # Remove partial download
            return None
    
    def extract_zip_file(self, zip_path: Path, zip_info: Dict) -> List[str]:
        """
        Extract OrcaFlex files from ZIP archive.
        
        Args:
            zip_path: Path to ZIP file
            zip_info: ZIP file information
            
        Returns:
            List of extracted file paths
        """
        extracted_files = []
        category_dir = self.raw_dir / zip_info['category_code']
        category_dir.mkdir(exist_ok=True)
        
        try:
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                # List all files in the ZIP
                file_list = zip_ref.namelist()
                
                # Extract only OrcaFlex files
                orcaflex_extensions = ['.dat', '.sim', '.yml', '.yaml']
                
                for file_name in file_list:
                    # Check if it's an OrcaFlex file
                    if any(file_name.lower().endswith(ext) for ext in orcaflex_extensions):
                        # Extract to category directory
                        target_path = category_dir / os.path.basename(file_name)
                        
                        # Read and write file to avoid nested directories
                        with zip_ref.open(file_name) as source:
                            with open(target_path, 'wb') as target:
                                target.write(source.read())
                        
                        extracted_files.append(str(target_path))
                        
                        # Update manifest
                        self.manifest['examples'][os.path.basename(file_name)] = {
                            'category': zip_info['category_code'],
                            'zip_file': zip_info['filename'],
                            'original_path': file_name,
                            'extracted_path': str(target_path.relative_to(self.base_dir)),
                            'extracted': datetime.now().isoformat()
                        }
                
                logger.info(f"Extracted {len(extracted_files)} OrcaFlex files from {zip_info['filename']}")
                
                # Also extract any documentation files
                doc_extensions = ['.pdf', '.docx', '.txt', '.md']
                doc_dir = category_dir / 'docs'
                
                for file_name in file_list:
                    if any(file_name.lower().endswith(ext) for ext in doc_extensions):
                        doc_dir.mkdir(exist_ok=True)
                        target_path = doc_dir / os.path.basename(file_name)
                        
                        with zip_ref.open(file_name) as source:
                            with open(target_path, 'wb') as target:
                                target.write(source.read())
                        
                        logger.info(f"Extracted documentation: {os.path.basename(file_name)}")
                
        except Exception as e:
            logger.error(f"Failed to extract {zip_path}: {e}")
        
        self.save_manifest()
        return extracted_files
    
    def download_all_examples(self) -> Tuple[int, int]:
        """
        Download and extract all examples.
        
        Returns:
            Tuple of (successful_downloads, failed_downloads)
        """
        logger.info("Starting download of all OrcaFlex examples...")
        
        # Parse the examples page
        zip_files = self.parse_examples_page()
        
        if not zip_files:
            logger.warning("No ZIP files found on the examples page")
            return 0, 0
        
        logger.info(f"Found {len(zip_files)} ZIP files to download")
        
        successful = 0
        failed = 0
        total_extracted = 0
        
        # Download and extract each ZIP file
        for zip_info in zip_files:
            zip_path = self.download_zip_file(zip_info)
            
            if zip_path:
                extracted = self.extract_zip_file(zip_path, zip_info)
                if extracted:
                    successful += 1
                    total_extracted += len(extracted)
                else:
                    failed += 1
            else:
                failed += 1
        
        logger.info(f"\nDownload complete!")
        logger.info(f"ZIP files: {successful} successful, {failed} failed")
        logger.info(f"Total OrcaFlex files extracted: {total_extracted}")
        
        return successful, failed
    
    def generate_catalog(self) -> str:
        """
        Generate a catalog of all downloaded examples.
        
        Returns:
            Catalog as formatted string
        """
        catalog = []
        catalog.append("# OrcaFlex Examples Catalog")
        catalog.append("")
        catalog.append(f"Generated: {datetime.now().isoformat()}")
        catalog.append(f"Total ZIP files: {len(self.manifest['zip_files'])}")
        catalog.append(f"Total examples: {len(self.manifest['examples'])}")
        catalog.append("")
        
        # Group examples by category
        by_category = {}
        for example_name, example_info in self.manifest['examples'].items():
            category = example_info['category']
            if category not in by_category:
                by_category[category] = []
            by_category[category].append(example_name)
        
        # Generate catalog by category
        for category in sorted(by_category.keys()):
            # Find ZIP file info for this category
            zip_info = None
            for zip_name, zip_data in self.manifest['zip_files'].items():
                if zip_data['category_code'] == category:
                    zip_info = zip_data
                    break
            
            catalog.append(f"## {category} - {zip_info['description'] if zip_info else 'Unknown'}")
            catalog.append("")
            
            if zip_info:
                catalog.append(f"- Source: {zip_info['url']}")
                catalog.append(f"- Size: {zip_info['size_mb']}MB")
                catalog.append("")
            
            catalog.append("### Files:")
            for example in sorted(by_category[category]):
                catalog.append(f"- {example}")
            catalog.append("")
        
        catalog_text = "\n".join(catalog)
        
        # Save catalog
        catalog_file = self.base_dir / "catalog" / "examples_catalog.md"
        catalog_file.parent.mkdir(exist_ok=True)
        with open(catalog_file, 'w') as f:
            f.write(catalog_text)
        
        logger.info(f"Catalog saved to: {catalog_file}")
        return catalog_text


def main():
    """Main function to run the enhanced downloader."""
    downloader = EnhancedOrcaflexDownloader()
    
    # Download all examples
    successful, failed = downloader.download_all_examples()
    
    # Generate catalog
    if successful > 0:
        catalog = downloader.generate_catalog()
        print("\nCatalog Preview:")
        print("=" * 60)
        print("\n".join(catalog.split("\n")[:30]))  # Show first 30 lines
        print("...")
        print("=" * 60)


if __name__ == "__main__":
    main()