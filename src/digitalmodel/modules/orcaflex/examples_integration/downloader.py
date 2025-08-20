"""
OrcaFlex Example Downloader

Downloads examples from the Orcina portal with proper error handling and retry logic.
"""

import os
import time
import json
import hashlib
import logging
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


class OrcaflexExampleDownloader:
    """Downloads OrcaFlex examples from Orcina portal with rate limiting and retry logic."""
    
    def __init__(self, base_dir: str = "docs/modules/orcaflex/examples"):
        """
        Initialize the downloader.
        
        Args:
            base_dir: Base directory for storing downloaded examples
        """
        self.base_url = "https://www.orcina.com/resources/examples/"
        self.base_dir = Path(base_dir)
        self.raw_dir = self.base_dir / "raw"
        self.metadata_dir = self.base_dir / "metadata"
        
        # Create directories if they don't exist
        self.raw_dir.mkdir(parents=True, exist_ok=True)
        self.metadata_dir.mkdir(parents=True, exist_ok=True)
        
        # Session configuration
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'OrcaFlex-Example-Collector/1.0 (Educational/Research Purpose)'
        })
        
        # Rate limiting configuration
        self.rate_limit_delay = 1.0  # seconds between requests
        self.last_request_time = 0
        
        # Retry configuration
        self.max_retries = 3
        self.retry_delay = 2.0  # Initial retry delay in seconds
        
        # Download manifest
        self.manifest_file = self.metadata_dir / "download_manifest.json"
        self.manifest = self.load_manifest()
    
    def load_manifest(self) -> Dict:
        """Load or create download manifest."""
        if self.manifest_file.exists():
            with open(self.manifest_file, 'r') as f:
                return json.load(f)
        return {
            "last_updated": None,
            "examples": {},
            "categories": {}
        }
    
    def save_manifest(self):
        """Save download manifest."""
        self.manifest["last_updated"] = datetime.now().isoformat()
        with open(self.manifest_file, 'w') as f:
            json.dump(self.manifest, f, indent=2)
    
    def _rate_limit(self):
        """Enforce rate limiting between requests."""
        current_time = time.time()
        time_since_last = current_time - self.last_request_time
        if time_since_last < self.rate_limit_delay:
            time.sleep(self.rate_limit_delay - time_since_last)
        self.last_request_time = time.time()
    
    def _download_with_retry(self, url: str, max_retries: Optional[int] = None) -> Optional[requests.Response]:
        """
        Download with exponential backoff retry logic.
        
        Args:
            url: URL to download
            max_retries: Maximum number of retries (uses self.max_retries if None)
            
        Returns:
            Response object or None if failed
        """
        max_retries = max_retries or self.max_retries
        delay = self.retry_delay
        
        for attempt in range(max_retries + 1):
            try:
                self._rate_limit()
                response = self.session.get(url, timeout=30)
                response.raise_for_status()
                return response
            except requests.exceptions.RequestException as e:
                if attempt < max_retries:
                    logger.warning(f"Attempt {attempt + 1} failed for {url}: {e}")
                    logger.info(f"Retrying in {delay} seconds...")
                    time.sleep(delay)
                    delay *= 2  # Exponential backoff
                else:
                    logger.error(f"Failed to download {url} after {max_retries + 1} attempts: {e}")
                    return None
    
    def parse_categories(self) -> List[Dict]:
        """
        Parse example categories from the main page.
        
        Returns:
            List of category dictionaries with name and URL
        """
        logger.info("Parsing example categories from Orcina portal...")
        response = self._download_with_retry(self.base_url)
        
        if not response:
            logger.error("Failed to fetch main examples page")
            return []
        
        soup = BeautifulSoup(response.content, 'html.parser')
        categories = []
        
        # Parse category links (adjust selectors based on actual HTML structure)
        # This is a template - actual selectors need to be determined from the website
        category_links = soup.find_all('a', class_='category-link')  # Placeholder selector
        
        for link in category_links:
            category = {
                'name': link.get_text(strip=True),
                'url': urljoin(self.base_url, link.get('href', '')),
                'examples': []
            }
            categories.append(category)
            logger.info(f"Found category: {category['name']}")
        
        # Store in manifest
        for cat in categories:
            if cat['name'] not in self.manifest['categories']:
                self.manifest['categories'][cat['name']] = {
                    'url': cat['url'],
                    'last_checked': datetime.now().isoformat(),
                    'example_count': 0
                }
        
        self.save_manifest()
        return categories
    
    def get_examples_in_category(self, category: Dict) -> List[Dict]:
        """
        Get all examples in a specific category.
        
        Args:
            category: Category dictionary with name and URL
            
        Returns:
            List of example dictionaries
        """
        logger.info(f"Getting examples for category: {category['name']}")
        response = self._download_with_retry(category['url'])
        
        if not response:
            logger.error(f"Failed to fetch category page: {category['name']}")
            return []
        
        soup = BeautifulSoup(response.content, 'html.parser')
        examples = []
        
        # Parse example links (adjust based on actual HTML)
        example_links = soup.find_all('a', href=lambda x: x and ('.dat' in x or '.sim' in x))
        
        for link in example_links:
            example = {
                'name': link.get_text(strip=True) or os.path.basename(link.get('href', '')),
                'url': urljoin(category['url'], link.get('href', '')),
                'category': category['name'],
                'file_type': 'dat' if '.dat' in link.get('href', '') else 'sim'
            }
            examples.append(example)
            logger.info(f"Found example: {example['name']}")
        
        return examples
    
    def download_example(self, example: Dict) -> bool:
        """
        Download a single example file.
        
        Args:
            example: Example dictionary with name, URL, and category
            
        Returns:
            True if successful, False otherwise
        """
        # Create category directory
        category_dir = self.raw_dir / example['category'].replace('/', '_')
        category_dir.mkdir(exist_ok=True)
        
        # Determine file path
        filename = os.path.basename(urlparse(example['url']).path)
        if not filename:
            filename = f"{example['name']}.{example['file_type']}"
        filepath = category_dir / filename
        
        # Check if already downloaded
        if filepath.exists():
            logger.info(f"Already downloaded: {filename}")
            return True
        
        logger.info(f"Downloading: {filename}")
        response = self._download_with_retry(example['url'])
        
        if not response:
            logger.error(f"Failed to download: {filename}")
            return False
        
        # Save file
        try:
            with open(filepath, 'wb') as f:
                f.write(response.content)
            
            # Calculate checksum
            checksum = hashlib.md5(response.content).hexdigest()
            
            # Update manifest
            self.manifest['examples'][filename] = {
                'category': example['category'],
                'url': example['url'],
                'path': str(filepath.relative_to(self.base_dir)),
                'checksum': checksum,
                'size': len(response.content),
                'downloaded': datetime.now().isoformat()
            }
            self.save_manifest()
            
            logger.info(f"Successfully downloaded: {filename} ({len(response.content)} bytes)")
            return True
            
        except Exception as e:
            logger.error(f"Error saving file {filename}: {e}")
            if filepath.exists():
                filepath.unlink()  # Remove partial file
            return False
    
    def download_all_examples(self, categories: Optional[List[str]] = None) -> Tuple[int, int]:
        """
        Download all examples from specified categories or all categories.
        
        Args:
            categories: List of category names to download (None for all)
            
        Returns:
            Tuple of (successful_downloads, failed_downloads)
        """
        logger.info("Starting bulk download of OrcaFlex examples...")
        
        # Get all categories
        all_categories = self.parse_categories()
        
        if not all_categories:
            logger.error("No categories found. The website structure might have changed.")
            return 0, 0
        
        # Filter categories if specified
        if categories:
            all_categories = [c for c in all_categories if c['name'] in categories]
        
        successful = 0
        failed = 0
        
        # Process each category
        for category in all_categories:
            logger.info(f"\nProcessing category: {category['name']}")
            examples = self.get_examples_in_category(category)
            
            # Download each example with progress bar
            for example in tqdm(examples, desc=f"Downloading {category['name']}"):
                if self.download_example(example):
                    successful += 1
                else:
                    failed += 1
        
        logger.info(f"\nDownload complete! Successful: {successful}, Failed: {failed}")
        return successful, failed
    
    def verify_downloads(self) -> Dict:
        """
        Verify all downloaded files against their checksums.
        
        Returns:
            Dictionary with verification results
        """
        logger.info("Verifying downloaded files...")
        results = {
            'valid': [],
            'corrupted': [],
            'missing': []
        }
        
        for filename, info in self.manifest['examples'].items():
            filepath = self.base_dir / info['path']
            
            if not filepath.exists():
                results['missing'].append(filename)
                logger.warning(f"Missing file: {filename}")
                continue
            
            # Calculate current checksum
            with open(filepath, 'rb') as f:
                current_checksum = hashlib.md5(f.read()).hexdigest()
            
            if current_checksum == info['checksum']:
                results['valid'].append(filename)
            else:
                results['corrupted'].append(filename)
                logger.warning(f"Checksum mismatch: {filename}")
        
        logger.info(f"Verification complete: {len(results['valid'])} valid, "
                   f"{len(results['corrupted'])} corrupted, {len(results['missing'])} missing")
        
        return results
    
    def generate_download_report(self) -> str:
        """
        Generate a comprehensive download report.
        
        Returns:
            Report as a formatted string
        """
        report = []
        report.append("=" * 60)
        report.append("OrcaFlex Examples Download Report")
        report.append("=" * 60)
        report.append(f"Generated: {datetime.now().isoformat()}")
        report.append(f"Last Updated: {self.manifest.get('last_updated', 'Never')}")
        report.append("")
        
        # Category summary
        report.append("Categories:")
        for cat_name, cat_info in self.manifest['categories'].items():
            report.append(f"  - {cat_name}: {cat_info.get('example_count', 0)} examples")
        report.append("")
        
        # Download summary
        report.append(f"Total Examples Downloaded: {len(self.manifest['examples'])}")
        
        # Size summary
        total_size = sum(info['size'] for info in self.manifest['examples'].values())
        report.append(f"Total Size: {total_size / (1024*1024):.2f} MB")
        report.append("")
        
        # File type breakdown
        dat_files = sum(1 for f in self.manifest['examples'] if f.endswith('.dat'))
        sim_files = sum(1 for f in self.manifest['examples'] if f.endswith('.sim'))
        report.append("File Types:")
        report.append(f"  - .dat files: {dat_files}")
        report.append(f"  - .sim files: {sim_files}")
        report.append("")
        
        # Verification results
        verification = self.verify_downloads()
        report.append("Verification Status:")
        report.append(f"  - Valid: {len(verification['valid'])}")
        report.append(f"  - Corrupted: {len(verification['corrupted'])}")
        report.append(f"  - Missing: {len(verification['missing'])}")
        
        if verification['corrupted']:
            report.append("\nCorrupted Files:")
            for f in verification['corrupted']:
                report.append(f"  - {f}")
        
        if verification['missing']:
            report.append("\nMissing Files:")
            for f in verification['missing']:
                report.append(f"  - {f}")
        
        report_text = "\n".join(report)
        
        # Save report
        report_file = self.base_dir / "reports" / f"download_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
        report_file.parent.mkdir(exist_ok=True)
        with open(report_file, 'w') as f:
            f.write(report_text)
        
        logger.info(f"Report saved to: {report_file}")
        return report_text


if __name__ == "__main__":
    # Example usage
    downloader = OrcaflexExampleDownloader()
    
    # Test parsing categories
    categories = downloader.parse_categories()
    print(f"Found {len(categories)} categories")
    
    # Generate report
    report = downloader.generate_download_report()
    print(report)