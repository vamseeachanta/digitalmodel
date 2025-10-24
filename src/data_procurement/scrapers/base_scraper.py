"""
Base Scraper Class

Provides common functionality for all web scrapers including:
- HTTP request handling with retry logic
- Rate limiting and polite scraping
- User agent rotation
- Error handling and logging
- Session management
"""

import time
import logging
import requests
from typing import Optional, Dict, Any, List
from pathlib import Path
from datetime import datetime
import pandas as pd
from urllib.parse import urljoin, urlparse
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry


class BaseScraper:
    """Base class for all web scrapers with common functionality."""

    # Default user agents (rotate to appear more natural)
    USER_AGENTS = [
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
        'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
        'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
    ]

    def __init__(
        self,
        base_url: str,
        rate_limit: float = 2.0,
        timeout: int = 30,
        max_retries: int = 3,
        output_dir: Optional[Path] = None
    ):
        """
        Initialize base scraper.

        Args:
            base_url: Base URL for the website to scrape
            rate_limit: Minimum seconds between requests (polite scraping)
            timeout: Request timeout in seconds
            max_retries: Maximum number of retry attempts
            output_dir: Directory to save scraped data
        """
        self.base_url = base_url
        self.rate_limit = rate_limit
        self.timeout = timeout
        self.max_retries = max_retries
        self.output_dir = output_dir or Path('data')

        # Setup logging
        self.logger = self._setup_logger()

        # Create session with retry logic
        self.session = self._create_session()

        # Track last request time for rate limiting
        self._last_request_time = 0
        self._user_agent_index = 0

        self.logger.info(f"Initialized {self.__class__.__name__} for {base_url}")

    def _setup_logger(self) -> logging.Logger:
        """Setup logger for this scraper."""
        logger = logging.getLogger(self.__class__.__name__)
        logger.setLevel(logging.INFO)

        # Console handler
        if not logger.handlers:
            handler = logging.StreamHandler()
            formatter = logging.Formatter(
                '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
            )
            handler.setFormatter(formatter)
            logger.addHandler(handler)

        return logger

    def _create_session(self) -> requests.Session:
        """Create requests session with retry logic."""
        session = requests.Session()

        # Retry strategy: retry on common temporary errors
        retry_strategy = Retry(
            total=self.max_retries,
            backoff_factor=1,  # Wait 1, 2, 4 seconds between retries
            status_forcelist=[429, 500, 502, 503, 504],
            allowed_methods=["HEAD", "GET", "OPTIONS"]
        )

        adapter = HTTPAdapter(max_retries=retry_strategy)
        session.mount("http://", adapter)
        session.mount("https://", adapter)

        return session

    def _get_user_agent(self) -> str:
        """Get next user agent (rotate through list)."""
        ua = self.USER_AGENTS[self._user_agent_index]
        self._user_agent_index = (self._user_agent_index + 1) % len(self.USER_AGENTS)
        return ua

    def _respect_rate_limit(self):
        """Ensure rate limit is respected between requests."""
        elapsed = time.time() - self._last_request_time
        if elapsed < self.rate_limit:
            sleep_time = self.rate_limit - elapsed
            self.logger.debug(f"Rate limiting: sleeping {sleep_time:.2f}s")
            time.sleep(sleep_time)
        self._last_request_time = time.time()

    def fetch_page(
        self,
        url: str,
        method: str = 'GET',
        params: Optional[Dict] = None,
        data: Optional[Dict] = None,
        headers: Optional[Dict] = None
    ) -> Optional[requests.Response]:
        """
        Fetch a web page with retry logic and rate limiting.

        Args:
            url: URL to fetch (can be relative to base_url)
            method: HTTP method ('GET' or 'POST')
            params: URL parameters
            data: POST data
            headers: Additional headers

        Returns:
            Response object or None if failed
        """
        # Respect rate limit
        self._respect_rate_limit()

        # Build full URL
        full_url = urljoin(self.base_url, url)

        # Prepare headers
        request_headers = {
            'User-Agent': self._get_user_agent(),
            'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
            'Accept-Language': 'en-US,en;q=0.5',
            'Accept-Encoding': 'gzip, deflate',
            'Connection': 'keep-alive',
        }
        if headers:
            request_headers.update(headers)

        try:
            self.logger.info(f"Fetching: {full_url}")

            if method.upper() == 'GET':
                response = self.session.get(
                    full_url,
                    params=params,
                    headers=request_headers,
                    timeout=self.timeout
                )
            else:
                response = self.session.post(
                    full_url,
                    data=data,
                    headers=request_headers,
                    timeout=self.timeout
                )

            response.raise_for_status()
            self.logger.info(f"Success: {response.status_code}")
            return response

        except requests.exceptions.RequestException as e:
            self.logger.error(f"Request failed for {full_url}: {e}")
            return None

    def extract_tables(self, html: str) -> List[pd.DataFrame]:
        """
        Extract all tables from HTML.

        Args:
            html: HTML content

        Returns:
            List of DataFrames, one per table
        """
        try:
            tables = pd.read_html(html)
            self.logger.info(f"Extracted {len(tables)} tables from HTML")
            return tables
        except ValueError as e:
            self.logger.warning(f"No tables found in HTML: {e}")
            return []

    def save_csv(
        self,
        df: pd.DataFrame,
        filename: str,
        domain: str = 'vessels',
        subdomain: str = 'raw'
    ) -> Path:
        """
        Save DataFrame to CSV in appropriate directory.

        Args:
            df: DataFrame to save
            filename: Output filename (e.g., 'fpso_database_2025.csv')
            domain: Domain folder (vessels, equipment, etc.)
            subdomain: Subdomain folder (raw, processed, results)

        Returns:
            Path to saved file
        """
        output_path = self.output_dir / domain / subdomain / filename
        output_path.parent.mkdir(parents=True, exist_ok=True)

        df.to_csv(output_path, index=False)
        self.logger.info(f"Saved {len(df)} rows to {output_path}")

        return output_path

    def save_metadata(
        self,
        filepath: Path,
        source_url: str,
        description: str,
        additional_info: Optional[Dict[str, Any]] = None
    ) -> Path:
        """
        Save metadata JSON for a dataset.

        Args:
            filepath: Path to the data file
            source_url: URL where data was scraped from
            description: Description of the dataset
            additional_info: Additional metadata fields

        Returns:
            Path to metadata file
        """
        import json

        metadata = {
            "dataset_name": filepath.stem,
            "version": "1.0.0",
            "created_date": datetime.now().strftime("%Y-%m-%d"),
            "updated_date": datetime.now().strftime("%Y-%m-%d"),
            "domain": filepath.parent.parent.name,
            "pipeline_stage": filepath.parent.name,
            "description": description,
            "file_format": filepath.suffix[1:].upper(),
            "data_source": {
                "name": urlparse(source_url).netloc,
                "url": source_url,
                "access_date": datetime.now().strftime("%Y-%m-%d"),
                "scraping_method": "Automated web scraping",
                "license": "To be determined - check source website"
            },
            "collection_method": f"Automated scraping via {self.__class__.__name__}",
        }

        if additional_info:
            metadata.update(additional_info)

        metadata_path = filepath.parent / f"{filepath.stem}_metadata.json"
        with open(metadata_path, 'w') as f:
            json.dump(metadata, f, indent=2)

        self.logger.info(f"Saved metadata to {metadata_path}")
        return metadata_path

    def scrape(self) -> Optional[pd.DataFrame]:
        """
        Main scraping method - to be implemented by subclasses.

        Returns:
            DataFrame with scraped data or None if failed
        """
        raise NotImplementedError("Subclasses must implement scrape() method")

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - cleanup resources."""
        self.session.close()
        self.logger.info(f"Closed session for {self.__class__.__name__}")
