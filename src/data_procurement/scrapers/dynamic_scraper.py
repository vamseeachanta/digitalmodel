"""
Dynamic Content Scraper

Extends BaseScraper with Selenium support for JavaScript-loaded content.
"""

import logging
import time
from pathlib import Path
from typing import Optional, List
import pandas as pd

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, WebDriverException
from webdriver_manager.chrome import ChromeDriverManager

from .base_scraper import BaseScraper


class DynamicScraper(BaseScraper):
    """
    Scraper for dynamic content loaded via JavaScript.

    Uses Selenium WebDriver to render JavaScript and extract tables
    that are loaded asynchronously after page load.
    """

    def __init__(
        self,
        base_url: str,
        headless: bool = True,
        page_load_timeout: int = 30,
        wait_for_tables: int = 10,
        **kwargs
    ):
        """
        Initialize dynamic scraper with Selenium.

        Args:
            base_url: Base URL for scraping
            headless: Run browser in headless mode (default: True)
            page_load_timeout: Page load timeout in seconds (default: 30)
            wait_for_tables: Time to wait for tables to load (default: 10s)
            **kwargs: Additional arguments for BaseScraper
        """
        super().__init__(base_url=base_url, **kwargs)

        self.headless = headless
        self.page_load_timeout = page_load_timeout
        self.wait_for_tables = wait_for_tables

        # Will be initialized when needed
        self.driver: Optional[webdriver.Chrome] = None

    def _init_driver(self):
        """Initialize Selenium WebDriver."""
        if self.driver is not None:
            return  # Already initialized

        self.logger.info("Initializing Selenium WebDriver...")

        # Chrome options
        chrome_options = Options()

        if self.headless:
            chrome_options.add_argument('--headless=new')  # New headless mode
            chrome_options.add_argument('--disable-gpu')

        # Critical options for headless Linux/Docker environments
        chrome_options.add_argument('--no-sandbox')
        chrome_options.add_argument('--disable-dev-shm-usage')
        chrome_options.add_argument('--disable-blink-features=AutomationControlled')
        chrome_options.add_argument('--disable-setuid-sandbox')
        chrome_options.add_argument('--remote-debugging-port=9222')
        chrome_options.add_argument('--window-size=1920,1080')
        chrome_options.add_argument('--start-maximized')

        # Set user agent to match BaseScraper
        user_agent = self._get_user_agent()
        chrome_options.add_argument(f'user-agent={user_agent}')

        # Initialize driver
        try:
            service = Service(ChromeDriverManager().install())
            self.driver = webdriver.Chrome(service=service, options=chrome_options)
            self.driver.set_page_load_timeout(self.page_load_timeout)

            self.logger.info(f"WebDriver initialized (headless={self.headless})")
        except WebDriverException as e:
            self.logger.error(f"Failed to initialize WebDriver: {e}")
            raise

    def fetch_dynamic_page(self, url: str) -> Optional[str]:
        """
        Fetch page with dynamic content using Selenium.

        Args:
            url: URL to fetch

        Returns:
            HTML content after JavaScript execution, or None if failed
        """
        self._respect_rate_limit()

        # Ensure driver is initialized
        self._init_driver()

        # Build full URL
        if not url.startswith(('http://', 'https://')):
            url = f"{self.base_url.rstrip('/')}/{url.lstrip('/')}"

        self.logger.info(f"Fetching dynamic page: {url}")

        try:
            # Navigate to page
            self.driver.get(url)

            # Wait for tables to load
            self.logger.info(f"Waiting {self.wait_for_tables}s for tables to load...")

            try:
                WebDriverWait(self.driver, self.wait_for_tables).until(
                    EC.presence_of_element_located((By.TAG_NAME, "table"))
                )
                self.logger.info("Tables detected on page")
            except TimeoutException:
                self.logger.warning(f"No tables found after {self.wait_for_tables}s - page may not have tables")

            # Get page source after JavaScript execution
            html = self.driver.page_source

            self.logger.info("Successfully fetched dynamic content")
            return html

        except WebDriverException as e:
            self.logger.error(f"WebDriver error fetching {url}: {e}")
            return None
        except Exception as e:
            self.logger.error(f"Unexpected error fetching {url}: {e}")
            return None

    def extract_dynamic_tables(self, url: str) -> List[pd.DataFrame]:
        """
        Extract all tables from a dynamic page.

        Args:
            url: URL containing tables

        Returns:
            List of DataFrames, one per table found
        """
        html = self.fetch_dynamic_page(url)

        if html is None:
            return []

        # Use pandas to extract tables from rendered HTML
        try:
            from io import StringIO
            tables = pd.read_html(StringIO(html))
            self.logger.info(f"Extracted {len(tables)} tables from dynamic content")
            return tables
        except ValueError as e:
            self.logger.warning(f"No tables found in HTML: {e}")
            return []

    def scrape_dynamic(
        self,
        url: str,
        table_selector: Optional[str] = None,
        min_rows: int = 1
    ) -> Optional[pd.DataFrame]:
        """
        Scrape data from dynamic page.

        Args:
            url: URL to scrape
            table_selector: CSS selector for specific table (optional)
            min_rows: Minimum rows for valid table (default: 1)

        Returns:
            DataFrame with scraped data, or None if failed
        """
        self.logger.info(f"Scraping dynamic content from: {url}")

        # If specific table selector provided, use it
        if table_selector:
            self._init_driver()
            self.driver.get(url)

            try:
                table_element = WebDriverWait(self.driver, self.wait_for_tables).until(
                    EC.presence_of_element_located((By.CSS_SELECTOR, table_selector))
                )

                # Get table HTML
                table_html = table_element.get_attribute('outerHTML')

                # Parse with pandas
                from io import StringIO
                tables = pd.read_html(StringIO(table_html))

                if tables:
                    return tables[0]

            except TimeoutException:
                self.logger.error(f"Table not found with selector: {table_selector}")
                return None

        # Otherwise extract all tables and return largest
        tables = self.extract_dynamic_tables(url)

        if not tables:
            self.logger.warning("No tables found on page")
            return None

        # Filter by minimum rows
        valid_tables = [t for t in tables if len(t) >= min_rows]

        if not valid_tables:
            self.logger.warning(f"No tables with >= {min_rows} rows found")
            return None

        # Return largest table
        largest_table = max(valid_tables, key=len)
        self.logger.info(f"Selected largest table: {len(largest_table)} rows x {len(largest_table.columns)} columns")

        return largest_table

    def close(self):
        """Close Selenium driver and clean up."""
        # Close WebDriver first
        if self.driver is not None:
            self.logger.info("Closing Selenium WebDriver...")
            try:
                self.driver.quit()
                self.driver = None
            except Exception as e:
                self.logger.warning(f"Error closing WebDriver: {e}")

        # Then close base scraper session
        if hasattr(super(), 'close'):
            super().close()

    def __del__(self):
        """Ensure driver is closed on deletion."""
        self.close()
