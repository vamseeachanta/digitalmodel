"""Google indexed page count checker."""

import logging
import time
from typing import Optional

import requests
from bs4 import BeautifulSoup


class IndexPageCounter:
    """Check number of indexed pages for a domain using Google search."""

    HEADERS = {
        'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) '
                      'AppleWebKit/537.36 (KHTML, like Gecko) '
                      'Chrome/86.0.4240.198 Safari/537.36'
    }

    def __init__(self):
        self.logger = logging.getLogger(__name__)

    def get_indexed_count(self, domain: str) -> Optional[int]:
        """Get number of indexed pages for a domain.

        Args:
            domain: Domain name (without protocol).

        Returns:
            Number of indexed pages or None if unavailable.

        Note:
            This method scrapes Google search results. Use responsibly
            and be aware this may violate Google's terms of service.
        """
        search_url = (
            f'https://www.google.com/search?q=site%3A{domain}'
            f'&oq=site%3A{domain}&sourceid=chrome&ie=UTF-8'
        )

        try:
            response = requests.get(search_url, headers=self.HEADERS, timeout=10)
            response.raise_for_status()
        except requests.RequestException as e:
            self.logger.error(f"Failed to fetch index count for {domain}: {e}")
            return None

        soup = BeautifulSoup(response.text, "html.parser")
        result_stats = soup.find('div', {'id': 'result-stats'})

        if not result_stats:
            self.logger.warning(f"No result stats found for {domain}")
            return None

        try:
            text = result_stats.text
            # Parse "About X results" format
            count_str = text.split('About ')[1].split(' results')[0]
            count = int(count_str.replace(',', ''))
            return count
        except (IndexError, ValueError) as e:
            self.logger.warning(f"Could not parse result count: {e}")
            return None

    def get_indexed_counts(
        self,
        domains: list[str],
        delay: float = 1.0
    ) -> dict[str, Optional[int]]:
        """Get indexed page counts for multiple domains.

        Args:
            domains: List of domain names.
            delay: Delay between requests in seconds.

        Returns:
            Dictionary mapping domain to indexed count.
        """
        results = {}
        for domain in domains:
            results[domain] = self.get_indexed_count(domain)
            time.sleep(delay)
        return results
