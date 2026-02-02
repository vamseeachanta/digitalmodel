"""Google search rank finder."""

import logging
from typing import Optional

import requests
from bs4 import BeautifulSoup


class SearchRankFinder:
    """Find website ranking in Google search results."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)

    def find_rank(
        self,
        keyword: str,
        website: str,
        num_results: int = 30
    ) -> list[int]:
        """Find ranking positions of a website for a keyword.

        Args:
            keyword: Search keyword.
            website: Website URL to find (without protocol).
            num_results: Number of search results to check.

        Returns:
            List of ranking positions where website appears.

        Note:
            This scrapes Google search results. Use responsibly.
        """
        keyword_encoded = keyword.replace(" ", "+")
        url = f"https://www.google.com/search?q={keyword_encoded}&num={num_results}"

        try:
            response = requests.get(url, timeout=10)
            response.raise_for_status()
        except requests.RequestException as e:
            self.logger.error(f"Search request failed: {e}")
            return []

        soup = BeautifulSoup(response.text, 'html.parser')
        result_divs = soup.find_all(
            'div', attrs={'class': 'Gx5Zad fP1Qef xpd EtOod pkphOe'}
        )

        ranks = []
        for rank, div in enumerate(result_divs, start=1):
            try:
                link = div.find("a", href=True)
                if link and website in link['href']:
                    ranks.append(rank)
            except Exception:
                continue

        return ranks
