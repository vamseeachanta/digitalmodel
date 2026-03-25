"""Robots.txt parser and analyzer."""

import logging
import subprocess
from typing import Optional
from urllib.parse import urlparse

import pandas as pd


class RobotsParser:
    """Parse and analyze robots.txt files."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)

    def get_robots_url(self, url: str) -> str:
        """Get robots.txt URL from any URL.

        Args:
            url: Any URL from the domain.

        Returns:
            URL to robots.txt file.
        """
        parsed = urlparse(url)
        return f"{parsed.scheme}://{parsed.netloc}/robots.txt"

    def fetch_robots_txt(self, url: str) -> Optional[str]:
        """Fetch robots.txt content from a URL.

        Args:
            url: Website URL.

        Returns:
            Content of robots.txt or None if unavailable.
        """
        robots_url = self.get_robots_url(url)

        try:
            import requests
            response = requests.get(robots_url, timeout=10)
            response.raise_for_status()
            return response.text
        except Exception as e:
            self.logger.warning(f"Could not fetch robots.txt: {e}")
            return None

    def parse(self, url: str) -> dict:
        """Parse robots.txt into structured format.

        Args:
            url: Website URL.

        Returns:
            Dict with User-agent as key and Allow/Disallow rules as values.
        """
        content = self.fetch_robots_txt(url)
        if not content:
            return {}

        result = {}
        current_agent = None

        for line in content.split('\n'):
            line = line.strip()
            if not line or line.startswith('#'):
                continue

            if line.lower().startswith('user-agent:'):
                current_agent = line.split(':', 1)[1].strip()
                if current_agent not in result:
                    result[current_agent] = {'Allow': [], 'Disallow': []}

            elif current_agent and ':' in line:
                directive, value = line.split(':', 1)
                directive = directive.strip().capitalize()
                value = value.strip()

                if directive in ('Allow', 'Disallow') and value:
                    result[current_agent][directive].append(value)

        return result

    def to_dataframe(self, url: str) -> pd.DataFrame:
        """Convert robots.txt to DataFrame format.

        Args:
            url: Website URL.

        Returns:
            DataFrame with User-agent, Status, and Pattern columns.
        """
        parsed = self.parse(url)

        rows = []
        for agent, rules in parsed.items():
            for status, patterns in rules.items():
                for pattern in patterns:
                    rows.append({
                        'User-agent': agent,
                        'Status': status,
                        'Pattern': pattern
                    })

        return pd.DataFrame(rows)
