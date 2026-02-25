"""Google Analytics Reporting API V4 integration."""

import logging
from typing import Any, Optional

try:
    from apiclient.discovery import build
    from oauth2client.service_account import ServiceAccountCredentials
    HAS_GOOGLE_API = True
except ImportError:
    HAS_GOOGLE_API = False


class GoogleAnalytics:
    """Google Analytics reporting client."""

    SCOPES = ['https://www.googleapis.com/auth/analytics.readonly']

    def __init__(self, key_file: str, view_id: str):
        """Initialize Google Analytics client.

        Args:
            key_file: Path to service account JSON key file.
            view_id: Google Analytics View ID.
        """
        self.logger = logging.getLogger(__name__)
        self.key_file = key_file
        self.view_id = view_id
        self._service = None

    def _initialize_service(self):
        """Initialize the Analytics Reporting API V4 service."""
        if not HAS_GOOGLE_API:
            raise ImportError("google-api-python-client and oauth2client required")

        credentials = ServiceAccountCredentials.from_json_keyfile_name(
            self.key_file, self.SCOPES
        )
        self._service = build('analyticsreporting', 'v4', credentials=credentials)
        return self._service

    @property
    def service(self):
        """Lazy-load the analytics service."""
        if self._service is None:
            self._initialize_service()
        return self._service

    def get_sessions_by_country(
        self,
        start_date: str = '7daysAgo',
        end_date: str = 'today'
    ) -> dict:
        """Get session count by country.

        Args:
            start_date: Start date (default: 7daysAgo).
            end_date: End date (default: today).

        Returns:
            Analytics API response.
        """
        return self.service.reports().batchGet(
            body={
                'reportRequests': [{
                    'viewId': self.view_id,
                    'dateRanges': [{'startDate': start_date, 'endDate': end_date}],
                    'metrics': [{'expression': 'ga:sessions'}],
                    'dimensions': [{'name': 'ga:country'}]
                }]
            }
        ).execute()

    def parse_response(self, response: dict) -> list[dict]:
        """Parse Analytics API response into list of dicts.

        Args:
            response: Raw API response.

        Returns:
            List of dictionaries with dimension/metric values.
        """
        results = []
        for report in response.get('reports', []):
            column_header = report.get('columnHeader', {})
            dimension_headers = column_header.get('dimensions', [])
            metric_headers = column_header.get('metricHeader', {}).get('metricHeaderEntries', [])

            for row in report.get('data', {}).get('rows', []):
                entry = {}
                dimensions = row.get('dimensions', [])
                for header, value in zip(dimension_headers, dimensions):
                    entry[header] = value

                metrics = row.get('metrics', [])
                for values in metrics:
                    for metric_header, value in zip(metric_headers, values.get('values', [])):
                        entry[metric_header.get('name')] = value

                results.append(entry)

        return results
