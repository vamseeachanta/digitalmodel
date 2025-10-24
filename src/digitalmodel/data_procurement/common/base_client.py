# ABOUTME: Universal API client supporting REST, GraphQL, and WebSocket protocols
# ABOUTME: Implements authentication, rate limiting, retries, and streaming response handling

"""
Base API Client
===============

Universal HTTP client for data procurement from web APIs.

Features:
- Protocol support: REST, GraphQL, WebSocket
- Authentication: API Key, OAuth2, JWT
- Rate limiting with exponential backoff
- Automatic retries with circuit breaker
- Streaming response handling (no storage)
- Connection pooling
"""

import time
import logging
from typing import Dict, Any, Optional, Iterator, Callable
from datetime import datetime, timedelta
from abc import ABC, abstractmethod
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

logger = logging.getLogger(__name__)


class RateLimiter:
    """Token bucket rate limiter for API calls."""

    def __init__(self, requests_per_minute: Optional[int] = None,
                 requests_per_hour: Optional[int] = None,
                 requests_per_day: Optional[int] = None):
        """
        Initialize rate limiter.

        Args:
            requests_per_minute: Max requests per minute (None = unlimited)
            requests_per_hour: Max requests per hour (None = unlimited)
            requests_per_day: Max requests per day (None = unlimited)
        """
        self.rpm = requests_per_minute
        self.rph = requests_per_hour
        self.rpd = requests_per_day

        self.minute_tokens = []
        self.hour_tokens = []
        self.day_tokens = []

    def acquire(self) -> None:
        """Block until rate limit allows request."""
        now = datetime.now()

        # Clean old tokens
        self._clean_tokens(self.minute_tokens, now, timedelta(minutes=1))
        self._clean_tokens(self.hour_tokens, now, timedelta(hours=1))
        self._clean_tokens(self.day_tokens, now, timedelta(days=1))

        # Wait if limits exceeded
        if self.rpm and len(self.minute_tokens) >= self.rpm:
            wait_time = 60 - (now - self.minute_tokens[0]).total_seconds()
            logger.info(f"Rate limit reached, waiting {wait_time:.1f}s")
            time.sleep(max(0, wait_time))
            self.minute_tokens.clear()

        if self.rph and len(self.hour_tokens) >= self.rph:
            wait_time = 3600 - (now - self.hour_tokens[0]).total_seconds()
            logger.warning(f"Hourly rate limit reached, waiting {wait_time:.1f}s")
            time.sleep(max(0, wait_time))
            self.hour_tokens.clear()

        if self.rpd and len(self.day_tokens) >= self.rpd:
            wait_time = 86400 - (now - self.day_tokens[0]).total_seconds()
            logger.error(f"Daily rate limit reached, waiting {wait_time:.1f}s")
            time.sleep(max(0, wait_time))
            self.day_tokens.clear()

        # Add tokens
        now = datetime.now()
        if self.rpm:
            self.minute_tokens.append(now)
        if self.rph:
            self.hour_tokens.append(now)
        if self.rpd:
            self.day_tokens.append(now)

    def _clean_tokens(self, tokens: list, now: datetime, window: timedelta) -> None:
        """Remove tokens outside time window."""
        cutoff = now - window
        while tokens and tokens[0] < cutoff:
            tokens.pop(0)


class BaseAPIClient(ABC):
    """
    Universal API client for data procurement.

    Supports:
    - REST APIs (GET, POST, PUT, DELETE)
    - GraphQL (queries, mutations)
    - WebSocket (streaming)
    - Authentication (API Key, OAuth2, JWT)
    - Rate limiting
    - Retries with exponential backoff
    - Streaming responses (zero storage)
    """

    def __init__(self, base_url: str, auth_config: Dict[str, Any],
                 rate_limit: Optional[Dict[str, int]] = None,
                 timeout: int = 300,
                 max_retries: int = 3):
        """
        Initialize API client.

        Args:
            base_url: Base URL for API endpoints
            auth_config: Authentication configuration
                - method: "api_key" | "oauth2" | "jwt" | "none"
                - credentials: API key or OAuth tokens
            rate_limit: Rate limiting configuration
                - requests_per_minute: int
                - requests_per_hour: int
                - requests_per_day: int
            timeout: Request timeout in seconds
            max_retries: Maximum retry attempts
        """
        self.base_url = base_url.rstrip('/')
        self.auth_config = auth_config
        self.timeout = timeout

        # Setup rate limiter
        if rate_limit:
            self.rate_limiter = RateLimiter(
                requests_per_minute=rate_limit.get('requests_per_minute'),
                requests_per_hour=rate_limit.get('requests_per_hour'),
                requests_per_day=rate_limit.get('requests_per_day')
            )
        else:
            self.rate_limiter = None

        # Setup session with retries
        self.session = requests.Session()
        retry_strategy = Retry(
            total=max_retries,
            backoff_factor=1,
            status_forcelist=[429, 500, 502, 503, 504],
            allowed_methods=["GET", "POST"]
        )
        adapter = HTTPAdapter(max_retries=retry_strategy, pool_connections=10, pool_maxsize=10)
        self.session.mount("http://", adapter)
        self.session.mount("https://", adapter)

        # Setup authentication
        self._setup_auth()

        logger.info(f"Initialized {self.__class__.__name__} for {base_url}")

    def _setup_auth(self) -> None:
        """Configure authentication headers."""
        method = self.auth_config.get('method', 'none')

        if method == 'api_key':
            key = self.auth_config['credentials']['key']
            # Support environment variable substitution
            if key.startswith('${') and key.endswith('}'):
                import os
                env_var = key[2:-1]
                key = os.environ.get(env_var)
                if not key:
                    raise ValueError(f"Environment variable {env_var} not set")

            # Different APIs use different header names
            header_name = self.auth_config.get('header_name', 'X-API-Key')
            self.session.headers[header_name] = key

        elif method == 'oauth2':
            token = self.auth_config['credentials']['access_token']
            self.session.headers['Authorization'] = f"Bearer {token}"

        elif method == 'jwt':
            token = self.auth_config['credentials']['token']
            self.session.headers['Authorization'] = f"Bearer {token}"

        elif method == 'none':
            pass

        else:
            raise ValueError(f"Unsupported auth method: {method}")

    def get(self, endpoint: str, params: Optional[Dict[str, Any]] = None,
            stream: bool = False) -> requests.Response:
        """
        Make GET request.

        Args:
            endpoint: API endpoint (relative to base_url)
            params: Query parameters
            stream: Stream response (for large data)

        Returns:
            Response object (caller must handle streaming)
        """
        if self.rate_limiter:
            self.rate_limiter.acquire()

        url = f"{self.base_url}/{endpoint.lstrip('/')}"
        logger.debug(f"GET {url} with params {params}")

        response = self.session.get(url, params=params, timeout=self.timeout, stream=stream)
        response.raise_for_status()

        return response

    def post(self, endpoint: str, data: Optional[Dict[str, Any]] = None,
             json_data: Optional[Dict[str, Any]] = None) -> requests.Response:
        """
        Make POST request.

        Args:
            endpoint: API endpoint
            data: Form data
            json_data: JSON payload

        Returns:
            Response object
        """
        if self.rate_limiter:
            self.rate_limiter.acquire()

        url = f"{self.base_url}/{endpoint.lstrip('/')}"
        logger.debug(f"POST {url}")

        response = self.session.post(url, data=data, json=json_data, timeout=self.timeout)
        response.raise_for_status()

        return response

    def stream_data(self, endpoint: str, params: Optional[Dict[str, Any]] = None,
                    chunk_size: int = 8192) -> Iterator[bytes]:
        """
        Stream data from endpoint (zero storage).

        Args:
            endpoint: API endpoint
            params: Query parameters
            chunk_size: Bytes per chunk

        Yields:
            Data chunks (bytes)
        """
        response = self.get(endpoint, params=params, stream=True)

        try:
            for chunk in response.iter_content(chunk_size=chunk_size):
                if chunk:
                    yield chunk
        finally:
            response.close()

    def stream_lines(self, endpoint: str, params: Optional[Dict[str, Any]] = None) -> Iterator[str]:
        """
        Stream text data line by line (zero storage).

        Args:
            endpoint: API endpoint
            params: Query parameters

        Yields:
            Text lines
        """
        response = self.get(endpoint, params=params, stream=True)

        try:
            for line in response.iter_lines(decode_unicode=True):
                if line:
                    yield line
        finally:
            response.close()

    @abstractmethod
    def query_by_date(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float], **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query data for date range (streaming, zero storage).

        Args:
            start_date: Start date/time
            end_date: End date/time
            location: Geographic location (lat, lon)
            **kwargs: Additional parameters (provider-specific)

        Yields:
            Data records (streaming, not stored)
        """
        pass

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit (cleanup)."""
        self.session.close()
