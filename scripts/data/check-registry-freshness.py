# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check an explicit YAML registry; dry-run unless --apply is supplied.

Input schema: a mapping with a resources list of mappings, each containing an
HTTP(S) url. Other fields are retained as YAML values on apply. No production
inventory or scheduler is supplied. Requires PyYAML and the Python standard
library. See docs/data/registry-freshness.md for an offline CLI example.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from datetime import datetime, timezone
from http import HTTPStatus
from http.client import HTTPException
import json
import math
import os
from pathlib import Path
import stat
import sys
import tempfile
from typing import Any, Callable
from urllib.error import HTTPError, URLError
from urllib.parse import urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener

import yaml


@dataclass(frozen=True)
class HTTPResult:
    """Carry the received HTTP status and optional Location header."""

    status: int
    location: str | None = None


class NoRedirect(HTTPRedirectHandler):
    """Expose redirect responses without issuing additional requests."""

    def redirect_request(self, req: Any, fp: Any, code: Any, msg: Any,
                         headers: Any, newurl: Any) -> None:
        """Decline urllib's automatic redirect handling."""
        return None


def head(url: str, timeout: float) -> HTTPResult:
    """Send HEAD with a per-attempt socket timeout, retaining HTTP errors."""
    request = Request(url, method="HEAD")
    opener = build_opener(NoRedirect())
    try:
        with opener.open(request, timeout=timeout) as response:
            return HTTPResult(response.status, response.headers.get("Location"))
    except HTTPError as exc:
        try:
            return HTTPResult(exc.code, exc.headers.get("Location"))
        finally:
            exc.close()


class RegistryLoader(yaml.SafeLoader):
    """Reject duplicate keys and aliases rather than silently losing fields."""

    def compose_node(self, parent: Any, index: Any) -> Any:
        """Reject aliases, including recursive collections."""
        if self.check_event(yaml.AliasEvent):
            raise ValueError("invalid registry: YAML aliases are unsupported")
        return super().compose_node(parent, index)

    def construct_mapping(self, node: Any, deep: bool = False) -> Any:
        """Require unique string keys throughout the document."""
        keys = set()
        for key_node, _ in node.value:
            key = self.construct_object(key_node, deep=deep)
            if not isinstance(key, str) or key in keys:
                raise ValueError("invalid registry: duplicate or non-string key")
            keys.add(key)
        return super().construct_mapping(node, deep=deep)


def read_registry(original: bytes) -> dict[str, Any]:
    """Validate the complete registry before any HTTP request or write."""
    try:
        document = yaml.load(original.decode("utf-8"), Loader=RegistryLoader)
    except (yaml.YAMLError, UnicodeError) as exc:
        raise ValueError("invalid registry: malformed YAML") from exc
    if not isinstance(document, dict) or not isinstance(document.get("resources"), list):
        raise ValueError("invalid registry: expected a resources list")
    for row in document["resources"]:
        if not isinstance(row, dict) or not isinstance(row.get("url"), str):
            raise ValueError("invalid registry: each resource requires a URL string")
        url = row["url"]
        try:
            parts = urlsplit(url)
            valid = (parts.scheme in {"http", "https"} and bool(parts.hostname)
                     and parts.username is None and parts.password is None)
            # Accessing port also validates its syntax and supported range.
            parts.port
        except ValueError as exc:
            raise ValueError("invalid registry: malformed URL") from exc
        if not valid or any(character.isspace() or ord(character) < ord(" ") for character in url):
            raise ValueError("invalid registry: expected an HTTP(S) URL without credentials or whitespace")
    return document


def validate_policy(timeout: float, retries: int) -> None:
    """Require a finite positive timeout and a nonnegative integer retry count."""
    # Zero is the mathematical boundary; retries count *additional* attempts.
    if (isinstance(timeout, bool) or not isinstance(timeout, (int, float))
            or not math.isfinite(timeout) or timeout <= 0):
        raise ValueError("timeout must be a finite positive number")
    if isinstance(retries, bool) or not isinstance(retries, int) or retries < 0:
        raise ValueError("retries must be a nonnegative integer")


def utc_now() -> datetime:
    """Return the current timezone-aware UTC clock value."""
    return datetime.now(timezone.utc)


def check_url(url: str, timeout: float, retries: int,
              transport: Callable[[str, float], HTTPResult], date: str) -> dict[str, Any]:
    """Classify a URL using at most an initial attempt plus the retry count."""
    # HTTP class boundaries and names follow Python HTTPStatus / RFC 9110,
    # section 15. Retryable responses are 429 and the server-error class;
    # this is an explicit checker policy, not a claim of mandated HTTP retries.
    attempts = 0
    while True:
        attempts += 1  # An attempt is one call of the supplied HEAD transport.
        code = None
        location = None
        error = None
        try:
            response = transport(url, timeout)
            code, location = int(response.status), response.location
            if HTTPStatus.OK <= code < HTTPStatus.MULTIPLE_CHOICES:
                status = "alive"
            elif HTTPStatus.MULTIPLE_CHOICES <= code < HTTPStatus.BAD_REQUEST:
                status = "redirected"
            else:
                status = "dead"
            retryable = code == HTTPStatus.TOO_MANY_REQUESTS or code >= HTTPStatus.INTERNAL_SERVER_ERROR
        except HTTPException:
            status = "dead"
            error = "protocol_error"
            retryable = True
        except (OSError, URLError) as exc:
            reason = exc.reason if isinstance(exc, URLError) else exc
            status = "timeout" if isinstance(reason, TimeoutError) else "dead"
            error = "timeout" if status == "timeout" else "connection_error"
            retryable = True
        if not retryable or attempts > retries:
            return {"url": url, "status": status, "http_status": code,
                    "location": location, "attempts": attempts, "error": error,
                    "last_checked": date}


def atomic_apply(path: Path, original: bytes, document: dict[str, Any]) -> None:
    """Replace the registry only after a complete, flushed temporary write."""
    # Same-directory staging is needed for os.replace's atomic rename contract.
    # Serialization is semantic: comments and scalar formatting may change.
    payload = yaml.safe_dump(document, sort_keys=False, allow_unicode=True).encode("utf-8")
    if path.is_symlink():
        raise ValueError("apply requires a regular file rather than a symlink")
    mode = stat.S_IMODE(path.stat().st_mode)
    with tempfile.NamedTemporaryFile(dir=path.parent, prefix=".registry-", delete=False) as stream:
        pending = Path(stream.name)
        try:
            stream.write(payload)
            stream.flush()
            os.fsync(stream.fileno())
            os.fchmod(stream.fileno(), mode)
            # Avoid overwriting an ordinary edit made during a slow check.
            if path.is_symlink() or path.read_bytes() != original:
                raise ValueError("registry changed during checking; apply cancelled")
            os.replace(pending, path)
        finally:
            pending.unlink(missing_ok=True)


def check_registry(path: Path, *, timeout: float, retries: int, apply: bool = False,
                   transport: Callable[[str, float], HTTPResult] | None = None,
                   clock: Callable[[], datetime] | None = None) -> dict[str, Any]:
    """Return sorted per-URL records and totals; optionally update two fields."""
    validate_policy(timeout, retries)
    path = Path(path)
    original = path.read_bytes()
    document = read_registry(original)
    now = (clock or utc_now)()
    if now.tzinfo is None or now.utcoffset() is None:
        raise ValueError("clock must return a timezone-aware datetime")
    date = now.astimezone(timezone.utc).date().isoformat()
    urls = sorted({row["url"] for row in document["resources"]})
    records = [check_url(url, timeout, retries, transport or head, date) for url in urls]
    summary = {status: sum(record["status"] == status for record in records)
               for status in ("alive", "redirected", "dead", "timeout")}
    summary = {"total": len(records), **summary}
    if apply:
        by_url = {record["url"]: record for record in records}
        for row in document["resources"]:
            record = by_url[row["url"]]
            row["last_checked"] = date
            row["download_status"] = ("dead_link" if record["status"] == "dead"
                                      else record["status"])
        atomic_apply(path, original, document)
    return {"applied": apply, "records": records, "summary": summary}


def main(argv: list[str] | None = None) -> int:
    """Print JSON records; require explicit input and timeout/retry policy."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("registry", type=Path, help="explicit YAML file to check")
    parser.add_argument("--timeout", required=True, type=float,
                        help="positive per-attempt socket timeout in seconds")
    parser.add_argument("--retries", required=True, type=int,
                        help="nonnegative additional attempts per URL")
    parser.add_argument("--apply", action="store_true",
                        help="atomically update last_checked and download_status")
    args = parser.parse_args(argv)
    try:
        report = check_registry(args.registry, timeout=args.timeout,
                                retries=args.retries, apply=args.apply)
    except (OSError, ValueError):
        # Do not echo paths, URLs, parser snippets or transport exception text.
        # argparse's error status is documented in the Python argparse API.
        parser.exit(2, "invalid registry, timeout/retry policy, or file operation\n")
    print(json.dumps(report, sort_keys=True))
    return 0  # Successful checking includes reported dead links.


if __name__ == "__main__":
    raise SystemExit(main())
