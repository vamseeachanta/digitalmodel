"""Validate dated waivers for the explicitly governed module-size check."""

from datetime import date
from pathlib import Path, PurePosixPath

import yaml

WAIVER_FILE = "scripts/ci/module_size_waivers.yml"
_FIELDS = {"path", "ceiling", "expires", "issue"}


class _WaiverLoader(yaml.SafeLoader):
    """Reject duplicate mapping fields before YAML can discard their values."""


def _unique_mapping(loader, node):
    loader.flatten_mapping(node)
    result = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node)
        if key in result:
            raise ValueError(f"duplicate field {key}")
        result[key] = loader.construct_object(value_node)
    return result


_WaiverLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _unique_mapping
)


def _read_entries(root):
    try:
        text = (root / WAIVER_FILE).read_text(encoding="utf-8")
    except OSError as exc:
        raise ValueError(f"waiver file {WAIVER_FILE} missing or unreadable") from exc
    try:
        entries = yaml.load(text, Loader=_WaiverLoader)
    except (yaml.YAMLError, ValueError, TypeError) as exc:
        raise ValueError(f"waiver file {WAIVER_FILE}: invalid YAML ({exc})") from exc
    if not isinstance(entries, list):
        raise ValueError(f"waiver file {WAIVER_FILE} must contain a list")
    return entries


def _expiry(value, label):
    if type(value) is date:
        return value
    if isinstance(value, str):
        try:
            parsed = date.fromisoformat(value)
            if parsed.isoformat() == value:
                return parsed
        except ValueError:
            pass
    raise ValueError(f"{label}: expires must be an ISO date (YYYY-MM-DD)")


def _validate_record(record, number):
    if not isinstance(record, dict):
        raise ValueError(f"waiver entry {number} must be a mapping")
    relative = record.get("path", "<missing path>")
    label = f"waiver entry {number} ({relative})"
    missing, extra = _FIELDS - record.keys(), record.keys() - _FIELDS
    if missing or extra:
        raise ValueError(f"{label}: missing fields {sorted(missing)}; "
                         f"unexpected fields {sorted(extra)}")
    if not isinstance(relative, str) or not relative:
        raise ValueError(f"{label}: path must be repository-relative")
    path = PurePosixPath(relative)
    if (path.is_absolute() or ".." in path.parts or "\\" in relative
            or ":" in relative or path.as_posix() != relative or relative == "."):
        raise ValueError(f"{label}: path must be canonical and repository-relative")
    for field in ("ceiling", "issue"):
        if type(record[field]) is not int or record[field] <= 0:
            raise ValueError(f"{label}: {field} must be a positive integer")
    return relative, _expiry(record["expires"], label), label


def _validate_file(root, relative, governed, limit, record, expires, today, label):
    if relative not in governed:
        raise ValueError(f"{label}: file is not governed")
    path = root / relative
    if not path.resolve().is_relative_to(root.resolve()):
        raise ValueError(f"{label}: file is outside repository")
    if not path.is_file():
        raise ValueError(f"{label}: stale waiver; file is missing")
    lines = len(path.read_text(encoding="utf-8").splitlines())
    if lines <= limit:
        raise ValueError(f"{label}: stale waiver; {lines} lines is at or below {limit}")
    if today >= expires:
        raise ValueError(f"{label}: waiver expired on {expires.isoformat()}; "
                         f"follow-up issue #{record['issue']}")
    if lines > record["ceiling"]:
        raise ValueError(f"{label}: {lines} lines exceeds ceiling {record['ceiling']}")


def load_waivers(root: Path, governed, limit: int, *, today: date | None = None):
    """Return ceilings only after every entry satisfies the dated-waiver rules."""
    today = date.today() if today is None else today
    ceilings = {}
    for number, record in enumerate(_read_entries(root), 1):
        relative, expires, label = _validate_record(record, number)
        if relative in ceilings:
            raise ValueError(f"{label}: duplicate path")
        _validate_file(root, relative, governed, limit, record, expires, today, label)
        ceilings[relative] = record["ceiling"]
    return ceilings
