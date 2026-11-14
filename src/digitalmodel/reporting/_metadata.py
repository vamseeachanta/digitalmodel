"""Package edition for report-producing call sites."""

from importlib.metadata import PackageNotFoundError, version


def package_code_edition() -> str:
    """Return the installed package edition, or the fallback 'unknown'."""
    try:
        return version("digitalmodel")
    except PackageNotFoundError:
        return "unknown"
