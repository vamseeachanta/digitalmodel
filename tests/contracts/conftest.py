import importlib.metadata
import pytest

try:
    AU_VERSION = importlib.metadata.version("assetutilities")
except Exception:
    import assetutilities
    AU_VERSION = assetutilities.__version__


@pytest.fixture
def au_version():
    """Return the installed assetutilities version string."""
    try:
        return importlib.metadata.version("assetutilities")
    except Exception:
        import assetutilities
        return assetutilities.__version__


def pytest_configure(config):
    config.addinivalue_line(
        "markers",
        "contracts: Contract tests asserting assetutilities public API stability",
    )


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item, call):
    outcome = yield
    report = outcome.get_result()
    if report.failed:
        longrepr_str = str(report.longrepr)
        prefix = (
            f"[CONTRACT VIOLATION] symbol={item.name} au_version={AU_VERSION}\n"
        )
        # Prepend CONTRACT VIOLATION context as a custom report section;
        # avoid mutating longrepr directly to stay compatible with pytest-html.
        report.sections.append(
            ("contract", prefix + longrepr_str)
        )
