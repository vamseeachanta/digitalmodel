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


@pytest.hookimpl(wrapper=True)
def pytest_runtest_makereport(item, call):
    report = yield
    if report.failed:
        prefix = f"[CONTRACT VIOLATION] symbol={item.name} au_version={AU_VERSION}\n"
        report.sections.append(("contract", prefix + str(report.longrepr)))
    return report
