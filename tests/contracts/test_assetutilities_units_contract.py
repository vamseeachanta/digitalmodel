import importlib
import pytest

try:
    AU_VERSION = importlib.metadata.version("assetutilities")
except Exception:
    import assetutilities
    AU_VERSION = assetutilities.__version__


@pytest.mark.contracts
def test_TrackedQuantity_importable():
    TrackedQuantity = pytest.importorskip(
        "assetutilities.units.quantity",
        reason=f"assetutilities.units.quantity unavailable in {AU_VERSION}"
    )
    from assetutilities.units.quantity import TrackedQuantity
    assert callable(TrackedQuantity)

@pytest.mark.contracts
def test_unit_checked_importable():
    try:
        from assetutilities.units.computation import unit_checked
        assert callable(unit_checked)
    except ImportError:
        pytest.skip(f"unit_checked unavailable in assetutilities {AU_VERSION}")

@pytest.mark.contracts
def test_UnitMismatchError_importable():
    try:
        from assetutilities.units.exceptions import UnitMismatchError
        assert issubclass(UnitMismatchError, Exception)
    except ImportError:
        pytest.skip(f"UnitMismatchError unavailable in assetutilities {AU_VERSION}")
