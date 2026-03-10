import importlib
import pytest

try:
    AU_VERSION = importlib.metadata.version("assetutilities")
except Exception:
    import assetutilities
    AU_VERSION = assetutilities.__version__


@pytest.mark.contracts
def test_WorkingWithYAML_importable():
    from assetutilities.common.yml_utilities import WorkingWithYAML
    assert callable(WorkingWithYAML)

@pytest.mark.contracts
def test_WorkingWithYAML_has_read_method():
    from assetutilities.common.yml_utilities import WorkingWithYAML
    assert hasattr(WorkingWithYAML, "yml_read_stream") or hasattr(WorkingWithYAML, "ymlInput") or hasattr(WorkingWithYAML, "router"), \
        f"[CONTRACT VIOLATION] symbol=WorkingWithYAML.yml_read_stream au_version={AU_VERSION}: method absent"

@pytest.mark.contracts
def test_WorkingWithYAML_has_update_method():
    from assetutilities.common.yml_utilities import WorkingWithYAML
    assert hasattr(WorkingWithYAML, "update_deep") or hasattr(WorkingWithYAML, "save_diff_files"), \
        f"[CONTRACT VIOLATION] symbol=WorkingWithYAML.update_deep au_version={AU_VERSION}: method absent"

@pytest.mark.contracts
def test_ymlInput_importable():
    """ymlInput is a provisional legacy alias — skip if removed."""
    ymlInput = pytest.importorskip("assetutilities.common.yml_utilities", reason="ymlInput module unavailable")
    try:
        from assetutilities.common.yml_utilities import ymlInput
        assert callable(ymlInput)
    except ImportError:
        pytest.skip(f"ymlInput removed in assetutilities {AU_VERSION} — update consumer imports")
