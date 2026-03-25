import importlib
import pytest

try:
    AU_VERSION = importlib.metadata.version("assetutilities")
except Exception:
    import assetutilities
    AU_VERSION = assetutilities.__version__


@pytest.mark.contracts
def test_FileManagement_importable():
    from assetutilities.common.file_management import FileManagement
    assert callable(FileManagement)

@pytest.mark.contracts
def test_FileManagement_has_router_method():
    from assetutilities.common.file_management import FileManagement
    assert hasattr(FileManagement, "router") or hasattr(FileManagement, "get_files_in_directory"), \
        f"[CONTRACT VIOLATION] symbol=FileManagement.router au_version={AU_VERSION}: method absent"

@pytest.mark.contracts
def test_update_deep_dictionary_importable():
    from assetutilities.common.update_deep import update_deep_dictionary
    base = {"a": {"b": 1}, "c": 2}
    update = {"a": {"d": 3}, "e": 4}
    result = update_deep_dictionary(base, update)
    assert result["a"]["b"] == 1
    assert result["a"]["d"] == 3
    assert result["c"] == 2

@pytest.mark.contracts
def test_is_file_valid_func_importable():
    from assetutilities.common.utilities import is_file_valid_func
    assert callable(is_file_valid_func)

@pytest.mark.contracts
def test_is_dir_valid_func_importable():
    from assetutilities.common.utilities import is_dir_valid_func
    assert callable(is_dir_valid_func)
