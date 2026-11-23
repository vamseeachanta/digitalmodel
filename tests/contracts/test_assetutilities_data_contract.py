import importlib
import inspect
import pytest

try:
    AU_VERSION = importlib.metadata.version("assetutilities")
except Exception:
    import assetutilities
    AU_VERSION = assetutilities.__version__


@pytest.mark.contracts
def test_SaveData_importable():
    from assetutilities.common.data import SaveData
    assert callable(SaveData)

@pytest.mark.contracts
def test_SaveData_instantiable():
    from assetutilities.common.data import SaveData
    s = SaveData()
    assert s is not None, \
        f"[CONTRACT VIOLATION] symbol=SaveData au_version={AU_VERSION}: not instantiable"

@pytest.mark.contracts
def test_ReadData_importable():
    from assetutilities.common.data import ReadData
    assert callable(ReadData)

@pytest.mark.contracts
def test_AttributeDict_importable():
    from assetutilities.common.data import AttributeDict
    d = AttributeDict({"key": "value"})
    assert d["key"] == "value"
    assert d.key == "value"

@pytest.mark.contracts
def test_Transform_importable():
    from assetutilities.common.data import Transform
    assert callable(Transform)
