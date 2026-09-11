"""Native exports may add fields; every selected physical value stays required."""
from types import SimpleNamespace

import pytest
import yaml

from digitalmodel.solvers.smoke.model_data_readback import verify_export


def test_nested_cylinder_subset_accepts_additional_native_metadata(tmp_path):
    data = {"6DBuoys": [{"Name": "buoy", "Cylinders": [
        {"UnitDampingForce": [0, 2], "OtherNativeField": 3}]}]}
    model = SimpleNamespace(SaveData=lambda path: path.write_text(yaml.safe_dump(data)))
    expected = {"6DBuoys": {"buoy": {"Cylinders": [{"UnitDampingForce": [0, 2]}]}}}
    assert len(verify_export(model, tmp_path, expected)) == 64


@pytest.mark.parametrize("cylinders", [[], [{"UnitDampingForce": [0, 3]}], [{}]])
def test_nested_cylinder_subset_rejects_changed_missing_values(tmp_path, cylinders):
    data = {"6DBuoys": [{"Name": "buoy", "Cylinders": cylinders}]}
    model = SimpleNamespace(SaveData=lambda path: path.write_text(yaml.safe_dump(data)))
    expected = {"6DBuoys": {"buoy": {"Cylinders": [{"UnitDampingForce": [0, 2]}]}}}
    with pytest.raises(ValueError):
        verify_export(model, tmp_path, expected)
