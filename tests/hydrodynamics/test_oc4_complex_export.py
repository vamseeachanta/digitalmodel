# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Complex exports exercise an adapter fake, without a licensed import."""

import copy
import importlib.util
import json
from pathlib import Path

import numpy as np
import pytest

EXAMPLE = Path(__file__).resolve().parents[2] / "examples/hydrodynamics/oc4-semisub"


def wrapper():
    spec = importlib.util.spec_from_file_location("complex_export_v2", EXAMPLE/"complex_export_v2.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def sample():
    return dict(omega_rad_s=np.array([0.35, 3.5]), headings_deg=np.array([-90, 0, 90]),
                added_mass=np.arange(72).reshape(2, 6, 6), damping=np.ones((2, 6, 6)),
                motion=np.full((3, 2, 6), 1+2j),
                load_haskind=np.full((3, 2, 6), 3-4j),
                load_diffraction=np.full((3, 2, 6), -5+6j),
                metadata=dict(axes=["surge", "sway", "heave", "roll", "pitch", "yaw"],
                    origin_m=[0,0,0], wave_reference_origin_m=[0,0,0],
                    coordinate_frame="Right handed; z up; x along the reference axis",
                    heading_convention="Propagation direction, counterclockwise from positive x",
                    phase_time_sign=1,
                    phase_convention="Re{RAO exp(+i omega t)} per unit incident wave amplitude at wave_reference_origin_m",
                    motion_units=["m/m"]*3+["rad/m"]*3,
                    load_units=["N/m"]*3+["N*m/m"]*3))


class FakeAdapter:
    def __init__(self, data=None, damage=None):
        self.data = sample() if data is None else data
        self.damage = damage
        self.calls = []

    def snapshot(self, origin_m):
        self.calls.append("snapshot")
        assert origin_m == [0,0,0]
        return copy.deepcopy(self.data)

    def save_native(self, path):
        self.calls.append("save")
        path.write_bytes(b"fake native result, no solver")

    def reload_native(self, path):
        self.calls.append("reload")
        assert path.read_bytes() == b"fake native result, no solver"
        reloaded = copy.deepcopy(self.data)
        if self.damage in ("motion", "load_haskind", "load_diffraction"):
            reloaded[self.damage][0,0,0] = np.conj(reloaded[self.damage][0,0,0])
        elif self.damage in ("omega_rad_s", "headings_deg", "added_mass", "damping"):
            reloaded[self.damage].flat[0] += 1
        elif self.damage == "metadata":
            reloaded["metadata"]["wave_reference_origin_m"][0] = 1
        return FakeAdapter(reloaded)


def test_complex_motion_and_loads_survive_native_reload_and_json(tmp_path):
    native, output = tmp_path/"result.native", tmp_path/"complex.json"
    adapter = FakeAdapter()
    wrapper().export_verified(adapter, native, output, origin_m=[0,0,0])
    exported = json.loads(output.read_text())
    assert exported["native_reload_verified"] is True
    assert exported["motion"]["real"][0][0][0] == 1
    assert exported["motion"]["imag"][0][0][0] == 2
    assert exported["load_haskind"]["imag"][0][0][0] == -4
    assert exported["load_diffraction"]["real"][0][0][0] == -5
    assert exported["metadata"] == sample()["metadata"]
    assert exported["array_axes"] == ["heading", "frequency", "dof"]
    assert adapter.calls == ["snapshot", "save", "reload"]


@pytest.mark.parametrize("damage", ["motion", "load_haskind", "load_diffraction", "omega_rad_s", "headings_deg", "added_mass", "damping", "metadata"])
def test_every_native_reload_mismatch_blocks_export(tmp_path, damage):
    output = tmp_path/"complex.json"
    with pytest.raises(ValueError, match="reload"):
        wrapper().export_verified(FakeAdapter(damage=damage), tmp_path/"result.native", output, origin_m=[0,0,0])
    assert not output.exists()


@pytest.mark.parametrize("damage", ["nan", "magnitude", "shape", "units", "phase", "origin", "duplicate_frequency", "missing_axes"])
def test_invalid_or_ambiguous_result_blocks_export(tmp_path, damage):
    data = sample()
    if damage == "nan":
        data["motion"][0,0,0] = complex(float("nan"), 1)
    elif damage == "magnitude":
        data["motion"] = abs(data["motion"])
    elif damage == "shape":
        data["motion"] = data["motion"][:2]
    elif damage == "units":
        data["metadata"]["load_units"] = ["kN/m"]*6
    elif damage == "phase":
        data["metadata"]["phase_time_sign"] = 0
    elif damage == "origin":
        data["metadata"]["origin_m"][0] = 1
    elif damage == "duplicate_frequency":
        data["omega_rad_s"][1] = data["omega_rad_s"][0]
    else:
        del data["metadata"]["axes"]
    with pytest.raises(ValueError):
        wrapper().export_verified(FakeAdapter(data), tmp_path/"result.native", tmp_path/"complex.json", origin_m=[0,0,0])
