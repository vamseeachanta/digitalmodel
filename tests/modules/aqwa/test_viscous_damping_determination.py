import sys
import types
from pathlib import Path

import numpy as np
import pytest


_custom_pkg = types.ModuleType("digitalmodel.custom")
_aqwa_pkg = types.ModuleType("digitalmodel.custom.aqwa")
_aqwa_raos = types.ModuleType("digitalmodel.custom.aqwa.aqwa_analysis_raos")
_aqwa_raos.AqwaRAOs = object
_aqwa_utilities = types.ModuleType("digitalmodel.custom.aqwa.aqwa_utilities")
_aqwa_utilities.AqwaUtilities = object

sys.modules.setdefault("digitalmodel.custom", _custom_pkg)
sys.modules.setdefault("digitalmodel.custom.aqwa", _aqwa_pkg)
sys.modules.setdefault("digitalmodel.custom.aqwa.aqwa_analysis_raos", _aqwa_raos)
sys.modules.setdefault("digitalmodel.custom.aqwa.aqwa_utilities", _aqwa_utilities)
setattr(_custom_pkg, "aqwa", _aqwa_pkg)
setattr(_aqwa_pkg, "aqwa_analysis_raos", _aqwa_raos)
setattr(_aqwa_pkg, "aqwa_utilities", _aqwa_utilities)


from digitalmodel.modules.aqwa.aqwa_dat_files import (
    DATDeckOneMetadata,
    DATFrequencyGrid,
    DATInspectionResult,
)
from digitalmodel.modules.aqwa.viscous_damping_determination import (
    ViscousDampingDetermination,
)


class _FakeDATParser:
    def __init__(self, inertia=None, restoring=None, mass=500.0):
        self._inspection = DATInspectionResult(
            dat_path="dummy",
            decks={1: [], 3: [], 4: [], 5: []},
            options={"LDRG": True},
            frequency_grid=DATFrequencyGrid(frequencies=[1.0], periods=[1.0]),
            drag_matrix=None,
            deck_one=DATDeckOneMetadata(
                cog_element=98000,
                cog_coordinates=(0.0, 0.0, 0.0),
                raw_lines=[],
            ),
            mass=mass,
            inertia=inertia or {"I44": 200.0, "I55": 300.0},
            restoring=restoring or {"C33": 10.0, "C44": 20.0, "C55": 30.0},
        )

    def inspect_dat_file(self, _):
        return self._inspection

    def validate_ldrg_option(self, _):
        return None

    def validate_cog_element(self, *_args, **_kwargs):
        return None


class _FakeLISParser:
    def __init__(self, restoring_diag=None):
        self._restoring_diag = restoring_diag or {}

    def parse_lis_file(self, _):
        zero = np.zeros((1, 1))
        raos = {dof: {"amplitude": zero, "phase": zero} for dof in [
            "surge",
            "sway",
            "heave",
            "roll",
            "pitch",
            "yaw",
        ]}
        return {
            "frequencies": np.array([1.0]),
            "headings": np.array([0.0]),
            "raos": raos,
        }

    def _matrix_payload(self, value):
        matrix = np.zeros((1, 6, 6))
        matrix[0, 2, 2] = value + 5.0
        matrix[0, 3, 3] = value + 6.0
        matrix[0, 4, 4] = value + 7.0
        return {
            "frequencies": np.array([1.0]),
            "matrices": matrix,
        }

    def extract_damping_matrices(self, *_):
        return self._matrix_payload(0.0)

    def extract_added_mass_matrices(self, *_):
        return self._matrix_payload(10.0)

    def extract_added_mass_at_infinite_frequency(self, *_):
        return {"diagonal": {"A33": 1.0, "A44": 2.0, "A55": 3.0}}

    def extract_drag_table(self, *_):
        return []

    def extract_external_damping(self, *_):
        zero = np.zeros(1)
        return {"B33": zero, "B44": zero, "B55": zero}

    def extract_percent_critical_damping(self, *_):
        zero = np.zeros(1)
        return {"frequencies": np.array([1.0]), "B33": zero, "B44": zero, "B55": zero}

    def extract_restoring_coefficients(self, *_):
        return {"diagonal": dict(self._restoring_diag)}


def _run_case(lis_diag, dat_restoring):
    module = ViscousDampingDetermination()
    module.dat_parser = _FakeDATParser(restoring=dat_restoring)
    module.lis_parser = _FakeLISParser(restoring_diag=lis_diag)

    dataset = {
        "id": "case",
        "description": "",
        "manifest": [
            {
                "id": "record",
                "vessel": "VES",
                "draft": "MWL",
                "dat": Path("dummy.dat"),
                "lis": Path("dummy.lis"),
            }
        ],
        "scratch": Path("."),
        "case_dir": Path("."),
    }

    cfg = {
        "context": {"fluid_density": 1025.0},
        "hydrostatics": {
            "inertia": {"fallback": {"I33": 999.0}},
            "restoring": {"fallback": {"C33": 888.0}},
        },
    }

    results = module._process_case(dataset, cfg)
    return results["records"][0]


def test_restoring_prioritizes_lis_values():
    lis_diag = {"C33": 1111.0, "C44": 2222.0, "C55": 3333.0}
    dat_restoring = {"C33": 10.0, "C44": 20.0, "C55": 30.0}

    record = _run_case(lis_diag, dat_restoring)

    assert record["restoring"]["C33"] == pytest.approx(1111.0)
    assert record["restoring"]["C44"] == pytest.approx(2222.0)
    assert record["restoring"]["C55"] == pytest.approx(3333.0)


def test_restoring_falls_back_to_dat_when_lis_missing():
    lis_diag = {}
    dat_restoring = {"C33": 101.0, "C44": 202.0, "C55": 303.0}

    record = _run_case(lis_diag, dat_restoring)

    assert record["restoring"]["C33"] == pytest.approx(101.0)
    assert record["restoring"]["C44"] == pytest.approx(202.0)
    assert record["restoring"]["C55"] == pytest.approx(303.0)
