"""Restoring precedence through actual DAT parsing and G0 validation."""
import numpy as np
import pytest

from tests.hydrodynamics.aqwa.test_audit_workflow import SyntheticLIS, case


class PrecedenceLIS(SyntheticLIS):
    def __init__(self, restoring):
        self.restoring = restoring

    def parse_lis_file(self, path):
        result = super().parse_lis_file(path)
        result["frequencies"] = np.array([1e-6, 0.5, 10.])
        return result

    def matrix(self, value):
        result = super().matrix(value)
        result["frequencies"] = np.array([1e-6, 0.5, 10.])
        return result

    def extract_restoring_coefficients(self, path):
        return {"diagonal": self.restoring}


def run_case(tmp_path, lis_diagonal, dat_diagonal):
    module, dataset, config = case(tmp_path)
    module.lis_parser = PrecedenceLIS(lis_diagonal)
    matrix = np.eye(6)
    for index, key in ((2, "C33"), (3, "C44"), (4, "C55")):
        matrix[index, index] = dat_diagonal[key]
    dat = dataset["manifest"][0]["dat"]
    with dat.open("a") as stream:
        stream.write("\nDECK 5\n" + " ".join(str(value) for value in matrix.ravel()))
    return module._process_case(dataset, config)["records"][0]


def test_restoring_prioritizes_lis_values(tmp_path):
    lis = {"C33": 1111., "C44": 2222., "C55": 3333.}
    record = run_case(tmp_path, lis, {"C33": 10., "C44": 20., "C55": 30.})
    assert record["restoring"] == pytest.approx(lis)


def test_restoring_falls_back_to_dat_when_lis_missing(tmp_path):
    dat = {"C33": 101., "C44": 202., "C55": 303.}
    record = run_case(tmp_path, {}, dat)
    assert record["restoring"] == pytest.approx(dat)
