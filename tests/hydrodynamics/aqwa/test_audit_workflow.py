"""Exercise the real DAT validators and workflow with synthetic LIS payloads."""
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.aqwa.aqwa_dat_files import AqwaDATFiles
from digitalmodel.hydrodynamics.aqwa.viscous_damping_determination import ViscousDampingDetermination
from digitalmodel.marine_ops.marine_analysis.aqwa_enhanced_parser import AQWAEnhancedParser


class SyntheticLIS:
    def parse_lis_file(self, path):
        return {"frequencies": np.array([0.2, 0.5, 0.8]), "headings": [0.],
                "raos": {d: {"amplitude": np.ones((3, 1)), "unit": u}
                         for d, u in (("heave", "m/m"), ("roll", "rad/m"), ("pitch", "rad/m"))}}

    def matrix(self, value):
        return {"frequencies": np.array([0.2, 0.5, 0.8]), "matrices": np.array([np.eye(6)*value]*3)}

    def extract_damping_matrices(self, path):
        return self.matrix(1e7)

    def extract_added_mass_matrices(self, path):
        return self.matrix(2e8)

    def extract_added_mass_at_infinite_frequency(self, path):
        return {"diagonal": {f"A{k}": 2e8 for k in (33, 44, 55)}}

    def extract_drag_table(self, path):
        return []

    def extract_external_damping(self, path):
        return {}

    def extract_percent_critical_damping(self, path):
        return {}

    def extract_restoring_coefficients(self, path):
        return {"diagonal": {f"C{k}": 2e8 for k in (33, 44, 55)}}


def case(tmp_path, node=98000):
    line = AqwaDATFiles().get_dc_01_body({}, {"data": {"raw": [
        {"structure": 1, "node": node, "X": 0., "Y": 0., "Z": 0.}
    ]}})[0]
    dat, lis = tmp_path / "input.dat", tmp_path / "input.lis"
    dat.write_text("DECK 0\nOPTIONS LDRG\nDECK 1\n" + line)
    lis.write_text("Synthetic fixture; no solver execution")
    dataset = {"id": "case", "manifest": [{"id": "record", "dat": dat, "lis": lis}]}
    config = {"context": {"wave_amplitude": {"value": 1., "unit": "m"}, "cog_element": node},
              "hydrostatics": {"inertia": {f"I{k}": 1e9 for k in (33, 44, 55)}}}
    module = ViscousDampingDetermination()
    module.lis_parser = SyntheticLIS()
    return module, dataset, config


def test_config_inertia_fills_missing_deck4(tmp_path):
    module, dataset, config = case(tmp_path)
    record = module._process_case(dataset, config)["records"][0]
    assert record["inertia"]["I44"] == 1e9
    assert record["critical"]["infinite_frequency"]["roll"]["zeta_at_natural_frequency"] == pytest.approx(0.0102062073)


def test_missing_inertia_raises(tmp_path):
    module, dataset, config = case(tmp_path)
    config["hydrostatics"] = {}
    with pytest.raises(ValueError, match="I33|I44|inertia"):
        module._process_case(dataset, config)


def test_configured_cog_is_passed(tmp_path):
    module, dataset, config = case(tmp_path, node=97000)
    record = module._process_case(dataset, config)["records"][0]
    assert record["inspection"].deck_one.cog_element == 97000


def test_lis_pages_and_units():
    path = Path(__file__).resolve().parents[3] / "docs/domains/aqwa/examples/03_dat/001_ship_raos/001_SHIP_RAOS.LIS"
    result = AQWAEnhancedParser().parse_lis_file(str(path))
    assert result["frequencies"].tolist() == [0.1, 0.407, 0.715, 1.022, 1.33, 1.637, 1.945, 2.252]
    assert result["raos"]["heave"]["amplitude"][0, 0] == pytest.approx(0.9973)
    assert result["raos"]["roll"]["unit"] is None
    assert "(RAD/S)" in result["units_rows"][0]


def test_multibody_lis_requires_and_honours_structure_selector():
    from digitalmodel.marine_ops.marine_analysis.aqwa_enhanced_parser import AQWAFileError
    root = Path(__file__).resolve().parents[3]
    fixtures = sorted((root / "docs/domains/orcaflex/aqwa/to_orcaflex").rglob("*.LIS"))
    assert len(fixtures) == 2
    for path in fixtures:
        parser = AQWAEnhancedParser()
        with pytest.raises(AQWAFileError, match="structure"):
            parser.parse_lis_file(str(path))
        first = parser.parse_lis_file(str(path), structure=1)
        second = parser.parse_lis_file(str(path), structure=2)
        for sid, payload in ((1, first), (2, second)):
            assert payload["structure"] == sid
            assert payload["headings"].tolist() == list(range(-180, 181, 15))
            assert all(np.isfinite(entry["amplitude"]).all() for entry in payload["raos"].values())
        assert not np.array_equal(first["raos"]["heave"]["amplitude"], second["raos"]["heave"]["amplitude"])
        with pytest.raises(AQWAFileError, match="structure"):
            parser.parse_lis_file(str(path), structure=3)
