"""Exercise the real DAT validators and workflow with synthetic LIS payloads."""
import os
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


@pytest.mark.parametrize("configured", [False, True])
def test_cog_evidence_distinguishes_configuration_from_default(tmp_path, configured):
    module, dataset, config = case(tmp_path)
    if not configured:
        del config["context"]["cog_element"]
    gate = module._process_case(dataset, config)["records"][0]["qa_gates"]["AQ-01"]
    assert gate["evidence"]["cog_source"] == ("configured" if configured else "default")
    assert gate["checks"]["expected_cog_matches"] is True


def test_lis_pages_and_units():
    path = Path(__file__).resolve().parents[3] / "docs/domains/aqwa/examples/03_dat/001_ship_raos/001_SHIP_RAOS.LIS"
    result = AQWAEnhancedParser().parse_lis_file(str(path))
    assert result["frequencies"].tolist() == [0.1, 0.407, 0.715, 1.022, 1.33, 1.637, 1.945, 2.252]
    assert result["raos"]["heave"]["amplitude"][0, 0] == pytest.approx(0.9973)
    assert result["raos"]["roll"]["unit"] is None
    assert "(RAD/S)" in result["units_rows"][0]


def _synthetic_two_body_lis(path: Path) -> Path:
    """A minimal two-structure AQWA listing with displacement RAO pages.

    Generic values, not taken from any project: 3 frequencies, headings -180
    to 180 deg in 15 deg steps, and a different heave RAO per structure. Rows
    follow the listing layout (period, frequency, direction, then amplitude and
    phase for X, Y, Z, RX, RY, RZ; repeat headings omit period and frequency).
    """
    lines = []
    for sid, heave_scale in ((1, 1.0), (2, 0.5)):
        lines += [
            "1",
            "        H Y D R O D Y N A M I C   P A R A M E T E R S   F O R   "
            f"S T R U C T U R E   {sid}",
            "",
            "                     R.A.O.S-VARIATION WITH WAVE DIRECTION",
            "  PERIOD   FREQ   DIRECTION    X          Y          Z"
            "          RX         RY         RZ",
            "  (SECS)  (RAD/S)  (DEGREES)  AMP PHASE  ...  (DEG/M)",
        ]
        for k, freq in enumerate((0.2, 0.6, 1.0)):
            period = 2.0 * np.pi / freq
            for j, heading in enumerate(range(-180, 181, 15)):
                heave = heave_scale * (1.0 - 0.3 * k) * (1.0 + 0.001 * j)
                values = [0.5, 10.0, 0.4, 20.0, heave, 30.0,
                          1.0, 40.0, 0.8, 50.0, 0.2, 60.0]
                body = " ".join(f"{v:9.4f}" for v in values)
                if j == 0:
                    lines.append(f"{period:8.2f}{freq:8.3f}{heading:11.2f} {body}")
                else:
                    lines.append(f"{'':16s}{heading:11.2f} {body}")
        lines.append("")
    path.write_text("\n".join(lines), encoding="utf-8")
    return path


def _multibody_listings(tmp_path: Path) -> list[Path]:
    fixtures = [_synthetic_two_body_lis(tmp_path / "two_body.LIS")]
    # A project listing kept in the private data repository can be added.
    private = os.environ.get("DM_AQWA_MULTIBODY_LIS", "")
    if private and Path(private).is_file():
        fixtures.append(Path(private))
    return fixtures


def test_multibody_lis_requires_and_honours_structure_selector(tmp_path):
    from digitalmodel.marine_ops.marine_analysis.aqwa_enhanced_parser import AQWAFileError
    for path in _multibody_listings(tmp_path):
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
