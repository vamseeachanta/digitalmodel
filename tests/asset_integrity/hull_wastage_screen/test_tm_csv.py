"""Representative-element CSV intake; synthetic inputs only."""
from copy import deepcopy
import hashlib
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.hull_wastage_screen.workflow import router

ROOT = Path(__file__).resolve().parents[3]
HEADER = "id,as_built_mm,measured_mm,uncertainty_mm,corrosion_mm,voluntary_mm"
ROW = "plate,12,8.4,0.1,3,1"
CSV = HEADER + "\n" + ROW + "\n"


def read(path, family="csr"):
    from digitalmodel.hull_wastage_screen.tm_csv import read_elements_csv
    return read_elements_csv(path, family)


@pytest.mark.parametrize("newline,bom", [("\n", False), ("\r\n", True)])
@pytest.mark.parametrize("family", ["csr", "percentage"])
def test_exact_bytes_and_explicit_units(tmp_path, newline, bom, family):
    text = CSV if family == "csr" else (
        "id,as_built_mm,measured_mm,uncertainty_mm,renewal_mm\nplate,12,8.4,0.1,8\n")
    raw = (("\ufeff" if bom else "") + text.replace("\n", newline)).encode("utf-8")
    path = tmp_path / "elements.csv"
    path.write_bytes(raw)
    result = read(path, family)
    expected = dict(id="plate", as_built_mm=12., measured_mm=8.4, uncertainty_mm=.1)
    if family == "csr":
        expected.update(corrosion_mm=3., voluntary_mm=1.)
    else:
        expected.update(renewal_mm=8.)
    assert result["elements"] == [expected]
    assert result["provenance"] == {
        "schema": "hull-wastage-elements/v1", "units": "mm",
        "sha256": hashlib.sha256(raw).hexdigest(), "record_count": 1,
    }
    assert path.read_bytes() == raw
    assert str(tmp_path) not in json.dumps(result["provenance"])


@pytest.mark.parametrize("text,diagnostic", [
    ("", "row 1.*header"),
    (HEADER + "\n", "row 2.*records"),
    (HEADER + ",id\n" + ROW, "row 1.*header"),
    (HEADER.replace("measured_mm", "measured_in") + "\n" + ROW, "row 1.*header"),
    (HEADER.replace(",voluntary_mm", "") + "\n" + ROW, "row 1.*header"),
    (HEADER + "\n" + ROW + ",extra", "row 2.*columns"),
    (HEADER + "\nplate,12,8.4,0.1,3", "row 2.*columns"),
    (HEADER + "\n\n" + ROW, "row 2.*columns"),
    (HEADER + "\n" + ROW.replace("8.4", ""), "row 2.*measured_mm"),
    (HEADER + "\n" + ROW.replace("8.4", "NaN"), "row 2.*measured_mm"),
    (HEADER + "\n" + ROW.replace("8.4", "inf"), "row 2.*measured_mm"),
    (HEADER + "\n" + ROW.replace("8.4", "1e999"), "row 2.*measured_mm"),
    (HEADER + "\n" + ROW.replace("8.4", '"8,4"'), "row 2.*measured_mm"),
    (HEADER + "\n" + ROW.replace("8.4", "1_000"), "row 2.*measured_mm"),
    (HEADER + "\n" + ROW.replace("8.4", "-1"), "row 2.*measured_mm"),
    (HEADER + "\n" + ROW.replace("12", "0"), "row 2.*as_built_mm"),
    (HEADER + "\n" + ROW.replace("plate", "  "), "row 2.*id"),
    (CSV + ROW, "row 3.*id"),
    (HEADER + '\n"unterminated', "row 2.*csv"),
])
def test_invalid_csv_has_row_and_field(tmp_path, text, diagnostic):
    path = tmp_path / "invalid.csv"
    path.write_text(text, encoding="utf-8")
    with pytest.raises(ValueError, match=diagnostic):
        read(path)


@pytest.mark.parametrize("family", ["percentage", "unknown"])
def test_family_header_mismatch(tmp_path, family):
    path = tmp_path / "elements.csv"
    path.write_text(CSV, encoding="utf-8")
    with pytest.raises(ValueError, match="header|rule_family"):
        read(path, family)


def test_invalid_encoding(tmp_path):
    path = tmp_path / "invalid.csv"
    path.write_bytes(b"\xff")
    with pytest.raises(ValueError, match="row 1.*encoding"):
        read(path)


@pytest.mark.parametrize("path", ["https://example.invalid/data.csv", "//share/data.csv",
                                  chr(92) * 2 + "share" + chr(92) + "data.csv", "", None, 5])
def test_only_explicit_local_paths(path):
    with pytest.raises(ValueError, match="local file"):
        read(path)


def test_header_order_and_quoted_ids_are_preserved(tmp_path):
    path = tmp_path / "elements.csv"
    path.write_text('voluntary_mm,id,corrosion_mm,uncertainty_mm,measured_mm,as_built_mm\n'
                    '1,"plate, A",3,.1,8.4,1.2e1\n', encoding="utf-8")
    item = read(path)["elements"][0]
    assert item["id"] == "plate, A"
    assert item["as_built_mm"] == 12.


@pytest.mark.parametrize("metadata", ["directory", "file", "both", "none", "absolute"])
@pytest.mark.parametrize("family", ["csr", "percentage"])
def test_import_matches_inline_and_preserves_inputs(tmp_path, monkeypatch, metadata, family):
    config_dir = tmp_path / "config"
    config_dir.mkdir()
    other = tmp_path / "other"
    other.mkdir()
    cfg = yaml.safe_load((ROOT / "examples/workflows/hull-wastage-screen/input.yml").read_text())
    block = cfg["hull_wastage_screen"]
    block["rule_family"] = family
    if family == "percentage":
        block["limits"] = {"substantial_fraction": .75, "source": "Synthetic selected limit"}
        block["elements"][0] = dict(id="plate", as_built_mm=12., measured_mm=8.4,
                                    uncertainty_mm=.1, renewal_mm=8.)
    block["output_dir"] = str(tmp_path / "inline")
    inline = router(deepcopy(cfg))["hull_wastage_screen"]["result"]
    raw = (CSV if family == "csr" else
           "id,as_built_mm,measured_mm,uncertainty_mm,renewal_mm\nplate,12,8.4,0.1,8\n").encode()
    path = (other if metadata == "none" else config_dir) / "elements.csv"
    path.write_bytes(raw)
    del block["elements"]
    block["elements_csv"] = str(path) if metadata == "absolute" else path.name
    block["output_dir"] = str(tmp_path / "imported")
    if metadata in {"directory", "both"}:
        cfg["_config_dir_path"] = str(config_dir)
    if metadata in {"file", "both"}:
        cfg["_config_file_path"] = str((other if metadata == "both" else config_dir) / "input.yml")
    before = deepcopy(cfg)
    monkeypatch.chdir(other)
    returned = router(cfg)
    result = returned["hull_wastage_screen"]["result"]
    assert cfg == before
    assert returned is not cfg
    assert result["ingestion"]["sha256"] == hashlib.sha256(raw).hexdigest()
    assert {k: v for k, v in result.items() if k != "ingestion"} == inline
    assert result["elements"][0]["status"] == "MONITOR"
    assert result["status"] == "NOT_EVALUATED"
    output = tmp_path / "imported"
    assert json.loads((output / "hull-wastage.json").read_text()) == result
    assert "MONITOR" in (output / "hull-wastage.html").read_text()
    assert path.read_bytes() == raw
    retained = {p.name: p.read_bytes() for p in output.iterdir()}
    with pytest.raises(FileExistsError):
        router(cfg)
    assert {p.name: p.read_bytes() for p in output.iterdir()} == retained


@pytest.mark.parametrize("mode", ["mixed", "empty-inline", "invalid", "missing", "network"])
def test_bad_intake_precedes_output_creation(tmp_path, mode):
    path = tmp_path / "elements.csv"
    path.write_text(CSV if mode in {"mixed", "empty-inline"} else "invalid", encoding="utf-8")
    cfg = yaml.safe_load((ROOT / "examples/workflows/hull-wastage-screen/input.yml").read_text())
    block = cfg["hull_wastage_screen"]
    block["output_dir"] = str(tmp_path / "results")
    block["elements_csv"] = str(tmp_path / "missing.csv") if mode == "missing" else str(path)
    if mode == "network":
        block["elements_csv"] = "//share/data.csv"
    if mode == "empty-inline":
        block["elements"] = []
    elif mode != "mixed":
        del block["elements"]
    before = deepcopy(cfg)
    with pytest.raises((ValueError, FileNotFoundError)):
        router(cfg)
    assert cfg == before
    assert not (tmp_path / "results").exists()


def test_checked_in_csv_matches_example(tmp_path):
    cfg = yaml.safe_load((ROOT / "examples/workflows/hull-wastage-screen/input.yml").read_text())
    expected = cfg["hull_wastage_screen"].pop("elements")
    path = ROOT / "examples/workflows/hull-wastage-screen/representative-elements.csv"
    assert read(path)["elements"] == expected
