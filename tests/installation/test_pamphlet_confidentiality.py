"""Codenamed fixtures for private provenance and configurable run selection."""
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.installation.installation_pamphlet import calculations as calc
from digitalmodel.installation.installation_pamphlet.workflow import router
from .test_installation_pamphlet import (
    BASE_CONFIG, _synthetic_rao_set, _write_rao_artifact,
)

ROOT = Path(__file__).resolve().parents[2]


def test_extra_run_token_selects_basis_artifact_and_provenance():
    runs = [
        {"run_id": "proxy", "input": "diffraction-fpso/input.yml",
         "rao_artifact": "proxy.json"},
        {"run_id": "vessel", "input": "PRIVATE-CASE/input.yml",
         "rao_artifact": "vessel.json"},
    ]
    tokens = ("hlv-a", "private-case")
    assert calc.select_rao_basis(runs, tokens) == "vessel"
    assert calc.select_basis_run_id(runs, "vessel", tokens) == "vessel"
    assert calc.resolve_rao_artifact("vessel", runs, vessel_run_tokens=tokens) == "vessel.json"
    assert calc.resolve_rao_artifact("vessel", runs, "explicit.json", tokens) == "explicit.json"
    assert calc.select_rao_basis(runs) == "drillship"


@pytest.mark.parametrize("tokens", [("",), (" ",), ()])
def test_empty_tokens_do_not_match_every_run(tokens):
    runs = [{"input": "unrelated/input.yml"}]
    assert calc.select_rao_basis(runs, tokens) == "barge"


def test_public_hlv_a_record_withholds_identity():
    data = json.loads((ROOT / "data/vessels/raw/install__particulars.json").read_text())
    rec = next(r for r in data["records"] if r["name"].startswith("HLV-A"))
    assert all(rec[k] == "withheld" for k in ("owner_operator", "year_built", "vessel_id"))
    assert rec["fields"]["builder"] == "withheld"
    assert rec["name"] == "HLV-A"
    assert rec["vessel_type"] == "Monohull crane vessel"
    assert {"source": "private provenance", "access": "private"} in rec["citations"]
    assert "HLV-A" not in json.dumps(data["verdict"])


def test_router_uses_extra_token_and_omits_withheld_operator(tmp_path):
    artifact = _write_rao_artifact(tmp_path, _synthetic_rao_set())
    cfg = yaml.safe_load(BASE_CONFIG.read_text())
    cfg["installation_pamphlet"].update(
        output_dir=str(tmp_path / "out"),
        vessel_run_tokens=["hlv-a", "private-case"],
        completed_runs=[
            {"run_id": "proxy", "input": "diffraction-fpso/input.yml",
             "rao_artifact": "missing-proxy.json"},
            {"run_id": "vessel", "input": "PRIVATE-CASE/input.yml",
             "rao_artifact": str(artifact)},
        ],
    )
    result = router(cfg)["installation_pamphlet"]["result"]
    assert result["rao_basis"] == "vessel"
    assert result["rao_artifact"] == str(artifact)
    assert result["provenance"]["basis_run_id"] == "vessel"
    assert result["rao_from_artifact"] is True
    html = Path(cfg["outputs"]["pamphlet_html"]).read_text(encoding="utf-8")
    assert "withheld" not in html.lower()
    assert "Owner / operator" not in html
