# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Campaign composition checks; synthetic inputs, independent arithmetic below."""

import copy
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.ffs_campaign import assess_campaign, router

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "examples/workflows/ffs-campaign-demo/input.yml"


@pytest.fixture
def campaign():
    return yaml.safe_load(EXAMPLE.read_text())["ffs_campaign"]


def test_campaign_hand_case(campaign):
    """Demo policy v1, validation record: t_req=1440*10/(2*50000*.72)=.200 in.

    Uniform .400 in, rate .010 in/yr: life=(.400-.200)/.010=20 yr;
    half life=10 yr, policy code cap=8 yr. RSF=1, RSFa=.9 -> ratio=10/9
    (category 1); life/reference=20/5=4 (category 2). Rate=.254 mm/yr
    is below the input .3 trend threshold. PoF=max(1,2)=2, CoF=2,
    score=4 -> LOW; min(10,8,8)=8 yr. No repair is requested.
    All geometry and policy values are synthetic, not standard limits.
    """
    result = assess_campaign(campaign)
    row = result["components"][0]
    assert row["assessment"]["t_min_in"] == pytest.approx(.2)
    assert row["sufficiency"]["status"] == "SUFFICIENT"
    assert row["rbi"]["risk_score"] == 4
    assert row["rbi"]["risk_band"] == "LOW"
    assert row["next_inspection_interval_yr"] == pytest.approx(8)
    assert row["repair"] is None
    assert result["rank_register"][0]["component_id"] == "local-loss"
    assert "Revision history" in result["ffs_report"]
    json.dumps(result, allow_nan=False)


def test_repair_hand_case(campaign):
    """Demo policy v1: mean (24*.190+.160)/25=.1888; RSF=.944, min .160 below t_req.

    Failed L1 and zero projected life require REPAIR. Non-leaking,
    non-axial pressure reinforcement selects internal candidates C,D.
    Repair life=6 yr times input review fraction=.5 gives 3 yr;
    the unrepaired component interval remains zero, never 3 yr.
    ISO/PCC-2 category qualification is NOT_EVALUATED (no published case).
    """
    row = assess_campaign(campaign)["components"][1]
    assert row["assessment"]["verdict"] == "REPAIR"
    assert row["repair"]["candidate_classes"] == ["C", "D"]
    assert row["repair"]["reinspection_interval_yr"] == 3
    assert row["next_inspection_interval_yr"] == 0
    assert row["action"] == "ENGINEERING_ACTION_NOW"
    assert row["repair"]["standards_validation"] == "NOT_EVALUATED"


def test_incomplete_measurements_withhold_interval(campaign):
    row = assess_campaign(campaign)["components"][2]
    assert row["sufficiency"]["status"] == "TAKE_MORE"
    assert row["sufficiency"]["actions"]
    assert row["next_inspection_interval_yr"] is None
    assert row["action"] == "TAKE_MORE"


def test_determinism_and_no_input_mutation(campaign):
    before = copy.deepcopy(campaign)
    assert assess_campaign(campaign) == assess_campaign(campaign)
    assert campaign == before


@pytest.mark.parametrize("key", ["rbi_policy", "repair_policy", "policy_basis"])
def test_policy_is_explicit(campaign, key):
    del campaign[key]
    with pytest.raises(ValueError, match=key):
        assess_campaign(campaign)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -1, True])
def test_invalid_measurements_rejected(campaign, value):
    campaign["components"][0]["grid"][0][0] = value
    with pytest.raises(ValueError):
        assess_campaign(campaign)


def test_duplicate_ids_rejected(campaign):
    campaign["components"].append(copy.deepcopy(campaign["components"][0]))
    with pytest.raises(ValueError, match="duplicate"):
        assess_campaign(campaign)


def test_missing_repair_context_is_not_guessed(campaign):
    del campaign["components"][1]["repair_context"]
    repair = assess_campaign(campaign)["components"][1]["repair"]
    assert repair["status"] == "NOT_EVALUATED"
    assert repair["candidate_classes"] == []


def test_html_escapes_identifiers(campaign):
    campaign["components"][0]["component"]["component_id"] = "<script>bad</script>"
    report = assess_campaign(campaign)["ffs_report"]
    assert "<script>bad</script>" not in report
    assert "&lt;script&gt;bad&lt;/script&gt;" in report


def test_router_writes_only_requested_report(campaign, tmp_path):
    cfg = {"ffs_campaign": campaign}
    assert router(cfg)["ffs_campaign"]["result"]["components"]
    campaign["output_dir"] = str(tmp_path)
    router({"ffs_campaign": campaign})
    assert (tmp_path / "ffs-campaign.html").read_text().startswith("<!doctype html>")


def test_registered_workflow_offline():
    from digitalmodel.workflow_api import run_workflow

    result = run_workflow("ffs-campaign-demo", verify_reproducible=True)
    assert result.status == "ok", result.warnings
    assert result.determinism["reproducible"] is True
    assert len(result.result["value"]["components"]) == 3


def test_readiness_page_matches_source_matrix():
    import importlib.util

    spec = importlib.util.spec_from_file_location(
        "readiness", ROOT / "scripts/ffs/build_ffs_readiness.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    page = ROOT / "docs/api/ffs/ffs-readiness.html"
    assert page.read_text() == module.render_readiness(ROOT)
    assert page.read_text().count("data-engine=") == 19
    assert "NOT_EVALUATED" in page.read_text()


def test_committed_example_report_is_reproducible(campaign):
    report = ROOT / "examples/workflows/ffs-campaign-demo/report.html"
    assert report.read_text() == assess_campaign(campaign)["ffs_report"]


def test_zero_corrosion_is_unbounded_life_not_zero_life(campaign):
    """Demo policy v1: no wall loss gives unbounded life, capped at input 8 yr."""
    campaign["components"][0]["component"]["corrosion_rate_in_per_yr"] = 0
    row = assess_campaign(campaign)["components"][0]
    assert row["rbi"]["inspection_plan"]["remaining_life_years"] is None
    assert row["next_inspection_interval_yr"] == 8
    json.dumps(row, allow_nan=False)


def test_boolean_repair_limit_rejected(campaign):
    campaign["repair_policy"]["max_pressure_psi_by_class"]["C"] = True
    with pytest.raises(ValueError):
        assess_campaign(campaign)


def test_each_engine_has_run_or_validation_link():
    from html.parser import HTMLParser

    class EvidenceParser(HTMLParser):
        def __init__(self):
            super().__init__()
            self.engine = None
            self.links = {}

        def handle_starttag(self, tag, attrs):
            attrs = dict(attrs)
            if tag == "tr" and "data-engine" in attrs:
                self.engine = attrs["data-engine"]
                self.links[self.engine] = []
            if tag == "a" and self.engine:
                self.links[self.engine].append(attrs["href"])

        def handle_endtag(self, tag):
            if tag == "tr":
                self.engine = None

    parser = EvidenceParser()
    parser.feed((ROOT / "docs/api/ffs/ffs-readiness.html").read_text())
    assert len(parser.links) == 19
    for engine, links in parser.links.items():
        assert any("input.yml" in link or "validation" in link for link in links), engine


def test_worked_example_manifest_integrity():
    import hashlib

    folder = EXAMPLE.parent
    manifest = json.loads((folder / "manifest.json").read_text())
    assert manifest["readiness"] == "partial"
    for name, digest in manifest["sha256"].items():
        assert hashlib.sha256((folder / name).read_bytes()).hexdigest() == digest
