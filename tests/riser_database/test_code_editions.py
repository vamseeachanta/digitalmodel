"""Edition labels stay consistent across modules (#2161).

Owner decision C04: API STD 2RD is cited as the 2nd Edition (2013) in every
module. DNV-RP-C203 is labelled with the edition whose tables the code
implements (2021). The AMJIG identifier carries the year of the cited
revision, Rev 2 (2000).

Standalone: reads repo files only; no wiki access.
"""

from __future__ import annotations

import csv
import re
from pathlib import Path

import yaml

from digitalmodel import codes
from digitalmodel.orcaflex import code_check_engine
from digitalmodel.riser_database import getters

REPO = Path(__file__).resolve().parents[2]
CROSSWALK = REPO / "data" / "riser_database" / "standards_crosswalk.csv"
SOURCES = REPO / "scripts" / "riser_database" / "sources.yml"
OPERABILITY = REPO / "src" / "digitalmodel" / "drilling_riser" / "operability_configs.yml"

API_STD_2RD_REVISION = "2e-2013"
C203_REVISION = "2021"
AMJIG_CODE_ID = "amjig-2000"


def _crosswalk() -> dict[str, dict[str, str]]:
    with CROSSWALK.open(newline="", encoding="utf-8") as fh:
        return {row["code_id"]: row for row in csv.DictReader(fh)}


def _sources_crosswalk() -> dict[str, dict]:
    spec = yaml.safe_load(SOURCES.read_text(encoding="utf-8"))
    return {row["code_id"]: row for row in spec["crosswalk_rows"]}


# -- API STD 2RD ---------------------------------------------------------------


def test_crosswalk_2rd_is_second_edition_2013():
    assert _crosswalk()["api-std-2rd"]["registry_revision"] == API_STD_2RD_REVISION


def test_sources_2rd_is_second_edition_2013():
    assert str(_sources_crosswalk()["api-std-2rd"]["registry_revision"]) == API_STD_2RD_REVISION


def test_getter_template_2rd_is_second_edition_2013():
    assert getters._API_STD_2RD_CITATION_TEMPLATE["revision"] == API_STD_2RD_REVISION


def test_operability_config_2rd_is_second_edition_2013():
    cfg = yaml.safe_load(OPERABILITY.read_text(encoding="utf-8"))
    editions = {s["id"]: str(s["edition"]) for s in cfg["provenance"]["standards"]}
    assert editions["API-STD-2RD"] == API_STD_2RD_REVISION


def test_codes_register_2rd_is_2013():
    assert codes.API_STD_2RD.edition == "2013"


def test_code_check_engine_cites_api_std_2rd_2013():
    doc = code_check_engine.__doc__
    assert "API STD 2RD (2nd Ed., 2013)" in doc
    assert "API STD 2RD (2nd Ed., 2013)" in code_check_engine.APIRP2RDInput.__doc__


def test_no_third_edition_2rd_label_left():
    pattern = re.compile(r"3e-2025|3rd Edition,? 2025")
    hits = []
    roots = [REPO / "src", REPO / "data" / "riser_database", SOURCES]
    for root in roots:
        files = [root] if root.is_file() else [p for p in root.rglob("*") if p.is_file()]
        for path in files:
            if path.suffix not in {".py", ".yml", ".yaml", ".csv", ".md"}:
                continue
            text = path.read_text(encoding="utf-8", errors="replace")
            if pattern.search(text):
                hits.append(str(path.relative_to(REPO)))
    assert not hits, hits


# -- DNV-RP-C203 ---------------------------------------------------------------


def test_crosswalk_c203_is_implemented_edition():
    from digitalmodel.fatigue.c203_editions import DNV_RP_C203_IMPLEMENTED_EDITION

    row = _crosswalk()["dnv-rp-c203"]
    assert row["registry_revision"] == C203_REVISION == DNV_RP_C203_IMPLEMENTED_EDITION


def test_getter_template_c203_is_implemented_edition():
    assert getters._DNV_RP_C203_CITATION_TEMPLATE["revision"] == C203_REVISION


# -- DNV ST-F201 ---------------------------------------------------------------


def test_code_check_engine_states_the_implemented_f201_edition_only():
    doc = code_check_engine.__doc__
    assert "DNV-OS-F201 (2010)" in doc
    assert "ST-F201 (2018)" not in doc
    assert "ST-F201 Section 5 (2018)" not in code_check_engine.DNVOSF201Input.__doc__


# -- AMJIG ---------------------------------------------------------------------


def test_amjig_identifier_matches_revision_year():
    row = _crosswalk()[AMJIG_CODE_ID]
    assert row["registry_revision"] == "Rev 2 (2000)"
    assert row["family_key"] == AMJIG_CODE_ID
    assert row["wiki_path"].endswith(f"/{AMJIG_CODE_ID}.md")
    assert "amjig-1997" not in _crosswalk()


def test_amjig_getter_template_matches_crosswalk():
    t = getters._AMJIG_CITATION_TEMPLATE
    row = _crosswalk()[AMJIG_CODE_ID]
    assert t["code_id"] == AMJIG_CODE_ID
    assert t["revision"] == row["registry_revision"]
    assert t["wiki_path"] == row["wiki_path"]
