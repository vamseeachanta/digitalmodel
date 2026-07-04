"""#1284: drilling-riser operability capabilities explorer — public page + results.json.

Every check runs in PUBLIC CI with NO wiki (the atlas is committed; screen_operability is
pure atlas lookup). The page must EQUAL the live screen, ship zero external references
(it renders the served grid), and carry only generic handles.
"""
from __future__ import annotations

import importlib.util
import json
import re
from pathlib import Path

import pytest

from digitalmodel.drilling_riser import operability_atlas as oa
from digitalmodel.drilling_riser.operability_screening import screen_operability
from tests.riser_database.test_leak_gate import STRUCTURAL_PATTERNS

REPO = Path(__file__).resolve().parents[2]
_ATLAS_ROOT = REPO / "atlases"


def _load_builder():
    path = REPO / "scripts" / "drilling_riser" / "build_operability_explorer.py"
    spec = importlib.util.spec_from_file_location("build_operability_explorer", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


build = _load_builder()


def _committed_json() -> dict:
    return json.loads(build._JSON.read_text())


def _inline_data(html: str) -> dict:
    body = html.split("const DATA = ", 1)[1].rsplit(";\nconst OFF", 1)[0]
    return json.loads(body)


def _structural_hits(text: str) -> list[str]:
    return [label for pattern, label in STRUCTURAL_PATTERNS if pattern.search(text)]


# 1. fidelity + atlas binding -------------------------------------------------
def test_results_json_regenerates_and_binds_to_atlas():
    committed = _committed_json()
    fresh = build.build_results()
    assert committed == fresh, "committed results.json drifted from a fresh regeneration"
    # serialised form is byte-identical (deterministic build)
    assert build._JSON.read_text() == json.dumps(fresh, indent=2, sort_keys=False) + "\n"
    # bound to the live atlas pointer — a rebuild fails CI rather than drifting
    default_id = (_ATLAS_ROOT / oa.BASENAME / "default.txt").read_text().strip()
    assert committed["atlas_id"] == default_id
    # the explorer's inline DATA is exactly results.json
    assert _inline_data(build._HTML.read_text()) == committed


# 2. every cell equals the live screen ---------------------------------------
def test_every_cell_equals_screen_operability():
    d = _committed_json()
    off, cur = d["offset_pct"], d["current_speed_mps"]
    for token, grid in d["configs"].items():
        for i, o in enumerate(off):          # i = offset index
            for j, c in enumerate(cur):      # j = current index
                s = screen_operability(token, o, c, atlas_root=_ATLAS_ROOT)
                assert grid["uc"][i][j] == round(float(s.governing_utilisation), 6)
                assert grid["light"][i][j] == s.light


# 3. total no-exfil on the grid-bearing page ---------------------------------
def test_explorer_html_has_no_external_reference():
    html = build._HTML.read_text()
    assert "://" not in html, "explorer HTML carries a URL scheme (exfil surface)"
    assert re.search(r'(?:src|href)\s*=\s*["\']?//', html) is None, "protocol-relative reference"


# 4. one-pager governance (host-allowlisted) ---------------------------------
_SAFE_HOSTS = ("t.me/the_deckhand_bot", "api.deckhand", "vamseeachanta.github.io", "w3.org")
_API = REPO / "docs" / "api" / "capabilities" / "api" / "drilling-riser-operability"


def _strip_safe(text: str) -> str:
    for h in _SAFE_HOSTS:
        text = text.replace(h, "")
    # neutralise the (now host-less) scheme tokens so only UNEXPECTED hosts/paths remain
    return text.replace("https://", "").replace("http://", "")


def test_onepager_is_governance_clean():
    html = _API.with_suffix(".html")
    js = _API.with_suffix(".json")
    assert html.is_file() and js.is_file(), "one-pager not generated (run build_onepagers.py)"
    assert not _structural_hits(_strip_safe(html.read_text()))
    assert not _structural_hits(_strip_safe(js.read_text()))
    env = json.loads(js.read_text())
    assert env["report_url"].startswith("https://vamseeachanta.github.io/"), env["report_url"]


# 5. results.json exclude-list + generic tokens ------------------------------
_ALLOWED_KEYS = {
    "response", "atlas_id", "content_fingerprint", "tier", "offset_pct",
    "current_speed_mps", "configs", "max_rel_error", "standards",
}


def test_results_json_is_a_strict_allowlist():
    d = _committed_json()
    assert set(d.keys()) <= _ALLOWED_KEYS, set(d.keys()) - _ALLOWED_KEYS
    # generic config handles only — no client token via a source edit
    assert set(d["configs"].keys()) == set(oa.ALLOWED_TOKENS)
    # no leaked provenance/validation/path fields
    raw = build._JSON.read_text()
    for forbidden in ("provenance", "validation", "source_sha256", "SOURCE_FILES", "samples"):
        assert forbidden not in raw, f"{forbidden} leaked into results.json"
    assert not _structural_hits(raw), "structural leak (path/url/email) in results.json"
    # standards are edition-cited, not licensed values
    for s in d["standards"]:
        assert set(s.keys()) == {"id", "edition"}


# 6. honest ESCALATE: no fake node; channel wired; boundary really escalates --
def test_escalate_is_honest_and_wired():
    d = _committed_json()
    # (a) no node is fabricated as escalate — this grid's boundary falls BETWEEN knots
    lights = {l for g in d["configs"].values() for row in g["light"] for l in row}
    assert lights == {"OPERABLE", "INOPERABLE"}
    # (b) the explorer renders ESCALATE via its own visual channel + legend
    html = build._HTML.read_text()
    assert "ESCALATE" in html
    assert "escalate" in html.lower() and "escsw" in html  # legend swatch
    assert "light==='ESCALATE'" in html.replace(" ", "")   # the separate render branch
    # (c) a between-node boundary point genuinely escalates via the live screen
    s = screen_operability("rsu_a__drilling", 3.5, 0.0, atlas_root=_ATLAS_ROOT)
    assert s.light == "ESCALATE", f"expected ESCALATE at the boundary, got {s.light}"


# 7. artifacts exist + registered --------------------------------------------
def test_artifacts_exist_and_registered():
    assert build._HTML.is_file() and build._JSON.is_file()
    index = (REPO / "docs" / "api" / "capabilities" / "index.html").read_text()
    assert build.SITE_PATH in index, "no index card links the operability explorer"
