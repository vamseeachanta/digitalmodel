# ABOUTME: Tests for the #2157 P4 crack-like-flaw report builder: house skeleton, captions,
# ABOUTME: portability, data-src traceability, no dropped findings, register lint.
"""Tests for ``crack_fe_report`` (#2157 P4; owner cards R06, J02, J03, G04, S04, B05-B08).

Comparator classes:

- conservation of values: every ``data-src`` element resolves to a value in the result
  record, the design-data register or the receipt metadata, and its displayed number
  equals that value to the displayed precision (resolved here independently of the
  builder's own resolver);
- coverage invariants: every finding id, check, sensitivity, screening bound, evidence
  item and register row of the record appears in the report;
- contract: skeleton order, captions below, no external resources, portable SVG, the
  conclusions box, deterministic regeneration, register lint.

The result is built with an injected receipt validator so the tests run in seconds; the
production path (:func:`crack_fe_report.generate` without a result) calls ``run`` and
validates every receipt.
"""

from __future__ import annotations

import json
import os
import re
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.assessment import crack_fe_assessment as cfa
from digitalmodel.asset_integrity.assessment import crack_fe_report as cfr

REPO = Path(__file__).resolve().parents[2]
EXAMPLE = REPO / "examples" / "workflows" / "crack-fe-weldolet"
INPUT = EXAMPLE / "input.yml"
REGISTER = EXAMPLE / "design-data-register.json"
FE_STATES = EXAMPLE / "fe_states"
_LINT_REL = Path("scripts") / "enforcement" / "check-engineering-register.py"


def _find_lint() -> Path:
    """The workspace-hub register lint: $WORKSPACE_HUB, else a sibling of an ancestor."""
    env = os.environ.get("WORKSPACE_HUB")
    candidates = [Path(env) / _LINT_REL] if env else []
    candidates += [p / "workspace-hub" / _LINT_REL for p in REPO.parents]
    return next((c for c in candidates if c.is_file()), candidates[-1])


LINT = _find_lint()
DATE = "2026-09-26"
ASSUMED = "ASSUMED - to be confirmed"


def _trust_all(*_args, **_kwargs) -> list[str]:
    return []


@pytest.fixture(scope="module")
def result() -> dict:
    return cfa._assess(cfa.load_case(INPUT), _trust_all).to_dict()


@pytest.fixture(scope="module")
def register() -> dict:
    return json.loads(REGISTER.read_text("utf-8"))


@pytest.fixture(scope="module")
def meta() -> dict:
    return cfr.receipts_meta(FE_STATES)


@pytest.fixture(scope="module")
def html_doc(result, register, meta) -> str:
    return cfr.build_report(result, register, meta, issue_date=DATE)


# --------------------------------------------------------------------------- #
# A small independent HTML model
# --------------------------------------------------------------------------- #
class _Node:
    def __init__(self, tag, attrs, parent):
        self.tag, self.attrs, self.parent = tag, dict(attrs), parent
        self.children: list = []

    def text(self) -> str:
        out = []
        for c in self.children:
            out.append(c if isinstance(c, str) else c.text())
        return "".join(out)

    def iter(self):
        for c in self.children:
            if not isinstance(c, str):
                yield c
                yield from c.iter()

    def classes(self) -> set:
        return set((self.attrs.get("class") or "").split())


_VOID = {"meta", "br", "hr", "img", "link", "input", "col", "source", "wbr"}


class _Tree(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.root = _Node("root", {}, None)
        self.cur = self.root

    def handle_starttag(self, tag, attrs):
        node = _Node(tag, attrs, self.cur)
        self.cur.children.append(node)
        if tag not in _VOID:
            self.cur = node

    def handle_startendtag(self, tag, attrs):
        self.cur.children.append(_Node(tag, attrs, self.cur))

    def handle_endtag(self, tag):
        node = self.cur
        while node is not self.root and node.tag != tag:
            node = node.parent
        if node is not self.root:
            self.cur = node.parent

    def handle_data(self, data):
        self.cur.children.append(data)


def _tree(doc: str) -> _Node:
    t = _Tree()
    t.feed(doc)
    return t.root


def _visible_text(doc: str) -> str:
    doc = re.sub(r"<(script|style)\b.*?</\1>", " ", doc, flags=re.S | re.I)
    return re.sub(r"\s+", " ", _tree(doc).text())


def _resolve(roots: dict, ref: str):
    root, _, path = ref.partition(":")
    node = roots[root]
    for part in [p for p in path.split("/") if p]:
        node = node[int(part)] if isinstance(node, list) else node[part]
    return node


_NUM = re.compile(r"^[+\-\u2212]?(?:\d{1,3}(?:,\d{3})+|\d+)(?:\.\d+)?(?:[eE][+\-]?\d+)?$")


def _parse_num(text: str):
    """(value, half-unit of the last displayed digit) of a displayed number."""
    t = text.strip().replace("\u2212", "-").replace(",", "")
    mant, _, exp = t.lower().partition("e")
    dec = len(mant.split(".")[1]) if "." in mant else 0
    scale = 10.0 ** int(exp) if exp else 1.0
    return float(t), 0.5 * 10.0 ** (-dec) * scale


def _matches(value, text: str, scale: float) -> bool:
    if isinstance(value, bool) or value is None:
        return False
    if isinstance(value, (list, tuple)):
        parts = [p for p in re.split(r"\s+\u2013\s+|,\s+", text.strip()) if p]
        return len(parts) == len(value) and all(
            _matches(v, p, scale) for v, p in zip(value, parts))
    if isinstance(value, str):
        return text.strip() == value
    if not _NUM.match(text.strip()):
        return False
    shown, half = _parse_num(text)
    return abs(shown - float(value) * scale) <= half * (1 + 1e-9) + 1e-12 * abs(value * scale)


# --------------------------------------------------------------------------- #
# Skeleton and document control
# --------------------------------------------------------------------------- #
SKELETON = [
    "Executive summary",
    "Introduction",
    "Design basis and source register",
    "Assumptions and limitations",
    "Acceptance criteria",
    "Analysis methodology",
    "FE model and verification",
    "Results by governing case",
    "Checks",
    "Conclusions",
    "Recommendations",
    "References",
    "Appendix A",
]


def test_report_skeleton_sections(html_doc):
    heads = [h.text().strip() for h in _tree(html_doc).iter() if h.tag == "h2"]
    positions = []
    for want in SKELETON:
        hit = [i for i, h in enumerate(heads) if h.startswith(want)]
        assert hit, f"section {want!r} missing from {heads}"
        positions.append(hit[0])
    assert positions == sorted(positions), heads
    # the cover and revision record precede the executive summary
    assert html_doc.index('id="cover"') < html_doc.index("Executive summary")


def test_cover_title_block(html_doc):
    root = _tree(html_doc)
    cover = next(n for n in root.iter() if n.attrs.get("id") == "cover")
    text = re.sub(r"\s+", " ", cover.text())
    assert "Rev A" in text and "issued for owner review" in text
    assert DATE in text
    rev = next(n for n in cover.iter() if n.tag == "table" and "revhist" in n.classes())
    heads = [th.text().strip() for th in rev.iter() if th.tag == "th"]
    assert heads[-3:] == ["Prepared", "Checked", "Approved"]
    cells = [td for td in rev.iter() if td.tag == "td"]
    assert cells[-3].text().strip(), "Prepared is filled"
    assert cells[-2].text().strip() == "" and cells[-1].text().strip() == ""


def test_report_captions_below(html_doc):
    """Every table and figure is followed by its caption; no caption precedes one."""
    tokens = [(m.start(), m.group(0)) for m in re.finditer(
        r"<table\b|</table>|<svg\b|</svg>|<p class=\"caption\">(?:Table|Figure)", html_doc)]
    n_tables = n_figs = 0
    for i, (_, tok) in enumerate(tokens):
        if tok in ("</table>", "</svg>"):
            nxt = tokens[i + 1][1] if i + 1 < len(tokens) else ""
            kind = "Table" if tok == "</table>" else "Figure"
            assert nxt.endswith(kind), f"{kind} at {tokens[i][0]} not followed by its caption"
            n_tables += tok == "</table>"
            n_figs += tok == "</svg>"
        if tok.startswith("<p class"):
            prev = tokens[i - 1][1]
            assert prev in ("</table>", "</svg>"), "caption not directly below an object"
    assert n_tables >= 20 and n_figs >= 2


def test_report_no_external_resources(html_doc):
    low = html_doc.lower()
    assert not re.search(r"<script[^>]+src=", low)
    assert "<link" not in low
    assert "<img" not in low
    assert "@import" not in low
    assert not re.search(r"url\(\s*['\"]?https?:", low)
    assert "<iframe" not in low and "<object" not in low


def test_report_svg_portable(html_doc):
    svgs = re.findall(r"<svg\b.*?</svg>", html_doc, flags=re.S)
    assert len(svgs) >= 2
    for svg in svgs:
        low = svg.lower()
        for bad in ("clippath", "clip-path", "<pattern", "<filter", "filter=", "<mask",
                    "mask=", "<foreignobject", "<image"):
            assert bad not in low, bad


# --------------------------------------------------------------------------- #
# Robustness: traceability and coverage (owner note on R06)
# --------------------------------------------------------------------------- #
def test_report_table_cells_trace_to_result(html_doc, result, register, meta):
    roots = {"result": result, "register": register, "receipts": meta}
    traced = 0
    for node in _tree(html_doc).iter():
        ref = node.attrs.get("data-src")
        if not ref:
            continue
        value = _resolve(roots, ref)
        scale = float(node.attrs.get("data-scale", 1))
        assert _matches(value, node.text(), scale), (ref, value, node.text())
        traced += 1
    assert traced >= 400, traced


def test_every_numeric_cell_has_a_source(html_doc):
    missing = []
    for node in _tree(html_doc).iter():
        if node.tag == "td" and _NUM.match(node.text().strip()) and "data-src" not in node.attrs:
            missing.append(node.text().strip())
    assert not missing, missing


def test_svg_points_trace_to_result(html_doc, result, register, meta):
    roots = {"result": result, "register": register, "receipts": meta}
    points = [n for n in _tree(html_doc).iter() if n.tag == "circle" and "data-src-x" in n.attrs]
    assert len(points) >= 8
    for p in points:
        for axis in ("x", "y"):
            assert isinstance(_resolve(roots, p.attrs[f"data-src-{axis}"]), (int, float))


def test_every_finding_appears(html_doc, result):
    rows = {n.attrs["data-finding"]: n for n in _tree(html_doc).iter() if "data-finding" in n.attrs}
    for f in result["findings"]:
        assert f["id"] in rows, f["id"]
        assert f["id"] in rows[f["id"]].text()
        assert f["disposition"][:40] in re.sub(r"\s+", " ", rows[f["id"]].text())


def test_no_check_sensitivity_or_evidence_dropped(html_doc, result):
    text = _visible_text(html_doc)
    for key in result["sensitivities"]:
        assert f'data-sensitivity="{key}"' in html_doc, key
    assert text.count("SENSITIVITY") >= len(result["sensitivities"])
    for bound in result["screening"]["residual_bounds"]:
        assert f'data-screening="{bound["label"]}"' in html_doc
        assert bound["kind"] == "screening"
    for name in result["checks"]:
        if isinstance(result["checks"][name], dict):
            assert f'data-check="{name}"' in html_doc, name
    for name in result["evidence"]:
        assert f'data-evidence="{name}"' in html_doc, name
    for item in result["missing_evidence"]:
        assert item in text
    for name in result["inputs"]:
        assert f'data-input="{name}"' in html_doc, name
    for state in result["receipts"]:
        assert f'data-receipt="{state}"' in html_doc, state
    # the J03 meaning text and the SSY life basis are carried
    assert result["checks"]["ssy_meaning"][:60] in text
    assert result["growth"]["life_to_limit_state"]["status"] in text


def test_every_assumed_register_row_is_labelled(html_doc, register):
    rows = {n.attrs["data-register-id"]: n for n in _tree(html_doc).iter()
            if n.tag == "tr" and "data-register-id" in n.attrs}
    assert len(register["design_data"]) == 39
    for item in register["design_data"]:
        assert item["id"] in rows, item["id"]
        text = rows[item["id"]].text()
        if item["status_label"] == ASSUMED:
            label = [c for c in rows[item["id"]].iter() if "assumed-label" in c.classes()]
            assert label and label[0].text().strip() == ASSUMED, item["id"]
        assert item["note"][:50] in text


def test_assumed_notes_beside_affected_results(html_doc):
    """Owner card J02: assumed inputs are flagged where they are used, not only listed."""
    notes = re.findall(r'<p class="note-assumed"[^>]*>(.*?)</p>', html_doc, flags=re.S)
    assert len(notes) >= 5
    assert all(ASSUMED in n for n in notes)


def test_report_does_not_reference_source(html_doc):
    text = _visible_text(html_doc).lower()
    low = html_doc.lower()
    for word in ("linkedin", "frolov", "published case", "source article"):
        assert word not in low, word
    assert not re.search(r"\barticles?\b", text)


def test_register_vocabulary(html_doc):
    """No unqualified 'validated'; no 'safe', 'conservative' or 'acceptable' at all."""
    text = _visible_text(html_doc)
    assert not re.search(r"\bvalidat", text, flags=re.I)
    for word in ("safe", "conservative", "acceptable", "acceptably"):
        assert not re.search(rf"\b{word}\b", text, flags=re.I), word


def test_no_host_or_machine_names(html_doc):
    low = html_doc.lower()
    for token in ("acma", "ace-win", "ace-linux", "rds0", "c:\\users", "d:\\ws", "/mnt/ace",
                  "solve_seconds", "cost"):
        assert token not in low, token


def _run_lint(path: Path) -> subprocess.CompletedProcess:
    return subprocess.run([sys.executable, str(LINT), str(path)], capture_output=True,
                          text=True, encoding="utf-8", errors="replace")


@pytest.mark.skipif(not LINT.is_file(), reason="workspace-hub sibling checkout absent; "
                    "register lint check-engineering-register.py not available")
def test_report_register_lint(html_doc, tmp_path):
    page = tmp_path / "crack-fe-weldolet-report.html"
    page.write_text(html_doc, encoding="utf-8")
    res = _run_lint(page)
    assert res.returncode == 0, res.stdout + res.stderr
    # the visible text alone, so markup quoting cannot mask a finding
    text = tmp_path / "crack-fe-weldolet-report-text.md"
    text.write_text(_visible_text(html_doc).replace(". ", ".\n"), encoding="utf-8")
    res = _run_lint(text)
    assert res.returncode == 0, res.stdout + res.stderr


# --------------------------------------------------------------------------- #
# Executive summary and conclusions (owner card J03: highlight the conclusions)
# --------------------------------------------------------------------------- #
def test_conclusions_box(html_doc, result):
    root = _tree(html_doc)
    box = next(n for n in root.iter() if "conclusions-box" in n.classes())
    text = re.sub(r"\s+", " ", box.text())
    assert result["verdict"] in text and "MONITOR" in text
    assert result["evidence_status"] in text and "INCOMPLETE" in text
    assert "passes = false" in text
    assert "1,980" in text and "2.369" in text          # SSY-valid life and depth
    assert "59,339" in text and "2.97" in text          # life to the last FE state
    assert "1.89" in text and "2.21" in text            # load-factor range
    assert "3.13" in text and "56,479" in text          # cyclic-zone meaning
    assert "crotch" in text.lower()
    assert "J-based" in text or "elastic-plastic" in text
    for item in result["missing_evidence"]:
        assert item.replace("_", " ").split(" basis")[0] in text.replace("_", " ")
    # the box sits in the executive summary, before the introduction
    assert html_doc.index("conclusions-box") < html_doc.index(">Introduction<")


def test_conclusions_state_criterion_and_disposition(html_doc):
    root = _tree(html_doc)
    items = [n for n in root.iter() if n.tag == "li" and "conclusion" in n.classes()]
    assert len(items) >= 5
    for li in items:
        parts = {c.attrs.get("data-part") for c in li.iter() if c.attrs.get("data-part")}
        assert {"basis", "result", "criterion", "disposition"} <= parts, li.text()[:80]
    recs = [n for n in root.iter() if n.tag == "li" and "recommendation" in n.classes()]
    assert len(recs) >= 5


def test_deterministic_apart_from_date(result, register, meta):
    a = cfr.build_report(result, register, meta, issue_date=DATE)
    b = cfr.build_report(result, register, meta, issue_date=DATE)
    c = cfr.build_report(result, register, meta, issue_date="2027-01-02")
    assert a == b
    assert a != c
    assert a.replace(DATE, "<date>") == c.replace("2027-01-02", "<date>")


def test_build_accepts_result_object(register, meta):
    res = cfa._assess(cfa.load_case(INPUT), _trust_all)
    assert cfr.build_report(res, register, meta, issue_date=DATE) == cfr.build_report(
        res.to_dict(), register, meta, issue_date=DATE)


def test_receipts_meta_is_host_free(meta):
    blob = json.dumps(meta).lower()
    assert "argv" not in blob and "solve_seconds" not in blob
    assert meta["solver"]["mapdl_release"]
    assert set(meta["states"]) >= {"p0a_verification", "p0b_uncracked"}


def test_generate_and_cli(tmp_path, monkeypatch, result):
    out = tmp_path / "r.html"
    path = cfr.generate(INPUT, out, issue_date=DATE, result=result)
    assert path == out and out.read_text("utf-8").startswith("<!DOCTYPE html>")
    monkeypatch.setattr(cfr, "_run_case", lambda case: result)
    out2 = tmp_path / "cli.html"
    assert cfr.main([str(INPUT), "-o", str(out2), "--date", DATE]) == 0
    assert out2.read_text("utf-8") == out.read_text("utf-8")
