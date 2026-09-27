# ABOUTME: Tests for the #2157 P4 crack-like-flaw report builder: house skeleton, captions,
# ABOUTME: portability, data-src traceability, no dropped findings, register lint, Rev B figures.
"""Tests for ``crack_fe_report`` (#2157 P4; owner cards R06, J02, J03, G04, S04, B05-B08;
Rev B owner review comments 1-3).

Comparator classes:

- conservation of values: every ``data-src`` element resolves to a value in the result
  record, the design-data register or the receipt metadata, and its displayed number
  equals that value to the displayed precision (resolved here independently of the
  builder's own resolver); a utilisation cell (``data-src-num`` / ``data-src-den``)
  equals the quotient of its two resolved sources;
- closed-form: the growth curve drawn in the executive summary is re-integrated here from
  the record's own law and Delta K table, and must end at the record's life to the last FE
  state and pass through the record's SSY-valid life;
- coverage invariants: every finding id, check, sensitivity, screening bound, evidence
  item, register row and acronym of the page appears where it must;
- contract: standard skeleton order (front matter, then Introduction ... Appendix A),
  captions below, no external resources, portable SVG, host-free embedded PNG, the
  conclusions box, deterministic regeneration, register lint.

The result is built with an injected receipt validator so the tests run in seconds; the
production path (:func:`crack_fe_report.generate` without a result) calls ``run`` and
validates every receipt.
"""

from __future__ import annotations

import base64
import html
import json
import os
import re
import struct
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import pytest

from digitalmodel.asset_integrity.assessment import crack_fe_assessment as cfa
from digitalmodel.asset_integrity.assessment import crack_fe_report as cfr
from digitalmodel.fatigue.crack_growth_history import (
    GrowthLaw,
    TabulatedDeltaK,
    Threshold,
    life,
)

REPO = Path(__file__).resolve().parents[2]
EXAMPLE = REPO / "examples" / "workflows" / "crack-fe-weldolet"
INPUT = EXAMPLE / "input.yml"
REGISTER = EXAMPLE / "design-data-register.json"
FE_STATES = EXAMPLE / "fe_states"
FIGURES = EXAMPLE / "figures"
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
STATUSES = {"WITHIN_CRITERION", "EXCEEDS_CRITERION", "NOT_EVALUATED"}


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
def figures() -> dict:
    return cfr.load_figures(FIGURES)


@pytest.fixture(scope="module")
def html_doc(result, register, meta, figures) -> str:
    return cfr.build_report(result, register, meta, issue_date=DATE, figures=figures)


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

    def elements(self) -> list:
        return [c for c in self.children if not isinstance(c, str)]

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


def _section(doc: str, sid: str) -> _Node:
    return next(n for n in _tree(doc).iter() if n.tag == "section" and n.attrs.get("id") == sid)


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


def _png_chunks(blob: bytes) -> list[bytes]:
    assert blob[:8] == b"\x89PNG\r\n\x1a\n"
    pos, kinds = 8, []
    while pos < len(blob):
        (n,) = struct.unpack(">I", blob[pos:pos + 4])
        kinds.append(blob[pos + 4:pos + 8])
        pos += 12 + n
    return kinds


# --------------------------------------------------------------------------- #
# Skeleton, front matter and document control (owner comment 2)
# --------------------------------------------------------------------------- #
SKELETON = [
    "Introduction",
    "Summary and conclusions",
    "Design basis",
    "Methodology",
    "Results",
    "Verification",
    "Conclusions",
    "Recommendations",
    "References",
    "Appendix A",
]
FRONT_MATTER = ["Document control", "Revision history", "Abbreviations",
                "Holds and assumptions register", "Contents", "List of tables",
                "List of figures"]


def test_report_skeleton_sections(html_doc):
    heads = [h.text().strip() for h in _tree(html_doc).iter() if h.tag == "h2"]
    positions = []
    for want in SKELETON:
        hit = [i for i, h in enumerate(heads) if h.startswith(want)]
        assert hit, f"section {want!r} missing from {heads}"
        positions.append(hit[0])
    assert positions == sorted(positions), heads
    assert html_doc.index('id="cover"') < html_doc.index('id="front-matter"') < html_doc.index(
        '<main class="main">')


def test_front_matter_order(html_doc):
    fm = next(n for n in _tree(html_doc).iter() if n.attrs.get("id") == "front-matter")
    heads = [h.text().strip() for h in fm.iter() if h.tag == "h3"]
    positions = [next(i for i, h in enumerate(heads) if h.startswith(w)) for w in FRONT_MATTER]
    assert positions == sorted(positions), heads


def test_lists_of_tables_and_figures_are_complete(html_doc):
    root = _tree(html_doc)
    caps = [re.sub(r"\s+", " ", n.text()).strip() for n in root.iter()
            if n.tag == "p" and "caption" in n.classes()]
    tables = [c.split(" – ")[0] for c in caps if c.startswith("Table ")]
    figs = [c.split(" – ")[0] for c in caps if c.startswith("Figure ")]
    assert len(tables) == len(set(tables)) and len(figs) == len(set(figs))
    lot = next(n for n in root.iter() if n.attrs.get("id") == "list-of-tables")
    lof = next(n for n in root.iter() if n.attrs.get("id") == "list-of-figures")
    lot_text, lof_text = lot.text(), lof.text()
    # front-matter tables are listed too (lists are built from every caption)
    for t in tables:
        assert t in lot_text, t
    for f in figs:
        assert f in lof_text, f
    assert len(figs) >= 8


def test_caption_numbers_follow_document_order(html_doc):
    root = _tree(html_doc)
    seen: dict[tuple, list] = {}
    for n in root.iter():
        if n.tag == "p" and "caption" in n.classes():
            m = re.match(r"(Table|Figure) (\w+)[.-](\d+)", n.text().strip())
            assert m, n.text()[:40]
            seen.setdefault((m.group(1), m.group(2)), []).append(int(m.group(3)))
    for key, nums in seen.items():
        assert nums == list(range(1, len(nums) + 1)), (key, nums)


def test_cover_title_block(html_doc):
    root = _tree(html_doc)
    cover = next(n for n in root.iter() if n.attrs.get("id") == "cover")
    text = re.sub(r"\s+", " ", cover.text())
    assert "Rev B" in text and "issued for owner review" in text
    assert DATE in text
    fm = next(n for n in root.iter() if n.attrs.get("id") == "front-matter")
    rev = next(n for n in fm.iter() if n.tag == "table" and "revhist" in n.classes())
    heads = [th.text().strip() for th in rev.iter() if th.tag == "th"]
    assert heads[:3] == ["Rev", "Sections", "Description"]
    assert heads[-3:] == ["Prepared", "Checked", "Approved"]
    rows = [tr for tr in rev.iter() if tr.tag == "tr" and tr.elements()[0].tag == "td"]
    assert [r.elements()[0].text().strip() for r in rows] == ["A", "B"]
    cells = rows[-1].elements()
    assert cells[-3].text().strip(), "Prepared is filled"
    assert cells[-2].text().strip() == "" and cells[-1].text().strip() == ""
    assert "owner review" in rows[-1].text()


def test_report_captions_below(html_doc):
    """Every table and figure block ends with its caption; no caption stands elsewhere."""
    root = _tree(html_doc)
    blocks = [n for n in root.iter() if n.tag == "div" and n.classes() & {"tblock", "fblock"}]
    for b in blocks:
        kids = b.elements()
        kind = "Table" if "tblock" in b.classes() else "Figure"
        assert kids and kids[-1].tag == "p" and "caption" in kids[-1].classes(), kind
        assert kids[-1].text().strip().startswith(kind)
        objects = [k for k in b.iter() if k.tag in ("table", "svg", "img")]
        assert objects, "block without an object"
        if kind == "Table":
            assert all(o.tag == "table" for o in objects)
    for cap in (n for n in root.iter() if n.tag == "p" and "caption" in n.classes()):
        assert cap.parent.classes() & {"tblock", "fblock"}
    assert sum("tblock" in b.classes() for b in blocks) >= 25
    assert sum("fblock" in b.classes() for b in blocks) >= 8


def test_report_no_external_resources(html_doc):
    low = html_doc.lower()
    assert not re.search(r"<script[^>]+src=", low)
    assert "<link" not in low
    assert "@import" not in low
    assert not re.search(r"url\(\s*['\"]?https?:", low)
    assert "<iframe" not in low and "<object" not in low
    for m in re.finditer(r"<img\b[^>]*>", html_doc):
        assert re.search(r"src=['\"]data:image/png;base64,", m.group(0)), m.group(0)[:80]


def test_embedded_images_are_host_free(html_doc):
    srcs = re.findall(r"src=['\"]data:image/png;base64,([^'\"]+)['\"]", html_doc)
    assert len(srcs) >= 6
    for s in srcs:
        blob = base64.b64decode(s)
        assert set(_png_chunks(blob)) <= {b"IHDR", b"PLTE", b"IDAT", b"IEND"}
        low = blob.lower()
        for tok in (b"acma", b"ace-win", b"ace-linux", b"rds0", b"vamsee", b"c:\\users"):
            assert tok not in low


def test_report_svg_portable(html_doc):
    svgs = re.findall(r"<svg\b.*?</svg>", html_doc, flags=re.S)
    assert len(svgs) >= 4
    for svg in svgs:
        low = svg.lower()
        for bad in ("clippath", "clip-path", "<pattern", "<filter", "filter=", "<mask",
                    "mask=", "<foreignobject", "<image"):
            assert bad not in low, bad


# --------------------------------------------------------------------------- #
# Abbreviations and SSY in practical terms (owner comment 1)
# --------------------------------------------------------------------------- #
# words in capitals that are status labels or record keywords, not acronyms
NOT_ACRONYMS = {
    "ACCEPT", "MONITOR", "REJECT", "INCOMPLETE", "COMPLETE", "ASSUMED", "SENSITIVITY",
    "GROWS", "ARRESTED", "NOT", "FAIL", "ON", "OFF", "CONDITIONAL", "MEANING", "EXCEEDS",
    "WITHIN_CRITERION", "EXCEEDS_CRITERION", "NOT_EVALUATED", "INCONSISTENT", "EVALUATED",
    "OR", "AND", "A", "B", "L", "T", "HOLD", "REPAIR", "VALID", "BELOW",
    # record identifiers (component id and an owner-decision id), not acronyms
    "WELDOLET", "ROOT", "FLAW", "S05M",
}
REQUIRED_ABBREVIATIONS = {"SSY", "FAD", "FE", "FEA", "LEFM", "EPFM", "CINT", "SIF", "K",
                          "K_gov", "J", "Kr", "Lr", "Lr_max", "PSF", "NDE", "HAZ", "SA",
                          "SMA", "GTA", "EPP", "TES", "R", "ΔK", "ΔK_th"}


def _abbrev_table(html_doc) -> dict:
    root = _tree(html_doc)
    tab = next(n for n in root.iter() if n.tag == "table" and "abbrev" in n.classes())
    heads = [th.text().strip() for th in tab.iter() if th.tag == "th"]
    assert heads == ["Abbreviation", "Definition"]
    rows = [tr.elements() for tr in tab.iter() if tr.tag == "tr" and tr.elements()[0].tag == "td"]
    return {r[0].text().strip(): r[1].text().strip() for r in rows}


def test_abbreviations_table_covers_every_acronym(html_doc):
    table = _abbrev_table(html_doc)
    keys = {p.strip() for k in table for p in k.split(",")}
    assert REQUIRED_ABBREVIATIONS <= keys, REQUIRED_ABBREVIATIONS - keys
    assert all(len(d) > 3 for d in table.values())
    body = re.sub(r"<(script|style)\b.*?</\1>", " ", html_doc, flags=re.S | re.I)
    body = re.sub(r"<wbr\s*/?>", "", body)                  # a line-break hint, not a boundary
    text = html.unescape(re.sub(r"<[^>]+>", " ", body))   # element boundaries as spaces
    tokens = set(re.findall(r"(?<![\w/.-])([A-Z][A-Z0-9]*[A-Z](?:-\d+)?)(?![\w-])", text))
    defined = set(table)
    for t in list(table):
        defined |= set(re.split(r"[/ ,]+", t))
    missing = sorted(t for t in tokens
                     if t not in defined and re.sub(r"-\d+$", "", t) not in defined
                     and t not in NOT_ACRONYMS and re.sub(r"-\d+$", "", t) not in NOT_ACRONYMS)
    assert not missing, missing


@pytest.mark.parametrize("abbr, words", [
    ("SSY", "small-scale yielding"), ("FAD", "failure assessment diagram"),
    ("LEFM", "linear-elastic fracture mechanics"), ("FE", "finite-element"),
    ("PSF", "partial safety factor"), ("NDE", "non-destructive examination"),
])
def test_first_use_spells_out(html_doc, abbr, words):
    main = _visible_text(html_doc[html_doc.index('<main class="main">'):])
    first = re.search(rf"(?<![\w-]){re.escape(abbr)}(?![\w-])", main)
    spelled = re.search(rf"{re.escape(words)}\s*\({re.escape(abbr)}\)", main, flags=re.I)
    assert spelled, abbr
    assert spelled.end() >= first.end() and spelled.start() <= first.start(), abbr


def test_ssy_limitation_in_practical_terms(html_doc):
    s2 = re.sub(r"\s+", " ", _section(html_doc, "s2").text())
    low = s2.lower()
    assert "small-scale yielding (ssy)" in low
    assert "plastic zone" in low and "remaining ligament" in low
    assert "method limitation" in low
    assert "not a code restriction" in low and "not a work-scope exclusion" in low
    assert "further analysis" in low and "j-based" in low
    assert "further data" in low
    # the results summary row carries the class in words
    row = next(tr for tr in _section(html_doc, "s2").iter()
               if tr.tag == "tr" and "data-check-row" in tr.attrs
               and tr.attrs["data-check-row"] == "growth_within_ssy")
    rt = row.text().lower()
    assert "method limitation" in rt and "2.369" in rt


def test_results_summary_table_standard_form(html_doc):
    s2 = _section(html_doc, "s2")
    tab = next(n for n in s2.iter() if n.tag == "table" and "results-summary" in n.classes())
    heads = [th.text().strip() for th in tab.iter() if th.tag == "th"]
    assert heads[0] == "Check" and heads[1] == "Criterion"
    assert heads[2].startswith("Demand") and heads[3].startswith("Capacity")
    assert heads[4].startswith("UC") and heads[5] == "Governing case" and heads[6] == "Status"
    rows = [tr for tr in tab.iter() if tr.tag == "tr" and "data-check-row" in tr.attrs]
    assert len(rows) >= 7
    statuses = [tr.elements()[6].text().strip() for tr in rows]
    assert set(statuses) <= STATUSES
    assert {"WITHIN_CRITERION", "EXCEEDS_CRITERION", "NOT_EVALUATED"} <= set(statuses)


# --------------------------------------------------------------------------- #
# Figures: design (model, mesh) and results (end result); executive summary charts
# --------------------------------------------------------------------------- #
def _fblocks(node: _Node) -> list:
    return [n for n in node.iter() if n.tag == "div" and "fblock" in n.classes()]


def _caption(block: _Node) -> str:
    return re.sub(r"\s+", " ", block.elements()[-1].text())


def test_design_section_has_model_and_mesh_pictures(html_doc):
    blocks = {b.attrs.get("data-figure"): b for b in _fblocks(_section(html_doc, "s3"))}
    for fid in ("model", "section", "mesh-global", "mesh-crack"):
        assert fid in blocks, (fid, list(blocks))
    for fid in ("model", "mesh-global", "mesh-crack"):
        cap = _caption(blocks[fid])
        assert any(k.tag == "img" for k in blocks[fid].iter()), fid
        assert "MAPDL" in cap and "2026 R1" in cap and "generator" in cap.lower(), cap
        assert "p0b_" in cap, cap
    assert "_build_section" in _caption(blocks["section"])
    sec_svg = next(k for k in blocks["section"].iter() if k.tag == "svg")
    labels = sec_svg.text().lower()
    assert "crotch" in labels and "fusion" in labels


def test_results_section_has_end_result_pictures(html_doc):
    blocks = {b.attrs.get("data-figure"): b for b in _fblocks(_section(html_doc, "s5"))}
    for fid in ("result-hoop", "result-crack", "k-front"):
        assert fid in blocks, (fid, list(blocks))
    for fid in ("result-hoop", "result-crack"):
        cap = _caption(blocks[fid])
        assert any(k.tag == "img" for k in blocks[fid].iter())
        assert "MAPDL" in cap and "2026 R1" in cap and "p0b_" in cap
    assert "receipt" in _caption(blocks["k-front"]).lower()
    # the results section refers back to the model and mesh figures
    text = _section(html_doc, "s5").text()
    assert "Figure 3." in text


def test_executive_summary_charts_and_conclusions(html_doc):
    s2 = _section(html_doc, "s2")
    blocks = {b.attrs.get("data-figure"): b for b in _fblocks(s2)}
    assert "fad" in blocks and "growth" in blocks
    assert _caption(blocks["fad"]).startswith("Figure 2.1")
    assert _caption(blocks["growth"]).startswith("Figure 2.2")
    box = next(n for n in s2.iter() if "conclusions-box" in n.classes())
    items = [re.sub(r"\s+", " ", li.text()) for li in box.iter() if li.tag == "li"]
    assert any("Figure 2.1" in t for t in items) and any("Figure 2.2" in t for t in items)
    fad_item = next(t for t in items if "Figure 2.1" in t)
    assert "1.89" in fad_item and "2.21" in fad_item
    g_item = next(t for t in items if "Figure 2.2" in t)
    for s in ("1,980", "20,000", "59,339", "3.20"):
        assert s in g_item, s


def test_growth_chart_marks_validity_and_demand(html_doc):
    s2 = _section(html_doc, "s2")
    svg = next(k for b in _fblocks(s2) if b.attrs.get("data-figure") == "growth"
               for k in b.iter() if k.tag == "svg")
    marks = {(n.attrs.get("data-src-x"), n.attrs.get("data-src-y"))
             for n in svg.iter() if n.tag in ("line", "circle", "rect")}
    assert any(y == "result:/growth/life_to_last_ssy_valid/a_mm" for _, y in marks)
    assert any(x == "result:/growth/demand_cycles" for x, _ in marks)
    assert ("result:/growth/life_to_last_ssy_valid/cycles",
            "result:/growth/life_to_last_ssy_valid/a_mm") in marks
    assert ("result:/growth/governing_life_cycles", "result:/growth/a_last_fe_mm") in marks
    text = svg.text().lower()
    assert "demand" in text and "ssy" in text


def test_growth_curve_reintegrated(result):
    """Closed-form comparator: re-integrate the record's own law and table."""
    g = result["growth"]
    law = GrowthLaw(A=g["law"]["A_mpa_sqrt_m"], m=g["law"]["m"], basis="test")
    tab = TabulatedDeltaK(g["table"]["a_mm"], g["table"]["delta_k_mpa_sqrt_m"])
    th = Threshold(result["inputs"]["threshold"]["value"], "none")
    n_last = life(law, tab, g["a0_mm"], g["a_last_fe_mm"], threshold=th).cycles
    assert n_last == pytest.approx(g["governing_life_cycles"], rel=1e-6)
    curve = cfr.growth_curve(result)
    assert curve[0] == (0.0, g["a0_mm"])
    assert curve[-1][0] == pytest.approx(g["governing_life_cycles"], rel=1e-6)
    assert curve[-1][1] == pytest.approx(g["a_last_fe_mm"])
    a_s, n_s = g["life_to_last_ssy_valid"]["a_mm"], g["life_to_last_ssy_valid"]["cycles"]
    n_interp = next(n for (n, a), (n2, a2) in zip(curve, curve[1:]) if a <= a_s <= a2)
    assert n_interp <= n_s * 1.05 + 1.0
    assert all(b[0] > a[0] and b[1] > a[1] for a, b in zip(curve, curve[1:]))


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


def test_utilisation_cells_trace_to_their_sources(html_doc, result, register, meta):
    roots = {"result": result, "register": register, "receipts": meta}
    n = 0
    for node in _tree(html_doc).iter():
        if "data-src-num" not in node.attrs:
            continue
        num = float(_resolve(roots, node.attrs["data-src-num"]))
        den = float(_resolve(roots, node.attrs["data-src-den"]))
        assert _matches(num / den, node.text(), 1.0), (node.attrs, node.text())
        n += 1
    assert n >= 6


def test_every_numeric_cell_has_a_source(html_doc):
    missing = []
    for node in _tree(html_doc).iter():
        if (node.tag == "td" and _NUM.match(node.text().strip())
                and "data-src" not in node.attrs and "data-src-num" not in node.attrs):
            missing.append(node.text().strip())
    assert not missing, missing


def test_svg_points_trace_to_result(html_doc, result, register, meta):
    roots = {"result": result, "register": register, "receipts": meta}
    points = [n for n in _tree(html_doc).iter() if n.tag == "circle" and "data-src-x" in n.attrs]
    assert len(points) >= 12
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


def test_holds_register_rows(html_doc):
    root = _tree(html_doc)
    tab = next(n for n in root.iter() if n.tag == "table" and "holds" in n.classes())
    heads = [th.text().strip() for th in tab.iter() if th.tag == "th"]
    assert heads[:2] == ["ID", "Item"] and "Effect if changed" in heads and heads[-1] == "Status"
    rows = [tr for tr in tab.iter() if tr.tag == "tr" and tr.elements()[0].tag == "td"]
    assert len(rows) >= 10


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
    low = re.sub(r"src=['\"]data:image/png;base64,[^'\"]+['\"]", "", html_doc).lower()
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
    # the box sits in the summary section, before the design basis
    s2 = html_doc.index('<section id="s2"')
    assert s2 < html_doc.index('class="conclusions-box"') < html_doc.index('<section id="s3"')


def test_conclusions_state_criterion_and_disposition(html_doc):
    root = _tree(html_doc)
    items = [n for n in root.iter() if n.tag == "li" and "conclusion" in n.classes()]
    assert len(items) >= 5
    for li in items:
        parts = {c.attrs.get("data-part") for c in li.iter() if c.attrs.get("data-part")}
        assert {"basis", "result", "criterion", "disposition"} <= parts, li.text()[:80]
    recs = [n for n in root.iter() if n.tag == "li" and "recommendation" in n.classes()]
    assert len(recs) >= 5


def test_deterministic_apart_from_date(result, register, meta, figures):
    a = cfr.build_report(result, register, meta, issue_date=DATE, figures=figures)
    b = cfr.build_report(result, register, meta, issue_date=DATE, figures=figures)
    c = cfr.build_report(result, register, meta, issue_date="2027-01-02", figures=figures)
    assert a == b
    assert a != c
    assert a.replace(DATE, "<date>") == c.replace("2027-01-02", "<date>")


def test_build_accepts_result_object(register, meta, figures):
    res = cfa._assess(cfa.load_case(INPUT), _trust_all)
    assert cfr.build_report(res, register, meta, issue_date=DATE, figures=figures) == \
        cfr.build_report(res.to_dict(), register, meta, issue_date=DATE, figures=figures)


def test_receipts_meta_is_host_free(meta):
    blob = json.dumps(meta).lower()
    assert "argv" not in blob and "solve_seconds" not in blob
    assert meta["solver"]["mapdl_release"]
    assert set(meta["states"]) >= {"p0a_verification", "p0b_uncracked"}
    prof = meta["states"]["p0b_crotch_a2p35"]["front_profile"]
    assert len(prof["phi_deg"]) == len(prof["k_gov_mpa_sqrt_m"]) >= 13


def test_generate_and_cli(tmp_path, monkeypatch, result):
    out = tmp_path / "r.html"
    path = cfr.generate(INPUT, out, issue_date=DATE, result=result)
    assert path == out and out.read_text("utf-8").startswith("<!DOCTYPE html>")
    monkeypatch.setattr(cfr, "_run_case", lambda case: result)
    out2 = tmp_path / "cli.html"
    assert cfr.main([str(INPUT), "-o", str(out2), "--date", DATE]) == 0
    assert out2.read_text("utf-8") == out.read_text("utf-8")
