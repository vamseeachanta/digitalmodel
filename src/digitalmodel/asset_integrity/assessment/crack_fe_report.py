# ABOUTME: House-style HTML report of the #2157 crack-like-flaw assessment (P4), built on the
# ABOUTME: CalcReport engine; every table number traces to the result, register or receipts.
"""Crack-like-flaw assessment report (#2157 P4; owner cards R06, J02, J03, G04, S04).

:func:`build_report` renders a :class:`~digitalmodel.asset_integrity.assessment.
crack_fe_assessment.CrackAssessmentResult` (or its ``to_dict()``), the design-data
register and the FE receipt metadata (:func:`receipts_meta`) as one self-contained HTML
page. :func:`generate` and :func:`main` regenerate it from ``input.yml`` with one command::

    python -m digitalmodel.asset_integrity.assessment.crack_fe_report \\
        examples/workflows/crack-fe-weldolet/input.yml -o <dir>/crack-fe-weldolet-report.html

Engine (owner card R06). The page is built on the ``CalcReport`` engine in
:mod:`digitalmodel.reporting.calc_report`: its vendored house stylesheet and scrollspy,
its section, subsection and equation-card markup (:class:`MethodBlock`,
:class:`Equation`), its KPI strip and its revision and reference models. The fixed
seven-section ``CalcReport.render_html`` layout is not used, because the house report
skeleton for an assessment (executive summary, introduction, design basis, assumptions
and limitations, acceptance criteria, methodology, verification, results, checks,
conclusions, recommendations, references, appendices) does not fit it, its design-data
tables put the caption above the table, and its masthead legend carries a
"validated" confidence level that this report may not use without a named referent.

Robustness (owner note on R06):

- every numeric table cell and every number in the summary carries ``data-src``, a
  ``root:/json/pointer`` into the result record (``result``), the register
  (``register``) or the receipt metadata (``receipts``), with ``data-scale`` where the
  display is scaled (percent); the tests resolve each one independently;
- every finding, check, sensitivity, screening bound, evidence item, input and receipt of
  the record is rendered with a ``data-*`` key the tests enumerate;
- assumed inputs show "ASSUMED - to be confirmed" in the register and beside the results
  they affect (owner card J02).

Record wording. Free text from the result record and the register is shown as recorded,
with two wording substitutions applied by :func:`record_text` so the page meets the
engineering register: "validated" in the receipt summary becomes "verified" (its named
referent, the receipt checks, follows it), and the first-person "our own" becomes "the
model's own". Numbers are never changed.

No published case or article is referenced (owner cards S04, B05-B08).
"""

from __future__ import annotations

import argparse
import datetime as _dt
import html
import json
from pathlib import Path
from typing import Any, Mapping, Optional, Sequence

from digitalmodel.asset_integrity.assessment.fad_curves import api579_2016_level2
from digitalmodel.reporting.calc_report import (
    _DEFAULT_TEMPLATE,
    KPI,
    Confidence,
    Equation,
    MethodBlock,
    Reference,
    RevisionEntry,
    VariableDef,
    _template_parts,
    _wordmark,
)

ASSUMED = "ASSUMED - to be confirmed"
REPORT_ID = "DM-FFS-2157-01"
TITLE = "Crack-like flaw assessment of a weldolet attachment-weld root flaw"
DISCIPLINE = "Asset integrity"
ORGANISATION = "AceEngineer"
PREPARED_BY = "crack_fe_report (generated from the result record)"
GUARDS = ("a_equilibrium", "b_mesh_load", "c_contour", "d_complete", "e_sanitised",
          "f_units", "g_j_mesh")
RECORD_GUARDS = ("c_contour_legacy", "g_end_nodes_record")
_RECORD_WORDING = (
    ("declared FE states validated", "declared FE states verified"),
    ("our own", "the model's own"),
)


# --------------------------------------------------------------------------- #
# Receipt metadata (host-free)
# --------------------------------------------------------------------------- #
def _repo_relative(path: Path) -> str:
    from digitalmodel.asset_integrity.assessment.crack_fe_assessment import find_repo_root

    path = Path(path).resolve()
    try:
        return path.relative_to(find_repo_root(path)).as_posix()
    except (FileNotFoundError, ValueError):
        return path.name


def receipts_meta(fe_states_dir: Path) -> dict:
    """Host-free metadata of every declared FE receipt, for the report.

    Carries only what the report states: the solver release, the modelling route, mesh
    sizes, deck hashes, the producing commit, every guard (status, value, limit,
    definition), the P0a Newman-Raju comparison, the uncracked far-field hoop check and
    the limit-load collapse record. Solver argv, run times and platform are left out.
    """
    fe_states_dir = Path(fe_states_dir)
    manifest = json.loads((fe_states_dir / "declared_states.json").read_text("utf-8"))
    states: dict[str, dict] = {}
    versions: set = set()
    releases: set = set()
    top: dict[str, Any] = {}
    for entry in manifest["states"]:
        name = entry["state"]
        rec = json.loads((fe_states_dir / f"{name}.receipt.json").read_text("utf-8"))
        run = rec.get("run", {})
        meshing = rec.get("meshing", {})
        versions.add(run.get("mapdl_version"))
        releases.add(run.get("mapdl_release"))
        crack = rec.get("crack") or {}
        spec = rec.get("spec") or {}
        depth = crack.get("depth_mm", spec.get("crack_depth_mm"))
        if depth is None:
            depth = (spec.get("base") or {}).get("crack_depth_mm")
        gov = rec.get("governing") or {}
        states[name] = {
            "kind": rec["kind"],
            "plane": rec.get("plane"),
            "a_mm": depth,
            "modelling_route": meshing.get("modelling_route"),
            "element": meshing.get("element"),
            "approach": meshing.get("approach"),
            "k_reported": meshing.get("k_reported"),
            "producing_commit": run.get("producing_commit"),
            "mapdl_version": run.get("mapdl_version"),
            "mapdl_release": run.get("mapdl_release"),
            "primary_level": rec.get("primary_level"),
            "meshes": [
                {
                    "level": m["level"],
                    "n_nodes": m.get("n_nodes"),
                    "n_elements": m.get("n_elements"),
                    "deck_sha256": m.get("deck_sha256"),
                    "declared_front_nodes": m.get("declared_front_nodes"),
                    "declared_contours": m.get("declared_contours"),
                }
                for m in rec.get("meshes", [])
            ],
            "guards": {k: dict(v) for k, v in rec.get("guards", {}).items()},
            "governing": {
                k: gov[k] for k in ("e_prime_mpa", "k_gov_max_mpa_sqrt_m", "governing_phi_deg",
                                     "j_at_governing_n_per_mm", "basis") if k in gov
            },
        }
        if rec["kind"] == "verification" and "verification" not in top:
            top["verification"] = {"state": name, "spec": dict(spec),
                                   "comparator": rec.get("comparator", {})}
        if rec["kind"] == "weldolet_uncracked" and "far_field_hoop" not in top:
            hoop = dict((rec.get("plausibility") or {}).get("hoop") or {})
            top["far_field_hoop"] = {"state": name, **hoop}
        if rec["kind"] == "limit_load" and "limit_load" not in top:
            ll = dict(rec.get("limit_load") or {})
            ll["n_rejected_sets"] = len(ll.pop("rejected_sets", []) or [])
            ll.pop("converged_sets", None)
            top["limit_load"] = {"state": name, "material": meshing.get("material"), **ll}
    versions.discard(None)
    releases.discard(None)
    return {
        "fe_states_dir": _repo_relative(fe_states_dir),
        "solver": {
            "program": "ANSYS MAPDL",
            "mapdl_version": ", ".join(sorted(versions)),
            "mapdl_release": ", ".join(sorted(releases)),
        },
        "states": states,
        **top,
    }


# --------------------------------------------------------------------------- #
# Formatting and traceability
# --------------------------------------------------------------------------- #
def record_text(text: Any) -> str:
    """Record free text with the register wording substitutions (numbers untouched)."""
    s = "" if text is None else str(text)
    for old, new in _RECORD_WORDING:
        s = s.replace(old, new)
    return s


def resolve_src(roots: Mapping[str, Any], ref: str) -> Any:
    """Value at ``root:/json/pointer`` (list indices are integers)."""
    root, _, path = ref.partition(":")
    node = roots[root]
    for part in [p for p in path.split("/") if p]:
        node = node[int(part)] if isinstance(node, list) else node[part]
    return node


def fmt(value: Any, spec: str = "f3", scale: float = 1.0, signed: bool = False) -> str:
    """Display form of a record value.

    ``spec``: ``fN`` fixed decimals with thousands separators, ``int`` whole number with
    separators, ``eN`` scientific, ``g`` shortest round-trip form. A numeric pair is a range
    and joins with an en dash; any other list joins with commas. ``None`` renders as ``-``
    (no entry).
    """
    if value is None:
        return "-"
    if isinstance(value, bool):
        return "yes" if value else "no"
    if isinstance(value, str):
        return value
    if isinstance(value, (list, tuple)):
        is_range = len(value) == 2 and all(
            isinstance(v, (int, float)) and not isinstance(v, bool) for v in value)
        sep = " – " if is_range else ", "
        return sep.join(fmt(v, spec, scale, signed) for v in value)
    x = float(value) * scale
    if spec == "int":
        s = f"{x:,.0f}"
    elif spec.startswith("e"):
        s = f"{x:.{int(spec[1:])}E}"
    elif spec == "g":
        s = f"{x:.10g}"
    else:
        s = f"{x:,.{int(spec[1:])}f}"
    if signed and x > 0:
        s = "+" + s
    return s


def _e(text: Any) -> str:
    return html.escape(record_text(text), quote=False)


def _attr(text: str) -> str:
    return html.escape(text, quote=True)


# --------------------------------------------------------------------------- #
# SVG (portable: no clipPath, pattern, filter or mask)
# --------------------------------------------------------------------------- #
class _Axes:
    def __init__(self, x0, x1, y0, y1, w=660, h=400, left=64, right=18, top=18, bottom=52):
        self.x0, self.x1, self.y0, self.y1 = x0, x1, y0, y1
        self.w, self.h, self.l, self.r, self.t, self.b = w, h, left, right, top, bottom

    def x(self, v: float) -> float:
        return self.l + (v - self.x0) / (self.x1 - self.x0) * (self.w - self.l - self.r)

    def y(self, v: float) -> float:
        return self.h - self.b - (v - self.y0) / (self.y1 - self.y0) * (self.h - self.t - self.b)

    def frame(self, xticks, yticks, xlabel, ylabel, xfmt="{:.1f}", yfmt="{:.1f}") -> list:
        out = []
        for v in xticks:
            px = self.x(v)
            out.append(f'<line class="grid" x1="{px:.1f}" y1="{self.y(self.y0):.1f}" '
                       f'x2="{px:.1f}" y2="{self.y(self.y1):.1f}"/>')
            out.append(f'<text class="tick" x="{px:.1f}" y="{self.y(self.y0) + 18:.1f}" '
                       f'text-anchor="middle">{xfmt.format(v)}</text>')
        for v in yticks:
            py = self.y(v)
            out.append(f'<line class="grid" x1="{self.x(self.x0):.1f}" y1="{py:.1f}" '
                       f'x2="{self.x(self.x1):.1f}" y2="{py:.1f}"/>')
            out.append(f'<text class="tick" x="{self.x(self.x0) - 8:.1f}" y="{py + 4:.1f}" '
                       f'text-anchor="end">{yfmt.format(v)}</text>')
        out.append(f'<line class="axis" x1="{self.x(self.x0):.1f}" y1="{self.y(self.y0):.1f}" '
                   f'x2="{self.x(self.x1):.1f}" y2="{self.y(self.y0):.1f}"/>')
        out.append(f'<line class="axis" x1="{self.x(self.x0):.1f}" y1="{self.y(self.y0):.1f}" '
                   f'x2="{self.x(self.x0):.1f}" y2="{self.y(self.y1):.1f}"/>')
        out.append(f'<text class="lab" x="{(self.x(self.x0) + self.x(self.x1)) / 2:.1f}" '
                   f'y="{self.h - 10:.1f}" text-anchor="middle">{xlabel}</text>')
        cy = (self.y(self.y0) + self.y(self.y1)) / 2
        out.append(f'<text class="lab" x="16" y="{cy:.1f}" text-anchor="middle" '
                   f'transform="rotate(-90 16 {cy:.1f})">{ylabel}</text>')
        return out

    def svg(self, body: list, label: str) -> str:
        return (f'<svg viewBox="0 0 {self.w} {self.h}" width="{self.w}" height="{self.h}" '
                f'role="img" aria-label="{_attr(label)}" xmlns="http://www.w3.org/2000/svg">'
                + "".join(body) + "</svg>")


def _ticks(a: float, b: float, step: float) -> list:
    n = int(round((b - a) / step))
    return [round(a + i * step, 10) for i in range(n + 1)]


# --------------------------------------------------------------------------- #
# The report
# --------------------------------------------------------------------------- #
_EXTRA_CSS = """
<style>
  .doc .wrapcell{white-space:normal;text-align:left;min-width:22ch;max-width:60ch}
  .doc td.txt{text-align:left}
  .doc .caption{font-size:13.5px;color:var(--ink-muted);margin:8px 0 22px;font-weight:600}
  .doc .tblock .tbl-wrap{margin-top:14px}
  .doc .fblock .flow{margin-top:14px}
  .doc .conclusions-box{border:2px solid var(--proj);background:var(--proj-soft);border-radius:14px;
    padding:18px 22px;margin:18px 0 8px}
  .doc .conclusions-box h3{margin:0 0 10px;font-size:18px}
  .doc .conclusions-box li,.doc .concl li{margin:0 0 10px;color:var(--ink)}
  .doc .concl{border-left:4px solid var(--proj);background:var(--proj-soft);border-radius:10px;
    padding:14px 20px;margin-top:14px}
  .doc .meaning{border-left:4px solid var(--ro);background:var(--ro-soft);border-radius:10px;
    padding:12px 18px;margin:14px 0}
  .doc .note-assumed{border-left:4px solid var(--proj);background:var(--surface-2);
    border-radius:8px;padding:8px 14px;font-size:14px;margin:10px 0}
  .doc .assumed-label{font-family:var(--mono);font-size:11.5px;color:var(--proj);font-weight:700;
    white-space:nowrap}
  .doc .labeltag{font-family:var(--mono);font-size:11px;font-weight:700;letter-spacing:.06em;
    color:var(--proj);border:1px solid var(--proj);border-radius:6px;padding:1px 6px;margin-right:6px}
  .doc .v{font-family:var(--mono);font-weight:700;color:var(--ink)}
  .doc .coverblock{margin-top:26px}
  .doc .revhist td.blank{min-width:10ch}
  .doc .fblock svg .grid{stroke:var(--hairline);stroke-width:1}
  .doc .fblock svg .axis{stroke:var(--ink-muted);stroke-width:1.4}
  .doc .fblock svg .tick{fill:var(--ink-muted);font-family:var(--mono);font-size:11px}
  .doc .fblock svg .lab{fill:var(--ink-2);font-family:var(--sans);font-size:13px}
  .doc .fblock svg .curve{stroke:var(--ink);stroke-width:2;fill:none}
  .doc .fblock svg .cut{stroke:var(--ink);stroke-width:2;fill:none;stroke-dasharray:6 4}
  .doc .fblock svg .ray{stroke:var(--ro);stroke-width:1.2;fill:none;stroke-dasharray:4 4}
  .doc .fblock svg .ln-gov{stroke:var(--ro);stroke-width:1.8;fill:none}
  .doc .fblock svg .ln-ff{stroke:var(--cfd);stroke-width:1.8;fill:none}
  .doc .fblock svg .ln-th{stroke:var(--proj);stroke-width:1.4;fill:none;stroke-dasharray:6 4}
  .doc .fblock svg .ln-mark{stroke:var(--ink-muted);stroke-width:1.2;fill:none;stroke-dasharray:2 4}
  .doc .fblock svg .pt-gov{fill:var(--ro);stroke:var(--ro)}
  .doc .fblock svg .pt-ff{fill:var(--surface);stroke:var(--cfd);stroke-width:2}
  .doc .fblock svg .pt-rec{fill:var(--surface);stroke:var(--ink-muted);stroke-width:2}
  .doc .fblock svg .pt-sens{fill:var(--proj);stroke:var(--proj)}
  .doc .fblock svg .pt-scr{fill:var(--surface);stroke:var(--proj);stroke-width:2}
  .doc .fblock svg .leg{fill:var(--ink-2);font-family:var(--sans);font-size:12px}
</style>"""


class _Report:
    def __init__(self, result: Mapping, register: Mapping, meta: Mapping, issue_date: str):
        self.r = result
        self.reg = register
        self.m = meta
        self.date = issue_date
        self.roots = {"result": result, "register": register, "receipts": meta}
        self.sec = ""
        self.ntab = 0
        self.nfig = 0
        self.neq = 1
        self.nsub = 0
        self.refs: list[tuple[str, Reference]] = []
        self.ref_no: dict[str, int] = {}
        self.depths = list(result["depths"])
        self.est = [i for i, d in enumerate(self.depths) if d["status"] == "established"]
        f = [self.depths[i]["envelope_margin"]["factor"] for i in self.est]
        self.i_fmin = self.est[f.index(min(f))]
        self.i_fmax = self.est[f.index(max(f))]
        self.reg_items = {it["id"]: (k, it) for k, it in enumerate(register["design_data"])}
        self.fid = {f["id"]: k for k, f in enumerate(result["findings"])}
        first_fad = next(f["id"] for f in result["findings"] if f["id"].startswith("fad."))
        self.fad_cmp = self.fsrc(first_fad, "comparator")          # envelope criterion F > 1
        self.gv_cmp = self.fsrc("check.growth_validity", "comparator")
        self._build_references()

    # ---- primitives ---------------------------------------------------------
    def get(self, ref: str) -> Any:
        return resolve_src(self.roots, ref)

    def v(self, ref: str, spec: str = "f3", scale: float = 1.0, signed: bool = False,
          tag: str = "span", cls: str = "v") -> str:
        value = self.get(ref)
        sc = f' data-scale="{scale:g}"' if scale != 1.0 else ""
        c = f' class="{cls}"' if cls else ""
        return (f'<{tag}{c} data-src="{_attr(ref)}"{sc}>'
                f"{html.escape(fmt(value, spec, scale, signed), quote=False)}</{tag}>")

    def td(self, ref: str, spec: str = "f3", scale: float = 1.0, signed: bool = False,
           cls: str = "") -> str:
        return self.v(ref, spec, scale, signed, tag="td", cls=cls)

    @staticmethod
    def t(text: Any, cls: str = "txt", raw: bool = False) -> str:
        body = text if raw else _e(text)
        return f'<td class="{cls}">{body}</td>'

    def begin(self, label: str) -> None:
        self.sec, self.ntab, self.nfig, self.nsub = label, 0, 0, 0

    def table(self, heads: Sequence[str], rows: Sequence[str], caption: str,
              cls: str = "", intro: str = "") -> str:
        self.ntab += 1
        th = "".join(f"<th>{h}</th>" for h in heads)
        c = f' class="{cls}"' if cls else ""
        pre = f'<p class="prose">{intro}</p>' if intro else ""
        return (f'{pre}<div class="tblock"><div class="tbl-wrap"><table{c}><thead><tr>{th}'
                f'</tr></thead><tbody>{"".join(rows)}</tbody></table></div>'
                f'<p class="caption">Table {self.sec}.{self.ntab} – {caption}</p></div>')

    def figure(self, svg: str, caption: str) -> str:
        self.nfig += 1
        return (f'<div class="fblock"><div class="flow">{svg}</div>'
                f'<p class="caption">Figure {self.sec}.{self.nfig} – {caption}</p></div>')

    def next_table(self) -> str:
        return f"Table {self.sec}.{self.ntab + 1}"

    def next_figure(self) -> str:
        return f"Figure {self.sec}.{self.nfig + 1}"

    def sub(self, key: str, title: str, body: str) -> str:
        self.nsub += 1
        return (f'<div class="l2" id="{key}"><div class="l2head"><span class="n"></span>'
                f"<h3>{title}</h3></div>{body}</div>")

    @staticmethod
    def section(key: str, title: str, subtitle: str, body: str, numbered: bool = True) -> str:
        num = '<span class="secnum"></span>' if numbered else ""
        return (f'<section id="{key}"><div class="l1head">{num}<h2>{title}</h2></div>'
                f'<p class="l1sub">{subtitle}</p>{body}'
                f'<a class="backtop" href="#top">↑ contents</a></section>')

    @staticmethod
    def p(text: str) -> str:
        return f'<p class="prose">{text}</p>'

    @staticmethod
    def assumed_note(text: str) -> str:
        return f'<p class="note-assumed"><span class="assumed-label">{ASSUMED}</span> {text}</p>'

    def cite(self, *keys: str) -> str:
        return "".join(f"[{self.ref_no[k]}]" for k in keys)

    def reg_ref(self, rid: str, field: str = "value", spec: str = "g") -> str:
        k, _ = self.reg_items[rid]
        return self.v(f"register:/design_data/{k}/{field}", spec)

    # ---- references ---------------------------------------------------------
    def _build_references(self) -> None:
        cites = self.r.get("citations", [])

        def add(key: str, text: str, url: Optional[str] = None) -> None:
            self.refs.append((key, Reference(text=text, url=url)))
            self.ref_no[key] = len(self.refs)

        for c in cites:
            name = {"api-std-579-asme-ffs-1": "API 579-1/ASME FFS-1", "bs-7910": "BS 7910"}.get(
                c["code_id"], c["code_id"])
            add(c["code_id"], f"{c['publisher']}, {name} ({c['revision']}), {c['section']}. "
                f"Cited for the procedure only ({c['note']}).")
        add("newman_raju", "J. C. Newman Jr. and I. S. Raju, Stress-intensity factor equations "
            "for cracks in three-dimensional finite bodies, NASA TM-83200, NASA Langley Research "
            "Center, August 1981 (US Government work). Verification comparator, evaluated by "
            "crack_fad.newman_raju_k.", "https://ntrs.nasa.gov/citations/19810023035")
        _, m06 = self.reg_items["M-06"]
        lit = next((x for x in m06.get("reference_ids", []) if x.startswith("docs:literature")),
                   "docs:literature")
        add("nureg_6428", "U.S. Nuclear Regulatory Commission, NUREG/CR-6428 Rev. 1 (ANL/EVS-17/3), "
            "Effects of Thermal Aging on Fracture Toughness and Charpy-Impact Strength of "
            "Stainless Steel Pipe Welds (US Government work): lower-bound J_Ic of austenitic "
            "stainless SA/SMA weld metal (PDF p.54), the basis of the Kmat input (register "
            f"M-06). Archived copy: {lit}.",
            "https://www.govinfo.gov/content/pkg/GOVPUB-Y3_N88-PURL-gpo152202/pdf/"
            "GOVPUB-Y3_N88-PURL-gpo152202.pdf")
        add("nureg_7185", "U.S. Nuclear Regulatory Commission, NUREG/CR-7185 (ANL-14/10), Effect of "
            "Thermal Aging and Neutron Irradiation on Crack Growth Rate and Fracture Toughness of "
            "Cast Stainless Steels and Austenitic Stainless Steel Welds (US Government work), "
            "Eq. 46: the same lower bound, recorded as a cross-reference in register M-06.",
            "https://www.govinfo.gov/content/pkg/GOVPUB-Y3_N88-PURL-gpo59687/pdf/"
            "GOVPUB-Y3_N88-PURL-gpo59687.pdf")
        for ref in self.reg.get("references", []):
            add(ref["id"], f"{ref['citation']}. Archived copy: {ref['source']}; SHA-256 "
                f"{ref['sha256']}; retrieved {ref['retrieved']}.", ref.get("url"))

    def reg_refs(self, ids: Sequence[str]) -> str:
        out = []
        for rid in ids:
            if rid in self.ref_no:
                out.append(f"[{self.ref_no[rid]}]")
            elif rid.startswith("decision:"):
                out.append(f"owner decision {rid.split(':', 1)[1]}")
            elif rid.startswith("docs:literature") and "NUREG-CR-6428" in rid:
                out.append(f"[{self.ref_no['nureg_6428']}]")
            else:
                out.append(rid)
        return ", ".join(out)

    # ---- convenience --------------------------------------------------------
    def dref(self, i: int, path: str) -> str:
        return f"result:/depths/{i}/{path}"

    def fsrc(self, finding_id: str, field: str) -> str:
        return f"result:/findings/{self.fid[finding_id]}/{field}"

    def depth_index(self, key: str) -> int:
        """Index of the depth whose a_mm displays as ``key`` (check-table keys)."""
        return next(i for i, d in enumerate(self.depths) if fmt(d["a_mm"], "f2") == key)

    def first_depth(self, status: str) -> int:
        return next(i for i, d in enumerate(self.depths) if d["status"] == status)

    def a(self, i: int, spec: str = "f2") -> str:
        return self.v(self.dref(i, "a_mm"), spec)

    # ======================================================================= #
    # Cover
    # ======================================================================= #
    def cover(self) -> str:
        r = self.r
        self.begin("F")
        info = [
            f"<tr>{self.t('Document number')}{self.t(REPORT_ID)}</tr>",
            f"<tr>{self.t('Revision')}{self.t('Rev A — issued for owner review')}</tr>",
            f"<tr>{self.t('Date')}{self.t(self.date)}</tr>",
            f"<tr>{self.t('Component')}{self.v('result:/component_id', tag='td', cls='txt')}</tr>",
            f"<tr>{self.t('Procedure')}{self.v('result:/code_reference', tag='td', cls='txt')}</tr>",
            f"<tr>{self.t('Design basis')}"
            f"{self.v('result:/design_basis_status', tag='td', cls='txt wrapcell')}</tr>",
            f"<tr>{self.t('Distribution')}{self.t('Local owner review before any issue')}</tr>",
        ]
        infotab = self.table(["Item", "Entry"], info, "Document control")
        rev = RevisionEntry(revision="A", date=self.date,
                            description="Issued for owner review", by=PREPARED_BY)
        rows = [f'<tr>{self.t(rev.revision)}{self.t(rev.date)}{self.t(rev.description)}'
                f'{self.t(rev.by)}<td class="blank"></td><td class="blank"></td></tr>']
        revtab = self.table(["Rev", "Date", "Description", "Prepared", "Checked", "Approved"],
                            rows, "Revision and approval record", cls="revhist")
        g = r["growth"]
        kpis = [
            KPI(value=self.v("result:/verdict", cls=""), caption="Verdict (engineering criteria "
                "met; a consistency check failed)", confidence=Confidence.ANALYTICAL),
            KPI(value=self.v("result:/evidence_status", cls=""),
                caption="Evidence status: residual-stress and PSF bases missing; passes = false"),
            KPI(value=self.v(self.dref(self.i_fmin, "envelope_margin/factor"), "f2", cls=""),
                caption="Minimum load factor to the FAD envelope (criterion F > 1)",
                confidence=Confidence.ANALYTICAL),
            KPI(value=self.v("result:/growth/life_to_last_ssy_valid/cycles", "int", cls=""),
                unit="cycles", caption="Growth life within the stated SSY validity (demand "
                + fmt(g["demand_cycles"], "int") + " cycles)"),
        ]
        return (
            '<div class="hero" id="cover"><div class="wrap" style="max-width:1240px">'
            f'<div class="kicker"><span class="dot"></span><span>Engineering assessment report '
            f'&middot; {REPORT_ID} &middot; Rev A &middot; {html.escape(self.date)}</span></div>'
            f"<h1>{html.escape(TITLE)}</h1>"
            '<p class="lede">Fitness-for-service assessment of an assumed weld-root flaw by the '
            "API 579-1/ASME FFS-1 (2016) Part 9 Level 2 failure assessment diagram, from crack "
            "driving forces computed with the assessment's own finite-element model, followed by "
            "fatigue crack growth to the last solved crack depth. The whole design basis is "
            f"assumed: every input carries the label <b>{ASSUMED}</b>.</p>"
            f'<div class="kpis">{"".join(k.render() for k in kpis)}</div>'
            f'<div class="coverblock">{infotab}{revtab}</div>'
            "</div></div>"
        )

    # ======================================================================= #
    # 1 Executive summary
    # ======================================================================= #
    def summary(self) -> str:
        r, g = self.r, self.r["growth"]
        self.begin("1")
        imin, imax = self.i_fmin, self.i_fmax
        rule = g["governing_rule"]
        ssy_key = max(r["checks"]["ssy"], key=lambda k: r["checks"]["ssy"][k]["ratio"])
        box = (
            '<div class="conclusions-box"><h3>Governing result and disposition</h3><ul>'
            "<li><b>Governing plane.</b> At every established crack depth ("
            + ", ".join(self.a(i) for i in self.est) + " mm) the crotch radial–axial root flaw "
            "(plane <i>" + self.v(self.dref(self.est[0], "governing_plane"), cls="") + "</i>, "
            "normal to the run-pipe hoop stress) governs: K_gov = "
            + self.v(self.dref(self.est[-1], "k_gov_mpa_sqrt_m"), "f2") + " MPa√m at a = "
            + self.a(self.est[-1]) + " mm against "
            + self.v(self.dref(self.est[-1], "planes/fusion_face/k_gov_mpa_sqrt_m"), "f2")
            + " MPa√m on the fusion-face plane.</li>"
            "<li><b>Fracture and collapse (FAD).</b> Every established depth lies inside the "
            "API 579-1:2016 Level 2 envelope. The load factor to the envelope is F = "
            + self.v(self.dref(imin, "envelope_margin/factor"), "f2") + " (a = " + self.a(imin)
            + " mm) to " + self.v(self.dref(imax, "envelope_margin/factor"), "f2") + " (a = "
            + self.a(imax) + " mm), against the criterion F &gt; "
            + self.v(self.fad_cmp, "f1") + ", at Lr = "
            + self.v(self.dref(imin, "lr"), "f3") + " below the cut-off Lr_max = "
            + self.v("result:/basis/lr_max/value", "f3") + ".</li>"
            "<li><b>Fatigue life to the last FE state.</b> Growth from a0 = "
            + self.v("result:/growth/a0_mm", "f2") + " mm to a = "
            + self.v("result:/growth/a_last_fe_mm", "f2") + " mm takes "
            + self.v("result:/growth/governing_life_cycles", "int") + " cycles, "
            + self.v(f"result:/growth/life_to_last_fe_state/{rule}/margin_on_demand", "f2")
            + " times the demand of " + self.v("result:/growth/demand_cycles", "int")
            + " cycles (threshold rule <i>" + self.v("result:/growth/governing_rule", cls="")
            + "</i> governs).</li>"
            "<li><b>Headline limitation.</b> Only " + self.v("result:/growth/life_to_last_ssy_valid/cycles", "int")
            + " cycles, to a = " + self.v("result:/growth/life_to_last_ssy_valid/a_mm", "f3")
            + " mm, lie within the stated small-scale-yielding validity (Irwin r_p / ligament at "
            "most " + self.v("result:/inputs/ssy_max_ratio/value", "f1") + ", " + ASSUMED
            + "): " + self.v("result:/growth/life_to_last_ssy_valid/margin_on_demand", "f3")
            + " times the demand, <b>below the demand</b>. Meaning: the cyclic plastic zone that "
            "governs growth is a quarter of the monotonic zone at R = 0, and on it the validity "
            "extends to a = " + self.v("result:/growth/life_to_last_cyclic_ssy_valid/a_mm", "f2")
            + " mm, " + self.v("result:/growth/life_to_last_cyclic_ssy_valid/cycles", "int")
            + " cycles (reported for meaning, not gating). The linear-elastic growth life beyond "
            "a = " + self.v("result:/growth/life_to_last_ssy_valid/a_mm", "f3") + " mm is not "
            "established; an elastic-plastic (J-based) growth assessment is needed to close "
            "it.</li>"
            "<li><b>Disposition.</b> Verdict <b>" + self.v("result:/verdict", cls="") + "</b>: "
            "the envelope and life criteria are met, and the failed small-scale-yielding check ("
            + self.v("result:/engineering/failed_checks") + ") downgrades ACCEPT to MONITOR. "
            "Evidence <b>" + self.v("result:/evidence_status", cls="") + "</b>, missing: "
            + self.v("result:/missing_evidence", cls="") + " (no residual-stress basis and no "
            "partial-safety-factor basis is supplied). passes = "
            + str(bool(r["passes"])).lower() + ". This result is not a fitness-for-service "
            "acceptance.</li></ul></div>"
        )
        rows = [
            f"<tr>{self.t('Load factor to the envelope F, minimum over established depths')}"
            f"{self.td(self.dref(imin, 'envelope_margin/factor'))}{self.t('-')}"
            f"{self.td(self.fad_cmp, 'f1')}"
            f"{self.t('inside the envelope at a = ' + self.a(imin) + ' mm', 'txt wrapcell', raw=True)}</tr>",
            f"<tr>{self.t('Load factor to the envelope F, maximum over established depths')}"
            f"{self.td(self.dref(imax, 'envelope_margin/factor'))}{self.t('-')}"
            f"{self.td(self.fad_cmp, 'f1')}"
            f"{self.t('inside the envelope at a = ' + self.a(imax) + ' mm', 'txt wrapcell', raw=True)}</tr>",
            f"<tr>{self.t('Growth life to the last FE state')}"
            f"{self.td('result:/growth/governing_life_cycles', 'int')}{self.t('cycles')}"
            f"{self.td('result:/growth/demand_cycles', 'int')}"
            f"{self.t('exceeds the demand', 'txt wrapcell')}</tr>",
            f"<tr>{self.t('Growth life within the stated SSY validity')}"
            f"{self.td('result:/growth/life_to_last_ssy_valid/cycles', 'int')}{self.t('cycles')}"
            f"{self.td('result:/growth/demand_cycles', 'int')}"
            f"{self.t('below the demand: growth life not established beyond a = ' + self.v('result:/growth/life_to_last_ssy_valid/a_mm', 'f3') + ' mm', 'txt wrapcell', raw=True)}</tr>",
            f"<tr>{self.t('Growth life within cyclic-zone validity (meaning, not gating)')}"
            f"{self.td('result:/growth/life_to_last_cyclic_ssy_valid/cycles', 'int')}{self.t('cycles')}"
            f"{self.td('result:/growth/demand_cycles', 'int')}"
            f"{self.t('reported for meaning only', 'txt wrapcell')}</tr>",
            f"<tr>{self.t('Largest Irwin r_p / ligament over established depths')}"
            f"{self.td(f'result:/checks/ssy/{ssy_key}/ratio')}{self.t('-')}"
            f"{self.td(f'result:/checks/ssy/{ssy_key}/max_ratio', 'f1')}"
            f"{self.t('exceeds the assumed limit (' + self.v('result:/engineering/failed_checks', cls='') + ')', 'txt wrapcell', raw=True)}</tr>",
        ]
        tab = self.table(["Quantity", "Value", "Unit", "Comparator", "Disposition"], rows,
                         "Governing results, criteria and dispositions")
        lim = (
            "<ul>"
            "<li>The design basis is assumed: " + self.v("result:/design_basis_status", cls="")
            + ". Every result is conditional on confirmation of those items (Section 3).</li>"
            "<li>The growth life is established only to a = "
            + self.v("result:/growth/life_to_last_ssy_valid/a_mm", "f3") + " mm under the "
            "stated small-scale-yielding limit (Sections 8.5 and 9.2).</li>"
            "<li>No residual-stress basis is supplied; residual stress appears only as screening "
            "bounds, which are not the disposition (Section 8.7).</li>"
            "<li>No code partial safety factors are supplied; indicative factors of "
            + self.v("result:/sensitivities/psf_indicative/stress_factor", "f1") + " on stress and "
            + self.v("result:/sensitivities/psf_indicative/kmat_factor", "f1") + " on Kmat are a "
            "labelled sensitivity only (Section 8.6).</li>"
            "<li>Kmat = " + self.v("result:/basis/kmat_mpa_sqrt_m", "f1") + " MPa√m is a cited "
            "public lower bound that assumes a flux-welded root " + self.cite("nureg_6428")
            + "; the weld-root process is not confirmed.</li>"
            "<li>The growth law is a user-supplied steel law applied to austenitic weld metal "
            "at the design temperature; its applicability is not confirmed.</li>"
            "</ul>"
        )
        body = (
            self.p("The governing numbers and the disposition are stated first. Every number on "
                   "this page is drawn from the assessment result record, the design-data "
                   "register or the FE receipt record, and each carries its source path.")
            + box + tab
            + self.sub("s1-lim", "Governing limitations", lim)
        )
        return self.section("s1", "Executive summary",
                            "Governing result, disposition and governing limitations.", body)

    # ======================================================================= #
    # 2 Introduction
    # ======================================================================= #
    def introduction(self) -> str:
        self.begin("2")
        _, f02 = self.reg_items["F-02"]
        obj = self.p(
            "The objective is to assess an assumed crack-like root flaw in the attachment weld "
            "of a 6 × ½ STD weldolet on an NPS 6 Sch 40S run pipe of 1.4404 stainless steel at "
            + self.reg_ref("M-07") + " °C under internal pressure of " + self.reg_ref("L-01")
            + " MPa, by the API 579-1/ASME FFS-1 (2016) Part 9 Level 2 failure assessment "
            "diagram " + self.cite("api-std-579-asme-ffs-1") + ", and to estimate the fatigue "
            "crack growth life under pressure cycling against a demand of "
            + self.v("result:/growth/demand_cycles", "int") + " cycles. The crack driving "
            "forces are computed with the assessment's own finite-element model; no external "
            "result is used.")
        scope = (
            "<ul>"
            "<li>Two base-case crack planes from a0 = " + self.v("result:/growth/a0_mm", "f2")
            + " mm: a full-circumference lack-of-fusion flaw on the run-pipe fusion face, and a "
            "semicircular root flaw in the radial–axial plane at the crotch, normal to the "
            "run-pipe hoop stress. The governing plane at each depth is the one with the larger "
            "K_gov.</li>"
            "<li>Solved crack depths: crotch plane "
            + ", ".join(self.a(i) for i in range(len(self.depths)) if "crotch" in self.depths[i]["planes"])
            + " mm; fusion-face plane " + ", ".join(self.a(i) for i in range(len(self.depths)))
            + " mm.</li>"
            "<li>FAD assessment at each depth, fatigue growth between the established depths, "
            "consistency checks, sensitivities and residual-stress screening bounds.</li>"
            "</ul>")
        excl = (
            "<ul>"
            "<li><b>Crotch-arc flaw shape (not modelled).</b> The crotch-arc sensitivity with "
            "a/2c = " + self.reg_ref("F-02") + " (register F-02) is not modelled; the crotch "
            "plane is solved for a semicircular flaw (a/c = " + self.reg_ref("F-06")
            + ", register F-06) only. The effect of a longer flaw on K_gov is Not Evaluated.</li>"
            "<li><b>Fusion-face depths beyond 4.0 mm (not modelled).</b> Register F-04 declares "
            "fusion-face depths to " + self.v(f"register:/design_data/{self.reg_items['F-04'][0]}/value/{len(self.reg_items['F-04'][1]['value']) - 1}", "g")
            + " mm; states beyond " + self.a(len(self.depths) - 1, "f1") + " mm are not solved. "
            "They are not needed for the verdict because the crotch plane reaches its limit "
            "state at " + self.v("result:/inputs/limit_state_crotch/value", "f3")
            + " mm first.</li>"
            "<li><b>Crotch plane at 3.6 mm and beyond.</b> Not solved; the depth "
            + self.a(self.first_depth("governing_plane_not_established"), "f1")
            + " mm is recorded but excluded from the verdict, and the life "
            "from the last FE state to the crotch limit state is Not Evaluated in the base "
            "result (an extrapolated value is reported as a sensitivity).</li>"
            "<li>Growth into the run-pipe wall or through the weld throat along paths other "
            "than the two modelled planes (register G-15).</li>"
            "<li>Weld residual stress as a disposition input; environmental effects on "
            "toughness and growth rate; the FE-derived Option 3 FAD curve.</li>"
            "</ul>")
        body = (self.sub("s2-1", "Objective", obj) + self.sub("s2-2", "Scope", scope)
                + self.sub("s2-3", "Exclusions", excl))
        return self.section("s2", "Introduction", "Objective, scope and exclusions.", body)

    # ======================================================================= #
    # 3 Design basis
    # ======================================================================= #
    def design_basis(self) -> str:
        self.begin("3")
        reg = self.reg
        conv = self.p(
            "The design basis is held in a design-data register (schema <i>"
            + self.v("register:/schema", cls="") + "</i>, generated "
            + self.v("register:/generated", cls="") + "). Every item carries a source class "
            "and a status label; an assumed item reads <span class=\"assumed-label\">" + ASSUMED
            + "</span> with a note stating the reason and the evidence that would confirm it. "
            "Units: " + _e(reg["conventions"]["units"]) + ". Coordinates: "
            + _e(reg["conventions"]["coordinates"]) + ".")
        rows = []
        for cls_name, meaning in reg["source_class_values"].items():
            rows.append(f"<tr>{self.t(cls_name)}{self.t(meaning, 'txt wrapcell')}"
                        f"{self.td(f'register:/summary_counts/{cls_name}', 'int')}</tr>")
        counts = self.table(["Source class", "Meaning", "Items"], rows,
                            "Register source classes and item counts")
        rows = []
        for k, it in enumerate(reg["design_data"]):
            label = (f'<span class="assumed-label">{_e(it["status_label"])}</span>'
                     if it["status_label"] == ASSUMED else _e(it["status_label"]))
            rows.append(
                f'<tr data-register-id="{_attr(it["id"])}">{self.t(it["id"])}'
                f'{self.t(it["parameter"], "txt wrapcell")}'
                f'{self.td(f"register:/design_data/{k}/value", "g", cls="txt wrapcell")}'
                f'{self.t(it["unit"])}{self.t(it["source_class"])}'
                f'{self.t(label, "txt", raw=True)}'
                f'{self.t(it["note"], "txt wrapcell")}'
                f'{self.t(self.reg_refs(it.get("reference_ids", [])), "txt wrapcell")}</tr>')
        full = self.table(["Id", "Parameter", "Value", "Unit", "Source class", "Status", "Note",
                           "Sources"], rows, "Design-data register (all items)",
                          intro="The full register follows. Each assumed item shows its label "
                          "and its confirmation route beside the value.")
        rows = []
        for name, inp in self.r["inputs"].items():
            rows.append(
                f'<tr data-input="{_attr(name)}">{self.t(name)}'
                f'{self.td(f"result:/inputs/{name}/value", "g")}{self.t(inp["unit"] or "-")}'
                f'{self.t(inp["source_class"])}{self.t(inp["register_id"] or "inline")}'
                f'{self.t(inp["status_label"], "txt assumed-label" if inp["status_label"] == ASSUMED else "txt")}'
                f'{self.t(inp["basis"], "txt wrapcell")}</tr>')
        inputs = self.table(["Input", "Value", "Unit", "Source class", "Register id", "Status",
                             "Basis"], rows, "Resolved assessment inputs",
                            intro="The assessment resolves the following inputs from the "
                            "register or from the input file, each with its basis.")
        body = (conv + counts
                + self.sub("s3-reg", "Design-data register", full)
                + self.sub("s3-inputs", "Resolved assessment inputs", inputs)
                + self.sub("s3-sources", "Cited public sources", self.p(
                    "Material properties are read from public datasheets and dimensions from a "
                    "public manufacturer catalogue and distributor charts "
                    + self.cite(*[x["id"] for x in reg.get("references", [])]) + "; archived "
                    "copies are held under docs:literature with their SHA-256 digests. The "
                    "toughness lower bound is cited to NUREG/CR-6428 Rev. 1 "
                    + self.cite("nureg_6428", "nureg_7185") + ".")))
        return self.section("s3", "Design basis and source register",
                            "Assumed design basis, source classes and the resolved inputs.", body)

    # ======================================================================= #
    # 4 Assumptions and limitations
    # ======================================================================= #
    def assumptions(self) -> str:
        self.begin("4")
        items = [
            ("Assumed design basis", "all geometry, material and load inputs",
             "Every result is conditional on confirmation of the register items.",
             "Sections 3, 8"),
            ("Small-scale-yielding limit r_p / ligament ≤ "
             + self.v("result:/inputs/ssy_max_ratio/value", "f1"),
             "growth life beyond a = " + self.v("result:/growth/life_to_last_ssy_valid/a_mm", "f3")
             + " mm",
             "Growth life established only to "
             + self.v("result:/growth/life_to_last_ssy_valid/cycles", "int")
             + " cycles, below the demand.",
             "Sections 8.5, 9.2"),
            ("No residual-stress basis", "Kr (secondary term)",
             "Evidence INCOMPLETE; residual stress enters only as screening bounds.",
             "Sections 8.7, 8.8"),
            ("No code partial safety factors", "Kr and Lr",
             "Evidence INCOMPLETE; indicative factors are a sensitivity only.",
             "Sections 8.6, 8.8"),
            ("Kmat lower bound for a flux-welded root", "Kr at every depth",
             "A different root process changes Kmat (register M-06 note).", "Section 8.1"),
            ("User-supplied steel growth law and E-ratio", "growth life",
             "Applicability to austenitic weld metal at the design temperature is not "
             "confirmed.", "Section 8.5"),
            ("R = 0 pressure cycling and the cycle demand", "ΔK and the life criterion",
             "A different pressure history changes ΔK and the demand.", "Section 8.5"),
            ("Semicircular crotch flaw (register F-06); crotch-arc a/2c = "
             + self.reg_ref("F-02") + " not modelled",
             "K_gov on the governing plane", "The effect of a longer flaw is Not Evaluated.",
             "Section 2.3"),
            ("σ_ref without net-section amplification", "Lr at every depth",
             "Lr is the same at every depth of a plane; the limit-load Lr is a sensitivity.",
             "Sections 6.4, 8.6"),
            ("Shakedown on a stress-component range", "shakedown check",
             "Not an equivalent-stress range.", "Section 9.3"),
            ("No FE state between 3.2 mm and the crotch limit state", "life to the limit state",
             "Not Evaluated in the base result; extrapolated life is a sensitivity.",
             "Section 8.6"),
            ("Crack-face pressure ON as the base case (register L-03)", "K_gov",
             "OFF is run as a sensitivity.", "Section 8.6"),
        ]
        rows = [f"<tr>{self.t('L' + str(n))}{self.t(a, 'txt wrapcell', raw=True)}"
                f"{self.t(b, 'txt wrapcell', raw=True)}{self.t(c, 'txt wrapcell', raw=True)}"
                f"{self.t(d)}</tr>"
                for n, (a, b, c, d) in enumerate(items, 1)]
        tab = self.table(["No.", "Assumption or limitation", "Affects", "Consequence",
                          "Carried in"], rows, "Assumptions and limitations, with the input or "
                         "result each affects")
        body = self.p(
            "Each assumption is stated beside the input it affects (Section 3, and the notes "
            f"marked {ASSUMED} beside the results in Sections 8 and 9). The governing ones are "
            "repeated in the executive summary. The table below collects them with their "
            "consequence.") + tab
        return self.section("s4", "Assumptions and limitations",
                            "What is assumed, what it affects and what follows.", body)

    # ======================================================================= #
    # 5 Acceptance criteria
    # ======================================================================= #
    def criteria(self) -> str:
        self.begin("5")
        rows = [
            f"<tr>{self.t('FAD envelope')}{self.t('assessment point inside the API 579-1:2016 Level 2 curve; load factor to the envelope F &gt; 1', 'txt wrapcell', raw=True)}"
            f"{self.td(self.fad_cmp, 'f1')}{self.t('API 579-1 Part 9 Level 2 ' + self.cite('api-std-579-asme-ffs-1'), 'txt wrapcell')}</tr>",
            f"<tr>{self.t('Lr cut-off Lr_max')}{self.t('flow rule (σ_y + σ_u) / (2 σ_y)', 'txt wrapcell')}"
            f"{self.td('result:/basis/lr_max/value')}{self.v('result:/basis/lr_max/basis', tag='td', cls='txt wrapcell')}</tr>",
            f"<tr>{self.t('Fatigue life')}{self.t('cycles from a0 to the last FE state at least the demand', 'txt wrapcell')}"
            f"{self.td('result:/growth/demand_cycles', 'int')}{self.t('demand, ' + ASSUMED, 'txt wrapcell')}</tr>",
            f"<tr>{self.t('Growth threshold')}{self.t('ΔK above ΔK_th,eff at every depth means growth is predicted (rule none)', 'txt wrapcell')}"
            f"{self.td('result:/growth/threshold_margin/none/dk_th_effective', 'f3')}{self.t('MPa√m; ' + ASSUMED, 'txt wrapcell')}</tr>",
            f"<tr>{self.t('Small-scale yielding')}{self.t('Irwin plane-stress r_p / remaining ligament at most the limit', 'txt wrapcell')}"
            f"{self.td('result:/checks/ssy/2.35/max_ratio', 'f1')}{self.t(self.r['checks']['ssy_basis'], 'txt wrapcell')}</tr>",
            f"<tr>{self.t('σ_ref consistency')}{self.t('σ_ref / σ_implied interval intersects the band', 'txt wrapcell')}"
            f"{self.td('result:/checks/sigma_ref_consistency/2.35/band', 'f1')}{self.t('band fixed in the plan before any run', 'txt wrapcell')}</tr>",
            f"<tr>{self.t('Shakedown')}{self.t('elastic stress range at most 2 σ_y (MPa)', 'txt wrapcell')}"
            f"{self.td('result:/checks/shakedown/limit_mpa', 'f1')}{self.t('2 σ_y with σ_y from register M-03', 'txt wrapcell')}</tr>",
            f"<tr>{self.t('Growth validity')}{self.t('Lr at most 1 at every established depth, else the growth result is CONDITIONAL', 'txt wrapcell')}"
            f"{self.td(self.gv_cmp, 'f1')}{self.t('owner card B15', 'txt wrapcell')}</tr>",
        ]
        tab = self.table(["Check", "Criterion", "Limit", "Basis"], rows,
                         "Acceptance criteria and check limits")
        body = tab + self.p(
            "Verdict logic: " + self.v("result:/engineering/criteria", cls="") + ". "
            "Independently, <i>passes</i> is true only when the verdict is ACCEPT or MONITOR "
            "<b>and</b> the evidence status is COMPLETE, which requires a Kmat basis, an "
            "Lr_max basis, a residual-stress basis, a partial-safety-factor basis, geometry "
            "validity and a verified receipt for every FE state.")
        return self.section("s5", "Acceptance criteria",
                            "Criteria, limits and the verdict logic, stated before the results.",
                            body)

    # ======================================================================= #
    # 6 Methodology
    # ======================================================================= #
    def methodology(self) -> str:
        self.begin("6")
        m = self.m
        crotch = m["states"]["p0b_crotch_a2p35"]
        guard_def = {
            "a_equilibrium": "Equilibrium: the solved axial reaction sum on the constrained end "
                             "equals the applied end load within the limit.",
            "b_mesh_load": "Mesh independence of load: the solved reaction sums of the two mesh "
                           "densities agree within the limit, before any cross-mesh K "
                           "comparison.",
            "c_contour": "Contour independence (redefined by owner card G13): (i) the K spread "
                         "over the last three contours at each front node, normalised by the "
                         "maximum |K| on the front, and (ii) the J spread normalised by that "
                         "node's mean J, each within the limit. The earlier per-node metric is "
                         "undefined where K_I passes through zero and is kept as a record "
                         "(c, legacy).",
            "d_complete": "Extraction completeness: every declared front node has K_I, K_II, "
                          "K_III and J on every contour.",
            "e_sanitised": "Sanitisation: no host or user tokens in the parsed records.",
            "f_units": "Units: mm–N–MPa declared; K converted to MPa√m exactly once.",
            "g_j_mesh": "J mesh convergence (added by owner card G14): J changes by at most the "
                        "limit between the two finest meshes, over interior front nodes (owner "
                        "card G16); the free-surface end-node spread is recorded separately and "
                        "the governing K_gov still includes the end nodes.",
        }
        quantity = {
            "c_contour": "(i) K spread / max |K| on the front; (ii) J spread / node mean J; "
                         "last three contours",
        }
        rows = []
        for gname in GUARDS:
            gd = crotch["guards"][gname]
            what = quantity.get(gname, gd.get("detail") or "-")
            rows.append(f"<tr>{self.t('(' + gname[0] + ')')}{self.t(guard_def[gname], 'txt wrapcell')}"
                        f"{self.td(f'receipts:/states/p0b_crotch_a2p35/guards/{gname}/limit', 'g')}"
                        f"{self.t(what, 'txt wrapcell')}</tr>")
        guard_tab = self.table(["Guard", "Definition", "Limit", "Evaluated quantity"], rows,
                               "Conservation and extraction guards (a)–(g), evaluated on "
                               "solved output")
        fe = MethodBlock(
            heading="Finite-element model",
            prose=(
                "The crack driving forces come from the assessment's own ANSYS MAPDL model ("
                + self.v("receipts:/solver/mapdl_release", cls="") + ", version "
                + self.v("receipts:/solver/mapdl_version", cls="") + "), run in batch through "
                "the fail-closed solver wrapper. Element: " + _e(crotch["element"])
                + ". Modelling route: " + _e(crotch["modelling_route"]) + ". Mesh: "
                "a structured crack block around the front (spider-web rings of elements "
                "about a focused crack-tip tube) embedded in the component mesh; the recorded "
                "meshing statement is quoted in Appendix A. J and K_I, K_II, K_III are "
                "extracted by the CINT contour integral and interaction integral on six "
                "contours, and the reported value is the "
                + _e(crotch["k_reported"]) + ". Each state is solved at two mesh densities "
                "(levels 0 and 1). Internal pressure acts on the bore with the closed-end "
                "thrust applied as an equivalent end pressure; crack-face pressure is ON in the "
                "base case and OFF in a sensitivity. The uncracked model supplies the "
                "linearised stresses for σ_ref and the far-field hoop check."),
        )
        kgov = MethodBlock(
            heading="Governing crack driving force",
            prose=("The governing driving force is taken from J (owner card G14) and maximised "
                   "over all front nodes, including free-surface end nodes (owner card G16). "
                   "The mode mix from the interaction integral is reported alongside."),
            equations=[Equation(
                markup=("<var>K</var><sub>gov</sub> = max<sub>front</sub> "
                        "<span class=\"rad\">√<span class=\"rc\"><var>E</var>′ <var>J</var></span></span>"
                        ", &nbsp; <var>E</var>′ = <span class=\"frac\"><span class=\"n\"><var>E</var></span>"
                        "<span class=\"d\">1 − <var>ν</var><sup>2</sup></span></span>"),
                variables=[VariableDef(symbol="J", description="J-integral, mean of the last three contours", unit="N/mm"),
                           VariableDef(symbol="E", description="Young's modulus at 250 °C (register M-01)", unit="MPa"),
                           VariableDef(symbol="ν", description="Poisson's ratio (register M-02)", unit="-"),
                           VariableDef(symbol="E′", description="plane-strain modulus", unit="MPa")],
                note="E′ = " + self.v("receipts:/states/p0b_crotch_a2p35/governing/e_prime_mpa", "f0")
                     + " MPa in the receipt of the governing crotch state.")],
        )
        fad = MethodBlock(
            heading="Reference stress and failure assessment diagram",
            prose=("σ_ref is the membrane plus bending stress of the uncracked FE linearisation "
                   "on the flaw plane (owner card B02), the largest over the plane's declared "
                   "paths, with no net-section amplification. Record: " + _e(self.r["basis"]["sigma_ref"])
                   + " The load factor F scales the primary terms (Lr, K_P) along a ray until "
                   "the point reaches the envelope; secondary terms are held constant."),
            equations=[
                Equation(markup=("<var>σ</var><sub>ref</sub> = <var>σ</var><sub>m</sub> + "
                                 "<var>σ</var><sub>b</sub>, &nbsp; <var>L</var><sub>r</sub> = "
                                 "<span class=\"frac\"><span class=\"n\"><var>σ</var><sub>ref</sub></span>"
                                 "<span class=\"d\"><var>σ</var><sub>y</sub></span></span>, &nbsp; "
                                 "<var>K</var><sub>r</sub> = <span class=\"frac\"><span class=\"n\">"
                                 "<var>K</var><sub>gov</sub></span><span class=\"d\"><var>K</var>"
                                 "<sub>mat</sub></span></span>"),
                         variables=[VariableDef(symbol="σ<sub>m</sub>, σ<sub>b</sub>", description="linearised membrane and bending stress", unit="MPa"),
                                    VariableDef(symbol="σ<sub>y</sub>", description="0.2 % proof strength at 250 °C (register M-03)", unit="MPa"),
                                    VariableDef(symbol="K<sub>mat</sub>", description="fracture toughness (register M-06)", unit="MPa√m")]),
                Equation(markup=("<var>K</var><sub>r</sub> = (1 − 0.14 <var>L</var><sub>r</sub><sup>2</sup>)"
                                 "(0.3 + 0.7 e<sup>−0.65 <var>L</var><sub>r</sub><sup>6</sup></sup>), "
                                 "&nbsp; <var>L</var><sub>r</sub> ≤ <var>L</var><sub>r,max</sub>"),
                         note="API 579-1:2016 Level 2 curve " + self.cite("api-std-579-asme-ffs-1")
                              + " (edition attribution on secondary sources; the curve form is "
                              "also the original R6 Option 1 curve in the open literature)."),
                Equation(markup=("<var>L</var><sub>r,max</sub> = <span class=\"frac\"><span class=\"n\">"
                                 "<var>σ</var><sub>y</sub> + <var>σ</var><sub>u</sub></span><span class=\"d\">"
                                 "2 <var>σ</var><sub>y</sub></span></span>"),
                         variables=[VariableDef(symbol="σ<sub>u</sub>", description="tensile strength at 250 °C (register M-04)", unit="MPa")]),
            ],
        )
        ll = m["limit_load"]
        limit = MethodBlock(
            heading="Limit load and its collapse criterion",
            prose=(
                "An elastic-perfectly-plastic run of the crotch state at a0 gives the limit "
                "pressure P_L for the sensitivity Lr = p / P_L (owner card S02). Record: "
                + _e(ll["criterion"]) + " Thresholds applied, from the receipt: T1 last "
                "increment at most "
                + self.v("receipts:/limit_load/collapse/thresholds/bisection_exhausted_factor", "g")
                + " × the minimum step; T2 twice-elastic-slope gap at most "
                + self.v("receipts:/limit_load/collapse/thresholds/tes_gap_max", "g", scale=100.0)
                + " %; T3 tangent over elastic stiffness at most "
                + self.v("receipts:/limit_load/collapse/thresholds/tangent_ratio_max", "g", scale=100.0)
                + " %; T4 at least "
                + self.v("receipts:/limit_load/collapse/thresholds/min_converged_substeps", "g")
                + " converged substeps. A run that fails any check produces no receipt, and "
                "every check passed for the receipt used here (Table 8.8)."),
            equations=[Equation(markup="<var>L</var><sub>r</sub><sup>LL</sup> = <span class=\"frac\">"
                                       "<span class=\"n\"><var>p</var></span><span class=\"d\"><var>P</var>"
                                       "<sub>L</sub></span></span>")],
        )
        gl = self.r["growth"]["law"]
        growth = MethodBlock(
            heading="Fatigue crack growth",
            prose=(
                "The growth law is a user input with its basis (owner card G05): A = "
                + self.v("result:/growth/law/A_input", "e3") + " (" + _e(gl["A_input_units"])
                + "), m = " + self.v("result:/growth/law/m", "f1") + ", converted to A = "
                + self.v("result:/growth/law/A_mpa_sqrt_m", "e4") + " in mm/cycle with ΔK in "
                "MPa√m after the E-ratio scaling. Record: " + _e(gl["basis"]) + ". ΔK(a) is "
                "tabulated at the established depths and interpolated piecewise-linearly "
                "with no extrapolation in the base result ("
                + _e(self.r["growth"]["delta_k_basis"]) + "). The life is a composite-Simpson "
                "quadrature; if ΔK falls to the threshold anywhere in the interval the result is "
                "ARRESTED at that depth rather than a finite life. Both threshold temperature "
                "rules are evaluated (owner card B14)."),
            equations=[
                Equation(markup=("<span class=\"frac\"><span class=\"n\">d<var>a</var></span><span class=\"d\">"
                                 "d<var>N</var></span></span> = <var>A</var> Δ<var>K</var><sup><var>m</var></sup>"
                                 ", &nbsp; Δ<var>K</var> = (1 − <var>R</var>) <var>K</var><sub>gov</sub>(<var>a</var>)"),
                         variables=[VariableDef(symbol="R", description="stress ratio (0, pressure cycles from zero)", unit="-")]),
                Equation(markup=("<var>N</var> = ∫<sub><var>a</var><sub>0</sub></sub><sup><var>a</var><sub>f</sub></sup> "
                                 "<span class=\"frac\"><span class=\"n\">d<var>a</var></span><span class=\"d\">"
                                 "<var>A</var> Δ<var>K</var>(<var>a</var>)<sup><var>m</var></sup></span></span>"
                                 ", &nbsp; <var>A</var><sub>T</sub> = <var>A</var> (<var>E</var><sub>ref</sub> / "
                                 "<var>E</var><sub>T</sub>)<sup><var>m</var></sup>")),
            ],
        )
        checks = MethodBlock(
            heading="Consistency checks",
            prose=("Four checks test the internal consistency of the result; a failed check "
                   "turns ACCEPT into MONITOR (owner card J04). The implied crack-opening stress "
                   "is compared with σ_ref over a geometry-factor range; the Irwin plastic zone "
                   "is compared with the remaining ligament; the elastic stress range is "
                   "compared with 2 σ_y; and Lr above 1 at any state marks the growth result "
                   "CONDITIONAL."),
            equations=[
                Equation(markup=("<var>σ</var><sub>impl</sub> = <span class=\"frac\"><span class=\"n\">"
                                 "<var>K</var><sub>gov</sub></span><span class=\"d\"><var>Y</var> "
                                 "<span class=\"rad\">√<span class=\"rc\">π<var>a</var></span></span></span></span>"
                                 ", &nbsp; <var>Y</var> ∈ [2/π, 1.12]")),
                Equation(markup=("<var>r</var><sub>p</sub> = <span class=\"frac\"><span class=\"n\">1</span>"
                                 "<span class=\"d\">2π</span></span>(<span class=\"frac\"><span class=\"n\">"
                                 "<var>K</var></span><span class=\"d\"><var>σ</var><sub>y</sub></span></span>)"
                                 "<sup>2</sup>, &nbsp; <var>r</var><sub>p,cyc</sub> = <span class=\"frac\">"
                                 "<span class=\"n\">1</span><span class=\"d\">2π</span></span>(<span class=\"frac\">"
                                 "<span class=\"n\">Δ<var>K</var></span><span class=\"d\">2<var>σ</var><sub>y</sub>"
                                 "</span></span>)<sup>2</sup>"),
                         note="At R = 0 the cyclic plastic zone is a quarter of the monotonic zone."),
            ],
        )
        screen = MethodBlock(
            heading="Residual-stress screening",
            prose=("Residual stress is not a disposition input because no basis is supplied. "
                   "Three screening bounds (relaxed yield, yield and flow magnitude, owner card "
                   "B11) show its potential effect: a uniform residual stress σ_r on the crack "
                   "plane gives K_S = Y σ_r √(πa), with Y derived by superposition from the "
                   "model's own crack-face-pressure pair and a relaxation factor for the "
                   "relaxed-yield bound (owner card J05); plasticity interaction ρ = 0 is "
                   "stated, not computed."),
        )
        parts = []
        for blk in (fe, kgov, fad, limit, growth, checks, screen):
            rendered, self.neq = blk.render(self.nsub + 1, self.neq)
            self.nsub += 1
            parts.append(rendered)
            if blk is fe:
                parts.append(self.sub("s6-guards", "Guards (a)–(g)", self.p(
                    "Each guard is evaluated independently from the solved output, never from "
                    "the prescribed loads, and each has a negative fixture in the test suite that "
                    "makes that guard alone fail. Guards (c) and (g) were redefined or added by "
                    "owner decisions before the runs they govern; the superseded forms are kept "
                    "as records (Section 7.4).") + guard_tab))
        return self.section("s6", "Analysis methodology",
                            "FE model, guards, driving force, FAD, limit load, growth and checks.",
                            "".join(parts))

    # ======================================================================= #
    # 7 FE model and verification
    # ======================================================================= #
    def verification(self) -> str:
        self.begin("7")
        m = self.m
        ver = m["verification"]
        rows = []
        for key in ("deepest", "surface", "surface_mirror"):
            if key not in ver["comparator"]:
                continue
            base = f"receipts:/verification/comparator/{key}"
            rows.append(f"<tr>{self.t(key.replace('_', ' '))}{self.td(base + '/phi_deg', 'f0')}"
                        f"{self.td(base + '/newman_raju_mpa_sqrt_m', 'f4')}"
                        f"{self.td(base + '/fe_mpa_sqrt_m', 'f4')}"
                        f"{self.td(base + '/error_pct', 'f2', signed=True)}"
                        f"{self.td(base + '/band_pct', 'f1')}"
                        f"{self.t('within the band' if ver['comparator'][key]['within_band'] else 'outside the band')}</tr>")
        spec = "receipts:/verification/spec"
        nr = self.table(["Point", "φ (deg)", "Newman–Raju K (MPa√m)", "FE K (MPa√m)",
                         "Difference (%)", "Band (%)", "Disposition"], rows,
                        "Verification of the CINT extraction against Newman–Raju",
                        intro=("Before the weldolet model was used, the extraction was verified "
                               "on a flat plate with a semi-elliptical surface crack under "
                               "uniform tension (a = " + self.v(spec + "/crack_depth_mm", "f1")
                               + " mm, c = " + self.v(spec + "/crack_half_length_mm", "f1")
                               + " mm, t = " + self.v(spec + "/thickness_mm", "f1") + " mm, σ = "
                               + self.v(spec + "/stress_mpa", "f1") + " MPa) against the "
                               "Newman–Raju solution " + self.cite("newman_raju")
                               + ", within its stated accuracy band."))
        hp = "receipts:/far_field_hoop"
        rows = [f"<tr>{self.t('Hoop stress at the mean radius, far from the branch')}"
                f"{self.td(hp + '/fe_hoop_mpa', 'f2')}{self.td(hp + '/comparator_mpa', 'f2')}"
                f"{self.td(hp + '/relative_error', 'f2', scale=100.0, signed=True)}"
                f"{self.td(hp + '/tolerance', 'f1', scale=100.0)}"
                f"{self.t('within the tolerance' if m['far_field_hoop']['within_tolerance'] else 'outside the tolerance')}</tr>",
                f"<tr>{self.t('Axial stress (closed end)')}{self.td(hp + '/fe_axial_mpa', 'f2')}"
                f"{self.td(hp + '/axial_closed_end_mpa', 'f2')}{self.t('-')}{self.t('-')}"
                f"{self.t('for information')}</tr>"]
        hoop = self.table(["Quantity", "FE (MPa)", "Closed form (MPa)", "Difference (%)",
                           "Tolerance (%)", "Disposition"], rows,
                          "Uncracked far-field check against the thin-wall closed form",
                          intro=("The uncracked weldolet model is checked far from the branch "
                                 "against the Barlow hoop stress on the inside diameter ("
                                 + _e(m["far_field_hoop"].get("comparator", "")) + ")."))
        rows = []
        for name, st in m["states"].items():
            cells = []
            for gname in GUARDS:
                gd = st["guards"].get(gname, {})
                if gd.get("status") == "not_applicable":
                    cells.append(self.t("n/a", "txt"))
                else:
                    cells.append(self.td(f"receipts:/states/{name}/guards/{gname}/value", "e2"))
            gating = [st["guards"][gname]["status"] for gname in GUARDS if gname in st["guards"]]
            status = "all pass" if all(s in ("pass", "not_applicable") for s in gating) else "FAIL"
            rows.append(f"<tr>{self.t(name)}{''.join(cells)}{self.t(status)}</tr>")
        lim = "receipts:/states/p0b_crotch_a2p35/guards"
        heads = ["State"] + [f"({gname[0]}) ≤ " + fmt(m["states"]["p0b_crotch_a2p35"]["guards"][gname]["limit"], "g")
                             for gname in GUARDS] + ["Gating guards"]
        guards = self.table(heads, rows, "Guard values per FE state (n/a: the guard does not "
                            "apply to a model without a crack front or with one mesh density)",
                            intro=("Every guard value per state follows. Limits are those of "
                                   "Table 6.1 (" + ", ".join(
                                       f"({gname[0]}) " + self.v(f"{lim}/{gname}/limit", "g")
                                       for gname in GUARDS) + ")."))
        rows = []
        for name, st in m["states"].items():
            cells = []
            for gname in RECORD_GUARDS:
                gd = st["guards"].get(gname, {})
                if gd.get("status") == "not_applicable" or not gd:
                    cells.append(self.t("n/a") + self.t("-"))
                else:
                    cells.append(self.td(f"receipts:/states/{name}/guards/{gname}/value", "e2")
                                 + self.t(gd["status"]))
            rows.append(f"<tr>{self.t(name)}{''.join(cells)}</tr>")
        records = self.table(["State", "(c) legacy value", "(c) legacy status",
                              "(g) end-node value", "(g) end-node status"], rows,
                             "Record-only guard values (not gating)",
                             intro=("Two guard forms are kept for the record and do not gate "
                                    "the result: the legacy per-node contour metric (owner card "
                                    "G13) and the J mesh change at free-surface end nodes (owner "
                                    "card G16). The end-node record exceeds its 1 % reference "
                                    "value on the crack fronts that end on a free surface; those "
                                    "nodes carry a non-square-root singularity, and K_gov is "
                                    "still taken over all nodes including them. The legacy "
                                    "contour metric fails on the fusion-face front at a0, "
                                    "where K_I passes through zero and the per-node ratio is "
                                    "undefined; that failure is the reason guard (c) was "
                                    "redefined, and it is kept here as evidence."))
        rows = []
        for name, st in m["states"].items():
            ms = {mm["level"]: (k, mm) for k, mm in enumerate(st["meshes"])}
            cells = []
            for level in (0, 1):
                if level in ms:
                    k = ms[level][0]
                    cells.append(self.td(f"receipts:/states/{name}/meshes/{k}/n_nodes", "int")
                                 + self.td(f"receipts:/states/{name}/meshes/{k}/n_elements", "int"))
                else:
                    cells.append(self.t("-") + self.t("-"))
            a_cell = (self.td(f"receipts:/states/{name}/a_mm", "f2") if st["a_mm"] is not None
                      else self.t("n/a"))
            rows.append(f"<tr>{self.t(name)}{self.t(st['kind'])}{self.t(st['plane'] or '-')}"
                        f"{a_cell}{''.join(cells)}{self.t(st['modelling_route'] or '-', 'txt wrapcell')}</tr>")
        meshes = self.table(["State", "Kind", "Plane", "a (mm)", "L0 nodes", "L0 elements",
                             "L1 nodes", "L1 elements", "Modelling route"], rows,
                            "FE states, mesh sizes and modelling route")
        n = len(self.r["receipts"])
        n_ok = sum(1 for x in self.r["receipts"].values() if x["validated"])
        prov = self.p(
            f"Provenance. {n_ok} of {n} declared FE states pass the receipt checks: schema, "
            "provenance (producing commit and generator blobs), re-derivation from the committed "
            "solver artifacts, regeneration of each deck and comparison of its SHA-256 with the "
            "receipt, and recomputation of every gating guard. Record: "
            + _e(self.r["evidence"]["fe_receipts"]["basis"]) + ". The same checks run in "
            "the repository's continuous-integration test suite without a solver licence, and "
            "a missing or stale receipt fails that suite rather than being skipped. Receipt "
            "digests are listed in Appendix A.")
        body = (self.sub("s7-1", "Verification against Newman–Raju", nr)
                + self.sub("s7-2", "Uncracked far-field hoop check", hoop)
                + self.sub("s7-3", "Guard values per state", guards)
                + self.sub("s7-4", "Record-only guards", records)
                + self.sub("s7-5", "States and meshes", meshes)
                + self.sub("s7-6", "Provenance", prov))
        return self.section("s7", "FE model and verification",
                            "Extraction verification, conservation guards and provenance.", body)

    # ======================================================================= #
    # 8 Results
    # ======================================================================= #
    def _fad_svg(self) -> str:
        r = self.r
        lr_cut = r["basis"]["lr_max"]["value"]
        ax = _Axes(0.0, 2.2, 0.0, 1.1)
        body = ax.frame(_ticks(0, 2.2, 0.2), _ticks(0, 1.1, 0.1), "Lr (load ratio)",
                        "Kr (toughness ratio)")
        pts = []
        n = 240
        for k in range(n + 1):
            x = lr_cut * k / n
            pts.append(f"{ax.x(x):.1f},{ax.y(api579_2016_level2(x, lr_cut)):.1f}")
        body.append(f'<polyline class="curve" points="{" ".join(pts)}"/>')
        body.append(f'<line class="cut" x1="{ax.x(lr_cut):.1f}" y1="{ax.y(api579_2016_level2(lr_cut, lr_cut)):.1f}" '
                    f'x2="{ax.x(lr_cut):.1f}" y2="{ax.y(0):.1f}"/>')
        i = self.i_fmin
        em = self.depths[i]["envelope_margin"]
        body.append(f'<line class="ray" x1="{ax.x(0):.1f}" y1="{ax.y(0):.1f}" '
                    f'x2="{ax.x(em["contact_lr"]):.1f}" y2="{ax.y(em["contact_kr"]):.1f}" '
                    f'data-src-x="result:/depths/{i}/envelope_margin/contact_lr" '
                    f'data-src-y="result:/depths/{i}/envelope_margin/contact_kr"/>')

        def circ(xref, yref, cls, rad=5.0):
            x, y = self.get(xref), self.get(yref)
            return (f'<circle class="{cls}" cx="{ax.x(x):.1f}" cy="{ax.y(y):.1f}" r="{rad}" '
                    f'data-src-x="{_attr(xref)}" data-src-y="{_attr(yref)}"/>')

        for i, d in enumerate(self.depths):
            gov_cls = "pt-gov" if d["status"] == "established" else "pt-rec"
            body.append(circ(self.dref(i, "lr"), self.dref(i, "kr"), gov_cls))
            if "fusion_face" in d["planes"] and d["governing_plane"] != "fusion_face":
                body.append(circ(self.dref(i, "planes/fusion_face/lr"),
                                 self.dref(i, "planes/fusion_face/kr"), "pt-ff", 4.0))
        body.append(circ("result:/sensitivities/limit_load_lr/lr",
                         "result:/sensitivities/limit_load_lr/kr", "pt-sens", 4.5))
        for k, _ in enumerate(r["sensitivities"]["psf_indicative"]["depths"]):
            body.append(circ(f"result:/sensitivities/psf_indicative/depths/{k}/lr",
                             f"result:/sensitivities/psf_indicative/depths/{k}/kr", "pt-sens", 3.5))
        for k, _ in enumerate(r["screening"]["residual_bounds"]):
            body.append(circ("result:/depths/0/lr", f"result:/screening/residual_bounds/{k}/kr",
                             "pt-scr", 3.5))
        lx, ly = ax.x(0.95), ax.y(1.04)
        legend = [("pt-gov", "governing crotch plane, established depths"),
                  ("pt-rec", "record-only depths (excluded from the verdict)"),
                  ("pt-ff", "fusion-face plane (non-governing)"),
                  ("pt-sens", "SENSITIVITY: limit-load Lr; indicative factors"),
                  ("pt-scr", "screening: residual-stress bounds at a0")]
        for k, (cls, text) in enumerate(legend):
            y = ly + 18 * k
            body.append(f'<circle class="{cls}" cx="{lx:.1f}" cy="{y:.1f}" r="4.5"/>')
            body.append(f'<text class="leg" x="{lx + 10:.1f}" y="{y + 4:.1f}">{text}</text>')
        body.append(f'<text class="leg" x="{ax.x(lr_cut) - 4:.1f}" y="{ax.y(0.62):.1f}" '
                    f'text-anchor="end">Lr_max = {fmt(lr_cut, "f3")}</text>')
        return ax.svg(body, "Failure assessment diagram with the assessment points")

    def _k_svg(self) -> str:
        g = self.r["growth"]
        ax = _Axes(2.2, 4.2, 0.0, 12.0)
        body = ax.frame(_ticks(2.2, 4.2, 0.2), _ticks(0, 12, 2), "crack depth a (mm)",
                        "K_gov (MPa√m)", yfmt="{:.0f}")
        for rule, cls in (("none", "ln-th"), ("e_ratio", "ln-th")):
            y = g["threshold_margin"][rule]["dk_th_effective"]
            body.append(f'<line class="{cls}" x1="{ax.x(2.2):.1f}" y1="{ax.y(y):.1f}" '
                        f'x2="{ax.x(4.2):.1f}" y2="{ax.y(y):.1f}" '
                        f'data-src-y="result:/growth/threshold_margin/{rule}/dk_th_effective"/>')
        marks = [("result:/growth/life_to_last_ssy_valid/a_mm", "SSY limit"),
                 ("result:/growth/life_to_last_cyclic_ssy_valid/a_mm", "cyclic zone"),
                 ("result:/inputs/limit_state_crotch/value", "crotch limit state")]
        for ref, text in marks:
            x = self.get(ref)
            body.append(f'<line class="ln-mark" x1="{ax.x(x):.1f}" y1="{ax.y(0):.1f}" '
                        f'x2="{ax.x(x):.1f}" y2="{ax.y(12):.1f}" data-src-x="{ref}"/>')
            body.append(f'<text class="leg" x="{ax.x(x) + 4:.1f}" y="{ax.y(11.3) + (0 if text != "cyclic zone" else 16):.1f}">{text}</text>')
        for plane, lcls, pcls in (("crotch", "ln-gov", "pt-gov"), ("fusion_face", "ln-ff", "pt-ff")):
            idx = [i for i, d in enumerate(self.depths) if plane in d["planes"]]
            pts = " ".join(f"{ax.x(self.depths[i]['a_mm']):.1f},"
                           f"{ax.y(self.depths[i]['planes'][plane]['k_gov_mpa_sqrt_m']):.1f}" for i in idx)
            body.append(f'<polyline class="{lcls}" points="{pts}"/>')
            for i in idx:
                xr = self.dref(i, "a_mm")
                yr = self.dref(i, f"planes/{plane}/k_gov_mpa_sqrt_m")
                body.append(f'<circle class="{pcls}" cx="{ax.x(self.get(xr)):.1f}" '
                            f'cy="{ax.y(self.get(yr)):.1f}" r="4.5" data-src-x="{xr}" data-src-y="{yr}"/>')
        lx, ly = ax.x(3.3), ax.y(7.0)
        for k, (cls, text) in enumerate((("pt-gov", "crotch plane (governing)"),
                                          ("pt-ff", "fusion-face plane"))):
            body.append(f'<circle class="{cls}" cx="{lx:.1f}" cy="{ly + 18 * k:.1f}" r="4.5"/>')
            body.append(f'<text class="leg" x="{lx + 10:.1f}" y="{ly + 18 * k + 4:.1f}">{text}</text>')
        body.append(f'<text class="leg" x="{ax.x(2.3):.1f}" y="{ax.y(2.0) - 6:.1f}">ΔK_th,eff '
                    f'(rules none and e_ratio)</text>')
        return ax.svg(body, "Governing crack driving force against crack depth")

    def results(self) -> str:
        self.begin("8")
        r, g = self.r, self.r["growth"]
        # 8.1 FAD table
        rows = []
        for i, d in enumerate(self.depths):
            rows.append(
                f"<tr>{self.td(self.dref(i, 'a_mm'), 'f2')}{self.t(d['status'].replace('_', ' '))}"
                f"{self.t(d['governing_plane'])}{self.td(self.dref(i, 'k_gov_mpa_sqrt_m'), 'f3')}"
                f"{self.td(self.dref(i, 'sigma_ref_mpa'), 'f2')}{self.td(self.dref(i, 'lr'), 'f3')}"
                f"{self.td(self.dref(i, 'kr'), 'f4')}{self.td(self.dref(i, 'envelope_margin/factor'), 'f3')}"
                f"{self.t(d['envelope_margin']['mode'])}"
                f"{self.td(self.dref(i, 'envelope_margin/contact_lr'), 'f3')}"
                f"{self.td(self.dref(i, 'envelope_margin/contact_kr'), 'f4')}"
                f"{self.t(d['disposition'], 'txt wrapcell')}</tr>")
        fad_tab = self.table(["a (mm)", "Status", "Governing plane", "K_gov (MPa√m)",
                              "σ_ref (MPa)", "Lr", "Kr", "F", "Mode", "Contact Lr", "Contact Kr",
                              "Disposition"], rows,
                             "FAD assessment per depth, governing plane (Lr_max = "
                             + self.v("result:/basis/lr_max/value", "f3", cls="") + ")",
                             intro=("The assessment point at each depth, with the load factor "
                                    "F to the envelope and the contact point, follows. Depths "
                                    "whose governing plane is not established are recorded "
                                    "but excluded from the verdict."))
        fad_note = self.assumed_note(
            "Kr uses Kmat = " + self.v("result:/inputs/kmat/value", "f1") + " MPa√m (register "
            "M-06), the cited lower bound for a flux-welded root " + self.cite("nureg_6428")
            + "; Lr uses σ_y = " + self.v("result:/inputs/sigma_y/value", "f1") + " MPa, the "
            "datasheet minimum (register M-03). Kr and Lr are unfactored; no code partial "
            "safety factors are supplied.")
        fad_fig = self.figure(self._fad_svg(),
                              "Failure assessment diagram: API 579-1:2016 Level 2 curve with the "
                              "cut-off, assessment points, sensitivities and screening bounds; "
                              "the dashed ray shows the load factor at the minimum-F depth")
        # 8.3 K(a)
        rows = []
        for i, d in enumerate(self.depths):
            pl = d["planes"]
            if "crotch" in pl:
                c = (self.td(self.dref(i, "planes/crotch/k_gov_mpa_sqrt_m"), "f3")
                     + self.td(self.dref(i, "planes/crotch/ligament_mm"), "f2"))
            else:
                c = self.t("Not Evaluated") + self.t("-")
            f = (self.td(self.dref(i, "planes/fusion_face/k_gov_mpa_sqrt_m"), "f3")
                 + self.td(self.dref(i, "planes/fusion_face/ligament_mm"), "f2"))
            mm = "".join(self.td(self.dref(i, f"mode_mix_mpa_sqrt_m/{k}"), "f3")
                         for k in ("K1", "K2", "K3"))
            rows.append(f"<tr>{self.td(self.dref(i, 'a_mm'), 'f2')}{c}{f}{self.t(d['governing_plane'])}"
                        f"{self.td(self.dref(i, 'j_n_per_mm'), 'f4')}{mm}</tr>")
        k_label = self.next_table()
        k_tab = self.table(["a (mm)", "Crotch K_gov (MPa√m)", "Crotch ligament (mm)",
                            "Fusion-face K_gov (MPa√m)", "Fusion-face ligament (mm)", "Governing",
                            "J at governing node (N/mm)", "K_I (MPa√m)", "K_II (MPa√m)",
                            "K_III (MPa√m)"], rows,
                           "K_gov against depth on both planes, with the mode mix at the "
                           "governing node")
        k_fig = self.figure(self._k_svg(), "K_gov against crack depth on both planes, with the "
                            "effective thresholds, the SSY-valid and cyclic-zone depths and the "
                            "crotch limit state")
        # 8.5 growth
        law = g["law"]
        rows = [
            f"<tr>{self.t('A (input)')}{self.td('result:/growth/law/A_input', 'e3')}{self.t(law['A_input_units'], 'txt wrapcell')}</tr>",
            f"<tr>{self.t('A (after E-ratio, MPa√m units)')}{self.td('result:/growth/law/A_mpa_sqrt_m', 'e4')}{self.t('mm/cycle with ΔK in MPa√m')}</tr>",
            f"<tr>{self.t('m')}{self.td('result:/growth/law/m', 'f1')}{self.t('-')}</tr>",
            f"<tr>{self.t('R')}{self.td('result:/growth/r_ratio', 'f1')}{self.t('-')}</tr>",
            f"<tr>{self.t('E_ref for the E-ratio')}{self.td('result:/inputs/growth_e_ratio/value', 'f1')}{self.t('GPa')}</tr>",
            f"<tr>{self.t('E_T at 250 °C')}{self.td('result:/inputs/growth_e_t/value', 'f1')}{self.t('MPa')}</tr>",
            f"<tr>{self.t('a0')}{self.td('result:/growth/a0_mm', 'f2')}{self.t('mm')}</tr>",
            f"<tr>{self.t('Last FE state')}{self.td('result:/growth/a_last_fe_mm', 'f2')}{self.t('mm')}</tr>",
            f"<tr>{self.t('Demand')}{self.td('result:/growth/demand_cycles', 'int')}{self.t('cycles')}</tr>",
        ]
        law_tab = self.table(["Parameter", "Value", "Unit"], rows, "Growth-law inputs")
        law_note = self.assumed_note(
            "The growth law, R = " + self.v("result:/growth/r_ratio", "f1") + ", the threshold "
            "ΔK_th = " + self.v("result:/inputs/threshold/value", "f1") + " MPa√m and the demand "
            "of " + self.v("result:/inputs/demand_cycles/value", "int") + " cycles are "
            "user-supplied assumptions. Record: " + _e(law["basis"]))
        rows = [f"<tr>{self.td(f'result:/growth/table/a_mm/{k}', 'f2')}"
                f"{self.td(f'result:/growth/table/delta_k_mpa_sqrt_m/{k}', 'f3')}"
                f"{self.td(f'result:/depths/{k}/remaining_life_to_last_fe_cycles', 'int')}</tr>"
                for k in range(len(g["table"]["a_mm"]))]
        dk_tab = self.table(["a (mm)", "ΔK (MPa√m)", "Remaining life to the last FE state (cycles)"],
                            rows, "Tabulated ΔK(a) of the governing plane and the remaining life "
                            "from each depth")
        rows = []
        for rule, rec in g["life_to_last_fe_state"].items():
            base = f"result:/growth/life_to_last_fe_state/{rule}"
            rows.append(f"<tr>{self.t(rule)}{self.t(rec['status'])}{self.td(base + '/cycles', 'int')}"
                        f"{self.td(base + '/margin_on_demand', 'f3')}"
                        f"{self.td(base + '/dk_multiplier_to_demand', 'f3')}"
                        f"{self.td(f'result:/growth/threshold_margin/{rule}/dk_th_effective', 'f3')}"
                        f"{self.td(f'result:/growth/threshold_margin/{rule}/margin', 'f3')}</tr>")
        life_tab = self.table(["Threshold rule", "Status", "Life to last FE state (cycles)",
                               "Margin on demand", "ΔK multiplier to demand",
                               "ΔK_th,eff (MPa√m)", "Threshold margin"], rows,
                              "Growth life to the last FE state under both threshold rules "
                              "(governing rule: " + _e(g["governing_rule"]) + ")")
        s1, s2 = g["life_to_last_ssy_valid"], g["life_to_last_cyclic_ssy_valid"]
        rows = [
            f"<tr>{self.t('Monotonic zone (gating)')}{self.td('result:/growth/life_to_last_ssy_valid/a_mm', 'f3')}"
            f"{self.td('result:/growth/life_to_last_ssy_valid/cycles', 'int')}"
            f"{self.td('result:/growth/life_to_last_ssy_valid/margin_on_demand', 'f3')}"
            f"{self.t(s1['basis'], 'txt wrapcell')}</tr>",
            f"<tr>{self.t('Cyclic zone — ' + s2['label'])}{self.td('result:/growth/life_to_last_cyclic_ssy_valid/a_mm', 'f3')}"
            f"{self.td('result:/growth/life_to_last_cyclic_ssy_valid/cycles', 'int')}{self.t('-')}"
            f"{self.t(s2['basis'], 'txt wrapcell')}</tr>",
        ]
        ssy_tab = self.table(["Validity basis", "Last valid depth (mm)", "Life to that depth (cycles)",
                              "Margin on demand", "Basis"], rows,
                             "Growth life within small-scale-yielding validity (owner card J03)")
        ssy_meaning = (
            '<div class="meaning"><b>Meaning.</b> The growth life of '
            + self.v("result:/growth/governing_life_cycles", "int") + " cycles to the last FE "
            "state rests on linear-elastic K. Under the stated monotonic limit that basis holds "
            "only to a = " + self.v("result:/growth/life_to_last_ssy_valid/a_mm", "f3")
            + " mm, which the crack reaches after " + self.v("result:/growth/life_to_last_ssy_valid/cycles", "int")
            + " cycles, below the demand of " + self.v("result:/growth/demand_cycles", "int")
            + " cycles. The static FAD already accounts for plasticity through Lr, so the "
            "limitation bears on the growth life rather than on the fracture result. On the "
            "cyclic plastic zone, which governs fatigue growth, validity extends to a = "
            + self.v("result:/growth/life_to_last_cyclic_ssy_valid/a_mm", "f3") + " mm and "
            + self.v("result:/growth/life_to_last_cyclic_ssy_valid/cycles", "int") + " cycles, "
            "which would exceed the demand; that reading is reported for meaning and does not "
            "gate the result. Closing the gap needs an elastic-plastic (J-based) growth "
            "assessment of the deeper states.</div>")
        nonrows = []
        for plane, rules in g["non_governing_planes"].items():
            for rule, rec in rules.items():
                base = f"result:/growth/non_governing_planes/{plane}/{rule}"
                nonrows.append(f"<tr>{self.t(plane)}{self.t(rule)}{self.t(rec['status'])}"
                               f"{self.td(base + '/a_arrest_mm', 'f2')}"
                               f"{self.td(base + '/k_gov_max_mpa_sqrt_m', 'f3')}"
                               f"{self.td(f'result:/growth/threshold_margin/{rule}/dk_th_effective', 'f3')}</tr>")
        non_label = self.next_table()
        non_tab = self.table(["Plane", "Rule", "Status", "Arrest depth (mm)",
                              "Largest K_gov (MPa√m)", "ΔK_th,eff (MPa√m)"], nonrows,
                             "Non-governing plane: growth status")
        lls = g["life_to_limit_state"]
        limit_state = self.p("Life to the limit state: <b>" + _e(lls["status"]) + "</b> — "
                             + _e(lls["reason"]) + ".")
        # 8.6 sensitivities
        sens = r["sensitivities"]
        ll = sens["limit_load_lr"]
        rows = [f"<tr>{self.td('result:/sensitivities/limit_load_lr/a_mm', 'f2')}{self.t(ll['plane'])}"
                f"{self.td('result:/sensitivities/limit_load_lr/p_design_mpa', 'f2')}"
                f"{self.td('result:/sensitivities/limit_load_lr/p_limit_mpa', 'f3')}"
                f"{self.td('result:/sensitivities/limit_load_lr/lr', 'f3')}"
                f"{self.td('result:/sensitivities/limit_load_lr/kr', 'f4')}"
                f"{self.td('result:/sensitivities/limit_load_lr/envelope_margin/factor', 'f3')}"
                f"{self.td('receipts:/limit_load/p_tes_mpa', 'f3')}"
                f"{self.td('receipts:/limit_load/tes_gap', 'f2', scale=100.0)}"
                f"{self.td('receipts:/limit_load/tangent_over_elastic_stiffness_last', 'e2')}"
                f"{self.td('receipts:/limit_load/n_converged_substeps', 'int')}</tr>"]
        ll_tab = self.table(["a (mm)", "Plane", "p (MPa)", "P_L (MPa)", "Lr = p/P_L", "Kr", "F",
                             "p_TES (MPa)", "TES gap (%)", "Tangent / elastic", "Converged substeps"],
                            rows, "SENSITIVITY: limit-load Lr (collapse corroborated: "
                            + ", ".join(k for k, v in self.m["limit_load"]["collapse"]["checks"].items() if v)
                            + ")")
        cf = sens["crack_face_pressure_off"]
        rows = [f"<tr>{self.td('result:/sensitivities/crack_face_pressure_off/a_mm', 'f2')}{self.t(cf['plane'])}"
                f"{self.td(self.dref(0, 'k_gov_mpa_sqrt_m'), 'f3')}"
                f"{self.td('result:/sensitivities/crack_face_pressure_off/k_gov_mpa_sqrt_m', 'f3')}"
                f"{self.td('result:/sensitivities/crack_face_pressure_off/lr', 'f3')}"
                f"{self.td('result:/sensitivities/crack_face_pressure_off/kr', 'f4')}"
                f"{self.td('result:/sensitivities/crack_face_pressure_off/envelope_margin/factor', 'f3')}</tr>"]
        cf_tab = self.table(["a (mm)", "Plane", "K_gov, pressure ON (MPa√m)",
                             "K_gov, pressure OFF (MPa√m)", "Lr", "Kr", "F"], rows,
                            "SENSITIVITY: crack-face pressure OFF (state " + _e(cf["state"]) + ")")
        psf = sens["psf_indicative"]
        rows = [f"<tr>{self.td(f'result:/sensitivities/psf_indicative/depths/{k}/a_mm', 'f2')}"
                f"{self.td(f'result:/sensitivities/psf_indicative/depths/{k}/lr', 'f3')}"
                f"{self.td(f'result:/sensitivities/psf_indicative/depths/{k}/kr', 'f4')}"
                f"{self.td(f'result:/sensitivities/psf_indicative/depths/{k}/envelope_margin/factor', 'f3')}"
                f"{self.t('inside' if d['fad_inside'] else 'outside')}</tr>"
                for k, d in enumerate(psf["depths"])]
        psf_tab = self.table(["a (mm)", "Lr", "Kr", "F", "Envelope"], rows,
                             "SENSITIVITY: indicative factors "
                             + self.v("result:/sensitivities/psf_indicative/stress_factor", "f1", cls="")
                             + " on stress and "
                             + self.v("result:/sensitivities/psf_indicative/kmat_factor", "f1", cls="")
                             + " on Kmat (not code partial safety factors)")
        psf_note = self.assumed_note("Record: " + _e(psf["basis"]))
        ex = sens["life_to_ligament_exhaustion"]
        rows = [f"<tr>{self.td(f'result:/sensitivities/life_to_ligament_exhaustion/remaining_by_depth/{k}/a_mm', 'f2')}"
                f"{self.td(f'result:/sensitivities/life_to_ligament_exhaustion/remaining_by_depth/{k}/cycles', 'int')}</tr>"
                for k in range(len(ex["remaining_by_depth"]))]
        rows += [f"<tr>{self.t('from a0, rule ' + rule + ' (' + rec['status'] + ')')}"
                 f"{self.td(f'result:/sensitivities/life_to_ligament_exhaustion/by_rule/{rule}/cycles', 'int')}</tr>"
                 for rule, rec in ex["by_rule"].items()]
        ex_tab = self.table(["From a (mm)", "Cycles to the crotch limit state"], rows,
                            "SENSITIVITY: extrapolated life to ligament exhaustion at "
                            + self.v("result:/sensitivities/life_to_ligament_exhaustion/a_limit_mm", "f3", cls="")
                            + " mm (margin on demand "
                            + self.v("result:/sensitivities/life_to_ligament_exhaustion/margin_on_demand", "f3", cls="")
                            + ")",
                            intro="No FE state supports this range; the values are not a "
                            "result.")

        def sens_block(key: str, heading: str, body: str, lead: Optional[str] = None) -> str:
            text = _e(sens[key].get("basis", "")) if lead is None else lead
            return (f'<div data-sensitivity="{key}"><p class="prose"><span class="labeltag">'
                    f'{_e(sens[key]["label"])}</span><b>{heading}.</b> ' + text
                    + f"</p>{body}</div>")

        sens_html = (
            self.p("Each sensitivity varies one input or modelling choice and is labelled "
                   "SENSITIVITY; none changes the verdict or the evidence status.")
            + sens_block("limit_load_lr", "Limit-load Lr", ll_tab)
            + sens_block("crack_face_pressure_off", "Crack-face pressure OFF", cf_tab,
                         lead="The crotch state at a0 is re-solved with no pressure on the crack "
                         "faces (register L-03).")
            + sens_block("psf_indicative", "Indicative factors", psf_tab + psf_note,
                         lead="Factors on stress and on Kmat that are not code partial safety "
                         "factors; the evidence status is unchanged.")
            + sens_block("life_to_ligament_exhaustion", "Life to ligament exhaustion", ex_tab))
        # 8.7 screening
        sc = r["screening"]
        rows = []
        for k, b in enumerate(sc["residual_bounds"]):
            base = f"result:/screening/residual_bounds/{k}"
            rows.append(f'<tr data-screening="{_attr(b["label"])}">{self.t(b["label"])}{self.t(b["kind"])}'
                        f"{self.td(base + '/sigma_r_mpa', 'f1')}{self.td(base + '/k_secondary', 'f3')}"
                        f"{self.td(base + '/kr', 'f4')}{self.td(base + '/envelope_margin/factor', 'f3')}</tr>")
        sc_tab = self.table(["Bound", "Kind", "σ_r (MPa)", "K_S (MPa√m)", "Kr", "F"], rows,
                            "Residual-stress screening bounds at a0 = "
                            + self.v("result:/screening/a_mm", "f2", cls="")
                            + " mm (screening, not the disposition)",
                            intro=("Screening inputs: Y = " + self.v("result:/screening/y", "f4")
                                   + ", relaxation factor " + self.v("result:/screening/relaxation", "f2")
                                   + ", ρ = " + self.v("result:/screening/rho", "f1") + ". Records: "
                                   + _e(sc["y_basis"]) + "; " + _e(sc["relaxation_basis"]) + "."))
        sc_note = self.assumed_note("Record: " + _e(sc["basis"]))
        # 8.8 evidence
        rows = []
        for name, ev in r["evidence"].items():
            rows.append(f'<tr data-evidence="{_attr(name)}">{self.t(name)}'
                        f'{self.t("established" if ev["established"] else "NOT established")}'
                        f'{self.t(ev["basis"], "txt wrapcell")}</tr>')
        ev_tab = self.table(["Evidence item", "Status", "Basis"], rows,
                            "Evidence completeness (status: " + _e(r["evidence_status"]) + ")")
        body = (
            self.sub("s8-1", "FAD assessment per depth", fad_tab + fad_note)
            + self.sub("s8-2", "Failure assessment diagram", fad_fig)
            + self.sub("s8-3", "Crack driving force on both planes", k_tab + k_fig)
            + self.sub("s8-4", "Governing plane", self.p(
                "The crotch plane governs at every established depth: its K_gov exceeds that of "
                "the fusion-face plane at each depth (" + k_label + "), and the fusion-face K_gov "
                "stays below the growth threshold, so that plane is ARRESTED (" + non_label
                + ")."))
            + self.sub("s8-5", "Fatigue crack growth", law_tab + law_note + dk_tab + life_tab
                       + ssy_tab + ssy_meaning + non_tab + limit_state)
            + self.sub("s8-6", "Sensitivities", sens_html)
            + self.sub("s8-7", "Residual-stress screening", sc_tab + sc_note)
            + self.sub("s8-8", "Evidence completeness", ev_tab)
        )
        return self.section("s8", "Results by governing case",
                            "FAD, driving force, growth, sensitivities, screening and evidence.",
                            body)

    # ======================================================================= #
    # 9 Checks
    # ======================================================================= #
    def checks(self) -> str:
        self.begin("9")
        c = self.r["checks"]
        rows = []
        for key, rec in c["sigma_ref_consistency"].items():
            base = f"result:/checks/sigma_ref_consistency/{key}"
            rows.append(f"<tr>{self.td(self.dref(self.depth_index(key), 'a_mm'), 'f2')}{self.td(base + '/ratio_min', 'f3')}{self.td(base + '/ratio_max', 'f3')}"
                        f"{self.td(base + '/band', 'f1')}{self.td(base + '/y_range', 'f3')}"
                        f"{self.t('consistent' if rec['passed'] else 'INCONSISTENT')}</tr>")
        t1 = ('<div data-check="sigma_ref_consistency">'
              + self.table(["a (mm)", "Ratio min", "Ratio max", "Band", "Y range", "Result"], rows,
                           "σ_ref consistency: σ_ref / σ_implied over the geometry-factor range")
              + "</div>")
        rows = []
        for key, rec in c["ssy"].items():
            base = f"result:/checks/ssy/{key}"
            rows.append(f"<tr>{self.td(self.dref(self.depth_index(key), 'a_mm'), 'f2')}{self.td(base + '/ligament_mm', 'f2')}"
                        f"{self.td(base + '/ratio', 'f3')}{self.td(base + '/cyclic_ratio', 'f3')}"
                        f"{self.td(base + '/max_ratio', 'f1')}{self.t(rec['plastic_zone'])}"
                        f"{self.t('within the limit' if rec['passed'] else 'EXCEEDS the limit')}</tr>")
        t2 = ('<div data-check="ssy">'
              + self.table(["a (mm)", "Ligament (mm)", "r_p / ligament", "r_p,cyc / ligament",
                            "Limit", "Plastic zone", "Result"], rows,
                           "Small-scale yielding: plastic zone against the remaining ligament")
              + self.assumed_note("Record: " + _e(c["ssy_basis"]))
              + f'<div class="meaning">{_e(c["ssy_meaning"])}</div></div>')
        sh = c["shakedown"]
        rows = [f"<tr>{self.t(sh['status'])}{self.td('result:/checks/shakedown/elastic_range_mpa', 'f2')}"
                f"{self.td('result:/checks/shakedown/limit_mpa', 'f1')}{self.td('result:/checks/shakedown/ratio', 'f3')}"
                f"{self.t('within 2 σ_y' if sh['passed'] else 'EXCEEDS 2 σ_y')}{self.t(sh['basis'], 'txt wrapcell')}</tr>"]
        t3 = ('<div data-check="shakedown">'
              + self.table(["Status", "Elastic range (MPa)", "Limit 2σ_y (MPa)", "Ratio", "Result",
                            "Basis"], rows, "Shakedown") + "</div>")
        gv = c["growth_validity"]
        rows = [f"<tr>{self.t(gv['status'])}{self.td('result:/checks/growth_validity/lr_max_seen', 'f3')}"
                f"{self.td(self.gv_cmp, 'f1')}{self.t(gv['reason'], 'txt wrapcell')}</tr>"]
        t4 = ('<div data-check="growth_validity">'
              + self.table(["Status", "Largest Lr", "Limit", "Reason"], rows,
                           "Growth validity (Lr at most 1, owner card B15)") + "</div>")
        rows = []
        for k, f in enumerate(self.r["findings"]):
            base = f"result:/findings/{k}"

            def cell(field: str) -> str:
                val = f[field]
                if isinstance(val, str):
                    return self.t(val)
                if isinstance(val, (int, float)) and abs(val) >= 1000:
                    return self.td(f"{base}/{field}", "int")
                return self.td(f"{base}/{field}", "f4" if isinstance(val, float) or isinstance(val, list) else "g")

            rows.append(f'<tr data-finding="{_attr(f["id"])}">{self.t(f["id"])}'
                        f'{self.t(f["criterion"], "txt wrapcell")}{cell("comparator")}{cell("value")}'
                        f'{self.t(f["disposition"], "txt wrapcell")}</tr>')
        t5 = self.table(["Finding", "Criterion", "Comparator", "Value", "Disposition"], rows,
                        "Findings record: every finding of the result, as recorded",
                        intro="The complete findings record of the result follows; no finding "
                        "is omitted.")
        body = (self.sub("s9-1", "σ_ref consistency", t1)
                + self.sub("s9-2", "Small-scale yielding", t2)
                + self.sub("s9-3", "Shakedown", t3)
                + self.sub("s9-4", "Growth validity", t4)
                + self.sub("s9-5", "Findings record", t5))
        return self.section("s9", "Checks", "Consistency checks and the complete findings record.",
                            body)

    # ======================================================================= #
    # 10 Conclusions, 11 Recommendations
    # ======================================================================= #
    @staticmethod
    def concl(basis: str, result: str, criterion: str, disposition: str) -> str:
        return ('<li class="conclusion">'
                f'<span data-part="basis">{basis}</span> '
                f'<span data-part="result">{result}</span> '
                f'<span data-part="criterion">{criterion}</span> '
                f'<span data-part="disposition"><b>{disposition}</b></span></li>')

    def conclusions(self) -> str:
        self.begin("10")
        r, g = self.r, self.r["growth"]
        imin, imax = self.i_fmin, self.i_fmax
        ff = g["non_governing_planes"]["fusion_face"]["none"]
        items = [
            self.concl(
                "On the assumed design basis, with the model's own FE crack driving forces and "
                "Kmat = " + self.v("result:/basis/kmat_mpa_sqrt_m", "f1") + " MPa√m,",
                "the crotch radial–axial root flaw governs at every established depth, with the "
                "load factor to the envelope ranging from F = "
                + self.v(self.dref(imin, "envelope_margin/factor"), "f3") + " at a = " + self.a(imin)
                + " mm to " + self.v(self.dref(imax, "envelope_margin/factor"), "f3") + " at a = "
                + self.a(imax) + " mm,",
                "against the API 579-1:2016 Level 2 criterion F &gt; "
                + self.v(self.fad_cmp, "f1") + ":",
                "every established depth is inside the envelope."),
            self.concl(
                "With the user-supplied growth law and R = " + self.v("result:/growth/r_ratio", "f1") + ",",
                "the growth life from a0 = " + self.v("result:/growth/a0_mm", "f2") + " mm to the "
                "last FE state at " + self.v("result:/growth/a_last_fe_mm", "f2") + " mm is "
                + self.v("result:/growth/governing_life_cycles", "int") + " cycles,",
                "against the demand of " + self.v("result:/growth/demand_cycles", "int") + " cycles:",
                "the life exceeds the demand by a factor of "
                + self.v(f"result:/growth/life_to_last_fe_state/{g['governing_rule']}/margin_on_demand", "f2")
                + ", conditional on linear-elastic validity (next conclusion)."),
            self.concl(
                "Under the stated small-scale-yielding limit r_p / ligament ≤ "
                + self.v("result:/inputs/ssy_max_ratio/value", "f1") + ",",
                "the linear-elastic growth life is established only to a = "
                + self.v("result:/growth/life_to_last_ssy_valid/a_mm", "f3") + " mm, "
                + self.v("result:/growth/life_to_last_ssy_valid/cycles", "int") + " cycles,",
                "against the demand of " + self.v("result:/growth/demand_cycles", "int") + " cycles:",
                "below the demand; the growth life beyond a = "
                + self.v("result:/growth/life_to_last_ssy_valid/a_mm", "f3") + " mm is not "
                "established. On the cyclic plastic zone (meaning, not gating) validity extends "
                "to " + self.v("result:/growth/life_to_last_cyclic_ssy_valid/a_mm", "f2") + " mm, "
                + self.v("result:/growth/life_to_last_cyclic_ssy_valid/cycles", "int") + " cycles."),
            self.concl(
                "On the fusion-face plane,",
                "the largest K_gov over the solved depths is "
                + self.v("result:/growth/non_governing_planes/fusion_face/none/k_gov_max_mpa_sqrt_m", "f3")
                + " MPa√m,",
                "against the effective threshold ΔK_th,eff = "
                + self.v("result:/growth/threshold_margin/none/dk_th_effective", "f3") + " MPa√m:",
                "no growth is predicted on that plane (" + _e(ff["status"]) + ")."),
            self.concl(
                "For the uncracked elastic stress range at the crotch,",
                "the range is " + self.v("result:/checks/shakedown/elastic_range_mpa", "f1") + " MPa,",
                "against 2 σ_y = " + self.v("result:/checks/shakedown/limit_mpa", "f1") + " MPa:",
                "shakedown is satisfied on the stress-component basis stated."),
            self.concl(
                "Applying the verdict logic and the evidence rules,",
                "the engineering verdict is " + self.v("result:/verdict", cls="")
                + " (failed checks: " + self.v("result:/engineering/failed_checks", cls="")
                + ") and the evidence status is " + self.v("result:/evidence_status", cls="")
                + " (missing: " + self.v("result:/missing_evidence", cls="") + "),",
                "against the requirement of ACCEPT or MONITOR with COMPLETE evidence for passes:",
                "passes = " + str(bool(r["passes"])).lower() + "; the flaw is not accepted as fit "
                "for service on this assessment."),
        ]
        body = (self.p("Each conclusion states its basis, the governing result, the criterion "
                       "and the disposition. Recommendations follow separately in Section 11.")
                + f'<div class="concl"><ul>{"".join(items)}</ul></div>')
        return self.section("s10", "Conclusions",
                            "Basis, governing result, criterion and disposition.", body)

    def recommendations(self) -> str:
        self.begin("11")
        recs = [
            ("Elastic-plastic growth.", "A J-based (elastic-plastic) fatigue growth assessment "
             "of the states beyond a = "
             + self.v("result:/growth/life_to_last_ssy_valid/a_mm", "f3")
             + " mm is recommended; it is needed to establish the growth life against the demand."),
            ("Code partial safety factors.", "Partial safety factors with a basis should be "
             "supplied so that the evidence item psf_basis can be established."),
            ("Residual stress.", "A residual-stress measurement at the weld root, or a cited "
             "profile with a named plasticity-interaction method, should be obtained so that "
             "residual stress enters the disposition rather than screening only."),
            ("Weld toughness.", "Weld-metal and HAZ fracture-toughness tests at 250 °C are "
             "recommended to replace the literature lower bound for Kmat."),
            ("Weld-root process.", "The weld-root process should be confirmed; a confirmed "
             "unaged GTAW root would support a higher Kmat than the flux-weld lower bound "
             "(register M-06)."),
            ("Flaw characterisation.", "NDE sizing of the flaw depth, length and orientation "
             "should be obtained to confirm the assumed flaw planes and shapes (registers F-01, "
             "F-06, G-15), including the crotch-arc shape that is not modelled."),
        ]
        items = "".join(f'<li class="recommendation"><b>{h}</b> {t}</li>' for h, t in recs)
        body = self.p("The following actions are recommended to close the limitations that "
                      "govern the disposition.") + f"<ol>{items}</ol>"
        return self.section("s11", "Recommendations", "Actions that close the limitations.", body)

    # ======================================================================= #
    # 12 References, Appendix A
    # ======================================================================= #
    def references(self) -> str:
        self.begin("12")
        items = []
        for n, (_, ref) in enumerate(self.refs, 1):
            text = _e(ref.text)
            if ref.url and ref.url.startswith(("http://", "https://")):
                text = (f'<a href="{_attr(ref.url)}" rel="noopener noreferrer" '
                        f'target="_blank">{text}</a>')
            items.append(f'<li><span class="rn">[{n}]</span><span>{text}</span></li>')
        body = (self.p("Only the procedures, laws and public data sources used are cited.")
                + f'<ol class="refs">{"".join(items)}</ol>')
        return self.section("s12", "References", "Cited procedures, methods and data sources.",
                            body)

    def appendix(self) -> str:
        self.begin("A")
        m = self.m
        rows = []
        for state, rec in self.r["receipts"].items():
            st = m["states"].get(state, {})
            l0 = next((k for k, mm in enumerate(st.get("meshes", [])) if mm["level"] == 0), None)
            deck = (self.v(f"receipts:/states/{state}/meshes/{l0}/deck_sha256", tag="td", cls="txt")
                    if l0 is not None else self.t("-"))
            rows.append(f'<tr data-receipt="{_attr(state)}">{self.t(state)}{self.t(st.get("kind", "-"))}'
                        f'{self.v(f"result:/receipts/{state}/sha256", tag="td", cls="txt")}'
                        f'{self.t("verified" if rec["validated"] else "NOT verified")}'
                        f'{self.v(f"receipts:/states/{state}/producing_commit", tag="td", cls="txt")}'
                        f"{deck}</tr>")
        rec_tab = self.table(["State", "Kind", "Receipt SHA-256", "Receipt checks",
                              "Producing commit", "Level-0 deck SHA-256"], rows,
                             "FE receipts with digests")
        crotch = m["states"]["p0b_crotch_a2p35"]
        body = (
            '<div class="l2" id="sA-1"><h3>A.1 Receipts</h3>'
            + self.p("Receipts are held in the repository under "
                     + self.v("receipts:/fe_states_dir", cls="") + ". Solver: "
                     + self.v("receipts:/solver/program", cls="") + " "
                     + self.v("receipts:/solver/mapdl_release", cls="") + " (version "
                     + self.v("receipts:/solver/mapdl_version", cls="") + ").")
            + rec_tab + "</div>"
            '<div class="l2" id="sA-2"><h3>A.2 Regeneration</h3>'
            + self.p("The report is regenerated from the input file with one command: "
                     "<code>python -m digitalmodel.asset_integrity.assessment.crack_fe_report "
                     "examples/workflows/crack-fe-weldolet/input.yml -o "
                     "&lt;dir&gt;/crack-fe-weldolet-report.html</code>. The command re-runs the "
                     "assessment, which re-verifies every receipt, and renders this page; apart "
                     "from the issue date the output is deterministic.")
            + "</div>"
            '<div class="l2" id="sA-3"><h3>A.3 Meshing statement (as recorded)</h3>'
            + self.p(_e(crotch["approach"])) + "</div>"
        )
        return self.section("appendix-a", "Appendix A – Reproducibility data",
                            "Receipt digests, solver version and the regeneration command.",
                            body, numbered=False)

    # ======================================================================= #
    # Page
    # ======================================================================= #
    def render(self) -> str:
        cover = self.cover()
        sections = [self.summary(), self.introduction(), self.design_basis(), self.assumptions(),
                    self.criteria(), self.methodology(), self.verification(), self.results(),
                    self.checks(), self.conclusions(), self.recommendations(), self.references(),
                    self.appendix()]
        toc_items = [("s1", "Executive summary"), ("s2", "Introduction"),
                     ("s3", "Design basis and source register"),
                     ("s4", "Assumptions and limitations"), ("s5", "Acceptance criteria"),
                     ("s6", "Analysis methodology"), ("s7", "FE model and verification"),
                     ("s8", "Results by governing case"), ("s9", "Checks"),
                     ("s10", "Conclusions"), ("s11", "Recommendations"), ("s12", "References")]
        toc = ('<nav class="toc" aria-label="Contents"><p class="tt">Contents</p><ol>'
               + "".join(f'<li><a href="#{k}">{v}</a></li>' for k, v in toc_items)
               + '</ol><p class="tt" style="margin-top:14px"><a href="#appendix-a">Appendix A '
               "– Reproducibility data</a></p></nav>")
        style, script = _template_parts(_DEFAULT_TEMPLATE)
        return f"""<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>{html.escape(TITLE)} &middot; {REPORT_ID}</title>
{style}{_EXTRA_CSS}
</head><body>
<div class="doc" id="top">
  <header class="masthead"><div class="wrap" style="max-width:1240px">
    <div class="brand">{_wordmark(ORGANISATION)} &middot; {DISCIPLINE}</div>
    <div class="mh-legend"><span class="lg">{REPORT_ID} &middot; Rev A &middot; {html.escape(self.date)}</span></div>
  </div></header>
  {cover}
  <div class="shell">
    {toc}
    <main class="main">{"".join(sections)}</main>
  </div>
  <footer><div class="wrap" style="max-width:1240px">
    <div class="foot-grid">
      <div><h3>{REPORT_ID} &middot; Rev A</h3>
        <p>Issued for owner review, {html.escape(self.date)}. Design basis assumed throughout.</p></div>
      <div><h3>Provenance</h3><ul>
        <li>Numbers carry their source path (data-src).</li>
        <li>FE receipts verified by schema, deck-hash and guard checks.</li></ul></div>
    </div>
    <div class="foot-bar"><span>{ORGANISATION} &middot; {DISCIPLINE}</span>
      <span>House calculation report format (CalcReport engine)</span></div>
  </div></footer>
</div>
{script}
</body></html>"""


def build_report(result: Any, register: Mapping, receipts_meta: Mapping, *,
                 issue_date: Optional[str] = None) -> str:
    """The report HTML for a crack assessment result (object or ``to_dict()``).

    ``issue_date`` (ISO date) is the only non-deterministic input; it defaults to today.
    """
    record = result.to_dict() if hasattr(result, "to_dict") else dict(result)
    record = json.loads(json.dumps(record))  # plain JSON types, as the data-src paths see them
    date = issue_date or _dt.date.today().isoformat()
    return _Report(record, register, receipts_meta, date).render()


def _run_case(case: Mapping) -> dict:
    from digitalmodel.asset_integrity.assessment.crack_fe_assessment import run

    return run(case).to_dict()


def generate(input_path: Path, out_path: Path, *, issue_date: Optional[str] = None,
             result: Any = None) -> Path:
    """Run the assessment from ``input_path`` (unless ``result`` is given) and write the report."""
    from digitalmodel.asset_integrity.assessment.crack_fe_assessment import load_case

    case = load_case(Path(input_path))
    repo = Path(case["_repo_root"])
    if result is None:
        result = _run_case(case)
    register = json.loads((repo / case["design_data_register"]).read_text("utf-8"))
    meta = receipts_meta(repo / case["fe_states_dir"])
    page = build_report(result, register, meta, issue_date=issue_date)
    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(page, encoding="utf-8")
    return out_path


def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(description="Regenerate the #2157 crack-like-flaw report.")
    ap.add_argument("input", nargs="?", default="examples/workflows/crack-fe-weldolet/input.yml")
    ap.add_argument("-o", "--output", default="crack-fe-weldolet-report.html")
    ap.add_argument("--date", default=None, help="issue date (ISO); default today")
    a = ap.parse_args(list(argv) if argv is not None else None)
    path = generate(Path(a.input), Path(a.output), issue_date=a.date)
    print(path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
