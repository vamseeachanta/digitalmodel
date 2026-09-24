"""Reconcile a rendered riser stack-up SVG against its spec (#2152).

``reconcile(spec, svg_text) -> dict`` depends only on :mod:`.schema` and the
standard library. It reconstructs the elevation transform from the zone table
embedded in the SVG (it does not import the renderer), then checks:

  (a) mapping   - every spec component drawn exactly once; no orphan groups;
                  group data-* attributes equal the spec; every component has a callout
  (b) positions - body top/bottom y equals T(elevation) within 0.5 px; joint seams in
                  runs (including the condensed zone) sit at T(top - i*L); datum lines,
                  sheaves and axis ticks sit at T(z)
  (c) numbers   - every printed number is traceable (data-field) and equals the spec
                  value at its printed rounding; unlabelled numbers fail
  (d) totals    - component lengths sum to the stack-up length, elevations are
                  continuous, and water depth + air gap closes (to the source-flagged residual)
  (e) NOT_FOUND - never printed or attributed as a number; shown as grey "n/a"

CLI::

    python -m digitalmodel.drilling_riser.stackup_drawing.reconcile SPEC.json SVG.svg \
        [--out reconcile.json]

exits 1 on failure.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Any

from digitalmodel.drilling_riser.stackup_drawing.schema import (
    NESTED_TYPES,
    NOT_FOUND,
    StackupDrawingSpec,
    not_found_fields,
)
from digitalmodel.drilling_riser.stackup_drawing.schema import (
    ComponentType as CT,
)

__all__ = ["PX_TOL", "reconcile"]

NS = "{http://www.w3.org/2000/svg}"
M2FT = 1.0 / 0.3048
PX_TOL = 0.5
ALLOWED_ROLES = {
    "decor",
    "axis",
    "datum",
    "datum-label",
    "tensioner",
    "component",
    "callout",
    "break",
    "titleblock",
}
NUM_RE = re.compile(r"[+\-−]?\d[\d,]*(?:\.\d+)?")


class _T:
    """Independent inverse/forward of the embedded zone table."""

    def __init__(self, zones: list[dict]):
        self.z = zones

    def y(self, z: float, prefer: str = "upper") -> float:
        c = [zn for zn in self.z if zn["z_lo"] - 1e-9 <= z <= zn["z_hi"] + 1e-9]
        zn = (
            (c[0] if prefer == "upper" else c[-1])
            if c
            else (self.z[0] if z > self.z[0]["z_hi"] else self.z[-1])
        )
        return zn["y_top"] + (zn["z_hi"] - z) * zn["px_per_m"]


def _num(text: str) -> float:
    return float(
        text.replace(",", "").replace("−", "-").replace("+", "").replace("†", "")
    )


def _known(v: Any) -> bool:
    return v is not None and v != NOT_FOUND


def _path(d: Any, path: str) -> Any:
    for part in path.split("."):
        if isinstance(d, dict):
            if part not in d:
                raise KeyError(path)
            d = d[part]
        else:
            raise KeyError(path)
    return d


def reconcile(spec: StackupDrawingSpec, svg_text: str) -> dict[str, Any]:
    """Check ``svg_text`` against ``spec``; ``result`` is ``"pass"`` or ``"fail"``.

    The report carries one entry per check (``a_mapping`` .. ``e_not_found``)
    with ``status``, ``failures`` and ``notes``, plus the spec's NOT_FOUND
    paths, data conflicts and gaps.
    """
    root = ET.fromstring(svg_text)
    parent = {c: p for p in root.iter() for c in p}
    sd = spec.to_dict()
    comps = {c.id: c for c in spec.components}
    res: dict[str, dict[str, Any]] = {
        k: {"status": "pass", "failures": [], "notes": []}
        for k in ("a_mapping", "b_positions", "c_numbers", "d_totals", "e_not_found")
    }

    def fail(k, msg):
        res[k]["status"] = "fail"
        res[k]["failures"].append(msg)

    def note(k, msg):
        res[k]["notes"].append(msg)

    def role_of(el):
        while el is not None:
            r = el.get("data-role")
            if r:
                return r, el
            el = parent.get(el)
        return None, None

    tf = next(
        (g for g in root.iter(NS + "g") if g.get("id") == "elevation-transform"), None
    )
    if tf is None:
        fail("b_positions", "no embedded elevation transform")
        return _finish(spec, res)
    zones = json.loads(tf.get("data-zones"))
    T = _T(zones)
    px_per_in = float(tf.get("data-px-per-in"))
    y_bottom = float(tf.get("data-y-bottom"))
    z_min = zones[-1]["z_lo"]
    derived = {
        "derived.diameter_exaggeration": px_per_in
        * (1 / 0.0254)
        / zones[0]["px_per_m"],
        "derived.condensed_ratio": (
            (zones[0]["px_per_m"] / zones[1]["px_per_m"]) if len(zones) > 1 else None
        ),
        "derived.wd_plus_air_gap_m": (
            float(spec.datums.water_depth_m) + float(spec.datums.air_gap_m)
            if _known(spec.datums.water_depth_m) and _known(spec.datums.air_gap_m)
            else NOT_FOUND
        ),
    }

    # (a) mapping -----------------------------------------------------------------
    groups = list(root.iter(NS + "g"))
    comp_groups: dict[str, list] = {}
    for g in groups:
        r = g.get("data-role")
        cid = g.get("data-component-id")
        if r is None and cid is None:
            fail(
                "a_mapping",
                f"orphan <g> without data-role/data-component-id (id={g.get('id')})",
            )
            continue
        if r is not None and r not in ALLOWED_ROLES:
            fail("a_mapping", f"unknown data-role {r!r}")
        if r == "component":
            comp_groups.setdefault(cid, []).append(g)
            if cid not in comps:
                fail("a_mapping", f"component group {cid!r} not in spec (orphan)")
        if r == "callout" and cid not in comps:
            fail("a_mapping", f"callout for unknown component {cid!r}")
        if r == "decor":
            for t in g.iter(NS + "text"):
                if re.search(r"\d", "".join(t.itertext())):
                    fail("a_mapping", "decor element carries a number")
    for cid, c in comps.items():
        gl = comp_groups.get(cid, [])
        if len(gl) != 1:
            fail("a_mapping", f"{cid}: drawn {len(gl)} times (expected exactly once)")
            continue
        g = gl[0]
        expect = {
            "data-type": c.type.value,
            "data-count": str(c.count),
            "data-source": c.source,
            "data-od-in": "" if c.od_in is None else str(c.od_in),
        }
        for k, v in expect.items():
            if g.get(k) != v:
                fail("a_mapping", f"{cid}: {k}={g.get(k)!r} != spec {v!r}")
        for k, f in (
            ("data-top-el-m", c.top_el_m),
            ("data-bottom-el-m", c.bottom_el_m),
        ):
            a = g.get(k)
            if _known(f):
                if a in (None, NOT_FOUND) or abs(float(a) - float(f)) > 1e-9:
                    fail("a_mapping", f"{cid}: {k}={a!r} != spec {f!r}")
            elif a != str(f):
                fail("a_mapping", f"{cid}: {k}={a!r} but spec is {f!r}")
        if not any(t.get("data-component-id") == cid for t in root.iter(NS + "text")):
            fail("a_mapping", f"{cid}: no callout text")
    note(
        "a_mapping",
        f"{len(comps)} spec components, {sum(len(v) for v in comp_groups.values())} component groups",
    )

    # (b) positions ---------------------------------------------------------------
    def ext(el):
        if el.tag == NS + "rect":
            y, h = float(el.get("y")), float(el.get("height"))
            return y, y + h
        if el.tag == NS + "line":
            y1, y2 = float(el.get("y1")), float(el.get("y2"))
            return min(y1, y2), max(y1, y2)
        return None

    max_dev = 0.0
    for cid, c in comps.items():
        gl = comp_groups.get(cid, [])
        if len(gl) != 1:
            continue
        g = gl[0]
        bodies = [e for e in g.iter() if e.get("data-part") == "body"]
        if not (_known(c.top_el_m) and _known(c.bottom_el_m)):
            if bodies:
                fail("b_positions", f"{cid}: elevation NOT_FOUND but a body is drawn")
            else:
                note("b_positions", f"{cid}: not drawn (elevation NOT_FOUND)")
            continue
        if not bodies:
            fail("b_positions", f"{cid}: no body element")
            continue
        exts = [ext(e) for e in bodies if ext(e)]
        y_top, y_bot = min(e[0] for e in exts), max(e[1] for e in exts)
        e_top = T.y(float(c.top_el_m), "upper")
        clipped = float(c.bottom_el_m) < z_min - 1e-9
        e_bot = y_bottom if clipped else T.y(float(c.bottom_el_m), "lower")
        if clipped and g.get("data-clipped") != "bottom":
            fail(
                "b_positions",
                f"{cid}: extends below the sheet but is not flagged data-clipped",
            )
        for lab, drawn, exp in (("top", y_top, e_top), ("bottom", y_bot, e_bot)):
            dev = abs(drawn - exp)
            max_dev = max(max_dev, dev)
            if dev > PX_TOL:
                fail(
                    "b_positions",
                    f"{cid}: drawn {lab} y={drawn:.2f} vs T(el)={exp:.2f} (|d|={dev:.2f} px)",
                )
        # joint seams (verifies count x length inside the condensed break too)
        if c.type in (CT.RISER_JOINT_BARE, CT.RISER_JOINT_BUOYANT) and _known(
            c.joint_length_m
        ):
            n = int(c.count)
            tag = NS + ("line" if c.type == CT.RISER_JOINT_BUOYANT else "rect")
            seams = sorted(
                (
                    float(e.get("y1"))
                    if e.tag == NS + "line"
                    else float(e.get("y")) + float(e.get("height")) / 2
                )
                for e in g.iter(tag)
                if e.get("data-part") == "seam"
            )
            exp = sorted(
                T.y(float(c.top_el_m) - i * float(c.joint_length_m))
                for i in range(1, n)
            )
            if len(seams) != len(exp):
                fail(
                    "b_positions",
                    f"{cid}: {len(seams)} joint seams drawn, data implies {len(exp)}",
                )
            else:
                bad = [(a, b) for a, b in zip(seams, exp) if abs(a - b) > PX_TOL]
                if bad:
                    fail(
                        "b_positions",
                        f"{cid}: {len(bad)} joint seams off T(top - i*L) by > {PX_TOL} px",
                    )
                zk = {
                    zn["kind"]
                    for zn in zones
                    if zn["z_lo"] < float(c.top_el_m)
                    and zn["z_hi"] > float(c.bottom_el_m)
                }
                note(
                    "b_positions",
                    f"{cid}: {n} joints x {float(c.joint_length_m):.4f} m, {len(seams)} seams at "
                    f"T(top-i*L) (zones: {', '.join(sorted(zk))})",
                )
    for g in groups:
        if g.get("data-role") == "datum":
            name = g.get("data-datum")
            val = getattr(spec.datums, name)
            for ln in g.iter(NS + "line"):
                if ln.get("data-part") == "datum-line":
                    dev = abs(float(ln.get("y1")) - T.y(float(val)))
                    if dev > PX_TOL:
                        fail("b_positions", f"datum {name}: line off by {dev:.2f} px")
        if g.get("data-role") == "tensioner" and spec.tensioner_system is not None:
            for cc in g.iter(NS + "circle"):
                if cc.get("data-part") == "sheave":
                    dev = abs(
                        float(cc.get("cy"))
                        - T.y(float(spec.tensioner_system.sheave_el_m))
                    )
                    if dev > PX_TOL:
                        fail("b_positions", f"tensioner sheave off by {dev:.2f} px")
    n_ticks = 0
    for ln in root.iter(NS + "line"):
        z = ln.get("data-tick-el-m")
        if z is not None:
            n_ticks += 1
            dev = abs(float(ln.get("y1")) - T.y(float(z)))
            if dev > PX_TOL:
                fail("b_positions", f"axis tick {z} off by {dev:.2f} px")
    note(
        "b_positions",
        f"max component edge deviation {max_dev:.3f} px; {n_ticks} axis ticks checked",
    )

    # (c) numbers + (e) NOT_FOUND -----------------------------------------------------
    n_checked = 0
    for t in root.iter(NS + "text"):
        cid = t.get("data-component-id")
        r, _ = role_of(t)
        for sp in t.iter(NS + "tspan"):
            txt = sp.text or ""
            fld = sp.get("data-field")
            if "na" in (sp.get("class") or "").split():
                if fld is None:
                    continue
                exp = _resolve(fld, cid, comps, sd, derived)
                if exp != NOT_FOUND:
                    fail(
                        "e_not_found",
                        f"{cid or ''}:{fld} shown as n/a but spec value is {exp!r}",
                    )
                continue
            if not re.search(r"\d", txt):
                continue
            if sp.get("data-tick-el-m") is not None:
                z = float(sp.get("data-tick-el-m"))
                v = z * (M2FT if sp.get("data-unit") == "ft" else 1.0)
                if abs(_num(txt) - v) > 0.5 + 1e-6:
                    fail("c_numbers", f"axis label {txt!r} != {v:.3f}")
                n_checked += 1
                continue
            if fld is None:
                fail(
                    "c_numbers",
                    f"untraceable number {txt!r} (role={r}, component={cid})",
                )
                continue
            if sp.get("data-kind") == "text":
                exp = _path(sd, fld)
                if txt != exp:
                    fail("c_numbers", f"{fld}: {txt!r} != {exp!r}")
                n_checked += 1
                continue
            exp = _resolve(fld, cid, comps, sd, derived)
            if not _known(exp):
                fail(
                    "e_not_found",
                    f"{cid or ''}:{fld} printed as {txt!r} but spec value is {exp!r}",
                )
                continue
            last = fld.split(".")[-2] if fld.endswith(".value") else fld.split(".")[-1]
            v = float(exp) * (
                M2FT if sp.get("data-unit") == "ft" and last.endswith("_m") else 1.0
            )
            d = int(sp.get("data-decimals", "0"))
            got = NUM_RE.search(txt)
            if got is None or abs(_num(got.group(0)) - v) > 0.5 * 10 ** (-d) + 1e-9:
                fail(
                    "c_numbers",
                    f"{cid or ''}:{fld} printed {txt!r}, spec {v:.6g} at {d} dp",
                )
            n_checked += 1
    note("c_numbers", f"{n_checked} printed numbers verified")

    for cid, c in comps.items():
        for f in (
            "od_in",
            "envelope_width_in",
            "buoyancy_od_in",
            "wall_thickness_in",
            "joint_length_m",
            "top_el_m",
            "bottom_el_m",
            "count",
            "buoyancy_depth_rating_ft",
        ):
            if getattr(c, f) == NOT_FOUND:
                shown = [
                    sp
                    for t in root.iter(NS + "text")
                    if t.get("data-component-id") == cid
                    for sp in t.iter(NS + "tspan")
                    if sp.get("data-field") == f
                ]
                if any("na" not in (sp.get("class") or "") for sp in shown):
                    fail("e_not_found", f"{cid}:{f} NOT_FOUND drawn as a value")
                note(
                    "e_not_found",
                    f"{cid}:{f} NOT_FOUND - "
                    + (
                        "rendered as grey n/a"
                        if shown
                        else "not printed (field not shown for this type)"
                    ),
                )
        g = comp_groups.get(cid, [None])[0]
        if g is not None and c.od_in == NOT_FOUND and g.get("data-od-in") != NOT_FOUND:
            fail(
                "e_not_found",
                f"{cid}: data-od-in attribute shows {g.get('data-od-in')!r} for NOT_FOUND",
            )
        if c.od_in == NOT_FOUND and c.type in (
            CT.UPPER_FLEX_JOINT,
            CT.LOWER_FLEX_JOINT,
        ):
            note(
                "e_not_found",
                f"{cid}: OD n/a - body drawn at the nominal riser width as an unscaled symbol",
            )
        if c.envelope_width_in == NOT_FOUND:
            note(
                "e_not_found",
                f"{cid}: width n/a - frame drawn at the drag diameter, callout shows n/a",
            )

    # (d) totals ---------------------------------------------------------------------------
    d_ = spec.datums
    rt = spec.reference_totals
    stacked = [c for c in spec.components if c.type not in NESTED_TYPES]
    for a, b in zip(stacked, stacked[1:]):
        if (
            _known(a.bottom_el_m)
            and _known(b.top_el_m)
            and abs(float(a.bottom_el_m) - float(b.top_el_m)) > 1e-6
        ):
            fail(
                "d_totals",
                f"elevation gap/overlap between {a.id} and {b.id}: "
                f"{float(a.bottom_el_m) - float(b.top_el_m):+.4f} m",
            )
    mud = float(d_.mudline_el_m)
    above = [
        c
        for c in stacked
        if _known(c.bottom_el_m) and float(c.bottom_el_m) >= mud - 1e-6
    ]
    sum_el = sum(float(c.top_el_m) - float(c.bottom_el_m) for c in above)
    sum_len = sum(
        int(c.count) * float(c.joint_length_m)
        for c in above
        if _known(c.joint_length_m) and _known(c.count)
    )
    res["d_totals"]["values"] = vals = {
        "sum_component_lengths_m": round(sum_len, 4),
        "sum_elevation_spans_m": round(sum_el, 4),
    }
    if abs(sum_len - sum_el) > 1e-3:
        fail(
            "d_totals",
            f"sum(count x joint length) {sum_len:.4f} m != sum(elevation spans) {sum_el:.4f} m",
        )
    ref = rt.get("stackup_length_m")
    if ref is not None and _known(ref.value):
        vals["stackup_length_ref_m"] = ref.value
        if abs(sum_len - float(ref.value)) > 1e-3:
            fail(
                "d_totals",
                f"sum of lengths {sum_len:.4f} m != stack-up length {ref.value} ({ref.source})",
            )
    reff = rt.get("stackup_length_ft")
    if (
        reff is not None
        and _known(reff.value)
        and abs(sum_len * M2FT - float(reff.value)) > 0.01
    ):
        fail(
            "d_totals",
            f"sum of lengths {sum_len * M2FT:.3f} ft != {reff.value} ft ({reff.source})",
        )
    ufj = next((c for c in stacked if c.type == CT.UPPER_FLEX_JOINT), None)
    lfj = next((c for c in stacked if c.type == CT.LOWER_FLEX_JOINT), None)
    rl = rt.get("riser_length_ufj_lfj_m")
    if ufj and lfj and rl is not None and _known(rl.value):
        L = float(ufj.top_el_m) - float(lfj.top_el_m)
        vals["riser_length_ufj_top_to_lfj_top_m"] = round(L, 4)
        if abs(L - float(rl.value)) > 1e-3:
            fail("d_totals", f"UFJ-LFJ length {L:.4f} m != {rl.value} ({rl.source})")
    if _known(d_.water_depth_m) and _known(d_.air_gap_m):
        wd_ag = float(d_.water_depth_m) + float(d_.air_gap_m)
        resid = sum_len - wd_ag
        vals.update(
            {
                "water_depth_plus_air_gap_m": round(wd_ag, 4),
                "closure_residual_m": round(resid, 4),
            }
        )
        flag = rt.get("closure_residual_m")
        if flag is not None and _known(flag.value):
            vals["closure_residual_source_m"] = flag.value
            if abs(resid - float(flag.value)) > 1e-3:
                fail(
                    "d_totals",
                    f"closure residual {resid:.4f} m != source-flagged {flag.value} ({flag.source})",
                )
            elif abs(resid) > 0.01:
                note(
                    "d_totals",
                    f"stack-up does NOT close to the drill floor: residual {resid:+.3f} m, identical "
                    f"to the source's own flag ({flag.source}); recorded as a data gap",
                )
        elif abs(resid) > 0.01:
            fail(
                "d_totals",
                f"stack-up does not close: residual {resid:+.4f} m and no source flag",
            )
    top = stacked[0]
    rk = rt.get("rkb_node_above_mudline_m")
    if (
        rk is not None
        and _known(rk.value)
        and abs(float(top.top_el_m) - (float(rk.value) + mud)) > 1e-3
    ):
        fail(
            "d_totals", f"top of stack {top.top_el_m} != RKB node {rk.value} + mudline"
        )
    return _finish(spec, res)


def _resolve(fld: str, cid, comps, sd, derived):
    """Spec value behind a printed ``data-field``; absent paths are NOT_FOUND."""
    if fld.startswith("derived."):
        return derived.get(fld, NOT_FOUND)
    if cid is not None and "." not in fld:
        return getattr(comps[cid], fld)
    try:
        return _path(sd, fld)
    except KeyError:
        return NOT_FOUND


def _finish(spec: StackupDrawingSpec, res: dict) -> dict[str, Any]:
    ok = all(v["status"] == "pass" for v in res.values())
    return {
        "spec_id": spec.spec_id,
        "result": "pass" if ok else "fail",
        "checks": res,
        "not_found": not_found_fields(spec),
        "data_conflicts": spec.data_conflicts,
        "gaps": spec.gaps,
    }


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("spec")
    ap.add_argument("svg")
    ap.add_argument("--out")
    a = ap.parse_args(argv)
    spec = StackupDrawingSpec.from_json(Path(a.spec).read_text(encoding="utf-8"))
    rep = reconcile(spec, Path(a.svg).read_text(encoding="utf-8"))
    text = json.dumps(rep, indent=2, ensure_ascii=False) + "\n"
    if a.out:
        Path(a.out).write_text(text, encoding="utf-8")
    for k, v in rep["checks"].items():
        print(f"{k:14s} {v['status'].upper():4s}  " + "; ".join(v["failures"][:5]))
    print("RESULT", rep["result"].upper())
    return 0 if rep["result"] == "pass" else 1


if __name__ == "__main__":
    sys.exit(main())
