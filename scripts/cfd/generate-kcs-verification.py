#!/usr/bin/env python
"""Build the #1173 KCS verification artifact (JSON manifest + HTML report).

Consumes a solved case's forces output and emits the committed evidence:

    docs/api/cfd/kcs-calm-water-resistance-verification.json
    docs/api/cfd/kcs-calm-water-resistance-verification.html

Host paths are REDACTED. The verification artifact is a published document and
the execution host's directory layout is not part of the result; an absolute
path in committed evidence is also a CI failure under the repository's
no-absolute-paths gate.

Usage:
    generate-kcs-verification.py --production <case> [--companion <case>]
"""

from __future__ import annotations

import argparse
import html
import json
import re
import sys
from datetime import datetime, timezone
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src"))

from digitalmodel.solvers.openfoam.validation.ship_resistance import (  # noqa: E402
    ShipResistanceConfig,
    evaluate_ship_resistance_run,
)

OUT_DIR = REPO_ROOT / "docs" / "api" / "cfd"


def redact(text: str) -> str:
    """Strip anything that looks like a host path.

    Applied to every string that reaches the artifact, not only to the ones
    expected to contain a path, because the point of a redaction pass is to
    catch the string nobody thought to check.
    """
    text = re.sub(r"/home/[^\s\"']*", "<redacted>", text)
    text = re.sub(r"/mnt/[^\s\"']*", "<redacted>", text)
    text = re.sub(r"/tmp/[^\s\"']*", "<redacted>", text)
    return text


def find_force_dat(case_dir: Path) -> Path:
    candidates = sorted(case_dir.glob("postProcessing/forces/*/force.dat"))
    if not candidates:
        raise FileNotFoundError(f"no forces output under {case_dir.name}")
    return candidates[-1]


def cell_count(case_dir: Path) -> int | None:
    log = case_dir / "log.checkMesh"
    if not log.is_file():
        return None
    match = re.search(r"^\s*cells:\s*(\d+)", log.read_text(errors="replace"),
                      re.MULTILINE)
    return int(match.group(1)) if match else None


def checkmesh_verdict(case_dir: Path) -> dict:
    """The STRICT rule: read the OUTPUT TEXT, not the exit code.

    checkMesh returns 0 even when it reports failed checks, so an exit-code
    gate would certify a mesh it had just been told was bad.
    """
    log = case_dir / "log.checkMesh"
    if not log.is_file():
        return {"available": False}
    text = log.read_text(errors="replace")
    failed = re.search(r"Failed (\d+) mesh checks", text)
    return {
        "available": True,
        "mesh_ok": bool(re.search(r"^Mesh OK", text, re.MULTILINE)),
        "failed_checks": int(failed.group(1)) if failed else 0,
        "passed": bool(re.search(r"^Mesh OK", text, re.MULTILINE))
        and not failed,
    }


def build_manifest(production: Path, companion: Path | None) -> dict:
    config = ShipResistanceConfig()
    manifest = evaluate_ship_resistance_run(
        find_force_dat(production),
        config,
        companion_force_dat=find_force_dat(companion) if companion else None,
        companion_config=config if companion else None,
        mesh_cells=cell_count(production),
        companion_mesh_cells=cell_count(companion) if companion else None,
    )
    manifest["generated_utc"] = datetime.now(timezone.utc).isoformat()
    manifest["mesh_quality"] = {
        "production": checkmesh_verdict(production),
        "companion": checkmesh_verdict(companion) if companion else None,
    }
    manifest["issue"] = "#1173"
    return json.loads(redact(json.dumps(manifest)))


# --------------------------------------------------------------------------- #
#  HTML
# --------------------------------------------------------------------------- #

_CSS = """
:root { --bg:#fff; --fg:#16202c; --muted:#5b6b7d; --line:#dfe6ee;
        --pass:#0f7b4f; --fail:#b3261e; --accent:#0b4f8a; --card:#f7f9fc; }
@media (prefers-color-scheme: dark) { :root {
  --bg:#0f1620; --fg:#e6edf5; --muted:#93a4b8; --line:#243244;
  --pass:#4ade80; --fail:#ff6b6b; --accent:#7cc4ff; --card:#16202c; } }
* { box-sizing:border-box; }
body { margin:0; padding:2rem 1.25rem 4rem; background:var(--bg); color:var(--fg);
  font:16px/1.65 -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,sans-serif; }
main { max-width:60rem; margin:0 auto; }
h1 { font-size:1.75rem; margin:0 0 .25rem; letter-spacing:-.02em; }
h2 { font-size:1.15rem; margin:2.5rem 0 .75rem; padding-bottom:.35rem;
  border-bottom:1px solid var(--line); }
.sub { color:var(--muted); margin:0 0 2rem; }
table { border-collapse:collapse; width:100%; font-size:.92rem; }
th,td { text-align:left; padding:.5rem .6rem; border-bottom:1px solid var(--line); }
th { color:var(--muted); font-weight:600; }
td.num { font-variant-numeric:tabular-nums; text-align:right; }
.wrap { overflow-x:auto; }
.pass { color:var(--pass); font-weight:600; }
.fail { color:var(--fail); font-weight:600; }
.card { background:var(--card); border:1px solid var(--line); border-radius:8px;
  padding:1rem 1.1rem; margin:1rem 0; }
.note { color:var(--muted); font-size:.9rem; }
code { font-family:ui-monospace,SFMono-Regular,Menlo,monospace; font-size:.88em; }
"""


def verdict_cell(passed: bool) -> str:
    return ('<span class="pass">PASS</span>' if passed
            else '<span class="fail">FAIL</span>')


def build_html(m: dict) -> str:
    prov = m["provenance"]
    meas = m["measurement"]
    crit = m["criteria"]

    rows = []
    for name in ("V1", "V2a", "V2b"):
        c = crit[name]
        if name == "V2a":
            tol = f"{c['tolerance_low']:+.0%} / {c['tolerance_high']:+.0%}"
        else:
            tol = f"±{c['tolerance']:.1%}"
        rows.append(
            f"<tr><td><b>{name}</b></td><td>{c['quantity']}</td>"
            f"<td class='num'>{c['computed']:.5e}</td>"
            f"<td class='num'>{c['reference']:.5e}</td>"
            f"<td class='num'>{c['relative_error']:+.2%}</td>"
            f"<td class='num'>{tol}</td>"
            f"<td>{verdict_cell(c['passed'])}</td></tr>"
        )
    if "V3" in crit:
        c = crit["V3"]
        extra = " (escalated)" if c.get("escalated") else ""
        rows.append(
            f"<tr><td><b>V3</b></td><td>two-level Ct</td>"
            f"<td class='num'>{c['ct_fine']:.5e}</td>"
            f"<td class='num'>{c['ct_coarse']:.5e}</td>"
            f"<td class='num'>{c['epsilon']:.2%}</td>"
            f"<td class='num'>≤{c['threshold']:.1%}</td>"
            f"<td>{verdict_cell(c['passed'])}{extra}</td></tr>"
        )

    dev_rows = "".join(
        f"<tr><td><code>{html.escape(k)}</code></td>"
        f"<td>{html.escape(v)}</td></tr>"
        for k, v in prov["declared_deviations"].items()
    )

    mq = m.get("mesh_quality", {}).get("production") or {}
    mesh_line = (
        f"{meas.get('mesh_cells'):,} cells; checkMesh "
        f"{'PASS' if mq.get('passed') else 'see manifest'}"
        if meas.get("mesh_cells") else "n/a"
    )

    return f"""<title>KCS calm-water resistance — verification (#1173)</title>
<style>{_CSS}</style>
<main>
<h1>KCS calm-water hull resistance — verification</h1>
<p class="sub">Issue #1173 · interFoam towing · generated {m['generated_utc']}</p>

<div class="card">
<b>The referent is a tuple, not a number.</b>
Ct = {prov['reference']['ct']:.3e} ·
{html.escape(prov['body_condition'])} ·
appendages: {html.escape(prov['appendages'])} ·
S = {prov['wetted_surface_m2']} m² ·
Re = {prov['reynolds']:.2e} · Fr = {prov['froude']:.4f}.
<span class="note">A total resistance coefficient quoted without its condition
tuple cannot be gated against at any tolerance.</span>
</div>

<h2>Criteria</h2>
<div class="wrap"><table>
<tr><th>criterion</th><th>quantity</th><th>computed</th><th>reference</th>
<th>error</th><th>tolerance</th><th>verdict</th></tr>
{''.join(rows)}
</table></div>
<p class="note">Each criterion reports its own verdict. There is deliberately
no single aggregate: a compensating-error pair can pass V1 while failing V2a,
and a rolled-up result would hide which.</p>

<h2>Measurement</h2>
<div class="wrap"><table>
<tr><th>quantity</th><th>value</th></tr>
<tr><td>Ct</td><td class="num">{meas['ct']:.5e}</td></tr>
<tr><td>Cp (pressure)</td><td class="num">{meas['cp']:.5e}</td></tr>
<tr><td>Cv (viscous)</td><td class="num">{meas['cv']:.5e}</td></tr>
<tr><td>total force (full body)</td><td class="num">{meas['force_total_N']:.4f} N</td></tr>
<tr><td>averaging window</td><td class="num">{meas['averaging_window']:,} iterations
 ({meas['window_first_iteration']:,}–{meas['window_last_iteration']:,})</td></tr>
<tr><td>iterative scatter on Ct</td><td class="num">{meas['iterative_scatter_ct']:.3e}</td></tr>
<tr><td>mesh</td><td class="num">{mesh_line}</td></tr>
</table></div>
<p class="note">Ct = Cp + Cv holds identically
({'verified' if m['identity_check']['holds'] else 'VIOLATED'}). Forces are
full-body; the half-domain doubling is applied once, in the parser.</p>

<h2>Limit of the method</h2>
<div class="card"><p style="margin:0">
The decomposition gate cannot detect a compensating pair whose net effect on Ct
is below <b>{m['detection_floor']['ct_fraction']:.2%}</b>. Because Ct = Cp + Cv
holds identically, a pair sitting on both component boundaries in compensating
directions lands inside V1. This is a property of the arithmetic, not of the
mesh, and it is stated here rather than discovered afterwards.
</p></div>

<h2>Declared deviations from the DTCHull tutorial</h2>
<div class="wrap"><table>
<tr><th>item</th><th>reason</th></tr>
{dev_rows}
</table></div>
<p class="note">Asserted in both directions by the test suite: every declared
deviation must be present, and no undeclared deviation may exist.</p>

<h2>Provenance</h2>
<div class="wrap"><table>
<tr><th>field</th><th>value</th></tr>
<tr><td>hull</td><td>{html.escape(prov['hull'])}</td></tr>
<tr><td>geometry source</td><td>{html.escape(prov['geometry_source'])}</td></tr>
<tr><td>model scale</td><td>{html.escape(prov['model_scale'])}</td></tr>
<tr><td>ν (from stated Re)</td><td class="num">{prov['nu']:.6e} m²/s</td></tr>
<tr><td>U used to derive ν</td><td class="num">{prov['velocity_used_for_nu']} m/s</td></tr>
<tr><td>ρ</td><td class="num">{prov['density']} kg/m³</td></tr>
<tr><td>iterations</td><td class="num">{prov['iterations']:,}</td></tr>
<tr><td>ranks</td><td class="num">{prov['ranks']}</td></tr>
</table></div>
<p class="note">Host paths are redacted; the execution host's directory layout
is not part of the result.</p>
</main>
"""


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--production", required=True, type=Path)
    ap.add_argument("--companion", type=Path, default=None)
    ap.add_argument("--out-dir", type=Path, default=OUT_DIR)
    args = ap.parse_args()

    manifest = build_manifest(args.production, args.companion)
    args.out_dir.mkdir(parents=True, exist_ok=True)

    stem = "kcs-calm-water-resistance-verification"
    (args.out_dir / f"{stem}.json").write_text(
        json.dumps(manifest, indent=2) + "\n"
    )
    (args.out_dir / f"{stem}.html").write_text(build_html(manifest))

    print(json.dumps(manifest["summary"], indent=2))
    print(f"all_passed: {manifest['all_passed']}")
    return 0 if manifest["all_passed"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
