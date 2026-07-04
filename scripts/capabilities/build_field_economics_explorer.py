#!/usr/bin/env python
"""Build the field-development economics (NPV vs oil price) explorer.

Runs the field_development DCF engine over CAPEX levels and an oil-price sweep
for a reference deepwater field, then writes a self-contained interactive HTML
dashboard + raw JSON.

Outputs:
    docs/api/structural/field-economics-explorer.html
    docs/api/structural/field-economics-explorer.json

Run:
    python scripts/capabilities/build_field_economics_explorer.py
"""
from __future__ import annotations

import json
import warnings
from dataclasses import replace
from pathlib import Path

warnings.filterwarnings("ignore")

from digitalmodel.field_development.economics import (
    EconomicsInput,
    FiscalRegime,
    evaluate_economics,
)

_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"

_CAPEX_MM = [6000.0, 8500.0, 11000.0]                       # low / base / high
_PRICE = [round(30.0 + 5.0 * i, 0) for i in range(0, 15)]   # $30 .. $100/bbl


def _base(capex: float) -> EconomicsInput:
    return EconomicsInput(
        field_name="Reference deepwater field", water_depth_m=2100.0, host_type="Spar",
        production_capacity_bopd=100_000.0, oil_price_usd_per_bbl=75.0,
        discount_rate=0.10, fiscal_regime=FiscalRegime.US, field_life_years=30,
        reservoir_size_mmbbl=400.0, opex_usd_per_bbl=25.0,
        capex_usd_mm=capex, abex_usd_mm=500.0,
    )


def build_study() -> dict:
    series = {}
    breakeven = {}
    for capex in _CAPEX_MM:
        base = _base(capex)
        pts = []
        be = None
        for pr in _PRICE:
            r = evaluate_economics(replace(base, oil_price_usd_per_bbl=float(pr)))
            npv = float(r.metrics.npv_usd_mm)
            pb = r.metrics.payback_years
            pts.append({"p": pr, "npv": round(npv, 0),
                        "pb": (int(pb) if pb is not None else None)})
            if be is None and npv >= 0:
                be = pr
        series[str(int(capex))] = pts
        breakeven[str(int(capex))] = be
    return {
        "meta": {
            "capex": [int(c) for c in _CAPEX_MM], "price": [int(p) for p in _PRICE],
            "field": "Reference deepwater Spar · 100 kbopd · EUR 60 MMbbl · OPEX $25/bbl · 10% discount · pre-tax",
            "standard": "Discounted cash flow (Arps decline)",
        },
        "series": series, "breakeven": breakeven,
    }


_HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Field-development economics explorer — digitalmodel</title>
<style>
 :root{--navy:#0B3D91;--teal:#0f8a7e;--bg:#eef3fa;--panel:#fff;--ink:#13233f;
       --muted:#5b6b86;--line:#dbe4f0;--soft:#f4f8fc;--ok:#1f9d57;--bad:#c0392b}
 *{box-sizing:border-box;margin:0;padding:0}
 body{font-family:-apple-system,"Segoe UI",Roboto,Arial,sans-serif;background:var(--bg);
      color:var(--ink);line-height:1.5;padding:26px 20px 60px;max-width:1080px;margin:0 auto}
 h1{font-size:23px;color:var(--navy);letter-spacing:-.3px}
 .sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:800px}
 .spec{display:flex;flex-wrap:wrap;gap:8px;margin-bottom:20px}
 .spec span{font-size:12px;font-weight:600;color:var(--muted);background:#fff;
            border:1px solid var(--line);border-radius:20px;padding:4px 11px}
 .spec b{color:var(--navy)}
 .panel{background:var(--panel);border:1px solid var(--line);border-radius:14px;
        padding:18px 20px;margin-bottom:18px;box-shadow:0 1px 2px rgba(16,40,80,.04)}
 .ctrl{display:flex;align-items:center;gap:14px;flex-wrap:wrap;margin-bottom:10px}
 .ctrl label{font-weight:700;color:var(--navy);font-size:13.5px}
 select{font:inherit;font-size:13px;padding:5px 9px;border:1px solid var(--line);
        border-radius:8px;background:#fff;color:var(--ink)}
 input[type=range]{flex:1;min-width:220px;accent-color:var(--teal)}
 .wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);font-size:16px;min-width:80px}
 .reads{display:flex;gap:26px;flex-wrap:wrap;margin:4px 0 2px}
 .read{font-size:13px;color:var(--muted)}
 .read b{display:block;font-family:ui-monospace,Menlo,monospace;font-size:22px;font-weight:700}
 .cap{font-size:12px;color:var(--muted);margin:8px 0}
 .chart{margin-top:6px;position:relative;display:flex;align-items:center;gap:4px;height:220px;
        border-left:2px solid var(--line);padding-left:4px}
 .zero{position:absolute;left:0;right:0;top:50%;border-top:1px dashed var(--muted);opacity:.6}
 .col{flex:1;display:flex;flex-direction:column;justify-content:center;height:100%;position:relative}
 .bar{border-radius:3px;position:absolute;left:8%;right:8%}
 .bar.pos{bottom:50%;background:linear-gradient(0deg,var(--ok),#5ec98a)}
 .bar.neg{top:50%;background:linear-gradient(180deg,var(--bad),#e07a6e)}
 .col.on .bar{outline:2px solid var(--navy);outline-offset:1px}
 .xax{display:flex;gap:4px;padding-left:6px;margin-top:4px}
 .xax div{flex:1;text-align:center;font-size:9px;color:var(--muted)}
 .foot{color:var(--muted);font-size:12.5px;margin-top:14px}.foot a{color:var(--teal)}
</style></head><body>
<h1>Field-development economics &mdash; NPV vs oil price</h1>
<p class="sub">Pre-tax discounted-cash-flow value of a reference deepwater development against oil
price, on the field_development DCF engine (Arps decline). Pick a CAPEX level and drag the oil price
to watch NPV cross break-even. Every point is a live <code>evaluate_economics</code> run.</p>
<div class="spec" id="spec"></div>

<div class="panel">
  <div class="ctrl">
    <label>Sanctioned CAPEX</label><select id="capex"></select>
    <label for="p">Oil price</label>
    <input type="range" id="p" min="0" max="0" step="1">
    <span class="wval" id="pval"></span>
  </div>
  <div class="reads">
    <div class="read">Project NPV<b id="rN">&mdash;</b></div>
    <div class="read">Payback<b id="rP">&mdash;</b></div>
    <div class="read">Break-even oil price<b id="rB" style="color:var(--navy)">&mdash;</b></div>
  </div>
  <div class="cap">NPV ($MM) vs oil price &mdash; green above the break-even line, red below; selected price outlined.</div>
  <div class="chart" id="chart"><div class="zero"></div></div>
  <div class="xax" id="xax"></div>
</div>

<p class="foot">Built by <code>scripts/capabilities/build_field_economics_explorer.py</code> from
<code>src/digitalmodel/field_development/economics.py</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/blob/main/src/digitalmodel/field_development/economics.py">engine source &rarr;</a></p>

<script>
const DATA = {data};
const M = DATA.meta, P = M.price;
const spec = document.getElementById('spec');
spec.innerHTML = [['Field', M.field], ['Method', M.standard]]
  .map(([k,v])=>`<span><b>${k}</b> ${v}</span>`).join('');
const selC = document.getElementById('capex');
M.capex.forEach(c=>selC.add(new Option('$'+c.toLocaleString()+' MM', c)));
selC.value = 8500;
const slider = document.getElementById('p');
slider.max = P.length-1;

function absMax(){ let mx=1; for(const c in DATA.series) for(const q of DATA.series[c])
  if(Math.abs(q.npv)>mx) mx=Math.abs(q.npv); return mx; }
const MX = absMax();

function fmt(n){ return (n<0?'−':'')+'$'+Math.abs(n).toLocaleString()+' MM'; }

function render(){
  const c = selC.value, idx = +slider.value, ser = DATA.series[c], q = ser[idx];
  document.getElementById('pval').textContent = '$'+P[idx]+'/bbl';
  const nEl = document.getElementById('rN');
  nEl.textContent = fmt(q.npv); nEl.style.color = q.npv>=0 ? 'var(--ok)' : 'var(--bad)';
  document.getElementById('rP').textContent = q.pb==null ? 'never' : q.pb+' yr';
  const be = DATA.breakeven[c];
  document.getElementById('rB').textContent = be==null ? '> $'+P[P.length-1] : '$'+be+'/bbl';
  document.getElementById('chart').innerHTML = '<div class="zero"></div>' + ser.map((x,i)=>{
    const h = (Math.abs(x.npv)/MX*48).toFixed(1);
    const cls = x.npv>=0 ? 'pos' : 'neg';
    return `<div class="col ${i===idx?'on':''}"><div class="bar ${cls}" style="height:${h}%" title="$${P[i]}: ${fmt(x.npv)}"></div></div>`;
  }).join('');
  document.getElementById('xax').innerHTML = ser.map((x,i)=>`<div>${(i%2===0)?'$'+P[i]:''}</div>`).join('');
}
let start = P.indexOf(70); if(start<0) start = Math.floor(P.length/2);
slider.value = start; render();
[selC, slider].forEach(el=>el.addEventListener('input', render));
</script>
</body></html>
"""


def main() -> None:
    _OUT_DIR.mkdir(parents=True, exist_ok=True)
    study = build_study()
    (_OUT_DIR / "field-economics-explorer.json").write_text(
        json.dumps(study, indent=2) + "\n", encoding="utf-8")
    html = _HTML.replace("{data}", json.dumps(study, separators=(",", ":")))
    (_OUT_DIR / "field-economics-explorer.html").write_text(html, encoding="utf-8")
    n = sum(len(v) for v in study["series"].values())
    print(f"wrote {(_OUT_DIR / 'field-economics-explorer.html').relative_to(_REPO)} ({n} points)")


if __name__ == "__main__":
    main()
