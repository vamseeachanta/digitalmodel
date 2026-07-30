#!/usr/bin/env python
"""Render the compact wall-thickness tension-moment 3D explorer.

Consumes the tidy study produced by ``build_wall_thickness_3d.py`` and writes
``docs/api/structural/wall-thickness-3d-explorer.html``.  The page embeds a
regularly down-sampled grid, not the full study.
"""
from __future__ import annotations

import json
import math
from pathlib import Path


_REPO = Path(__file__).resolve().parents[2]
_OUT_DIR = _REPO / "docs" / "api" / "structural"
_SOURCE = _OUT_DIR / "wall-thickness-3d.json"
_OUTPUT = _OUT_DIR / "wall-thickness-3d-explorer.html"
_WALL_STRIDE = 2
_LOAD_STRIDE = 3
_UTILISATION_SCALE = 10_000
_MAX_PAYLOAD_BYTES = 400_000


def _sample(values: list[float], stride: int) -> list[float]:
    if not values:
        raise ValueError("study grid axes must not be empty")
    sampled = list(values[::stride])
    if sampled[-1] != values[-1]:
        sampled.append(values[-1])
    return sampled


def _axes(study: dict) -> tuple[list[str], list[float], list[float], list[float]]:
    try:
        grid = study["meta"]["grid"]
        codes = list(grid["design_codes"])
        walls = list(grid["wall_thickness_mm"]["values"])
        tensions = list(grid["effective_tension_n"]["values"])
        moments = list(grid["bending_moment_nm"]["values"])
    except (KeyError, TypeError) as error:
        raise ValueError("input does not match wall-thickness-3d schema") from error
    if not codes or len(codes) != len(set(codes)):
        raise ValueError("design-code axis must contain unique values")
    return codes, walls, tensions, moments


def _spec(meta: dict) -> dict:
    geometry = meta["geometry"]
    material = meta["material"]
    pressures = meta["pressures"]
    return {
        "od": round(float(geometry["outer_diameter_m"]) * 1000, 1),
        "grade": material["grade"],
        "smys": round(float(material["smys_pa"]) / 1e6, 1),
        "smts": round(float(material["smts_pa"]) / 1e6, 1),
        "pi": round(float(pressures["internal_pa"]) / 1e6, 1),
        "pe": round(float(pressures["external_pa"]) / 1e6, 1),
        "sc": meta["safety_class"],
        "ft": float(geometry["fabrication_tolerance_fraction"]),
    }


def _empty_series(
    codes: list[str],
    wall_count: int,
    plane_size: int,
) -> dict[str, list[list[list[int | str | None]]]]:
    return {
        code: [[[None] * plane_size, [None] * plane_size] for _ in range(wall_count)]
        for code in codes
    }


def _pack_rows(
    study: dict,
    codes: list[str],
    walls: list[float],
    tensions: list[float],
    moments: list[float],
) -> tuple[dict, list[str]]:
    wi = {value: index for index, value in enumerate(walls)}
    ti = {value: index for index, value in enumerate(tensions)}
    bi = {value: index for index, value in enumerate(moments)}
    series = _empty_series(codes, len(walls), len(tensions) * len(moments))
    code_set = set(codes)
    governing = set()
    for row in study.get("rows", []):
        indexes = (
            wi.get(row.get("wall_thickness_mm")),
            ti.get(row.get("effective_tension_n")),
            bi.get(row.get("bending_moment_nm")),
        )
        if row.get("code") not in code_set or any(index is None for index in indexes):
            continue
        wall_index, tension_index, moment_index = indexes
        plane_index = tension_index * len(moments) + moment_index
        values, checks = series[row["code"]][wall_index]
        if values[plane_index] is not None:
            raise ValueError("study contains duplicate grid rows")
        value = float(row["utilisation"])
        if not math.isfinite(value) or not 0 <= value < 1000:
            raise ValueError("utilisation must be finite and between 0 and 1000")
        values[plane_index] = round(value * _UTILISATION_SCALE)
        checks[plane_index] = str(row["governing_check"])
        governing.add(checks[plane_index])
    categories = sorted(governing)
    category_index = {name: index for index, name in enumerate(categories)}
    for code_slices in series.values():
        for values, checks in code_slices:
            if any(value is None for value in values + checks):
                raise ValueError("down-sampled study grid is incomplete")
            checks[:] = [category_index[name] for name in checks]
    return series, categories


def _payload(study: dict) -> dict:
    codes, all_walls, all_tensions, all_moments = _axes(study)
    walls = _sample(all_walls, _WALL_STRIDE)
    tensions = _sample(all_tensions, _LOAD_STRIDE)
    moments = _sample(all_moments, _LOAD_STRIDE)
    series, categories = _pack_rows(study, codes, walls, tensions, moments)
    point_count = len(codes) * len(walls) * len(tensions) * len(moments)
    return {
        "m": {
            "c": codes,
            "w": walls,
            "t": [round(value) for value in tensions],
            "b": [round(value) for value in moments],
            "q": _UTILISATION_SCALE,
            "x": [_WALL_STRIDE, _LOAD_STRIDE, _LOAD_STRIDE],
            "o": [len(all_walls), len(all_tensions), len(all_moments)],
            "p": point_count,
            "r": study["meta"]["grid"].get("row_count", len(study.get("rows", []))),
            "z": 0,
            "s": _spec(study["meta"]),
        },
        "g": categories,
        "s": series,
    }


def _encode_payload(payload: dict) -> tuple[str, int]:
    for _attempt in range(6):
        raw = json.dumps(
            payload,
            separators=(",", ":"),
            ensure_ascii=True,
            allow_nan=False,
        )
        raw = raw.replace("</", r"<\/")
        byte_count = len(raw.encode("utf-8"))
        if payload["m"]["z"] == byte_count:
            if byte_count >= _MAX_PAYLOAD_BYTES:
                raise ValueError(
                    f"embedded JSON is {byte_count:,} bytes; limit is "
                    f"{_MAX_PAYLOAD_BYTES:,}"
                )
            return raw, byte_count
        payload["m"]["z"] = byte_count
    raise RuntimeError("embedded payload size did not converge")


def build_page(study: dict) -> str:
    """Return the complete explorer HTML for a tidy 3D study."""
    payload, _byte_count = _encode_payload(_payload(study))
    return _HTML.replace("__PAYLOAD__", payload)


_HTML = r"""<!DOCTYPE html>
<html lang="en" data-theme="light"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Wall-thickness 3D load explorer — digitalmodel</title>
<link rel="stylesheet" href="../_assets/brand.css">
<script defer src="https://cdn.plot.ly/plotly-3.6.0.min.js"
 onload="window.dispatchEvent(new Event('plotly-ready'))"></script>
<style>
*{box-sizing:border-box}html,body{max-width:100%;overflow-x:hidden}
body{padding:26px 20px 60px;max-width:1080px;margin:0 auto}
h1{font-size:23px;color:var(--navy);letter-spacing:-.3px;margin:0}
h2{font-size:15px;color:var(--navy);margin:0 0 8px}
.sub{color:var(--muted);font-size:14px;margin:6px 0 18px;max-width:820px}
.spec,.legend,.summary{display:flex;flex-wrap:wrap;gap:8px}
.spec{margin-bottom:20px}.spec span,.summary span{font-size:12px;font-weight:600;
color:var(--muted);background:var(--panel);border:1px solid var(--line);
border-radius:20px;padding:4px 11px}.spec b,.summary b{color:var(--navy)}
.panel{background:var(--panel);border:1px solid var(--line);border-radius:14px;
padding:18px 20px;margin-bottom:18px;box-shadow:0 1px 2px rgba(16,40,80,.04);
min-width:0}.controls{display:grid;grid-template-columns:auto minmax(170px,240px) auto
minmax(220px,1fr) auto;align-items:center;gap:10px 14px;margin-bottom:12px}
.controls label{font-weight:700;color:var(--navy);font-size:13.5px}
select{font:inherit;font-size:13px;padding:6px 9px;border:1px solid var(--line);
border-radius:8px;background:var(--panel);color:var(--ink);width:100%;min-width:0}
input[type=range]{width:100%;min-width:0;accent-color:var(--teal)}
.wval{font-family:ui-monospace,Menlo,monospace;font-weight:700;color:var(--teal);
font-size:15px;white-space:nowrap}.plot-shell{position:relative;width:100%;height:540px;
min-width:0;overflow:hidden;border:1px solid var(--line);border-radius:10px;
background:var(--soft)}#plot,#fallback{position:absolute;inset:0;width:100%;height:100%}
#plot{display:none}#fallback{display:block}.mode{font-size:12px;color:var(--muted);
margin:8px 0}.summary{margin:10px 0 2px}.legend{margin-top:10px;font-size:12px;
color:var(--muted)}.legend-item{display:inline-flex;align-items:center;gap:5px}
.swatch{width:12px;height:12px;border-radius:3px;border:1px solid rgba(19,35,63,.25)}
.boundary{width:18px;height:0;border-top:3px double var(--ink)}
.axis-note{font-size:11.5px;color:var(--muted);text-align:center;margin-top:7px}
.info{font-size:12.5px;color:var(--muted)}.info ul{padding-left:20px;margin:6px 0}
.info li{margin:5px 0}.info a,.foot a{color:var(--teal)}
.foot{color:var(--muted);font-size:12.5px;margin-top:8px;overflow-wrap:anywhere}
code{overflow-wrap:anywhere}
@media(max-width:640px){body{padding:18px 12px 44px}.panel{padding:14px 12px}
.controls{grid-template-columns:1fr;gap:5px}.controls label:not(:first-child){margin-top:5px}
.plot-shell{height:390px}.wval{justify-self:start}.axis-note{text-align:left}}
</style></head><body>
<h1>Wall-thickness &mdash; 3D tension&ndash;moment explorer</h1>
<p class="sub">Explore how effective tension and bending moment change governing utilisation
for one design code and wall thickness. Surface height is utilisation; categorical colour
shows the governing check. The double line marks utilisation&nbsp;=&nbsp;1.0.</p>
<div class="spec" id="spec"></div>
<section class="panel" aria-labelledby="viewer-heading">
<h2 id="viewer-heading">Load interaction surface</h2>
<div class="controls">
 <label for="code">Design code</label><select id="code"></select>
 <label for="wall">Wall thickness</label>
 <input id="wall" type="range" min="0" max="0" step="1">
 <output class="wval" id="wall-value" for="wall"></output>
</div>
<p class="mode" id="mode" role="status" aria-live="polite">2D fallback ready.</p>
<div class="plot-shell">
 <canvas id="fallback" role="img" aria-label="Governing-check heatmap with utilisation acceptance boundary"></canvas>
 <div id="plot" role="img" aria-label="Interactive wall-thickness load interaction plot"></div>
</div>
<p class="axis-note">Plane: effective tension (MN) &times; bending moment (kN&middot;m).
Height in 3D: utilisation. Drag to orbit; scroll or pinch to zoom.</p>
<div class="summary" id="summary"></div><div class="legend" id="legend"></div>
<noscript><p class="mode">JavaScript is disabled. Use the linked full dataset below.</p></noscript>
</section>
<section class="panel info" aria-labelledby="provenance-heading">
<h2 id="provenance-heading">Data, provenance &amp; limits</h2>
<ul>
 <li id="sampling"></li>
 <li>The embedded view retains utilisation and governing check only. Per-check values remain
 in the <a href="wall-thickness-3d.json">full tidy dataset</a>.</li>
 <li>The coarser display grid is for interaction and screening; use the full 41 &times; 31
 &times; 31 grid and governing standard for engineering decisions.</li>
</ul>
</section>
<p class="foot">Built by <code>scripts/capabilities/build_wall_thickness_3d_page.py</code> from
<code>docs/api/structural/wall-thickness-3d.json</code>. Regenerate to refresh.
&mdash; <a href="https://github.com/vamseeachanta/digitalmodel/tree/main/src/digitalmodel/structural/analysis/wall_thickness_codes">engine source &rarr;</a></p>
<script id="study-data" type="application/json">__PAYLOAD__</script>
<script>
"use strict";
const DATA=JSON.parse(document.getElementById("study-data").textContent),M=DATA.m;
const COLORS=["#0072b2","#d55e00","#009e73","#cc79a7","#e69f00","#56b4e9","#6f4e9c","#8c564b","#666666"];
const STYLES=getComputedStyle(document.documentElement);
const INK=STYLES.getPropertyValue("--ink").trim(),SOFT=STYLES.getPropertyValue("--soft").trim();
const code=document.getElementById("code"),wall=document.getElementById("wall");
const canvas=document.getElementById("fallback"),plot=document.getElementById("plot");
let plotMode="canvas",allow3d,pendingPlot,plotting=false,renderToken=0;
const label=s=>s.replace(/_/g," ").replace(/\b\w/g,c=>c.toUpperCase());
const esc=s=>String(s).replace(/[&<>"']/g,c=>({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[c]));
const slice=()=>DATA.s[code.value][+wall.value];
const matrices=()=>{const [u,g]=slice(),nu=M.t.length,nb=M.b.length,um=[],gm=[];
 for(let j=0;j<nb;j++){um[j]=[];gm[j]=[];for(let i=0;i<nu;i++){
  const k=i*nb+j;um[j][i]=u[k]/M.q;gm[j][i]=g[k];}}return [um,gm];};
function webgl(){try{const c=document.createElement("canvas");return !!(window.WebGLRenderingContext&&
 (c.getContext("webgl2")||c.getContext("webgl")));}catch(_error){return false;}}
function majority(values){const count={};values.forEach(v=>count[v]=(count[v]||0)+1);
 return +Object.keys(count).sort((a,b)=>count[b]-count[a])[0];}
function cellCategories(gm){return gm.slice(0,-1).map((row,j)=>row.slice(0,-1).map((_,i)=>
 majority([gm[j][i],gm[j][i+1],gm[j+1][i],gm[j+1][i+1]])));}
function segments(um){const out=[],nu=M.t.length,nb=M.b.length,level=1;
 const point=(a,b)=>{const f=(level-a.u)/(b.u-a.u);return{x:a.x+f*(b.x-a.x),y:a.y+f*(b.y-a.y)};};
 for(let i=0;i<nu-1;i++)for(let j=0;j<nb-1;j++){const id=(x,y)=>({x,y,u:um[y][x]});
  for(const tri of [[id(i,j),id(i+1,j),id(i+1,j+1)],[id(i,j),id(i+1,j+1),id(i,j+1)]]){
   const hits=[];for(const [a,b] of [[tri[0],tri[1]],[tri[1],tri[2]],[tri[2],tri[0]]]){
    if(a.u===level&&b.u===level)out.push([a,b]);else if(a.u===level)hits.push(a);
    else if(b.u===level)hits.push(b);else if((a.u-level)*(b.u-level)<0)hits.push(point(a,b));}
   const unique=hits.filter((p,k)=>hits.findIndex(q=>q.x===p.x&&q.y===p.y)===k);
   if(unique.length>=2)out.push([unique[0],unique[1]]);}}return out;}
function updateText(um,gm){const flat=um.flat(),active=new Set(gm.flat());
 const safe=100*flat.filter(v=>v<1).length/flat.length;
 document.getElementById("wall-value").textContent=M.w[+wall.value].toFixed(1)+" mm";
 document.getElementById("summary").innerHTML=`<span><b>Range</b> ${Math.min(...flat).toFixed(3)}–${Math.max(...flat).toFixed(3)}</span>
 <span><b>Safe grid points</b> ${safe.toFixed(0)}%</span>`;
 document.getElementById("legend").innerHTML=DATA.g.map((g,i)=>`<span class="legend-item" style="opacity:${active.has(i)?1:.38}">
 <i class="swatch" style="background:${COLORS[i%COLORS.length]}"></i>${esc(label(g))}</span>`).join("")+
 '<span class="legend-item"><i class="boundary"></i>Utilisation = 1.0</span>';
 canvas.setAttribute("aria-label",`${code.value}, ${M.w[+wall.value].toFixed(1)} mm: governing-check heatmap and utilisation 1.0 boundary`);}
function nativePlot(um,gm){const box=canvas.parentElement.getBoundingClientRect(),dpr=Math.min(devicePixelRatio||1,2);
 canvas.width=Math.max(1,Math.round(box.width*dpr));canvas.height=Math.max(1,Math.round(box.height*dpr));
 const c=canvas.getContext("2d"),w=canvas.width,h=canvas.height,L=58*dpr,R=12*dpr,T=18*dpr,B=50*dpr;
 c.fillStyle=SOFT;c.fillRect(0,0,w,h);const pw=w-L-R,ph=h-T-B,nu=M.t.length,nb=M.b.length,cells=cellCategories(gm);
 for(let i=0;i<nu-1;i++)for(let j=0;j<nb-1;j++){c.fillStyle=COLORS[cells[j][i]%COLORS.length];
  c.fillRect(L+i*pw/(nu-1),T+(nb-2-j)*ph/(nb-1),pw/(nu-1)+1,ph/(nb-1)+1);}
 const segs=segments(um);for(const [colour,width] of [[INK,5],["#fff",2]]){c.strokeStyle=colour;
  c.lineWidth=width*dpr;c.beginPath();for(const [a,b] of segs){c.moveTo(L+a.x*pw/(nu-1),T+(nb-1-a.y)*ph/(nb-1));
   c.lineTo(L+b.x*pw/(nu-1),T+(nb-1-b.y)*ph/(nb-1));}c.stroke();}
 c.fillStyle=INK;c.font=`${11*dpr}px sans-serif`;c.textAlign="center";
 c.fillText("Effective tension (MN)",L+pw/2,h-10*dpr);c.save();c.translate(14*dpr,T+ph/2);
 c.rotate(-Math.PI/2);c.fillText("Bending moment (kN·m)",0,0);c.restore();
 c.textAlign="left";c.fillText("0",L,h-31*dpr);c.textAlign="right";
 c.fillText((M.t[M.t.length-1]/1e6).toFixed(2),L+pw,h-31*dpr);}
function discreteScale(){const n=DATA.g.length,scale=[];for(let i=0;i<n;i++){
 scale.push([i/n,COLORS[i%COLORS.length]],[(i+1)/n-1e-7,COLORS[i%COLORS.length]]);}return scale;}
function traces2d(um,gm){const cells=cellCategories(gm),text=cells.map(row=>row.map(i=>label(DATA.g[i])));
 const x=M.t.slice(0,-1).map((v,i)=>(v+M.t[i+1])/2e6),y=M.b.slice(0,-1).map((v,i)=>(v+M.b[i+1])/2e3);
 const util=cells.map((row,j)=>row.map((_,i)=>(um[j][i]+um[j][i+1]+um[j+1][i]+um[j+1][i+1])/4));
 return[{type:"heatmap",x,y,z:cells.map(r=>r.map(v=>v+.5)),customdata:util,text,
  zmin:0,zmax:DATA.g.length,colorscale:discreteScale(),showscale:false,zsmooth:false,
  hovertemplate:"T %{x:.2f} MN<br>M %{y:.1f} kN·m<br>u %{customdata:.3f}<br>%{text}<extra></extra>"},
 {type:"contour",x:M.t.map(v=>v/1e6),y:M.b.map(v=>v/1e3),z:um,showscale:false,
  contours:{start:1,end:1,size:1,coloring:"none"},line:{color:INK,width:7},hoverinfo:"skip"},
 {type:"contour",x:M.t.map(v=>v/1e6),y:M.b.map(v=>v/1e3),z:um,showscale:false,
  contours:{start:1,end:1,size:1,coloring:"none"},line:{color:"#fff",width:3},hoverinfo:"skip"}];}
function traces3d(um,gm){const x=[],y=[],z=[],nu=M.t.length,nb=M.b.length;
 for(let i=0;i<nu;i++)for(let j=0;j<nb;j++){x.push(M.t[i]/1e6);y.push(M.b[j]/1e3);z.push(um[j][i]);}
 const groups=DATA.g.map(()=>({i:[],j:[],k:[]})),cells=cellCategories(gm);
 for(let i=0;i<nu-1;i++)for(let j=0;j<nb-1;j++){const a=i*nb+j,b=(i+1)*nb+j,c=b+1,d=a+1;
  for(const tri of [[a,b,c],[a,c,d]]){const group=groups[cells[j][i]];group.i.push(tri[0]);group.j.push(tri[1]);group.k.push(tri[2]);}}
 const traces=groups.map((g,i)=>[g,i]).filter(([g])=>g.i.length).map(([g,i])=>({type:"mesh3d",x,y,z,i:g.i,j:g.j,k:g.k,name:label(DATA.g[i]),
  color:COLORS[i%COLORS.length],flatshading:true,opacity:.96,showlegend:false,
  hovertemplate:"T %{x:.2f} MN<br>M %{y:.1f} kN·m<br>u %{z:.3f}<br>"+label(DATA.g[i])+"<extra></extra>"}));
 const line={x:[],y:[],z:[]};for(const [a,b] of segments(um)){for(const p of [a,b]){
  line.x.push((M.t[0]+p.x*(M.t[M.t.length-1]-M.t[0])/(nu-1))/1e6);
  line.y.push((M.b[0]+p.y*(M.b[M.b.length-1]-M.b[0])/(nb-1))/1e3);line.z.push(1);}line.x.push(null);line.y.push(null);line.z.push(null);}
 const planeX=[M.t[0]/1e6,M.t[M.t.length-1]/1e6,M.t[M.t.length-1]/1e6,M.t[0]/1e6];
 const planeY=[M.b[0]/1e3,M.b[0]/1e3,M.b[M.b.length-1]/1e3,M.b[M.b.length-1]/1e3];
 traces.push({type:"mesh3d",x:planeX,y:planeY,z:[1,1,1,1],i:[0,0],j:[1,2],k:[2,3],
  color:INK,opacity:.08,showlegend:false,hoverinfo:"skip"});
 for(const [colour,width] of [[INK,10],["#fff",5]])traces.push(
  {type:"scatter3d",mode:"lines",...line,line:{color:colour,width},showlegend:false,hoverinfo:"skip"});
 return traces;}
function layout(mode){const common={margin:{l:55,r:15,t:12,b:52},paper_bgcolor:"rgba(0,0,0,0)",
 plot_bgcolor:SOFT,font:{family:"-apple-system,Segoe UI,Roboto,Arial,sans-serif",color:INK},uirevision:"viewer"};
 if(mode==="2d")return{...common,xaxis:{title:{text:"Effective tension (MN)"},automargin:true},
  yaxis:{title:{text:"Bending moment (kN·m)"},automargin:true},showlegend:false};
 return{...common,scene:{xaxis:{title:{text:"Effective tension (MN)"}},yaxis:{title:{text:"Bending moment (kN·m)"}},
  zaxis:{title:{text:"Utilisation"},rangemode:"tozero"},camera:{eye:{x:1.45,y:1.45,z:1.1}},aspectmode:"manual",
  aspectratio:{x:1.35,y:1,z:.85}},showlegend:false};}
async function plotlyPlot(job){const {id,mode,um,gm}=job;
 try{const traces=mode==="3d"?traces3d(um,gm):traces2d(um,gm),fn=plot._fullLayout?"react":"newPlot";
  await Plotly[fn](plot,traces,layout(mode),{responsive:true,displaylogo:false,scrollZoom:true});
  if(id!==renderToken)return;plot.style.visibility="visible";canvas.style.display="none";plotMode=mode;
  if(mode==="3d")plot.querySelectorAll("canvas").forEach(item=>{if(!item.dataset.wtFallback){
   item.dataset.wtFallback="1";item.addEventListener("webglcontextlost",event=>{
    event.preventDefault();allow3d=false;plot.style.visibility="hidden";canvas.style.display="block";render();},{once:true});}});
  document.getElementById("mode").textContent=mode==="3d"?"3D WebGL surface.":"2D fallback — WebGL is unavailable.";
 }catch(error){if(id!==renderToken)return;if(mode==="3d"){allow3d=false;await plotlyPlot({...job,mode:"2d"});}else{
  plot.style.visibility="hidden";canvas.style.display="block";plotMode="canvas";
  document.getElementById("mode").textContent="2D canvas fallback — interactive library unavailable.";}}}
function queuePlot(mode,um,gm){pendingPlot={id:++renderToken,mode,um,gm};if(!plotting)drainPlots();}
async function drainPlots(){plotting=true;while(pendingPlot){const job=pendingPlot;pendingPlot=null;
 await plotlyPlot(job);}plotting=false;}
function render(){const [um,gm]=matrices();nativePlot(um,gm);updateText(um,gm);
 plotMode="canvas";if(window.Plotly){plot.style.display="block";plot.style.visibility="hidden";canvas.style.display="block";
  queuePlot(allow3d?"3d":"2d",um,gm);}}
M.c.forEach(c=>code.add(new Option(c,c)));code.value=M.c.includes("DNV-ST-F101")?"DNV-ST-F101":M.c[0];
allow3d=webgl();wall.max=M.w.length-1;let initial=M.w.indexOf(20);
wall.value=initial<0?Math.floor(M.w.length/2):initial;
const S=M.s;document.getElementById("spec").innerHTML=[["OD",S.od+" mm"],["Grade",S.grade],
 ["SMYS",S.smys+" MPa"],["pᵢ / pₑ",S.pi+" / "+S.pe+" MPa"],["Safety class",S.sc]]
 .map(([k,v])=>`<span><b>${esc(k)}</b> ${esc(v)}</span>`).join("");
document.getElementById("sampling").innerHTML=`Embedded display: every ${M.x[0]}nd wall value and every
 ${M.x[1]}rd load value (${M.w.length} × ${M.t.length} × ${M.b.length} × ${M.c.length} =
 ${M.p.toLocaleString()} points; ${(M.z/1000).toFixed(1)} kB JSON, ${(100*M.p/M.r).toFixed(1)}% of source rows).`;
[code,wall].forEach(control=>control.addEventListener("input",render));
window.addEventListener("resize",()=>{if(plotMode==="canvas")render();});
window.addEventListener("plotly-ready",render);render();
</script></body></html>
"""


def main() -> None:
    if not _SOURCE.exists():
        raise FileNotFoundError(
            f"{_SOURCE.relative_to(_REPO)} is missing; run "
            "scripts/capabilities/build_wall_thickness_3d.py first"
        )
    study = json.loads(_SOURCE.read_text(encoding="utf-8"))
    html = build_page(study)
    _OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    _OUTPUT.write_text(html, encoding="utf-8")
    raw_payload = html.split(
        '<script id="study-data" type="application/json">',
        1,
    )[1].split("</script>", 1)[0]
    payload_meta = json.loads(raw_payload)["m"]
    print(
        f"wrote {_OUTPUT.relative_to(_REPO)} "
        f"({payload_meta['p']:,} points; "
        f"{payload_meta['z']:,} embedded JSON bytes)"
    )


if __name__ == "__main__":
    main()
