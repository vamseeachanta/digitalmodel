"""Interactive figures of the fender design guide (standard library only).

The guide's Markdown keeps a static image for every figure. In the HTML page each image is
replaced by a figure drawn in the browser from the same data: hover gives the value with its
unit, and a legend entry shows or hides its series. The data of every figure sit in the page
as JSON (``data-spec``) and are the values the static chart plots.

Three chart forms (``line``, ``bars``, ``ranges``) and two schematics drawn here as SVG
(``summary_svg``, ``codes_svg``). One script, :data:`SCRIPT`, draws every chart on the page.
"""
from __future__ import annotations

import html
import json

SERIES = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948"]
LIMIT = "#d03b3b"
INK = "#0b0b0b"
MUTED = "#6b6a64"
NAVY = "#1f4e8c"

CSS = (
    "figure.ifig{margin:1rem 0;padding:0}"
    "figure.ifig .ic{position:relative;border:1px solid #e1e0d9;background:#fff;margin:.4rem 0}"
    "figure.ifig svg{display:block;width:100%;height:auto;background:#fff}"
    "figure.ifig svg text{font-family:Segoe UI,Helvetica,Arial,sans-serif}"
    ".ic-legend{display:flex;flex-wrap:wrap;gap:.3rem .8rem;border:1px solid #c3c2b7;padding:.4rem .6rem;"
    "margin:.2rem .6rem .6rem;font-size:.9rem;align-items:center}"
    ".ic-legend b{font-weight:600;margin-right:.3rem}"
    ".ic-legend button{border:0;background:none;font:inherit;cursor:pointer;padding:.1rem .2rem;color:#0b0b0b}"
    ".ic-legend button.off{text-decoration:line-through;color:#898781}"
    ".ic-legend i{display:inline-block;width:1.4rem;height:.7rem;margin-right:.3rem;vertical-align:baseline}"
    ".ic-tip{position:absolute;pointer-events:none;background:#0b0b0b;color:#fff;font-size:.85rem;line-height:1.3;"
    "padding:.3rem .5rem;border-radius:4px;max-width:22rem;z-index:5;white-space:pre-line}"
    ".ic-tip[hidden]{display:none}"
    "figure.ifig noscript img{max-width:100%}"
    "figure.ifig [data-tip]{cursor:help}"
)

SCRIPT = r"""<script>
(function(){
var NS="http://www.w3.org/2000/svg";
function el(n,a,t){var e=document.createElementNS(NS,n);for(var k in a)e.setAttribute(k,a[k]);if(t!=null)e.textContent=t;return e}
function nice(lo,hi,n){var span=(hi-lo)||1,s0=span/n,mag=Math.pow(10,Math.floor(Math.log10(s0))),f=s0/mag,
 st=(f<1.5?1:f<3?2:f<7?5:10)*mag,t=[];for(var v=Math.ceil(lo/st-1e-9)*st;v<=hi+st*1e-6;v+=st)t.push(+v.toPrecision(12));return t}
function fmt(v,d){if(v==null||isNaN(v))return "not rated";
 return v.toLocaleString("en-US",{minimumFractionDigits:d,maximumFractionDigits:d}).replace("-","−")}
function dec(ticks){var d=0;ticks.forEach(function(t){var s=String(+t.toPrecision(10));var i=s.indexOf(".");if(i>=0)d=Math.max(d,s.length-i-1)});return Math.min(d,4)}
function tipper(box){var tip=document.createElement("div");tip.className="ic-tip";tip.hidden=true;box.appendChild(tip);
 return {show:function(ev,text){tip.textContent=text;tip.hidden=false;var r=box.getBoundingClientRect();
  var x=ev.clientX-r.left+12,y=ev.clientY-r.top+12;if(x>r.width-200)x=Math.max(0,x-220);tip.style.left=x+"px";tip.style.top=y+"px"},
  hide:function(){tip.hidden=true}}}
function legend(box,spec,redraw){if(!spec.series||(spec.series.length<2&&!(spec.keys&&spec.keys.length)))return;
 var lg=document.createElement("div");lg.className="ic-legend";var b=document.createElement("b");b.textContent="Legend";lg.appendChild(b);
 (spec.series||[]).forEach(function(s){if(spec.series.length<2)return;var bt=document.createElement("button");bt.type="button";
  var sw=document.createElement("i");sw.style.background=s.color;bt.appendChild(sw);bt.appendChild(document.createTextNode(s.name));
  if(s.hidden)bt.className="off";bt.setAttribute("aria-pressed",s.hidden?"false":"true");
  bt.onclick=function(){s.hidden=!s.hidden;bt.className=s.hidden?"off":"";bt.setAttribute("aria-pressed",s.hidden?"false":"true");redraw()};lg.appendChild(bt)});
 (spec.keys||[]).forEach(function(k){var sp=document.createElement("span");var sw=document.createElement("i");
  sw.style.background=k.color;if(k.dash){sw.style.background="none";sw.style.borderTop="2px dashed "+k.color;sw.style.height="0"}
  sp.appendChild(sw);sp.appendChild(document.createTextNode(k.name));lg.appendChild(sp)});
 box.appendChild(lg)}
function frame(svg,spec,W,H,m,xs,ys,xt,yt,xd,yd,cat){
 svg.appendChild(el("rect",{x:m.l,y:m.t,width:W-m.l-m.r,height:H-m.t-m.b,fill:"#fff",stroke:"#c3c2b7"}));
 yt.forEach(function(v){var y=ys(v);svg.appendChild(el("line",{x1:m.l,x2:W-m.r,y1:y,y2:y,stroke:"#e1e0d9"}));
  svg.appendChild(el("text",{x:m.l-8,y:y+5,"text-anchor":"end","font-size":15,fill:"#0b0b0b"},fmt(v,yd)))});
 if(!cat)xt.forEach(function(v){var x=xs(v);svg.appendChild(el("line",{x1:x,x2:x,y1:m.t,y2:H-m.b,stroke:"#eeede8"}));
  svg.appendChild(el("text",{x:x,y:H-m.b+20,"text-anchor":"middle","font-size":15,fill:"#0b0b0b"},fmt(v,xd)))});
 svg.appendChild(el("text",{x:(m.l+W-m.r)/2,y:H-8,"text-anchor":"middle","font-size":17,"font-weight":600,fill:"#0b0b0b"},spec.xlabel||""));
 var yl=el("text",{x:18,y:(m.t+H-m.b)/2,"text-anchor":"middle","font-size":17,"font-weight":600,fill:"#0b0b0b",
  transform:"rotate(-90 18 "+(m.t+H-m.b)/2+")"},spec.ylabel||"");svg.appendChild(yl);
 if(spec.title)svg.appendChild(el("text",{x:m.l,y:22,"font-size":19,"font-weight":700,fill:"#0b0b0b"},spec.title))}
function hlines(svg,spec,W,m,ys){(spec.hlines||[]).forEach(function(h){var y=ys(h.y);
 svg.appendChild(el("line",{x1:m.l,x2:W-m.r,y1:y,y2:y,stroke:"#d03b3b","stroke-width":2,"stroke-dasharray":"7 5"}));
 svg.appendChild(el("text",{x:W-m.r-6,y:y-6,"text-anchor":"end","font-size":14,fill:"#a32020","paint-order":"stroke",stroke:"#fff","stroke-width":3},h.label))})}
function drawLine(box,spec){var W=900,H=spec.height||400,m={l:78,r:24,t:36,b:62};
 var svg=el("svg",{viewBox:"0 0 "+W+" "+H,role:"img","aria-label":spec.alt||spec.title||""});box.insertBefore(svg,box.firstChild);
 var tip=tipper(box);
 function redraw(){while(svg.firstChild)svg.removeChild(svg.firstChild);
  var vis=spec.series.filter(function(s){return !s.hidden});var X=[],Y=[];
  spec.series.forEach(function(s){s.points.forEach(function(p){X.push(p[0]);if(p[1]!=null)Y.push(p[1])})});
  (spec.hlines||[]).forEach(function(h){Y.push(h.y)});
  var x0=spec.xmin!=null?spec.xmin:Math.min.apply(null,X),x1=spec.xmax!=null?spec.xmax:Math.max.apply(null,X);
  var y0=spec.ymin!=null?spec.ymin:0,y1=spec.ymax!=null?spec.ymax:Math.max.apply(null,Y)*1.12;
  var lg=!!spec.logx,tx=function(v){return lg?Math.log10(v):v};
  var xs=function(v){return m.l+(tx(v)-tx(x0))/(tx(x1)-tx(x0))*(W-m.l-m.r)},ys=function(v){return H-m.b-(v-y0)/(y1-y0)*(H-m.t-m.b)};
  var xt=spec.xticks||nice(x0,x1,7),yt=nice(y0,y1,6),xd=spec.xdec!=null?spec.xdec:dec(xt),yd=dec(yt);
  frame(svg,spec,W,H,m,xs,ys,xt,yt,xd,yd,false);
  (spec.bands||[]).forEach(function(b){svg.appendChild(el("rect",{x:xs(b.x0),y:m.t,width:Math.max(1,xs(b.x1)-xs(b.x0)),height:H-m.t-m.b,fill:b.color||"#e8e7e1",opacity:b.opacity||0.7}))});
  (spec.vlines||[]).forEach(function(v){var x=xs(v.x);svg.appendChild(el("line",{x1:x,x2:x,y1:m.t,y2:H-m.b,stroke:"#6b6a64","stroke-dasharray":"2 4","stroke-width":1.5}))});
  hlines(svg,spec,W,m,ys);
  vis.forEach(function(s){var d="",pen=false;s.points.forEach(function(p){if(p[1]==null){pen=false;return}d+=(pen?"L":"M")+xs(p[0]).toFixed(1)+" "+ys(p[1]).toFixed(1);pen=true});
   svg.appendChild(el("path",{d:d,fill:"none",stroke:s.color,"stroke-width":2.5,"stroke-dasharray":s.dash||""}))});
  (spec.marks||[]).forEach(function(k){svg.appendChild(el("circle",{cx:xs(k.x),cy:ys(k.y),r:6,fill:k.color||"#0b0b0b",stroke:"#fff","stroke-width":2}));
   svg.appendChild(el("text",{x:xs(k.x)+12,y:ys(k.y)-34,"font-size":14,fill:"#0b0b0b","paint-order":"stroke",stroke:"#fff","stroke-width":3},k.label))});
  var dot=el("circle",{r:6,fill:"#fff","stroke-width":3,visibility:"hidden"});svg.appendChild(dot);
  var pad=el("rect",{x:m.l,y:m.t,width:W-m.l-m.r,height:H-m.t-m.b,fill:"transparent"});svg.appendChild(pad);
  pad.addEventListener("mousemove",function(ev){var r=svg.getBoundingClientRect(),k=W/r.width,px=(ev.clientX-r.left)*k,py=(ev.clientY-r.top)*k,best=null,bd=1e9;
   vis.forEach(function(s){s.points.forEach(function(p){if(p[1]==null)return;var dx=xs(p[0])-px,dy=ys(p[1])-py,q=dx*dx+dy*dy;if(q<bd){bd=q;best=[s,p]}})});
   if(!best)return;dot.setAttribute("cx",xs(best[1][0]));dot.setAttribute("cy",ys(best[1][1]));dot.setAttribute("stroke",best[0].color);dot.setAttribute("visibility","visible");
   tip.show(ev,best[0].name+"\n"+(spec.xname||"x")+": "+fmt(best[1][0],spec.xhover!=null?spec.xhover:xd+1)+" "+(spec.xunit||"")+"\n"+(spec.yname||"y")+": "+fmt(best[1][1],spec.yhover!=null?spec.yhover:yd+1)+" "+(spec.yunit||""))});
  pad.addEventListener("mouseleave",function(){tip.hide();dot.setAttribute("visibility","hidden")})}
 redraw();legend(box,spec,redraw)}
function drawBars(box,spec){var W=900,H=spec.height||420,lines=spec.labelLines||1,m={l:78,r:24,t:36,b:44+18*lines};
 var svg=el("svg",{viewBox:"0 0 "+W+" "+H,role:"img","aria-label":spec.alt||spec.title||""});box.insertBefore(svg,box.firstChild);var tip=tipper(box);
 function redraw(){while(svg.firstChild)svg.removeChild(svg.firstChild);
  var vis=spec.series.filter(function(s){return !s.hidden}),Y=[];
  spec.series.forEach(function(s){s.values.forEach(function(v){if(v!=null)Y.push(v)})});(spec.hlines||[]).forEach(function(h){Y.push(h.y)});
  var y0=0,y1=spec.ymax!=null?spec.ymax:Math.max.apply(null,Y)*1.15,n=spec.categories.length,bw=(W-m.l-m.r)/n;
  var ys=function(v){return H-m.b-(v-y0)/(y1-y0)*(H-m.t-m.b)},yt=nice(y0,y1,6),yd=dec(yt);
  frame(svg,spec,W,H,m,null,ys,[],yt,0,yd,true);var labels=[];
  spec.categories.forEach(function(c,i){var cx=m.l+bw*(i+0.5);
   if(spec.highlight===i)svg.appendChild(el("rect",{x:m.l+bw*i,y:m.t,width:bw,height:H-m.t-m.b,fill:"#f0efec"}));
   String(c).split("|").forEach(function(t,j){svg.appendChild(el("text",{x:cx,y:H-m.b+20+18*j,"text-anchor":"middle","font-size":14,fill:"#0b0b0b"},t))});
   var gw=bw*0.72,w=gw/Math.max(1,vis.length);
   vis.forEach(function(s,j){var v=s.values[i];if(v==null)return;var x=cx-gw/2+w*j,y=ys(v);
    var r=el("rect",{x:x+1,y:y,width:Math.max(2,w-2),height:Math.max(0,H-m.b-y),fill:(s.colors&&s.colors[i])||s.color});svg.appendChild(r);
    if(spec.valueLabels!==false&&w>34)labels.push(el("text",{x:x+w/2,y:y-5,"text-anchor":"middle","font-size":13,fill:"#0b0b0b","paint-order":"stroke",stroke:"#fff","stroke-width":4},fmt(v,spec.vdec!=null?spec.vdec:yd)));
    r.addEventListener("mousemove",function(ev){tip.show(ev,String(c).replace(/\|/g," ")+(spec.series.length>1?"\n"+s.name:"")+"\n"+(spec.yname||"value")+": "+fmt(v,spec.yhover!=null?spec.yhover:yd+1)+" "+(spec.yunit||"")+((s.notes&&s.notes[i])?"\n"+s.notes[i]:""))});
    r.addEventListener("mouseleave",tip.hide)})});
  hlines(svg,spec,W,m,ys);labels.forEach(function(l){svg.appendChild(l)})}
 redraw();legend(box,spec,redraw)}
function drawRanges(box,spec){var n=spec.rows.length,W=900,H=70+34*n+50,m={l:300,r:30,t:36,b:56};
 var svg=el("svg",{viewBox:"0 0 "+W+" "+H,role:"img","aria-label":spec.alt||spec.title||""});box.insertBefore(svg,box.firstChild);var tip=tipper(box);
 var V=[0];spec.rows.forEach(function(r){V.push(r.lo,r.hi)});var x0=Math.min.apply(null,V),x1=Math.max.apply(null,V);var padv=(x1-x0)*0.06;x0-=padv;x1+=padv;
 var xs=function(v){return m.l+(v-x0)/(x1-x0)*(W-m.l-m.r)},xt=nice(x0,x1,7);
 svg.appendChild(el("rect",{x:m.l,y:m.t,width:W-m.l-m.r,height:H-m.t-m.b,fill:"#fff",stroke:"#c3c2b7"}));
 xt.forEach(function(v){var x=xs(v);svg.appendChild(el("line",{x1:x,x2:x,y1:m.t,y2:H-m.b,stroke:"#eeede8"}));
  svg.appendChild(el("text",{x:x,y:H-m.b+20,"text-anchor":"middle","font-size":15,fill:"#0b0b0b"},fmt(v,0)))});
 svg.appendChild(el("line",{x1:xs(0),x2:xs(0),y1:m.t,y2:H-m.b,stroke:"#6b6a64","stroke-width":1.5}));
 svg.appendChild(el("text",{x:(m.l+W-m.r)/2,y:H-8,"text-anchor":"middle","font-size":17,"font-weight":600,fill:"#0b0b0b"},spec.xlabel||""));
 if(spec.title)svg.appendChild(el("text",{x:20,y:22,"font-size":19,"font-weight":700,fill:"#0b0b0b"},spec.title));
 spec.rows.forEach(function(r,i){var y=m.t+14+34*i,a=Math.min(r.lo,r.hi),b=Math.max(r.lo,r.hi);
  svg.appendChild(el("text",{x:m.l-10,y:y+15,"text-anchor":"end","font-size":15,fill:"#0b0b0b"},r.name));
  var bar=el("rect",{x:xs(a),y:y,width:Math.max(2,xs(b)-xs(a)),height:22,fill:r.color||"#2a78d6"});svg.appendChild(bar);
  bar.addEventListener("mousemove",function(ev){tip.show(ev,r.name+"\n"+r.tip)});bar.addEventListener("mouseleave",tip.hide)})}
function tips(box){var tip=tipper(box);box.querySelectorAll("[data-tip]").forEach(function(g){
 g.addEventListener("mousemove",function(ev){tip.show(ev,g.getAttribute("data-tip"))});g.addEventListener("mouseleave",tip.hide)})}
document.querySelectorAll("div.ic").forEach(function(box){
 if(box.hasAttribute("data-spec")){var spec=JSON.parse(box.getAttribute("data-spec"));
  ({line:drawLine,bars:drawBars,ranges:drawRanges})[spec.type](box,spec)}
 else tips(box)});
})();
</script>"""


def line(title, series, xlabel, ylabel, *, xname, yname, xunit="", yunit="", logx=False, xticks=None, xdec=None,
         hlines=(), vlines=(), bands=(), marks=(), keys=(), ymin=None, ymax=None, xmin=None, xmax=None,
         xhover=None, yhover=None, alt="") -> dict:
    """Line chart: ``series`` is a list of (name, xs, ys); a ``None`` y breaks the line."""
    out = []
    for i, s in enumerate(series):
        name, xs, ys = s[:3]
        opt = s[3] if len(s) > 3 else {}
        out.append(dict(name=name, color=opt.get("color", SERIES[i % len(SERIES)]), dash=opt.get("dash", ""),
                        hidden=bool(opt.get("hidden")),
                        points=[[_r(x), None if y is None else _r(y)] for x, y in zip(xs, ys)]))
    spec = dict(type="line", title=title, series=out, xlabel=xlabel, ylabel=ylabel, xname=xname, yname=yname,
                xunit=xunit, yunit=yunit, logx=logx, hlines=list(hlines), vlines=list(vlines), bands=list(bands),
                marks=list(marks), keys=list(keys), alt=alt)
    for k, v in dict(xticks=xticks, xdec=xdec, ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax, xhover=xhover,
                     yhover=yhover).items():
        if v is not None:
            spec[k] = v
    return spec


def bars(title, categories, series, ylabel, *, yname, yunit="", hlines=(), keys=(), highlight=None, label_lines=1,
         ymax=None, vdec=None, yhover=None, value_labels=True, xlabel="", alt="") -> dict:
    """Grouped bars: ``series`` is a list of dicts with name, values and optional colour, colours, notes."""
    out = []
    for i, s in enumerate(series):
        d = dict(name=s["name"], color=s.get("color", SERIES[i % len(SERIES)]),
                 values=[None if v is None else _r(v) for v in s["values"]], hidden=bool(s.get("hidden")))
        for k in ("colors", "notes"):
            if s.get(k):
                d[k] = list(s[k])
        out.append(d)
    spec = dict(type="bars", title=title, categories=list(categories), series=out, xlabel=xlabel, ylabel=ylabel,
                yname=yname, yunit=yunit, hlines=list(hlines), keys=list(keys), labelLines=label_lines,
                valueLabels=value_labels, alt=alt)
    for k, v in dict(highlight=highlight, ymax=ymax, vdec=vdec, yhover=yhover).items():
        if v is not None:
            spec[k] = v
    return spec


def ranges(title, rows, xlabel, *, alt="") -> dict:
    """Horizontal range bars (ranking chart): rows of dict(name, lo, hi, tip)."""
    return dict(type="ranges", title=title, xlabel=xlabel, alt=alt,
                rows=[dict(name=r["name"], lo=_r(r["lo"]), hi=_r(r["hi"]), tip=r["tip"]) for r in rows])


def _r(x):
    return round(float(x), 6)


def figure(fig_id: str, specs, fallback_img: str = "", alt: str = "") -> str:
    """One figure: a chart box per spec (dict) or inline SVG (str); the static image stays for
    a reader without scripts."""
    parts = [f'<figure class="ifig" id="{fig_id}">']
    for spec in specs:
        if isinstance(spec, str):
            parts.append(f'<div class="ic">{spec}</div>')
        else:
            parts.append('<div class="ic" data-spec="' + html.escape(json.dumps(spec, ensure_ascii=False), quote=True)
                         + '"></div>')
    if fallback_img:
        parts.append(f'<noscript><img alt="{html.escape(alt, quote=True)}" src="{fallback_img}"></noscript>')
    parts.append("</figure>")
    return "\n".join(parts)


# ------------------------------------------------------------------ schematics (SVG)

def _t(x, y, text, size=15, weight=400, anchor="start", fill=INK) -> str:
    return (f'<text x="{x}" y="{y}" font-size="{size}" font-weight="{weight}" text-anchor="{anchor}" '
            f'fill="{fill}">{html.escape(str(text))}</text>')


def _box(x, y, w, h, title, lines, fill="#f3f6fb", stroke=NAVY) -> str:
    out = [f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="8" fill="{fill}" stroke="{stroke}" stroke-width="1.5"/>',
           _t(x + 14, y + 28, title, 18, 700, fill=NAVY)]
    for i, ln in enumerate(lines):
        out.append(_t(x + 14, y + 54 + 21 * i, ln, 15))
    return "".join(out)


def _arrow(x1, y1, x2, y2) -> str:
    return (f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="{MUTED}" stroke-width="2.5" '
            f'marker-end="url(#gs-arrow)"/>')


def summary_svg(v: dict) -> str:
    """Figure 1: what the guide holds, and what its sensitivities mean.

    ``v``: n_methods, n_bodies, db_rows, db_vendors, db_types, selected, e_design, util,
    spread_pct, top_input, top_span, n_design, n_oper.
    """
    W, H = 900, 560
    s = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" role="img" '
         f'aria-label="What the fender design guide holds: inputs, methods, fender check and sensitivity">',
         '<defs><marker id="gs-arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" '
         f'orient="auto"><path d="M0 0L10 5L0 10z" fill="{MUTED}"/></marker></defs>',
         f'<rect width="{W}" height="{H}" fill="#fff"/>',
         _t(20, 30, "What the guide holds", 21, 700)]
    s.append(_box(20, 50, 200, 150, "1 Inputs", ["Ship: size, draught", "Berth: depth, structure", "Approach: angle, contact", "Conditions and tugs"]))
    s.append(_box(250, 50, 200, 150, "2 Berthing energy", [f"{v['n_methods']} methods", f"{v['n_bodies']} governing bodies", "Each coefficient with", "its source"]))
    s.append(_box(480, 50, 200, 150, "3 Fender check", ["Energy against capacity", "Reaction against limit", "Hull pressure against", "limit"]))
    s.append(_box(710, 50, 170, 150, "Database", [f"{v['db_rows']} rows", f"{v['db_vendors']} vendors", f"{v['db_types']} fender types", "Size ranges"], fill="#f7f6f2", stroke=MUTED))
    s += [_arrow(220, 125, 248, 125), _arrow(450, 125, 478, 125), _arrow(710, 125, 682, 125)]
    s.append(_t(20, 240, f"Generic case: {v['selected']}, design energy {v['e_design']} kNm, utilisation {v['util']}", 16, 600))
    s.append(_t(20, 290, "4 Sensitivity: what moves the answer, in three groups", 19, 700, fill=NAVY))
    y = 306
    for i, (title, colour, lines) in enumerate((
            ("Project and design", SERIES[1],
             ["Choices of the design:", "design factor, berth", "structure, fender size,", "fenders in contact.", "Settle them early: they", "change the design."]),
            ("Operational", SERIES[0],
             ["Seen in operation:", "velocity, angle, contact", "point, displacement,", "water depth.", f"Largest: {v['top_input']}", f"(span {v['top_span']} points)."]),
            ("Method (code)", SERIES[2],
             ["The code chosen:", f"{v['n_methods']} methods, same ship", "and berth.", f"Lowest to highest:", f"+{v['spread_pct']} % design energy.", "Select one; show the rest."]))):
        x = 20 + 293 * i
        s.append(f'<rect x="{x}" y="{y}" width="274" height="190" rx="8" fill="#fff" stroke="{colour}" stroke-width="2.5"/>')
        s.append(f'<rect x="{x}" y="{y}" width="274" height="36" rx="8" fill="{colour}"/>')
        s.append(_t(x + 14, y + 25, title, 18, 700, fill="#fff"))
        for j, ln in enumerate(lines):
            s.append(_t(x + 14, y + 60 + 21 * j, ln, 15))
    s.append(_t(20, 530, "A sensitivity is the change in the result when one input moves alone over its stated range.", 15, fill=MUTED))
    s.append(_t(20, 550, "A robust selection still meets every check after a project or design change.", 15, fill=MUTED))
    s.append("</svg>")
    return "".join(s)


CODE_COLUMNS = (("velocity", "Velocity"), ("added_mass_cm", "Added mass"), ("eccentricity_ce", "Eccentricity"),
                ("softness_cs", "Softness"), ("configuration_cc", "Berth config."), ("design_factor", "Design factor"),
                ("fender_capacity", "Fender capacity"))


def _cell_kind(text: str) -> str:
    """own rule, taken from PIANC 2002, or not used."""
    t = text.strip().lower()
    if t in ("-", "", "removed", "none"):
        return "none"
    if t == "pianc" or t.startswith("as pianc") or t.startswith("pianc table") or t.startswith("pianc k"):
        return "pianc"
    return "own"


def codes_svg(bodies: list[dict], matrix: dict[str, dict]) -> str:
    """Figure 2: governing bodies and codes against the terms the module takes from each.

    ``bodies``: rows of governing_bodies.csv (method, governing_body, region, code, edition,
    held_as); ``matrix``: method -> row of method_matrix.csv. Each mark carries the matrix
    text as ``data-tip``, shown on hover.
    """
    x0, y0, cw, rh = 450, 84, 64, 46
    W, H = x0 + cw * len(CODE_COLUMNS) + 200, y0 + rh * len(bodies) + 70
    s = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" role="img" '
         f'aria-label="Governing bodies and codes, with the terms taken from each">',
         f'<rect width="{W}" height="{H}" fill="#fff"/>',
         _t(16, 26, "Governing bodies and codes in the module", 19, 700)]
    for j, (_, head) in enumerate(CODE_COLUMNS):
        cx = x0 + cw * j + cw / 2
        s.append(f'<text x="{cx}" y="{y0 - 12}" font-size="13" font-weight="600" text-anchor="start" fill="{INK}" '
                 f'transform="rotate(-28 {cx} {y0 - 12})">{html.escape(head)}</text>')
    s.append(_t(x0 + cw * len(CODE_COLUMNS) + 12, y0 - 12, "Held as", 13, 600))
    for i, b in enumerate(bodies):
        y = y0 + rh * i
        if i % 2 == 0:
            s.append(f'<rect x="8" y="{y}" width="{W - 16}" height="{rh}" fill="#f7f6f2"/>')
        s.append(_t(16, y + 19, b["governing_body"], 14, 700))
        s.append(_t(16, y + 38, f"{b['code']}, {b['edition']}, {b['region']}", 13, fill=MUTED))
        row = matrix[b["method"]]
        for j, (key, head) in enumerate(CODE_COLUMNS):
            cx, cy = x0 + cw * j + cw / 2, y + rh / 2
            kind = _cell_kind(row[key])
            tip = html.escape(f"{b['code']}, {b['edition']}: {head.lower()}\n{row[key]}", quote=True)
            if kind == "own":
                mark = f'<circle cx="{cx}" cy="{cy}" r="10" fill="{NAVY}"/>'
            elif kind == "pianc":
                mark = f'<circle cx="{cx}" cy="{cy}" r="9" fill="#fff" stroke="{NAVY}" stroke-width="3"/>'
            else:
                mark = f'<line x1="{cx - 8}" y1="{cy}" x2="{cx + 8}" y2="{cy}" stroke="{MUTED}" stroke-width="3"/>'
            s.append(f'<g data-tip="{tip}"><rect x="{cx - cw / 2}" y="{y}" width="{cw}" height="{rh}" fill="transparent"/>{mark}</g>')
        s.append(_t(x0 + cw * len(CODE_COLUMNS) + 12, y + 28, b["held_as"], 13))
    ly = y0 + rh * len(bodies) + 30
    s.append(f'<rect x="8" y="{ly - 22}" width="{W - 16}" height="44" fill="#fff" stroke="#c3c2b7"/>')
    s.append(_t(20, ly + 5, "Legend", 14, 700))
    s.append(f'<circle cx="100" cy="{ly}" r="9" fill="{NAVY}"/>' + _t(116, ly + 5, "own rule of the code", 14))
    s.append(f'<circle cx="300" cy="{ly}" r="8" fill="#fff" stroke="{NAVY}" stroke-width="3"/>' + _t(316, ly + 5, "taken from PIANC WG33 (2002)", 14))
    s.append(f'<line x1="560" y1="{ly}" x2="576" y2="{ly}" stroke="{MUTED}" stroke-width="3"/>' + _t(586, ly + 5, "not used or removed", 14))
    s.append("</svg>")
    return "".join(s)
