"""Standalone review dashboard for assumed envelopes and causal simulated forecasts."""
from __future__ import annotations

from html import escape
import json
import math
from pathlib import Path

from digitalmodel.workflows.installation_partial_report import STYLE


def _series(series, name):
    times, values = series['times'], series['values']
    if not times or len(times) != len(values):
        raise ValueError(f'Aligned nonempty {name} arrays required')
    if any(not isinstance(v, (int, float)) or isinstance(v, bool)
           or not math.isfinite(v) for v in times + values):
        raise ValueError(f'Finite {name} values required')
    if any(b <= a for a, b in zip(times, times[1:])):
        raise ValueError(f'Strictly increasing {name} times required')
    return times


def _validate_channel(channel, now):
    history = _series(channel['history'], 'history')
    future = _series(channel['forecast'], 'forecast')
    if history[-1] > now or not math.isclose(history[-1], now, abs_tol=1e-7):
        raise ValueError('History must end at NOW without future samples')
    if future[0] <= now or not math.isclose(future[-1], now + 120, abs_tol=1e-7):
        raise ValueError('Forecast must start after NOW and end at NOW +120 s')
    if 'truth' in channel:
        truth = _series(channel['truth'], 'truth')
        if truth != future:
            raise ValueError('Withheld truth must align with forecast times')
    if 'wave_preview' in channel and _series(channel['wave_preview'], 'wave preview') != future:
        raise ValueError('Wave preview must align with forecast times')
    limit = channel.get('assumed_limit')
    if limit is not None and (isinstance(limit, bool) or not math.isfinite(limit)):
        raise ValueError('Finite assumed limit required')


def _validate(payload):
    cases = payload['cases']
    indexes = {row['index']: row for row in cases}
    if len(indexes) != len(cases) or len({(r['hs_m'], r['tp_s']) for r in cases}) != len(cases):
        raise ValueError('Unique case indexes and coordinates required')
    for row in cases:
        if row['status'] not in ('WITHIN_ASSUMPTIONS', 'EXCEEDS_ASSUMPTIONS', 'NOT_EVALUATED'):
            raise ValueError('Unsupported assessment status')
        if any(not math.isfinite(row[k]) or row[k] <= 0 for k in ('hs_m', 'tp_s')):
            raise ValueError('Positive finite sea-state coordinates required')
    demo = payload['demo']
    if demo.get('default_mode', 'history_only') not in ('history_only', 'wave_preview'):
        raise ValueError('Unknown preview mode')
    scenarios = demo.get('scenarios', [demo])
    if not scenarios:
        raise ValueError('At least one simulated scenario required')
    for scenario in scenarios:
        case = indexes.get(scenario['case_index'])
        if case is None or any(case[k] != scenario[k] for k in ('hs_m', 'tp_s')):
            raise ValueError('Prescribed demo sea state must match its case')
        frames = scenario['frames']
        if not frames or any(b['now_s'] <= a['now_s'] for a, b in zip(frames, frames[1:])):
            raise ValueError('Increasing demo frame origins required')
        for frame in frames:
            if not math.isfinite(frame['now_s']) or not frame['channels']:
                raise ValueError('Finite origin and channels required')
            for channel in frame['channels']:
                _validate_channel(channel, frame['now_s'])
                if demo.get('default_mode') == 'wave_preview' and 'wave_preview' not in channel:
                    raise ValueError('Default preview mode requires every preview channel')
    snapshot = payload.get('snapshot')
    if snapshot is not None:
        matches = [s for s in scenarios if all(s[k] == snapshot[k] for k in ('hs_m', 'tp_s'))]
        if len(matches) != 1 or not any(f['now_s'] == snapshot['now_s'] for f in matches[0]['frames']):
            raise ValueError('Configured snapshot must match an exact scenario and frame')
    json.dumps(payload, allow_nan=False)


EXTRA_STYLE = '''
.controls{display:flex;gap:16px;align-items:center;flex-wrap:wrap}button,select{font:inherit;padding:8px}
input[type=range]{flex:1;min-width:200px}.grid-cell{cursor:pointer;stroke:white;stroke-width:1}
.grid-cell:focus{outline:3px solid #112f48}.legend{display:flex;gap:20px;flex-wrap:wrap;font-size:13px}
.swatch{display:inline-block;width:14px;height:14px;margin-right:5px}.charts svg{max-width:none}
svg .axis-label{font-size:12px}svg .tick{font-size:11px}svg .now{stroke:#a62942;stroke-width:2;stroke-dasharray:6 4}
pre{white-space:pre-wrap;overflow-wrap:anywhere;font-size:12px}.demo-badge{background:#fff1d6;padding:8px;font-weight:700}
.chart-panel{margin-top:24px}.chart-panel h3{margin-bottom:4px}.status-note{font-size:13px;color:#536575}
@media print{button,input,select{display:none}section{break-inside:auto}.chart-panel{break-inside:avoid}}
'''


JAVASCRIPT = r'''
const payload=JSON.parse(document.getElementById('payload').textContent);
const scenarios=payload.demo.scenarios||[payload.demo];
const colors={WITHIN_ASSUMPTIONS:'#4c9fbe',EXCEEDS_ASSUMPTIONS:'#df9b45',NOT_EVALUATED:'#c5ccd3'};
let scenarioIndex=0,frameIndex=0,timer=null;
let forecastMode=payload.demo.default_mode||'history_only';
if(payload.snapshot){
  scenarioIndex=scenarios.findIndex(s=>s.hs_m===payload.snapshot.hs_m&&s.tp_s===payload.snapshot.tp_s);
  frameIndex=scenarios[scenarioIndex].frames.findIndex(f=>f.now_s===payload.snapshot.now_s);
}
const byId=id=>document.getElementById(id);
const svgNode=(tag,attributes={},text=null)=>{
  const node=document.createElementNS('http://www.w3.org/2000/svg',tag);
  Object.entries(attributes).forEach(([key,value])=>node.setAttribute(key,value));
  if(text!==null)node.textContent=text;
  return node;
};
function selectCell(cell){
  byId('cell-title').textContent=`Case ${cell.index}: Hs ${cell.hs_m} m / Tp ${cell.tp_s} s`;
  byId('cell-status').textContent=cell.status.replaceAll('_',' ')+' — '+(cell.reason||'Assumed criteria only');
  byId('cell-details').textContent=JSON.stringify({metrics:cell.metrics||{},checks:cell.checks||(cell.metrics&&cell.metrics.checks)||[]},null,2);
}
function drawGrid(){
  const svg=byId('envelope');svg.replaceChildren();
  const periods=[...new Set(payload.cases.map(c=>c.tp_s))].sort((a,b)=>a-b);
  const heights=[...new Set(payload.cases.map(c=>c.hs_m))].sort((a,b)=>b-a);
  const width=760,height=390,left=72,top=25,cw=(width-left-30)/periods.length,ch=(height-top-50)/heights.length;
  svg.setAttribute('viewBox',`0 0 ${width} ${height}`);
  payload.cases.forEach(cell=>{
    const x=left+periods.indexOf(cell.tp_s)*cw,y=top+heights.indexOf(cell.hs_m)*ch;
    const rect=svgNode('rect',{x,y,width:cw,height:ch,fill:colors[cell.status],class:'grid-cell',tabindex:0,role:'button','aria-label':`Hs ${cell.hs_m} Tp ${cell.tp_s}: ${cell.status}`});
    rect.append(svgNode('title',{},`Hs ${cell.hs_m} m / Tp ${cell.tp_s} s: ${cell.status}`));
    rect.addEventListener('click',()=>selectCell(cell));
    rect.addEventListener('keydown',event=>{if(event.key==='Enter'||event.key===' '){event.preventDefault();selectCell(cell);}});
    svg.append(rect);
  });
  periods.forEach((p,i)=>svg.append(svgNode('text',{x:left+(i+.5)*cw,y:height-29,'text-anchor':'middle'},p)));
  heights.forEach((h,i)=>svg.append(svgNode('text',{x:left-10,y:top+(i+.6)*ch,'text-anchor':'end'},h)));
  svg.append(svgNode('text',{x:width/2,y:height-5,'text-anchor':'middle'},'Peak period Tp (s)'));
  svg.append(svgNode('text',{x:3,y:14},'Hs (m)'));
  const s=scenarios[scenarioIndex],mx=left+(periods.indexOf(s.tp_s)+.5)*cw,my=top+(heights.indexOf(s.hs_m)+.5)*ch;
  svg.append(svgNode('circle',{cx:mx,cy:my,r:Math.max(5,Math.min(cw,ch)*.3),fill:'#fff',stroke:'#a62942','stroke-width':3}));
  svg.append(svgNode('text',{x:mx+10,y:my-7,fill:'#a62942','font-weight':'bold'},'DEMO'));
}
function drawChart(channel,frame){
  const holder=document.createElement('div');holder.className='chart-panel';
  const title=document.createElement('h3');title.textContent=`${channel.label} (${channel.units})`;holder.append(title);
  const svg=svgNode('svg',{viewBox:'0 0 1000 290',role:'img','aria-label':`${channel.label}: history and 120 s prediction`});holder.append(svg);
  const showTruth=byId('truth-toggle').checked&&channel.truth;
  const displayed=forecastMode==='wave_preview'?channel.wave_preview:channel.forecast;
  const series=[channel.history,displayed,...(showTruth?[channel.truth]:[])];
  const values=series.flatMap(s=>s.values);if(channel.assumed_limit!==null&&channel.assumed_limit!==undefined)values.push(channel.assumed_limit);
  let ymin=Math.min(...values),ymax=Math.max(...values),pad=Math.max((ymax-ymin)*.12,.01);ymin-=pad;ymax+=pad;
  const xmin=channel.history.times[0],xmax=frame.now_s+120;
  const x=t=>65+(t-xmin)/(xmax-xmin)*915,y=v=>245-(v-ymin)/(ymax-ymin)*215;
  svg.append(svgNode('rect',{x:x(frame.now_s),y:25,width:x(xmax)-x(frame.now_s),height:220,fill:'#eef4fc'}));
  for(let i=0;i<=4;i++){
    const v=ymin+(ymax-ymin)*i/4,yy=y(v);
    svg.append(svgNode('line',{x1:65,y1:yy,x2:980,y2:yy,stroke:'#dae2e8'}));
    svg.append(svgNode('text',{x:57,y:yy+4,'text-anchor':'end',class:'tick'},v.toFixed(2)));
  }
  [xmin,frame.now_s,xmax].forEach(t=>svg.append(svgNode('text',{x:x(t),y:267,'text-anchor':'middle'},`${t.toFixed(0)} s`)));
  const line=(s,color,dash='')=>svg.append(svgNode('polyline',{points:s.times.map((t,i)=>`${x(t)},${y(s.values[i])}`).join(' '),fill:'none',stroke:color,'stroke-width':2,'stroke-dasharray':dash}));
  line(channel.history,'#173e56');line(displayed,'#147fac');if(showTruth)line(channel.truth,'#777','3 5');
  if(channel.assumed_limit!==null&&channel.assumed_limit!==undefined){
    svg.append(svgNode('line',{x1:65,y1:y(channel.assumed_limit),x2:980,y2:y(channel.assumed_limit),stroke:'#c07716','stroke-width':2,'stroke-dasharray':'8 4'}));
    svg.append(svgNode('text',{x:970,y:y(channel.assumed_limit)-5,'text-anchor':'end'},`Assumed limit ${channel.assumed_limit} ${channel.units}`));
  }
  svg.append(svgNode('line',{x1:x(frame.now_s),y1:20,x2:x(frame.now_s),y2:245,class:'now'}));
  svg.append(svgNode('text',{x:x(frame.now_s)+6,y:16,fill:'#a62942'},'NOW'));
  svg.append(svgNode('text',{x:65,y:16},'Recorded simulated history'));
  const forecastLabel=forecastMode==='history_only'?'120 s prediction':channel.id==='wave_elevation'?'Supplied simulated wave preview':'120 s conditional load forecast';
  svg.append(svgNode('text',{x:980,y:16,'text-anchor':'end'},forecastLabel));
  const note=document.createElement('p');note.className='status-note';
  const fit=forecastMode==='wave_preview'?channel.preview_fit_status:channel.fit_status;
  note.textContent=`${forecastLabel}. Fit: ${fit||'not supplied'}. Dark: history; blue: displayed preview/forecast; dashed grey: withheld truth when enabled. Uncertainty bounds are not established.`;holder.append(note);
  holder.append(metricsNote(channel));
  return holder;
}
function metricsNote(channel){
  const note=document.createElement('p');note.className='status-note';
  if(forecastMode==='wave_preview'&&channel.id==='wave_elevation'){note.textContent='Supplied random-wave input, not a wave prediction. Forecast-error metrics do not apply.';return note;}
  const source=forecastMode==='wave_preview'?channel.wave_preview_metrics:channel.metrics;
  const metrics=source&&((source['120'])||source),method=forecastMode==='wave_preview'?'oracle_wave_fir':'autoregression';
  if(!metrics||!metrics[method]||!metrics.persistence||!metrics.history_mean){note.textContent='120 s forecast comparison: not supplied.';return note;}
  const ar=metrics[method].rmse,persistence=metrics.persistence.rmse,mean=metrics.history_mean.rmse;
  const verdict=ar<Math.min(persistence,mean)?'Lower observed RMSE than both naive baselines for this origin.':'No demonstrated advantage over the best naive baseline for this origin.';
  note.textContent=`120 s RMSE (${channel.units}): forecast ${ar.toFixed(3)}; persistence ${persistence.toFixed(3)}; history mean ${mean.toFixed(3)}. ${verdict} This comparison does not establish offshore forecast skill.`;
  return note;
}
function utilizationText(frame){
  const rows=frame.channels.filter(c=>c.id!=='wave_elevation'&&Number.isFinite(c.assumed_limit)&&c.assumed_limit>0).map(c=>{
    const shown=forecastMode==='wave_preview'?c.wave_preview:c.forecast;
    const peak=Math.max(0,...shown.values);return {label:c.label,peak,units:c.units,ratio:peak/c.assumed_limit};
  });
  if(!rows.length)return 'Forecast peak utilization: no positive assumed load limits supplied.';
  const governing=rows.reduce((a,b)=>a.ratio>=b.ratio?a:b);
  return rows.map(r=>`${r.label}: positive forecast peak ${r.peak.toFixed(2)} ${r.units}, assumed-limit utilization ${r.ratio.toFixed(3)}`).join(' · ')+`. Governing among shown load channels only: ${governing.label} (${governing.ratio.toFixed(3)}). This does not reclassify the full-record Hs–Tp cell.`;
}
function renderFrame(){
  const s=scenarios[scenarioIndex],frame=s.frames[frameIndex];
  byId('frame-slider').max=s.frames.length-1;byId('frame-slider').value=frameIndex;
  byId('frame-label').textContent=`NOW ${frame.now_s} s · forecast to ${frame.now_s+120} s`;
  byId('demo-label').textContent=`SIMULATED DEMO: Hs ${s.hs_m} m / Tp ${s.tp_s} s · ${s.source_label}`;
  byId('charts').replaceChildren(...frame.channels.map(c=>drawChart(c,frame)));
  byId('mode-notice').textContent=forecastMode==='wave_preview'?'SIMULATED WAVE PREVIEW INPUT — conditional load forecast; offshore wave prediction not validated':'History-only autoregression: inputs end at NOW; future observations are held out.';
  byId('forecast-utilization').textContent=utilizationText(frame);
  drawGrid();
}
function stopPlayback(){if(timer!==null)clearInterval(timer);timer=null;byId('play').textContent='Play recorded frames';}
function initialize(){
  scenarios.forEach((s,i)=>{const option=document.createElement('option');option.value=i;option.textContent=`Hs ${s.hs_m} m / Tp ${s.tp_s} s — case ${s.case_index}`;byId('scenario').append(option);});
  byId('scenario').addEventListener('change',event=>{stopPlayback();scenarioIndex=Number(event.target.value);frameIndex=0;renderFrame();});
  byId('frame-slider').addEventListener('input',event=>{stopPlayback();frameIndex=Number(event.target.value);renderFrame();});
  byId('truth-toggle').addEventListener('change',renderFrame);
  byId('forecast-mode').value=forecastMode;
  byId('forecast-mode').disabled=!scenarios.every(s=>s.frames.every(f=>f.channels.every(c=>c.wave_preview)));
  byId('forecast-mode').addEventListener('change',event=>{forecastMode=event.target.value;renderFrame();});
  byId('play').addEventListener('click',()=>{
    if(timer!==null){stopPlayback();return;}
    if(frameIndex===scenarios[scenarioIndex].frames.length-1)frameIndex=0;
    byId('play').textContent='Pause';renderFrame();
    timer=setInterval(()=>{if(frameIndex>=scenarios[scenarioIndex].frames.length-1){stopPlayback();return;}frameIndex++;renderFrame();},1500);
  });
  byId('scenario').value=scenarioIndex;
  selectCell(payload.cases.find(c=>c.index===scenarios[scenarioIndex].case_index));renderFrame();
}
initialize();
'''


def _criteria_html(payload):
    rows = []
    for criterion in payload.get('criteria', []):
        fields = [criterion.get(k, '') for k in ('label', 'limit', 'units', 'status')]
        rows.append('<tr>' + ''.join(f'<td>{escape(str(value))}</td>' for value in fields) + '</tr>')
    return ('<table><thead><tr><th>Check</th><th>Assumed limit</th><th>Unit</th><th>Basis status</th></tr>'
            '</thead><tbody>' + ''.join(rows) + '</tbody></table>')


def render_dashboard(payload):
    _validate(payload)
    encoded = json.dumps(payload, allow_nan=False, separators=(',', ':'))
    encoded = encoded.replace('&', '\\u0026').replace('<', '\\u003c').replace('>', '\\u003e')
    limitations = ''.join(f'<li>{escape(str(item))}</li>' for item in payload.get('limitations', []))
    return f'''<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1"><title>{escape(payload['title'])}</title>
<style>{STYLE}{EXTRA_STYLE}</style></head><body><main><header><div class="tag">Installation capability / review demonstration</div>
<h1>{escape(payload['title'])}</h1><p>Snapshot: {escape(payload['created_utc'])}</p></header>
<section><p class="demo-badge">SIMULATED DEMO · Not connected offshore</p><p>Assumed criteria support comparative screening.
These cells are not approved operating limits. The marker identifies a prescribed simulation case, not measured offshore Hs or Tp.</p>
<h2>1 · Assumed criteria</h2>{_criteria_html(payload)}<p class="caption">Table 1. Project assumptions; operational acceptance remains NOT EVALUATED.</p></section>
<section><h2>2 · Hs–Tp envelope under assumptions</h2><div class="legend">
<span><i class="swatch" style="background:#4c9fbe"></i>Within assumptions</span>
<span><i class="swatch" style="background:#df9b45"></i>Exceeds assumptions</span>
<span><i class="swatch" style="background:#c5ccd3"></i>Not evaluated</span></div>
<svg id="envelope" role="img" aria-label="Interactive Hs Tp assumed-criteria grid"></svg>
<p class="caption">Figure 1. Select a cell to inspect its checks. The DEMO marker follows the separate scenario selector.</p>
<h3 id="cell-title"></h3><p id="cell-status"></p><details><summary>Selected cell metrics and detailed checks</summary><pre id="cell-details"></pre></details></section>
<section><h2>3 · Simulated near-real-time review</h2><div class="controls"><label>Recorded case <select id="scenario"></select></label>
<label>Forecast mode <select id="forecast-mode"><option value="wave_preview">Supplied random-wave preview / conditional load</option><option value="history_only">History-only autoregression</option></select></label>
<button id="play" type="button">Play recorded frames</button><input id="frame-slider" type="range" min="0" max="0" step="1" value="0" aria-label="Recorded forecast origin">
<strong id="frame-label"></strong></div><p id="demo-label"></p>
<p>Playback advances precomputed forecast origins through a recorded simulation. Hs and Tp stay fixed within that case.
History-only inputs end at NOW. Conditional load forecasts use a supplied future random-wave record and models fitted to past wave/load data.</p>
<p id="mode-notice" class="demo-badge">SIMULATED WAVE PREVIEW INPUT — conditional load forecast; offshore wave prediction not validated</p>
<p id="forecast-utilization"></p>
<label><input id="truth-toggle" type="checkbox"> Show withheld simulated truth</label><div id="charts" class="charts"></div></section>
<section><h2>4 · Limitations and pending qualification</h2><ul>{limitations}</ul>
<p>No offshore connection, uncertainty qualification or operational forecast approval is represented.</p></section>
<footer>Reusable engineering result owner: private digitalmodel-data. Offline review artifact.</footer></main>
<script id="payload" type="application/json">{encoded}</script><script>{JAVASCRIPT}</script></body></html>'''


def write_dashboard(payload, output):
    output = Path(output)
    if output.exists():
        raise FileExistsError('New dashboard path required')
    html = render_dashboard(payload)
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open('x', encoding='utf-8', newline='\n') as stream:
        stream.write(html)
    if output.read_text(encoding='utf-8') != html:
        raise ValueError('Dashboard readback mismatch')
    return output
