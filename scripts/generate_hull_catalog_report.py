#!/usr/bin/env python3
"""Generate interactive HTML report for hull panel catalog.

ABOUTME: Reads all GDF files from the hull panel catalog, renders 3D mesh
visualizations and 2D orthographic projections using Plotly, and produces
a self-contained HTML report with each hull shown as a row of text + schematics.

Usage:
    PYTHONPATH="src:../assetutilities/src" python3 scripts/generate_hull_catalog_report.py
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO_ROOT / "src"))

from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import GDFHandler
from digitalmodel.hydrodynamics.hull_library.panel_catalog import PanelCatalog

CATALOG_YAML = REPO_ROOT / "data" / "hull_library" / "catalog" / "hull_panel_catalog.yaml"
OUTPUT_HTML = REPO_ROOT / "data" / "hull_library" / "catalog" / "hull_panel_catalog.html"

TYPE_COLORS = {
    "barge": "#2196F3",
    "cylinder": "#FF9800",
    "ellipsoid": "#9C27B0",
    "custom": "#795548",
    "sphere": "#E91E63",
    "semi_pontoon": "#4CAF50",
    "ship": "#00BCD4",
    "spar": "#F44336",
    "fpso": "#3F51B5",
    "lngc": "#CDDC39",
    "tanker": "#607D8B",
}


def load_mesh_data(gdf_path: Path) -> dict | None:
    """Parse GDF file and return mesh data for 3D and 2D rendering."""
    try:
        handler = GDFHandler()
        mesh = handler.read(gdf_path)
        verts = mesh.vertices
        panels = mesh.panels

        # Triangulate quads for Mesh3d
        i_list, j_list, k_list = [], [], []
        for panel in panels:
            v = [int(idx) for idx in panel]
            i_list.append(v[0]); j_list.append(v[1]); k_list.append(v[2])
            if v[2] != v[3]:
                i_list.append(v[0]); j_list.append(v[2]); k_list.append(v[3])

        # Build edge list for 2D projections (unique edges from panels)
        edge_set = set()
        for panel in panels:
            v = [int(idx) for idx in panel]
            n = 4 if v[2] != v[3] else 3
            for e in range(n):
                a, b = v[e], v[(e + 1) % n]
                edge_set.add((min(a, b), max(a, b)))

        edges = list(edge_set)

        return {
            "x": verts[:, 0].tolist(),
            "y": verts[:, 1].tolist(),
            "z": verts[:, 2].tolist(),
            "i": i_list,
            "j": j_list,
            "k": k_list,
            "edges": edges,
            "n_panels": int(mesh.n_panels),
            "n_vertices": int(mesh.n_vertices),
        }
    except Exception as exc:
        print(f"  WARNING: Could not parse {gdf_path}: {exc}")
        return None


def build_report_data(catalog: PanelCatalog) -> list[dict]:
    """Build data for each catalog entry including mesh geometry."""
    entries = []
    for entry in catalog.entries:
        fp = Path(entry.file_path)
        if not fp.is_absolute():
            fp = REPO_ROOT / fp

        mesh_data = None
        if fp.exists() and fp.suffix.lower() == ".gdf":
            mesh_data = load_mesh_data(fp)

        entries.append({
            "hull_id": entry.hull_id,
            "name": entry.name,
            "hull_type": entry.hull_type.value,
            "panel_format": entry.panel_format.value,
            "panel_count": entry.panel_count,
            "vertex_count": entry.vertex_count,
            "symmetry": entry.symmetry or "none",
            "length_m": entry.length_m,
            "beam_m": entry.beam_m,
            "draft_m": entry.draft_m,
            "color": TYPE_COLORS.get(entry.hull_type.value, "#999"),
            "mesh": mesh_data,
            "file_path": entry.file_path,
        })

    return entries


def generate_html(entries: list[dict]) -> str:
    """Generate the complete HTML report with column layout per hull."""
    entries_json = json.dumps(entries)
    total_panels = sum(e["panel_count"] or 0 for e in entries)
    n_types = len(set(e["hull_type"] for e in entries))

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Hull Panel Catalog</title>
<script src="https://cdn.plot.ly/plotly-2.32.0.min.js"></script>
<style>
  * {{ margin: 0; padding: 0; box-sizing: border-box; }}
  body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
         background: #0d1117; color: #c9d1d9; line-height: 1.5; }}

  /* Header */
  .header {{ background: linear-gradient(135deg, #161b22 0%, #1a2332 100%);
             padding: 32px 48px; border-bottom: 1px solid #30363d; }}
  .header h1 {{ font-size: 28px; font-weight: 600; color: #e6edf3; }}
  .header p {{ color: #8b949e; margin-top: 4px; font-size: 14px; }}
  .stats {{ display: flex; gap: 40px; margin-top: 16px; }}
  .stat-n {{ font-size: 28px; font-weight: 700; color: #58a6ff; }}
  .stat-l {{ font-size: 11px; color: #8b949e; text-transform: uppercase; letter-spacing: 0.5px; }}

  /* Hull Entry Row */
  .hull-entry {{ display: grid; grid-template-columns: 300px 1fr;
                 border-bottom: 1px solid #21262d; min-height: 420px; }}
  .hull-entry:nth-child(even) {{ background: #0d1117; }}
  .hull-entry:nth-child(odd) {{ background: #111820; }}

  /* Text column */
  .hull-text {{ padding: 24px; display: flex; flex-direction: column; gap: 12px;
                border-right: 1px solid #21262d; }}
  .hull-name {{ font-size: 18px; font-weight: 600; color: #e6edf3; }}
  .hull-badge {{ display: inline-block; font-size: 11px; padding: 2px 10px; border-radius: 12px;
                 color: #fff; font-weight: 500; margin-top: 4px; }}
  .dims-grid {{ display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin-top: 8px; }}
  .dim {{ background: #161b22; border: 1px solid #30363d; border-radius: 6px; padding: 8px 10px; }}
  .dim-value {{ font-size: 16px; font-weight: 600; color: #e6edf3; }}
  .dim-label {{ font-size: 10px; color: #8b949e; text-transform: uppercase; letter-spacing: 0.3px; }}
  .hull-meta {{ font-size: 12px; color: #8b949e; margin-top: auto; }}
  .hull-meta span {{ color: #58a6ff; }}

  /* Schematics column */
  .hull-schematics {{ display: grid; grid-template-columns: 1.2fr 1fr; grid-template-rows: 1fr 1fr;
                      gap: 2px; padding: 4px; }}
  .schem-cell {{ position: relative; min-height: 200px; }}
  .schem-3d {{ grid-row: 1 / 3; }}
  .schem-label {{ position: absolute; top: 4px; left: 8px; font-size: 10px; color: #484f58;
                  text-transform: uppercase; letter-spacing: 0.5px; z-index: 1; pointer-events: none; }}

  /* Overview charts */
  .overview {{ padding: 32px 48px; }}
  .overview h2 {{ font-size: 20px; font-weight: 600; color: #e6edf3; margin-bottom: 16px;
                  padding-bottom: 8px; border-bottom: 1px solid #21262d; }}
  .chart-row {{ display: grid; grid-template-columns: 1fr 1fr; gap: 16px; margin-bottom: 32px; }}
  .chart-box {{ height: 420px; }}

  .footer {{ padding: 20px 48px; text-align: center; color: #484f58; font-size: 12px;
             border-top: 1px solid #21262d; }}
</style>
</head>
<body>

<div class="header">
  <h1>Hull Panel Catalog</h1>
  <p>Consolidated inventory of hull panel meshes for diffraction and hydrodynamic analysis</p>
  <div class="stats">
    <div><div class="stat-n">{len(entries)}</div><div class="stat-l">Hulls</div></div>
    <div><div class="stat-n">{n_types}</div><div class="stat-l">Types</div></div>
    <div><div class="stat-n">{total_panels:,}</div><div class="stat-l">Total Panels</div></div>
  </div>
</div>

<!-- Overview Charts -->
<div class="overview">
  <h2>Overview</h2>
  <div class="chart-row">
    <div class="chart-box" id="size-chart"></div>
    <div class="chart-box" id="bubble-chart"></div>
  </div>
</div>

<!-- Per-Hull Entries -->
<div id="hull-entries"></div>

<div class="footer">
  Hull Panel Catalog v1.0 &mdash; Generated 2026-02-10 &mdash; digitalmodel/hydrodynamics/hull_library
</div>

<script>
const entries = {entries_json};
const plotCfg = {{ responsive: true, displaylogo: false, displayModeBar: false }};
const plotCfg3d = {{ responsive: true, displaylogo: false,
  modeBarButtonsToRemove: ['toImage','sendDataToCloud','resetCameraLastSave3d'],
  displayModeBar: 'hover' }};
const bg = '#0d1117';
const bgAlt = '#111820';

// ─── Overview: Size Comparison ───
(function() {{
  const sorted = [...entries].filter(e => e.length_m > 2)
    .sort((a,b) => (b.length_m||0) - (a.length_m||0));
  const mk = (name, key, op) => ({{
    name, type:'bar', orientation:'h',
    y: sorted.map(e=>e.name), x: sorted.map(e=>e[key]||0),
    marker: {{ color: sorted.map(e=>e.color), opacity: op }},
    hovertemplate: '%{{y}}: %{{x:.1f}} m<extra>'+name+'</extra>',
  }});
  Plotly.newPlot('size-chart',
    [mk('Length','length_m',0.9), mk('Beam','beam_m',0.6), mk('Draft','draft_m',0.35)],
    {{ barmode:'group',
       xaxis:{{title:'Dimension (m)',color:'#8b949e',gridcolor:'#21262d'}},
       yaxis:{{color:'#8b949e',automargin:true}},
       paper_bgcolor:bg, plot_bgcolor:bg, font:{{color:'#c9d1d9',size:12}},
       legend:{{orientation:'h',y:1.08,font:{{color:'#8b949e'}}}},
       margin:{{l:160,r:10,t:20,b:40}},
       title:{{text:'Dimensions by Hull',font:{{size:14,color:'#8b949e'}},x:0.5,y:0.98}},
    }}, plotCfg);
}})();

// ─── Overview: Bubble Chart ───
(function() {{
  const w = entries.filter(e => e.length_m && e.draft_m && e.panel_count);
  Plotly.newPlot('bubble-chart', [{{
    type:'scatter', mode:'markers+text',
    x: w.map(e=>e.length_m), y: w.map(e=>e.draft_m),
    text: w.map(e=>e.name), textposition:'top center',
    textfont:{{size:10,color:'#8b949e'}},
    marker:{{ size: w.map(e=>Math.sqrt(e.panel_count)*1.8),
              color: w.map(e=>e.color), opacity:0.8,
              line:{{color:'rgba(255,255,255,0.3)',width:1}} }},
    hovertemplate:'<b>%{{text}}</b><br>L=%{{x:.1f}}m, D=%{{y:.1f}}m<br>%{{customdata}} panels<extra></extra>',
    customdata: w.map(e=>e.panel_count),
  }}], {{
    xaxis:{{title:'Length (m)',color:'#8b949e',gridcolor:'#21262d',zeroline:false}},
    yaxis:{{title:'Draft (m)',color:'#8b949e',gridcolor:'#21262d',zeroline:false}},
    paper_bgcolor:bg, plot_bgcolor:bg, font:{{color:'#c9d1d9',size:12}},
    margin:{{l:50,r:10,t:20,b:40}}, showlegend:false,
    title:{{text:'Panel Count vs Size',font:{{size:14,color:'#8b949e'}},x:0.5,y:0.98}},
    annotations:[{{x:0.98,y:0.02,xref:'paper',yref:'paper',
      text:'bubble size = panel count',showarrow:false,font:{{size:10,color:'#484f58'}}}}],
  }}, plotCfg);
}})();

// ─── Per-Hull Entries ───
(function() {{
  const container = document.getElementById('hull-entries');

  entries.forEach((entry, idx) => {{
    const row = document.createElement('div');
    row.className = 'hull-entry';
    const isEven = idx % 2 === 0;
    const rowBg = isEven ? bg : bgAlt;

    const dims = [
      ['Length', entry.length_m, 'm'],
      ['Beam', entry.beam_m, 'm'],
      ['Draft', entry.draft_m, 'm'],
      ['Panels', entry.panel_count, ''],
      ['Vertices', entry.vertex_count, ''],
      ['Symmetry', entry.symmetry, ''],
    ];
    const dimsHtml = dims.map(([label, val, unit]) =>
      `<div class="dim"><div class="dim-value">${{val != null ? val : '-'}}${{unit && val != null ? ' '+unit : ''}}</div>` +
      `<div class="dim-label">${{label}}</div></div>`
    ).join('');

    row.innerHTML = `
      <div class="hull-text">
        <div>
          <div class="hull-name">${{entry.name}}</div>
          <span class="hull-badge" style="background:${{entry.color}}">${{entry.hull_type}}</span>
        </div>
        <div class="dims-grid">${{dimsHtml}}</div>
        <div class="hull-meta">
          Format: <span>${{entry.panel_format}}</span><br>
          Path: <span>${{entry.file_path}}</span>
        </div>
      </div>
      <div class="hull-schematics">
        <div class="schem-cell schem-3d" id="schem3d-${{idx}}">
          <div class="schem-label">3D Mesh</div>
        </div>
        <div class="schem-cell" id="schem-profile-${{idx}}">
          <div class="schem-label">Profile (side)</div>
        </div>
        <div class="schem-cell" id="schem-plan-${{idx}}">
          <div class="schem-label">Plan (top)</div>
        </div>
      </div>
    `;
    container.appendChild(row);

    if (!entry.mesh) {{
      ['schem3d','schem-profile','schem-plan'].forEach(id => {{
        document.getElementById(`${{id}}-${{idx}}`).innerHTML +=
          '<div style="display:flex;align-items:center;justify-content:center;height:100%;color:#484f58;font-size:12px;">No mesh data</div>';
      }});
      return;
    }}

    const m = entry.mesh;

    // ─── 3D Mesh ───
    const meshTrace = {{
      type:'mesh3d', x:m.x, y:m.y, z:m.z, i:m.i, j:m.j, k:m.k,
      color: entry.color, opacity:0.85, flatshading:true,
      lighting:{{ambient:0.6,diffuse:0.8,specular:0.3,roughness:0.5}},
      lightposition:{{x:1000,y:1000,z:1000}},
      hoverinfo:'x+y+z',
    }};
    // Wireframe for 3D
    const wx=[],wy=[],wz=[];
    for (const [a,b] of m.edges) {{
      wx.push(m.x[a],m.x[b],null);
      wy.push(m.y[a],m.y[b],null);
      wz.push(m.z[a],m.z[b],null);
    }}
    const wireTrace = {{
      type:'scatter3d',mode:'lines',
      x:wx,y:wy,z:wz,
      line:{{color:'rgba(255,255,255,0.1)',width:0.8}},
      hoverinfo:'skip',showlegend:false,
    }};

    Plotly.newPlot(`schem3d-${{idx}}`, [meshTrace, wireTrace], {{
      scene: {{
        xaxis:{{visible:false}}, yaxis:{{visible:false}}, zaxis:{{visible:false}},
        bgcolor: rowBg,
        camera:{{eye:{{x:1.6,y:1.2,z:0.7}}}},
        aspectmode:'data',
      }},
      margin:{{l:0,r:0,t:0,b:0}},
      paper_bgcolor: rowBg, plot_bgcolor: rowBg, showlegend:false,
    }}, plotCfg3d);

    // ─── Profile View (side: X vs Z) ───
    const profX=[], profZ=[];
    for (const [a,b] of m.edges) {{
      profX.push(m.x[a],m.x[b],null);
      profZ.push(m.z[a],m.z[b],null);
    }}
    Plotly.newPlot(`schem-profile-${{idx}}`, [{{
      type:'scatter',mode:'lines',
      x:profX, y:profZ,
      line:{{color:entry.color,width:0.6}},
      hoverinfo:'skip',
    }}], {{
      xaxis:{{title:'X (m)',color:'#484f58',gridcolor:'#21262d',zeroline:false,
              titlefont:{{size:10}},tickfont:{{size:9}}}},
      yaxis:{{title:'Z (m)',color:'#484f58',gridcolor:'#21262d',zeroline:false,
              scaleanchor:'x',titlefont:{{size:10}},tickfont:{{size:9}}}},
      paper_bgcolor:rowBg, plot_bgcolor:rowBg,
      margin:{{l:40,r:8,t:20,b:30}}, showlegend:false,
    }}, plotCfg);

    // ─── Plan View (top: X vs Y) ───
    const planX=[], planY=[];
    for (const [a,b] of m.edges) {{
      planX.push(m.x[a],m.x[b],null);
      planY.push(m.y[a],m.y[b],null);
    }}
    Plotly.newPlot(`schem-plan-${{idx}}`, [{{
      type:'scatter',mode:'lines',
      x:planX, y:planY,
      line:{{color:entry.color,width:0.6}},
      hoverinfo:'skip',
    }}], {{
      xaxis:{{title:'X (m)',color:'#484f58',gridcolor:'#21262d',zeroline:false,
              titlefont:{{size:10}},tickfont:{{size:9}}}},
      yaxis:{{title:'Y (m)',color:'#484f58',gridcolor:'#21262d',zeroline:false,
              scaleanchor:'x',titlefont:{{size:10}},tickfont:{{size:9}}}},
      paper_bgcolor:rowBg, plot_bgcolor:rowBg,
      margin:{{l:40,r:8,t:20,b:30}}, showlegend:false,
    }}, plotCfg);
  }});
}})();
</script>
</body>
</html>"""


def main() -> None:
    print("Loading catalog...")
    catalog = PanelCatalog.from_yaml(CATALOG_YAML)
    print(f"  {len(catalog.entries)} entries")

    print("Parsing GDF meshes for 3D rendering...")
    entries = build_report_data(catalog)

    meshes_loaded = sum(1 for e in entries if e["mesh"] is not None)
    print(f"  {meshes_loaded}/{len(entries)} meshes loaded")

    print("Generating HTML report...")
    html = generate_html(entries)

    OUTPUT_HTML.parent.mkdir(parents=True, exist_ok=True)
    with open(OUTPUT_HTML, "w") as f:
        f.write(html)

    size_kb = OUTPUT_HTML.stat().st_size / 1024
    print(f"  Written: {OUTPUT_HTML} ({size_kb:.0f} KB)")
    print("Done.")


if __name__ == "__main__":
    main()
