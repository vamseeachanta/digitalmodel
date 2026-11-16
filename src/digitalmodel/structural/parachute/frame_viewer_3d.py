"""
ABOUTME: 3D interactive frame viewer using three.js
ABOUTME: Generates self-contained HTML from FrameGeometry3D for browser inspection
"""

import argparse
import base64
import hashlib
import json
import math
import os
from pathlib import Path
from typing import Dict, List, Tuple

from digitalmodel.structural.parachute.frame_geometry_3d import (
    FrameGeometry3D,
    Member3D,
    Node3D,
    build_gt1r_frame_3d,
)

# ── CDN URLs (jsdelivr, same pattern as calc_report_html.py) ──────────────
THREEJS_VERSION = "0.167.0"
THREEJS_CDN = f"https://cdn.jsdelivr.net/npm/three@{THREEJS_VERSION}/build/three.module.min.js"
THREEJS_ORBIT_CDN = f"https://cdn.jsdelivr.net/npm/three@{THREEJS_VERSION}/examples/jsm/controls/OrbitControls.js"
THREEJS_CSS2D_CDN = f"https://cdn.jsdelivr.net/npm/three@{THREEJS_VERSION}/examples/jsm/renderers/CSS2DRenderer.js"

# Pinned files from the unpacked three@0.167.0 npm package; never vendored.
THREEJS_FILES = {
    "THREEJS_CDN": ("build/three.module.min.js", "54a7a52ddce0ad06fbd1d021fd03b1e1812d946114a2111e1260bcd490c6b840"),
    "ORBIT_CDN": ("examples/jsm/controls/OrbitControls.js", "f260591ef315aa04888152e7f121865214e33fb54727145cf4e4445058db1297"),
    "CSS2D_CDN": ("examples/jsm/renderers/CSS2DRenderer.js", "e900e58dbc428d8d3211f87f3ff0301e514f6dc563655c710bcda15c96397feb"),
}
THREEJS_LICENSE_SHA256 = "4c40a1ef62450b857c3b2aaf294936304cd552d965fbcd9d32d4c5bcf4ba4454"

# ── Assembly colors ───────────────────────────────────────────────────────
ASSEMBLY_COLORS = {
    "rear_trunk": "#4488ff",
    "under_chassis": "#ff8844",
    "shared": "#88cc88",
}

# ── BC marker colors ─────────────────────────────────────────────────────
BC_COLORS = {
    "fixed": "#cc0000",
    "bolted": "#880000",
    "free": "#999999",
    "load": "#cc66ff",
}


def compute_arc_midpoint(
    nodes: Dict[int, Node3D], member: Member3D
) -> Tuple[float, float, float]:
    """Compute arc midpoint for a bend member using sag formula.

    Reuses geometry from freecad_frame_builder.py lines 84-101:
    sag = R - sqrt(R^2 - (chord/2)^2), applied perpendicular to chord.
    """
    n0 = nodes[member.start_node]
    n1 = nodes[member.end_node]

    mid_x = (n0.x + n1.x) / 2
    mid_y = (n0.y + n1.y) / 2
    mid_z = (n0.z + n1.z) / 2

    dx, dy, dz = n1.x - n0.x, n1.y - n0.y, n1.z - n0.z
    chord_len = math.sqrt(dx * dx + dy * dy + dz * dz)

    if chord_len < 1e-6:
        return (mid_x, mid_y, mid_z)

    R = member.bend_clr
    if chord_len > 2 * R:
        return (mid_x, mid_y, mid_z)

    half_chord = chord_len / 2
    sag = R - math.sqrt(max(R * R - half_chord * half_chord, 0))

    # Perpendicular direction: use Z-up reference, project out chord component
    chord_dir = (dx / chord_len, dy / chord_len, dz / chord_len)
    up = (0.0, 0.0, 1.0)
    dot = chord_dir[0] * up[0] + chord_dir[1] * up[1] + chord_dir[2] * up[2]
    perp = (up[0] - chord_dir[0] * dot, up[1] - chord_dir[1] * dot, up[2] - chord_dir[2] * dot)
    perp_len = math.sqrt(perp[0] ** 2 + perp[1] ** 2 + perp[2] ** 2)

    if perp_len < 1e-6:
        # Chord is vertical, use Y as reference
        up = (0.0, 1.0, 0.0)
        dot = chord_dir[0] * up[0] + chord_dir[1] * up[1] + chord_dir[2] * up[2]
        perp = (up[0] - chord_dir[0] * dot, up[1] - chord_dir[1] * dot, up[2] - chord_dir[2] * dot)
        perp_len = math.sqrt(perp[0] ** 2 + perp[1] ** 2 + perp[2] ** 2)

    if perp_len < 1e-6:
        return (mid_x, mid_y, mid_z)

    perp = (perp[0] / perp_len, perp[1] / perp_len, perp[2] / perp_len)

    return (
        mid_x + perp[0] * sag,
        mid_y + perp[1] * sag,
        mid_z + perp[2] * sag,
    )


def _arc_points(nodes, member, apex):
    """Sample the minor circular arc through the endpoints and sag apex."""
    a, b = nodes[member.start_node], nodes[member.end_node]
    start, end = (a.x, a.y, a.z), (b.x, b.y, b.z)
    mid = [(x + y) / 2 for x, y in zip(start, end)]
    sag = math.dist(mid, apex)
    if sag == 0:
        return None  # Preserve compute_arc_midpoint's straight-tube fallbacks.
    normal = [(y - x) / sag for x, y in zip(mid, apex)]
    chord = member.length(nodes)
    tangent = [(y - x) / chord for x, y in zip(start, end)]
    radius = member.bend_clr
    centre = [x - (radius - sag) * n for x, n in zip(mid, normal)]
    half_angle = math.atan2(chord / 2, radius - sag)
    points = [[c + radius * (math.cos(t) * n + math.sin(t) * u)
               for c, n, u in zip(centre, normal, tangent)]
              for t in (half_angle * (i / 8 - 1) for i in range(17))]
    points[0], points[-1] = list(start), list(end)
    return points


def frame_to_json(geo: FrameGeometry3D) -> dict:
    """Serialize FrameGeometry3D to a JSON-compatible dict."""
    data: dict = {"nodes": {}, "members": [], "connections": []}

    data["origin_node"] = next((nid for nid, n in geo.nodes.items()
                                if all(abs(v) <= 1e-9 for v in (n.x, n.y, n.z))), None)

    for nid, node in geo.nodes.items():
        data["nodes"][str(nid)] = {
            "x": node.x, "y": node.y, "z": node.z,
            "label": node.label, "assembly": node.assembly,
        }

    for m in geo.members:
        entry = {
            "id": m.id, "start": m.start_node, "end": m.end_node,
            "label": m.label, "assembly": m.assembly,
            "is_bend": m.is_bend, "bend_clr": m.bend_clr,
            "od": m.section.get("OD", 1.75),
        }
        entry["chord_in"] = round(m.length(geo.nodes), 3)
        entry["dim_text"] = f'{entry["chord_in"]:.3f} in'
        a, b = geo.nodes[m.start_node], geo.nodes[m.end_node]
        entry["dim_pos"] = [(getattr(a, k) + getattr(b, k)) / 2 for k in ("x", "y", "z")]
        if m.is_bend:
            entry["arc_mid"] = list(compute_arc_midpoint(geo.nodes, m))
            entry["dim_pos"] = entry["arc_mid"]
            entry["dim_text"] = f'chord {entry["dim_text"]}, R {m.bend_clr:.3f} in'
            points = _arc_points(geo.nodes, m, entry["arc_mid"])
            if points is not None:
                entry["arc_points"] = points
        data["members"].append(entry)

    for c in geo.connections:
        data["connections"].append({
            "node_id": c.node_id, "conn_type": c.conn_type,
            "bc_type": c.bc_type, "description": c.description,
        })

    return data


def _inline_threejs(threejs_dir):
    """Validate all four package files before returning inline module sources."""
    sources = {}
    for key, (name, expected) in {**THREEJS_FILES, "LICENSE": ("LICENSE", THREEJS_LICENSE_SHA256)}.items():
        try:
            content = (Path(threejs_dir) / name).read_bytes()
        except OSError as exc:
            raise ValueError(f"{name}: expected SHA-256 {expected}; actual missing/unreadable") from exc
        actual = hashlib.sha256(content).hexdigest()
        if actual != expected:
            raise ValueError(f"{name}: expected SHA-256 {expected}; actual {actual}")
        sources[key] = (content.decode("utf-8") if key == "LICENSE" else
                        "data:text/javascript;base64," + base64.b64encode(content).decode("ascii"))
    return sources


def generate_viewer_html(geo: FrameGeometry3D, output_path: str, threejs_dir=None) -> str:
    """Generate viewer HTML; inline verified three.js when its package is supplied."""
    data = frame_to_json(geo)
    json_payload = json.dumps(data, indent=2)

    html = _HTML_TEMPLATE.replace("/* {{FRAME_DATA}} */", json_payload)
    sources = (_inline_threejs(threejs_dir) if threejs_dir is not None else
               {"THREEJS_CDN": THREEJS_CDN, "ORBIT_CDN": THREEJS_ORBIT_CDN, "CSS2D_CDN": THREEJS_CSS2D_CDN})
    for key, source in sources.items():
        html = html.replace("{{" + key + "}}", source)
    notice = ('<script type="text/plain" id="threejs-license">' + sources["LICENSE"] + '</script>'
              if threejs_dir is not None else "")
    html = html.replace("{{THREEJS_LICENSE}}", notice)
    html = html.replace("{{ASSEMBLY_COLORS}}", json.dumps(ASSEMBLY_COLORS))
    html = html.replace("{{BC_COLORS}}", json.dumps(BC_COLORS))

    Path(output_path).parent.mkdir(parents=True, exist_ok=True)
    Path(output_path).write_text(html, encoding="utf-8")
    return output_path


# ── HTML Template ─────────────────────────────────────────────────────────
_HTML_TEMPLATE = r"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>GT1R Parachute Frame — 3D Viewer</title>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body { overflow: hidden; background: #1a1a2e; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
  canvas { display: block; }
  #controls {
    position: absolute; top: 12px; left: 12px; background: rgba(20,20,40,0.92);
    color: #ccc; padding: 14px 16px; border-radius: 8px; font-size: 13px;
    max-width: 220px; z-index: 10; border: 1px solid rgba(255,255,255,0.08);
  }
  #controls h3 { color: #fff; margin-bottom: 8px; font-size: 14px; }
  #controls label { display: block; margin: 4px 0; cursor: pointer; }
  #controls label:hover { color: #fff; }
  #controls input[type=checkbox] { margin-right: 6px; accent-color: #4488ff; }
  .section-title { color: #888; font-size: 11px; text-transform: uppercase;
    letter-spacing: 0.5px; margin-top: 10px; margin-bottom: 4px; }
  #info {
    position: absolute; bottom: 12px; left: 12px; color: rgba(255,255,255,0.4);
    font-size: 11px; z-index: 10;
  }
  .color-swatch { display: inline-block; width: 10px; height: 10px;
    border-radius: 2px; margin-right: 4px; vertical-align: middle; }
  .dim-label { color: #ddd; font-size: 10px; background: rgba(0,0,0,0.65); padding: 2px 4px; white-space: nowrap; }
  .node-label {
    color: #fff; font-size: 11px; background: rgba(0,0,0,0.6);
    padding: 1px 5px; border-radius: 3px; pointer-events: none;
    white-space: nowrap;
  }
</style>
</head>
<body>

<div id="controls">
  <h3>GT1R Frame Viewer</h3>

  <div class="section-title">Assemblies</div>
  <label><input type="checkbox" id="tog-rear_trunk" checked>
    <span class="color-swatch" style="background:#4488ff"></span>Rear Trunk</label>
  <label><input type="checkbox" id="tog-under_chassis" checked>
    <span class="color-swatch" style="background:#ff8844"></span>Under-Chassis</label>
  <label><input type="checkbox" id="tog-shared" checked>
    <span class="color-swatch" style="background:#88cc88"></span>Shared</label>

  <div class="section-title">Display</div>
  <label><input type="checkbox" id="tog-labels" checked> Node Labels</label>
  <label><input type="checkbox" id="tog-dimensions" checked> Dimensions</label>
  <label><input type="checkbox" id="tog-connections" checked> BC Markers</label>
  <label><input type="checkbox" id="tog-grid" checked> Grid</label>
  <small>Red square: fixed | dark red triangle: bolted | gold diamond: origin | purple diamond: load</small>
</div>

<div id="info">Orbit: left-click drag | Zoom: scroll | Pan: right-click drag</div>

{{THREEJS_LICENSE}}
<script type="importmap">
{
  "imports": {
    "three": "{{THREEJS_CDN}}",
    "three/addons/controls/OrbitControls.js": "{{ORBIT_CDN}}",
    "three/addons/renderers/CSS2DRenderer.js": "{{CSS2D_CDN}}"
  }
}
</script>

<script type="module">
import * as THREE from 'three';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';
import { CSS2DRenderer, CSS2DObject } from 'three/addons/renderers/CSS2DRenderer.js';

// ── Data ──────────────────────────────────────────────────────────────
const FRAME = /* {{FRAME_DATA}} */;
const COLORS = {{ASSEMBLY_COLORS}};
const BC_COLORS = {{BC_COLORS}};

// ── Scene setup ───────────────────────────────────────────────────────
const scene = new THREE.Scene();
scene.background = new THREE.Color(0x1a1a2e);

const camera = new THREE.PerspectiveCamera(50, window.innerWidth / window.innerHeight, 0.1, 500);
camera.position.set(30, -40, 25);
camera.up.set(0, 0, 1);

const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setSize(window.innerWidth, window.innerHeight);
renderer.setPixelRatio(window.devicePixelRatio);
document.body.appendChild(renderer.domElement);

const labelRenderer = new CSS2DRenderer();
labelRenderer.setSize(window.innerWidth, window.innerHeight);
labelRenderer.domElement.style.position = 'absolute';
labelRenderer.domElement.style.top = '0';
labelRenderer.domElement.style.pointerEvents = 'none';
document.body.appendChild(labelRenderer.domElement);

const controls = new OrbitControls(camera, renderer.domElement);
controls.target.set(0, 5, 0);
controls.enableDamping = true;
controls.dampingFactor = 0.08;

// ── Lights ────────────────────────────────────────────────────────────
scene.add(new THREE.AmbientLight(0xffffff, 0.5));
const dirLight = new THREE.DirectionalLight(0xffffff, 0.8);
dirLight.position.set(20, -30, 40);
scene.add(dirLight);
const dirLight2 = new THREE.DirectionalLight(0xffffff, 0.3);
dirLight2.position.set(-20, 30, -10);
scene.add(dirLight2);

// ── Grid ──────────────────────────────────────────────────────────────
const grid = new THREE.GridHelper(80, 40, 0x333355, 0x222244);
grid.rotation.x = Math.PI / 2; // XY plane (Z-up)
scene.add(grid);

// ── Groups for toggle control ─────────────────────────────────────────
const groups = {
  rear_trunk: new THREE.Group(),
  under_chassis: new THREE.Group(),
  shared: new THREE.Group(),
  labels: new THREE.Group(),
  dimensions: new THREE.Group(),
  connections: new THREE.Group(),
};
Object.values(groups).forEach(g => scene.add(g));

// ── Helper: orient cylinder between two points ───────────────────────
function makeTube(p1, p2, radius, color) {
  const dir = new THREE.Vector3().subVectors(p2, p1);
  const len = dir.length();
  if (len < 0.001) return null;

  const geo = new THREE.CylinderGeometry(radius, radius, len, 12, 1, true);
  const mat = new THREE.MeshPhongMaterial({ color, side: THREE.DoubleSide });
  const mesh = new THREE.Mesh(geo, mat);

  const mid = new THREE.Vector3().addVectors(p1, p2).multiplyScalar(0.5);
  mesh.position.copy(mid);

  const axis = new THREE.Vector3(0, 1, 0);
  const normedDir = dir.clone().normalize();
  const quat = new THREE.Quaternion().setFromUnitVectors(axis, normedDir);
  mesh.quaternion.copy(quat);

  return mesh;
}

// ── Helper: bent tube swept through circular-arc samples ──────────────
function makeBentTube(points, radius, color) {
  const curve = new THREE.CatmullRomCurve3(points.map(p => new THREE.Vector3(...p)));
  const geo = new THREE.TubeGeometry(curve, 20, radius, 12, false);
  const mat = new THREE.MeshPhongMaterial({ color, side: THREE.DoubleSide });
  return new THREE.Mesh(geo, mat);
}

// ── Build members ─────────────────────────────────────────────────────
for (const m of FRAME.members) {
  const n0 = FRAME.nodes[m.start];
  const n1 = FRAME.nodes[m.end];
  const p1 = new THREE.Vector3(n0.x, n0.y, n0.z);
  const p2 = new THREE.Vector3(n1.x, n1.y, n1.z);
  const color = COLORS[m.assembly] || '#aaaaaa';
  const radius = (m.od || 1.75) / 2;

  let mesh;
  if (m.is_bend && m.arc_points) {
    mesh = makeBentTube(m.arc_points, radius, color);
  } else {
    mesh = makeTube(p1, p2, radius, color);
  }
  if (mesh) {
    const group = groups[m.assembly] || groups.shared;
    group.add(mesh);
  }
}

// ── Build dimension labels ───────────────────────────────────────────
for (const m of FRAME.members) {
  const div = document.createElement('div');
  div.className = 'dim-label';
  div.textContent = m.dim_text;
  const label = new CSS2DObject(div);
  label.position.set(...m.dim_pos);
  groups.dimensions.add(label);
}

// ── Build node labels ─────────────────────────────────────────────────
for (const [nid, node] of Object.entries(FRAME.nodes)) {
  const div = document.createElement('div');
  div.className = 'node-label';
  div.textContent = `N${nid}`;
  div.title = node.label;

  const label = new CSS2DObject(div);
  label.position.set(node.x, node.y, node.z + 1.2);
  groups.labels.add(label);

  // Small sphere at node
  const sphere = new THREE.Mesh(
    new THREE.SphereGeometry(0.3, 8, 8),
    new THREE.MeshPhongMaterial({ color: COLORS[node.assembly] || '#aaaaaa' })
  );
  sphere.position.set(node.x, node.y, node.z);
  const asmGroup = groups[node.assembly] || groups.shared;
  asmGroup.add(sphere);
}

// ── Build BC markers ──────────────────────────────────────────────────
for (const c of FRAME.connections) {
  const node = FRAME.nodes[c.node_id];
  const pos = new THREE.Vector3(node.x, node.y, node.z);
  let geo, color;

  if (c.bc_type === 'fixed') {
    geo = new THREE.BoxGeometry(1.4, 1.4, 1.4);
    color = BC_COLORS.fixed;
  } else if (c.bc_type === 'bolted') {
    geo = new THREE.ConeGeometry(0.8, 1.6, 3);
    color = BC_COLORS.bolted;
  } else if (c.conn_type === 'coupler_pin') {
    geo = new THREE.OctahedronGeometry(0.9);
    color = BC_COLORS.load;
  } else {
    continue; // skip free connections without special markers
  }

  const mat = new THREE.MeshPhongMaterial({ color, transparent: true, opacity: 0.85 });
  const mesh = new THREE.Mesh(geo, mat);
  mesh.position.copy(pos);
  groups.connections.add(mesh);
}

if (FRAME.origin_node !== null) {
  const node = FRAME.nodes[FRAME.origin_node];
  const marker = new THREE.Mesh(new THREE.OctahedronGeometry(0.9),
    new THREE.MeshPhongMaterial({ color: '#daa520', transparent: true, opacity: 0.85 }));
  marker.position.set(node.x, node.y, node.z);
  groups.connections.add(marker);
}

// ── Axes helper ───────────────────────────────────────────────────────
const axes = new THREE.AxesHelper(5);
scene.add(axes);

// ── Toggle controls ───────────────────────────────────────────────────
function bindToggle(id, target) {
  const el = document.getElementById(id);
  if (!el) return;
  el.addEventListener('change', () => { target.visible = el.checked; });
}
bindToggle('tog-rear_trunk', groups.rear_trunk);
bindToggle('tog-under_chassis', groups.under_chassis);
bindToggle('tog-shared', groups.shared);
bindToggle('tog-labels', groups.labels);
bindToggle('tog-dimensions', groups.dimensions);
bindToggle('tog-connections', groups.connections);
document.getElementById('tog-grid')?.addEventListener('change', function() {
  grid.visible = this.checked;
});

Object.defineProperty(window, 'frameViewer', { value: Object.freeze({ groups, FRAME }) });

// ── Resize ────────────────────────────────────────────────────────────
window.addEventListener('resize', () => {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
  labelRenderer.setSize(window.innerWidth, window.innerHeight);
});

// ── Animate ───────────────────────────────────────────────────────────
function animate() {
  requestAnimationFrame(animate);
  controls.update();
  renderer.render(scene, camera);
  labelRenderer.render(scene, camera);
}
animate();
</script>
</body>
</html>
"""


def main():
    parser = argparse.ArgumentParser(
        description="Generate 3D interactive frame viewer HTML"
    )
    parser.add_argument(
        "--output", "-o",
        default="outputs/frame_viewer_3d.html",
        help="Output HTML file path (default: outputs/frame_viewer_3d.html)",
    )
    parser.add_argument("--threejs-dir", default=os.environ.get("DIGITALMODEL_THREEJS_DIR"),
                        help="Unpacked three@0.167.0 package for self-contained output")
    args = parser.parse_args()

    geo = build_gt1r_frame_3d()
    generate_viewer_html(geo, args.output, threejs_dir=args.threejs_dir)
    mode = "self-contained" if args.threejs_dir else "loads three.js from the CDN"
    print(f"Viewer written ({mode}).")


if __name__ == "__main__":
    main()
