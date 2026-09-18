"""Scientific plots of retained model geometry and unaveraged native stresses."""
import json
import math
from pathlib import Path

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.collections import PolyCollection
from mpl_toolkits.mplot3d.art3d import Poly3DCollection

from .example_vessel_presol import read_presol
from .example_vessel_fea_results import invariants


def peak_by_element(points):
    peaks = {}
    for row in points:
        identity = row['element_id']
        value = invariants(row)['von_mises_mpa']
        peaks[identity] = max(peaks.get(identity, 0), value)
    return peaks


def _model(path):
    model = json.loads((Path(path)/'model.json').read_text(encoding='utf-8'))
    solid = 'radial_layers' in model['basis']
    source = 'stress_nodes.txt' if solid else 'stress_mid_nodes.txt'
    points = read_presol(Path(path)/source, model, kind='solid' if solid else 'shell')
    if not solid:
        points = points['top']+points['bottom']+points['mid']
    return model, solid, peak_by_element(points)


def plot_damage(path, destination, *, centre_x, centre_theta, half_x, half_arc):
    model, solid, peaks = _model(path)
    nodes = {n[0]: n[1:] for n in model['nodes']}
    polygons, stresses, walls = [], [], []
    column_peaks = {}
    for e in model['elements']:
        column = e.get('column_id', e['element_id'])
        column_peaks[column] = max(column_peaks.get(column, 0), peaks[e['element_id']])
    for e in model['elements']:
        if solid and e['radial_layer'] != model['basis']['radial_layers']-1:
            continue
        ids = e['nodes'][4:] if solid else e['nodes']
        coords = [nodes[n] for n in ids]
        x = sum(n[0] for n in coords)/4
        arc = ((e['theta_rad']-centre_theta+math.pi)%(2*math.pi)-math.pi)*1000
        if abs(x-centre_x) > half_x+150 or abs(arc) > half_arc+150:
            continue
        angles = [((math.atan2(n[2], n[1])-centre_theta+math.pi)%(2*math.pi)-math.pi)*1000 for n in coords]
        polygons.append([(n[0]-centre_x, s) for n, s in zip(coords, angles)])
        stresses.append(column_peaks[e.get('column_id', e['element_id'])])
        walls.append(sum(math.hypot(n[1],n[2])-1000 for n in coords)/4 if solid else e['thickness_mm'])
    fig, axes = plt.subplots(1, 2, figsize=(12, 5.4), constrained_layout=True)
    for ax, values, title in zip(axes, (walls, stresses), ('Assessed thickness (mm)', 'Element-node peak von Mises (MPa)')):
        mesh = PolyCollection(polygons, array=values, cmap='viridis' if values is walls else 'inferno',
                              edgecolors='#44444455', linewidths=.2)
        mesh.set_clim(0, 16 if values is walls else max(180, math.ceil(max(stresses)/100)*100))
        ax.add_collection(mesh)
        ax.autoscale_view()
        ax.set_aspect('equal')
        ax.set(xlabel='Axial distance from patch centre (mm)', ylabel='Bore arc distance (mm)', title=title)
        fig.colorbar(mesh, ax=ax, shrink=.8)
    fig.suptitle(f"{model['basis']['case']} — actual {('SOLID185' if solid else 'SHELL181')} model, "
                 f"{model['basis']['pressure_mpa']:.3f} MPa; local mesh {model['basis']['local_pitch_mm']:g} mm")
    fig.savefig(destination, dpi=150)
    plt.close(fig)


def plot_mesh(path, destination):
    model = json.loads((Path(path)/'model.json').read_text(encoding='utf-8'))
    solid = 'radial_layers' in model['basis']
    nodes = {n[0]: n[1:] for n in model['nodes']}
    polygons, walls = [], []
    for e in model['elements']:
        if solid and e['radial_layer'] != model['basis']['radial_layers']-1:
            continue
        ids = e['nodes'][4:] if solid else e['nodes']
        coordinates = [nodes[n] for n in ids]
        polygons.append(coordinates)
        walls.append(sum(math.hypot(n[1],n[2])-1000 for n in coordinates)/4 if solid else e['thickness_mm'])
    fig = plt.figure(figsize=(11, 5.5))
    ax = fig.add_axes([0.01, 0.04, .85, .9], projection='3d')
    mesh = Poly3DCollection(polygons, array=walls, cmap='viridis', edgecolor='#22222244', linewidth=.15)
    ax.add_collection3d(mesh)
    ax.set(xlim=(0,6000), ylim=(-1100,1100), zlim=(-1100,1100), xlabel='Axial X (mm)',
           ylabel='Y (mm)', zlabel='Z (mm)', title=f"Actual FE mesh: {len(model['elements']):,} elements / {len(nodes):,} nodes")
    ax.set_box_aspect((6,2.2,2.2))
    ax.set_yticks([-1000, 0, 1000])
    ax.set_zticks([-1000, 0, 1000])
    ax.view_init(elev=25, azim=-65)
    fig.colorbar(mesh, cax=fig.add_axes([.89,.2,.025,.6]), label='Assessed wall (mm)')
    fig.savefig(destination, dpi=150)
    plt.close(fig)
