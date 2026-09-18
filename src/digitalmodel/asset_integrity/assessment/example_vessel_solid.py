"""SOLID185 linear example cross-check; no solver execution or FFS acceptance."""
from __future__ import annotations

import hashlib
import json
import math
import re
from pathlib import Path

from . import example_vessel_data as data
from . import example_vessel_fea as shell

ELEMENT_REFERENCE = 'https://ansyshelp.ansys.com/public/Views/Secured/corp/v261/en/ans_elem/Hlp_E_SOLID185.html'


def _wall(case, area, x, arc):
    if area is None or case == 'repair':
        return data.NOMINAL_MM-data.UNCERTAINTY_MM-data.FUTURE_LOSS_MM
    local_s = arc-math.radians(area.theta_deg)*data.RADIUS_MM
    return data.thickness(area,x-area.centre_x_mm,local_s)-data.UNCERTAINTY_MM-data.FUTURE_LOSS_MM


def _geometry(case, area, xs, arcs, layers):
    count = len(arcs)-1
    def node_id(i,j,k):
        return (i*count+j%count)*(layers+1)+k+1
    nodes, elements = [], []
    for i,x in enumerate(xs):
        for j,s in enumerate(arcs[:-1]):
            wall, theta = _wall(case,area,x,s), s/data.RADIUS_MM
            for k in range(layers+1):
                radius = data.RADIUS_MM+wall*k/layers
                nodes.append((node_id(i,j,k),x,radius*math.cos(theta),radius*math.sin(theta)))
    for i in range(len(xs)-1):
        for j in range(count):
            for k in range(layers):
                corners = ((i,j),(i,j+1),(i+1,j+1),(i+1,j))
                ids = [node_id(a,b,k+r) for r in (0,1) for a,b in corners]
                xyz = [sum(nodes[n-1][q] for n in ids)/8 for q in (1,2,3)]
                elements.append(dict(element_id=len(elements)+1,nodes=ids,centroid_mm=xyz,
                                     column_id=i*count+j+1,radial_layer=k,
                                     theta_rad=(arcs[j]+arcs[j+1])/2/data.RADIUS_MM,
                                     radial_fraction=(k+.5)/layers))
    return nodes,elements,node_id


def _balance_weights(weights, nodes):
    """Remove parasitic end moments caused by nonuniform angular faceting."""
    total = sum(weights.values())
    weights = {n:w/total for n,w in weights.items()}
    ys = {n:nodes[n-1][2] for n in weights}
    zs = {n:nodes[n-1][3] for n in weights}
    my = sum(weights[n]*ys[n] for n in weights)
    mz = sum(weights[n]*zs[n] for n in weights)
    yy = sum(weights[n]*(ys[n]-my)**2 for n in weights)
    zz = sum(weights[n]*(zs[n]-mz)**2 for n in weights)
    yz = sum(weights[n]*(ys[n]-my)*(zs[n]-mz) for n in weights)
    determinant = yy*zz-yz*yz
    a,b = (-my*zz+mz*yz)/determinant,(-mz*yy+my*yz)/determinant
    corrected = {n:w*(1+a*(ys[n]-my)+b*(zs[n]-mz)) for n,w in weights.items()}
    if min(corrected.values()) <= 0:
        raise ValueError('Moment-balancing produced nonpositive end weights')
    return corrected


def _thrust(xs, arcs, layers, node_id, pressure, nodes):
    """Consistent bilinear end-face radial weights, normalized to circular bore thrust."""
    wall = data.NOMINAL_MM-data.UNCERTAINTY_MM-data.FUTURE_LOSS_MM
    result = {}
    for label,i,sign in (('left',0,-1),('right',len(xs)-1,1)):
        weights = {}
        for j in range(len(arcs)-1):
            sine = math.sin((arcs[j+1]-arcs[j])/data.RADIUS_MM)
            for k in range(layers):
                lo,hi = data.RADIUS_MM+wall*k/layers,data.RADIUS_MM+wall*(k+1)/layers
                for level,coefficient in ((k,2*lo+hi),(k+1,lo+2*hi)):
                    weight = sine*(hi-lo)*coefficient/12
                    for angular in (j,j+1):
                        nid = node_id(i,angular,level)
                        weights[nid] = weights.get(nid,0)+weight
        weights = _balance_weights(weights,nodes)
        thrust = pressure*math.pi*data.RADIUS_MM**2
        result[label] = [(nid,sign*thrust*w) for nid,w in sorted(weights.items())]
    return result


def build_model(case, pressure, *, local_pitch=25., far_pitch=100., radial_layers=3):
    if case not in ('intact','C','D','repair'):
        raise ValueError('Solid example case must be intact, C, D or repair')
    if type(radial_layers) is not int or not 1 <= radial_layers <= 12:
        raise ValueError('Radial layers must be an integer from 1 through 12')
    if any(not math.isfinite(v) or v <= 0 for v in (pressure,local_pitch,far_pitch)):
        raise ValueError('Pressure and pitches must be finite positive values')
    if far_pitch > 1000 or local_pitch > 1000:
        raise ValueError('Angular faceting too coarse for this example')
    area,xs,arcs = shell._mesh_axes(case,local_pitch,far_pitch)
    if len(xs)*(len(arcs)-1)*(radial_layers+1) > 200000:
        raise ValueError('Solid example exceeds 200000 node bound')
    nodes,elements,node_id = _geometry(case,area,xs,arcs,radial_layers)
    refs = [min(range(len(arcs)-1),key=lambda j:abs(arcs[j]-k*arcs[-1]/3)) for k in range(3)]
    basis = dict(case=case,pressure_mpa=pressure,local_pitch_mm=local_pitch,far_pitch_mm=far_pitch,
                 radial_layers=radial_layers,material=data._material_basis(),element_reference=ELEMENT_REFERENCE,
                 assumptions='full cylinder; external smooth loss; closed-end thrust; pressure only; no heads',
                 coordinate_mapping='axis X; Y=r*cos(theta); Z=r*sin(theta)',
                 repair='ideal flush insert at sound assessed thickness; weld/HAZ qualification not evaluated',
                 geometry='trilinear faceted hexahedra; constant 1000 mm inner-node radius',
                 code_acceptance='NOT EVALUATED',solver_execution='NOT PERFORMED',
                 stress_output='global Cartesian ETABLE element-averaged stress assigned to element centroid',
                 stress_limitation='not exact point stress; through-wall quadrature requires radial convergence',
                 end_thrust='annular tributary weights; normalize P*pi*R^2 and balance zero end moments',
                 pressure_face='SOLID185 face1 J-I-L-K; positive pressure into material, radially outward')
    return dict(basis=basis,nodes=nodes,elements=elements,
                reference_nodes=[(node_id(0,j,0),arcs[j]/data.RADIUS_MM) for j in refs],
                end_forces=_thrust(xs,arcs,radial_layers,node_id,pressure,nodes))


def _loads(model):
    pressure = model['basis']['pressure_mpa']
    lines = [f"SFE,{e['element_id']},1,PRES,,{pressure:.12g}"
             for e in model['elements'] if e['radial_layer'] == 0]
    for forces in model['end_forces'].values():
        lines.extend(f'F,{nid},FX,{force:.12g}' for nid,force in forces)
    for nid,angle in model['reference_nodes']:
        lines.append(f'D,{nid},UX,0')
        sine,cosine = math.sin(angle),math.cos(angle)
        if abs(sine) < 1e-12:
            lines.append(f'D,{nid},UZ,0')
        else:
            lines.append(f'CE,NEXT,0,{nid},UY,{-sine:.12g},{nid},UZ,{cosine:.12g}')
    return lines


def _stress_exports(count):
    lines = ['ETABLE,ERAS',f'*DIM,EIDS,ARRAY,{count}','*VFILL,EIDS(1),RAMP,1,1']
    for i,component in enumerate(('X','Y','Z','XY','YZ','XZ'),1):
        lines += [f'ETABLE,S{i},S,{component}',f'*DIM,V{i},ARRAY,{count}',
                  f'*VGET,V{i}(1),ELEM,1,ETAB,S{i}']
    for i,component in enumerate(('X','Y','Z'),7):
        lines += [f'ETABLE,S{i},CENT,{component}',f'*DIM,V{i},ARRAY,{count}',
                  f'*VGET,V{i}(1),ELEM,1,ETAB,S{i}']
    lines += ['*CFOPEN,stress_solid,csv','*VLEN,1',
              "*VWRITE,'element_id,sx,sy,sz,sxy,syz,sxz,x,y,z'",'%C',f'*VLEN,{count}',
              '*VWRITE,EIDS(1),V1(1),V2(1),V3(1),V4(1),V5(1),V6(1),V7(1),V8(1),V9(1)',
              "(F12.0,9(',',E22.14))",'*CFCLOS']
    return lines


def render_deck(model):
    material = model['basis']['material']
    lines = ['/BATCH','/FILNAME,vessel,1','/PREP7','ET,1,SOLID185','KEYOPT,1,2,2',
             f"MP,EX,1,{material['elastic_modulus_mpa']}",f"MP,PRXY,1,{material['poisson_ratio']}",
             'TYPE,1','MAT,1']
    lines += [f'N,{nid},{x:.12g},{y:.12g},{z:.12g}' for nid,x,y,z in model['nodes']]
    lines += ['E,'+','.join(map(str,e['nodes'])) for e in model['elements']]
    lines += _loads(model)
    lines += ['ALLSEL,ALL','FINISH','/SOLU','ANTYPE,STATIC','NLGEOM,OFF',
              'OUTRES,ALL,ALL','SOLVE','FINISH','/POST1','SET,LAST','RSYS,0']
    lines += ['/FORMAT,12,E,22,14','/OUTPUT,stress_nodes,txt','PRESOL,S,COMP','/OUTPUT']
    lines += _stress_exports(len(model['elements']))+shell._displacement_exports(model)
    lines = ['%C' if line == '(A)' else line for line in lines]
    return '\n'.join(lines+['FINISH','/COM,VSL_SOLID_DONE','/EXIT,NOSAVE'])+'\n'


def write_case(parent, name, case, pressure, **mesh):
    if not re.fullmatch(r'[a-z0-9][a-z0-9-]*',name):
        raise ValueError('Output name must be one lowercase path segment')
    parent = Path(parent).absolute()
    for ancestor in (parent,*parent.parents):
        if ancestor.is_symlink() or getattr(ancestor,'is_junction',lambda:False)():
            raise ValueError('Output ancestry must not contain links or junctions')
    parent = parent.resolve(strict=True)
    target = parent/name
    if target.exists():
        raise FileExistsError(target)
    model = build_model(case,pressure,**mesh)
    payloads = {'vessel.inp':render_deck(model).encode('ascii'),
                'model.json':json.dumps(model,sort_keys=True,indent=2,allow_nan=False).encode()}
    sources = [Path(__file__),Path(data.__file__),Path(shell.__file__)]
    manifest = dict(files={n:hashlib.sha256(b).hexdigest() for n,b in payloads.items()},
                    generators={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in sources},
                    status='PREPARED; NO SOLVER RUN; NO CODE ACCEPTANCE')
    payloads['input-manifest.json'] = json.dumps(manifest,sort_keys=True,indent=2).encode()
    target.mkdir()
    for name,raw in payloads.items():
        with (target/name).open('xb') as stream:
            stream.write(raw)
    return target
