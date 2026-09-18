"""Original pressure-only SHELL181 example; no solver launch or code verdict."""
from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path
import re

from . import example_vessel_data as data


def _axis(length, centre, half_extent, local_pitch, far_pitch, anchors=()):
    cuts = {0.0, length, *anchors}
    if centre is not None:
        cuts.update(max(0.0, min(length, centre + d)) for d in
                    (-half_extent-150, -half_extent, 0, half_extent, half_extent+150))
    cuts = sorted(cuts)
    result = [0.0]
    for left, right in zip(cuts, cuts[1:]):
        local = centre is not None and abs((left+right)/2-centre) <= half_extent+150
        quotient = (right-left)/(local_pitch if local else far_pitch)
        if not math.isfinite(quotient) or quotient > 200000:
            raise ValueError("Requested axis exceeds resource bound")
        count = math.ceil(quotient)
        if len(result)+count > 200000:
            raise ValueError("Requested axis exceeds resource bound")
        result.extend(left+(right-left)*i/count for i in range(1,count+1))
    return result


def _mesh_axes(case, local_pitch, far_pitch):
    area = next((a for a in data.AREAS if a.area_id == ("D" if case == "repair" else case)), None)
    circumference = 2 * math.pi * data.RADIUS_MM
    x = _axis(6000, area.centre_x_mm if area else None,
              area.axial_extent_mm/2 if area else 0, local_pitch, far_pitch)
    arc = _axis(circumference, math.radians(area.theta_deg)*data.RADIUS_MM if area else None,
                area.circumferential_extent_mm/2 if area else 0, local_pitch, far_pitch,
                anchors=(circumference/3, 2*circumference/3))
    return area, x, arc


def _elements(case, area, xs, arcs):
    count = len(arcs)-1
    elements = []
    sound = data.NOMINAL_MM-data.UNCERTAINTY_MM-data.FUTURE_LOSS_MM
    for i in range(len(xs)-1):
        for j in range(count):
            x, s = (xs[i]+xs[i+1])/2, (arcs[j]+arcs[j+1])/2
            thickness = sound
            if area and case != "repair":
                local_s = s-math.radians(area.theta_deg)*data.RADIUS_MM
                thickness = data.thickness(area,x-area.centre_x_mm,local_s)-0.7
            nodes = (i*count+j+1, i*count+(j+1)%count+1,
                     (i+1)*count+(j+1)%count+1, (i+1)*count+j+1)
            elements.append(dict(element_id=len(elements)+1, nodes=nodes,
                                 x_mm=x, theta_rad=s/data.RADIUS_MM,
                                 thickness_mm=round(thickness,9)))
    return elements


def _end_forces(xs, arcs, pressure):
    count, circumference = len(arcs)-1, arcs[-1]
    force = pressure * math.pi * data.RADIUS_MM**2
    weights = [((arcs[j+1]-arcs[j])+(arcs[j]-arcs[j-1] if j else arcs[-1]-arcs[-2]))
               / (2*circumference) for j in range(count)]
    return dict(left=[(j+1,-force*w) for j,w in enumerate(weights)],
                right=[((len(xs)-1)*count+j+1,force*w) for j,w in enumerate(weights)])


def _end_moments(nodes,forces):
    coordinates = {n[0]:n[1:] for n in nodes}
    inner = data.RADIUS_MM
    outer = inner+data.NOMINAL_MM-data.UNCERTAINTY_MM-data.FUTURE_LOSS_MM
    offset = 2/3*(outer**3-inner**3)/(outer**2-inner**2)-inner
    return {end:[(node,force*offset*coordinates[node][2]/inner,
                       -force*offset*coordinates[node][1]/inner) for node,force in rows]
            for end,rows in forces.items()}


def build_model(case, pressure, *, local_pitch=25.0, far_pitch=50.0):
    if case not in ("intact","A","B","C","D","repair"):
        raise ValueError("Unknown example configuration")
    if any(not math.isfinite(v) or v <= 0 for v in (pressure,local_pitch,far_pitch)):
        raise ValueError("Finite positive pressure and mesh pitches required")
    area,xs,arcs = _mesh_axes(case,local_pitch,far_pitch)
    if len(xs)*(len(arcs)-1) > 150000:
        raise ValueError("Example mesh exceeds 150000 node resource bound")
    nodes = [(i*(len(arcs)-1)+j+1,x,data.RADIUS_MM*math.cos(s/data.RADIUS_MM),
              data.RADIUS_MM*math.sin(s/data.RADIUS_MM))
             for i,x in enumerate(xs) for j,s in enumerate(arcs[:-1])]
    indices = [min(range(len(arcs)-1),key=lambda j:abs(arcs[j]-k*arcs[-1]/3)) for k in range(3)]
    references = [(j+1,arcs[j]/data.RADIUS_MM) for j in indices]
    basis = dict(case=case,pressure_mpa=pressure,local_pitch_mm=local_pitch,
                 far_pitch_mm=far_pitch,material=data._material_basis(),
                 active_constitutive_model="linear elastic; no plasticity activated",
                 load_scope="uniform internal pressure and closed-end thrust only; no heads/nozzles/supports",
                 end_thrust_basis="P*pi*Ri^2 with exact annular traction-centroid offset moments about bore nodes",
                 thermal_scope="isothermal; no imposed thermal strain or thermal restraint",
                 geometry_scope="full cylinder; one independent example defect; assessed external thickness",
                 repair="ideal flush full-penetration insert; sound assessed wall; identical parent/weld properties",
                 repair_extent_mm=[1100,1000] if case == "repair" else None,
                 physical_repair_qualification="NOT EVALUATED",code_acceptance="NOT EVALUATED",
                 coordinate_units="mm",force_units="N",stress_units="MPa",
                 coordinate_mapping="axis=X; Y=R*cos(theta), Z=R*sin(theta); crown=+Y; increasing theta clockwise viewed along+X",
                 element_reference="https://ansyshelp.ansys.com/public/Views/Secured/corp/v261/en/ans_elem/Hlp_E_SHELL181.html")
    forces = _end_forces(xs,arcs,pressure)
    return dict(basis=basis,nodes=nodes,elements=_elements(case,area,xs,arcs),
                reference_nodes=references,end_forces=forces,end_moments=_end_moments(nodes,forces))


def _geometry_commands(model):
    lines = []
    for node,x,y,z in model["nodes"]:
        lines.append(f"N,{node},{x:.12g},{y:.12g},{z:.12g}")
    sections = {}
    for element in model["elements"]:
        thickness = element["thickness_mm"]
        if thickness not in sections:
            section = len(sections)+1
            sections[thickness] = section
            lines.extend((f"SECTYPE,{section},SHELL",f"SECDATA,{thickness:.12g},1,0,5","SECOFFSET,BOT"))
        lines.extend((f"SECNUM,{sections[thickness]}","E,"+','.join(map(str,element["nodes"]))))
    return lines


def _load_commands(model):
    lines = [f"SFE,ALL,1,PRES,,{model['basis']['pressure_mpa']:.12g}"]
    for forces in model["end_forces"].values():
        lines.extend(f"F,{node},FX,{force:.12g}" for node,force in forces)
    for moments in model["end_moments"].values():
        for node,my,mz in moments:
            lines.extend((f"F,{node},MY,{my:.12g}",f"F,{node},MZ,{mz:.12g}"))
    for node,angle in model["reference_nodes"]:
        lines.append(f"D,{node},UX,0")
        sine,cosine = math.sin(angle),math.cos(angle)
        if abs(sine) < 1e-12:
            lines.append(f"D,{node},UZ,0")
        else:
            lines.append(f"CE,NEXT,0,{node},UY,{-sine:.12g},{node},UZ,{cosine:.12g}")
    return lines


def _stress_exports(count):
    lines = [f"*VLEN,{count}",f"*DIM,EIDS,ARRAY,{count}","*VFILL,EIDS(1),RAMP,1,1"]
    for i in range(1,7):
        lines.append(f"*DIM,T{i},ARRAY,{count}")
    for surface,filename in (("MID","mid"),("TOP","top"),("BOT","bottom")):
        lines.extend((f"SHELL,{surface}","ETABLE,ERAS",f"*VLEN,{count}"))
        if surface == "MID":
            lines.extend(("/FORMAT,8,E,22,14","/OUTPUT,stress_mid_nodes,txt","PRESOL,S,COMP","/OUTPUT"))
        for i,component in enumerate(("X","Y","Z","XY","YZ","XZ"),1):
            lines.extend((f"ETABLE,V{i},S,{component}",f"*VGET,T{i}(1),ELEM,1,ETAB,V{i}"))
        lines.extend((f"*CFOPEN,stress_{filename},csv","*VLEN,1","*VWRITE,'element_id,sx,sy,sz,sxy,syz,sxz'",
                      "%C",f"*VLEN,{count}","*VWRITE,EIDS(1),T1(1),T2(1),T3(1),T4(1),T5(1),T6(1)",
                      "(F12.0,6(',',E22.14))","*CFCLOS"))
    return lines


def _displacement_exports(model):
    count = len(model["nodes"])
    lines = [f"*VLEN,{count}",f"*DIM,NIDS,ARRAY,{count}","*VFILL,NIDS(1),RAMP,1,1"]
    for i,component in enumerate(("X","Y","Z"),1):
        lines.extend((f"*DIM,U{i},ARRAY,{count}",f"*VGET,U{i}(1),NODE,1,U,{component}"))
    lines.extend(("*CFOPEN,displacements,csv","*VLEN,1","*VWRITE,'node_id,ux,uy,uz'","%C",f"*VLEN,{count}",
                  "*VWRITE,NIDS(1),U1(1),U2(1),U3(1)","(F12.0,3(',',E22.14))","*CFCLOS"))
    lines.extend(("*CFOPEN,reactions,csv","*VLEN,1","*VWRITE,'node_id,fx,fy,fz'","%C"))
    for node,_ in model["reference_nodes"]:
        for label,component in (("RX","FX"),("RY","FY"),("RZ","FZ")):
            lines.append(f"*GET,{label},NODE,{node},RF,{component}")
        lines.extend(("*VLEN,1",f"*VWRITE,{node},RX,RY,RZ","(F12.0,3(',',E22.14))"))
    return lines+["*CFCLOS"]


def render_deck(model):
    material = model["basis"]["material"]
    commands = ["/BATCH","/FILNAME,vessel,1","/PREP7","ET,1,SHELL181","KEYOPT,1,3,2",
                "KEYOPT,1,8,2","KEYOPT,1,10,1",f"MP,EX,1,{material['elastic_modulus_mpa']}",
                f"MP,PRXY,1,{material['poisson_ratio']}","TYPE,1","MAT,1"]
    commands += _geometry_commands(model)+_load_commands(model)
    commands += ["ALLSEL,ALL","FINISH","/SOLU","ANTYPE,STATIC","NLGEOM,OFF","NROPT,UNSYM",
                 "OUTRES,ALL,ALL","SOLVE","FINISH","/POST1","SET,LAST","RSYS,SOLU"]
    commands += _stress_exports(len(model["elements"]))+_displacement_exports(model)
    commands += ["FINISH","/COM,VSL_DONE","/EXIT,NOSAVE"]
    return '\n'.join(commands)+'\n'


def write_case(parent, name, case, pressure, **mesh):
    if not re.fullmatch(r"[a-z0-9][a-z0-9-]*",name):
        raise ValueError("Case directory must be one lowercase path segment")
    parent = Path(parent).resolve(strict=True)
    target = parent/name
    if target.exists():
        raise FileExistsError(target)
    model = build_model(case,pressure,**mesh)
    payloads = {"vessel.inp":render_deck(model).encode("ascii"),
                "model.json":json.dumps(model,sort_keys=True,indent=2,allow_nan=False).encode("utf-8")}
    manifest = dict(files={name:hashlib.sha256(raw).hexdigest() for name,raw in payloads.items()},
                    generator_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                    geometry_generator_sha256=hashlib.sha256(Path(data.__file__).read_bytes()).hexdigest(),
                    status="PREPARED; NO SOLVER RUN; NO CODE ACCEPTANCE")
    payloads["input-manifest.json"] = json.dumps(manifest,sort_keys=True,indent=2).encode("utf-8")
    target.mkdir()
    for name,raw in payloads.items():
        with (target/name).open("xb") as stream:
            stream.write(raw)
    return target
