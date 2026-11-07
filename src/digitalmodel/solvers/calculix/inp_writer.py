#!/usr/bin/env python3
"""
ABOUTME: CalculiX INP file writer — converts mesh data (nodes + elements) into
Abaqus-format .inp files for CalculiX ccx solver.
"""

import math
import re
from pathlib import Path
from typing import Optional

import numpy as np

# Element type mapping: gmsh name -> CalculiX type
ELEMENT_TYPE_MAP = {
    "Tetrahedron 4": "C3D4",
    "Tetrahedron 10": "C3D10",
    "Hexahedron 8": "C3D8",
    "Hexahedron 20": "C3D20",
    "Wedge 6": "C3D6",
    "Wedge 15": "C3D15",
    "Line 2": "B31",
    "Line 3": "B32",
    # CalculiX 2.22 manual (2024), sections 6.2 and 7.117.
    "Quadrilateral 4": "S4",
    "Quadrilateral 8": "S8R",
}

# CalculiX beam element types
BEAM_ELEMENT_TYPES = {"B31", "B32"}
SHELL_ELEMENT_TYPES = {"S4", "S8R"}


def _name(value: str) -> str:
    """Reject input-deck delimiters in symbolic identifiers."""
    if not re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", value):
        raise ValueError("Names must start with a letter and contain letters, digits or underscores")
    return value


def _finite(value: float) -> float:
    value = float(value)
    if not math.isfinite(value):
        raise ValueError("Numeric input must be finite")
    return value


class INPWriter:
    """
    Write CalculiX-compatible INP files from mesh data.

    Mesh data uses 0-based indexing (gmsh convention). This writer converts
    all IDs to 1-based when writing the INP file (CalculiX convention).
    """

    def __init__(self, nodes: np.ndarray, elements: dict):
        self.nodes = nodes
        self.elements = elements
        self._materials: list[dict] = []
        self._node_sets: list[dict] = []
        self._element_sets: list[dict] = []
        self._beam_sections: list[dict] = []
        self._shell_sections: list[dict] = []
        self._boundary_conditions: list[dict] = []
        self._loads: list[dict] = []
        self._steps: list[dict] = []

    def add_material(
        self, name: str, youngs_modulus: float, poissons_ratio: float
    ):
        """Add a material definition."""
        self._materials.append({
            "name": _name(name),
            "E": youngs_modulus,
            "nu": poissons_ratio,
        })

    def add_plasticity(self, material: str, rows):
        """Add (true stress, equivalent plastic strain) pairs.

        CalculiX 2.22 (2024), section 7.105: first strain is zero; stresses
        use the same units as E. Accepts TrueStressStrainCurve.plastic_table()
        output from the existing FFS material library; no allowables are inferred.
        """
        curve = [tuple(map(_finite, row)) for row in rows]
        if not curve or any(len(row) != 2 for row in curve):
            raise ValueError("Plastic rows require stress and plastic strain")
        if curve[0][1] != 0 or any(s <= 0 or e < 0 for s, e in curve):
            raise ValueError("Plastic curve must start at zero strain with positive stress")
        if any(b[1] <= a[1] or b[0] < a[0] for a, b in zip(curve, curve[1:])):
            raise ValueError("Plastic strain must increase and stress must not decrease")
        for mat in self._materials:
            if mat["name"] == material:
                mat["plastic"] = curve
                return
        raise ValueError("Plasticity requires an existing material")

    def add_shell_section(self, elset: str, material: str, thickness: float):
        """Homogeneous shell thickness; CalculiX 2.22 (2024), section 7.117."""
        thickness = _finite(thickness)
        if thickness <= 0:
            raise ValueError("Shell thickness must be positive")
        self._shell_sections.append({"elset": _name(elset),
                                     "material": _name(material), "thickness": thickness})

    def add_node_set(self, name: str, node_ids: list[int]):
        """Define a named node set. node_ids are 0-based."""
        self._node_sets.append({"name": _name(name), "ids": node_ids})

    def add_element_set(self, name: str, element_ids: list[int]):
        """Define a named element set. element_ids are 0-based."""
        self._element_sets.append({"name": _name(name), "ids": element_ids})

    def add_boundary_condition(
        self, node_set: str, dof: int, value: float = 0.0
    ):
        """Add a boundary condition on a named node set."""
        if isinstance(dof, bool) or dof not in range(1, 7):
            raise ValueError("Structural degree of freedom must be from 1 through 6")
        target = self._steps[-1]["bcs"] if self._steps else self._boundary_conditions
        target.append({
            "node_set": _name(node_set), "dof": dof, "value": _finite(value),
        })

    def add_load(
        self,
        element_set: str,
        load_type: str,
        magnitude: float,
        direction: Optional[tuple] = None,
    ):
        """Add a load definition."""
        if load_type.lower() != "cload":
            raise ValueError("Use add_pressure for distributed loading; only cload is supported here")
        if direction is not None:
            direction = tuple(map(_finite, direction))
            if len(direction) != 3:
                raise ValueError("Concentrated load direction requires three components")
        target = self._steps[-1]["loads"] if self._steps else self._loads
        target.append({
            "set": _name(element_set),
            "type": "cload",
            "magnitude": _finite(magnitude),
            "direction": direction,
        })

    def add_pressure(self, element_set: str, magnitude: float, face: str = "P"):
        """DLOAD pressure; shell P or solid P1..P6, per 2.22 section 7.43.

        Shell P acts along the positive element normal for positive magnitude;
        negative magnitude reverses it. Connectivity defines that normal.
        Solid pressures act on the selected numbered face.
        """
        if not re.fullmatch(r"P[1-6]?", face):
            raise ValueError("Pressure face must be P or P1 through P6")
        target = self._steps[-1]["loads"] if self._steps else self._loads
        target.append({"set": _name(element_set), "type": "pressure",
                       "magnitude": _finite(magnitude), "face": face})

    def add_hydrostatic_pressure(self, element_elevations: dict[int, float], *,
                                 density: float, gravity: float,
                                 surface_elevation: float, face: str = "P"):
        """Piecewise constant centroid pressure from dp/dz=-rho*g.

        Density, gravity and vertical elevations are required consistent-unit
        inputs. Pressure is zero above the free surface; mesh refinement is
        required at the wet/dry boundary. Not an exact facewise linear load.
        DLOAD representation: CalculiX 2.22 (2024), section 7.43.
        For shell P the non-negative pressure acts along the positive element
        normal, representing fluid on the negative-normal side. Orient shell
        connectivity accordingly. For the opposite direction, reverse the
        connectivity or use add_pressure with a negative magnitude; this helper
        does not choose the wetted side from elevation alone.
        """
        density, gravity, surface_elevation = map(_finite, (density, gravity, surface_elevation))
        if density <= 0 or gravity <= 0 or not re.fullmatch(r"P[1-6]?", face):
            raise ValueError("Positive density and gravity and a valid pressure face are required")
        loads = []
        total = sum(len(data["connectivity"]) for name, data in self.elements.items()
                    if name in ELEMENT_TYPE_MAP)
        for eid, elevation in element_elevations.items():
            if (not isinstance(eid, (int, np.integer)) or isinstance(eid, bool)
                    or eid < 0 or eid >= total):
                raise ValueError("Element IDs must index the exported mesh")
            pressure = _finite(density * gravity * max(surface_elevation - _finite(elevation), 0))
            loads.append({"set": str(eid + 1), "type": "pressure",
                          "magnitude": pressure, "face": face})
        target = self._steps[-1]["loads"] if self._steps else self._loads
        target.extend(loads)

    def add_beam_section(
        self,
        elset: str,
        material: str,
        section_type: str,
        dimensions: tuple,
        direction: tuple = (0.0, 0.0, 1.0),
    ):
        """Add a beam section definition.

        Args:
            elset: Element set name for the beam elements.
            material: Material name (must match an add_material call).
            section_type: Cross-section shape — RECT, CIRC, PIPE, etc.
            dimensions: Section dimensions tuple (depends on section_type).
                RECT: (width, height), CIRC: (radius,), PIPE: (outer_r, thickness).
            direction: Normal direction vector for the beam cross-section.
        """
        self._beam_sections.append({
            "elset": elset,
            "material": material,
            "section_type": section_type.upper(),
            "dimensions": dimensions,
            "direction": direction,
        })

    def add_step(self, step_type: str = "static", description: str = "", *,
                 nlgeom: bool = False, increments: tuple | None = None,
                 modes: int | None = None, max_increments: int | None = None,
                 perturbation: bool = False):
        """Start a step; subsequent BCs and loads belong to this step only.

        Pending BCs/loads before the first step move into that step. CalculiX
        itself retains static loads/BCs between steps (2.22 sections 7.5, 7.122,
        7.124); no OP=NEW reset is implied. Section 7.124 (*STEP), NLGEOM
        clause: geometric nonlinearity persists in later static steps until
        NLGEOM=NO. A static nlgeom=False writes that reset after an enabled
        step, including across an intervening buckling step. Buckling does not
        reset the static setting; its reference state follows PERTURBATION.
        """
        kind = step_type.lower()
        if kind in {"riks", "arc_length"}:
            raise NotImplementedError("Riks arc-length is not documented in CalculiX 2.22; collapse NOT_EVALUATED")
        if kind not in {"static", "buckle"}:
            raise ValueError("Supported steps are static and buckle")
        if kind == "buckle" and (type(modes) is not int or modes <= 0):
            raise ValueError("Buckling requires a positive integer mode count")
        if kind == "buckle" and (nlgeom or increments or max_increments):
            raise ValueError("Static controls cannot be applied to a buckling step")
        if kind == "static" and (modes is not None or perturbation):
            raise ValueError("Modes and perturbation are buckling-only controls")
        if max_increments is not None and (type(max_increments) is not int or max_increments <= 0):
            raise ValueError("Maximum increments must be a positive integer")
        if increments is not None:
            increments = tuple(map(_finite, increments))
            if len(increments) != 4 or not 0 < increments[2] <= increments[0] <= increments[3] <= increments[1]:
                raise ValueError("Require 0 < minimum <= initial <= maximum <= period")
        self._steps.append({"type": kind, "description": description,
                            "nlgeom": nlgeom, "increments": increments, "modes": modes,
                            "max_increments": max_increments, "perturbation": perturbation,
                            "bcs": self._boundary_conditions, "loads": self._loads})
        self._boundary_conditions, self._loads = [], []

    # ------------------------------------------------------------------
    # Write
    # ------------------------------------------------------------------

    def write(self, filepath: Path) -> Path:
        """Write the .inp file and return its path."""
        filepath = Path(filepath)
        self._validate_shell_sections()
        lines: list[str] = []

        self._write_heading(lines)
        self._write_nodes(lines)
        self._write_elements(lines)
        self._write_node_sets(lines)
        self._write_element_sets(lines)
        self._write_materials(lines)
        self._write_solid_sections(lines)
        self._write_beam_sections(lines)
        self._write_shell_sections(lines)
        self._write_steps(lines)

        filepath.write_text("\n".join(lines) + "\n")
        return filepath

    # ------------------------------------------------------------------
    # Private section writers
    # ------------------------------------------------------------------

    def _write_heading(self, lines: list[str]):
        lines.append("*HEADING")
        lines.append("CalculiX analysis generated by digitalmodel")

    def _write_nodes(self, lines: list[str]):
        lines.append("*NODE")
        for i, (x, y, z) in enumerate(self.nodes):
            lines.append(f"{i + 1}, {x:.8E}, {y:.8E}, {z:.8E}")

    def _write_elements(self, lines: list[str]):
        elem_id = 1
        for gmsh_name, data in self.elements.items():
            ccx_type = ELEMENT_TYPE_MAP.get(gmsh_name)
            if ccx_type is None:
                continue
            if ccx_type in BEAM_ELEMENT_TYPES:
                elset = f"EBEAM_{ccx_type}"
            elif ccx_type in SHELL_ELEMENT_TYPES:
                elset = f"ESHELL_{ccx_type}"
            else:
                elset = "EALL"
            lines.append(f"*ELEMENT, TYPE={ccx_type}, ELSET={elset}")
            for row in data["connectivity"]:
                if ccx_type == "B32":
                    # Gmsh manual, Node ordering: Line3 is end/end/middle.
                    # CalculiX 2.22, section 6.2.33, Fig. 73: end/middle/end.
                    row = [row[0], row[2], row[1]]
                node_refs = ", ".join(str(int(n) + 1) for n in row)
                lines.append(f"{elem_id}, {node_refs}")
                elem_id += 1

    def _write_node_sets(self, lines: list[str]):
        for ns in self._node_sets:
            lines.append(f"*NSET, NSET={ns['name']}")
            ids_1based = [str(nid + 1) for nid in ns["ids"]]
            for i in range(0, len(ids_1based), 16):
                lines.append(", ".join(ids_1based[i:i + 16]))

    def _write_element_sets(self, lines: list[str]):
        for es in self._element_sets:
            lines.append(f"*ELSET, ELSET={es['name']}")
            ids_1based = [str(eid + 1) for eid in es["ids"]]
            for i in range(0, len(ids_1based), 16):
                lines.append(", ".join(ids_1based[i:i + 16]))

    def _write_materials(self, lines: list[str]):
        for mat in self._materials:
            lines.append(f"*MATERIAL, NAME={mat['name']}")
            lines.append("*ELASTIC")
            lines.append(f"{mat['E']}, {mat['nu']}")
            if "plastic" in mat:
                lines.append("*PLASTIC")
                lines.extend(f"{stress}, {strain}" for stress, strain in mat["plastic"])

    def _write_solid_sections(self, lines: list[str]):
        has_solid = any(
            ELEMENT_TYPE_MAP.get(name) not in BEAM_ELEMENT_TYPES | SHELL_ELEMENT_TYPES
            for name in self.elements
            if ELEMENT_TYPE_MAP.get(name) is not None
        )
        if self._materials and has_solid:
            mat_name = self._materials[0]["name"]
            lines.append(f"*SOLID SECTION, ELSET=EALL, MATERIAL={mat_name}")

    def _write_beam_sections(self, lines: list[str]):
        for bs in self._beam_sections:
            lines.append(
                f"*BEAM SECTION, ELSET={bs['elset']}, "
                f"MATERIAL={bs['material']}, SECTION={bs['section_type']}"
            )
            lines.append(", ".join(str(d) for d in bs["dimensions"]))
            lines.append(", ".join(str(v) for v in bs["direction"]))

    def _write_shell_sections(self, lines):
        for section in self._shell_sections:
            lines.append(f"*SHELL SECTION, ELSET={section['elset']}, MATERIAL={section['material']}")
            lines.append(str(section["thickness"]))

    def _validate_shell_sections(self):
        """Require exactly one shell section per shell element; reject mixed sets."""
        groups, shell_ids, kinds, eid = {}, set(), {}, 0
        for name, data in self.elements.items():
            kind = ELEMENT_TYPE_MAP.get(name)
            if kind is None:
                continue
            ids = set(range(eid, eid + len(data["connectivity"])))
            kinds.update(dict.fromkeys(ids, kind))
            if kind in SHELL_ELEMENT_TYPES:
                groups[f"ESHELL_{kind}"] = ids
                shell_ids.update(ids)
            else:
                group = f"EBEAM_{kind}" if kind in BEAM_ELEMENT_TYPES else "EALL"
                groups.setdefault(group, set()).update(ids)
            eid += len(ids)
        for group in self._element_sets:
            groups.setdefault(group["name"], set()).update(group["ids"])
        assigned = set()
        for section in self._shell_sections:
            ids = groups.get(section["elset"], set())
            if not ids or not ids <= shell_ids or assigned & ids:
                raise ValueError("Shell section requires a nonempty, disjoint shell-only set")
            if section["material"] not in {m["name"] for m in self._materials}:
                raise ValueError("Shell section material is undefined")
            assigned.update(ids)
        if assigned != shell_ids:
            raise ValueError("Every shell element requires a shell section")
        self._validate_pressures(groups, shell_ids, kinds)

    def _validate_pressures(self, groups, shell_ids, kinds):
        loads = self._loads + [load for step in self._steps for load in step["loads"]]
        for load in loads:
            if load["type"] != "pressure":
                continue
            ids = ({int(load["set"]) - 1} if load["set"].isdigit()
                   else groups.get(load["set"], set()))
            if not ids or not ids <= kinds.keys() or (ids & shell_ids and not ids <= shell_ids):
                raise ValueError("Pressure requires a nonempty, unmixed element set")
            shell = ids <= shell_ids
            if shell != (load["face"] == "P"):
                raise ValueError("Pressure requires P for shells and a numbered solid face")
            if not shell:
                # Face counts follow CalculiX 2.22 (2024), section 7.43.
                faces = {"C3D4": 4, "C3D10": 4, "C3D6": 5, "C3D15": 5,
                         "C3D8": 6, "C3D20": 6}
                if any(int(load["face"][1:]) > faces.get(kinds[i], 0) for i in ids):
                    raise ValueError("Pressure face is not supported by the selected elements")

    def _write_steps(self, lines: list[str]):
        if not self._steps and (self._boundary_conditions or self._loads):
            self.add_step()
        nlgeom_active = False
        for step in self._steps:
            options = []
            if step["nlgeom"]:
                options.append("NLGEOM")
                nlgeom_active = True
            elif step["type"] == "static" and nlgeom_active:
                options.append("NLGEOM=NO")
                nlgeom_active = False
            if step["perturbation"]:
                options.append("PERTURBATION")
            if step["max_increments"] is not None:
                options.append(f"INC={step['max_increments']}")
            lines.append("*STEP" + (", " + ", ".join(options) if options else ""))
            lines.append(f"*{step['type'].upper()}")
            if step["type"] == "buckle":
                lines.append(str(step["modes"]))
            elif step["increments"] is not None:
                lines.append(", ".join(map(str, step["increments"])))
            self._write_boundary_conditions(lines, step["bcs"])
            self._write_loads(lines, step["loads"])
            lines.append("*NODE FILE")
            lines.append("U")
            lines.append("*EL FILE")
            lines.append("S, E, PEEQ" if step["type"] == "static" else "S")
            lines.append("*END STEP")

    def _write_boundary_conditions(self, lines: list[str], conditions):
        if not conditions:
            return
        lines.append("*BOUNDARY")
        for bc in conditions:
            lines.append(
                f"{bc['node_set']}, {bc['dof']}, {bc['dof']}, {bc['value']}"
            )

    def _write_loads(self, lines: list[str], loads):
        for load in loads:
            if load["type"] == "pressure":
                lines.extend(["*DLOAD", f"{load['set']}, {load['face']}, {load['magnitude']}"])
            if load["type"].lower() == "cload":
                lines.append("*CLOAD")
                direction = load.get("direction") or (1, 0, 0)
                for dof_idx, component in enumerate(direction, start=1):
                    if component != 0:
                        mag = load["magnitude"] * component
                        lines.append(f"{load['set']}, {dof_idx}, {mag}")
