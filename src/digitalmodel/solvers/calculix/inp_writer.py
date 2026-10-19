#!/usr/bin/env python3
"""
ABOUTME: CalculiX INP file writer — converts mesh data (nodes + elements) into
Abaqus-format .inp files for CalculiX ccx solver.
"""

import re
from numbers import Integral
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
}

# CalculiX beam element types
BEAM_ELEMENT_TYPES = {"B31", "B32"}


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
        self._solid_sections: list[dict] = []
        self._boundary_conditions: list[dict] = []
        self._loads: list[dict] = []
        self._steps: list[dict] = []

    def add_material(
        self, name: str, youngs_modulus: float, poissons_ratio: float
    ):
        """Add a material definition."""
        self._materials.append({
            "name": name,
            "E": youngs_modulus,
            "nu": poissons_ratio,
        })

    def add_node_set(self, name: str, node_ids: list[int]):
        """Define a named node set. node_ids are 0-based."""
        self._node_sets.append({"name": name, "ids": node_ids})

    def add_element_set(self, name: str, element_ids: list[int]):
        """Define a named element set using global zero-based element indices.

        Indices follow element-block insertion order, then connectivity row
        order within each block, including beams. EALL and generated beam-set
        names are reserved. Names use ASCII letters, digits and underscores,
        start with a letter, and are compared without case distinctions.
        """
        self._element_sets.append({"name": name, "ids": element_ids})

    def add_solid_section(self, elset: str, material: str):
        """Assign a material to a solid element set, checked during write.

        Explicit sections must cover every solid exactly once, without beam
        members. Without explicit sections, a single defined material applies
        to EALL for compatibility. Multiple materials require explicit sections.
        EALL contains solids only. References are case insensitive.

        Source: CalculiX CrunchiX User's Manual 2.22 (2024), sections 7.53
        (*ELSET) and 7.118 (*SOLID SECTION).
        """
        self._solid_sections.append({"elset": elset, "material": material})

    def add_boundary_condition(
        self, node_set: str, dof: int, value: float = 0.0
    ):
        """Add a boundary condition on a named node set."""
        self._boundary_conditions.append({
            "node_set": node_set, "dof": dof, "value": value,
        })

    def add_load(
        self,
        element_set: str,
        load_type: str,
        magnitude: float,
        direction: Optional[tuple] = None,
    ):
        """Add a load definition."""
        self._loads.append({
            "set": element_set,
            "type": load_type,
            "magnitude": magnitude,
            "direction": direction,
        })

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

    def add_step(self, step_type: str = "static", description: str = ""):
        """Add an analysis step."""
        self._steps.append({"type": step_type, "description": description})

    # ------------------------------------------------------------------
    # Write
    # ------------------------------------------------------------------

    def write(self, filepath: Path) -> Path:
        """Write the .inp file and return its path."""
        # Validate before touching the destination: unsupported physics must
        # never silently disappear from an otherwise valid-looking deck.
        for name in self.elements:
            if name not in ELEMENT_TYPE_MAP:
                raise ValueError(f"Unsupported element type: {name!r}")
        solid_sections = self._resolve_solid_sections()
        filepath = Path(filepath)
        lines: list[str] = []

        self._write_heading(lines)
        self._write_nodes(lines)
        self._write_elements(lines)
        self._write_node_sets(lines)
        self._write_element_sets(lines)
        self._write_materials(lines)
        self._write_solid_sections(lines, solid_sections)
        self._write_beam_sections(lines)
        self._write_steps(lines)

        filepath.write_text("\n".join(lines) + "\n")
        return filepath

    # ------------------------------------------------------------------
    # Private section writers
    # ------------------------------------------------------------------

    @staticmethod
    def _identity(name: str, kind: str) -> str:
        # CalculiX 2.22 (2024), section 7: names are case insensitive and at
        # most 80 characters. This adapter accepts a narrower ASCII subset
        # to prevent whitespace/delimiters from changing keyword identity.
        if (
            not isinstance(name, str)
            or len(name) > 80
            or re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", name) is None
        ):
            raise ValueError(f"Invalid {kind} name: {name!r}")
        return name.upper()

    def _resolve_solid_sections(self) -> list[dict]:
        """Resolve set identities and require a disjoint, complete partition."""
        materials = {}
        for mat in self._materials:
            key = self._identity(mat["name"], "material")
            if key in materials:
                raise ValueError(f"Duplicate material name: {mat['name']!r}")
            materials[key] = mat["name"]

        # Same traversal as _write_elements: beams participate in global IDs.
        element_sets = {"EALL": set()}
        element_count = 0
        for name, data in self.elements.items():
            ccx_type = ELEMENT_TYPE_MAP[name]
            key = f"EBEAM_{ccx_type}" if ccx_type in BEAM_ELEMENT_TYPES else "EALL"
            count = len(data["connectivity"])
            element_sets.setdefault(key, set()).update(
                range(element_count, element_count + count)
            )
            element_count += count

        reserved = {"EALL"} | {f"EBEAM_{t}" for t in BEAM_ELEMENT_TYPES}
        set_names = {key: key for key in element_sets}
        for region in self._element_sets:
            key = self._identity(region["name"], "element set")
            if key in reserved:
                raise ValueError(f"Reserved element set name: {region['name']!r}")
            if key in element_sets:
                raise ValueError(f"Duplicate element set name: {region['name']!r}")
            for eid in region["ids"]:
                if (
                    isinstance(eid, (bool, np.bool_))
                    or not isinstance(eid, Integral)
                    or not 0 <= eid < element_count
                ):
                    raise ValueError(
                        f"Invalid element ID in set {region['name']!r}: {eid!r}"
                    )
            element_sets[key] = set(region["ids"])
            set_names[key] = region["name"]

        solid_ids = element_sets["EALL"]
        if not self._solid_sections:
            if not solid_ids:
                return []
            if len(materials) != 1:
                raise ValueError("Every solid requires an unambiguous material assignment")
            return [{"elset": "EALL", "material": next(iter(materials.values()))}]

        assigned = set()
        sections = []
        for section in self._solid_sections:
            key = self._identity(section["elset"], "element set")
            material = self._identity(section["material"], "material")
            if key not in element_sets:
                raise ValueError(f"Unknown element set: {section['elset']!r}")
            if material not in materials:
                raise ValueError(f"Unknown material: {section['material']!r}")
            ids = element_sets[key]
            if not ids:
                raise ValueError(
                    f"Solid section references an empty set: {section['elset']!r}"
                )
            if ids - solid_ids:
                raise ValueError(
                    f"Solid section contains non-solid elements: {section['elset']!r}"
                )
            if assigned & ids:
                raise ValueError("Elements assigned to multiple solid sections")
            assigned.update(ids)
            sections.append({"elset": set_names[key], "material": materials[material]})
        if assigned != solid_ids:
            raise ValueError("Unassigned solid elements require explicit material sections")
        return sections

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
            ccx_type = ELEMENT_TYPE_MAP[gmsh_name]
            if ccx_type in BEAM_ELEMENT_TYPES:
                elset = f"EBEAM_{ccx_type}"
            else:
                elset = "EALL"
            lines.append(f"*ELEMENT, TYPE={ccx_type}, ELSET={elset}")
            for row in data["connectivity"]:
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

    def _write_solid_sections(self, lines: list[str], sections: list[dict]):
        for section in sections:
            lines.append(
                f"*SOLID SECTION, ELSET={section['elset']}, "
                f"MATERIAL={section['material']}"
            )

    def _write_beam_sections(self, lines: list[str]):
        for bs in self._beam_sections:
            lines.append(
                f"*BEAM SECTION, ELSET={bs['elset']}, "
                f"MATERIAL={bs['material']}, SECTION={bs['section_type']}"
            )
            lines.append(", ".join(str(d) for d in bs["dimensions"]))
            lines.append(", ".join(str(v) for v in bs["direction"]))

    def _write_steps(self, lines: list[str]):
        for step in self._steps:
            lines.append("*STEP")
            lines.append(f"*{step['type'].upper()}")
            self._write_boundary_conditions(lines)
            self._write_loads(lines)
            lines.append("*NODE FILE")
            lines.append("U")
            lines.append("*EL FILE")
            lines.append("S")
            lines.append("*END STEP")

        # If no steps defined but we have BCs/loads, create a default step
        if not self._steps and (self._boundary_conditions or self._loads):
            lines.append("*STEP")
            lines.append("*STATIC")
            self._write_boundary_conditions(lines)
            self._write_loads(lines)
            lines.append("*NODE FILE")
            lines.append("U")
            lines.append("*EL FILE")
            lines.append("S")
            lines.append("*END STEP")

    def _write_boundary_conditions(self, lines: list[str]):
        if not self._boundary_conditions:
            return
        lines.append("*BOUNDARY")
        for bc in self._boundary_conditions:
            lines.append(
                f"{bc['node_set']}, {bc['dof']}, {bc['dof']}, {bc['value']}"
            )

    def _write_loads(self, lines: list[str]):
        for load in self._loads:
            if load["type"].lower() == "cload":
                lines.append("*CLOAD")
                direction = load.get("direction") or (1, 0, 0)
                for dof_idx, component in enumerate(direction, start=1):
                    if abs(component) > 1e-15:
                        mag = load["magnitude"] * component
                        lines.append(f"{load['set']}, {dof_idx}, {mag}")
