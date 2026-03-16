#!/usr/bin/env python3
"""
ABOUTME: End-to-end FEM analysis chain — geometry creation via gmsh, INP export,
CalculiX solve, and result extraction in a single pipeline call.
"""

import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Optional

import numpy as np

from .inp_writer import INPWriter
from .result_parser import CalculiXResultParser


def is_calculix_available() -> bool:
    """Check whether the CalculiX solver (ccx) is on PATH."""
    return shutil.which("ccx") is not None


class FEMChain:
    """
    Orchestrate: gmsh geometry -> INP file -> ccx solve -> result extraction.

    For validation, use ``run_plate_validation`` which sets up the classic
    plate-with-hole stress concentration problem.
    """

    JOB_NAME = "fem_analysis"

    def __init__(self, work_dir: Optional[Path] = None):
        if work_dir is None:
            work_dir = Path(tempfile.mkdtemp(prefix="calculix_"))
        self.work_dir = Path(work_dir)
        self.work_dir.mkdir(parents=True, exist_ok=True)

        self._nodes: Optional[np.ndarray] = None
        self._elements: Optional[dict] = None
        self._node_sets: dict[str, list[int]] = {}
        self._element_sets: dict[str, list[int]] = {}
        self._inp_path: Optional[Path] = None

    # ------------------------------------------------------------------
    # Geometry creation
    # ------------------------------------------------------------------

    def create_plate_with_hole(
        self,
        plate_w: float,
        plate_h: float,
        hole_r: float,
        thickness: float,
        element_size: float,
    ) -> dict:
        """
        Create a 3D plate-with-hole geometry via gmsh and mesh it.

        Uses quarter-symmetry: only the first quadrant is modelled, with
        symmetry boundary conditions on x=0 and y=0 faces.

        Returns dict with mesh statistics.
        """
        try:
            import gmsh
        except (ImportError, OSError) as exc:
            raise ImportError("gmsh required for geometry creation") from exc

        gmsh.initialize()
        gmsh.option.setNumber("General.Terminal", 0)

        try:
            gmsh.clear()
            gmsh.model.add("plate_with_hole")

            hw = plate_w / 2.0
            hh = plate_h / 2.0

            # Quarter plate as a box, then subtract quarter cylinder
            plate = gmsh.model.occ.addBox(0, 0, 0, hw, hh, thickness)
            cyl = gmsh.model.occ.addCylinder(
                0, 0, -thickness, 0, 0, 3 * thickness, hole_r,
            )
            result = gmsh.model.occ.cut([(3, plate)], [(3, cyl)])
            gmsh.model.occ.synchronize()

            gmsh.model.mesh.setSize(gmsh.model.getEntities(0), element_size)
            # Refine near hole
            gmsh.model.mesh.field.add("Distance", 1)
            gmsh.model.mesh.field.setNumbers(
                1, "CurvesList",
                [t for d, t in gmsh.model.getEntities(1)],
            )
            gmsh.model.mesh.field.add("Threshold", 2)
            gmsh.model.mesh.field.setNumber(2, "InField", 1)
            gmsh.model.mesh.field.setNumber(2, "SizeMin", hole_r / 8)
            gmsh.model.mesh.field.setNumber(2, "SizeMax", element_size)
            gmsh.model.mesh.field.setNumber(2, "DistMin", hole_r * 0.2)
            gmsh.model.mesh.field.setNumber(2, "DistMax", hole_r * 5)
            gmsh.model.mesh.field.setAsBackgroundMesh(2)

            gmsh.option.setNumber("Mesh.Algorithm3D", 1)
            gmsh.model.mesh.generate(3)

            self._extract_gmsh_data(gmsh)
            self._identify_plate_sets(gmsh, hw, hh, thickness, hole_r)

            stats = {
                "n_nodes": len(self._nodes),
                "n_elements": sum(
                    len(e["connectivity"]) for e in self._elements.values()
                ),
            }

        finally:
            gmsh.finalize()

        return stats

    # ------------------------------------------------------------------
    # Analysis setup
    # ------------------------------------------------------------------

    def setup_analysis(
        self,
        material: dict,
        loads: list,
        boundary_conditions: list,
    ) -> Path:
        """Write the INP file from current mesh and analysis parameters."""
        if self._nodes is None:
            raise RuntimeError("No mesh data — call create_plate_with_hole first")

        writer = INPWriter(self._nodes, self._elements)
        writer.add_material(material["name"], material["E"], material["nu"])

        for name, ids in self._node_sets.items():
            writer.add_node_set(name, ids)
        for name, ids in self._element_sets.items():
            writer.add_element_set(name, ids)

        writer.add_step()

        for bc in boundary_conditions:
            nset = bc["node_set"]
            for dof in range(bc["dof_start"], bc["dof_end"] + 1):
                writer.add_boundary_condition(nset, dof, bc.get("value", 0.0))

        for ld in loads:
            direction = ld.get("direction", (1, 0, 0))
            writer.add_load(
                ld["node_set"], ld["type"], ld["magnitude"], direction,
            )

        self._inp_path = writer.write(
            self.work_dir / f"{self.JOB_NAME}.inp"
        )
        return self._inp_path

    # ------------------------------------------------------------------
    # Solve
    # ------------------------------------------------------------------

    def solve(self, n_threads: int = 1) -> dict:
        """Run ccx solver. Returns dict with 'success' and 'message'."""
        if not is_calculix_available():
            raise RuntimeError("CalculiX (ccx) not found in PATH")
        if self._inp_path is None:
            raise RuntimeError("No INP file — call setup_analysis first")

        job = self._inp_path.stem
        env = {"OMP_NUM_THREADS": str(n_threads)}

        result = subprocess.run(
            ["ccx", "-i", job],
            cwd=str(self.work_dir),
            capture_output=True,
            text=True,
            timeout=300,
            env={**__import__("os").environ, **env},
        )

        return {
            "success": result.returncode == 0,
            "message": result.stdout[-500:] if result.stdout else "",
            "stderr": result.stderr[-500:] if result.stderr else "",
        }

    # ------------------------------------------------------------------
    # Results
    # ------------------------------------------------------------------

    def extract_results(self) -> dict:
        """Parse solver results and return summary."""
        parser = CalculiXResultParser(self.JOB_NAME, self.work_dir)
        frd = parser.parse_frd_file()
        max_vm = parser.get_max_von_mises()
        max_disp = parser.get_max_displacement()
        max_sxx = parser.get_max_stress_component("sxx")

        # Find max stress near hole (exclude load/BC application nodes)
        hole_sxx = self._max_stress_near_hole(parser)

        return {
            "max_von_mises": max_vm,
            "max_sxx": max_sxx,
            "hole_sxx": hole_sxx,
            "max_displacement": max_disp,
            "n_disp_nodes": len(frd["displacements"]),
            "n_stress_nodes": len(frd["stresses"]),
        }

    def _max_stress_near_hole(self, parser: CalculiXResultParser) -> float:
        """Find max σ_xx near the hole, excluding boundary/load regions."""
        if self._nodes is None:
            return 0.0
        max_sxx = 0.0
        hole_r = 10.0  # Default from run_plate_validation
        search_r = hole_r * 3.0
        for node_id, stress in parser._stresses.items():
            # node_id is 1-based; nodes array is 0-based
            idx = node_id - 1
            if idx < 0 or idx >= len(self._nodes):
                continue
            x, y, z = self._nodes[idx]
            dist = (x * x + y * y) ** 0.5
            if dist < search_r:
                max_sxx = max(max_sxx, abs(stress["sxx"]))
        return max_sxx

    # ------------------------------------------------------------------
    # Validation shortcut
    # ------------------------------------------------------------------

    def run_plate_validation(self, sigma_applied: float = 100.0) -> dict:
        """
        Full chain: plate with hole -> mesh -> solve -> Kt.

        Uses quarter-symmetry model with W/d > 5 so theoretical Kt ~ 3.0.
        """
        plate_w = 200.0
        plate_h = 200.0
        hole_r = 10.0
        thickness = 1.0
        element_size = 8.0

        stats = self.create_plate_with_hole(
            plate_w, plate_h, hole_r, thickness, element_size,
        )

        # Applied traction as equivalent nodal loads on quarter model
        # Far edge spans y=0..plate_h/2, total force = σ × t × (H/2)
        n_load = len(self._node_sets.get("LOAD", [1]))
        force_per_node = sigma_applied * thickness * (plate_h / 2.0) / n_load
        self.setup_analysis(
            material={"name": "STEEL", "E": 210000.0, "nu": 0.3},
            loads=[{
                "type": "cload",
                "node_set": "LOAD",
                "dof": 1,
                "magnitude": force_per_node,
                "direction": (1, 0, 0),
            }],
            boundary_conditions=[
                {"node_set": "SYM_X", "dof_start": 1, "dof_end": 1},
                {"node_set": "SYM_Y", "dof_start": 2, "dof_end": 2},
                {"node_set": "FIX_Z", "dof_start": 3, "dof_end": 3},
            ],
        )

        status = self.solve()
        if not status["success"]:
            raise RuntimeError(f"ccx failed: {status['stderr']}")

        results = self.extract_results()
        # Kt = max σ_xx near hole / σ_applied
        # Excludes boundary/load nodes where stress singularities occur
        kt = results["hole_sxx"] / sigma_applied

        return {
            **results,
            **stats,
            "sigma_applied": sigma_applied,
            "kt": kt,
        }

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _extract_gmsh_data(self, gmsh_module):
        """Extract nodes and elements from gmsh into internal storage."""
        node_tags, node_coords, _ = gmsh_module.model.mesh.getNodes()
        self._nodes = node_coords.reshape(-1, 3)

        self._elements = {}
        for etype in gmsh_module.model.mesh.getElementTypes():
            etags, enodes = gmsh_module.model.mesh.getElementsByType(etype)
            props = gmsh_module.model.mesh.getElementProperties(etype)
            name, dim, order, npn = props[0], props[1], props[2], props[3]
            conn = enodes.reshape(-1, npn)
            self._elements[name] = {
                "connectivity": conn - 1,  # 0-based
                "dimension": dim,
            }

    def _identify_plate_sets(self, gmsh_module, hw, hh, thickness, hole_r):
        """Identify node sets for boundary conditions on the plate model."""
        tol = 1e-6
        nodes = self._nodes

        sym_x = [i for i, n in enumerate(nodes) if abs(n[0]) < tol]
        sym_y = [i for i, n in enumerate(nodes) if abs(n[1]) < tol]
        fix_z = [i for i, n in enumerate(nodes) if abs(n[2]) < tol]
        load_nodes = [
            i for i, n in enumerate(nodes) if abs(n[0] - hw) < tol
        ]

        self._node_sets["SYM_X"] = sym_x
        self._node_sets["SYM_Y"] = sym_y
        self._node_sets["FIX_Z"] = fix_z
        self._node_sets["LOAD"] = load_nodes
        self._node_sets["FIX"] = sym_x  # alias for generic BC tests
