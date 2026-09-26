#!/usr/bin/env python3
"""
ABOUTME: End-to-end MYSTRAN chain — structured mesh generation (or gmsh-style
mesh injection), BDF export, MYSTRAN solve, F06 result extraction, and a
cantilever mesh-convergence sweep for validation.
"""

import os
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Optional, Sequence

import numpy as np

from .bdf_writer import BDFWriter
from .convergence import MeshConvergenceStudy
from .result_parser import MystranResultParser

MYSTRAN_EXE_ENV = "MYSTRAN_EXE"
DEFAULT_CONVERGENCE_LEVELS = ((4, 1, 1), (8, 2, 2), (16, 2, 4), (32, 4, 8))


def find_mystran() -> Optional[str]:
    """Path to the MYSTRAN executable: ``$MYSTRAN_EXE`` override, else PATH."""
    override = os.environ.get(MYSTRAN_EXE_ENV)
    if override and Path(override).exists():
        return override
    return shutil.which("mystran")


def is_mystran_available() -> bool:
    """Check whether the MYSTRAN solver can be run."""
    return find_mystran() is not None


class MystranChain:
    """
    Orchestrate: mesh -> BDF -> mystran -> results.

    Mirrors :class:`digitalmodel.solvers.calculix.FEMChain`. Meshes may be
    generated with the built-in structured cantilever generators (no gmsh
    required) or injected with :meth:`load_mesh` from any gmsh-style
    extraction (0-based connectivity keyed by gmsh element type name).
    """

    JOB_NAME = "mystran_analysis"

    def __init__(self, work_dir: Optional[Path] = None):
        if work_dir is None:
            work_dir = Path(tempfile.mkdtemp(prefix="mystran_"))
        self.work_dir = Path(work_dir)
        self.work_dir.mkdir(parents=True, exist_ok=True)

        self._nodes: Optional[np.ndarray] = None
        self._elements: Optional[dict] = None
        self._node_sets: dict[str, list[int]] = {}
        self._bdf_path: Optional[Path] = None
        self._last_run: Optional[dict] = None

    # ------------------------------------------------------------------
    # Mesh
    # ------------------------------------------------------------------

    def load_mesh(
        self,
        nodes: np.ndarray,
        elements: dict,
        node_sets: Optional[dict[str, Sequence[int]]] = None,
    ) -> dict:
        """Inject a gmsh-style mesh (0-based connectivity)."""
        self._nodes = np.asarray(nodes, dtype=float).reshape(-1, 3)
        self._elements = elements
        self._node_sets = {k: list(v) for k, v in (node_sets or {}).items()}
        return self.mesh_stats()

    def create_cantilever_hex_mesh(
        self,
        length: float,
        width: float,
        height: float,
        nx: int,
        ny: int,
        nz: int,
    ) -> dict:
        """
        Structured CHEXA8 cantilever: x along ``length``, y ``width``,
        z ``height``. Node sets: ``FIX`` (x=0), ``TIP`` (x=length),
        ``TIP_CENTER`` (node nearest the tip-face centroid).
        """
        if min(nx, ny, nz) < 1:
            raise ValueError("nx, ny, nz must be >= 1")
        xs = np.linspace(0.0, length, nx + 1)
        ys = np.linspace(0.0, width, ny + 1)
        zs = np.linspace(0.0, height, nz + 1)

        def nid(i, j, k):
            return (i * (ny + 1) + j) * (nz + 1) + k

        nodes = np.zeros(((nx + 1) * (ny + 1) * (nz + 1), 3))
        for i, x in enumerate(xs):
            for j, y in enumerate(ys):
                for k, z in enumerate(zs):
                    nodes[nid(i, j, k)] = (x, y, z)

        conn = []
        for i in range(nx):
            for j in range(ny):
                for k in range(nz):
                    # CHEXA node order: bottom face CCW (z=k), then top (z=k+1)
                    conn.append([
                        nid(i, j, k), nid(i + 1, j, k),
                        nid(i + 1, j + 1, k), nid(i, j + 1, k),
                        nid(i, j, k + 1), nid(i + 1, j, k + 1),
                        nid(i + 1, j + 1, k + 1), nid(i, j + 1, k + 1),
                    ])
        elements = {
            "Hexahedron 8": {
                "connectivity": np.array(conn, dtype=int),
                "dimension": 3,
            }
        }
        fix = [nid(0, j, k) for j in range(ny + 1) for k in range(nz + 1)]
        tip = [nid(nx, j, k) for j in range(ny + 1) for k in range(nz + 1)]
        tip_pts = nodes[tip]
        centre = np.array([length, width / 2.0, height / 2.0])
        tip_center = tip[int(np.argmin(np.linalg.norm(tip_pts - centre, axis=1)))]
        return self.load_mesh(
            nodes, elements,
            {"FIX": fix, "TIP": tip, "TIP_CENTER": [tip_center]},
        )

    def create_cantilever_bar_mesh(self, length: float, n_elems: int) -> dict:
        """CBAR cantilever along x. Node sets ``FIX`` (root) and ``TIP``."""
        if n_elems < 1:
            raise ValueError("n_elems must be >= 1")
        nodes = np.array(
            [[i * length / n_elems, 0.0, 0.0] for i in range(n_elems + 1)]
        )
        elements = {
            "Line 2": {
                "connectivity": np.array(
                    [[i, i + 1] for i in range(n_elems)], dtype=int
                ),
                "dimension": 1,
            }
        }
        return self.load_mesh(
            nodes, elements,
            {"FIX": [0], "TIP": [n_elems], "TIP_CENTER": [n_elems]},
        )

    def mesh_stats(self) -> dict:
        if self._nodes is None or self._elements is None:
            return {"n_nodes": 0, "n_elements": 0}
        return {
            "n_nodes": int(len(self._nodes)),
            "n_elements": int(sum(
                len(e["connectivity"]) for e in self._elements.values()
            )),
        }

    @property
    def node_sets(self) -> dict[str, list[int]]:
        return {k: list(v) for k, v in self._node_sets.items()}

    # ------------------------------------------------------------------
    # Analysis setup
    # ------------------------------------------------------------------

    def setup_analysis(
        self,
        material: dict,
        sections: Sequence[dict],
        loads: Sequence[dict],
        boundary_conditions: Sequence[dict],
        title: Optional[str] = None,
    ) -> Path:
        """
        Write the BDF from the current mesh.

        ``material``: ``{"name", "E", "nu", "rho"?}``.
        ``sections``: one per element type, e.g.
        ``{"element_type": "Hexahedron 8", "type": "solid"}``,
        ``{"element_type": "Line 2", "type": "bar", "A", "I1", "I2", "J",
        "orientation"?, "stress_points"?}``,
        ``{"element_type": "Quadrilateral 4", "type": "shell", "thickness"}``.
        ``loads``: ``{"node_set", "magnitude", "direction", "distribute"?}``
        (``distribute=True`` splits ``magnitude`` evenly over the set).
        ``boundary_conditions``: ``{"node_set", "dofs"}``.
        """
        if self._nodes is None or self._elements is None:
            raise RuntimeError("No mesh — call a create_* method or load_mesh first")

        writer = BDFWriter(
            self._nodes, self._elements,
            title=title or f"digitalmodel {self.JOB_NAME}",
        )
        writer.add_material(
            material["name"], material["E"], material["nu"],
            material.get("rho", 0.0),
        )
        for sec in sections:
            kind = sec["type"]
            et = sec["element_type"]
            if kind == "solid":
                writer.add_solid_section(
                    et, material["name"], sec.get("integration_order", 2)
                )
            elif kind == "bar":
                writer.add_bar_section(
                    et, material["name"], sec["A"], sec["I1"], sec["I2"],
                    sec["J"], sec.get("orientation", (0.0, 0.0, 1.0)),
                    sec.get("stress_points"),
                )
            elif kind == "shell":
                writer.add_shell_section(et, material["name"], sec["thickness"])
            else:
                raise ValueError(f"Unknown section type {kind!r}")

        for bc in boundary_conditions:
            writer.add_spc(self._set(bc["node_set"]), bc.get("dofs", "123456"))

        for ld in loads:
            ids = self._set(ld["node_set"])
            mag = float(ld["magnitude"])
            if ld.get("distribute", False):
                mag = mag / len(ids)
            writer.add_force(ids, mag, ld.get("direction", (0.0, 0.0, 1.0)))

        self._bdf_path = writer.write(self.work_dir / f"{self.JOB_NAME}.bdf")
        return self._bdf_path

    # ------------------------------------------------------------------
    # Solve
    # ------------------------------------------------------------------

    def solve(self, timeout: float = 300.0) -> dict:
        """Run MYSTRAN. Returns dict with ``success``, ``message``, ``errors``."""
        exe = find_mystran()
        if exe is None:
            raise RuntimeError("MYSTRAN executable not found (PATH or $MYSTRAN_EXE)")
        if self._bdf_path is None:
            raise RuntimeError("No BDF file — call setup_analysis first")

        # Remove stale outputs so a failed run cannot be read as success
        parser = MystranResultParser(self.JOB_NAME, self.work_dir)
        for ext in ("F06", "OP2", "ERR"):
            p = parser.find_output(ext)
            if p is not None:
                p.unlink()

        result = subprocess.run(
            [exe, self._bdf_path.name],
            cwd=str(self.work_dir),
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        stdout = result.stdout or ""
        f06 = parser.find_output("F06")
        errors: list[str] = []
        if f06 is not None:
            errors = parser.parse_f06()["errors"]
        # MYSTRAN exits 0 even on FATAL errors; inspect stdout and F06.
        success = (
            result.returncode == 0
            and f06 is not None
            and "FATAL" not in stdout.upper()
            and not errors
        )
        self._last_run = {
            "success": success,
            "message": stdout[-500:],
            "stderr": (result.stderr or "")[-500:],
            "errors": errors,
            "f06": str(f06) if f06 else None,
        }
        return self._last_run

    # ------------------------------------------------------------------
    # Results
    # ------------------------------------------------------------------

    def extract_results(self, tip_component: str = "t3") -> dict:
        """Parse F06 and return a summary dict."""
        parser = MystranResultParser(self.JOB_NAME, self.work_dir)
        summary = parser.parse_f06()
        disp = summary["displacements"]
        tip = None
        if "TIP_CENTER" in self._node_sets:
            gid = self._node_sets["TIP_CENTER"][0] + 1
            if gid in disp:
                tip = disp[gid][tip_component]
        tip_mean = None
        if "TIP" in self._node_sets:
            vals = [
                disp[n + 1][tip_component]
                for n in self._node_sets["TIP"] if n + 1 in disp
            ]
            tip_mean = float(np.mean(vals)) if vals else None
        return {
            "max_displacement": {
                c: parser.get_max_displacement(c) for c in ("t1", "t2", "t3", "mag")
            },
            "tip_displacement": tip,
            "tip_displacement_mean": tip_mean,
            "max_von_mises": parser.get_max_von_mises(),
            "reaction_sum": parser.get_reaction_sum(),
            "epsilon": summary["epsilon"],
            "n_disp_nodes": len(disp),
            **self.mesh_stats(),
        }

    # ------------------------------------------------------------------
    # Validation shortcuts
    # ------------------------------------------------------------------

    @staticmethod
    def cantilever_tip_deflection(P: float, L: float, E: float, I: float) -> float:
        """Euler-Bernoulli tip deflection ``P L^3 / (3 E I)``."""
        return P * L ** 3 / (3.0 * E * I)

    def run_cantilever_bar_validation(
        self,
        n_elems: int = 4,
        L: float = 1.0,
        b: float = 0.1,
        h: float = 0.1,
        E: float = 210.0e9,
        nu: float = 0.3,
        P: float = 1000.0,
    ) -> dict:
        """CBAR cantilever with tip load in z; compares to ``PL^3/3EI``."""
        A = b * h
        I1 = b * h ** 3 / 12.0  # bending about y (deflection in z)
        I2 = h * b ** 3 / 12.0
        J = 0.141 * b * h ** 3
        stats = self.create_cantilever_bar_mesh(L, n_elems)
        self.setup_analysis(
            material={"name": "STEEL", "E": E, "nu": nu},
            sections=[{
                "element_type": "Line 2", "type": "bar",
                "A": A, "I1": I1, "I2": I2, "J": J,
                "orientation": (0.0, 1.0, 0.0),
            }],
            loads=[{"node_set": "TIP", "magnitude": P, "direction": (0, 0, 1)}],
            boundary_conditions=[{"node_set": "FIX", "dofs": "123456"}],
            title="CBAR cantilever validation",
        )
        status = self.solve()
        if not status["success"]:
            raise RuntimeError(f"mystran failed: {status['errors'] or status['message']}")
        res = self.extract_results("t3")
        exact = self.cantilever_tip_deflection(P, L, E, I1)
        return {
            **res, **stats,
            "exact": exact,
            "error_pct": abs(res["tip_displacement"] - exact) / exact * 100.0,
        }

    def run_cantilever_hex_validation(
        self,
        nx: int,
        ny: int,
        nz: int,
        L: float = 1.0,
        b: float = 0.1,
        h: float = 0.1,
        E: float = 210.0e9,
        nu: float = 0.3,
        P: float = 1000.0,
    ) -> dict:
        """CHEXA8 cantilever, tip load in z distributed over the tip face."""
        stats = self.create_cantilever_hex_mesh(L, b, h, nx, ny, nz)
        self.setup_analysis(
            material={"name": "STEEL", "E": E, "nu": nu},
            sections=[{"element_type": "Hexahedron 8", "type": "solid"}],
            loads=[{
                "node_set": "TIP", "magnitude": P,
                "direction": (0, 0, 1), "distribute": True,
            }],
            boundary_conditions=[{"node_set": "FIX", "dofs": "123"}],
            title=f"CHEXA8 cantilever {nx}x{ny}x{nz}",
        )
        status = self.solve()
        if not status["success"]:
            raise RuntimeError(f"mystran failed: {status['errors'] or status['message']}")
        res = self.extract_results("t3")
        I = b * h ** 3 / 12.0
        exact = self.cantilever_tip_deflection(P, L, E, I)
        tip = res["tip_displacement_mean"]
        return {
            **res, **stats,
            "exact": exact,
            "error_pct": abs(tip - exact) / exact * 100.0,
        }

    def run_mesh_convergence(
        self,
        levels: Sequence[tuple[int, int, int]] = DEFAULT_CONVERGENCE_LEVELS,
        tolerance: float = 0.02,
        **kwargs,
    ) -> MeshConvergenceStudy:
        """
        Cantilever mesh-convergence sweep on tip deflection (mean over the
        tip face). Each level runs in its own sub-directory of ``work_dir``.
        Extra ``kwargs`` (L, b, h, E, nu, P) pass through to the hex model.
        """
        L = kwargs.get("L", 1.0)
        b = kwargs.get("b", 0.1)
        h = kwargs.get("h", 0.1)
        E = kwargs.get("E", 210.0e9)
        P = kwargs.get("P", 1000.0)
        exact = self.cantilever_tip_deflection(P, L, E, b * h ** 3 / 12.0)
        study = MeshConvergenceStudy(tolerance=tolerance, reference=exact)

        def evaluate(level):
            nx, ny, nz = level
            sub = MystranChain(self.work_dir / f"level_{nx}x{ny}x{nz}")
            r = sub.run_cantilever_hex_validation(nx, ny, nz, **kwargs)
            return {
                "value": r["tip_displacement_mean"],
                "n_nodes": r["n_nodes"],
                "n_elements": r["n_elements"],
                "max_von_mises": r["max_von_mises"],
                "epsilon": r["epsilon"],
            }

        study.run(levels, evaluate, label=lambda l: f"{l[0]}x{l[1]}x{l[2]}")
        return study

    # ------------------------------------------------------------------
    # Internal
    # ------------------------------------------------------------------

    def _set(self, name: str) -> list[int]:
        if name not in self._node_sets:
            raise KeyError(
                f"Unknown node set {name!r}; have {sorted(self._node_sets)}"
            )
        return self._node_sets[name]
