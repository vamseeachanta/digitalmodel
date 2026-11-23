# ABOUTME: Generate ANSYS APDL command scripts from Python configuration objects
# ABOUTME: Template-based approach — produces text, does NOT run ANSYS

"""
APDL Generator
==============

Generates complete ANSYS APDL command scripts from Python configuration
dataclasses.  Each generator method returns a string of APDL commands that
can be written to a ``.inp`` file and submitted to ANSYS Mechanical APDL.

Supported command blocks:

    - Material definitions (MP commands)
    - Element types (ET commands)
    - Meshing commands (ESIZE, MSHAPE, MSHKEY, AMESH/VMESH)
    - Boundary conditions (D, F, SF commands)
    - Solution settings (ANTYPE, NLGEOM, NSUBST, AUTOTS)
    - Post-processing (PLNSOL, PRNSOL, ETABLE)

All generated output is plain text.  No ANSYS installation required.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import List, Optional


# ---------------------------------------------------------------------------
# Configuration dataclasses
# ---------------------------------------------------------------------------

@dataclass
class MaterialConfig:
    """Material property definition for APDL MP commands."""
    mat_id: int = 1
    name: str = "Steel_SA516_Gr70"
    elastic_modulus_mpa: float = 200000.0
    poissons_ratio: float = 0.3
    density_kg_m3: float = 7850.0
    thermal_expansion_per_c: Optional[float] = 1.2e-5
    thermal_conductivity_w_mk: Optional[float] = 50.0
    specific_heat_j_kgk: Optional[float] = 480.0
    yield_strength_mpa: Optional[float] = 260.0


@dataclass
class ElementTypeConfig:
    """Element type definition for APDL ET commands."""
    type_id: int = 1
    element_name: str = "SHELL181"
    keyopts: dict[int, int] = field(default_factory=dict)
    real_constants: list[float] = field(default_factory=list)


@dataclass
class MeshConfig:
    """Meshing parameters for APDL meshing commands."""
    element_size_mm: float = 25.0
    mesh_shape: str = "TET"  # TET, HEX, QUAD, TRI
    mesh_key: int = 0  # 0=free, 1=mapped
    refine_level: int = 0
    target_areas: list[int] = field(default_factory=list)
    target_volumes: list[int] = field(default_factory=list)


@dataclass
class BoundaryCondition:
    """Single boundary condition for APDL D/F/SF commands."""
    bc_type: str = "displacement"  # displacement, force, pressure, convection
    target: str = "NODE"  # NODE, AREA, LINE
    target_id: int = 1
    dof: str = "ALL"  # UX, UY, UZ, ALL, ROTX, ROTY, ROTZ
    value: float = 0.0
    value2: Optional[float] = None  # for convection: bulk temp


@dataclass
class SolutionConfig:
    """Solution settings for APDL SOLVE block."""
    analysis_type: str = "STATIC"  # STATIC, MODAL, TRANS, HARMIC
    nonlinear: bool = True
    large_deflection: bool = True
    num_substeps: int = 20
    max_substeps: int = 100
    min_substeps: int = 5
    auto_time_stepping: bool = True
    convergence_tolerance: float = 0.001
    line_search: bool = True
    stabilization: bool = False
    time_end: float = 1.0


@dataclass
class PostProcessConfig:
    """Post-processing command configuration."""
    result_type: str = "SEQV"  # SEQV, S1, S3, USUM, UX, UY, UZ
    output_format: str = "TABLE"  # TABLE, PLOT, BOTH
    component_list: list[str] = field(default_factory=lambda: ["SEQV", "USUM"])
    etable_items: list[tuple[str, str, str]] = field(default_factory=list)
    node_selection: Optional[str] = None  # component name or None for ALL


@dataclass
class APDLScriptConfig:
    """Complete APDL script configuration."""
    title: str = "Pressure Vessel FEA"
    units: str = "MPA"  # MPA (mm,N,MPa) or SI (m,N,Pa)
    materials: list[MaterialConfig] = field(default_factory=list)
    element_types: list[ElementTypeConfig] = field(default_factory=list)
    mesh: MeshConfig = field(default_factory=MeshConfig)
    boundary_conditions: list[BoundaryCondition] = field(default_factory=list)
    solution: SolutionConfig = field(default_factory=SolutionConfig)
    postprocess: PostProcessConfig = field(default_factory=PostProcessConfig)


# ---------------------------------------------------------------------------
# Generator class
# ---------------------------------------------------------------------------

class APDLGenerator:
    """Generate ANSYS APDL command scripts from Python configuration.

    Each method returns a block of APDL command text.  Use
    :meth:`generate_full_script` to produce a complete .inp file.
    """

    # ------------------------------------------------------------------
    # Header
    # ------------------------------------------------------------------

    def generate_header(self, config: APDLScriptConfig) -> str:
        """Generate script header with title and unit system."""
        lines = [
            f"/TITLE,{config.title}",
            "/NOPR",
            "KEYW,PR_SET,1",
            "KEYW,PR_STRUC,1",
            f"! Unit system: {config.units} (mm, N, MPa, tonne)"
            if config.units == "MPA"
            else f"! Unit system: {config.units} (m, N, Pa, kg)",
            "",
        ]
        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Materials
    # ------------------------------------------------------------------

    def generate_materials(self, materials: list[MaterialConfig]) -> str:
        """Generate MP material property commands."""
        lines = ["/PREP7", "! --- Material Definitions ---"]
        for mat in materials:
            lines.append(f"! Material {mat.mat_id}: {mat.name}")
            lines.append(f"MP,EX,{mat.mat_id},{mat.elastic_modulus_mpa}")
            lines.append(f"MP,NUXY,{mat.mat_id},{mat.poissons_ratio}")
            # Convert density: kg/m3 to tonne/mm3 for MPA system
            dens_tonne_mm3 = mat.density_kg_m3 * 1e-12
            lines.append(f"MP,DENS,{mat.mat_id},{dens_tonne_mm3:.6E}")
            if mat.thermal_expansion_per_c is not None:
                lines.append(f"MP,ALPX,{mat.mat_id},{mat.thermal_expansion_per_c:.6E}")
            if mat.thermal_conductivity_w_mk is not None:
                lines.append(f"MP,KXX,{mat.mat_id},{mat.thermal_conductivity_w_mk}")
            if mat.specific_heat_j_kgk is not None:
                lines.append(f"MP,C,{mat.mat_id},{mat.specific_heat_j_kgk}")
            lines.append("")
        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Element types
    # ------------------------------------------------------------------

    def generate_element_types(self, element_types: list[ElementTypeConfig]) -> str:
        """Generate ET element type commands and keyoptions."""
        lines = ["! --- Element Types ---"]
        for et in element_types:
            lines.append(f"ET,{et.type_id},{et.element_name}")
            for kopt_num, kopt_val in et.keyopts.items():
                lines.append(f"KEYOPT,{et.type_id},{kopt_num},{kopt_val}")
            if et.real_constants:
                rc_str = ",".join(str(v) for v in et.real_constants)
                lines.append(f"R,{et.type_id},{rc_str}")
            lines.append("")
        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Meshing
    # ------------------------------------------------------------------

    def generate_mesh_commands(self, mesh: MeshConfig) -> str:
        """Generate APDL meshing commands."""
        lines = ["! --- Meshing ---"]
        lines.append(f"ESIZE,{mesh.element_size_mm}")

        shape_map = {"TET": "1", "HEX": "0", "QUAD": "0", "TRI": "1"}
        dim_map = {"TET": "3D", "HEX": "3D", "QUAD": "2D", "TRI": "2D"}
        shape_val = shape_map.get(mesh.mesh_shape.upper(), "0")
        dim = dim_map.get(mesh.mesh_shape.upper(), "3D")

        lines.append(f"MSHAPE,{shape_val},{dim}")
        lines.append(f"MSHKEY,{mesh.mesh_key}")

        if mesh.refine_level > 0:
            lines.append(f"SMRTSIZE,{mesh.refine_level}")

        if mesh.target_volumes:
            vol_str = ",".join(str(v) for v in mesh.target_volumes)
            lines.append(f"VMESH,{vol_str}")
        elif mesh.target_areas:
            area_str = ",".join(str(a) for a in mesh.target_areas)
            lines.append(f"AMESH,{area_str}")
        else:
            lines.append("VMESH,ALL")

        lines.append("")
        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Boundary conditions
    # ------------------------------------------------------------------

    def generate_boundary_conditions(
        self, bcs: list[BoundaryCondition]
    ) -> str:
        """Generate APDL boundary condition commands (D, F, SF, SFA)."""
        lines = ["! --- Boundary Conditions ---"]
        for bc in bcs:
            if bc.bc_type == "displacement":
                lines.append(f"D,{bc.target_id},{bc.dof},{bc.value}")
            elif bc.bc_type == "force":
                lines.append(f"F,{bc.target_id},{bc.dof},{bc.value}")
            elif bc.bc_type == "pressure":
                if bc.target == "AREA":
                    lines.append(f"SFA,{bc.target_id},1,PRES,{bc.value}")
                else:
                    lines.append(f"SF,{bc.target_id},PRES,{bc.value}")
            elif bc.bc_type == "convection":
                val2 = bc.value2 if bc.value2 is not None else 25.0
                if bc.target == "AREA":
                    lines.append(f"SFA,{bc.target_id},1,CONV,{bc.value},{val2}")
                else:
                    lines.append(f"SF,{bc.target_id},CONV,{bc.value},{val2}")
        lines.append("")
        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Solution
    # ------------------------------------------------------------------

    def generate_solution(self, sol: SolutionConfig) -> str:
        """Generate APDL solution settings and solve commands."""
        analysis_map = {
            "STATIC": 0, "MODAL": 2, "TRANS": 4, "HARMIC": 3,
        }
        antype = analysis_map.get(sol.analysis_type.upper(), 0)

        lines = [
            "FINISH",
            "/SOLU",
            "! --- Solution Settings ---",
            f"ANTYPE,{antype}",
        ]

        if sol.analysis_type.upper() == "STATIC":
            nlgeom_val = "ON" if sol.large_deflection else "OFF"
            lines.append(f"NLGEOM,{nlgeom_val}")
            lines.append(
                f"NSUBST,{sol.num_substeps},{sol.max_substeps},{sol.min_substeps}"
            )
            autots_val = "ON" if sol.auto_time_stepping else "OFF"
            lines.append(f"AUTOTS,{autots_val}")
            lines.append(f"CNVTOL,F,,{sol.convergence_tolerance}")
            if sol.line_search:
                lines.append("LNSRCH,ON")
            if sol.stabilization:
                lines.append("STABILIZE,CONST,ENERGY,0.001")
            lines.append(f"TIME,{sol.time_end}")

        lines.append("SOLVE")
        lines.append("FINISH")
        lines.append("")
        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Post-processing
    # ------------------------------------------------------------------

    def generate_postprocess(self, post: PostProcessConfig) -> str:
        """Generate APDL post-processing commands."""
        lines = [
            "/POST1",
            "SET,LAST",
            "! --- Post-Processing ---",
        ]

        if post.node_selection:
            lines.append(f"CMSEL,S,{post.node_selection}")

        for comp in post.component_list:
            if post.output_format in ("TABLE", "BOTH"):
                lines.append(f"PRNSOL,{comp}")
            if post.output_format in ("PLOT", "BOTH"):
                lines.append(f"PLNSOL,{comp}")

        for etab_name, etab_item, etab_comp in post.etable_items:
            lines.append(f"ETABLE,{etab_name},{etab_item},{etab_comp}")

        lines.append("FINISH")
        lines.append("")
        return "\n".join(lines)

    # ------------------------------------------------------------------
    # Full script
    # ------------------------------------------------------------------

    def generate_full_script(self, config: APDLScriptConfig) -> str:
        """Generate a complete APDL .inp script from configuration.

        Parameters
        ----------
        config : APDLScriptConfig
            Full script configuration.

        Returns
        -------
        str
            Complete APDL command script text.
        """
        sections = [
            self.generate_header(config),
            self.generate_materials(config.materials),
            self.generate_element_types(config.element_types),
            self.generate_mesh_commands(config.mesh),
            self.generate_boundary_conditions(config.boundary_conditions),
            self.generate_solution(config.solution),
            self.generate_postprocess(config.postprocess),
        ]
        return "\n".join(sections)
