#!/usr/bin/env python3
"""
ABOUTME: OpenFOAM case directory builder that generates the standard directory
tree (0/, constant/, system/) with all required dict files from an OpenFOAMCase
configuration. Does not require OpenFOAM to be installed.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, List, Optional

from loguru import logger

from .initial_fields import (
    write_alpha_water,
    write_pressure_field,
    write_turbulence_fields,
    write_velocity_field,
)
from .models import OpenFOAMCase, TurbulenceType
from .motion import render_dynamic_mesh_dict_body
from .partial_fill import (
    partial_fill_box,
    render_set_fields_dict_body,
    snap_fill_to_cell_face,
)
from .pressure_taps import PressureTap, render_pressure_tap_functions
from .templates import (
    FV_SOLUTION_SOLVERS,
    PIMPLE_BLOCK,
    SIMPLE_BLOCK,
    TRANSPORT_MULTIPHASE,
    TRANSPORT_SINGLE,
)

# ---------------------------------------------------------------------------
# FoamFile header template
# ---------------------------------------------------------------------------

_FOAM_FILE_HEADER = """\
/*--------------------------------*- C++ -*----------------------------------*\\
  =========                 |
  \\\\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\\\    /   O peration     | Version: v2306
    \\\\  /    A nd           | Website: www.openfoam.com
     \\/     M anipulation   |
\\*---------------------------------------------------------------------------*/
FoamFile
{{
    version     2.0;
    format      ascii;
    class       {foam_class};
    object      {foam_object};
}}
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //
"""

_FOOTER = ("// ****"
           "******* * * * * * * * * * * * * * * * * * * * * //\n")


def _foam_header(foam_class: str, foam_object: str) -> str:
    return _FOAM_FILE_HEADER.format(
        foam_class=foam_class, foam_object=foam_object
    )


# ---------------------------------------------------------------------------
# OpenFOAMCaseBuilder
# ---------------------------------------------------------------------------


class OpenFOAMCaseBuilder:
    """Generate a standard OpenFOAM case directory tree from an OpenFOAMCase.

    Creates the three-directory layout expected by all OpenFOAM solvers::

        <case>/0/           -- initial field conditions
        <case>/constant/    -- mesh, physical properties
        <case>/system/      -- solver control and numerical schemes

    No OpenFOAM installation is required; this class only writes text files.

    Example::

        case = OpenFOAMCase.for_case_type(CaseType.CURRENT_LOADING, "my_case")
        builder = OpenFOAMCaseBuilder(case)
        case_dir = builder.build(Path("/tmp/runs"))
    """

    def __init__(
        self,
        case: OpenFOAMCase,
        pressure_taps: Optional[List[PressureTap]] = None,
        *,
        tap_write_interval: int = 1,
    ) -> None:
        self._case = case
        self._pressure_taps: List[PressureTap] = list(pressure_taps or [])
        self._tap_write_interval = tap_write_interval

    def build(self, parent_dir: Path) -> Path:
        """Build the full case directory tree under parent_dir.

        Args:
            parent_dir: Parent directory where the case subdirectory is created.

        Returns:
            Path to the created case directory.
        """
        parent_dir = Path(parent_dir)
        case_dir = parent_dir / self._case.name
        logger.info(f"Building OpenFOAM case: {case_dir}")

        zero_dir = case_dir / "0"
        constant_dir = case_dir / "constant"
        system_dir = case_dir / "system"

        for d in (zero_dir, constant_dir, system_dir):
            d.mkdir(parents=True, exist_ok=True)

        self._write_system(system_dir)
        self._write_constant(constant_dir)
        self._write_zero(zero_dir)

        logger.info(f"Case build complete: {case_dir}")
        return case_dir

    # ------------------------------------------------------------------ #
    #  system/ writers                                                    #
    # ------------------------------------------------------------------ #

    def _write_system(self, system_dir: Path) -> None:
        """Write all files in system/."""
        self._write_control_dict(system_dir)
        self._write_fv_schemes(system_dir)
        self._write_fv_solution(system_dir)
        self._write_block_mesh_dict(system_dir)
        self._write_decompose_par_dict(system_dir)
        if (
            self._case.solver_config.is_multiphase
            and self._case.fill_level is not None
        ):
            self._write_set_fields_dict(system_dir)

    def _write_set_fields_dict(self, system_dir: Path) -> None:
        """Write system/setFieldsDict for a VOF partial fill (#659).

        The still-water level ``h = fill_level * tank_height`` is snapped onto a
        vertical cell face (the block mesh is z-up, so the vertical extent is the
        z direction) so the free surface never bisects a cell. ``alpha.water`` is
        set to 1 (liquid) below ``h`` and defaults to 0 (air) above.
        """
        dc = self._case.domain
        vertical_axis = 2  # block mesh is z-up (bottom = zmin face)
        tank_height = dc.max_coords[vertical_axis] - dc.min_coords[vertical_axis]
        n_cells = dc.cell_counts()[vertical_axis]
        snap = snap_fill_to_cell_face(tank_height, n_cells, self._case.fill_level)
        box_min, box_max = partial_fill_box(
            dc.min_coords, dc.max_coords, snap.fill_height,
            vertical_axis=vertical_axis,
        )
        content = _foam_header("dictionary", "setFieldsDict")
        content += "\n" + render_set_fields_dict_body(box_min, box_max) + "\n"
        content += _FOOTER
        (system_dir / "setFieldsDict").write_text(content)

    def _write_control_dict(self, system_dir: Path) -> None:
        """Write system/controlDict."""
        cd = self._case.solver_config.to_control_dict()
        lines: List[str] = [_foam_header("dictionary", "controlDict")]

        for key, val in cd.items():
            if isinstance(val, bool):
                val_str = "yes" if val else "no"
            elif isinstance(val, str):
                val_str = val
            else:
                val_str = str(val)
            lines.append(f"{key:<24} {val_str};")

        # Optional, additive: named wall pressure taps (dm#661). With no taps
        # the controlDict is byte-for-byte identical to the taps-free build.
        if self._pressure_taps:
            block = render_pressure_tap_functions(
                self._pressure_taps,
                write_interval=self._tap_write_interval,
            )
            lines.append("\n" + block)

        lines.append("\n" + _FOOTER)
        (system_dir / "controlDict").write_text("\n".join(lines))

    def _write_fv_schemes(self, system_dir: Path) -> None:
        """Write system/fvSchemes with sensible defaults."""
        solver = self._case.solver_config.solver_name
        is_transient = solver in ("interFoam", "pimpleFoam")
        time_scheme = "backward" if is_transient else "steadyState"

        content = _foam_header("dictionary", "fvSchemes")
        content += f"""
ddtSchemes
{{
    default {time_scheme};
}}

gradSchemes
{{
    default         Gauss linear;
    grad(p)         Gauss linear;
}}

divSchemes
{{
    default                         none;
    div(phi,U)                      Gauss linearUpwind grad(U);
    div(phi,k)                      Gauss upwind;
    div(phi,omega)                  Gauss upwind;
    div(phi,epsilon)                Gauss upwind;
    div((nuEff*dev(T(grad(U)))))    Gauss linear;
    div(phi,alpha.water)            Gauss vanLeer;
    div(phirb,alpha.water)          Gauss interfaceCompression;
}}

laplacianSchemes
{{
    default Gauss linear corrected;
}}

interpolationSchemes
{{
    default linear;
}}

snGradSchemes
{{
    default corrected;
}}

"""
        content += _FOOTER
        (system_dir / "fvSchemes").write_text(content)

    def _write_fv_solution(self, system_dir: Path) -> None:
        """Write system/fvSolution with solver tolerances."""
        solver = self._case.solver_config.solver_name
        is_transient = solver in ("interFoam", "pimpleFoam")

        content = _foam_header("dictionary", "fvSolution")
        content += FV_SOLUTION_SOLVERS
        if is_transient:
            content += PIMPLE_BLOCK
        else:
            content += SIMPLE_BLOCK
        content += _FOOTER
        (system_dir / "fvSolution").write_text(content)

    def _write_block_mesh_dict(self, system_dir: Path) -> None:
        """Write system/blockMeshDict from DomainConfig."""
        dc = self._case.domain
        verts = dc.block_mesh_vertices()
        nx, ny, nz = dc.cell_counts()

        vert_lines = "\n    ".join(
            f"( {v[0]:>10.4f}  {v[1]:>10.4f}  {v[2]:>10.4f} )"
            for v in verts
        )

        content = _foam_header("dictionary", "blockMeshDict")
        content += f"""
convertToMeters 1;

vertices
(
    {vert_lines}
);

blocks
(
    hex (0 1 2 3 4 5 6 7) ({nx} {ny} {nz}) simpleGrading (1 1 1)
);

edges
(
);

boundary
(
    inlet
    {{
        type patch;
        faces
        (
            (0 4 7 3)
        );
    }}
    outlet
    {{
        type patch;
        faces
        (
            (1 2 6 5)
        );
    }}
    bottom
    {{
        type wall;
        faces
        (
            (0 1 2 3)
        );
    }}
    top
    {{
        type patch;
        faces
        (
            (4 5 6 7)
        );
    }}
    sides
    {{
        type symmetry;
        faces
        (
            (0 1 5 4)
            (3 7 6 2)
        );
    }}
);

mergePatchPairs
(
);

"""
        content += _FOOTER
        (system_dir / "blockMeshDict").write_text(content)

    def _write_decompose_par_dict(self, system_dir: Path) -> None:
        """Write system/decomposeParDict for parallel execution."""
        n_sub = self._case.solver_config.n_subdomains
        content = _foam_header("dictionary", "decomposeParDict")
        content += f"\nnumberOfSubdomains  {n_sub};\n\nmethod  scotch;\n\n"
        content += _FOOTER
        (system_dir / "decomposeParDict").write_text(content)

    # ------------------------------------------------------------------ #
    #  constant/ writers                                                  #
    # ------------------------------------------------------------------ #

    def _write_constant(self, constant_dir: Path) -> None:
        """Write all files in constant/."""
        self._write_transport_properties(constant_dir)
        self._write_turbulence_properties(constant_dir)
        if self._case.solver_config.is_multiphase:
            self._write_gravity(constant_dir)
        if self._case.motion is not None:
            self._write_dynamic_mesh_dict(constant_dir)

    def _write_dynamic_mesh_dict(self, constant_dir: Path) -> None:
        """Write constant/dynamicMeshDict for a prescribed forced motion (#658)."""
        content = _foam_header("dictionary", "dynamicMeshDict")
        content += "\n" + render_dynamic_mesh_dict_body(self._case.motion) + "\n"
        content += _FOOTER
        (constant_dir / "dynamicMeshDict").write_text(content)

    def _write_transport_properties(self, constant_dir: Path) -> None:
        """Write constant/transportProperties for water and air."""
        if self._case.solver_config.is_multiphase:
            content = _foam_header("dictionary", "transportProperties")
            content += TRANSPORT_MULTIPHASE
        else:
            content = _foam_header("dictionary", "transportProperties")
            content += TRANSPORT_SINGLE
        content += _FOOTER
        (constant_dir / "transportProperties").write_text(content)

    def _write_turbulence_properties(self, constant_dir: Path) -> None:
        """Write constant/turbulenceProperties."""
        tm = self._case.turbulence_model
        tm_dict = tm.to_dict()
        sim_type = tm_dict.get("simulationType", "RAS")

        content = _foam_header("dictionary", "turbulenceProperties")
        content += f"\nsimulationType  {sim_type};\n"

        if sim_type in ("RAS", "LES"):
            inner = tm_dict.get(sim_type, {})
            model = inner.get("model", tm.turbulence_type.value)
            content += (
                f"\n{sim_type}\n{{\n"
                f"    model           {model};\n"
                f"    turbulence      on;\n"
                f"    printCoeffs     on;\n}}\n"
            )
        content += _FOOTER
        (constant_dir / "turbulenceProperties").write_text(content)

    def _write_gravity(self, constant_dir: Path) -> None:
        """Write constant/g for multiphase simulations."""
        content = _foam_header("uniformDimensionedVectorField", "g")
        content += "\ndimensions  [0 1 -2 0 0 0 0];\nvalue       (0 0 -9.81);\n\n"
        content += _FOOTER
        (constant_dir / "g").write_text(content)

    # ------------------------------------------------------------------ #
    #  0/ writers — delegated to initial_fields module                   #
    # ------------------------------------------------------------------ #

    def _write_zero(self, zero_dir: Path) -> None:
        """Write all initial field files in 0/."""
        is_mp = self._case.solver_config.is_multiphase
        bcs = self._case.boundary_conditions
        write_velocity_field(zero_dir, boundary_conditions=bcs)
        write_pressure_field(zero_dir, is_multiphase=is_mp, boundary_conditions=bcs)
        write_turbulence_fields(zero_dir, self._case.turbulence_model)
        if is_mp:
            write_alpha_water(zero_dir, boundary_conditions=bcs)
