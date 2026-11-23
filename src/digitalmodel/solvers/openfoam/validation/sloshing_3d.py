"""Reusable, frame-locked writer for the synthetic 3D sloshing case."""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path

from digitalmodel.solvers.openfoam.motion import (
    MotionType,
    PrescribedMotion,
    render_dynamic_mesh_dict_body,
)
from digitalmodel.solvers.openfoam.poly_mesh_contract import BoundaryContract

G = 9.80665
FRAME = "x-transverse,y-up,z-longitudinal"
BENCHMARK_CONTRACT = BoundaryContract(
    wall_patches=("leftWall", "rightWall", "lowerWall", "frontWall", "backWall"),
    atmosphere_patch="atmosphere",
)
_H = "FoamFile {{ version 2.0; format ascii; class {cls}; object {obj}; }}\n"


@dataclass(frozen=True)
class Sloshing3DConfig:
    """Case frame: vessel roll is z (YAW), vessel pitch is x (ROLL).

    New comparison controls are keyword-only; legacy defaults retain their
    dictionaries. ``fill`` is a fraction of breadth, rather than height.
    """
    cpb: int
    end_time: float
    delta_t: float
    breadth: float = 0.9
    height: float = 0.9
    length: float = 0.9
    fill: float = 0.50
    roll_deg: float = 4.0
    length_unit: str = "m"
    frame: str = FRAME
    mesh_mode: str = "blockMesh"
    decompose_ranks: int | None = None
    rotation: str = field(default="roll", kw_only=True)
    period: float | None = field(default=None, kw_only=True)
    max_co: float = field(default=0.9, kw_only=True)
    function_objects: bool | None = field(default=None, kw_only=True)
    use_2d_settings: bool = field(default=False, kw_only=True)


def _validate(config: Sloshing3DConfig, contract: BoundaryContract) -> None:
    if config.rotation not in {"roll", "pitch"}:
        raise ValueError("rotation must be 'roll' or 'pitch'")
    if not math.isfinite(config.max_co) or config.max_co <= 0:
        raise ValueError("max_co must be finite and positive")
    if config.period is not None and (not math.isfinite(config.period) or config.period <= 0):
        raise ValueError("period must be finite and positive")
    if config.length_unit != "m":
        raise ValueError("length_unit must be 'm'")
    if config.frame != FRAME:
        raise ValueError(f"frame must be {FRAME!r}")
    if config.mesh_mode not in {"blockMesh", "prebuilt"}:
        raise ValueError("mesh_mode must be 'blockMesh' or 'prebuilt'")
    if config.cpb <= 0 or config.end_time <= 0 or config.delta_t <= 0:
        raise ValueError("cpb, end_time, and delta_t must be positive")
    if config.decompose_ranks is not None and (
        type(config.decompose_ranks) is not int or config.decompose_ranks < 1
    ):
        raise ValueError("decompose_ranks must be a positive integer")
    if any(value <= 0 for value in (config.breadth, config.height, config.length)):
        raise ValueError("dimensions must be positive")
    if not 0 < config.fill < 1:
        raise ValueError("fill must be between zero and one")
    if not contract.wall_patches or contract.atmosphere_patch in contract.wall_patches:
        raise ValueError("boundary contract must contain distinct wall and atmosphere patches")


def _first_mode_period(config: Sloshing3DConfig) -> float:
    h = config.fill * config.breadth
    w2 = (math.pi * G / config.breadth) * math.tanh(math.pi * h / config.breadth)
    return 2.0 * math.pi / math.sqrt(w2)


def _blockmesh(config: Sloshing3DConfig, nx: int, ny: int, nz: int, contract: BoundaryContract) -> str:
    f = "{:.6g}".format
    walls = contract.wall_patches
    return _H.format(cls="dictionary", obj="blockMeshDict") + f"""
scale 1;
vertices
(
    (0 0 0) ({f(config.breadth)} 0 0) ({f(config.breadth)} {f(config.height)} 0) (0 {f(config.height)} 0)
    (0 0 {f(config.length)}) ({f(config.breadth)} 0 {f(config.length)}) ({f(config.breadth)} {f(config.height)} {f(config.length)}) (0 {f(config.height)} {f(config.length)})
);
blocks ( hex (0 1 2 3 4 5 6 7) ({nx} {ny} {nz}) simpleGrading (1 1 1) );
edges ();
boundary
(
    {walls[0]}  {{ type wall; faces ( (0 4 7 3) ); }}
    {walls[1]} {{ type wall; faces ( (1 2 6 5) ); }}
    {walls[2]} {{ type wall; faces ( (0 1 5 4) ); }}
    {walls[3]} {{ type wall; faces ( (0 3 2 1) ); }}
    {walls[4]}  {{ type wall; faces ( (4 5 6 7) ); }}
    {contract.atmosphere_patch} {{ type patch; faces ( (3 7 6 2) ); }}
);
mergePatchPairs ();
"""


def _field(name: str, contract: BoundaryContract) -> str:
    walls = contract.wall_patches
    if name == "U":
        entries = [f"    {wall} {{ type movingWallVelocity; value uniform (0 0 0); }}" for wall in walls]
        entries.append(f"    {contract.atmosphere_patch} {{ type pressureInletOutletVelocity; value uniform (0 0 0); }}")
        header, dimensions, internal = "volVectorField", "[0 1 -1 0 0 0 0]", "(0 0 0)"
    elif name == "p_rgh":
        entries = [f"    {wall} {{ type fixedFluxPressure; value uniform 0; }}" for wall in walls]
        entries.append(f"    {contract.atmosphere_patch} {{ type totalPressure; p0 uniform 0; }}")
        header, dimensions, internal = "volScalarField", "[1 -1 -2 0 0 0 0]", "0"
    else:
        entries = [f"    {wall} {{ type zeroGradient; }}" for wall in walls]
        entries.append(f"    {contract.atmosphere_patch} {{ type inletOutlet; inletValue uniform 0; value uniform 0; }}")
        header, dimensions, internal = "volScalarField", "[0 0 0 0 0 0 0]", "0"
    return _H.format(cls=header, obj=name) + f"""
dimensions {dimensions};
internalField uniform {internal};
boundaryField
{{
{chr(10).join(entries)}
}}
"""


_FVSCHEMES = """
ddtSchemes { default Euler; }
gradSchemes { default Gauss linear; }
divSchemes
{
    div(rhoPhi,U) Gauss linearUpwind grad(U);
    div(phi,alpha) Gauss vanLeer;
    div(phirb,alpha) Gauss linear;
    div(((rho*nuEff)*dev2(T(grad(U))))) Gauss linear;
}
laplacianSchemes { default Gauss linear corrected; }
interpolationSchemes { default linear; }
snGradSchemes { default corrected; }
"""

_FVSOLUTION = """
solvers
{
    "alpha.water.*"
    {
        nAlphaCorr 2; nAlphaSubCycles 1; cAlpha 1;
        MULESCorr yes; nLimiterIter 5;
        solver smoothSolver; smoother symGaussSeidel; tolerance 1e-8; relTol 0;
    }
    "pcorr.*" { solver PCG; preconditioner DIC; tolerance 1e-5; relTol 0; }
    p_rgh { solver PCG; preconditioner DIC; tolerance 1e-07; relTol 0.05; }
    p_rghFinal { $p_rgh; relTol 0; }
    U { solver smoothSolver; smoother symGaussSeidel; tolerance 1e-06; relTol 0; }
    cellDisplacement { solver PCG; preconditioner DIC; tolerance 1e-06; relTol 0; }
}
PIMPLE { momentumPredictor no; nOuterCorrectors 2; nCorrectors 3; nNonOrthogonalCorrectors 0; }
relaxationFactors { equations { ".*" 1; } }
"""

_TRANSPORT = """
phases (water air);
water { transportModel Newtonian; nu 1e-06; rho 1000; }
air   { transportModel Newtonian; nu 1.48e-05; rho 1; }
sigma 0.07;
"""


def _control(end_time: float, delta_t: float, max_co: float = 0.9) -> str:
    return _H.format(cls="dictionary", obj="controlDict") + f"""
application interFoam;
startFrom startTime; startTime 0; stopAt endTime;
endTime {end_time:.6g}; deltaT {delta_t:.6g};
writeControl adjustableRunTime; writeInterval {end_time:.6g}; purgeWrite 1;
writeFormat ascii; writePrecision 8; writeCompression off;
timeFormat general; timePrecision 8; runTimeModifiable no;
adjustTimeStep yes; maxCo {max_co:.6g}; maxAlphaCo {max_co:.6g}; maxDeltaT 0.01;
"""


def wall_probe_locations(config: Sloshing3DConfig) -> dict[str, tuple[float, float, float]]:
    """Mid-wall and corner locations, half a cell from each vertical wall.

    One interfaceHeight object per location preserves the 2D parser contract.
    The first two probes are normal to the excitation; the next two measure
    out-of-plane response. Corners are reported separately.
    """
    dx = config.breadth / config.cpb
    dz = config.length / max(1, round(config.length / dx))
    b, z = config.breadth, config.length
    roll = [(dx / 2, 0, z / 2), (b - dx / 2, 0, z / 2)]
    pitch = [(b / 2, 0, dz / 2), (b / 2, 0, z - dz / 2)]
    points = (roll + pitch if config.rotation == "roll" else pitch + roll)
    points += [(dx / 2, 0, dz / 2), (b - dx / 2, 0, z - dz / 2)]
    return dict(zip((f"interfaceHeight{i}" for i in range(1, 7)), points))


def _comparison_control(config: Sloshing3DConfig, contract: BoundaryContract) -> str:
    from . import sloshing_2d_dicts as d2

    if config.use_2d_settings:
        text = d2._CONTROLDICT.split("functions\n", 1)[0]
        values = {
            "ENDTIME": config.end_time, "DELTAT": config.delta_t,
            "WRITECONTROL": "adjustableRunTime", "WRITEINTERVAL": config.end_time,
            "ADJUST": "yes", "MAXCO": config.max_co,
        }
        for key, value in values.items():
            text = text.replace(f"@{key}@", f"{value:.6g}" if isinstance(value, float) else str(value))
        text = _H.format(cls="dictionary", obj="controlDict") + text
    else:
        text = _control(config.end_time, config.delta_t, config.max_co)
    if config.function_objects:
        probes = []
        for name, xyz in wall_probe_locations(config).items():
            # Reuse the held interface-height settings and sampling stride.
            probe = d2._CONTROLDICT.split("functions\n{\n", 1)[1].split("@EXTRAFUNCTIONS@", 1)[0]
            probe = probe.replace("interfaceHeight1", name)
            for token, value in zip(("PROBEX", "PROBEZ"), (xyz[0], xyz[2])):
                probe = probe.replace(f"@{token}@", f"{value:.6g}")
            probes.append(probe.replace("@SAMPLEEVERY@", "5"))
        forces = d2.roll_moment_function_object(
            (config.breadth / 2, 0, config.length / 2), patches=contract.wall_patches,
        )
        # The forces object writes all axes; the selected component is reduced
        # by the comparison driver. Keep the shared type and settings.
        text += "functions\n{\n" + "".join(probes) + forces + "\n}\n"
    return text


def _setfields(config: Sloshing3DConfig) -> str:
    h = config.fill * config.breadth
    f = "{:.6g}".format
    return _H.format(cls="dictionary", obj="setFieldsDict") + f"""
defaultFieldValues ( volScalarFieldValue alpha.water 0 );
regions
(
    boxToCell
    {{
        box (0 0 0) ({f(config.breadth)} {f(h)} {f(config.length)});
        fieldValues ( volScalarFieldValue alpha.water 1 );
    }}
);
"""


def _decompose(n: int) -> str:
    return _H.format(cls="dictionary", obj="decomposeParDict") + f"""
numberOfSubdomains {n};
method scotch;
"""


def _write_dictionaries(case: Path, config: Sloshing3DConfig, contract: BoundaryContract) -> int:
    nx = config.cpb
    ny = max(1, round(config.height / config.breadth * config.cpb))
    nz = max(1, round(config.length / config.breadth * config.cpb))
    if config.mesh_mode == "blockMesh":
        (case / "system" / "blockMeshDict").write_text(_blockmesh(config, nx, ny, nz, contract))
    (case / "system" / "controlDict").write_text(_comparison_control(config, contract))
    from . import sloshing_2d_dicts as d2
    schemes = d2._FVSCHEMES if config.use_2d_settings else _FVSCHEMES
    solution = d2._FVSOLUTION if config.use_2d_settings else _FVSOLUTION
    (case / "system" / "fvSchemes").write_text(_H.format(cls="dictionary", obj="fvSchemes") + schemes)
    (case / "system" / "fvSolution").write_text(_H.format(cls="dictionary", obj="fvSolution") + solution)
    (case / "system" / "setFieldsDict").write_text(_setfields(config))
    if config.decompose_ranks is not None:
        (case / "system" / "decomposeParDict").write_text(
            _decompose(config.decompose_ranks)
        )
    (case / "constant" / "transportProperties").write_text(_H.format(cls="dictionary", obj="transportProperties") + _TRANSPORT)
    (case / "constant" / "g").write_text(_H.format(cls="uniformDimensionedVectorField", obj="g") + "dimensions [0 1 -2 0 0 0 0];\nvalue (0 -9.81 0);\n")
    (case / "constant" / "turbulenceProperties").write_text(_H.format(cls="dictionary", obj="turbulenceProperties") + "simulationType laminar;\n")
    axis = MotionType.YAW if config.rotation == "roll" else MotionType.ROLL
    roll = PrescribedMotion(axis, amplitude=config.roll_deg, period=config.period or _first_mode_period(config), origin=(0.5 * config.breadth, 0.0, 0.5 * config.length))
    (case / "constant" / "dynamicMeshDict").write_text(_H.format(cls="dictionary", obj="dynamicMeshDict") + "\n" + render_dynamic_mesh_dict_body(roll) + "\n")
    (case / "0" / "U").write_text(_field("U", contract))
    (case / "0" / "p_rgh").write_text(_field("p_rgh", contract))
    (case / "0" / "alpha.water").write_text(_field("alpha.water", contract))
    return nx * ny * nz


def write_sloshing_case(case: Path | str, config: Sloshing3DConfig, boundary_contract: BoundaryContract | None = None) -> int:
    """Write the deterministic synthetic case and return its cell count."""
    case_path = Path(case)
    contract = boundary_contract or BENCHMARK_CONTRACT
    _validate(config, contract)
    try:
        case_path.mkdir(parents=True)
    except FileExistsError as exc:
        raise FileExistsError(
            f"case destination already exists: {case_path}"
        ) from exc
    (case_path / "system").mkdir()
    (case_path / "constant").mkdir()
    (case_path / "0").mkdir()
    return _write_dictionaries(case_path, config, contract)
