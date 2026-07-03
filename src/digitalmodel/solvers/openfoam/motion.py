#!/usr/bin/env python3
"""
ABOUTME: Prescribed single-DOF rigid-body motion engine for OpenFOAM dynamic-mesh
cases. Generates a ``constant/dynamicMeshDict`` that drives the whole mesh with a
``solidBodyMotionFvMesh`` sinusoidal forcing — sinusoidal roll (or pitch/yaw)
rotation, or sway/surge/heave translation — for forced tank-sloshing studies.

The imposed law is a single sinusoid ``x(t) = A * sin(omega * t)``:

  * rotation  -> OpenFOAM ``oscillatingRotatingMotion`` (amplitude in DEGREES,
    Euler-angle vector about x/y/z; ``origin`` = centre of rotation),
  * translation -> OpenFOAM ``oscillatingLinearMotion`` (amplitude in METRES,
    displacement vector along x/y/z).

This is a *rigid* whole-mesh motion via the ESI ``solidBody`` motion solver
(``dynamicMotionSolverFvMesh`` + ``motionSolver solidBody``, no ``cellZone`` so
the whole mesh moves). It prescribes a rigid transform — no Laplacian point
solve — so it is robust on 2-D slab meshes (unlike the displacement-Laplacian
path that FPE'd in prior overset work). Gravity stays in the global frame, so
rotating the tank reproduces the physical oscillating body force used in
canonical forced-roll sloshing benchmarks (e.g. SPHERIC Test 10 and the ESI
``interFoam/testTubeMixer`` tutorial).

Syntax note: ESI OpenFOAM (v2312) does NOT provide the Foundation-branch
``solidBodyMotionFvMesh`` dynamicFvMesh type — the solid-body function is a
``motionSolver`` instead. This engine targets the ESI form and is verified
against OpenFOAM v2312 ``moveDynamicMesh``.

Reference: digitalmodel #658 (prescribed single-DOF tank motion engine).
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Tuple


class MotionType(Enum):
    """Single degree of freedom to force.

    Rotational DOFs use ``oscillatingRotatingMotion`` (amplitude in degrees);
    translational DOFs use ``oscillatingLinearMotion`` (amplitude in metres).
    """

    ROLL = "roll"      # rotation about x
    PITCH = "pitch"    # rotation about y
    YAW = "yaw"        # rotation about z
    SURGE = "surge"    # translation along x
    SWAY = "sway"      # translation along y
    HEAVE = "heave"    # translation along z

    @property
    def is_rotational(self) -> bool:
        return self in (MotionType.ROLL, MotionType.PITCH, MotionType.YAW)

    @property
    def axis_index(self) -> int:
        """Index (0=x, 1=y, 2=z) of the active axis for this DOF."""
        return {
            MotionType.ROLL: 0,
            MotionType.PITCH: 1,
            MotionType.YAW: 2,
            MotionType.SURGE: 0,
            MotionType.SWAY: 1,
            MotionType.HEAVE: 2,
        }[self]


Vec3 = Tuple[float, float, float]


@dataclass
class PrescribedMotion:
    """A single-DOF sinusoidal forced motion ``x(t) = A * sin(omega * t)``.

    Attributes:
        motion_type: Which DOF to force (roll/pitch/yaw or surge/sway/heave).
        amplitude: Oscillation amplitude ``A``. Degrees for rotational DOFs,
            metres for translational DOFs. Must be > 0.
        period: Forcing period ``T`` (s); ``omega = 2*pi/T``. Must be > 0.
            For a tank-sloshing sweep this is the vessel roll natural period or
            one of the target excitation periods.
        origin: Centre of rotation (m), used only for rotational DOFs. For a
            ballast tank this is typically the tank/roll rotation centre.
    """

    motion_type: MotionType
    amplitude: float
    period: float
    origin: Vec3 = (0.0, 0.0, 0.0)

    def __post_init__(self) -> None:
        if self.period <= 0.0:
            raise ValueError(f"period must be > 0, got {self.period}")
        if self.amplitude <= 0.0:
            raise ValueError(f"amplitude must be > 0, got {self.amplitude}")
        if len(self.origin) != 3:
            raise ValueError("origin must be a 3-tuple (x y z)")

    # ---- derived kinematics --------------------------------------------- #

    @property
    def omega(self) -> float:
        """Circular frequency of the forcing (rad/s)."""
        return 2.0 * math.pi / self.period

    @property
    def frequency_hz(self) -> float:
        """Forcing frequency (Hz)."""
        return 1.0 / self.period

    @property
    def amplitude_vector(self) -> Vec3:
        """Amplitude as an (x, y, z) vector on the active axis.

        Degrees for rotation, metres for translation.
        """
        v = [0.0, 0.0, 0.0]
        v[self.motion_type.axis_index] = float(self.amplitude)
        return (v[0], v[1], v[2])

    # ---- constructors --------------------------------------------------- #

    @classmethod
    def from_frequency_hz(
        cls,
        motion_type: MotionType,
        amplitude: float,
        frequency_hz: float,
        origin: Vec3 = (0.0, 0.0, 0.0),
    ) -> "PrescribedMotion":
        """Build from a frequency in Hz instead of a period."""
        if frequency_hz <= 0.0:
            raise ValueError(f"frequency_hz must be > 0, got {frequency_hz}")
        return cls(motion_type, amplitude, 1.0 / frequency_hz, origin)

    @classmethod
    def from_omega(
        cls,
        motion_type: MotionType,
        amplitude: float,
        omega: float,
        origin: Vec3 = (0.0, 0.0, 0.0),
    ) -> "PrescribedMotion":
        """Build from a circular frequency omega (rad/s)."""
        if omega <= 0.0:
            raise ValueError(f"omega must be > 0, got {omega}")
        return cls(motion_type, amplitude, 2.0 * math.pi / omega, origin)


def _fmt(x: float) -> str:
    """Format a float for an OpenFOAM dict (trim trailing zeros, keep precision)."""
    return f"{x:.10g}"


def _vec(v: Vec3) -> str:
    return f"({_fmt(v[0])} {_fmt(v[1])} {_fmt(v[2])})"


def render_dynamic_mesh_dict_body(motion: PrescribedMotion) -> str:
    """Return the ``dynamicMeshDict`` entries (no FoamFile header).

    Suitable for embedding under a caller-supplied header (e.g. the
    ``OpenFOAMCaseBuilder`` header) or for the standalone writer below.
    """
    if motion.motion_type.is_rotational:
        coeffs = (
            "solidBodyMotionFunction oscillatingRotatingMotion;\n"
            "oscillatingRotatingMotionCoeffs\n"
            "{\n"
            f"    origin      {_vec(motion.origin)};\n"
            f"    omega       {_fmt(motion.omega)};  // rad/s\n"
            f"    amplitude   {_vec(motion.amplitude_vector)};  // degrees, Euler angles\n"
            "}\n"
        )
    else:
        coeffs = (
            "solidBodyMotionFunction oscillatingLinearMotion;\n"
            "oscillatingLinearMotionCoeffs\n"
            "{\n"
            f"    amplitude   {_vec(motion.amplitude_vector)};  // metres\n"
            f"    omega       {_fmt(motion.omega)};  // rad/s\n"
            "}\n"
        )

    return (
        f"// Prescribed single-DOF forcing: {motion.motion_type.value}, "
        f"A={_fmt(motion.amplitude)} "
        f"{'deg' if motion.motion_type.is_rotational else 'm'}, "
        f"T={_fmt(motion.period)} s (omega={_fmt(motion.omega)} rad/s)\n"
        "// ESI whole-mesh rigid motion (entire mesh moves; no sub-zone).\n"
        "dynamicFvMesh    dynamicMotionSolverFvMesh;\n"
        "motionSolverLibs (fvMotionSolvers);\n"
        "motionSolver     solidBody;\n\n"
        f"{coeffs}"
    )


_FOAM_HEADER = """/*--------------------------------*- C++ -*----------------------------------*\\
| =========                 |                                                 |
| \\\\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox           |
|  \\\\    /   O peration     |                                                 |
|   \\\\  /    A nd           | digitalmodel prescribed-motion engine (#658)    |
|    \\\\/     M anipulation  |                                                 |
\\*---------------------------------------------------------------------------*/
FoamFile
{
    version     2.0;
    format      ascii;
    class       dictionary;
    object      dynamicMeshDict;
}
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

"""


def render_dynamic_mesh_dict(motion: PrescribedMotion) -> str:
    """Return a complete ``dynamicMeshDict`` file (FoamFile header + entries)."""
    return _FOAM_HEADER + render_dynamic_mesh_dict_body(motion)


def write_dynamic_mesh_dict(motion: PrescribedMotion, constant_dir: Path) -> Path:
    """Write ``constant/dynamicMeshDict`` for the given motion.

    Args:
        motion: The prescribed single-DOF forcing.
        constant_dir: The case ``constant/`` directory (created if needed).

    Returns:
        Path to the written ``dynamicMeshDict``.
    """
    constant_dir = Path(constant_dir)
    constant_dir.mkdir(parents=True, exist_ok=True)
    target = constant_dir / "dynamicMeshDict"
    target.write_text(render_dynamic_mesh_dict(motion))
    return target
