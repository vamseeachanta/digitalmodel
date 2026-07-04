#!/usr/bin/env python3
"""
ABOUTME: Marine CFD validation case #1302 — wave-excited floating body
(overInterDyMFoam, 2D overset). A half-density box floats at mid-tank in the
verified #1170 regular wave (StokesII, H=0.05 m, T=3 s, d=0.4 m) constrained
to pure heave (#1169 pattern). In the long-wavelength limit (lambda/B ~ 29,
T >> heave natural period) the body is a wave follower: heave RAO -> 1.
First wave-body interaction case of the suite — the direct precursor to CFD
RAO spot-checks against the OrcaWave/AQWA diffraction pipeline.

The body rides on a rigid *overset* component mesh (dynamicOversetFvMesh)
instead of morphing the background: the deforming-mesh attempt collapsed a
cell at first wave arrival (deltaCoeffs FPE — see the #1302 diagnosis), which
the overset approach eliminates structurally because no cell ever deforms.

Source / citation
-----------------
- OpenFOAM tutorials (ESI v2312):
  $FOAM_TUTORIALS/multiphase/overInterDyMFoam/floatingBody — overset
  mesh-merge/zoneID recipe, overset fvSchemes/fvSolution settings;
  $FOAM_TUTORIALS/multiphase/interFoam/laminar/waves/stokesII — wave
  generation/absorption (as verified in #1170);
  $FOAM_TUTORIALS/multiphase/overInterDyMFoam/twoSquaresOutDomain — 2D
  (empty-patch) overset precedent.
- Long-wave limit: a freely floating small body follows the free surface;
  heave RAO -> 1 as (kB -> 0, omega/omega_n -> 0). Newman, "Marine
  Hydrodynamics" (1977), ch. 6 (quasi-static response); any seakeeping text.
- Incident/reflected split at the upstream gauge array: Goda & Suzuki (1976)
  style least squares (reused from the verified #1170 module).

Validation gates (#1302): heave RAO within +/-15% of 1.0 (incident amplitude
from the upstream gauge split); mean draft within 5% of Archimedes; steady
heave periodic at the wave period within 5%.

NOTE: the ``sixDoFRigidBodyState`` function object refCasts the mesh to
``dynamicMotionSolverFvMesh`` and FATALs under ``dynamicOversetFvMesh`` —
heave history is parsed from the sixDoF ``report on`` log lines instead
(per-timestep dedupe, keeping the last outer-corrector value).
"""

from __future__ import annotations

import json
import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Tuple

from .wave_tank import GRAVITY, dispersion_wavenumber

RHO_WATER = 1000.0  # matches the verified #1170 tank transportProperties

# Gates (#1302). RAO band reflects the long-wave limit, not a digitized
# curve: at lambda/B ~ 29 the quasi-static RAO deviates from 1 by O(kB/2,
# (omega/omega_n)^2) ~ a few %, well inside +/-15%.
RAO_TOLERANCE = 0.15
DRAFT_TOLERANCE = 0.05
PERIOD_TOLERANCE = 0.05


# ---------------------------------------------------------------------------
# Pure response helpers (no OpenFOAM dependency)
# ---------------------------------------------------------------------------


def harmonic_amplitude(
    t: List[float] | Any, y: List[float] | Any, period: float
) -> Tuple[float, float, float]:
    """Least-squares single-harmonic fit ``y ~ mean + A cos(wt) + B sin(wt)``.

    Returns:
        ``(mean, amplitude, phase)`` with ``amplitude = sqrt(A^2 + B^2)``.

    Raises:
        ValueError: If period is not positive or fewer than 4 samples.
    """
    import numpy as np

    if period <= 0.0:
        raise ValueError(f"period must be positive, got {period}")
    t_arr = np.asarray(t, dtype=float)
    y_arr = np.asarray(y, dtype=float)
    if t_arr.size < 4 or t_arr.size != y_arr.size:
        raise ValueError("need >= 4 matching (t, y) samples")
    om = 2.0 * math.pi / period
    m = np.column_stack(
        [np.ones_like(t_arr), np.cos(om * t_arr), np.sin(om * t_arr)]
    )
    coef, *_ = np.linalg.lstsq(m, y_arr, rcond=None)
    mean, a, b = (float(c) for c in coef)
    return mean, math.hypot(a, b), math.atan2(-b, a)


def response_period(t: List[float] | Any, y: List[float] | Any) -> float:
    """Dominant period from zero-upcrossings of ``y`` about its mean (s).

    Returns NaN when fewer than two upcrossings are found.
    """
    import numpy as np

    t_arr = np.asarray(t, dtype=float)
    y_arr = np.asarray(y, dtype=float)
    z = y_arr - y_arr.mean()
    idx = np.where((z[:-1] < 0) & (z[1:] >= 0))[0]
    if len(idx) < 2:
        return float("nan")
    zc = np.array(
        [t_arr[i] - z[i] * (t_arr[i + 1] - t_arr[i]) / (z[i + 1] - z[i])
         for i in idx]
    )
    return float(np.mean(np.diff(zc)))


# ---------------------------------------------------------------------------
# Case configuration
# ---------------------------------------------------------------------------


@dataclass
class WaveExcitedBodyConfig:
    """Config for the wave-excited floating body (2D overset) case.

    Background = the verified #1170 NWT (x-z plane, one empty cell in y)
    with the tank top raised so the overset fringe stays interior; component
    = a block around the box, hole carved by topoSet + subsetMesh, rigid
    overset motion, heave-only sixDoF. The default mesh/duration is the
    *fast* variant; the verified reference report refines the background
    (``nx=375, nz=49``) with the component unchanged — the RAO hinges on
    incident-wave fidelity, while refining the component only pins the
    adjustable time step on spurious air-side velocities in the fine
    near-body cells (measured: component refinement halves dt for no
    gate-relevant gain).

    Attributes:
        wave_height: Input regular wave height H (m).
        wave_period: Input wave period T (s).
        depth: Still-water depth d (m).
        tank_length: Tank length (m).
        tank_height: Tank height (m) — 0.7 m (vs 0.55 in #1170) so the
            component top stays clear of the atmosphere.
        nx, nz: Background mesh cells. ``nz`` must keep the waterline
            ``depth`` on a cell face (VOF lesson from #1165).
        body_beam: Body beam B (m, x extent).
        body_height: Body height (m, z extent); with half density the
            Archimedes draft is half of it, so the body floats with its
            centre exactly at the waterline.
        body_density: Body density (kg/m^3).
        body_x: Body centre x (m) — in the established wave region.
        comp_margin_x, comp_z0, comp_z1: Component-mesh extents around the
            body (margins sized so the fringe has interior donors over the
            full heave range).
        comp_nx, comp_nz: Component mesh cells.
        slab_thickness: 2D slab thickness in y (m, one cell).
        end_time: Physical end time (s).
        steady_window: (t0, t1) analysis window after the wave field and
            body response are established at the body.
        gauges_x: Wave-gauge x positions (m); gauges in x = 8..12 feed the
            incident/reflected split (>= 1 wavelength from the wavemaker,
            upstream of the component region).
        name: Case root directory name (contains background/ + body/).
    """

    wave_height: float = 0.05
    wave_period: float = 3.0
    depth: float = 0.4
    tank_length: float = 20.0
    tank_height: float = 0.7
    nx: int = 250
    nz: int = 35
    body_beam: float = 0.2
    body_height: float = 0.2
    body_density: float = 500.0
    body_x: float = 13.0
    comp_margin_x: float = 0.4
    comp_z0: float = 0.2
    comp_z1: float = 0.62
    comp_nx: int = 50
    comp_nz: int = 42
    slab_thickness: float = 0.04
    initial_heave_offset: float = 0.0
    end_time: float = 34.0
    write_interval: float = 1.0
    steady_window: Tuple[float, float] = (22.0, 34.0)
    gauges_x: Tuple[float, ...] = (
        2.0, 8.0, 8.8, 9.6, 10.4, 11.2, 12.0, 18.0,
    )
    name: str = "validation_wave_excited_body"

    # -- wave properties (delegated to the #1170 helpers) ------------------
    @property
    def wavenumber(self) -> float:
        """Dispersion wavenumber (rad/m)."""
        return dispersion_wavenumber(self.wave_period, self.depth)

    @property
    def wavelength(self) -> float:
        """Wavelength (m)."""
        return 2.0 * math.pi / self.wavenumber

    @property
    def lambda_over_beam(self) -> float:
        """Long-wave parameter lambda/B (~29 for the defaults)."""
        return self.wavelength / self.body_beam

    # -- body properties ----------------------------------------------------
    @property
    def mass(self) -> float:
        """Body mass (kg) for the one-cell slab."""
        return (self.body_density * self.body_beam * self.slab_thickness
                * self.body_height)

    @property
    def waterplane_area(self) -> float:
        """Waterplane area B * slab thickness (m^2)."""
        return self.body_beam * self.slab_thickness

    @property
    def draft(self) -> float:
        """Archimedes equilibrium draft (m)."""
        return self.mass / (RHO_WATER * self.waterplane_area)

    @property
    def equilibrium_com_height(self) -> float:
        """CoM height when floating at the Archimedes draft (m)."""
        return self.depth - self.draft + self.body_height / 2.0

    @property
    def centre_of_mass(self) -> Tuple[float, float, float]:
        """Initial centre of mass.

        At hydrostatic equilibrium by default; raised by
        ``initial_heave_offset`` for a still-water free-decay test (release
        from a displaced position — #1332).
        """
        return (self.body_x, self.slab_thickness / 2.0,
                self.equilibrium_com_height + self.initial_heave_offset)

    @property
    def moment_of_inertia(self) -> Tuple[float, float, float]:
        """Cuboid principal moments of inertia (kg m^2)."""
        m = self.mass
        lx, ly, lz = self.body_beam, self.slab_thickness, self.body_height
        return (
            m / 12.0 * (ly**2 + lz**2),
            m / 12.0 * (lx**2 + lz**2),
            m / 12.0 * (lx**2 + ly**2),
        )

    @property
    def heave_natural_period(self) -> float:
        """Hydrostatic heave natural period, no added mass (s).

        With added mass Ca ~ O(1) the true period is ~sqrt(1+Ca) larger;
        either way it is << the wave period, hence the quasi-static limit.
        """
        k = RHO_WATER * GRAVITY * self.waterplane_area
        return 2.0 * math.pi * math.sqrt(self.mass / k)

    # -- derived geometry ---------------------------------------------------
    @property
    def body_extent(self) -> Tuple[float, float, float, float]:
        """Body box ``(x0, x1, z0, z1)`` at the Archimedes draft.

        Raised by ``initial_heave_offset`` (0 by default) for the free-decay
        test — the body mesh, component mesh and centre of mass all shift
        together so the body is released from a displaced position in still
        water at the unchanged depth.
        """
        x0 = self.body_x - self.body_beam / 2.0
        z0 = self.depth - self.draft + self.initial_heave_offset
        return (x0, x0 + self.body_beam, z0, z0 + self.body_height)

    @property
    def comp_extent(self) -> Tuple[float, float, float, float]:
        """Component mesh block ``(x0, x1, z0, z1)``."""
        return (
            self.body_x - self.body_beam / 2.0 - self.comp_margin_x,
            self.body_x + self.body_beam / 2.0 + self.comp_margin_x,
            self.comp_z0 + self.initial_heave_offset,
            self.comp_z1 + self.initial_heave_offset,
        )


_HDR = "FoamFile {{ version 2.0; format ascii; class {cls}; object {obj}; }}\n"

_BG_BLOCKMESHDICT = """
// 2D NWT background (x-z plane, one empty cell in y): verified #1170
// numerics with the tank top raised so the overset fringe stays interior.
scale 1;
vertices
(
    (0 0 0) (@LX@ 0 0) (@LX@ @YT@ 0) (0 @YT@ 0)
    (0 0 @LZ@) (@LX@ 0 @LZ@) (@LX@ @YT@ @LZ@) (0 @YT@ @LZ@)
);
blocks ( hex (0 1 2 3 4 5 6 7) (@NX@ 1 @NZ@) simpleGrading (1 1 1) );
edges ();
boundary
(
    // Dummy patch to trigger overset interpolation before any other bcs
    oversetPatch { type overset; faces (); }
    inlet  { type patch; faces ( (0 4 7 3) ); }
    outlet { type patch; faces ( (1 5 6 2) ); }
    ground { type wall;  faces ( (0 1 2 3) ); }
    top    { type patch; faces ( (4 5 6 7) ); }
    sides  { type empty; faces ( (0 1 5 4) (3 2 6 7) ); }
);
mergePatchPairs ();
"""

_COMP_BLOCKMESHDICT = """
// Overset component block around the floating box (rigid motion, no
// morphing). The body hole is carved by topoSet + subsetMesh.
scale 1;
vertices
(
    (@X0@ 0 @Z0@) (@X1@ 0 @Z0@) (@X1@ @YT@ @Z0@) (@X0@ @YT@ @Z0@)
    (@X0@ 0 @Z1@) (@X1@ 0 @Z1@) (@X1@ @YT@ @Z1@) (@X0@ @YT@ @Z1@)
);
blocks ( hex (0 1 2 3 4 5 6 7) (@CNX@ 1 @CNZ@) simpleGrading (1 1 1) );
edges ();
boundary
(
    oversetBoundary
    {
        type overset;
        faces ( (0 4 7 3) (1 5 6 2) (0 1 2 3) (4 5 6 7) );
    }
    bodyFrontBack { type empty; faces ( (0 1 5 4) (3 2 6 7) ); }
    floatingObject { type wall; faces (); }
);
mergePatchPairs ();
"""

_COMP_TOPOSETDICT = """
actions
(
    {
        name    c0;
        type    cellSet;
        action  new;
        source  boxToCell;
        box     (@BX0@ -1 @BZ0@) (@BX1@ 1 @BZ1@);
    }
    { name c0; type cellSet; action invert; }
);
"""

_COMP_CONTROLDICT = """
// Mesh-prep-only sub-case: blockMesh -> topoSet -> subsetMesh. The
// application entry satisfies the runner contract; nothing is solved here.
application     blockMesh;
startFrom       startTime;
startTime       0;
stopAt          endTime;
endTime         1;
deltaT          1;
writeControl    timeStep;
writeInterval   1;
"""

_COMP_FVSCHEMES = """
ddtSchemes { default Euler; }
gradSchemes { default Gauss linear; }
divSchemes { default none; }
laplacianSchemes { default Gauss linear corrected; }
interpolationSchemes { default linear; }
snGradSchemes { default corrected; }
"""

# zones on the merged mesh: c0 = background region, c1 = component region
_BG_TOPOSETDICT = """
actions
(
    {
        name    c0;
        type    cellSet;
        action  new;
        source  regionToCell;
        insidePoints ((@PX@ @PY@ @PZ@));
    }
    { name c1; type cellSet; action new; source cellToCell; set c0; }
    { name c1; type cellSet; action invert; }
);
"""

_BG_SETFIELDSDICT = """
defaultFieldValues ( volScalarFieldValue alpha.water 0 volScalarFieldValue zoneID 123 );
regions
(
    boxToCell
    {
        box (-1 -1 -1) (@LXP@ 1 @DEPTH@);
        fieldValues ( volScalarFieldValue alpha.water 1 );
    }
    cellToCell { set c0; fieldValues ( volScalarFieldValue zoneID 0 ); }
    cellToCell { set c1; fieldValues ( volScalarFieldValue zoneID 1 ); }
);
"""

_BG_CONTROLDICT = """
libs            (waveModels);
application     overInterDyMFoam;
startFrom       startTime;
startTime       0;
stopAt          endTime;
endTime         @ENDTIME@;
deltaT          0.005;
writeControl    adjustable;
writeInterval   @WRITEINTERVAL@;
purgeWrite      0;
writeFormat     ascii;
writePrecision  8;
writeCompression off;
timeFormat      general;
timePrecision   6;
runTimeModifiable yes;
adjustTimeStep  yes;
maxCo           0.65;
maxAlphaCo      0.65;
maxDeltaT       0.05;
functions
{
    // NOTE: no sixDoFRigidBodyState FO — it refCasts the mesh to
    // dynamicMotionSolverFvMesh and FATALs under dynamicOversetFvMesh.
    // Heave comes from the sixDoF 'report on' log lines instead.
    waveGauges
    {
        type            interfaceHeight;
        libs            (fieldFunctionObjects);
        alpha           alpha.water;
        writeControl    timeStep;
        writeInterval   1;
        locations       ( @GAUGES@ );
    }
}
"""

# #1170 wave-fidelity div schemes + the overset blocks from the v2312
# floatingBody overset tutorial.
_BG_FVSCHEMES = """
ddtSchemes      { default Euler; }
gradSchemes     { default Gauss linear; }
divSchemes
{
    div(rhoPhi,U)   Gauss linearUpwind grad(U);
    div(U)          Gauss linear;
    div(phi,alpha)  Gauss vanLeer;
    div(phirb,alpha) Gauss linear;
    div(((rho*nuEff)*dev2(T(grad(U))))) Gauss linear;
}
laplacianSchemes { default Gauss linear corrected; }
interpolationSchemes { default linear; }
snGradSchemes   { default corrected; }
oversetInterpolation
{
    method              inverseDistance;
    holeLayers          6;
    useLayer            3;
}
oversetInterpolationSuppressed
{
    grad(p_rgh);
    surfaceIntegrate(phiHbyA);
}
fluxRequired
{
    default         no;
    p_rgh;
    pcorr;
    alpha.water;
}
"""

_BG_FVSOLUTION = """
solvers
{
    "cellDisplacement.*"
    {
        solver          PCG;
        preconditioner  DIC;
        tolerance       1e-06;
        relTol          0;
        maxIter         100;
    }
    "alpha.water.*"
    {
        nAlphaCorr      3;
        nAlphaSubCycles 2;
        cAlpha          1;
        icAlpha         0;
        MULESCorr       yes;
        nLimiterIter    5;
        alphaApplyPrevCorr no;
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-8;
        relTol          0;
    }
    "pcorr.*" { solver PCG; preconditioner DIC; tolerance 1e-9; relTol 0; }
    p_rgh { solver PBiCGStab; preconditioner DILU; tolerance 1e-9; relTol 0.01; }
    p_rghFinal { $p_rgh; relTol 0; }
    "(U|k|omega|epsilon).*"
    {
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-08;
        relTol          0;
    }
}
PIMPLE
{
    momentumPredictor   no;
    nOuterCorrectors    2;
    nCorrectors         2;
    nNonOrthogonalCorrectors 0;
    oversetAdjustPhi    no;
}
relaxationFactors { equations { ".*" 1; } }
"""

_WAVEPROPERTIES = """
inlet
{
    alpha           alpha.water;
    waveModel       StokesII;
    nPaddle         1;
    waveHeight      @H@;
    waveAngle       0.0;
    rampTime        3.0;
    activeAbsorption yes;
    wavePeriod      @T@;
}
outlet
{
    alpha           alpha.water;
    waveModel       shallowWaterAbsorption;
    nPaddle         1;
}
"""

_DYNAMICMESHDICT = """
motionSolverLibs    (sixDoFRigidBodyMotion);
dynamicFvMesh       dynamicOversetFvMesh;
solver              sixDoFRigidBodyMotion;

sixDoFRigidBodyMotionCoeffs
{
    patches         (floatingObject);
    innerDistance   100.0;   // whole component mesh moves rigidly
    outerDistance   101.0;

    centreOfMass    (@CMX@ @CMY@ @CMZ@);
    mass            @MASS@;
    momentOfInertia (@IXX@ @IYY@ @IZZ@);
    rhoInf          1;
    report          on;
    accelerationRelaxation 0.7;

    solver { type Newmark; }

    constraints
    {
        // Pure heave: translation along z only, no rotation.
        fixedLine
        {
            sixDoFRigidBodyMotionConstraint line;
            centreOfRotation (@CMX@ @CMY@ @CMZ@);
            direction        (0 0 1);
        }
        fixedOrientation
        {
            sixDoFRigidBodyMotionConstraint orientation;
        }
    }
}
"""

_TRANSPORT = """
phases          (water air);
water { transportModel Newtonian; nu 1e-06; rho 1000; }
air   { transportModel Newtonian; nu 1.48e-05; rho 1; }
sigma           0.07;
"""

_GRAVITY_DICT = """
dimensions      [0 1 -2 0 0 0 0];
value           (0 0 -9.81);
"""

# 0/ fields — explicit entry for every merged-mesh patch; overset patches
# get 'type overset' for advected fields and patchType-overridden physical
# BCs for p_rgh / pointDisplacement (v2312 floatingBody overset tutorial).
_FIELDS = {
    ("volVectorField", "U"): """
dimensions      [0 1 -1 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    oversetPatch    { type overset; }
    oversetBoundary { type overset; }
    inlet  { type waveVelocity; value uniform (0 0 0); }
    outlet { type waveVelocity; value uniform (0 0 0); }
    ground { type fixedValue; value uniform (0 0 0); }
    top    { type pressureInletOutletVelocity; value uniform (0 0 0); }
    sides  { type empty; }
    bodyFrontBack { type empty; }
    floatingObject { type movingWallVelocity; value uniform (0 0 0); }
}
""",
    ("volScalarField", "alpha.water"): """
dimensions      [0 0 0 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    oversetPatch    { type overset; }
    oversetBoundary { type overset; }
    inlet  { type waveAlpha; value uniform 0; }
    outlet { type zeroGradient; }
    ground { type zeroGradient; }
    top    { type inletOutlet; inletValue uniform 0; value uniform 0; }
    sides  { type empty; }
    bodyFrontBack { type empty; }
    floatingObject { type zeroGradient; }
}
""",
    ("volScalarField", "p_rgh"): """
dimensions      [1 -1 -2 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    oversetPatch    { type overset; }
    oversetBoundary { patchType overset; type fixedFluxPressure; }
    inlet  { type fixedFluxPressure; value uniform 0; }
    outlet { type fixedFluxPressure; value uniform 0; }
    ground { type fixedFluxPressure; value uniform 0; }
    top
    {
        type totalPressure; rho rho; psi none; gamma 1;
        p0 uniform 0; value uniform 0;
    }
    sides  { type empty; }
    bodyFrontBack { type empty; }
    floatingObject { type fixedFluxPressure; value uniform 0; }
}
""",
    ("pointVectorField", "pointDisplacement"): """
dimensions      [0 1 0 0 0 0 0];
internalField   uniform (0 0 0);
boundaryField
{
    oversetPatch    { patchType overset; type zeroGradient; }
    oversetBoundary { patchType overset; type zeroGradient; }
    inlet  { type fixedValue; value uniform (0 0 0); }
    outlet { type fixedValue; value uniform (0 0 0); }
    ground { type fixedValue; value uniform (0 0 0); }
    top    { type fixedValue; value uniform (0 0 0); }
    sides  { type empty; }
    bodyFrontBack { type empty; }
    floatingObject { type calculated; value uniform (0 0 0); }
}
""",
    ("volScalarField", "zoneID"): """
dimensions      [0 0 0 0 0 0 0];
internalField   uniform 0;
boundaryField
{
    oversetPatch    { patchType overset; type zeroGradient; }
    oversetBoundary { patchType overset; type zeroGradient; }
    inlet  { type zeroGradient; }
    outlet { type zeroGradient; }
    ground { type zeroGradient; }
    top    { type zeroGradient; }
    sides  { type empty; }
    bodyFrontBack { type empty; }
    floatingObject { type zeroGradient; }
}
""",
}


def build_wave_excited_body_case(
    config: WaveExcitedBodyConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the wave-excited floating body case (background + component).

    Writes ``<name>/background`` (the case that runs ``overInterDyMFoam``)
    and ``<name>/body`` (the component sub-case whose mesh is merged in).
    Run sequence (as driven by :class:`OpenFOAMRunner`):

    1. body:  ``blockMesh -> topoSet -> subsetMesh -overwrite c0 -patch
       floatingObject`` (``run_solver=False``)
    2. background: ``blockMesh -> mergeMeshes . ../body -overwrite ->
       topoSet -> setFields -> overInterDyMFoam``
       (``merge_meshes_source="../body", run_topo_set=True,
       run_set_fields=True``)

    Returns:
        Path to the case *root* (parent of ``background/`` and ``body/``).
    """
    config = config or WaveExcitedBodyConfig()
    root = Path(parent_dir) / config.name
    bg, body = root / "background", root / "body"
    for case in (bg, body):
        for sub in ("system", "constant", "0"):
            (case / sub).mkdir(parents=True, exist_ok=True)

    fmt = "{:.6g}".format
    bx0, bx1, bz0, bz1 = config.body_extent
    cx0, cx1, cz0, cz1 = config.comp_extent
    cm = config.centre_of_mass
    moi = config.moment_of_inertia

    # ---- component sub-case ----
    comp_mesh = (
        _COMP_BLOCKMESHDICT
        .replace("@X0@", fmt(cx0)).replace("@X1@", fmt(cx1))
        .replace("@Z0@", fmt(cz0)).replace("@Z1@", fmt(cz1))
        .replace("@YT@", fmt(config.slab_thickness))
        .replace("@CNX@", str(config.comp_nx))
        .replace("@CNZ@", str(config.comp_nz))
    )
    comp_topo = (
        _COMP_TOPOSETDICT
        .replace("@BX0@", fmt(bx0)).replace("@BX1@", fmt(bx1))
        .replace("@BZ0@", fmt(bz0)).replace("@BZ1@", fmt(bz1))
    )
    bsys = body / "system"
    (bsys / "blockMeshDict").write_text(_HDR.format(cls="dictionary", obj="blockMeshDict") + comp_mesh)
    (bsys / "topoSetDict").write_text(_HDR.format(cls="dictionary", obj="topoSetDict") + comp_topo)
    (bsys / "controlDict").write_text(_HDR.format(cls="dictionary", obj="controlDict") + _COMP_CONTROLDICT)
    (bsys / "fvSchemes").write_text(_HDR.format(cls="dictionary", obj="fvSchemes") + _COMP_FVSCHEMES)
    (bsys / "fvSolution").write_text(_HDR.format(cls="dictionary", obj="fvSolution") + "\n")

    # ---- background case ----
    bg_mesh = (
        _BG_BLOCKMESHDICT
        .replace("@LX@", fmt(config.tank_length))
        .replace("@LZ@", fmt(config.tank_height))
        .replace("@YT@", fmt(config.slab_thickness))
        .replace("@NX@", str(config.nx))
        .replace("@NZ@", str(config.nz))
    )
    # inside point for the background region: near the inlet bottom, well
    # clear of the component block
    bg_topo = (
        _BG_TOPOSETDICT
        .replace("@PX@", fmt(min(1.0, cx0 / 2.0)))
        .replace("@PY@", fmt(config.slab_thickness / 2.0))
        .replace("@PZ@", fmt(config.depth / 8.0))
    )
    setfields = (
        _BG_SETFIELDSDICT
        .replace("@LXP@", fmt(config.tank_length + 1))
        .replace("@DEPTH@", fmt(config.depth))
    )
    gauges = " ".join(
        f"({fmt(x)} {fmt(config.slab_thickness / 2.0)} 0.0)"
        for x in config.gauges_x
    )
    control = (
        _BG_CONTROLDICT
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@WRITEINTERVAL@", fmt(config.write_interval))
        .replace("@GAUGES@", gauges)
    )
    waveprops = (
        _WAVEPROPERTIES
        .replace("@H@", fmt(config.wave_height))
        .replace("@T@", fmt(config.wave_period))
    )
    dyn = (
        _DYNAMICMESHDICT
        .replace("@CMX@", fmt(cm[0])).replace("@CMY@", fmt(cm[1]))
        .replace("@CMZ@", fmt(cm[2]))
        .replace("@MASS@", fmt(config.mass))
        .replace("@IXX@", fmt(moi[0])).replace("@IYY@", fmt(moi[1]))
        .replace("@IZZ@", fmt(moi[2]))
    )

    sysd, constd, zerod = bg / "system", bg / "constant", bg / "0"
    (sysd / "blockMeshDict").write_text(_HDR.format(cls="dictionary", obj="blockMeshDict") + bg_mesh)
    (sysd / "topoSetDict").write_text(_HDR.format(cls="dictionary", obj="topoSetDict") + bg_topo)
    (sysd / "setFieldsDict").write_text(_HDR.format(cls="dictionary", obj="setFieldsDict") + setfields)
    (sysd / "controlDict").write_text(_HDR.format(cls="dictionary", obj="controlDict") + control)
    (sysd / "fvSchemes").write_text(_HDR.format(cls="dictionary", obj="fvSchemes") + _BG_FVSCHEMES)
    (sysd / "fvSolution").write_text(_HDR.format(cls="dictionary", obj="fvSolution") + _BG_FVSOLUTION)
    (constd / "waveProperties").write_text(_HDR.format(cls="dictionary", obj="waveProperties") + waveprops)
    (constd / "dynamicMeshDict").write_text(_HDR.format(cls="dictionary", obj="dynamicMeshDict") + dyn)
    (constd / "transportProperties").write_text(_HDR.format(cls="dictionary", obj="transportProperties") + _TRANSPORT)
    (constd / "g").write_text(_HDR.format(cls="uniformDimensionedVectorField", obj="g") + _GRAVITY_DICT)
    (constd / "turbulenceProperties").write_text(_HDR.format(cls="dictionary", obj="turbulenceProperties") + "simulationType laminar;\n")
    for (cls, name), body_txt in _FIELDS.items():
        (zerod / name).write_text(_HDR.format(cls=cls, obj=name) + body_txt)

    (root / "provenance.json").write_text(
        json.dumps(_provenance(config), indent=2) + "\n"
    )
    return root


# ---------------------------------------------------------------------------
# Solved-case extraction
# ---------------------------------------------------------------------------


def incident_wave_split(
    case_dir: Path | str,
    config: WaveExcitedBodyConfig | None = None,
    window: Tuple[float, float] | None = None,
    split_x: Tuple[float, float] = (7.9, 12.9),
    level_tolerance: float = 0.05,
) -> Dict[str, Any]:
    """Incident/reflected split over the upstream gauges, broken-gauge safe.

    An ``interfaceHeight`` gauge whose sample ray lands *bit-exactly on a
    cell face* double-counts cell columns and reports a physically
    impossible mean water level (observed: 0.78 m and 0.47 m for the same
    x = 8.8 m gauge on two meshes of a 0.4 m-deep tank). Gauges whose mean
    level over the window deviates from the still-water depth by more than
    ``level_tolerance`` (relative) are excluded before the Goda-Suzuki
    least-squares split; the exclusions are reported, not hidden.

    Returns:
        Dict with ``incident_amplitude``, ``reflection_kr``, ``k_measured``,
        ``k_error``, per-gauge records and the excluded gauge list.

    Raises:
        RuntimeError: If no gauge output exists or fewer than 3 clean
            gauges remain in ``split_x``.
    """
    import numpy as np

    config = config or WaveExcitedBodyConfig()
    case_dir = Path(case_dir)
    om = 2.0 * math.pi / config.wave_period
    k_th = config.wavenumber
    w0, w1 = window or config.steady_window

    files = sorted(
        case_dir.glob("postProcessing/waveGauges/*/height.dat"),
        key=lambda q: float(q.parent.name),
    )
    if not files:
        raise RuntimeError(f"no waveGauges output under {case_dir}")
    raw = np.vstack([np.loadtxt(f, comments="#") for f in files])
    t = raw[:, 0]
    ncol = raw.shape[1] - 1
    ng = len(config.gauges_x)
    if ncol == ng:
        levels = raw[:, 1:]
    elif ncol == 2 * ng:
        levels = raw[:, 1::2]
    else:
        raise RuntimeError(f"unexpected gauge column count {ncol} for {ng} gauges")
    m = (t >= w0) & (t <= w1)
    t, levels = t[m], levels[m]

    gauges: List[Dict[str, Any]] = []
    excluded: List[float] = []
    for j, xg in enumerate(config.gauges_x):
        lvl = levels[:, j]
        mean_level = float(lvl.mean())
        healthy = abs(mean_level - config.depth) <= level_tolerance * config.depth
        e = lvl - mean_level
        amp = np.trapz(e * np.exp(-1j * om * t), t) * 2 / (t[-1] - t[0])
        gauges.append({
            "x": xg, "mean_level": mean_level, "healthy": healthy,
            "H": float(2.0 * abs(amp)), "amp": complex(amp),
            "phase": float(np.angle(amp)),
        })
        if not healthy:
            excluded.append(xg)

    arr = [g for g in gauges if split_x[0] <= g["x"] <= split_x[1] and g["healthy"]]
    if len(arr) < 3:
        raise RuntimeError(
            f"fewer than 3 healthy gauges in x = {split_x} "
            f"(excluded: {excluded})"
        )
    xs = np.array([g["x"] for g in arr])
    phs = np.unwrap([g["phase"] for g in arr])
    k_meas = float(-np.polyfit(xs, phs, 1)[0])
    from .wave_tank import reflection_coefficient
    kr, a_i, _a_r = reflection_coefficient(xs, [g["amp"] for g in arr], k_th)

    return {
        "incident_amplitude": float(a_i),
        "reflection_kr": float(kr),
        "k_measured": k_meas,
        "k_error": float((k_meas - k_th) / k_th),
        "gauges": [{k: v for k, v in g.items() if k != "amp"} for g in gauges],
        "excluded_gauges_x": excluded,
        "window": [float(w0), float(w1)],
    }


def extract_heave_from_log(case_dir: Path | str,
                           log_name: str = "log.overInterDyMFoam",
                           ) -> Tuple[List[float], List[float]]:
    """Heave history ``(t, z_cm)`` from the sixDoF ``report on`` log lines.

    ``sixDoFRigidBodyState`` cannot be used under ``dynamicOversetFvMesh``
    (it refCasts to ``dynamicMotionSolverFvMesh``), so the CoM history is
    parsed from the solver log, keeping the *last* outer-corrector value per
    time step (same dedupe as the #1169 fallback).
    """
    case_dir = Path(case_dir)
    log = case_dir / log_name
    if not log.is_file():
        candidates = sorted(case_dir.glob("log.*Foam"))
        if not candidates:
            raise RuntimeError(f"no solver log found in {case_dir}")
        log = candidates[0]
    t: List[float] = []
    cz: List[float] = []
    tc = None
    for line in log.read_text(errors="replace").splitlines():
        if line.startswith("Time = "):
            tc = float(line.split()[-1].rstrip("s"))
        mm = re.search(
            r"Centre of mass: \(([-\d.eE+]+) ([-\d.eE+]+) ([-\d.eE+]+)\)", line
        )
        if mm and tc is not None:
            if t and t[-1] == tc:
                cz[-1] = float(mm.group(3))
            else:
                t.append(tc)
                cz.append(float(mm.group(3)))
    if not t:
        raise RuntimeError(f"no 'Centre of mass' report lines in {log}")
    return t, cz


def analyze_excited_heave(
    t: List[float],
    z_cm: List[float],
    incident_amplitude: float,
    config: WaveExcitedBodyConfig | None = None,
) -> Dict[str, Any]:
    """Wave-excited response metrics over the steady window.

    Args:
        t, z_cm: Heave (CoM height) history from the solver log.
        incident_amplitude: Incident wave amplitude ``a_i`` (m) from the
            upstream gauge split (``wave_tank.extract_wave_quality`` /
            ``reflection_coefficient``).
        config: Case configuration (defaults to the fast variant).

    Returns:
        Dict with the mean draft error, heave amplitude, RAO
        (``amplitude / a_i``), response period and gate booleans.
    """
    import numpy as np

    config = config or WaveExcitedBodyConfig()
    if incident_amplitude <= 0.0:
        raise ValueError("incident_amplitude must be positive")
    t_arr = np.asarray(t, dtype=float)
    z_arr = np.asarray(z_cm, dtype=float)
    w0, w1 = config.steady_window
    m = (t_arr >= w0) & (t_arr <= w1)
    if int(m.sum()) < 16:
        raise RuntimeError(
            f"too few samples in steady window {config.steady_window}"
        )
    ts, zs = t_arr[m], z_arr[m]

    mean_z, amp, _ = harmonic_amplitude(ts, zs, config.wave_period)
    period = response_period(ts, zs)

    draft_meas = config.draft + (config.equilibrium_com_height - mean_z)
    draft_err = (draft_meas - config.draft) / config.draft
    rao = amp / incident_amplitude
    period_err = (period - config.wave_period) / config.wave_period

    return {
        "z_cm_mean": float(mean_z),
        "heave_amplitude": float(amp),
        "incident_amplitude": float(incident_amplitude),
        "rao": float(rao),
        "rao_error": float(rao - 1.0),
        "draft_measured": float(draft_meas),
        "draft_error": float(draft_err),
        "period_measured": float(period),
        "period_error": float(period_err),
        "window": [float(w0), float(w1)],
        "gates": {
            "rao": bool(abs(rao - 1.0) <= RAO_TOLERANCE),
            "draft": bool(abs(draft_err) <= DRAFT_TOLERANCE),
            "period": bool(abs(period_err) <= PERIOD_TOLERANCE),
        },
    }


def _provenance(config: WaveExcitedBodyConfig) -> Dict[str, Any]:
    """Provenance metadata stamped into the case for traceability."""
    return {
        "validation_case": "wave_excited_body",
        "issue": "#1302",
        "reference": "long-wave limit: freely heaving small body is a wave "
                     "follower, heave RAO -> 1 (Newman 1977, ch. 6)",
        "citations": [
            "$FOAM_TUTORIALS/multiphase/overInterDyMFoam/floatingBody "
            "(ESI v2312) — overset mesh-merge/zoneID recipe and settings",
            "$FOAM_TUTORIALS/multiphase/interFoam/laminar/waves/stokesII "
            "(ESI v2312) — wave generation/absorption, verified in #1170",
            "Newman (1977), Marine Hydrodynamics — quasi-static long-wave "
            "response",
            "Goda & Suzuki (1976) — incident/reflected split at the "
            "upstream gauge array",
        ],
        "wave": {
            "H_m": config.wave_height, "T_s": config.wave_period,
            "depth_m": config.depth,
            "wavelength_m": config.wavelength,
            "lambda_over_beam": config.lambda_over_beam,
        },
        "body": {
            "beam_m": config.body_beam,
            "height_m": config.body_height,
            "density_kg_m3": config.body_density,
            "mass_kg": config.mass,
            "draft_m": config.draft,
            "x_m": config.body_x,
            "heave_natural_period_s": config.heave_natural_period,
        },
        "tank": {
            "length_m": config.tank_length, "height_m": config.tank_height,
            "mesh": [config.nx, 1, config.nz],
            "component_mesh": [config.comp_nx, 1, config.comp_nz],
        },
        "gauges_x_m": list(config.gauges_x),
        "tolerances": {
            "rao": RAO_TOLERANCE,
            "draft": DRAFT_TOLERANCE,
            "period": PERIOD_TOLERANCE,
        },
    }
