"""
ABOUTME: OpenFOAM dict block string constants for fvSchemes, fvSolution,
transportProperties and other solver configuration files.
"""

FV_SOLUTION_SOLVERS = """
solvers
{
    p
    {
        solver          GAMG;
        smoother        DICGaussSeidel;
        tolerance       1e-06;
        relTol          0.1;
    }
    pFinal
    {
        solver          GAMG;
        smoother        DICGaussSeidel;
        tolerance       1e-06;
        relTol          0;
    }
    U
    {
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-08;
        relTol          0.1;
    }
    UFinal
    {
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-08;
        relTol          0;
    }
    "(k|omega|epsilon)"
    {
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-08;
        relTol          0.1;
    }
    "(k|omega|epsilon)Final"
    {
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-08;
        relTol          0;
    }
}
"""

# VOF (interFoam) solvers block.
#
# Every literal here is taken from the OpenFOAM v2312 tutorial
# $FOAM_TUTORIALS/multiphase/interFoam/laminar/damBreak/damBreak (issue #1959,
# design decision D4). None of it comes from a hand-authored case directory on
# the CFD node.
#
# Two deliberate departures from the tutorial text, both structural rather than
# numeric:
#   - p_rghFinal is written longhand instead of the tutorial's `$p_rgh;` macro,
#     because this repository forbids macro shorthand in Final blocks
#     (tests/solvers/openfoam/test_case_builder.py).
#   - The "(k|omega|epsilon)" entries are retained from the pre-#1959 builder.
#     The tutorial case is laminar and has none, but this builder defaults to
#     kOmegaSST, so those fields are solved and need entries.
FV_SOLUTION_SOLVERS_VOF = """
solvers
{
    "alpha.water.*"
    {
        nAlphaCorr      2;
        nAlphaSubCycles 1;
        cAlpha          1;

        MULESCorr       yes;
        nLimiterIter    5;

        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-08;
        relTol          0;
    }
    "pcorr.*"
    {
        solver          PCG;
        preconditioner  DIC;
        tolerance       1e-05;
        relTol          0;
    }
    p_rgh
    {
        solver          PCG;
        preconditioner  DIC;
        tolerance       1e-07;
        relTol          0.05;
    }
    p_rghFinal
    {
        solver          PCG;
        preconditioner  DIC;
        tolerance       1e-07;
        relTol          0;
    }
    U
    {
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-06;
        relTol          0;
    }
    "(k|omega|epsilon)"
    {
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-08;
        relTol          0.1;
    }
    "(k|omega|epsilon)Final"
    {
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-08;
        relTol          0;
    }
}
"""

PIMPLE_BLOCK = """
PIMPLE
{
    nOuterCorrectors    3;
    nCorrectors         2;
    nNonOrthogonalCorrectors 1;
}
"""

SIMPLE_BLOCK = """
SIMPLE
{
    nNonOrthogonalCorrectors 1;
    consistent      yes;
    residualControl
    {
        p               1e-04;
        U               1e-04;
        "(k|omega|epsilon)" 1e-04;
    }
}
"""

TRANSPORT_MULTIPHASE = """
phases (water air);

water
{
    transportModel  Newtonian;
    nu              1e-06;
    rho             1025;
}

air
{
    transportModel  Newtonian;
    nu              1.48e-05;
    rho             1.225;
}

sigma           0.07;

"""

TRANSPORT_SINGLE = """
transportModel  Newtonian;

nu              1e-06;

"""
