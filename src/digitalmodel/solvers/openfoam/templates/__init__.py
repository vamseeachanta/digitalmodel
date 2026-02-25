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
    "(k|omega|epsilon|alpha.water)"
    {
        solver          smoothSolver;
        smoother        symGaussSeidel;
        tolerance       1e-08;
        relTol          0.1;
    }
    "(k|omega|epsilon|alpha.water)Final"
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
