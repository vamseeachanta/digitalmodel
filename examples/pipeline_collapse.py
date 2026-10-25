# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic collapse example; see docs/domains/pipelines/collapse-analysis.md.

Hand calculation, using the existing component definitions and the interaction
equation in EPRG, Deeper, Longer and Thicker (2022), section 3.1, equation (3):
Pel=2*144000*0.05**3=36 MPa, Pp=2*350*0.05=35 MPa.
At Pc=28 MPa, (28-36)*(28**2-35**2)=3528=28*36*35*0.005/0.05.
For pi=0, pe=14 MPa and zero mechanical loads, U=abs(-14/28)=0.5.
All input values are chosen for arithmetic, not standard material properties.
"""

from digitalmodel.subsea.pipeline.pipeline_pressure import (
    PipeMaterial,
    dnv_combined_loading_check,
)


def run_example() -> dict[str, object]:
    """Return the synthetic calculation with its explicit assessment limits."""
    return dnv_combined_loading_check(
        D=1.0,
        t=0.05,
        mat=PipeMaterial(name="synthetic", SMYS=350.0, SMTS=400.0),
        f_0=0.005,
        E=144000.0,
        nu=0.0,
        M_Sd=0.0,
        S_Sd=0.0,
        p_li=0.0,
        p_e=14.0,
    )


if __name__ == "__main__":
    result = run_example()
    print(f"Collapse model pressure: {result['p_c']:.1f} MPa")
    print(f"Legacy utilization: {result['utilization']:.1f}")
    print(f"Standard compliance: {result['standard_compliance']}")
    print("Not evaluated: " + ", ".join(result["not_evaluated"]))
