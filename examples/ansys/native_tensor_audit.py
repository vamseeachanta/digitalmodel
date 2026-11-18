# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic interior-peak example; run with PYTHONPATH=src python this_file.py.

Inputs and independent hand solution are documented in
docs/domains/ansys/native-tensor-audit.md. No solver or result file is needed.
"""

import json

from digitalmodel.ansys.native_tensor_audit import (
    CitedLimit,
    Equilibrium,
    PathSpec,
    ResultSet,
    SampleKey,
    TensorSample,
    Wrench,
    audit_tensors,
)


def run_example():
    """Return evidence for the hand-calculated synthetic hydrostatic peak."""
    result_set = ResultSet(1, 1, "1", "GLOBAL_CARTESIAN", "MPa", "mm")
    keys = tuple(SampleKey(1, node, "SOLID", 0) for node in (1, 2, 3))
    # Constructed input: each normal component is [0,20,0] MPa, all shears 0.
    # Integral per component = 20 MPa mm, t=2 mm, so membrane=10 MPa,
    # bending=0, raw trace=60 MPa and reconstructed face trace=30 MPa.
    samples = tuple(
        TensorSample(
            key,
            result_set,
            (value, value, value, "0", "0", "0"),
            "ELEMENT_NODAL_UNAVERAGED",
            "solid",
            "",
        )
        for key, value in zip(keys, ("0", "20", "0"))
    )
    path = PathSpec("wall", tuple(zip(keys, ("0", "1", "2"))), "0", "2", "CARTESIAN")
    # Synthetic exact balance: F=(0,10,0)+(0,-10,0)=0 N at the same x=2 mm.
    # Mz=2*10 + 2*(-10)=0 N mm. Zero tolerance is an exact-arithmetic test.
    equilibrium = Equilibrium(
        result_set,
        (Wrench("applied", ("2", "0", "0"), ("0", "10", "0"), ("0",) * 3),),
        (Wrench("support", ("2", "0", "0"), ("0", "-10", "0"), ("0",) * 3),),
        ("0",) * 3,
        CitedLimit("0", "Synthetic exact force sum"),
        CitedLimit("0", "Synthetic exact moment sum"),
    )
    # 40 MPa is deliberately chosen between 30 and 60 for this demonstration.
    # It is a synthetic test requirement, never a design-code allowable.
    limit = CitedLimit(
        "40", "Synthetic demonstration threshold between face and raw traces"
    )
    return audit_tensors(
        samples,
        expected=keys,
        result_set=result_set,
        paths=(path,),
        equilibrium=equilibrium,
        limits={"raw_abs_trace": limit, "linearized_abs_trace": limit},
    )


if __name__ == "__main__":
    answer = run_example()
    print(
        json.dumps(
            {
                "raw_evidence": answer["raw_status"],
                "raw_abs_trace_mpa": f"{answer['raw_abs_trace']:.1f}",
                "linearized_face_abs_trace_mpa": f"{answer['linearized_abs_trace']:.1f}",
                "raw_von_mises_mpa": f"{answer['raw_von_mises']:.1f}",
                "raw_trace_criterion": answer["criteria"]["raw_abs_trace"]["status"],
                "face_trace_criterion": answer["criteria"]["linearized_abs_trace"][
                    "status"
                ],
                "design_code_acceptance": answer["code_acceptance"]["status"],
            },
            indent=2,
        )
    )
