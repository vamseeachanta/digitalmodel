# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic checks for the native tensor audit boundary.

All numbers below are constructed test inputs, not engineering allowables.
Hand calculations are given with each test. Format basis: Mechanical APDL
Command Reference, 2024 R2, PRESOL Notes/Table 221. Cartesian decomposition
basis: Mechanical APDL Theory Reference, 2024 R2, section 17.4.1.
"""

from dataclasses import replace
from decimal import Decimal as D, localcontext
from pathlib import Path
import runpy

import pytest

from digitalmodel.ansys.native_tensor_audit import (
    CitedLimit,
    Equilibrium,
    PathSpec,
    ResultSet,
    SampleKey,
    TensorSample,
    Wrench,
    audit_tensors,
    parse_presol_plane183,
)


RESULT = ResultSet(1, 1, "1", "GLOBAL_CARTESIAN", "MPa", "mm")
KEYS = tuple(SampleKey(1, n, "SOLID", 0) for n in (1, 2, 3))
PATH = PathSpec("wall", tuple(zip(KEYS, ("0", "1", "2"))), "0", "2", "CARTESIAN")
LIMIT = CitedLimit("10", "Synthetic test requirement; no design-code claim")


def sample(key, components=("0",) * 6, **kwargs):
    return TensorSample(
        key, RESULT, components, "ELEMENT_NODAL_UNAVERAGED", "solid", "", **kwargs
    )


def balanced():
    """10 N at x=2 and -10 N at x=2: net F=0, Mz=20-20=0 N mm."""
    return Equilibrium(
        RESULT,
        (Wrench("load", ("2", "0", "0"), ("0", "10", "0"), ("0",) * 3),),
        (Wrench("support", ("2", "0", "0"), ("0", "-10", "0"), ("0",) * 3),),
        ("0",) * 3,
        CitedLimit("0", "Exact synthetic force balance"),
        CitedLimit("0", "Exact synthetic moment balance"),
    )


def audit(rows, **kwargs):
    return audit_tensors(
        rows, expected=KEYS, result_set=RESULT, paths=(PATH,), **kwargs
    )


def test_interior_hydrostatic_peak_survives_face_linearization():
    """For diagonal stresses [0,20,0] on x=[0,1,2], each component
    has integral 20, membrane 10, zero first moment. Face trace=30;
    interior raw trace=60; VM=0 everywhere. A supplied limit of 40
    passes reconstructed faces and fails the raw trace independently.
    """
    rows = [sample(k, (v, v, v, "0", "0", "0")) for k, v in zip(KEYS, ("0", "20", "0"))]
    result = audit(
        rows,
        equilibrium=balanced(),
        limits={
            "raw_abs_trace": CitedLimit("40", "Synthetic hand example"),
            "linearized_abs_trace": CitedLimit("40", "Synthetic hand example"),
        },
    )
    assert result["raw_status"] == "COMPLETE"
    assert result["raw_abs_trace"] == D(60)
    assert result["raw_trace_keys"] == (KEYS[1],)
    assert result["raw_von_mises"] == 0
    assert result["paths"]["wall"]["membrane"][:3] == (D(10),) * 3
    assert result["paths"]["wall"]["bending_outer"] == (D(0),) * 6
    assert result["linearized_abs_trace"] == 30
    assert result["criteria"]["raw_abs_trace"]["status"] == "NON_PASS"
    assert result["criteria"]["linearized_abs_trace"]["status"] == "PASS"


@pytest.mark.parametrize("component", (3, 4, 5))
def test_all_three_shears_contribute_to_von_mises(component):
    """Pure shear 2 gives VM=sqrt(3*2**2)=sqrt(12), trace=0."""
    tensor = ["0"] * 6
    tensor[component] = "2"
    result = audit([sample(k, tuple(tensor)) for k in KEYS])
    # Compare at the independent Decimal default precision.
    assert +result["raw_von_mises"] == D(12).sqrt()
    assert result["raw_abs_trace"] == 0


def test_nonuniform_linear_profile_is_integrated_exactly():
    """SX=2+3x, x=[0,.5,2]: mean=5, outer bending=3, faces=2,8.
    Exact segment integration avoids trapezoidal integration of x*SX,
    which is quadratic and would produce a false bending stress.
    """
    path = replace(PATH, points=tuple(zip(KEYS, ("0", ".5", "2"))))
    rows = [
        sample(k, (v, "0", "0", "0", "0", "0")) for k, v in zip(KEYS, ("2", "3.5", "8"))
    ]
    result = audit_tensors(rows, expected=KEYS, result_set=RESULT, paths=(path,))
    wall = result["paths"]["wall"]
    assert wall["membrane"][0] == 5
    assert wall["bending_outer"][0] == 3
    assert wall["inner"][0] == 2 and wall["outer"][0] == 8


@pytest.mark.parametrize(
    "fault",
    (
        "missing",
        "duplicate",
        "extra",
        "step",
        "frame",
        "averaged",
        "units",
        "empty_expected",
    ),
)
def test_raw_failures_never_become_engineering_nonpasses(fault):
    rows = [sample(k) for k in KEYS]
    expected = KEYS
    if fault == "missing":
        rows.pop(1)
    elif fault == "duplicate":
        rows.append(rows[0])
    elif fault == "extra":
        rows.append(sample(SampleKey(2, 4, "SOLID", 0)))
    elif fault in ("step", "frame", "units"):
        fields = {
            "step": {"substep": 2},
            "frame": {"frame": "LOCAL"},
            "units": {"stress_unit": "Pa"},
        }[fault]
        rows[1] = replace(rows[1], result_set=replace(RESULT, **fields))
    elif fault == "averaged":
        rows[1] = replace(rows[1], recovery="NODAL_AVERAGED")
    else:
        expected = ()
    result = audit_tensors(rows, expected=expected, result_set=RESULT, paths=(PATH,))
    assert result["raw_status"] == "INCOMPLETE"
    assert result["errors"]
    assert result["criteria"]["raw_abs_trace"]["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "fault",
    (
        "absent_path",
        "missing_layer",
        "repeat_position",
        "wrong_endpoint",
        "duplicate_path",
        "unknown_key",
    ),
)
def test_independent_path_inventory_is_required(fault):
    paths = (PATH,)
    points = list(PATH.points)
    if fault == "absent_path":
        paths = ()
    elif fault == "missing_layer":
        paths = (replace(PATH, points=(points[0], points[-1])),)
    elif fault == "repeat_position":
        points[1] = (KEYS[1], "0")
        paths = (replace(PATH, points=tuple(points)),)
    elif fault == "wrong_endpoint":
        paths = (replace(PATH, outer="3"),)
    elif fault == "duplicate_path":
        paths = (PATH, PATH)
    else:
        points[1] = (SampleKey(9, 9, "SOLID", 0), "1")
        paths = (replace(PATH, points=tuple(points)),)
    result = audit_tensors(
        [sample(k) for k in KEYS], expected=KEYS, result_set=RESULT, paths=paths
    )
    assert result["raw_status"] == "INCOMPLETE"


def test_force_balance_alone_cannot_open_the_engineering_gate():
    """Move support to x=0: net force remains zero; residual Mz=20 N mm."""
    eq = balanced()
    support = replace(eq.reactions[0], position=("0",) * 3)
    result = audit(
        [sample(k) for k in KEYS],
        equilibrium=replace(eq, reactions=(support,)),
        limits={"raw_abs_trace": LIMIT},
    )
    assert result["raw_status"] == "COMPLETE"
    assert result["equilibrium"]["force"] == (D(0),) * 3
    assert result["equilibrium"]["moment"] == (D(0), D(0), D(20))
    assert result["equilibrium"]["status"] == "NON_PASS"
    assert result["criteria"]["raw_abs_trace"]["status"] == "NOT_EVALUATED"


def test_missing_balance_and_allowables_are_not_evaluated():
    result = audit([sample(k) for k in KEYS])
    assert result["equilibrium"]["status"] == "NOT_EVALUATED"
    assert all(c["status"] == "NOT_EVALUATED" for c in result["criteria"].values())


@pytest.mark.parametrize(
    "basis,reference,status",
    (
        ("", "", "INCOMPLETE"),
        ("ordered", "PRESOL", "INCOMPLETE"),
        ("explicit", "export surface field", "COMPLETE"),
        ("verified-format", "retained format witness", "INCOMPLETE"),
    ),
)
def test_shell_surface_is_never_inferred_from_row_order(basis, reference, status):
    keys = tuple(SampleKey(1, 1, surface, 1) for surface in ("BOTTOM", "MID", "TOP"))
    rows = [
        TensorSample(
            k, RESULT, ("0",) * 6, "ELEMENT_NODAL_UNAVERAGED", basis, reference
        )
        for k in keys
    ]
    path = replace(PATH, points=tuple(zip(keys, ("0", "1", "2"))))
    result = audit_tensors(rows, expected=keys, result_set=RESULT, paths=(path,))
    assert result["raw_status"] == status


@pytest.mark.parametrize("token", ("1.2-003", "1.2+003", "NaN", "inf", "****"))
def test_ambiguous_fortran_numbers_refuse(token):
    with pytest.raises(ValueError):
        sample(KEYS[0], (token,) + ("0",) * 5)


def test_explicit_exponents_and_signed_trace_are_retained():
    """-2D+1=-20, 3E0=3, 4=4: trace=-13; absolute demand=13."""
    result = audit([sample(k, ("-2D+1", "3E0", "4", "0", "0", "0")) for k in KEYS])
    assert result["raw_abs_trace"] == 13
    assert result["signed_traces"][KEYS[0]] == -13


def test_existing_native_profile_visits_all_element_node_rows():
    """Retained listing node 1/element 1: component tokens are copied verbatim.
    Mesh inventory: 4 radial by 24 axial quads, four corners per element;
    node stride=14 (nine full-row nodes plus five midside-row nodes).
    The expected corner IDs are independently enumerated from this grid.
    """
    raw = (Path(__file__).parent / "fixtures/n4_recovery/presol.txt").read_bytes()
    rows = parse_presol_plane183(raw, stress_unit="MPa", length_unit="mm")
    expected = {
        SampleKey(e, n, "SOLID", 0)
        for j in range(24)
        for i in range(4)
        for e in (j * 4 + i + 1,)
        for n in (
            j * 14 + i * 2 + 1,
            j * 14 + i * 2 + 3,
            j * 14 + i * 2 + 17,
            j * 14 + i * 2 + 15,
        )
    }
    assert {s.key for s in rows} == expected
    assert len(rows) == len(expected)
    assert rows[0].components == tuple(
        map(
            D,
            (
                "-0.9986391738688940E+001",
                "0.4544815298103753E-008",
                "0.1301786994304169E+003",
                "-0.3830757578798265E-011",
                "0",
                "0",
            ),
        )
    )


def test_decimal_caller_precision_does_not_change_the_audit():
    rows = [sample(k, ("0", "0", "0", "2", "0", "0")) for k in KEYS]
    expected = audit(rows)
    with localcontext() as ctx:
        ctx.prec = 3
        assert audit(rows) == expected


def test_axisymmetric_linearization_is_not_silently_cartesian():
    path = replace(PATH, method="AXISYMMETRIC")
    result = audit_tensors(
        [sample(k) for k in KEYS],
        expected=KEYS,
        result_set=RESULT,
        paths=(path,),
        equilibrium=balanced(),
        limits={"linearized_abs_trace": LIMIT, "raw_abs_trace": LIMIT},
    )
    assert result["raw_status"] == "COMPLETE"
    assert result["raw_abs_trace"] == 0
    assert result["criteria"]["raw_abs_trace"]["status"] == "PASS"
    assert result["linearized_abs_trace"] is None
    assert result["paths"]["wall"]["status"] == "NOT_EVALUATED"
    assert result["criteria"]["linearized_abs_trace"]["status"] == "NOT_EVALUATED"


def test_components_are_not_recombined_across_element_node_rows():
    """Three separate uniaxial states each have VM=9 and trace=9.
    Combining component maxima across rows would instead give a hydrostatic
    (9,9,9) tensor, VM=0 and trace=27, which is a different stress state.
    """
    rows = [
        sample(k, tuple("9" if i == j else "0" for i in range(6)))
        for j, k in enumerate(KEYS)
    ]
    result = audit(rows)
    assert result["raw_von_mises"] == 9
    assert result["raw_abs_trace"] == 9
    assert result["raw_vm_keys"] == KEYS


def test_missing_material_layer_cannot_pass_on_complete_faces_of_another_layer():
    keys = tuple(
        SampleKey(1, 1, surface, layer)
        for layer in (1, 2)
        for surface in ("BOTTOM", "TOP")
    )
    rows = [
        TensorSample(
            k,
            RESULT,
            ("0",) * 6,
            "ELEMENT_NODAL_UNAVERAGED",
            "explicit",
            "synthetic layer/surface fields",
        )
        for k in keys[:2]
    ]
    paths = tuple(
        PathSpec(
            str(layer), tuple(zip(keys[i : i + 2], ("0", "1"))), "0", "1", "CARTESIAN"
        )
        for layer, i in ((1, 0), (2, 2))
    )
    result = audit_tensors(rows, expected=keys, result_set=RESULT, paths=paths)
    assert result["raw_status"] == "INCOMPLETE"


@pytest.mark.parametrize(
    "fault",
    (
        "duplicate_element",
        "duplicate_node",
        "implicit_exponent",
        "truncated",
        "frame",
        "unsupported_element",
    ),
)
def test_retained_native_parser_refuses_adversarial_mutations(fault):
    raw = (Path(__file__).parent / "fixtures/n4_recovery/presol.txt").read_bytes()
    if fault == "duplicate_element":
        raw = raw.replace(b"ELEMENT=       2 ", b"ELEMENT=       1 ", 1)
    elif fault == "duplicate_node":
        raw = raw.replace(b"        3 -0.726473", b"        1 -0.726473", 1)
    elif fault == "implicit_exponent":
        raw = raw.replace(b"E+001", b" +001", 1)
    elif fault == "truncated":
        raw = raw[:-1]
    elif fault == "frame":
        raw = raw.replace(b"GLOBAL COORDINATES", b"LOCAL COORDINATES")
    else:
        raw = raw.replace(b"PLANE183", b"SHELL181")
    with pytest.raises(ValueError):
        parse_presol_plane183(raw, stress_unit="MPa", length_unit="mm")


@pytest.mark.parametrize(
    "fault", ("force", "result_set", "empty", "duplicate", "units")
)
def test_other_balance_failures_block_criteria_but_retain_raw_peaks(fault):
    eq = balanced()
    if fault == "force":
        eq = replace(eq, reactions=(replace(eq.reactions[0], force=("0", "-9", "0")),))
    elif fault == "result_set":
        eq = replace(eq, result_set=replace(RESULT, substep=2))
    elif fault == "empty":
        eq = replace(eq, reactions=())
    elif fault == "duplicate":
        eq = replace(eq, reactions=eq.applied)
    else:
        eq = replace(eq, result_set=replace(RESULT, length_unit="m"))
    result = audit(
        [sample(k, ("9", "0", "0", "0", "0", "0")) for k in KEYS],
        equilibrium=eq,
        limits={"raw_abs_trace": LIMIT},
    )
    assert result["raw_status"] == "COMPLETE"
    assert result["raw_abs_trace"] == 9
    assert result["criteria"]["raw_abs_trace"]["status"] == "NOT_EVALUATED"


def test_applied_couple_is_included_about_the_declared_origin():
    """F_y=10 at x=2 gives Mz=20. Reaction -10 at origin gives zero
    force residual but no moment; an applied -20 couple restores equilibrium.
    """
    eq = balanced()
    eq = replace(
        eq,
        reactions=(
            replace(eq.reactions[0], position=("0",) * 3, couple=("0", "0", "-20")),
        ),
    )
    result = audit([sample(k) for k in KEYS], equilibrium=eq)
    assert result["equilibrium"]["status"] == "PASS"
    assert result["equilibrium"]["moment"] == (D(0),) * 3


@pytest.mark.parametrize(
    "value,reference", (("-1", "test"), ("1", ""), ("NaN", "test"))
)
def test_allowables_must_be_finite_nonnegative_and_cited(value, reference):
    with pytest.raises(ValueError):
        CitedLimit(value, reference)


def test_worked_example_matches_the_written_hand_solution():
    """The example's triangle has raw trace=60, mean/face trace=30, VM=0.
    Its artificial threshold 40 therefore gives NON_PASS and PASS respectively.
    """
    namespace = runpy.run_path(
        str(Path(__file__).parents[2] / "examples/ansys/native_tensor_audit.py")
    )
    result = namespace["run_example"]()
    assert result["raw_abs_trace"] == 60
    assert result["linearized_abs_trace"] == 30
    assert result["raw_von_mises"] == 0
    assert result["criteria"]["raw_abs_trace"]["status"] == "NON_PASS"
    assert result["criteria"]["linearized_abs_trace"]["status"] == "PASS"


def test_incomplete_audit_reports_exact_missing_extra_and_duplicate_keys():
    """Inventory {1,2,3}, observations {1,1,4}: missing {2,3}, extra {4}."""
    extra = SampleKey(1, 4, "SOLID", 0)
    result = audit([sample(KEYS[0]), sample(KEYS[0]), sample(extra)])
    assert result["coverage"]["expected"] == KEYS
    assert result["coverage"]["visited"] == (KEYS[0], extra)
    assert result["coverage"]["missing"] == KEYS[1:]
    assert result["coverage"]["unexpected"] == (extra,)
    assert result["coverage"]["duplicates"] == (KEYS[0],)
