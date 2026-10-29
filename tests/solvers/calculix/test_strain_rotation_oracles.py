# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent, synthetic strain oracles; no material or acceptance values.

Format basis: CalculiX GraphiX 2.19, section 11.6, Nodal Results Block,
https://dhondt.de/cgx_2.19.pdf: long ASCII records use (1X,I2,I10,6E12.5),
with symmetric tensor components XX, YY, ZZ, XY, YZ, ZX. Section 12.4
defines principal strains by the strain tensor eigenproblem.
CalculiX 2.22, section 6 equations (2)-(3) and section 7.51 (*EL FILE),
https://www.dhondt.de/ccx_2.22.pdf: TOSTRAIN contains tensor strains;
off-diagonal values are half the engineering shear in the linear limit.

All other numerical inputs and expectations are hand calculations below.
Node numbers are arbitrary synthetic identifiers. Tolerances allow floating
point eigensolution roundoff; the chosen components fit E12.5 exactly.
"""

import importlib.util
import math
from pathlib import Path

import pytest

# Load only the parser: package initializers also import analysis chains.
_SOURCE = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/solvers/calculix/result_parser.py"
)
_SPEC = importlib.util.spec_from_file_location("strain_oracle_parser", _SOURCE)
_MODULE = importlib.util.module_from_spec(_SPEC)
_SPEC.loader.exec_module(_MODULE)
CalculiXResultParser = _MODULE.CalculiXResultParser

COMPONENTS = ("exx", "eyy", "ezz", "exy", "eyz", "ezx")
# Q = [[1,2,2], [2,1,-2], [-2,2,-1]] / 3 is a proper rotation:
# each row has squared length 9/9, pairwise dot products vanish, det(Q)=1.
# Q diag(45,9,-18) Q^T = [[1,20,-2], [20,13,-22], [-2,-22,22]].
# Divide by 500 to obtain the following dimensionless strain components.
ROTATED = (0.002, 0.026, 0.044, 0.040, -0.044, -0.004)
PRINCIPAL = (45 / 500, 9 / 500, -18 / 500)
ROUND_OFF = 1e-12  # Absolute allowance, well below the smallest input (0.002).


def _tensor_block(rows):
    """Generate only the ASCII tensor field records defined in section 11.6."""
    indices = ((1, 1), (2, 2), (3, 3), (1, 2), (2, 3), (3, 1))
    header = f" -4  {'TOSTRAIN':8s}{6:5d}{1:5d}\n"
    definitions = "".join(
        f" -5  {name.upper():8s}{1:5d}{4:5d}{row:5d}{col:5d}{0:5d}\n"
        for name, (row, col) in zip(COMPONENTS, indices)
    )
    return header + definitions + "".join(
        f" -1{node:10d}" + "".join(f"{value:12.5E}" for value in values) + "\n"
        for node, values in rows
    ) + " -3\n"


def _parse(tmp_path, text):
    (tmp_path / "strain.frd").write_text(text, encoding="ascii")
    return CalculiXResultParser("strain", tmp_path).parse_frd_file()


def _determinant(strain):
    """Expand the symmetric determinant, independently of the eigensolver."""
    xx, yy, zz, xy, yz, zx = (strain[key] for key in COMPONENTS)
    return xx * yy * zz + 2 * xy * yz * zx - xx * yz**2 - yy * zx**2 - zz * xy**2


def test_rotated_three_axis_principal_strains(tmp_path):
    """The hand rotation above gives trace=36/500 and det=-7290/500**3.

    Integer determinant expansion: (286-484)-20*(440-44)-2*(-440+26)
    = -7290, also 45*9*(-18). The eigenvalues are known before parsing;
    neither inputs nor expectations are generated with numpy.linalg.
    """
    strain = _parse(tmp_path, _tensor_block([(1, ROTATED)]))["strains"][1]
    assert tuple(strain[key] for key in COMPONENTS) == ROTATED
    principal = strain["principal"]
    assert principal == pytest.approx(PRINCIPAL, rel=0, abs=ROUND_OFF)
    assert strain["exx"] + strain["eyy"] + strain["ezz"] == pytest.approx(36 / 500)
    assert sum(principal) == pytest.approx(36 / 500, rel=0, abs=ROUND_OFF)
    assert _determinant(strain) == pytest.approx(-7290 / 500**3)
    assert math.prod(principal) == pytest.approx(-7290 / 500**3)


def test_repeated_eigenvalues(tmp_path):
    """E=.012 I + .036 u u^T, u=(1,2,2)/3, has .048,.012,.012.

    u.u=1: the axial eigenvalue is .012+.036 and both perpendicular
    eigenvalues are .012. Expanding the outer product gives the six inputs.
    """
    values = (0.016, 0.028, 0.028, 0.008, 0.016, 0.008)
    strain = _parse(tmp_path, _tensor_block([(1, values)]))["strains"][1]
    assert strain["principal"] == pytest.approx(
        (0.048, 0.012, 0.012), rel=0, abs=ROUND_OFF
    )


@pytest.mark.parametrize("plane", ("exy", "eyz", "ezx"))
@pytest.mark.parametrize("shear", (0.006, -0.006))
def test_tensor_shear_convention(tmp_path, plane, shear):
    """A pure-shear 2x2 block has lambda**2-shear**2=0; third root=0.

    Synthetic tensor shear +/- .006 therefore gives (.006,0,-.006),
    without dividing by two. Both signs and all three planes are exercised.
    """
    values = tuple(shear if key == plane else 0.0 for key in COMPONENTS)
    strain = _parse(tmp_path, _tensor_block([(1, values)]))["strains"][1]
    assert tuple(strain[key] for key in COMPONENTS) == values
    assert strain["principal"] == pytest.approx(
        (0.006, 0.0, -0.006), rel=0, abs=ROUND_OFF
    )


def test_latest_complete_tensor(tmp_path):
    """Latest complete fields replace nodes, independently of other fields.

    Two synthetic frames contain different node sets and tensors. A later
    scalar PE-only update must retain the second tensor, per the parser's
    documented field semantics, without requiring synchronized frames.
    Scalar .015 and .025 are arbitrary markers, not material limits.
    """
    first_tensor = _tensor_block([(1, ROTATED), (2, ROTATED)])
    repeated = (0.016, 0.028, 0.028, 0.008, 0.016, 0.008)
    second_tensor = _tensor_block([(2, repeated), (3, ROTATED)])
    first_scalar = " -4  PE\n -5  PE\n" + f" -1{1:10d}{0.015:12.5E}\n -3\n"
    second_scalar = " -4  PE\n -5  PE\n" + f" -1{2:10d}{0.025:12.5E}\n -3\n"
    path = tmp_path / "strain.frd"
    parser = CalculiXResultParser("strain", tmp_path)
    path.write_text(first_tensor + first_scalar, encoding="ascii")
    first = parser.parse_frd_file()
    assert set(first["strains"]) == {1, 2}
    path.write_text(
        first_tensor + first_scalar + second_tensor + second_scalar, encoding="ascii"
    )
    second = parser.parse_frd_file()
    assert set(second["strains"]) == {2, 3}
    assert second["strains"][2]["principal"] == pytest.approx((0.048, 0.012, 0.012))
    assert second["strains"][3]["principal"] == pytest.approx(PRINCIPAL)
    assert second["equivalent_plastic_strain"] == {2: 0.025}
    assert first["strains"][1]["principal"] == pytest.approx(PRINCIPAL)
    # A field can be newer than the last PE record; PE is not cleared with it.
    path.write_text(first_tensor + first_scalar + second_tensor, encoding="ascii")
    tensors_only = parser.parse_frd_file()
    assert tensors_only["strains"] == second["strains"]
    assert tensors_only["equivalent_plastic_strain"] == {1: 0.015}
    path.write_text("", encoding="ascii")
    assert parser.parse_frd_file()["strains"] == {}


@pytest.mark.parametrize("terminated", (False, True), ids=("eof", "end-marker"))
def test_truncated_tensor_rejected(tmp_path, terminated):
    """Five components cannot complete the six-component tensor contract."""
    incomplete = _tensor_block([(1, ROTATED[:-1])])
    if not terminated:
        incomplete = incomplete.removesuffix(" -3\n")
    with pytest.raises(ValueError, match="Truncated FRD field record"):
        _parse(tmp_path, _tensor_block([(2, ROTATED)]) + incomplete)


@pytest.mark.parametrize(
    "damage, message",
    (
        ("unterminated", "Unterminated FRD field"),
        ("nonnumeric", "could not convert string to float"),
        ("nonfinite", "Non-finite solver result"),
        ("duplicate-node", "Duplicate node in FRD field"),
        ("interrupted", "Unsupported or incomplete ASCII FRD field"),
    ),
)
def test_malformed_tensor_rejected(tmp_path, damage, message):
    """A complete earlier field must not hide errors in the later tensor."""
    later = _tensor_block([(2, ROTATED)])
    if damage == "unterminated":
        later = later.removesuffix(" -3\n")
    elif damage in {"nonnumeric", "nonfinite"}:
        replacement = "bad" if damage == "nonnumeric" else "NaN"
        later = later.replace(f"{ROTATED[0]:12.5E}", f"{replacement:>12}")
    elif damage == "duplicate-node":
        later = _tensor_block([(2, ROTATED), (2, ROTATED)])
    else:
        later = later.removesuffix(" -3\n") + _tensor_block([(3, ROTATED)])
    with pytest.raises(ValueError, match=message):
        _parse(tmp_path, _tensor_block([(1, ROTATED)]) + later)
