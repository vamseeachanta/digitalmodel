# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""STEP and IGES units parsing and kernel round trips on synthetic shapes.

Unit-declaration tests use hand-written exchange-file fragments and need no
geometry kernel. Round-trip tests build a box and a cylinder with build123d,
export them with the OCP writers in tmp_path and skip when OCP is absent.
"""

import builtins
import os
import re
import shlex
import textwrap
from pathlib import Path
from math import pi, sqrt

import pytest

from digitalmodel.cad import CadReadError, UnitsError, read_cad
from digitalmodel.cad.brep import (
    OCCT_UNIT,
    iges_declared_units,
    iges_global_parameters,
    step_declared_units,
)

LENGTH_MM = "( LENGTH_UNIT() NAMED_UNIT(*) SI_UNIT(.MILLI.,.METRE.) )"
LENGTH_M = "( LENGTH_UNIT() NAMED_UNIT(*) SI_UNIT($,.METRE.) )"


def _p21(*data: str) -> str:
    """ISO 10303-21 text: a header string with a semicolon, then DATA lines."""
    return "\n".join(
        [
            "ISO-10303-21;",
            "HEADER;",
            "FILE_NAME('synthetic; not a unit','2026-11-03',(''),(''),'','','');",
            "ENDSEC;",
            "DATA;",
            "/* GLOBAL_UNIT_ASSIGNED_CONTEXT((#9)); is a comment */",
            *data,
            "ENDSEC;",
            "END-ISO-10303-21;",
        ]
    )


def _context(number: int, *units: int) -> str:
    refs = ",".join(f"#{unit}" for unit in units)
    return (
        f"#{number} = ( GEOMETRIC_REPRESENTATION_CONTEXT(3)\n"
        f"  GLOBAL_UNIT_ASSIGNED_CONTEXT(({refs})) REPRESENTATION_CONTEXT('c','') );"
    )


def _conversion(name: str, factor: str) -> list[str]:
    return [
        f"#2 = ( CONVERSION_BASED_UNIT('{name}',#4) LENGTH_UNIT() NAMED_UNIT(#5) );",
        f"#4 = LENGTH_MEASURE_WITH_UNIT(LENGTH_MEASURE({factor}),#6);",
        "#5 = DIMENSIONAL_EXPONENTS(1.,0.,0.,0.,0.,0.,0.);",
        f"#6 = {LENGTH_M};",
    ]


@pytest.mark.parametrize(
    ("data", "expected"),
    [
        (
            [_context(1, 2, 3), f"#2 = {LENGTH_MM};", "#3 = ( PLANE_ANGLE_UNIT() );"],
            "mm",
        ),
        ([_context(1, 2), "#2 = ( LENGTH_UNIT() SI_UNIT(.CENTI.,.METRE.) );"], "cm"),
        ([_context(1, 2), *_conversion("FOOT", "0.3048")], "ft"),
        ([_context(1, 2), *_conversion("inch", "2.54E-02")], "in"),
        ([_context(1, 3), f"#2 = {LENGTH_MM};", "#3 = ( PLANE_ANGLE_UNIT() );"], None),
    ],
    ids=["milli", "centi", "foot", "inch", "unassigned"],
)
def test_step_length_unit_declarations(data, expected):
    """Hand: FOOT factor 0.3048 m and inch 0.0254 m equal NIST SP 811 (2008),
    Appendix B.8. An SI_UNIT not assigned to a context is not a declaration.
    """
    assert step_declared_units(_p21(*data)) == expected


@pytest.mark.parametrize(
    ("data", "message"),
    [
        ([_context(1, 2), "#2 = ( LENGTH_UNIT() SI_UNIT(.KILO.,.METRE.) );"], "prefix"),
        ([_context(1, 2), *_conversion("INCH", "0.025")], "conflict"),
        ([_context(1, 2), *_conversion("FURLONG", "201.168")], "Unsupported"),
        (
            [_context(1, 2), _context(7, 8), f"#2 = {LENGTH_MM};", f"#8 = {LENGTH_M};"],
            "more than one",
        ),
    ],
    ids=["kilo", "inch-factor", "named", "mixed"],
)
def test_step_unit_refusals(data, message):
    with pytest.raises(UnitsError, match=message):
        step_declared_units(_p21(*data))


def _iges(scale="1.0", flag="2", name="2HMM", delimiter=",", end=";") -> str:
    """Start and Global sections; IGES 5.3 parameters 13-15 are set by the test."""
    # fmt: off
    params = [
        "", "", "4Hdemo", "4Hdemo", "4Hdemo", "4Hdemo", "32", "38", "6", "308",
        "15", "4Hdemo", scale, flag, name, "1", "0.01", "13H261103.120000",
        "1E-07", "1.0", "4Hdemo", "4Hdemo", "11", "0",
    ]
    # fmt: on
    if delimiter != ",":
        params[:2] = [f"1H{delimiter}", f"1H{end}"]
    text = delimiter.join(params) + end
    rows = [text[i : i + 72] for i in range(0, len(text), 72)]
    lines = ["synthetic".ljust(72) + "S0000001"]
    lines += [row.ljust(72) + f"G{n:07d}" for n, row in enumerate(rows, 1)]
    return "\n".join(lines) + "\n"


@pytest.mark.parametrize(
    ("kwargs", "expected"),
    [
        ({}, "mm"),
        ({"flag": "3", "name": "4HINCH"}, "in"),
        ({"flag": "4", "name": ""}, "ft"),
        ({"flag": "6", "name": "1HM", "delimiter": ";", "end": "/"}, "m"),
        ({"flag": "", "name": ""}, None),
    ],
    ids=["flag-mm", "name-inch", "flag-ft", "custom-delimiters", "blank"],
)
def test_iges_units(kwargs, expected):
    """IGES 5.3 Global parameter 14 flags: 2 mm, 3 named, 4 ft, 6 m.

    A blank flag is not resolved to the specification default of inches.
    """
    assert iges_declared_units(_iges(**kwargs)) == expected


@pytest.mark.parametrize(
    ("kwargs", "error", "message"),
    [
        ({"flag": "6", "name": "2HMM"}, UnitsError, "conflict"),
        ({"flag": "5", "name": ""}, UnitsError, "Unsupported"),
        ({"flag": "3", "name": ""}, UnitsError, "requires"),
        ({"scale": "2.0"}, CadReadError, "scale"),
    ],
    ids=["flag-name", "miles", "flag-3", "scale"],
)
def test_iges_unit_refusals(kwargs, error, message):
    with pytest.raises(error, match=message):
        iges_declared_units(_iges(**kwargs))


def test_iges_without_global_section():
    with pytest.raises(CadReadError, match="Global section is missing"):
        iges_global_parameters("not an IGES file\n")


# Kernel round trips -------------------------------------------------------

BOX = {
    "volume": 0.006,
    "area": 0.22,
    "box": ((-0.05, -0.1, -0.15), (0.05, 0.1, 0.15)),
    "faces": 6,
}
CYLINDER = {
    "volume": 0.001 * pi,
    "area": 0.045 * pi,
    "box": ((-0.05, -0.05, -0.2), (0.05, 0.05, 0.2)),
    "faces": 3,
}


@pytest.fixture
def build123d():
    if os.environ.get("CAD_REQUIRE_KERNEL") == "1":
        import OCP  # noqa: F401
        import build123d

        return build123d
    pytest.importorskip("OCP")
    return pytest.importorskip("build123d")


def _shape(build123d, name):
    """build123d lengths are written as millimetres (OCCT cascade unit MM)."""
    if name == "box":
        return build123d.Box(100, 200, 300).wrapped
    return build123d.Cylinder(50, 400).wrapped


def _export(shape, path, unit):
    from OCP.IFSelect import IFSelect_RetDone
    from OCP.IGESControl import IGESControl_Controller, IGESControl_Writer
    from OCP.Interface import Interface_Static
    from OCP.STEPControl import (
        STEPControl_AsIs,
        STEPControl_Controller,
        STEPControl_Writer,
    )

    shapes = shape if isinstance(shape, list) else [shape]
    STEPControl_Controller.Init_s()
    IGESControl_Controller.Init_s()
    saved = {
        key: Interface_Static.CVal_s(key)
        for key in ("xstep.cascade.unit", "write.step.unit")
    }
    try:
        Interface_Static.SetCVal_s("xstep.cascade.unit", "MM")
        if path.suffix == ".step":
            Interface_Static.SetCVal_s("write.step.unit", OCCT_UNIT[unit])
            writer = STEPControl_Writer()
            for item in shapes:
                assert writer.Transfer(item, STEPControl_AsIs) == IFSelect_RetDone
            assert writer.Write(str(path)) == IFSelect_RetDone
        else:
            writer = IGESControl_Writer(OCCT_UNIT[unit], 1)  # 1: B-rep solids
            for item in shapes:
                assert writer.AddShape(item)
            writer.ComputeModel()
            assert writer.Write(str(path))
    finally:
        for key, value in saved.items():
            Interface_Static.SetCVal_s(key, value)
    return path


def _check(result, expected):
    brep = result.brep
    assert result.units == "m"
    assert brep.solids == 1 and brep.faces == expected["faces"]
    assert brep.is_valid
    assert brep.volume_m3 == pytest.approx(expected["volume"], rel=1e-6)
    assert brep.surface_area_m2 == pytest.approx(expected["area"], rel=1e-6)
    for corner, hand in zip(brep.bounding_box, expected["box"]):
        assert corner == pytest.approx(hand, abs=1e-6)
    assert result.entities and {e.kind for e in result.entities} == {"triangle"}
    # Independent display policy: 0.1% of the hand-calculated box diagonal.
    diagonal = sqrt(sum((b - a) ** 2 for a, b in zip(*expected["box"])))
    assert brep.linear_deflection_m == pytest.approx(diagonal * (0.1 / 100))
    low, high = brep.bounding_box
    slack = 2 * brep.linear_deflection_m
    assert all(
        lo - slack <= value <= hi + slack
        for point in result.points
        for value, lo, hi in zip(point, low, high)
    )


@pytest.mark.parametrize("suffix", ["step", "igs"])
@pytest.mark.parametrize(
    ("name", "unit", "expected"),
    [("box", "mm", BOX), ("cylinder", "in", CYLINDER)],
    ids=["box-mm", "cylinder-in"],
)
def test_round_trip_reads_file_units(build123d, tmp_path, suffix, name, unit, expected):
    """Hand: box 100 x 200 x 300 mm -> 0.006 m^3, area 2(0.02+0.06+0.03) m^2.

    Cylinder r = 50 mm, h = 400 mm -> V = pi r^2 h = 0.001 pi m^3 and
    A = 2 pi r^2 + 2 pi r h = 0.045 pi m^2. Both are centred on the origin.
    The cylinder file is written in inches (0.0254 m, NIST SP 811, B.8).
    """
    path = _export(_shape(build123d, name), tmp_path / f"{name}.{suffix}", unit)
    with pytest.raises(UnitsError, match="conflict"):
        read_cad(path, units="m")
    result = read_cad(path)
    assert result.format == ("step" if suffix == "step" else "iges")
    assert result.unit_decision.source_units == unit
    assert result.unit_decision.basis == "file"
    _check(result, expected)


def _strip_units(path):
    """Remove the length unit from a kernel-written file, keeping its layout."""
    if path.suffix == ".step":
        text = path.read_text(encoding="ascii")
        text, count = re.subn(
            r"(GLOBAL_UNIT_ASSIGNED_CONTEXT\s*\(\s*\()#\d+,\s*", r"\1", text
        )
        assert count == 1
        path.write_text(text, encoding="ascii")
        assert step_declared_units(text) is None
        return
    lines = path.read_text(encoding="ascii").splitlines()
    rows = [i for i, line in enumerate(lines) if line[72:73] == "G"]
    section = "".join(lines[i][:72] for i in rows)
    assert section.count(",2,2HMM,") == 1
    section = section.replace(",2,2HMM,", ",,,     ")  # same length, same rows
    for n, i in enumerate(rows):
        lines[i] = section[72 * n : 72 * (n + 1)] + lines[i][72:]
    path.write_text("\n".join(lines) + "\n", encoding="ascii")
    assert iges_declared_units(path.read_text(encoding="ascii")) is None


@pytest.mark.parametrize("suffix", ["step", "igs"])
def test_file_without_units_needs_caller_units(build123d, tmp_path, suffix):
    """Hand: unchanged millimetre coordinates read as mm give 0.006 m^3.

    The IGES result also shows the blank flag is not read as inches, which
    would give 0.006 x 25.4^3 m^3.
    """
    path = _export(_shape(build123d, "box"), tmp_path / f"box.{suffix}", "mm")
    _strip_units(path)
    with pytest.raises(UnitsError, match="State units"):
        read_cad(path)
    result = read_cad(path, units="mm")
    assert result.unit_decision.basis == "caller"
    _check(result, BOX)


def test_unreadable_step_fails_with_package_error(build123d, tmp_path):
    path = tmp_path / "broken.stp"
    path.write_text("not a STEP file\n", encoding="ascii")
    with pytest.raises(CadReadError):
        read_cad(path, units="m")


@pytest.mark.parametrize("suffix", ["step", "igs"])
def test_all_roots_are_read(build123d, tmp_path, suffix):
    """Two independent boxes: 2 * (0.1 * 0.2 * 0.3) = 0.012 m^3."""
    shapes = [build123d.Box(100, 200, 300).wrapped for _ in range(2)]
    path = _export(shapes, tmp_path / f"two-boxes.{suffix}", "mm")
    result = read_cad(path)
    assert result.brep.solids == 2
    assert result.brep.volume_m3 == pytest.approx(0.012)
    assert result.brep.is_valid


@pytest.mark.parametrize("suffix", ["step", "igs"])
@pytest.mark.parametrize("transferred", [0, 1])
def test_incomplete_root_transfer_is_refused(
    build123d, tmp_path, monkeypatch, suffix, transferred
):
    """A valid first box must not hide a failed second root in either adapter."""
    from OCP import IGESControl, STEPControl

    shapes = [build123d.Box(100, 200, 300).wrapped for _ in range(2)]
    path = _export(shapes, tmp_path / f"partial.{suffix}", "mm")
    module, name = (
        (STEPControl, "STEPControl_Reader")
        if suffix == "step"
        else (IGESControl, "IGESControl_Reader")
    )
    factory = getattr(module, name)

    class InterruptedReader:
        def __init__(self):
            self.reader = factory()

        def __getattr__(self, attr):
            return getattr(self.reader, attr)

        def TransferRoots(self):
            assert self.reader.NbRootsForTransfer() == 2
            if transferred:
                assert self.reader.TransferOneRoot(1)
                assert not self.reader.OneShape().IsNull()
            return transferred

    monkeypatch.setattr(module, name, InterruptedReader)
    message = "incomplete" if transferred else "no transferable shape"
    with pytest.raises(CadReadError, match=message):
        read_cad(path)


def test_cad_ci_enrollment(monkeypatch, capsys):
    """Execute the workflow's command builder, checking the effective gate."""
    import yaml

    root = Path(__file__).resolve().parents[2]
    rows = (root / "tests/DOMAINS.md").read_text().splitlines()
    contracts = next(row for row in rows if row.startswith("| contracts "))
    assert "`tests/cad/`" in contracts
    workflow = yaml.safe_load(
        (root / ".github/workflows/quality-gates-by-domain.yml").read_text()
    )
    steps = workflow["jobs"]["domain-tests"]["steps"]
    builder = next(step for step in steps if step.get("id") == "gate")
    script = builder["run"].split("<<'PY'\n", 1)[1].split("\nPY", 1)[0]
    monkeypatch.setenv("GATE_NAME", "tests-contracts")
    exec(compile(textwrap.dedent(script), "<command builder>", "exec"), {})
    argv = shlex.split(capsys.readouterr().out.strip())
    assert "tests/cad/" in argv
    for pin in (
        "cadquery-ocp==7.9.3.0",
        "build123d==0.7.0",
        "py-lib3mf==2.3.1",
        "numpy==1.26.4",
        "ezdxf==1.4.5",
        "meshio==5.3.5",
    ):
        assert any(argv[i : i + 2] == ["--with", pin] for i in range(len(argv)))
    gate = next(step for step in steps if step.get("id") == "domain-gate")
    assert gate["env"]["CAD_REQUIRE_KERNEL"] == "1"
    assert not gate.get("continue-on-error", False)


def test_required_kernel_cannot_skip(monkeypatch):
    """Gating CI treats a missing kernel as a failure, including import errors."""
    monkeypatch.setenv("CAD_REQUIRE_KERNEL", "1")
    actual_import = builtins.__import__

    def missing(name, *args, **kwargs):
        if name == "OCP":
            raise ModuleNotFoundError("OCP")
        return actual_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", missing)
    with pytest.raises(ModuleNotFoundError, match="OCP"):
        build123d.__wrapped__()


@pytest.mark.parametrize(
    ("name", "factor", "accepted"),
    [
        ("inch", "0.025400000012", True),
        ("inch", "0.025400000052", False),
        ("foot", "0.30480000015", True),
        ("foot", "0.30480000062", False),
    ],
)
def test_step_factor_precision_policy(name, factor, accepted):
    """Independent probes near 0.5 and 2 parts per billion of exact factors.

    Inch: 0.000000000012 / 0.0254 < 1e-9, but 0.000000000052 / 0.0254 > 1e-9.
    Foot: 0.00000000015 / 0.3048 < 1e-9, but 0.00000000062 / 0.3048 > 1e-9.
    """
    text = _p21(_context(1, 2), *_conversion(name, factor))
    if accepted:
        assert step_declared_units(text) == {"inch": "in", "foot": "ft"}[name]
    else:
        with pytest.raises(UnitsError, match="conflict"):
            step_declared_units(text)
