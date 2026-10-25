# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic geometry only; expected dimensions are hand calculations below."""

import subprocess
import sys
import re
import json
import runpy
import shlex
from pathlib import Path

import ezdxf
import meshio
import pytest
import yaml

from digitalmodel.cad_readers import (
    CadReadError,
    Converter,
    UnitsError,
    read_cad,
)


@pytest.mark.parametrize(
    "unit,code,factor",
    [
        ("mm", ezdxf.units.MM, 0.001),
        ("m", ezdxf.units.M, 1.0),
        ("in", ezdxf.units.IN, 0.0254),
        ("ft", ezdxf.units.FT, 0.3048),
    ],
)
def test_dxf_units_and_geometry(tmp_path, unit, code, factor):
    """NIST SP 811 (2008), B.8: inch=0.0254 m, foot=0.3048 m; SI milli.

    A synthetic line from (0,0) to (2,0) has length 2 drawing units,
    hence length in metres = 2 * the cited conversion factor.
    """
    path = tmp_path / "line.dxf"
    doc = ezdxf.new()
    doc.units = code
    doc.modelspace().add_line((0, 0), (2, 0))
    doc.saveas(path)
    result = read_cad(path)
    line = list(result.geometry.document.modelspace())[0]
    assert line.dxf.end.x * result.units.metres_per_unit == 2 * factor
    assert result.units.coordinate_unit == unit
    assert result.units.source == "file"
    with pytest.raises(UnitsError, match="conflict"):
        read_cad(path, units="cm")


def test_unitless_dxf_requires_declaration(tmp_path):
    """ezdxf Units documentation: INSUNITS=0 means unitless, not metres."""
    path = tmp_path / "unitless.dxf"
    doc = ezdxf.new()
    doc.units = 0  # ezdxf Units documentation: INSUNITS=0 is unitless.
    doc.modelspace().add_circle((0, 0), 2)
    doc.saveas(path)
    with pytest.raises(UnitsError, match="required"):
        read_cad(path)
    result = read_cad(path, units="cm")
    assert result.units.source == "caller"
    assert result.units.metres_per_unit == 0.01  # SI centi, NIST SP 811 B.8.


@pytest.mark.parametrize("suffix", ["stl", "obj"])
def test_mesh_units_and_triangle(tmp_path, suffix):
    """Hand calculation: vertices (0,0,0), (3,0,0), (0,4,0) span 3 by 4.

    NIST SP 811 (2008), B.8: millimetre=0.001 m; extents are 0.003,
    0.004, 0 m. Both mesh formats lack a standard length-unit declaration.
    """
    path = tmp_path / f"triangle.{suffix}"
    meshio.write_points_cells(
        path, [[0, 0, 0], [3, 0, 0], [0, 4, 0]], [("triangle", [[0, 1, 2]])]
    )
    with pytest.raises(UnitsError, match="required"):
        read_cad(path)
    result = read_cad(path, units="mm")
    points = result.geometry.mesh.points
    assert (points.max(axis=0) - points.min(axis=0)).tolist() == [3, 4, 0]
    assert result.geometry.mesh.cells[0].data.tolist() == [[0, 1, 2]]
    assert result.units.metres_per_unit == 0.001


def _write_brep(path, unit, roots=1):
    from OCP.BRepBuilderAPI import BRepBuilderAPI_MakeFace
    from OCP.gp import gp_Pln, gp_Pnt, gp_Dir
    from OCP.IGESControl import IGESControl_Writer
    from OCP.Interface import Interface_Static
    from OCP.STEPControl import STEPControl_Writer, STEPControl_AsIs
    from OCP.IFSelect import IFSelect_RetDone

    # Synthetic planar face: x in [0,30], y in [0,40], in kernel mm.
    face = BRepBuilderAPI_MakeFace(
        gp_Pln(gp_Pnt(0, 0, 0), gp_Dir(0, 0, 1)), 0, 30, 0, 40
    ).Face()
    if path.suffix == ".step":
        writer = STEPControl_Writer()
        previous = Interface_Static.CVal_s("write.step.unit")
        try:
            Interface_Static.SetCVal_s("write.step.unit", unit)
            for _ in range(roots):
                face = BRepBuilderAPI_MakeFace(
                    gp_Pln(gp_Pnt(0, 0, 0), gp_Dir(0, 0, 1)), 0, 30, 0, 40
                ).Face()
                assert writer.Transfer(face, STEPControl_AsIs) == IFSelect_RetDone
            assert writer.Write(str(path)) == IFSelect_RetDone
        finally:
            Interface_Static.SetCVal_s("write.step.unit", previous)
    else:
        writer = IGESControl_Writer(unit, 0)  # OCCT IGES guide: Face mode=0.
        assert writer.AddShape(face)
        assert writer.Write(str(path))


@pytest.mark.parametrize("suffix", ["step", "iges"])
@pytest.mark.parametrize("unit,expected", [("MM", "mm"), ("M", "m"), ("INCH", "in")])
def test_occt_face_units_and_bounds(tmp_path, suffix, unit, expected):
    """Hand calculation: 30 by 40 mm face has spans 30 and 40 kernel mm.

    OCCT 7.9 STEP/IGES user guides: kernel output defaults to mm. Export
    to MM/M/INCH and reimport must preserve those physical dimensions.
    Numerical comparison uses OCCT Precision::Confusion, not a design limit.
    """
    from OCP.Bnd import Bnd_Box
    from OCP.BRepBndLib import BRepBndLib
    from OCP.Precision import Precision

    path = tmp_path / f"face.{suffix}"
    _write_brep(path, unit)
    result = read_cad(path)
    assert result.units.coordinate_unit == "mm"
    assert result.units.declared_units == (expected,)
    bounds = Bnd_Box()
    BRepBndLib.AddOptimal_s(result.geometry.shape, bounds, False, False)
    xmin, ymin, zmin, xmax, ymax, zmax = bounds.Get()
    assert xmax - xmin == pytest.approx(30, abs=Precision.Confusion_s(), rel=0)
    assert ymax - ymin == pytest.approx(40, abs=Precision.Confusion_s(), rel=0)
    assert zmax - zmin == pytest.approx(0, abs=Precision.Confusion_s(), rel=0)
    read_cad(path, units=expected)
    with pytest.raises(UnitsError, match="conflict"):
        read_cad(path, units="ft")


def test_converter_uses_argument_vector_and_retains_output(tmp_path):
    """A fake converter emits a synthetic 2-unit line; no DWG software runs."""
    source = tmp_path / "drawing with spaces.dwg"
    source.write_bytes(b"synthetic")
    output = tmp_path / "converted.dxf"
    template = tmp_path / "template.dxf"
    doc = ezdxf.new()
    doc.units = ezdxf.units.MM
    doc.modelspace().add_line((0, 0), (2, 0))
    doc.saveas(template)
    fake = tmp_path / "fake_converter.py"
    fake.write_text(
        "import pathlib, shutil, sys\n"
        "assert pathlib.Path(sys.argv[1]).suffix == '.dwg'\n"
        "shutil.copyfile(sys.argv[3], sys.argv[2])\n",
        encoding="utf-8",
        newline="\n",
    )
    converter = Converter(
        (sys.executable, str(fake), "{input}", "{output}", str(template)),
        timeout_seconds=10,  # Test harness budget, not an engineering limit.
    )
    result = read_cad(source, converter=converter, converted_path=output)
    assert result.format == "dwg"
    assert result.units.declared_units == ("mm",)
    assert output.is_file()
    with pytest.raises(CadReadError, match="exists"):
        read_cad(source, converter=converter, converted_path=output)


@pytest.mark.parametrize("behavior", ["exit", "missing", "timeout"])
def test_converter_failure(tmp_path, monkeypatch, behavior):
    """A failed, timed-out or output-free conversion is never accepted."""
    path = tmp_path / "input.dwg"
    path.write_bytes(b"synthetic")

    def fake_run(*args, **kwargs):
        assert kwargs["shell"] is False
        if behavior == "exit":
            raise subprocess.CalledProcessError(1, args[0])
        if behavior == "timeout":
            raise subprocess.TimeoutExpired(args[0], kwargs["timeout"])
        return subprocess.CompletedProcess(args[0], 0)

    monkeypatch.setattr(subprocess, "run", fake_run)
    converter = Converter(("fake", "{input}", "{output}"), timeout_seconds=1)
    with pytest.raises(CadReadError):
        read_cad(path, converter=converter, converted_path=tmp_path / "out.dxf")


@pytest.mark.parametrize("suffix", ["dxf", "step", "iges", "stl", "obj"])
def test_malformed_files_fail(tmp_path, suffix):
    """Malformed synthetic data must not return an empty successful result."""
    path = tmp_path / f"invalid.{suffix}"
    path.write_text("not CAD data\n", encoding="utf-8", newline="\n")
    with pytest.raises(CadReadError):
        read_cad(path, units="mm")


def test_validation(tmp_path):
    with pytest.raises(CadReadError, match="Unsupported"):
        read_cad(tmp_path / "drawing.pdf")
    with pytest.raises(CadReadError, match="file"):
        read_cad(tmp_path / "absent.dxf")
    with pytest.raises(UnitsError):
        read_cad(tmp_path / "absent.dxf", units="cubits")
    with pytest.raises(ValueError):
        Converter(("fake", "{input}"), timeout_seconds=1)
    with pytest.raises(ValueError):
        Converter(("fake", "{input}", "{output}"), timeout_seconds=0)


def test_step_without_unit_context(tmp_path):
    """Hand calculation: assume cm for raw spans 30 and 40 -> 300,400 mm.

    NIST SP 811 (2008): centi/milli = 10. The synthetic STEP representation
    context has its unit component removed, retaining all geometry records.
    """
    from OCP.Bnd import Bnd_Box
    from OCP.BRepBndLib import BRepBndLib
    from OCP.Precision import Precision

    path = tmp_path / "unitless.step"
    _write_brep(path, "MM")
    text = re.sub(
        r"GLOBAL_UNIT_ASSIGNED_CONTEXT\s*\(\s*\([^)]*\)\)",
        "",
        path.read_text(encoding="utf-8"),
    )
    path.write_text(text, encoding="utf-8", newline="\n")
    with pytest.raises(UnitsError, match="required"):
        read_cad(path)
    result = read_cad(path, units="cm")
    assert result.units.source == "caller"
    bounds = Bnd_Box()
    BRepBndLib.AddOptimal_s(result.geometry.shape, bounds, False, False)
    x0, y0, _, x1, y1, _ = bounds.Get()
    assert x1 - x0 == pytest.approx(300, abs=Precision.Confusion_s(), rel=0)
    assert y1 - y0 == pytest.approx(400, abs=Precision.Confusion_s(), rel=0)


def _write_step_contexts(path, unitless_root=None, mixed_units=False):
    """Create two independent face roots with explicit or missing units."""
    _write_brep(path, "MM", roots=2)
    text = path.read_text(encoding="utf-8")
    text = text.replace(
        "SI_UNIT(.MILLI.,.METRE.)", "SI_UNIT($,.METRE.)", 1 if mixed_units else 2
    )
    contexts = list(
        re.finditer(r"GLOBAL_UNIT_ASSIGNED_CONTEXT\s*\(\s*\([^)]*\)\)", text)
    )
    assert len(contexts) == 2
    if unitless_root is not None:
        context = contexts[unitless_root]
        text = text[: context.start()] + text[context.end() :]
    path.write_text(text, encoding="utf-8", newline="\n")


@pytest.mark.parametrize("unitless_root", [0, 1])
@pytest.mark.parametrize("caller", [None, "m"])
def test_step_incomplete_unit_coverage_is_rejected(tmp_path, unitless_root, caller):
    """A caller cannot repair one missing context by scaling both roots."""
    path = tmp_path / "incomplete.step"
    _write_step_contexts(path, unitless_root)
    with pytest.raises(UnitsError, match="incomplete"):
        read_cad(path, units=caller)


@pytest.mark.parametrize("mixed_units", [False, True])
def test_step_complete_unit_coverage_preserves_each_face(tmp_path, mixed_units):
    """NIST SP 811 (2008), B.8: 30,40 m = 30000,40000 mm; mm unchanged."""
    from OCP.Bnd import Bnd_Box
    from OCP.BRepBndLib import BRepBndLib
    from OCP.Precision import Precision
    from OCP.TopAbs import TopAbs_FACE
    from OCP.TopExp import TopExp_Explorer

    path = tmp_path / "complete.step"
    _write_step_contexts(path, mixed_units=mixed_units)
    result = read_cad(path)
    assert set(result.units.declared_units) == ({"m", "mm"} if mixed_units else {"m"})
    faces = TopExp_Explorer(result.geometry.shape, TopAbs_FACE)
    spans = []
    while faces.More():
        bounds = Bnd_Box()
        BRepBndLib.AddOptimal_s(faces.Current(), bounds, False, False)
        x0, y0, z0, x1, y1, z1 = bounds.Get()
        spans.append((x1 - x0, y1 - y0, z1 - z0))
        faces.Next()
    # Two 30 by 40 coordinate faces: both m, or one m and one mm.
    expected = (
        [(30, 40, 0), (30000, 40000, 0)] if mixed_units else [(30000, 40000, 0)] * 2
    )
    assert len(spans) == 2
    for actual, wanted in zip(sorted(spans), expected):
        assert actual == pytest.approx(wanted, abs=Precision.Confusion_s(), rel=0)


def test_kernel_settings_restored(tmp_path):
    """OCCT IGES guide xstep.cascade.unit: reader changes must be scoped."""
    from OCP.Interface import Interface_Static

    path = tmp_path / "face.iges"
    _write_brep(path, "MM")
    previous = Interface_Static.CVal_s("xstep.cascade.unit")
    try:
        Interface_Static.SetCVal_s("xstep.cascade.unit", "M")
        read_cad(path)
        assert Interface_Static.CVal_s("xstep.cascade.unit") == "M"
        with pytest.raises(UnitsError):
            read_cad(path, units="ft")
        assert Interface_Static.CVal_s("xstep.cascade.unit") == "M"
    finally:
        Interface_Static.SetCVal_s("xstep.cascade.unit", previous)


def test_worked_example(tmp_path):
    """Independent hand calculation: 2000 mm / 1000 mm per m = 2 m (NIST)."""
    script = Path(__file__).resolve().parents[2] / "examples/cad_readers/read_line.py"
    result = subprocess.run(
        [sys.executable, str(script), "--output-directory", str(tmp_path)],
        check=True,
        capture_output=True,
        text=True,
        encoding="utf-8",
    )
    assert json.loads(result.stdout) == {
        "input_length": 2000.0,
        "input_unit": "mm",
        "length_m": 2.0,
        "units_source": "file",
    }


def test_unsupported_dxf_units(tmp_path):
    """Unsupported declarations must not be overridden by a caller guess."""
    path = tmp_path / "unsupported.dxf"
    doc = ezdxf.new()
    doc.units = ezdxf.units.KM
    doc.modelspace().add_point((0, 0))
    doc.saveas(path)
    with pytest.raises(UnitsError, match="Unsupported"):
        read_cad(path, units="m")


def test_missing_dependency_is_actionable(tmp_path, monkeypatch):
    """The dependency error must be from the selected reader only."""
    import builtins

    real_import = builtins.__import__

    def without_meshio(name, *args, **kwargs):
        if name == "meshio":
            raise ImportError("synthetic absent dependency")
        return real_import(name, *args, **kwargs)

    path = tmp_path / "mesh.stl"
    path.write_text("synthetic", encoding="utf-8", newline="\n")
    monkeypatch.setattr(builtins, "__import__", without_meshio)
    with pytest.raises(CadReadError, match="dependency"):
        read_cad(path, units="mm")


def test_iges_without_units(tmp_path):
    """Hand calculation: raw 30 by 40 mm reinterpreted as cm -> 300 by 400 mm.

    OCCT 7.9 IGESData_BasicEditor unit flags and NIST SP 811 SI prefixes.
    The file is generated with unit flag zero, without a named unit.
    """
    from OCP.BRepBuilderAPI import BRepBuilderAPI_MakeFace
    from OCP.IGESControl import IGESControl_Writer
    from OCP.TCollection import TCollection_HAsciiString
    from OCP.gp import gp_Pln, gp_Pnt, gp_Dir
    from OCP.Bnd import Bnd_Box
    from OCP.BRepBndLib import BRepBndLib
    from OCP.Precision import Precision

    path = tmp_path / "unitless.iges"
    writer = IGESControl_Writer("MM", 0)
    face = BRepBuilderAPI_MakeFace(
        gp_Pln(gp_Pnt(), gp_Dir(0, 0, 1)), 0, 30, 0, 40
    ).Face()
    assert writer.AddShape(face)
    section = writer.Model().GlobalSection()
    section.SetUnitFlag(0)
    section.SetUnitName(TCollection_HAsciiString(""))
    writer.Model().SetGlobalSection(section)
    assert writer.Write(str(path))
    with pytest.raises(UnitsError, match="required"):
        read_cad(path)
    result = read_cad(path, units="cm")
    assert result.units.source == "caller"
    bounds = Bnd_Box()
    BRepBndLib.AddOptimal_s(result.geometry.shape, bounds, False, False)
    x0, y0, _, x1, y1, _ = bounds.Get()
    assert x1 - x0 == pytest.approx(300, abs=Precision.Confusion_s(), rel=0)
    assert y1 - y0 == pytest.approx(400, abs=Precision.Confusion_s(), rel=0)


@pytest.mark.parametrize("suffix", ["stp", "igs", "STEP", "IGES"])
def test_extension_aliases(tmp_path, suffix):
    """Aliases select the same synthetic face reader, with no geometry changes."""
    original = tmp_path / (
        "face.step" if suffix.lower() in {"stp", "step"} else "face.iges"
    )
    _write_brep(original, "MM")
    alias = tmp_path / f"alias.{suffix}"
    alias.write_bytes(original.read_bytes())
    assert read_cad(alias).units.declared_units == ("mm",)


def test_empty_dxf(tmp_path):
    """An empty document contains no geometry to return."""
    path = tmp_path / "empty.dxf"
    doc = ezdxf.new()
    doc.units = ezdxf.units.M
    doc.saveas(path)
    with pytest.raises(CadReadError, match="no entities"):
        read_cad(path)


def test_invalid_mesh_connectivity(tmp_path):
    """Three stored vertices cannot support OBJ's one-based vertex index 9."""
    path = tmp_path / "invalid.obj"
    path.write_text(
        "v 0 0 0\nv 3 0 0\nv 0 4 0\nf 1 2 9\n", encoding="utf-8", newline="\n"
    )
    with pytest.raises(CadReadError):
        read_cad(path, units="m")


@pytest.mark.parametrize("separator,terminator", [(",", ";"), ("|", "!")])
@pytest.mark.parametrize("flag", ["0", "2", ""])
def test_original_iges_flag_handles_strings_and_delimiters(
    tmp_path, separator, terminator, flag
):
    """OCCT igesread.c and GlobalSection::Init: raw field 14 is the unit flag.

    A long Hollerith string containing separators crosses a global record
    boundary. It must not change the parameter count or manufacture units.
    """
    from digitalmodel.cad_readers.iges_header import original_unit_flag

    label = ("synthetic" + separator) * 10
    fields = [f"1H{separator}", f"1H{terminator}", f"{len(label)}H{label}"]
    fields += [""] * 10
    fields += [flag]
    data = separator.join(fields) + terminator
    path = tmp_path / "header.iges"
    path.write_text(
        "\n".join(
            f"{data[i:i+72]:72}G{n:07d}"
            for n, i in enumerate(range(0, len(data), 72), start=1)
        )
        + "\n",
        encoding="utf-8",
        newline="\n",
    )
    assert original_unit_flag(path) == (int(flag) if flag else 0)


def test_import_requires_no_geometry_dependencies():
    """The public API remains importable without installing geometry backends."""
    code = """
import builtins
original = builtins.__import__
def blocked(name, *args, **kwargs):
    if name.split('.')[0] in {'OCP', 'ezdxf', 'meshio', 'numpy'}:
        raise ImportError('backend is absent')
    return original(name, *args, **kwargs)
builtins.__import__ = blocked
from digitalmodel.cad_readers import read_cad
assert callable(read_cad)
"""
    subprocess.run([sys.executable, "-c", code], check=True, capture_output=True)


def test_cad_suite_is_owned_and_executed_with_geometry_dependencies():
    """CAD regressions must reach an enabled shard and the required aggregate."""
    root = Path(__file__).resolve().parents[2]
    detector = runpy.run_path(str(root / "scripts/ci/detect_touched_domains.py"))
    domains = detector["parse_domains"](root / "tests/DOMAINS.md")
    test_root = "tests/cad_readers/"
    owners = [domain for domain in domains if test_root in domain.roots]
    assert len(owners) == 1
    domain = owners[0]
    for changed in (
        "tests/cad_readers/test_readers.py",
        "src/digitalmodel/cad_readers/occt.py",
    ):
        selected = detector["touched_domains"]([changed], domains)
        assert domain.name in {item.name for item in selected}
    gates = yaml.safe_load(
        (root / ".claude/quality-gates.yaml").read_text(encoding="utf-8")
    )["gates"]
    name = f"tests-{domain.name}"
    gate = gates[name]
    assert gate["enabled"] is True
    assert gate["failure_action"] == "block"
    assert test_root in gate["test_roots"]
    command = shlex.split(gate["command"])
    assert test_root in command
    assert command[command.index("-m") + 1] == "pytest"
    dependencies = {
        re.split(r"[<>=!]", command[index + 1])[0]
        for index, token in enumerate(command)
        if token == "--with"
    }
    assert {
        "pytest",
        "pytest-asyncio",
        "numpy",
        "ezdxf",
        "meshio",
        "cadquery-ocp",
        "pyyaml",
    } <= dependencies
    assert name in gates["tests-all"]["depends_on"]
