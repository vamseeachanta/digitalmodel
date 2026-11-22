# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline contracts and opt-in native cantilevers; native qualification pending."""

import importlib
from pathlib import Path
import subprocess
from types import SimpleNamespace

import numpy as np
import pytest


def helpers():
    return importlib.import_module("tests.solvers.calculix.beam_b32_helpers")


def table(quantity="displacements", name="TIP", row="5 0 1.19D-3 0"):
    axes = "vx,vy,vz" if quantity == "displacements" else "fx,fy,fz"
    return f"\n {quantity} ({axes}) for set {name} and time 1.0\n\n {row}\n\n"


@pytest.mark.parametrize("kind,stride", [("B31", 1), ("B32", 2)])
@pytest.mark.parametrize("count", [2, 5, 10])
def test_deck_contract(tmp_path, kind, stride, count):
    beam = helpers()
    path, tip = beam.build_cantilever(tmp_path, kind, count)
    text = path.read_text(encoding="utf-8")
    assert tip == stride * count + 1
    nodes = text.split("*NODE\n")[1].split("*ELEMENT")[0].strip().splitlines()
    assert len(nodes) == tip
    for index, row in enumerate(nodes):
        assert [float(v) for v in row.split(",")] == pytest.approx(
            [index + 1, index / (stride * count), 0, 0])
    rows = text.split(f"*ELEMENT, TYPE={kind}, ELSET=EBEAM_{kind}\n")[1]
    rows = rows.split("*")[0].strip().splitlines()
    assert len(rows) == count
    for index, row in enumerate(rows):
        ids = [int(v) for v in row.split(",")]
        expected = [index + 1, *range(stride * index + 1, stride * index + stride + 2)]
        assert ids == expected  # Preordered end, midpoint, end for B32.
        assert len(set(ids[1:])) == stride + 1
    assert text.count("*BEAM SECTION") == 1
    assert f"ELSET=EBEAM_{kind}, MATERIAL=MAT, SECTION=RECT" in text
    assert "0.02, 0.02\n0.0, 0.0, 1.0" in text
    assert "*ELASTIC\n210000000000.0, 0.3" in text
    assert "*NSET, NSET=ROOT\n1\n" in text
    assert f"*NSET, NSET=TIP\n{tip}\n" in text
    for dof in range(1, 7):
        assert f"ROOT, {dof}, {dof}, 0.0" in text
    assert text.count("*CLOAD") == 1
    assert "*CLOAD\nTIP, 2, 10.0\n" in text
    assert "*NODE PRINT, NSET=TIP\nU\n*NODE PRINT, NSET=ROOT\nRF\n*END STEP" in text


def test_reference():
    bending, shear = helpers().reference()
    assert bending == pytest.approx(10 / (3 * 210e9 * (0.02**4 / 12)))
    assert shear == pytest.approx(10 / ((5 / 6) * (210e9 / 2.6) * 0.02**2))


@pytest.mark.parametrize("quantity,name,node", [("U", "TIP", 5), ("RF", "ROOT", 1)])
def test_named_table(quantity, name, node):
    text = table(name="OTHER", row="99 0 999 0") + table(name="TIP_EXTRA")
    text += table(name="TIP") + table("forces", "ROOT", "1 0 -10 0")
    expected = (0, 0.00119, 0) if quantity == "U" else (0, -10, 0)
    assert helpers().read_vector(text, quantity, name, node) == expected


@pytest.mark.parametrize("quantity,name,node", [("U", "TIP", 5), ("RF", "ROOT", 1)])
@pytest.mark.parametrize("text", ["", table(name="TIP_EXTRA"), table(row=""),
    table(row="5 0 1"), table(row="5 0 1 0 7"), table(row="bad 0 1 0"),
    table(row="6 0 1 0"), table(row="5 0 1 0\n5 0 2 0"), table() + table(),
    *[table(row=f"5 0 {v} 0") for v in ("nan", "inf", "-inf", "1e999", "1,2")]])
def test_bad_named_table(text, quantity, name, node):
    if quantity == "RF":
        text = text.replace("displacements (vx,vy,vz)", "forces (fx,fy,fz)")
        text = text.replace("TIP", "ROOT").replace("5 ", "1 ")
    with pytest.raises(ValueError):
        helpers().read_vector(text, quantity, name, node)


@pytest.mark.parametrize("code,banner,accepted", [
    (0, "This is Version 2.23\n", True), (201, "This is Version 2.23\n", True),
    (0, "Version 2.21", False), (201, "Version 2.23.1", False),
    (0, "Version 2.23-beta", False), (0, "Version 2.230", False),
    (0, "", False), *[(c, "Version 2.23", False) for c in (1, 2, 127, 139, 255)]])
def test_release_probe(monkeypatch, code, banner, accepted):
    beam = helpers()
    def fake(command, **kw):
        assert command == ["explicit-ccx", "-v"]
        assert kw["timeout"] == 60 and kw["capture_output"] and kw["text"]
        assert kw["env"]["OMP_NUM_THREADS"] == "1"
        return SimpleNamespace(returncode=code, stdout=banner, stderr="")
    monkeypatch.setattr(beam.subprocess, "run", fake)
    if accepted:
        beam.probe_release(Path("explicit-ccx"))
    else:
        with pytest.raises(RuntimeError, match="2.23"):
            beam.probe_release(Path("explicit-ccx"))


@pytest.mark.parametrize("flag", [None, "0", "yes"])
def test_disabled_gate_never_probes(monkeypatch, flag):
    beam = helpers()
    monkeypatch.delenv("CALCULIX_NATIVE_TESTS", raising=False)
    if flag is not None:
        monkeypatch.setenv("CALCULIX_NATIVE_TESTS", flag)
    def forbidden(*args, **kwargs):
        pytest.fail("Disabled gate performed executable lookup or invocation")
    monkeypatch.setattr(beam.Path, "is_file", forbidden)
    monkeypatch.setattr(beam.subprocess, "run", forbidden)
    with pytest.raises(pytest.skip.Exception, match="CALCULIX_NATIVE_TESTS=1"):
        beam.native_executable()


@pytest.mark.parametrize("problem", ["unset", "absent", "directory", "wrong", "timeout", "oserror", "ok"])
def test_enabled_gate(monkeypatch, tmp_path, problem):
    beam = helpers()
    executable = tmp_path / "explicit-ccx"
    executable.write_text("fixture", encoding="utf-8")
    monkeypatch.setenv("CALCULIX_NATIVE_TESTS", "1")
    monkeypatch.setenv("CALCULIX_NATIVE_EXECUTABLE", str(executable))
    if problem == "unset":
        monkeypatch.delenv("CALCULIX_NATIVE_EXECUTABLE")
    elif problem == "absent":
        monkeypatch.setenv("CALCULIX_NATIVE_EXECUTABLE", str(tmp_path / "absent"))
    elif problem == "directory":
        monkeypatch.setenv("CALCULIX_NATIVE_EXECUTABLE", str(tmp_path))
    def fake(*args, **kwargs):
        if problem == "timeout":
            raise subprocess.TimeoutExpired("fixture", 60)
        if problem == "oserror":
            raise OSError("synthetic launch failure")
        return SimpleNamespace(returncode=0, stdout="Version 2.21" if problem == "wrong"
                               else "Version 2.23", stderr="")
    monkeypatch.setattr(beam.subprocess, "run", fake)
    if problem == "ok":
        assert beam.native_executable() == executable.resolve()
    else:
        with pytest.raises(pytest.fail.Exception):
            beam.native_executable()


@pytest.mark.parametrize("problem", ["ok", "exit", "stdout", "stderr", "missing", "timeout"])
def test_native_invocation_contract(monkeypatch, tmp_path, problem):
    beam = helpers()
    path, tip = beam.build_cantilever(tmp_path, "B32", 2)
    def fake(command, **kw):
        assert command == ["explicit-ccx", "-i", path.stem]
        assert kw["cwd"] == path.parent and kw["timeout"] == 60
        assert kw["env"]["OMP_NUM_THREADS"] == "1"
        if problem == "timeout":
            raise subprocess.TimeoutExpired("fixture", 60)
        if problem != "missing":
            path.with_suffix(".dat").write_text(table() + table("forces", "ROOT", "1 0 -10 0"),
                                                encoding="utf-8")
        output = "*ERROR" + "x" * 3000
        return SimpleNamespace(returncode=int(problem == "exit"),
            stdout=output if problem == "stdout" else "", stderr=output if problem == "stderr" else "")
    monkeypatch.setattr(beam.subprocess, "run", fake)
    if problem == "ok":
        assert beam.run_case(Path("explicit-ccx"), path, tip) == ((0, .00119, 0), (0, -10, 0))
    else:
        with pytest.raises((AssertionError, FileNotFoundError, subprocess.TimeoutExpired)):
            beam.run_case(Path("explicit-ccx"), path, tip)


@pytest.mark.parametrize("kind", ["B31", "B32"])
@pytest.mark.parametrize("count", [2, 5, 10])
def test_native_cantilever(tmp_path, kind, count):
    beam = helpers()
    executable = beam.native_executable()  # Gate precedes deck creation and probes.
    path, tip_node = beam.build_cantilever(tmp_path, kind, count)
    displacement, reaction = beam.run_case(executable, path, tip_node)
    bending, shear = beam.reference()
    tip = displacement[1]
    error = abs(tip - bending) / bending
    residual = np.array(reaction) + (0, beam.LOAD, 0)
    print(f"{kind} n={count}: tip={tip * 1e6:.1f} um, error={100 * error:.1f} %, "
          f"shear={shear * 1e6:.1f} um, root residuals={tuple(f'{v:.1e}' for v in residual)} N")
    assert tip * beam.LOAD > 0
    if kind == "B32":
        assert np.all(np.abs(residual) <= 1e-5 * abs(beam.LOAD))
        if count == 10:
            assert error < .01
