"""Exercise the shipped gate with Python's standard library only.

Binary fixture layout: OpenFOAM 10 (2022), CompactIOList::writeData,
CompactListList stream operators, and UList stream output. These fixtures are
synthetic geometry, not measured or solver-generated engineering input.
"""

from __future__ import annotations

import importlib.util
import hashlib
import json
import os
from pathlib import Path
import shutil
import struct
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[2]
MODULE = Path("digitalmodel/solvers/openfoam/hull_face_resolution.py")
SCRIPT = ROOT / "scripts/cfd/check_face_resolution.py"


@pytest.fixture
def reader():
    name = "_test_face_resolution_reader"
    spec = importlib.util.spec_from_file_location(name, ROOT / "src" / MODULE)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    try:
        spec.loader.exec_module(module)
        yield module
    finally:
        sys.modules.pop(name, None)


def write_mesh(case, fmt="binary", arch="LSB;label=32;scalar=64"):
    """Hand calculation: an internal 9 x 9 square precedes a 2 x 2 hull
    square. Hull area = 4 m2, centre = (1, 1, 0), face index = 1. At a
    specified 1 m target and factor 5, ratio = 4 / 1**2 = 4 < 5.

    Label/scalar widths and byte order encode the FoamFile arch declaration;
    see OpenFOAM 10 IOstream architecture metadata and UList binary output.
    """
    poly = case / "constant/polyMesh"
    poly.mkdir(parents=True)

    def header(cls, obj):
        return (
            f'FoamFile\n{{\nversion 2.0;\nformat {fmt};\n'
            f'class {cls};\narch "{arch}";\nobject {obj};\n}}\n'
        ).encode()

    xyz = [(0, 0, 0), (9, 0, 0), (9, 9, 0), (0, 9, 0),
           (0, 0, 0), (2, 0, 0), (2, 2, 0), (0, 2, 0)]
    if fmt == "binary":
        endian = "<" if arch.startswith("LSB") else ">"
        label = endian + ("i" if "label=32" in arch else "q")
        scalar = endian + ("f" if "scalar=32" in arch else "d")

        def packed_list(values, code):
            return (f"\n{len(values)}\n(".encode()
                    + b"".join(struct.pack(code, v) for v in values) + b")\n")

        (poly / "points").write_bytes(
            header("vectorField", "points") + b"\n8\n("
            + b"".join(struct.pack(scalar, v) for p in xyz for v in p) + b")\n"
        )
        (poly / "faces").write_bytes(
            header("faceCompactList", "faces")
            + packed_list([0, 4, 8], label)
            + packed_list(list(range(8)), label)
        )
    else:
        (poly / "points").write_bytes(
            header("vectorField", "points") + b"\n8\n(\n"
            + "\n".join(f"({x} {y} {z})" for x, y, z in xyz).encode() + b"\n)\n"
        )
        (poly / "faces").write_bytes(
            header("faceList", "faces") + b"\n2\n(\n4(0 1 2 3)\n4(4 5 6 7)\n)\n"
        )
    # polyBoundaryMesh remains text even when its header declares binary.
    (poly / "boundary").write_bytes(
        header("polyBoundaryMesh", "boundary")
        + b"\n1\n(\nhull\n{\ntype wall;\nnFaces 1;\nstartFace 1;\n}\n)\n"
    )
    (case / "case_provenance.json").write_text(
        json.dumps({"mesh": {"finest_in_plane_cell_m": 1}})
    )
    return poly


@pytest.mark.parametrize("fmt", ["ascii", "binary"])
@pytest.mark.parametrize("route", ["checkout", "deployed", "configured"])
def test_cli_needs_only_stdlib(tmp_path, fmt, route):
    """Use write_mesh's hand result: ratio 4 passes k=5, fails k=4.

    -I -S excludes environment paths and site packages. Poison package
    initializers prove the CLI loads the canonical file without package imports.
    """
    case = tmp_path / "case"
    poly = write_mesh(case, fmt)
    before = {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in poly.iterdir()}
    root = tmp_path / "installation"
    script = root / "scripts/cfd/check_face_resolution.py"
    script.parent.mkdir(parents=True)
    shutil.copyfile(SCRIPT, script)
    source = root / {"checkout": "src", "deployed": "dm_src",
                     "configured": "custom"}[route]
    target = source / MODULE
    target.parent.mkdir(parents=True)
    shutil.copyfile(ROOT / "src" / MODULE, target)
    for parent in [target.parent, target.parent.parent, source / "digitalmodel"]:
        (parent / "__init__.py").write_text("raise RuntimeError('package imported')\n")
    env = os.environ.copy()
    env.pop("DM_CFD_SRC", None)
    env.pop("DM_CFD_FACE_AREA_K", None)
    env.pop("DM_CFD_FINEST_CELL_M", None)
    env.pop("DM_CFD_HULL_PATCH", None)
    if route == "configured":
        env["DM_CFD_SRC"] = str(source)
    for factor, status, verdict in [("5", 0, "PASS"), ("4", 1, "FAIL")]:
        result = subprocess.run(
            [sys.executable, "-I", "-S", str(script), str(case), "--k", factor],
            capture_output=True, text=True, env=env, check=False,
        )
        assert result.returncode == status, result.stdout + result.stderr
        assert f"FACE RESOLUTION VERDICT: {verdict}" in result.stdout + result.stderr
        assert "Traceback" not in result.stderr
    assert before == {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                      for p in poly.iterdir()}


@pytest.mark.parametrize("endian", ["LSB", "MSB"])
@pytest.mark.parametrize("label", [32, 64])
@pytest.mark.parametrize("scalar", [32, 64])
def test_binary_architectures(reader, tmp_path, endian, label, scalar):
    """All encodings represent write_mesh's independent 4 m2 square."""
    poly = write_mesh(tmp_path, arch=f"{endian};label={label};scalar={scalar}")
    areas = reader.patch_face_areas(poly, "hull")
    assert areas.n_faces == 1
    assert areas.max_area_face == 1
    assert areas.max_area_m2 == pytest.approx(4)
    assert areas.total_area_m2 == pytest.approx(4)
    assert areas.max_area_centre == pytest.approx((1, 1, 0))


def test_binary_list_headers_allow_foam_comments(reader, tmp_path):
    """The standard FoamFile divider must not obscure the 4 m2 square.

    Comments occur outside raw blocks, including between compact sublists.
    Count-like text inside a comment is not a binary list declaration.
    """
    poly = write_mesh(tmp_path)
    for name in ("faces", "points"):
        path = poly / name
        data = path.read_bytes().replace(
            b"}\n", b"}\n// 123\n// (not a list)\n/* file divider */\n"
        )
        if name == "faces":
            data = data.replace(b")\n\n8\n(", b")\n/* labels */\n8\n(")
        path.write_bytes(data)
    assert reader.patch_face_areas(poly, "hull").max_area_m2 == pytest.approx(4)


@pytest.mark.parametrize("damage", ["truncated_points", "truncated_faces",
                                    "negative_label", "bad_offsets", "bad_arch",
                                    "missing_arch", "point_out_of_range",
                                    "nonfinite_point", "bad_class"])
def test_malformed_binary_fails_closed(reader, tmp_path, damage):
    """No geometric verdict is valid when referenced data cannot be decoded."""
    poly = write_mesh(tmp_path)
    path = poly / ("points" if damage in ("truncated_points", "nonfinite_point")
                   else "faces")
    data = path.read_bytes()
    if damage.startswith("truncated"):
        data = data[:-10]
    elif damage == "negative_label":
        data = data.replace(struct.pack("<8i", *range(8)),
                            struct.pack("<8i", 0, 1, 2, 3, -1, 5, 6, 7))
    elif damage == "bad_offsets":
        data = data.replace(struct.pack("<3i", 0, 4, 8), struct.pack("<3i", 0, 4, 3))
    elif damage == "bad_arch":
        data = data.replace(b"label=32", b"label=16")
    elif damage == "missing_arch":
        data = data.replace(b'arch "LSB;label=32;scalar=64";', b"")
    elif damage == "point_out_of_range":
        data = data.replace(struct.pack("<8i", *range(8)),
                            struct.pack("<8i", 0, 1, 2, 3, 8, 5, 6, 7))
    elif damage == "nonfinite_point":
        start = data.index(b"\n8\n(") + len(b"\n8\n(")
        offset = start + 4 * struct.calcsize("<3d")
        data = data[:offset] + struct.pack("<d", float("nan")) + data[offset + 8:]
    else:
        data = data.replace(b"faceCompactList", b"faceList")
    path.write_bytes(data)
    with pytest.raises(reader.HullFaceResolutionError):
        reader.patch_face_areas(poly, "hull")


def test_configured_source_takes_precedence(tmp_path):
    """An explicit source selection must override an adjacent source tree."""
    root = tmp_path / "installation"
    script = root / "scripts/cfd/check_face_resolution.py"
    script.parent.mkdir(parents=True)
    shutil.copyfile(SCRIPT, script)
    for directory, marker in [("src", "wrong reader"), ("configured", "selected reader")]:
        target = root / directory / MODULE
        target.parent.mkdir(parents=True)
        target.write_text(f"raise ImportError({marker!r})\n")
    env = dict(os.environ, DM_CFD_SRC=str(root / "configured"))
    result = subprocess.run(
        [sys.executable, "-I", "-S", str(script), "--help"],
        capture_output=True, text=True, env=env, check=False,
    )
    assert result.returncode == 1
    assert "selected reader" in result.stderr
    assert "wrong reader" not in result.stderr


def test_missing_source_fails_with_configuration_hint(tmp_path):
    """No reader means no passing gate, even for a help-only invocation."""
    script = tmp_path / "installation/scripts/cfd/check_face_resolution.py"
    script.parent.mkdir(parents=True)
    shutil.copyfile(SCRIPT, script)
    env = os.environ.copy()
    env.pop("DM_CFD_SRC", None)
    result = subprocess.run(
        [sys.executable, "-I", "-S", str(script), "--help"],
        capture_output=True, text=True, env=env, check=False,
    )
    assert result.returncode == 1
    assert "FACE RESOLUTION VERDICT: FAIL" in result.stderr
    assert "DM_CFD_SRC" in result.stderr
