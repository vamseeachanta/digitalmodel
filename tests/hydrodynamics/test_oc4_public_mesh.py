"""Offline mesh acceptance from NREL/TP-5000-60601 (2014).

Table 3-1 (p. 7): draft 20 m, main diameter 6.5 m, upper diameter
12 m, base diameter 24 m, base height 6 m, top of base z=-14 m,
offset spacing 50 m, member diameter 1.6 m. Table 3-2 (pp. 7-8)
defines the member endpoints. Table 4-6 (p. 25): displacement 13917 m3,
COB z=-13.15 m. Table 3-3 (p. 9): platform CM z=-13.46 m.
The roughly 380 m2 waterplane includes the inclined braces, not just
pi*(6.5**2 + 3*12**2)/4 = 372.5 m2 from the columns.
Source: https://www.nlr.gov/docs/fy14osti/60601.pdf
Acceptance limits (maintainer task): 1% displacement and waterplane area,
aspect <=3, maximum panel span <=3.66 m, zero waterline crossings. These
are mesh criteria, not publication dimensions. COB depth is informational.
GDF gravity input 9.80665 m/s2: public WAMIT manual, section 6.1,
https://www.wamit.com/manual7.x/html/wamit_v75manualch6.html.
"""

import importlib.util
from pathlib import Path
import subprocess
import sys

import numpy as np
import pytest


EXAMPLE = Path(__file__).resolve().parents[2] / "examples/hydrodynamics/oc4-semisub"


def load_script(name):
    spec = importlib.util.spec_from_file_location(name, EXAMPLE / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def checker():
    return load_script("check_mesh")


@pytest.fixture(scope="module")
def mesh_file(tmp_path_factory):
    output = tmp_path_factory.mktemp("oc4") / "public.gdf"
    load_script("generate_mesh").generate_mesh(output, gravity=9.80665)
    return output


def test_public_mesh_hydrostatics_and_limits(mesh_file, checker):
    panels = checker.read_gdf(mesh_file)
    values = checker.measure(panels)
    assert abs(values["volume"] / 13917 - 1) < 0.01
    assert abs(values["waterplane"] / 380 - 1) < 0.01
    # Columns plus three SWL ellipses. CB1 rise/run from Table 3-2:
    # (1.625,2.815,-16.2) -> (11.43,19.81,9.13), diameter 1.6 m.
    rise = 9.13 - (-16.2)
    run_squared = (11.43 - 1.625) ** 2 + (19.81 - 2.815) ** 2
    cosine = rise / np.sqrt(rise**2 + run_squared)
    analytic_waterplane = np.pi * (6.5**2 + 3 * 12**2) / 4 + 3 * np.pi * 0.8**2 / cosine
    assert abs(values["waterplane"] / analytic_waterplane - 1) < 0.01
    # Geometry regression against Table 4-6, rather than a task acceptance limit.
    assert abs(values["cob_z"] / -13.15 - 1) < 0.01
    assert abs(values["cob_x"]) < 0.02  # rounded Table 3-2 coordinates
    assert abs(values["cob_y"]) < 0.02
    assert values["crossings"] == 0
    assert values["max_span"] <= 3.66
    assert values["max_aspect"] <= 3
    assert values["bad_edges"] == 0
    assert values["degenerate"] == 0
    assert min(panels[:, :, 2].ravel()) == pytest.approx(-20)
    assert max(panels[:, :, 2].ravel()) == pytest.approx(0)
    # Outer base radius 12 m at x=-28.87 m (Table 3-2, BC2).
    assert min(panels[:, :, 0].ravel()) == pytest.approx(-28.87 - 12, abs=0.02)
    assert all(row[3] in ("PASS", "INFO") for row in checker.checks(values))


def test_checker_cli_reads_export_and_reports_unknown_mass(mesh_file):
    run = subprocess.run(
        [sys.executable, str(EXAMPLE / "check_mesh.py"), str(mesh_file)],
        capture_output=True,
        text=True,
        check=False,
    )
    assert run.returncode == 0, run.stdout + run.stderr
    assert "NOT_EVALUATED" in run.stdout
    assert "platform CM" in run.stdout
    assert "FAIL" not in run.stdout
    buoyancy = next(
        line for line in run.stdout.splitlines() if line.startswith("COB z")
    )
    assert buoyancy.endswith("| INFO")


def box_panels():
    """Analytic unit cube below SWL: V=1, Aw=1, COB=(0.5,0.5,-0.5)."""
    vertices = np.array(
        [
            [0, 0, -1],
            [1, 0, -1],
            [1, 1, -1],
            [0, 1, -1],
            [0, 0, 0],
            [1, 0, 0],
            [1, 1, 0],
            [0, 1, 0],
        ],
        dtype=float,
    )
    return vertices[
        [[0, 3, 2, 1], [0, 1, 5, 4], [1, 2, 6, 5], [2, 3, 7, 6], [3, 0, 4, 7]]
    ]


@pytest.mark.parametrize(
    ("shift", "cob"),
    [((0, 0, 0), (0.5, 0.5, -0.5)), ((5, -3, 0), (5.5, -2.5, -0.5))],
)
def test_flux_hydrostatics_are_independent_of_horizontal_origin(checker, shift, cob):
    values = checker.measure(box_panels() + shift)
    assert values["volume"] == pytest.approx(1)
    assert values["waterplane"] == pytest.approx(1)
    assert values["cob_z"] == pytest.approx(cob[2])
    assert values["cob_x"] == pytest.approx(cob[0])
    assert values["cob_y"] == pytest.approx(cob[1])
    assert values["bad_edges"] == 0


def test_checker_limits_are_enforced(checker):
    values = checker.measure(box_panels())
    values.update(volume=13917, waterplane=380, cob_z=-13.15)
    assert all(row[3] in ("PASS", "INFO") for row in checker.checks(values))
    values.update(max_aspect=3.01, max_span=3.67)
    rows = {row[0]: row[3] for row in checker.checks(values)}
    assert rows["Maximum aspect"] == "FAIL"
    assert rows["Maximum span (m)"] == "FAIL"


@pytest.mark.parametrize("cob_z", [-13.15, -12.0])
def test_cob_depth_is_informational(checker, cob_z):
    values = checker.measure(box_panels())
    values.update(volume=13917, waterplane=380, cob_z=cob_z)
    rows = checker.checks(values)
    buoyancy = next(row for row in rows if row[0] == "COB z (m) [T4-6]")
    assert buoyancy[2] == cob_z
    assert buoyancy[3] == "INFO"
    assert all(row[3] == "PASS" for row in rows if row is not buoyancy)


def test_measured_aspect_excludes_only_triangle_repeat(checker):
    panels = np.array([[[0, 0, -1], [10, 0, -1], [0, 1, -1], [0, 1, -1]]], float)
    assert checker.measure(panels)["max_aspect"] == pytest.approx(np.sqrt(101))


@pytest.mark.parametrize("rotation", range(4))
def test_triangle_repeat_position_preserves_cube_measurements(checker, rotation):
    quads = box_panels()
    triangles = np.concatenate([quads[:, [0, 1, 2, 2]], quads[:, [0, 2, 3, 3]]])
    values = checker.measure(np.roll(triangles, rotation, axis=1))
    assert values["volume"] == pytest.approx(1)
    assert values["waterplane"] == pytest.approx(1)
    assert values["cob_x"] == pytest.approx(0.5)
    assert values["cob_y"] == pytest.approx(0.5)
    assert values["cob_z"] == pytest.approx(-0.5)
    assert values["degenerate"] == 0
    assert values["bad_edges"] == 0
    assert values["max_aspect"] == pytest.approx(np.sqrt(2))
    assert values["max_span"] == pytest.approx(np.sqrt(2))


@pytest.mark.parametrize("damage", ["reverse", "hole", "duplicate", "crossing", "zero"])
def test_checker_rejects_damaged_mesh(checker, damage):
    panels = box_panels()
    if damage == "reverse":
        panels = panels[:, ::-1]
    elif damage == "hole":
        panels = panels[1:]
    elif damage == "duplicate":
        panels = np.concatenate([panels, panels[:1]])
    elif damage == "crossing":
        panels[1, 2, 2] = 0.2
    else:
        panels[0] = 0
    values = checker.measure(panels)
    if damage == "reverse":
        assert values["volume"] < 0 and values["waterplane"] < 0
    elif damage in ("hole", "duplicate"):
        assert values["bad_edges"] > 0
    elif damage == "crossing":
        assert values["crossings"] > 0 and values["above_water"] == 1
    else:
        assert values["degenerate"] > 0
    assert any(row[3] == "FAIL" for row in checker.checks(values))


@pytest.mark.parametrize(
    "content", ["", "title\n1 9.81\n0 0\n2\n", "title\n1 9.81\n0 0\n1\nnan 0 0\n"]
)
def test_bad_gdf_rejected(tmp_path, checker, content):
    path = tmp_path / "bad.gdf"
    path.write_text(content)
    with pytest.raises(ValueError):
        checker.read_gdf(path)


def test_gravity_is_required_and_must_be_positive(tmp_path):
    generator = load_script("generate_mesh")
    for gravity in (0, -1, float("nan")):
        with pytest.raises(ValueError):
            generator.generate_mesh(tmp_path / "bad.gdf", gravity=gravity)
