"""Case-definition tests for the solver baseline pack (#2300)."""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.solvers.benchmark import cases


def _read_gdf(path: Path):
    lines = path.read_text().splitlines()
    count = int(lines[3].split()[0])
    coords = [tuple(map(float, line.split())) for line in lines[4:]]
    panels = [coords[i : i + 4] for i in range(0, len(coords), 4)]
    return count, panels


def _normal(panel):
    (x1, y1, z1), (x2, y2, z2), (x3, y3, z3) = panel[0], panel[1], panel[2]
    a = (x2 - x1, y2 - y1, z2 - z1)
    b = (x3 - x1, y3 - y1, z3 - z1)
    return (
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    )


def test_barge_gdf_panel_count_and_header(tmp_path):
    path = cases.write_barge_gdf(tmp_path / "barge.gdf")
    count, panels = _read_gdf(path)
    assert count == cases.BARGE_PANELS == len(panels)
    # 100 x 20 x 8 m at 2 m plan / 1 m vertical spacing
    assert count == 50 * 10 + 2 * 50 * 8 + 2 * 10 * 8


def test_barge_gdf_normals_point_out_of_the_body(tmp_path):
    _, panels = _read_gdf(cases.write_barge_gdf(tmp_path / "barge.gdf"))
    half_l, half_b, draft = 50.0, 10.0, 8.0
    for panel in panels:
        cx = sum(p[0] for p in panel) / 4
        cy = sum(p[1] for p in panel) / 4
        cz = sum(p[2] for p in panel) / 4
        n = _normal(panel)
        # outward = from the body centre towards the panel centroid
        outward = (cx, cy, cz + draft / 2)
        assert sum(n[i] * outward[i] for i in range(3)) > 0
        assert -half_l - 1e-9 <= cx <= half_l + 1e-9
        assert -half_b - 1e-9 <= cy <= half_b + 1e-9
        assert -draft - 1e-9 <= cz <= 0.0


def test_diffraction_spec_is_fixed_and_references_mesh():
    spec = cases.diffraction_spec("barge.gdf")
    assert spec["vessel"]["geometry"]["mesh_file"] == "barge.gdf"
    assert len(spec["frequencies"]["values"]) == 20
    assert spec["wave_headings"]["values"] == [0.0, 45.0, 90.0, 135.0, 180.0]
    # displaced mass equals the barge displacement in sea water
    assert spec["vessel"]["inertia"]["mass"] == pytest.approx(100 * 20 * 8 * 1025.0)


def test_apdl_deck_contains_solve_and_fingerprint():
    deck = cases.apdl_deck()
    for token in ("SOLID186", "EQSLV,SPARSE", "SOLVE", "*CFOPEN,fingerprint,txt"):
        assert token in deck
    # *VWRITE must be followed immediately by its format line
    lines = deck.splitlines()
    i = lines.index(next(line for line in lines if line.startswith("*VWRITE")))
    assert lines[i + 1].startswith("(")


def test_apdl_deck_reads_designated_tip_node():
    assert "NODE(10,0.5,0.5)" in cases.apdl_deck()


def test_parse_mapdl_fingerprint(tmp_path):
    (tmp_path / "fingerprint.txt").write_text(
        " -0.123456789012E-02  -0.120000000000E-02       350000.       41000.\n"
    )
    fp = cases.parse_mapdl_fingerprint(tmp_path / "fingerprint.txt")
    assert fp == {"uz_min_m": -0.123456789012e-02, "uz_tip_m": -0.12e-02,
                  "nodes": 350000, "elements": 41000}


def test_parse_mapdl_elapsed():
    text = "junk\n Elapsed time spent computing solution     =      12.345\n" \
        " Elapsed Time (sec) =         98.700       Date =  10/08/2026\n"
    assert cases.parse_mapdl_elapsed(text) == {"solution_s": 12.345, "total_s": 98.7}


def _fake_motorbike(root: Path) -> Path:
    (root / "system").mkdir(parents=True)
    (root / "system" / "controlDict").write_text(
        "application     simpleFoam;\nstartFrom       startTime;\n"
        "endTime         500;\nwriteInterval   100;\n"
        "functions\n{\n    #include \"streamLines\"\n"
        "    #include \"wallBoundedStreamLines\"\n    #include \"cuttingPlane\"\n"
        "    #include \"forceCoeffs\"\n    #include \"ensightWrite\"\n}\n"
    )
    (root / "system" / "decomposeParDict.6").write_text("numberOfSubdomains 6;\n")
    return root


def test_prepare_openfoam_case_edits_dictionaries(tmp_path):
    case = _fake_motorbike(tmp_path / "motorBike")
    cases.prepare_openfoam_case(case, n_procs=8)
    control = (case / "system" / "controlDict").read_text()
    assert "endTime         100;" in control
    assert "writeInterval   100;" in control
    assert '#include "forceCoeffs"' in control
    for dropped in ("streamLines", "cuttingPlane", "ensightWrite"):
        assert f'#include "{dropped}"' not in control
    decomp = (case / "system" / "decomposeParDict").read_text()
    assert "numberOfSubdomains 8;" in decomp
    assert "method          scotch;" in decomp
    # meshing always uses a fixed rank count so every variant solves one mesh
    mesh = (case / "system" / "decomposeParDict.mesh").read_text()
    assert f"numberOfSubdomains {cases.OPENFOAM_MESH_PROCS};" in mesh


def test_parse_openfoam_procs_and_iterations():
    log = "nProcs : 8\nTime = 1\n\nTime = 2\n\nTime = 100\n\nEnd\n"
    assert cases.parse_openfoam_procs(log) == 8
    assert cases.parse_openfoam_last_time(log) == 100


def test_parse_force_coeffs_last_cd(tmp_path):
    dat = tmp_path / "coefficient.dat"
    dat.write_text(
        "# Time Cd Cs Cl CmRoll CmPitch CmYaw Cd(f) Cd(r) Cs(f) Cs(r) Cl(f) Cl(r)\n"
        "99\t0.41\t0\t0.05\n100\t0.4123\t0\t0.051\n"
    )
    assert cases.parse_force_coeffs(dat) == {"iterations": 100, "cd": 0.4123}


def test_parse_force_coeffs_uses_header_column(tmp_path):
    dat = tmp_path / "coefficient.dat"
    dat.write_text("# Time\tCl\tCd\n100\t0.05\t0.4\n")
    assert cases.parse_force_coeffs(dat)["cd"] == 0.4


def test_parse_cell_count():
    log = "Mesh stats\n    cells:            352129\n    faces:    1\n"
    assert cases.parse_cell_count(log) == 352129


def test_resolve_variants_dedupes_and_expands_all():
    assert cases.resolve_variants([1, "all"], cores=8) == [1, 8]
    assert cases.resolve_variants([8, "all"], cores=8) == [8]
    assert cases.resolve_variants([16, "all"], cores=8) == [8]
    assert cases.resolve_variants(["default"], cores=8) == ["default"]


def test_pack_folder_imports_standalone_with_stdlib_only():
    """Linux CFD hosts load the pack without the digitalmodel package."""
    import subprocess
    import sys

    solvers_dir = Path(cases.__file__).resolve().parents[1]
    code = (
        "import sys; sys.path.insert(0, sys.argv[1]);"
        "import benchmark.solvers as s, benchmark.compare;"
        "assert 'digitalmodel' not in sys.modules; print(sorted(s.CASES))"
    )
    out = subprocess.run([sys.executable, "-I", "-c", code, str(solvers_dir)],
                         capture_output=True, text=True, timeout=60)
    assert out.returncode == 0, out.stderr
    assert "openfoam" in out.stdout


def test_solver_module_imports_without_solvers_and_lists_every_case():
    from digitalmodel.solvers.benchmark import solvers

    assert set(solvers.CASES) == {"orcaflex", "orcawave", "aqwa", "mapdl", "openfoam"}
    for make in solvers.CASES.values():
        case = make()
        assert case.variants and callable(case.run)
