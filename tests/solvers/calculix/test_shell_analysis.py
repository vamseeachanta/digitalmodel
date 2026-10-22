"""Shell contracts, based on the CalculiX 2.22 manual (2024).

All dimensional numbers below are synthetic test inputs, not material allowables.
Keyword expectations follow sections 7.5, 7.43, 7.51, 7.105, 7.117, 7.122
and 7.124. Numerical expected values are independently derived in docstrings.
"""

import numpy as np
import pytest

from digitalmodel.solvers.calculix import CalculiXResultParser, FEMChain, INPWriter


def shell_writer(kind="Quadrilateral 4"):
    count = 4 if kind.endswith("4") else 8
    nodes = np.zeros((count, 3))
    elements = {kind: {"connectivity": np.arange(count).reshape(1, count)}}
    writer = INPWriter(nodes, elements)
    writer.add_material("MAT", 200000.0, 0.3)
    return writer


@pytest.mark.parametrize("kind, ccx", [("Quadrilateral 4", "S4"),
                                      ("Quadrilateral 8", "S8R")])
def test_shell_section_is_not_solid(tmp_path, kind, ccx):
    writer = shell_writer(kind)
    writer.add_shell_section(f"ESHELL_{ccx}", "MAT", 0.5)
    writer.add_step(nlgeom=True, increments=(0.1, 1.0, 0.001, 0.2))
    text = writer.write(tmp_path / "shell.inp").read_text()
    assert f"*ELEMENT, TYPE={ccx}, ELSET=ESHELL_{ccx}" in text
    assert f"*SHELL SECTION, ELSET=ESHELL_{ccx}, MATERIAL=MAT\n0.5" in text
    assert "*SOLID SECTION" not in text
    assert "*STEP, NLGEOM" in text
    assert "*STATIC\n0.1, 1.0, 0.001, 0.2" in text
    assert "S, E, PEEQ" in text


def test_mixed_shell_solid_sections_are_disjoint(tmp_path):
    writer = shell_writer()
    writer.elements["Tetrahedron 4"] = {"connectivity": np.array([[0, 1, 2, 3]])}
    writer.add_shell_section("ESHELL_S4", "MAT", 1.0)
    text = writer.write(tmp_path / "mixed.inp").read_text()
    assert "*ELEMENT, TYPE=S4, ELSET=ESHELL_S4" in text
    assert "*ELEMENT, TYPE=C3D4, ELSET=EALL" in text
    assert "*SOLID SECTION, ELSET=EALL" in text


def test_plastic_curve_and_imposed_displacement(tmp_path):
    writer = shell_writer()
    writer.add_plasticity("MAT", [(250.0, 0.0), (300.0, 0.02)])
    writer.add_shell_section("ESHELL_S4", "MAT", 1.0)
    writer.add_node_set("EDGE", [1, 2])
    writer.add_step(nlgeom=True)
    writer.add_boundary_condition("EDGE", 1, 0.02)
    text = writer.write(tmp_path / "plastic.inp").read_text()
    assert "*PLASTIC\n250.0, 0.0\n300.0, 0.02" in text
    assert "EDGE, 1, 1, 0.02" in text


def test_ffs_material_library_rows_are_used_without_conversion(tmp_path):
    """Library contract: plastic_table supplies true stress, plastic strain.

    Synthetic E=1000, yield=10, ultimate true stress=20, ultimate strain=.1
    imply final hardening point ep=.1-20/1000=.08; no strength is a code value.
    """
    from digitalmodel.materials.ffs_material_library import TrueStressStrainCurve

    curve = TrueStressStrainCurve(E_mpa=1000, sigma_y_mpa=10, sigma_u_mpa=20,
                                 n=.2, ultimate_true_strain=.1, sigma_u_is_true=True)
    writer = shell_writer()
    writer.add_plasticity("MAT", curve.plastic_table(n_points=3))
    assert writer._materials[0]["plastic"][0] == (10., 0.)
    assert writer._materials[0]["plastic"][1] == pytest.approx((20., .08))


@pytest.mark.parametrize("rows", [[], [(1.0, 0.1)], [(1.0, 0), (2.0, 0)],
                                  [(2.0, 0), (1.0, 0.1)], [(float("nan"), 0)]])
def test_invalid_plastic_curve_rejected(rows):
    with pytest.raises(ValueError):
        shell_writer().add_plasticity("MAT", rows)


def test_steps_keep_distinct_loads(tmp_path):
    writer = shell_writer()
    writer.add_shell_section("ESHELL_S4", "MAT", 1.0)
    writer.add_step(nlgeom=True)
    writer.add_pressure("ESHELL_S4", 2.0)
    writer.add_step("buckle", modes=2)
    writer.add_pressure("ESHELL_S4", 1.0)
    text = writer.write(tmp_path / "steps.inp").read_text()
    static, buckle = text.split("*END STEP")[:2]
    assert "ESHELL_S4, P, 2.0" in static
    assert "*BUCKLE\n2" in buckle
    assert "ESHELL_S4, P, 1.0" in buckle
    assert "ESHELL_S4, P, 2.0" not in buckle


@pytest.mark.parametrize("with_buckle", [False, True])
def test_static_step_turns_nlgeom_off_explicitly(tmp_path, with_buckle):
    """STEP section 7.124: NLGEOM persists until NLGEOM=NO is written."""
    writer = shell_writer()
    writer.add_shell_section("ESHELL_S4", "MAT", 1.0)
    writer.add_step(nlgeom=True)
    if with_buckle:
        writer.add_step("buckle", modes=1)
    writer.add_step(nlgeom=False)
    text = writer.write(tmp_path / "steps.inp").read_text()
    headers = [line for line in text.splitlines() if line.startswith("*STEP")]
    expected = ["*STEP, NLGEOM"] + (["*STEP"] if with_buckle else [])
    assert headers == expected + ["*STEP, NLGEOM=NO"]


def test_nlgeom_can_be_enabled_again_after_linear_steps(tmp_path):
    writer = shell_writer()
    writer.add_shell_section("ESHELL_S4", "MAT", 1.0)
    for nonlinear in (False, True, False, False, True):
        writer.add_step(nlgeom=nonlinear)
    text = writer.write(tmp_path / "steps.inp").read_text()
    assert [line for line in text.splitlines() if line.startswith("*STEP")] == [
        "*STEP", "*STEP, NLGEOM", "*STEP, NLGEOM=NO", "*STEP", "*STEP, NLGEOM",
    ]


def test_hydrostatic_centroid_pressure(tmp_path):
    """p=rho*g*max(z_surface-z,0): 2*5*(3-1)=20; dry point=0.

    Hydrostatic equilibrium dp/dz=-rho*g integrated from a zero-gauge surface.
    Density, gravity and elevations are synthetic, dimensionally consistent inputs.
    """
    writer = shell_writer()
    writer.add_shell_section("ESHELL_S4", "MAT", 1.0)
    writer.add_step()
    writer.add_hydrostatic_pressure({0: 1.0}, density=2.0, gravity=5.0,
                                    surface_elevation=3.0)
    text = writer.write(tmp_path / "wet.inp").read_text()
    assert "*DLOAD\n1, P, 20.0" in text
    dry = shell_writer()
    dry.add_hydrostatic_pressure({0: 4.0}, density=2.0, gravity=5.0,
                                 surface_elevation=3.0)
    assert dry._loads[0]["magnitude"] == 0.0


@pytest.mark.parametrize("kwargs", [{"step_type": "riks"}, {"step_type": "arc_length"}])
def test_riks_is_explicitly_unsupported(kwargs):
    with pytest.raises(NotImplementedError, match="Riks"):
        shell_writer().add_step(**kwargs)


@pytest.mark.parametrize("kwargs", [{"step_type": "typo"},
                                    {"step_type": "buckle", "modes": 0},
                                    {"increments": (0.2, 1, 0.3, 0.1)}])
def test_invalid_step_rejected(kwargs):
    with pytest.raises(ValueError):
        shell_writer().add_step(**kwargs)


@pytest.mark.parametrize("thickness", [0, -1, float("inf")])
def test_invalid_thickness_rejected(thickness):
    with pytest.raises(ValueError):
        shell_writer().add_shell_section("ESHELL_S4", "MAT", thickness)


def frd_block(label, components, rows):
    """Synthetic FRD fixed fields, per CalculiX GraphiX ASCII result format."""
    return (f" -4  {label}\n" + "".join(f" -5  {name}\n" for name in components)
            + "".join(f" -1{node:10d}" + "".join(f"{x:12.5E}" for x in values)
                      + "\n" for node, values in rows) + " -3\n")


def test_strain_principal_values_and_peeq(tmp_path):
    """Tensor [[.02,.01,0],[.01,.02,0],[0,0,-.01]] has eigenvalues
    .03,.01,-.01 from (.02-lambda)^2-.01^2=0 and the third diagonal.
    CalculiX 2.22 section 6, equations (2)-(3), defines tensor shear as
    half the engineering gamma; section 7.51 identifies TOSTRAIN. PE is scalar.
    """
    text = frd_block("TOSTRAIN", ["EXX", "EYY", "EZZ", "EXY", "EYZ", "EZX"],
                     [(1, [.02, .02, -.01, .01, 0, 0])])
    text += frd_block("PE", ["PE"], [(1, [.015])])
    (tmp_path / "shell.frd").write_text(text)
    result = CalculiXResultParser("shell", tmp_path).parse_frd_file()
    assert result["strains"][1]["principal"] == pytest.approx([.03, .01, -.01])
    assert result["equivalent_plastic_strain"] == {1: .015}


def test_latest_complete_frame_does_not_mix_nodes(tmp_path):
    first = frd_block("PE", ["PE"], [(1, [.01]), (2, [.02])])
    last = frd_block("PE", ["PE"], [(1, [.03])])
    path = tmp_path / "shell.frd"
    path.write_text(first + last)
    parser = CalculiXResultParser("shell", tmp_path)
    assert parser.parse_frd_file()["equivalent_plastic_strain"] == {1: .03}
    path.write_text("")
    assert parser.parse_frd_file()["equivalent_plastic_strain"] == {}


def test_truncated_strain_block_rejected(tmp_path):
    (tmp_path / "shell.frd").write_text(" -4  TOSTRAIN\n -1         1 1.0\n")
    with pytest.raises(ValueError):
        CalculiXResultParser("shell", tmp_path).parse_frd_file()


@pytest.mark.parametrize("header", [" -4", " -4   "])
def test_frd_header_without_label_raises_value_error(tmp_path, header):
    (tmp_path / "shell.frd").write_text(header + "\n")
    with pytest.raises(ValueError, match="FRD field header"):
        CalculiXResultParser("shell", tmp_path).parse_frd_file()


def test_buckling_factor_not_frequency(tmp_path):
    """Synthetic factors: Pcr=lambda*Pref; 2.5*4=10 force units.

    CalculiX 2.22 section 7.5 defines the eigenvalue as the step-load multiplier.
    A frequency eigenvalue is unrelated and must not be returned as buckling.
    """
    (tmp_path / "shell.dat").write_text(
        "     B U C K L I N G   F A C T O R   O U T P U T\n\n"
        " MODE NO       BUCKLING FACTOR\n\n 1 2.500000D+00\n 2 -3.0E+00\n\n"
        " E I G E N V A L U E   O U T P U T\n 1 9.0E+05\n")
    result = CalculiXResultParser("shell", tmp_path).parse_dat_file()
    assert result["buckling_factors"] == [{"mode": 1, "factor": 2.5},
                                         {"mode": 2, "factor": -3.0}]


def test_last_converged_increment_is_not_collapse(tmp_path):
    (tmp_path / "shell.sta").write_text(
        " STEP INC ATT ITRS TOT TIME STEP TIME INC TIME\n"
        " 1 1 1 3 1.0E-1 1.0E-1 1.0E-1\n"
        " 1 2 1U 5 1.0E-1 1.0E-1 1.0E-1\n"
        " 1 2 2 4 1.5E-1 1.5E-1 5.0E-2\n"
        " 1 3 1U 8 1.5E-1 1.5E-1 1.0E-1\n")
    result = CalculiXResultParser("shell", tmp_path).parse_sta_file()
    assert result["last_converged"]["increment"] == 2
    assert result["last_converged"]["step_time"] == .15
    assert result["has_unconverged_attempts"] is True
    assert result["collapse_status"] == "NOT_EVALUATED"


def test_empty_status_is_unknown(tmp_path):
    (tmp_path / "shell.sta").write_text(" STEP INC ATT ITRS TOT TIME STEP TIME INC TIME\n")
    assert CalculiXResultParser("shell", tmp_path).parse_sta_file()["last_converged"] is None


def test_generic_mesh_setup(tmp_path):
    writer = shell_writer()
    chain = FEMChain(tmp_path)
    chain.set_mesh(writer.nodes, writer.elements, node_sets={"FIX": [0, 3]})
    path = chain.setup_analysis(
        {"name": "MAT", "E": 200000, "nu": .3,
         "plastic_rows": [(250, 0), (300, .02)]},
        [{"type": "pressure", "element_set": "ESHELL_S4", "magnitude": 2}],
        [{"node_set": "FIX", "dof_start": 1, "dof_end": 3}],
        shell_sections=[{"elset": "ESHELL_S4", "material": "MAT", "thickness": .5}],
        step={"nlgeom": True},
    )
    assert "*SHELL SECTION" in path.read_text()
    assert "ESHELL_S4, P, 2.0" in path.read_text()


def test_generic_mesh_rejects_bad_connectivity(tmp_path):
    chain = FEMChain(tmp_path)
    with pytest.raises(ValueError, match="connectivity"):
        chain.set_mesh(np.zeros((4, 3)), {"Quadrilateral 4": {
            "connectivity": np.array([[0, 1, 2, 9]])}})


@pytest.mark.parametrize("action", [
    lambda w: w.add_shell_section("ESHELL_S4", "MAT", 1),
    lambda w: w.add_shell_section("EALL", "MAT", 1),
    lambda w: w.add_shell_section("ESHELL_S4", "UNKNOWN", 1),
])
def test_invalid_or_duplicate_shell_assignment_rejected(tmp_path, action):
    writer = shell_writer()
    writer.add_shell_section("ESHELL_S4", "MAT", 1)
    action(writer)
    with pytest.raises(ValueError):
        writer.write(tmp_path / "bad.inp")


def test_missing_shell_section_rejected(tmp_path):
    with pytest.raises(ValueError, match="Every shell"):
        shell_writer().write(tmp_path / "bad.inp")


def test_hydrostatic_bad_element_rejected():
    with pytest.raises(ValueError):
        shell_writer().add_hydrostatic_pressure({5: 0.}, density=1., gravity=1.,
                                                surface_elevation=1.)


@pytest.mark.parametrize("direction", [(1, 2), (float("nan"), 0, 0)])
def test_bad_load_direction_rejected(direction):
    with pytest.raises(ValueError):
        shell_writer().add_load("FIX", "cload", 1., direction)


def test_buckling_only_result_has_no_stress_claim(tmp_path):
    (tmp_path / "fem_analysis.frd").write_text("")
    (tmp_path / "fem_analysis.dat").write_text(" MODE NO BUCKLING FACTOR\n 1 2.0\n")
    result = FEMChain(tmp_path).extract_results()
    assert result["max_sxx"] is None
    assert result["hole_sxx"] is None


@pytest.mark.parametrize("record", [
    " 1 1 1\n",
    " 1 1 invalid 3 0.1 0.1 0.1\n",
    " 1 1 1 3 NaN 0.1 0.1\n",
])
def test_sta_parse_error_preserves_result_fields(tmp_path, record):
    """An unreadable status history must not discard FRD or DAT results."""
    (tmp_path / "fem_analysis.frd").write_text(
        frd_block("DISP", ["D1", "D2", "D3"], [(1, [.1, 0, 0])]))
    (tmp_path / "fem_analysis.dat").write_text(" MODE NO BUCKLING FACTOR\n 1 2.0\n")
    # A preceding valid increment must not become a partial success claim.
    (tmp_path / "fem_analysis.sta").write_text(" 1 1 1 3 0.1 0.1 0.1\n" + record)
    result = FEMChain(tmp_path).extract_results()
    assert result["status"]["parse_status"] == "ERROR"
    assert result["status"]["error"]
    assert result["status"]["collapse_status"] == "NOT_EVALUATED"
    assert "last_converged" not in result["status"]
    assert result["fields"]["displacements"][1]["dx"] == .1
    assert result["max_displacement"] == .1
    assert result["dat"]["buckling_factors"] == [{"mode": 1, "factor": 2.0}]


def test_sta_parse_success_is_explicit(tmp_path):
    (tmp_path / "fem_analysis.frd").write_text("")
    (tmp_path / "fem_analysis.sta").write_text(" 1 1 1 3 0.1 0.1 0.1\n")
    result = FEMChain(tmp_path).extract_results()
    assert result["status"]["parse_status"] == "OK"
    assert result["status"]["last_converged"]["step_time"] == .1


def test_set_mesh_invalidates_old_deck(tmp_path):
    writer = shell_writer()
    chain = FEMChain(tmp_path)
    chain._inp_path = tmp_path / "old.inp"
    chain.set_mesh(writer.nodes, writer.elements)
    assert chain._inp_path is None


def test_automatic_solid_set_cannot_be_redefined_as_shell(tmp_path):
    writer = shell_writer()
    writer.elements["Tetrahedron 4"] = {"connectivity": np.array([[0, 1, 2, 3]])}
    writer.add_element_set("EALL", [0])
    writer.add_shell_section("EALL", "MAT", .1)
    with pytest.raises(ValueError, match="shell-only"):
        writer.write(tmp_path / "mixed.inp")


@pytest.mark.parametrize("target, face", [("UNKNOWN", "P"), ("ESHELL_S4", "P1")])
def test_pressure_requires_a_matching_element_face(tmp_path, target, face):
    writer = shell_writer()
    writer.add_shell_section("ESHELL_S4", "MAT", 1.)
    writer.add_pressure(target, 1., face)
    with pytest.raises(ValueError, match="Pressure"):
        writer.write(tmp_path / "bad.inp")


def test_tetrahedron_pressure_face_must_exist(tmp_path):
    writer = INPWriter(np.zeros((4, 3)), {"Tetrahedron 4": {
        "connectivity": np.array([[0, 1, 2, 3]])}})
    writer.add_material("MAT", 1000., 0.)
    writer.add_pressure("EALL", 1., "P5")
    with pytest.raises(ValueError, match="Pressure"):
        writer.write(tmp_path / "bad.inp")
