# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic input decks exercise the pre-run gate without a solver."""

import json
import math
from pathlib import Path
from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.solvers.calculix.model_checker import check_deck, validate_model, ModelCheckError
from digitalmodel.solvers.calculix.cli import main
from digitalmodel.solvers.calculix.fem_chain import FEMChain
from digitalmodel.solvers.calculix._deck import Block, Deck, read_blocks
from digitalmodel.solvers.calculix.model_checker import _surface_nodes


FIXTURES = Path(__file__).with_name("fixtures")


@pytest.fixture
def deck(tmp_path):
    """Write variations of a constrained tetrahedron."""
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8")

    def write(extra="", text=None):
        path = tmp_path / "model.inp"
        path.write_text((base if text is None else text) + extra, encoding="utf-8", newline="\n")
        return path

    return write


def codes(report):
    return {finding.code for finding in report.findings}


def test_supported_deck_and_serializable_evidence(deck):
    report = validate_model(deck())
    assert report.status == "PASS"
    assert report.tolerance == 1e-6
    assert json.loads(json.dumps(report.to_dict()))["status"] == "PASS"


def test_unconstrained_component_reports_elements_and_hint():
    report = check_deck(FIXTURES / "disconnected.inp")
    finding = next(f for f in report.findings if f.code == "unconstrained_part")
    assert finding.severity == "error"
    assert "element:2" in finding.entity_ids
    assert "element:1" not in finding.entity_ids
    assert finding.fix_hint


@pytest.mark.parametrize("link", [
    "*TIE, NAME=JOIN\nSLAVE, MASTER\n",
    "*CONTACT PAIR, INTERACTION=FRICTIONLESS\nSLAVE, MASTER\n"
    "*SURFACE INTERACTION, NAME=FRICTIONLESS\n",
])
def test_surface_path_to_supported_component(link, tmp_path):
    text = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8")
    text += "*SURFACE, NAME=MASTER\nBODY, S1\n*SURFACE, NAME=SLAVE\nOTHER, S1\n" + link
    path = tmp_path / "contact.inp"
    path.write_text(text, encoding="utf-8", newline="\n")
    report = check_deck(path)
    assert "unconstrained_part" not in codes(report)
    if "CONTACT PAIR" in link:
        assert "contact_path" in codes(report)
    assert report.status == "PASS"


@pytest.mark.parametrize("attached", [False, True])
def test_mass_requires_structural_connection(deck, attached):
    extra = "*NODE\n99, 9, 9, 9\n*ELEMENT, TYPE=MASS, ELSET=LUMP\n"
    extra += f"90, {1 if attached else 99}\n*MASS, ELSET=LUMP\n2.0\n"
    report = check_deck(deck(extra))
    assert ("floating_mass" in codes(report)) is not attached
    if not attached:
        finding = next(f for f in report.findings if f.code == "floating_mass")
        assert "element:90" in finding.entity_ids
        assert "node:99" in finding.entity_ids


def test_boundary_on_mass_alone_does_not_supply_structure(deck):
    report = check_deck(deck("*NODE\n99,9,9,9\n*ELEMENT,TYPE=MASS,ELSET=LUMP\n"
                             "90,99\n*MASS,ELSET=LUMP\n2\n*BOUNDARY\n99,1,3\n"))
    assert "floating_mass" in codes(report)


@pytest.mark.parametrize("distance,tolerance,expected", [
    (0, 1e-6, True), (5e-7, 1e-6, True), (2e-6, 1e-6, False),
    (2e-6, 1e-5, True), (1e-6, 1e-6, True),
])
def test_coincident_node_tolerance(deck, distance, tolerance, expected):
    report = check_deck(deck(f"*NODE\n99,{distance},0,0\n"), tolerance=tolerance)
    assert ("unmerged_nodes" in codes(report)) is expected
    if expected:
        assert next(f for f in report.findings if f.code == "unmerged_nodes").entity_ids == (
            "node:1", "node:99"
        )


def test_tie_exempts_only_nodes_on_opposite_surfaces(deck):
    extra = "*NODE\n99,0,0,0\n100,0,0,0\n*NSET,NSET=LEFT\n1\n*NSET,NSET=RIGHT\n99\n"
    extra += ("*SURFACE,NAME=MASTER,TYPE=NODE\nLEFT\n"
              "*SURFACE,NAME=SLAVE,TYPE=NODE\nRIGHT\n*TIE,NAME=JOIN\nSLAVE,MASTER\n")
    pairs = [f.entity_ids for f in check_deck(deck(extra)).findings if f.code == "unmerged_nodes"]
    assert ("node:1", "node:99") not in pairs
    assert ("node:1", "node:100") in pairs
    assert ("node:99", "node:100") in pairs


@pytest.mark.parametrize("extra,entity", [
    ("*BOUNDARY\nABSENT,1,3\n", "nset:ABSENT"),
    ("*CLOAD,AMPLITUDE=ABSENT\n1,1,1\n", "amplitude:ABSENT"),
    ("*SOLID SECTION,ELSET=BODY,MATERIAL=ABSENT\n", "material:ABSENT"),
    ("*DLOAD\nABSENT,P1,1\n", "elset:ABSENT"),
    ("*TIE,NAME=JOIN\nABSENT,ALSO_ABSENT\n", "surface:ABSENT"),
    ("*SURFACE,NAME=FACE\nABSENT,S1\n", "elset:ABSENT"),
    ("*NSET,NSET=GROUP\nABSENT\n", "nset:ABSENT"),
    ("*ELEMENT,TYPE=C3D4\n8,1,2,3,99\n", "node:99"),
])
def test_missing_references_have_severity_entity_and_fix(deck, extra, entity):
    report = check_deck(deck(extra))
    assert report.status == "FAIL"
    assert any(f.code == "missing_reference" and entity in f.entity_ids
               and f.severity == "error" and f.fix_hint for f in report.findings)


@pytest.mark.parametrize("extra,entity", [
    ("*NSET,NSET=UNUSED\n1\n", "nset:UNUSED"),
    ("*ELSET,ELSET=UNUSED\n1\n", "elset:UNUSED"),
    ("*MATERIAL,NAME=UNUSED\n*ELASTIC\n100,0.3\n", "material:UNUSED"),
    ("*SURFACE,NAME=UNUSED\nBODY,S1\n", "surface:UNUSED"),
    ("*AMPLITUDE,NAME=UNUSED\n0,0,1,1\n", "amplitude:UNUSED"),
])
def test_unused_definitions_warn_and_gate_passes(deck, extra, entity):
    report = validate_model(deck(extra))
    assert any(f.code == "unused_definition" and f.severity == "warning"
               and entity in f.entity_ids for f in report.findings)


def test_missing_section_blocks_mesh_only_deck(deck):
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8")
    report = check_deck(deck(text=base.replace("*SOLID SECTION, ELSET=BODY, MATERIAL=STEEL\n", "")))
    assert "missing_section" in codes(report)


def test_unused_section_warns(deck):
    report = check_deck(deck("*ELSET,ELSET=EMPTY\n*SOLID SECTION,ELSET=EMPTY,MATERIAL=STEEL\n"))
    assert "unused_section" in codes(report)


def test_case_comments_generated_nested_sets_and_d_exponents(deck):
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8").lower()
    base = base.replace("1, 0, 0, 0", "1, 0d0, 0, 0")
    base = base.replace("*boundary\nfixed", "*nset,nset=all,generate\n1,4,1\n"
                        "*nset,nset=alias\nfixed\n** comment\n*boundary\nalias")
    assert validate_model(deck(text=base)).status == "PASS"


def test_element_connectivity_continuation(deck):
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8").replace("1, 1, 2, 3, 4", "1, 1, 2,\n3, 4")
    assert validate_model(deck(text=base)).status == "PASS"


@pytest.mark.parametrize("text", [
    "", "*NODE\n1,nan,0,0\n", "*NODE\n1,0,0,0\n1,1,0,0\n",
    "*NSET,NSET=BAD,GENERATE\n1,5,0\n", "*INCLUDE,INPUT=absent.inp\n",
    "*NSET,NSET=A\nB\n*NSET,NSET=B\nA\n", "*ELEMENT,TYPE=C3D4\n1,1,2\n",
])
def test_invalid_input_returns_errors_instead_of_crashing(deck, text):
    report = check_deck(deck(text=text))
    assert report.status == "FAIL"
    assert report.findings


def test_includes_resolve_relative_to_containing_file_and_detect_cycles(tmp_path):
    child = tmp_path / "mesh.inp"
    child.write_text((FIXTURES / "supported.inp").read_text(encoding="utf-8"),
                     encoding="utf-8", newline="\n")
    root = tmp_path / "model.inp"
    root.write_text('*INCLUDE, INPUT="mesh.inp"\n', encoding="utf-8", newline="\n")
    assert validate_model(root).status == "PASS"
    child.write_text('*INCLUDE, INPUT=model.inp\n', encoding="utf-8", newline="\n")
    assert "parse_error" in codes(check_deck(root))


@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf")])
def test_invalid_tolerance_rejected(deck, value):
    with pytest.raises(ValueError, match="tolerance"):
        check_deck(deck(), tolerance=value)


def test_later_boundary_cannot_hide_unconstrained_first_step(deck):
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8")
    base = base.replace("*BOUNDARY\nFIXED, 1, 3", "*STEP\n*STATIC\n*END STEP\n"
                        "*STEP\n*STATIC\n*BOUNDARY\nFIXED,1,3\n*END STEP")
    assert "unconstrained_part" in codes(check_deck(deck(text=base)))


def test_boundary_reset_is_checked_per_step(deck):
    report = check_deck(deck("*STEP\n*STATIC\n*BOUNDARY,OP=NEW\n*END STEP\n"))
    assert "unconstrained_part" in codes(report)


@pytest.mark.parametrize("first_op", ["NEW", "MOD"])
def test_only_first_boundary_card_applies_op_in_each_step(deck, first_op):
    # Element 1 uses {1,2,3,4}; element 2 uses {5,6,7,8}; their intersection is empty.
    # Restraints on nodes 1 and 5 therefore support two independent components.
    base = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8").split("*BOUNDARY")[0]
    text = base + (f"*STEP\n*STATIC\n*BOUNDARY,OP={first_op}\n1,1,3\n"
                   "*BOUNDARY,OP=NEW\n5,1,3\n*END STEP\n")
    assert check_deck(deck(text=text)).status == "PASS"


def test_first_boundary_flag_restarts_at_next_step(deck):
    base = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8")
    text = base + ("*STEP\n*STATIC\n*BOUNDARY\n5,1,3\n*END STEP\n"
                   "*STEP\n*STATIC\n*BOUNDARY,OP=NEW\n1,1,3\n*END STEP\n")
    findings = [f.entity_ids for f in check_deck(deck(text=text)).findings
                if f.code == "unconstrained_part"]
    assert findings == [("step:2", "element:2")]


@pytest.mark.parametrize("keyword", [
    "SHELL SECTION", "BEAM SECTION", "BEAM GENERAL SECTION", "MEMBRANE SECTION",
    "MASS", "ROTARY INERTIA", "SPRING", "DASHPOT",
])
def test_incompatible_property_does_not_assign_solid_elements(deck, keyword):
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8")
    text = base.replace("*SOLID SECTION, ELSET=BODY, MATERIAL=STEEL\n",
                        f"*{keyword},ELSET=BODY,MATERIAL=STEEL\n1\n")
    report = check_deck(deck(text=text))
    assert report.status == "FAIL"
    assert "missing_section" in codes(report)
    finding = next(f for f in report.findings if f.code == "incompatible_section")
    assert "element:1" in finding.entity_ids
    assert finding.severity == "error" and finding.fix_hint


@pytest.mark.parametrize("kind,width,section", [
    ("C3D4", 4, "SOLID SECTION"), ("CPS3", 3, "SOLID SECTION"),
    ("CPE4", 4, "SOLID SECTION"), ("CAX4", 4, "SOLID SECTION"),
    ("T3D2", 2, "SOLID SECTION"), ("S3", 3, "SHELL SECTION"),
    ("B31", 2, "BEAM SECTION"), ("SPRING2", 2, "SPRING"),
])
def test_matching_section_families_pass(deck, kind, width, section):
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8")
    text = base.replace("TYPE=C3D4", f"TYPE={kind}").replace(
        "1, 1, 2, 3, 4", "1," + ",".join(str(i) for i in range(1, width + 1)))
    text = text.replace("*SOLID SECTION, ELSET=BODY, MATERIAL=STEEL\n",
                        f"*{section},ELSET=BODY,MATERIAL=STEEL\n1,1\n1\n")
    assert check_deck(deck(text=text)).status == "PASS"


@pytest.mark.parametrize("kind,width", [
    ("S4", 4), ("B31", 2), ("SPRING2", 2), ("MASS", 1),
])
def test_solid_section_rejects_other_element_families(deck, kind, width):
    extra = (f"*ELEMENT,TYPE={kind},ELSET=OTHER\n2," +
             ",".join(str(i) for i in range(1, width + 1)) +
             "\n*SOLID SECTION,ELSET=OTHER,MATERIAL=STEEL\n")
    report = check_deck(deck(extra))
    assert "incompatible_section" in codes(report)
    assert any(f.code == "missing_section" and f.entity_ids == ("element:2",)
               for f in report.findings)


def test_mixed_property_set_reports_only_incompatible_members(deck):
    extra = ("*ELEMENT,TYPE=MASS,ELSET=LUMP\n2,1\n"
             "*ELSET,ELSET=MIXED\nBODY,LUMP\n*MASS,ELSET=MIXED\n1\n")
    report = check_deck(deck(extra))
    assert report.status == "FAIL"
    finding = next(f for f in report.findings if f.code == "incompatible_section")
    assert "element:1" in finding.entity_ids and "element:2" not in finding.entity_ids
    assert "missing_section" not in codes(report)


@pytest.mark.parametrize("kind,connectivity,keyword", [
    ("ROTARYI", "1", "ROTARY INERTIA"),
    ("DASHPOT2", "1,2", "DASHPOT"),
    ("B31", "1,2", "BEAM GENERAL SECTION"),
])
def test_unqualified_property_syntax_blocks(deck, kind, connectivity, keyword):
    report = check_deck(deck(f"*ELEMENT,TYPE={kind},ELSET=OTHER\n2,{connectivity}\n"
                             f"*{keyword},ELSET=OTHER\n1\n"))
    assert report.status == "FAIL"
    assert "missing_section" in codes(report)


@pytest.mark.parametrize("keyword", ["*", "*,", "*,,", "*,ELSET=BODY", "* ,MATERIAL=STEEL"])
def test_empty_keyword_returns_structured_api_failure(deck, keyword):
    report = check_deck(deck(keyword + "\n"))
    assert report.status == "FAIL"
    assert codes(report) == {"parse_error"}


@pytest.mark.parametrize("keyword", ["*", "*,", "*,,", "*,ELSET=BODY", "* ,MATERIAL=STEEL"])
def test_empty_keyword_returns_cli_json_and_failure_exit(deck, capsys, keyword):
    assert main([deck(keyword + "\n").as_posix(), "--json"]) == 1
    evidence = json.loads(capsys.readouterr().out)
    assert evidence["status"] == "FAIL"
    assert evidence["findings"][0]["code"] == "parse_error"


def test_utf8_bom_remains_supported(deck):
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8")
    assert check_deck(deck(text="\ufeff" + base)).status == "PASS"


@pytest.mark.parametrize("point,expected_distance", [
    ((3, 4, 0), 5),       # sqrt(3**2 + 4**2) = 5
    ((1, 2, 2), 3),       # sqrt(1 + 4 + 4) = 3
    ((2, 3, 6), 7),       # sqrt(4 + 9 + 36) = 7
])
def test_hand_worked_euclidean_distance_controls_coincidence(deck, point, expected_distance):
    assert math.dist((0, 0, 0), point) == pytest.approx(expected_distance)
    path = deck(f"*NODE\n99,{point[0]},{point[1]},{point[2]}\n")
    pair = ("node:1", "node:99")
    at_limit = check_deck(path, tolerance=expected_distance)
    below_limit = check_deck(path, tolerance=expected_distance - 0.1)
    assert pair in [f.entity_ids for f in at_limit.findings if f.code == "unmerged_nodes"]
    assert pair not in [f.entity_ids for f in below_limit.findings if f.code == "unmerged_nodes"]


def test_tetrahedron_face_connectivity_has_independent_expected_members(deck):
    model = Deck(read_blocks(deck()))
    model.parse()
    # Each face omits the opposite vertex: 4, 3, 1, 2, respectively.
    # CalculiX 2.22 manual, section 7.128 (*SURFACE), fixes the S1-S4 order.
    for face, expected in [("S1", {1, 2, 3}), ("S2", {1, 2, 4}),
                           ("S3", {2, 3, 4}), ("S4", {1, 3, 4})]:
        surface = Block("SURFACE", {"NAME": "FACE"}, [["BODY", face]], 0)
        assert _surface_nodes(model, surface) == expected


def test_gate_exception_retains_report(deck):
    with pytest.raises(ModelCheckError) as exc:
        validate_model(deck(text=""))
    assert exc.value.report.status == "FAIL"


@pytest.mark.parametrize("valid", [True, False])
def test_cli_json_exit_status(deck, capsys, valid):
    path = deck() if valid else deck(text="")
    assert main([str(path), "--json"]) == (0 if valid else 1)
    assert json.loads(capsys.readouterr().out)["status"] == ("PASS" if valid else "FAIL")


def test_full_solve_blocks_errors_before_discovery_or_subprocess(deck, monkeypatch, tmp_path):
    chain = FEMChain(tmp_path)
    chain._inp_path = deck(text="")

    def forbidden(*args, **kwargs):
        pytest.fail("An invalid deck reached executable discovery or solver launch")

    monkeypatch.setattr("digitalmodel.solvers.calculix.fem_chain.is_calculix_available", forbidden)
    monkeypatch.setattr("digitalmodel.solvers.calculix.fem_chain.subprocess.run", forbidden)
    with pytest.raises(ModelCheckError):
        chain.solve()


def test_full_solve_passes_warnings_and_returns_gate_evidence(deck, monkeypatch, tmp_path):
    chain = FEMChain(tmp_path)
    chain._inp_path = deck("*NSET,NSET=UNUSED\n1\n")
    monkeypatch.setattr("digitalmodel.solvers.calculix.fem_chain.is_calculix_available", lambda: True)
    calls = []

    def solve(*args, **kwargs):
        calls.append(args)
        return SimpleNamespace(returncode=0, stdout="done", stderr="")

    monkeypatch.setattr("digitalmodel.solvers.calculix.fem_chain.subprocess.run", solve)
    result = chain.solve()
    assert result["success"] and len(calls) == 1
    assert result["qa_gate"]["status"] == "PASS"
    assert result["qa_gate"]["findings"][0]["severity"] == "warning"


def test_gmsh_style_mesh_through_writer_is_checked(tmp_path):
    chain = FEMChain(tmp_path)
    chain.set_mesh(np.array([[0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1]]),
                   {"Tetrahedron 4": {"connectivity": np.array([[0, 1, 2, 3]])}},
                   node_sets={"FIXED": [0]})
    path = chain.setup_analysis({"name": "STEEL", "E": 200000, "nu": 0.3}, [],
                                [{"node_set": "FIXED", "dof_start": 1, "dof_end": 3}])
    assert validate_model(path).status == "PASS"


@pytest.mark.parametrize("text", [
    "*NODE\n1,0,0,0\n*NSET,NSET=INVALID\n0\n",
    "*NODE\n1,0,0,0\n*ELEMENT,TYPE=C3D4\n1,1,1,1,1\n1,1,1,1,1\n",
    "*NODE\n1,0,0,0\n*ELEMENT,TYPE=UNKNOWN\n1,1\n",
])
def test_bad_identifiers_and_unsupported_types_are_findings(deck, text):
    assert check_deck(deck(text=text)).status == "FAIL"


@pytest.mark.parametrize("keyword", ["EQUATION", "MPC", "RIGID BODY", "SYSTEM", "TRANSFORM",
                                      "NGEN", "ELGEN", "PART", "ASSEMBLY"])
def test_unsupported_topology_or_coordinates_cannot_pass_silently(deck, keyword):
    assert "unsupported_topology" in codes(check_deck(deck(f"*{keyword}\n")))


@pytest.mark.parametrize("extra", [
    "*DSLOAD,AMPLITUDE=ABSENT\nABSENT_SURFACE,P,10\n",
    "*SURFACE,NAME=FACE\nBODY,S9\n",
    "*BOUNDARY\nFIXED,99\n",
])
def test_invalid_load_surface_or_dof_blocks(deck, extra):
    assert check_deck(deck(extra)).status == "FAIL"


def test_surface_load_references_are_resolved(deck):
    report = check_deck(deck("*DSLOAD\nABSENT,P,1\n"))
    assert any("surface:ABSENT" in f.entity_ids for f in report.findings)


def test_contact_does_not_exempt_coincident_nodes(deck):
    extra = ("*NODE\n99,0,0,0\n*NSET,NSET=DUPLICATE\n99\n"
             "*SURFACE,NAME=MASTER,TYPE=NODE\nFIXED\n"
             "*SURFACE,NAME=SLAVE,TYPE=NODE\nDUPLICATE\n"
             "*CONTACT PAIR\nSLAVE,MASTER\n")
    assert "unmerged_nodes" in codes(check_deck(deck(extra)))


def test_extreme_finite_coordinates_and_small_tolerance_do_not_crash(deck):
    report = check_deck(deck("*NODE\n99,1e308,0,0\n"), tolerance=1e-300)
    assert report.status == "PASS"


def test_solve_rechecks_deck_after_edits(deck, monkeypatch, tmp_path):
    chain = FEMChain(tmp_path)
    chain._inp_path = deck()
    assert validate_model(chain._inp_path).status == "PASS"
    chain._inp_path.write_text("", encoding="utf-8", newline="\n")
    with pytest.raises(ModelCheckError):
        chain.solve()


def test_cli_text_and_bad_tolerance(deck, capsys):
    assert main([str(deck("*NODE\n99,0,0,0\n"))]) == 1
    assert "Fix:" in capsys.readouterr().out
    with pytest.raises(SystemExit) as exc:
        main([str(deck()), "--tolerance", "0"])
    assert exc.value.code == 2


def test_gmsh_export_multiple_blocks_has_global_element_ids(tmp_path, monkeypatch):
    from digitalmodel.solvers.gmsh_meshing.mesh_generator import GMSHMeshGenerator

    # Isolate the INP writer from the optional native mesher.
    gen = object.__new__(GMSHMeshGenerator)
    gen.initialized = True
    nodes = np.array([[0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1]])
    elements = {
        "Tetrahedron 4": {"dimension": 3, "connectivity": np.array([[0, 1, 2, 3]])},
        "Triangle 3": {"dimension": 2, "connectivity": np.array([[0, 1, 2]])},
    }
    monkeypatch.setattr(gen, "_extract_mesh_data", lambda: (nodes, elements))
    path = gen.export_mesh_inp(str(tmp_path / "export.inp"))
    # Mesh-only exports are intentionally incomplete analysis decks.
    assert "missing_section" in codes(check_deck(path))
    with path.open("a", encoding="utf-8", newline="\n") as output:
        output.write("*MATERIAL,NAME=STEEL\n*ELASTIC\n200000,0.3\n"
                     "*SOLID SECTION,ELSET=EC3D4,MATERIAL=STEEL\n"
                     "*SOLID SECTION,ELSET=ECPS3,MATERIAL=STEEL\n"
                     "*BOUNDARY\nNALL,1,3\n")
    assert validate_model(path).status == "PASS"
