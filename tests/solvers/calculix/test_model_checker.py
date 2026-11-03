# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic input decks exercise the pre-run gate without a solver."""

import json
import math
import os
from pathlib import Path
import subprocess
import sys
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


@pytest.mark.parametrize("face", ["SPOS", "SNEG", "S1", "S2"])
@pytest.mark.parametrize("interface", ["api", "cli"])
def test_spring_element_surface_cannot_supply_support(deck, capsys, face, interface):
    extra = ("*NODE\n11,2,0,0\n12,3,0,0\n"
             "*ELEMENT,TYPE=SPRING2,ELSET=OTHER\n2,11,12\n"
             "*SPRING,ELSET=OTHER\n1,1\n100\n")
    untied = check_deck(deck(extra))
    assert untied.status == "FAIL"
    assert "unconstrained_part" in codes(untied)
    path = deck(extra + "*SURFACE,NAME=MASTER\nBODY,S1\n"
                f"*SURFACE,NAME=SLAVE\nOTHER,{face}\n"
                "*TIE,NAME=JOIN\nSLAVE,MASTER\n")
    if interface == "api":
        evidence = check_deck(path).to_dict()
    else:
        assert main([path.as_posix(), "--json"]) == 1
        evidence = json.loads(capsys.readouterr().out)
    assert evidence["status"] == "FAIL"
    assert {f["code"] for f in evidence["findings"]} == {
        "parse_error", "empty_surface", "unconstrained_part",
    }
    unsupported = next(f for f in evidence["findings"] if f["code"] == "unconstrained_part")
    assert "element:2" in unsupported["entity_ids"]


@pytest.mark.parametrize("kind,connectivity,expected", [
    ("S3", "11,12,13", {11, 12, 13}),
    ("S4", "11,12,13,14", {11, 12, 13, 14}),
    ("S4R", "11,12,13,14", {11, 12, 13, 14}),
    ("S6", "11,12,13,14,15,16", {11, 12, 13, 14, 15, 16}),
    ("S8", "11,12,13,14,15,16,17,18", {11, 12, 13, 14, 15, 16, 17, 18}),
    ("S8R", "11,12,13,14,15,16,17,18", {11, 12, 13, 14, 15, 16, 17, 18}),
])
@pytest.mark.parametrize("face", ["S1", "S2", "SNEG", "SPOS", "s1", "s2"])
@pytest.mark.parametrize("interface", ["api", "cli"])
def test_shell_surface_aliases_supply_support(deck, capsys, kind, connectivity, expected,
                                             face, interface):
    extra = ("*NODE\n11,2,0,0\n12,3,0,0\n13,2,1,0\n14,3,1,0\n"
             "15,2.5,0,0\n16,2.5,0.5,0\n17,2,0.5,0\n18,3,0.5,0\n"
             f"*ELEMENT,TYPE={kind},ELSET=OTHER\n2,{connectivity}\n"
             "*SHELL SECTION,ELSET=OTHER,MATERIAL=STEEL\n0.1\n")
    assert "unconstrained_part" in codes(check_deck(deck(extra)))
    path = deck(extra + "*SURFACE,NAME=MASTER\nBODY,S1\n"
                f"*SURFACE,NAME=SLAVE\nOTHER,{face}\n"
                "*TIE,NAME=JOIN\nSLAVE,MASTER\n")
    if interface == "api":
        evidence = check_deck(path).to_dict()
    else:
        assert main([path.as_posix(), "--json"]) == 0
        evidence = json.loads(capsys.readouterr().out)
    assert evidence["status"] == "PASS"
    assert evidence["findings"] == []
    # CalculiX 2.22 manual, section 7.128 (*SURFACE): S1/S2 designate
    # the negative/positive shell faces; both contain all shell nodes.
    model = Deck(read_blocks(path))
    model.parse()
    surface = Block("SURFACE", {"NAME": "FACE"}, [["OTHER", face]], 0)
    assert _surface_nodes(model, surface) == expected


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


@pytest.fixture
def generated_set_deck(deck):
    """Make generated sets decide support or sections for two components."""
    base = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8")

    def write(kind, record):
        generated = f"*{kind},{kind}=GENERATED,GENERATE\n{record}\n"
        if kind == "NSET":
            text = base.replace("*BOUNDARY\n1, 1, 3\n",
                                generated + "*BOUNDARY\nGENERATED,1,3\n")
        else:
            text = base.replace("*SOLID SECTION, ELSET=BODY, MATERIAL=STEEL\n"
                                "*SOLID SECTION, ELSET=OTHER, MATERIAL=STEEL\n",
                                generated + "*SOLID SECTION, ELSET=GENERATED,MATERIAL=STEEL\n")
            text = text.replace("*BOUNDARY\n1, 1, 3\n", "*BOUNDARY\n1,1,3\n5,1,3\n")
        return deck(text=text)

    return write


@pytest.mark.parametrize("kind,last", [("NSET", 5), ("ELSET", 2)])
@pytest.mark.parametrize("interface", ["api", "cli"])
@pytest.mark.parametrize("record", [
    "1,,{last}", ",1,{last}", "1, ,{last}", " ,1,{last}",
    "1,{last},,1", "1,{last},1,1",
])
def test_generated_set_rejects_missing_endpoints_and_extra_fields(
    generated_set_deck, capsys, kind, last, interface, record,
):
    path = generated_set_deck(kind, record.format(last=last))
    if interface == "api":
        report = check_deck(path).to_dict()
    else:
        assert main([path.as_posix(), "--json"]) == 1
        report = json.loads(capsys.readouterr().out)
    assert report["status"] == "FAIL"
    assert any(f["code"] == "parse_error" and f["severity"] == "error"
               and f"keyword:{kind}" in f["entity_ids"] and f["line"] > 0
               for f in report["findings"])


@pytest.mark.parametrize("kind,last,expected", [
    ("NSET", 5, {1, 2, 3, 4, 5}), ("ELSET", 2, {1, 2}),
])
@pytest.mark.parametrize("interface", ["api", "cli"])
@pytest.mark.parametrize("record", ["1,{last}", "1,{last},", "1,{last}, ", "1,{last},1"])
def test_generated_set_defaults_only_omitted_increment(
    generated_set_deck, capsys, kind, last, expected, interface, record,
):
    # CalculiX 2.22 User's Manual, 7.53 (*ELSET) and 7.100 (*NSET):
    # endpoints occupy the first two fields; only the increment defaults to 1.
    path = generated_set_deck(kind, record.format(last=last))
    if interface == "api":
        report = validate_model(path).to_dict()
    else:
        assert main([path.as_posix(), "--json"]) == 0
        report = json.loads(capsys.readouterr().out)
    assert report["status"] == "PASS"
    parsed = Deck(read_blocks(path))
    parsed.parse()
    assert parsed.resolve_set(kind.lower(), "GENERATED") == expected


def test_direct_boundary_keeps_members_when_declared(deck):
    base = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8")
    text = base.replace("*BOUNDARY\n1, 1, 3",
                        "*NSET,NSET=FIXED\n1\n*BOUNDARY\nFIXED,1,3\n"
                        "*NSET,NSET=FIXED\n5")
    report = check_deck(deck(text=text))
    assert report.status == "FAIL"
    assert [f.entity_ids for f in report.findings if f.code == "unconstrained_part"] == [
        ("step:0", "element:2")]


def test_direct_section_keeps_members_when_declared(deck):
    base = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8")
    text = base.replace("*ELEMENT, TYPE=C3D4, ELSET=OTHER", "*ELEMENT, TYPE=C3D4")
    text = text.replace("*SOLID SECTION, ELSET=OTHER, MATERIAL=STEEL",
                        "*ELSET,ELSET=BODY\n2")
    report = check_deck(deck(text=text))
    assert report.status == "FAIL"
    assert [f.entity_ids for f in report.findings if f.code == "missing_section"] == [
        ("element:2",)]


@pytest.mark.parametrize("keyword,kind", [("CLOAD", "NSET"), ("DLOAD", "ELSET")])
def test_direct_load_keeps_members_when_declared(deck, monkeypatch, keyword, kind):
    base = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8")
    record = "SELECTED,1,1" if keyword == "CLOAD" else "SELECTED,P1,1"
    text = base + (f"*{kind},{kind}=SELECTED\n1\n*{keyword}\n{record}\n"
                   f"*{kind},{kind}=SELECTED\n2\n")
    resolved = []
    original = Deck.target

    def observe(model, target_kind, value, *args, **kwargs):
        ids = original(model, target_kind, value, *args, **kwargs)
        if value == "SELECTED":
            resolved.append(ids)
        return ids

    monkeypatch.setattr(Deck, "target", observe)
    check_deck(deck(text=text))
    assert resolved == [{1}]


@pytest.mark.parametrize("surface_type,kind,expected", [
    ("NODE", "NSET", {1}), ("ELEMENT", "ELSET", {1, 2, 3}),
])
def test_direct_surface_keeps_members_when_declared(deck, surface_type, kind, expected):
    from digitalmodel.solvers.calculix.model_checker import _references_and_sections

    base = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8")
    later = 5 if kind == "NSET" else 2
    row = "SELECTED" if kind == "NSET" else "SELECTED,S1"
    text = base + (f"*{kind},{kind}=SELECTED\n1\n"
                   f"*SURFACE,NAME=FACE,TYPE={surface_type}\n{row}\n"
                   f"*{kind},{kind}=SELECTED\n{later}\n")
    model = Deck(read_blocks(deck(text=text)))
    model.parse()
    assert _references_and_sections(model)["FACE"] == expected


@pytest.mark.parametrize("card,kind", [
    ("*BOUNDARY\nLATER,1,3", "NSET"), ("*CLOAD\nLATER,1,1", "NSET"),
    ("*DLOAD\nLATER,P1,1", "ELSET"),
    ("*SURFACE,NAME=FACE\nLATER,S1", "ELSET"),
    ("*SOLID SECTION,ELSET=LATER,MATERIAL=STEEL", "ELSET"),
])
def test_direct_card_cannot_acquire_forward_set(deck, card, kind):
    report = check_deck(deck(f"{card}\n*{kind},{kind}=LATER\n1\n"))
    assert report.status == "FAIL"
    assert any(f.code == "missing_reference" and f"{kind.lower()}:LATER" in f.entity_ids
               for f in report.findings)


@pytest.mark.parametrize("nested", [False, True])
def test_reopened_node_set_does_not_expand_earlier_support(deck, nested):
    base = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8")
    sets = "*NSET,NSET=FIXED\n1\n*NSET,NSET=SUPPORT\nFIXED\n"
    if nested:
        sets += "*NSET,NSET=ALIAS\nSUPPORT\n"
    sets += "*NSET,NSET=FIXED\n5\n"
    target = "ALIAS" if nested else "SUPPORT"
    text = base.replace("*BOUNDARY\n1, 1, 3", sets + f"*BOUNDARY\n{target},1,3")
    report = check_deck(deck(text=text))
    assert report.status == "FAIL"
    findings = [f.entity_ids for f in report.findings if f.code == "unconstrained_part"]
    assert findings == [("step:0", "element:2")]


@pytest.mark.parametrize("nested", [False, True])
def test_reopened_element_set_does_not_expand_earlier_section(deck, nested):
    base = (FIXTURES / "disconnected.inp").read_text(encoding="utf-8")
    sets = "*ELSET,ELSET=SELECTED\n1\n*ELSET,ELSET=ASSIGNED\nSELECTED\n"
    if nested:
        sets += "*ELSET,ELSET=ALIAS\nASSIGNED\n"
    sets += "*ELSET,ELSET=SELECTED\n2\n"
    target = "ALIAS" if nested else "ASSIGNED"
    text = base.replace("*MATERIAL", sets + "*MATERIAL")
    text = text.replace("*SOLID SECTION, ELSET=BODY", f"*SOLID SECTION, ELSET={target}")
    text = text.replace("*SOLID SECTION, ELSET=OTHER, MATERIAL=STEEL\n", "")
    report = check_deck(deck("*BOUNDARY\n5,1,3\n", text=text))
    assert report.status == "FAIL"
    findings = [f.entity_ids for f in report.findings if f.code == "missing_section"]
    assert findings == [("element:2",)]


@pytest.mark.parametrize("kind", ["NSET", "ELSET"])
@pytest.mark.parametrize("source", ["explicit", "generated", "inline"])
def test_set_union_copies_expanded_members_at_declaration(deck, kind, source):
    mesh = "*NODE\n1,0,0,0\n2,1,0,0\n3,2,0,0\n*ELEMENT,TYPE=MASS\n1,1\n2,2\n3,3\n"
    if source == "inline":
        first = ("*NODE,NSET=SOURCE\n4,3,0,0\n" if kind == "NSET" else
                 "*ELEMENT,TYPE=MASS,ELSET=SOURCE\n4,3\n")
        expected = {4}
    elif source == "generated":
        first = f"*{kind},{kind}=SOURCE,GENERATE\n1,2,1\n"
        expected = {1, 2}
    else:
        first = f"*{kind},{kind}=SOURCE\n1\n"
        expected = {1}
    text = mesh + first + f"*{kind},{kind}=COPY\nsource\n*{kind},{kind}=NESTED\nCOPY\n"
    text += f"*{kind},{kind}=SOURCE\n3\n"
    parsed = Deck(read_blocks(deck(text=text)))
    parsed.parse()
    assert not parsed.findings
    assert parsed.resolve_set(kind.lower(), "COPY") == expected
    assert parsed.resolve_set(kind.lower(), "NESTED") == expected
    assert parsed.resolve_set(kind.lower(), "SOURCE") == expected | {3}
    assert (kind.lower(), "SOURCE") in parsed.used


@pytest.mark.parametrize("kind", ["NSET", "ELSET"])
@pytest.mark.parametrize("interface", ["api", "cli"])
def test_forward_set_reference_cannot_acquire_later_members(deck, capsys, kind, interface):
    path = deck(f"*{kind},{kind}=COPY\nLATER\n*{kind},{kind}=LATER\n1\n")
    if interface == "cli":
        assert main([path.as_posix(), "--json"]) == 1
        assert json.loads(capsys.readouterr().out)["status"] == "FAIL"
    report = check_deck(path)
    assert report.status == "FAIL"
    assert any(f.code == "missing_reference" and f"{kind.lower()}:LATER" in f.entity_ids
               for f in report.findings)


@pytest.mark.parametrize("kind", ["NSET", "ELSET"])
def test_repeated_set_references_do_not_multiply_members(deck, kind):
    text = f"*{kind},{kind}=S0\n1\n"
    for index in range(1, 41):
        text += f"*{kind},{kind}=S{index}\nS{index - 1},S{index - 1},S{index - 1}\n"
    parsed = Deck([])
    # Stop before the next expansion if duplicates appear, so the regression
    # demonstrates unbounded growth without exhausting the test process.
    for block in read_blocks(deck(text=text)):
        parsed._definition(block)
        assert len(parsed.sets[kind.lower()][block.params[kind]]) == 1
    parsed.nodes[1] = (0.0, 0.0, 0.0)
    parsed.elements[1] = ("MASS", (1,))
    assert parsed.resolve_set(kind.lower(), "S40") == {1}


@pytest.mark.parametrize("kind", ["NSET", "ELSET"])
def test_first_set_declaration_cannot_reference_itself(deck, kind):
    report = check_deck(deck(f"*{kind},{kind}=SELF\nSELF\n"))
    assert report.status == "FAIL"
    assert any(f.code == "missing_reference" and f"{kind.lower()}:SELF" in f.entity_ids
               for f in report.findings)


@pytest.mark.parametrize("kind", ["NSET", "ELSET"])
def test_reopened_self_union_and_forward_mesh_ids_remain_supported(deck, kind):
    text = (f"*{kind},{kind}=SOURCE\n1\n*{kind},{kind}=COPY\nSOURCE\n"
            f"*{kind},{kind}=SOURCE\nSOURCE,2\n"
            "*NODE\n1,0,0,0\n2,1,0,0\n*ELEMENT,TYPE=MASS\n1,1\n2,2\n")
    parsed = Deck(read_blocks(deck(text=text)))
    parsed.parse()
    assert not parsed.findings
    assert parsed.resolve_set(kind.lower(), "SOURCE") == {1, 2}
    assert parsed.resolve_set(kind.lower(), "COPY") == {1}


@pytest.mark.parametrize("kind,entity", [("NSET", "node"), ("ELSET", "element")])
def test_missing_set_members_have_stable_cli_order_across_processes(deck, kind, entity):
    path = deck(f"*{kind},{kind}=MISSING\n12,8,9\n")
    for seed in ("1", "2"):
        result = subprocess.run(
            [sys.executable, "-m", "digitalmodel.solvers.calculix.cli", path.as_posix(), "--json"],
            env={**os.environ, "PYTHONHASHSEED": seed},
            capture_output=True, text=True, encoding="utf-8",
        )
        assert result.returncode == 1
        report = json.loads(result.stdout)
        missing = [f["entity_ids"][0] for f in report["findings"] if f["code"] == "missing_reference"]
        assert missing == [f"{entity}:8", f"{entity}:9", f"{entity}:12"]


@pytest.mark.parametrize("extra", [
    "*NSET,NSET=INVALID\n²\n", "*ELSET,ELSET=INVALID\n²\n",
    "*BOUNDARY\n²,1,3\n", "*DLOAD\n²,P1,1\n",
])
@pytest.mark.parametrize("interface", ["api", "cli"])
def test_nondecimal_digit_ids_return_structured_failure(deck, capsys, extra, interface):
    path = deck(extra)
    if interface == "api":
        report = check_deck(path).to_dict()
    else:
        assert main([path.as_posix(), "--json"]) == 1
        report = json.loads(capsys.readouterr().out)
    assert report["status"] == "FAIL"
    assert any(f["severity"] == "error" for f in report["findings"])


@pytest.mark.parametrize("kind", ["NSET", "ELSET"])
@pytest.mark.parametrize("interface", ["api", "cli"])
def test_oversized_decimal_set_member_returns_parse_error(deck, capsys, kind, interface):
    # Exceed the interpreter's active decimal-conversion limit using input text.
    oversized = "9" * (sys.get_int_max_str_digits() + 1)
    path = deck(f"*{kind},{kind}=INVALID\n{oversized}\n")
    if interface == "api":
        evidence = check_deck(path).to_dict()
    else:
        assert main([path.as_posix(), "--json"]) == 1
        evidence = json.loads(capsys.readouterr().out)
    assert evidence["status"] == "FAIL"
    assert any(f["code"] == "parse_error" and f["severity"] == "error"
               and f"keyword:{kind}" in f["entity_ids"] and f["line"] > 0
               for f in evidence["findings"])


@pytest.mark.parametrize("record", [
    "1,1,2,\n3,4", "1,1,2,3,4,", "1,1,2,\n3,4,",
    "1,1,\n** continuation comment\n2,3,\n4",
])
def test_element_connectivity_continuation(deck, record):
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8")
    path = deck(text=base.replace("1, 1, 2, 3, 4", record))
    assert validate_model(path).status == "PASS"
    parsed = Deck(read_blocks(path))
    parsed.parse()
    assert parsed.elements == {1: ("C3D4", (1, 2, 3, 4))}


@pytest.mark.parametrize("interface", ["api", "cli"])
@pytest.mark.parametrize("record", [
    ",1,1,2,3,4", "1,,1,2,3,4", "1,1,,2,3,4", "1,1,2,,3,4",
    "1,1,2,3,,4", "1,1,2,3,4,,", "1,1, ,2,3,4",
    "1,1,2,,\n3,4", "1,1,2,\n,3,4", "1,1,2,\n3,,4",
    "1,1,2,\n3,4,,", "1,1,2,3,",
])
def test_missing_connectivity_ids_return_structured_failure(deck, capsys, record, interface):
    base = (FIXTURES / "supported.inp").read_text(encoding="utf-8")
    path = deck(text=base.replace("1, 1, 2, 3, 4", record))
    if interface == "api":
        report = check_deck(path).to_dict()
    else:
        assert main([path.as_posix(), "--json"]) == 1
        report = json.loads(capsys.readouterr().out)
    assert report["status"] == "FAIL"
    assert any(f["code"] == "parse_error" and f["severity"] == "error"
               and "keyword:ELEMENT" in f["entity_ids"]
               for f in report["findings"])


def test_incomplete_connectivity_at_end_of_file_returns_parse_error(deck):
    report = check_deck(deck(text="*NODE\n1,0,0,0\n*ELEMENT,TYPE=C3D4\n1,1,2,3, \n"))
    assert report.status == "FAIL"
    assert "parse_error" in codes(report)


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


def test_plate_extraction_excludes_boundary_entities_before_gate(tmp_path):
    nodes = np.array([[0., 0., 0.], [1., 0., 0.], [0., 1., 0.], [0., 0., 1.]])
    # A volume mesh also contains geometric edges, faces and vertices. They
    # describe its boundary and must not become structural beams or shells.
    entities = {
        1: ("Line 2", 1, 2, [1, 2]),
        2: ("Triangle 3", 2, 3, [1, 2, 3]),
        4: ("Tetrahedron 4", 3, 4, [1, 2, 3, 4]),
        15: ("Point", 0, 1, [1]),
    }
    mesh = SimpleNamespace(
        getNodes=lambda: (np.arange(1, 5), nodes.ravel(), []),
        getElementTypes=lambda: list(entities),
        getElementsByType=lambda kind: (np.array([kind]), np.array(entities[kind][3])),
        getElementProperties=lambda kind: (
            entities[kind][0], entities[kind][1], 1, entities[kind][2]),
    )
    chain = FEMChain(tmp_path)
    chain._extract_gmsh_data(SimpleNamespace(model=SimpleNamespace(mesh=mesh)))
    chain._node_sets = {"FIXED": [0]}
    path = chain.setup_analysis({"name": "STEEL", "E": 200000, "nu": 0.3}, [],
                                [{"node_set": "FIXED", "dof_start": 1, "dof_end": 3}])
    assert validate_model(path).status == "PASS"
    assert set(chain._elements) == {"Tetrahedron 4"}
    parsed = Deck(read_blocks(path))
    parsed.parse()
    assert parsed.elements == {1: ("C3D4", (1, 2, 3, 4))}


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
