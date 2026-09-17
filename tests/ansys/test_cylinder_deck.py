"""Synthetic command-contract checks; native grammar still needs B2 validation."""
import pytest

from tests.ansys.test_cylinder_benchmark import CASES, build


def lines(case):
    return case["deck_bytes"].decode("ascii").splitlines()


@pytest.mark.parametrize("case_id,n,node_count,element_count", CASES)
def test_deck_mesh_loads_and_omissions(case_id, n, node_count, element_count):
    case = build(case_id)
    deck = lines(case)
    assert len([s for s in deck if s.startswith("N,")]) == node_count
    assert len([s for s in deck if s.startswith("EN,")]) == element_count
    assert [s for s in deck if s.startswith("D,")] == [f"D,{i},UY,0" for i in case["bottom_node_ids"]]
    assert [s for s in deck if s.startswith("SFE,")] == [
        f"SFE,{f['element_id']},4,PRES,1,10,10" for f in case["pressure_faces"]]
    assert not any(s.split(",")[0] in {"SF", "F", "BF", "BFE", "TUNIF", "TREF", "ACEL", "OMEGA"} for s in deck)
    assert "ET,1,PLANE183" in deck
    assert all(s in deck for s in ("KEYOPT,1,1,0", "KEYOPT,1,3,1", "KEYOPT,1,6,0",
                                   "MP,EX,1,200000", "MP,PRXY,1,0.3"))


def test_precision_controls_and_exact_audit_to_solve_window():
    deck = lines(build("ocv-t60-p10-n4"))
    assert deck.count("/CONFIG,STAT") == 2
    assert deck.index("/CONFIG,RESUPREC,0") < deck.index("/PREP7")
    start = deck.index("SFELIST,ALL,ALL")
    assert deck[start:start + 10] == ["SFELIST,ALL,ALL", "FLIST,ALL", "DLIST,ALL",
        "FINISH", "/CONFIG,STAT", "/SOLU", "SOLVE", "FINISH", "/CONFIG,STAT", "/POST1"]
    for command in ("NLGEOM,OFF", "AUTOTS,OFF", "NSUBST,1,1,1", "ERESX,YES",
                    "OUTRES,ERASE", "OUTRES,ALL,ALL", "OUTRES,NAR,NONE", "OUTRES,STAT"):
        assert deck.index(command) < start
    assert "SET,1,1" in deck and "/NERR,STAT" in deck


def test_complete_keyed_export_and_separate_witness():
    case = build("ocv-t60-p10-n4")
    deck = lines(case)
    start = deck.index("*CFOPEN,station_values,txt")
    end = deck.index("*CFCLOS", start)
    exports = [s for s in deck[start:end] if s.startswith("*VWRITE,")]
    assert len(exports) == 54
    for s in exports:
        assert s.split(",")[1] == "'P10N4'"
    stress = [s for s in deck[start:end] if s.startswith("*GET,") and ",S," in s]
    assert len(stress) == 36 and all(s.endswith(",ESOL") for s in stress)
    assert deck[start:end].count("(A8,F8.0,2E24.16,A8,E24.16)") == 54
    assert "*CFOPEN,precision_witness,txt" in deck
    assert not any("APPEND" in s for s in deck)
    assert "*CFOPEN,support_reactions,txt" in deck


def test_full_selection_contributions_and_station_only_listings():
    deck = lines(build("ocv-t60-p10-n4"))
    assert "/GRAPHICS,FULL" in deck and "RSYS,0" in deck
    assert "/FORMAT,8,E,24,16,100,240" in deck and "/FORMAT,STAT" in deck
    p = deck.index("PRESOL,S,COMP")
    s = deck.index("PRNSOL,S,COMP,,,,,ESOL")
    assert p < s
    assert "NSEL,NONE" in deck[p:s]
    assert len([v for v in deck[p:s] if v.startswith("NSEL,A,NODE,,")]) == 9
    assert not any(v.startswith("ESEL") for v in deck[p:s])
    assert "PRNSOL,U,COMP" in deck[s:]
    assert "PRRSOL,F" in deck
    for item in ("NSET", "LSTP", "SBST"):
        assert any(f",ACTIVE,0,SET,{item}" in v for v in deck)


@pytest.mark.parametrize("mutation", ["one_status", "single_precision", "late_load", "wrong_face"])
def test_deck_contract_refuses_mutations(mutation):
    from digitalmodel.ansys.cylinder_benchmark import validate_deck
    case = build("ocv-t60-p10-n4")
    deck = case["deck_bytes"]
    if mutation == "one_status":
        deck = deck.replace(b"/CONFIG,STAT\n", b"", 1)
    elif mutation == "single_precision":
        deck = deck.replace(b"RESUPREC,0", b"RESUPREC,1")
    elif mutation == "late_load":
        deck = deck.replace(b"DLIST,ALL\nFINISH", b"DLIST,ALL\nF,1,FY,1\nFINISH")
    else:
        deck = deck.replace(b",4,PRES,1,10,10", b",2,PRES,1,10,10", 1)
    with pytest.raises(ValueError):
        validate_deck(case["case_id"], deck)
