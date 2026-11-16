"""Every workbook input is read only through a checked cell that applies the
Python module's rule, and every value the module refuses is #N/A in the
workbook (recalculated with the ``formulas`` package).

Second-provider reviews found one unchecked input per round (x and the
angular factor at 39213804, the tolerances at 1be46a8d, the fender count and
the velocity override at e19273b9). These tests cover the class: the
structural test fails for any formula that reads an input cell other than
through its checked cell, and the recalculation tests set every value the
module refuses, for every inventoried input
(docs/reports/modules/fender_design/workbook-input-checks.md), and accepted
boundary values that must equal the module.
"""

from __future__ import annotations

import dataclasses
import math
import re
import sys
import warnings
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "examples" / "hydrodynamics" / "fender_design"
COMMITTED = ROOT / "docs" / "reports" / "modules" / "fender_design" / "fender-design-generic.xlsx"
INVENTORY = ROOT / "docs" / "reports" / "modules" / "fender_design" / "workbook-input-checks.md"
sys.path.insert(0, str(EXAMPLE))

openpyxl = pytest.importorskip("openpyxl")
pytest.importorskip("yaml")

import build_fender_workbook as bw  # noqa: E402
from fender_case import compute, load_case, size_checks  # noqa: E402

from digitalmodel.hydrodynamics import fender_design as fd  # noqa: E402

BLANK = "<blank>"   # an empty input cell (None in the case file)

#: Inputs value -> the case-file key it mirrors.
CASE_KEY = {
    "M": ("ship", "displacement_t"), "DWT": ("ship", "dwt_t"), "LBP": ("ship", "lbp_m"),
    "BEAM": ("ship", "beam_m"), "T": ("ship", "draft_m"), "RHO": ("ship", "water_density_t_m3"),
    "DEPTH": ("berth", "water_depth_m"), "X": ("berth", "contact_x_from_bow_m"),
    "ALPHA": ("berth", "berthing_angle_deg"), "STRUCT": ("berth", "structure"),
    "DEFL": ("berth", "fender_deflection_m"), "TUGS": ("conditions", "tugs"),
    "NCALLS": ("conditions", "berthings_per_year"), "CONT": ("conditions", "continuous_berth"),
    "VOVR": ("design", "velocity_m_s"), "N": ("design", "fenders_in_contact"),
    "ETOL": ("design", "energy_tolerance"), "RTOL": ("design", "reaction_tolerance"),
    "ANG": ("design", "angular_factor"), "RLIM": ("design", "reaction_limit_kn"),
    "PLIM": ("design", "hull_pressure_limit_kpa"), "UFCF": ("conditions", "ufc_f_basis"),
}
#: Workbook codes of the module's yes/no and choice values; any other value is passed as is.
CODES = {"TUGS": {1: True, 0: False}, "CONT": {1: True, 0: False},
         "UFCF": {1: "berthing_coefficient", 0: "block_coefficient"}}

#: Catalogue columns on Fenders -> FenderRating field.
FENDER_FIELD = {col: field for col, field, *_ in bw.FENDER_ROW_RULES}


def _case_value(name, value):
    """The case-file value a workbook value stands for (Excel holds 2 and 2.0 alike)."""
    if value == BLANK:
        return None
    if name in CODES and not isinstance(value, bool) and value in CODES[name]:
        return CODES[name][value]
    if name == "N" and isinstance(value, float) and value.is_integer():
        return int(value)
    return value


def _case(changes):
    case = load_case(EXAMPLE / "generic_case.yaml")
    for name, value in changes.items():
        block, key = CASE_KEY[name]
        case.setdefault(block, {})[key] = _case_value(name, value)
    return case


def _key(k):
    return str(k).upper().replace("'", "").split("]")[-1]


class Book:
    """The generated workbook, compiled once; ``calc`` recalculates with cells changed."""

    def __init__(self, path):
        import schedula as sh
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            import formulas
            self.model = formulas.ExcelModel().loads(str(path)).finish()
            base = self.model.calculate()
        self._empty = [[sh.EMPTY]]
        self.keys = {_key(k): k for k in base}
        self.base = self._values(base)
        self.wb = openpyxl.load_workbook(path)

    @staticmethod
    def _values(sol):
        out = {}
        for k, v in sol.items():
            try:
                out[_key(k)] = v.value[0][0]
            except (TypeError, IndexError, AttributeError):
                continue
        return out

    def ref(self, name):
        sheet, ref = list(self.wb.defined_names[name].destinations)[0]
        return f"{sheet.upper()}!{ref.replace('$', '')}"

    def row(self, name):
        return int(self.ref(name).split("!B")[1])

    def calc(self, cells):
        """``cells``: ``SHEET!A1`` or an input name -> value (BLANK for an empty cell)."""
        inputs = {}
        for cell, value in cells.items():
            cell = cell if "!" in cell else self.ref(cell)
            inputs[self.keys[cell]] = self._empty if value == BLANK else value
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            return self._values(self.model.calculate(inputs=inputs))


@pytest.fixture(scope="module")
def built(tmp_path_factory):
    pytest.importorskip("formulas")
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    out = tmp_path_factory.mktemp("wbchk") / "generic.xlsx"
    bw.build_workbook(res, out)
    return res, out, Book(out)


def _na(v):
    return str(v) == "#N/A"


def _number(v):
    return isinstance(v, (int, float)) and not isinstance(v, bool) and math.isfinite(v)


# --------------------------------------------------------------------------
# Structure: no formula reads an input except through its checked cell
# --------------------------------------------------------------------------


def _formula_cells(wb):
    for ws in wb.worksheets:
        for row in ws.iter_rows():
            for c in row:
                if isinstance(c.value, str) and c.value.startswith("="):
                    # quoted text cannot reference a cell
                    yield f"{ws.title.upper()}!{c.coordinate}", re.sub(r'"[^"]*"', '""', c.value)


def _input_readers(wb):
    """Input name -> cells whose formula reads it, and the formulas that address Inputs directly."""
    names = [n for n, *_ in bw.INPUT_ROWS]
    pats = {n: re.compile(rf"(?<![A-Za-z0-9_.!]){n}(?![A-Za-z0-9_(])") for n in names}
    readers = {n: set() for n in names}
    direct = []
    for cell, f in _formula_cells(wb):
        if re.search(r"inputs!", f, re.IGNORECASE):
            direct.append(cell)
        for n, pat in pats.items():
            if pat.search(f):
                readers[n].add(cell)
    return readers, direct


def _check_structure(wb, n_rows):
    ref = lambda n: (lambda s, r: f"{s.upper()}!{r.replace('$', '')}")(  # noqa: E731
        *list(wb.defined_names[n].destinations)[0])
    readers, direct = _input_readers(wb)
    assert direct == [], f"formulas addressing Inputs cells directly: {direct}"
    for name, cells in readers.items():
        checked = ref(bw.checked_name(name))
        check = f"INPUTS!E{ref(name).split('!B')[1]}"
        assert checked in cells, f"{name}: its checked cell {checked} does not read it"
        assert cells <= {checked, check}, f"{name} read unchecked by {sorted(cells - {checked, check})}"
    # every input cell on Inputs is inventoried (a defined name with a rule)
    on_inputs = {n for n, dn in wb.defined_names.items()
                 if any(s == "Inputs" and r.startswith("$B$") for s, r in dn.destinations)}
    assert on_inputs == set(bw.INPUT_RULES) == {n for n, *_ in bw.INPUT_ROWS}
    # Fenders: results read the catalogue values only through the checked columns
    fen = wb["Fenders"]
    raw = "DEFGHIJ"
    checked_cols = {c for c, _ in bw.FENDER_CHECKED_COLS.values()} | {bw.FENDER_ROW_CHECK_COL}
    for row in fen.iter_rows(min_row=2, max_row=n_rows + 1):
        for c in row:
            f = c.value
            if not (isinstance(f, str) and f.startswith("=")):
                continue
            used = set(re.findall(r"(?<![A-Za-z_])([A-Z]{1,2})(\d+)(?![A-Za-z0-9_(])", re.sub(r'"[^"]*"', "", f)))
            raw_used = {col for col, _ in used if col in raw}
            if c.column_letter not in checked_cols:
                assert not raw_used, f"Fenders!{c.coordinate} reads catalogue column(s) {raw_used} unchecked"
            elif c.column_letter != bw.FENDER_ROW_CHECK_COL:
                src = next(k for k, (v, _) in bw.FENDER_CHECKED_COLS.items() if v == c.column_letter)
                assert raw_used == {src}, (c.coordinate, raw_used)
    for i in range(2, n_rows + 2):
        assert fen[f"{bw.FENDER_ROW_CHECK_COL}{i}"].value == bw._fender_row_check(i)


def test_generated_workbook_reads_every_input_only_through_its_checked_cell(built):
    res, out, _ = built
    _check_structure(openpyxl.load_workbook(out), len(res.catalogue))


def test_committed_workbook_reads_every_input_only_through_its_checked_cell():
    wb = openpyxl.load_workbook(COMMITTED)
    _check_structure(wb, wb["Fenders"].max_row - 1)


def test_reader_scan_catches_an_unchecked_read(built):
    # the scan itself: a formula reading N directly (the e19273b9 defect) is reported
    _, out, _ = built
    wb = openpyxl.load_workbook(out)
    wb["Methods"]["Z1"] = "=E_DESIGN/N"
    with pytest.raises(AssertionError, match="N read unchecked"):
        _check_structure(wb, wb["Fenders"].max_row - 1)
    wb = openpyxl.load_workbook(out)
    wb["Fenders"]["Z2"] = "=G2*2"
    with pytest.raises(AssertionError, match="unchecked"):
        _check_structure(wb, wb["Fenders"].max_row - 1)


def test_checked_cells_validation_and_check_text_use_one_rule(built):
    _, out, book = built
    wb = openpyxl.load_workbook(out)
    ws = wb["Inputs"]
    rules = {rng.coord: dv for dv in ws.data_validations.dataValidation for rng in dv.sqref.ranges}
    for name, *_ in bw.INPUT_ROWS:
        rule, r = bw.INPUT_RULES[name], book.row(name)
        sheet, cell = list(wb.defined_names[bw.checked_name(name)].destinations)[0]
        assert wb[sheet][cell.replace("$", "")].value == rule.checked(name), name
        dv = rules[f"B{r}"]
        assert dv.type == "custom" and dv.formula1 == rule.valid(f"B{r}"), name
        assert dv.showErrorMessage and dv.errorStyle == "stop", name
        assert bool(dv.allow_blank) is (rule.blank is not None), name
        assert ws.cell(row=r, column=5).value == bw._check_text_formula(name), name
    fen = wb["Fenders"]
    frules = {rng.coord: dv for dv in fen.data_validations.dataValidation for rng in dv.sqref.ranges}
    last = fen.max_row
    for col, _field, cond, blank, _ in bw.FENDER_ROW_RULES:
        dv = frules[f"{col}2:{col}{last}"]
        assert dv.formula1 == cond.format(v=f"{col}2") and dv.errorStyle == "stop"
        assert bool(dv.allow_blank) is blank


def test_shipped_inputs_are_accepted(built):
    _, _, book = built
    for name, *_ in bw.INPUT_ROWS:
        text = book.base[f"INPUTS!E{book.row(name)}"]
        assert text == "ok" or text.startswith("blank: "), (name, text)
    assert book.base[book.ref("INPUTS_OK")] == 1
    for i in range(2, book.wb["Fenders"].max_row + 1):
        assert book.base[f"FENDERS!{bw.FENDER_ROW_CHECK_COL}{i}"] == "ok"


def test_every_input_is_covered_by_these_tests_and_the_inventory():
    names = {n for n, *_ in bw.INPUT_ROWS}
    assert set(CASE_KEY) == names
    assert {n for n, values in INVALID.items() if values} == names
    text = INVENTORY.read_text(encoding="utf-8")
    listed = set(re.findall(r"^\| `([A-Z]+)` \|", text, re.MULTILINE))
    assert listed == names
    cols = set(re.findall(r"^\| Fenders `([D-J])` \|", text, re.MULTILINE))
    assert cols == set(FENDER_FIELD)


# --------------------------------------------------------------------------
# Every value the module refuses is #N/A
# --------------------------------------------------------------------------

_POS = [0, -1.0, "abc", True, BLANK]          # a number above 0 required
_POS_OR_BLANK = [0, -1.0, "abc", True]        # blank allowed
_CODE = [2, -1, 0.5, "yes", BLANK]            # 1 or 0 required (TRUE: see STRICTER)

INVALID = {
    "M": _POS, "DWT": _POS_OR_BLANK, "LBP": _POS, "BEAM": _POS, "T": _POS, "RHO": _POS,
    "DEPTH": [*_POS, 12.0, 11.0],             # draught 12 m: no keel clearance
    "X": [-1.0, 180.5, 1800.0, "abc", True, BLANK],
    "ALPHA": ["abc", True, BLANK],
    "STRUCT": ["OPEN", "piled", 1, True, BLANK],
    "DEFL": _POS, "TUGS": _CODE, "NCALLS": _POS, "CONT": _CODE,
    "VOVR": _POS_OR_BLANK,
    "N": [0, -1, 1.5, "two", True, BLANK],
    "ETOL": [-0.2, -1e-9, 1.0, 1.5, "abc", True, BLANK],
    "RTOL": [-0.1, -1e-9, "abc", True, BLANK],
    "ANG": [0, -0.5, 1.0000001, 1.2, "abc", True, BLANK],
    "RLIM": _POS_OR_BLANK, "PLIM": _POS_OR_BLANK, "UFCF": [*_CODE, True],
}

#: Values the workbook refuses although the module would take their meaning: a
#: logical TRUE or FALSE typed where the cell's codes are 1 and 0 (the module's
#: yes/no is a boolean). Refusing them cannot produce a result the module would
#: not; the inventory states the difference.
STRICTER = [("TUGS", True), ("TUGS", False), ("CONT", True), ("CONT", False)]

_ALL = ("pianc2002", "wg211", "bs6349", "eau", "rom", "ufc", "ueda", "is4651")
#: Calc cells derived from each input (beyond its checked cell), #N/A with it.
DEPENDS = {
    "M": ("CB",), "DWT": ("V_b_DWT", "V_a_DWT"), "LBP": ("CB", "K", "XG", "XA"),
    "BEAM": ("CB", "RCG", "CM_VC"), "T": ("CB", "KCD", "CM_VC"), "RHO": ("CB",),
    "DEPTH": ("KCD", "CM_P", "CM_UFC"), "X": ("XG", "XA", "CE", "CE_UFC", "CE_IS"),
    "ALPHA": ("RN", "CE", "CE_IS", "CC_UFC"), "STRUCT": ("CC_P", "CC_UFC"), "DEFL": ("CS_P", "CS_ROM"),
    "TUGS": (), "NCALLS": (), "CONT": ("CG_ROM",), "VOVR": (), "N": (), "ETOL": (), "RTOL": (),
    "ANG": (), "RLIM": (), "PLIM": (), "UFCF": ("CF_UFC", "CM_UFC"),
}


def _refused(case):
    try:
        compute(case, citations="record")
    except ValueError:
        return True
    return False


@pytest.mark.parametrize("name, value", [(n, v) for n, vs in INVALID.items() for v in vs],
                         ids=lambda x: repr(x))
def test_a_refused_input_is_na_in_its_results_and_checks(built, name, value):
    res, _, book = built
    assert _refused(_case({name: value})), f"the module accepts {name}={value!r}"
    vals = book.calc({name: value})
    assert _na(vals[book.ref(bw.checked_name(name))])
    assert str(vals[f"INPUTS!E{book.row(name)}"]).startswith("ERROR: "), vals[f"INPUTS!E{book.row(name)}"]
    for n in DEPENDS[name]:
        assert _na(vals[book.ref(n)]), n
    # the module refuses the case: every energy, capacity, reaction and check
    for n in ("INPUTS_OK", "E_DESIGN", "E_FENDER"):
        assert _na(vals[book.ref(n)]), n
    for i, r in enumerate(res.methods, start=2):
        assert _na(vals[f"METHODS!H{i}"]) and _na(vals[f"METHODS!J{i}"]), r.method
    for i, f in enumerate(res.catalogue, start=2):
        for col in "KLMOPQ":         # capacity, utilisation, design R, Energy/Reaction/Pressure OK
            assert _na(vals[f"FENDERS!{col}{i}"]), (col, i)
        if f.hull_pressure_kpa is not None:
            assert _na(vals[f"FENDERS!N{i}"]), i


@pytest.mark.parametrize("name, value", STRICTER, ids=lambda x: repr(x))
def test_a_logical_in_a_yes_no_code_cell_is_refused(built, name, value):
    _, _, book = built
    assert not _refused(_case({name: value}))           # the module reads a boolean
    vals = book.calc({name: value})
    assert _na(vals[book.ref(bw.checked_name(name))]) and _na(vals[book.ref("INPUTS_OK")])
    assert str(vals[f"INPUTS!E{book.row(name)}"]).startswith("ERROR: ")


@pytest.mark.parametrize("col, value", [(c, v) for c in "GH" for v in (0, -700.0, "abc", True, BLANK)]
                         + [(c, v) for c in "DEFI" for v in (0, -1.0, "abc", True)]
                         + [("J", v) for v in (0, -5.0, 100.5, "abc", True)], ids=lambda x: repr(x))
def test_a_refused_catalogue_value_is_na_in_its_row_and_every_check(built, col, value):
    res, _, book = built
    f = res.catalogue[1]
    with pytest.raises(ValueError):
        dataclasses.replace(f, **{FENDER_FIELD[col]: None if value == BLANK else value})
    vals = book.calc({f"FENDERS!{col}3": value})
    assert str(vals[f"FENDERS!{bw.FENDER_ROW_CHECK_COL}3"]).startswith("ERROR: ")
    for c in (*(v for v, _ in bw.FENDER_CHECKED_COLS.values()), "K", "L", "M", "O"):
        assert _na(vals[f"FENDERS!{c}3"]), c
    assert _na(vals[book.ref("INPUTS_OK")]) and _na(vals[book.ref("E_DESIGN")])
    for i in range(2, len(res.methods) + 2):
        assert _na(vals[f"METHODS!J{i}"]), i
    for i in range(2, len(res.catalogue) + 2):
        for c in "KLMOPQ":
            assert _na(vals[f"FENDERS!{c}{i}"]), (c, i)
    assert vals["FENDERS!V2"] == "ok"


# --------------------------------------------------------------------------
# Accepted values, boundaries included, equal the module
# --------------------------------------------------------------------------


def _module(changes, res):
    """Per-method energies (None where the method refuses), the selected energy and the
    fender checks of every catalogue row, from the module for the changed case."""
    case = _case(changes)
    s, b, d = case["ship"], case["berth"], case["design"]
    fcase = fd.Case(fd.Ship(**s), fd.Berth(**b), **case["conditions"])
    kw = dict(velocity_m_s=d.get("velocity_m_s"), factor=d.get("factor"),
              factor_position=d.get("factor_position", "smallest"), citations="off")
    energies = {}
    for m in _ALL:
        try:
            energies[m] = fd.energy_by_method(fcase, method=m, **kw)
        except ValueError:
            energies[m] = None
    sel = energies[d["method"]]
    n = fd.check_fenders_in_contact(d.get("fenders_in_contact", 1))
    checks = None
    if sel is not None:
        checks = size_checks(res.catalogue, res.base_fender.fender_id, sel.design_energy_knm / n, d,
                             sel.capacity_divisor, curve=res.curve, curve_type=res.base_fender.fender_type)
    return energies, sel, checks


def _yn(x):
    return "-" if x is None else ("yes" if x else "no")


ACCEPTED = [
    ("X", 0.0), ("X", 180.0), ("ALPHA", 0.0), ("ALPHA", -5.0), ("DEPTH", 12.0001),
    ("STRUCT", "closed"), ("STRUCT", "semi_closed"), ("DEFL", 0.15), ("TUGS", 0), ("CONT", 0),
    ("NCALLS", 301.0), ("UFCF", 0), ("VOVR", 0.08), ("VOVR", BLANK), ("N", 1), ("N", 2.0),
    ("ETOL", 0.0), ("ETOL", 0.999), ("RTOL", 0.0), ("ANG", 1.0), ("ANG", 1e-6),
    ("RLIM", BLANK), ("RLIM", 1e-6), ("PLIM", BLANK), ("PLIM", 1e9), ("M", 90_000.0),
    ("DWT", 3_000.0), ("DWT", 600_000.0), ("DWT", BLANK), ("M", 500.0), ("M", 600_000.0),
]


@pytest.mark.parametrize("name, value", ACCEPTED, ids=lambda x: repr(x))
def test_an_accepted_input_matches_the_module(built, name, value):
    res, _, book = built
    vals = book.calc({name: value})
    text = vals[f"INPUTS!E{book.row(name)}"]
    assert text == "ok" or (value == BLANK and text.startswith("blank: ")), text
    assert vals[book.ref("INPUTS_OK")] == 1
    energies, sel, checks = _module({name: value}, res)
    # method rows: #N/A exactly where the module refuses that method (table ranges, no DWT)
    for i, r in enumerate(res.methods, start=2):
        e = energies[r.method]
        if e is None:
            assert _na(vals[f"METHODS!J{i}"]), r.method
        else:
            assert vals[f"METHODS!J{i}"] == pytest.approx(e.design_energy_knm, rel=1e-9), r.method
    if sel is None:
        assert _na(vals[book.ref("E_DESIGN")])
        return
    assert vals[book.ref("E_DESIGN")] == pytest.approx(sel.design_energy_knm, rel=1e-9)
    ids = [f.fender_id for f in res.catalogue]
    for c in checks:
        i = ids.index(c.fender_id) + 2
        assert vals[f"FENDERS!K{i}"] == pytest.approx(c.capacity_knm, rel=1e-9), c.fender_id
        assert vals[f"FENDERS!L{i}"] == pytest.approx(c.utilisation, rel=1e-9), c.fender_id
        assert vals[f"FENDERS!M{i}"] == pytest.approx(c.reaction_kn, rel=1e-9), c.fender_id
        assert vals[f"FENDERS!O{i}"] == _yn(c.meets_energy), c.fender_id
        assert vals[f"FENDERS!P{i}"] == _yn(c.meets_reaction), c.fender_id
        assert vals[f"FENDERS!Q{i}"] == ("no (no datum)" if c.pressure_missing else _yn(c.meets_pressure))


def test_accepted_values_include_each_method_table_limit(built):
    # the cases above reach a method refusal without a refused input (no silent extrapolation)
    res, _, _ = built
    refused = set()
    for name, value in ACCEPTED:
        energies, _, _ = _module({name: value}, res)
        refused |= {m for m, e in energies.items() if e is None}
    assert {"pianc2002", "ueda", "eau", "bs6349", "ufc"} <= refused


@pytest.mark.parametrize("col, value", [("J", 100.0), ("D", BLANK), ("E", BLANK), ("F", BLANK), ("I", BLANK)],
                         ids=lambda x: repr(x))
def test_an_accepted_catalogue_value_keeps_the_row(built, col, value):
    res, _, book = built
    f = dataclasses.replace(res.catalogue[1], **{FENDER_FIELD[col]: None if value == BLANK else value})
    vals = book.calc({f"FENDERS!{col}3": value})
    assert vals[f"FENDERS!{bw.FENDER_ROW_CHECK_COL}3"] == "ok"
    assert vals[book.ref("INPUTS_OK")] == 1
    per = res.selected.design_energy_knm / res.case["design"]["fenders_in_contact"]
    c = size_checks([f], res.base_fender.fender_id, per, res.case["design"], res.selected.capacity_divisor,
                    curve=res.curve, curve_type=res.base_fender.fender_type)[0]
    assert vals["FENDERS!K3"] == pytest.approx(c.capacity_knm, rel=1e-12)
    assert vals["FENDERS!M3"] == pytest.approx(c.reaction_kn, rel=1e-12)
    assert vals["FENDERS!Q3"] == ("no (no datum)" if c.pressure_missing else _yn(c.meets_pressure))
