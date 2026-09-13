"""Synthetic blocked exports exercise parsing, not native qualification."""
import math

import pytest


def integers(values, width=9):
    return "".join(f"{value:{width}d}" for value in values)


def reals(values, width=20, precision=9):
    return "".join(f"{value:{width}.{precision}G}" for value in values)


def synthetic_cdb():
    """Independent coarse rectangle-with-hole fixture; no production imports."""
    corners = [0, math.atan2(80, 200), math.pi / 2,
               math.pi - math.atan2(80, 200), math.pi,
               math.pi + math.atan2(220, 200), 3 * math.pi / 2,
               2 * math.pi - math.atan2(220, 200), 2 * math.pi]
    angles = [a + (b - a) * j / 4 for a, b in zip(corners, corners[1:])
              for j in range(4)]
    nodes = []
    for outer in (False, True):
        for a in angles:
            x, y = math.cos(a), math.sin(a)
            radius = min(200 / abs(x) if abs(x) > 1e-14 else 1e20,
                         (80 if y > 0 else 220) / abs(y) if abs(y) > 1e-14 else 1e20)
            radius = radius if outer else 40
            nodes.append((200 + radius * x, 220 + radius * y))
    lines = ["/COM,ANSYS RELEASE 2026 R1.01 BUILD 26.1", "/PREP7",
             "ETBLOCK,1,1", "(2i9,19a9)",
             integers([1, 182, 0, 0, 3] + [0] * 16), "       -1",
             "NBLOCK,6,SOLID,64,64", "(3i9,6e21.13e3)"]
    lines += [integers([i, 0, 0]) + reals(xy, 21, 13)
              for i, xy in enumerate(nodes, 1)]
    lines += ["N,UNBL,LOC,-1,", "EBLOCK,19,SOLID,32,32", "(19i10)"]
    lines += [integers([1, 1, 1, 1, 0, 0, 0, 0, 4, 0, i + 1,
                        i + 1, i + 33, (i + 1) % 32 + 33, (i + 1) % 32 + 1], 10)
              for i in range(32)]
    lines += ["        -1", "RLBLOCK,1,1,6,7", "(2i8,6g16.9)", "(7g16.9)",
              integers([1, 6], 8) + reals([8, 0, 0, 0, 0, 0], 16)]
    for name, value in [("EX", 205000), ("NUXY", .3), ("PRXY", .3)]:
        lines += ["MPTEMP,UNBL,1,1,0,", f"MPDATA,UNBL,1,{name},1,1,{value},"]
    base = [i for i, (_, y) in enumerate(nodes, 1) if abs(y) < 1e-7]
    lines += [f"DBLOCK,{2 * len(base)}", "(i9,a6,2(pg16.9))"]
    lines += [integers([i]) + f"{dof:6}" + reals([0, 0], 16)
              for i in base for dof in ("UX", "UY")]
    lines += ["       -1", "SFEBLOCK,4,PRES,32,256,0", "(i9,i4,i4,6(pg20.9))"]
    lines += [integers([i]) + integers([4, kval], 4) + reals([1, 2, 0, 0] if kval == 1 else [0] * 4)
              for i in range(1, 17) for kval in (1, 2)]
    return "\n".join(lines + ["SFE,end,LOC,-1,", "/GO", "FINISH"])


def parse(text):
    from tests.ansys.padeye_pressure_cdb import parse_pressure_cdb
    return parse_pressure_cdb(text, 16)


def test_observed_block_profile_and_blank_z():
    mesh, pressures = parse(synthetic_cdb())
    assert len(mesh["nodes"]) == 64
    assert mesh["elements"][0] == {"id": 1, "nodes": [1, 33, 34, 2]}
    assert mesh["thickness_mm"] == 8
    assert mesh["youngs_modulus_mpa"] == 205000
    assert mesh["poisson_ratio"] == .3
    assert mesh["center_mm"] == [200, 220]
    assert mesh["radius_mm"] == 40
    assert len(pressures) == 16  # SFEBLOCK 256 is not 256 row records.
    assert pressures[0] == {"element": 1, "face": 4, "p1_mpa": 1, "p2_mpa": 2}


@pytest.mark.parametrize("old,new", [
    ("2026 R1.01", "2025 R1.01"), ("NBLOCK,6", "NBLOCK,3"),
    ("64,64", "64,63"), ("32,32", "32,31"),
    (integers([1, 182]), integers([1, 183])),
    (integers([1, 182, 0, 0, 3]), integers([1, 182, 0, 0, 0])),
    (reals([8, 0, 0, 0, 0, 0], 16), reals([9, 0, 0, 0, 0, 0], 16)),
    ("EX,1,1,205000", "EX,1,1,200000"),
    ("NUXY,1,1,0.3", "NUXY,1,1,0.4"),
    ("PRXY,1,1,0.3", "PRXY,1,1,nan"),
    ("SFEBLOCK,4,PRES", "SFEBLOCK,4,CONV"),
    ("32,256,0", "32,256,1"),
    ("SFE,end,LOC,-1,", ""), ("FINISH", "F,1,FY,50000\nFINISH"),
    ("FINISH", "ACEL,0,1,0\nFINISH"),
    ("FINISH", "MPDATA,UNBL,1,EX,1,1,205000,\nFINISH"),
    ("FINISH", "ETBLOCK,1,1\nFINISH"),
    ("FINISH", "D,1,UX,0\nFINISH"),
    ("FINISH", "SOLVE\nFINISH"),
])
def test_reject_unsupported_or_tampered_records(old, new):
    text = synthetic_cdb()
    assert old in text
    with pytest.raises(ValueError):
        parse(text.replace(old, new, 1))


@pytest.mark.parametrize("kind", ["duplicate_node", "duplicate_element", "duplicate_pressure",
                                 "missing_imag", "nonzero_imag", "extra_pressure_value",
                                 "negative_pressure", "nonfinite_node", "nonzero_z",
                                 "nonzero_constraint", "missing_constraint", "unknown_node"])
def test_reject_block_row_corruption(kind):
    lines = synthetic_cdb().splitlines()
    node = lines.index("(3i9,6e21.13e3)") + 1
    elem = lines.index("(19i10)") + 1
    pressure = lines.index("(i9,i4,i4,6(pg20.9))") + 1
    support = lines.index("(i9,a6,2(pg16.9))") + 1
    if kind.startswith("duplicate_"):
        row = {"duplicate_node": node, "duplicate_element": elem,
               "duplicate_pressure": pressure}[kind]
        lines.insert(row, lines[row])
    elif kind == "missing_imag":
        del lines[pressure + 1]
    elif kind in ("nonzero_imag", "extra_pressure_value", "negative_pressure"):
        row = pressure + (kind == "nonzero_imag")
        values = {"nonzero_imag": [1, 0, 0, 0], "extra_pressure_value": [1, 2, 1, 0],
                  "negative_pressure": [-1, 2, 0, 0]}[kind]
        lines[row] = lines[row][:17] + reals(values)
    elif kind == "nonfinite_node":
        lines[node] = lines[node][:27] + reals([float("nan"), 220], 21, 13)
    elif kind == "nonzero_z":
        lines[node] += reals([1], 21, 13)
    elif kind == "nonzero_constraint":
        lines[support] = lines[support][:15] + reals([1, 0], 16)
    elif kind == "missing_constraint":
        del lines[support]
    elif kind == "unknown_node":
        lines[elem] = lines[elem][:-10] + integers([999], 10)
    with pytest.raises(ValueError):
        parse("\n".join(lines))
