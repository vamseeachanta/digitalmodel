"""Read the observed v261 preparation export; never execute CDB commands.

Format reference: Ansys v261 Programmer's Reference, Coded Database File
Commands (NBLOCK, EBLOCK, ETBLOCK, RLBLOCK, DBLOCK and SFEBLOCK):
https://ansyshelp.ansys.com/public/Views/Secured/corp/v261/en/ans_prog/Hlp_P_INT3_3.html
Observed source SHA256:
2844eac33b334cd67469c9d49d21bbd1cc600a010f473a2b7a96522eb966c8fe
That preparation failed mesh quality. Its format is evidence, not qualification.
Corrected no-SOLVE source was also replayed read-only, SHA256:
f1b53a670920d562e2d3657e5917a73f1d2f87f7481d9d0cc8aaa3a0e32e599f
The observed SFEBLOCK 256 header differs from its 32 real/imaginary rows;
the terminator and complete paired face records govern parsing, not that field.
Geometry precision 1e-7 mm allows formatted export rounding only. Returned
geometry is checked against the approved case, not inferred as design intent.
Pressure integration and native quality disposition remain separate checks.
"""
import math
from collections import Counter


def require(condition, message):
    if not condition:
        raise ValueError(message)


def number(value):
    result = float(value)
    require(math.isfinite(result), "Nonfinite numeric field")
    return result


def fields(line, widths):
    require(not line[sum(widths):].strip(), "Unexpected trailing fields")
    padded = line.ljust(sum(widths))
    values, start = [], 0
    for width in widths:
        values.append(padded[start:start + width].strip())
        start += width
    return values


def tokens(line):
    return [part.strip().upper() for part in line.split("!", 1)[0].split(",")]


def block(lines, index, formats, end):
    for fmt in formats:
        require(index < len(lines) and lines[index].strip().lower() == fmt, "Unsupported block format")
        index += 1
    rows = []
    while index < len(lines) and tokens(lines[index]) != end:
        require(bool(lines[index].strip()), "Blank block row")
        rows.append(lines[index])
        index += 1
    require(index < len(lines), "Missing block terminator")
    return rows, index + 1


def read_nodes(header, rows):
    require(header[:3] == ["NBLOCK", "6", "SOLID"] and len(header) == 5, "Unsupported NBLOCK")
    nodes = []
    for row in rows:
        values = fields(row, [9] * 3 + [21] * 6)
        identifier, solid, location = map(int, values[:3])
        require(solid == location == 0, "Unsupported node flags")
        xyz = [number(v or "0") for v in values[3:]]
        # Native exports omit trailing zero Y as well as Z on base nodes.
        require(values[3] and not any(xyz[2:]), "Nonplanar or rotated node")
        nodes.append({"id": identifier, "x_mm": xyz[0], "y_mm": xyz[1]})
    check_ids(nodes, header[3:])
    return nodes


def check_ids(records, declared):
    ids = [record["id"] for record in records]
    require(ids and min(ids) > 0 and len(set(ids)) == len(ids), "Missing or duplicate IDs")
    require([max(ids), len(ids)] == list(map(int, declared)), "Block count/max ID mismatch")


def read_elements(header, rows):
    require(header[:3] == ["EBLOCK", "19", "SOLID"] and len(header) == 5, "Unsupported EBLOCK")
    elements = []
    for row in rows:
        values = list(map(int, fields(row, [10] * 15)))
        require(values[:10] == [1, 1, 1, 1, 0, 0, 0, 0, 4, 0], "Unsupported element attributes")
        require(len(set(values[11:])) == 4, "Degenerate element")
        elements.append({"id": values[10], "nodes": values[11:]})
    check_ids(elements, header[3:])
    return elements


def read_type(header, rows):
    require(header == ["ETBLOCK", "1", "1"] and len(rows) == 1, "Unsupported ETBLOCK")
    require(list(map(int, fields(rows[0], [9] * 21))) ==
            [1, 182, 0, 0, 3] + [0] * 16, "Expected PLANE182 plane stress with thickness")


def read_real(header, row):
    require(header == ["RLBLOCK", "1", "1", "6", "7"], "Unsupported RLBLOCK")
    values = fields(row, [8, 8] + [16] * 6)
    require(list(map(int, values[:2])) == [1, 6], "Unsupported real constant ID/count")
    require(list(map(number, values[2:])) == [8, 0, 0, 0, 0, 0], "Unexpected real constants")


def read_constraints(header, rows, nodes):
    require(len(header) == 2 and int(header[1]) == len(rows), "DBLOCK count mismatch")
    actual = []
    for row in rows:
        identifier, dof, real, imag = fields(row, [9, 6, 16, 16])
        require(number(real) == number(imag) == 0, "Nonzero prescribed displacement")
        actual.append((int(identifier), dof))
    base = {n["id"] for n in nodes if abs(n["y_mm"]) <= 1e-7}
    expected = {(identifier, dof) for identifier in base for dof in ("UX", "UY")}
    require(base and len(actual) == len(set(actual)) and set(actual) == expected,
            "Expected exactly zero UX/UY on all base nodes")
    return sorted(base)


def read_pressures(header, rows, elements, upper_edges):
    require(len(header) == 6 and header[:3] == ["SFEBLOCK", "4", "PRES"] and
            int(header[3]) == max(e["id"] for e in elements) and
            int(header[4]) > 0 and header[5] == "0", "Unsupported SFEBLOCK")
    records = {}
    ids = {e["id"] for e in elements}
    for row in rows:
        values = fields(row, [9, 4, 4] + [20] * 4)
        element, face, kval = map(int, values[:3])
        require(element in ids and face in (1, 2, 3, 4) and kval in (1, 2), "Unknown pressure face/key")
        key = element, face, kval
        require(key not in records, "Duplicate pressure row")
        pressure = list(map(number, values[3:]))
        require(min(pressure) >= 0 and pressure[2:] == [0, 0], "Unsupported pressure values")
        require(kval == 1 or pressure == [0] * 4, "Imaginary pressure unsupported")
        records[key] = pressure
    faces = {(e, f) for e, f, _ in records}
    require(len(faces) == upper_edges and len(records) == 2 * len(faces), "Missing pressure pairs/faces")
    return [{"element": e, "face": f, "p1_mpa": records[e, f, 1][0],
             "p2_mpa": records[e, f, 1][1]} for e, f in sorted(faces)]


def check_geometry(nodes, elements):
    xy = {n["id"]: (n["x_mm"], n["y_mm"]) for n in nodes}
    require(len(set(xy.values())) == len(xy), "Duplicate coordinates")
    edges = Counter()
    for element in elements:
        ids = element["nodes"]
        require(set(ids) <= xy.keys(), "Unknown connectivity node")
        edges.update(tuple(sorted((a, b))) for a, b in zip(ids, ids[1:] + ids[:1]))
    require(set().union(*(set(e["nodes"]) for e in elements)) == xy.keys(), "Unused nodes")
    require(all(count <= 2 for count in edges.values()), "Nonmanifold incidence")
    for x, y in xy.values():
        require(-1e-7 <= x <= 400 + 1e-7 and -1e-7 <= y <= 300 + 1e-7 and
                math.hypot(x - 200, y - 220) >= 40 - 1e-7, "Unexpected case geometry")
    boundary = [edge for edge, count in edges.items() if count == 1]
    for a, b in boundary:
        p, q = xy[a], xy[b]
        circle = all(abs(math.hypot(x - 200, y - 220) - 40) <= 1e-7 for x, y in (p, q))
        outer = any(abs(p[axis] - value) <= 1e-7 and abs(q[axis] - value) <= 1e-7
                    for axis, value in ((0, 0), (0, 400), (1, 0), (1, 300)))
        require(circle or outer, "Unexpected free boundary")
    require(all(any(abs(x - a) <= 1e-7 and abs(y - b) <= 1e-7 for x, y in xy.values())
                for a, b in ((0, 0), (400, 0), (400, 300), (0, 300))), "Missing plate corners")


def check_material(parts, materials):
    require(len(parts) == 8 and parts[:3] == ["MPDATA", "UNBL", "1"] and
            parts[4:6] == ["1", "1"] and parts[7] == "", "Unsupported MPDATA")
    name = parts[3]
    require(name in ("EX", "NUXY", "PRXY") and name not in materials, "Unexpected/duplicate material")
    require(number(parts[6]) == (205000 if name == "EX" else .3), "Material mismatch")
    materials.add(name)


def inert_command(parts):
    """Permit only the observed zero-load and nonloading export boilerplate."""
    joined = ",".join(parts)
    fixed = {"", "/PREP7", "/NOPR", "/GO", "FINISH", "DOF,DELETE", "*ELSE", "*ENDIF",
             "*IF,_CDRDOFF,EQ,1,THEN", "_CDRDOFF=", "BFUNIF,TEMP,_TINY",
             "ERESX,DEFA", "EXTOPT,ACLEAR,0", "EXTOPT,ATTR,0,0,0"}
    if joined in fixed or parts[0] in ("/COM", "/TITLE"):
        return True
    zero_counts = {"ACEL": 3, "OMEGA": 4, "DOMEGA": 3, "CGLOC": 3, "CGOMEGA": 3,
                   "DCGOMG": 3, "TREF": 1, "IRLF": 1, "KUSE": 1, "TIME": 1,
                   "ALPHAD": 1, "BETAD": 1, "DMPRAT": 1, "DMPSTR": 1, "NEQIT": 1}
    if parts[0] in zero_counts:
        return len(parts) == zero_counts[parts[0]] + 1 and all(number(v) == 0 for v in parts[1:])
    if parts[:2] == ["EXTOPT", "ESIZE"]:
        return len(parts) == 4 and all(number(v) == 0 for v in parts[2:])
    if parts[0] == "CRPLIM":
        return len(parts) == 3 and (number(parts[1]), int(parts[2])) in ((.1, 0), (0, 1))
    if parts[0] == "NCNV":
        return list(map(number, parts[1:])) == [1, 0, 0, 0, 0]
    if parts[:2] in (["*SET", "_RETURN"], ["*SET", "_STATUS"]):
        return len(parts) == 3 and number(parts[2]) == 0
    return False


def read_blocks(lines):
    """Consume all records; nothing unknown is silently skipped."""
    parsed, materials, offsets = {}, set(), {}
    index = 0
    formats = {"ETBLOCK": (["(2i9,19a9)"], ["-1"]),
               "NBLOCK": (["(3i9,6e21.13e3)"], ["N", "UNBL", "LOC", "-1", ""]),
               "EBLOCK": (["(19i10)"], ["-1"]),
               "DBLOCK": (["(i9,a6,2(pg16.9))"], ["-1"]),
               "SFEBLOCK": (["(i9,i4,i4,6(pg20.9))"], ["SFE", "END", "LOC", "-1", ""])}
    while index < len(lines):
        parts = tokens(lines[index])
        command = parts[0]
        index += 1
        if command in formats or command == "RLBLOCK":
            require(command not in parsed, "Duplicate block")
            if command == "RLBLOCK":
                require(index + 2 < len(lines) and lines[index:index + 2] ==
                        ["(2i8,6g16.9)", "(7g16.9)"], "Unsupported real format")
                rows, index = [lines[index + 2]], index + 3
            else:
                rows, index = block(lines, index, *formats[command])
            parsed[command] = (parts, rows)
        elif command == "MPDATA":
            check_material(parts, materials)
        elif command == "MPTEMP":
            require(len(parts) == 6 and parts[:4] == ["MPTEMP", "UNBL", "1", "1"] and
                    number(parts[4]) == 0 and parts[5] == "", "Unsupported material temperature")
        elif command == "NUMOFF":
            require(len(parts) == 3 and parts[1] not in offsets, "Unsupported offsets")
            offsets[parts[1]] = int(parts[2])
        else:
            require(inert_command(parts), f"Unsupported CDB command: {command}")
    require(materials == {"EX", "NUXY", "PRXY"}, "Missing material properties")
    require(set(parsed) == set(formats) | {"RLBLOCK"}, "Missing required block")
    return parsed, offsets


def parse_pressure_cdb(text, upper_edges):
    """Return checked native-record mesh/pressures; unsupported profiles raise ValueError."""
    require(type(upper_edges) is int and upper_edges in (16, 32, 64), "Unsupported arc count")
    require(isinstance(text, str) and "\x00" not in text and "$" not in text, "Invalid CDB text")
    lines = text.splitlines()
    require(lines and lines[0].startswith("/COM,ANSYS RELEASE 2026 R1.01"), "Unsupported CDB release")
    require(lines[-1].strip() == "FINISH", "Incomplete CDB")
    parsed, offsets = read_blocks(lines)
    read_type(*parsed["ETBLOCK"])
    nodes = read_nodes(*parsed["NBLOCK"])
    elements = read_elements(*parsed["EBLOCK"])
    read_real(parsed["RLBLOCK"][0], parsed["RLBLOCK"][1][0])
    check_geometry(nodes, elements)
    base = read_constraints(*parsed["DBLOCK"], nodes)
    pressures = read_pressures(*parsed["SFEBLOCK"], elements, upper_edges)
    expected = {"NODE": max(n["id"] for n in nodes), "ELEM": max(e["id"] for e in elements),
                "MAT": 1, "REAL": 1, "TYPE": 1}
    require(not offsets or offsets == expected, "Unexpected offsets")
    mesh = {"nodes": nodes, "elements": elements, "thickness_mm": 8,
            "center_mm": [200, 220], "radius_mm": 40, "upper_edges": upper_edges,
            "youngs_modulus_mpa": 205000, "poisson_ratio": .3, "fixed_base_node_ids": base}
    return mesh, pressures
