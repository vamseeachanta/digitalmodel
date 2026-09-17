"""SYNTHETIC complete protocol fixture, never observed MAPDL evidence.

Geometry and numbers come from the local generator and producer reference.
Headers match the narrow parser contract by construction. This tests software
composition only, not native grammar compatibility or independent mathematics.
No files, native processes, network resources or licensed captures are used.
"""
from decimal import Context, Decimal, localcontext

from digitalmodel.ansys.cylinder_benchmark import build_case
from digitalmodel.ansys.cylinder_reference import reference

STRESS = ("sigma_r", "sigma_z", "sigma_theta", "tau_rz", None, None)
COMPONENTS = {"SX": "sigma_r", "SY": "sigma_z", "SZ": "sigma_theta",
              "SXY": "tau_rz", "UX": "u_r", "UY": "u_z"}


def e24(value):
    """Exactly 24 synthetic columns with sixteen mantissa decimal places."""
    number = Decimal(value)
    if number == 0:
        return "  0.0000000000000000E+00"
    with localcontext(Context(prec=50)):
        mantissa, exponent = format(number, ".16E").split("E")
    return (mantissa + "E" + f"{int(exponent):+03d}").rjust(24)


def keyed(token, node, x, y, component, value):
    return (f"{token:<8}{node:8d}" + e24(x) + e24(y)
            + f"{component:<8}" + e24(value) + "\n").encode("ascii")


def block(name, lines):
    return f"OCV_{name}_BEGIN\n" + "".join(lines) + f"OCV_{name}_END\n"


def cdb(case):
    nodes, elements = case["nodes"], case["elements"]
    lines = ["/COM,ANSYS RELEASE SYNTHETIC TEST ONLY\n", "/PREP7\n",
             "ETBLOCK,1,1\n", "(2i9,19a9)\n",
             "".join(f"{n:9d}" for n in [1, 183, 0, 0, 1] + [0] * 16) + "\n",
             "-1\n", "MPDATA,UNBL,1,EX,1,1,200000,\n",
             "MPDATA,UNBL,1,PRXY,1,1,0.3,\n",
             f"NBLOCK,6,SOLID,{len(nodes)},{len(nodes)}\n", "(3i9,6e21.13e3)\n"]
    for node in nodes:
        xyz = [node["x_mm"], node["y_mm"], "0", "0", "0", "0"]
        lines.append(f'{node["node_id"]:9d}{0:9d}{0:9d}'
                     + "".join(f"{Decimal(value):21.13E}" for value in xyz) + "\n")
    lines += ["N,UNBL,LOC,-1,\n", f"EBLOCK,19,SOLID,{len(elements)},{len(elements)}\n",
              "(19i10)\n"]
    for element in elements:
        fields = [1, 1, 0, 0, 0, 0, 0, 0, 8, 0, element["element_id"], *element["nodes"]]
        lines.append("".join(f"{value:10d}" for value in fields) + "\n")
    return ("".join(lines) + "-1\nFINISH\n").encode("ascii")


def load_window(case):
    lines = ["OCV_LOAD_AUDIT_BEGIN\n"]
    if case["pressure_faces"]:
        lines.append("ELEMENT FACE KVAL P1 P2\n")
        for face in case["pressure_faces"]:
            lines.append(f'{face["element_id"]:8d}{4:8d}{1:8d}'
                         + e24(case["pressure_mpa"]) * 2 + "\n")
    else:
        lines.append("NO SURFACE LOADS\n")
    lines.append("NO NODAL FORCES\nNODE DOF VALUE\n")
    lines += [f'{node:8d}{"UY":8}' + e24("0") + "\n" for node in case["bottom_node_ids"]]
    lines += [" ***** ROUTINE COMPLETED *****  ELAPSED TIME = 1.0\n",
              "RESUPREC = 0\n ***** MAPDL SOLUTION ROUTINE *****\n",
              " ***** MAPDL SOLVE COMMAND *****\n FINISH SOLUTION PROCESSING\n",
              " ***** ROUTINE COMPLETED *****  ELAPSED TIME = 2.0\n",
              "RESUPREC = 0\n ***** MAPDL RESULTS INTERPRETATION (POST1) *****\n",
              "OCV_SOLVED_STATE_BEGIN\n"]
    return "".join(lines)


def stress_row(node, values):
    return f"{node:8d}" + "".join(e24(values[q] if q else "0") for q in STRESS) + "\n"


def native_output(case, numerical):
    stress = ["NODE SX SY SZ SXY SYZ SXZ\n"]
    displacement = ["NODE UX UY UZ\n"]
    for station in case["stations"]:
        node = station["node_id"]
        stress.append(stress_row(node, numerical[node]))
        displacement.append(f"{node:8d}" + e24(numerical[node]["u_r"])
                            + e24(numerical[node]["u_z"]) + e24("0") + "\n")
    contributions = []
    for element in case["elements"]:
        contributions += [f'ELEMENT = {element["element_id"]}\n', "NODE SX SY SZ SXY SYZ SXZ\n"]
        contributions += [stress_row(node, numerical[node]) for node in element["nodes"]]
    support = ["NODE FX FY\n"] + [f"{n:8d}" + e24("0") * 2 + "\n"
                                        for n in case["bottom_node_ids"]]
    raw = "/COM,SYNTHETIC PROTOCOL FIXTURE; NOT OBSERVED SOLVER OUTPUT\n"
    raw += block("FORMAT", ["NDIGIT=8\nFTYPE=E\nNWIDTH=24\nDSIGNF=16\nLINE=100\nCHAR=240\n"])
    raw += block("NERR", ["NMERR=200\n"]) + block("OUTRES", ["ALL=ALL\nNAR=NONE\n"])
    raw += load_window(case) + block("STRESS", stress) + block("DISP", displacement)
    raw += block("PRESOL", contributions) + block("SUPPORT", support)
    raw += "NUMBER OF WARNING MESSAGES ENCOUNTERED = 0\nNUMBER OF ERROR MESSAGES ENCOUNTERED = 0\n"
    return raw.encode("ascii")


def dedicated_exports(case, numerical):
    token = case["case_token"]
    stations = b"".join(keyed(token, s["node_id"], s["x_mm"], s["y_mm"], comp,
                             numerical[s["node_id"]][quantity])
                        for s in case["stations"] for comp, quantity in COMPONENTS.items())
    state = dict(NSET=1, LSTP=1, SBST=1, PRE_N=len(case["nodes"]),
                 PRE_E=len(case["elements"]), LIST_N=9, LIST_E=len(case["elements"]),
                 POST_N=len(case["nodes"]), POST_E=len(case["elements"]))
    state_bytes = b"".join(keyed(token, 0, "0", "0", key, str(value)) for key, value in state.items())
    support = b"".join(keyed(token, n["node_id"], n["x_mm"], "0", "RFY", "0")
                       for n in case["nodes"] if n["node_id"] in case["bottom_node_ids"])
    return {"station_values.txt": stations, "state_values.txt": state_bytes,
            "support_reactions.txt": support,
            "precision_witness.txt": keyed(token, 0, "0", "0", "WITNESS", "1.234567890123456")}


def synthetic_protocol(case_id):
    """Return case and raw artifact bytes; COMPLETE is not native qualification."""
    case = build_case(case_id)
    numerical = {node["node_id"]: reference(case["pressure_mpa"], node["x_mm"], node["y_mm"])
                 for node in case["nodes"]}
    artifacts = dedicated_exports(case, numerical)
    artifacts.update({"model.cdb": cdb(case), "native.out": native_output(case, numerical),
                      "jobname.err": b"", "stdout": b"", "stderr": b""})
    return case, artifacts
