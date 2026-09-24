"""APDL generation for the approved PLANE183 canary; native execution unverified.

Native grammar: pinned v242 Command Reference pages Hlp_C_EN, Hlp_C_GET,
Hlp_C_PRNSOL, Hlp_C_PRESOL, Hlp_C_FORMAT, Hlp_C_CFOPEN, Hlp_C_VWRITE,
Hlp_C_CONFIG, Hlp_C_NERR, Hlp_C_SFELIST, Hlp_C_PRRSOL and Hlp_C_CDWRITE:
https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/ans_cmd/
Facts/source hashes are in the issue-2121 vendor-source-metadata register.
The generated markers delimit evidence, never assert numerical acceptance.
"""
from digitalmodel.ansys.cylinder_deck_exports import postprocessing


def marked(name: str, commands: list[str]) -> list[str]:
    return [f"/COM,OCV_{name}_BEGIN", *commands, f"/COM,OCV_{name}_END"]


def preparation(case: dict) -> list[str]:
    lines = ["/BATCH", "/CLEAR,NOSTART", "/CONFIG,RESUPREC,0",
             f"/TITLE,Open cylinder verification {case['case_token']}", "/PREP7",
             "CSYS,0", "ET,1,PLANE183", "KEYOPT,1,1,0", "KEYOPT,1,3,1", "KEYOPT,1,6,0",
             "MP,EX,1,200000", "MP,PRXY,1,0.3", "TYPE,1", "MAT,1", "ESYS,0"]
    lines += [f"N,{n['node_id']},{n['x_mm']},{n['y_mm']},0" for n in case["nodes"]]
    lines += ["EN," + ",".join(map(str, [e["element_id"], *e["nodes"]])) for e in case["elements"]]
    lines += ["ALLSEL,ALL", "CDWRITE,DB,model,cdb"]
    lines += marked("MESH", ["NLIST,ALL", "ELIST,ALL", "ETLIST,ALL", "MPLIST,ALL"])
    return lines + ["FINISH", "/SOLU"]


def solution(case: dict) -> list[str]:
    lines = ["ANTYPE,STATIC", "NLGEOM,OFF", "AUTOTS,OFF", "NSUBST,1,1,1", "TIME,1",
             "ERESX,YES", "ALLSEL,ALL"]
    lines += [f"D,{identifier},UY,0" for identifier in case["bottom_node_ids"]]
    lines += [f"SFE,{face['element_id']},4,PRES,1,10,10" for face in case["pressure_faces"]]
    lines += marked("OUTRES", ["OUTRES,ERASE", "OUTRES,ALL,ALL", "OUTRES,NAR,NONE", "OUTRES,STAT"])
    lines += marked("NERR", ["/NERR,STAT"])
    lines += ["ALLSEL,ALL", "/COM,OCV_LOAD_AUDIT_BEGIN", "SFELIST,ALL,ALL", "FLIST,ALL", "DLIST,ALL",
              "FINISH", "/CONFIG,STAT", "/SOLU", "SOLVE", "FINISH", "/CONFIG,STAT",
              "/POST1", "SET,1,1", "/COM,OCV_SOLVED_STATE_BEGIN"]
    return lines


def render_deck(case: dict) -> bytes:
    lines = preparation(case) + solution(case) + postprocessing(case)
    return ("\n".join(lines) + "\n").encode("ascii")
