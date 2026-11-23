"""Native keyed exports and independent native listings for the frozen canary.

*GET NODE S uses explicit ESOL; PRNSOL DataKey is argument7 in v242.
The native exporter does not decide whether retrieved values are valid.
Complete diagnostics, configuration, selection, mesh and independent readback
must be adjudicated separately before an engineering comparison is established.
"""
FORMAT = "(A8,F8.0,2E24.16,A8,E24.16)"


def record(token: str, node: int, component: str, value: str, x="OCVX", y="OCVY") -> list[str]:
    return [f"*VWRITE,'{token}',{node},{x},{y},'{component}',{value}", FORMAT]


def coordinates(node: int) -> list[str]:
    return [f"*GET,OCVX,NODE,{node},LOC,X", f"*GET,OCVY,NODE,{node},LOC,Y"]


def counts(prefix: str) -> list[str]:
    return [f"*GET,OCV{prefix}N,NODE,0,COUNT", f"*GET,OCV{prefix}E,ELEM,0,COUNT"]


def station_export(case: dict) -> list[str]:
    lines = ["*CFOPEN,station_values,txt"]
    for station in case["stations"]:
        node = station["node_id"]
        lines += coordinates(node)
        for token, item, component in (("SX", "S", "X"), ("SY", "S", "Y"),
                                      ("SZ", "S", "Z"), ("SXY", "S", "XY"),
                                      ("UX", "U", "X"), ("UY", "U", "Y")):
            # Sentinel does not replace missing-value checks: native warning gate
            # and independent listing/contribution comparison remain mandatory.
            lines += ["OCVQ=9.876543210123456E90",
                      f"*GET,OCVQ,NODE,{node},{item},{component}" + (",ESOL" if item == "S" else "")]
            lines += record(case["case_token"], node, token, "OCVQ")
    return lines + ["*CFCLOS"]


def independent_listings(case: dict) -> list[str]:
    lines = ["/COM,OCV_PRESOL_BEGIN", "PRESOL,S,COMP", "/COM,OCV_PRESOL_END", "NSEL,NONE"]
    lines += [f"NSEL,A,NODE,,{s['node_id']}" for s in case["stations"]]
    lines += counts("LIST")
    lines += ["/COM,OCV_STRESS_BEGIN", "PRNSOL,S,COMP,,,,,ESOL", "/COM,OCV_STRESS_END",
              "/COM,OCV_DISP_BEGIN", "PRNSOL,U,COMP", "/COM,OCV_DISP_END", "NSEL,ALL"]
    return lines + counts("POST")


def reaction_export(case: dict) -> list[str]:
    lines = ["NSEL,NONE"]
    lines += [f"NSEL,A,NODE,,{identifier}" for identifier in case["bottom_node_ids"]]
    lines += ["/COM,OCV_SUPPORT_BEGIN", "PRRSOL,F", "/COM,OCV_SUPPORT_END",
              "*CFOPEN,support_reactions,txt"]
    for node in case["bottom_node_ids"]:
        lines += coordinates(node)
        lines += ["OCVQ=9.876543210123456E90", f"*GET,OCVQ,NODE,{node},RF,FY"]
        lines += record(case["case_token"], node, "RFY", "OCVQ")
    return lines + ["*CFCLOS", "NSEL,ALL"]


def metadata_export(case: dict) -> list[str]:
    token = case["case_token"]
    lines = ["*CFOPEN,state_values,txt"]
    for component, parameter in [(name, "OCV" + name) for name in ("NSET", "LSTP", "SBST")]:
        lines += record(token, 0, component, parameter, "0", "0")
    for prefix in ("PRE", "LIST", "POST"):
        for entity in ("N", "E"):
            lines += record(token, 0, prefix + "_" + entity, "OCV" + prefix + entity, "0", "0")
    lines += ["*CFCLOS", "*CFOPEN,precision_witness,txt", "OCVWIT=1.234567890123456"]
    lines += record(token, 0, "WITNESS", "OCVWIT", "0", "0")
    return lines + ["*CFCLOS"]


def postprocessing(case: dict) -> list[str]:
    lines = ["/GRAPHICS,FULL", "RSYS,0", "ALLSEL,ALL"]
    lines += counts("PRE")
    lines += [f"*GET,OCV{name},ACTIVE,0,SET,{name}" for name in ("NSET", "LSTP", "SBST")]
    lines += ["/COM,OCV_FORMAT_BEGIN", "/FORMAT,8,E,24,16,100,240", "/FORMAT,STAT",
              "/COM,OCV_FORMAT_END"]
    lines += station_export(case) + independent_listings(case)
    lines += reaction_export(case) + metadata_export(case)
    return lines + ["FINISH"]
