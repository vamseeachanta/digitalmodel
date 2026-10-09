"""Deterministic rectangular Q8 mesh for the frozen open-cylinder canary.

PLANE183 node ordering and pressure face4=(I,L) are defined by Ansys v242:
https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/ans_elem/Hlp_E_PLANE183.html
This is explicit mesh construction, not an independent FE solver.
"""
from decimal import (Context, Decimal, localcontext, ROUND_HALF_EVEN,
                     InvalidOperation, DivisionByZero, Overflow)

from digitalmodel.ansys.analysis_records import decimal_text

NUMERIC_CONTEXT = Context(prec=50, rounding=ROUND_HALF_EVEN, Emin=-4096, Emax=4096,
                          capitals=1, clamp=0, flags=[],
                          traps=[InvalidOperation, DivisionByZero, Overflow])


def mapped_mesh(radial_divisions: int, axial_divisions: int) -> dict:
    with localcontext(NUMERIC_CONTEXT):
        return _mapped_mesh(radial_divisions, axial_divisions)


def _mapped_mesh(radial_divisions: int, axial_divisions: int) -> dict:
    """Number half-grid nodes rowwise, omitting each Q8 cell centre."""
    nodes, indices = [], {}
    for j in range(2 * axial_divisions + 1):
        for i in range(2 * radial_divisions + 1):
            if i % 2 and j % 2:
                continue
            identifier = len(nodes) + 1
            indices[i, j] = identifier
            nodes.append({"node_id": identifier,
                          "x_mm": decimal_text(str(750 + Decimal(60) * i / (2 * radial_divisions))),
                          "y_mm": decimal_text(str(Decimal(240) * j / (2 * axial_divisions))),
                          "node_type": "corner" if i % 2 == j % 2 == 0 else "midside"})
    elements = []
    offsets = ((0, 0), (2, 0), (2, 2), (0, 2), (1, 0), (2, 1), (1, 2), (0, 1))
    for j in range(axial_divisions):
        for i in range(radial_divisions):
            elements.append({"element_id": len(elements) + 1,
                             "nodes": [indices[2 * i + di, 2 * j + dj] for di, dj in offsets]})
    return {"nodes": nodes, "elements": elements,
            "stations": sample_stations(nodes, elements),
            "bottom_node_ids": [n["node_id"] for n in nodes if n["y_mm"] == "0"]}


def sample_stations(nodes: list[dict], elements: list[dict]) -> list[dict]:
    by_xy = {(n["x_mm"], n["y_mm"]): n for n in nodes}
    stations = []
    for y in ("60", "120", "180"):
        for radial_id, x in (("inner", "750"), ("middle", "780"), ("outer", "810")):
            node = by_xy[x, y]
            adjacent = [e["element_id"] for e in elements if node["node_id"] in e["nodes"][:4]]
            stations.append({"id": f"{radial_id}_y{y}", "node_id": node["node_id"],
                             "x_mm": x, "y_mm": y, "radial_id": radial_id,
                             "node_type": node["node_type"], "adjacent_element_ids": adjacent,
                             "adjacent_elements": len(adjacent)})
    return stations
