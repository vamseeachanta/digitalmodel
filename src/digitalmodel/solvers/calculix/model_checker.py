# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pre-run connectivity and reference QA for structural CalculiX INP decks.

Support means a topological path to at least one structural boundary condition
in every step. It does not establish sufficient restraint, stiffness rank or
contact engagement. Coordinates and tolerance use the deck's length unit.
"""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import asdict, dataclass
from itertools import product
import math
from pathlib import Path

from ._deck import Block, Deck, Finding, MASS_TYPES, SECTION_KEYWORDS, number, read_blocks


@dataclass(frozen=True)
class ModelCheckReport:
    """Structured evidence retained by the CLI and solver gate."""

    findings: tuple[Finding, ...]
    tolerance: float

    @property
    def status(self) -> str:
        """Return FAIL for errors; warnings pass the gate."""
        return "FAIL" if any(f.severity == "error" for f in self.findings) else "PASS"

    def to_dict(self) -> dict:
        """Return JSON-compatible evidence without embedding input paths."""
        return {"status": self.status, "tolerance": self.tolerance,
                "findings": [asdict(f) for f in self.findings]}


class ModelCheckError(ValueError):
    """Block solver launch while retaining all gate findings."""

    def __init__(self, report: ModelCheckReport) -> None:
        """Retain structured gate evidence for a blocked run."""
        self.report = report
        codes = sorted({f.code for f in report.findings if f.severity == "error"})
        super().__init__("CalculiX model QA gate failed: " + ", ".join(codes))


class _Components:
    def __init__(self, nodes: Iterable[int]) -> None:
        self.parent = {nid: nid for nid in nodes}

    def root(self, nid: int) -> int:
        while self.parent[nid] != nid:
            self.parent[nid] = self.parent[self.parent[nid]]
            nid = self.parent[nid]
        return nid

    def join(self, nodes: Iterable[int]) -> None:
        known = [nid for nid in nodes if nid in self.parent]
        if known:
            first = self.root(known[0])
            for nid in known[1:]:
                self.parent[self.root(nid)] = first


def _surface_nodes(model: Deck, block: Block) -> set[int]:
    """Resolve actual element faces so unrelated face nodes are not tie-exempt."""
    nodes = set()
    # Zero-based indices derived from CalculiX 2.22 User's Manual, section
    # 7.128 (*SURFACE), with midside nodes from section 6.2.1-10 diagrams:
    # https://www.dhondt.de/ccx_2.22.pdf. Face orientation is irrelevant here;
    # only membership establishes the connectivity/tie-exemption node sets.
    faces = {
        4: ((0, 1, 2), (0, 3, 1), (1, 3, 2), (2, 3, 0)),
        10: ((0, 1, 2, 4, 5, 6), (0, 3, 1, 7, 8, 4),
             (1, 3, 2, 8, 9, 5), (2, 3, 0, 9, 7, 6)),
        8: ((0, 1, 2, 3), (4, 7, 6, 5), (0, 4, 5, 1),
            (1, 5, 6, 2), (2, 6, 7, 3), (3, 7, 4, 0)),
        20: ((0, 1, 2, 3, 8, 9, 10, 11), (4, 7, 6, 5, 15, 14, 13, 12),
             (0, 4, 5, 1, 16, 12, 17, 8), (1, 5, 6, 2, 17, 13, 18, 9),
             (2, 6, 7, 3, 18, 14, 19, 10), (3, 7, 4, 0, 19, 15, 16, 11)),
        6: ((0, 1, 2), (3, 5, 4), (0, 3, 4, 1), (1, 4, 5, 2), (2, 5, 3, 0)),
        15: ((0, 1, 2, 6, 7, 8), (3, 5, 4, 11, 10, 9),
             (0, 3, 4, 1, 12, 9, 13, 6), (1, 4, 5, 2, 13, 10, 14, 7),
             (2, 5, 3, 0, 14, 11, 12, 8)),
    }
    for row in block.rows:
        if not row or not row[0]:
            raise ValueError("Empty surface target")
        if block.params.get("TYPE", "ELEMENT").upper() == "NODE":
            nodes.update(model.target("nset", row[0], block.line))
            continue
        if block.params.get("TYPE", "ELEMENT").upper() != "ELEMENT":
            raise ValueError("Unsupported surface type")
        for eid in model.target("elset", row[0], block.line):
            kind, conn = model.elements[eid]
            face = row[1].upper()
            if kind.startswith("C3D"):
                if not face.startswith("S"):
                    raise ValueError("Invalid face")
                index = int(face[1:]) - 1
                if index < 0 or index >= len(faces[len(conn)]):
                    raise ValueError("Invalid face")
                nodes.update(conn[i] for i in faces[len(conn)][index])
            elif kind.startswith("S") and face in {"SPOS", "SNEG"}:
                nodes.update(conn)
            else:
                # Unsupported surface geometry must not manufacture a support path.
                raise ValueError("Unsupported surface face")
    return nodes.intersection(model.nodes)


def _references_and_sections(model: Deck) -> dict[str, set[int]]:
    assigned = set()
    surfaces = {}
    for block in model.blocks:
        k, p = block.keyword, block.params
        try:
            for param, kind in (("NSET", "nset"), ("ELSET", "elset"),
                                ("MATERIAL", "material"), ("AMPLITUDE", "amplitude"),
                                ("INTERACTION", "interaction"), ("SURFACE", "surface")):
                if param in p and not (k in {"NODE", "ELEMENT", "NSET", "ELSET"}
                                       and param in {"NSET", "ELSET"}):
                    model.reference(kind, p[param], block.line)
            if k in SECTION_KEYWORDS:
                ids = model.target("elset", p["ELSET"], block.line)
                if not ids:
                    model.findings.append(Finding("warning", "unused_section",
                        (f"section:{k}:{p['ELSET'].upper()}",), "Section has no elements",
                        "Assign the section to a populated element set or remove it.", block.line))
                # A missing material must not count as a valid structural section.
                if k in {"SOLID SECTION", "SHELL SECTION", "BEAM SECTION", "MEMBRANE SECTION"}:
                    model.reference("material", p["MATERIAL"], block.line)
                if "COMPOSITE" in p:
                    raise ValueError("Composite section references require a reader extension")
                for eid in sorted(ids):
                    kind, _ = model.elements[eid]
                    if kind.startswith(SECTION_KEYWORDS[k]):
                        assigned.add(eid)
                    else:
                        model.error("incompatible_section", (f"element:{eid}", f"section:{k}"),
                                    f"{k} does not support element type {kind} in this gate",
                                    "Use a compatible section/property; see CalculiX 2.22 "
                                    "manual sections 6.2, 7.3, 7.81, 7.117, 7.118 and 7.121.",
                                    block.line)
            elif k == "SURFACE":
                surfaces[p["NAME"].upper()] = _surface_nodes(model, block)
            elif k in {"CLOAD", "DLOAD", "DSLOAD"}:
                for row in block.rows:
                    if k == "DSLOAD":
                        model.reference("surface", row[0], block.line)
                    else:
                        model.target("nset" if k == "CLOAD" else "elset", row[0], block.line)
            elif k in {"MPC", "EQUATION", "RIGID BODY", "COUPLING", "SUBMODEL", "MODEL CHANGE",
                       "SYSTEM", "TRANSFORM", "NGEN", "NFILL", "NCOPY", "ELGEN", "ELCOPY",
                       "PART", "ASSEMBLY", "INSTANCE"}:
                model.error("unsupported_topology", (f"keyword:{k}",),
                            "This constraint or topology operation is not supported",
                            "Extend and test the gate for this operation before running the deck.", block.line)
        except (ValueError, KeyError, IndexError):
            model.error("parse_error", (f"keyword:{k}",), "Invalid section, reference or surface data",
                        "Correct the keyword data or extend its supported syntax.", block.line)
    for eid in sorted(model.elements.keys() - assigned):
        model.error("missing_section", (f"element:{eid}",), "Element has no section or mass property",
                    "Assign an appropriate section or mass property to its element set.")
    return surfaces


def _connections(model: Deck, surfaces: dict[str, set[int]],
                 components: _Components) -> list[tuple[set[int], set[int]]]:
    ties = []
    for block in model.blocks:
        if block.keyword not in {"TIE", "CONTACT PAIR"}:
            continue
        for row in block.rows:
            if len(row) != 2 or not all(row):
                model.error("parse_error", (f"keyword:{block.keyword}",), "Expected two surface names",
                            "Provide a defined slave surface and master surface.", block.line)
                continue
            names = [v.upper() for v in row]
            for name in names:
                model.reference("surface", name, block.line)
            left, right = (surfaces.get(name, set()) for name in names)
            if not left or not right:
                model.error("empty_surface", tuple(f"surface:{n}" for n in names),
                            "Connection has an empty or unresolved surface",
                            "Populate both surfaces with valid mesh entities.", block.line)
                continue
            components.join(left | right)
            if block.keyword == "TIE":
                ties.append((left, right))
            else:
                model.findings.append(Finding("warning", "contact_path",
                    tuple(f"surface:{n}" for n in names),
                    "Contact supplies a possible support path; engagement is not established",
                    "Confirm initial gaps, normals and active contact in the analysis.", block.line))
    return ties


def _boundary_stages(model: Deck) -> list[tuple[int, set[int]]]:
    active: set[int] = set()
    stages = []
    in_step = False
    first_boundary = True
    step = 0
    rotational = {n for kind, nodes in model.elements.values()
                  if kind.startswith(("S", "B")) and kind != "SPRING2" for n in nodes}
    for block in model.blocks:
        try:
            if block.keyword == "STEP":
                if in_step:
                    raise ValueError("Nested step")
                step += 1
                in_step = True
                first_boundary = True
            elif block.keyword == "END STEP":
                if not in_step:
                    raise ValueError("Unmatched end step")
                stages.append((step, active.copy()))
                in_step = False
            elif block.keyword == "BOUNDARY":
                # CalculiX 2.22 User's Manual, section 7.4 (*BOUNDARY): OP
                # applies only on the first boundary card in each step.
                if first_boundary and block.params.get("OP", "MOD").upper() == "NEW":
                    active.clear()
                first_boundary = False
                for row in block.rows:
                    nodes = model.target("nset", row[0], block.line)
                    kind = row[1].upper()
                    if kind in {"ENCASTRE", "PINNED", "XSYMM", "YSYMM", "ZSYMM",
                                "XASYMM", "YASYMM", "ZASYMM"}:
                        active.update(nodes)
                    else:
                        first = int(kind)
                        last = int(row[2]) if len(row) > 2 and row[2] else first
                        if not 1 <= first <= last <= 6:
                            raise ValueError("Invalid structural degree of freedom")
                        if len(row) > 3 and row[3]:
                            number(row[3])
                        # Rotational BCs do not constrain translational-only solid nodes.
                        active.update(nodes if first <= 3 else nodes & rotational)
        except (ValueError, IndexError):
            model.error("parse_error", (f"keyword:{block.keyword}",), "Invalid step or boundary data",
                        "Correct step pairing and structural boundary condition records.", block.line)
    if in_step:
        model.error("parse_error", (f"step:{step}",), "Step has no END STEP",
                    "Terminate the analysis step.")
    if not stages:
        stages.append((0, active.copy()))
    return stages


def _check_components(model: Deck, components: _Components) -> None:
    structural: dict[int, list[int]] = {}
    masses = []
    for eid, (kind, nodes) in model.elements.items():
        existing = [nid for nid in nodes if nid in model.nodes]
        if not existing:
            continue
        root = components.root(existing[0])
        if kind in MASS_TYPES:
            masses.append((eid, existing, root))
        else:
            structural.setdefault(root, []).append(eid)
    for eid, nodes, root in masses:
        if root not in structural:
            model.error("floating_mass", (f"element:{eid}", *(f"node:{n}" for n in nodes)),
                        "Mass has no connection to structural elements",
                        "Attach the mass node to the structure with shared nodes or a validated tie.")
    for step, constrained in _boundary_stages(model):
        supported = {components.root(nid) for nid in constrained if nid in model.nodes}
        for root, eids in structural.items():
            if root not in supported:
                model.error("unconstrained_part", (f"step:{step}", *(f"element:{e}" for e in sorted(eids))),
                            "Connected part has no boundary condition or support path",
                            "Apply structural restraints or connect the part to a constrained part.")


def _coincident_nodes(model: Deck, tolerance: float,
                      ties: list[tuple[set[int], set[int]]]) -> None:
    # Spatial hashing avoids an all-pairs scan for ordinary separated meshes.
    buckets: dict[tuple[int, ...], list[int]] = {}
    offsets = tuple(product((-1, 0, 1), repeat=3))
    tolerance_num, tolerance_den = tolerance.as_integer_ratio()

    def cell_index(value: float) -> int:
        # Integer ratios also handle finite coordinates whose scaled value
        # would overflow a float for an exceptionally small tolerance.
        num, den = value.as_integer_ratio()
        return (num * tolerance_den) // (den * tolerance_num)

    for nid, xyz in sorted(model.nodes.items()):
        cell = tuple(cell_index(v) for v in xyz)
        for offset in offsets:
            neighbour = tuple(a + b for a, b in zip(cell, offset))
            for other in buckets.get(neighbour, ()):
                if math.dist(xyz, model.nodes[other]) > tolerance:
                    continue
                if any((nid in left and other in right) or (other in left and nid in right)
                       for left, right in ties):
                    continue
                model.error("unmerged_nodes", (f"node:{other}", f"node:{nid}"),
                            "Distinct nodes coincide within the stated length tolerance",
                            "Merge the nodes or define and validate an intentional tie.")
        buckets.setdefault(cell, []).append(nid)


def check_deck(path: str | Path, *, tolerance: float = 1e-6) -> ModelCheckReport:
    """Check an INP deck and included files without executable discovery.

    Tolerance is an absolute Euclidean distance in the input length unit.
    The default is a screening choice, one millionth of an input length unit
    (six orders below the synthetic fixture's unit edge), not a solver limit.
    Select it for the actual deck units and coordinate precision; no physical
    merge tolerance or automatic unit inference is established by this default.
    Missing input files and unsupported topology produce blocking findings.
    Findings use stable node/element IDs and case-normalized symbolic names.
    """
    if not math.isfinite(tolerance) or tolerance <= 0:
        raise ValueError("Node tolerance must be finite and positive")
    try:
        blocks = read_blocks(Path(path))
    except (OSError, UnicodeError, ValueError):
        return ModelCheckReport((Finding("error", "parse_error", ("deck",),
            "Input or included file is unreadable, malformed or cyclic",
            "Check input encoding, include names and include cycles."),), tolerance)
    model = Deck(blocks)
    model.parse()
    surfaces = _references_and_sections(model)
    components = _Components(model.nodes)
    for _, nodes in model.elements.values():
        components.join(nodes)
    ties = _connections(model, surfaces, components)
    _check_components(model, components)
    _coincident_nodes(model, tolerance, ties)
    definitions = {kind: set(names) for kind, names in model.sets.items()}
    definitions.update(model.definitions)
    for kind, names in definitions.items():
        for name in sorted(names):
            if (kind, name) not in model.used:
                model.findings.append(Finding("warning", "unused_definition", (f"{kind}:{name}",),
                    "Definition is not referenced", "Use the definition or remove it if obsolete."))
    # Repeated references can expose the same missing definition more than once.
    return ModelCheckReport(tuple(dict.fromkeys(model.findings)), tolerance)


def validate_model(path: str | Path, *, tolerance: float = 1e-6) -> ModelCheckReport:
    """Follow the QA-gate convention: return PASS evidence or raise ValueError."""
    report = check_deck(path, tolerance=tolerance)
    if report.status == "FAIL":
        raise ModelCheckError(report)
    return report
