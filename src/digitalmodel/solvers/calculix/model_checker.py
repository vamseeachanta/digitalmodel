# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pre-run connectivity and reference QA for structural CalculiX INP decks.

Support means a topological path to at least one structural boundary condition
in every step. It does not establish sufficient restraint, stiffness rank or
contact engagement. Coordinates and tolerance use the deck's length unit.
"""

from __future__ import annotations

from collections import Counter
from collections.abc import Iterable
from dataclasses import asdict, dataclass
from itertools import product
import math
from pathlib import Path

from ._deck import (
    Block, Deck, Finding, MASS_TYPES, SECTION_KEYWORDS, canonical_token, number, read_blocks,
)


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


def _surface_nodes(model: Deck, block: Block, *,
                   element_types: set[str] | None = None) -> set[int]:
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
        if canonical_token(block.params.get("TYPE", "ELEMENT")) == "NODE":
            nodes.update(model.target("nset", row[0], block.line, block=block))
            continue
        if canonical_token(block.params.get("TYPE", "ELEMENT")) != "ELEMENT":
            raise ValueError("Unsupported surface type")
        for eid in model.target("elset", row[0], block.line, block=block):
            kind, conn = model.elements[eid]
            if element_types is not None:
                element_types.add(kind)
            face = canonical_token(row[1])
            if kind.startswith("C3D"):
                if not face.startswith("S"):
                    raise ValueError("Invalid face")
                index = int(face[1:]) - 1
                if index < 0 or index >= len(faces[len(conn)]):
                    raise ValueError("Invalid face")
                nodes.update(conn[i] for i in faces[len(conn)][index])
            elif kind in {"S3", "S4", "S4R", "S6", "S8", "S8R"}:
                # Shell faces 1/2 alias NEG/POS in section 7.128.
                face = {"S1": "SNEG", "S2": "SPOS"}.get(face, face)
                if face not in {"SPOS", "SNEG"}:
                    raise ValueError("Unsupported surface face")
                nodes.update(conn)
            else:
                # Unsupported surface geometry must not manufacture a support path.
                raise ValueError("Unsupported surface face")
    if element_types is not None and canonical_token(block.params.get("TYPE", "ELEMENT")) == "NODE":
        element_types.update(kind for kind, conn in model.elements.values()
                             if kind not in MASS_TYPES and not nodes.isdisjoint(conn))
    return nodes.intersection(model.nodes)


@dataclass(frozen=True)
class _Surface:
    surface_type: str
    nodes: set[int]
    element_types: set[str]


def _references_and_sections(model: Deck) -> dict[str, _Surface]:
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
                    model.reference(kind, p[param], block.line, block=block)
            if k in SECTION_KEYWORDS:
                ids = model.target("elset", p["ELSET"], block.line, block=block)
                if not ids:
                    model.findings.append(Finding("warning", "unused_section",
                        (f"section:{k}:{canonical_token(p['ELSET'])}",), "Section has no elements",
                        "Assign the section to a populated element set or remove it.", block.line))
                # A missing material must not count as a valid structural section.
                if k in {"SOLID SECTION", "SHELL SECTION", "BEAM SECTION", "MEMBRANE SECTION"}:
                    model.reference("material", p["MATERIAL"], block.line, block=block)
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
                element_types: set[str] = set()
                surfaces[canonical_token(p["NAME"])] = _Surface(
                    canonical_token(p.get("TYPE", "ELEMENT")),
                    _surface_nodes(model, block, element_types=element_types), element_types)
            elif k in {"CLOAD", "DLOAD", "DSLOAD"}:
                for row in block.rows:
                    if k == "DSLOAD":
                        model.reference("surface", row[0], block.line, block=block)
                    else:
                        model.target("nset" if k == "CLOAD" else "elset", row[0], block.line, block=block)
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


def _connections(model: Deck, surfaces: dict[str, _Surface],
                 components: _Components) -> list[tuple[set[int], set[int]]]:
    ties = []
    tie_names = Counter(canonical_token(b.params.get("NAME", "")) for b in model.blocks
                        if b.keyword == "TIE")
    contact_types = {canonical_token(b.params.get("TYPE", "")) for b in model.blocks
                     if b.keyword == "CONTACT PAIR"}
    for block in model.blocks:
        if block.keyword not in {"TIE", "CONTACT PAIR"}:
            continue
        # CalculiX 2.22 manual, sections 7.22 (*CONTACT PAIR) and 7.132 (*TIE):
        # required parameters must be valid before a connection supplies support.
        parameter = "NAME" if block.keyword == "TIE" else "INTERACTION"
        value = canonical_token(block.params.get(parameter, ""))
        if not value:
            model.error("missing_connection_parameter",
                        (f"keyword:{block.keyword}", f"parameter:{parameter}"),
                        f"{block.keyword} requires a nonempty {parameter}",
                        f"Specify {parameter} with a nonempty value; contact INTERACTION "
                        "must name a defined SURFACE INTERACTION; see CalculiX 2.22 "
                        "manual sections 7.22 (*CONTACT PAIR) and 7.132 (*TIE).", block.line)
            continue
        if block.keyword == "TIE":
            # CalculiX 2.22 manual, section 7.132 (*TIE): MULTISTAGE and
            # CYCLIC SYMMETRY use distinct constraints and are mutually
            # exclusive. Neither may supply ordinary-tie support/exemptions.
            modes = {"MULTISTAGE", "CYCLICSYMMETRY"}.intersection(block.params)
            if modes:
                model.error("unsupported_tie_mode",
                            ("keyword:TIE", *(f"parameter:{mode}" for mode in sorted(modes))),
                            "Special tie modes are unsupported by this gate",
                            "Use an ordinary tie or extend and test the special constraints; "
                            "MULTISTAGE and CYCLIC SYMMETRY are mutually exclusive; "
                            "see CalculiX 2.22 manual section 7.132 (*TIE).", block.line)
                continue
            # CalculiX 2.22 manual, section 7.132 (*TIE): exactly one pair
            # and a name unused by any other tie. Reject every duplicate
            # before the first declaration can join or exempt any nodes.
            if tie_names[value] > 1:
                model.error("duplicate_tie_name", (f"tie:{value}",),
                            "Tie name is used by more than one TIE card",
                            "Assign a unique NAME to each tie; see CalculiX 2.22 "
                            "manual section 7.132 (*TIE).", block.line)
                continue
            if len(block.rows) != 1:
                model.error("parse_error", ("keyword:TIE",),
                            f"TIE requires exactly one surface pair; found {len(block.rows)} rows",
                            "Provide one slave/master surface pair per uniquely named tie; "
                            "see CalculiX 2.22 manual section 7.132 (*TIE).", block.line)
                continue
        if block.keyword == "CONTACT PAIR" and value not in model.definitions["interaction"]:
            # Reference validation has already reported the undefined interaction.
            continue
        for row in block.rows:
            if len(row) != 2 or not all(row):
                model.error("parse_error", (f"keyword:{block.keyword}",), "Expected two surface names",
                            "Provide a defined slave surface and master surface.", block.line)
                continue
            names = [canonical_token(v) for v in row]
            for name in names:
                model.reference("surface", name, block.line, block=block)
            left, right = (surfaces.get(name) for name in names)
            if left is None or right is None or not left.nodes or not right.nodes:
                model.error("empty_surface", tuple(f"surface:{n}" for n in names),
                            "Connection has an empty or unresolved surface",
                            "Populate both surfaces with valid mesh entities.", block.line)
                continue
            # CalculiX 2.22 manual, sections 7.22, 7.128 and 7.132:
            # ordinary ties and contact pairs require an element-face master.
            if right.surface_type != "ELEMENT":
                model.error("incompatible_master_surface",
                            (f"keyword:{block.keyword}", f"surface:{names[1]}"),
                            "Connection requires an element-face master surface",
                            "Define the master with TYPE=ELEMENT and valid element faces.",
                            block.line)
                continue
            # CalculiX 2.22 manual, section 7.132 (*TIE): ordinary ties
            # exclude shells, beams and two-dimensional element families.
            if block.keyword == "TIE" and any(
                    not kind.startswith("C3D") for kind in left.element_types | right.element_types):
                model.error("incompatible_tie_surface",
                            ("keyword:TIE", *(f"surface:{n}" for n in names)),
                            "Ordinary tie requires three-dimensional solid surface members",
                            "Use solid surfaces or a separately supported constraint; "
                            "see CalculiX 2.22 manual section 7.132 (*TIE).", block.line)
                continue
            if block.keyword == "CONTACT PAIR":
                # CalculiX 2.22 manual, section 7.22 (*CONTACT PAIR):
                # TYPE is required and contact formulations cannot be mixed.
                mode = canonical_token(block.params.get("TYPE", ""))
                if mode not in {"NODETOSURFACE", "SURFACETOSURFACE", "MORTAR", "MASSLESS"}:
                    model.error("unsupported_contact_type", ("keyword:CONTACT PAIR",),
                                "Contact TYPE is missing or unsupported",
                                "Specify NODE TO SURFACE, SURFACE TO SURFACE, MORTAR or MASSLESS; "
                                "see CalculiX 2.22 manual section 7.22 (*CONTACT PAIR).", block.line)
                    continue
                if len(contact_types) != 1:
                    model.error("incompatible_contact_type", ("keyword:CONTACT PAIR",),
                                "Contact pairs declare different TYPE values",
                                "Use one contact formulation throughout the deck; "
                                "see CalculiX 2.22 manual section 7.22 (*CONTACT PAIR).", block.line)
                    continue
                quadratic = {"C3D10", "C3D15", "C3D20", "C3D20R", "S6", "S8", "S8R"}
                if left.surface_type != "ELEMENT" and (
                        mode in {"SURFACETOSURFACE", "MORTAR"}
                        or quadratic.intersection(left.element_types | right.element_types)):
                    model.error("incompatible_slave_surface",
                                ("keyword:CONTACT PAIR", f"surface:{names[0]}"),
                                f"{mode} contact with these element families requires an element-face slave",
                                "Define the slave with TYPE=ELEMENT for face-to-face or quadratic contact; "
                                "see CalculiX 2.22 manual section 7.22 (*CONTACT PAIR).", block.line)
                    continue
            components.join(left.nodes | right.nodes)
            if block.keyword == "TIE":
                ties.append((left.nodes, right.nodes))
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
                # Section 7 ignores blanks in semantic values; normalize OP
                # here so the parser still preserves include filenames.
                operation = canonical_token(block.params.get("OP", "MOD"))
                if first_boundary and operation == "NEW":
                    active.clear()
                first_boundary = False
                for row in block.rows:
                    nodes = model.target("nset", row[0], block.line, block=block)
                    # CalculiX 2.22 User's Manual, sections 7.4 and 10.1.2:
                    # structural DOFs are numeric; delete blanks before parsing.
                    first = int(canonical_token(row[1]))
                    last = int(canonical_token(row[2])) if len(row) > 2 and row[2] else first
                    if not 1 <= first <= last <= 6:
                        raise ValueError("Invalid structural degree of freedom")
                    if len(row) > 3 and row[3]:
                        number(row[3])
                    # Rotational BCs do not constrain translational-only solid nodes.
                    active.update(nodes if first <= 3 else nodes & rotational)
        except (ValueError, IndexError):
            model.error("parse_error", (f"keyword:{block.keyword}",), "Invalid step or boundary data",
                        "Correct step pairing and use numeric structural boundary degrees of freedom.",
                        block.line)
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
