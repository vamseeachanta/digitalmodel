"""Parser and conservation guards for MAPDL crack-front (CINT) output (#2157 P0).

The crack verification deck (``crack_verification.py``) writes two small text
files per mesh density, both from **solved** results:

* ``crack_cint_L<n>.txt``: K_I, K_II, K_III (interaction integral) and J for
  every crack-front node and contour, raw in MPa*sqrt(mm) and N/mm;
* ``crack_reac_L<n>.txt``: the solved reaction sums on the constrained faces.

This module parses them into a host-free record and evaluates guards (a)-(f)
independently from that solved output (never from prescribed loads):

(a) equilibrium: the solved ligament reaction balances sigma x loaded area
    within 0.5 %;
(b) mesh independence of load: the two densities' solved reaction sums agree
    within 0.1 %;
(c) contour independence: the spread of K_I and of J across the last three
    contours is <= 3 % at every front node;
(d) completeness: every declared front node has K_I, K_II, K_III and J for
    every declared contour;
(e) sanitisation: no host or user tokens in the raw output or the record;
(f) units: mm-N-MPa declared, raw K tagged MPa*sqrt(mm), and K converted to
    MPa*sqrt(m) exactly once.

The limits are fixed by the approved plan and are not parameters of the
guards. The parser needs no licence.
"""

from __future__ import annotations

import csv
import getpass
import io
import json
import math
import os
import re
import socket
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field

RECEIPT_SCHEMA_ID = "digitalmodel.crack_fe.receipt/v1"

# Guard limits, frozen by the approved plan (#2157, P0 step 3).
EQUILIBRIUM_TOL = 0.005
MESH_LOAD_TOL = 0.001
CONTOUR_SPREAD_TOL = 0.03
LAST_CONTOURS = 3

RAW_K_UNIT = "MPa*sqrt(mm)"
K_UNIT = "MPa*sqrt(m)"
J_UNIT = "N/mm"
K_RAW_TO_SI = 1.0 / math.sqrt(1000.0)  # MPa*sqrt(mm) -> MPa*sqrt(m)
EXPECTED_BASE_UNITS = {"length": "mm", "force": "N", "stress": "MPa"}

GUARD_NAMES = (
    "a_equilibrium",
    "b_mesh_load",
    "c_contour",
    "d_complete",
    "e_sanitised",
    "f_units",
)

# Generic leakage patterns, checked everywhere (the producing host is unknown
# to CI): key=value host/user fields, absolute drive/home paths, UNC shares.
_HOST_PATTERNS = (
    re.compile(r"(?i)\b(host|hostname|computername|user|username|userprofile)\s*[=:]"),
    re.compile(r"(?i)(?<![A-Za-z])[A-Z]:[\\/][^\s,\"]"),
    re.compile(r"(?i)/(home|Users)/[A-Za-z]"),
    re.compile(r"\\\\[A-Za-z0-9]"),
)


# --------------------------------------------------------------------------- #
# Records
# --------------------------------------------------------------------------- #
@dataclass
class CintRow:
    node: int
    contour: int
    x: float | None
    y: float | None
    z: float | None
    k1: float | None
    k2: float | None
    k3: float | None
    j: float | None


@dataclass
class CintTable:
    units: dict[str, str] = field(default_factory=dict)
    declared_front_nodes: int | None = None
    declared_contours: int | None = None
    mesh_level: int | None = None
    rows: list[CintRow] = field(default_factory=list)
    malformed_lines: int = 0


@dataclass
class ReactionRecord:
    units: dict[str, str] = field(default_factory=dict)
    mesh_level: int | None = None
    mapdl_rev: str | None = None
    stress_mpa: float | None = None
    loaded_area_mm2: float | None = None
    fz_ligament: float | None = None
    fx_symmetry: float | None = None
    fy_point: float | None = None
    n_ligament: int | None = None


@dataclass
class GuardResult:
    status: str  # "pass" | "fail"
    value: float | None
    limit: float | None
    detail: str = ""

    def to_dict(self) -> dict:
        return {
            "status": self.status,
            "value": self.value,
            "limit": self.limit,
            "detail": self.detail,
        }


# --------------------------------------------------------------------------- #
# Parsing
# --------------------------------------------------------------------------- #
def _num(tok: str) -> float | None:
    try:
        v = float(tok)
    except ValueError:
        return None
    return v if math.isfinite(v) else None


def _parse_units(line: str) -> dict[str, str]:
    body = line.split(":", 1)[1]
    units = {}
    for tok in body.split():
        if "=" in tok:
            k, v = tok.split("=", 1)
            units[k.strip()] = v.strip()
    return units


def parse_cint_table(text: str) -> CintTable:
    """Parse a ``crack_cint_L<n>.txt`` table written by the verification deck."""
    table = CintTable()
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        if line.startswith("#"):
            body = line[1:].strip()
            if body.lower().startswith("units:"):
                table.units = _parse_units(body)
                continue
            parts = body.split()
            if len(parts) == 2 and _num(parts[1]) is not None:
                val = round(float(parts[1]))
                if parts[0] == "front_nodes":
                    table.declared_front_nodes = val
                elif parts[0] == "contours":
                    table.declared_contours = val
                elif parts[0] == "mesh_level":
                    table.mesh_level = val
            continue
        toks = line.split()
        vals = [_num(t) for t in toks]
        if len(vals) < 2 or vals[0] is None or vals[1] is None:
            table.malformed_lines += 1
            continue
        rest = vals[2:] + [None] * max(0, 7 - len(vals[2:]))
        if len(toks) != 9:
            table.malformed_lines += 1
        table.rows.append(
            CintRow(
                round(vals[0]),
                round(vals[1]),
                *rest[:7],
            )
        )
    return table


def parse_reaction_file(text: str) -> ReactionRecord:
    """Parse a ``crack_reac_L<n>.txt`` reaction summary."""
    rec = ReactionRecord()
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        if line.startswith("#"):
            body = line[1:].strip()
            if body.lower().startswith("units:"):
                rec.units = _parse_units(body)
            continue
        parts = line.split()
        if len(parts) != 2:
            continue
        key, val = parts
        num = _num(val)
        if num is None:
            continue
        if key == "mapdl_rev":
            rec.mapdl_rev = f"{num:.2f}".rstrip("0").rstrip(".")
        elif key in ("mesh_level", "n_ligament"):
            setattr(rec, key, round(num))
        elif key in (
            "stress_mpa",
            "loaded_area_mm2",
            "fz_ligament",
            "fx_symmetry",
            "fy_point",
        ):
            setattr(rec, key, num)
    return rec


def _phi_deg(x: float, y: float, a: float, c: float) -> float:
    """Parametric angle of a front point (Newman-Raju phi); 90 = deepest."""
    phi = math.degrees(math.atan2(y / a, x / c))
    return round(phi, 9) + 0.0


def front_records(
    table: CintTable, *, crack_depth_mm: float, crack_half_length_mm: float
) -> list[dict]:
    """Per-node records with K converted **once** to MPa*sqrt(m), ordered by phi.

    ``K1_reported`` is the mean K_I over the last three contours.
    """
    by_node: dict[int, list[CintRow]] = {}
    for row in table.rows:
        by_node.setdefault(row.node, []).append(row)
    out = []
    for node, rows in by_node.items():
        rows = sorted(rows, key=lambda r: r.contour)
        ref = next((r for r in rows if r.x is not None and r.y is not None), None)
        if ref is None:
            continue
        contours = []
        for r in rows:
            contours.append(
                {
                    "contour": r.contour,
                    "K1": None if r.k1 is None else r.k1 * K_RAW_TO_SI,
                    "K2": None if r.k2 is None else r.k2 * K_RAW_TO_SI,
                    "K3": None if r.k3 is None else r.k3 * K_RAW_TO_SI,
                    "J": r.j,
                }
            )
        last = [c["K1"] for c in contours[-LAST_CONTOURS:] if c["K1"] is not None]
        out.append(
            {
                "node": node,
                "phi_deg": _phi_deg(
                    ref.x, ref.y, crack_depth_mm, crack_half_length_mm
                ),
                "x": ref.x,
                "y": ref.y,
                "z": ref.z,
                "contours": contours,
                "K1_reported": sum(last) / len(last) if last else None,
            }
        )
    out.sort(key=lambda n: (n["phi_deg"], n["node"]))
    return out


def centre_crack_secant_k(a_mm: float, width_mm: float, sigma_mpa: float) -> float:
    """Centre-cracked plate, Feddersen secant form (MPa*sqrt(m)).

    K = sigma * sqrt(pi*a) * sqrt(sec(pi*a/W)), a = half crack length,
    W = full plate width. Textbook closed form, used as a parser sanity value.
    """
    a_m = a_mm / 1000.0
    return (
        sigma_mpa
        * math.sqrt(math.pi * a_m)
        * math.sqrt(1.0 / math.cos(math.pi * a_mm / width_mm))
    )


# --------------------------------------------------------------------------- #
# Host / user token scan
# --------------------------------------------------------------------------- #
def runtime_host_tokens() -> tuple[str, ...]:
    """Host and user names of the current process (never written anywhere)."""
    tokens: set[str] = set()
    for getter in (socket.gethostname, getpass.getuser):
        try:
            value = getter()
        except (OSError, KeyError, ImportError):  # best effort; absence is fine
            value = ""
        tokens.add(value)
    for var in ("COMPUTERNAME", "USERNAME", "USER", "HOSTNAME", "USERDOMAIN"):
        val = os.environ.get(var)
        if val:
            tokens.add(val)
    home = os.path.expanduser("~")
    if home and home != "~":
        tokens.add(home)
    return tuple(sorted(t for t in tokens if t and len(t) >= 3))


def find_host_tokens(
    text: str, extra_tokens: Iterable[str] | None = None
) -> list[str]:
    """Return the host/user leak findings in ``text`` (empty list = clean).

    ``extra_tokens`` defaults to the current host and user names; pass ``()``
    to check only the generic patterns.
    """
    tokens = runtime_host_tokens() if extra_tokens is None else tuple(extra_tokens)
    findings = []
    for pat in _HOST_PATTERNS:
        m = pat.search(text)
        if m:
            findings.append(f"pattern {pat.pattern!r} at offset {m.start()}")
    low = text.lower()
    for tok in tokens:
        if tok and tok.lower() in low:
            findings.append("runtime host/user token present")
    return findings


# --------------------------------------------------------------------------- #
# Guards
# --------------------------------------------------------------------------- #
def _pass(ok: bool) -> str:
    return "pass" if ok else "fail"


def guard_equilibrium(reactions: Mapping[int, ReactionRecord]) -> GuardResult:
    worst = None
    for lvl, r in sorted(reactions.items()):
        if None in (r.fz_ligament, r.stress_mpa, r.loaded_area_mm2):
            return GuardResult("fail", None, EQUILIBRIUM_TOL, f"L{lvl}: missing field")
        applied = r.stress_mpa * r.loaded_area_mm2
        # reaction balances the applied tension: sum(RF_z) = -sigma*A
        err = abs(r.fz_ligament + applied) / abs(applied)
        worst = err if worst is None else max(worst, err)
    ok = worst is not None and worst <= EQUILIBRIUM_TOL
    return GuardResult(_pass(ok), worst, EQUILIBRIUM_TOL, "|sum RF_z + sigma*A| / sigma*A")


def guard_mesh_load(reactions: Mapping[int, ReactionRecord]) -> GuardResult:
    if len(reactions) != 2:
        return GuardResult("fail", None, MESH_LOAD_TOL, "need exactly two mesh densities")
    (l0, r0), (l1, r1) = sorted(reactions.items())
    if r0.fz_ligament is None or r1.fz_ligament is None or r1.fz_ligament == 0:
        return GuardResult("fail", None, MESH_LOAD_TOL, "missing reaction sum")
    diff = abs(r0.fz_ligament - r1.fz_ligament) / abs(r1.fz_ligament)
    return GuardResult(
        _pass(diff <= MESH_LOAD_TOL),
        diff,
        MESH_LOAD_TOL,
        f"|RF_z(L{l0}) - RF_z(L{l1})| / |RF_z(L{l1})|",
    )


def _spread(vals: list[float]) -> float:
    mean = sum(vals) / len(vals)
    if mean == 0:
        return math.inf
    return (max(vals) - min(vals)) / abs(mean)


def contour_spread(front: list[dict], key: str) -> float | None:
    """Worst spread of ``key`` over the last three contours, across all nodes."""
    worst = None
    for node in front:
        vals = [
            c[key] for c in node["contours"][-LAST_CONTOURS:] if c[key] is not None
        ]
        if len(vals) < 2:
            continue
        s = _spread(vals)
        worst = s if worst is None else max(worst, s)
    return worst


def guard_contour(fronts: Mapping[int, list[dict]]) -> GuardResult:
    worst = None
    where = ""
    for lvl, front in sorted(fronts.items()):
        for key in ("K1", "J"):
            s = contour_spread(front, key)
            if s is not None and (worst is None or s > worst):
                worst, where = s, f"L{lvl} {key}"
    ok = worst is not None and worst <= CONTOUR_SPREAD_TOL
    return GuardResult(
        _pass(ok), worst, CONTOUR_SPREAD_TOL, f"max (max-min)/mean, last 3 contours ({where})"
    )


def guard_complete(tables: Mapping[int, CintTable]) -> GuardResult:
    missing = 0
    for lvl, t in sorted(tables.items()):
        if t.declared_front_nodes is None or t.declared_contours is None:
            return GuardResult("fail", None, 0.0, f"L{lvl}: front/contour count not declared")
        seen = {}
        for r in t.rows:
            ok = None not in (r.k1, r.k2, r.k3, r.j)
            seen[(r.node, r.contour)] = ok and seen.get((r.node, r.contour), True)
        nodes = {n for n, _ in seen}
        if len(nodes) != t.declared_front_nodes:
            missing += abs(t.declared_front_nodes - len(nodes)) * t.declared_contours
        for n in nodes:
            for ic in range(1, t.declared_contours + 1):
                if not seen.get((n, ic), False):
                    missing += 1
        missing += t.malformed_lines
    return GuardResult(
        _pass(missing == 0), float(missing), 0.0, "missing (node, contour) values"
    )


def guard_sanitised(
    texts: Iterable[str], extra_tokens: Iterable[str] | None = None
) -> GuardResult:
    findings = []
    for text in texts:
        findings.extend(find_host_tokens(text, extra_tokens))
    return GuardResult(
        _pass(not findings), float(len(findings)), 0.0, "; ".join(findings[:3])
    )


def guard_units(
    tables: Mapping[int, CintTable],
    reactions: Mapping[int, ReactionRecord],
    fronts: Mapping[int, list[dict]],
) -> GuardResult:
    problems = []
    for lvl, t in sorted(tables.items()):
        for k, v in EXPECTED_BASE_UNITS.items():
            if t.units.get(k) != v:
                problems.append(f"L{lvl} CINT {k}={t.units.get(k)}")
        if t.units.get("K") != RAW_K_UNIT:
            problems.append(f"L{lvl} CINT K={t.units.get('K')}")
        if t.units.get("J") != J_UNIT:
            problems.append(f"L{lvl} CINT J={t.units.get('J')}")
    for lvl, r in sorted(reactions.items()):
        for k, v in EXPECTED_BASE_UNITS.items():
            if r.units.get(k) != v:
                problems.append(f"L{lvl} reactions {k}={r.units.get(k)}")
    # conversion applied exactly once: record K * sqrt(1000) == raw K
    for lvl, front in sorted(fronts.items()):
        raw = {(r.node, r.contour): r.k1 for r in tables[lvl].rows}
        for node in front:
            for c in node["contours"]:
                rk = raw.get((node["node"], c["contour"]))
                if rk is None or c["K1"] is None:
                    continue
                if not math.isclose(c["K1"] / K_RAW_TO_SI, rk, rel_tol=1e-12):
                    problems.append(f"L{lvl} node {node['node']}: K not converted once")
                    break
    return GuardResult(
        _pass(not problems), float(len(problems)), 0.0, "; ".join(problems[:3])
    )


def evaluate_guards(
    cint_texts: Mapping[int, str],
    reac_texts: Mapping[int, str],
    *,
    crack_depth_mm: float = 2.0,
    crack_half_length_mm: float = 4.0,
    extra_tokens: Iterable[str] | None = None,
) -> dict[str, GuardResult]:
    """Evaluate guards (a)-(f) from the solved output of two mesh densities."""
    tables = {lvl: parse_cint_table(t) for lvl, t in cint_texts.items()}
    reactions = {lvl: parse_reaction_file(t) for lvl, t in reac_texts.items()}
    fronts = {
        lvl: front_records(
            t, crack_depth_mm=crack_depth_mm, crack_half_length_mm=crack_half_length_mm
        )
        for lvl, t in tables.items()
    }
    tokens = None if extra_tokens is None else tuple(extra_tokens)
    return {
        "a_equilibrium": guard_equilibrium(reactions),
        "b_mesh_load": guard_mesh_load(reactions),
        "c_contour": guard_contour(fronts),
        "d_complete": guard_complete(tables),
        "e_sanitised": guard_sanitised(
            list(cint_texts.values()) + list(reac_texts.values()), tokens
        ),
        "f_units": guard_units(tables, reactions, fronts),
    }


# --------------------------------------------------------------------------- #
# Host-free records, CSV and receipt checks
# --------------------------------------------------------------------------- #
def build_mesh_record(
    *,
    level: int,
    cint_text: str,
    reac_text: str,
    crack_depth_mm: float,
    crack_half_length_mm: float,
) -> dict:
    """Host-free record of one solved mesh density (K in MPa*sqrt(m))."""
    table = parse_cint_table(cint_text)
    reac = parse_reaction_file(reac_text)
    return {
        "level": level,
        "declared_front_nodes": table.declared_front_nodes,
        "declared_contours": table.declared_contours,
        "reactions": {
            "stress_mpa": reac.stress_mpa,
            "loaded_area_mm2": reac.loaded_area_mm2,
            "fz_ligament_n": reac.fz_ligament,
            "fx_symmetry_n": reac.fx_symmetry,
            "fy_point_n": reac.fy_point,
            "n_ligament_nodes": reac.n_ligament,
        },
        "front": front_records(
            table,
            crack_depth_mm=crack_depth_mm,
            crack_half_length_mm=crack_half_length_mm,
        ),
    }


def front_csv(record: dict) -> str:
    """Flat CSV of one mesh record: one row per front node and contour."""
    buf = io.StringIO()
    w = csv.writer(buf, lineterminator="\n")
    w.writerow(["node", "phi_deg", "contour", "K1_MPa_sqrt_m", "K2_MPa_sqrt_m",
                "K3_MPa_sqrt_m", "J_N_per_mm"])
    for node in record["front"]:
        for c in node["contours"]:
            w.writerow([node["node"], node["phi_deg"], c["contour"], c["K1"], c["K2"],
                        c["K3"], c["J"]])
    return buf.getvalue()


_REQUIRED_TOP = {
    "schema": str,
    "state": str,
    "issue": int,
    "spec": dict,
    "units": dict,
    "meshing": dict,
    "run": dict,
    "meshes": list,
    "primary_level": int,
    "comparator": dict,
    "guards": dict,
}
_REQUIRED_RUN = {
    "producing_commit": str,
    "mapdl_version": str,
    "argv": list,
    "cores": int,
    "platform": str,
}
_REQUIRED_MESH = {
    "level": int,
    "deck_sha256": str,
    "declared_front_nodes": int,
    "declared_contours": int,
    "reactions": dict,
    "front": list,
}


def _check_fields(obj: dict, spec: dict, where: str, problems: list[str]) -> None:
    for key, typ in spec.items():
        if key not in obj:
            problems.append(f"{where}: missing '{key}'")
        elif not isinstance(obj[key], typ) or (typ is int and isinstance(obj[key], bool)):
            problems.append(f"{where}: '{key}' is not {typ.__name__}")


def validate_receipt_schema(receipt: dict) -> list[str]:
    """Structural validation of a verification receipt (empty list = valid)."""
    problems: list[str] = []
    if not isinstance(receipt, dict):
        return ["receipt is not an object"]
    _check_fields(receipt, _REQUIRED_TOP, "receipt", problems)
    if problems:
        return problems
    if receipt["schema"] != RECEIPT_SCHEMA_ID:
        problems.append(f"receipt: schema is not {RECEIPT_SCHEMA_ID}")
    _check_fields(receipt["run"], _REQUIRED_RUN, "run", problems)
    if not re.fullmatch(r"[0-9a-f]{40}", str(receipt["run"].get("producing_commit", ""))):
        problems.append("run: producing_commit is not a 40-hex SHA")
    units = receipt["units"]
    if units.get("k") != K_UNIT or units.get("length") != "mm":
        problems.append("units: expected length=mm and k=MPa*sqrt(m)")
    levels = []
    for i, mesh in enumerate(receipt["meshes"]):
        where = f"meshes[{i}]"
        if not isinstance(mesh, dict):
            problems.append(f"{where}: not an object")
            continue
        _check_fields(mesh, _REQUIRED_MESH, where, problems)
        if not re.fullmatch(r"[0-9a-f]{64}", str(mesh.get("deck_sha256", ""))):
            problems.append(f"{where}: deck_sha256 is not a 64-hex digest")
        levels.append(mesh.get("level"))
        for j, node in enumerate(mesh.get("front", [])):
            for key in ("node", "phi_deg", "contours", "K1_reported"):
                if key not in node:
                    problems.append(f"{where}.front[{j}]: missing '{key}'")
    if len(levels) != 2:
        problems.append("meshes: exactly two mesh densities required")
    if receipt["primary_level"] not in levels:
        problems.append("primary_level is not one of the mesh levels")
    missing_guards = set(GUARD_NAMES) - set(receipt["guards"])
    if missing_guards:
        problems.append(f"guards: missing {sorted(missing_guards)}")
    for name, g in receipt["guards"].items():
        if not isinstance(g, dict) or g.get("status") not in ("pass", "fail"):
            problems.append(f"guards.{name}: status must be 'pass' or 'fail'")
    for key in ("deepest", "surface"):
        if key not in receipt["comparator"]:
            problems.append(f"comparator: missing '{key}'")
    return problems


def evaluate_receipt_guards(
    receipt: dict, extra_tokens: Iterable[str] | None = None
) -> dict[str, GuardResult]:
    """Re-evaluate the guards from the solved data recorded in a receipt.

    (a), (b) and (c) are recomputed from the recorded reactions and per-contour
    values; (d) from the declared counts; (e) on the receipt text; (f) from the
    recorded unit tags.
    """
    reactions = {}
    fronts = {}
    missing = 0
    for mesh in receipt["meshes"]:
        r = mesh["reactions"]
        reactions[mesh["level"]] = ReactionRecord(
            units=dict(EXPECTED_BASE_UNITS),
            stress_mpa=r.get("stress_mpa"),
            loaded_area_mm2=r.get("loaded_area_mm2"),
            fz_ligament=r.get("fz_ligament_n"),
        )
        fronts[mesh["level"]] = mesh["front"]
        if len(mesh["front"]) != mesh["declared_front_nodes"]:
            missing += 1
        for node in mesh["front"]:
            if len(node["contours"]) != mesh["declared_contours"]:
                missing += 1
            for c in node["contours"]:
                if any(c.get(k) is None for k in ("K1", "K2", "K3", "J")):
                    missing += 1
    units = receipt["units"]
    unit_ok = (
        all(units.get(k) == v for k, v in EXPECTED_BASE_UNITS.items())
        and units.get("k") == K_UNIT
        and units.get("k_raw") == RAW_K_UNIT
        and units.get("j") == J_UNIT
        and math.isclose(float(units.get("k_conversion_factor", 0.0)), K_RAW_TO_SI,
                         rel_tol=1e-12)
    )
    return {
        "a_equilibrium": guard_equilibrium(reactions),
        "b_mesh_load": guard_mesh_load(reactions),
        "c_contour": guard_contour(fronts),
        "d_complete": GuardResult(_pass(missing == 0), float(missing), 0.0, "recorded"),
        "e_sanitised": guard_sanitised(
            [json.dumps(receipt, sort_keys=True)], extra_tokens
        ),
        "f_units": GuardResult(_pass(unit_ok), None, None, "recorded unit tags"),
    }
