# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline audit of coherent element-nodal tensors and independent path coverage.

This boundary validates supplied evidence; it cannot authenticate its producer.
No solver is imported. Design-code allowables and curved-path linearization
are deliberately absent. See docs/domains/ansys/native-tensor-audit.md.
"""

from __future__ import annotations

from dataclasses import dataclass
from decimal import Decimal, InvalidOperation
import re

from digitalmodel.ansys.cylinder_results import (
    EvidenceError,
    decimal_context,
    parse_e24,
)
from digitalmodel.ansys.cylinder_results_native_listings import normalize_presol

D = Decimal
COMPONENTS = ("SX", "SY", "SZ", "SXY", "SYZ", "SXZ")
MEASURES = ("raw_abs_trace", "raw_von_mises", "linearized_abs_trace")
_NUMBER = re.compile(r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[EeDd][+-]?[0-9]+)?")


def number(value):
    """Reject implicit Fortran exponents, nonfinite values and float coercion."""
    if isinstance(value, D):
        result = value
    elif isinstance(value, str) and _NUMBER.fullmatch(value):
        try:
            result = D(value.replace("d", "E").replace("D", "E"))
        except InvalidOperation as exc:
            raise EvidenceError("Invalid numeric token") from exc
    else:
        raise EvidenceError("Explicit decimal or E/D exponent required")
    if not result.is_finite():
        raise EvidenceError("Finite numeric value required")
    return result


def _vector(values, size):
    result = tuple(number(v) for v in values)
    if len(result) != size:
        raise EvidenceError("Component count differs")
    return result


@dataclass(frozen=True)
class ResultSet:
    """Selected result identity, common frame and explicit consistent units."""

    load_step: int
    substep: int
    time: str
    frame: str
    stress_unit: str
    length_unit: str

    def __post_init__(self):
        if any(type(n) is not int or n <= 0 for n in (self.load_step, self.substep)):
            raise EvidenceError("Positive integer result indices required")
        object.__setattr__(self, "time", number(self.time))
        if not all(
            isinstance(s, str) and s.strip()
            for s in (self.frame, self.stress_unit, self.length_unit)
        ):
            raise EvidenceError("Result frame and units required")


@dataclass(frozen=True, order=True)
class SampleKey:
    """No merging across element, node, shell surface or material layer."""

    element: int
    node: int
    surface: str
    layer: int

    def __post_init__(self):
        if any(type(n) is not int or n <= 0 for n in (self.element, self.node)):
            raise EvidenceError("Positive integer element/node IDs required")
        if type(self.layer) is not int or not (
            (self.surface == "SOLID" and self.layer == 0)
            or (self.surface in ("TOP", "BOTTOM", "MID") and self.layer > 0)
        ):
            raise EvidenceError("Explicit solid or shell surface/layer required")


@dataclass(frozen=True)
class TensorSample:
    """One coherent six-component row; shell evidence is supplied by an adapter."""

    key: SampleKey
    result_set: ResultSet
    components: tuple
    recovery: str
    surface_basis: str
    surface_reference: str

    def __post_init__(self):
        object.__setattr__(self, "components", _vector(self.components, 6))


@dataclass(frozen=True)
class PathSpec:
    """Independently supplied sampling plan, including every expected radial point.

    Positions are measured along a straight Cartesian through-thickness path.
    All expected samples must be assigned to at least one supplied path.
    """

    name: str
    points: tuple  # Ordered (SampleKey, position) pairs; never sorted by the audit.
    inner: str
    outer: str
    method: str


@dataclass(frozen=True)
class CitedLimit:
    """A caller-supplied nonnegative limit with an edition/clause or test basis."""

    value: str
    reference: str

    def __post_init__(self):
        object.__setattr__(self, "value", number(self.value))
        if (
            self.value < 0
            or not isinstance(self.reference, str)
            or not self.reference.strip()
        ):
            raise EvidenceError("Nonnegative limit and source reference required")


@dataclass(frozen=True)
class Wrench:
    """Applied or reaction force/couple, in N, mm and N mm, global Cartesian."""

    name: str
    position: tuple
    force: tuple
    couple: tuple

    def __post_init__(self):
        if not isinstance(self.name, str) or not self.name.strip():
            raise EvidenceError("Force contribution identity required")
        for field in ("position", "force", "couple"):
            object.__setattr__(self, field, _vector(getattr(self, field), 3))


@dataclass(frozen=True)
class Equilibrium:
    """Complete supplied load/reaction resultants; origin is explicit.

    Loads and supports must be enumerated independently by the caller. This
    object does not prove that a native load or support listing is complete.
    """

    result_set: ResultSet
    applied: tuple
    reactions: tuple
    origin: tuple
    force_limit: CitedLimit
    moment_limit: CitedLimit


def _not_evaluated(reason):
    return {"status": "NOT_EVALUATED", "reason": reason}


def _check(value, limit):
    return {
        "status": "PASS" if value <= limit.value else "NON_PASS",
        "demand": value,
        "limit": limit.value,
        "reference": limit.reference,
    }


def _balance(equilibrium, result_set):
    if equilibrium is None:
        return _not_evaluated("Force and moment evidence not supplied")
    eq = equilibrium
    if (
        eq.result_set != result_set
        or result_set.frame != "GLOBAL_CARTESIAN"
        or result_set.length_unit != "mm"
        or not eq.applied
        or not eq.reactions
    ):
        return _not_evaluated(
            "Balance identity, units or load/support evidence differs"
        )
    origin = _vector(eq.origin, 3)
    rows = (*eq.applied, *eq.reactions)
    if len({r.name for r in rows}) != len(rows):
        return _not_evaluated("Duplicate force contribution identity")
    force = [D(0)] * 3
    moment = [D(0)] * 3
    # OpenStax, University Physics Volume 1 (2016), 12.1, equations 12.2/12.5:
    # sum F=0, sum (r cross F + C)=0. The cross-product definition and its
    # expansion are given in the worked example; no default tolerance is used.
    for row in rows:
        x, y, z = (r - o for r, o in zip(row.position, origin))
        fx, fy, fz = row.force
        cross = (y * fz - z * fy, z * fx - x * fz, x * fy - y * fx)
        for i in range(3):
            force[i] += row.force[i]
            moment[i] += cross[i] + row.couple[i]
    fc = _check(max(map(abs, force)), eq.force_limit)
    mc = _check(max(map(abs, moment)), eq.moment_limit)
    return {
        "status": "PASS" if fc["status"] == mc["status"] == "PASS" else "NON_PASS",
        "force": tuple(force),
        "moment": tuple(moment),
        "force_check": fc,
        "moment_check": mc,
        "origin": origin,
    }


def _invariants(tensor):
    sx, sy, sz, sxy, syz, sxz = tensor
    # ContinuumMechanics.org, "Von Mises Stress", "Von Mises Stress in
    # Component Form" (undated, accessed 2026-10-22). See the linked source
    # in the guide. The factors 1/2 and 3 define the invariant, not allowables.
    vm = (
        ((sx - sy) ** 2 + (sy - sz) ** 2 + (sz - sx) ** 2) / 2
        + 3 * (sxy**2 + syz**2 + sxz**2)
    ).sqrt()
    return sx + sy + sz, vm


def _linearize(points, rows):
    x = [number(position) for _, position in points]
    tensors = [rows[key].components for key, _ in points]
    thickness = x[-1] - x[0]
    center = (x[-1] + x[0]) / 2
    membrane, bending = [], []
    # Mechanical APDL Theory Reference, 2024 R2, 17.4.1, equations 17-16,
    # 17-18 and 17-19: M=int(s)/t, B_outer=6*int(s*(x-center))/t**2.
    # Integrate the piecewise-linear input exactly on each segment. The
    # divisors 2 and 6 follow from integrating 1, x and x**2; see guide.
    for component in range(6):
        area = first_moment = D(0)
        for i in range(len(x) - 1):
            h = x[i + 1] - x[i]
            a, b = tensors[i][component], tensors[i + 1][component]
            u, v = x[i] - center, x[i + 1] - center
            area += h * (a + b) / 2
            first_moment += h * ((2 * u + v) * a + (u + 2 * v) * b) / 6
        membrane.append(area / thickness)
        bending.append(6 * first_moment / thickness**2)
    inner = tuple(m - b for m, b in zip(membrane, bending))
    outer = tuple(m + b for m, b in zip(membrane, bending))
    return {
        "membrane": tuple(membrane),
        "bending_outer": tuple(bending),
        "inner": inner,
        "outer": outer,
        "status": "COMPLETE",
        "method": "Cartesian piecewise-linear component integration; no code acceptance",
    }


def _coverage(samples, expected, result_set, paths):
    errors, rows, counts = [], {}, {}
    if not expected or len(set(expected)) != len(expected):
        errors.append("Independent nonempty unique sample inventory required")
    if result_set.frame != "GLOBAL_CARTESIAN":
        errors.append("Only a common global Cartesian stress frame is supported")
    for row in samples:
        counts[row.key] = counts.get(row.key, 0) + 1
        if row.key in rows:
            errors.append("Duplicate element/node/surface/layer tensor")
        rows[row.key] = row
        if row.result_set != result_set:
            errors.append("Tensor result set, frame or units differ")
        if row.recovery != "ELEMENT_NODAL_UNAVERAGED":
            errors.append("Unaveraged coherent element-nodal tensor required")
        if row.key.surface != "SOLID" and not (
            row.surface_basis == "explicit"
            and isinstance(row.surface_reference, str)
            and row.surface_reference.strip()
        ):
            errors.append(
                "Explicit shell surface identity required; no verified-format shell adapter is available"
            )
    if set(rows) != set(expected):
        errors.append("Element/node/surface/layer coverage mismatch")
    visited, names = set(), set()
    if not paths:
        errors.append("Independent radial/path inventory required")
    for path in paths:
        if not path.name or path.name in names:
            errors.append("Missing or duplicate path identity")
        names.add(path.name)
        keys = [key for key, _ in path.points]
        positions = [number(position) for _, position in path.points]
        if (
            len(keys) < 2
            or len(set(keys)) != len(keys)
            or not set(keys) <= set(expected)
            or any(b <= a for a, b in zip(positions, positions[1:]))
            or positions[0] != number(path.inner)
            or positions[-1] != number(path.outer)
        ):
            errors.append("Incomplete, repeated or unordered radial/path sampling")
        visited.update(keys)
    if visited != set(expected):
        errors.append("Radial/path inventory does not visit every expected sample")
    coverage = {
        "expected": tuple(sorted(set(expected))),
        "visited": tuple(sorted(rows)),
        "missing": tuple(sorted(set(expected) - set(rows))),
        "unexpected": tuple(sorted(set(rows) - set(expected))),
        "duplicates": tuple(sorted(key for key, count in counts.items() if count > 1)),
    }
    return rows, errors, coverage


@decimal_context
def audit_tensors(
    samples,
    *,
    expected,
    result_set,
    paths,
    equilibrium=None,
    limits=None,
    source_result_sha256=None,
):
    """Audit all supplied tensors, then gate independent engineering criteria.

    Inventories must come from the model/sampling plan, never from parsed rows.
    Missing evidence produces NOT_EVALUATED, raw failures produce INCOMPLETE,
    and an evaluated engineering exceedance produces NON_PASS. No acceptance
    limit, equilibrium tolerance or design-code classification is inferred.
    """
    samples, expected, paths = tuple(samples), tuple(expected), tuple(paths)
    limits = {} if limits is None else dict(limits)
    if set(limits) - set(MEASURES):
        raise EvidenceError("Unknown engineering criterion")
    if source_result_sha256 is not None and not re.fullmatch(
        r"[0-9a-f]{64}", source_result_sha256
    ):
        raise EvidenceError("Invalid source result digest")
    rows, errors, coverage = _coverage(samples, expected, result_set, paths)
    criteria = {m: _not_evaluated("Raw evidence incomplete") for m in MEASURES}
    result = {
        "raw_status": "INCOMPLETE" if errors else "COMPLETE",
        "errors": tuple(errors),
        "coverage": coverage,
        "criteria": criteria,
        "equilibrium": _not_evaluated("Raw evidence incomplete"),
        "result_set": result_set,
        "source_result_sha256": source_result_sha256,
        "code_acceptance": _not_evaluated(
            "Edition-specific code assessment not supplied"
        ),
    }
    if errors:
        return result
    invariants = {key: _invariants(row.components) for key, row in rows.items()}
    traces = {key: pair[0] for key, pair in invariants.items()}
    raw_trace = max(abs(v) for v in traces.values())
    raw_vm = max(pair[1] for pair in invariants.values())
    linearized = {
        path.name: (
            _linearize(path.points, rows)
            if path.method == "CARTESIAN"
            else _not_evaluated("Unsupported geometry-specific linearization method")
        )
        for path in paths
    }
    linear_trace = (
        max(
            abs(_invariants(p[face])[0])
            for p in linearized.values()
            for face in ("inner", "outer")
        )
        if all(p["status"] == "COMPLETE" for p in linearized.values())
        else None
    )
    result.update(
        raw_abs_trace=raw_trace,
        raw_von_mises=raw_vm,
        raw_trace_keys=tuple(
            sorted(k for k, v in traces.items() if abs(v) == raw_trace)
        ),
        raw_vm_keys=tuple(sorted(k for k, v in invariants.items() if v[1] == raw_vm)),
        signed_traces=traces,
        paths=linearized,
        linearized_abs_trace=linear_trace,
    )
    result["sample_surfaces"] = tuple(sorted({key.surface for key in rows}))
    result["equilibrium"] = _balance(equilibrium, result_set)
    for measure in MEASURES:
        if result["equilibrium"]["status"] != "PASS":
            criteria[measure] = _not_evaluated("Force and moment gate has not passed")
        elif result[measure] is None:
            criteria[measure] = _not_evaluated(
                "Required linearization method not evaluated"
            )
        elif measure not in limits:
            criteria[measure] = _not_evaluated("Cited allowable not supplied")
        else:
            criteria[measure] = _check(result[measure], limits[measure])
    return result


def parse_presol_plane183(raw, *, stress_unit, length_unit):
    """Visit every row of the retained v261 PLANE183 PRESOL profile.

    The existing normalizer verifies every page's step/substep/time/global
    frame and fixed-width numeric fields. Its narrow profile accepts only
    step 1, substep 1, time 1 and four corner rows per PLANE183 element.
    Native listings do not state units, so units are required caller inputs.
    No shell or other element format is claimed by this adapter. In particular
    this parser does not establish Cartesian geometry for linearization.
    """
    normalized = normalize_presol(raw)
    identity = ResultSet(1, 1, "1", "GLOBAL_CARTESIAN", stress_unit, length_unit)
    result, seen_elements, seen_keys = [], set(), set()
    element = None
    for line in normalized.splitlines():
        if line.startswith(b"ELEMENT= "):
            element = int(line.split()[1])
            if element in seen_elements:
                raise EvidenceError("Duplicate element section")
            seen_elements.add(element)
        elif line == b"NODE SX SY SZ SXY SYZ SXZ":
            continue
        else:
            key = SampleKey(element, int(line[:8]), "SOLID", 0)
            if key in seen_keys:
                raise EvidenceError("Duplicate element-node tensor")
            seen_keys.add(key)
            components = tuple(
                parse_e24(line[8 + i * 24 : 32 + i * 24], "MPa")[0] for i in range(6)
            )
            result.append(
                TensorSample(
                    key, identity, components, "ELEMENT_NODAL_UNAVERAGED", "solid", ""
                )
            )
    return tuple(result)
