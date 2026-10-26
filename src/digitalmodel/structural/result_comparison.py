# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compare already-extracted static structural results without solver imports.

Datasets must use the same identifiers, quantities, components, locations,
coordinate frames, units and reference metadata. Reference metadata declares the
common model, load case, state, origin/identity map and sign/axis conventions;
the caller is responsible for establishing their physical equivalence.

No unit conversion, coordinate transformation, interpolation or missing-value
substitution is performed. The explicit unit vocabulary below is dimensional
validation only: no conversion factors or engineering tolerances are embedded.
Additional units require an explicit extension rather than guessed dimensions.
Result values and tolerances must convert to finite binary floats without losing
precision; inputs that would be rounded during conversion are refused.

For each identity, signed error = candidate - reference; absolute error = its
magnitude. Relative error = absolute error / abs(reference), or None at a zero
reference, including when both values are zero. PASS means absolute error <=
absolute tolerance + relative tolerance * abs(reference). Tolerances are supplied
per identity in the result's units; relative tolerances and errors are fractions.
These are arithmetic definitions, not design criteria or a compliance claim.

Input may be a mapping or an iterable of (ResultKey, ResultValue) pairs. Use pairs
when duplicate detection is needed before constructing a mapping: a duplicate
already overwritten by a dict cannot be recovered. Repeated entity identifiers
with different components or member locations are distinct result identities.
"""

from collections.abc import Iterable, Mapping
from dataclasses import dataclass, fields
from math import isfinite
from numbers import Integral, Real


@dataclass(frozen=True)
class ReferenceMetadata:
    """Required common physical references, supplied explicitly by the caller.

    Attributes:
        model_id: Identity of the common physical model and revision.
        load_case: Identity of the common applied loads and boundary conditions.
        state: Common evaluation state, including time or step where relevant.
        reference: Common coordinate origin and node/member identity mapping.
        convention: Common axis, sign and member orientation conventions.
    """

    model_id: str
    load_case: str
    state: str
    reference: str
    convention: str


@dataclass(frozen=True)
class ResultKey:
    """Exact identity of one scalar action or displacement.

    Attributes:
        identifier: Common node or member identifier; strings only.
        quantity: Force, moment, translation or rotation, in lowercase.
        component: Explicit component label, such as fy, uy, rz or axial.
        location: Node, member end or station label in the common convention.
    """

    identifier: str
    quantity: str
    component: str
    location: str


@dataclass(frozen=True)
class ResultValue:
    """Scalar with an explicit unit and coordinate-frame identity."""

    value: float
    unit: str
    coordinate_frame: str


@dataclass(frozen=True)
class Tolerance:
    """Nonnegative absolute and relative limits, with absolute-limit units."""

    absolute: float
    relative: float
    unit: str


@dataclass(frozen=True)
class Difference:
    """One matched result and its arithmetic error and tolerance outcome."""

    key: ResultKey
    unit: str
    coordinate_frame: str
    reference: float
    candidate: float
    signed_difference: float
    absolute_difference: float
    relative_difference: float | None
    allowed_difference: float
    status: str


@dataclass(frozen=True)
class ComparisonReport:
    """Nonempty comparison with common metadata and named source datasets."""

    reference_name: str
    candidate_name: str
    metadata: ReferenceMetadata
    differences: tuple[Difference, ...]

    @property
    def status(self) -> str:
        """Return PASS only for a nonempty collection of passing differences."""
        return (
            "PASS"
            if self.differences and all(row.status == "PASS" for row in self.differences)
            else "FAIL"
        )


ResultInput = Mapping[ResultKey, ResultValue] | Iterable[tuple[ResultKey, ResultValue]]

# Unit symbols carry dimensions only; spelling and scale must match exactly.
_UNITS = {
    "force": frozenset({"N", "kN", "MN", "lbf", "kip"}),
    "moment": frozenset({"N*m", "kN*m", "MN*m", "lbf*ft", "kip*ft"}),
    "translation": frozenset({"m", "mm", "cm", "in", "ft"}),
    "rotation": frozenset({"rad", "deg"}),
}
_QUANTITIES = {
    "reactions": frozenset({"force", "moment"}),
    "displacements": frozenset({"translation", "rotation"}),
    "member_forces": frozenset({"force", "moment"}),
}


def _text(value: object, label: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{label} must be a nonempty string")


def _finite(value: object, label: str) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{label} must be a finite real number")
    if isinstance(value, Integral):
        value = int(value)
    try:
        number = float(value)
    except (OverflowError, ValueError) as error:
        raise ValueError(f"{label} must be a finite real number") from error
    if not isfinite(number):
        raise ValueError(f"{label} must be a finite real number")
    if number != value:
        raise ValueError(f"{label} cannot convert to float without losing precision")
    return number


def _snapshot(data: ResultInput, family: str) -> dict[ResultKey, ResultValue]:
    snapshot: dict[ResultKey, ResultValue] = {}
    items = data.items() if isinstance(data, Mapping) else data
    for key, value in items:
        if not isinstance(key, ResultKey):
            raise ValueError("result identity must be a ResultKey")
        for field in fields(key):
            _text(getattr(key, field.name), f"identity {field.name}")
        if key in snapshot:
            raise ValueError("duplicate result identity")
        if key.quantity not in _QUANTITIES[family]:
            raise ValueError("quantity is incompatible with result family")
        if not isinstance(value, ResultValue):
            raise ValueError("result must be a ResultValue with units and frame")
        _text(value.unit, "unit")
        if value.unit not in _UNITS[key.quantity]:
            raise ValueError("unsupported unit or incompatible quantity dimension")
        _text(value.coordinate_frame, "coordinate frame")
        snapshot[key] = ResultValue(
            _finite(value.value, "result value"), value.unit, value.coordinate_frame
        )
    return snapshot


@dataclass(frozen=True)
class _RegisteredResult:
    metadata: ReferenceMetadata
    tables: dict[str, dict[ResultKey, ResultValue]]


class ResultComparison:
    """Register extracted datasets and compare complete, matching result tables."""

    def __init__(self) -> None:
        """Create an empty offline result registry."""
        self._results: dict[str, _RegisteredResult] = {}

    def add_result(
        self,
        name: str,
        *,
        metadata: ReferenceMetadata,
        reactions: ResultInput = (),
        displacements: ResultInput = (),
        member_forces: ResultInput = (),
    ) -> None:
        """Copy and validate one named dataset, refusing duplicate identities.

        Any family may be absent, but at least one must contain data. Comparing
        an absent family raises ValueError. A failed registration stores nothing.
        No data extraction or solver import is performed.
        """
        _text(name, "result name")
        if name in self._results:
            raise ValueError("duplicate result name")
        if not isinstance(metadata, ReferenceMetadata):
            raise ValueError("reference metadata must be ReferenceMetadata")
        for field in fields(metadata):
            _text(getattr(metadata, field.name), f"reference metadata {field.name}")
        tables = {
            "reactions": _snapshot(reactions, "reactions"),
            "displacements": _snapshot(displacements, "displacements"),
            "member_forces": _snapshot(member_forces, "member_forces"),
        }
        if not any(tables.values()):
            raise ValueError("empty result dataset")
        self._results[name] = _RegisteredResult(metadata, tables)

    def compare_reactions(
        self, reference: str, candidate: str, *, tolerances: Mapping[ResultKey, Tolerance]
    ) -> ComparisonReport:
        """Compare complete nodal force and moment reaction tables."""
        return self._compare("reactions", reference, candidate, tolerances)

    def compare_deflections(
        self, reference: str, candidate: str, *, tolerances: Mapping[ResultKey, Tolerance]
    ) -> ComparisonReport:
        """Compare complete nodal translation and rotation tables."""
        return self._compare("displacements", reference, candidate, tolerances)

    def compare_member_forces(
        self, reference: str, candidate: str, *, tolerances: Mapping[ResultKey, Tolerance]
    ) -> ComparisonReport:
        """Compare member forces and moments at identical ends or stations."""
        return self._compare("member_forces", reference, candidate, tolerances)

    def _compare(
        self,
        family: str,
        reference: str,
        candidate: str,
        tolerances: Mapping[ResultKey, Tolerance],
    ) -> ComparisonReport:
        if reference not in self._results or candidate not in self._results:
            raise ValueError("unknown result name")
        ref = self._results[reference]
        other = self._results[candidate]
        if ref.metadata != other.metadata:
            raise ValueError("incompatible reference metadata")
        left, right = ref.tables[family], other.tables[family]
        if not left or not right:
            raise ValueError("empty result family")
        if not left.keys() & right.keys():
            raise ValueError("empty identity overlap")
        if left.keys() != right.keys():
            raise ValueError("missing or incompatible result identities")
        if not isinstance(tolerances, Mapping) or left.keys() != tolerances.keys():
            raise ValueError("tolerance identities must exactly match result identities")
        differences = tuple(
            self._difference(key, value, right[key], tolerances[key])
            for key, value in left.items()
        )
        return ComparisonReport(reference, candidate, ref.metadata, differences)

    @staticmethod
    def _difference(
        key: ResultKey, ref: ResultValue, candidate: ResultValue, tolerance: Tolerance
    ) -> Difference:
        if ref.unit != candidate.unit:
            raise ValueError("incompatible result units; explicit conversion required")
        if ref.coordinate_frame != candidate.coordinate_frame:
            raise ValueError("incompatible coordinate frames")
        if not isinstance(tolerance, Tolerance) or tolerance.unit != ref.unit:
            raise ValueError("tolerance units must match result units")
        absolute = _finite(tolerance.absolute, "absolute tolerance")
        relative = _finite(tolerance.relative, "relative tolerance")
        if absolute < 0 or relative < 0:
            raise ValueError("tolerances must be nonnegative")
        signed = _finite(candidate.value - ref.value, "signed difference")
        magnitude = abs(signed)
        ratio = (
            _finite(magnitude / abs(ref.value), "relative difference")
            if ref.value != 0
            else None
        )
        allowed = _finite(absolute + relative * abs(ref.value), "tolerance bound")
        return Difference(
            key=key,
            unit=ref.unit,
            coordinate_frame=ref.coordinate_frame,
            reference=ref.value,
            candidate=candidate.value,
            signed_difference=signed,
            absolute_difference=magnitude,
            relative_difference=ratio,
            allowed_difference=allowed,
            status="PASS" if magnitude <= allowed else "FAIL",
        )
