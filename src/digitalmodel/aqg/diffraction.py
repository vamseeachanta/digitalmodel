# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""G4 evidence records; mesh packaging screens do not establish acceptance.

Criteria: diffraction knowledge pack revision 4, section 5 (AQG r0 section 8
transcription). Original-source qualification is not established. The public
engineering status therefore remains NOT_RUN unless a recorded check fails.
Record completeness is reported separately and is not engineering acceptance.
This module imports no solver package and interprets no native program syntax.
Inputs are validated as finite floats; their canonical decimal representations
are interpreted as exact rational values for inclusive arithmetic comparisons.
This computational policy adds no engineering tolerance. Serialized values
remain floats, while boundary decisions use the unrounded rational values.
"""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import InitVar, dataclass, field
from enum import Enum
from fractions import Fraction
from pathlib import Path
from types import MappingProxyType
from typing import Iterable, Mapping

SOURCE_DOCUMENT = "hydrodynamics/diffraction/docs/agent-pack.md"
SOURCE_DIGEST = "0b997e73ea95ecdc41457535561687396693615680bdeb5b61e5eda48d655671"

# Each criterion below is transcribed from knowledge pack r4, section 5,
# rows G4.1a-G4.8e (AQG r0 section 8). These are internal QA requirements,
# not independently qualified values from a licensed engineering standard.
_CRITERIA = {
    "G4.1a": ("abs(M-rho*V)/M <= 0.01, input mass denominator", "1"),
    "G4.1b": ("restrained vertical residual <= 0.01*M*g; common rho and g", "N"),
    "G4.2a": ("independent/native volume difference <= 0.5%", "m^3"),
    "G4.2b": ("axis-volume spread/declared scale <= 0.005", "1"),
    "G4.3a": (
        "CoB differences <= 0.005*Lwl horizontally and 0.01*draft vertically",
        "m",
    ),
    "G4.3b": ("mass-model CoG reconciled with equilibrium heel and trim", "m"),
    "G4.3c": ("both GM > 0 and within 2.0% of independent values", "m"),
    "G4.4a": ("panel count <= installed-version limit; retain limit source", "count"),
    "G4.4b": (
        "aspect <= 3; <= 5 only at second-order waterline with quality note",
        "1",
    ),
    "G4.4c": ("maximum panel dimension <= shortest wavelength/6", "m"),
    "G4.4d": ("no unresolved native overlap or gap errors", "count"),
    "G4.4e": ("native memory estimate <= available memory", "byte"),
    "G4.5a": ("every wetted boundary top abs(z) <= 0.001", "m"),
    "G4.5b": ("waterline loop endpoint degree = 2", "count"),
    "G4.5c": ("abs(mesh draft-design draft) <= 0.01", "m"),
    "G4.5d": (
        "no prohibited above-water wetted panels; interior lid separate",
        "count",
    ),
    "G4.6a": ("every panel outward, positive signed volume; reliable topology", "m^3"),
    "G4.6b": ("zero-area panels = 0", "count"),
    "G4.6c": ("no duplicate symmetry declarations; verify reconstructed body", "count"),
    "G4.7a": (
        "per-body lid at omega_max >= 0.9*omega_i; resolve every control-surface body",
        "rad/s",
    ),
    "G4.7b": (
        "paired enabled removal in native inputs when omega_max >= omega_i",
        "count",
    ),
    "G4.8a": (
        "isometric, waterline close-up and panel-type views with digests",
        "count",
    ),
    "G4.8b": (
        "longest-period abs(heave-1) <= 0.02; record finite-depth applicability",
        "m/m",
    ),
    "G4.8c": ("diagonal damping >= 0 in declared term units", "declared term units"),
    "G4.8d": (
        "added-mass normalized asymmetry <= 2.0%; declare zero-term criterion",
        "declared term units",
    ),
    "G4.8e": (
        "no unresolved spikes within +/-5.0% of estimated irregular frequency",
        "rad/s",
    ),
}
CRITERIA = MappingProxyType(_CRITERIA)
_COMPUTED = frozenset({"G4.1a", "G4.4a", "G4.4b", "G4.4c", "G4.4e", "G4.7a", "G4.7b"})
_CALCULATOR_ORIGIN = object()


class Status(str, Enum):
    PASS = "PASS"  # nosec B105 - public result label
    FAIL = "FAIL"
    NOT_RUN = "NOT_RUN"
    NOT_APPLICABLE = "NOT_APPLICABLE"


def _text(value: str, name: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be nonempty text")
    return value


def _positive(value: float, name: str) -> float:
    if isinstance(value, bool):
        raise ValueError(f"{name} must be a finite positive number")
    try:
        number = float(value)
    except (TypeError, ValueError, OverflowError) as error:
        raise ValueError(f"{name} must be a finite positive number") from error
    if not math.isfinite(number) or number <= 0:
        raise ValueError(f"{name} must be a finite positive number")
    return number


def _count(value: int, name: str) -> int:
    if type(value) is not int or value <= 0:
        raise ValueError(f"{name} must be a positive integer")
    return value


def criteria_source() -> dict:
    """Fail closed if the source transcription is absent or changed."""
    path = Path(__file__).resolve().parents[1] / SOURCE_DOCUMENT
    try:
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
    except OSError as error:
        raise ValueError("criteria source is unavailable") from error
    if digest != SOURCE_DIGEST:
        raise ValueError(
            "criteria source digest mismatch; review the criteria before use"
        )
    return {
        "document": SOURCE_DOCUMENT,
        "revision": "4",
        "section": "5; AQG r0 section 8",
        "sha256": digest,
        "qualified": False,
        "limitation": "Original AQG source and licensed clause qualification are not established",
    }


@dataclass(frozen=True)
class Artifact:
    """Exact evidence bytes; caller establishes identity, rights and readiness."""

    reference: str
    content: bytes = field(repr=False)

    def __post_init__(self):
        _text(self.reference, "artifact reference")
        if not isinstance(self.content, bytes) or not self.content:
            raise ValueError("artifact content must be nonempty bytes")

    @property
    def sha256(self) -> str:
        return hashlib.sha256(self.content).hexdigest()

    def to_dict(self) -> dict:
        return {"reference": self.reference, "sha256": self.sha256}


@dataclass(frozen=True)
class Check:
    check_id: str
    applicable: bool | None
    status: Status
    observed: Mapping[str, float | int | str | bool]
    evidence: tuple[Artifact, ...]
    reason: str
    _origin: InitVar[object] = None

    def __post_init__(self, _origin):
        if self.check_id not in CRITERIA or not isinstance(self.status, Status):
            raise ValueError("unknown G4 check or status")
        if self.applicable is not None and type(self.applicable) is not bool:
            raise ValueError("applicability must be boolean or unknown")
        _text(self.reason, "reason")
        artifacts = tuple(self.evidence)
        if not all(isinstance(e, Artifact) for e in artifacts):
            raise ValueError("every supplied evidence item must be an Artifact")
        object.__setattr__(self, "evidence", artifacts)
        if self.status != Status.NOT_RUN:
            if not artifacts:
                raise ValueError("evaluated checks require artifact evidence")
            if self.check_id in _COMPUTED and _origin is not _CALCULATOR_ORIGIN:
                raise ValueError("computed rows require their calculator API")
            if self.applicable is not (self.status != Status.NOT_APPLICABLE):
                raise ValueError("status and applicability disagree")
        if self.applicable is False and self.check_id not in {"G4.1b", "G4.7b"}:
            raise ValueError("this required row cannot be excluded")
        snapshot = dict(self.observed)
        self._validate_exclusion(snapshot)
        if any(type(v) not in (int, float, str, bool) for v in snapshot.values()):
            raise ValueError("observations must be JSON scalars")
        json.dumps(snapshot, allow_nan=False)
        object.__setattr__(self, "observed", MappingProxyType(snapshot))
        object.__setattr__(self, "evidence", tuple(self.evidence))

    def _validate_exclusion(self, snapshot):
        if self.applicable is False:
            if (
                self.check_id == "G4.1b"
                and snapshot.get("equilibrium_mode") != "free_floating"
            ):
                raise ValueError(
                    "preload exclusion requires a free-floating equilibrium basis"
                )
            if self.check_id == "G4.7b":
                maximum = _positive(snapshot.get("maximum_rad_s"), "maximum_rad_s")
                first = _positive(
                    snapshot.get("first_irregular_rad_s"), "first_irregular_rad_s"
                )
                if maximum >= first:
                    raise ValueError(
                        "native-treatment exclusion requires frequencies below the trigger"
                    )

    @property
    def criterion(self) -> str:
        return CRITERIA[self.check_id][0]

    @property
    def units(self) -> str:
        return CRITERIA[self.check_id][1]

    def to_dict(self) -> dict:
        return {
            "check_id": self.check_id,
            "applicable": self.applicable,
            "status": self.status.value,
            "criterion": self.criterion,
            "units": self.units,
            "observed": dict(self.observed),
            "evidence": [e.to_dict() for e in self.evidence],
            "reason": self.reason,
        }


def reviewed_check(
    check_id,
    applicable,
    passed,
    evidence,
    basis,
    reviewer_role,
    *,
    equilibrium_mode=None,
) -> Check:
    """Record an external criterion-specific review, not a generic mesh verdict.

    The reviewer interprets the fixed criterion, including units and independent
    reference. The receipt records a role, not authenticated review authority.
    Only G4.1b (free flotation) and G4.7b (below trigger) permit exclusions.
    """
    criteria_source()
    if check_id in _COMPUTED:
        raise ValueError("computed rows require their calculator API")
    if type(applicable) is not bool or (
        passed is not None and type(passed) is not bool
    ):
        raise ValueError("review applicability and decision must be explicit booleans")
    _text(reviewer_role, "reviewer role")
    _text(basis, "review basis")
    if not evidence:
        raise ValueError("a review requires evidence, including for an exclusion")
    if not applicable and passed is not None:
        raise ValueError("an excluded row cannot carry a pass or fail decision")
    status = (
        Status.NOT_APPLICABLE
        if not applicable
        else Status.NOT_RUN
        if passed is None
        else Status.PASS
        if passed
        else Status.FAIL
    )
    observed = {"reviewer_role": reviewer_role}
    if not applicable:
        if check_id != "G4.1b" or equilibrium_mode != "free_floating":
            raise ValueError(
                "manual exclusion requires documented free flotation for G4.1b"
            )
        observed["equilibrium_mode"] = equilibrium_mode
    return Check(check_id, applicable, status, observed, tuple(evidence), basis)


def _numeric(check_id, passed, observed, artifact, reason):
    criteria_source()
    return Check(
        check_id,
        True,
        Status.PASS if passed else Status.FAIL,
        observed,
        (artifact,),
        reason,
        _CALCULATOR_ORIGIN,
    )


def check_mass(mass_kg, displacement_m3, density_kg_m3, artifact: Artifact) -> Check:
    """G4.1a: input-mass normalization; SI inputs, no unit conversion."""
    mass = _positive(mass_kg, "mass_kg")
    volume = _positive(displacement_m3, "displacement_m3")
    density = _positive(density_kg_m3, "density_kg_m3")
    displaced_exact = Fraction(str(volume)) * Fraction(str(density))
    displaced = _positive(float(displaced_exact), "displaced_mass_kg")
    residual_exact = abs(Fraction(str(mass)) - displaced_exact) / Fraction(str(mass))
    residual = float(residual_exact)
    # Knowledge pack r4 section 5, G4.1a: one percent of input weight.
    return _numeric(
        "G4.1a",
        residual_exact <= Fraction("0.01"),
        {
            "mass_kg": mass,
            "volume_m3": volume,
            "density_kg_m3": density,
            "displaced_mass_kg": displaced,
            "relative_mass_residual": residual,
        },
        artifact,
        "Compared mass residual against input mass",
    )


def check_panels(
    aspect_ratios,
    at_waterline,
    maximum_dimension_m,
    shortest_wavelength_m,
    artifact: Artifact,
    *,
    second_order=False,
    waterline_note="",
) -> tuple[Check, Check]:
    """G4.4b/c; shortest wavelength requires an independent dispersion basis."""
    ratios, flags = list(aspect_ratios), list(at_waterline)
    if (
        not ratios
        or len(ratios) != len(flags)
        or any(type(f) is not bool for f in flags)
    ):
        raise ValueError(
            "every panel requires an aspect ratio and boolean waterline flag"
        )
    if type(second_order) is not bool or not isinstance(waterline_note, str):
        raise ValueError("second_order must be boolean and waterline_note text")
    ratios = [_positive(r, "aspect ratio") for r in ratios]
    if min(ratios) < 1:
        raise ValueError("aspect ratio must be longest edge divided by shortest edge")
    dimension = _positive(maximum_dimension_m, "maximum_dimension_m")
    wavelength = _positive(shortest_wavelength_m, "shortest_wavelength_m")
    # Knowledge pack r4 section 5 G4.4b/c: aspect 3 (5 with exception), wavelength/6.
    limits = [5 if f and second_order and waterline_note.strip() else 3 for f in flags]
    failed = sum(r > limit for r, limit in zip(ratios, limits))
    aspect = _numeric(
        "G4.4b",
        failed == 0,
        {
            "maximum_aspect": max(ratios),
            "failed_panels": failed,
            "second_order": second_order,
            "waterline_note": waterline_note,
        },
        artifact,
        "Every supplied panel compared with its applicable bound",
    )
    resolution = _numeric(
        "G4.4c",
        Fraction(str(dimension)) <= Fraction(str(wavelength)) / 6,
        {"maximum_dimension_m": dimension, "shortest_wavelength_m": wavelength},
        artifact,
        "Compared maximum dimension with supplied wavelength/6",
    )
    return aspect, resolution


def check_resources(
    panel_count,
    installed_panel_limit,
    estimated_bytes,
    available_bytes,
    limit_source,
    artifact: Artifact,
) -> tuple[Check, Check]:
    """G4.4a/e; native program/version limit and memory snapshot are explicit."""
    values = [
        _count(v, n)
        for v, n in zip(
            (panel_count, installed_panel_limit, estimated_bytes, available_bytes),
            (
                "panel_count",
                "installed_panel_limit",
                "estimated_bytes",
                "available_bytes",
            ),
        )
    ]
    count, limit, estimated, available = values
    _text(limit_source, "installed limit source")
    return (
        _numeric(
            "G4.4a",
            count <= limit,
            {
                "panel_count": count,
                "installed_panel_limit": limit,
                "limit_source": limit_source,
            },
            artifact,
            "Compared count against the supplied installed-version limit",
        ),
        _numeric(
            "G4.4e",
            estimated <= available,
            {"estimated_bytes": estimated, "available_bytes": available},
            artifact,
            "Compared native estimate with available memory at the time of measurement",
        ),
    )


@dataclass(frozen=True)
class InteriorLid:
    panel_ids: tuple[int, ...]
    artifact: Artifact
    reviewer_role: str

    def __post_init__(self):
        if not self.panel_ids or len(set(self.panel_ids)) != len(self.panel_ids):
            raise ValueError("interior lid requires distinct panel identifiers")
        for panel in self.panel_ids:
            _count(panel, "interior lid panel identifier")
        if not isinstance(self.artifact, Artifact):
            raise ValueError("interior lid requires panel evidence")
        _text(self.reviewer_role, "lid reviewer role")
        object.__setattr__(self, "panel_ids", tuple(self.panel_ids))


@dataclass(frozen=True)
class NativeRemoval:
    """Reviewed native syntax and readback; a line alone is insufficient.

    ``enabled`` is an external syntax interpretation, never an automatic guess.
    ``result_input_sha256`` is the digest established by native result readback;
    a caller's unsupported assertion does not authenticate that relationship.
    """

    program: str
    version: str
    native_input: Artifact
    setting_line: str
    enabled: bool | None
    reviewer_role: str
    result: Artifact | None = None
    result_input_sha256: str | None = None

    def __post_init__(self):
        for name in ("program", "version", "reviewer_role", "setting_line"):
            _text(getattr(self, name), name)
        if self.program not in {"aqwa", "orcawave"}:
            raise ValueError("native program must use a supported canonical identifier")
        line = self.setting_line.strip()
        if line.startswith(("#", "!", "//", ";")):
            raise ValueError("native setting cannot be a comment")
        if line not in [
            s.strip() for s in self.native_input.content.decode("utf-8").splitlines()
        ]:
            raise ValueError(
                "native setting must be a complete line in the supplied input"
            )
        if self.enabled is not None and type(self.enabled) is not bool:
            raise ValueError("enabled must be boolean or unknown")
        if (
            self.result_input_sha256 is not None
            and self.result_input_sha256 != self.native_input.sha256
        ):
            raise ValueError("native result/input digest mismatch")
        if self.result is not None and not isinstance(self.result, Artifact):
            raise ValueError("native result must be an artifact")


@dataclass(frozen=True)
class BodySurface:
    """One declared body's treatment and its reviewed surface evidence."""

    body_id: str
    control_surface_required: bool
    lid: InteriorLid | None = None
    control_surface: Artifact | None = None

    def __post_init__(self):
        _text(self.body_id, "body identifier")
        if type(self.control_surface_required) is not bool:
            raise ValueError("control-surface applicability must be explicit")
        if self.lid is not None and not isinstance(self.lid, InteriorLid):
            raise ValueError("body lid requires typed panel evidence")
        if self.control_surface is not None and not isinstance(
            self.control_surface, Artifact
        ):
            raise ValueError("control surface requires an artifact")


def _body_inventory(bodies):
    if bodies is None:
        return None
    result = tuple(bodies)
    if not result or any(not isinstance(b, BodySurface) for b in result):
        raise ValueError("body inventory requires at least one typed body record")
    names = [b.body_id.strip().casefold() for b in result]
    if len(names) != len(set(names)):
        raise ValueError("body identifiers must be distinct")
    return result


def _body_observations(observed, bodies):
    result = dict(observed)
    for index, body in enumerate(bodies):
        result[f"body_{index}"] = body.body_id
        result[f"control_surface_required_{index}"] = body.control_surface_required
        if body.lid is not None:
            result[f"lid_panel_ids_{index}"] = ",".join(
                str(i) for i in body.lid.panel_ids
            )
            result[f"lid_reviewer_{index}"] = body.lid.reviewer_role
    return result


def _lid_row(observed, estimate, bodies):
    if bodies is None:
        return Check(
            "G4.7a",
            True,
            Status.NOT_RUN,
            observed,
            (estimate,),
            "Complete body inventory and treatment applicability not supplied",
        )
    # Knowledge pack r4 section 5 G4.7a: inclusive 0.9*omega_i trigger,
    # plus resolvable control surfaces on every applicable body.
    trigger = Fraction("0.9") * Fraction(str(observed["first_irregular_rad_s"]))
    needed = Fraction(str(observed["maximum_rad_s"])) >= trigger
    missing = any(
        (needed and b.lid is None)
        or (b.control_surface_required and b.control_surface is None)
        for b in bodies
    )
    artifacts = [estimate]
    for body in bodies:
        if body.lid is not None:
            artifacts.append(body.lid.artifact)
        if body.control_surface is not None:
            artifacts.append(body.control_surface)
    return Check(
        "G4.7a",
        True,
        Status.NOT_RUN if missing else Status.PASS,
        _body_observations(observed, bodies),
        tuple(artifacts),
        "Per-body lid and applicable control-surface coverage required",
        _CALCULATOR_ORIGIN,
    )


def _validate_native_pair(removal, compared_programs):
    proofs = tuple(removal)
    if any(not isinstance(p, NativeRemoval) for p in proofs):
        raise ValueError("native removal requires typed evidence")
    names = [p.program for p in proofs]
    if compared_programs is not None:
        if len(compared_programs) != 2 or set(compared_programs) != {
            "aqwa",
            "orcawave",
        }:
            raise ValueError(
                "comparison must identify the supported canonical program pair"
            )
    if len(names) != len(set(names)):
        raise ValueError("native removal proofs must identify distinct programs")
    return proofs


def _proof_observations(observed, proofs):
    result = {**observed, "native_program_count": len(proofs)}
    for index, proof in enumerate(proofs):
        result[f"program_{index}"] = proof.program
        result[f"version_{index}"] = proof.version
        result[f"reviewer_{index}"] = proof.reviewer_role
        result[f"setting_{index}"] = proof.setting_line
        result[f"setting_line_number_{index}"] = next(
            i
            for i, line in enumerate(
                proof.native_input.content.decode().splitlines(), 1
            )
            if line.strip() == proof.setting_line.strip()
        )
        result[f"input_sha256_{index}"] = proof.native_input.sha256
        if proof.result_input_sha256 is not None:
            result[f"result_input_sha256_{index}"] = proof.result_input_sha256
    return result


def _removal_row(observed, estimate, proofs, compared_programs):
    if observed["maximum_rad_s"] < observed["first_irregular_rad_s"]:
        return Check(
            "G4.7b",
            False,
            Status.NOT_APPLICABLE,
            observed,
            (estimate,),
            "Comparison below the supplied first irregular frequency",
            _CALCULATOR_ORIGIN,
        )
    names = {p.program for p in proofs}
    disabled = any(p.enabled is False for p in proofs)
    # Knowledge pack r4 section 5 G4.7b: enabled treatment in both programs.
    complete = (
        compared_programs is not None
        and names == set(compared_programs)
        and len(proofs) == 2
    ) and all(
        p.enabled is True and p.result is not None and p.result_input_sha256 is not None
        for p in proofs
    )
    status = Status.FAIL if disabled else Status.PASS if complete else Status.NOT_RUN
    artifacts = (estimate,) + tuple(
        e for p in proofs for e in (p.native_input, p.result) if e is not None
    )
    return Check(
        "G4.7b",
        True,
        status,
        _proof_observations(observed, proofs),
        artifacts,
        "Paired native inputs and linked result readback required; disabled treatment fails",
        _CALCULATOR_ORIGIN,
    )


def check_irregular_frequencies(
    maximum_rad_s,
    first_irregular_rad_s,
    estimate: Artifact,
    *,
    bodies: Iterable[BodySurface] | None = None,
    removal: Iterable[NativeRemoval] = (),
    compared_programs: tuple[str, str] | None = None,
) -> tuple[Check, Check]:
    """G4.7a/b; omega_i and its independent basis are required inputs.

    No frequency estimate is guessed. Setting syntax is reviewed externally;
    successful removal and response-spike checks remain separate evidence rows.
    """
    criteria_source()
    maximum = _positive(maximum_rad_s, "maximum_rad_s")
    first = _positive(first_irregular_rad_s, "first_irregular_rad_s")
    if not isinstance(estimate, Artifact):
        raise ValueError("frequency estimate requires typed evidence")
    # Knowledge pack r4 section 5 G4.7a: lid trigger at 0.9*omega_i.
    observed = {
        "maximum_rad_s": maximum,
        "first_irregular_rad_s": first,
        "lid_trigger_rad_s": float(Fraction("0.9") * Fraction(str(first))),
    }
    proofs = _validate_native_pair(removal, compared_programs)
    return (
        _lid_row(observed, estimate, _body_inventory(bodies)),
        _removal_row(observed, estimate, proofs, compared_programs),
    )


@dataclass(frozen=True)
class Assessment:
    checks: tuple[Check, ...]
    record_status: Status = field(init=False)
    status: Status = field(init=False)

    def __post_init__(self):
        criteria_source()
        rows = tuple(self.checks)
        if not all(isinstance(c, Check) for c in rows) or tuple(
            c.check_id for c in rows
        ) != tuple(CRITERIA):
            raise ValueError("assessment requires the complete ordered G4 inventory")
        record = (
            Status.FAIL
            if any(c.status == Status.FAIL for c in rows)
            else Status.NOT_RUN
            if any(c.status == Status.NOT_RUN for c in rows)
            else Status.PASS
        )
        object.__setattr__(self, "checks", rows)
        object.__setattr__(self, "record_status", record)
        object.__setattr__(
            self, "status", Status.FAIL if record == Status.FAIL else Status.NOT_RUN
        )

    def to_dict(self) -> dict:
        return {
            "status": self.status.value,
            "record_status": self.record_status.value,
            "criteria_source": criteria_source(),
            "limitation": "Record completeness is not native diffraction acceptance",
            "checks": [c.to_dict() for c in self.checks],
        }


def assess(checks: Iterable[Check]) -> Assessment:
    """Complete the fixed inventory; never promote packaging or comparison PASS."""
    criteria_source()
    supplied = {}
    for check in checks:
        if not isinstance(check, Check):
            raise ValueError("G4 requires Check records, not a mesh packaging status")
        if check.check_id in supplied:
            raise ValueError(f"duplicate check: {check.check_id}")
        supplied[check.check_id] = check
    rows = tuple(
        supplied.get(key)
        or Check(
            key,
            None,
            Status.NOT_RUN,
            {},
            (),
            "Applicability and required evidence not supplied",
        )
        for key in CRITERIA
    )
    return Assessment(rows)
