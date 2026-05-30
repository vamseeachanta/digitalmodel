"""Inverse resolver for complete, solver-agnostic diffraction specs."""

from __future__ import annotations

from enum import Enum
from pathlib import Path
from typing import Any, Optional

from pydantic import BaseModel, Field, model_validator

from digitalmodel.hydrodynamics.diffraction.assumption_ledger import (
    AssumptionLedger,
    AssumptionSource,
    Confidence,
)
from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    DiffractionSpec,
    EnvironmentSpec,
    FrequencyInputType,
    FrequencyRangeSpec,
    FrequencySpec,
    HeadingRangeSpec,
    MeshFormatType,
    SolverOptions,
    SymmetryType,
    VesselGeometry,
    VesselInertia,
    VesselSpec,
    WaveHeadingSpec,
)
from digitalmodel.hydrodynamics.diffraction.parametric_spec_generator import (
    GRAVITY,
    RHO_SEAWATER,
    estimate_cog_from_dimensions,
    estimate_mass_from_dimensions,
    estimate_radii_of_gyration_from_dimensions,
)
from digitalmodel.hydrodynamics.hull_library.lookup import (
    HullLookup,
    HullLookupTarget,
)


class Outcome(str, Enum):
    """Supported user outcomes for inverse resolution."""

    SHIP_RAOS = "ship_raos"
    ADDED_MASS_DAMPING = "added_mass_damping"
    QTF = "qtf"


OUTCOME_REQUIREMENTS: dict[Outcome, set[str]] = {
    Outcome.SHIP_RAOS: {
        "vessel.geometry.mesh_file",
        "vessel.inertia.mass",
        "vessel.inertia.centre_of_gravity",
        "environment.water_depth",
        "frequencies",
        "wave_headings",
    },
    Outcome.ADDED_MASS_DAMPING: {
        "vessel.geometry.mesh_file",
        "vessel.inertia.mass",
        "vessel.inertia.centre_of_gravity",
        "environment.water_depth",
        "frequencies",
        "wave_headings",
    },
    Outcome.QTF: {
        "vessel.geometry.mesh_file",
        "vessel.inertia.mass",
        "vessel.inertia.centre_of_gravity",
        "environment.water_depth",
        "frequencies",
        "wave_headings",
        "solver_options.qtf_calculation",
    },
}


ASSUMPTION_CONTROLLED_FIELDS: tuple[str, ...] = (
    "vessel.geometry.mesh_file",
    "vessel.geometry.mesh_format",
    "vessel.geometry.symmetry",
    "vessel.geometry.reference_point",
    "vessel.geometry.waterline_z",
    "vessel.geometry.length_units",
    "vessel.inertia.mass",
    "vessel.inertia.centre_of_gravity",
    "vessel.inertia.radii_of_gyration",
    "vessel.inertia.inertia_tensor",
    "vessel.inertia.cog_z",
    "environment.water_depth",
    "environment.water_density",
    "environment.gravity",
    "frequencies.range",
    "wave_headings.range",
    "solver_options.qtf_calculation",
)


class PrincipalDimensions(BaseModel):
    """Principal vessel dimensions in metres and displacement in tonnes."""

    loa: Optional[float] = Field(default=None, gt=0)
    length_bp: Optional[float] = Field(default=None, gt=0)
    beam: float = Field(gt=0)
    draft: float = Field(gt=0)
    displacement_t: Optional[float] = Field(default=None, gt=0)
    block_coefficient: Optional[float] = Field(default=None, gt=0)

    @model_validator(mode="after")
    def check_length_available(self) -> "PrincipalDimensions":
        if self.loa is None and self.length_bp is None:
            raise ValueError("Either loa or length_bp must be supplied")
        return self

    @property
    def length(self) -> float:
        return self.length_bp if self.length_bp is not None else self.loa


class ResolverInputs(BaseModel):
    """Inputs accepted by the inverse resolver."""

    dimensions: Optional[PrincipalDimensions] = None
    mesh_file: Optional[str] = None
    water_depth: Optional[float | str] = None
    partial_spec: Optional[dict[str, Any]] = None
    hull_id: Optional[str] = None
    inertia_mode: str = "free_floating"


class ResolverConfig(BaseModel):
    """Configuration for default and lookup resolution behaviour."""

    exact_thresh: float = Field(default=0.95, ge=0.0, le=1.0)
    near_thresh: float = Field(default=0.80, ge=0.0, le=1.0)
    freq_start: float = Field(default=0.2, gt=0)
    freq_end: float = Field(default=2.0, gt=0)
    freq_count: int = Field(default=30, gt=0)
    heading_start: float = 0.0
    heading_end: float = 180.0
    heading_increment: float = Field(default=15.0, gt=0)
    water_density: float = RHO_SEAWATER
    gravity: float = GRAVITY


def resolve(
    outcome: Outcome | str,
    inputs: ResolverInputs | dict[str, Any],
    *,
    hull_lookup: Any = None,
    rao_db: Any = None,
    config: ResolverConfig | None = None,
) -> tuple[DiffractionSpec, AssumptionLedger]:
    """Resolve user outcome and sparse inputs into a complete DiffractionSpec."""
    del rao_db  # Source separation: RAODatabase may cross-check later, not write.
    outcome = Outcome(outcome)
    resolver_inputs = (
        inputs
        if isinstance(inputs, ResolverInputs)
        else ResolverInputs.model_validate(inputs)
    )
    config = config or ResolverConfig()
    data = _deep_copy(resolver_inputs.partial_spec or {})
    ledger = AssumptionLedger()

    dims = resolver_inputs.dimensions
    geometry_data = _nested(data, "vessel", "geometry")
    mesh_file = resolver_inputs.mesh_file or geometry_data.get("mesh_file")

    _resolve_geometry(data, resolver_inputs, geometry_data, mesh_file, ledger)
    _resolve_environment(data, resolver_inputs, config, ledger)
    _resolve_frequencies(data, config, ledger)
    _resolve_headings(data, config, ledger)
    _resolve_solver_options(data, outcome, ledger)
    _resolve_inertia(
        data,
        dims,
        resolver_inputs,
        config,
        hull_lookup,
        ledger,
    )

    spec = DiffractionSpec.model_validate(data)
    return spec, ledger


def _resolve_geometry(
    data: dict[str, Any],
    inputs: ResolverInputs,
    geometry_data: dict[str, Any],
    mesh_file: str | None,
    ledger: AssumptionLedger,
) -> None:
    vessel = _nested(data, "vessel")
    geometry = _nested(vessel, "geometry")
    if "name" not in vessel:
        vessel["name"] = inputs.hull_id or Path(mesh_file or "resolved_hull").stem
        ledger.record(
            "vessel.name",
            vessel["name"],
            AssumptionSource.ASSUMED_DEFAULT,
            "Name derived from hull hint or mesh filename",
            Confidence.LOW,
            reference="default",
            impact=1,
        )
    if mesh_file and "mesh_file" not in geometry:
        geometry["mesh_file"] = mesh_file
    if "mesh_file" not in geometry:
        raise ValueError("mesh_file is required when partial_spec lacks geometry")
    if "mesh_format" not in geometry:
        geometry["mesh_format"] = _mesh_format_from_path(geometry["mesh_file"])
        ledger.record(
            "vessel.geometry.mesh_format",
            geometry["mesh_format"],
            AssumptionSource.ASSUMED_DEFAULT,
            "Mesh format inferred from file extension",
            Confidence.LOW,
            reference="extension",
            impact=2,
        )
    for field, value in (
        ("symmetry", SymmetryType.NONE.value),
        ("reference_point", [0.0, 0.0, 0.0]),
        ("waterline_z", 0.0),
        ("length_units", "m"),
    ):
        if field not in geometry:
            geometry[field] = value
            ledger.record(
                f"vessel.geometry.{field}",
                value,
                AssumptionSource.ASSUMED_DEFAULT,
                "Conservative geometry default",
                Confidence.LOW,
                reference="default",
                impact=2,
            )
    if "type" not in vessel and inputs.hull_id:
        vessel["type"] = inputs.hull_id


def _resolve_environment(
    data: dict[str, Any],
    inputs: ResolverInputs,
    config: ResolverConfig,
    ledger: AssumptionLedger,
) -> None:
    env = _nested(data, "environment")
    if inputs.water_depth is not None:
        env["water_depth"] = inputs.water_depth
    if "water_depth" not in env:
        raise ValueError("water_depth is required")
    if "water_density" not in env:
        env["water_density"] = config.water_density
        ledger.record(
            "environment.water_density",
            config.water_density,
            AssumptionSource.ASSUMED_DEFAULT,
            "Default seawater density",
            Confidence.LOW,
            reference="default",
            impact=3,
        )
    if "gravity" not in env:
        env["gravity"] = config.gravity
        ledger.record(
            "environment.gravity",
            config.gravity,
            AssumptionSource.ASSUMED_DEFAULT,
            "Standard gravity",
            Confidence.LOW,
            reference="default",
            impact=3,
        )
    EnvironmentSpec.model_validate(env)


def _resolve_frequencies(
    data: dict[str, Any],
    config: ResolverConfig,
    ledger: AssumptionLedger,
) -> None:
    if "frequencies" in data:
        return
    data["frequencies"] = FrequencySpec(
        input_type=FrequencyInputType.FREQUENCY,
        range=FrequencyRangeSpec(
            start=config.freq_start,
            end=config.freq_end,
            count=config.freq_count,
        ),
    ).model_dump(mode="json", exclude_none=True)
    ledger.record(
        "frequencies.range",
        data["frequencies"]["range"],
        AssumptionSource.ASSUMED_DEFAULT,
        "Default diffraction frequency sweep",
        Confidence.LOW,
        reference="default",
        impact=4,
    )


def _resolve_headings(
    data: dict[str, Any],
    config: ResolverConfig,
    ledger: AssumptionLedger,
) -> None:
    if "wave_headings" in data:
        return
    data["wave_headings"] = WaveHeadingSpec(
        range=HeadingRangeSpec(
            start=config.heading_start,
            end=config.heading_end,
            increment=config.heading_increment,
        ),
        symmetry=True,
    ).model_dump(mode="json", exclude_none=True)
    ledger.record(
        "wave_headings.range",
        data["wave_headings"]["range"],
        AssumptionSource.ASSUMED_DEFAULT,
        "Default heading sweep",
        Confidence.LOW,
        reference="default",
        impact=4,
    )


def _resolve_solver_options(
    data: dict[str, Any],
    outcome: Outcome,
    ledger: AssumptionLedger,
) -> None:
    solver = _nested(data, "solver_options")
    if outcome == Outcome.QTF and "qtf_calculation" not in solver:
        solver["qtf_calculation"] = True
        ledger.record(
            "solver_options.qtf_calculation",
            True,
            AssumptionSource.ASSUMED_DEFAULT,
            "QTF outcome requires QTF calculation",
            Confidence.LOW,
            reference="outcome:qtf",
            impact=4,
        )
    SolverOptions.model_validate(solver)


def _resolve_inertia(
    data: dict[str, Any],
    dims: PrincipalDimensions | None,
    inputs: ResolverInputs,
    config: ResolverConfig,
    hull_lookup: Any,
    ledger: AssumptionLedger,
) -> None:
    vessel = _nested(data, "vessel")
    inertia = _nested(vessel, "inertia")
    inertia.setdefault("mode", inputs.inertia_mode)
    if dims is None and _inertia_needs_dimensions(inertia):
        geometry = _nested(vessel, "geometry")
        dims = _derive_dimensions_from_mesh(geometry["mesh_file"], geometry, ledger)

    if "mass" not in inertia:
        # Only consult the hull lookup when mass actually needs resolving, so a
        # user-supplied mass never triggers a spurious DATABASE_LOOKUP record
        # (keeps the "detailed run -> empty ledger" contract intact even when
        # dimensions happen to also be supplied).
        lookup_values = _lookup_values(dims, hull_lookup, config, ledger)
        mass, source, confidence, reference, basis = _mass_from_sources(
            dims, lookup_values, config
        )
        inertia["mass"] = mass
        ledger.record(
            "vessel.inertia.mass",
            mass,
            source,
            basis,
            confidence,
            reference=reference,
            impact=5,
        )
    if "centre_of_gravity" not in inertia:
        if dims is None:
            raise ValueError("dimensions are required to estimate centre_of_gravity")
        cog = estimate_cog_from_dimensions(dims)
        inertia["centre_of_gravity"] = cog
        ledger.record(
            "vessel.inertia.centre_of_gravity",
            cog,
            AssumptionSource.ESTIMATED_FROM_DATA,
            "Empirical CoG from principal dimensions",
            Confidence.MEDIUM,
            reference="estimate_cog_from_dimensions",
            impact=5,
        )
    if inertia["mode"] == "free_floating" and "radii_of_gyration" not in inertia:
        if dims is None:
            raise ValueError("dimensions are required to estimate radii_of_gyration")
        radii = estimate_radii_of_gyration_from_dimensions(dims)
        inertia["radii_of_gyration"] = radii
        ledger.record(
            "vessel.inertia.radii_of_gyration",
            radii,
            AssumptionSource.ESTIMATED_FROM_DATA,
            "Empirical radii from principal dimensions",
            Confidence.MEDIUM,
            reference="estimate_radii_of_gyration_from_dimensions",
            impact=5,
        )
    if (
        inertia["mode"] == "explicit"
        and "radii_of_gyration" not in inertia
        and "inertia_tensor" not in inertia
    ):
        if dims is None:
            raise ValueError("dimensions are required to estimate inertia_tensor")
        radii = estimate_radii_of_gyration_from_dimensions(dims)
        mass = inertia["mass"]
        inertia["inertia_tensor"] = {
            "Ixx": mass * radii[0] ** 2,
            "Iyy": mass * radii[1] ** 2,
            "Izz": mass * radii[2] ** 2,
            "Ixy": 0.0,
            "Ixz": 0.0,
            "Iyz": 0.0,
        }
        # Radii of gyration are defined about the centre of mass, so the
        # estimated diagonal tensor is CG-relative. Label it accordingly
        # instead of inheriting the schema default ("body_origin"), which
        # would mis-state the reference point whenever the CoG is offset.
        inertia["inertia_tensor_origin"] = "centre_of_mass"
        ledger.record(
            "vessel.inertia.inertia_tensor",
            inertia["inertia_tensor"],
            AssumptionSource.ESTIMATED_FROM_DATA,
            "Diagonal inertia tensor estimated from empirical radii",
            Confidence.MEDIUM,
            reference="estimate_radii_of_gyration_from_dimensions",
            impact=5,
        )
    if inertia["mode"] == "free_floating" and "cog_z" not in inertia:
        inertia["cog_z"] = inertia["centre_of_gravity"][2]
        ledger.record(
            "vessel.inertia.cog_z",
            inertia["cog_z"],
            AssumptionSource.ESTIMATED_FROM_DATA,
            "Derived from centre_of_gravity z coordinate",
            Confidence.MEDIUM,
            reference="centre_of_gravity",
            impact=3,
        )
    VesselInertia.model_validate(inertia)
    VesselSpec.model_validate(vessel)


def _inertia_needs_dimensions(inertia: dict[str, Any]) -> bool:
    if "mass" not in inertia or "centre_of_gravity" not in inertia:
        return True
    if inertia.get("mode", "free_floating") == "free_floating":
        return "radii_of_gyration" not in inertia or "cog_z" not in inertia
    return "radii_of_gyration" not in inertia and "inertia_tensor" not in inertia


def _lookup_values(
    dims: PrincipalDimensions | None,
    hull_lookup: Any,
    config: ResolverConfig,
    ledger: AssumptionLedger,
) -> dict[str, Any] | None:
    if dims is None:
        return None
    lookup = hull_lookup or HullLookup()
    match = lookup.get_hull_form(
        HullLookupTarget(
            loa_m=dims.length,
            beam_m=dims.beam,
            draft_m=dims.draft,
            displacement_t=dims.displacement_t,
        )
    )
    if match.similarity_score < config.near_thresh:
        return None
    confidence = (
        Confidence.HIGH
        if match.similarity_score >= config.exact_thresh
        else Confidence.MEDIUM
    )
    entry = match.matched_entry
    displacement_t = _entry_value(entry, "displacement_t")
    if displacement_t is None:
        return None
    ledger.record(
        "vessel.hull_lookup",
        match.hull_id,
        AssumptionSource.DATABASE_LOOKUP,
        f"Nearest hull similarity {match.similarity_score:.3f}",
        confidence,
        reference=match.hull_id,
        impact=2,
    )
    return {
        "mass": float(displacement_t) * 1000.0,
        "confidence": confidence,
        "reference": match.hull_id,
    }


def _mass_from_sources(
    dims: PrincipalDimensions | None,
    lookup_values: dict[str, Any] | None,
    config: ResolverConfig,
) -> tuple[float, AssumptionSource, Confidence, str | None, str]:
    if lookup_values is not None:
        return (
            lookup_values["mass"],
            AssumptionSource.DATABASE_LOOKUP,
            lookup_values["confidence"],
            lookup_values["reference"],
            "Mass from gated nearest-neighbour hull lookup",
        )
    if dims is None:
        raise ValueError("dimensions are required to estimate mass")
    return (
        estimate_mass_from_dimensions(dims, config.water_density),
        AssumptionSource.ESTIMATED_FROM_DATA,
        Confidence.MEDIUM,
        "estimate_mass_from_dimensions",
        "Empirical mass from principal dimensions",
    )


def _derive_dimensions_from_mesh(
    mesh_file: str,
    geometry: dict[str, Any],
    ledger: AssumptionLedger,
) -> PrincipalDimensions:
    units = geometry.get("length_units", "m")
    symmetry = SymmetryType(geometry.get("symmetry", SymmetryType.NONE.value))
    waterline_z = geometry.get("waterline_z", 0.0)
    factor = _unit_factor(units)
    points = _read_mesh_points(Path(mesh_file))
    if not points:
        raise ValueError(f"Cannot derive dimensions: no vertices in {mesh_file}")
    xs = [p[0] * factor for p in points]
    ys = [p[1] * factor for p in points]
    zs = [p[2] * factor for p in points]
    waterline = float(waterline_z) * factor
    loa = max(xs) - min(xs)
    beam = max(ys) - min(ys)
    if symmetry in (SymmetryType.YZ, SymmetryType.XZ_YZ):
        loa *= 2.0
    if symmetry in (SymmetryType.XZ, SymmetryType.XZ_YZ):
        beam *= 2.0
    submerged = [z for z in zs if z <= waterline + 1e-9]
    if not submerged:
        raise ValueError(
            "Cannot derive draft: mesh has no vertices below waterline_z"
        )
    draft = waterline - min(submerged)
    if loa <= 0 or beam <= 0 or draft <= 0:
        raise ValueError(
            "Cannot derive positive dimensions from mesh; supply dimensions"
        )
    dims = PrincipalDimensions(loa=loa, beam=beam, draft=draft)
    ledger.record(
        "vessel.principal_dimensions",
        dims.model_dump(mode="json", exclude_none=True),
        AssumptionSource.ESTIMATED_FROM_DATA,
        "Derived from mesh bounding box with units, symmetry, and waterline",
        Confidence.MEDIUM,
        reference=str(mesh_file),
        impact=5,
    )
    return dims


def _read_mesh_points(path: Path) -> list[tuple[float, float, float]]:
    if not path.exists():
        raise ValueError(f"Cannot derive dimensions: mesh file not found: {path}")
    suffix = path.suffix.lower()
    if suffix == ".obj":
        return _read_obj_points(path)
    if suffix in {".gdf", ".dat", ".stl", ""}:
        return _read_numeric_triplets(path)
    raise ValueError(
        f"Cannot derive dimensions from unsupported mesh format '{suffix}'"
    )


def _read_obj_points(path: Path) -> list[tuple[float, float, float]]:
    points = []
    for line in path.read_text(encoding="utf-8").splitlines():
        parts = line.split()
        if len(parts) >= 4 and parts[0] == "v":
            points.append((float(parts[1]), float(parts[2]), float(parts[3])))
    return points


def _read_numeric_triplets(path: Path) -> list[tuple[float, float, float]]:
    points = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.lstrip().startswith("#"):
            continue
        parts = line.replace(",", " ").split()
        if len(parts) != 3:
            continue
        try:
            points.append((float(parts[0]), float(parts[1]), float(parts[2])))
        except ValueError:
            continue
    return points


def _unit_factor(units: str) -> float:
    normalised = units.lower()
    factors = {
        "m": 1.0,
        "meter": 1.0,
        "meters": 1.0,
        "metre": 1.0,
        "metres": 1.0,
        "ft": 0.3048,
        "feet": 0.3048,
        "foot": 0.3048,
        "mm": 0.001,
        "cm": 0.01,
    }
    if normalised not in factors:
        raise ValueError(
            f"Cannot derive dimensions: unknown length_units '{units}'"
        )
    return factors[normalised]


def _mesh_format_from_path(path: str) -> str:
    suffix = Path(path).suffix.lower().lstrip(".")
    try:
        return MeshFormatType(suffix).value
    except ValueError:
        return MeshFormatType.AUTO.value


def _entry_value(entry: Any, key: str) -> Any:
    if isinstance(entry, dict):
        return entry.get(key)
    return getattr(entry, key, None)


def _nested(data: dict[str, Any], *keys: str) -> dict[str, Any]:
    current = data
    for key in keys:
        value = current.setdefault(key, {})
        if not isinstance(value, dict):
            raise ValueError(f"{'.'.join(keys)} must be a mapping")
        current = value
    return current


def _deep_copy(data: dict[str, Any]) -> dict[str, Any]:
    return {
        key: _deep_copy(value) if isinstance(value, dict) else value
        for key, value in data.items()
    }


__all__ = [
    "ASSUMPTION_CONTROLLED_FIELDS",
    "OUTCOME_REQUIREMENTS",
    "Outcome",
    "PrincipalDimensions",
    "ResolverConfig",
    "ResolverInputs",
    "resolve",
]
