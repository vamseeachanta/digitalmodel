"""Canonical input schema for diffraction analysis (WRK-057).

Solver-agnostic specification that can generate inputs for AQWA, OrcaWave,
and other BEM solvers. This is the 'spec.yml' format.

Uses Pydantic v2 models to capture the superset of fields needed by both
AQWA and OrcaWave backends. Each sub-model is independently validatable
and composable into the top-level DiffractionSpec.

Example usage:
    from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec

    spec = DiffractionSpec.from_yaml("analysis.yml")
    freqs = spec.frequencies.to_frequencies_rad_s()
    headings = spec.wave_headings.to_heading_list()
    bodies = spec.get_bodies()
"""

from __future__ import annotations

from enum import Enum
from pathlib import Path
import warnings
from typing import Any, Literal, Optional

import numpy as np
from pydantic import (
    BaseModel,
    Field,
    field_validator,
    model_serializer,
    model_validator,
)


# ---------------------------------------------------------------------------
# Enumerations
# ---------------------------------------------------------------------------


class AnalysisType(str, Enum):
    """Type of hydrodynamic analysis."""

    DIFFRACTION = "diffraction"
    RADIATION = "radiation"
    FULL_QTF = "full_qtf"
    FREQUENCY_DOMAIN = "frequency_domain"
    TIME_DOMAIN = "time_domain"


class MeshFormatType(str, Enum):
    """Supported mesh file formats."""

    GDF = "gdf"
    DAT = "dat"
    STL = "stl"
    MSH = "msh"
    OBJ = "obj"
    AUTO = "auto"  # detect from extension


class SymmetryType(str, Enum):
    """Mesh symmetry options."""

    NONE = "none"
    XZ = "xz"
    YZ = "yz"
    XZ_YZ = "xz+yz"


class FrequencyInputType(str, Enum):
    """How frequencies are specified."""

    PERIOD = "period"
    FREQUENCY = "frequency"


class FrequencyDistribution(str, Enum):
    """Frequency spacing distribution."""

    LINEAR = "linear"
    LOGARITHMIC = "logarithmic"


class LoadRAOMethod(str, Enum):
    """Load RAO calculation method."""

    HASKIND = "haskind"
    DIRECT = "direct"
    BOTH = "both"


class SolverPrecision(str, Enum):
    """Solver numerical precision."""

    SINGLE = "single"
    DOUBLE = "double"


class OutputFormat(str, Enum):
    """Output file formats."""

    CSV = "csv"
    XLSX = "xlsx"
    HDF5 = "hdf5"
    ORCAFLEX_YML = "orcaflex_yml"
    ORCAFLEX_JSON = "orcaflex_json"


class OutputComponent(str, Enum):
    """Result components to output."""

    RAOS = "raos"
    ADDED_MASS = "added_mass"
    DAMPING = "damping"
    EXCITATION = "excitation"
    MEAN_DRIFT = "mean_drift"
    QTF = "qtf"
    PRESSURE = "pressure"


# ---------------------------------------------------------------------------
# Sub-models
# ---------------------------------------------------------------------------


class VesselGeometry(BaseModel):
    """Vessel mesh geometry specification."""

    mesh_file: str = Field(
        ...,
        description="Path to mesh file (relative to spec.yml location)",
    )
    mesh_format: MeshFormatType = Field(
        MeshFormatType.AUTO,
        description=(
            "Mesh file format, auto-detected from extension if not specified"
        ),
    )
    symmetry: SymmetryType = Field(
        SymmetryType.NONE,
        description="Mesh symmetry plane(s)",
    )
    reference_point: list[float] = Field(
        default=[0.0, 0.0, 0.0],
        description="Reference point [x, y, z] in meters",
    )
    waterline_z: float = Field(
        default=0.0,
        description="Z-coordinate of waterline in meters",
    )
    length_units: str = Field(
        default="m",
        description="Length units of the mesh file",
    )

    @field_validator("reference_point")
    @classmethod
    def validate_reference_point(cls, v: list[float]) -> list[float]:
        if len(v) != 3:
            raise ValueError(
                "reference_point must have exactly 3 values [x, y, z]"
            )
        return v


class VesselInertia(BaseModel):
    """Vessel mass and inertia properties.

    Supports two modes:
    - ``explicit`` (default): Mass and inertia specified directly.
    - ``free_floating``: Mass auto-computed from displaced volume by solver.
      Only ``radii_of_gyration`` and ``cog_z`` are used; ``mass`` and
      ``centre_of_gravity`` are kept for schema compatibility but ignored
      by the OrcaWave backend.
    """

    mode: Literal["explicit", "free_floating"] = Field(
        "explicit",
        description=(
            "Inertia specification mode: 'explicit' (mass and inertia "
            "given directly) or 'free_floating' (mass auto-computed "
            "from displaced volume by the solver)"
        ),
    )
    mass: float = Field(..., gt=0, description="Vessel mass in kg")
    centre_of_gravity: list[float] = Field(
        ...,
        description="Centre of gravity [x, y, z] in meters",
    )
    radii_of_gyration: Optional[list[float]] = Field(
        None,
        description="Radii of gyration [kxx, kyy, kzz] in meters",
    )
    inertia_tensor: Optional[dict[str, float]] = Field(
        None,
        description=(
            "Full inertia tensor components: "
            "Ixx, Iyy, Izz, Ixy, Ixz, Iyz (kg.m^2)"
        ),
    )
    cog_z: Optional[float] = Field(
        None,
        description=(
            "Centre of gravity z-position relative to free surface (m). "
            "Only used in free_floating mode. Defaults to 0.0."
        ),
    )
    inertia_tensor_origin: str = Field(
        "body_origin",
        description=(
            "Origin for inertia tensor specification: "
            "'body_origin' or 'centre_of_mass'"
        ),
    )

    @field_validator("centre_of_gravity")
    @classmethod
    def validate_cog(cls, v: list[float]) -> list[float]:
        if len(v) != 3:
            raise ValueError(
                "centre_of_gravity must have exactly 3 values [x, y, z]"
            )
        return v

    @field_validator("radii_of_gyration")
    @classmethod
    def validate_rog(
        cls, v: Optional[list[float]]
    ) -> Optional[list[float]]:
        if v is not None and len(v) != 3:
            raise ValueError(
                "radii_of_gyration must have exactly 3 values [kxx, kyy, kzz]"
            )
        return v

    @model_validator(mode="after")
    def check_inertia_specified(self) -> VesselInertia:
        if self.mode == "free_floating":
            if self.radii_of_gyration is None:
                raise ValueError(
                    "radii_of_gyration is required in free_floating mode"
                )
        elif self.radii_of_gyration is None and self.inertia_tensor is None:
            raise ValueError(
                "Either radii_of_gyration or inertia_tensor must be specified"
            )
        return self


class VesselSpec(BaseModel):
    """Complete vessel specification."""

    name: str = Field(..., description="Vessel name identifier")
    type: Optional[str] = Field(
        None,
        description=(
            "Vessel type (e.g., FPSO, semi-sub, ship, spar, barge)"
        ),
    )
    geometry: VesselGeometry
    inertia: VesselInertia
    external_stiffness: Optional[list[list[float]]] = Field(
        None,
        description="6x6 external stiffness matrix",
    )
    external_damping: Optional[list[list[float]]] = Field(
        None,
        description="6x6 external damping matrix",
    )
    fixed_dofs: Optional[list[str]] = Field(
        None,
        description=(
            "Fixed degrees of freedom: surge, sway, heave, roll, pitch, yaw"
        ),
    )
    control_surface: Optional["ControlSurfaceSpec"] = Field(
        None,
        description="Control surface mesh for mean drift loads",
    )


class EnvironmentSpec(BaseModel):
    """Environmental conditions for the analysis."""

    water_depth: float | str = Field(
        ...,
        description="Water depth in meters, or 'infinite' for deep water",
    )
    water_density: float = Field(
        default=1025.0,
        gt=0,
        description="Water density in kg/m^3",
    )
    gravity: float = Field(
        default=9.80665,
        gt=0,
        description="Gravitational acceleration in m/s^2",
    )

    @field_validator("water_depth", mode="before")
    @classmethod
    def validate_water_depth(cls, v: Any) -> float | str:
        if isinstance(v, str):
            if v.lower() in ("infinite", "inf", "deep"):
                return "infinite"
            raise ValueError(
                f"Invalid water_depth string: {v}. "
                "Use 'infinite' for deep water."
            )
        if isinstance(v, (int, float)):
            if v <= 0:
                raise ValueError(
                    "water_depth must be positive or 'infinite'"
                )
            return float(v)
        raise ValueError(
            f"water_depth must be a number or 'infinite', got {type(v)}"
        )

    @field_validator("water_density")
    @classmethod
    def validate_water_density(cls, v: float) -> float:
        """Reject values outside the physical range for water (500–1100 kg/m³).

        Fresh water ≈ 1000 kg/m³, seawater ≈ 1025 kg/m³, brines up to ~1100 kg/m³.
        Values below 500 are likely unit errors (e.g. t/m³ stored as kg/m³).
        Values above 1100 are outside any engineering water context.
        """
        if not (500.0 <= v <= 1100.0):
            raise ValueError(
                f"water_density={v} kg/m³ is outside the physical range [500, 1100]. "
                f"Did you supply t/m³ instead of kg/m³? "
                f"Fresh water = 1000, seawater ≈ 1025."
            )
        return v


class FrequencyRangeSpec(BaseModel):
    """Frequency range specification (alternative to explicit values)."""

    start: float = Field(..., gt=0)
    end: float = Field(..., gt=0)
    count: int = Field(..., gt=0)
    distribution: FrequencyDistribution = Field(
        FrequencyDistribution.LINEAR,
    )


class FrequencySpec(BaseModel):
    """Frequency/period specification for the analysis."""

    input_type: FrequencyInputType = Field(
        FrequencyInputType.FREQUENCY,
        description="Whether values are periods (s) or frequencies (rad/s)",
    )
    values: Optional[list[float]] = Field(
        None,
        description="Explicit list of frequency/period values",
    )
    range: Optional[FrequencyRangeSpec] = Field(
        None,
        description="Range specification (alternative to explicit values)",
    )

    @model_validator(mode="after")
    def check_values_or_range(self) -> FrequencySpec:
        if self.values is None and self.range is None:
            raise ValueError(
                "Either 'values' or 'range' must be specified for frequencies"
            )
        if self.values is not None and self.range is not None:
            raise ValueError(
                "Specify either 'values' or 'range', not both"
            )
        return self

    def to_frequencies_rad_s(self) -> list[float]:
        """Convert to frequencies in rad/s regardless of input type."""
        if self.values is not None:
            raw = self.values
        else:
            r = self.range
            if r.distribution == FrequencyDistribution.LINEAR:
                raw = list(np.linspace(r.start, r.end, r.count))
            else:
                raw = list(
                    np.logspace(np.log10(r.start), np.log10(r.end), r.count)
                )

        invalid = [value for value in raw if not np.isfinite(value) or value <= 0]
        if invalid:
            raise ValueError(
                "Frequency/period values must be finite and greater than zero; "
                f"received {invalid!r}"
            )

        if self.input_type == FrequencyInputType.PERIOD:
            return [2.0 * np.pi / t for t in raw]
        return list(raw)

    def to_periods_s(self) -> list[float]:
        """Convert to periods in seconds regardless of input type."""
        freqs = self.to_frequencies_rad_s()
        return [2.0 * np.pi / w for w in freqs if w > 0]


class HeadingRangeSpec(BaseModel):
    """Heading range specification."""

    start: float = Field(default=0.0)
    end: float = Field(default=180.0)
    increment: float = Field(default=15.0, gt=0)


class WaveHeadingSpec(BaseModel):
    """Wave heading specification."""

    values: Optional[list[float]] = Field(
        None,
        description="Explicit list of headings in degrees",
    )
    range: Optional[HeadingRangeSpec] = Field(
        None,
        description="Range specification",
    )
    symmetry: bool = Field(
        default=False,
        description=(
            "Exploit heading symmetry (0-180 mirrored to 180-360)"
        ),
    )

    @model_validator(mode="after")
    def check_values_or_range(self) -> WaveHeadingSpec:
        if self.values is None and self.range is None:
            raise ValueError(
                "Either 'values' or 'range' must be specified "
                "for wave headings"
            )
        if self.values is not None and self.range is not None:
            raise ValueError(
                "Specify either 'values' or 'range', not both"
            )
        return self

    def to_heading_list(self) -> list[float]:
        """Get headings as a flat list in degrees."""
        if self.values is not None:
            return self.values
        r = self.range
        headings: list[float] = []
        h = r.start
        while h <= r.end + 1e-9:
            headings.append(round(h, 6))
            h += r.increment
        return headings


class IrregularFrequencyMethod(str, Enum):
    """Irregular frequency removal method (#501).

    - ``none``: no removal (legacy ``remove_irregular_frequencies: false``)
    - ``interior_panels``: interior surface panels, triangulation method
      (legacy ``remove_irregular_frequencies: true`` and the default)
    - ``control_surface``: removal via the body's control surface mesh
    """

    NONE = "none"
    INTERIOR_PANELS = "interior_panels"
    CONTROL_SURFACE = "control_surface"


class QTFOptions(BaseModel):
    """Quadratic Transfer Function configuration (#501).

    Supersedes the flat ``qtf_calculation`` / ``qtf_min_frequency`` /
    ``qtf_max_frequency`` fields on :class:`SolverOptions` (which remain as
    deprecated aliases). Resolution goes through
    :meth:`SolverOptions.resolved_qtf` — the single source of truth.
    """

    enabled: bool = False
    # int literals match today's emitted YAML tokens (0 / 180), keeping the
    # byte-identity gate honest (plan r2, M1 fix).
    min_crossing_angle: int = 0
    max_crossing_angle: int = 180
    min_frequency: Optional[float] = Field(
        None, description="Minimum QTF frequency (rad/s)"
    )
    max_frequency: Optional[float] = Field(
        None, description="Maximum QTF frequency (rad/s)"
    )
    # Pass-through of OrcaWave's own QTFCalculationMethod vocabulary — see
    # OrcaWave User Manual, "QTF calculation method" (Direct / Indirect /
    # Both); no translation layer (plan r2, L1 fix).
    load_calculation_method: Literal["Direct", "Indirect", "Both"] = "Both"


class SolverOptions(BaseModel):
    """Solver-specific options."""

    solve_type: Literal[
        "potential_only",
        "potential_and_source",
        "mean_drift",
        "diagonal_qtf",
        "full_qtf",
    ] = Field(
        default="potential_and_source",
        description=(
            "OrcaWave solve type. Options: "
            "'potential_only', 'potential_and_source', 'mean_drift', "
            "'diagonal_qtf', 'full_qtf'"
        ),
    )
    irregular_frequency_method: IrregularFrequencyMethod = Field(
        default=IrregularFrequencyMethod.INTERIOR_PANELS,
        description=(
            "Irregular frequency removal method: none | interior_panels | "
            "control_surface (#501)"
        ),
    )
    remove_irregular_frequencies: Optional[bool] = Field(
        default=None,
        # excluded from dumps: after validation this is a derived value;
        # serializing it alongside irregular_frequency_method would trip the
        # mutual-exclusion guard on reload.
        exclude=True,
        description=(
            "DEPRECATED legacy alias - use irregular_frequency_method. "
            "True -> interior_panels, False -> none. After validation this "
            "field is normalized to the effective boolean for legacy readers."
        ),
    )
    qtf: Optional[QTFOptions] = Field(
        default=None,
        description="QTF configuration (#501); supersedes flat qtf_* fields",
    )
    qtf_calculation: Optional[bool] = Field(
        default=None,
        # excluded from dumps for the same reason as
        # remove_irregular_frequencies above.
        exclude=True,
        description=(
            "DEPRECATED legacy alias - use qtf.enabled. After validation "
            "this field is normalized to the effective boolean for legacy "
            "readers."
        ),
    )
    load_rao_method: LoadRAOMethod = Field(
        default=LoadRAOMethod.BOTH,
        description="Load RAO calculation method",
    )
    precision: SolverPrecision = Field(
        default=SolverPrecision.DOUBLE,
        description="Numerical precision",
    )
    preferred_load_rao_method: Optional[str] = Field(
        None,
        description=(
            "Preferred Load RAO method when load_rao_method='both'. "
            "Options: 'haskind', 'diffraction'. Default: auto (Haskind)."
        ),
    )
    divide_non_planar_panels: bool = Field(
        default=True,
        description="Divide non-planar panels into triangles",
    )
    output_ah1: bool = False
    """Generate .AH1 ASCII hydrodynamic database file (OPTIONS AHD1).
    Alternative to binary .HYD — produces readable text output with
    added mass, damping, and force RAO data."""
    qtf_min_frequency: Optional[float] = Field(
        None,
        description="Minimum QTF frequency (rad/s)",
    )
    qtf_max_frequency: Optional[float] = Field(
        None,
        description="Maximum QTF frequency (rad/s)",
    )

    @model_validator(mode="after")
    def _migrate_irregular_frequency_fields(self) -> "SolverOptions":
        """Legacy bool -> enum migration with mutual exclusion (#501).

        Unset-both-fields keeps today's default (interior_panels) with no
        warning (plan r2, H1 fix). After migration the legacy field is
        normalized to the effective boolean so existing readers
        (AQWA backend, comparison tooling) keep working.
        """
        legacy_set = "remove_irregular_frequencies" in self.model_fields_set
        method_set = "irregular_frequency_method" in self.model_fields_set

        if legacy_set:
            if method_set:
                raise ValueError(
                    "Set either remove_irregular_frequencies (deprecated) or "
                    "irregular_frequency_method, not both."
                )
            warnings.warn(
                "remove_irregular_frequencies is deprecated; use "
                "irregular_frequency_method ('interior_panels' | 'none' | "
                "'control_surface').",
                DeprecationWarning,
                stacklevel=2,
            )
            self.irregular_frequency_method = (
                IrregularFrequencyMethod.INTERIOR_PANELS
                if self.remove_irregular_frequencies
                else IrregularFrequencyMethod.NONE
            )
        # Normalize the legacy field for downstream legacy readers, then
        # drop it from fields_set so revalidation of this instance (nested
        # model validation, model_copy) does not re-trip mutual exclusion.
        self.remove_irregular_frequencies = (
            self.irregular_frequency_method
            is not IrregularFrequencyMethod.NONE
        )
        self.__pydantic_fields_set__.discard("remove_irregular_frequencies")
        return self

    @model_validator(mode="after")
    def _migrate_qtf_fields(self) -> "SolverOptions":
        """Flat qtf_* -> nested QTFOptions migration with mutual exclusion."""
        flat_set = [
            name
            for name in ("qtf_calculation", "qtf_min_frequency", "qtf_max_frequency")
            if name in self.model_fields_set
        ]
        if self.qtf is not None and flat_set:
            raise ValueError(
                f"Set either the nested 'qtf' options or the deprecated flat "
                f"fields ({', '.join(flat_set)}), not both."
            )
        if flat_set:
            warnings.warn(
                f"Flat QTF fields ({', '.join(flat_set)}) are deprecated; "
                "use the nested 'qtf' options.",
                DeprecationWarning,
                stacklevel=2,
            )
        # Normalize the legacy flag for downstream legacy readers; drop it
        # from fields_set so revalidation does not re-trip mutual exclusion.
        if self.qtf_calculation is None:
            self.qtf_calculation = (
                self.qtf.enabled if self.qtf is not None else False
            )
        self.__pydantic_fields_set__.discard("qtf_calculation")
        return self

    @model_validator(mode="after")
    def _validate_qtf_solve_type(self) -> "SolverOptions":
        """Explicit nested QTF opt-in requires a QTF solve type (plan C1).

        Deliberately scoped to the NEW nested ``qtf`` model only: the legacy
        flat ``qtf_calculation: true`` with a non-QTF solve type is a
        long-standing supported combination (the L03 ship benchmark fixture
        uses it), so the strict gate binds only where users opted into the
        new schema.
        """
        if (
            self.qtf is not None
            and self.qtf.enabled
            and self.solve_type not in ("diagonal_qtf", "full_qtf")
        ):
            raise ValueError(
                f"QTFOptions.enabled=True requires solve_type 'diagonal_qtf' "
                f"or 'full_qtf'; got {self.solve_type!r}. Set a QTF solve "
                f"type or leave QTF disabled."
            )
        return self

    @model_serializer(mode="wrap")
    def _serialize_preserving_legacy_qtf(self, handler: Any) -> dict[str, Any]:
        """Keep legacy-flat QTF enablement in dumps (#623/#624).

        ``qtf_calculation`` is ``Field(exclude=True)`` (dumping it alongside a
        nested ``qtf`` would trip the mutual-exclusion guard on reload), but
        when ONLY the legacy flat path enabled QTF there is no nested ``qtf``
        to serialize — without re-adding the flag here, ``to_yaml`` would drop
        QTF enablement entirely and a reloaded spec would run with QTF off.
        The nested ``qtf`` model, when present, already round-trips on its own.
        """
        data = handler(self)
        if self.qtf is None and self.qtf_calculation:
            data["qtf_calculation"] = True
        return data

    def resolved_qtf(self) -> QTFOptions:
        """Single source of truth for QTF settings (#501).

        Returns the nested options when present, else a QTFOptions built
        from the deprecated flat fields (today's defaults when unset).
        """
        if self.qtf is not None:
            return self.qtf
        return QTFOptions(
            enabled=bool(self.qtf_calculation),
            min_frequency=self.qtf_min_frequency,
            max_frequency=self.qtf_max_frequency,
        )


class FieldPointSpec(BaseModel):
    """Named group of field points for wave elevation/pressure output (#501).

    OrcaWave likely honors only the global
    ``OutputSpec.detect_field_points_inside_bodies`` switch; the per-group
    flag is informational and validated at schema level only.
    """

    name: str
    points: list[tuple[float, float, float]] = Field(
        ...,
        min_length=1,
        description="Field point coordinates [(x, y, z), ...]",
    )
    detect_inside_bodies: bool = True


class OutputSpec(BaseModel):
    """Output configuration."""

    formats: list[OutputFormat] = Field(
        default=[OutputFormat.CSV],
        description="Output file formats",
    )
    field_points: list[FieldPointSpec] = Field(
        default=[],
        description="Field point groups for wave elevation/pressure (#501)",
    )
    detect_field_points_inside_bodies: bool = Field(
        default=True,
        description=(
            "Skip field points that fall inside body meshes (emits "
            "DetectAndSkipFieldPointsInsideBodies; #501)"
        ),
    )
    components: list[OutputComponent] = Field(
        default=[
            OutputComponent.RAOS,
            OutputComponent.ADDED_MASS,
            OutputComponent.DAMPING,
        ],
        description="Result components to include",
    )
    directory: Optional[str] = Field(
        None,
        description="Output directory (defaults to ./results/)",
    )


class MetadataSpec(BaseModel):
    """Project metadata."""

    project: Optional[str] = Field(
        None,
        description="Project name or code",
    )
    author: Optional[str] = Field(
        None,
        description="Author or organization",
    )
    date: Optional[str] = Field(
        None,
        description="Analysis date (ISO 8601)",
    )
    description: Optional[str] = Field(
        None,
        description="Analysis description",
    )
    tags: list[str] = Field(
        default=[],
        description="Tags for categorization",
    )


class DampingLidSpec(BaseModel):
    """Resonance damping lid specification (for moonpool bodies)."""

    mesh_file: str = Field(
        ...,
        description="Path to damping lid mesh file (relative to spec.yml)",
    )
    mesh_format: str = Field(
        default="gdf",
        description="Mesh file format (gdf, dat, etc.)",
    )
    length_units: str = Field(
        default="m",
        description="Length units of the lid mesh file",
    )
    damping_factor: float = Field(
        ...,
        gt=0,
        description="Damping factor epsilon for the lid",
    )


class FreeSurfaceZoneSpec(BaseModel):
    """Free surface zone specification for QTF calculations."""

    type: str = Field(
        default="mesh",
        description="'mesh' for explicit mesh file, 'auto' for automatic",
    )
    mesh_file: Optional[str] = Field(
        None,
        description="Path to free surface zone mesh file (.fdf)",
    )
    mesh_format: str = Field(
        default="fdf",
        description="Mesh format (typically 'fdf' for Wamit)",
    )
    length_units: str = Field(
        default="m",
        description="Length units of the zone mesh",
    )
    inner_radius: Optional[float] = Field(
        None,
        description="Inner radius of panelled zone",
    )


class ControlSurfaceSpec(BaseModel):
    """Control surface for irregular frequency removal."""

    type: str = Field(
        default="auto",
        description=(
            "'auto' for automatic generation, or 'mesh' for explicit "
            "mesh file"
        ),
    )
    mesh_file: Optional[str] = Field(
        None,
        description=(
            "Path to control surface mesh file (when type='mesh')"
        ),
    )
    mesh_format: Optional[str] = Field(
        None,
        description=(
            "Control surface mesh format ('csf', 'gdf', 'dat', 'stl'). "
            "None = auto-detect from the mesh_file extension. Native "
            "OrcaWave pairs .gdf with 'Wamit gdf' and .csf with 'Wamit csf'."
        ),
    )
    panel_size: Optional[float] = Field(
        None,
        description="Panel size for auto-generated control surface",
    )
    separation: Optional[float] = Field(
        None,
        description="Separation distance from body surface",
    )

    @model_validator(mode="after")
    def check_control_surface_defined(self) -> "ControlSurfaceSpec":
        """A control surface must be generatable: mesh file, or auto params.

        Without this, an auto control surface missing panel_size/separation
        (or type='mesh' without mesh_file) validated cleanly and was then
        silently dropped from the generated body section while
        QuadraticLoadControlSurface stayed enabled (#324).
        """
        if self.mesh_file is None:
            if self.type == "mesh":
                raise ValueError(
                    "control_surface type='mesh' requires mesh_file"
                )
            if self.panel_size is None or self.separation is None:
                raise ValueError(
                    "Auto-generated control surface requires panel_size and "
                    "separation (emitted as BodyControlSurfacePanelSize / "
                    "BodyControlSurfaceSeparationFromBody)"
                )
        return self


class MorisonElementSpec(BaseModel):
    """Morison element specification (for viscous drag on slender members)."""

    name: str
    position: list[float] = Field(description="Position [x, y, z]")
    attitude: list[float] = Field(
        default=[0, 0, 0],
        description="Attitude [rx, ry, rz] degrees",
    )
    length: float = Field(gt=0)
    diameter: float = Field(gt=0)
    cd: float = Field(default=1.0, description="Drag coefficient")
    cm: float = Field(default=2.0, description="Inertia coefficient")


class BodySpec(BaseModel):
    """Single body in the analysis (extends VesselSpec for multi-body)."""

    vessel: VesselSpec
    position: list[float] = Field(
        default=[0.0, 0.0, 0.0],
        description="Body position [x, y, z]",
    )
    attitude: list[float] = Field(
        default=[0.0, 0.0, 0.0],
        description="Body attitude [rx, ry, rz] degrees",
    )
    control_surface: Optional[ControlSurfaceSpec] = Field(
        None,
        description="Control surface spec",
    )
    morison_elements: list[MorisonElementSpec] = Field(
        default=[],
        description="Morison drag elements",
    )
    connection_parent: Optional[str] = Field(
        None,
        description=(
            "Parent body name for constraints, or None for free"
        ),
    )

    def resolve_control_surface(self) -> Optional["ControlSurfaceSpec"]:
        """Resolve this body's control surface (#609).

        Precedence: a body-level ``BodySpec.control_surface`` overrides the
        vessel-level ``VesselSpec.control_surface``. This is the single
        resolution rule shared by backend generation, mesh packaging, and
        validation — multi-body specs with per-body control surfaces must
        never be silently ignored.
        """
        if self.control_surface is not None:
            return self.control_surface
        return self.vessel.control_surface


# ---------------------------------------------------------------------------
# Top-level spec
# ---------------------------------------------------------------------------


class DiffractionSpec(BaseModel):
    """Top-level canonical specification for diffraction analysis.

    This is the schema for spec.yml files. It captures all information
    needed to generate solver-specific inputs for AQWA, OrcaWave, or
    other BEM solvers.

    Example::

        spec = DiffractionSpec.from_yaml("analysis.yml")
        bodies = spec.get_bodies()
        freqs = spec.frequencies.to_frequencies_rad_s()
    """

    version: str = Field(
        default="1.0",
        description="Schema version",
    )
    analysis_type: AnalysisType = Field(
        default=AnalysisType.DIFFRACTION,
        description="Type of analysis",
    )

    # Single body (simple case)
    vessel: Optional[VesselSpec] = Field(
        None,
        description="Single vessel (for single-body analysis)",
    )
    # Multi-body
    bodies: Optional[list[BodySpec]] = Field(
        None,
        description="Multiple bodies (for multi-body analysis)",
    )

    environment: EnvironmentSpec
    frequencies: FrequencySpec
    wave_headings: WaveHeadingSpec
    solver_options: SolverOptions = Field(default_factory=SolverOptions)
    outputs: OutputSpec = Field(default_factory=OutputSpec)
    metadata: MetadataSpec = Field(default_factory=MetadataSpec)
    damping_lid: Optional[DampingLidSpec] = Field(
        None,
        description="Resonance damping lid for moonpool bodies",
    )
    free_surface_zone: Optional[FreeSurfaceZoneSpec] = Field(
        None,
        description="Free surface zone for QTF calculations",
    )

    @model_validator(mode="after")
    def check_vessel_or_bodies(self) -> DiffractionSpec:
        if self.vessel is None and self.bodies is None:
            raise ValueError(
                "Either 'vessel' (single body) or 'bodies' (multi-body) "
                "must be specified"
            )
        if self.vessel is not None and self.bodies is not None:
            raise ValueError(
                "Specify either 'vessel' or 'bodies', not both"
            )
        return self

    @model_validator(mode="after")
    def _validate_control_surface_method(self) -> "DiffractionSpec":
        """irregular_frequency_method=control_surface needs a CS mesh (#501)."""
        if (
            self.solver_options is not None
            and self.solver_options.irregular_frequency_method
            is IrregularFrequencyMethod.CONTROL_SURFACE
        ):
            missing = [
                body.vessel.name
                for body in self.get_bodies()
                if body.resolve_control_surface() is None
            ]
            if missing:
                raise ValueError(
                    "irregular_frequency_method='control_surface' requires a "
                    f"control surface on every body; missing on: {missing}"
                )
        return self

    def get_bodies(self) -> list[BodySpec]:
        """Get all bodies as a list, wrapping single vessel if needed."""
        if self.bodies is not None:
            return self.bodies
        return [BodySpec(vessel=self.vessel)]

    @classmethod
    def from_yaml(cls, path: str | Path) -> DiffractionSpec:
        """Load spec from a YAML file."""
        import yaml

        with open(path) as f:
            data = yaml.safe_load(f)
        return cls.model_validate(data)

    def to_yaml(self, path: str | Path) -> Path:
        """Save spec to a YAML file."""
        import yaml

        path = Path(path)
        data = self.model_dump(mode="json", exclude_none=True)
        with open(path, "w") as f:
            yaml.dump(data, f, default_flow_style=False, sort_keys=False)
        return path

    def to_json_schema(self) -> dict:
        """Export JSON Schema for external validation."""
        return self.model_json_schema()


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    # Enumerations
    "AnalysisType",
    "MeshFormatType",
    "SymmetryType",
    "FrequencyInputType",
    "FrequencyDistribution",
    "LoadRAOMethod",
    "SolverPrecision",
    "OutputFormat",
    "OutputComponent",
    # Sub-models
    "VesselGeometry",
    "VesselInertia",
    "VesselSpec",
    "EnvironmentSpec",
    "FrequencyRangeSpec",
    "FrequencySpec",
    "HeadingRangeSpec",
    "WaveHeadingSpec",
    "IrregularFrequencyMethod",
    "QTFOptions",
    "FieldPointSpec",
    "SolverOptions",
    "OutputSpec",
    "MetadataSpec",
    "ControlSurfaceSpec",
    "MorisonElementSpec",
    "BodySpec",
    # Top-level spec
    "DiffractionSpec",
]
