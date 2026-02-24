# WRK-032 Data Model Design

> Phase 1 artifact | Generated: 2026-02-03 | Rev: 2 (post iteration-1 review)

## Design Principles

1. **Extend, don't replace** — new models added alongside existing ones
2. **Backward compatible** — single-run `ProjectInputSpec` unchanged
3. **Campaign is optional** — `CampaignSpec` wraps `ProjectInputSpec` with a parameter matrix
4. **No SectionRegistry facade** — use `BuilderRegistry` directly (avoid sync burden)
5. **Variable substitution with YAML validation** — `${param}` tokens resolved, then parsed as YAML to catch structural errors
6. **Use `model_copy(deep=True)`** — not `copy.deepcopy()` for Pydantic v2 spec duplication

## New Models

### 1. `RollerArrangement` (extends roller parametrisation)

Replaces the minimal `Rollers` model for installation analysis.

```python
class RollerType(str, Enum):
    V_ROLLER = "v_roller"
    FLAT = "flat"
    CRADLE = "cradle"

class RollerStation(BaseModel):
    """Single roller station along the installation route."""
    position: list[float] = Field(..., min_length=3, max_length=3,
                                   description="[x, y, z] position (m)")
    support_count: int = Field(default=4, ge=1, le=20, description="Supports per station")
    support_type: str = Field(default="Support type2", description="Support type name")
    v_angle: float | None = Field(default=None, ge=30, le=180,
                                   description="V-angle (deg), required for v_roller only")
    diameter: float = Field(default=0.5, gt=0.05, le=5.0,
                            description="Roller drum diameter (m)")
    friction_coefficient: float = Field(default=0.1, ge=0, le=1,
                                         description="Roller friction coefficient")
    height_offset: float = Field(default=0.0, description="Vertical offset from reference (m)")

    @field_validator("position")
    @classmethod
    def validate_position(cls, v: list[float]) -> list[float]:
        if len(v) != 3:
            raise ValueError(f"Position must have exactly 3 components, got {len(v)}")
        return v

class RollerArrangement(BaseModel):
    """Parametric roller arrangement for pipeline support.

    Note: uniform() factory spaces stations along the X-axis only.
    This is valid for straight-line installation routes. For curved
    routes, define station positions explicitly.
    """
    type: RollerType = RollerType.V_ROLLER
    stations: list[RollerStation] = Field(..., min_length=1,
                                          description="Ordered roller stations")
    spacing_pattern: str | None = Field(default=None,
                                         description="'uniform:<spacing_m>' or None for custom")

    @model_validator(mode="after")
    def validate_v_angle_for_type(self) -> "RollerArrangement":
        """Enforce v_angle is set for V_ROLLER type, default to 120 if missing."""
        if self.type == RollerType.V_ROLLER:
            for station in self.stations:
                if station.v_angle is None:
                    station.v_angle = 120.0  # Default V-angle
        return self

    @classmethod
    def uniform(cls, type: RollerType, count: int, first_position: list[float],
                spacing: float, **station_kwargs) -> "RollerArrangement":
        """Create evenly-spaced stations along the X-axis (straight routes only)."""
        stations = []
        for i in range(count):
            pos = [first_position[0] + i * spacing, first_position[1], first_position[2]]
            stations.append(RollerStation(position=pos, **station_kwargs))
        return cls(type=type, stations=stations, spacing_pattern=f"uniform:{spacing}")
```

**Changes from Rev 1**:
- `v_angle` now `float | None` — only required for V_ROLLER (Codex #3, #8)
- All fields use `Field()` constraints matching codebase convention (Codex #4, #6, #7)
- `position` has `field_validator` matching existing pattern (Codex #6)
- `uniform()` docstring states X-axis limitation (Gemini A4, #14)

### 2. `CampaignMatrix` and variations

```python
class EnvironmentVariation(BaseModel):
    """Named environment configuration for campaign matrix.
    Overrides: spec.environment.waves, spec.environment.current, spec.environment.wind
    Does NOT override: spec.environment.water (use water_depths), spec.environment.seabed (use soils)
    """
    name: str = Field(..., min_length=1)
    waves: Waves
    current: Current
    wind: Wind

class SoilVariation(BaseModel):
    """Named soil/seabed configuration for campaign matrix.
    Overrides: spec.environment.seabed.stiffness, spec.environment.seabed.friction_coefficient,
               and optionally spec.environment.seabed.slope
    """
    name: str = Field(..., min_length=1)
    stiffness: SeabedStiffness
    friction_coefficient: float = Field(..., ge=0, le=1.5)
    slope: float | None = Field(default=None, ge=-45, le=45,
                                 description="Seabed slope override (deg). None = keep base value")

class CampaignMatrix(BaseModel):
    """Defines the parametric variation space for batch generation."""
    water_depths: list[float] = Field(..., min_length=1, description="Water depths (m)")
    route_lengths: list[float] | None = Field(default=None,
        description="Route lengths (m). Adjusts last pipeline segment only.")
    tensions: list[float] | None = Field(default=None,
        description="Tension values (kN). S-lay only — maps to equipment.tensioner.tension_value")
    environments: list[EnvironmentVariation] | None = None
    soils: list[SoilVariation] | None = None

    @field_validator("water_depths")
    @classmethod
    def validate_water_depths(cls, v: list[float]) -> list[float]:
        for depth in v:
            if depth <= 0:
                raise ValueError(f"Water depth must be positive, got {depth}")
        return v

    def combinations(self) -> list[dict[str, float | str]]:
        """Generate cartesian product of all non-None parameters.

        Returns list of dicts with keys:
          - "water_depth": float (always present)
          - "route_length": float (if route_lengths specified)
          - "tension": float (if tensions specified)
          - "environment": str (name, if environments specified)
          - "soil": str (name, if soils specified)
        """
        ...
```

**Changes from Rev 1**:
- Removed unused `ParameterRange` model (Codex #17)
- `SoilVariation` now includes optional `slope` field (Gemini #13)
- Override targets documented as docstrings (Codex #1, #4, #5; Gemini #16)
- `route_lengths` description clarifies "adjusts last segment only" (Codex #2)
- `combinations()` return type documented with exact key names (Codex #12)

### 3. `CampaignSpec` (top-level campaign model)

```python
class CampaignSpec(BaseModel):
    """Top-level model for parametric installation campaign."""
    schema_version: str = Field(default="1.0", description="Schema version for migration")
    base: ProjectInputSpec
    campaign: CampaignMatrix
    sections: list[InstallationSection] = Field(default_factory=list,
        description="Optional section overrides (custom templates, variable overrides)")
    output_naming: str = Field(
        default="{base_name}_wd{water_depth}m_{environment}",
        description="Output directory naming template")
    max_runs: int | None = Field(default=None, ge=1,
        description="Maximum runs to generate. None = unlimited.")

    @model_validator(mode="after")
    def validate_installation_type_constraints(self) -> "CampaignSpec":
        """Cross-validate campaign parameters against installation type."""
        if self.campaign.tensions and not self.base.is_s_lay():
            raise ValueError(
                "tensions can only be specified for S-lay models "
                "(base spec must define equipment.vessel)"
            )
        return self

    @model_validator(mode="after")
    def validate_output_naming_coverage(self) -> "CampaignSpec":
        """Warn if varying parameters are not in output_naming template."""
        varying = []
        if self.campaign.water_depths: varying.append("water_depth")
        if self.campaign.route_lengths: varying.append("route_length")
        if self.campaign.tensions: varying.append("tension")
        if self.campaign.environments: varying.append("environment")
        if self.campaign.soils: varying.append("soil")
        for param in varying:
            if f"{{{param}}}" not in self.output_naming:
                raise ValueError(
                    f"Parameter '{param}' varies in campaign but is missing from "
                    f"output_naming template: '{self.output_naming}'"
                )
        return self

    def generate_run_specs(self) -> Iterator[tuple[str, ProjectInputSpec]]:
        """Yield (name, modified_spec) pairs. Streaming — one at a time.

        Uses model_copy(deep=True) for Pydantic v2 optimized copy.
        """
        for combo in self.campaign.combinations():
            spec = self.base.model_copy(deep=True)
            _apply_overrides(spec, combo, self.campaign)
            name = self.output_naming.format(
                base_name=spec.metadata.name, **combo
            )
            yield (name, spec)
```

**Changes from Rev 1**:
- Added `schema_version` field (Gemini #9)
- Added `max_runs` field (Codex #13; Gemini risk)
- Cross-validates `tensions` vs S-lay (Gemini #5)
- Validates output naming covers all varying params (Gemini #18)
- `generate_run_specs()` is now a generator/iterator (Gemini #12) using `model_copy(deep=True)` (Codex #10; Gemini A1, #4)

### 4. `InstallationSection` (simplified — no SectionRegistry)

```python
class InstallationSection(BaseModel):
    """Override for a specific include-file section.

    template_path resolved relative to the campaign YAML file's directory.
    """
    builder_file: str = Field(...,
        description="Builder output filename to override (e.g., '08_buoys.yml'). "
                    "Must match a BuilderRegistry entry.")
    template: str | None = Field(default=None,
        description="Path to template file (resolved relative to campaign YAML directory)")
    variables: dict[str, Any] = Field(default_factory=dict)
    enabled: bool = Field(default=True)
```

**Changes from Rev 1**:
- Removed `SectionType` enum and `SectionRegistry` facade (Gemini #3)
- Uses `builder_file` string matching `BuilderRegistry` entries directly
- Template path resolution rule documented (Codex #11; Gemini D6)

### 5. Extended `Equipment` model — unchanged from Rev 1

Same as before. Note: the `resolve_roller_compat` validator only runs during construction — it does not affect YAML serialization of user-provided data because the `rollers` field remains populated.

### Parameter Override Mapping

| Campaign Parameter | Target Field on `ProjectInputSpec` | Notes |
|--------------------|-------------------------------------|-------|
| `water_depth` | `environment.water.depth` | |
| `route_length` | Last `pipeline.segments[-1].length` adjusted | Total = sum(segments); delta applied to last segment |
| `tension` | `equipment.tensioner.tension_value` | S-lay only; validated |
| `environment` (name) | `environment.waves`, `environment.current`, `environment.wind` | Replaces all three from `EnvironmentVariation` |
| `soil` (name) | `environment.seabed.stiffness`, `environment.seabed.friction_coefficient`, optionally `.slope` | From `SoilVariation` |

**Route length adjustment algorithm**: The total pipeline length is the sum of all segment lengths. When `route_length` differs from the total, the delta (`route_length - total`) is applied to the **last segment only**. Rationale: in most installations, the initial segments near the vessel/shore have fixed lengths; only the trailing seabed section varies with route distance. If the last segment's adjusted length would be <= 0, a `ValueError` is raised.

**Current profile depth-scaling**: When `water_depth` changes, current profile depth values are scaled proportionally: `new_depth_i = old_depth_i * (new_water_depth / old_water_depth)`. This preserves the relative profile shape.

## Variable Substitution (revised)

```python
class VariableResolver:
    """Resolves ${variable_name} tokens in template content.

    After substitution, parses result as YAML to validate structure.
    Supports format specifiers: ${water_depth:.0f} for integer formatting.
    """

    @staticmethod
    def resolve(template_content: str, variables: dict[str, Any]) -> str:
        import re
        import yaml

        def replacer(match: re.Match) -> str:
            key = match.group(1)
            fmt = match.group(2)  # Optional format spec after :
            if key not in variables:
                raise ValueError(f"Unresolved variable: ${{{key}}}")
            value = variables[key]
            if fmt:
                return f"{value:{fmt}}"
            # Auto-format: whole-number floats as int
            if isinstance(value, float) and value == int(value):
                return str(int(value))
            return str(value)

        # Pattern: ${key} or ${key:format_spec}
        result = re.sub(r'\$\{(\w+)(?::([^}]+))?\}', replacer, template_content)

        # Validate the result is valid YAML
        try:
            yaml.safe_load(result)
        except yaml.YAMLError as e:
            raise ValueError(
                f"Variable substitution produced invalid YAML: {e}"
            ) from e

        return result
```

**Changes from Rev 1**:
- Regex-based substitution with format specifier support (Gemini #8)
- Post-substitution YAML validation (Codex #24; Gemini A5, #15)
- Auto-format whole-number floats as int (Gemini #8)

## Schema Relationships (updated)

```
CampaignSpec (new, optional top-level)
├── schema_version: str (NEW)
├── max_runs: int | None (NEW)
├── base: ProjectInputSpec (existing, unchanged)
│   ├── metadata: Metadata
│   ├── environment: Environment
│   │   ├── water: Water        ← overridden by campaign.water_depths
│   │   ├── seabed: Seabed      ← overridden by campaign.soils
│   │   ├── waves: Waves        ← overridden by campaign.environments
│   │   ├── current: Current    ← overridden by campaign.environments
│   │   └── wind: Wind          ← overridden by campaign.environments
│   ├── pipeline: Pipeline
│   │   └── segments[-1].length ← adjusted by campaign.route_lengths
│   ├── equipment: Equipment
│   │   ├── rollers: Rollers (legacy, auto-converts)
│   │   ├── roller_arrangement: RollerArrangement (NEW)
│   │   │   └── stations: list[RollerStation] (NEW)
│   │   ├── tugs: Tugs
│   │   ├── buoyancy_modules: BuoyancyModules
│   │   ├── vessel: Vessel (S-lay)
│   │   ├── stinger: Stinger (S-lay)
│   │   └── tensioner: Tensioner (S-lay)
│   │       └── tension_value   ← overridden by campaign.tensions
│   └── simulation: Simulation
├── campaign: CampaignMatrix (NEW)
│   ├── water_depths: list[float]
│   ├── route_lengths: list[float] | None
│   ├── tensions: list[float] | None
│   ├── environments: list[EnvironmentVariation] (NEW)
│   └── soils: list[SoilVariation] (NEW, now with optional slope)
├── sections: list[InstallationSection] (NEW, uses builder_file not SectionType)
└── output_naming: str (validated for parameter coverage)
```
