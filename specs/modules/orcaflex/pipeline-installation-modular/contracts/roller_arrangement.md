# Contract: RollerArrangement Models

> API contract for parametric roller arrangement
> Rev: 2 (post iteration-3 — v_angle type corrected, get_support_geometry ownership clarified)

## Module

`digitalmodel.solvers.orcaflex.modular_generator.schema.equipment`

## Model: `RollerStation`

```python
class RollerStation(BaseModel):
    position: list[float]               # [x, y, z] in metres, length=3
    support_count: int                   # 1-20, default=4
    support_type: str                    # default="Support type2"
    v_angle: float | None               # V-angle (deg), default=None, range [30, 180] when set.
                                        # Required for V_ROLLER only — model validator defaults to 120.0
    diameter: float                      # Roller drum diameter in metres, default=0.5, range (0.05, 5.0]
    friction_coefficient: float          # default=0.1, range [0, 1]
    height_offset: float                 # Vertical offset from reference, default=0.0
```

### Validators
- `position` must have exactly 3 components
- `v_angle`: `None` allowed for flat/cradle types; range [30, 180] when set
- `RollerArrangement.validate_v_angle_for_type` auto-fills `v_angle=120.0` for V_ROLLER stations where `None`
- `friction_coefficient` range: 0.0-1.0
- `diameter` range: (0.05, 5.0] metres
- `support_count` range: [1, 20]

## Model: `RollerArrangement`

```python
class RollerArrangement(BaseModel):
    type: RollerType                     # v_roller | flat | cradle
    stations: list[RollerStation]        # min_length=1
    spacing_pattern: str | None          # "uniform:<spacing_m>" or None for custom
```

### Factory: `RollerArrangement.uniform()`

```python
@classmethod
def uniform(
    cls,
    type: RollerType,
    count: int,
    first_position: list[float],
    spacing: float,
    **station_kwargs
) -> "RollerArrangement"
```

Creates `count` evenly-spaced stations starting at `first_position` along the X-axis.

### Static Method: `BuoysBuilder.get_support_geometry(station, roller_type) -> list[list[float]]`

Returns support positions relative to station origin based on `roller_type` and `v_angle`.
Defined on `BuoysBuilder` (not on the schema model) to keep schema models data-only.

**For V-roller:**
- Supports arranged symmetrically about the vertical plane
- Half-angle = `v_angle / 2`
- Each support at radial distance = `diameter / 2` from pipe centreline
- Mirrored in pairs: `[+z, -z]`

**For flat:**
- Supports in a horizontal row at `height_offset` below pipe

**For cradle:**
- Supports arranged in a semicircular arc below pipe

## Enum: `RollerType`

```python
class RollerType(str, Enum):
    V_ROLLER = "v_roller"
    FLAT = "flat"
    CRADLE = "cradle"
```

## Integration with BuoysBuilder

`BuoysBuilder._build_roller()` currently generates a single roller buoy. The new `_build_roller_arrangement()` method:

1. Iterates `roller_arrangement.stations`
2. For each station, generates a 6D buoy with support geometry from `get_support_geometry()`
3. Names: `"Roller_1"`, `"Roller_2"`, etc.
4. Registers all roller buoy names in context

The legacy `_build_roller()` method is retained but delegates to `_build_roller_arrangement()` when `equipment.get_effective_rollers()` returns a `RollerArrangement`.
