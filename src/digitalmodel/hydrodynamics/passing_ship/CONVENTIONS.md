# Passing Ship Module — Sign Conventions & Unit System

## Coordinate System

All formulations use a right-handed coordinate system centered at the moored vessel midship:

```
+x (forward/bow)
    ^
    |
    |  Moored Vessel
    +--------->  +y (starboard)
    |
    |
    v  (port)
```

## Variable Mapping

| Concept | Python var | VBA var | Definition |
|---------|-----------|---------|------------|
| Moored ship position (integration) | `eta` | `x1` | [-L1/2, +L1/2] from midship |
| Passing ship position (integration) | `xi` | `x2` | [-L2/2, +L2/2] from midship |
| Stagger distance | `x` | `Xi` | Longitudinal offset: passing midship - moored midship |
| Lateral separation | `y` | `Eta` | Centerline-to-centerline transverse distance (always positive) |
| Moored vessel length | `L` (first arg) | `Length_M` | Length between perpendiculars |
| Passing vessel length | `L` (context) | `Length_P` | Length between perpendiculars |

## Stagger Sign Convention

- `x > 0` (Xi > 0): Passing ship midship is **ahead** of moored ship midship
- `x = 0`: Ships are **abeam** (midships aligned)
- `x < 0` (Xi < 0): Passing ship midship is **behind** moored ship midship

## Force Sign Convention

| Force/Moment | Positive Direction | Physical Meaning |
|-------------|-------------------|------------------|
| Surge (Fx) | Forward (+x) | Pulled toward bow |
| Sway (Fy) | Starboard (+y) | Pushed to starboard |
| Yaw (Mz) | Clockwise (bow-to-starboard) | Bow turns to starboard |

### Key Properties at Special Positions

| Position | Surge | Sway | Yaw |
|----------|-------|------|-----|
| ξ = 0 (abeam) | ≈ 0 (antisymmetric) | Maximum (suction peak) | ≈ 0 (antisymmetric) |
| ξ = +L/4 | Non-zero | Decreasing | Non-zero |
| ξ → ∞ | → 0 | → 0 | → 0 |

## Kernel Functions

| Python | VBA | Role | Physics |
|--------|-----|------|---------|
| `f_kernel` | `funcF` / `funcFInt` | Velocity potential interaction | Used for sway force and yaw moment |
| `g_kernel` | `funcG` / `funcGInt` | Stream function interaction | Used for surge force |

## Sectional Area Functions

Both implementations use a parabolic distribution:
- `S(x) = A_max × (1 - 4x²/L²)` — maximum at midship, zero at ends
- Python `s1_function` returns **dimensionless** [0,1]; multiplied by area externally
- VBA `SecArea1` returns **dimensional** (includes area factor)

## Force Scaling Differences

| Force | Python coefficient | VBA coefficient | Note |
|-------|-------------------|-----------------|------|
| Surge | `-ρU²A` | `ρU² / (2π)` | Different sign/scaling — verify in benchmarks |
| Sway | `+ρU²A` | `ρU²η / π` | VBA multiplies by η outside integral |
| Yaw | `ρU²AL` | `ρU²η / π` | Different moment arm treatment |

**These scaling differences are the primary source of potential numerical mismatches** and must be verified in Phase 1d benchmarks.

## Unit System

| Quantity | Calculator (SI) | MathCAD Reference | Conversion |
|----------|----------------|-------------------|------------|
| Length | m | ft | × 0.3048 |
| Area | m² | ft² | × 0.09290 |
| Velocity | m/s | ft/s | × 0.3048 |
| Density | kg/m³ | slug/ft³ | × 515.379 |
| Force | N | lbf | × 4.44822 |
| Moment | N·m | lbf·ft | × 1.35582 |

### Unit Strategy

- **Calculator API**: Always SI (m, kg/m³, N)
- **Benchmark reference data**: Imperial (as published in Wang paper / MathCAD)
- **Tests**: Convert Imperial reference → SI before calling calculator, convert results back for comparison
- **PassingShipSpec schema**: SI (user-facing)

## OrcaFlex / AQWA Coordinate Mapping (Phase 3)

| This Module | OrcaFlex | AQWA | Notes |
|-------------|----------|------|-------|
| +x (fwd) | +x (fwd) | +x (fwd) | Aligned |
| +y (stbd) | +y (stbd) | +y (port) | **AQWA FLIPPED** |
| +yaw (CW) | +Rz (CW) | +Rz (CCW) | **AQWA FLIPPED** |

When applying forces to AQWA: negate Fy and Mz.
