# Plan: CP arithmetic/unit fixes and quarantine of non-physical models (#2209)

> Issue: #2209 | Epic: #2206 | Owner decision: D7 (recorded 2026-09-25)
> Status: approved by owner instruction "continue with tasks" (2026-09-25); implementation in worktree `feat/cp-2209-arithmetic-quarantine`.

## Fix now
| Location | Defect | Fix | Test |
|---|---|---|---|
| `corrosion_rate.py:108-114` | Faraday factors 10× high | Fe 0.00116 mm/yr per mA/m² (M=55.85, z=2, ρ=7870); recompute Al, Cu, Zn, cast iron from M/z/ρ | hand-derived Faraday test |
| `api_rp_1632.py:33` | Mg H-1 500 A·h/kg is per-pound | 1100 A·h/kg (practical, ≈50 % efficiency of 2200 theoretical) | value + life test |
| `pipeline_cp.py:195-226` | holiday voltage 20× low; misnamed parameter | delegate to `dnv_rp_f106.holiday_detection_voltage`; keep name with `DeprecationWarning`; parameter renamed `coating_thickness_mm` | 0.4 mm FBE ≈ 2 kV order; equality with F106 |
| `iccp_design.py:153-164` | rectifier silently caps at 120 V / 200 A | add `exceeds_standard_range: bool` and `units_required: int`; never silently cap | 300 A / 5 Ω case |

## Quarantine (raise unless `experimental=True`; removed from `__all__`)
`stray_current.assess_stray_current` and `design_drainage_bond` (DC/AC models), `fuel_system_cp.check_protection`, `corrosion_rate.galvanic_corrosion`, `iccp_design.anode_bed_design` life figures (mass-based life for MMO/Pt). Each raises `ExperimentalModelError` with the reason and the standard that a re-model must follow (EN 50162 Table 1; ISO 18086; NACE SP0169 §6; BS PD 6484).

## Verification
Existing tests for quarantined functions are converted to `pytest.raises` plus `experimental=True` smoke; new hand-derived tests for the four fixes; `tests/cathodic_protection/` green; ruff/mypy clean on touched files.
