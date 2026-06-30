# Plan: Tug analysis toolkit (epic #1193)

Approved 2026-06-30. Implements the engineering capability behind the tug
service one-pager. New domain `src/digitalmodel/tug/` + `tests/tug/`.

## Issues
- #1194 bollard-pull & escort-performance prediction → `bollard_pull.py`
- #1195 intact/dynamic stability + girting heeling-arm check → `stability_girting.py`
- #1196 fender-height optimization vs assisted-fleet envelope → `fendering.py`
- #1197 towline load & breaking-strength / class safety-factor → `towline.py`
- #1198 hybrid power sizing & fuel/emissions compliance → `emissions.py`

## Design
- House style: `# ABOUTME:` header, References block, dataclasses for
  inputs/results, pure functions with formula in docstring, result objects
  carrying pass/fail bools (matches `naval_architecture/floating_platform_stability.py`).
- Reuse `naval_architecture.floating_platform_stability.compute_gz_curve` and
  `compute_area_under_gz` for girting heeling-arm vs GZ (same math as wind heel).
- Shared constants in `tug/constants.py` (propulsion BP factors, class gear
  safety factors, fuel emission factors).

## Validation
- `tests/tug/` with known-value checks tied to brochure numbers:
  - ASD 4,000 kW → ~52–60 t BP; conventional lower.
  - girting equilibrium heel > deck-edge angle ⇒ fail.
  - towline required MBL ≥ 2.5× load (LR).
  - assist-speed load amplification 2–5× across 2–3 → 4.6 kn.
- Run: `PYTHONPATH=src .venv/bin/python -m pytest tests/tug/`.

## Process
worktree off origin/main → implement+test → adversarial cross-review (Route B/C)
→ push feature branch → PR (human merges).
