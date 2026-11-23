# Inspection Planning — Remaining Life & Interval

Condition-based (thickness/corrosion) inspection planning for fixed equipment — pressure vessels, piping, and tanks — per the API 510 / 570 / 653 approach.

```
corrosion_rate = (t_previous − t_current) / years_between      [mm/yr]
remaining_life = (t_current − t_required) / corrosion_rate     [yr]
next_interval  = min(remaining_life / 2, code_max_interval)    [yr]
```

The **half-life rule** sets the next inspection at no more than half the remaining life, capped by the code maximum interval. The corrosion rate may be supplied directly (`corrosion.rate_mm_per_year`) or derived from two thickness readings (`previous_mm`, `years_between`). The component is flagged `fail` when the current thickness is already at or below the minimum required thickness (repair / re-rate / retire), or when the remaining life is below a stated `required_remaining_life_years`.

The workflow reports the corrosion rate, remaining corrosion allowance, remaining life, the governing interval, and a top-level `screening_status` (pass/fail). The example drives an API 570 piping circuit corroding at 0.5 mm/yr to a `fail` — 5.0 yr remaining life against a 10 yr service requirement, with a 2.5 yr next-inspection interval.

Run with `uv run python -m digitalmodel examples/workflows/inspection-planning/input.yml`.

Reference: API 510 (pressure vessel inspection), API 570 (piping inspection), API 653 (tank inspection) — remaining-life and inspection-interval rules.
