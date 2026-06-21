# Weather-Window (Workability) Screening

Screens a marine operation against a metocean Hs record to answer: **what fraction of the time can the operation run, and is there a continuous window long enough to complete it?**

A weather window is a continuous spell where the significant wave height is at or below the operational limit (`hs_limit_m`). The workflow runs a persistence analysis (reusing the tested `digitalmodel.orcaflex.weather_window.analyse_persistence`) and reports:

- **workability** — the fraction of the record with `Hs ≤ hs_limit`;
- **window durations** — mean / p10 / p90 / longest continuous window;
- **window fit** — whether any window is long enough for the operation's `required_duration_hours`, and how many such viable windows occur.

The screen **passes** when at least one weather window is long enough for the operation (and, if given, the workability meets `required_workability_pct`); otherwise `screening_status: fail`.

The example screens a 48 h operation (Hs limit 2.0 m) against an illustrative 7-day record: workability is 73 %, but the longest continuous window is only 36 h → `fail` (no window long enough — split the operation, use a vessel that can suspend, or wait for a calmer period).

Run with `uv run python -m digitalmodel examples/workflows/weather-window/input.yml`.

Reference: DNV-OS-H101 / DNV-ST-N001 (Marine Operations); DNV-RP-H103; Noble Denton 0027/ND; API RP 2MET.
