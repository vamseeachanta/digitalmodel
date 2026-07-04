# Interactive report generators (#1276)

Scripts that produce the interactive verification reports. All chart data
and animation frames are computed from real OpenFOAM v2312 solves of the
archived cases under `../cases/` — nothing is hand-entered.

- `house.py` — shared pieces: suite CSS + Plotly (CDN) chart helper + the
  self-contained animation flipbook (base64 JPEG frames, play/pause/scrub).
- `build_dam_break.py <solved_case> <repo_root>` — full dam-break report
  (case = `cases/dam_break/` solved at 288×144, endTime 0.5).
- `build_wave_tank.py <solved_case> <results_dir> <repo_root>` — NWT report
  (case = `cases/wave_tank/` solved at 500×55, `analyze_wave_tank.py` run).
- `build_floating_body.py <solved_case> <results_dir> <repo_root>` — heave
  decay report (case = `cases/floating_body_decay/` solved 14 s).
- `build_kleefsman.py <solved_case> <exp_csv> <repo_root>` — green-water
  impact report (case = `cases/kleefsman_impact/` solved at 161×50×50).
- `build_wave_excited_body.py <case_root> <results_dir> <repo_root>
  [fast_results_dir]` — wave-excited floating body (overset) report
  (case root = `cases/wave_excited_body/` layout with `background/` solved
  34 s and reconstructed; `analyze_wave_excited_body.py` run per variant).
- `upgrade_reports.py <runs_dir> <repo_root>` — upgrades the other four
  reports in place (run against a fresh checkout; it refuses to double-apply).
  Expects solved runs at `<runs_dir>/{fp,tfp,naca,cyl}`:
  - `fp` = `cases/flat_plate_blasius/` solved + `analyze_flat_plate.py` run
  - `tfp` = `cases/turbulent_flat_plate/` solved + `analyze_tflat.py` run
  - `naca` = airFoil2D tutorial + `cases/naca0012_polar/run_polar.sh` + `analyze_naca.py`
  - `cyl` = `cases/cylinder_re100/` solved 0→150 s (forceCoeffs history), then
    restarted 150→162.25 s with `writeInterval 0.25; purgeWrite 0` for the
    vortex-street animation frames, + `analyze_cyl.py`
