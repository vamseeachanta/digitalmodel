"""Engine router for the solver smoke test (``basename: solver_smoke_test``).

Makes the OrcaFlex/AQWA end-to-end licence probe in
``scripts/solver_smoke_test.py`` reachable through the fixed licensed-run lane
command ``uv run python -m digitalmodel <input.yml>``, so a licensed host can be
asked "can you actually solve right now?" without an operator on the console.

This exists because a *readiness* check is not enough. ``orcawave-doctor`` and
``openfoam doctor`` confirm a binding imports and an executable exists; neither
proves the licence can be checked out, which is the failure that actually stops
a run. It also matters that this runs *inside the agent's session*: OrcaFlex
licence checkout fails under an SSH public-key logon token even though the
environment, registry and licence-server reachability are identical, so a
remote SSH probe cannot answer the question for OrcaFlex. The lane can.

Input contract::

    basename: solver_smoke_test
    solver_smoke_test:
      solvers: [orcaflex, aqwa]   # subset to probe; default both
      output_directory: null      # relative to Analysis.result_folder; null = it
      require_all: true           # false -> report failures without raising
      keep_scratch: false         # true -> retain the solve scratch dir for triage

Fail-closed, matching the ANSYS/OpenFOAM routers: any probed solver that cannot
solve raises, so the lane records a real failure instead of a false finish.
The report is written as ``solver_smoke_test.json`` under the output directory,
which is where the lane's ``result_return`` collector picks small JSON up from.
"""

from __future__ import annotations

import json
import shutil
import tempfile
from pathlib import Path
from typing import Any

DEFAULT_SOLVERS = ("orcaflex", "aqwa")


class SolverSmokeTestWorkflow:
    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        settings = cfg.setdefault("solver_smoke_test", {})

        solvers = settings.get("solvers") or list(DEFAULT_SOLVERS)
        if isinstance(solvers, str):
            solvers = [solvers]
        unknown = [s for s in solvers if s not in DEFAULT_SOLVERS]
        if unknown:
            raise ValueError(
                f"Unknown solver(s) in solver_smoke_test.solvers: {unknown}. "
                f"Supported: {', '.join(DEFAULT_SOLVERS)}"
            )

        output_dir = self._resolve_output_dir(cfg, settings)
        output_dir.mkdir(parents=True, exist_ok=True)

        # Lazy import: the probe pulls in OrcFxAPI and the diffraction stack, and
        # the engine must stay importable on hosts that have neither.
        from digitalmodel.solvers.smoke.probes import run_probes

        # Solve scratch (.sim/.dat/.LIS, plus the AQWA validator's own JSON) is
        # deliberately kept OUT of the results folder. The lane's result_return
        # collector walks that folder recursively and ships small JSON back
        # through the queue; scratch there would return validator noise as if it
        # were a result, and strand heavy licensed artifacts in a returned tree.
        keep_scratch = settings.get("keep_scratch", False)
        scratch_root = tempfile.mkdtemp(prefix="solver_smoke_")
        try:
            report = run_probes(solvers, Path(scratch_root))
            if keep_scratch:
                settings["scratch_dir"] = scratch_root
        finally:
            if not keep_scratch:
                shutil.rmtree(scratch_root, ignore_errors=True)

        report_path = output_dir / "solver_smoke_test.json"
        report_path.write_text(json.dumps(report, indent=2, default=str))

        settings["report_path"] = str(report_path)
        settings["report"] = report

        if settings.get("require_all", True) and not report["ok"]:
            failed = [r["solver"] for r in report["results"] if not r["ok"]]
            raise RuntimeError(
                f"Solver smoke test FAILED for: {', '.join(failed)}. "
                f"Report: {report_path}"
            )
        return cfg

    @staticmethod
    def _resolve_output_dir(cfg: dict[str, Any], settings: dict[str, Any]) -> Path:
        result_folder = cfg.get("Analysis", {}).get("result_folder")
        base_dir = Path(result_folder) if result_folder else Path.cwd()
        output = settings.get("output_directory")
        if output is None:
            return base_dir.resolve()
        output_dir = Path(output)
        if output_dir.is_absolute():
            return output_dir.resolve()
        return (base_dir / output_dir).resolve()
