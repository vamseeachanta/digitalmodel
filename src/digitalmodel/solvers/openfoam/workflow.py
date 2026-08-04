"""Engine router for OpenFOAM CFD case build + solve (digitalmodel #1161).

This is the thin engine-facing handler that makes a CFD config resolvable
through the main digitalmodel engine (``basename: openfoam``). It does NOT
author any dict files or re-implement case generation — it delegates entirely
to :class:`OpenFOAMCaseBuilder` (case authoring, license-free) and
:class:`OpenFOAMRunner` (fail-closed subprocess execution).

It mirrors the ANSYS/OrcaWave/AQWA router contract so the same fixed
``uv run python -m digitalmodel <input>`` lane command can drive a CFD solve.

Input contract. Two forms are accepted and must not be mixed (#1575).

Canonical — carries the full case definition, validated fail-closed::

    basename: openfoam
    openfoam:
      operation: run_openfoam     # build_case | run_openfoam (default)
      output_directory: out       # relative to Analysis.result_folder
      case_definition:
        schema_version: 1
        kind: authored            # "prebuilt" is reserved and refused in v1
        authored:
          case_type: sloshing
          name: my_case
          solver: interFoam
          domain: {...}           # SI metres, right-handed, z up
          motion: {...}           # optional prescribed single-DOF forcing
          fill: {...}             # optional VOF partial fill
          time: {...}             # start/end/step/write controls
          function_objects: {...} # optional pressure taps + write controls
      execution:
        mesh_utility: blockMesh
        run_snappy: false
        run_set_fields: false
        to_vtk: false
        timeout_seconds: 43200
        dry_run: false

Legacy — the pre-#1575 flat form, still supported and normalised onto the
schema above. It cannot express domain, motion, fill, time or function
objects; those keys are refused here rather than silently dropped::

    basename: openfoam            # (or "cfd")
    openfoam:
      operation: run_openfoam     # build_case | run_openfoam (default)
      case_type: current_loading  # any CaseType value
      name: my_case               # optional; defaults to <case_type>_case
      output_directory: out       # relative to Analysis.result_folder
      solver: simpleFoam          # optional override; else the case-type default
      mesh_utility: blockMesh
      run_snappy: false
      to_vtk: true
      dry_run: false

Unknown keys are rejected by name in both forms. Before #1575 they were
accepted and dropped, so a request carrying a full case definition rendered a
default static case and still reported success.

Fail-closed: if a real solve was requested but no OpenFOAM is on PATH, the
runner reports DRY_RUN and this router raises, so the licensed-run lane can
never record a false finish (same guard the ANSYS router uses).
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

_SUPPORTED_OPERATIONS = ("run_openfoam", "build_case")


class OpenFOAMWorkflow:
    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        settings = cfg.setdefault("openfoam", {})
        operation = settings.get("operation", "run_openfoam")
        if operation not in _SUPPORTED_OPERATIONS:
            raise ValueError(
                f"Unsupported openfoam operation: {operation!r}. "
                f"Supported operations: {', '.join(_SUPPORTED_OPERATIONS)}"
            )

        case_dir, parsed = self._build_case(cfg, settings)
        settings["case_dir"] = str(case_dir)

        if operation == "build_case":
            settings["run_status"] = "built"
            return cfg

        return self._run_openfoam(cfg, settings, case_dir, parsed)

    # ------------------------------------------------------------------ #
    #  build (license-free) — delegate to OpenFOAMCaseBuilder            #
    # ------------------------------------------------------------------ #
    def _build_case(
        self, cfg: dict[str, Any], settings: dict[str, Any]
    ) -> tuple[Path, Any]:
        # Lazy import: keep the engine import graph light.
        from .case_builder import OpenFOAMCaseBuilder
        from .case_definition import parse_case_request

        # The whole request is validated as one closed contract (#1575). Every
        # accepted semantic leaf is carried onto the typed case here; anything
        # unknown is refused rather than silently dropped, which is what let a
        # mock batch pass while rendering a default static case.
        parsed = parse_case_request(self._case_request(settings))

        parent_dir = self._resolve_output_dir(cfg, settings)
        parent_dir.mkdir(parents=True, exist_ok=True)
        case_dir = OpenFOAMCaseBuilder(
            parsed.case,
            list(parsed.function_objects.pressure_taps),
            tap_write_control=parsed.function_objects.write_control,
            tap_write_interval=parsed.function_objects.write_interval,
        ).build(parent_dir)
        return case_dir, parsed

    @staticmethod
    def _case_request(settings: dict[str, Any]) -> dict[str, Any]:
        """Strip adapter-owned keys the schema does not describe.

        ``case_dir`` and the run-report keys are written back onto ``settings``
        by this router, so a second pass over the same mapping must not see
        them as unknown input.
        """
        adapter_owned = {"case_dir", "run_status", "outputs"}
        return {
            key: value
            for key, value in settings.items()
            if key not in adapter_owned
        }

    # ------------------------------------------------------------------ #
    #  run (fail-closed) — delegate to OpenFOAMRunner                    #
    # ------------------------------------------------------------------ #
    def _run_openfoam(
        self, cfg: dict[str, Any], settings: dict[str, Any], case_dir: Path,
        parsed: Any,
    ) -> dict[str, Any]:
        from .runner import OpenFOAMRunConfig, OpenFOAMRunner

        # The runner is driven from the validated execution plan, so a canonical
        # request and a normalised legacy one select the same run identically.
        plan = parsed.execution
        requested_dry_run = plan.dry_run
        run_cfg = OpenFOAMRunConfig(
            solver=parsed.case.solver_config.solver_name,
            mesh_utility=plan.mesh_utility,
            run_snappy=plan.run_snappy,
            to_vtk=plan.to_vtk,
            timeout_seconds=plan.timeout_seconds,
            dry_run=requested_dry_run,
        )
        result = OpenFOAMRunner(run_cfg).run(case_dir)

        status = str(getattr(result.status, "value", result.status)).lower()
        settings["run_status"] = status
        settings["solver"] = result.solver
        settings["outputs"] = {
            "case_dir": str(result.case_dir),
            "vtk_dir": str(result.vtk_dir) if result.vtk_dir else None,
            "stages": [
                {"name": s.name, "return_code": s.return_code}
                for s in result.stages
            ],
        }

        if status == "failed":
            raise RuntimeError(
                f"OpenFOAM solve failed: {result.error_message or 'unknown error'}"
            )
        # A silent dry-run fallback (no OpenFOAM on PATH) must NOT read as success
        # for a real solve request — the licensed-run lane would record a false
        # finish. Mirrors the ANSYS router's fail-closed guard.
        if status == "dry_run" and not requested_dry_run:
            raise RuntimeError(
                "OpenFOAM solver unavailable: run fell back to dry-run "
                f"({result.error_message or 'no OpenFOAM on PATH'}). "
                "Run on a host with OpenFOAM installed."
            )
        return cfg

    # ------------------------------------------------------------------ #
    #  helpers                                                            #
    # ------------------------------------------------------------------ #
    @staticmethod
    def _resolve_output_dir(cfg: dict[str, Any], settings: dict[str, Any]) -> Path:
        result_folder = cfg.get("Analysis", {}).get("result_folder")
        base_dir = Path(result_folder) if result_folder else Path.cwd()
        output = settings.get("output_directory")
        if output is None:
            return base_dir.resolve()
        output_dir = Path(output)
        if output_dir.is_absolute():
            return output_dir
        return (base_dir / output_dir).resolve()
