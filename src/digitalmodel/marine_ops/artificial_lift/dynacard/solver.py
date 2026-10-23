# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# ABOUTME: Main orchestrator for Dynacard analysis in digitalmodel.
# ABOUTME: Integrates physics solvers, calculations, and diagnostics into unified workflow.

from dataclasses import asdict
from pathlib import Path

import numpy as np
from typing import Any, Literal

from .models import DynacardAnalysisContext, AnalysisResults
from .comparison import compare_cards
from .exceptions import PhysicsError
from .solver_guards import COMPARISON_LOAD_NRMSE_LIMIT_PCT, validate_stroke
from .physics import DynacardPhysicsSolver
from .finite_difference import FiniteDifferenceSolver
from .diagnostics import PumpDiagnostics
from .calculations import run_p1_calculations
from .report_sections import (
    _build_alarm_block,
    _classification_verdict,
    build_diagnostic_report_html,
)


class DynacardWorkflow:
    """
    Main orchestrator for Dynacard Analysis in digitalmodel.

    Supports three physics solvers:
        - 'everitt_jennings': Space-marching finite difference (SPE 18189).
          The default. Rebuilds downhole load from the strain field and
          handles deviated wells.
        - 'gibbs': Frequency-domain analytical method. Retained for
          comparison only.
        - 'finite_difference': Time-domain finite difference. Numerically
          unstable — see below.

    Measured against the stored reference downhole cards on all five fixture
    wells (phase-aligned, since a card is a closed loop):

        method              med nRMSE   med |stroke err|   med corr
        everitt_jennings         0.9%              0.0%      1.000
        gibbs                   17.3%              2.7%      0.966
        finite_difference     diverges           diverges     0.022

    'gibbs' does not transform the load at all — it returns an affine rescale
    of the surface card (issue #1857) — so it cannot resolve pump condition.
    'finite_difference' overflows to ~1e45 and beyond on three of five wells.
    Neither should be used for diagnosis.
    """

    def __init__(
        self,
        context: DynacardAnalysisContext = None,
        solver_method: Literal[
            'everitt_jennings', 'gibbs', 'finite_difference'
        ] = 'everitt_jennings'
    ):
        """Initialize the dynacard analyzer.

        Args:
            context: Complete well analysis context. Can be set later
                before calling analysis methods.
            solver_method: Physics solver to use. Defaults to
                ``'everitt_jennings'``, the only one that reproduces the
                reference downhole cards. ``'gibbs'`` and
                ``'finite_difference'`` are retained for comparison and
                should not be used for diagnosis — see the class docstring.
        """
        self.ctx = context
        self.solver_method = solver_method
        self.solver = None
        self.diagnostics = PumpDiagnostics()
        self._last_diagnostic_result = None

        if context:
            self._init_solver()

    def _init_solver(self):
        """Initialize the appropriate physics solver."""
        if self.solver_method == 'finite_difference':
            self.solver = FiniteDifferenceSolver(self.ctx)
        elif self.solver_method == 'everitt_jennings':
            # Imported here to keep the scipy/numba-backed solver off the
            # import path of callers that never select it.
            from .everitt_jennings.adapter import EverittJenningsContextSolver
            self.solver = EverittJenningsContextSolver(self.ctx)
        elif self.solver_method == 'gibbs':
            self.solver = DynacardPhysicsSolver(self.ctx)
        else:
            raise ValueError(f"unknown solver method: {self.solver_method!r}")

    def router(self, cfg: dict) -> dict:
        """
        Maps configuration dict to dynacard workflow.
        """
        self._apply_synthetic_card(cfg)

        # 1. Transform dict to Context Model
        if 'well_data' in cfg:
            self.ctx = DynacardAnalysisContext(**cfg['well_data'])
            self._init_solver()

        # Check for solver method override
        if 'solver_method' in cfg:
            self.solver_method = cfg['solver_method']
            self._init_solver()

        # 2. Run Analysis
        results = self.run_full_analysis()
        diag = self._last_diagnostic_result
        if diag is None:
            diag = self.diagnostics.classify_with_context(results)
        verdict = _classification_verdict(diag.classification)
        alarm_block = _build_alarm_block(self.ctx, diag.classification)

        # 3. Update cfg with results
        cfg['screening_status'] = verdict['screening_status']
        cfg['artificial_lift'] = {
            'screening_status': verdict['screening_status'],
            'classification': verdict['classification'],
            'severity': verdict['severity'],
            'setpoint_reference': alarm_block['reference'],
            'alarm_note': alarm_block['note'],
            'setpoints': alarm_block['setpoints'],
            'alarms': alarm_block['alarms'],
        }
        cfg['results'] = results.model_dump()
        if cfg.get('report', {}).get('html', False):
            html_report = self._write_html_report(
                cfg,
                results,
                diag,
                verdict,
                alarm_block,
            )
            cfg.setdefault('outputs', {})['html_report'] = str(html_report)
        return cfg

    def _apply_synthetic_card(self, cfg: dict) -> None:
        """Build well_data from a pinned synthetic card generator.

        The generators produce DOWNHOLE cards -- they are named for pump
        conditions and draw the shape those conditions make at the pump. So
        the generated card is run *up* the rod string first, and it is the
        resulting surface card that goes into ``well_data``. The
        surface-to-downhole solver then has a real conversion to perform and
        recovers the generated pump card, which is what the classifier is
        trained on.

        Handing the generated card straight to ``surface_card`` instead asks
        the solver to strip a rod string that was never there. That only ever
        looked right under a solver that left the load alone (#1857).
        """
        if 'synthetic_card' not in cfg or 'well_data' in cfg:
            return

        from .card_generators import get_generator, surface_card_from_pump_card

        synthetic_cfg = cfg['synthetic_card']
        mode = synthetic_cfg['mode']
        pump_card = get_generator(mode)(seed=int(synthetic_cfg.get('seed', 0)))
        well_cfg = cfg.get('well', {})
        rod_cfg = well_cfg.get('rod', {})
        pump_cfg = well_cfg.get('pump', {})
        surface_unit_cfg = well_cfg.get('surface_unit', {})

        well_data = {
            'api14': well_cfg.get('api14', f'SIM-{mode}'),
            # Placeholder: replaced below by the forward-modelled surface card.
            'surface_card': pump_card.model_dump(),
            'rod_string': [rod_cfg],
            'pump': pump_cfg,
            'surface_unit': surface_unit_cfg,
            'spm': well_cfg.get('spm', 10.0),
        }
        synthetic_ctx = DynacardAnalysisContext(**well_data)
        surface_card = surface_card_from_pump_card(pump_card, synthetic_ctx)
        well_data['surface_card'] = surface_card.model_dump()

        cfg['well_data'] = well_data

    def _write_html_report(
        self,
        cfg: dict,
        results: AnalysisResults,
        diag: Any,
        verdict: dict[str, str],
        alarm_block: dict[str, Any],
    ) -> Path:
        """Write a standalone diagnostic report beside the saved cfg."""
        from .visualization.diagnostic_annotator import DiagnosticAnnotator

        result_folder = self._result_folder(cfg)
        result_folder.mkdir(parents=True, exist_ok=True)
        file_base = self._result_file_base(cfg)
        report_path = result_folder / f'{file_base}_diagnostic_report.html'

        svg = DiagnosticAnnotator().render(
            results.diagnostic_card or results.downhole_card,
            diag.classification,
            diag.confidence,
            diag.differential,
        )
        html = build_diagnostic_report_html(cfg, results, svg, verdict, alarm_block)
        report_path.write_text(html)
        return report_path

    def _result_folder(self, cfg: dict) -> Path:
        analysis = cfg.get('Analysis', {})
        result_folder = analysis.get('result_folder')
        if result_folder:
            return Path(result_folder)
        if '_config_dir_path' in cfg:
            return Path(cfg['_config_dir_path']) / 'results'
        return Path('results')

    def _result_file_base(self, cfg: dict) -> str:
        analysis = cfg.get('Analysis', {})
        if analysis.get('file_name'):
            return Path(str(analysis['file_name'])).stem
        if cfg.get('_config_file_path'):
            return Path(str(cfg['_config_file_path'])).stem
        return 'input'

    def run_full_analysis(self) -> AnalysisResults:
        """
        Executes the complete engineering and diagnostic workflow.

        Steps:
            1. Physics: Surface to Downhole card conversion
            2. Physics: Mechanical integrity check (buckling)
            3. Calculations: Fluid load, CPIP, Fillage, Production
            4. Diagnostics: AI-driven troubleshooting
        """
        # 1. Physics: Surface to Downhole conversion
        if self.solver_method in ('finite_difference', 'everitt_jennings'):
            results = self.solver.solve()
        else:
            results = self.solver.solve_wave_equation()

        if results.downhole_card is None:
            raise PhysicsError("solver returned no downhole card", solver=self.solver_method)
        validate_stroke(
            self.ctx.surface_card.position, results.downhole_card.position,
            results.downhole_card.load, self.solver_method,
        )

        results.ctx = self.ctx
        results.solver_method = self.solver_method

        # 2. Physics: Mechanical Integrity
        self.solver.detect_buckling()

        # 3. Calculations: P1 calculations
        if results.downhole_card is not None:
            p1_results = run_p1_calculations(self.ctx, results.downhole_card)

            results.fluid_load = p1_results['fluid_load']
            results.cpip = p1_results['cpip']
            results.fillage = p1_results['fillage']
            results.production = p1_results['production']

            # Update legacy fields for compatibility
            results.pump_fillage = p1_results['fillage'].fillage
            results.inferred_production = p1_results['production'].theoretical_production

        # 4. AI Diagnostics: Troubleshooting
        diag = self.diagnostics.classify_with_context(results)
        self.diagnostics.generate_troubleshooting_report(results, diag=diag)
        self._last_diagnostic_result = diag

        return results

    def compare_solvers(self, *, allow_divergence: bool = False) -> dict:
        """Compare each alternative against the default Everitt-Jennings card.

        Load nRMSE is phase- and direction-invariant, normalized by the
        reference card's load range. The 4.40% envelope is twice the 2.20%
        maximum reference-parity bound in ``test_solver_parity.py``: two
        outputs at opposite ends of that bound can differ by twice the bound.
        This stated engineering comparison envelope is not a guarantee of
        physical accuracy or an exact bound under changing normalization.
        Stroke differences are reported separately. Only the empirical Gibbs
        comparison path is restricted to the measured surface stroke; other
        methods may exhibit plunger overtravel.

        Invalid cards and load nRMSE above the envelope raise PhysicsError.
        ``allow_divergence=True`` returns the failed gate and its evidence for
        inspection, without returning invalid cards or waiving stroke guards.
        The reference is always the default solver, even when this workflow
        explicitly selects a comparison-only alternative.
        """
        from .everitt_jennings.adapter import EverittJenningsContextSolver

        if type(allow_divergence) is not bool:
            raise ValueError("allow_divergence must be a boolean")
        if self.ctx is None:
            raise ValueError("solver comparison requires a context")
        reference_results = EverittJenningsContextSolver(self.ctx).solve()
        reference = reference_results.downhole_card
        if reference is None:
            raise PhysicsError("default solver returned no downhole card")
        validate_stroke(self.ctx.surface_card.position, reference.position,
                        reference.load, "everitt_jennings")
        report = self._comparison_report(reference_results)
        if not report["within_threshold"] and not allow_divergence:
            raise PhysicsError(
                "solver disagreement exceeds comparison envelope or output is invalid",
                solver="comparison",
                details={key: report[key] for key in
                         ("comparisons", "threshold_load_nrmse_pct")},
            )
        return report

    def _comparison_report(self, reference_results):
        """Run both alternatives once and retain the gate for each candidate."""
        report = {
            "reference_solver": "everitt_jennings",
            "reference_results": reference_results,
            "threshold_load_nrmse_pct": COMPARISON_LOAD_NRMSE_LIMIT_PCT,
            "solver_results": {},
            "comparisons": {},
            "within_threshold": True,
        }
        candidates = {
            "gibbs": DynacardPhysicsSolver(self.ctx).solve_wave_equation,
            "finite_difference": FiniteDifferenceSolver(self.ctx).solve,
        }
        for method, solve in candidates.items():
            comparison, results = self._compare_candidate(method, solve, reference_results.downhole_card)
            report["comparisons"][method] = comparison
            if results is not None:
                report["solver_results"][method] = results
            if not comparison["within_threshold"]:
                report["within_threshold"] = False
        return report

    def _compare_candidate(self, method, solve, reference):
        """Keep invalid candidate output visible without treating it as a score."""
        try:
            results = solve()
            candidate = results.downhole_card
            if candidate is None:
                raise PhysicsError("solver returned no downhole card", solver=method)
            validate_stroke(self.ctx.surface_card.position, candidate.position,
                            candidate.load, method)
            with np.errstate(over="raise", invalid="raise", divide="raise"):
                metrics = asdict(compare_cards(candidate, reference))
            metrics["reference_stroke"] = float(np.ptp(reference.position))
            metrics["candidate_stroke"] = float(np.ptp(candidate.position))
            metrics["within_threshold"] = (
                metrics["load_nrmse_pct"] <= COMPARISON_LOAD_NRMSE_LIMIT_PCT
            )
            return metrics, results
        except (PhysicsError, FloatingPointError) as error:
            return {"error": str(error), "within_threshold": False,
                    "details": getattr(error, "details", None)}, None


def perform_well_troubleshooting(
    context_dict: dict,
    solver_method: str = 'everitt_jennings'
) -> AnalysisResults:
    """
    Utility function for CLI or API integration.

    Defaults to the same solver :class:`DynacardWorkflow` does. It used to
    default to ``'gibbs'``, which does not transform the load at all (#1857),
    so every caller that took the default was diagnosing a rescaled surface
    card and calling it a pump card.
    """
    ctx = DynacardAnalysisContext(**context_dict)
    workflow = DynacardWorkflow(ctx, solver_method=solver_method)
    return workflow.run_full_analysis()
