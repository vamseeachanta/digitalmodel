# ABOUTME: Main orchestrator for Dynacard analysis in digitalmodel.
# ABOUTME: Integrates physics solvers, calculations, and diagnostics into unified workflow.

from pathlib import Path
from typing import Any, Literal

from .models import DynacardAnalysisContext, AnalysisResults
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

    Supports two physics solvers:
        - 'gibbs': Frequency-domain Gibbs analytical method (faster, default)
        - 'finite_difference': Time-domain finite difference (more detailed)
    """

    def __init__(
        self,
        context: DynacardAnalysisContext = None,
        solver_method: Literal['gibbs', 'finite_difference'] = 'gibbs'
    ):
        """Initialize the dynacard analyzer.

        Args:
            context: Complete well analysis context. Can be set later
                before calling analysis methods.
            solver_method: Physics solver to use — ``'gibbs'`` for
                frequency-domain or ``'finite_difference'`` for
                time-domain.
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
        else:
            self.solver = DynacardPhysicsSolver(self.ctx)

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
        """Build well_data from a pinned synthetic card generator."""
        if 'synthetic_card' not in cfg or 'well_data' in cfg:
            return

        from .card_generators import ALL_GENERATORS

        synthetic_cfg = cfg['synthetic_card']
        mode = synthetic_cfg['mode']
        card = ALL_GENERATORS[mode](seed=int(synthetic_cfg.get('seed', 0)))
        well_cfg = cfg.get('well', {})
        rod_cfg = well_cfg.get('rod', {})
        pump_cfg = well_cfg.get('pump', {})
        surface_unit_cfg = well_cfg.get('surface_unit', {})

        cfg['well_data'] = {
            'api14': well_cfg.get('api14', f'SIM-{mode}'),
            'surface_card': card.model_dump(),
            'rod_string': [rod_cfg],
            'pump': pump_cfg,
            'surface_unit': surface_unit_cfg,
            'spm': well_cfg.get('spm', 10.0),
        }

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
            results.downhole_card,
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
        if self.solver_method == 'finite_difference':
            results = self.solver.solve()
        else:
            results = self.solver.solve_wave_equation()

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

    def compare_solvers(self) -> dict:
        """
        Run both solvers and compare results.

        Useful for validation and understanding solver differences.
        """
        # Run Gibbs solver
        gibbs_solver = DynacardPhysicsSolver(self.ctx)
        gibbs_results = gibbs_solver.solve_wave_equation()

        # Run FD solver
        fd_solver = FiniteDifferenceSolver(self.ctx)
        fd_results = fd_solver.solve()

        # Compare key metrics
        import numpy as np

        gibbs_pos = np.array(gibbs_results.downhole_card.position)
        fd_pos = np.array(fd_results.downhole_card.position)
        gibbs_load = np.array(gibbs_results.downhole_card.load)
        fd_load = np.array(fd_results.downhole_card.load)

        # Stroke comparison
        gibbs_stroke = np.max(gibbs_pos) - np.min(gibbs_pos)
        fd_stroke = np.max(fd_pos) - np.min(fd_pos)
        stroke_diff_pct = abs(gibbs_stroke - fd_stroke) / gibbs_stroke * 100

        # Load comparison (RMSE)
        # Resample if different lengths
        if len(gibbs_load) != len(fd_load):
            from scipy.interpolate import interp1d
            x_gibbs = np.linspace(0, 1, len(gibbs_load))
            x_fd = np.linspace(0, 1, len(fd_load))
            f_fd = interp1d(x_fd, fd_load, kind='linear')
            fd_load_resampled = f_fd(x_gibbs)
        else:
            fd_load_resampled = fd_load

        load_rmse = np.sqrt(np.mean((gibbs_load - fd_load_resampled) ** 2))
        load_max = np.max(gibbs_load)
        load_rmse_pct = load_rmse / load_max * 100

        return {
            'gibbs_results': gibbs_results,
            'fd_results': fd_results,
            'comparison': {
                'gibbs_stroke': float(gibbs_stroke),
                'fd_stroke': float(fd_stroke),
                'stroke_diff_pct': float(stroke_diff_pct),
                'load_rmse': float(load_rmse),
                'load_rmse_pct': float(load_rmse_pct),
            }
        }


def perform_well_troubleshooting(
    context_dict: dict,
    solver_method: str = 'gibbs'
) -> AnalysisResults:
    """
    Utility function for CLI or API integration.
    """
    ctx = DynacardAnalysisContext(**context_dict)
    workflow = DynacardWorkflow(ctx, solver_method=solver_method)
    return workflow.run_full_analysis()
