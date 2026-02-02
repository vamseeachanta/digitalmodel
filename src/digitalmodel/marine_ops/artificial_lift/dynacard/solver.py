# ABOUTME: Main orchestrator for Dynacard analysis in digitalmodel.
# ABOUTME: Integrates physics solvers, calculations, and diagnostics into unified workflow.

from typing import Optional, Literal
from .models import DynacardAnalysisContext, AnalysisResults
from .physics import DynacardPhysicsSolver
from .finite_difference import FiniteDifferenceSolver
from .diagnostics import PumpDiagnostics
from .calculations import run_p1_calculations


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
        self.ctx = context
        self.solver_method = solver_method
        self.solver = None
        self.diagnostics = PumpDiagnostics()

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

        # 3. Update cfg with results
        cfg['results'] = results.model_dump()
        return cfg

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
        self.diagnostics.generate_troubleshooting_report(results)

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
