from .models import DynacardAnalysisContext, AnalysisResults
from .physics import DynacardPhysicsSolver
from .diagnostics import PumpDiagnostics

class DynacardWorkflow:
    """
    Main orchestrator for Dynacard Analysis in digitalmodel.
    """
    
    def __init__(self, context: DynacardAnalysisContext = None):
        self.ctx = context
        if context:
            self.solver = DynacardPhysicsSolver(context)
        self.diagnostics = PumpDiagnostics()

    def router(self, cfg: dict) -> dict:
        """
        Maps configuration dict to dynacard workflow.
        """
        # 1. Transform dict to Context Model
        # Assuming cfg has necessary well and card data
        # For now, we simulate or use provided data in cfg
        # In production, this would use a data loader
        if 'well_data' in cfg:
            self.ctx = DynacardAnalysisContext(**cfg['well_data'])
            self.solver = DynacardPhysicsSolver(self.ctx)
            
        # 2. Run Analysis
        results = self.run_full_analysis()
        
        # 3. Update cfg with results
        cfg['results'] = results.dict()
        return cfg

    def run_full_analysis(self) -> AnalysisResults:
        """
        Executes the complete engineering and diagnostic workflow.
        """
        # 1. Physics: Surface to Downhole conversion
        results = self.solver.solve_wave_equation()
        results.ctx = self.ctx
        
        # 2. Physics: Mechanical Integrity
        self.solver.detect_buckling()
        
        # 3. Analytics: Fillage and Production
        self.solver.calculate_fillage()
        
        # 4. AI Diagnostics: Troubleshooting
        self.diagnostics.generate_troubleshooting_report(results)
        
        return results

def perform_well_troubleshooting(context_dict: dict):
    """
    Utility function for CLI or API integration.
    """
    ctx = DynacardAnalysisContext(**context_dict)
    workflow = DynacardWorkflow(ctx)
    return workflow.run_full_analysis()
