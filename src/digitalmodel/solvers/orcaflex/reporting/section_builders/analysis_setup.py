"""
Analysis Setup section builder.
"""
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_analysis_setup_html(report: OrcaFlexAnalysisReport) -> str:
    """Builds the analysis setup and solver settings section."""
    if not report.analysis_setup:
        return ""

    setup = report.analysis_setup
    
    # Metadata Table
    meta_rows = [
        ["Analysis Types", _escape(setup.analysis_types)],
        ["Design Life [yrs]", f"{setup.design_life_yrs:.1f}" if setup.design_life_yrs is not None else "-"],
        ["Safety Class", _escape(setup.safety_class or "-")],
        ["Location Class", _escape(setup.location_class or "-")],
        ["Input Files", _escape(setup.input_files or "-")]
    ]
    meta_table_html = build_table(["Parameter", "Value"], meta_rows)

    # Solver Settings Table
    solver_table_html = ""
    if setup.solver_settings:
        s = setup.solver_settings
        s_rows = [
            ["OrcaFlex Version", _escape(s.orcaflex_version)],
            ["Python API Version", _escape(s.python_api_version or "N/A")],
            ["Static Conv. Criterion", f"{s.static_convergence_criterion:.2e}"],
            ["Static Max Iterations", str(s.static_max_iterations)],
            ["Dynamic Time Step [s]", f"{s.dynamic_time_step_s:.4f}"],
            ["Ramp Duration [s]", f"{s.ramp_duration_s:.1f}"],
            ["Simulation Duration [s]", f"{s.simulation_duration_s:.1f}"],
            ["Wave Theory", _escape(s.wave_theory)],
            ["Damping Model", _escape(s.damping_model or "Standard")]
        ]
        solver_table_html = build_table(["Solver Parameter", "Value"], s_rows)

    body_html = f"""
    <div id="analysis-metadata">
        {meta_table_html}
    </div>
    
    <h3 id="analysis-solver-settings">Solver Settings</h3>
    {solver_table_html if solver_table_html else '<p class="no-data">Detailed solver settings not provided.</p>'}
    """
    
    return wrap_section("analysis-setup", "10. Analysis Setup", body_html)
