import os
import sys
import json
import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

# Add src to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src')))

from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext, CardData, RodSection, PumpProperties, SurfaceUnit, AnalysisResults
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow

def load_legacy_json(file_path):
    """Parses legacy JSON format into modern DynacardAnalysisContext."""
    with open(file_path, 'r') as f:
        data = json.load(f)
    
    # 1. Extract Rods
    rods = []
    for r in data['equipmentData']['Rods']:
        rods.append(RodSection(
            diameter=r['Diameter'],
            length=r['TotalLength'],
            modulus_of_elasticity=r['ModulusOfElasticity'],
            density=490.0 # Default steel density
        ))
    
    # 2. Extract Pump
    pump = PumpProperties(
        diameter=data['equipmentData']['Pump']['Diameter'],
        depth=data['equipmentData']['Pump']['Depth']
    )
    
    # 3. Extract Surface Unit
    su_data = data['equipmentData']['SurfaceUnit']
    surface_unit = SurfaceUnit(
        manufacturer=su_data.get('PumpingUnitManufacturer', 'Unknown'),
        unit_type=su_data.get('PumpingUnitGeometry', 'C'),
        stroke_length=su_data.get('StrokeLengthSetting', 144),
        gear_box_rating=su_data.get('GearBoxRating', 0)
    )
    
    # 4. Extract Card Data
    surface_card = CardData(
        position=data['cardData']['Position'],
        load=data['cardData']['Load']
    )
    
    return DynacardAnalysisContext(
        api14=data['CardDetails']['Api14'],
        surface_card=surface_card,
        rod_string=rods,
        pump=pump,
        surface_unit=surface_unit,
        spm=data['InputParameters']['StrokesPerMinute']
    )

def simulate_card(mode="NORMAL"):
    """Generates synthetic surface cards for different failure modes."""
    t = np.linspace(0, 2 * np.pi, 100)
    pos = 100 * (1 - np.cos(t))
    
    if mode == "NORMAL":
        load = 15000 + 5000 * np.sin(t)
    elif mode == "GAS_INTERFERENCE":
        load = 15000 + 5000 * np.sin(t)
        load[50:] = load[50:] * np.exp(-0.5 * (t[50:] - np.pi))
    elif mode == "FLUID_POUND":
        load = 15000 + 5000 * np.sin(t)
        load[60:80] = 8000 
    elif mode == "PUMP_TAGGING":
        load = 15000 + 5000 * np.sin(t)
        load[0:5] = 40000
        load[-5:] = 40000
    else:
        load = 15000 + 5000 * np.sin(t)
        
    return pos.tolist(), load.tolist()

def run_mixed_suite():
    all_results = []
    
    # 1. Run Simulations
    modes = ["NORMAL", "GAS_INTERFERENCE", "FLUID_POUND", "PUMP_TAGGING"]
    for mode in modes:
        pos, load = simulate_card(mode)
        ctx = DynacardAnalysisContext(
            api14=f"SIM-{mode}",
            surface_card=CardData(position=pos, load=load),
            rod_string=[RodSection(diameter=1.0, length=5000)],
            pump=PumpProperties(diameter=1.75, depth=5000),
            surface_unit=SurfaceUnit(manufacturer="Sim", unit_type="Test", stroke_length=144, gear_box_rating=640000),
            spm=10.0
        )
        workflow = DynacardWorkflow(ctx)
        all_results.append(workflow.run_full_analysis())
    
    # 2. Run Real Data Analysis
    real_data_path = "/mnt/github/workspace-hub/client_projects/energy_firm_data_analytics/dynacard/Code/Oxy.Cipher.DynaCard/tests/testdata/11708328.json"
    if os.path.exists(real_data_path):
        try:
            ctx_real = load_legacy_json(real_data_path)
            workflow_real = DynacardWorkflow(ctx_real)
            all_results.append(workflow_real.run_full_analysis())
        except Exception as e:
            print(f"Error loading real data: {e}")
        
    return all_results

def create_consolidated_report(results_list):
    fig = make_subplots(
        rows=len(results_list), cols=1,
        subplot_titles=[f"Well Analysis: {r.ctx.api14}" for r in results_list],
        vertical_spacing=0.05
    )

    for i, res in enumerate(results_list):
        # Surface Card
        fig.add_trace(
            go.Scatter(x=res.ctx.surface_card.position, y=res.ctx.surface_card.load, 
                       name=f"Surface ({res.ctx.api14})", line=dict(color='blue', width=1)),
            row=i+1, col=1
        )
        # Downhole Card
        fig.add_trace(
            go.Scatter(x=res.downhole_card.position, y=res.downhole_card.load, 
                       name=f"Downhole ({res.ctx.api14})", line=dict(color='red', width=2)),
            row=i+1, col=1
        )
        
        # Add classification annotation
        fig.add_annotation(
            text=f"<b>Diagnostic:</b> {res.diagnostic_message}",
            xref="x domain", yref="y domain",
            x=0.95, y=0.95, 
            showarrow=False, align="right",
            bgcolor="white", bordercolor="black",
            row=i+1, col=1
        )

    fig.update_layout(height=400 * len(results_list), title_text="Advanced Artificial Lift Diagnostic Suite", showlegend=True)
    fig.write_html("dynacard_comprehensive_report.html")
    print("Comprehensive report generated: dynacard_comprehensive_report.html")

if __name__ == "__main__":
    results = run_mixed_suite()
    create_consolidated_report(results)