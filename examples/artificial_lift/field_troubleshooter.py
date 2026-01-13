import os
import sys
import json
import glob
import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

# Add src to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src')))

from digitalmodel.modules.artificial_lift.dynacard.models import (
    DynacardAnalysisContext, CardData, RodSection, PumpProperties, SurfaceUnit
)
from digitalmodel.modules.artificial_lift.dynacard.solver import DynacardWorkflow

LEGACY_DATA_DIR = "/mnt/github/workspace-hub/client_projects/energy_firm_data_analytics/dynacard/Code/Oxy.Cipher.DynaCard/tests/testdata"

def load_and_map_well(file_path):
    """Maps legacy JSON to modern Context."""
    with open(file_path, 'r') as f:
        data = json.load(f)
    
    rods = [RodSection(diameter=r.get('Diameter', 1.0), length=r.get('TotalLength', 0.0)) 
            for r in data.get('equipmentData', {}).get('Rods', [])]
    
    pump = PumpProperties(
        diameter=data.get('equipmentData', {}).get('Pump', {}).get('Diameter', 1.5),
        depth=data.get('equipmentData', {}).get('Pump', {}).get('Depth', 5000.0)
    )
    
    surface_unit = SurfaceUnit(
        manufacturer=data.get('equipmentData', {}).get('SurfaceUnit', {}).get('PumpingUnitManufacturer', 'Unknown'),
        unit_type="C",
        stroke_length=data.get('InputParameters', {}).get('StrokeLengthSetting', 144.0),
        gear_box_rating=0
    )
    
    surface_card = CardData(
        position=data.get('cardData', {}).get('Position', []),
        load=data.get('cardData', {}).get('Load', [])
    )
    
    return DynacardAnalysisContext(
        api14=data.get('CardDetails', {}).get('Api14', os.path.basename(file_path)),
        surface_card=surface_card,
        rod_string=rods,
        pump=pump,
        surface_unit=surface_unit,
        spm=data.get('InputParameters', {}).get('StrokesPerMinute', 10.0)
    )

def run_field_troubleshooter():
    files = glob.glob(os.path.join(LEGACY_DATA_DIR, "*.json"))
    all_results = []
    
    print(f"Starting field analysis for {len(files)} wells...")
    
    for f in files:
        try:
            ctx = load_and_map_well(f)
            if not ctx.surface_card.position: continue
            
            workflow = DynacardWorkflow(ctx)
            res = workflow.run_full_analysis()
            all_results.append(res)
            print(f"  Processed Well: {ctx.api14} -> {res.diagnostic_message.split('.')[0]}")
        except Exception as e:
            print(f"  Error processing {f}: {e}")
            
    return all_results

def create_health_dashboard(results):
    # 1. Summary Table Data
    api_list = [r.ctx.api14 for r in results]
    classifications = [r.diagnostic_message.split('.')[0].replace("Classification: ", "") for r in results]
    fillages = [f"{r.pump_fillage*100:.1f}%" for r in results]
    buckling = ["YES" if r.buckling_detected else "NO" for r in results]
    
    # Define colors based on status
    colors = ["red" if "NORMAL" not in c else "green" for c in classifications]

    # 2. Create Multi-plot Dashboard
    fig = make_subplots(
        rows=len(results) + 1, cols=1,
        vertical_spacing=0.03,
        subplot_titles=["Field Health Overview"] + [f"Well: {api}" for api in api_list],
        specs=[[{"type": "table"}]] + [[{"type": "scatter"}]] * len(results)
    )

    # A. Summary Table
    fig.add_trace(
        go.Table(
            header=dict(values=["Well API14", "AI Diagnostic Status", "Pump Fillage", "Buckling Risk"],
                        fill_color='navy', font=dict(color='white', size=12)),
            cells=dict(values=[api_list, classifications, fillages, buckling],
                       fill_color=[['lavender']*len(results), colors, ['lavender']*len(results), ['lavender']*len(results)])
        ),
        row=1, col=1
    )

    # B. Individual Well Plots
    for i, res in enumerate(results):
        row_idx = i + 2
        # Surface
        fig.add_trace(
            go.Scatter(x=res.ctx.surface_card.position, y=res.ctx.surface_card.load, 
                       name=f"Surf-{res.ctx.api14}", line=dict(color='blue', width=1), opacity=0.5),
            row=row_idx, col=1
        )
        # Downhole
        fig.add_trace(
            go.Scatter(x=res.downhole_card.position, y=res.downhole_card.load, 
                       name=f"Pump-{res.ctx.api14}", line=dict(color='red', width=2)),
            row=row_idx, col=1
        )
        
        # Actionable Advice Annotation
        fig.add_annotation(
            text=f"<b>AI Insight:</b> {res.diagnostic_message}",
            xref="x domain", yref="y domain",
            x=0.98, y=0.1, showarrow=False, align="right",
            bgcolor="white", bordercolor="black", borderwidth=1,
            row=row_idx, col=1
        )

    fig.update_layout(
        height=400 * (len(results) + 1),
        title_text="Oxy Cipher: Field-Wide Sucker Rod Pump Health Dashboard",
        template="plotly_white",
        showlegend=False
    )
    
    output_fn = "field_health_dashboard.html"
    fig.write_html(output_fn)
    print(f"\nDashboard generated: {output_fn}")

if __name__ == "__main__":
    results = run_field_troubleshooter()
    if results:
        create_health_dashboard(results)
    else:
        print("No results generated.")
