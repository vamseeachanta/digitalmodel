import os
import sys
import pandas as pd
import numpy as np

# Add src to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src')))

from digitalmodel.infrastructure.common.visualizations_interactive import generate_interactive_heatmap
from digitalmodel.structural.structural_analysis.legacy.ffs.engine import LegacyFFSEngine

def simulate_metal_loss_grid(rows=20, cols=36):
    """
    Simulates a pipe wall thickness grid with a localized defect.
    Rows represent length, Cols represent circumference (10 degree intervals).
    """
    # Base wall thickness 0.500 inches
    base_wt = 0.500
    grid = np.full((rows, cols), base_wt)
    
    # Introduce a localized defect (metal loss)
    grid[8:12, 15:20] = 0.350
    grid[9:11, 16:19] = 0.280
    
    df = pd.DataFrame(grid)
    df.index.name = "Length_Index"
    df.columns.name = "Circum_Index"
    return df

def run_ffs_demo():
    # 1. Simulate Data
    wt_grid = simulate_metal_loss_grid()
    
    # 2. Perform Assessment
    engine = LegacyFFSEngine()
    # Simplified assessment for demo: Remaining Life = (WT - Tmin) / FCA
    t_min = 0.250
    fca = 0.010
    
    life_grid = (wt_grid - t_min) / fca
    life_grid[life_grid < 0] = 0
    
    # 3. Generate Visualizations
    # Thickness Heatmap
    fig_wt = generate_interactive_heatmap(
        wt_grid, 
        x_label="Circumference", 
        y_label="Length", 
        z_label="WT", 
        title="Pipe Wall Thickness Profile (Defect Visualization)"
    )
    
    # Remaining Life Heatmap
    fig_life = generate_interactive_heatmap(
        life_grid, 
        x_label="Circumference", 
        y_label="Length", 
        z_label="Years", 
        title="Calculated Remaining Life (API 579 Legacy Pattern)"
    )
    
    # 4. Export to HTML
    with open("legacy_ffs_dashboard.html", "w") as f:
        f.write("<html><head><title>FFS Legacy Integration Dashboard</title></head><body>")
        f.write("<h1>DigitalModel: API 579 Fitness-for-Service Dashboard</h1>")
        f.write("<p>This dashboard demonstrates the integration of legacy metal loss patterns and interactive plotting.</p>")
        f.write(fig_wt.to_html(full_html=False, include_plotlyjs='cdn'))
        f.write("<hr>")
        f.write(fig_life.to_html(full_html=False, include_plotlyjs='cdn'))
        f.write("</body></html>")
        
    print("Legacy FFS Dashboard generated: legacy_ffs_dashboard.html")

if __name__ == "__main__":
    run_ffs_demo()
