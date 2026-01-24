#!/usr/bin/env python
"""Generate interactive HTML report for pipeline installation analysis."""
import OrcFxAPI
import pandas as pd
import plotly.graph_objects as go
from pathlib import Path

# X65 yield strength
X65_YIELD_MPa = 65 * 6.89476  # 448.2 MPa

runs_dir = Path(__file__).parent.parent / 'runs'
sim_files = sorted(runs_dir.glob('*.sim'))

print(f'Processing {len(sim_files)} sim files...')

results = []

for sim_file in sim_files:
    model = OrcFxAPI.Model(str(sim_file))
    pipeline = model['pipeline']

    period_static = OrcFxAPI.pnStaticState
    period_lw = OrcFxAPI.pnLatestWave

    end_a_static = pipeline.TimeHistory('Effective Tension', period_static, objectExtra=OrcFxAPI.oeEndA)[0]
    end_a_dyn = pipeline.TimeHistory('Effective Tension', period_lw, objectExtra=OrcFxAPI.oeEndA)

    rg_y = pipeline.RangeGraph('y', period_lw)
    rg_strain = pipeline.RangeGraph('Direct Tensile Strain', period_lw)
    rg_bend_stress = pipeline.RangeGraph('Max Bending Stress', period_lw)
    rg_tens_stress = pipeline.RangeGraph('Direct Tensile Stress', period_lw)

    max_bend_stress_mpa = max(rg_bend_stress.Max) / 1000
    max_tens_stress_mpa = max(rg_tens_stress.Max) / 1000
    bend_util = max_bend_stress_mpa / X65_YIELD_MPa
    combined_util = (max_bend_stress_mpa + max_tens_stress_mpa) / X65_YIELD_MPa

    parts = sim_file.stem.split('_')
    tension_val = int(parts[0].replace('kN', ''))
    env = parts[2]
    heading = parts[3]

    results.append({
        'Tension_kN': tension_val,
        'Environment': env,
        'Heading': heading,
        'EndA_Max_kN': round(max(end_a_dyn), 1),
        'EndA_Min_kN': round(min(end_a_dyn), 1),
        'GlobalY_Max_m': round(max(rg_y.Max), 2),
        'MaxStrain_pct': round(max(rg_strain.Max) * 100, 3),
        'MaxBendStress_MPa': round(max_bend_stress_mpa, 1),
        'BendUtil': round(bend_util, 3),
        'CombinedUtil': round(combined_util, 3),
        'Status': 'FAIL' if combined_util > 1.0 else 'OK',
    })

df = pd.DataFrame(results)

# Chart styles
styles = {
    ('001yr', '090deg'): {'color': 'red', 'dash': 'solid', 'name': '1yr 090 deg'},
    ('001yr', '135deg'): {'color': 'darkred', 'dash': 'dash', 'name': '1yr 135 deg'},
    ('95NE', '090deg'): {'color': 'green', 'dash': 'solid', 'name': '95%NE 090 deg'},
    ('95NE', '135deg'): {'color': 'darkgreen', 'dash': 'dash', 'name': '95%NE 135 deg'},
}

def create_chart(df, y_col, title, y_label, hline=None, hline_text=None):
    fig = go.Figure()
    for (env, heading), style in styles.items():
        subset = df[(df['Environment'] == env) & (df['Heading'] == heading)].sort_values('Tension_kN')
        fig.add_trace(go.Scatter(
            x=subset['Tension_kN'], y=subset[y_col],
            mode='lines+markers', name=style['name'],
            line=dict(color=style['color'], dash=style['dash'], width=2),
            marker=dict(size=10),
        ))
    if hline:
        fig.add_hline(y=hline, line_dash='dot', line_color='black',
                      annotation_text=hline_text, annotation_position='right')
    fig.update_layout(
        title=dict(text=title, font=dict(size=14)),
        xaxis_title='Tension (kN)',
        yaxis_title=y_label,
        legend=dict(orientation='h', yanchor='top', y=-0.15, xanchor='center', x=0.5),
        height=380,
        margin=dict(l=60, r=40, t=40, b=80),
    )
    return fig

# Create charts
fig1 = create_chart(df, 'EndA_Max_kN', 'End A Max Tension', 'Tension (kN)')
fig2 = create_chart(df, 'CombinedUtil', 'Combined Utilization (X65)', 'Utilization', 1.0, 'Unity')
fig3 = create_chart(df, 'MaxBendStress_MPa', 'Max Bending Stress', 'Stress (MPa)', X65_YIELD_MPa, 'X65 Yield')
fig4 = create_chart(df, 'MaxStrain_pct', 'Max Strain', 'Strain (%)')
fig5 = create_chart(df, 'GlobalY_Max_m', 'Global Y Max', 'Y (m)')
fig6 = create_chart(df, 'BendUtil', 'Bending Utilization', 'Utilization', 1.0, 'Unity')

# Generate table HTML
table_html = df.to_html(index=False, classes='results-table', border=0)
table_html = table_html.replace('>FAIL<', ' class="fail">FAIL<').replace('>OK<', ' class="ok">OK<')

# Build HTML
html_template = """<!DOCTYPE html>
<html>
<head>
    <title>24in Pipeline Installation - Postprocessing Results</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }}
        h1 {{ text-align: center; color: #333; }}
        h2 {{ color: #333; border-bottom: 2px solid #333; padding-bottom: 5px; }}
        .subtitle {{ text-align: center; color: #666; margin-bottom: 20px; }}
        .section {{ max-width: 1400px; margin: 0 auto 30px auto; }}
        .chart-container {{ display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }}
        .chart {{ background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }}
        .env-table {{ width: 100%; border-collapse: collapse; margin: 10px 0; }}
        .env-table th, .env-table td {{ border: 1px solid #ddd; padding: 8px; text-align: center; }}
        .env-table th {{ background: #4472C4; color: white; }}
        .env-table tr:nth-child(even) {{ background: #f9f9f9; }}
        .results-table {{ width: 100%; border-collapse: collapse; margin: 10px 0; font-size: 12px; }}
        .results-table th, .results-table td {{ border: 1px solid #ddd; padding: 6px; text-align: center; }}
        .results-table th {{ background: #4472C4; color: white; }}
        .results-table tr:nth-child(even) {{ background: #f9f9f9; }}
        .info-box {{ background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; }}
        .legend-note {{ text-align: center; color: #666; font-size: 12px; margin: 10px 0; }}
        .fail {{ color: red; font-weight: bold; }}
        .ok {{ color: green; font-weight: bold; }}
    </style>
</head>
<body>
    <h1>24in Pipeline Installation Analysis</h1>
    <p class="subtitle">LatestWave Period | X65 Steel (Yield = 448.2 MPa)</p>

    <div class="section">
        <h2>Environmental Conditions</h2>
        <div class="info-box">
            <table class="env-table">
                <tr>
                    <th>Parameter</th>
                    <th>1-Year Return</th>
                    <th>95% Non-Exceedance</th>
                </tr>
                <tr><td>Dean Stream Wave Height (m)</td><td>3.0</td><td>1.5</td></tr>
                <tr><td>Wave Period Tp (s)</td><td>10</td><td>9</td></tr>
                <tr><td>Current Speed (m/s)</td><td>0.4</td><td>0.2</td></tr>
                <tr><td>Wind Speed (m/s)</td><td>16.0</td><td>12.6</td></tr>
            </table>
        </div>
    </div>

    <div class="section">
        <h2>Interactive Charts</h2>
        <p class="legend-note">Click legend items to toggle visibility. Double-click to isolate a trace.</p>
        <div class="chart-container">
            <div class="chart" id="chart1"></div>
            <div class="chart" id="chart2"></div>
            <div class="chart" id="chart3"></div>
            <div class="chart" id="chart4"></div>
            <div class="chart" id="chart5"></div>
            <div class="chart" id="chart6"></div>
        </div>
    </div>

    <div class="section">
        <h2>Results Summary Table</h2>
        <div class="info-box">
            {table_html}
        </div>
    </div>

    <script>
        var chart1 = {chart1_json};
        var chart2 = {chart2_json};
        var chart3 = {chart3_json};
        var chart4 = {chart4_json};
        var chart5 = {chart5_json};
        var chart6 = {chart6_json};

        Plotly.newPlot('chart1', chart1.data, chart1.layout);
        Plotly.newPlot('chart2', chart2.data, chart2.layout);
        Plotly.newPlot('chart3', chart3.data, chart3.layout);
        Plotly.newPlot('chart4', chart4.data, chart4.layout);
        Plotly.newPlot('chart5', chart5.data, chart5.layout);
        Plotly.newPlot('chart6', chart6.data, chart6.layout);
    </script>
</body>
</html>
"""

html_content = html_template.format(
    table_html=table_html,
    chart1_json=fig1.to_json(),
    chart2_json=fig2.to_json(),
    chart3_json=fig3.to_json(),
    chart4_json=fig4.to_json(),
    chart5_json=fig5.to_json(),
    chart6_json=fig6.to_json(),
)

output_html = Path(__file__).parent / 'results_interactive.html'
with open(output_html, 'w', encoding='utf-8') as f:
    f.write(html_content)
print(f'Interactive HTML saved to: {output_html}')

# Also save CSV
output_csv = Path(__file__).parent / 'results_summary_latestwave.csv'
df.to_csv(output_csv, index=False)
print(f'CSV saved to: {output_csv}')
