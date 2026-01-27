#!/usr/bin/env python
"""Generate interactive HTML report for pipeline installation analysis.

Processes OrcaFlex simulation results and generates:
- Summary CSV with key metrics
- Interactive HTML report with Plotly charts
- Arc-length distributed CSV files (RangeGraphs)
- Grouped chart pages by load condition and tension level
"""
import OrcFxAPI
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from pathlib import Path

# X65 yield strength
X65_YIELD_MPa = 65 * 6.89476  # 448.2 MPa

# Find sim files in runs/ directory
runs_dir = Path(__file__).parent.parent / 'runs'
sim_files = sorted(runs_dir.glob('*.sim'))

# Create output directory for rangegraphs
rangegraph_dir = Path(__file__).parent / 'rangegraphs'
rangegraph_dir.mkdir(exist_ok=True)

print(f'Processing {len(sim_files)} sim files...')

results = []

# Model state constants
MS_RESET = 0
MS_CALCULATING_STATICS = 1
MS_IN_STATIC_STATE = 2
MS_SIMULATING = 3
MS_SIMULATION_COMPLETE = 4

# Store rangegraph data for chart generation
rangegraph_data = []

for sim_file in sim_files:
    print(f'  Processing: {sim_file.name}')
    model = OrcFxAPI.Model(str(sim_file))
    pipeline = model['pipeline']

    # Check if model has dynamic results
    has_dynamic = model.state == MS_SIMULATION_COMPLETE

    period_static = OrcFxAPI.pnStaticState

    if has_dynamic:
        period_rg = OrcFxAPI.pnLatestWave
        end_a_dyn = pipeline.TimeHistory('Effective Tension', period_rg, objectExtra=OrcFxAPI.oeEndA)
        end_a_max = max(end_a_dyn)
        end_a_min = min(end_a_dyn)
    else:
        # Static-only: use static state for RangeGraph
        period_rg = period_static
        end_a_static = pipeline.TimeHistory('Effective Tension', period_static, objectExtra=OrcFxAPI.oeEndA)[0]
        end_a_max = end_a_static
        end_a_min = end_a_static
        print(f'    (Static-only results)')

    # Extract RangeGraphs for arc-length distributions
    rg_tension = pipeline.RangeGraph('Effective Tension', period_rg)
    rg_y = pipeline.RangeGraph('y', period_rg)
    rg_strain = pipeline.RangeGraph('Direct Tensile Strain', period_rg)
    rg_bend_stress = pipeline.RangeGraph('Max Bending Stress', period_rg)
    rg_tens_stress = pipeline.RangeGraph('Direct Tensile Stress', period_rg)

    # Use tension arc length as reference (typically most complete)
    arc_length = np.array(rg_tension.X)

    # Interpolate y values to match tension arc length (y may have 1 fewer point)
    y_arc = np.array(rg_y.X)
    if len(y_arc) != len(arc_length):
        if has_dynamic:
            y_min_interp = np.interp(arc_length, y_arc, np.array(rg_y.Min))
            y_max_interp = np.interp(arc_length, y_arc, np.array(rg_y.Max))
        else:
            # For static state, use Mean as Min/Max are invalid placeholders
            y_min_interp = np.interp(arc_length, y_arc, np.array(rg_y.Mean))
            y_max_interp = y_min_interp.copy()
    else:
        if has_dynamic:
            y_min_interp = np.array(rg_y.Min)
            y_max_interp = np.array(rg_y.Max)
        else:
            # For static state, use Mean as Min/Max are invalid placeholders
            y_min_interp = np.array(rg_y.Mean)
            y_max_interp = y_min_interp.copy()

    # For static state, Min/Max contain invalid placeholder values - use Mean instead
    if has_dynamic:
        max_bend_stress_mpa = max(rg_bend_stress.Max) / 1000
        max_tens_stress_mpa = max(rg_tens_stress.Max) / 1000
    else:
        max_bend_stress_mpa = max(rg_bend_stress.Mean) / 1000
        max_tens_stress_mpa = max(rg_tens_stress.Mean) / 1000

    bend_util = max_bend_stress_mpa / X65_YIELD_MPa
    combined_util = (max_bend_stress_mpa + max_tens_stress_mpa) / X65_YIELD_MPa

    parts = sim_file.stem.split('_')
    tension_val = int(parts[0].replace('kN', ''))
    env = parts[2]
    heading = parts[3]

    # Save arc-length distributed CSV
    # For static state, use Mean as Min/Max contain invalid placeholders
    if has_dynamic:
        tension_min = np.array(rg_tension.Min) / 1000
        tension_max = np.array(rg_tension.Max) / 1000
        strain_min = np.array(rg_strain.Min) * 100
        strain_max = np.array(rg_strain.Max) * 100
        bend_min = np.array(rg_bend_stress.Min) / 1000
        bend_max = np.array(rg_bend_stress.Max) / 1000
        tens_min = np.array(rg_tens_stress.Min) / 1000
        tens_max = np.array(rg_tens_stress.Max) / 1000
    else:
        tension_min = np.array(rg_tension.Mean) / 1000
        tension_max = tension_min.copy()
        strain_min = np.array(rg_strain.Mean) * 100
        strain_max = strain_min.copy()
        bend_min = np.array(rg_bend_stress.Mean) / 1000
        bend_max = bend_min.copy()
        tens_min = np.array(rg_tens_stress.Mean) / 1000
        tens_max = tens_min.copy()

    rg_df = pd.DataFrame({
        'ArcLength_m': arc_length,
        'Tension_Min_kN': tension_min,
        'Tension_Max_kN': tension_max,
        'Tension_Mean_kN': np.array(rg_tension.Mean) / 1000,
        'Y_Min_m': y_min_interp,
        'Y_Max_m': y_max_interp,
        'Strain_Min_pct': strain_min,
        'Strain_Max_pct': strain_max,
        'BendStress_Min_MPa': bend_min,
        'BendStress_Max_MPa': bend_max,
        'TensStress_Min_MPa': tens_min,
        'TensStress_Max_MPa': tens_max,
    })

    # Calculate combined utilization along arc length
    rg_df['CombinedUtil'] = (rg_df['BendStress_Max_MPa'] + rg_df['TensStress_Max_MPa']) / X65_YIELD_MPa

    rg_csv_path = rangegraph_dir / f'{sim_file.stem}_rangegraph.csv'
    rg_df.to_csv(rg_csv_path, index=False)

    # Store data for chart generation
    rangegraph_data.append({
        'filename': sim_file.stem,
        'tension_kN': tension_val,
        'env': env,
        'heading': heading,
        'df': rg_df,
        'has_dynamic': has_dynamic,
    })

    # Calculate max strain using appropriate source
    if has_dynamic:
        max_strain_pct = max(rg_strain.Max) * 100
    else:
        max_strain_pct = max(rg_strain.Mean) * 100

    results.append({
        'Tension_kN': tension_val,
        'Environment': env,
        'Heading': heading,
        'EndA_Max_kN': round(end_a_max, 1),
        'EndA_Min_kN': round(end_a_min, 1),
        'GlobalY_Max_m': round(max(y_max_interp), 2),
        'MaxStrain_pct': round(max_strain_pct, 3),
        'MaxBendStress_MPa': round(max_bend_stress_mpa, 1),
        'BendUtil': round(bend_util, 3),
        'CombinedUtil': round(combined_util, 3),
        'Status': 'FAIL' if combined_util > 1.0 else 'OK',
        'HasDynamic': has_dynamic,
    })

print(f'Saved {len(rangegraph_data)} rangegraph CSVs to: {rangegraph_dir}')

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

# ============================================================================
# ARC-LENGTH DISTRIBUTION CHARTS
# ============================================================================

# Color palettes for different groupings
tension_colors = {
    900: '#1f77b4',    # blue
    1250: '#2ca02c',   # green
    1500: '#ff7f0e',   # orange
    2000: '#d62728',   # red
    2500: '#9467bd',   # purple
}

env_colors = {
    ('001yr', '090deg'): '#e41a1c',   # red
    ('001yr', '135deg'): '#984ea3',   # purple
    ('95NE', '090deg'): '#4daf4a',    # green
    ('95NE', '135deg'): '#377eb8',    # blue
}


def create_rangegraph_chart(data_list, y_col, title, y_label, xlim=None, hline=None, hline_text=None, color_by='tension'):
    """Create arc-length distribution chart with Min/Max envelope or Max only."""
    fig = go.Figure()

    for item in data_list:
        rg_df = item['df']
        if color_by == 'tension':
            color = tension_colors.get(item['tension_kN'], '#888888')
            name = f"{item['tension_kN']} kN"
        else:
            color = env_colors.get((item['env'], item['heading']), '#888888')
            name = f"{item['env']} {item['heading']}"

        x = rg_df['ArcLength_m']

        # Check if this is a Min/Max envelope variable
        if y_col.endswith('_Max') and y_col.replace('_Max', '_Min') in rg_df.columns:
            y_min_col = y_col.replace('_Max', '_Min')
            # Add Max line
            fig.add_trace(go.Scatter(
                x=x, y=rg_df[y_col],
                mode='lines', name=f'{name} Max',
                line=dict(color=color, width=1.5),
                legendgroup=name,
            ))
            # Add Min line with dash
            fig.add_trace(go.Scatter(
                x=x, y=rg_df[y_min_col],
                mode='lines', name=f'{name} Min',
                line=dict(color=color, width=1, dash='dash'),
                legendgroup=name,
                showlegend=False,
            ))
        else:
            # Single line
            fig.add_trace(go.Scatter(
                x=x, y=rg_df[y_col],
                mode='lines', name=name,
                line=dict(color=color, width=1.5),
            ))

    if hline is not None:
        fig.add_hline(y=hline, line_dash='dot', line_color='black',
                      annotation_text=hline_text, annotation_position='top right')

    layout_kwargs = dict(
        title=dict(text=title, font=dict(size=14)),
        xaxis_title='Arc Length (m)',
        yaxis_title=y_label,
        legend=dict(orientation='h', yanchor='top', y=-0.12, xanchor='center', x=0.5),
        height=400,
        margin=dict(l=60, r=40, t=50, b=80),
    )

    if xlim:
        layout_kwargs['xaxis'] = dict(range=[0, xlim])

    fig.update_layout(**layout_kwargs)
    return fig


def create_dual_view_charts(data_list, color_by='tension'):
    """Create all chart types with full length and critical zone views."""
    charts = {}

    chart_configs = [
        ('tension', 'Tension_Max_kN', 'Effective Tension', 'Tension (kN)', None, None),
        ('bend_stress', 'BendStress_Max_MPa', 'Max Bending Stress', 'Stress (MPa)', X65_YIELD_MPa, 'X65 Yield'),
        ('strain', 'Strain_Max_pct', 'Direct Tensile Strain', 'Strain (%)', None, None),
        ('y_pos', 'Y_Max_m', 'Global Y Position', 'Y (m)', None, None),
        ('util', 'CombinedUtil', 'Combined Utilization', 'Utilization', 1.0, 'Unity'),
    ]

    for key, y_col, title, y_label, hline, hline_text in chart_configs:
        # Full length view
        charts[f'{key}_full'] = create_rangegraph_chart(
            data_list, y_col, f'{title} - Full Length', y_label,
            xlim=None, hline=hline, hline_text=hline_text, color_by=color_by
        )
        # Critical zone view (0-300m)
        charts[f'{key}_critical'] = create_rangegraph_chart(
            data_list, y_col, f'{title} - Critical Zone (0-300m)', y_label,
            xlim=300, hline=hline, hline_text=hline_text, color_by=color_by
        )

    return charts


def generate_chart_page(charts, title, subtitle, output_path):
    """Generate HTML page with all charts."""
    chart_pairs = [
        ('tension_full', 'tension_critical', 'Effective Tension'),
        ('bend_stress_full', 'bend_stress_critical', 'Max Bending Stress'),
        ('strain_full', 'strain_critical', 'Direct Tensile Strain'),
        ('y_pos_full', 'y_pos_critical', 'Global Y Position'),
        ('util_full', 'util_critical', 'Combined Utilization'),
    ]

    chart_divs = ""
    chart_scripts = ""
    chart_data_vars = ""
    for i, (full_key, crit_key, label) in enumerate(chart_pairs):
        chart_divs += f'''
        <div class="chart-row">
            <h3>{label}</h3>
            <div class="chart-pair">
                <div class="chart" id="chart{i*2+1}"></div>
                <div class="chart" id="chart{i*2+2}"></div>
            </div>
        </div>'''
        full_json = charts[full_key].to_json()
        crit_json = charts[crit_key].to_json()
        chart_data_vars += f'''
        var chart{i*2+1}_data = {full_json};
        var chart{i*2+2}_data = {crit_json};'''
        chart_scripts += f'''
        Plotly.newPlot('chart{i*2+1}', chart{i*2+1}_data.data, chart{i*2+1}_data.layout);
        Plotly.newPlot('chart{i*2+2}', chart{i*2+2}_data.data, chart{i*2+2}_data.layout);'''

    html = f'''<!DOCTYPE html>
<html>
<head>
    <title>{title}</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }}
        h1 {{ text-align: center; color: #333; }}
        h2 {{ color: #333; border-bottom: 2px solid #333; padding-bottom: 5px; }}
        h3 {{ color: #555; margin: 15px 0 10px 0; }}
        .subtitle {{ text-align: center; color: #666; margin-bottom: 20px; }}
        .section {{ max-width: 1600px; margin: 0 auto 30px auto; }}
        .chart-row {{ background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; }}
        .chart-pair {{ display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }}
        .chart {{ min-height: 400px; }}
        .nav {{ text-align: center; margin: 20px 0; }}
        .nav a {{ margin: 0 10px; color: #4472C4; text-decoration: none; }}
        .nav a:hover {{ text-decoration: underline; }}
    </style>
</head>
<body>
    <h1>{title}</h1>
    <p class="subtitle">{subtitle}</p>

    <div class="nav">
        <a href="results_interactive.html">Summary Report</a> |
        <strong>By Load Condition:</strong>
        <a href="charts_env_001yr_090deg.html">1yr 090</a>
        <a href="charts_env_001yr_135deg.html">1yr 135</a>
        <a href="charts_env_95NE_090deg.html">95%NE 090</a>
        <a href="charts_env_95NE_135deg.html">95%NE 135</a> |
        <strong>By Tension:</strong>
        <a href="charts_tension_900kN.html">900kN</a>
        <a href="charts_tension_1250kN.html">1250kN</a>
        <a href="charts_tension_1500kN.html">1500kN</a>
        <a href="charts_tension_2000kN.html">2000kN</a>
        <a href="charts_tension_2500kN.html">2500kN</a>
    </div>

    <div class="section">
        {chart_divs}
    </div>

    <script>
        {chart_data_vars}
        {chart_scripts}
    </script>
</body>
</html>'''

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html)


# Generate charts grouped by load condition (env + heading)
print('Generating arc-length distribution charts...')
load_conditions = [('001yr', '090deg'), ('001yr', '135deg'), ('95NE', '090deg'), ('95NE', '135deg')]

for env, heading in load_conditions:
    filtered = [d for d in rangegraph_data if d['env'] == env and d['heading'] == heading]
    if not filtered:
        continue

    charts = create_dual_view_charts(filtered, color_by='tension')
    title = f'Arc-Length Distributions - {env} {heading}'
    subtitle = f'Comparing all tension levels | LatestWave Period'
    output_path = Path(__file__).parent / f'charts_env_{env}_{heading}.html'
    generate_chart_page(charts, title, subtitle, output_path)
    print(f'  Saved: {output_path.name}')

# Generate charts grouped by tension level
tensions = sorted(set(d['tension_kN'] for d in rangegraph_data))

for tension in tensions:
    filtered = [d for d in rangegraph_data if d['tension_kN'] == tension]
    if not filtered:
        continue

    charts = create_dual_view_charts(filtered, color_by='env')
    title = f'Arc-Length Distributions - {tension} kN'
    subtitle = f'Comparing all load conditions | LatestWave Period'
    output_path = Path(__file__).parent / f'charts_tension_{tension}kN.html'
    generate_chart_page(charts, title, subtitle, output_path)
    print(f'  Saved: {output_path.name}')

print('Post-processing complete!')
