"""
ABOUTME: Export functions for StandardReport to various formats
ABOUTME: Supports HTML (interactive Plotly), JSON, and CSV exports
"""

from pathlib import Path
from typing import Optional, List, Dict
import json
import csv
from datetime import datetime

from .models import StandardReport, ParametricStudy


def export_to_json(
    report: StandardReport,
    filepath: Path,
    indent: int = 2
) -> Path:
    """
    Export StandardReport to JSON file.

    Args:
        report: StandardReport instance
        filepath: Output file path
        indent: JSON indentation level

    Returns:
        Path to created JSON file

    Example:
        >>> report = StandardReport(...)
        >>> export_to_json(report, Path("reports/analysis.json"))
    """
    filepath = Path(filepath)
    filepath.parent.mkdir(parents=True, exist_ok=True)

    report.save_json(filepath)
    return filepath


def export_to_csv(
    report: StandardReport,
    filepath: Path,
    include_parameters: bool = True,
    include_results: bool = True,
    include_validations: bool = True
) -> Path:
    """
    Export StandardReport to CSV file.

    Creates CSV with sections for parameters, results, and validations.

    Args:
        report: StandardReport instance
        filepath: Output file path
        include_parameters: Include parameters section
        include_results: Include results section
        include_validations: Include validations section

    Returns:
        Path to created CSV file

    Example:
        >>> report = StandardReport(...)
        >>> export_to_csv(report, Path("reports/analysis.csv"))
    """
    filepath = Path(filepath)
    filepath.parent.mkdir(parents=True, exist_ok=True)

    with open(filepath, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)

        # Metadata section
        writer.writerow(["METADATA"])
        writer.writerow(["Module", report.metadata.module])
        writer.writerow(["Analysis Type", report.metadata.analysis_type])
        writer.writerow(["Generated At", report.metadata.generated_at.isoformat()])
        writer.writerow(["Execution Time (s)", report.metadata.execution_time_seconds])
        writer.writerow(["Status", report.metadata.status])
        writer.writerow([])

        # Parameters section
        if include_parameters and report.parameters:
            writer.writerow(["PARAMETERS"])
            writer.writerow(["Name", "Value", "Unit", "Description"])
            for param in report.parameters:
                writer.writerow([
                    param.name,
                    param.value,
                    param.unit or "",
                    param.description or ""
                ])
            writer.writerow([])

        # Results section
        if include_results and report.results:
            writer.writerow(["RESULTS"])
            writer.writerow(["Metric", "Value", "Unit", "Description", "Passed", "Threshold"])
            for result in report.results:
                # Handle complex values
                value_str = result.value
                if isinstance(result.value, (list, dict)):
                    value_str = json.dumps(result.value)

                writer.writerow([
                    result.metric_name,
                    value_str,
                    result.unit or "",
                    result.description or "",
                    result.passed if result.passed is not None else "",
                    result.threshold if result.threshold is not None else ""
                ])
            writer.writerow([])

        # Validations section
        if include_validations and report.validations:
            writer.writerow(["VALIDATIONS"])
            writer.writerow(["Check Name", "Passed", "Message", "Severity"])
            for validation in report.validations:
                writer.writerow([
                    validation.check_name,
                    validation.passed,
                    validation.message,
                    validation.severity
                ])
            writer.writerow([])

    return filepath


def export_to_html(
    report: StandardReport,
    filepath: Path,
    title: Optional[str] = None,
    include_plots: bool = True,
    theme: str = "plotly_white"
) -> Path:
    """
    Export StandardReport to interactive HTML file.

    Creates standalone HTML with:
    - Report metadata and summary
    - Parameters table
    - Results table with pass/fail indicators
    - Validation checks
    - Interactive plots (if provided)

    Args:
        report: StandardReport instance
        filepath: Output file path
        title: Custom report title (default: from metadata)
        include_plots: Include plot section
        theme: Plotly theme (plotly_white, plotly_dark, etc.)

    Returns:
        Path to created HTML file

    Example:
        >>> report = StandardReport(...)
        >>> export_to_html(report, Path("reports/analysis.html"))
    """
    filepath = Path(filepath)
    filepath.parent.mkdir(parents=True, exist_ok=True)

    # Determine title
    if title is None:
        title = f"{report.metadata.module} - {report.metadata.analysis_type}"

    # Build HTML
    html = _generate_html_template(report, title, include_plots, theme)

    # Write file
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(html)

    return filepath


def _generate_html_template(
    report: StandardReport,
    title: str,
    include_plots: bool,
    theme: str
) -> str:
    """Generate HTML template for report"""

    # Status color
    status_colors = {
        "success": "#28a745",
        "warning": "#ffc107",
        "error": "#dc3545"
    }
    status_color = status_colors.get(report.metadata.status, "#6c757d")

    # Build HTML
    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{title}</title>
    <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
    <style>
        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}

        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background-color: #f5f7fa;
            color: #2c3e50;
            line-height: 1.6;
            padding: 20px;
        }}

        .container {{
            max-width: 1400px;
            margin: 0 auto;
        }}

        .header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 40px;
            border-radius: 15px;
            margin-bottom: 30px;
            box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        }}

        .header h1 {{
            font-size: 2.5em;
            margin-bottom: 10px;
            font-weight: 600;
        }}

        .header .subtitle {{
            font-size: 1.2em;
            opacity: 0.95;
            font-weight: 300;
        }}

        .metadata-card {{
            background: white;
            padding: 30px;
            border-radius: 12px;
            margin-bottom: 25px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
        }}

        .metadata-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-top: 20px;
        }}

        .metadata-item {{
            padding: 15px;
            background: #f8f9fa;
            border-radius: 8px;
            border-left: 4px solid #667eea;
        }}

        .metadata-label {{
            font-size: 0.85em;
            color: #6c757d;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            margin-bottom: 5px;
        }}

        .metadata-value {{
            font-size: 1.2em;
            font-weight: 600;
            color: #2c3e50;
        }}

        .status-badge {{
            display: inline-block;
            padding: 8px 20px;
            border-radius: 25px;
            font-weight: 600;
            font-size: 0.9em;
            text-transform: uppercase;
            letter-spacing: 0.5px;
            background-color: {status_color};
            color: white;
        }}

        .section {{
            background: white;
            padding: 30px;
            border-radius: 12px;
            margin-bottom: 25px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
        }}

        .section-title {{
            font-size: 1.8em;
            margin-bottom: 20px;
            color: #2c3e50;
            border-bottom: 3px solid #667eea;
            padding-bottom: 10px;
            font-weight: 600;
        }}

        table {{
            width: 100%;
            border-collapse: collapse;
            margin-top: 15px;
        }}

        thead {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
        }}

        th {{
            padding: 15px;
            text-align: left;
            font-weight: 600;
            text-transform: uppercase;
            font-size: 0.85em;
            letter-spacing: 0.5px;
        }}

        td {{
            padding: 12px 15px;
            border-bottom: 1px solid #e9ecef;
        }}

        tbody tr:hover {{
            background-color: #f8f9fa;
        }}

        tbody tr:last-child td {{
            border-bottom: none;
        }}

        .pass {{
            color: #28a745;
            font-weight: 600;
        }}

        .fail {{
            color: #dc3545;
            font-weight: 600;
        }}

        .validation-item {{
            padding: 15px;
            border-radius: 8px;
            margin-bottom: 12px;
            border-left: 4px solid;
        }}

        .validation-passed {{
            background-color: #d4edda;
            border-left-color: #28a745;
        }}

        .validation-failed {{
            background-color: #f8d7da;
            border-left-color: #dc3545;
        }}

        .validation-warning {{
            background-color: #fff3cd;
            border-left-color: #ffc107;
        }}

        .validation-header {{
            font-weight: 600;
            margin-bottom: 5px;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }}

        .validation-message {{
            color: #495057;
            font-size: 0.95em;
        }}

        .severity-badge {{
            padding: 4px 12px;
            border-radius: 15px;
            font-size: 0.75em;
            font-weight: 600;
            text-transform: uppercase;
        }}

        .severity-info {{ background-color: #17a2b8; color: white; }}
        .severity-warning {{ background-color: #ffc107; color: #212529; }}
        .severity-error {{ background-color: #dc3545; color: white; }}
        .severity-critical {{ background-color: #8b0000; color: white; }}

        .plot-container {{
            margin-top: 20px;
            min-height: 400px;
        }}

        .footer {{
            text-align: center;
            margin-top: 40px;
            padding: 20px;
            color: #6c757d;
            font-size: 0.9em;
        }}

        .summary-stats {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 15px;
            margin-bottom: 20px;
        }}

        .stat-card {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            border-radius: 10px;
            text-align: center;
        }}

        .stat-value {{
            font-size: 2.5em;
            font-weight: 700;
            margin-bottom: 5px;
        }}

        .stat-label {{
            font-size: 0.9em;
            opacity: 0.9;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }}
    </style>
</head>
<body>
    <div class="container">
        <!-- Header -->
        <div class="header">
            <h1>{title}</h1>
            <div class="subtitle">
                Generated: {report.metadata.generated_at.strftime('%Y-%m-%d %H:%M:%S')}
                &nbsp;|&nbsp;
                Execution Time: {report.metadata.execution_time_seconds:.2f}s
            </div>
        </div>

        <!-- Metadata -->
        <div class="metadata-card">
            <h2 style="margin-bottom: 20px; color: #2c3e50;">Report Information</h2>
            <div class="metadata-grid">
                <div class="metadata-item">
                    <div class="metadata-label">Module</div>
                    <div class="metadata-value">{report.metadata.module}</div>
                </div>
                <div class="metadata-item">
                    <div class="metadata-label">Analysis Type</div>
                    <div class="metadata-value">{report.metadata.analysis_type}</div>
                </div>
                <div class="metadata-item">
                    <div class="metadata-label">Status</div>
                    <div class="metadata-value">
                        <span class="status-badge">{report.metadata.status}</span>
                    </div>
                </div>
                <div class="metadata-item">
                    <div class="metadata-label">Version</div>
                    <div class="metadata-value">{report.metadata.version}</div>
                </div>
            </div>
        </div>

        <!-- Summary Statistics -->
        <div class="section">
            <h2 class="section-title">Summary Statistics</h2>
            <div class="summary-stats">
                <div class="stat-card">
                    <div class="stat-value">{len(report.parameters)}</div>
                    <div class="stat-label">Parameters</div>
                </div>
                <div class="stat-card">
                    <div class="stat-value">{len(report.results)}</div>
                    <div class="stat-label">Results</div>
                </div>
                <div class="stat-card">
                    <div class="stat-value">{sum(1 for v in report.validations if v.passed)}/{len(report.validations)}</div>
                    <div class="stat-label">Checks Passed</div>
                </div>
                <div class="stat-card">
                    <div class="stat-value">{len(report.plots)}</div>
                    <div class="stat-label">Plots</div>
                </div>
            </div>
        </div>
"""

    # Parameters section
    if report.parameters:
        html += """
        <!-- Parameters -->
        <div class="section">
            <h2 class="section-title">Input Parameters</h2>
            <table>
                <thead>
                    <tr>
                        <th>Parameter</th>
                        <th>Value</th>
                        <th>Unit</th>
                        <th>Description</th>
                    </tr>
                </thead>
                <tbody>
"""
        for param in report.parameters:
            html += f"""
                    <tr>
                        <td><strong>{param.name}</strong></td>
                        <td>{param.value}</td>
                        <td>{param.unit or '-'}</td>
                        <td>{param.description or '-'}</td>
                    </tr>
"""
        html += """
                </tbody>
            </table>
        </div>
"""

    # Results section
    if report.results:
        html += """
        <!-- Results -->
        <div class="section">
            <h2 class="section-title">Analysis Results</h2>
            <table>
                <thead>
                    <tr>
                        <th>Metric</th>
                        <th>Value</th>
                        <th>Unit</th>
                        <th>Description</th>
                        <th>Status</th>
                    </tr>
                </thead>
                <tbody>
"""
        for result in report.results:
            # Format value
            if isinstance(result.value, float):
                value_str = f"{result.value:.4f}"
            elif isinstance(result.value, (list, dict)):
                value_str = "(complex)"
            else:
                value_str = str(result.value)

            # Pass/fail indicator
            status_html = ""
            if result.passed is not None:
                if result.passed:
                    status_html = '<span class="pass">✓ PASS</span>'
                else:
                    status_html = '<span class="fail">✗ FAIL</span>'

                if result.threshold is not None:
                    status_html += f" (threshold: {result.threshold})"

            html += f"""
                    <tr>
                        <td><strong>{result.metric_name}</strong></td>
                        <td>{value_str}</td>
                        <td>{result.unit or '-'}</td>
                        <td>{result.description or '-'}</td>
                        <td>{status_html or '-'}</td>
                    </tr>
"""
        html += """
                </tbody>
            </table>
        </div>
"""

    # Validations section
    if report.validations:
        html += """
        <!-- Validations -->
        <div class="section">
            <h2 class="section-title">Validation Checks</h2>
"""
        for validation in report.validations:
            if validation.passed:
                validation_class = "validation-passed"
            elif validation.severity == "warning":
                validation_class = "validation-warning"
            else:
                validation_class = "validation-failed"

            severity_class = f"severity-{validation.severity}"

            html += f"""
            <div class="validation-item {validation_class}">
                <div class="validation-header">
                    <span>{validation.check_name}</span>
                    <span class="severity-badge {severity_class}">{validation.severity}</span>
                </div>
                <div class="validation-message">{validation.message}</div>
            </div>
"""
        html += """
        </div>
"""

    # Footer
    html += f"""
        <!-- Footer -->
        <div class="footer">
            <p>Generated by <strong>digitalmodel</strong> reporting framework</p>
            <p style="margin-top: 5px; font-size: 0.85em;">
                Report Version {report.metadata.version} |
                {report.metadata.generated_at.strftime('%Y-%m-%d %H:%M:%S')}
            </p>
        </div>
    </div>
</body>
</html>
"""

    return html


def export_all_formats(
    report: StandardReport,
    base_filepath: Path,
    formats: Optional[List[str]] = None
) -> Dict[str, Path]:
    """
    Export report to multiple formats.

    Args:
        report: StandardReport instance
        base_filepath: Base file path (without extension)
        formats: List of formats ('html', 'json', 'csv'). Default: all

    Returns:
        Dictionary mapping format to output filepath

    Example:
        >>> report = StandardReport(...)
        >>> files = export_all_formats(report, Path("reports/analysis"))
        >>> # Creates: analysis.html, analysis.json, analysis.csv
    """
    if formats is None:
        formats = ['html', 'json', 'csv']

    base_filepath = Path(base_filepath)
    base_filepath.parent.mkdir(parents=True, exist_ok=True)

    output_files = {}

    if 'html' in formats:
        html_path = base_filepath.with_suffix('.html')
        export_to_html(report, html_path)
        output_files['html'] = html_path

    if 'json' in formats:
        json_path = base_filepath.with_suffix('.json')
        export_to_json(report, json_path)
        output_files['json'] = json_path

    if 'csv' in formats:
        csv_path = base_filepath.with_suffix('.csv')
        export_to_csv(report, csv_path)
        output_files['csv'] = csv_path

    return output_files


def export_parametric_study_html(
    study: ParametricStudy,
    filepath: Path,
    comparison_metrics: Optional[List[str]] = None
) -> Path:
    """
    Export parametric study to HTML with comparison table.

    Args:
        study: ParametricStudy instance
        filepath: Output HTML file path
        comparison_metrics: Metrics to include in comparison (None = all)

    Returns:
        Path to created HTML file
    """
    filepath = Path(filepath)
    filepath.parent.mkdir(parents=True, exist_ok=True)

    # Get comparison table
    table = study.get_comparison_table(comparison_metrics)

    # Build HTML (simplified template for parametric study)
    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Parametric Study: {study.study_name}</title>
    <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 40px; background: #f5f5f5; }}
        .container {{ max-width: 1200px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }}
        h1 {{ color: #333; border-bottom: 3px solid #667eea; padding-bottom: 10px; }}
        h2 {{ color: #555; margin-top: 30px; }}
        table {{ width: 100%; border-collapse: collapse; margin: 20px 0; }}
        th, td {{ padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }}
        th {{ background: #667eea; color: white; font-weight: 600; }}
        tr:hover {{ background-color: #f5f5f5; }}
        .info {{ background: #e7f3ff; padding: 15px; border-left: 4px solid #2196F3; margin: 20px 0; border-radius: 5px; }}
    </style>
</head>
<body>
    <div class="container">
        <h1>Parametric Study: {study.study_name}</h1>

        <div class="info">
            <strong>Description:</strong> {study.description or 'No description provided'}<br>
            <strong>Parameter Varied:</strong> {study.parameter_name}<br>
            <strong>Number of Cases:</strong> {len(study.reports)}<br>
            <strong>Created:</strong> {study.created_at.strftime('%Y-%m-%d %H:%M:%S')}
        </div>

        <h2>Comparison Table</h2>
        <table>
            <thead>
                <tr>
"""

    # Table headers
    for key in table.keys():
        html += f"<th>{key}</th>\n"

    html += """
                </tr>
            </thead>
            <tbody>
"""

    # Table rows
    num_rows = len(table[list(table.keys())[0]])
    for i in range(num_rows):
        html += "<tr>\n"
        for key in table.keys():
            value = table[key][i]
            if isinstance(value, float):
                html += f"<td>{value:.4f}</td>\n"
            else:
                html += f"<td>{value}</td>\n"
        html += "</tr>\n"

    html += """
            </tbody>
        </table>
    </div>
</body>
</html>
"""

    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(html)

    return filepath
