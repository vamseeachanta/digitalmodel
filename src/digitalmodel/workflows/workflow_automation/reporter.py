#!/usr/bin/env python3
"""
ABOUTME: HTML report generation for workflow execution results with interactive
Plotly visualizations and comprehensive analysis summaries.
"""

from pathlib import Path
from typing import Optional
import json
from datetime import datetime

from .models import WorkflowResult


class WorkflowHTMLReporter:
    """
    Generate interactive HTML reports for workflow execution results

    Creates comprehensive reports with:
    - Workflow execution summary
    - Task status visualizations (Plotly)
    - Performance metrics and timing analysis
    - Success/failure breakdown
    - Key result outputs
    """

    def __init__(self, output_dir: Optional[str] = None):
        """
        Initialize HTML reporter

        Args:
            output_dir: Directory for HTML reports (default: ./reports/)
        """
        self.output_dir = Path(output_dir) if output_dir else Path("./reports")
        self.output_dir.mkdir(parents=True, exist_ok=True)

    def generate_report(
        self,
        result: WorkflowResult,
        output_file: Optional[str] = None
    ) -> str:
        """
        Generate comprehensive HTML report for workflow result

        Args:
            result: WorkflowResult to generate report for
            output_file: Output filename (auto-generated if None)

        Returns:
            Path to generated HTML file
        """
        if output_file is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_file = f"workflow_{result.workflow_id}_{timestamp}.html"

        output_path = self.output_dir / output_file

        html_content = self._generate_html_content(result)

        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(html_content)

        return str(output_path)

    def _generate_html_content(self, result: WorkflowResult) -> str:
        """
        Generate complete HTML content for report

        Args:
            result: WorkflowResult to report on

        Returns:
            Complete HTML document as string
        """
        # Generate Plotly visualizations
        task_status_chart = self._create_task_status_chart(result)
        timeline_chart = self._create_timeline_chart(result)

        # Build HTML document
        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Workflow Report: {result.workflow_name}</title>
    <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background-color: #f5f5f5;
        }}
        .header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 30px;
        }}
        .header h1 {{
            margin: 0 0 10px 0;
        }}
        .header .subtitle {{
            opacity: 0.9;
            font-size: 16px;
        }}
        .summary {{
            background: white;
            padding: 25px;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            margin-bottom: 30px;
        }}
        .summary h2 {{
            margin-top: 0;
            color: #333;
            border-bottom: 2px solid #667eea;
            padding-bottom: 10px;
        }}
        .metric {{
            display: inline-block;
            margin: 10px 20px 10px 0;
        }}
        .metric-label {{
            font-size: 14px;
            color: #666;
            display: block;
        }}
        .metric-value {{
            font-size: 24px;
            font-weight: bold;
            color: #333;
        }}
        .status-success {{
            color: #10b981;
        }}
        .status-partial {{
            color: #f59e0b;
        }}
        .status-failed {{
            color: #ef4444;
        }}
        .chart-container {{
            background: white;
            padding: 25px;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            margin-bottom: 30px;
        }}
        .chart-container h2 {{
            margin-top: 0;
            color: #333;
            border-bottom: 2px solid #667eea;
            padding-bottom: 10px;
        }}
        .outputs {{
            background: white;
            padding: 25px;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        .outputs h2 {{
            margin-top: 0;
            color: #333;
            border-bottom: 2px solid #667eea;
            padding-bottom: 10px;
        }}
        .output-item {{
            padding: 10px;
            margin: 10px 0;
            background: #f9fafb;
            border-left: 4px solid #667eea;
            border-radius: 4px;
        }}
        .output-key {{
            font-weight: bold;
            color: #555;
        }}
        .output-value {{
            color: #333;
            font-family: 'Courier New', monospace;
        }}
        .footer {{
            text-align: center;
            margin-top: 30px;
            padding: 20px;
            color: #666;
            font-size: 14px;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>{result.workflow_name}</h1>
        <div class="subtitle">Workflow ID: {result.workflow_id}</div>
        <div class="subtitle">Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}</div>
    </div>

    <div class="summary">
        <h2>Execution Summary</h2>
        <div class="metric">
            <span class="metric-label">Status</span>
            <span class="metric-value status-{result.status}">{result.status.upper()}</span>
        </div>
        <div class="metric">
            <span class="metric-label">Duration</span>
            <span class="metric-value">{result.duration_seconds:.1f}s</span>
        </div>
        <div class="metric">
            <span class="metric-label">Success Rate</span>
            <span class="metric-value">{result.success_rate:.1f}%</span>
        </div>
        <div class="metric">
            <span class="metric-label">Total Tasks</span>
            <span class="metric-value">{len(result.task_statuses)}</span>
        </div>
        {self._generate_failed_tasks_html(result)}
    </div>

    <div class="chart-container">
        <h2>Task Status Distribution</h2>
        <div id="task-status-chart"></div>
    </div>

    {self._generate_outputs_html(result)}

    <div class="footer">
        Generated by DigitalModel Workflow Automation Module<br>
        <a href="https://github.com/ruvnet/digitalmodel">github.com/ruvnet/digitalmodel</a>
    </div>

    <script>
        {task_status_chart}
    </script>
</body>
</html>"""

        return html

    def _create_task_status_chart(self, result: WorkflowResult) -> str:
        """
        Create Plotly pie chart for task status distribution

        Args:
            result: WorkflowResult

        Returns:
            JavaScript code for Plotly chart
        """
        # Count status occurrences
        status_counts = {}
        for status in result.task_statuses.values():
            status_str = status.value if hasattr(status, 'value') else str(status)
            status_counts[status_str] = status_counts.get(status_str, 0) + 1

        # Prepare data for Plotly
        labels = list(status_counts.keys())
        values = list(status_counts.values())

        # Color mapping for statuses
        color_map = {
            'completed': '#10b981',
            'failed': '#ef4444',
            'cached': '#3b82f6',
            'skipped': '#94a3b8',
            'pending': '#d1d5db',
        }

        colors = [color_map.get(label, '#94a3b8') for label in labels]

        plotly_code = f"""
        var data = [{{
            values: {values},
            labels: {labels},
            type: 'pie',
            marker: {{
                colors: {colors}
            }},
            textinfo: 'label+percent',
            textposition: 'outside',
            hovertemplate: '<b>%{{label}}</b><br>Count: %{{value}}<br>Percentage: %{{percent}}<extra></extra>',
        }}];

        var layout = {{
            height: 400,
            showlegend: true,
            paper_bgcolor: 'rgba(0,0,0,0)',
            plot_bgcolor: 'rgba(0,0,0,0)',
        }};

        var config = {{
            responsive: true,
            displayModeBar: true,
            displaylogo: false,
        }};

        Plotly.newPlot('task-status-chart', data, layout, config);
        """

        return plotly_code

    def _create_timeline_chart(self, result: WorkflowResult) -> str:
        """
        Create Plotly timeline chart for task execution

        Args:
            result: WorkflowResult

        Returns:
            JavaScript code for Plotly timeline
        """
        # This would require task start/end times
        # Placeholder for future implementation
        return ""

    def _generate_failed_tasks_html(self, result: WorkflowResult) -> str:
        """Generate HTML for failed tasks section"""
        failed = result.get_failed_tasks()

        if not failed:
            return ""

        failed_html = '<div class="metric"><span class="metric-label">Failed Tasks</span><span class="metric-value status-failed">'
        failed_html += ', '.join(failed)
        failed_html += '</span></div>'

        return failed_html

    def _generate_outputs_html(self, result: WorkflowResult) -> str:
        """Generate HTML for workflow outputs section"""
        if not result.outputs:
            return ""

        outputs_html = '<div class="outputs"><h2>Key Outputs</h2>'

        for key, value in result.outputs.items():
            # Format value for display
            if isinstance(value, (int, float)):
                formatted_value = f"{value:,.4f}" if isinstance(value, float) else f"{value:,}"
            else:
                formatted_value = str(value)

            outputs_html += f"""
            <div class="output-item">
                <span class="output-key">{key}:</span>
                <span class="output-value">{formatted_value}</span>
            </div>
            """

        outputs_html += '</div>'

        return outputs_html


def generate_workflow_report(
    result: WorkflowResult,
    output_file: Optional[str] = None,
    output_dir: Optional[str] = None
) -> str:
    """
    Convenience function to generate workflow HTML report

    Args:
        result: WorkflowResult to report on
        output_file: Output filename (auto-generated if None)
        output_dir: Output directory (default: ./reports/)

    Returns:
        Path to generated HTML file
    """
    reporter = WorkflowHTMLReporter(output_dir=output_dir)
    return reporter.generate_report(result, output_file=output_file)
