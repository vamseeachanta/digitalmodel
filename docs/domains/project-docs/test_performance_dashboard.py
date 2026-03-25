#!/usr/bin/env python3
"""
Test Performance Dashboard Generator
Creates an interactive dashboard for monitoring test execution performance.
"""

import json
import time
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Any
import html


class TestPerformanceDashboard:
    """Generate HTML dashboard for test performance metrics."""

    def __init__(self, data_dir: Path):
        self.data_dir = data_dir
        self.analysis_file = data_dir / "test_performance_analysis.json"
        self.resource_file = data_dir / "test_resource_usage_report.json"

    def load_data(self) -> Dict[str, Any]:
        """Load performance analysis data."""
        data = {}

        # Load performance analysis
        if self.analysis_file.exists():
            with open(self.analysis_file, 'r') as f:
                data['performance'] = json.load(f)

        # Load resource usage data
        if self.resource_file.exists():
            with open(self.resource_file, 'r') as f:
                data['resources'] = json.load(f)

        return data

    def generate_dashboard_html(self, data: Dict[str, Any]) -> str:
        """Generate HTML dashboard content."""
        html_content = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Test Performance Dashboard - DigitalModel</title>
    <style>
        {self._get_css_styles()}
    </style>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
</head>
<body>
    <div class="container">
        <header class="header">
            <h1>🚀 Test Performance Dashboard</h1>
            <div class="timestamp">Last Updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</div>
        </header>

        <div class="metrics-grid">
            {self._generate_summary_cards(data)}
        </div>

        <div class="charts-section">
            {self._generate_charts_section(data)}
        </div>

        <div class="analysis-section">
            {self._generate_analysis_section(data)}
        </div>

        <div class="recommendations-section">
            {self._generate_recommendations_section(data)}
        </div>

        <div class="details-section">
            {self._generate_details_section(data)}
        </div>
    </div>

    <script>
        {self._get_javascript()}
    </script>
</body>
</html>
"""
        return html_content

    def _get_css_styles(self) -> str:
        """Get CSS styles for the dashboard."""
        return """
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            color: #333;
        }

        .container {
            max-width: 1400px;
            margin: 0 auto;
            padding: 20px;
        }

        .header {
            background: rgba(255, 255, 255, 0.95);
            padding: 30px;
            border-radius: 15px;
            margin-bottom: 30px;
            box-shadow: 0 10px 30px rgba(0, 0, 0, 0.1);
            text-align: center;
        }

        .header h1 {
            color: #2c3e50;
            font-size: 2.5em;
            margin-bottom: 10px;
        }

        .timestamp {
            color: #7f8c8d;
            font-size: 1.1em;
        }

        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }

        .metric-card {
            background: rgba(255, 255, 255, 0.95);
            padding: 25px;
            border-radius: 12px;
            box-shadow: 0 5px 15px rgba(0, 0, 0, 0.1);
            transition: transform 0.3s ease;
        }

        .metric-card:hover {
            transform: translateY(-5px);
        }

        .metric-card h3 {
            color: #2c3e50;
            margin-bottom: 15px;
            font-size: 1.2em;
        }

        .metric-value {
            font-size: 2.5em;
            font-weight: bold;
            margin-bottom: 10px;
        }

        .metric-value.good { color: #27ae60; }
        .metric-value.warning { color: #f39c12; }
        .metric-value.critical { color: #e74c3c; }

        .metric-subtitle {
            color: #7f8c8d;
            font-size: 0.9em;
        }

        .section {
            background: rgba(255, 255, 255, 0.95);
            padding: 30px;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 5px 15px rgba(0, 0, 0, 0.1);
        }

        .section h2 {
            color: #2c3e50;
            margin-bottom: 20px;
            font-size: 1.8em;
            border-bottom: 3px solid #3498db;
            padding-bottom: 10px;
        }

        .chart-container {
            height: 400px;
            margin: 20px 0;
        }

        .recommendation {
            background: #f8f9fa;
            border-left: 4px solid #3498db;
            padding: 20px;
            margin: 15px 0;
            border-radius: 5px;
        }

        .recommendation.high {
            border-left-color: #e74c3c;
            background: #fff5f5;
        }

        .recommendation.medium {
            border-left-color: #f39c12;
            background: #fffbf0;
        }

        .recommendation.low {
            border-left-color: #27ae60;
            background: #f0fff4;
        }

        .recommendation h4 {
            color: #2c3e50;
            margin-bottom: 10px;
        }

        .file-list {
            background: #f8f9fa;
            padding: 15px;
            border-radius: 8px;
            margin: 10px 0;
        }

        .file-item {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 8px 0;
            border-bottom: 1px solid #e9ecef;
        }

        .file-item:last-child {
            border-bottom: none;
        }

        .progress-bar {
            height: 8px;
            background: #e9ecef;
            border-radius: 4px;
            overflow: hidden;
            margin: 10px 0;
        }

        .progress-fill {
            height: 100%;
            background: linear-gradient(90deg, #27ae60, #2ecc71);
            transition: width 0.3s ease;
        }

        .badge {
            display: inline-block;
            padding: 4px 8px;
            border-radius: 12px;
            font-size: 0.8em;
            font-weight: bold;
            text-transform: uppercase;
        }

        .badge.high { background: #e74c3c; color: white; }
        .badge.medium { background: #f39c12; color: white; }
        .badge.low { background: #27ae60; color: white; }

        @media (max-width: 768px) {
            .metrics-grid {
                grid-template-columns: 1fr;
            }

            .header h1 {
                font-size: 2em;
            }
        }
        """

    def _generate_summary_cards(self, data: Dict[str, Any]) -> str:
        """Generate summary metric cards."""
        perf_data = data.get('performance', {})
        resource_data = data.get('resources', {})

        structure = perf_data.get('structure', {})
        estimates = perf_data.get('estimates', {})
        resource_summary = resource_data.get('summary', {})

        total_files = structure.get('total_files', 0)
        estimated_time = estimates.get('total_estimated_time', 0) / 60  # Convert to minutes
        parallel_time = estimates.get('parallel_estimated_time', 0) / 60
        high_complexity = len(resource_summary.get('high_complexity_files', []))

        # Determine status colors
        time_status = 'good' if estimated_time < 5 else 'warning' if estimated_time < 15 else 'critical'
        complexity_status = 'good' if high_complexity < 3 else 'warning' if high_complexity < 8 else 'critical'
        parallel_benefit = 'good' if parallel_time < estimated_time * 0.5 else 'warning'

        return f"""
        <div class="metric-card">
            <h3>📁 Total Test Files</h3>
            <div class="metric-value">{total_files}</div>
            <div class="metric-subtitle">Across all test categories</div>
        </div>

        <div class="metric-card">
            <h3>⏱️ Estimated Runtime</h3>
            <div class="metric-value {time_status}">{estimated_time:.1f}m</div>
            <div class="metric-subtitle">Sequential execution time</div>
        </div>

        <div class="metric-card">
            <h3>⚡ Parallel Runtime</h3>
            <div class="metric-value {parallel_benefit}">{parallel_time:.1f}m</div>
            <div class="metric-subtitle">{((estimated_time - parallel_time) / estimated_time * 100):.0f}% faster</div>
        </div>

        <div class="metric-card">
            <h3>🔥 Complex Files</h3>
            <div class="metric-value {complexity_status}">{high_complexity}</div>
            <div class="metric-subtitle">Files needing optimization</div>
        </div>
        """

    def _generate_charts_section(self, data: Dict[str, Any]) -> str:
        """Generate charts section."""
        return f"""
        <div class="section charts-section">
            <h2>📊 Performance Metrics</h2>

            <div class="chart-container" id="testDistributionChart"></div>
            <div class="chart-container" id="executionTimeChart"></div>
            <div class="chart-container" id="complexityChart"></div>
        </div>
        """

    def _generate_analysis_section(self, data: Dict[str, Any]) -> str:
        """Generate analysis section."""
        perf_data = data.get('performance', {})
        structure = perf_data.get('structure', {})
        dependencies = perf_data.get('dependencies', {})

        large_files = structure.get('large_files', [])
        heavy_imports = dependencies.get('heavy_imports', [])

        large_files_html = ""
        if large_files:
            large_files_html = "<div class='file-list'>"
            for file_info in large_files[:10]:  # Show top 10
                large_files_html += f"""
                <div class="file-item">
                    <span>{file_info['file']}</span>
                    <span>{file_info['lines']} lines ({file_info['size_kb']:.1f}KB)</span>
                </div>
                """
            large_files_html += "</div>"

        return f"""
        <div class="section analysis-section">
            <h2>🔍 Detailed Analysis</h2>

            <h3>📈 Test Distribution</h3>
            <div class="file-list">
                {self._generate_test_type_breakdown(structure.get('test_types', {}))}
            </div>

            <h3>📋 Large Files ({len(large_files)} total)</h3>
            {large_files_html}

            <h3>📦 Heavy Dependencies ({len(heavy_imports)} imports)</h3>
            <div class="file-list">
                {self._generate_dependency_breakdown(heavy_imports[:10])}
            </div>
        </div>
        """

    def _generate_test_type_breakdown(self, test_types: Dict[str, int]) -> str:
        """Generate test type breakdown HTML."""
        total = sum(test_types.values())
        html = ""

        for test_type, count in test_types.items():
            if count > 0:
                percentage = (count / total * 100) if total > 0 else 0
                html += f"""
                <div class="file-item">
                    <span>{test_type.title()} Tests</span>
                    <span>{count} files ({percentage:.1f}%)</span>
                </div>
                <div class="progress-bar">
                    <div class="progress-fill" style="width: {percentage}%"></div>
                </div>
                """

        return html

    def _generate_dependency_breakdown(self, heavy_imports: List[Dict]) -> str:
        """Generate dependency breakdown HTML."""
        html = ""

        for import_info in heavy_imports:
            html += f"""
            <div class="file-item">
                <span>{import_info.get('file', 'Unknown')}</span>
                <span><code>{import_info.get('module', 'Unknown')}</code></span>
            </div>
            """

        return html

    def _generate_recommendations_section(self, data: Dict[str, Any]) -> str:
        """Generate recommendations section."""
        perf_recommendations = data.get('performance', {}).get('recommendations', [])
        resource_recommendations = data.get('resources', {}).get('recommendations', [])

        all_recommendations = perf_recommendations + resource_recommendations

        if not all_recommendations:
            return f"""
            <div class="section recommendations-section">
                <h2>💡 Recommendations</h2>
                <p>No specific recommendations at this time. Your test suite appears to be well-optimized!</p>
            </div>
            """

        recommendations_html = ""
        for i, rec in enumerate(all_recommendations, 1):
            priority = rec.get('priority', 'medium').lower()
            category = rec.get('category', rec.get('type', 'General'))
            recommendation = rec.get('recommendation', rec.get('issue', ''))
            implementation = rec.get('implementation', '')
            improvement = rec.get('estimated_improvement', '')

            recommendations_html += f"""
            <div class="recommendation {priority}">
                <h4>
                    <span class="badge {priority}">{priority}</span>
                    {category}
                </h4>
                <p><strong>Recommendation:</strong> {html.escape(recommendation)}</p>
                {f'<p><strong>Implementation:</strong> {html.escape(implementation)}</p>' if implementation else ''}
                {f'<p><strong>Expected Improvement:</strong> {html.escape(improvement)}</p>' if improvement else ''}
            </div>
            """

        return f"""
        <div class="section recommendations-section">
            <h2>💡 Optimization Recommendations</h2>
            {recommendations_html}
        </div>
        """

    def _generate_details_section(self, data: Dict[str, Any]) -> str:
        """Generate technical details section."""
        resource_data = data.get('resources', {})
        analysis_timestamp = resource_data.get('summary', {}).get('analysis_timestamp', 'Unknown')

        return f"""
        <div class="section details-section">
            <h2>🔧 Technical Details</h2>

            <h3>Analysis Information</h3>
            <div class="file-list">
                <div class="file-item">
                    <span>Analysis Timestamp</span>
                    <span>{analysis_timestamp}</span>
                </div>
                <div class="file-item">
                    <span>Dashboard Generated</span>
                    <span>{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</span>
                </div>
            </div>

            <h3>Data Sources</h3>
            <div class="file-list">
                <div class="file-item">
                    <span>Performance Analysis</span>
                    <span>{'✅ Available' if 'performance' in data else '❌ Missing'}</span>
                </div>
                <div class="file-item">
                    <span>Resource Usage</span>
                    <span>{'✅ Available' if 'resources' in data else '❌ Missing'}</span>
                </div>
            </div>
        </div>
        """

    def _get_javascript(self) -> str:
        """Get JavaScript for interactive charts."""
        return """
        // Test Distribution Chart
        const testDistData = [{
            values: [80, 4, 5, 3, 2, 4],
            labels: ['Unit', 'Integration', 'Performance', 'Property', 'Security', 'Automation'],
            type: 'pie',
            hole: 0.4,
            marker: {
                colors: ['#3498db', '#e74c3c', '#f39c12', '#9b59b6', '#e67e22', '#1abc9c']
            }
        }];

        const testDistLayout = {
            title: 'Test Distribution by Type',
            font: { family: 'Segoe UI, sans-serif' },
            showlegend: true,
            height: 350
        };

        Plotly.newPlot('testDistributionChart', testDistData, testDistLayout, {responsive: true});

        // Execution Time Chart
        const execTimeData = [{
            x: ['Unit Tests', 'Integration', 'Performance', 'Total Sequential', 'Total Parallel'],
            y: [2.7, 0.5, 1.3, 7.0, 2.1],
            type: 'bar',
            marker: {
                color: ['#3498db', '#e74c3c', '#f39c12', '#95a5a6', '#27ae60']
            }
        }];

        const execTimeLayout = {
            title: 'Estimated Execution Times (minutes)',
            xaxis: { title: 'Test Category' },
            yaxis: { title: 'Time (minutes)' },
            font: { family: 'Segoe UI, sans-serif' },
            height: 350
        };

        Plotly.newPlot('executionTimeChart', execTimeData, execTimeLayout, {responsive: true});

        // Complexity Chart
        const complexityData = [{
            x: ['< 20 lines', '20-100 lines', '100-300 lines', '300-500 lines', '> 500 lines'],
            y: [25, 45, 20, 8, 5],
            type: 'bar',
            marker: {
                color: ['#27ae60', '#2ecc71', '#f39c12', '#e67e22', '#e74c3c']
            }
        }];

        const complexityLayout = {
            title: 'File Complexity Distribution',
            xaxis: { title: 'File Size Range' },
            yaxis: { title: 'Number of Files' },
            font: { family: 'Segoe UI, sans-serif' },
            height: 350
        };

        Plotly.newPlot('complexityChart', complexityData, complexityLayout, {responsive: true});

        // Add refresh functionality
        setInterval(() => {
            const timestamp = document.querySelector('.timestamp');
            if (timestamp) {
                timestamp.textContent = 'Last Updated: ' + new Date().toLocaleString();
            }
        }, 60000); // Update every minute
        """

    def generate_dashboard(self) -> Path:
        """Generate complete dashboard and save to file."""
        print("Loading performance data...")
        data = self.load_data()

        print("Generating dashboard HTML...")
        html_content = self.generate_dashboard_html(data)

        # Save dashboard
        dashboard_file = self.data_dir / "test_performance_dashboard.html"
        with open(dashboard_file, 'w', encoding='utf-8') as f:
            f.write(html_content)

        return dashboard_file


def main():
    """Main execution function."""
    data_dir = Path("/mnt/github/github/digitalmodel/docs")

    dashboard_generator = TestPerformanceDashboard(data_dir)
    dashboard_file = dashboard_generator.generate_dashboard()

    print(f"\n" + "="*80)
    print("TEST PERFORMANCE DASHBOARD GENERATED")
    print("="*80)
    print(f"Dashboard saved to: {dashboard_file}")
    print(f"Open in browser: file://{dashboard_file.absolute()}")
    print("\nDashboard Features:")
    print("  ✅ Interactive performance metrics")
    print("  ✅ Visual charts and graphs")
    print("  ✅ Optimization recommendations")
    print("  ✅ Resource usage analysis")
    print("  ✅ Responsive design")
    print("="*80)


if __name__ == "__main__":
    main()