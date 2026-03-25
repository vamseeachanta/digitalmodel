# ABOUTME: Interactive agent health monitoring dashboard using Plotly
# Generates HTML reports with agent metrics, success rates, and cost analysis
"""
Interactive Agent Health Monitoring Dashboard

Generates comprehensive HTML dashboards from agent metrics CSV data.
Follows HTML_REPORTING_STANDARDS.md - interactive Plotly visualizations only.
"""

import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.express as px
from pathlib import Path
from datetime import datetime, timedelta
from typing import Optional, Dict, List
import webbrowser


class AgentDashboard:
    """
    Agent Health Monitoring Dashboard Generator

    Creates interactive Plotly dashboards from agent metrics CSV data.
    Supports filtering, success rate analysis, cost tracking, and failure trends.
    """

    # Agent categories for organization
    AGENT_CATEGORIES = {
        "Core Development": ["coder", "reviewer", "tester", "planner", "researcher"],
        "Swarm Coordination": [
            "hierarchical-coordinator", "mesh-coordinator", "adaptive-coordinator",
            "collective-intelligence-coordinator", "swarm-memory-manager"
        ],
        "Consensus & Distributed": [
            "byzantine-coordinator", "raft-manager", "gossip-coordinator",
            "consensus-builder", "crdt-synchronizer", "quorum-manager", "security-manager"
        ],
        "Performance & Optimization": [
            "perf-analyzer", "performance-benchmarker", "task-orchestrator",
            "memory-coordinator", "smart-agent"
        ],
        "GitHub & Repository": [
            "github-modes", "pr-manager", "code-review-swarm", "issue-tracker",
            "release-manager", "workflow-automation", "project-board-sync",
            "repo-architect", "multi-repo-swarm"
        ],
        "SPARC Methodology": [
            "sparc-coord", "sparc-coder", "specification", "pseudocode",
            "architecture", "refinement"
        ],
        "Specialized Development": [
            "backend-dev", "mobile-dev", "ml-developer", "cicd-engineer",
            "api-docs", "system-architect", "code-analyzer", "base-template-generator"
        ],
        "Testing & Validation": ["tdd-london-swarm", "production-validator"],
        "Migration & Planning": ["migration-planner", "swarm-init"]
    }

    def __init__(self, data_dir: Path = None, output_dir: Path = None):
        """
        Initialize dashboard generator

        Args:
            data_dir: Directory containing agent metrics CSV files
            output_dir: Directory for HTML output
        """
        self.data_dir = data_dir or Path("data/agent_metrics")
        self.output_dir = output_dir or Path("reports")
        self.df: Optional[pd.DataFrame] = None

    def load_data(self, csv_file: str = "agent_metrics.csv") -> pd.DataFrame:
        """
        Load agent metrics from CSV

        Args:
            csv_file: CSV filename in data_dir

        Returns:
            DataFrame with parsed timestamps and proper dtypes
        """
        csv_path = self.data_dir / csv_file

        if not csv_path.exists():
            raise FileNotFoundError(f"Metrics file not found: {csv_path}")

        # Load CSV with proper types
        self.df = pd.read_csv(csv_path)
        self.df['timestamp'] = pd.to_datetime(self.df['timestamp'])
        self.df['success'] = self.df['success'].astype(bool)
        self.df['duration_ms'] = pd.to_numeric(self.df['duration_ms'])
        self.df['tokens_used'] = pd.to_numeric(self.df['tokens_used'])
        self.df['cost_usd'] = pd.to_numeric(self.df['cost_usd'])

        return self.df

    def filter_by_date(self, start_date: Optional[datetime] = None,
                       end_date: Optional[datetime] = None) -> pd.DataFrame:
        """
        Filter data by date range

        Args:
            start_date: Start of date range (default: 30 days ago)
            end_date: End of date range (default: now)

        Returns:
            Filtered DataFrame
        """
        if self.df is None:
            raise ValueError("No data loaded. Call load_data() first.")

        if start_date is None:
            start_date = datetime.now() - timedelta(days=30)
        if end_date is None:
            end_date = datetime.now()

        mask = (self.df['timestamp'] >= start_date) & (self.df['timestamp'] <= end_date)
        return self.df[mask]

    def _get_agent_category(self, agent_name: str) -> str:
        """Get category for an agent"""
        for category, agents in self.AGENT_CATEGORIES.items():
            if agent_name in agents:
                return category
        return "Other"

    def create_success_rate_gauges(self, df: pd.DataFrame) -> go.Figure:
        """
        Create success rate gauge charts by agent category

        Args:
            df: Filtered metrics DataFrame

        Returns:
            Plotly figure with gauge subplots
        """
        # Calculate success rates by category (create copy to avoid warning)
        df = df.copy()
        df['category'] = df['agent_name'].apply(self._get_agent_category)
        category_stats = df.groupby('category').agg({
            'success': ['sum', 'count']
        }).reset_index()
        category_stats.columns = ['category', 'successes', 'total']
        category_stats['success_rate'] = (
            category_stats['successes'] / category_stats['total'] * 100
        )

        # Create subplot grid
        n_categories = len(category_stats)
        cols = 3
        rows = (n_categories + cols - 1) // cols

        fig = make_subplots(
            rows=rows, cols=cols,
            specs=[[{'type': 'indicator'}] * cols for _ in range(rows)],
            subplot_titles=category_stats['category'].tolist()
        )

        # Add gauges
        for idx, row in category_stats.iterrows():
            row_idx = idx // cols + 1
            col_idx = idx % cols + 1

            fig.add_trace(
                go.Indicator(
                    mode="gauge+number+delta",
                    value=row['success_rate'],
                    title={'text': f"{row['category']}<br><sub>{row['total']} tasks</sub>"},
                    delta={'reference': 95, 'suffix': '%'},
                    gauge={
                        'axis': {'range': [0, 100]},
                        'bar': {'color': "darkblue"},
                        'steps': [
                            {'range': [0, 60], 'color': "lightgray"},
                            {'range': [60, 80], 'color': "lightyellow"},
                            {'range': [80, 95], 'color': "lightgreen"},
                            {'range': [95, 100], 'color': "green"}
                        ],
                        'threshold': {
                            'line': {'color': "red", 'width': 4},
                            'thickness': 0.75,
                            'value': 95
                        }
                    },
                    domain={'row': row_idx - 1, 'column': col_idx - 1}
                ),
                row=row_idx, col=col_idx
            )

        fig.update_layout(
            title="Agent Success Rates by Category",
            height=300 * rows,
            showlegend=False
        )

        return fig

    def create_load_distribution(self, df: pd.DataFrame) -> go.Figure:
        """
        Create load distribution bar chart

        Args:
            df: Filtered metrics DataFrame

        Returns:
            Plotly bar chart figure
        """
        # Count tasks per agent
        df = df.copy()
        load_data = df.groupby('agent_name').size().reset_index(name='task_count')
        load_data = load_data.sort_values('task_count', ascending=True)

        # Color by category
        load_data['category'] = load_data['agent_name'].apply(self._get_agent_category)

        fig = px.bar(
            load_data,
            x='task_count',
            y='agent_name',
            color='category',
            orientation='h',
            title="Agent Load Distribution (Task Count)",
            labels={'task_count': 'Number of Tasks', 'agent_name': 'Agent'},
            hover_data=['category']
        )

        fig.update_layout(
            height=max(600, len(load_data) * 20),
            showlegend=True,
            xaxis_title="Number of Tasks",
            yaxis_title="Agent Name"
        )

        return fig

    def create_failure_trends(self, df: pd.DataFrame) -> go.Figure:
        """
        Create failure trend line chart over time

        Args:
            df: Filtered metrics DataFrame

        Returns:
            Plotly line chart figure
        """
        # Aggregate by day (create copy to avoid warning)
        df = df.copy()
        df['date'] = df['timestamp'].dt.date
        daily_stats = df.groupby('date').agg({
            'success': ['sum', 'count']
        }).reset_index()
        daily_stats.columns = ['date', 'successes', 'total']
        daily_stats['failures'] = daily_stats['total'] - daily_stats['successes']
        daily_stats['failure_rate'] = (daily_stats['failures'] / daily_stats['total'] * 100)

        # Create figure with secondary y-axis
        fig = make_subplots(specs=[[{"secondary_y": True}]])

        # Add failure count
        fig.add_trace(
            go.Scatter(
                x=daily_stats['date'],
                y=daily_stats['failures'],
                name="Failure Count",
                mode='lines+markers',
                line=dict(color='red', width=2),
                marker=dict(size=6)
            ),
            secondary_y=False
        )

        # Add failure rate
        fig.add_trace(
            go.Scatter(
                x=daily_stats['date'],
                y=daily_stats['failure_rate'],
                name="Failure Rate (%)",
                mode='lines+markers',
                line=dict(color='orange', width=2, dash='dash'),
                marker=dict(size=6)
            ),
            secondary_y=True
        )

        fig.update_layout(
            title="Agent Failure Trends Over Time",
            hovermode='x unified',
            height=500
        )

        fig.update_xaxes(title_text="Date")
        fig.update_yaxes(title_text="Failure Count", secondary_y=False)
        fig.update_yaxes(title_text="Failure Rate (%)", secondary_y=True)

        return fig

    def create_cost_analysis(self, df: pd.DataFrame) -> go.Figure:
        """
        Create cost analysis pie chart by agent category

        Args:
            df: Filtered metrics DataFrame

        Returns:
            Plotly pie chart figure
        """
        # Sum costs by category (create copy to avoid warning)
        df = df.copy()
        df['category'] = df['agent_name'].apply(self._get_agent_category)
        cost_by_category = df.groupby('category')['cost_usd'].sum().reset_index()
        cost_by_category = cost_by_category.sort_values('cost_usd', ascending=False)

        fig = px.pie(
            cost_by_category,
            values='cost_usd',
            names='category',
            title="Cost Distribution by Agent Category",
            hole=0.3,
            hover_data={'cost_usd': ':.4f'}
        )

        fig.update_traces(
            textposition='inside',
            textinfo='percent+label',
            hovertemplate='<b>%{label}</b><br>Cost: $%{value:.4f}<br>Percent: %{percent}<extra></extra>'
        )

        fig.update_layout(height=500)

        return fig

    def generate_dashboard(self,
                          start_date: Optional[datetime] = None,
                          end_date: Optional[datetime] = None,
                          output_file: str = "agent_health_dashboard.html") -> Path:
        """
        Generate complete HTML dashboard

        Args:
            start_date: Start of date range filter
            end_date: End of date range filter
            output_file: Output HTML filename

        Returns:
            Path to generated HTML file
        """
        if self.df is None:
            self.load_data()

        # Filter data
        filtered_df = self.filter_by_date(start_date, end_date)

        if filtered_df.empty:
            raise ValueError("No data in selected date range")

        # Generate all visualizations
        success_gauges = self.create_success_rate_gauges(filtered_df)
        load_dist = self.create_load_distribution(filtered_df)
        failure_trends = self.create_failure_trends(filtered_df)
        cost_analysis = self.create_cost_analysis(filtered_df)

        # Build HTML with all charts
        html_parts = [
            "<!DOCTYPE html>",
            "<html>",
            "<head>",
            "    <meta charset='utf-8'>",
            "    <title>Agent Health Monitoring Dashboard</title>",
            "    <script src='https://cdn.plot.ly/plotly-2.27.0.min.js'></script>",
            "    <style>",
            "        body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }",
            "        h1 { color: #333; text-align: center; }",
            "        .metadata { background: white; padding: 15px; margin: 20px 0; border-radius: 5px; }",
            "        .chart-container { background: white; padding: 20px; margin: 20px 0; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }",
            "        .summary { display: grid; grid-template-columns: repeat(4, 1fr); gap: 15px; margin: 20px 0; }",
            "        .stat-card { background: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); text-align: center; }",
            "        .stat-value { font-size: 2em; font-weight: bold; color: #2c3e50; }",
            "        .stat-label { color: #7f8c8d; margin-top: 5px; }",
            "    </style>",
            "</head>",
            "<body>",
            "    <h1>Agent Health Monitoring Dashboard</h1>",
            "    <div class='metadata'>",
            f"        <p><strong>Report Generated:</strong> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>",
            f"        <p><strong>Date Range:</strong> {filtered_df['timestamp'].min().strftime('%Y-%m-%d')} to {filtered_df['timestamp'].max().strftime('%Y-%m-%d')}</p>",
            f"        <p><strong>Total Tasks:</strong> {len(filtered_df):,}</p>",
            f"        <p><strong>Unique Agents:</strong> {filtered_df['agent_name'].nunique()}</p>",
            "    </div>",
        ]

        # Summary statistics
        total_tasks = len(filtered_df)
        successful_tasks = filtered_df['success'].sum()
        total_cost = filtered_df['cost_usd'].sum()
        avg_duration = filtered_df['duration_ms'].mean()

        html_parts.extend([
            "    <div class='summary'>",
            "        <div class='stat-card'>",
            f"            <div class='stat-value'>{successful_tasks:,}</div>",
            "            <div class='stat-label'>Successful Tasks</div>",
            "        </div>",
            "        <div class='stat-card'>",
            f"            <div class='stat-value'>{(successful_tasks/total_tasks*100):.1f}%</div>",
            "            <div class='stat-label'>Success Rate</div>",
            "        </div>",
            "        <div class='stat-card'>",
            f"            <div class='stat-value'>${total_cost:.2f}</div>",
            "            <div class='stat-label'>Total Cost</div>",
            "        </div>",
            "        <div class='stat-card'>",
            f"            <div class='stat-value'>{avg_duration:.0f}ms</div>",
            "            <div class='stat-label'>Avg Duration</div>",
            "        </div>",
            "    </div>",
        ])

        # Add charts
        for fig, chart_id in [
            (success_gauges, "success-gauges"),
            (load_dist, "load-distribution"),
            (failure_trends, "failure-trends"),
            (cost_analysis, "cost-analysis")
        ]:
            html_parts.extend([
                f"    <div class='chart-container'>",
                f"        <div id='{chart_id}'></div>",
                f"    </div>",
            ])

        html_parts.extend([
            "    <script>",
        ])

        # Add Plotly chart data
        for fig, chart_id in [
            (success_gauges, "success-gauges"),
            (load_dist, "load-distribution"),
            (failure_trends, "failure-trends"),
            (cost_analysis, "cost-analysis")
        ]:
            chart_json = fig.to_json()
            html_parts.append(f"        Plotly.newPlot('{chart_id}', {chart_json});")

        html_parts.extend([
            "    </script>",
            "</body>",
            "</html>"
        ])

        # Write HTML file
        output_path = self.output_dir / output_file
        output_path.parent.mkdir(parents=True, exist_ok=True)

        with open(output_path, 'w', encoding='utf-8') as f:
            f.write('\n'.join(html_parts))

        print(f"Dashboard generated: {output_path}")
        return output_path


def generate_dashboard(data_dir: str = "data/agent_metrics",
                       output_dir: str = "reports",
                       start_date: Optional[str] = None,
                       end_date: Optional[str] = None) -> Path:
    """
    Generate agent health dashboard (convenience function)

    Args:
        data_dir: Directory containing CSV data
        output_dir: Output directory for HTML
        start_date: Start date (YYYY-MM-DD format, optional)
        end_date: End date (YYYY-MM-DD format, optional)

    Returns:
        Path to generated HTML file
    """
    dashboard = AgentDashboard(
        data_dir=Path(data_dir),
        output_dir=Path(output_dir)
    )

    # Parse dates if provided
    start = datetime.fromisoformat(start_date) if start_date else None
    end = datetime.fromisoformat(end_date) if end_date else None

    return dashboard.generate_dashboard(start_date=start, end_date=end)


def view_dashboard(html_file: str = "reports/agent_health_dashboard.html"):
    """
    Open dashboard in default browser

    Args:
        html_file: Path to HTML dashboard file
    """
    html_path = Path(html_file)

    if not html_path.exists():
        raise FileNotFoundError(f"Dashboard not found: {html_path}")

    print(f"Opening dashboard: {html_path.absolute()}")
    webbrowser.open(f"file://{html_path.absolute()}")
