# ABOUTME: Tests for agent health monitoring dashboard
# Validates data loading, filtering, visualization generation, and HTML output
"""Tests for agent_dashboard module"""

import pytest
import pandas as pd
from datetime import datetime, timedelta
from pathlib import Path
import tempfile
import shutil

from digitalmodel.visualization.agent_dashboard import (
    AgentDashboard,
    generate_dashboard,
    view_dashboard
)


@pytest.fixture
def sample_metrics_csv(tmp_path):
    """Create sample metrics CSV for testing"""
    data = {
        'timestamp': [
            '2026-01-01 10:00:00',
            '2026-01-01 11:00:00',
            '2026-01-02 10:00:00',
            '2026-01-02 11:00:00',
            '2026-01-03 10:00:00',
        ],
        'agent_name': ['coder', 'reviewer', 'tester', 'coder', 'planner'],
        'task_type': ['code_generation', 'code_review', 'testing', 'code_generation', 'planning'],
        'success': [True, True, False, True, True],
        'duration_ms': [1500.5, 2000.3, 3000.1, 1800.2, 1200.4],
        'tokens_used': [1000, 1500, 2000, 1200, 800],
        'cost_usd': [0.01, 0.015, 0.02, 0.012, 0.008],
        'error_type': ['', '', 'timeout', '', '']
    }

    df = pd.DataFrame(data)
    csv_path = tmp_path / "agent_metrics.csv"
    df.to_csv(csv_path, index=False)

    return tmp_path


class TestAgentDashboard:
    """Test AgentDashboard class"""

    def test_initialization(self, sample_metrics_csv):
        """Test dashboard initialization"""
        dashboard = AgentDashboard(
            data_dir=sample_metrics_csv,
            output_dir=sample_metrics_csv / "reports"
        )

        assert dashboard.data_dir == sample_metrics_csv
        assert dashboard.df is None

    def test_load_data(self, sample_metrics_csv):
        """Test CSV data loading"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)
        df = dashboard.load_data()

        assert len(df) == 5
        assert 'timestamp' in df.columns
        assert df['timestamp'].dtype == 'datetime64[ns]'
        assert df['success'].dtype == bool

    def test_filter_by_date(self, sample_metrics_csv):
        """Test date range filtering"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)
        dashboard.load_data()

        # Filter to single day
        start = datetime(2026, 1, 1)
        end = datetime(2026, 1, 1, 23, 59, 59)
        filtered = dashboard.filter_by_date(start, end)

        assert len(filtered) == 2
        assert all(filtered['timestamp'].dt.date == start.date())

    def test_get_agent_category(self, sample_metrics_csv):
        """Test agent categorization"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)

        assert dashboard._get_agent_category('coder') == 'Core Development'
        assert dashboard._get_agent_category('mesh-coordinator') == 'Swarm Coordination'
        assert dashboard._get_agent_category('unknown-agent') == 'Other'

    def test_create_success_rate_gauges(self, sample_metrics_csv):
        """Test success rate gauge generation"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)
        df = dashboard.load_data()

        fig = dashboard.create_success_rate_gauges(df)

        assert fig is not None
        assert len(fig.data) > 0
        assert fig.data[0].type == 'indicator'  # Plotly gauge uses indicator type
        assert fig.data[0].mode == 'gauge+number+delta'

    def test_create_load_distribution(self, sample_metrics_csv):
        """Test load distribution chart"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)
        df = dashboard.load_data()

        fig = dashboard.create_load_distribution(df)

        assert fig is not None
        assert len(fig.data) > 0

    def test_create_failure_trends(self, sample_metrics_csv):
        """Test failure trends chart"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)
        df = dashboard.load_data()

        fig = dashboard.create_failure_trends(df)

        assert fig is not None
        assert len(fig.data) > 0

    def test_create_cost_analysis(self, sample_metrics_csv):
        """Test cost analysis pie chart"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)
        df = dashboard.load_data()

        fig = dashboard.create_cost_analysis(df)

        assert fig is not None
        assert fig.data[0].type == 'pie'

    def test_generate_dashboard(self, sample_metrics_csv):
        """Test complete dashboard generation"""
        output_dir = sample_metrics_csv / "reports"
        dashboard = AgentDashboard(
            data_dir=sample_metrics_csv,
            output_dir=output_dir
        )

        output_path = dashboard.generate_dashboard()

        assert output_path.exists()
        assert output_path.suffix == '.html'

        # Verify HTML content
        html_content = output_path.read_text()
        assert 'Agent Health Monitoring Dashboard' in html_content
        assert 'Plotly.newPlot' in html_content
        assert 'success-gauges' in html_content
        assert 'load-distribution' in html_content
        assert 'failure-trends' in html_content
        assert 'cost-analysis' in html_content


class TestConvenienceFunctions:
    """Test convenience functions"""

    def test_generate_dashboard_function(self, sample_metrics_csv):
        """Test generate_dashboard convenience function"""
        output_dir = sample_metrics_csv / "reports"

        output_path = generate_dashboard(
            data_dir=str(sample_metrics_csv),
            output_dir=str(output_dir)
        )

        assert output_path.exists()
        assert output_path.name == "agent_health_dashboard.html"

    def test_generate_dashboard_with_date_filter(self, sample_metrics_csv):
        """Test dashboard generation with date filtering"""
        output_dir = sample_metrics_csv / "reports"

        output_path = generate_dashboard(
            data_dir=str(sample_metrics_csv),
            output_dir=str(output_dir),
            start_date='2026-01-01',
            end_date='2026-01-02'
        )

        assert output_path.exists()

    def test_view_dashboard_file_not_found(self):
        """Test view_dashboard with missing file"""
        with pytest.raises(FileNotFoundError):
            view_dashboard("nonexistent.html")


class TestErrorHandling:
    """Test error handling"""

    def test_load_data_file_not_found(self, tmp_path):
        """Test error when CSV file doesn't exist"""
        dashboard = AgentDashboard(data_dir=tmp_path)

        with pytest.raises(FileNotFoundError):
            dashboard.load_data()

    def test_filter_without_loading_data(self, sample_metrics_csv):
        """Test filtering before loading data"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)

        with pytest.raises(ValueError, match="No data loaded"):
            dashboard.filter_by_date()

    def test_generate_dashboard_empty_date_range(self, sample_metrics_csv):
        """Test dashboard generation with empty date range"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)

        # Date range with no data
        future_start = datetime(2027, 1, 1)
        future_end = datetime(2027, 1, 2)

        with pytest.raises(ValueError, match="No data in selected date range"):
            dashboard.generate_dashboard(start_date=future_start, end_date=future_end)


class TestDataValidation:
    """Test data validation and type handling"""

    def test_boolean_success_field(self, sample_metrics_csv):
        """Test success field is properly converted to boolean"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)
        df = dashboard.load_data()

        assert df['success'].dtype == bool
        assert df['success'].sum() == 4  # 4 True values in sample

    def test_numeric_fields(self, sample_metrics_csv):
        """Test numeric fields are properly typed"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)
        df = dashboard.load_data()

        assert pd.api.types.is_numeric_dtype(df['duration_ms'])
        assert pd.api.types.is_numeric_dtype(df['tokens_used'])
        assert pd.api.types.is_numeric_dtype(df['cost_usd'])

    def test_timestamp_parsing(self, sample_metrics_csv):
        """Test timestamp parsing"""
        dashboard = AgentDashboard(data_dir=sample_metrics_csv)
        df = dashboard.load_data()

        assert df['timestamp'].dtype == 'datetime64[ns]'
        assert df['timestamp'].min() == pd.Timestamp('2026-01-01 10:00:00')
