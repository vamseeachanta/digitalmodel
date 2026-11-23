"""
ABOUTME: Unit tests for intelligent agent selection system.
Tests scoring algorithm, performance tracking, and selection logic with mocked data.
"""

import json
import sqlite3
import tempfile
from datetime import datetime, timedelta
from pathlib import Path
import pytest
import yaml

from digitalmodel.workflows.automation.intelligent_agent_selector import (
    AgentPerformanceTracker,
    IntelligentAgentSelector,
)


@pytest.fixture
def temp_db():
    """Create temporary database for testing."""
    with tempfile.NamedTemporaryFile(suffix=".db", delete=False) as f:
        db_path = f.name
    yield db_path
    Path(db_path).unlink(missing_ok=True)


@pytest.fixture
def temp_config():
    """Create temporary config file for testing."""
    with tempfile.NamedTemporaryFile(
        mode='w', suffix=".yaml", delete=False
    ) as f:
        config = {
            "weights": {
                "historical_success": 0.70,
                "availability": 0.15,
                "cost_benefit": 0.10,
                "domain_expertise": 0.05
            }
        }
        yaml.dump(config, f)
        config_path = f.name
    yield config_path
    Path(config_path).unlink(missing_ok=True)


@pytest.fixture
def temp_registry():
    """Create temporary registry file for testing."""
    with tempfile.NamedTemporaryFile(
        mode='w', suffix=".json", delete=False
    ) as f:
        registry = {
            "agents": {
                "agent-a": {
                    "platform": "claude-code",
                    "type": "coder",
                    "capabilities": ["python", "testing", "refactoring"]
                },
                "agent-b": {
                    "platform": "claude-flow",
                    "type": "reviewer",
                    "capabilities": ["code review", "security", "quality"]
                },
                "agent-c": {
                    "platform": "factory-ai",
                    "type": "tester",
                    "capabilities": ["testing", "automation", "ci/cd"]
                }
            },
            "taskTypeAgentMapping": {
                "code-generation": {
                    "primary": "agent-a",
                    "alternatives": ["agent-b", "agent-c"]
                },
                "code-review": {
                    "primary": "agent-b",
                    "alternatives": ["agent-a"]
                }
            }
        }
        json.dump(registry, f)
        registry_path = f.name
    yield registry_path
    Path(registry_path).unlink(missing_ok=True)


class TestAgentPerformanceTracker:
    """Test suite for AgentPerformanceTracker."""

    def test_init_database(self, temp_db):
        """Test database initialization creates schema."""
        tracker = AgentPerformanceTracker(temp_db)

        with sqlite3.connect(temp_db) as conn:
            cursor = conn.execute(
                "SELECT name FROM sqlite_master WHERE type='table'"
            )
            tables = [row[0] for row in cursor.fetchall()]

        assert "agent_tasks" in tables

    def test_record_task(self, temp_db):
        """Test recording task completion."""
        tracker = AgentPerformanceTracker(temp_db)

        task_id = tracker.record_task(
            agent_type="agent-a",
            task_type="code-generation",
            success=True,
            execution_time=45.5,
            error_count=0,
            quality_score=0.95
        )

        assert task_id > 0

        # Verify data was stored
        with sqlite3.connect(temp_db) as conn:
            cursor = conn.execute(
                "SELECT * FROM agent_tasks WHERE id = ?", (task_id,)
            )
            row = cursor.fetchone()

        assert row is not None
        assert row[1] == "agent-a"  # agent_type
        assert row[2] == "code-generation"  # task_type
        assert row[5] == 1  # success
        assert row[6] == 45.5  # execution_time

    def test_get_success_rate_with_data(self, temp_db):
        """Test success rate calculation with historical data."""
        tracker = AgentPerformanceTracker(temp_db)

        # Record 7 successful and 3 failed tasks
        for i in range(7):
            tracker.record_task("agent-a", "test", success=True)
        for i in range(3):
            tracker.record_task("agent-a", "test", success=False)

        success_rate = tracker.get_success_rate("agent-a", days=30)
        assert success_rate == 0.7  # 7/10

    def test_get_success_rate_no_data(self, temp_db):
        """Test success rate for agent with no history returns default."""
        tracker = AgentPerformanceTracker(temp_db)

        success_rate = tracker.get_success_rate("unknown-agent", days=30)
        assert success_rate == 0.5  # Default neutral score

    def test_get_success_rate_respects_time_window(self, temp_db):
        """Test success rate only counts tasks within time window."""
        tracker = AgentPerformanceTracker(temp_db)

        # Insert old task (outside window)
        old_date = datetime.now() - timedelta(days=40)
        with sqlite3.connect(temp_db) as conn:
            conn.execute("""
                INSERT INTO agent_tasks (
                    agent_type, task_type, started_at, completed_at, success
                ) VALUES (?, ?, ?, ?, ?)
            """, ("agent-a", "test", old_date, old_date, 1))
            conn.commit()

        # Insert recent task
        tracker.record_task("agent-a", "test", success=False)

        # Should only count recent task
        success_rate = tracker.get_success_rate("agent-a", days=30)
        assert success_rate == 0.0  # 0/1 recent tasks

    def test_get_avg_execution_time(self, temp_db):
        """Test average execution time calculation."""
        tracker = AgentPerformanceTracker(temp_db)

        tracker.record_task("agent-a", "test", success=True, execution_time=30.0)
        tracker.record_task("agent-a", "test", success=True, execution_time=50.0)
        tracker.record_task("agent-a", "test", success=True, execution_time=40.0)

        avg_time = tracker.get_avg_execution_time("agent-a", days=30)
        assert avg_time == 40.0  # (30+50+40)/3

    def test_get_avg_execution_time_no_data(self, temp_db):
        """Test average execution time returns default when no data."""
        tracker = AgentPerformanceTracker(temp_db)

        avg_time = tracker.get_avg_execution_time("unknown-agent", days=30)
        assert avg_time == 60.0  # Default


class TestIntelligentAgentSelector:
    """Test suite for IntelligentAgentSelector."""

    def test_load_weights(self, temp_config, temp_registry, temp_db):
        """Test loading weights from config file."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        assert selector.weights["historical_success"] == 0.70
        assert selector.weights["availability"] == 0.15
        assert selector.weights["cost_benefit"] == 0.10
        assert selector.weights["domain_expertise"] == 0.05

    def test_load_weights_missing_file(self, temp_registry, temp_db):
        """Test default weights when config file missing."""
        selector = IntelligentAgentSelector(
            config_path="nonexistent.yaml",
            registry_path=temp_registry,
            performance_db=temp_db
        )

        # Should have default weights
        assert selector.weights["historical_success"] == 0.70

    def test_load_registry(self, temp_config, temp_registry, temp_db):
        """Test loading agent registry."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        assert "agent-a" in selector.registry["agents"]
        assert "code-generation" in selector.registry["taskTypeAgentMapping"]

    def test_calculate_historical_score(
        self, temp_config, temp_registry, temp_db
    ):
        """Test historical success score calculation."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        # Record some history
        for i in range(8):
            selector.tracker.record_task("agent-a", "test", success=True)
        for i in range(2):
            selector.tracker.record_task("agent-a", "test", success=False)

        score = selector._calculate_historical_score("agent-a")
        assert score == 0.8  # 8/10

    def test_calculate_availability_score_available(
        self, temp_config, temp_registry, temp_db
    ):
        """Test availability score for available agent."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        score = selector._calculate_availability_score(
            "agent-a", current_load=2, max_concurrent=5
        )
        assert score == 0.6  # 1 - (2/5)

    def test_calculate_availability_score_unavailable(
        self, temp_config, temp_registry, temp_db
    ):
        """Test availability score returns -1 for unavailable agent."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        score = selector._calculate_availability_score(
            "agent-a", current_load=5, max_concurrent=5
        )
        assert score == -1.0  # Hard constraint: unavailable

    def test_calculate_cost_benefit_score(
        self, temp_config, temp_registry, temp_db
    ):
        """Test cost-benefit score calculation."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        # Setup history: high success, fast execution
        for i in range(9):
            selector.tracker.record_task(
                "agent-a", "test", success=True, execution_time=10.0
            )
        selector.tracker.record_task(
            "agent-a", "test", success=False, execution_time=10.0
        )

        score = selector._calculate_cost_benefit_score("agent-a")
        # success_rate=0.9, avg_time=10s -> benefit=0.09
        # normalized by max_benefit=0.1 -> score=0.9
        assert 0.85 <= score <= 0.95

    def test_extract_task_keywords(
        self, temp_config, temp_registry, temp_db
    ):
        """Test keyword extraction from task description."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        keywords = selector._extract_task_keywords(
            "Create a Python function for testing API endpoints"
        )

        assert "python" in keywords
        assert "function" in keywords
        assert "testing" in keywords
        assert "endpoints" in keywords
        # Stop words should be excluded
        assert "a" not in keywords
        assert "for" not in keywords

    def test_calculate_domain_expertise_score(
        self, temp_config, temp_registry, temp_db
    ):
        """Test domain expertise matching score."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        # agent-a has capabilities: ["python", "testing", "refactoring"]
        score = selector._calculate_domain_expertise_score(
            "agent-a",
            ["python", "testing", "function"]
        )

        # 2/3 keywords match (python, testing)
        assert abs(score - 0.667) < 0.01

    def test_calculate_agent_score(
        self, temp_config, temp_registry, temp_db
    ):
        """Test overall agent score calculation."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        # Setup good history for agent-a
        for i in range(9):
            selector.tracker.record_task(
                "agent-a", "test", success=True, execution_time=20.0
            )

        total_score, components = selector.calculate_agent_score(
            "agent-a",
            "Create Python testing framework",
            current_load=1,
            max_concurrent=5
        )

        assert total_score > 0
        assert "historical_success" in components
        assert "availability" in components
        assert "cost_benefit" in components
        assert "domain_expertise" in components
        assert components["total"] == total_score

    def test_calculate_agent_score_excludes_unavailable(
        self, temp_config, temp_registry, temp_db
    ):
        """Test that unavailable agents get -1 score."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        total_score, components = selector.calculate_agent_score(
            "agent-a",
            "Test task",
            current_load=5,  # At max
            max_concurrent=5
        )

        assert total_score == -1.0
        assert components == {}

    def test_select_agent_basic(
        self, temp_config, temp_registry, temp_db
    ):
        """Test basic agent selection."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        # Setup history
        for i in range(9):
            selector.tracker.record_task(
                "agent-a", "code-generation", success=True, execution_time=30.0
            )

        result = selector.select_agent(
            "code-generation",
            "Create Python API endpoint"
        )

        assert result["agent"] == "agent-a"
        assert result["confidence"] > 0
        assert "reasoning" in result
        assert "scores" in result
        assert "alternatives" in result

    def test_select_agent_prefers_high_success_rate(
        self, temp_config, temp_registry, temp_db
    ):
        """Test selection prefers agent with higher success rate."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        # agent-a: 90% success
        for i in range(9):
            selector.tracker.record_task("agent-a", "test", success=True)
        selector.tracker.record_task("agent-a", "test", success=False)

        # agent-b: 50% success
        for i in range(5):
            selector.tracker.record_task("agent-b", "test", success=True)
        for i in range(5):
            selector.tracker.record_task("agent-b", "test", success=False)

        result = selector.select_agent(
            "code-generation",
            "Test task"
        )

        # Should prefer agent-a due to higher success rate (70% weight)
        assert result["agent"] == "agent-a"

    def test_select_agent_excludes_unavailable(
        self, temp_config, temp_registry, temp_db
    ):
        """Test selection excludes unavailable agents."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        # Give agent-a perfect history but unavailable
        for i in range(10):
            selector.tracker.record_task("agent-a", "test", success=True)

        # Give agent-b worse history but available
        for i in range(6):
            selector.tracker.record_task("agent-b", "test", success=True)
        for i in range(4):
            selector.tracker.record_task("agent-b", "test", success=False)

        agent_loads = {
            "agent-a": 5,  # Unavailable
            "agent-b": 1   # Available
        }

        result = selector.select_agent(
            "code-generation",
            "Test task",
            agent_loads=agent_loads,
            max_concurrent=5
        )

        # Should select agent-b despite worse history
        assert result["agent"] == "agent-b"

    def test_select_agent_handles_new_agents(
        self, temp_config, temp_registry, temp_db
    ):
        """Test selection handles agents with no history."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        result = selector.select_agent(
            "code-generation",
            "Create new feature"
        )

        # Should still select an agent (with default scores)
        assert result["agent"] is not None
        assert result["confidence"] >= 0

    def test_select_agent_no_candidates(
        self, temp_config, temp_db
    ):
        """Test selection when no agents available."""
        # Create empty registry
        with tempfile.NamedTemporaryFile(
            mode='w', suffix=".json", delete=False
        ) as f:
            json.dump({"agents": {}, "taskTypeAgentMapping": {}}, f)
            empty_registry = f.name

        try:
            selector = IntelligentAgentSelector(
                config_path=temp_config,
                registry_path=empty_registry,
                performance_db=temp_db
            )

            result = selector.select_agent(
                "code-generation",
                "Test task"
            )

            assert result["agent"] is None
            assert result["confidence"] == 0.0
            assert "No agents available" in result["reasoning"]

        finally:
            Path(empty_registry).unlink(missing_ok=True)

    def test_select_agent_returns_alternatives(
        self, temp_config, temp_registry, temp_db
    ):
        """Test selection includes alternative agents."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        result = selector.select_agent(
            "code-generation",
            "Test task"
        )

        assert "alternatives" in result
        assert isinstance(result["alternatives"], list)
        # Should have up to 3 alternatives
        assert len(result["alternatives"]) <= 3

    def test_generate_reasoning(
        self, temp_config, temp_registry, temp_db
    ):
        """Test reasoning generation."""
        selector = IntelligentAgentSelector(
            config_path=temp_config,
            registry_path=temp_registry,
            performance_db=temp_db
        )

        scores = {
            "historical_success": 0.85,
            "availability": 0.9,
            "cost_benefit": 0.75,
            "domain_expertise": 0.7,
            "total": 0.82
        }

        reasoning = selector._generate_reasoning(
            "agent-a",
            scores,
            "code-generation"
        )

        assert "agent-a" in reasoning
        assert "code-generation" in reasoning
        assert "success rate" in reasoning.lower() or "available" in reasoning.lower()


class TestCLIIntegration:
    """Test CLI entry point."""

    def test_cli_output_format(
        self, temp_config, temp_registry, temp_db, capsys
    ):
        """Test CLI outputs valid JSON."""
        import sys
        from digitalmodel.workflows.automation.intelligent_agent_selector import main

        # Mock command line arguments
        sys.argv = [
            "intelligent_agent_selector.py",
            "code-generation",
            "Create Python API",
            "--config", temp_config,
            "--registry", temp_registry
        ]

        # Temporarily override database path
        import digitalmodel.workflows.automation.intelligent_agent_selector as module
        original_db = module.AgentPerformanceTracker.__init__.__defaults__
        module.AgentPerformanceTracker.__init__.__defaults__ = (temp_db,)

        try:
            main()
            captured = capsys.readouterr()
            result = json.loads(captured.out)

            assert "agent" in result
            assert "confidence" in result
            assert "reasoning" in result

        finally:
            # Restore original default
            module.AgentPerformanceTracker.__init__.__defaults__ = original_db


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
