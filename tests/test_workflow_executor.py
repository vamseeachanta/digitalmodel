"""ABOUTME: Tests for workflow executor functionality.
Integration tests for workflow template parsing, execution, and checkpointing."""

import json
from pathlib import Path
import pytest
import tempfile
import shutil

from digitalmodel.modules.automation import (
    WorkflowExecutor,
    WorkflowExecution,
    PhaseStatus,
    AgentStatus,
)


@pytest.fixture
def temp_config(tmp_path):
    """Create a temporary workflow configuration for testing."""
    config = {
        "version": "1.0.0",
        "global_config": {
            "max_phases_per_workflow": 10,
            "checkpoint_dir": str(tmp_path / "checkpoints"),
            "retry_policy": {
                "max_retries_per_agent": 1,
                "max_failures_per_phase": 2,
            },
            "default_quality_gates": {
                "code_coverage": {"enabled": False},
                "linting": {"enabled": False},
                "type_checking": {"enabled": False},
            },
        },
        "templates": {
            "test_workflow": {
                "name": "Test Workflow",
                "description": "Simple test workflow",
                "coordination": {
                    "topology": "mesh",
                    "use_mcp": False,
                    "use_claude_code_tasks": True,
                },
                "quality_gates": {"enabled": False},
                "phases": [
                    {
                        "id": "phase1",
                        "name": "Phase 1",
                        "dependencies": [],
                        "parallel": False,
                        "agents": [
                            {
                                "type": "coder",
                                "role": "developer",
                                "task": "Write code",
                                "coordination": "hooks",
                            }
                        ],
                        "outputs": ["src/code.py"],
                        "checkpoints": ["phase1_complete"],
                    },
                    {
                        "id": "phase2",
                        "name": "Phase 2",
                        "dependencies": ["phase1"],
                        "parallel": False,
                        "agents": [
                            {
                                "type": "tester",
                                "role": "test_engineer",
                                "task": "Write tests",
                                "coordination": "hooks",
                            }
                        ],
                        "outputs": ["tests/test_code.py"],
                        "checkpoints": ["phase2_complete"],
                    },
                ],
            }
        },
        "template_metadata": {
            "test_workflow": {
                "category": "testing",
                "complexity": "low",
                "estimated_duration_minutes": 5,
                "recommended_for": ["Testing"],
            }
        },
    }

    config_file = tmp_path / "test_config.yaml"
    import yaml

    with open(config_file, "w") as f:
        yaml.dump(config, f)

    return config_file


def test_load_templates(temp_config):
    """Test loading workflow templates from configuration."""
    executor = WorkflowExecutor(str(temp_config))

    assert executor.templates is not None
    assert "test_workflow" in executor.templates["templates"]
    assert executor.global_config["max_phases_per_workflow"] == 10


def test_list_templates(temp_config):
    """Test listing available templates."""
    executor = WorkflowExecutor(str(temp_config))
    templates = executor.list_templates()

    assert len(templates) == 1
    assert templates[0]["name"] == "test_workflow"
    assert templates[0]["phase_count"] == 2
    assert templates[0]["category"] == "testing"


def test_create_workflow_execution(temp_config):
    """Test creating a workflow execution from template."""
    executor = WorkflowExecutor(str(temp_config))
    execution = executor.create_workflow_execution("test_workflow", "test_run_001")

    assert execution.workflow_id == "test_run_001"
    assert execution.template_name == "test_workflow"
    assert len(execution.phases) == 2
    assert execution.status == "pending"

    # Check phase structure
    phase1 = execution.phases[0]
    assert phase1.phase_id == "phase1"
    assert phase1.name == "Phase 1"
    assert len(phase1.agents) == 1
    assert phase1.agents[0].agent_type == "coder"


def test_checkpoint_save_load(temp_config):
    """Test saving and loading checkpoints."""
    executor = WorkflowExecutor(str(temp_config))
    execution = executor.create_workflow_execution("test_workflow", "checkpoint_test")

    # Save checkpoint
    executor.save_checkpoint(execution, "test_checkpoint")

    # Verify checkpoint file exists
    checkpoint_file = execution.checkpoint_dir / "checkpoint_latest.json"
    assert checkpoint_file.exists()

    # Load checkpoint
    status = executor.get_workflow_status("checkpoint_test")
    assert status is not None
    assert status["workflow_id"] == "checkpoint_test"
    assert status["template_name"] == "test_workflow"


def test_phase_dependency_check(temp_config):
    """Test phase dependency validation."""
    executor = WorkflowExecutor(str(temp_config))
    execution = executor.create_workflow_execution("test_workflow", "dep_test")

    phase1 = execution.phases[0]
    phase2 = execution.phases[1]

    # Phase 1 should be executable (no dependencies)
    assert executor._can_execute_phase(phase1, set())

    # Phase 2 should not be executable without phase1
    assert not executor._can_execute_phase(phase2, set())

    # Phase 2 should be executable with phase1 completed
    assert executor._can_execute_phase(phase2, {"phase1"})


def test_workflow_execution_structure(temp_config):
    """Test workflow execution data structure."""
    executor = WorkflowExecutor(str(temp_config))
    execution = executor.create_workflow_execution("test_workflow", "struct_test")

    # Convert to dict and verify structure
    execution_dict = execution.to_dict()

    assert "workflow_id" in execution_dict
    assert "template_name" in execution_dict
    assert "phases" in execution_dict
    assert len(execution_dict["phases"]) == 2

    # Verify phase structure
    phase_dict = execution_dict["phases"][0]
    assert "phase_id" in phase_dict
    assert "agents" in phase_dict
    assert len(phase_dict["agents"]) == 1

    # Verify agent structure
    agent_dict = phase_dict["agents"][0]
    assert "agent_type" in agent_dict
    assert "role" in agent_dict
    assert "task" in agent_dict


def test_checkpoint_serialization(temp_config):
    """Test checkpoint JSON serialization."""
    executor = WorkflowExecutor(str(temp_config))
    execution = executor.create_workflow_execution("test_workflow", "serial_test")

    # Save checkpoint
    executor.save_checkpoint(execution, "serialization_test")

    # Load and verify JSON structure
    checkpoint_file = execution.checkpoint_dir / "checkpoint_latest.json"
    with open(checkpoint_file) as f:
        data = json.load(f)

    assert data["workflow_id"] == "serial_test"
    assert data["template_name"] == "test_workflow"
    assert "phases" in data
    assert isinstance(data["phases"], list)


def test_html_report_generation(temp_config, tmp_path):
    """Test HTML report generation."""
    executor = WorkflowExecutor(str(temp_config))
    execution = executor.create_workflow_execution("test_workflow", "report_test")

    # Mark some phases as completed for report
    execution.status = "completed"
    execution.phases[0].status = PhaseStatus.COMPLETED

    report_path = tmp_path / "test_report.html"
    executor.generate_html_report(execution, str(report_path))

    assert report_path.exists()

    # Verify HTML content
    with open(report_path) as f:
        html = f.read()

    assert "Workflow Execution Report" in html
    assert "test_workflow" in html
    assert "Plotly" in html  # Plotly for timeline
    assert "Phase 1" in html


def test_multiple_workflows(temp_config):
    """Test managing multiple workflow executions."""
    executor = WorkflowExecutor(str(temp_config))

    # Create multiple executions
    exec1 = executor.create_workflow_execution("test_workflow", "multi_test_1")
    exec2 = executor.create_workflow_execution("test_workflow", "multi_test_2")

    # Save checkpoints
    executor.save_checkpoint(exec1)
    executor.save_checkpoint(exec2)

    # Verify both can be loaded
    status1 = executor.get_workflow_status("multi_test_1")
    status2 = executor.get_workflow_status("multi_test_2")

    assert status1["workflow_id"] == "multi_test_1"
    assert status2["workflow_id"] == "multi_test_2"


def test_agent_status_transitions(temp_config):
    """Test agent status state transitions."""
    executor = WorkflowExecutor(str(temp_config))
    execution = executor.create_workflow_execution("test_workflow", "agent_test")

    agent = execution.phases[0].agents[0]

    # Initial state
    assert agent.status == AgentStatus.PENDING
    assert agent.retries == 0

    # Simulate status changes
    agent.status = AgentStatus.RUNNING
    assert agent.status == AgentStatus.RUNNING

    agent.status = AgentStatus.COMPLETED
    assert agent.status == AgentStatus.COMPLETED


def test_quality_gates_disabled(temp_config):
    """Test workflow with quality gates disabled."""
    executor = WorkflowExecutor(str(temp_config))
    execution = executor.create_workflow_execution("test_workflow", "quality_test")

    template = executor.templates["templates"]["test_workflow"]

    # Quality gates should be disabled in test config
    phase = execution.phases[0]
    result = executor._check_quality_gates(phase, template)

    # Should pass when disabled
    assert result is True


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
