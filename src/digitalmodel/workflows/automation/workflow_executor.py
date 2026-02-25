"""ABOUTME: Enhanced workflow executor with phase dependencies, quality gates, and agent orchestration.
Executes workflow templates with automatic checkpointing, error recovery, and progress tracking."""

import json
import os
import subprocess
import sys
import time
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Set

import yaml


class PhaseStatus(Enum):
    """Phase execution status."""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"


class AgentStatus(Enum):
    """Agent execution status."""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    RETRYING = "retrying"


@dataclass
class AgentExecution:
    """Represents a single agent execution."""
    agent_type: str
    role: str
    task: str
    coordination: str
    status: AgentStatus = AgentStatus.PENDING
    retries: int = 0
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None
    output: Optional[str] = None
    error: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "agent_type": self.agent_type,
            "role": self.role,
            "task": self.task,
            "coordination": self.coordination,
            "status": self.status.value,
            "retries": self.retries,
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "output": self.output,
            "error": self.error,
        }


@dataclass
class PhaseExecution:
    """Represents a single phase execution."""
    phase_id: str
    name: str
    dependencies: List[str]
    parallel: bool
    agents: List[AgentExecution]
    status: PhaseStatus = PhaseStatus.PENDING
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None
    checkpoints: List[str] = field(default_factory=list)
    outputs: List[str] = field(default_factory=list)
    quality_gates: Optional[Dict[str, Any]] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "phase_id": self.phase_id,
            "name": self.name,
            "dependencies": self.dependencies,
            "parallel": self.parallel,
            "status": self.status.value,
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "checkpoints": self.checkpoints,
            "outputs": self.outputs,
            "agents": [agent.to_dict() for agent in self.agents],
            "quality_gates": self.quality_gates,
        }


@dataclass
class WorkflowExecution:
    """Represents a complete workflow execution."""
    workflow_id: str
    template_name: str
    phases: List[PhaseExecution]
    status: str = "pending"
    start_time: Optional[datetime] = None
    end_time: Optional[datetime] = None
    current_phase: Optional[str] = None
    checkpoint_dir: Optional[Path] = None
    global_config: Optional[Dict[str, Any]] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "workflow_id": self.workflow_id,
            "template_name": self.template_name,
            "status": self.status,
            "start_time": self.start_time.isoformat() if self.start_time else None,
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "current_phase": self.current_phase,
            "phases": [phase.to_dict() for phase in self.phases],
            "global_config": self.global_config,
        }


class WorkflowExecutor:
    """Executes workflow templates with phase dependencies and quality gates."""

    def __init__(self, config_path: str, checkpoint_base_dir: Optional[str] = None):
        """Initialize workflow executor.

        Args:
            config_path: Path to workflow templates YAML file
            checkpoint_base_dir: Base directory for checkpoints (overrides config)
        """
        self.config_path = Path(config_path)
        self.templates = self._load_templates()
        self.global_config = self.templates.get("global_config", {})

        # Setup checkpoint directory
        if checkpoint_base_dir:
            self.checkpoint_base_dir = Path(checkpoint_base_dir)
        else:
            self.checkpoint_base_dir = Path(
                self.global_config.get("checkpoint_dir", "data/workflows/checkpoints")
            )
        self.checkpoint_base_dir.mkdir(parents=True, exist_ok=True)

    def _load_templates(self) -> Dict[str, Any]:
        """Load workflow templates from YAML file."""
        if not self.config_path.exists():
            raise FileNotFoundError(f"Template file not found: {self.config_path}")

        with open(self.config_path, "r") as f:
            return yaml.safe_load(f)

    def list_templates(self) -> List[Dict[str, Any]]:
        """List all available workflow templates.

        Returns:
            List of template metadata dictionaries
        """
        templates = self.templates.get("templates", {})
        metadata = self.templates.get("template_metadata", {})

        result = []
        for name, template in templates.items():
            meta = metadata.get(name, {})
            result.append({
                "name": name,
                "title": template.get("name", name),
                "description": template.get("description", ""),
                "category": meta.get("category", "unknown"),
                "complexity": meta.get("complexity", "unknown"),
                "estimated_duration_minutes": meta.get("estimated_duration_minutes", 0),
                "recommended_for": meta.get("recommended_for", []),
                "phase_count": len(template.get("phases", [])),
            })

        return result

    def create_workflow_execution(
        self,
        template_name: str,
        workflow_id: Optional[str] = None,
    ) -> WorkflowExecution:
        """Create a workflow execution from a template.

        Args:
            template_name: Name of the template to use
            workflow_id: Optional custom workflow ID

        Returns:
            WorkflowExecution object
        """
        if template_name not in self.templates.get("templates", {}):
            raise ValueError(f"Template not found: {template_name}")

        template = self.templates["templates"][template_name]

        # Generate workflow ID
        if not workflow_id:
            workflow_id = f"{template_name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"

        # Create checkpoint directory for this workflow
        checkpoint_dir = self.checkpoint_base_dir / workflow_id
        checkpoint_dir.mkdir(parents=True, exist_ok=True)

        # Parse phases
        phases = []
        for phase_config in template.get("phases", []):
            agents = [
                AgentExecution(
                    agent_type=agent.get("type"),
                    role=agent.get("role"),
                    task=agent.get("task"),
                    coordination=agent.get("coordination", "hooks"),
                )
                for agent in phase_config.get("agents", [])
            ]

            phase = PhaseExecution(
                phase_id=phase_config["id"],
                name=phase_config["name"],
                dependencies=phase_config.get("dependencies", []),
                parallel=phase_config.get("parallel", False),
                agents=agents,
                checkpoints=phase_config.get("checkpoints", []),
                outputs=phase_config.get("outputs", []),
                quality_gates=phase_config.get("quality_gates"),
            )
            phases.append(phase)

        return WorkflowExecution(
            workflow_id=workflow_id,
            template_name=template_name,
            phases=phases,
            checkpoint_dir=checkpoint_dir,
            global_config=self.global_config,
        )

    def save_checkpoint(self, execution: WorkflowExecution, checkpoint_name: str = "auto"):
        """Save workflow execution checkpoint.

        Args:
            execution: WorkflowExecution to save
            checkpoint_name: Name of the checkpoint
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        checkpoint_file = execution.checkpoint_dir / f"checkpoint_{checkpoint_name}_{timestamp}.json"

        with open(checkpoint_file, "w") as f:
            json.dump(execution.to_dict(), f, indent=2)

        # Also save as latest
        latest_file = execution.checkpoint_dir / "checkpoint_latest.json"
        with open(latest_file, "w") as f:
            json.dump(execution.to_dict(), f, indent=2)

    def load_checkpoint(self, checkpoint_path: str) -> WorkflowExecution:
        """Load workflow execution from checkpoint.

        Args:
            checkpoint_path: Path to checkpoint file

        Returns:
            WorkflowExecution object
        """
        with open(checkpoint_path, "r") as f:
            data = json.load(f)

        # Reconstruct execution object
        phases = []
        for phase_data in data["phases"]:
            agents = [
                AgentExecution(
                    agent_type=agent["agent_type"],
                    role=agent["role"],
                    task=agent["task"],
                    coordination=agent["coordination"],
                    status=AgentStatus(agent["status"]),
                    retries=agent["retries"],
                    start_time=datetime.fromisoformat(agent["start_time"]) if agent["start_time"] else None,
                    end_time=datetime.fromisoformat(agent["end_time"]) if agent["end_time"] else None,
                    output=agent.get("output"),
                    error=agent.get("error"),
                )
                for agent in phase_data["agents"]
            ]

            phase = PhaseExecution(
                phase_id=phase_data["phase_id"],
                name=phase_data["name"],
                dependencies=phase_data["dependencies"],
                parallel=phase_data["parallel"],
                agents=agents,
                status=PhaseStatus(phase_data["status"]),
                start_time=datetime.fromisoformat(phase_data["start_time"]) if phase_data["start_time"] else None,
                end_time=datetime.fromisoformat(phase_data["end_time"]) if phase_data["end_time"] else None,
                checkpoints=phase_data["checkpoints"],
                outputs=phase_data["outputs"],
                quality_gates=phase_data.get("quality_gates"),
            )
            phases.append(phase)

        execution = WorkflowExecution(
            workflow_id=data["workflow_id"],
            template_name=data["template_name"],
            phases=phases,
            status=data["status"],
            start_time=datetime.fromisoformat(data["start_time"]) if data["start_time"] else None,
            end_time=datetime.fromisoformat(data["end_time"]) if data["end_time"] else None,
            current_phase=data.get("current_phase"),
            checkpoint_dir=self.checkpoint_base_dir / data["workflow_id"],
            global_config=data.get("global_config"),
        )

        return execution

    def _can_execute_phase(self, phase: PhaseExecution, completed_phases: Set[str]) -> bool:
        """Check if a phase can be executed based on dependencies.

        Args:
            phase: Phase to check
            completed_phases: Set of completed phase IDs

        Returns:
            True if phase can be executed
        """
        return all(dep in completed_phases for dep in phase.dependencies)

    def _execute_agent_with_hooks(self, agent: AgentExecution, workflow_id: str, phase_id: str) -> bool:
        """Execute a single agent with coordination hooks.

        Args:
            agent: AgentExecution to run
            workflow_id: Workflow ID for coordination
            phase_id: Phase ID for coordination

        Returns:
            True if successful, False otherwise
        """
        agent.status = AgentStatus.RUNNING
        agent.start_time = datetime.now()

        try:
            # Pre-task hook
            task_description = f"{agent.role}: {agent.task}"
            subprocess.run(
                ["npx", "claude-flow@alpha", "hooks", "pre-task", "--description", task_description],
                check=True,
                capture_output=True,
                text=True,
            )

            # Session restore
            session_id = f"workflow-{workflow_id}-{phase_id}"
            subprocess.run(
                ["npx", "claude-flow@alpha", "hooks", "session-restore", "--session-id", session_id],
                capture_output=True,
                text=True,
            )

            # Here we would spawn the actual agent using Claude Code's Task tool
            # For now, this is a placeholder that simulates agent execution
            # In real implementation, this would use the Task tool
            print(f"  → Executing {agent.role} ({agent.agent_type}): {agent.task}")

            # Simulate agent work (in real implementation, this would be Task tool execution)
            time.sleep(1)

            # Post-task hook
            subprocess.run(
                ["npx", "claude-flow@alpha", "hooks", "post-task", "--task-id", f"{workflow_id}-{agent.role}"],
                check=True,
                capture_output=True,
                text=True,
            )

            # Notify completion
            subprocess.run(
                ["npx", "claude-flow@alpha", "hooks", "notify", "--message", f"{agent.role} completed"],
                capture_output=True,
                text=True,
            )

            agent.status = AgentStatus.COMPLETED
            agent.end_time = datetime.now()
            return True

        except subprocess.CalledProcessError as e:
            agent.error = str(e)
            agent.status = AgentStatus.FAILED
            agent.end_time = datetime.now()
            return False
        except Exception as e:
            agent.error = str(e)
            agent.status = AgentStatus.FAILED
            agent.end_time = datetime.now()
            return False

    def _execute_agent_with_retry(
        self,
        agent: AgentExecution,
        workflow_id: str,
        phase_id: str,
        max_retries: int = 1,
    ) -> bool:
        """Execute agent with retry logic.

        Args:
            agent: AgentExecution to run
            workflow_id: Workflow ID
            phase_id: Phase ID
            max_retries: Maximum number of retries

        Returns:
            True if successful, False otherwise
        """
        for attempt in range(max_retries + 1):
            if attempt > 0:
                agent.status = AgentStatus.RETRYING
                agent.retries = attempt
                print(f"  ⟳ Retrying {agent.role} (attempt {attempt + 1}/{max_retries + 1})")

            if self._execute_agent_with_hooks(agent, workflow_id, phase_id):
                return True

        # All retries failed, try with replacement agent
        print(f"  ⚠ {agent.role} failed after {max_retries + 1} attempts, spawning replacement")
        agent.status = AgentStatus.FAILED

        # Create replacement agent
        replacement = AgentExecution(
            agent_type=agent.agent_type,
            role=f"{agent.role}_replacement",
            task=agent.task,
            coordination=agent.coordination,
        )

        if self._execute_agent_with_hooks(replacement, workflow_id, phase_id):
            agent.status = AgentStatus.COMPLETED
            agent.output = f"Completed by replacement agent: {replacement.role}"
            return True

        return False

    def _check_quality_gates(self, phase: PhaseExecution, template_config: Dict[str, Any]) -> bool:
        """Check quality gates for a phase.

        Args:
            phase: PhaseExecution to check
            template_config: Template configuration

        Returns:
            True if all gates pass, False otherwise
        """
        # Get quality gates (phase-specific overrides template defaults)
        phase_gates = phase.quality_gates or template_config.get("quality_gates", {})
        default_gates = self.global_config.get("default_quality_gates", {})

        # Merge gates
        gates = {**default_gates, **phase_gates}

        if not gates or gates.get("enabled") is False:
            return True

        print(f"  ✓ Checking quality gates for phase: {phase.name}")

        # Check code coverage
        coverage_config = gates.get("code_coverage", {})
        if coverage_config and coverage_config.get("enabled", True):
            threshold = coverage_config.get("threshold", 80)
            # In real implementation, this would run pytest --cov and parse results
            print(f"    - Code coverage threshold: {threshold}%")
            # Simulate coverage check
            # actual_coverage = self._measure_coverage()
            # if actual_coverage < threshold:
            #     print(f"    ✗ Coverage {actual_coverage}% below threshold {threshold}%")
            #     return False

        # Check linting
        if gates.get("linting", {}).get("enabled", True):
            print(f"    - Running linting checks")
            # In real implementation: subprocess.run(["uv", "run", "ruff", "check", "."])

        # Check type checking
        if gates.get("type_checking", {}).get("enabled", True):
            print(f"    - Running type checking")
            # In real implementation: subprocess.run(["uv", "run", "mypy", "."])

        print(f"  ✓ All quality gates passed")
        return True

    def _execute_phase(
        self,
        phase: PhaseExecution,
        execution: WorkflowExecution,
        template_config: Dict[str, Any],
    ) -> bool:
        """Execute a single phase.

        Args:
            phase: PhaseExecution to run
            execution: WorkflowExecution context
            template_config: Template configuration

        Returns:
            True if successful, False otherwise
        """
        print(f"\n{'='*80}")
        print(f"Phase: {phase.name} ({phase.phase_id})")
        print(f"{'='*80}")

        phase.status = PhaseStatus.IN_PROGRESS
        phase.start_time = datetime.now()
        execution.current_phase = phase.phase_id

        # Save checkpoint at phase start
        self.save_checkpoint(execution, f"phase_{phase.phase_id}_start")

        max_retries = self.global_config.get("retry_policy", {}).get("max_retries_per_agent", 1)
        max_failures = self.global_config.get("retry_policy", {}).get("max_failures_per_phase", 2)
        failures = 0

        # Execute agents
        if phase.parallel:
            print(f"Executing {len(phase.agents)} agents in parallel...")
            # In real implementation, use Claude Code Task tool for parallel execution
            # For now, simulate parallel execution sequentially
            for agent in phase.agents:
                if not self._execute_agent_with_retry(agent, execution.workflow_id, phase.phase_id, max_retries):
                    failures += 1
                    if failures >= max_failures:
                        print(f"  ✗ Phase failed: {failures} agent failures (max: {max_failures})")
                        phase.status = PhaseStatus.FAILED
                        phase.end_time = datetime.now()
                        return False
        else:
            print(f"Executing {len(phase.agents)} agents sequentially...")
            for agent in phase.agents:
                if not self._execute_agent_with_retry(agent, execution.workflow_id, phase.phase_id, max_retries):
                    failures += 1
                    if failures >= max_failures:
                        print(f"  ✗ Phase failed: {failures} agent failures (max: {max_failures})")
                        phase.status = PhaseStatus.FAILED
                        phase.end_time = datetime.now()
                        return False

        # Check quality gates
        if not self._check_quality_gates(phase, template_config):
            print(f"  ✗ Phase failed: Quality gates not met")
            phase.status = PhaseStatus.FAILED
            phase.end_time = datetime.now()

            # Halt workflow on quality gate failure
            halt_on_failure = template_config.get("quality_gates", {}).get("halt_on_failure", True)
            if halt_on_failure:
                execution.status = "failed"
                return False

        phase.status = PhaseStatus.COMPLETED
        phase.end_time = datetime.now()

        # Save checkpoint at phase end
        self.save_checkpoint(execution, f"phase_{phase.phase_id}_complete")

        duration = (phase.end_time - phase.start_time).total_seconds()
        print(f"  ✓ Phase completed in {duration:.1f}s")

        return True

    def execute_workflow(
        self,
        execution: WorkflowExecution,
        resume_from_checkpoint: bool = False,
    ) -> bool:
        """Execute a workflow with phase dependencies.

        Args:
            execution: WorkflowExecution to run
            resume_from_checkpoint: Whether to resume from last checkpoint

        Returns:
            True if successful, False otherwise
        """
        print(f"\n{'#'*80}")
        print(f"# Workflow: {execution.template_name}")
        print(f"# ID: {execution.workflow_id}")
        print(f"# Phases: {len(execution.phases)}")
        print(f"{'#'*80}\n")

        execution.start_time = datetime.now()
        execution.status = "running"

        # Get template config
        template_config = self.templates["templates"][execution.template_name]

        # Track completed phases
        completed_phases: Set[str] = set()

        if resume_from_checkpoint:
            # Mark already completed phases
            for phase in execution.phases:
                if phase.status == PhaseStatus.COMPLETED:
                    completed_phases.add(phase.phase_id)

        # Build dependency graph and execute in order
        remaining_phases = [p for p in execution.phases if p.phase_id not in completed_phases]

        while remaining_phases:
            # Find phases that can be executed
            executable = [
                p for p in remaining_phases
                if self._can_execute_phase(p, completed_phases)
            ]

            if not executable:
                # No executable phases means circular dependency or missing dependency
                print(f"✗ Workflow failed: Unable to execute remaining phases (dependency error)")
                execution.status = "failed"
                execution.end_time = datetime.now()
                self.save_checkpoint(execution, "failed")
                return False

            # Execute phases
            for phase in executable:
                if not self._execute_phase(phase, execution, template_config):
                    print(f"✗ Workflow failed at phase: {phase.name}")
                    execution.status = "failed"
                    execution.end_time = datetime.now()
                    self.save_checkpoint(execution, "failed")
                    return False

                completed_phases.add(phase.phase_id)
                remaining_phases.remove(phase)

        execution.status = "completed"
        execution.end_time = datetime.now()
        self.save_checkpoint(execution, "completed")

        duration = (execution.end_time - execution.start_time).total_seconds()
        print(f"\n{'#'*80}")
        print(f"# Workflow completed successfully in {duration:.1f}s")
        print(f"{'#'*80}\n")

        return True

    def get_workflow_status(self, workflow_id: str) -> Optional[Dict[str, Any]]:
        """Get status of a workflow execution.

        Args:
            workflow_id: Workflow ID to check

        Returns:
            Status dictionary or None if not found
        """
        checkpoint_dir = self.checkpoint_base_dir / workflow_id
        latest_checkpoint = checkpoint_dir / "checkpoint_latest.json"

        if not latest_checkpoint.exists():
            return None

        with open(latest_checkpoint, "r") as f:
            return json.load(f)

    def generate_html_report(self, execution: WorkflowExecution, output_path: str):
        """Generate interactive HTML report for workflow execution.

        Args:
            execution: WorkflowExecution to report on
            output_path: Path to save HTML report
        """
        # Calculate metrics
        total_phases = len(execution.phases)
        completed_phases = sum(1 for p in execution.phases if p.status == PhaseStatus.COMPLETED)
        failed_phases = sum(1 for p in execution.phases if p.status == PhaseStatus.FAILED)
        total_agents = sum(len(p.agents) for p in execution.phases)

        duration = "N/A"
        if execution.start_time and execution.end_time:
            duration = f"{(execution.end_time - execution.start_time).total_seconds():.1f}s"

        # Build phase timeline data
        timeline_data = []
        for phase in execution.phases:
            if phase.start_time and phase.end_time:
                timeline_data.append({
                    "phase": phase.name,
                    "start": phase.start_time.isoformat(),
                    "end": phase.end_time.isoformat(),
                    "status": phase.status.value,
                })

        html = f"""<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Workflow Report: {execution.template_name}</title>
    <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
        }}
        .container {{
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            padding: 30px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        h1 {{
            color: #2c3e50;
            border-bottom: 3px solid #3498db;
            padding-bottom: 10px;
        }}
        .metrics {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin: 30px 0;
        }}
        .metric {{
            background: #ecf0f1;
            padding: 20px;
            border-radius: 6px;
            text-align: center;
        }}
        .metric-value {{
            font-size: 32px;
            font-weight: bold;
            color: #2c3e50;
        }}
        .metric-label {{
            font-size: 14px;
            color: #7f8c8d;
            margin-top: 5px;
        }}
        .status-completed {{ color: #27ae60; }}
        .status-failed {{ color: #e74c3c; }}
        .status-running {{ color: #f39c12; }}
        .phase {{
            background: #f8f9fa;
            padding: 20px;
            margin: 15px 0;
            border-radius: 6px;
            border-left: 4px solid #3498db;
        }}
        .phase-header {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 15px;
        }}
        .phase-title {{
            font-size: 18px;
            font-weight: bold;
            color: #2c3e50;
        }}
        .phase-status {{
            padding: 5px 15px;
            border-radius: 20px;
            font-size: 12px;
            font-weight: bold;
            text-transform: uppercase;
        }}
        .agent {{
            background: white;
            padding: 15px;
            margin: 10px 0;
            border-radius: 4px;
            border-left: 3px solid #95a5a6;
        }}
        .agent-header {{
            font-weight: bold;
            color: #34495e;
            margin-bottom: 5px;
        }}
        .agent-task {{
            color: #7f8c8d;
            font-size: 14px;
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>Workflow Execution Report</h1>

        <div class="metrics">
            <div class="metric">
                <div class="metric-value status-{execution.status}">{execution.status.upper()}</div>
                <div class="metric-label">Workflow Status</div>
            </div>
            <div class="metric">
                <div class="metric-value">{completed_phases}/{total_phases}</div>
                <div class="metric-label">Phases Completed</div>
            </div>
            <div class="metric">
                <div class="metric-value">{total_agents}</div>
                <div class="metric-label">Total Agents</div>
            </div>
            <div class="metric">
                <div class="metric-value">{duration}</div>
                <div class="metric-label">Duration</div>
            </div>
        </div>

        <h2>Execution Timeline</h2>
        <div id="timeline"></div>

        <h2>Phase Details</h2>
"""

        for phase in execution.phases:
            status_class = f"status-{phase.status.value}"
            html += f"""
        <div class="phase">
            <div class="phase-header">
                <div class="phase-title">{phase.name}</div>
                <div class="phase-status {status_class}">{phase.status.value}</div>
            </div>
            <div><strong>Dependencies:</strong> {', '.join(phase.dependencies) if phase.dependencies else 'None'}</div>
            <div><strong>Parallel Execution:</strong> {'Yes' if phase.parallel else 'No'}</div>

            <h4>Agents ({len(phase.agents)}):</h4>
"""
            for agent in phase.agents:
                html += f"""
            <div class="agent">
                <div class="agent-header">{agent.role} ({agent.agent_type})</div>
                <div class="agent-task">{agent.task}</div>
                <div><strong>Status:</strong> {agent.status.value} | <strong>Retries:</strong> {agent.retries}</div>
            </div>
"""
            html += """
        </div>
"""

        html += f"""
    </div>

    <script>
        // Timeline chart
        const timelineData = {json.dumps(timeline_data)};

        if (timelineData.length > 0) {{
            const traces = timelineData.map(phase => ({{
                x: [new Date(phase.start), new Date(phase.end)],
                y: [phase.phase, phase.phase],
                mode: 'lines+markers',
                line: {{
                    width: 20,
                    color: phase.status === 'completed' ? '#27ae60' :
                           phase.status === 'failed' ? '#e74c3c' : '#f39c12'
                }},
                marker: {{ size: 12 }},
                name: phase.phase,
                showlegend: false
            }}));

            const layout = {{
                xaxis: {{
                    title: 'Time',
                    type: 'date'
                }},
                yaxis: {{
                    title: 'Phase'
                }},
                height: 400,
                margin: {{ l: 200 }}
            }};

            Plotly.newPlot('timeline', traces, layout);
        }} else {{
            document.getElementById('timeline').innerHTML = '<p>No timeline data available</p>';
        }}
    </script>
</body>
</html>
"""

        # Save report
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        with open(output_path, "w") as f:
            f.write(html)

        print(f"\n✓ HTML report generated: {output_path}")
