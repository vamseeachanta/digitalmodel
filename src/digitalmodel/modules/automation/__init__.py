"""
ABOUTME: Automation module for intelligent agent orchestration.
Provides multi-factor agent selection, performance tracking, and workflow execution.
"""

from .intelligent_agent_selector import (
    AgentPerformanceTracker,
    IntelligentAgentSelector,
)
from .work_stealing_scheduler import (
    WorkStealingScheduler,
    Agent,
    Task,
    WorkStealingMetrics,
)
from .workflow_executor import (
    WorkflowExecutor,
    WorkflowExecution,
    PhaseExecution,
    AgentExecution,
    PhaseStatus,
    AgentStatus,
)

__all__ = [
    "AgentPerformanceTracker",
    "IntelligentAgentSelector",
    "WorkStealingScheduler",
    "Agent",
    "Task",
    "WorkStealingMetrics",
    "WorkflowExecutor",
    "WorkflowExecution",
    "PhaseExecution",
    "AgentExecution",
    "PhaseStatus",
    "AgentStatus",
]
