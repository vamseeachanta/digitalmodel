#!/usr/bin/env python3
"""
ABOUTME: Workflow automation module for orchestrating end-to-end engineering
analysis workflows combining multiple modules into complete solutions.
"""

__version__ = "1.0.0"

from .models import (
    WorkflowDefinition,
    WorkflowTask,
    WorkflowResult,
    TaskStatus,
    WorkflowConfig,
)
from .orchestrator import WorkflowOrchestrator
from .workflows import (
    CompleteRiserAnalysisWorkflow,
    MooringSystemDesignWorkflow,
    PlatformStructuralWorkflow,
    VesselResponseWorkflow,
)
from .reporter import WorkflowHTMLReporter, generate_workflow_report

__all__ = [
    "WorkflowDefinition",
    "WorkflowTask",
    "WorkflowResult",
    "TaskStatus",
    "WorkflowConfig",
    "WorkflowOrchestrator",
    "CompleteRiserAnalysisWorkflow",
    "MooringSystemDesignWorkflow",
    "PlatformStructuralWorkflow",
    "VesselResponseWorkflow",
    "WorkflowHTMLReporter",
    "generate_workflow_report",
]
