#!/usr/bin/env python3
"""
ABOUTME: Workflow automation module for orchestrating end-to-end engineering
analysis workflows combining multiple modules into complete solutions.
"""

__version__ = "1.1.0"

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
from .cache import WorkflowCache, CacheManager
from .parallel import ParallelExecutor, AdaptiveParallelExecutor
from .callbacks import (
    ProgressTracker,
    CallbackHandler,
    ConsoleCallback,
    FileCallback,
    JSONCallback,
    WorkflowEvent,
    ProgressUpdate,
)
from .templates import (
    WorkflowTemplate,
    TemplateLibrary,
    create_riser_analysis_template,
    create_mooring_design_template,
)

__all__ = [
    # Core models
    "WorkflowDefinition",
    "WorkflowTask",
    "WorkflowResult",
    "TaskStatus",
    "WorkflowConfig",
    # Orchestration
    "WorkflowOrchestrator",
    # Pre-built workflows
    "CompleteRiserAnalysisWorkflow",
    "MooringSystemDesignWorkflow",
    "PlatformStructuralWorkflow",
    "VesselResponseWorkflow",
    # Reporting
    "WorkflowHTMLReporter",
    "generate_workflow_report",
    # Caching
    "WorkflowCache",
    "CacheManager",
    # Parallel execution
    "ParallelExecutor",
    "AdaptiveParallelExecutor",
    # Progress callbacks
    "ProgressTracker",
    "CallbackHandler",
    "ConsoleCallback",
    "FileCallback",
    "JSONCallback",
    "WorkflowEvent",
    "ProgressUpdate",
    # Templates
    "WorkflowTemplate",
    "TemplateLibrary",
    "create_riser_analysis_template",
    "create_mooring_design_template",
]
