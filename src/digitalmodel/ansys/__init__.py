# ABOUTME: ANSYS FEA domain — APDL file parsing, design point logs, Workbench journals
# ABOUTME: No ANSYS software required; pure file parsing for AI-agent FEA workflows

"""
ANSYS FEA Domain
================

Public API:
    APDLReader          — parse .inp materials and sections
    DesignPointReader   — parse DesignPointLog.csv parametric studies
    WBJNReader          — parse .wbjn journal metadata
    APDLMaterial        — dataclass for material properties
    APDLSection         — dataclass for beam/rod sections
    DesignPoint         — dataclass for a single parametric design point
    ParametricStudy     — dataclass for full DesignXplorer study
    WBJNJournal         — dataclass for journal metadata
"""

from digitalmodel.ansys.apdl_reader import APDLReader
from digitalmodel.ansys.design_points import DesignPointReader
from digitalmodel.ansys.wbjn_reader import WBJNReader
from digitalmodel.ansys.models import (
    APDLMaterial,
    APDLSection,
    DesignPoint,
    ParametricStudy,
    WBJNJournal,
)

__all__ = [
    "APDLReader",
    "DesignPointReader",
    "WBJNReader",
    "APDLMaterial",
    "APDLSection",
    "DesignPoint",
    "ParametricStudy",
    "WBJNJournal",
]
