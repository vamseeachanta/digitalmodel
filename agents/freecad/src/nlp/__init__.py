"""
Natural Language Processing module for FreeCAD Agent
"""

from .parser import NaturalLanguageParser, ParsedCommand, CommandIntent, ObjectType
from .generator import ScriptGenerator
from .templates import ScriptTemplate, TemplateLibrary
from .ai_integration import AIIntegration

__all__ = [
    'NaturalLanguageParser',
    'ParsedCommand',
    'CommandIntent', 
    'ObjectType',
    'ScriptGenerator',
    'ScriptTemplate',
    'TemplateLibrary',
    'AIIntegration'
]