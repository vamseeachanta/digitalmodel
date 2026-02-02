"""
OrcaFlex Examples Integration Module

This module provides tools for downloading, converting, and analyzing OrcaFlex examples
from the official Orcina portal.
"""

from .downloader import OrcaflexExampleDownloader
from .converter import YamlConverter
from .analyzer import FeatureAnalyzer
from .integrator import KnowledgeIntegrator

__all__ = [
    'OrcaflexExampleDownloader',
    'YamlConverter',
    'FeatureAnalyzer',
    'KnowledgeIntegrator'
]

__version__ = '1.0.0'