#!/usr/bin/env python3
"""
ABOUTME: MYSTRAN FEM solver interface — Nastran BDF deck generation, solver
execution, F06/OP2 result parsing, and mesh-convergence studies.
"""

from .bdf_writer import BDFWriter
from .result_parser import MystranResultParser
from .convergence import MeshConvergenceStudy, richardson_extrapolation
from .fem_chain import MystranChain, is_mystran_available, find_mystran

__all__ = [
    "BDFWriter",
    "MystranResultParser",
    "MeshConvergenceStudy",
    "richardson_extrapolation",
    "MystranChain",
    "is_mystran_available",
    "find_mystran",
]
