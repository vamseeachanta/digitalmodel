#!/usr/bin/env python3
"""
ABOUTME: CalculiX FEM solver interface — INP file generation, solver execution,
and result parsing for structural analysis via CalculiX ccx.
"""

from .inp_writer import INPWriter
from .result_parser import CalculiXResultParser
from .fem_chain import FEMChain
from .model_checker import check_deck, validate_model, ModelCheckError, ModelCheckReport

__all__ = ["INPWriter", "CalculiXResultParser", "FEMChain", "check_deck",
           "validate_model", "ModelCheckError", "ModelCheckReport"]
