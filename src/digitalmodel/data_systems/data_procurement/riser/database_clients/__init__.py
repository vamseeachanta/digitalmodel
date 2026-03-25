# ABOUTME: Riser component database clients
# ABOUTME: Pipe, buoyancy module, VIV suppression device databases

"""
Riser Database Clients
=======================

Database clients for riser components:
- PipeSpecificationClient: API 5L/5CT pipe database
- BuoyancyModuleClient: Syntactic foam and air-can buoyancy
- VIVSuppressionClient: Strakes and fairings
"""

from .pipe_db_client import PipeSpecificationClient

__all__ = ['PipeSpecificationClient']
