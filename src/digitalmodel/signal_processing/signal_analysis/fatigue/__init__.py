"""
Fatigue analysis module

Specialized implementations for fatigue damage calculations:
- S-N curve implementations
- Damage accumulation methods
- Fatigue life predictions
"""

from .damage import FatigueDamageCalculator
from .curves import SNCurve

__all__ = ["FatigueDamageCalculator", "SNCurve"]