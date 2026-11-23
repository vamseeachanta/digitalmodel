# ABOUTME: Riser systems data procurement module
# ABOUTME: Provides access to pipe specifications, buoyancy modules, VIV suppression devices

"""
Riser Systems Data Procurement
================================

Comprehensive access to riser component data:
- Pipe specifications (API 5L, API 5CT)
- Buoyancy modules (syntactic foam, air-can)
- VIV suppression devices (strakes, fairings)
- Riser joints (flex joints, ball joints, telescopic)

Example:
    from digitalmodel.data_systems.data_procurement import RiserClient

    client = RiserClient()

    # Get pipe specification
    pipe = client.get_pipe_specification(diameter=10, schedule='SCH 80', grade='X52')

    # Calculate properties with coatings
    props = client.calculate_properties(
        pipe=pipe,
        coatings=[
            {'type': '3LPE', 'thickness': 3.2, 'density': 940},
            {'type': 'insulation', 'thickness': 50, 'density': 500}
        ]
    )
"""

from .client import RiserClient

__all__ = ['RiserClient']
