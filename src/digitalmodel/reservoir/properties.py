"""Reservoir Properties Module.

This module provides classes and functions for calculating and managing
reservoir rock and fluid properties essential for reservoir engineering.

Classes:
    ReservoirProperties: Main container for all reservoir properties
    RockProperties: Rock matrix and pore properties
    FluidProperties: Oil, gas, and water properties
    PVTProperties: Pressure-volume-temperature relationships

Functions:
    calculate_porosity: Calculate porosity from various log measurements
    calculate_permeability: Estimate permeability from porosity
    calculate_water_saturation: Calculate water saturation using various methods
"""

import numpy as np
import pandas as pd
from typing import Optional, Union, Dict, Any, List, Tuple
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
import warnings


@dataclass
class RockProperties:
    """Rock matrix and pore properties.
    
    Attributes:
        porosity: Effective porosity (fraction)
        permeability: Absolute permeability (mD)
        net_to_gross: Net-to-gross ratio (fraction)
        thickness: Net reservoir thickness (ft)
        compressibility: Rock compressibility (1/psi)
        grain_density: Grain density (g/cm³)
        bulk_density: Bulk density (g/cm³)
        formation_factor: Formation resistivity factor
        cementation_exponent: Archie's cementation exponent (m)
        saturation_exponent: Archie's saturation exponent (n)
    """
    porosity: float
    permeability: float
    net_to_gross: float = 1.0
    thickness: Optional[float] = None
    compressibility: Optional[float] = None
    grain_density: float = 2.65
    bulk_density: Optional[float] = None
    formation_factor: Optional[float] = None
    cementation_exponent: float = 2.0
    saturation_exponent: float = 2.0
    
    def __post_init__(self):
        """Validate rock properties."""
        if not 0 <= self.porosity <= 1:
            raise ValueError("Porosity must be between 0 and 1")
        if self.permeability < 0:
            raise ValueError("Permeability cannot be negative")
        if not 0 <= self.net_to_gross <= 1:
            raise ValueError("Net-to-gross ratio must be between 0 and 1")
    
    def calculate_bulk_density(self) -> float:
        """Calculate bulk density from porosity and grain density.
        
        Returns:
            Bulk density in g/cm³
        """
        if self.bulk_density is not None:
            return self.bulk_density
        
        # Assume water density of 1.0 g/cm³
        water_density = 1.0
        bulk_density = (1 - self.porosity) * self.grain_density + self.porosity * water_density
        return bulk_density
    
    def calculate_formation_factor(self) -> float:
        """Calculate formation factor using Archie's equation.
        
        Returns:
            Formation factor (dimensionless)
        """
        if self.formation_factor is not None:
            return self.formation_factor
        
        # Archie's equation: F = a * φ^(-m), assume a = 1
        formation_factor = self.porosity ** (-self.cementation_exponent)
        return formation_factor


@dataclass
class FluidProperties:
    """Oil, gas, and water properties.
    
    Attributes:
        oil_density: Oil density (°API or g/cm³)
        gas_density: Gas specific gravity (air = 1)
        water_density: Water density (g/cm³)
        oil_viscosity: Oil viscosity (cp)
        gas_viscosity: Gas viscosity (cp)
        water_viscosity: Water viscosity (cp)
        oil_formation_volume_factor: Oil FVF (res bbl/STB)
        gas_formation_volume_factor: Gas FVF (res ft³/scf)
        solution_gor: Solution gas-oil ratio (scf/STB)
        bubble_point_pressure: Bubble point pressure (psi)
        water_compressibility: Water compressibility (1/psi)
    """
    oil_density: Optional[float] = None  # °API
    gas_density: Optional[float] = None  # Specific gravity
    water_density: float = 1.0  # g/cm³
    oil_viscosity: Optional[float] = None  # cp
    gas_viscosity: Optional[float] = None  # cp
    water_viscosity: float = 1.0  # cp
    oil_formation_volume_factor: float = 1.0  # res bbl/STB
    gas_formation_volume_factor: float = 1.0  # res ft³/scf
    solution_gor: float = 0.0  # scf/STB
    bubble_point_pressure: Optional[float] = None  # psi
    water_compressibility: float = 3.0e-6  # 1/psi
    
    def api_to_specific_gravity(self) -> Optional[float]:
        """Convert API gravity to specific gravity.
        
        Returns:
            Oil specific gravity (water = 1)
        """
        if self.oil_density is None:
            return None
        
        # API to specific gravity conversion
        specific_gravity = 141.5 / (self.oil_density + 131.5)
        return specific_gravity
    
    def calculate_oil_density_from_api(self) -> Optional[float]:
        """Calculate oil density from API gravity.
        
        Returns:
            Oil density in g/cm³
        """
        sg = self.api_to_specific_gravity()
        if sg is None:
            return None
        
        return sg * self.water_density


@dataclass
class PVTProperties:
    """Pressure-Volume-Temperature relationships.
    
    Attributes:
        pressure: Reservoir pressure (psi)
        temperature: Reservoir temperature (°F)
        initial_pressure: Initial reservoir pressure (psi)
        bubble_point: Bubble point pressure (psi)
        dew_point: Dew point pressure (psi)
        z_factor: Gas compressibility factor
        gas_expansion_factor: Gas expansion factor
    """
    pressure: float
    temperature: float
    initial_pressure: Optional[float] = None
    bubble_point: Optional[float] = None
    dew_point: Optional[float] = None
    z_factor: float = 1.0
    gas_expansion_factor: Optional[float] = None
    
    def __post_init__(self):
        """Set initial pressure if not provided."""
        if self.initial_pressure is None:
            self.initial_pressure = self.pressure
    
    def calculate_gas_expansion_factor(self) -> float:
        """Calculate gas expansion factor.
        
        Returns:
            Gas expansion factor (dimensionless)
        """
        if self.gas_expansion_factor is not None:
            return self.gas_expansion_factor
        
        if self.initial_pressure is None:
            raise ValueError("Initial pressure is required for gas expansion factor calculation")
        
        # Simple gas expansion factor calculation
        expansion_factor = (self.initial_pressure * self.z_factor) / (self.pressure * 1.0)
        return expansion_factor


@dataclass
class ReservoirProperties:
    """Main container for all reservoir properties.
    
    Combines rock properties, fluid properties, and PVT data
    for comprehensive reservoir characterization.
    
    Attributes:
        rock: Rock properties
        fluids: Fluid properties
        pvt: PVT properties
        area: Reservoir area (acres)
        depth: Average reservoir depth (ft)
        water_saturation: Water saturation (fraction)
        oil_saturation: Oil saturation (fraction)
        gas_saturation: Gas saturation (fraction)
    """
    rock: RockProperties
    fluids: FluidProperties = field(default_factory=FluidProperties)
    pvt: PVTProperties = field(default_factory=lambda: PVTProperties(pressure=2000, temperature=180))
    area: Optional[float] = None
    depth: Optional[float] = None
    water_saturation: float = 0.3
    oil_saturation: Optional[float] = None
    gas_saturation: Optional[float] = None
    
    def __post_init__(self):
        """Calculate missing saturation values."""
        # Ensure saturations sum to 1
        if self.oil_saturation is None and self.gas_saturation is None:
            self.oil_saturation = 1.0 - self.water_saturation
            self.gas_saturation = 0.0
        elif self.oil_saturation is None:
            self.oil_saturation = 1.0 - self.water_saturation - self.gas_saturation
        elif self.gas_saturation is None:
            self.gas_saturation = 1.0 - self.water_saturation - self.oil_saturation
        
        # Validate saturations
        total_saturation = self.water_saturation + self.oil_saturation + self.gas_saturation
        if not 0.99 <= total_saturation <= 1.01:
            warnings.warn(f"Total saturation ({total_saturation:.3f}) does not equal 1.0")
    
    def calculate_pore_volume(self) -> Optional[float]:
        """Calculate total pore volume.
        
        Returns:
            Pore volume in acre-ft
        """
        if self.area is None or self.rock.thickness is None:
            return None
        
        pore_volume = self.area * self.rock.thickness * self.rock.porosity * self.rock.net_to_gross
        return pore_volume
    
    def calculate_hydrocarbon_pore_volume(self) -> Optional[float]:
        """Calculate hydrocarbon pore volume.
        
        Returns:
            Hydrocarbon pore volume in acre-ft
        """
        pv = self.calculate_pore_volume()
        if pv is None:
            return None
        
        hc_saturation = self.oil_saturation + self.gas_saturation
        return pv * hc_saturation
    
    def calculate_ooip(self) -> Optional[float]:
        """Calculate Original Oil In Place (OOIP).
        
        Returns:
            OOIP in stock tank barrels (STB)
        """
        hcpv = self.calculate_hydrocarbon_pore_volume()
        if hcpv is None or self.oil_saturation <= 0:
            return None
        
        # Convert acre-ft to reservoir barrels and then to stock tank barrels
        reservoir_barrels = hcpv * 7758  # acre-ft to bbl
        ooip = (reservoir_barrels * self.oil_saturation / 
                (self.oil_saturation + self.gas_saturation)) / self.fluids.oil_formation_volume_factor
        
        return ooip
    
    def calculate_ogip(self) -> Optional[float]:
        """Calculate Original Gas In Place (OGIP).
        
        Returns:
            OGIP in standard cubic feet (scf)
        """
        hcpv = self.calculate_hydrocarbon_pore_volume()
        if hcpv is None or self.gas_saturation <= 0:
            return None
        
        # Convert acre-ft to reservoir ft³ and then to standard ft³
        reservoir_ft3 = hcpv * 43560  # acre-ft to ft³
        ogip = (reservoir_ft3 * self.gas_saturation / 
                (self.oil_saturation + self.gas_saturation)) / self.fluids.gas_formation_volume_factor
        
        return ogip


# Utility Functions
def calculate_porosity(
    neutron_porosity: float,
    density_porosity: float,
    photoelectric_factor: Optional[float] = None,
    shale_correction: bool = True
) -> float:
    """Calculate effective porosity from log measurements.
    
    Args:
        neutron_porosity: Neutron porosity reading (fraction)
        density_porosity: Density porosity reading (fraction)
        photoelectric_factor: Photoelectric factor for lithology correction
        shale_correction: Whether to apply shale correction
    
    Returns:
        Effective porosity (fraction)
    """
    if shale_correction:
        # Simple neutron-density porosity averaging with shale correction
        porosity = np.sqrt((neutron_porosity**2 + density_porosity**2) / 2)
    else:
        # Simple average
        porosity = (neutron_porosity + density_porosity) / 2
    
    return max(0.0, min(1.0, porosity))


def calculate_permeability(
    porosity: float,
    method: str = "kozeny_carman",
    **kwargs
) -> float:
    """Estimate permeability from porosity using various correlations.
    
    Args:
        porosity: Porosity (fraction)
        method: Correlation method ('kozeny_carman', 'timur', 'coates')
        **kwargs: Additional parameters for specific methods
    
    Returns:
        Permeability in millidarcies (mD)
    """
    if method == "kozeny_carman":
        # Simplified Kozeny-Carman equation
        # k = C * φ³ / (1-φ)², where C is a constant
        c = kwargs.get('kozeny_constant', 5000)
        perm = c * (porosity**3) / ((1 - porosity)**2)
    
    elif method == "timur":
        # Timur correlation: k = 8581 * φ^4.4 / Swi^2
        swi = kwargs.get('irreducible_water_saturation', 0.2)
        perm = 8581 * (porosity**4.4) / (swi**2)
    
    elif method == "coates":
        # Coates correlation: k = (100 * φ)^6 / (Swi^2)
        swi = kwargs.get('irreducible_water_saturation', 0.2)
        perm = ((100 * porosity)**6) / (swi**2)
    
    else:
        raise ValueError(f"Unknown permeability correlation method: {method}")
    
    return max(0.001, perm)  # Minimum 0.001 mD


def calculate_water_saturation(
    resistivity_water: float,
    resistivity_formation: float,
    porosity: float,
    method: str = "archie",
    **kwargs
) -> float:
    """Calculate water saturation using various methods.
    
    Args:
        resistivity_water: Formation water resistivity (ohm-m)
        resistivity_formation: True formation resistivity (ohm-m)
        porosity: Porosity (fraction)
        method: Calculation method ('archie', 'waxman_smits', 'simandoux')
        **kwargs: Additional parameters for specific methods
    
    Returns:
        Water saturation (fraction)
    """
    if method == "archie":
        # Archie's equation: Sw = ((a * Rw) / (φ^m * Rt))^(1/n)
        a = kwargs.get('tortuosity_factor', 1.0)
        m = kwargs.get('cementation_exponent', 2.0)
        n = kwargs.get('saturation_exponent', 2.0)
        
        sw = ((a * resistivity_water) / (porosity**m * resistivity_formation))**(1/n)
    
    elif method == "waxman_smits":
        # Simplified Waxman-Smits for shaly sands
        # Additional parameters needed for full implementation
        cec = kwargs.get('cation_exchange_capacity', 0.1)
        # Simplified version - full implementation would need iterative solution
        sw = ((resistivity_water) / (porosity**2 * resistivity_formation))**(1/2)
        sw *= (1 + 0.1 * cec)  # Simplified shale correction
    
    elif method == "simandoux":
        # Simandoux equation for shaly sands
        vsh = kwargs.get('shale_volume', 0.1)
        rsh = kwargs.get('shale_resistivity', 2.0)
        
        # Simplified Simandoux
        a = 1 / resistivity_formation
        b = vsh / rsh
        c = (porosity**2) / resistivity_water
        
        sw = ((-b + np.sqrt(b**2 + 4*a*c)) / (2*c))**0.5
    
    else:
        raise ValueError(f"Unknown water saturation calculation method: {method}")
    
    return max(0.0, min(1.0, sw))


# Example reservoir types for quick setup
def create_sandstone_reservoir(
    porosity: float,
    permeability: float,
    water_saturation: float = 0.3,
    **kwargs
) -> ReservoirProperties:
    """Create a typical sandstone reservoir.
    
    Args:
        porosity: Porosity (fraction)
        permeability: Permeability (mD)
        water_saturation: Water saturation (fraction)
        **kwargs: Additional reservoir parameters
    
    Returns:
        ReservoirProperties instance for sandstone
    """
    rock = RockProperties(
        porosity=porosity,
        permeability=permeability,
        grain_density=2.65,
        cementation_exponent=2.0,
        saturation_exponent=2.0,
        **{k: v for k, v in kwargs.items() if k in RockProperties.__dataclass_fields__}
    )
    
    fluids = FluidProperties(
        oil_density=kwargs.get('oil_api', 35),
        gas_density=kwargs.get('gas_sg', 0.7),
        **{k: v for k, v in kwargs.items() if k in FluidProperties.__dataclass_fields__}
    )
    
    pvt = PVTProperties(
        pressure=kwargs.get('pressure', 3000),
        temperature=kwargs.get('temperature', 200),
        **{k: v for k, v in kwargs.items() if k in PVTProperties.__dataclass_fields__}
    )
    
    return ReservoirProperties(
        rock=rock,
        fluids=fluids,
        pvt=pvt,
        water_saturation=water_saturation,
        **{k: v for k, v in kwargs.items() if k in ReservoirProperties.__dataclass_fields__}
    )


def create_carbonate_reservoir(
    porosity: float,
    permeability: float,
    water_saturation: float = 0.2,
    **kwargs
) -> ReservoirProperties:
    """Create a typical carbonate reservoir.
    
    Args:
        porosity: Porosity (fraction)
        permeability: Permeability (mD)
        water_saturation: Water saturation (fraction)
        **kwargs: Additional reservoir parameters
    
    Returns:
        ReservoirProperties instance for carbonate
    """
    rock = RockProperties(
        porosity=porosity,
        permeability=permeability,
        grain_density=2.71,
        cementation_exponent=2.0,  # Can vary widely in carbonates
        saturation_exponent=2.0,
        **{k: v for k, v in kwargs.items() if k in RockProperties.__dataclass_fields__}
    )
    
    fluids = FluidProperties(
        oil_density=kwargs.get('oil_api', 30),
        gas_density=kwargs.get('gas_sg', 0.8),
        **{k: v for k, v in kwargs.items() if k in FluidProperties.__dataclass_fields__}
    )
    
    pvt = PVTProperties(
        pressure=kwargs.get('pressure', 4000),
        temperature=kwargs.get('temperature', 220),
        **{k: v for k, v in kwargs.items() if k in PVTProperties.__dataclass_fields__}
    )
    
    return ReservoirProperties(
        rock=rock,
        fluids=fluids,
        pvt=pvt,
        water_saturation=water_saturation,
        **{k: v for k, v in kwargs.items() if k in ReservoirProperties.__dataclass_fields__}
    )
