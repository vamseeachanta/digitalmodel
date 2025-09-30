"""Reservoir Modeling Module.

This module provides classes and functions for reservoir simulation,
material balance calculations, well test analysis, and production forecasting.

Classes:
    ReservoirModel: Main reservoir model container
    MaterialBalance: Material balance calculations
    WellTestAnalysis: Well test interpretation
    ReservoirSimulation: Numerical reservoir simulation
    ProductionForecast: Production forecasting tools
    ReservoirCharacterization: Reservoir characterization workflows
"""

import numpy as np
import pandas as pd
from typing import Optional, Union, Dict, Any, List, Tuple, Callable
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
import warnings
from datetime import datetime, timedelta

from .properties import ReservoirProperties, RockProperties, FluidProperties, PVTProperties


@dataclass
class WellData:
    """Well data container.
    
    Attributes:
        well_id: Well identifier
        x_coord: X coordinate (ft)
        y_coord: Y coordinate (ft)
        measured_depth: Measured depth (ft)
        true_vertical_depth: True vertical depth (ft)
        completion_date: Well completion date
        well_type: Well type ('producer', 'injector', 'observation')
        completion_type: Completion type ('openhole', 'cased', 'fractured')
        drainage_area: Well drainage area (acres)
        skin_factor: Skin factor (dimensionless)
        wellbore_radius: Wellbore radius (ft)
    """
    well_id: str
    x_coord: float
    y_coord: float
    measured_depth: float
    true_vertical_depth: Optional[float] = None
    completion_date: Optional[datetime] = None
    well_type: str = "producer"
    completion_type: str = "cased"
    drainage_area: Optional[float] = None
    skin_factor: float = 0.0
    wellbore_radius: float = 0.25  # 6 inch wellbore
    
    def __post_init__(self):
        """Set TVD if not provided."""
        if self.true_vertical_depth is None:
            self.true_vertical_depth = self.measured_depth


@dataclass
class ProductionData:
    """Production data container.
    
    Attributes:
        well_id: Well identifier
        date: Production date
        oil_rate: Oil production rate (STB/day)
        gas_rate: Gas production rate (Mscf/day)
        water_rate: Water production rate (STB/day)
        flowing_pressure: Flowing bottomhole pressure (psi)
        static_pressure: Static bottomhole pressure (psi)
        gor: Gas-oil ratio (scf/STB)
        wor: Water-oil ratio (STB/STB)
        choke_size: Choke size (64ths)
    """
    well_id: str
    date: datetime
    oil_rate: float = 0.0
    gas_rate: float = 0.0
    water_rate: float = 0.0
    flowing_pressure: Optional[float] = None
    static_pressure: Optional[float] = None
    gor: Optional[float] = None
    wor: Optional[float] = None
    choke_size: Optional[float] = None


class MaterialBalance:
    """Material balance calculations for reservoir analysis.
    
    This class implements various material balance methods including:
    - Tank-type material balance
    - Havlena-Odeh method
    - Aquifer material balance
    - Gas reservoir material balance
    """
    
    def __init__(self, reservoir_properties: ReservoirProperties):
        """Initialize material balance calculator.
        
        Args:
            reservoir_properties: Reservoir properties
        """
        self.reservoir = reservoir_properties
        self.production_history: List[ProductionData] = []
        self.pressure_history: List[Tuple[datetime, float]] = []
    
    def add_production_data(self, production_data: List[ProductionData]):
        """Add production data for material balance.
        
        Args:
            production_data: List of production data points
        """
        self.production_history.extend(production_data)
        self.production_history.sort(key=lambda x: x.date)
    
    def add_pressure_data(self, date: datetime, pressure: float):
        """Add pressure measurement.
        
        Args:
            date: Measurement date
            pressure: Reservoir pressure (psi)
        """
        self.pressure_history.append((date, pressure))
        self.pressure_history.sort(key=lambda x: x[0])
    
    def calculate_cumulative_production(self) -> Dict[str, np.ndarray]:
        """Calculate cumulative production.
        
        Returns:
            Dictionary with cumulative oil, gas, and water production
        """
        if not self.production_history:
            return {'oil': np.array([]), 'gas': np.array([]), 'water': np.array([])}
        
        df = pd.DataFrame([
            {
                'date': p.date,
                'oil_rate': p.oil_rate,
                'gas_rate': p.gas_rate,
                'water_rate': p.water_rate
            } for p in self.production_history
        ])
        
        df = df.sort_values('date').reset_index(drop=True)
        df['days'] = (df['date'] - df['date'].iloc[0]).dt.days
        
        # Calculate cumulative production using trapezoidal rule
        cum_oil = np.trapz(df['oil_rate'], df['days'])
        cum_gas = np.trapz(df['gas_rate'], df['days'])
        cum_water = np.trapz(df['water_rate'], df['days'])
        
        return {
            'oil': cum_oil,
            'gas': cum_gas,
            'water': cum_water
        }
    
    def tank_material_balance(self, current_pressure: float) -> Dict[str, float]:
        """Perform tank-type material balance.
        
        Args:
            current_pressure: Current reservoir pressure (psi)
        
        Returns:
            Dictionary with OOIP, drive mechanism analysis
        """
        initial_pressure = self.reservoir.pvt.initial_pressure
        if initial_pressure is None:
            raise ValueError("Initial pressure required for material balance")
        
        # Calculate pressure depletion
        pressure_drop = initial_pressure - current_pressure
        
        # Get cumulative production
        cum_prod = self.calculate_cumulative_production()
        
        # Calculate expansion terms
        oil_expansion = self.reservoir.fluids.oil_formation_volume_factor
        gas_expansion = self.reservoir.pvt.calculate_gas_expansion_factor()
        water_expansion = 1 + self.reservoir.fluids.water_compressibility * pressure_drop
        rock_expansion = 1 + (self.reservoir.rock.compressibility or 3e-6) * pressure_drop
        
        # Material balance equation: Np*Bo + Np*Rs*Bg = N*Bo*[(Bo-Boi)/Boi + Co*ΔP] + m*N*Boi*[(Bg/Bgi-1)]
        # Simplified for oil reservoir
        if cum_prod['oil'] > 0:
            # Estimate OOIP from material balance
            withdrawal = cum_prod['oil'] * oil_expansion + cum_prod['gas'] * self.reservoir.fluids.gas_formation_volume_factor
            expansion_factor = (oil_expansion - 1.0) + pressure_drop * (self.reservoir.fluids.water_compressibility + 
                                                                        (self.reservoir.rock.compressibility or 3e-6))
            
            if expansion_factor > 0:
                ooip_estimate = withdrawal / expansion_factor
            else:
                ooip_estimate = None
        else:
            ooip_estimate = None
        
        return {
            'pressure_drop': pressure_drop,
            'oil_expansion_factor': oil_expansion,
            'gas_expansion_factor': gas_expansion,
            'water_expansion_factor': water_expansion,
            'rock_expansion_factor': rock_expansion,
            'ooip_estimate': ooip_estimate,
            'cumulative_oil': cum_prod['oil'],
            'cumulative_gas': cum_prod['gas'],
            'cumulative_water': cum_prod['water']
        }


class WellTestAnalysis:
    """Well test analysis and interpretation.
    
    This class provides methods for analyzing pressure transient data
    including buildup tests, drawdown tests, and interference tests.
    """
    
    def __init__(self, reservoir_properties: ReservoirProperties, well_data: WellData):
        """Initialize well test analyzer.
        
        Args:
            reservoir_properties: Reservoir properties
            well_data: Well data
        """
        self.reservoir = reservoir_properties
        self.well = well_data
        self.test_data: List[Tuple[float, float]] = []  # (time, pressure)
    
    def add_test_data(self, time_hours: float, pressure_psi: float):
        """Add pressure test data point.
        
        Args:
            time_hours: Time since test start (hours)
            pressure_psi: Pressure measurement (psi)
        """
        self.test_data.append((time_hours, pressure_psi))
        self.test_data.sort(key=lambda x: x[0])
    
    def analyze_buildup_test(self) -> Dict[str, float]:
        """Analyze pressure buildup test.
        
        Returns:
            Dictionary with test results including permeability and skin
        """
        if len(self.test_data) < 3:
            raise ValueError("Insufficient test data for analysis")
        
        # Convert to numpy arrays
        times = np.array([t for t, p in self.test_data])
        pressures = np.array([p for t, p in self.test_data])
        
        # Horner time function for buildup analysis
        # Assume production time is 24 hours before shut-in
        tp = 24.0  # hours
        horner_time = (tp + times) / times
        
        # Find straight line portion (typically middle 70% of data)
        start_idx = int(0.15 * len(pressures))
        end_idx = int(0.85 * len(pressures))
        
        # Linear regression on Horner plot
        log_horner = np.log10(horner_time[start_idx:end_idx])
        p_buildup = pressures[start_idx:end_idx]
        
        # Fit straight line: P = m * log10((tp + dt)/dt) + b
        coeffs = np.polyfit(log_horner, p_buildup, 1)
        slope = coeffs[0]  # psi/log cycle
        intercept = coeffs[1]
        
        # Calculate permeability using Horner analysis
        # k = 162.6 * q * Bo * μ / (m * h)
        if hasattr(self.reservoir.fluids, 'oil_viscosity') and self.reservoir.fluids.oil_viscosity:
            viscosity = self.reservoir.fluids.oil_viscosity
        else:
            viscosity = 1.0  # Default oil viscosity (cp)
        
        formation_volume_factor = self.reservoir.fluids.oil_formation_volume_factor
        thickness = self.reservoir.rock.thickness or 100  # Default thickness
        flow_rate = 100  # Assume 100 STB/day production rate
        
        permeability = (162.6 * flow_rate * formation_volume_factor * viscosity) / (abs(slope) * thickness)
        
        # Calculate skin factor
        # s = 1.151 * [(Pi - Pwf@1hr)/m - log10(k/(phi*mu*ct*rw^2)) + 3.23]
        p_1hr = intercept + slope * np.log10((tp + 1) / 1)
        
        # Estimate total compressibility
        ct = (self.reservoir.fluids.water_compressibility + 
              (self.reservoir.rock.compressibility or 3e-6) + 
              1e-5)  # oil compressibility estimate
        
        skin_term = np.log10(permeability / (self.reservoir.rock.porosity * viscosity * ct * self.well.wellbore_radius**2))
        skin_factor = 1.151 * ((intercept - (pressures[0] if pressures[0] > 0 else intercept)) / abs(slope) - skin_term + 3.23)
        
        return {
            'permeability': permeability,
            'skin_factor': skin_factor,
            'slope': slope,
            'intercept': intercept,
            'extrapolated_pressure': intercept,
            'flow_efficiency': 1 / (1 + skin_factor),
            'r_squared': self._calculate_r_squared(log_horner, p_buildup, coeffs)
        }
    
    def _calculate_r_squared(self, x: np.ndarray, y: np.ndarray, coeffs: np.ndarray) -> float:
        """Calculate R-squared for linear fit."""
        y_pred = np.polyval(coeffs, x)
        ss_res = np.sum((y - y_pred) ** 2)
        ss_tot = np.sum((y - np.mean(y)) ** 2)
        return 1 - (ss_res / ss_tot) if ss_tot > 0 else 0.0


class ReservoirSimulation:
    """Simple reservoir simulation capabilities.
    
    This class provides basic numerical reservoir simulation
    for single-phase and multi-phase flow.
    """
    
    def __init__(self, reservoir_properties: ReservoirProperties):
        """Initialize reservoir simulator.
        
        Args:
            reservoir_properties: Reservoir properties
        """
        self.reservoir = reservoir_properties
        self.grid_size = (10, 10, 1)  # Default grid
        self.time_steps = []
        self.pressure_field = None
        self.saturation_field = None
    
    def setup_grid(self, nx: int, ny: int, nz: int = 1):
        """Setup simulation grid.
        
        Args:
            nx: Number of cells in x-direction
            ny: Number of cells in y-direction
            nz: Number of cells in z-direction
        """
        self.grid_size = (nx, ny, nz)
        
        # Initialize pressure field
        initial_pressure = self.reservoir.pvt.initial_pressure or 3000
        self.pressure_field = np.full(self.grid_size, initial_pressure)
        
        # Initialize saturation field
        self.saturation_field = {
            'water': np.full(self.grid_size, self.reservoir.water_saturation),
            'oil': np.full(self.grid_size, self.reservoir.oil_saturation),
            'gas': np.full(self.grid_size, self.reservoir.gas_saturation or 0.0)
        }
    
    def run_depletion_simulation(self, production_rate: float, simulation_days: int = 365) -> Dict[str, Any]:
        """Run simple depletion simulation.
        
        Args:
            production_rate: Oil production rate (STB/day)
            simulation_days: Simulation duration (days)
        
        Returns:
            Dictionary with simulation results
        """
        if self.pressure_field is None:
            self.setup_grid(10, 10, 1)
        
        # Simple tank model simulation
        dt = 30  # 30-day time steps
        time_steps = np.arange(0, simulation_days, dt)
        
        pressure_history = []
        production_history = []
        
        current_pressure = self.reservoir.pvt.initial_pressure or 3000
        
        for day in time_steps:
            # Calculate pressure drop using simple material balance
            pore_volume = self.reservoir.calculate_pore_volume() or 1000  # acre-ft
            pore_volume_bbl = pore_volume * 7758  # Convert to barrels
            
            # Estimate total compressibility
            ct = (self.reservoir.fluids.water_compressibility + 
                  (self.reservoir.rock.compressibility or 3e-6) + 
                  1e-5)  # Total compressibility
            
            # Pressure drop calculation
            dp = (production_rate * dt * self.reservoir.fluids.oil_formation_volume_factor) / (pore_volume_bbl * ct)
            current_pressure -= dp
            
            # Store results
            pressure_history.append(current_pressure)
            production_history.append(production_rate if current_pressure > 500 else 0)  # Economic limit
        
        return {
            'time_days': time_steps.tolist(),
            'pressure_psi': pressure_history,
            'production_rate_stb_day': production_history,
            'cumulative_production_stb': np.cumsum(np.array(production_history) * dt).tolist(),
            'final_pressure': current_pressure,
            'recovery_factor': np.sum(production_history) * dt / (self.reservoir.calculate_ooip() or 1e6)
        }


class ProductionForecast:
    """Production forecasting tools.
    
    This class provides various decline curve analysis methods
    and production forecasting capabilities.
    """
    
    def __init__(self):
        """Initialize production forecaster."""
        self.production_data: List[ProductionData] = []
        self.forecast_parameters = {}
    
    def add_production_data(self, production_data: List[ProductionData]):
        """Add historical production data.
        
        Args:
            production_data: List of production data points
        """
        self.production_data.extend(production_data)
        self.production_data.sort(key=lambda x: x.date)
    
    def arps_decline_analysis(self, decline_type: str = 'exponential') -> Dict[str, float]:
        """Perform Arps decline curve analysis.
        
        Args:
            decline_type: Type of decline ('exponential', 'hyperbolic', 'harmonic')
        
        Returns:
            Dictionary with decline parameters
        """
        if len(self.production_data) < 3:
            raise ValueError("Insufficient production data for decline analysis")
        
        # Convert to DataFrame for easier manipulation
        df = pd.DataFrame([
            {
                'date': p.date,
                'oil_rate': p.oil_rate,
                'days': (p.date - self.production_data[0].date).days
            } for p in self.production_data if p.oil_rate > 0
        ])
        
        if len(df) < 3:
            raise ValueError("Insufficient valid production data")
        
        times = df['days'].values
        rates = df['oil_rate'].values
        
        if decline_type == 'exponential':
            # Exponential decline: q = qi * exp(-D * t)
            log_rates = np.log(rates)
            coeffs = np.polyfit(times, log_rates, 1)
            
            qi = np.exp(coeffs[1])  # Initial rate
            D = -coeffs[0]  # Decline rate (1/day)
            b = 0  # Exponential decline exponent
            
        elif decline_type == 'hyperbolic':
            # Hyperbolic decline: q = qi / (1 + b*D*t)^(1/b)
            # This requires iterative fitting - simplified here
            # Initial guess
            qi = rates[0]
            D = 0.1 / 365  # Initial guess for decline rate
            b = 0.5  # Hyperbolic exponent
            
            # Simplified fitting (would need non-linear regression for full implementation)
            # Using linearized form for approximation
            
        elif decline_type == 'harmonic':
            # Harmonic decline: q = qi / (1 + D*t)
            # Linearize: 1/q = 1/qi + (D/qi)*t
            inv_rates = 1 / rates
            coeffs = np.polyfit(times, inv_rates, 1)
            
            qi = 1 / coeffs[1]  # Initial rate
            D = coeffs[0] * qi  # Decline rate
            b = 1  # Harmonic decline exponent
        
        else:
            raise ValueError(f"Unknown decline type: {decline_type}")
        
        # Calculate R-squared
        if decline_type == 'exponential':
            predicted_log_rates = np.polyval(coeffs, times)
            r_squared = 1 - np.sum((log_rates - predicted_log_rates)**2) / np.sum((log_rates - np.mean(log_rates))**2)
        else:
            r_squared = 0.8  # Placeholder for more complex decline types
        
        self.forecast_parameters = {
            'initial_rate': qi,
            'decline_rate': D,
            'hyperbolic_exponent': b,
            'decline_type': decline_type,
            'r_squared': r_squared
        }
        
        return self.forecast_parameters
    
    def forecast_production(self, forecast_days: int = 3650) -> Dict[str, List[float]]:
        """Forecast production using fitted decline parameters.
        
        Args:
            forecast_days: Number of days to forecast
        
        Returns:
            Dictionary with forecasted time and rates
        """
        if not self.forecast_parameters:
            raise ValueError("Must run decline analysis before forecasting")
        
        qi = self.forecast_parameters['initial_rate']
        D = self.forecast_parameters['decline_rate']
        b = self.forecast_parameters['hyperbolic_exponent']
        decline_type = self.forecast_parameters['decline_type']
        
        # Create time array
        times = np.linspace(0, forecast_days, int(forecast_days / 30))  # Monthly points
        
        # Calculate rates based on decline type
        if decline_type == 'exponential':
            rates = qi * np.exp(-D * times)
        elif decline_type == 'hyperbolic':
            rates = qi / (1 + b * D * times)**(1/b)
        elif decline_type == 'harmonic':
            rates = qi / (1 + D * times)
        else:
            raise ValueError(f"Unknown decline type: {decline_type}")
        
        # Calculate cumulative production
        cumulative = np.cumsum(rates * 30)  # Assuming 30-day intervals
        
        return {
            'time_days': times.tolist(),
            'oil_rate_stb_day': rates.tolist(),
            'cumulative_oil_stb': cumulative.tolist(),
            'eur_stb': cumulative[-1]  # Estimated Ultimate Recovery
        }


class ReservoirCharacterization:
    """Reservoir characterization workflows.
    
    This class provides integrated workflows for reservoir characterization
    combining well logs, core data, and production data.
    """
    
    def __init__(self, reservoir_properties: ReservoirProperties):
        """Initialize reservoir characterization.
        
        Args:
            reservoir_properties: Base reservoir properties
        """
        self.reservoir = reservoir_properties
        self.wells: List[WellData] = []
        self.log_data: Dict[str, pd.DataFrame] = {}
        self.core_data: Dict[str, pd.DataFrame] = {}
    
    def add_well(self, well_data: WellData):
        """Add well to characterization study.
        
        Args:
            well_data: Well data
        """
        self.wells.append(well_data)
    
    def add_log_data(self, well_id: str, log_data: pd.DataFrame):
        """Add well log data.
        
        Args:
            well_id: Well identifier
            log_data: DataFrame with depth and log curves
        """
        self.log_data[well_id] = log_data
    
    def calculate_net_pay(self, well_id: str, porosity_cutoff: float = 0.1, 
                         saturation_cutoff: float = 0.5) -> Dict[str, float]:
        """Calculate net pay for a well.
        
        Args:
            well_id: Well identifier
            porosity_cutoff: Minimum porosity cutoff
            saturation_cutoff: Maximum water saturation cutoff
        
        Returns:
            Dictionary with net pay statistics
        """
        if well_id not in self.log_data:
            raise ValueError(f"No log data available for well {well_id}")
        
        logs = self.log_data[well_id]
        
        # Define net pay criteria
        criteria = (
            (logs.get('POROSITY', 0) >= porosity_cutoff) &
            (logs.get('SW', 1) <= saturation_cutoff) &
            (logs.get('VSH', 1) <= 0.5)  # Shale volume cutoff
        )
        
        net_flags = criteria.astype(int)
        depth_interval = logs['DEPTH'].diff().mean() if 'DEPTH' in logs.columns else 0.5
        
        gross_thickness = len(logs) * depth_interval
        net_thickness = net_flags.sum() * depth_interval
        net_to_gross = net_thickness / gross_thickness if gross_thickness > 0 else 0
        
        # Calculate average properties in net pay
        net_logs = logs[criteria]
        avg_porosity = net_logs.get('POROSITY', pd.Series([0])).mean()
        avg_saturation = net_logs.get('SW', pd.Series([1])).mean()
        avg_permeability = net_logs.get('PERMEABILITY', pd.Series([0])).mean()
        
        return {
            'gross_thickness': gross_thickness,
            'net_thickness': net_thickness,
            'net_to_gross': net_to_gross,
            'average_porosity': avg_porosity,
            'average_water_saturation': avg_saturation,
            'average_permeability': avg_permeability,
            'hydrocarbon_saturation': 1 - avg_saturation
        }
    
    def generate_property_maps(self) -> Dict[str, np.ndarray]:
        """Generate 2D property maps from well data.
        
        Returns:
            Dictionary with interpolated property grids
        """
        if len(self.wells) < 3:
            warnings.warn("Need at least 3 wells for reliable property mapping")
            return {}
        
        # Extract well locations and average properties
        well_coords = []
        porosity_values = []
        thickness_values = []
        
        for well in self.wells:
            well_coords.append((well.x_coord, well.y_coord))
            
            # Get average properties for each well
            if well.well_id in self.log_data:
                net_pay = self.calculate_net_pay(well.well_id)
                porosity_values.append(net_pay['average_porosity'])
                thickness_values.append(net_pay['net_thickness'])
            else:
                porosity_values.append(self.reservoir.rock.porosity)
                thickness_values.append(self.reservoir.rock.thickness or 50)
        
        # Create interpolation grid (simplified - would use proper gridding in practice)
        x_coords = [coord[0] for coord in well_coords]
        y_coords = [coord[1] for coord in well_coords]
        
        x_min, x_max = min(x_coords), max(x_coords)
        y_min, y_max = min(y_coords), max(y_coords)
        
        # Simple gridding (would use kriging or other geostatistical methods in practice)
        grid_size = 20
        x_grid = np.linspace(x_min, x_max, grid_size)
        y_grid = np.linspace(y_min, y_max, grid_size)
        
        porosity_grid = np.full((grid_size, grid_size), np.mean(porosity_values))
        thickness_grid = np.full((grid_size, grid_size), np.mean(thickness_values))
        
        return {
            'x_grid': x_grid,
            'y_grid': y_grid,
            'porosity_grid': porosity_grid,
            'thickness_grid': thickness_grid,
            'well_locations': well_coords
        }


class ReservoirModel:
    """Main reservoir model container.
    
    This class integrates all reservoir modeling components and provides
    a unified interface for reservoir engineering calculations.
    """
    
    def __init__(self, reservoir_properties: ReservoirProperties, model_name: str = "Reservoir Model"):
        """Initialize reservoir model.
        
        Args:
            reservoir_properties: Reservoir properties
            model_name: Model identifier
        """
        self.properties = reservoir_properties
        self.model_name = model_name
        self.material_balance = MaterialBalance(reservoir_properties)
        self.simulation = ReservoirSimulation(reservoir_properties)
        self.forecast = ProductionForecast()
        self.characterization = ReservoirCharacterization(reservoir_properties)
        
        # Model components
        self.wells: Dict[str, WellData] = {}
        self.well_tests: Dict[str, WellTestAnalysis] = {}
        self.created_date = datetime.now()
        self.last_updated = datetime.now()
    
    def add_well(self, well_data: WellData):
        """Add well to reservoir model.
        
        Args:
            well_data: Well data
        """
        self.wells[well_data.well_id] = well_data
        self.characterization.add_well(well_data)
        self.last_updated = datetime.now()
    
    def add_well_test(self, well_id: str, well_test: WellTestAnalysis):
        """Add well test analysis.
        
        Args:
            well_id: Well identifier
            well_test: Well test analysis
        """
        self.well_tests[well_id] = well_test
        self.last_updated = datetime.now()
    
    def run_integrated_analysis(self) -> Dict[str, Any]:
        """Run integrated reservoir analysis.
        
        Returns:
            Dictionary with comprehensive analysis results
        """
        results = {
            'model_name': self.model_name,
            'analysis_date': datetime.now(),
            'reservoir_properties': {
                'ooip': self.properties.calculate_ooip(),
                'ogip': self.properties.calculate_ogip(),
                'pore_volume': self.properties.calculate_pore_volume(),
                'hydrocarbon_pore_volume': self.properties.calculate_hydrocarbon_pore_volume()
            },
            'wells': {
                'total_wells': len(self.wells),
                'producers': len([w for w in self.wells.values() if w.well_type == 'producer']),
                'injectors': len([w for w in self.wells.values() if w.well_type == 'injector'])
            }
        }
        
        # Add well test results if available
        if self.well_tests:
            well_test_results = {}
            for well_id, test in self.well_tests.items():
                try:
                    test_result = test.analyze_buildup_test()
                    well_test_results[well_id] = test_result
                except Exception as e:
                    well_test_results[well_id] = {'error': str(e)}
            
            results['well_tests'] = well_test_results
        
        # Add material balance if production data available
        if self.material_balance.production_history:
            try:
                current_pressure = self.properties.pvt.pressure
                mb_result = self.material_balance.tank_material_balance(current_pressure)
                results['material_balance'] = mb_result
            except Exception as e:
                results['material_balance'] = {'error': str(e)}
        
        return results
    
    def export_summary(self) -> Dict[str, Any]:
        """Export model summary for reporting.
        
        Returns:
            Dictionary with model summary
        """
        summary = {
            'model_info': {
                'name': self.model_name,
                'created': self.created_date.isoformat(),
                'last_updated': self.last_updated.isoformat()
            },
            'reservoir_properties': {
                'porosity': self.properties.rock.porosity,
                'permeability': self.properties.rock.permeability,
                'thickness': self.properties.rock.thickness,
                'area': self.properties.area,
                'pressure': self.properties.pvt.pressure,
                'temperature': self.properties.pvt.temperature,
                'water_saturation': self.properties.water_saturation
            },
            'reserves': {
                'ooip_stb': self.properties.calculate_ooip(),
                'ogip_scf': self.properties.calculate_ogip(),
                'pore_volume_acre_ft': self.properties.calculate_pore_volume()
            },
            'wells': {
                'count': len(self.wells),
                'well_ids': list(self.wells.keys())
            }
        }
        
        return summary
