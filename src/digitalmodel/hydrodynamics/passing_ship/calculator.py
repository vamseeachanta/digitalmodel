"""
Passing ship force calculator implementation.

Main calculation engine that integrates mathematical formulations with configuration
system to compute hydrodynamic interaction forces between passing vessels.
"""

import numpy as np
from scipy import integrate
from typing import Dict, List, Optional, Callable, Tuple, Any
from dataclasses import dataclass, field
from pathlib import Path
import time
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor
import hashlib
import json
import logging
from functools import lru_cache

from .configuration import (
    VesselConfig, EnvironmentalConfig, CalculationConfig, 
    PassingShipConfig, YAMLConfigParser
)
from .formulations import (
    calculate_surge_force_infinite as surge_force_infinite,
    calculate_sway_force_infinite as sway_force_infinite, 
    calculate_yaw_moment_infinite as yaw_moment_infinite,
    finite_depth_correction, calculate_forces_with_depth
)

logger = logging.getLogger(__name__)


@dataclass
class ForceResults:
    """Container for force calculation results."""
    surge: float  # N
    sway: float   # N
    yaw: float    # N·m
    
    # Optional metadata
    calculation_time: Optional[float] = None
    convergence_achieved: Optional[bool] = None
    integration_error: Optional[float] = None
    
    def to_dict(self) -> Dict[str, float]:
        """Convert to dictionary format."""
        return {
            'surge': self.surge,
            'sway': self.sway,
            'yaw': self.yaw
        }


class ResultCache:
    """Cache for calculation results with hash-based keys."""
    
    def __init__(self, max_size: int = 1000):
        """Initialize cache with maximum size."""
        self.cache: Dict[str, ForceResults] = {}
        self.max_size = max_size
        self.hits = 0
        self.misses = 0
    
    def _make_key(self, **kwargs) -> str:
        """Create cache key from parameters."""
        # Sort keys for consistent hashing
        sorted_params = sorted(kwargs.items())
        key_str = json.dumps(sorted_params, sort_keys=True, default=str)
        return hashlib.md5(key_str.encode()).hexdigest()
    
    def get(self, **kwargs) -> Optional[ForceResults]:
        """Get cached result if available."""
        key = self._make_key(**kwargs)
        result = self.cache.get(key)
        
        if result:
            self.hits += 1
            logger.debug(f"Cache hit for key {key[:8]}...")
        else:
            self.misses += 1
            logger.debug(f"Cache miss for key {key[:8]}...")
        
        return result
    
    def set(self, result: ForceResults, **kwargs):
        """Store result in cache."""
        if len(self.cache) >= self.max_size:
            # Simple FIFO eviction
            first_key = next(iter(self.cache))
            del self.cache[first_key]
        
        key = self._make_key(**kwargs)
        self.cache[key] = result
        logger.debug(f"Cached result for key {key[:8]}...")
    
    def clear(self):
        """Clear all cached results."""
        self.cache.clear()
        self.hits = 0
        self.misses = 0
        logger.info("Cache cleared")
    
    def stats(self) -> Dict[str, Any]:
        """Get cache statistics."""
        total = self.hits + self.misses
        hit_rate = self.hits / total if total > 0 else 0
        
        return {
            'size': len(self.cache),
            'hits': self.hits,
            'misses': self.misses,
            'hit_rate': hit_rate,
            'max_size': self.max_size
        }


class PassingShipCalculator:
    """Main calculator for passing ship hydrodynamic forces."""
    
    def __init__(
        self,
        moored_vessel: VesselConfig,
        passing_vessel: VesselConfig,
        environment: Optional[EnvironmentalConfig] = None,
        calculation_config: Optional[CalculationConfig] = None
    ):
        """
        Initialize calculator with vessel and environment configurations.
        
        Args:
            moored_vessel: Configuration for the moored/stationary vessel
            passing_vessel: Configuration for the passing vessel
            environment: Environmental conditions (water depth, density)
            calculation_config: Calculation parameters (tolerance, etc.)
        """
        self.moored_vessel = moored_vessel
        self.passing_vessel = passing_vessel
        self.environment = environment or EnvironmentalConfig()
        self.calculation_config = calculation_config or CalculationConfig()
        
        # Initialize cache
        self.cache = ResultCache(max_size=self.calculation_config.cache_size)
        
        # Integration parameters from config
        self.integration_tolerance = self.calculation_config.integration_tolerance
        self.max_iterations = self.calculation_config.max_iterations
        
        logger.info(f"Initialized calculator for {moored_vessel.name or 'moored vessel'} "
                   f"and {passing_vessel.name or 'passing vessel'}")
    
    @classmethod
    def from_config_file(cls, config_path: Path) -> 'PassingShipCalculator':
        """Create calculator from YAML configuration file."""
        parser = YAMLConfigParser()
        config = parser.parse_file(config_path)

        return cls(
            moored_vessel=config.moored_vessel,
            passing_vessel=config.passing_vessel,
            environment=config.environment,
            calculation_config=config.calculation
        )

    @classmethod
    def from_imperial(
        cls,
        moored_length_ft: float,
        moored_beam_ft: float,
        moored_draft_ft: float,
        moored_cb: float,
        passing_length_ft: float,
        passing_beam_ft: float,
        passing_draft_ft: float,
        passing_cb: float,
        water_depth_ft: Optional[float] = None,
        water_density_slug_ft3: float = 1.9905,
        **kwargs,
    ) -> 'PassingShipCalculator':
        """Create calculator from Imperial units with automatic SI conversion.

        Uses TrackedQuantity for provenance-tracked unit conversions.
        All values are converted to SI internally before creating configs.

        Args:
            moored_length_ft: Moored vessel LBP [ft]
            moored_beam_ft: Moored vessel beam [ft]
            moored_draft_ft: Moored vessel draft [ft]
            moored_cb: Moored vessel block coefficient [-]
            passing_length_ft: Passing vessel LBP [ft]
            passing_beam_ft: Passing vessel beam [ft]
            passing_draft_ft: Passing vessel draft [ft]
            passing_cb: Passing vessel block coefficient [-]
            water_depth_ft: Water depth [ft] (None for infinite)
            water_density_slug_ft3: Water density [slug/ft³]
            **kwargs: Passed through to CalculationConfig (SI units expected)
        """
        from assetutilities.units import TrackedQuantity

        def _ft_to_m(value_ft):
            return TrackedQuantity(
                value_ft, 'ft', source='imperial_input'
            ).to('m').magnitude

        moored = VesselConfig(
            length=_ft_to_m(moored_length_ft),
            beam=_ft_to_m(moored_beam_ft),
            draft=_ft_to_m(moored_draft_ft),
            block_coefficient=moored_cb,
        )
        passing = VesselConfig(
            length=_ft_to_m(passing_length_ft),
            beam=_ft_to_m(passing_beam_ft),
            draft=_ft_to_m(passing_draft_ft),
            block_coefficient=passing_cb,
        )

        depth_m = _ft_to_m(water_depth_ft) if water_depth_ft is not None else None
        density_kg_m3 = TrackedQuantity(
            water_density_slug_ft3, 'slug/ft**3', source='imperial_input'
        ).to('kg/m**3').magnitude

        env = EnvironmentalConfig(water_depth=depth_m, water_density=density_kg_m3)
        calc_config = CalculationConfig(**kwargs) if kwargs else None

        return cls(
            moored_vessel=moored,
            passing_vessel=passing,
            environment=env,
            calculation_config=calc_config,
        )

    def calculate_forces(
        self,
        separation: float,
        stagger: float,
        velocity: Optional[float] = None,
        use_cache: bool = True
    ) -> Dict[str, float]:
        """
        Calculate hydrodynamic forces for given scenario.
        
        Args:
            separation: Lateral separation distance [m]
            stagger: Longitudinal stagger distance [m] (positive = passing ahead)
            velocity: Passing vessel velocity [m/s] (overrides config if provided)
            use_cache: Whether to use cached results
        
        Returns:
            Dictionary with 'surge', 'sway', and 'yaw' forces
        """
        # Use provided velocity or fall back to vessel config
        if velocity is None:
            velocity = getattr(self.passing_vessel, 'velocity', 5.0)
        
        # Account for current if present
        if self.environment.current_velocity:
            velocity_relative = velocity - self.environment.current_velocity
        else:
            velocity_relative = velocity
        
        # Check cache
        if use_cache:
            cache_params = {
                'separation': separation,
                'stagger': stagger,
                'velocity': velocity_relative,
                'moored_length': self.moored_vessel.length,
                'passing_length': self.passing_vessel.length,
                'water_depth': self.environment.water_depth
            }
            
            cached = self.cache.get(**cache_params)
            if cached:
                return cached.to_dict()
        
        # Perform calculation
        start_time = time.perf_counter()
        
        try:
            if self.environment.is_infinite_depth:
                # Infinite depth calculation
                forces = self._calculate_infinite_depth(
                    separation, stagger, velocity_relative
                )
            else:
                # Finite depth with corrections
                forces = self._calculate_finite_depth(
                    separation, stagger, velocity_relative
                )
            
            calc_time = time.perf_counter() - start_time
            
            # Create result object
            result = ForceResults(
                surge=forces['surge'],
                sway=forces['sway'],
                yaw=forces['yaw'],
                calculation_time=calc_time,
                convergence_achieved=True
            )
            
            # Cache result
            if use_cache:
                self.cache.set(result, **cache_params)
            
            logger.debug(f"Calculated forces in {calc_time:.3f}s: "
                        f"Fx={result.surge:.1f}N, Fy={result.sway:.1f}N, "
                        f"Mz={result.yaw:.1f}N·m")
            
            return result.to_dict()
            
        except Exception as e:
            logger.error(f"Force calculation failed: {e}")
            raise
    
    def _calculate_infinite_depth(
        self,
        separation: float,
        stagger: float,
        velocity: float
    ) -> Dict[str, float]:
        """Calculate forces for infinite water depth.

        Uses separate vessel parameters (L1/A1, L2/A2) per Wang (1975).
        """
        rho = self.environment.water_density
        L1 = self.moored_vessel.length
        A1 = self.moored_vessel.midship_area
        L2 = self.passing_vessel.length
        A2 = self.passing_vessel.midship_area

        common = dict(
            U=velocity, y=separation, x=stagger, rho=rho,
            L1=L1, A1=A1, L2=L2, A2=A2,
        )

        surge = surge_force_infinite(**common)
        sway = sway_force_infinite(**common)
        yaw = yaw_moment_infinite(**common)

        return {'surge': surge, 'sway': sway, 'yaw': yaw}
    
    def _calculate_finite_depth(
        self,
        separation: float,
        stagger: float,
        velocity: float
    ) -> Dict[str, float]:
        """Calculate forces with finite water depth corrections."""
        # Get infinite depth forces first
        forces_inf = self._calculate_infinite_depth(separation, stagger, velocity)
        
        # Apply finite depth corrections
        h = self.environment.water_depth
        T_avg = (self.moored_vessel.draft + self.passing_vessel.draft) / 2
        
        # Calculate correction factors for each force component
        surge_corr = finite_depth_correction(
            h=h, T=T_avg, L=self.moored_vessel.length,
            y=separation, mode='surge'
        )
        
        sway_corr = finite_depth_correction(
            h=h, T=T_avg, L=self.moored_vessel.length,
            y=separation, mode='sway'
        )
        
        yaw_corr = finite_depth_correction(
            h=h, T=T_avg, L=self.moored_vessel.length,
            y=separation, mode='yaw'
        )
        
        # Apply corrections
        return {
            'surge': forces_inf['surge'] * surge_corr,
            'sway': forces_inf['sway'] * sway_corr,
            'yaw': forces_inf['yaw'] * yaw_corr
        }
    
    def calculate_batch(
        self,
        scenarios: List[Dict[str, float]],
        parallel: bool = True,
        n_workers: Optional[int] = None,
        progress_callback: Optional[Callable[[int, int, str], None]] = None
    ) -> List[Dict[str, float]]:
        """
        Calculate forces for multiple scenarios.
        
        Args:
            scenarios: List of scenario dictionaries with 'separation', 
                      'stagger', and optionally 'velocity'
            parallel: Whether to use parallel processing
            n_workers: Number of parallel workers (None for auto)
            progress_callback: Function called with (current, total, message)
        
        Returns:
            List of force result dictionaries
        """
        n_scenarios = len(scenarios)
        logger.info(f"Starting batch calculation for {n_scenarios} scenarios")
        
        if progress_callback:
            progress_callback(0, n_scenarios, "Starting batch calculation...")
        
        if parallel and n_scenarios > 1:
            # Use thread pool for I/O bound cache lookups
            with ThreadPoolExecutor(max_workers=n_workers) as executor:
                futures = []
                for scenario in scenarios:
                    future = executor.submit(
                        self.calculate_forces,
                        **scenario
                    )
                    futures.append(future)
                
                results = []
                for i, future in enumerate(futures):
                    result = future.result()
                    results.append(result)
                    
                    if progress_callback:
                        progress_callback(
                            i + 1, n_scenarios,
                            f"Completed {i + 1}/{n_scenarios} calculations"
                        )
        else:
            # Serial processing
            results = []
            for i, scenario in enumerate(scenarios):
                result = self.calculate_forces(**scenario)
                results.append(result)
                
                if progress_callback:
                    progress_callback(
                        i + 1, n_scenarios,
                        f"Completed {i + 1}/{n_scenarios} calculations"
                    )
        
        logger.info(f"Batch calculation complete. Cache stats: {self.cache.stats()}")
        return results
    
    def parametric_study(
        self,
        parameter: str,
        values: np.ndarray,
        base_scenario: Dict[str, float]
    ) -> Tuple[np.ndarray, Dict[str, np.ndarray]]:
        """
        Perform parametric study varying one parameter.
        
        Args:
            parameter: Parameter to vary ('separation', 'stagger', 'velocity')
            values: Array of parameter values
            base_scenario: Base scenario dictionary
        
        Returns:
            Tuple of (parameter_values, force_results_dict)
        """
        scenarios = []
        for value in values:
            scenario = base_scenario.copy()
            scenario[parameter] = value
            scenarios.append(scenario)
        
        results = self.calculate_batch(scenarios)
        
        # Organize results by force component
        forces = {
            'surge': np.array([r['surge'] for r in results]),
            'sway': np.array([r['sway'] for r in results]),
            'yaw': np.array([r['yaw'] for r in results])
        }
        
        return values, forces
    
    def clear_cache(self):
        """Clear the result cache."""
        self.cache.clear()
    
    def get_cache_stats(self) -> Dict[str, Any]:
        """Get cache statistics."""
        return self.cache.stats()