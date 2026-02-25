"""RAO Data Interpolation Module.

This module provides interpolation functionality for RAO data including:
- 2D interpolation for frequency and heading grids
- Phase-aware interpolation handling wraparound
- Quality metrics for interpolation accuracy
"""

from typing import Optional, Tuple
import numpy as np
from scipy.interpolate import RectBivariateSpline, griddata, RegularGridInterpolator
from scipy.interpolate import UnivariateSpline
import warnings


class RAOInterpolator:
    """Interpolate RAO data to target frequency and heading grids."""
    
    def __init__(self, config: Optional[dict] = None):
        """Initialize interpolator with configuration."""
        self.config = config or {}
        
        # Default interpolation settings
        self.settings = {
            'method': 'cubic_spline',    # linear, cubic_spline, pchip
            'extrapolation': 'constant',  # constant, linear, zero
            'quality_threshold': 0.95,    # R² for interpolation quality
            'phase_unwrap': True         # Unwrap phase before interpolation
        }
        
        # Update with user config
        if 'interpolation' in self.config:
            self.settings.update(self.config['interpolation'])
    
    def interpolate_2d(self, rao_data, target_frequencies: np.ndarray, 
                      target_headings: np.ndarray):
        """Interpolate RAO data to target frequency and heading grids.
        
        Args:
            rao_data: Source RAO data
            target_frequencies: Target frequency array (rad/s)
            target_headings: Target heading array (degrees)
            
        Returns:
            Interpolated RAOData on target grid
        """
        # Create copy of RAO data to avoid modifying original
        from copy import deepcopy
        interpolated_data = deepcopy(rao_data)
        
        # Update frequencies and headings
        interpolated_data.frequencies = target_frequencies
        interpolated_data.headings = target_headings
        
        # Prepare heading data for circular interpolation
        source_headings = rao_data.headings
        extended_headings, extended_indices = self._prepare_circular_headings(source_headings)
        
        # Interpolate each DOF
        for dof in rao_data.raos:
            # Get source data
            source_amplitude = rao_data.raos[dof]['amplitude']
            source_phase = rao_data.raos[dof]['phase']
            
            # Extend data for circular interpolation
            extended_amplitude = source_amplitude[:, extended_indices]
            extended_phase = source_phase[:, extended_indices]
            
            # Interpolate amplitude
            interp_amplitude = self._interpolate_amplitude(
                rao_data.frequencies, extended_headings, extended_amplitude,
                target_frequencies, target_headings
            )
            
            # Interpolate phase
            interp_phase = self._interpolate_phase(
                rao_data.frequencies, extended_headings, extended_phase,
                target_frequencies, target_headings
            )
            
            # Store interpolated data
            interpolated_data.raos[dof]['amplitude'] = interp_amplitude
            interpolated_data.raos[dof]['phase'] = interp_phase
        
        # Add interpolation metadata
        interpolated_data.metadata['interpolation'] = {
            'method': self.settings['method'],
            'source_freq_range': [rao_data.frequencies.min(), rao_data.frequencies.max()],
            'source_heading_range': [rao_data.headings.min(), rao_data.headings.max()],
            'target_freq_range': [target_frequencies.min(), target_frequencies.max()],
            'target_heading_range': [target_headings.min(), target_headings.max()]
        }
        
        return interpolated_data
    
    def _prepare_circular_headings(self, headings: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """Prepare headings for circular interpolation by extending the data."""
        # Add wrapped data points for smooth circular interpolation
        # E.g., add 360° as copy of 0°, and -15° as copy of 345°
        
        extended_headings = []
        extended_indices = []
        
        # Add points before 0°
        if headings[0] == 0:
            # Add negative headings using data from high headings
            for i in range(len(headings)-1, -1, -1):
                if headings[i] > 180:
                    extended_headings.append(headings[i] - 360)
                    extended_indices.append(i)
                else:
                    break
        
        # Add original headings
        extended_headings.extend(headings.tolist())
        extended_indices.extend(range(len(headings)))
        
        # Add points after 360°
        if headings[-1] < 360:
            # Add headings > 360 using data from low headings
            for i in range(len(headings)):
                if headings[i] < 180:
                    extended_headings.append(headings[i] + 360)
                    extended_indices.append(i)
                else:
                    break
        
        return np.array(extended_headings), np.array(extended_indices)
    
    def _interpolate_amplitude(self, source_freq: np.ndarray, source_head: np.ndarray,
                              source_data: np.ndarray, target_freq: np.ndarray,
                              target_head: np.ndarray) -> np.ndarray:
        """Interpolate amplitude data."""
        method = self.settings['method']
        
        if method == 'linear':
            # Use RegularGridInterpolator for linear interpolation
            # Note: RegularGridInterpolator expects (freq, head) order and strictly monotonic data
            try:
                f = RegularGridInterpolator(
                    (source_freq, source_head), source_data,
                    method='linear', bounds_error=False, fill_value=0.0
                )
                # Create mesh grid for target points
                target_freq_mesh, target_head_mesh = np.meshgrid(target_freq, target_head, indexing='ij')
                points = np.column_stack([target_freq_mesh.ravel(), target_head_mesh.ravel()])
                result = f(points).reshape(len(target_freq), len(target_head))
                return result
            except ValueError:
                # Fall back to griddata for non-regular grids
                freq_mesh, head_mesh = np.meshgrid(source_freq, source_head, indexing='ij')
                points = np.column_stack([freq_mesh.ravel(), head_mesh.ravel()])
                values = source_data.ravel()
                
                target_freq_mesh, target_head_mesh = np.meshgrid(target_freq, target_head, indexing='ij')
                target_points = np.column_stack([target_freq_mesh.ravel(), target_head_mesh.ravel()])
                
                result = griddata(points, values, target_points, method='linear', fill_value=0.0)
                return result.reshape(len(target_freq), len(target_head))
        
        elif method == 'cubic_spline':
            # Use cubic spline interpolation
            try:
                # RectBivariateSpline requires regular grid
                f = RectBivariateSpline(source_freq, source_head, source_data,
                                      kx=min(3, len(source_freq)-1),
                                      ky=min(3, len(source_head)-1))
                return f(target_freq, target_head)
            except Exception:
                # Fall back to linear if cubic fails
                warnings.warn("Cubic spline failed, falling back to linear interpolation")
                try:
                    f = RegularGridInterpolator(
                        (source_freq, source_head), source_data,
                        method='linear', bounds_error=False, fill_value=0.0
                    )
                    target_freq_mesh, target_head_mesh = np.meshgrid(target_freq, target_head, indexing='ij')
                    points = np.column_stack([target_freq_mesh.ravel(), target_head_mesh.ravel()])
                    result = f(points).reshape(len(target_freq), len(target_head))
                    return result
                except ValueError:
                    # Use griddata for irregular grids
                    freq_mesh, head_mesh = np.meshgrid(source_freq, source_head, indexing='ij')
                    points = np.column_stack([freq_mesh.ravel(), head_mesh.ravel()])
                    values = source_data.ravel()
                    
                    target_freq_mesh, target_head_mesh = np.meshgrid(target_freq, target_head, indexing='ij')
                    target_points = np.column_stack([target_freq_mesh.ravel(), target_head_mesh.ravel()])
                    
                    result = griddata(points, values, target_points, method='linear', fill_value=0.0)
                    return result.reshape(len(target_freq), len(target_head))
        
        else:
            # Default to linear
            try:
                f = RegularGridInterpolator(
                    (source_freq, source_head), source_data,
                    method='linear', bounds_error=False, fill_value=0.0
                )
                target_freq_mesh, target_head_mesh = np.meshgrid(target_freq, target_head, indexing='ij')
                points = np.column_stack([target_freq_mesh.ravel(), target_head_mesh.ravel()])
                result = f(points).reshape(len(target_freq), len(target_head))
                return result
            except ValueError:
                # Use griddata for irregular grids
                freq_mesh, head_mesh = np.meshgrid(source_freq, source_head, indexing='ij')
                points = np.column_stack([freq_mesh.ravel(), head_mesh.ravel()])
                values = source_data.ravel()
                
                target_freq_mesh, target_head_mesh = np.meshgrid(target_freq, target_head, indexing='ij')
                target_points = np.column_stack([target_freq_mesh.ravel(), target_head_mesh.ravel()])
                
                result = griddata(points, values, target_points, method='linear', fill_value=0.0)
                return result.reshape(len(target_freq), len(target_head))
    
    def _interpolate_phase(self, source_freq: np.ndarray, source_head: np.ndarray,
                          source_data: np.ndarray, target_freq: np.ndarray,
                          target_head: np.ndarray) -> np.ndarray:
        """Interpolate phase data with proper handling of wraparound."""
        if self.settings['phase_unwrap']:
            # Convert to complex representation for better interpolation
            source_complex = np.exp(1j * np.deg2rad(source_data))
            
            # Interpolate real and imaginary parts separately
            real_part = self._interpolate_amplitude(
                source_freq, source_head, np.real(source_complex),
                target_freq, target_head
            )
            imag_part = self._interpolate_amplitude(
                source_freq, source_head, np.imag(source_complex),
                target_freq, target_head
            )
            
            # Convert back to phase
            interp_phase = np.rad2deg(np.angle(real_part + 1j * imag_part))
            
        else:
            # Direct interpolation of phase values
            interp_phase = self._interpolate_amplitude(
                source_freq, source_head, source_data,
                target_freq, target_head
            )
        
        # Ensure phase is in [-180, 180] or [0, 360] range
        interp_phase = self._normalize_phase(interp_phase)
        
        return interp_phase
    
    def _normalize_phase(self, phase: np.ndarray) -> np.ndarray:
        """Normalize phase to [-180, 180] degrees."""
        # Wrap to [-180, 180]
        phase = np.mod(phase + 180, 360) - 180
        return phase
    
    def interpolate_1d_frequency(self, rao_data, target_frequencies: np.ndarray,
                               heading_index: int = 0):
        """Interpolate RAO data along frequency axis only for a specific heading.
        
        Useful for quick frequency interpolation at a single heading.
        """
        interpolated_raos = {}
        
        for dof in rao_data.raos:
            # Extract data for specific heading
            amplitude_slice = rao_data.raos[dof]['amplitude'][:, heading_index]
            phase_slice = rao_data.raos[dof]['phase'][:, heading_index]
            
            # Interpolate amplitude
            amp_interp = self._interpolate_1d(
                rao_data.frequencies, amplitude_slice, target_frequencies
            )
            
            # Interpolate phase
            if self.settings['phase_unwrap']:
                # Use complex representation
                complex_data = amplitude_slice * np.exp(1j * np.deg2rad(phase_slice))
                complex_interp = self._interpolate_1d_complex(
                    rao_data.frequencies, complex_data, target_frequencies
                )
                amp_interp = np.abs(complex_interp)
                phase_interp = np.rad2deg(np.angle(complex_interp))
            else:
                phase_interp = self._interpolate_1d(
                    rao_data.frequencies, phase_slice, target_frequencies
                )
            
            interpolated_raos[dof] = {
                'amplitude': amp_interp,
                'phase': self._normalize_phase(phase_interp)
            }
        
        return interpolated_raos
    
    def _interpolate_1d(self, x: np.ndarray, y: np.ndarray, 
                       x_new: np.ndarray) -> np.ndarray:
        """1D interpolation with extrapolation handling."""
        # Handle extrapolation
        y_new = np.zeros_like(x_new)
        
        # Interpolation region
        mask = (x_new >= x.min()) & (x_new <= x.max())
        
        if np.any(mask):
            if self.settings['method'] == 'linear':
                y_new[mask] = np.interp(x_new[mask], x, y)
            else:
                # Use spline interpolation
                try:
                    spline = UnivariateSpline(x, y, k=min(3, len(x)-1), s=0)
                    y_new[mask] = spline(x_new[mask])
                except Exception:
                    # Fall back to linear
                    y_new[mask] = np.interp(x_new[mask], x, y)
        
        # Handle extrapolation
        if self.settings['extrapolation'] == 'constant':
            # Use nearest values
            y_new[x_new < x.min()] = y[0]
            y_new[x_new > x.max()] = y[-1]
        elif self.settings['extrapolation'] == 'zero':
            # Already zero from initialization
            pass
        elif self.settings['extrapolation'] == 'linear':
            # Linear extrapolation (use with caution)
            if np.any(x_new < x.min()):
                slope_low = (y[1] - y[0]) / (x[1] - x[0])
                y_new[x_new < x.min()] = y[0] + slope_low * (x_new[x_new < x.min()] - x[0])
            if np.any(x_new > x.max()):
                slope_high = (y[-1] - y[-2]) / (x[-1] - x[-2])
                y_new[x_new > x.max()] = y[-1] + slope_high * (x_new[x_new > x.max()] - x[-1])
        
        return y_new
    
    def _interpolate_1d_complex(self, x: np.ndarray, y_complex: np.ndarray,
                               x_new: np.ndarray) -> np.ndarray:
        """1D interpolation of complex data."""
        # Interpolate real and imaginary parts separately
        real_interp = self._interpolate_1d(x, np.real(y_complex), x_new)
        imag_interp = self._interpolate_1d(x, np.imag(y_complex), x_new)
        
        return real_interp + 1j * imag_interp
    
    def compute_interpolation_quality(self, original_data: np.ndarray,
                                    interpolated_data: np.ndarray,
                                    test_indices: Optional[np.ndarray] = None) -> float:
        """Compute R² quality metric for interpolation accuracy.
        
        Args:
            original_data: Original data values
            interpolated_data: Interpolated values at same locations
            test_indices: Optional indices to use for testing (if doing cross-validation)
            
        Returns:
            R² value (1.0 = perfect, 0.0 = poor)
        """
        if test_indices is not None:
            original = original_data.flat[test_indices]
            interpolated = interpolated_data.flat[test_indices]
        else:
            original = original_data.flatten()
            interpolated = interpolated_data.flatten()
        
        # Remove any NaN values
        mask = ~(np.isnan(original) | np.isnan(interpolated))
        original = original[mask]
        interpolated = interpolated[mask]
        
        if len(original) == 0:
            return 0.0
        
        # Compute R²
        ss_res = np.sum((original - interpolated) ** 2)
        ss_tot = np.sum((original - np.mean(original)) ** 2)
        
        if ss_tot == 0:
            return 1.0 if ss_res == 0 else 0.0
        
        r2 = 1 - (ss_res / ss_tot)
        return max(0.0, min(1.0, r2))  # Clamp to [0, 1]