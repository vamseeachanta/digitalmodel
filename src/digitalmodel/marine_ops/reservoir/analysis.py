"""Reservoir Analysis Module.

This module provides comprehensive reservoir analysis capabilities including
log analysis, stratigraphic correlation, well correlation, and performance analysis.

Classes:
    ReservoirAnalysis: Main reservoir analysis container
    StratigraphicAnalysis: Stratigraphic correlation and visualization
    LogAnalysis: Well log interpretation and analysis
    CoreAnalysis: Core data analysis and correlation
    WellCorrelation: Multi-well correlation and mapping
    ReservoirMapping: Reservoir property mapping
    PerformanceAnalysis: Production performance analysis
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from typing import Optional, Union, Dict, Any, List, Tuple, Callable
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
import warnings
from datetime import datetime
from scipy import interpolate, stats
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler

from .properties import ReservoirProperties
from .modeling import WellData, ProductionData


@dataclass
class LogCurve:
    """Well log curve data container.
    
    Attributes:
        name: Curve name (e.g., 'GR', 'RHOB', 'NPHI')
        depth: Depth values (ft)
        values: Curve values in appropriate units
        units: Measurement units
        null_value: Null/missing value indicator
        description: Curve description
    """
    name: str
    depth: np.ndarray
    values: np.ndarray
    units: str
    null_value: float = -999.25
    description: str = ""
    
    def __post_init__(self):
        """Validate log curve data."""
        if len(self.depth) != len(self.values):
            raise ValueError("Depth and values arrays must have same length")
        
        # Mask null values
        self.valid_mask = self.values != self.null_value
    
    def get_valid_data(self) -> Tuple[np.ndarray, np.ndarray]:
        """Get valid (non-null) depth and values.
        
        Returns:
            Tuple of valid depth and values arrays
        """
        return self.depth[self.valid_mask], self.values[self.valid_mask]
    
    def interpolate_to_depth(self, target_depth: np.ndarray) -> np.ndarray:
        """Interpolate curve to target depth array.
        
        Args:
            target_depth: Target depth array
        
        Returns:
            Interpolated values
        """
        valid_depth, valid_values = self.get_valid_data()
        
        if len(valid_depth) < 2:
            return np.full_like(target_depth, self.null_value)
        
        # Linear interpolation
        interp_func = interpolate.interp1d(
            valid_depth, valid_values, 
            kind='linear', 
            bounds_error=False, 
            fill_value=self.null_value
        )
        
        return interp_func(target_depth)


@dataclass
class StratigraphicLayer:
    """Stratigraphic layer definition.
    
    Attributes:
        name: Layer name
        top_depth: Top depth (ft)
        base_depth: Base depth (ft)
        lithology: Lithology description
        depositional_environment: Depositional environment
        sequence_order: Stratigraphic sequence order (oldest = 1)
        marker_type: Type of stratigraphic marker
    """
    name: str
    top_depth: float
    base_depth: float
    lithology: str = "unknown"
    depositional_environment: str = "unknown"
    sequence_order: int = 1
    marker_type: str = "formation"
    
    @property
    def thickness(self) -> float:
        """Calculate layer thickness."""
        return abs(self.base_depth - self.top_depth)
    
    def contains_depth(self, depth: float) -> bool:
        """Check if depth is within layer."""
        return min(self.top_depth, self.base_depth) <= depth <= max(self.top_depth, self.base_depth)


class LogAnalysis:
    """Well log interpretation and analysis.
    
    This class provides methods for analyzing well log data including
    facies analysis, petrophysical calculations, and log quality control.
    """
    
    def __init__(self, well_id: str):
        """Initialize log analysis.
        
        Args:
            well_id: Well identifier
        """
        self.well_id = well_id
        self.curves: Dict[str, LogCurve] = {}
        self.formations: List[StratigraphicLayer] = []
        self.facies_model = None
        self.petrophysical_results = {}
    
    def add_curve(self, curve: LogCurve):
        """Add log curve to analysis.
        
        Args:
            curve: Log curve data
        """
        self.curves[curve.name] = curve
    
    def add_formation(self, formation: StratigraphicLayer):
        """Add stratigraphic formation.
        
        Args:
            formation: Formation definition
        """
        self.formations.append(formation)
        self.formations.sort(key=lambda x: x.top_depth)
    
    def calculate_facies_from_logs(self, n_clusters: int = 5, log_names: List[str] = None) -> np.ndarray:
        """Calculate facies using K-means clustering of log data.
        
        Args:
            n_clusters: Number of facies clusters
            log_names: List of log curve names to use
        
        Returns:
            Array of facies classifications
        """
        if log_names is None:
            log_names = ['GR', 'RHOB', 'NPHI', 'RT']
        
        # Check available curves
        available_curves = [name for name in log_names if name in self.curves]
        
        if len(available_curves) < 2:
            raise ValueError("Need at least 2 log curves for facies analysis")
        
        # Get common depth range
        depth_ranges = []
        for name in available_curves:
            valid_depth, _ = self.curves[name].get_valid_data()
            if len(valid_depth) > 0:
                depth_ranges.append((valid_depth.min(), valid_depth.max()))
        
        if not depth_ranges:
            raise ValueError("No valid log data found")
        
        # Find common depth range
        min_depth = max([r[0] for r in depth_ranges])
        max_depth = min([r[1] for r in depth_ranges])
        
        # Create common depth array
        depth_step = 0.5  # 0.5 ft sampling
        common_depth = np.arange(min_depth, max_depth + depth_step, depth_step)
        
        # Interpolate all curves to common depth
        log_matrix = []
        for name in available_curves:
            interpolated_values = self.curves[name].interpolate_to_depth(common_depth)
            log_matrix.append(interpolated_values)
        
        log_matrix = np.array(log_matrix).T
        
        # Remove null values
        null_mask = np.any(log_matrix == -999.25, axis=1)
        valid_log_matrix = log_matrix[~null_mask]
        valid_depths = common_depth[~null_mask]
        
        if len(valid_log_matrix) < n_clusters:
            raise ValueError(f"Insufficient valid data points ({len(valid_log_matrix)}) for {n_clusters} clusters")
        
        # Standardize data
        scaler = StandardScaler()
        scaled_logs = scaler.fit_transform(valid_log_matrix)
        
        # K-means clustering
        kmeans = KMeans(n_clusters=n_clusters, random_state=42, n_init=10)
        facies = kmeans.fit_predict(scaled_logs)
        
        # Store results
        self.facies_model = {
            'kmeans': kmeans,
            'scaler': scaler,
            'depths': valid_depths,
            'facies': facies,
            'log_names': available_curves,
            'cluster_centers': kmeans.cluster_centers_
        }
        
        return facies
    
    def calculate_porosity_from_logs(self, method: str = "neutron_density") -> np.ndarray:
        """Calculate porosity from log data.
        
        Args:
            method: Calculation method
        
        Returns:
            Porosity array
        """
        if method == "neutron_density" and 'NPHI' in self.curves and 'RHOB' in self.curves:
            # Get common depths
            nphi_depth, nphi_values = self.curves['NPHI'].get_valid_data()
            rhob_depth, rhob_values = self.curves['RHOB'].get_valid_data()
            
            # Find overlapping depth range
            min_depth = max(nphi_depth.min(), rhob_depth.min())
            max_depth = min(nphi_depth.max(), rhob_depth.max())
            
            depth_array = np.arange(min_depth, max_depth + 0.5, 0.5)
            
            # Interpolate both curves
            nphi_interp = self.curves['NPHI'].interpolate_to_depth(depth_array)
            rhob_interp = self.curves['RHOB'].interpolate_to_depth(depth_array)
            
            # Calculate density porosity
            matrix_density = 2.65  # Quartz matrix
            fluid_density = 1.0   # Water
            
            density_porosity = (matrix_density - rhob_interp) / (matrix_density - fluid_density)
            
            # Combine neutron and density porosity
            # Account for gas effect and clay content
            porosity = np.sqrt((nphi_interp**2 + density_porosity**2) / 2)
            
            # Apply limits
            porosity = np.clip(porosity, 0, 0.5)
            
            self.petrophysical_results['porosity'] = {
                'depth': depth_array,
                'values': porosity,
                'method': method
            }
            
            return porosity
        
        else:
            raise ValueError(f"Method {method} not available or required curves missing")
    
    def calculate_water_saturation(self, rw: float = 0.1, method: str = "archie") -> np.ndarray:
        """Calculate water saturation from logs.
        
        Args:
            rw: Formation water resistivity (ohm-m)
            method: Calculation method
        
        Returns:
            Water saturation array
        """
        if method == "archie" and 'RT' in self.curves:
            # Need porosity for Archie calculation
            if 'porosity' not in self.petrophysical_results:
                self.calculate_porosity_from_logs()
            
            if 'porosity' not in self.petrophysical_results:
                raise ValueError("Porosity calculation failed - needed for water saturation")
            
            # Get resistivity data
            rt_depth, rt_values = self.curves['RT'].get_valid_data()
            
            # Get porosity on same depth scale
            porosity_depth = self.petrophysical_results['porosity']['depth']
            porosity_values = self.petrophysical_results['porosity']['values']
            
            # Interpolate to common depth
            min_depth = max(rt_depth.min(), porosity_depth.min())
            max_depth = min(rt_depth.max(), porosity_depth.max())
            depth_array = np.arange(min_depth, max_depth + 0.5, 0.5)
            
            rt_interp = self.curves['RT'].interpolate_to_depth(depth_array)
            porosity_interp = interpolate.interp1d(
                porosity_depth, porosity_values, 
                bounds_error=False, fill_value=-999.25
            )(depth_array)
            
            # Archie's equation: Sw = ((a * Rw) / (phi^m * Rt))^(1/n)
            a = 1.0  # Tortuosity factor
            m = 2.0  # Cementation exponent
            n = 2.0  # Saturation exponent
            
            # Calculate water saturation
            valid_mask = (rt_interp > 0) & (porosity_interp > 0) & (rt_interp != -999.25) & (porosity_interp != -999.25)
            
            sw = np.full_like(depth_array, -999.25)
            sw[valid_mask] = ((a * rw) / (porosity_interp[valid_mask]**m * rt_interp[valid_mask]))**(1/n)
            
            # Apply limits
            sw[valid_mask] = np.clip(sw[valid_mask], 0, 1)
            
            self.petrophysical_results['water_saturation'] = {
                'depth': depth_array,
                'values': sw,
                'method': method,
                'rw': rw
            }
            
            return sw
        
        else:
            raise ValueError(f"Method {method} not available or required curves missing")
    
    def calculate_net_pay(self, porosity_cutoff: float = 0.1, sw_cutoff: float = 0.5) -> Dict[str, float]:
        """Calculate net pay based on cutoffs.
        
        Args:
            porosity_cutoff: Minimum porosity
            sw_cutoff: Maximum water saturation
        
        Returns:
            Dictionary with net pay statistics
        """
        # Ensure calculations are done
        if 'porosity' not in self.petrophysical_results:
            self.calculate_porosity_from_logs()
        
        if 'water_saturation' not in self.petrophysical_results:
            self.calculate_water_saturation()
        
        porosity_data = self.petrophysical_results['porosity']
        sw_data = self.petrophysical_results['water_saturation']
        
        # Find common depth range
        min_depth = max(porosity_data['depth'].min(), sw_data['depth'].min())
        max_depth = min(porosity_data['depth'].max(), sw_data['depth'].max())
        
        depth_interval = 0.5  # ft
        
        # Apply cutoffs
        porosity_mask = porosity_data['values'] >= porosity_cutoff
        sw_mask = sw_data['values'] <= sw_cutoff
        valid_mask = (porosity_data['values'] != -999.25) & (sw_data['values'] != -999.25)
        
        net_pay_mask = porosity_mask & sw_mask & valid_mask
        
        gross_thickness = len(porosity_data['depth']) * depth_interval
        net_thickness = np.sum(net_pay_mask) * depth_interval
        net_to_gross = net_thickness / gross_thickness if gross_thickness > 0 else 0
        
        # Average properties in net pay
        if np.any(net_pay_mask):
            avg_porosity = np.mean(porosity_data['values'][net_pay_mask])
            avg_sw = np.mean(sw_data['values'][net_pay_mask])
        else:
            avg_porosity = 0
            avg_sw = 1
        
        return {
            'gross_thickness': gross_thickness,
            'net_thickness': net_thickness,
            'net_to_gross': net_to_gross,
            'average_porosity': avg_porosity,
            'average_water_saturation': avg_sw,
            'hydrocarbon_saturation': 1 - avg_sw,
            'porosity_cutoff': porosity_cutoff,
            'sw_cutoff': sw_cutoff
        }


class StratigraphicAnalysis:
    """Stratigraphic correlation and visualization.
    
    Enhanced version of the existing stratigraphic plotting functionality
    with improved organization and additional analysis capabilities.
    """
    
    def __init__(self):
        """Initialize stratigraphic analysis."""
        self.wells: Dict[str, WellData] = {}
        self.log_analyses: Dict[str, LogAnalysis] = {}
        self.correlation_markers: Dict[str, List[StratigraphicLayer]] = {}
        self.well_order: List[str] = []
    
    def add_well_analysis(self, well_id: str, log_analysis: LogAnalysis):
        """Add well log analysis.
        
        Args:
            well_id: Well identifier
            log_analysis: Log analysis object
        """
        self.log_analyses[well_id] = log_analysis
        if well_id not in self.well_order:
            self.well_order.append(well_id)
    
    def set_well_order(self, well_order: List[str]):
        """Set the order of wells for cross-section display.
        
        Args:
            well_order: List of well IDs in desired order
        """
        # Validate that all wells exist
        for well_id in well_order:
            if well_id not in self.log_analyses:
                raise ValueError(f"Well {well_id} not found in analyses")
        
        self.well_order = well_order
    
    def add_correlation_marker(self, well_id: str, marker: StratigraphicLayer):
        """Add stratigraphic correlation marker.
        
        Args:
            well_id: Well identifier
            marker: Stratigraphic marker
        """
        if well_id not in self.correlation_markers:
            self.correlation_markers[well_id] = []
        
        self.correlation_markers[well_id].append(marker)
        self.correlation_markers[well_id].sort(key=lambda x: x.top_depth)
    
    def create_flattened_section(self, datum_marker: str = None) -> Dict[str, Any]:
        """Create flattened stratigraphic section.
        
        Args:
            datum_marker: Marker name to use as flattening datum
        
        Returns:
            Dictionary with flattened section data
        """
        if not datum_marker:
            # Use shallowest common marker as datum
            common_markers = self._find_common_markers()
            if not common_markers:
                warnings.warn("No common markers found - using top of data as datum")
                datum_marker = None
            else:
                datum_marker = common_markers[0]
        
        flattened_data = {}
        datum_depths = {}
        
        # Find datum depth in each well
        for well_id in self.well_order:
            if datum_marker and well_id in self.correlation_markers:
                # Find datum marker
                markers = self.correlation_markers[well_id]
                datum_depth = None
                for marker in markers:
                    if marker.name == datum_marker:
                        datum_depth = marker.top_depth
                        break
                
                if datum_depth is None:
                    # Use average depth of all wells' datum
                    all_datum_depths = []
                    for wid, wmarkers in self.correlation_markers.items():
                        for m in wmarkers:
                            if m.name == datum_marker:
                                all_datum_depths.append(m.top_depth)
                    
                    datum_depth = np.mean(all_datum_depths) if all_datum_depths else 0
            else:
                # Use minimum depth in well
                log_analysis = self.log_analyses[well_id]
                min_depths = []
                for curve in log_analysis.curves.values():
                    valid_depth, _ = curve.get_valid_data()
                    if len(valid_depth) > 0:
                        min_depths.append(valid_depth.min())
                
                datum_depth = min(min_depths) if min_depths else 0
            
            datum_depths[well_id] = datum_depth
        
        # Calculate flattened depths for each well
        for well_id in self.well_order:
            log_analysis = self.log_analyses[well_id]
            flattened_curves = {}
            
            for curve_name, curve in log_analysis.curves.items():
                # Adjust depths relative to datum
                flattened_depth = curve.depth - datum_depths[well_id]
                flattened_curves[curve_name] = {
                    'depth': flattened_depth,
                    'values': curve.values,
                    'original_depth': curve.depth
                }
            
            flattened_data[well_id] = {
                'curves': flattened_curves,
                'datum_depth': datum_depths[well_id],
                'datum_marker': datum_marker
            }
        
        return flattened_data
    
    def _find_common_markers(self) -> List[str]:
        """Find markers common to all wells."""
        if not self.correlation_markers:
            return []
        
        # Get marker names from first well
        well_ids = list(self.correlation_markers.keys())
        if not well_ids:
            return []
        
        first_well_markers = {m.name for m in self.correlation_markers[well_ids[0]]}
        
        # Find intersection with other wells
        common_markers = first_well_markers
        for well_id in well_ids[1:]:
            well_markers = {m.name for m in self.correlation_markers[well_id]}
            common_markers = common_markers.intersection(well_markers)
        
        return sorted(list(common_markers))
    
    def plot_stratigraphic_section(
        self, 
        figsize: Tuple[int, int] = (18, 10),
        track_names: List[str] = None,
        show_facies: bool = True,
        flatten: bool = False,
        datum_marker: str = None
    ) -> plt.Figure:
        """Plot stratigraphic cross-section.
        
        Args:
            figsize: Figure size
            track_names: List of log curve names to display
            show_facies: Whether to show facies track
            flatten: Whether to flatten section to datum
            datum_marker: Marker to use for flattening
        
        Returns:
            Matplotlib figure
        """
        if not self.well_order:
            raise ValueError("No wells defined for plotting")
        
        if track_names is None:
            track_names = ['GR', 'RT', 'RHOB_NPHI']
        
        n_wells = len(self.well_order)
        tracks_per_well = len(track_names) + (1 if show_facies else 0)
        
        # Create subplot configuration
        width_ratios = [1, 0.8, 0.8] + ([0.6] if show_facies else [])
        width_ratios = width_ratios * n_wells
        
        fig, axes = plt.subplots(
            nrows=1, 
            ncols=tracks_per_well * n_wells, 
            figsize=figsize,
            gridspec_kw={
                'hspace': 0, 
                'wspace': 0, 
                'width_ratios': width_ratios
            },
            sharey=True
        )
        
        # Ensure axes is always a list
        if n_wells == 1 and tracks_per_well == 1:
            axes = [axes]
        elif n_wells == 1 or tracks_per_well == 1:
            axes = axes.flatten()
        
        # Get flattened data if requested
        if flatten:
            flattened_data = self.create_flattened_section(datum_marker)
        
        # Define facies colors
        facies_colors = {
            0: 'magenta',
            1: 'blue', 
            2: 'red',
            3: 'green',
            4: 'yellow',
            5: 'black'
        }
        color_list = [facies_colors[i] for i in sorted(facies_colors.keys())]
        cmap_facies = mcolors.ListedColormap(color_list)
        
        # Plot each well
        for i, well_id in enumerate(self.well_order):
            log_analysis = self.log_analyses[well_id]
            
            # Get depth reference
            if flatten and well_id in flattened_data:
                depth_ref = 'flattened'
                curves_data = flattened_data[well_id]['curves']
            else:
                depth_ref = 'original'
                curves_data = {name: {'depth': curve.depth, 'values': curve.values} 
                              for name, curve in log_analysis.curves.items()}
            
            # Calculate track indices
            base_track = i * tracks_per_well
            
            # Plot tracks
            for j, track_name in enumerate(track_names):
                track_idx = base_track + j
                
                if track_name == 'GR' and 'GR' in curves_data:
                    self._plot_gr_track(axes[track_idx], curves_data['GR'], well_id if j == 0 else None)
                
                elif track_name == 'RT' and 'RT' in curves_data:
                    self._plot_rt_track(axes[track_idx], curves_data['RT'])
                
                elif track_name == 'RHOB_NPHI' and 'RHOB' in curves_data and 'NPHI' in curves_data:
                    self._plot_rhob_nphi_track(axes[track_idx], curves_data['RHOB'], curves_data['NPHI'])
            
            # Plot facies track if requested
            if show_facies:
                facies_track_idx = base_track + len(track_names)
                
                # Calculate facies if not already done
                if log_analysis.facies_model is None:
                    try:
                        log_analysis.calculate_facies_from_logs()
                    except Exception as e:
                        warnings.warn(f"Could not calculate facies for well {well_id}: {e}")
                
                if log_analysis.facies_model is not None:
                    self._plot_facies_track(axes[facies_track_idx], log_analysis.facies_model, cmap_facies)
            
            # Plot correlation markers
            if well_id in self.correlation_markers:
                self._plot_correlation_markers(axes[base_track:base_track + tracks_per_well], 
                                               self.correlation_markers[well_id], flatten, 
                                               flattened_data.get(well_id, {}).get('datum_depth', 0) if flatten else 0)
        
        # Draw flattening datum line if flattened
        if flatten:
            for ax in axes:
                ax.axhline(y=0, color='black', linewidth=2, label='Datum')
        
        # Set y-axis direction
        axes[0].invert_yaxis()
        
        plt.tight_layout()
        return fig
    
    def _plot_gr_track(self, ax, gr_data, title=None):
        """Plot gamma ray track."""
        depth = gr_data['depth']
        values = gr_data['values']
        
        # Remove null values
        valid_mask = values != -999.25
        depth_valid = depth[valid_mask]
        values_valid = values[valid_mask]
        
        if len(values_valid) == 0:
            return
        
        ax.plot(values_valid, depth_valid, color='green', label='GR')
        
        # Shading
        ax.fill_betweenx(depth_valid, values_valid, 30, 
                        where=(values_valid < 30), color='yellow', alpha=0.5)
        ax.fill_betweenx(depth_valid, 30, values_valid, 
                        where=(values_valid > 30), color='grey', alpha=0.5)
        
        ax.set_xlim(0, 150)
        if title:
            ax.set_title(title)
        ax.legend(loc="upper right")
    
    def _plot_rt_track(self, ax, rt_data):
        """Plot resistivity track."""
        depth = rt_data['depth']
        values = rt_data['values']
        
        valid_mask = (values != -999.25) & (values > 0)
        depth_valid = depth[valid_mask]
        values_valid = values[valid_mask]
        
        if len(values_valid) == 0:
            return
        
        ax.plot(values_valid, depth_valid, color='black', label='RT')
        
        # Color fill
        norm = plt.Normalize(values_valid.min(), values_valid.max())
        colors = plt.cm.rainbow(norm(values_valid))
        
        for j in range(len(values_valid) - 1):
            ax.fill_betweenx(
                depth_valid[j:j+2], 0, values_valid[j:j+2],
                color=colors[j], edgecolor='none'
            )
        
        ax.set_xlim(0.1, 1000)
        ax.set_xscale('log')
        ax.legend(loc="upper right")
    
    def _plot_rhob_nphi_track(self, ax, rhob_data, nphi_data):
        """Plot density-neutron track."""
        # Get common valid data
        rhob_depth, rhob_values = rhob_data['depth'], rhob_data['values']
        nphi_depth, nphi_values = nphi_data['depth'], nphi_data['values']
        
        rhob_valid = rhob_values != -999.25
        nphi_valid = nphi_values != -999.25
        
        # Plot RHOB
        if np.any(rhob_valid):
            secax_rhob = ax.twiny()
            secax_rhob.plot(rhob_values[rhob_valid], rhob_depth[rhob_valid], 
                           color='red', label='RHOB')
            secax_rhob.set_xlim(1.95, 2.95)
            secax_rhob.legend(loc="upper right")
        
        # Plot NPHI
        if np.any(nphi_valid):
            secax_nphi = ax.twiny()
            secax_nphi.plot(nphi_values[nphi_valid], nphi_depth[nphi_valid], 
                           color='blue', label='NPHI')
            secax_nphi.set_xlim(0.45, -0.15)
            secax_nphi.legend(loc="upper left")
            
            # Hydrocarbon/shale indication
            if np.any(rhob_valid):
                # Calculate density porosity for comparison
                dphi = (2.65 - rhob_values) / (2.65 - 1.0)
                
                common_mask = rhob_valid & nphi_valid
                if np.any(common_mask):
                    secax_nphi.fill_betweenx(
                        rhob_depth[common_mask], dphi[common_mask], nphi_values[common_mask],
                        where=(dphi[common_mask] > nphi_values[common_mask]),
                        color="yellow", alpha=0.7
                    )
                    secax_nphi.fill_betweenx(
                        rhob_depth[common_mask], dphi[common_mask], nphi_values[common_mask],
                        where=(dphi[common_mask] < nphi_values[common_mask]),
                        color="grey", alpha=0.7
                    )
    
    def _plot_facies_track(self, ax, facies_model, cmap):
        """Plot facies track."""
        depths = facies_model['depths']
        facies = facies_model['facies']
        
        # Create 2D array for imshow
        facies_2d = facies.reshape(-1, 1)
        
        ax.imshow(facies_2d, aspect='auto', cmap=cmap,
                 extent=[-1, 1, depths.max(), depths.min()])
        ax.set_xlabel("FACIES")
    
    def _plot_correlation_markers(self, axes, markers, flatten, datum_depth):
        """Plot correlation markers across tracks."""
        for marker in markers:
            depth = marker.top_depth
            if flatten:
                depth = depth - datum_depth
            
            for ax in axes:
                ax.axhline(y=depth, color='purple', linestyle='--', linewidth=2)


class WellCorrelation:
    """Multi-well correlation and mapping."""
    
    def __init__(self):
        """Initialize well correlation."""
        self.wells: Dict[str, WellData] = {}
        self.log_analyses: Dict[str, LogAnalysis] = {}
        self.structural_surfaces: Dict[str, Dict[str, float]] = {}  # surface_name -> {well_id: depth}
    
    def add_well_data(self, well_data: WellData, log_analysis: LogAnalysis):
        """Add well data and analysis.
        
        Args:
            well_data: Well data
            log_analysis: Log analysis
        """
        self.wells[well_data.well_id] = well_data
        self.log_analyses[well_data.well_id] = log_analysis
    
    def add_structural_surface(self, surface_name: str, well_depths: Dict[str, float]):
        """Add structural surface depths.
        
        Args:
            surface_name: Surface identifier
            well_depths: Dictionary of well_id -> depth
        """
        self.structural_surfaces[surface_name] = well_depths
    
    def create_structure_map(self, surface_name: str, grid_size: int = 50) -> Dict[str, np.ndarray]:
        """Create structure map for a surface.
        
        Args:
            surface_name: Surface to map
            grid_size: Grid resolution
        
        Returns:
            Dictionary with gridded structure data
        """
        if surface_name not in self.structural_surfaces:
            raise ValueError(f"Surface {surface_name} not found")
        
        surface_data = self.structural_surfaces[surface_name]
        
        # Extract well locations and depths
        x_coords = []
        y_coords = []
        depths = []
        
        for well_id, depth in surface_data.items():
            if well_id in self.wells:
                well = self.wells[well_id]
                x_coords.append(well.x_coord)
                y_coords.append(well.y_coord)
                depths.append(depth)
        
        if len(x_coords) < 3:
            raise ValueError("Need at least 3 wells for structure mapping")
        
        # Create grid
        x_min, x_max = min(x_coords), max(x_coords)
        y_min, y_max = min(y_coords), max(y_coords)
        
        # Add buffer
        x_buffer = (x_max - x_min) * 0.1
        y_buffer = (y_max - y_min) * 0.1
        
        x_grid = np.linspace(x_min - x_buffer, x_max + x_buffer, grid_size)
        y_grid = np.linspace(y_min - y_buffer, y_max + y_buffer, grid_size)
        
        X, Y = np.meshgrid(x_grid, y_grid)
        
        # Interpolate using scipy
        points = np.column_stack((x_coords, y_coords))
        values = np.array(depths)
        
        # Use linear interpolation
        Z = interpolate.griddata(points, values, (X, Y), method='linear')
        
        return {
            'x_grid': x_grid,
            'y_grid': y_grid,
            'x_mesh': X,
            'y_mesh': Y,
            'depth_mesh': Z,
            'well_x': x_coords,
            'well_y': y_coords,
            'well_depths': depths,
            'surface_name': surface_name
        }


class ReservoirMapping:
    """Reservoir property mapping."""
    
    def __init__(self):
        """Initialize reservoir mapping."""
        self.property_data: Dict[str, Dict[str, float]] = {}  # property -> {well_id: value}
        self.well_locations: Dict[str, Tuple[float, float]] = {}  # well_id -> (x, y)
    
    def add_property_data(self, property_name: str, well_id: str, value: float, x_coord: float, y_coord: float):
        """Add property data for a well.
        
        Args:
            property_name: Property name
            well_id: Well identifier
            value: Property value
            x_coord: Well x coordinate
            y_coord: Well y coordinate
        """
        if property_name not in self.property_data:
            self.property_data[property_name] = {}
        
        self.property_data[property_name][well_id] = value
        self.well_locations[well_id] = (x_coord, y_coord)
    
    def create_property_map(self, property_name: str, grid_size: int = 50) -> Dict[str, np.ndarray]:
        """Create property map.
        
        Args:
            property_name: Property to map
            grid_size: Grid resolution
        
        Returns:
            Dictionary with gridded property data
        """
        if property_name not in self.property_data:
            raise ValueError(f"Property {property_name} not found")
        
        prop_data = self.property_data[property_name]
        
        # Extract coordinates and values
        x_coords = []
        y_coords = []
        values = []
        
        for well_id, value in prop_data.items():
            if well_id in self.well_locations:
                x, y = self.well_locations[well_id]
                x_coords.append(x)
                y_coords.append(y)
                values.append(value)
        
        if len(x_coords) < 3:
            raise ValueError("Need at least 3 wells for property mapping")
        
        # Create grid and interpolate (similar to structure mapping)
        x_min, x_max = min(x_coords), max(x_coords)
        y_min, y_max = min(y_coords), max(y_coords)
        
        x_buffer = (x_max - x_min) * 0.1
        y_buffer = (y_max - y_min) * 0.1
        
        x_grid = np.linspace(x_min - x_buffer, x_max + x_buffer, grid_size)
        y_grid = np.linspace(y_min - y_buffer, y_max + y_buffer, grid_size)
        
        X, Y = np.meshgrid(x_grid, y_grid)
        
        points = np.column_stack((x_coords, y_coords))
        Z = interpolate.griddata(points, values, (X, Y), method='linear')
        
        return {
            'x_grid': x_grid,
            'y_grid': y_grid,
            'x_mesh': X,
            'y_mesh': Y,
            'property_mesh': Z,
            'well_x': x_coords,
            'well_y': y_coords,
            'well_values': values,
            'property_name': property_name
        }


class PerformanceAnalysis:
    """Production performance analysis."""
    
    def __init__(self):
        """Initialize performance analysis."""
        self.production_data: Dict[str, List[ProductionData]] = {}  # well_id -> production data
        self.well_properties: Dict[str, Dict[str, float]] = {}  # well_id -> properties
    
    def add_production_data(self, well_id: str, production_data: List[ProductionData]):
        """Add production data for a well.
        
        Args:
            well_id: Well identifier
            production_data: List of production data
        """
        self.production_data[well_id] = production_data
    
    def calculate_eur_by_analogy(self, target_well: str, analog_wells: List[str]) -> Dict[str, float]:
        """Estimate EUR using analog wells.
        
        Args:
            target_well: Target well for EUR estimation
            analog_wells: List of analog well IDs
        
        Returns:
            Dictionary with EUR estimates
        """
        if not analog_wells:
            raise ValueError("No analog wells provided")
        
        # Calculate cumulative production for analogs
        analog_eurs = []
        for well_id in analog_wells:
            if well_id in self.production_data:
                prod_data = self.production_data[well_id]
                if prod_data:
                    # Simple cumulative calculation
                    total_days = (prod_data[-1].date - prod_data[0].date).days
                    total_oil = sum(p.oil_rate for p in prod_data) * (total_days / len(prod_data))
                    analog_eurs.append(total_oil)
        
        if not analog_eurs:
            raise ValueError("No valid analog production data found")
        
        # Statistical analysis of analogs
        eur_stats = {
            'mean_eur': np.mean(analog_eurs),
            'median_eur': np.median(analog_eurs),
            'p90_eur': np.percentile(analog_eurs, 10),
            'p50_eur': np.percentile(analog_eurs, 50),
            'p10_eur': np.percentile(analog_eurs, 90),
            'std_eur': np.std(analog_eurs),
            'analog_count': len(analog_eurs)
        }
        
        return eur_stats


class CoreAnalysis:
    """Core data analysis and correlation."""
    
    def __init__(self):
        """Initialize core analysis."""
        self.core_data: Dict[str, pd.DataFrame] = {}  # well_id -> core measurements
        self.thin_sections: Dict[str, Dict] = {}  # well_id -> thin section analysis
    
    def add_core_data(self, well_id: str, core_data: pd.DataFrame):
        """Add core analysis data.
        
        Args:
            well_id: Well identifier
            core_data: DataFrame with core measurements
        """
        required_columns = ['depth', 'porosity', 'permeability']
        if not all(col in core_data.columns for col in required_columns):
            raise ValueError(f"Core data must contain columns: {required_columns}")
        
        self.core_data[well_id] = core_data
    
    def correlate_core_to_logs(self, well_id: str, log_analysis: LogAnalysis) -> Dict[str, float]:
        """Correlate core data to log data.
        
        Args:
            well_id: Well identifier
            log_analysis: Log analysis object
        
        Returns:
            Dictionary with correlation statistics
        """
        if well_id not in self.core_data:
            raise ValueError(f"No core data for well {well_id}")
        
        core_df = self.core_data[well_id]
        
        # Get log porosity at core depths
        if 'porosity' in log_analysis.petrophysical_results:
            log_por_data = log_analysis.petrophysical_results['porosity']
            log_depths = log_por_data['depth']
            log_porosity = log_por_data['values']
            
            # Interpolate log porosity to core depths
            interp_func = interpolate.interp1d(
                log_depths, log_porosity, 
                bounds_error=False, fill_value=np.nan
            )
            
            log_por_at_core = interp_func(core_df['depth'])
            
            # Calculate correlation
            valid_mask = ~np.isnan(log_por_at_core) & ~np.isnan(core_df['porosity'])
            
            if np.sum(valid_mask) > 3:
                correlation = stats.pearsonr(
                    core_df['porosity'][valid_mask], 
                    log_por_at_core[valid_mask]
                )
                
                return {
                    'porosity_correlation': correlation[0],
                    'porosity_p_value': correlation[1],
                    'valid_points': np.sum(valid_mask),
                    'rmse': np.sqrt(np.mean((core_df['porosity'][valid_mask] - log_por_at_core[valid_mask])**2))
                }
        
        return {'error': 'Insufficient data for correlation'}


class ReservoirAnalysis:
    """Main reservoir analysis container.
    
    This class integrates all analysis components and provides
    a unified interface for comprehensive reservoir analysis.
    """
    
    def __init__(self, reservoir_properties: ReservoirProperties):
        """Initialize reservoir analysis.
        
        Args:
            reservoir_properties: Reservoir properties
        """
        self.properties = reservoir_properties
        self.log_analysis = LogAnalysis
        self.stratigraphic_analysis = StratigraphicAnalysis()
        self.well_correlation = WellCorrelation()
        self.reservoir_mapping = ReservoirMapping()
        self.performance_analysis = PerformanceAnalysis()
        self.core_analysis = CoreAnalysis()
        
        # Analysis results
        self.analysis_results: Dict[str, Any] = {}
        self.created_date = datetime.now()
    
    def add_well_analysis(self, well_data: WellData, log_analysis: LogAnalysis):
        """Add well analysis to reservoir analysis.
        
        Args:
            well_data: Well data
            log_analysis: Log analysis
        """
        # Add to all relevant components
        self.stratigraphic_analysis.add_well_analysis(well_data.well_id, log_analysis)
        self.well_correlation.add_well_data(well_data, log_analysis)
        
        # Extract net pay properties for mapping
        try:
            net_pay = log_analysis.calculate_net_pay()
            self.reservoir_mapping.add_property_data(
                'net_thickness', well_data.well_id, net_pay['net_thickness'],
                well_data.x_coord, well_data.y_coord
            )
            self.reservoir_mapping.add_property_data(
                'porosity', well_data.well_id, net_pay['average_porosity'],
                well_data.x_coord, well_data.y_coord
            )
        except Exception as e:
            warnings.warn(f"Could not calculate net pay for well {well_data.well_id}: {e}")
    
    def run_comprehensive_analysis(self) -> Dict[str, Any]:
        """Run comprehensive reservoir analysis.
        
        Returns:
            Dictionary with all analysis results
        """
        results = {
            'analysis_date': datetime.now(),
            'reservoir_properties': {
                'ooip': self.properties.calculate_ooip(),
                'ogip': self.properties.calculate_ogip(),
                'pore_volume': self.properties.calculate_pore_volume()
            }
        }
        
        # Add well-by-well analysis
        well_results = {}
        for well_id, log_analysis in self.stratigraphic_analysis.log_analyses.items():
            try:
                net_pay = log_analysis.calculate_net_pay()
                well_results[well_id] = {
                    'net_pay': net_pay,
                    'facies_calculated': log_analysis.facies_model is not None
                }
            except Exception as e:
                well_results[well_id] = {'error': str(e)}
        
        results['wells'] = well_results
        
        # Store results
        self.analysis_results = results
        return results
    
    def export_analysis_report(self) -> Dict[str, Any]:
        """Export comprehensive analysis report.
        
        Returns:
            Dictionary with formatted analysis report
        """
        report = {
            'header': {
                'title': 'Reservoir Analysis Report',
                'date': datetime.now().isoformat(),
                'analyst': 'DigitalModel Reservoir Analysis'
            },
            'executive_summary': {
                'reservoir_type': 'Conventional',  # Could be determined from analysis
                'total_wells': len(self.stratigraphic_analysis.log_analyses),
                'ooip_stb': self.properties.calculate_ooip(),
                'average_porosity': self.properties.rock.porosity,
                'average_permeability': self.properties.rock.permeability
            },
            'detailed_results': self.analysis_results
        }
        
        return report
