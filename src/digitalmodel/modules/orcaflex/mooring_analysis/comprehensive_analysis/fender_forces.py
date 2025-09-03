"""Fender forces analysis module for mooring systems."""

from pathlib import Path
from typing import Dict, List
import pandas as pd
import numpy as np
import logging
from datetime import datetime

from .models import FenderData, FenderForceData, FenderMetrics
from .exceptions import DataParsingError, FenderAnalysisError

logger = logging.getLogger(__name__)


class FenderForcesAnalyzer:
    """Analyzes fender forces and utilization."""
    
    def __init__(self, warning_utilization: float = 0.8, critical_utilization: float = 0.95):
        """Initialize fender forces analyzer.
        
        Args:
            warning_utilization: Warning threshold for fender utilization
            critical_utilization: Critical threshold for fender utilization
        """
        self.warning_utilization = warning_utilization
        self.critical_utilization = critical_utilization
    
    def parse_fender_csv(self, file: Path) -> FenderForceData:
        """Parse fender forces CSV file.
        
        Args:
            file: Path to CSV file
            
        Returns:
            Parsed fender force data
            
        Raises:
            DataParsingError: If CSV cannot be parsed
        """
        try:
            df = pd.read_csv(file)
            
            # Handle different possible column names
            fender_id_col = 'FenderID' if 'FenderID' in df else 'ObjectName'
            
            if fender_id_col not in df:
                raise DataParsingError(str(file), "No fender identifier column found")
            
            # Parse fender data
            fenders = []
            for fender_id in df[fender_id_col].unique():
                fender_df = df[df[fender_id_col] == fender_id]
                
                fender_data = FenderData(
                    fender_id=str(fender_id),
                    location=(
                        float(fender_df['Location_X'].iloc[0]) if 'Location_X' in fender_df else 0,
                        float(fender_df['Location_Y'].iloc[0]) if 'Location_Y' in fender_df else 0,
                        float(fender_df['Location_Z'].iloc[0]) if 'Location_Z' in fender_df else 0
                    ),
                    forces=fender_df['Force'].tolist() if 'Force' in fender_df else [],
                    max_force=float(fender_df['Max_Force'].iloc[0]) if 'Max_Force' in fender_df else 0,
                    mean_force=float(fender_df['Mean_Force'].iloc[0]) if 'Mean_Force' in fender_df else 0,
                    contact_events=int(fender_df['Contact_Events'].iloc[0]) if 'Contact_Events' in fender_df else 0,
                    contact_duration=float(fender_df['Contact_Duration'].iloc[0]) if 'Contact_Duration' in fender_df else 0,
                    compression=float(fender_df['Compression'].iloc[0]) if 'Compression' in fender_df else None
                )
                fenders.append(fender_data)
            
            return FenderForceData(
                filename=file,
                timestamp=datetime.now(),
                fenders=fenders,
                raw_dataframe=df
            )
            
        except Exception as e:
            raise DataParsingError(str(file), str(e))
    
    def calculate_utilization(self, data: FenderForceData, design_capacity: Dict[str, float] = None) -> FenderMetrics:
        """Calculate fender utilization metrics.
        
        Args:
            data: Fender force data
            design_capacity: Design capacity for each fender in kN
            
        Returns:
            Fender metrics
        """
        if design_capacity is None:
            design_capacity = {}
        
        utilization_rates = {}
        max_forces = {}
        mean_forces = {}
        contact_percentages = {}
        design_margin = {}
        
        for fender in data.fenders:
            max_forces[fender.fender_id] = fender.max_force
            mean_forces[fender.fender_id] = fender.mean_force
            contact_percentages[fender.fender_id] = fender.contact_duration
            
            # Calculate utilization if design capacity is known
            if fender.fender_id in design_capacity:
                capacity = design_capacity[fender.fender_id]
                utilization_rates[fender.fender_id] = (fender.max_force / capacity) * 100 if capacity > 0 else 0
                design_margin[fender.fender_id] = 100 - utilization_rates[fender.fender_id]
            else:
                utilization_rates[fender.fender_id] = 0
                design_margin[fender.fender_id] = 100
        
        # Identify critical and overloaded fenders
        critical_fenders = [
            fid for fid, util in utilization_rates.items()
            if util >= self.warning_utilization * 100
        ]
        
        overloaded_fenders = [
            fid for fid, util in utilization_rates.items()
            if util >= self.critical_utilization * 100
        ]
        
        # Calculate load sharing
        total_force = sum(max_forces.values())
        load_sharing = {
            fid: (force / total_force) * 100 if total_force > 0 else 0
            for fid, force in max_forces.items()
        }
        
        # Force distribution statistics
        force_values = list(max_forces.values())
        force_distribution_stats = {
            'mean': np.mean(force_values),
            'std': np.std(force_values),
            'min': np.min(force_values),
            'max': np.max(force_values)
        }
        
        return FenderMetrics(
            utilization_rates=utilization_rates,
            max_forces=max_forces,
            mean_forces=mean_forces,
            critical_fenders=critical_fenders,
            load_sharing=load_sharing,
            contact_percentages=contact_percentages,
            force_distribution_stats=force_distribution_stats,
            overloaded_fenders=overloaded_fenders,
            design_margin=design_margin,
            recommendations=self._generate_recommendations(utilization_rates, load_sharing)
        )
    
    def _generate_recommendations(self, utilization: Dict[str, float], load_sharing: Dict[str, float]) -> List[str]:
        """Generate recommendations based on fender analysis."""
        recommendations = []
        
        # Check for overutilized fenders
        high_util = [fid for fid, util in utilization.items() if util > 80]
        if high_util:
            recommendations.append(f"Fenders {', '.join(high_util)} are highly utilized - consider upgrading")
        
        # Check for uneven load distribution
        if load_sharing:
            max_share = max(load_sharing.values())
            min_share = min(load_sharing.values())
            if max_share > min_share * 3:
                recommendations.append("Uneven load distribution detected - review mooring configuration")
        
        return recommendations