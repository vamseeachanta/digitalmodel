"""
Line property management module.

This module handles mooring line property modifications, backup/restore,
and stiffness calculations.
"""

import numpy as np
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field
from loguru import logger
import copy

from .orcaflex_interface import OrcaFlexModelInterface
from .config import LineProperties


@dataclass
class LineState:
    """State of a mooring line at a point in time."""
    name: str
    sections: List[float]  # Section lengths
    total_length: float
    tension: Optional[float] = None
    EA: Optional[float] = None  # Axial stiffness
    
    def copy(self) -> 'LineState':
        """Create a deep copy of the line state."""
        return LineState(
            name=self.name,
            sections=self.sections.copy(),
            total_length=self.total_length,
            tension=self.tension,
            EA=self.EA
        )


class LinePropertyManager:
    """
    Manager for mooring line property modifications.
    
    Handles line length adjustments, property backup/restore,
    and stiffness-based calculations.
    """
    
    def __init__(self, model_interface: OrcaFlexModelInterface):
        """
        Initialize the line property manager.
        
        Args:
            model_interface: Interface to OrcaFlex model
        """
        self.model = model_interface
        self.original_state: Dict[str, LineState] = {}
        self.current_state: Dict[str, LineState] = {}
        self.line_properties: Dict[str, LineProperties] = {}
        
    def set_line_properties(self, properties: List[LineProperties]) -> None:
        """
        Set line properties from configuration.
        
        Args:
            properties: List of LineProperties objects
        """
        self.line_properties = {prop.name: prop for prop in properties}
        logger.info(f"Set properties for {len(properties)} lines")
    
    def backup_original_properties(self) -> None:
        """
        Backup original line properties before modifications.
        """
        logger.info("Backing up original line properties")
        
        line_lengths = self.model.get_line_lengths()
        
        for line_name, sections in line_lengths.items():
            total_length = sum(s for s in sections if s is not None)
            
            state = LineState(
                name=line_name,
                sections=sections.copy(),
                total_length=total_length
            )
            
            # Add EA if available
            if line_name in self.line_properties:
                state.EA = self.line_properties[line_name].EA
            
            self.original_state[line_name] = state
            self.current_state[line_name] = state.copy()
        
        logger.info(f"Backed up properties for {len(self.original_state)} lines")
    
    def restore_original_properties(self) -> None:
        """
        Restore original line properties.
        """
        logger.info("Restoring original line properties")
        
        lengths_to_restore = {}
        for line_name, state in self.original_state.items():
            lengths_to_restore[line_name] = state.sections
        
        self.model.set_line_lengths(lengths_to_restore)
        self.current_state = {name: state.copy() 
                             for name, state in self.original_state.items()}
        
        logger.info("Original properties restored")
    
    def adjust_line_lengths(self, new_lengths: Dict[str, List[float]]) -> None:
        """
        Adjust line lengths with validation.
        
        Args:
            new_lengths: Dictionary of line names to new section lengths
        """
        validated_lengths = {}
        
        for line_name, sections in new_lengths.items():
            # Validate against constraints if available
            if line_name in self.line_properties:
                prop = self.line_properties[line_name]
                total = sum(s for s in sections if s is not None)
                
                # Check min/max constraints
                if prop.min_length and total < prop.min_length:
                    logger.warning(f"{line_name}: Length {total:.1f}m below minimum {prop.min_length:.1f}m")
                    # Apply minimum constraint
                    scale = prop.min_length / total
                    sections = [s * scale if s else None for s in sections]
                    
                elif prop.max_length and total > prop.max_length:
                    logger.warning(f"{line_name}: Length {total:.1f}m above maximum {prop.max_length:.1f}m")
                    # Apply maximum constraint
                    scale = prop.max_length / total
                    sections = [s * scale if s else None for s in sections]
            
            validated_lengths[line_name] = sections
            
            # Update current state
            if line_name in self.current_state:
                self.current_state[line_name].sections = sections.copy()
                self.current_state[line_name].total_length = sum(
                    s for s in sections if s is not None
                )
        
        # Apply to model
        self.model.set_line_lengths(validated_lengths)
        logger.debug(f"Adjusted lengths for {len(validated_lengths)} lines")
    
    def calculate_stiffness_matrix(self, line_names: List[str]) -> np.ndarray:
        """
        Calculate approximate stiffness matrix for mooring lines.
        
        Uses simplified catenary stiffness approximation.
        
        Args:
            line_names: List of line names in order
            
        Returns:
            Stiffness matrix K where K[i,j] = dT_i/dL_j approximation
        """
        n = len(line_names)
        K = np.zeros((n, n))
        
        for i, line_name in enumerate(line_names):
            if line_name not in self.line_properties:
                # Use default stiffness
                K[i, i] = 1000.0  # Default diagonal stiffness
            else:
                prop = self.line_properties[line_name]
                state = self.current_state.get(line_name)
                
                if state:
                    # Simplified stiffness: k = EA / L
                    # This is a first-order approximation
                    k_axial = prop.EA / state.total_length
                    
                    # Add catenary effect (simplified)
                    # Actual catenary stiffness depends on pretension and geometry
                    catenary_factor = 0.5  # Simplified factor
                    k_effective = k_axial * catenary_factor
                    
                    K[i, i] = k_effective
                else:
                    K[i, i] = prop.EA / 200.0  # Assume 200m default length
        
        # Add small off-diagonal coupling for stability
        # This represents vessel motion coupling
        coupling_factor = 0.05
        for i in range(n):
            for j in range(n):
                if i != j:
                    K[i, j] = -coupling_factor * min(K[i, i], K[j, j])
        
        return K
    
    def calculate_length_updates_ea(self, 
                                   tension_errors: Dict[str, float]) -> Dict[str, List[float]]:
        """
        Calculate length updates using EA-based method (from existing mooring.py).
        
        Args:
            tension_errors: Dictionary of line names to tension errors (current - target)
            
        Returns:
            Dictionary of line names to updated section lengths
        """
        updated_lengths = {}
        
        for line_name, error in tension_errors.items():
            if line_name not in self.current_state:
                continue
                
            state = self.current_state[line_name]
            prop = self.line_properties.get(line_name)
            
            if not prop:
                logger.warning(f"No properties for line {line_name}")
                continue
            
            # Calculate length adjustment using EA
            # Delta_L = (L / EA) * Delta_T
            # This is based on the existing mooring.py approach
            sections = state.sections.copy()
            new_sections = []
            
            for i, length in enumerate(sections):
                if length is None:
                    # Variable section - will be calculated from total
                    new_sections.append(None)
                else:
                    # Fixed section - adjust proportionally
                    delta_length = (length / prop.EA) * error
                    new_length = length - delta_length  # Negative because we subtract error
                    new_sections.append(max(0.1, new_length))  # Minimum 0.1m
            
            # Handle variable section
            if None in new_sections:
                # Calculate total required length
                total_target = state.total_length - (state.total_length / prop.EA) * error
                fixed_length = sum(s for s in new_sections if s is not None)
                variable_length = max(0.1, total_target - fixed_length)
                
                # Assign to variable section
                for i, s in enumerate(new_sections):
                    if s is None:
                        new_sections[i] = variable_length
                        break
            
            updated_lengths[line_name] = new_sections
        
        return updated_lengths
    
    def apply_safety_limits(self, 
                           length_updates: Dict[str, float],
                           max_change_factor: float = 0.1) -> Dict[str, float]:
        """
        Apply safety limits to length updates.
        
        Args:
            length_updates: Dictionary of line names to length changes
            max_change_factor: Maximum fractional change allowed (0.1 = 10%)
            
        Returns:
            Limited length updates
        """
        limited_updates = {}
        
        for line_name, delta_length in length_updates.items():
            if line_name in self.current_state:
                current_length = self.current_state[line_name].total_length
                max_change = current_length * max_change_factor
                
                if abs(delta_length) > max_change:
                    logger.warning(f"{line_name}: Limiting change from {delta_length:.1f}m to {max_change:.1f}m")
                    limited_updates[line_name] = np.sign(delta_length) * max_change
                else:
                    limited_updates[line_name] = delta_length
            else:
                limited_updates[line_name] = delta_length
        
        return limited_updates
    
    def get_current_lengths(self) -> Dict[str, List[float]]:
        """
        Get current line section lengths.
        
        Returns:
            Dictionary of line names to section lengths
        """
        return {name: state.sections.copy() 
                for name, state in self.current_state.items()}
    
    def get_total_lengths(self) -> Dict[str, float]:
        """
        Get total line lengths.
        
        Returns:
            Dictionary of line names to total lengths
        """
        return {name: state.total_length 
                for name, state in self.current_state.items()}
    
    def generate_length_report(self) -> str:
        """
        Generate a report of line length changes.
        
        Returns:
            Formatted report string
        """
        lines = ["LINE LENGTH MODIFICATIONS"]
        lines.append("-" * 60)
        
        if not self.original_state:
            lines.append("No original state recorded")
            return "\n".join(lines)
        
        lines.append(f"{'Line':<10} {'Original':>12} {'Current':>12} {'Change':>12} {'Change %':>10}")
        lines.append("-" * 60)
        
        for line_name in sorted(self.original_state.keys()):
            original = self.original_state[line_name].total_length
            current = self.current_state.get(line_name, self.original_state[line_name]).total_length
            change = current - original
            change_pct = (change / original) * 100 if original != 0 else 0
            
            lines.append(
                f"{line_name:<10} "
                f"{original:>12.2f} "
                f"{current:>12.2f} "
                f"{change:>12.2f} "
                f"{change_pct:>10.2f}"
            )
        
        return "\n".join(lines)
