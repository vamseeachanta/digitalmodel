"""Context extraction module using pattern matching and LLM."""

from pathlib import Path
from typing import Dict, Optional
import re
import logging

from .models import ContextInfo

logger = logging.getLogger(__name__)


class ContextExtractor:
    """Extracts context from filenames and content."""
    
    def __init__(self, use_llm: bool = False):
        """Initialize context extractor.
        
        Args:
            use_llm: Whether to use LLM for complex patterns
        """
        self.use_llm = use_llm
        self.patterns = {
            'vessel': r'(?P<vessel>lngc|fsru|fpso|fso)',
            'depth': r'(?P<depth>\d+)km',
            'condition': r'(?P<condition>ballast|loaded|pb|sb)',
            'environment': r'(?P<env>\d+yr|survival|operational)',
            'analysis': r'(?P<analysis>static|dynamic|statics|dynamics)',
            'return_period': r'(?P<return>l?\d+)',
        }
    
    def extract_from_filename(self, filename: str) -> ContextInfo:
        """Extract context from filename.
        
        Args:
            filename: Filename to analyze
            
        Returns:
            Extracted context information
        """
        context_dict = {}
        confidence = 0.0
        
        # Try pattern matching first
        for key, pattern in self.patterns.items():
            match = re.search(pattern, filename.lower())
            if match:
                groups = match.groupdict()
                for group_key, value in groups.items():
                    if value:
                        context_dict[group_key] = value
                        confidence += 0.15
        
        # Parse specific values
        context = ContextInfo(
            vessel_type=context_dict.get('vessel'),
            water_depth=self._parse_depth(context_dict.get('depth')),
            environment=context_dict.get('env'),
            loading_condition=context_dict.get('condition'),
            return_period=self._parse_return_period(context_dict.get('return')),
            analysis_type=context_dict.get('analysis'),
            confidence_score=min(confidence, 1.0)
        )
        
        # Use LLM if enabled and confidence is low
        if self.use_llm and confidence < 0.5:
            context = self._llm_extract_context(filename, context)
        
        return context
    
    def _parse_depth(self, depth_str: Optional[str]) -> Optional[float]:
        """Parse water depth from string."""
        if not depth_str:
            return None
        try:
            return float(depth_str)
        except ValueError:
            return None
    
    def _parse_return_period(self, period_str: Optional[str]) -> Optional[int]:
        """Parse return period from string."""
        if not period_str:
            return None
        try:
            # Remove 'l' prefix if present
            period_str = period_str.lstrip('l')
            return int(period_str)
        except ValueError:
            return None
    
    def _llm_extract_context(self, filename: str, initial_context: ContextInfo) -> ContextInfo:
        """Use LLM to extract context from complex patterns.
        
        Args:
            filename: Filename to analyze
            initial_context: Initial context from pattern matching
            
        Returns:
            Enhanced context information
        """
        # Placeholder for LLM integration
        logger.info(f"LLM context extraction for: {filename}")
        
        # For now, return the initial context with slightly higher confidence
        initial_context.confidence_score = min(initial_context.confidence_score + 0.2, 1.0)
        return initial_context
    
    def apply_standards(self, context: ContextInfo) -> Dict:
        """Apply relevant industry standards based on context.
        
        Args:
            context: Context information
            
        Returns:
            Applicable standards and criteria
        """
        standards = {
            'primary': 'DNV-OS-E301',
            'secondary': [],
            'criteria': {}
        }
        
        # Apply vessel-specific standards
        if context.vessel_type == 'lngc':
            standards['secondary'].append('SIGTTO Guidelines')
            standards['criteria']['min_safety_factor'] = 1.67
        elif context.vessel_type == 'fsru':
            standards['secondary'].append('API RP 2SK')
            standards['criteria']['min_safety_factor'] = 1.5
        
        # Apply environment-specific criteria
        if context.environment == 'survival':
            standards['criteria']['tension_limit'] = 0.95
        else:
            standards['criteria']['tension_limit'] = 0.85
        
        return standards