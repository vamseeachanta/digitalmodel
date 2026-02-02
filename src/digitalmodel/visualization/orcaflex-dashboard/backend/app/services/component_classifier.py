"""
Component Classification Service

Classifies OrcaFlex analysis components based on column names and data patterns.
Identifies: fst1, fst2, strut, jacket, lngc components and their associated data.
"""

import re
import logging
from typing import Dict, List, Optional, Set, Tuple
from dataclasses import dataclass
from pathlib import Path
import pandas as pd
import numpy as np
from enum import Enum

logger = logging.getLogger(__name__)


class ComponentType(Enum):
    """OrcaFlex component types"""
    FST1 = "fst1"          # Floater 1
    FST2 = "fst2"          # Floater 2  
    STRUT = "strut"        # Connector/strut
    JACKET = "jacket"      # Foundation/jacket
    LNGC = "lngc"         # LNG Carrier vessel
    UNKNOWN = "unknown"    # Unclassified


class ParameterType(Enum):
    """Engineering parameter types"""
    FORCE_X = "force_x"
    FORCE_Y = "force_y"
    FORCE_Z = "force_z"
    MOMENT_X = "moment_x"
    MOMENT_Y = "moment_y"
    MOMENT_Z = "moment_z"
    DISPLACEMENT_X = "displacement_x"
    DISPLACEMENT_Y = "displacement_y"
    DISPLACEMENT_Z = "displacement_z"
    ROTATION_X = "rotation_x"
    ROTATION_Y = "rotation_y"
    ROTATION_Z = "rotation_z"
    VELOCITY = "velocity"
    ACCELERATION = "acceleration"
    TENSION = "tension"
    CURVATURE = "curvature"
    OTHER = "other"


@dataclass
class ComponentInfo:
    """Information about a classified component"""
    name: str
    component_type: ComponentType
    columns: List[str]
    parameters: Dict[str, ParameterType]
    confidence_score: float
    metadata: Dict = None
    
    def __post_init__(self):
        if self.metadata is None:
            self.metadata = {}


@dataclass
class ClassificationResult:
    """Result of component classification"""
    components: List[ComponentInfo]
    unclassified_columns: List[str]
    classification_confidence: float
    total_columns: int
    classified_columns: int


class ComponentClassifier:
    """
    Classifies OrcaFlex analysis components and parameters.
    
    Uses pattern matching and heuristics to identify:
    - Component types (fst1, fst2, strut, jacket, lngc)
    - Parameter types (forces, moments, displacements, etc.)
    - Data relationships and hierarchies
    """
    
    # Component identification patterns
    COMPONENT_PATTERNS = {
        ComponentType.FST1: [
            r'fst1\b',
            r'float.*1\b',
            r'floater.*1\b',
            r'hull.*1\b',
            r'platform.*1\b'
        ],
        ComponentType.FST2: [
            r'fst2\b',
            r'float.*2\b', 
            r'floater.*2\b',
            r'hull.*2\b',
            r'platform.*2\b'
        ],
        ComponentType.STRUT: [
            r'strut\b',
            r'connector\b',
            r'connection\b',
            r'link\b',
            r'tie\b'
        ],
        ComponentType.JACKET: [
            r'jacket\b',
            r'foundation\b',
            r'base\b',
            r'pile\b',
            r'suction\b'
        ],
        ComponentType.LNGC: [
            r'lngc\b',
            r'vessel\b',
            r'ship\b',
            r'carrier\b',
            r'tanker\b'
        ]
    }
    
    # Parameter identification patterns
    PARAMETER_PATTERNS = {
        ParameterType.FORCE_X: [
            r'fx\b', r'force.*x\b', r'x.*force\b'
        ],
        ParameterType.FORCE_Y: [
            r'fy\b', r'force.*y\b', r'y.*force\b'
        ],
        ParameterType.FORCE_Z: [
            r'fz\b', r'force.*z\b', r'z.*force\b'
        ],
        ParameterType.MOMENT_X: [
            r'mx\b', r'moment.*x\b', r'x.*moment\b'
        ],
        ParameterType.MOMENT_Y: [
            r'my\b', r'moment.*y\b', r'y.*moment\b'
        ],
        ParameterType.MOMENT_Z: [
            r'mz\b', r'moment.*z\b', r'z.*moment\b'
        ],
        ParameterType.DISPLACEMENT_X: [
            r'\bx\b', r'disp.*x\b', r'x.*disp\b', r'pos.*x\b'
        ],
        ParameterType.DISPLACEMENT_Y: [
            r'\by\b', r'disp.*y\b', r'y.*disp\b', r'pos.*y\b'
        ],
        ParameterType.DISPLACEMENT_Z: [
            r'\bz\b', r'disp.*z\b', r'z.*disp\b', r'pos.*z\b'
        ],
        ParameterType.ROTATION_X: [
            r'rx\b', r'rot.*x\b', r'x.*rot\b'
        ],
        ParameterType.ROTATION_Y: [
            r'ry\b', r'rot.*y\b', r'y.*rot\b'
        ],
        ParameterType.ROTATION_Z: [
            r'rz\b', r'rot.*z\b', r'z.*rot\b'
        ],
        ParameterType.VELOCITY: [
            r'vel\b', r'velocity\b', r'speed\b'
        ],
        ParameterType.ACCELERATION: [
            r'acc\b', r'accel\b', r'acceleration\b'
        ],
        ParameterType.TENSION: [
            r'tension\b', r'tens\b', r'eff.*tens\b'
        ],
        ParameterType.CURVATURE: [
            r'curv\b', r'curvature\b', r'bend\b'
        ]
    }
    
    # Units that help identify parameter types
    UNIT_PARAMETER_MAPPING = {
        'kN': [ParameterType.FORCE_X, ParameterType.FORCE_Y, ParameterType.FORCE_Z, ParameterType.TENSION],
        'kNm': [ParameterType.MOMENT_X, ParameterType.MOMENT_Y, ParameterType.MOMENT_Z],
        'kN.m': [ParameterType.MOMENT_X, ParameterType.MOMENT_Y, ParameterType.MOMENT_Z],
        'm': [ParameterType.DISPLACEMENT_X, ParameterType.DISPLACEMENT_Y, ParameterType.DISPLACEMENT_Z],
        'deg': [ParameterType.ROTATION_X, ParameterType.ROTATION_Y, ParameterType.ROTATION_Z],
        'm/s': [ParameterType.VELOCITY],
        'm/s2': [ParameterType.ACCELERATION],
        'm/s²': [ParameterType.ACCELERATION]
    }
    
    def __init__(self):
        """Initialize component classifier"""
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
        self._compile_patterns()
    
    def _compile_patterns(self):
        """Compile regex patterns for performance"""
        self.compiled_component_patterns = {}
        for comp_type, patterns in self.COMPONENT_PATTERNS.items():
            self.compiled_component_patterns[comp_type] = [
                re.compile(pattern, re.IGNORECASE) for pattern in patterns
            ]
        
        self.compiled_parameter_patterns = {}
        for param_type, patterns in self.PARAMETER_PATTERNS.items():
            self.compiled_parameter_patterns[param_type] = [
                re.compile(pattern, re.IGNORECASE) for pattern in patterns
            ]
    
    def classify_column(self, column_name: str, 
                       data_sample: Optional[pd.Series] = None) -> Tuple[ComponentType, ParameterType, float]:
        """
        Classify a single column.
        
        Args:
            column_name: Name of the column
            data_sample: Optional data sample for analysis
            
        Returns:
            Tuple of (component_type, parameter_type, confidence_score)
        """
        # Normalize column name
        normalized_name = column_name.lower().strip()
        
        # Classify component
        component_type, comp_confidence = self._classify_component(normalized_name)
        
        # Classify parameter
        parameter_type, param_confidence = self._classify_parameter(normalized_name, data_sample)
        
        # Overall confidence is minimum of both classifications
        overall_confidence = min(comp_confidence, param_confidence)
        
        self.logger.debug(f"Classified '{column_name}': {component_type.value}/{parameter_type.value} "
                         f"(confidence: {overall_confidence:.2f})")
        
        return component_type, parameter_type, overall_confidence
    
    def _classify_component(self, column_name: str) -> Tuple[ComponentType, float]:
        """Classify component type from column name"""
        best_match = ComponentType.UNKNOWN
        best_confidence = 0.0
        
        for comp_type, patterns in self.compiled_component_patterns.items():
            for pattern in patterns:
                match = pattern.search(column_name)
                if match:
                    # Calculate confidence based on match quality
                    match_length = len(match.group(0))
                    confidence = min(0.9, match_length / len(column_name) + 0.3)
                    
                    if confidence > best_confidence:
                        best_match = comp_type
                        best_confidence = confidence
        
        # If no specific component found, check for generic identifiers
        if best_match == ComponentType.UNKNOWN:
            if any(word in column_name for word in ['float', 'hull', 'platform']):
                # Default to FST1 for generic floater terms
                return ComponentType.FST1, 0.3
        
        return best_match, best_confidence
    
    def _classify_parameter(self, column_name: str, 
                           data_sample: Optional[pd.Series] = None) -> Tuple[ParameterType, float]:
        """Classify parameter type from column name and data"""
        best_match = ParameterType.OTHER
        best_confidence = 0.0
        
        # Pattern-based classification
        for param_type, patterns in self.compiled_parameter_patterns.items():
            for pattern in patterns:
                match = pattern.search(column_name)
                if match:
                    match_length = len(match.group(0))
                    confidence = min(0.9, match_length / len(column_name) + 0.4)
                    
                    if confidence > best_confidence:
                        best_match = param_type
                        best_confidence = confidence
        
        # Unit-based classification enhancement
        unit_confidence = self._classify_by_unit(column_name)
        if unit_confidence[1] > best_confidence:
            best_match, best_confidence = unit_confidence
        
        # Data-based classification (if sample provided)
        if data_sample is not None and len(data_sample) > 0:
            data_confidence = self._classify_by_data_pattern(data_sample)
            if data_confidence[1] > best_confidence * 0.8:  # Slight preference for pattern matching
                best_match = data_confidence[0]
                best_confidence = max(best_confidence, data_confidence[1] * 0.9)
        
        return best_match, best_confidence
    
    def _classify_by_unit(self, column_name: str) -> Tuple[ParameterType, float]:
        """Classify parameter type based on units in column name"""
        for unit, param_types in self.UNIT_PARAMETER_MAPPING.items():
            if unit in column_name:
                # Return first matching parameter type with high confidence
                return param_types[0], 0.8
        
        return ParameterType.OTHER, 0.0
    
    def _classify_by_data_pattern(self, data_sample: pd.Series) -> Tuple[ParameterType, float]:
        """Classify parameter type based on data patterns"""
        try:
            numeric_data = pd.to_numeric(data_sample, errors='coerce').dropna()
            if len(numeric_data) == 0:
                return ParameterType.OTHER, 0.0
            
            data_range = numeric_data.max() - numeric_data.min()
            data_mean = numeric_data.mean()
            data_std = numeric_data.std()
            
            # Heuristics based on typical offshore engineering values
            if 0 <= abs(data_mean) <= 360 and data_range <= 360:
                # Likely rotation/heading in degrees
                return ParameterType.ROTATION_Z, 0.6
            
            elif data_range > 1000 and abs(data_mean) > 100:
                # Large values likely forces
                return ParameterType.FORCE_Z, 0.5
            
            elif abs(data_mean) < 10 and data_std < 5:
                # Small values likely displacements
                return ParameterType.DISPLACEMENT_Z, 0.5
            
        except Exception as e:
            self.logger.debug(f"Data pattern analysis failed: {e}")
        
        return ParameterType.OTHER, 0.0
    
    def classify_file(self, parse_result: Dict) -> List[ComponentInfo]:
        """
        Classify all components in a parsed file.
        
        Args:
            parse_result: Result from CSVParser.parse_file()
            
        Returns:
            List of ComponentInfo objects
        """
        df = parse_result['dataframe']
        column_metadata = parse_result.get('column_metadata', {})
        
        # Classify each column
        column_classifications = {}
        for col in df.columns:
            data_sample = df[col].head(100) if len(df) > 0 else None
            comp_type, param_type, confidence = self.classify_column(col, data_sample)
            
            column_classifications[col] = {
                'component_type': comp_type,
                'parameter_type': param_type,
                'confidence': confidence,
                'metadata': column_metadata.get(col)
            }
        
        # Group columns by component
        component_groups = {}
        for col, classification in column_classifications.items():
            comp_type = classification['component_type']
            
            if comp_type not in component_groups:
                component_groups[comp_type] = {
                    'columns': [],
                    'parameters': {},
                    'confidences': []
                }
            
            component_groups[comp_type]['columns'].append(col)
            component_groups[comp_type]['parameters'][col] = classification['parameter_type']
            component_groups[comp_type]['confidences'].append(classification['confidence'])
        
        # Create ComponentInfo objects
        components = []
        for comp_type, group_data in component_groups.items():
            if comp_type == ComponentType.UNKNOWN and len(group_data['columns']) < 3:
                # Skip small unknown groups
                continue
            
            avg_confidence = np.mean(group_data['confidences']) if group_data['confidences'] else 0.0
            
            # Generate component name
            component_name = comp_type.value
            if comp_type == ComponentType.UNKNOWN:
                component_name = f"unknown_component_{len(components) + 1}"
            
            component_info = ComponentInfo(
                name=component_name,
                component_type=comp_type,
                columns=group_data['columns'],
                parameters=group_data['parameters'],
                confidence_score=avg_confidence,
                metadata={
                    'column_count': len(group_data['columns']),
                    'parameter_types': list(set(group_data['parameters'].values()))
                }
            )
            
            components.append(component_info)
        
        # Sort by confidence (highest first)
        components.sort(key=lambda c: c.confidence_score, reverse=True)
        
        self.logger.info(f"Classified {len(components)} components from {len(df.columns)} columns")
        
        return components
    
    def get_component_summary(self, components: List[ComponentInfo]) -> Dict:
        """
        Generate summary statistics for classified components.
        
        Args:
            components: List of ComponentInfo objects
            
        Returns:
            Dictionary with classification summary
        """
        if not components:
            return {'total_components': 0, 'classification_rate': 0.0}
        
        # Count by component type
        type_counts = {}
        total_columns = 0
        total_confidence = 0.0
        
        for component in components:
            comp_type = component.component_type.value
            type_counts[comp_type] = type_counts.get(comp_type, 0) + 1
            total_columns += len(component.columns)
            total_confidence += component.confidence_score
        
        # Parameter type distribution
        param_counts = {}
        for component in components:
            for param_type in component.parameters.values():
                param_name = param_type.value
                param_counts[param_name] = param_counts.get(param_name, 0) + 1
        
        return {
            'total_components': len(components),
            'total_columns': total_columns,
            'average_confidence': total_confidence / len(components),
            'component_types': type_counts,
            'parameter_types': param_counts,
            'highest_confidence': max(c.confidence_score for c in components),
            'lowest_confidence': min(c.confidence_score for c in components)
        }
    
    def validate_classification(self, components: List[ComponentInfo], 
                              expected_components: Optional[Set[str]] = None) -> Dict:
        """
        Validate classification results against expected components.
        
        Args:
            components: List of classified components
            expected_components: Set of expected component names
            
        Returns:
            Dictionary with validation results
        """
        validation_result = {
            'is_valid': True,
            'issues': [],
            'coverage': 0.0,
            'missing_components': [],
            'unexpected_components': []
        }
        
        if expected_components:
            found_components = {c.name for c in components}
            
            missing = expected_components - found_components
            unexpected = found_components - expected_components
            
            if missing:
                validation_result['missing_components'] = list(missing)
                validation_result['issues'].append(f"Missing expected components: {missing}")
                validation_result['is_valid'] = False
            
            if unexpected:
                validation_result['unexpected_components'] = list(unexpected)
                validation_result['issues'].append(f"Unexpected components found: {unexpected}")
            
            validation_result['coverage'] = len(found_components & expected_components) / len(expected_components)
        
        # Check for low confidence components
        low_confidence_components = [c for c in components if c.confidence_score < 0.5]
        if low_confidence_components:
            validation_result['issues'].append(
                f"Low confidence components: {[c.name for c in low_confidence_components]}")
        
        # Check for components with very few columns
        sparse_components = [c for c in components if len(c.columns) < 2]
        if sparse_components:
            validation_result['issues'].append(
                f"Components with few columns: {[c.name for c in sparse_components]}")
        
        return validation_result


# Utility functions
def quick_classify(file_path: str) -> List[ComponentInfo]:
    """Quick classification with default settings"""
    from .csv_parser import CSVParser
    
    parser = CSVParser()
    parse_result = parser.parse_file(Path(file_path))
    
    classifier = ComponentClassifier()
    return classifier.classify_file(parse_result)


def get_component_hierarchy(components: List[ComponentInfo]) -> Dict:
    """
    Generate component hierarchy for visualization.
    
    Args:
        components: List of ComponentInfo objects
        
    Returns:
        Hierarchical structure of components and parameters
    """
    hierarchy = {}
    
    for component in components:
        comp_name = component.name
        hierarchy[comp_name] = {
            'type': component.component_type.value,
            'confidence': component.confidence_score,
            'parameters': {}
        }
        
        # Group parameters by type
        param_groups = {}
        for col, param_type in component.parameters.items():
            param_name = param_type.value
            if param_name not in param_groups:
                param_groups[param_name] = []
            param_groups[param_name].append(col)
        
        hierarchy[comp_name]['parameters'] = param_groups
    
    return hierarchy