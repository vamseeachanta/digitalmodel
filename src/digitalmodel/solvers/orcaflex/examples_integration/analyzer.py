"""
Feature Analyzer Module

Analyzes OrcaFlex YAML files to extract model features and characteristics.
"""

import json
import logging
from pathlib import Path
from typing import Dict, List, Any
import yaml

logger = logging.getLogger(__name__)


class FeatureAnalyzer:
    """Analyzes OrcaFlex models to extract features."""
    
    def __init__(self):
        """Initialize the analyzer."""
        self.feature_schema = self._define_feature_schema()
    
    def _define_feature_schema(self) -> Dict:
        """Define the schema for feature extraction."""
        return {
            'metadata': {
                'name': None,
                'category': None,
                'orcaflex_version': None,
                'file_size': None,
                'conversion_date': None
            },
            'components': {
                'vessels': {
                    'count': 0,
                    'types': []
                },
                'lines': {
                    'count': 0,
                    'types': []
                },
                'buoys': {
                    '6d_buoys': 0,
                    '3d_buoys': 0,
                    'types': []
                },
                'constraints': {
                    'types': []
                }
            },
            'analysis': {
                'static_analysis': False,
                'dynamic_analysis': False,
                'modal_analysis': False,
                'fatigue_analysis': False,
                'viv_analysis': False,
                'time_domain_duration': 0.0
            },
            'environment': {
                'waves': {
                    'type': None,
                    'height': 0.0,
                    'period': 0.0
                },
                'current': {
                    'profile': None,
                    'surface_speed': 0.0
                },
                'wind': {
                    'type': None,
                    'speed': 0.0
                }
            }
        }
    
    def analyze_file(self, yaml_path: Path) -> Dict:
        """
        Analyze a single YAML file to extract features.
        
        Args:
            yaml_path: Path to YAML file
            
        Returns:
            Dictionary of extracted features
        """
        features = self._define_feature_schema()
        
        try:
            with open(yaml_path, 'r') as f:
                data = yaml.safe_load(f)
            
            # Extract metadata
            features['metadata']['name'] = yaml_path.stem
            features['metadata']['file_size'] = yaml_path.stat().st_size
            
            # Extract components (simplified for now)
            if isinstance(data, dict):
                # Look for vessels
                if 'Vessels' in data or 'vessels' in data:
                    vessel_data = data.get('Vessels', data.get('vessels', {}))
                    features['components']['vessels']['count'] = len(vessel_data)
                
                # Look for lines
                if 'Lines' in data or 'lines' in data:
                    line_data = data.get('Lines', data.get('lines', {}))
                    features['components']['lines']['count'] = len(line_data)
                
                # Look for buoys
                if 'Buoys' in data or 'buoys' in data:
                    buoy_data = data.get('Buoys', data.get('buoys', {}))
                    # Count different buoy types
                    for buoy_name, buoy_info in buoy_data.items():
                        if isinstance(buoy_info, dict):
                            buoy_type = buoy_info.get('Type', '6DOF')
                            if '6' in str(buoy_type):
                                features['components']['buoys']['6d_buoys'] += 1
                            else:
                                features['components']['buoys']['3d_buoys'] += 1
                
                # Look for environment
                if 'Environment' in data:
                    env_data = data['Environment']
                    if 'Waves' in env_data:
                        wave_data = env_data['Waves']
                        features['environment']['waves']['type'] = wave_data.get('Type', 'Unknown')
                    
                    if 'Current' in env_data:
                        current_data = env_data['Current']
                        features['environment']['current']['profile'] = current_data.get('Profile', 'Unknown')
            
        except Exception as e:
            logger.error(f"Failed to analyze {yaml_path}: {e}")
        
        return features
    
    def analyze_directory(self, directory: Path) -> List[Dict]:
        """
        Analyze all YAML files in a directory.
        
        Args:
            directory: Directory containing YAML files
            
        Returns:
            List of feature dictionaries
        """
        results = []
        yaml_files = list(directory.glob('**/*.yml')) + list(directory.glob('**/*.yaml'))
        
        for yaml_file in yaml_files:
            logger.info(f"Analyzing {yaml_file}")
            features = self.analyze_file(yaml_file)
            results.append(features)
        
        return results
    
    def save_analysis(self, features: Dict, output_path: Path):
        """
        Save analysis results to JSON.
        
        Args:
            features: Feature dictionary
            output_path: Path to save JSON file
        """
        with open(output_path, 'w') as f:
            json.dump(features, f, indent=2)
        
        logger.info(f"Analysis saved to {output_path}")
    
    def generate_summary(self, analyses: List[Dict]) -> Dict:
        """
        Generate a summary of all analyses.
        
        Args:
            analyses: List of analysis dictionaries
            
        Returns:
            Summary dictionary
        """
        summary = {
            'total_examples': len(analyses),
            'components_summary': {
                'total_vessels': sum(a['components']['vessels']['count'] for a in analyses),
                'total_lines': sum(a['components']['lines']['count'] for a in analyses),
                'total_6d_buoys': sum(a['components']['buoys']['6d_buoys'] for a in analyses),
                'total_3d_buoys': sum(a['components']['buoys']['3d_buoys'] for a in analyses)
            },
            'analysis_types': {
                'static': sum(1 for a in analyses if a['analysis']['static_analysis']),
                'dynamic': sum(1 for a in analyses if a['analysis']['dynamic_analysis']),
                'modal': sum(1 for a in analyses if a['analysis']['modal_analysis']),
                'fatigue': sum(1 for a in analyses if a['analysis']['fatigue_analysis'])
            }
        }
        
        return summary