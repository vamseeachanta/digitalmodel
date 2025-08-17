#!/usr/bin/env python
"""
File Type Detection for OrcaFlex and DigitalModel Files
========================================================
This module provides utilities to distinguish between:
1. OrcaFlex model files (.yml, .dat, .sim)
2. DigitalModel input/config files (.yml)
3. Includefiles for OrcaFlex
4. Target tension CSV files
5. Batch configuration files
"""

import yaml
import json
from pathlib import Path
from typing import Dict, Any, Optional, Tuple
from enum import Enum
import logging

logger = logging.getLogger(__name__)


class FileType(Enum):
    """Enumeration of file types in the OrcaFlex/DigitalModel ecosystem"""
    ORCAFLEX_MODEL_YML = "orcaflex_model_yml"      # OrcaFlex model in YAML format
    ORCAFLEX_MODEL_DAT = "orcaflex_model_dat"      # OrcaFlex model in DAT format
    ORCAFLEX_SIMULATION = "orcaflex_simulation"     # OrcaFlex .sim file
    ORCAFLEX_INCLUDEFILE = "orcaflex_includefile"   # OrcaFlex includefile
    DIGITALMODEL_CONFIG = "digitalmodel_config"      # DigitalModel configuration
    DIGITALMODEL_BATCH = "digitalmodel_batch"        # Batch processing configuration
    TARGET_TENSIONS_CSV = "target_tensions_csv"      # Target tensions CSV
    UNKNOWN = "unknown"                              # Unknown file type


class FileTypeDetector:
    """Detector for distinguishing between different file types"""
    
    # Key identifiers for OrcaFlex model files
    ORCAFLEX_MODEL_KEYS = {
        'General', 'Environment', 'BaseFile', 'Lines', 'Vessels', 
        'LineTypes', 'WaveTrains', 'WindLoads', 'CurrentLoads',
        'SolidFrictionCoefficients', 'RayleighDampingCoefficients',
        'UnstretchedLength'  # Can appear in both models and includefiles
    }
    
    # Key identifiers for OrcaFlex includefiles
    ORCAFLEX_INCLUDE_KEYS = {
        'UnstretchedLength', 'Length', 'TargetSegmentLength',
        'NumberOfSections', 'LineType', 'AttachmentType',
        'Connection', 'EndAConnection', 'EndBConnection'
    }
    
    # Key identifiers for DigitalModel configuration files
    DIGITALMODEL_CONFIG_KEYS = {
        'meta', 'default', 'orcaflex', 'file_management',
        'post_process', 'summary', 'cfg_environment',
        'library', 'version', 'digitalmodel'
    }
    
    # Key identifiers for batch configuration files
    BATCH_CONFIG_KEYS = {
        'batch_info', 'simulation_settings', 'output_settings',
        'models', 'mooring_parameters', 'processing_options'
    }
    
    @classmethod
    def detect_file_type(cls, file_path: Path) -> Tuple[FileType, Dict[str, Any]]:
        """
        Detect the type of a file based on extension and content.
        
        Args:
            file_path: Path to the file to analyze
            
        Returns:
            Tuple of (FileType, metadata dict with details)
        """
        if not file_path.exists():
            return FileType.UNKNOWN, {"error": "File does not exist"}
        
        # Check extension first
        extension = file_path.suffix.lower()
        
        if extension == '.dat':
            return cls._analyze_dat_file(file_path)
        elif extension == '.sim':
            return FileType.ORCAFLEX_SIMULATION, {
                "description": "OrcaFlex simulation results file",
                "can_load_in_orcaflex": True
            }
        elif extension == '.csv':
            return cls._analyze_csv_file(file_path)
        elif extension in ['.yml', '.yaml']:
            return cls._analyze_yaml_file(file_path)
        else:
            return FileType.UNKNOWN, {"extension": extension}
    
    @classmethod
    def _analyze_yaml_file(cls, file_path: Path) -> Tuple[FileType, Dict[str, Any]]:
        """Analyze a YAML file to determine its specific type."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = yaml.safe_load(f)
            
            if not isinstance(content, dict):
                return FileType.UNKNOWN, {"error": "YAML file does not contain a dictionary"}
            
            top_level_keys = set(content.keys())
            metadata = {
                "top_level_keys": list(top_level_keys)[:10],  # First 10 keys
                "total_keys": len(top_level_keys)
            }
            
            # Check for batch configuration
            if cls._is_batch_config(top_level_keys):
                metadata["description"] = "Batch processing configuration for multiple OrcaFlex models"
                metadata["model_count"] = len(content.get('models', []))
                metadata["batch_name"] = content.get('batch_info', {}).get('name', 'Unknown')
                return FileType.DIGITALMODEL_BATCH, metadata
            
            # Check for DigitalModel configuration
            if cls._is_digitalmodel_config(top_level_keys, content):
                metadata["description"] = "DigitalModel analysis configuration file"
                metadata["library"] = content.get('meta', {}).get('library', 'Unknown')
                metadata["version"] = content.get('meta', {}).get('version', 'Unknown')
                return FileType.DIGITALMODEL_CONFIG, metadata
            
            # Check for OrcaFlex includefile
            if cls._is_orcaflex_includefile(top_level_keys, content):
                metadata["description"] = "OrcaFlex includefile for modifying model parameters"
                metadata["modifies_lengths"] = 'UnstretchedLength' in content
                metadata["line_count"] = len(content.get('UnstretchedLength', {}))
                return FileType.ORCAFLEX_INCLUDEFILE, metadata
            
            # Check for OrcaFlex model
            if cls._is_orcaflex_model(top_level_keys, content):
                metadata["description"] = "OrcaFlex model file in YAML format"
                metadata["has_environment"] = 'Environment' in content
                metadata["has_vessels"] = 'Vessels' in content
                metadata["has_lines"] = 'Lines' in content
                metadata["model_version"] = content.get('General', {}).get('ModelVersion', 'Unknown')
                return FileType.ORCAFLEX_MODEL_YML, metadata
            
            # If we can't determine, provide more info
            metadata["description"] = "Unknown YAML file type"
            return FileType.UNKNOWN, metadata
            
        except yaml.YAMLError as e:
            return FileType.UNKNOWN, {"error": f"Invalid YAML: {str(e)}"}
        except Exception as e:
            return FileType.UNKNOWN, {"error": f"Error reading file: {str(e)}"}
    
    @classmethod
    def _analyze_dat_file(cls, file_path: Path) -> Tuple[FileType, Dict[str, Any]]:
        """Analyze a .dat file (OrcaFlex native format)."""
        try:
            # Read first few lines to check if it's an OrcaFlex file
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                first_lines = [f.readline() for _ in range(10)]
            
            # Check for OrcaFlex signatures
            is_orcaflex = any(
                'OrcaFlex' in line or 'Orcina' in line 
                for line in first_lines
            )
            
            if is_orcaflex:
                return FileType.ORCAFLEX_MODEL_DAT, {
                    "description": "OrcaFlex model file in DAT format",
                    "can_load_in_orcaflex": True
                }
            else:
                return FileType.UNKNOWN, {
                    "description": "DAT file (unknown format)"
                }
                
        except Exception as e:
            return FileType.UNKNOWN, {"error": f"Error reading DAT file: {str(e)}"}
    
    @classmethod
    def _analyze_csv_file(cls, file_path: Path) -> Tuple[FileType, Dict[str, Any]]:
        """Analyze a CSV file to determine if it's a target tensions file."""
        try:
            import pandas as pd
            df = pd.read_csv(file_path, nrows=5)
            
            # Check for target tensions columns
            expected_columns = {'line_name', 'target_tension', 'section_to_be_modified'}
            if expected_columns.issubset(set(df.columns)):
                return FileType.TARGET_TENSIONS_CSV, {
                    "description": "Target mooring tensions CSV file",
                    "line_count": len(df),
                    "columns": list(df.columns)
                }
            else:
                return FileType.UNKNOWN, {
                    "description": "CSV file (unknown format)",
                    "columns": list(df.columns)
                }
                
        except Exception as e:
            return FileType.UNKNOWN, {"error": f"Error reading CSV: {str(e)}"}
    
    @classmethod
    def _is_batch_config(cls, keys: set) -> bool:
        """Check if keys indicate a batch configuration file."""
        required_keys = {'batch_info', 'models'}
        return required_keys.issubset(keys)
    
    @classmethod
    def _is_digitalmodel_config(cls, keys: set, content: dict) -> bool:
        """Check if keys indicate a DigitalModel configuration."""
        # Check for meta section with library field
        if 'meta' in content:
            meta = content.get('meta', {})
            if isinstance(meta, dict) and meta.get('library') == 'digitalmodel':
                return True
        
        # Check for characteristic DigitalModel keys
        digitalmodel_indicators = {'cfg', 'post_process', 'file_management'}
        return bool(keys & digitalmodel_indicators)
    
    @classmethod
    def _is_orcaflex_includefile(cls, keys: set, content: dict) -> bool:
        """Check if this is an OrcaFlex includefile."""
        # Includefiles typically have very few top-level keys
        # and usually contain UnstretchedLength or similar modification keys
        if len(keys) <= 5:  # Includefiles are usually simple
            if 'UnstretchedLength' in keys:
                # Check structure of UnstretchedLength
                ul = content.get('UnstretchedLength', {})
                if isinstance(ul, dict) and ul:
                    # Check if it has line names as keys
                    first_value = next(iter(ul.values()))
                    if isinstance(first_value, dict):
                        # Check for Length[n] pattern
                        return any('Length[' in k for k in first_value.keys())
        return False
    
    @classmethod
    def _is_orcaflex_model(cls, keys: set, content: dict) -> bool:
        """Check if keys indicate an OrcaFlex model file."""
        # OrcaFlex models typically have General and/or Environment sections
        if 'General' in keys or 'Environment' in keys:
            return True
        
        # Check for multiple OrcaFlex-specific keys
        orcaflex_keys = keys & cls.ORCAFLEX_MODEL_KEYS
        return len(orcaflex_keys) >= 3


class FileClassifier:
    """High-level classifier for batch processing"""
    
    def __init__(self):
        self.detector = FileTypeDetector()
    
    def classify_directory(self, directory: Path) -> Dict[str, list]:
        """
        Classify all files in a directory by type.
        
        Returns:
            Dictionary mapping file types to lists of file paths
        """
        classification = {
            'orcaflex_models': [],
            'includefiles': [],
            'digitalmodel_configs': [],
            'batch_configs': [],
            'target_tensions': [],
            'simulations': [],
            'unknown': []
        }
        
        for file_path in directory.iterdir():
            if file_path.is_file():
                file_type, metadata = self.detector.detect_file_type(file_path)
                
                if file_type in [FileType.ORCAFLEX_MODEL_YML, FileType.ORCAFLEX_MODEL_DAT]:
                    classification['orcaflex_models'].append(str(file_path))
                elif file_type == FileType.ORCAFLEX_INCLUDEFILE:
                    classification['includefiles'].append(str(file_path))
                elif file_type == FileType.DIGITALMODEL_CONFIG:
                    classification['digitalmodel_configs'].append(str(file_path))
                elif file_type == FileType.DIGITALMODEL_BATCH:
                    classification['batch_configs'].append(str(file_path))
                elif file_type == FileType.TARGET_TENSIONS_CSV:
                    classification['target_tensions'].append(str(file_path))
                elif file_type == FileType.ORCAFLEX_SIMULATION:
                    classification['simulations'].append(str(file_path))
                else:
                    classification['unknown'].append(str(file_path))
        
        return classification
    
    def validate_batch_config(self, config_path: Path) -> Tuple[bool, list]:
        """
        Validate a batch configuration file and its referenced files.
        
        Returns:
            Tuple of (is_valid, list of issues)
        """
        issues = []
        
        file_type, metadata = self.detector.detect_file_type(config_path)
        
        if file_type != FileType.DIGITALMODEL_BATCH:
            issues.append(f"File is not a batch configuration: {file_type.value}")
            return False, issues
        
        # Load the configuration
        try:
            with open(config_path, 'r') as f:
                config = yaml.safe_load(f)
            
            base_dir = Path(config['batch_info']['base_directory'])
            
            # Check each model
            for model_config in config.get('models', []):
                model_file = base_dir / model_config['model_file']
                
                # Check model file type
                model_type, model_meta = self.detector.detect_file_type(model_file)
                if model_type not in [FileType.ORCAFLEX_MODEL_YML, FileType.ORCAFLEX_MODEL_DAT]:
                    issues.append(f"Model file '{model_file.name}' is not an OrcaFlex model: {model_type.value}")
                
                # Check includefile if specified
                if 'includefile' in model_config:
                    include_file = base_dir / model_config['includefile']
                    if include_file.exists():
                        inc_type, inc_meta = self.detector.detect_file_type(include_file)
                        if inc_type != FileType.ORCAFLEX_INCLUDEFILE:
                            issues.append(f"Includefile '{include_file.name}' is not an OrcaFlex includefile: {inc_type.value}")
                
                # Check target tensions if specified
                if 'target_tensions' in model_config:
                    tensions_file = base_dir / model_config['target_tensions']
                    if tensions_file.exists():
                        tens_type, tens_meta = self.detector.detect_file_type(tensions_file)
                        if tens_type != FileType.TARGET_TENSIONS_CSV:
                            issues.append(f"Target tensions file '{tensions_file.name}' is not a valid CSV: {tens_type.value}")
            
        except Exception as e:
            issues.append(f"Error validating configuration: {str(e)}")
        
        return len(issues) == 0, issues


def main():
    """Demonstration of file type detection"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Detect OrcaFlex and DigitalModel file types')
    parser.add_argument('path', help='File or directory path to analyze')
    parser.add_argument('--validate', action='store_true', help='Validate batch configuration')
    
    args = parser.parse_args()
    
    path = Path(args.path)
    
    if path.is_file():
        detector = FileTypeDetector()
        file_type, metadata = detector.detect_file_type(path)
        
        print(f"File: {path.name}")
        print(f"Type: {file_type.value}")
        print("Metadata:")
        for key, value in metadata.items():
            print(f"  {key}: {value}")
        
        if args.validate and file_type == FileType.DIGITALMODEL_BATCH:
            classifier = FileClassifier()
            is_valid, issues = classifier.validate_batch_config(path)
            print(f"\nValidation: {'PASSED' if is_valid else 'FAILED'}")
            if issues:
                print("Issues:")
                for issue in issues:
                    print(f"  - {issue}")
    
    elif path.is_dir():
        classifier = FileClassifier()
        classification = classifier.classify_directory(path)
        
        print(f"Directory: {path}")
        print("\nFile Classification:")
        for category, files in classification.items():
            if files:
                print(f"\n{category.replace('_', ' ').title()} ({len(files)}):")
                for file_path in files[:5]:  # Show first 5
                    print(f"  - {Path(file_path).name}")
                if len(files) > 5:
                    print(f"  ... and {len(files) - 5} more")


if __name__ == "__main__":
    main()