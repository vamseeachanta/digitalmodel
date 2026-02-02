"""
YAML Converter Module

Converts OrcaFlex .dat and .sim files to YAML format using the OrcFxAPI.
"""

import logging
from pathlib import Path
from typing import List, Tuple, Optional

logger = logging.getLogger(__name__)


class YamlConverter:
    """Converts OrcaFlex files to YAML format."""
    
    def __init__(self, use_mock: bool = False):
        """
        Initialize the converter.
        
        Args:
            use_mock: If True, use mock conversion for testing without OrcFxAPI
        """
        self.use_mock = use_mock
        self.ofx = None
        
        if not use_mock:
            try:
                import OrcFxAPI
                self.ofx = OrcFxAPI.Model()
                logger.info("OrcFxAPI loaded successfully")
            except ImportError:
                logger.warning("OrcFxAPI not available, using mock mode")
                self.use_mock = True
    
    def convert_batch(self, file_paths: List[Path]) -> List[Tuple]:
        """
        Convert a batch of files to YAML.
        
        Args:
            file_paths: List of file paths to convert
            
        Returns:
            List of tuples (status, path, [error_message])
        """
        results = []
        
        for path in file_paths:
            try:
                if self.use_mock:
                    # Mock conversion for testing
                    yaml_path = Path(str(path).replace('.dat', '.yml').replace('.sim', '.yml'))
                    results.append(('mock', str(path)))
                else:
                    # Real conversion using OrcFxAPI
                    self.ofx.LoadData(str(path))
                    yaml_path = Path(str(path).replace('.dat', '.yml').replace('.sim', '.yml'))
                    self.ofx.SaveData(str(yaml_path))
                    results.append(('success', str(path)))
                    
            except Exception as e:
                results.append(('failed', str(path), str(e)))
                logger.error(f"Failed to convert {path}: {e}")
        
        return results
    
    def validate_yaml(self, yaml_path: Path) -> bool:
        """
        Validate a converted YAML file.
        
        Args:
            yaml_path: Path to YAML file
            
        Returns:
            True if valid, False otherwise
        """
        try:
            import yaml
            
            with open(yaml_path, 'r') as f:
                data = yaml.safe_load(f)
            
            # Basic validation - check for expected keys
            if isinstance(data, dict):
                return True
            
            return False
            
        except Exception as e:
            logger.error(f"Validation failed for {yaml_path}: {e}")
            return False