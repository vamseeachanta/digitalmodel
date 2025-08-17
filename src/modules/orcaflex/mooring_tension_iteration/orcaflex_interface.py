"""
OrcaFlex Interface Module for Mooring Tension Iteration
Interfaces with OrcaFlex software to run analyses and extract results
"""

import subprocess
import yaml
import json
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import logging
import time
from dataclasses import dataclass
import pandas as pd

# Set up logging
logger = logging.getLogger(__name__)


@dataclass
class OrcaFlexResult:
    """Container for OrcaFlex analysis results"""
    line_tensions: Dict[str, float]  # Line name -> tension in kN
    line_lengths: Dict[str, float]   # Line name -> unstretched length in m
    fender_forces: Dict[str, float]  # Fender name -> force in kN
    convergence_status: str
    analysis_time: float  # seconds
    warnings: List[str]


class OrcaFlexInterface:
    """Interface to OrcaFlex software for mooring analysis"""
    
    def __init__(self, base_path: Path, digitalmodel_path: Optional[Path] = None):
        """
        Initialize OrcaFlex interface
        
        Args:
            base_path: Base directory containing OrcaFlex files
            digitalmodel_path: Path to digitalmodel executable (if not in PATH)
        """
        self.base_path = Path(base_path)
        self.digitalmodel_path = digitalmodel_path or "digitalmodel"
        
        # Verify paths exist
        if not self.base_path.exists():
            raise FileNotFoundError(f"Base path does not exist: {self.base_path}")
        
        # Set up paths for different file types
        self.model_path = self.base_path
        self.pretension_path = self.base_path / "fsts_lngc_pretension"
        self.output_path = self.base_path / "output"
        self.includefile_path = self.base_path / "includefiles"
        
        # Create output directories if needed
        self.output_path.mkdir(parents=True, exist_ok=True)
        self.includefile_path.mkdir(parents=True, exist_ok=True)
        
        logger.info(f"OrcaFlex interface initialized with base path: {self.base_path}")
    
    def run_static_analysis(self, model_file: str, includefile: Optional[Path] = None) -> bool:
        """
        Run OrcaFlex static analysis using digitalmodel
        
        Args:
            model_file: Name of the model file (e.g., 'fsts_lngc_vessel_statics_6dof.yml')
            includefile: Optional includefile to apply before analysis
        
        Returns:
            Success status
        """
        model_path = self.model_path / model_file
        
        if not model_path.exists():
            logger.error(f"Model file not found: {model_path}")
            return False
        
        # Build command
        cmd = [str(self.digitalmodel_path), str(model_path)]
        
        # Add includefile if provided
        if includefile and includefile.exists():
            cmd.extend(["--include", str(includefile)])
        
        logger.info(f"Running OrcaFlex analysis: {' '.join(cmd)}")
        
        try:
            # Run the analysis
            start_time = time.time()
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=str(self.base_path),
                timeout=600  # 10 minute timeout
            )
            
            elapsed = time.time() - start_time
            
            if result.returncode == 0:
                logger.info(f"Analysis completed successfully in {elapsed:.1f}s")
                return True
            else:
                logger.error(f"Analysis failed: {result.stderr}")
                return False
                
        except subprocess.TimeoutExpired:
            logger.error("Analysis timed out after 10 minutes")
            return False
        except Exception as e:
            logger.error(f"Error running analysis: {e}")
            return False
    
    def extract_results(self, extraction_config: str = "dm_ofx_post_fsts_lngc.yml") -> OrcaFlexResult:
        """
        Extract results from OrcaFlex analysis using post-processing config
        
        Args:
            extraction_config: YAML config file for result extraction
        
        Returns:
            OrcaFlexResult object with extracted data
        """
        config_path = self.pretension_path / extraction_config
        
        if not config_path.exists():
            logger.error(f"Extraction config not found: {config_path}")
            return None
        
        # Run extraction
        cmd = [str(self.digitalmodel_path), str(config_path)]
        
        logger.info(f"Extracting results: {' '.join(cmd)}")
        
        try:
            start_time = time.time()
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=str(self.base_path),
                timeout=120  # 2 minute timeout
            )
            
            elapsed = time.time() - start_time
            
            if result.returncode != 0:
                logger.error(f"Extraction failed: {result.stderr}")
                return None
            
            # Parse the output to get tensions and lengths
            # This assumes the extraction writes to a known output file
            output_file = self.output_path / "mooring_tensions.csv"
            
            if output_file.exists():
                return self._parse_extraction_output(output_file, elapsed)
            else:
                logger.error("Extraction output file not found")
                return None
                
        except Exception as e:
            logger.error(f"Error extracting results: {e}")
            return None
    
    def _parse_extraction_output(self, output_file: Path, analysis_time: float) -> OrcaFlexResult:
        """
        Parse the CSV output from extraction
        
        Args:
            output_file: Path to extraction output CSV
            analysis_time: Time taken for analysis
        
        Returns:
            OrcaFlexResult object
        """
        try:
            # Read CSV output
            df = pd.read_csv(output_file)
            
            line_tensions = {}
            line_lengths = {}
            fender_forces = {}
            warnings = []
            
            # Parse each row based on object type
            for _, row in df.iterrows():
                obj_name = row['ObjectName']
                
                if 'Line' in obj_name:
                    # Mooring line data
                    if 'Tension' in df.columns:
                        line_tensions[obj_name] = float(row['Tension'])
                    if 'UnstretchedLength' in df.columns:
                        line_lengths[obj_name] = float(row['UnstretchedLength'])
                        
                elif 'Fender' in obj_name:
                    # Fender data
                    if 'Force' in df.columns:
                        fender_forces[obj_name] = float(row['Force'])
            
            # Check for any warnings in the output
            if 'Warning' in df.columns:
                warnings = df['Warning'].dropna().tolist()
            
            return OrcaFlexResult(
                line_tensions=line_tensions,
                line_lengths=line_lengths,
                fender_forces=fender_forces,
                convergence_status="extracted",
                analysis_time=analysis_time,
                warnings=warnings
            )
            
        except Exception as e:
            logger.error(f"Error parsing extraction output: {e}")
            return None
    
    def apply_includefile(self, includefile: Path, model_file: str) -> bool:
        """
        Apply an includefile to update the OrcaFlex model
        
        Args:
            includefile: Path to includefile with line length updates
            model_file: Model file to update
        
        Returns:
            Success status
        """
        if not includefile.exists():
            logger.error(f"Includefile not found: {includefile}")
            return False
        
        model_path = self.model_path / model_file
        
        # Use digitalmodel to apply the includefile
        cmd = [
            str(self.digitalmodel_path),
            str(model_path),
            "--apply-include",
            str(includefile)
        ]
        
        logger.info(f"Applying includefile: {includefile.name}")
        
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=str(self.base_path),
                timeout=60
            )
            
            if result.returncode == 0:
                logger.info("Includefile applied successfully")
                return True
            else:
                logger.error(f"Failed to apply includefile: {result.stderr}")
                return False
                
        except Exception as e:
            logger.error(f"Error applying includefile: {e}")
            return False
    
    def run_iteration_analysis(
        self,
        model_file: str,
        includefile: Path,
        extraction_config: str = "dm_ofx_post_fsts_lngc.yml"
    ) -> Optional[OrcaFlexResult]:
        """
        Run a complete iteration: apply includefile, run analysis, extract results
        
        Args:
            model_file: OrcaFlex model file
            includefile: Includefile with line length updates
            extraction_config: Extraction configuration file
        
        Returns:
            OrcaFlexResult or None if failed
        """
        logger.info(f"Running iteration with includefile: {includefile.name}")
        
        # Run static analysis with includefile
        if not self.run_static_analysis(model_file, includefile):
            logger.error("Static analysis failed")
            return None
        
        # Extract results
        results = self.extract_results(extraction_config)
        
        if results:
            logger.info(f"Iteration completed: {len(results.line_tensions)} lines analyzed")
        else:
            logger.error("Failed to extract results")
        
        return results
    
    def get_line_properties_from_model(self, model_file: str) -> Dict[str, Dict]:
        """
        Extract line properties (EA, lengths) directly from OrcaFlex model
        
        Args:
            model_file: OrcaFlex model file
        
        Returns:
            Dictionary of line properties
        """
        model_path = self.model_path / model_file
        
        # Use digitalmodel to extract properties
        cmd = [
            str(self.digitalmodel_path),
            str(model_path),
            "--extract-properties",
            "--output-json"
        ]
        
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=str(self.base_path)
            )
            
            if result.returncode == 0:
                properties = json.loads(result.stdout)
                return properties
            else:
                logger.warning(f"Could not extract properties: {result.stderr}")
                return {}
                
        except Exception as e:
            logger.warning(f"Error extracting properties: {e}")
            return {}


class OrcaFlexBatchRunner:
    """Utility class for running multiple OrcaFlex analyses in batch"""
    
    def __init__(self, interface: OrcaFlexInterface):
        """Initialize with OrcaFlex interface"""
        self.interface = interface
        self.results_history = []
    
    def run_sensitivity_analysis(
        self,
        model_file: str,
        parameter_ranges: Dict[str, List[float]],
        output_dir: Path
    ) -> pd.DataFrame:
        """
        Run sensitivity analysis varying parameters
        
        Args:
            model_file: Base model file
            parameter_ranges: Parameters to vary and their values
            output_dir: Directory for results
        
        Returns:
            DataFrame with sensitivity results
        """
        results = []
        
        for param_name, values in parameter_ranges.items():
            for value in values:
                # Create includefile with parameter value
                includefile = self._create_parameter_includefile(
                    param_name, value, output_dir
                )
                
                # Run analysis
                result = self.interface.run_iteration_analysis(
                    model_file, includefile
                )
                
                if result:
                    results.append({
                        'parameter': param_name,
                        'value': value,
                        'max_tension': max(result.line_tensions.values()),
                        'min_tension': min(result.line_tensions.values()),
                        'analysis_time': result.analysis_time
                    })
        
        return pd.DataFrame(results)
    
    def _create_parameter_includefile(
        self,
        param_name: str,
        value: float,
        output_dir: Path
    ) -> Path:
        """Create includefile for parameter variation"""
        includefile_data = {
            'parameters': {
                param_name: value
            }
        }
        
        includefile_path = output_dir / f"{param_name}_{value:.2f}.yml"
        with open(includefile_path, 'w') as f:
            yaml.dump(includefile_data, f)
        
        return includefile_path


# Example usage
if __name__ == "__main__":
    # Set up logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # Initialize interface
    base_path = Path(r"D:\1522\ctr7\orcaflex\rev_a08\base_files")
    interface = OrcaFlexInterface(base_path)
    
    # Run baseline analysis
    print("Running baseline analysis...")
    success = interface.run_static_analysis("fsts_lngc_vessel_statics_6dof.yml")
    
    if success:
        print("Extracting results...")
        results = interface.extract_results()
        
        if results:
            print(f"Found {len(results.line_tensions)} line tensions")
            print(f"Max tension: {max(results.line_tensions.values()):.1f} kN")
            print(f"Min tension: {min(results.line_tensions.values()):.1f} kN")