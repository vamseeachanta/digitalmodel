#!/usr/bin/env python3
"""
OrcaWave COM API Wrapper
Provides programmatic access to OrcaWave through COM interface
"""

import asyncio
import time
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path
from enum import Enum
import logging
from dataclasses import dataclass

import win32com.client
import pythoncom
import numpy as np
from tenacity import retry, stop_after_attempt, wait_exponential
import structlog

# Configure structured logging
logger = structlog.get_logger()

class AnalysisType(str, Enum):
    """OrcaWave analysis types"""
    DIFFRACTION = "Diffraction"
    RADIATION = "Radiation"
    BOTH = "DiffractionAndRadiation"

class VesselType(str, Enum):
    """Common vessel types"""
    SHIP = "Ship"
    FPSO = "FPSO"
    SEMI_SUBMERSIBLE = "SemiSubmersible"
    SPAR = "Spar"
    BARGE = "Barge"
    CUSTOM = "Custom"

@dataclass
class MeshStatistics:
    """Mesh quality statistics"""
    panel_count: int
    node_count: int
    waterline_panels: int
    max_aspect_ratio: float
    avg_aspect_ratio: float
    max_skewness: float
    avg_skewness: float
    symmetry_detected: bool
    quality_score: float

@dataclass
class AnalysisProgress:
    """Analysis progress information"""
    percentage: float
    current_frequency: float
    current_direction: float
    elapsed_time: float
    estimated_remaining: float
    is_complete: bool
    has_errors: bool
    error_message: Optional[str] = None

class OrcaWaveAPI:
    """
    OrcaWave COM API wrapper with error handling and async support
    """
    
    def __init__(self):
        """Initialize COM API wrapper"""
        self.app = None
        self.model = None
        self.is_connected = False
        self._com_thread_id = None
        logger.info("orcawave_api_initialized")
    
    @retry(stop=stop_after_attempt(3), wait=wait_exponential(multiplier=1, min=2, max=10))
    def connect(self) -> bool:
        """
        Connect to OrcaWave application via COM
        
        Returns:
            bool: True if connection successful
        """
        try:
            # Initialize COM for this thread
            pythoncom.CoInitialize()
            self._com_thread_id = pythoncom.GetCurrentThreadId()
            
            # Try to connect to existing instance first
            try:
                self.app = win32com.client.GetObject(Class="OrcaWave.Application")
                logger.info("connected_to_existing_orcawave")
            except:
                # Create new instance if none exists
                self.app = win32com.client.Dispatch("OrcaWave.Application")
                logger.info("created_new_orcawave_instance")
            
            # Make application visible
            self.app.Visible = True
            
            # Get or create model
            if self.app.ModelCount > 0:
                self.model = self.app.Model[0]
            else:
                self.model = self.app.CreateModel()
            
            self.is_connected = True
            logger.info("orcawave_connection_successful", 
                       version=self._get_version())
            return True
            
        except Exception as e:
            logger.error("orcawave_connection_failed", error=str(e))
            self.is_connected = False
            raise
    
    def disconnect(self):
        """Disconnect from OrcaWave and cleanup COM"""
        try:
            if self.model:
                self.model = None
            if self.app:
                # Don't close the application, just disconnect
                self.app = None
            
            # Uninitialize COM
            if self._com_thread_id:
                pythoncom.CoUninitialize()
                self._com_thread_id = None
            
            self.is_connected = False
            logger.info("orcawave_disconnected")
            
        except Exception as e:
            logger.error("disconnect_error", error=str(e))
    
    def _get_version(self) -> str:
        """Get OrcaWave version"""
        try:
            return self.app.Version
        except:
            return "Unknown"
    
    def _ensure_connected(self):
        """Ensure COM connection is active"""
        if not self.is_connected or not self.app:
            raise RuntimeError("Not connected to OrcaWave. Call connect() first.")
    
    # ========== Model Operations ==========
    
    def create_vessel(self, name: str, vessel_type: VesselType, 
                     dimensions: Dict[str, float]) -> Dict[str, Any]:
        """
        Create a new vessel with specified parameters
        
        Args:
            name: Vessel name
            vessel_type: Type of vessel
            dimensions: Dict with length, beam, draft
            
        Returns:
            Dict with vessel creation details
        """
        self._ensure_connected()
        
        try:
            # Create new vessel object
            vessel = self.model.CreateObject("Vessel")
            vessel.Name = name
            
            # Set basic dimensions
            vessel.Length = dimensions.get("length", 100.0)
            vessel.Beam = dimensions.get("beam", 20.0)
            vessel.Draft = dimensions.get("draft", 10.0)
            
            # Set vessel type specific parameters
            if vessel_type == VesselType.SHIP:
                vessel.VesselType = "Ship"
            elif vessel_type == VesselType.FPSO:
                vessel.VesselType = "FPSO"
                vessel.Turret = dimensions.get("turret", False)
            
            logger.info("vessel_created", name=name, type=vessel_type.value)
            
            return {
                "success": True,
                "vessel_id": vessel.Handle,
                "name": name,
                "type": vessel_type.value,
                "dimensions": dimensions
            }
            
        except Exception as e:
            logger.error("vessel_creation_failed", error=str(e))
            return {
                "success": False,
                "error": str(e)
            }
    
    def import_geometry(self, file_path: str, file_type: str = "auto",
                       auto_heal: bool = True) -> Dict[str, Any]:
        """
        Import vessel geometry from file
        
        Args:
            file_path: Path to geometry file
            file_type: File type (STL, GDF, etc.) or "auto"
            auto_heal: Automatically heal geometry issues
            
        Returns:
            Import result dictionary
        """
        self._ensure_connected()
        
        try:
            path = Path(file_path)
            if not path.exists():
                raise FileNotFoundError(f"Geometry file not found: {file_path}")
            
            # Determine file type
            if file_type == "auto":
                file_type = path.suffix.upper().replace(".", "")
            
            # Import geometry
            import_result = self.model.ImportGeometry(
                str(path),
                FileType=file_type,
                AutoHeal=auto_heal
            )
            
            # Get mesh statistics
            mesh_stats = self._get_mesh_statistics()
            
            logger.info("geometry_imported", 
                       file=file_path,
                       panels=mesh_stats.panel_count)
            
            return {
                "success": True,
                "file": file_path,
                "panel_count": mesh_stats.panel_count,
                "mesh_stats": mesh_stats,
                "warnings": import_result.Warnings if hasattr(import_result, 'Warnings') else []
            }
            
        except Exception as e:
            logger.error("geometry_import_failed", error=str(e))
            return {
                "success": False,
                "error": str(e)
            }
    
    def set_environment(self, water_depth: float, 
                        water_density: float = 1025.0) -> bool:
        """
        Set environmental parameters
        
        Args:
            water_depth: Water depth in meters
            water_density: Water density in kg/m³
            
        Returns:
            Success status
        """
        self._ensure_connected()
        
        try:
            env = self.model.Environment
            env.WaterDepth = water_depth
            env.WaterDensity = water_density
            
            logger.info("environment_set", 
                       depth=water_depth,
                       density=water_density)
            return True
            
        except Exception as e:
            logger.error("environment_setup_failed", error=str(e))
            return False
    
    def set_frequency_range(self, start: float, end: float, 
                           steps: int) -> List[float]:
        """
        Set analysis frequency range
        
        Args:
            start: Start frequency (rad/s)
            end: End frequency (rad/s)
            steps: Number of frequency steps
            
        Returns:
            List of frequencies
        """
        self._ensure_connected()
        
        try:
            analysis = self.model.Analysis
            analysis.FrequencyStart = start
            analysis.FrequencyEnd = end
            analysis.FrequencySteps = steps
            
            # Calculate actual frequencies
            frequencies = np.linspace(start, end, steps).tolist()
            
            logger.info("frequency_range_set",
                       start=start, end=end, steps=steps)
            
            return frequencies
            
        except Exception as e:
            logger.error("frequency_setup_failed", error=str(e))
            return []
    
    def set_wave_directions(self, directions: List[float]) -> bool:
        """
        Set wave directions for analysis
        
        Args:
            directions: List of wave directions in degrees
            
        Returns:
            Success status
        """
        self._ensure_connected()
        
        try:
            analysis = self.model.Analysis
            analysis.Directions = directions
            
            logger.info("directions_set", count=len(directions))
            return True
            
        except Exception as e:
            logger.error("direction_setup_failed", error=str(e))
            return False
    
    # ========== Mesh Operations ==========
    
    def _get_mesh_statistics(self) -> MeshStatistics:
        """Calculate mesh quality statistics"""
        try:
            mesh = self.model.Mesh
            
            # Get basic counts
            panel_count = mesh.PanelCount
            node_count = mesh.NodeCount
            
            # Calculate quality metrics (simplified)
            # In real implementation, would iterate through panels
            max_aspect = 2.5  # Placeholder
            avg_aspect = 1.8  # Placeholder
            max_skew = 0.3    # Placeholder
            avg_skew = 0.15   # Placeholder
            
            # Check for symmetry
            symmetry = mesh.HasSymmetry if hasattr(mesh, 'HasSymmetry') else False
            
            # Calculate quality score (0-1)
            quality_score = self._calculate_mesh_quality_score(
                avg_aspect, avg_skew, panel_count
            )
            
            return MeshStatistics(
                panel_count=panel_count,
                node_count=node_count,
                waterline_panels=mesh.WaterlinePanels if hasattr(mesh, 'WaterlinePanels') else 0,
                max_aspect_ratio=max_aspect,
                avg_aspect_ratio=avg_aspect,
                max_skewness=max_skew,
                avg_skewness=avg_skew,
                symmetry_detected=symmetry,
                quality_score=quality_score
            )
            
        except Exception as e:
            logger.error("mesh_statistics_failed", error=str(e))
            return MeshStatistics(0, 0, 0, 0, 0, 0, 0, False, 0)
    
    def _calculate_mesh_quality_score(self, aspect_ratio: float, 
                                     skewness: float, panel_count: int) -> float:
        """Calculate overall mesh quality score (0-1)"""
        # Aspect ratio score (ideal = 1, max = 3)
        aspect_score = max(0, 1 - (aspect_ratio - 1) / 2)
        
        # Skewness score (ideal = 0, max = 0.5)
        skew_score = max(0, 1 - skewness * 2)
        
        # Panel count score (ideal range 1000-5000)
        if panel_count < 500:
            panel_score = 0.5
        elif panel_count < 1000:
            panel_score = 0.7
        elif panel_count <= 5000:
            panel_score = 1.0
        else:
            panel_score = max(0.7, 1 - (panel_count - 5000) / 10000)
        
        # Weighted average
        quality = (aspect_score * 0.4 + skew_score * 0.4 + panel_score * 0.2)
        
        return round(quality, 2)
    
    def refine_mesh(self, refinement_params: Dict[str, Any]) -> MeshStatistics:
        """
        Refine mesh based on parameters
        
        Args:
            refinement_params: Dictionary of refinement parameters
            
        Returns:
            Updated mesh statistics
        """
        self._ensure_connected()
        
        try:
            mesh = self.model.Mesh
            
            # Apply refinement parameters
            if "target_panels" in refinement_params:
                mesh.TargetPanelCount = refinement_params["target_panels"]
            
            if "waterline_refinement" in refinement_params:
                mesh.WaterlineRefinement = refinement_params["waterline_refinement"]
            
            if "max_panel_size" in refinement_params:
                mesh.MaxPanelSize = refinement_params["max_panel_size"]
            
            # Regenerate mesh
            mesh.Generate()
            
            # Get updated statistics
            stats = self._get_mesh_statistics()
            
            logger.info("mesh_refined", 
                       panels=stats.panel_count,
                       quality=stats.quality_score)
            
            return stats
            
        except Exception as e:
            logger.error("mesh_refinement_failed", error=str(e))
            return self._get_mesh_statistics()
    
    # ========== Analysis Operations ==========
    
    def set_analysis_type(self, analysis_type: AnalysisType) -> bool:
        """Set the type of analysis to perform"""
        self._ensure_connected()
        
        try:
            self.model.Analysis.Type = analysis_type.value
            logger.info("analysis_type_set", type=analysis_type.value)
            return True
            
        except Exception as e:
            logger.error("analysis_type_failed", error=str(e))
            return False
    
    async def start_analysis_async(self) -> 'AnalysisHandle':
        """
        Start analysis asynchronously
        
        Returns:
            AnalysisHandle for monitoring progress
        """
        self._ensure_connected()
        
        try:
            # Start analysis in non-blocking mode
            analysis_id = self.model.StartAnalysis(Async=True)
            
            logger.info("analysis_started", id=analysis_id)
            
            return AnalysisHandle(self, analysis_id)
            
        except Exception as e:
            logger.error("analysis_start_failed", error=str(e))
            raise
    
    def run_analysis(self) -> Dict[str, Any]:
        """
        Run analysis synchronously (blocking)
        
        Returns:
            Analysis results
        """
        self._ensure_connected()
        
        try:
            start_time = time.time()
            
            # Run analysis
            result = self.model.RunAnalysis()
            
            elapsed = time.time() - start_time
            
            logger.info("analysis_completed", elapsed=elapsed)
            
            return {
                "success": result.Success,
                "elapsed_time": elapsed,
                "warnings": result.Warnings if hasattr(result, 'Warnings') else [],
                "errors": result.Errors if hasattr(result, 'Errors') else []
            }
            
        except Exception as e:
            logger.error("analysis_failed", error=str(e))
            return {
                "success": False,
                "error": str(e)
            }
    
    # ========== Results Extraction ==========
    
    def get_added_mass(self) -> Dict[str, np.ndarray]:
        """Get added mass coefficients"""
        self._ensure_connected()
        
        try:
            results = self.model.Results
            added_mass = {}
            
            # Get frequency array
            frequencies = results.Frequencies
            
            # Get added mass for each DOF
            for dof in range(6):
                added_mass[f"A{dof+1}{dof+1}"] = np.array(
                    results.AddedMass[dof, dof, :]
                )
            
            logger.info("added_mass_extracted", 
                       frequencies=len(frequencies))
            
            return {
                "frequencies": frequencies,
                "coefficients": added_mass
            }
            
        except Exception as e:
            logger.error("added_mass_extraction_failed", error=str(e))
            return {}
    
    def get_damping(self) -> Dict[str, np.ndarray]:
        """Get radiation damping coefficients"""
        self._ensure_connected()
        
        try:
            results = self.model.Results
            damping = {}
            
            frequencies = results.Frequencies
            
            for dof in range(6):
                damping[f"B{dof+1}{dof+1}"] = np.array(
                    results.Damping[dof, dof, :]
                )
            
            logger.info("damping_extracted", 
                       frequencies=len(frequencies))
            
            return {
                "frequencies": frequencies,
                "coefficients": damping
            }
            
        except Exception as e:
            logger.error("damping_extraction_failed", error=str(e))
            return {}
    
    def get_excitation_forces(self) -> Dict[str, Any]:
        """Get wave excitation forces"""
        self._ensure_connected()
        
        try:
            results = self.model.Results
            
            frequencies = results.Frequencies
            directions = results.Directions
            
            excitation = {}
            for dof in range(6):
                excitation[f"F{dof+1}"] = np.array(
                    results.ExcitationForce[dof, :, :]
                )
            
            logger.info("excitation_extracted",
                       frequencies=len(frequencies),
                       directions=len(directions))
            
            return {
                "frequencies": frequencies,
                "directions": directions,
                "forces": excitation
            }
            
        except Exception as e:
            logger.error("excitation_extraction_failed", error=str(e))
            return {}
    
    def get_raos(self) -> Dict[str, Any]:
        """Get response amplitude operators"""
        self._ensure_connected()
        
        try:
            results = self.model.Results
            
            raos = {}
            for dof in range(6):
                raos[f"RAO{dof+1}"] = np.array(
                    results.RAO[dof, :, :]
                )
            
            logger.info("raos_extracted")
            
            return {
                "frequencies": results.Frequencies,
                "directions": results.Directions,
                "raos": raos
            }
            
        except Exception as e:
            logger.error("rao_extraction_failed", error=str(e))
            return {}
    
    def save_model(self, file_path: str) -> bool:
        """Save OrcaWave model to file"""
        self._ensure_connected()
        
        try:
            self.model.SaveAs(file_path)
            logger.info("model_saved", path=file_path)
            return True
            
        except Exception as e:
            logger.error("save_failed", error=str(e))
            return False
    
    def load_model(self, file_path: str) -> bool:
        """Load OrcaWave model from file"""
        self._ensure_connected()
        
        try:
            self.model = self.app.OpenModel(file_path)
            logger.info("model_loaded", path=file_path)
            return True
            
        except Exception as e:
            logger.error("load_failed", error=str(e))
            return False


class AnalysisHandle:
    """Handle for monitoring async analysis progress"""
    
    def __init__(self, api: OrcaWaveAPI, analysis_id: str):
        self.api = api
        self.analysis_id = analysis_id
        self.start_time = time.time()
    
    async def get_progress(self) -> AnalysisProgress:
        """Get current analysis progress"""
        try:
            progress = self.api.model.GetAnalysisProgress(self.analysis_id)
            
            elapsed = time.time() - self.start_time
            
            # Estimate remaining time
            if progress.Percentage > 0:
                estimated_total = elapsed / (progress.Percentage / 100)
                estimated_remaining = estimated_total - elapsed
            else:
                estimated_remaining = -1
            
            return AnalysisProgress(
                percentage=progress.Percentage,
                current_frequency=progress.CurrentFrequency,
                current_direction=progress.CurrentDirection,
                elapsed_time=elapsed,
                estimated_remaining=estimated_remaining,
                is_complete=progress.IsComplete,
                has_errors=progress.HasErrors,
                error_message=progress.ErrorMessage if hasattr(progress, 'ErrorMessage') else None
            )
            
        except Exception as e:
            logger.error("progress_check_failed", error=str(e))
            return AnalysisProgress(
                percentage=0,
                current_frequency=0,
                current_direction=0,
                elapsed_time=time.time() - self.start_time,
                estimated_remaining=-1,
                is_complete=False,
                has_errors=True,
                error_message=str(e)
            )
    
    async def is_complete(self) -> bool:
        """Check if analysis is complete"""
        progress = await self.get_progress()
        return progress.is_complete
    
    async def get_results(self) -> Dict[str, Any]:
        """Get analysis results when complete"""
        # Wait for completion
        while not await self.is_complete():
            await asyncio.sleep(1)
        
        # Extract results
        return {
            "success": True,
            "elapsed_time": time.time() - self.start_time,
            "added_mass": self.api.get_added_mass(),
            "damping": self.api.get_damping(),
            "excitation": self.api.get_excitation_forces(),
            "raos": self.api.get_raos()
        }


# ========== Test Function ==========

def test_connection():
    """Test OrcaWave COM connection"""
    api = OrcaWaveAPI()
    
    try:
        if api.connect():
            print("✓ OrcaWave COM connection successful")
            print(f"  Version: {api._get_version()}")
            api.disconnect()
            return True
        else:
            print("✗ Failed to connect to OrcaWave")
            return False
            
    except Exception as e:
        print(f"✗ Connection test failed: {e}")
        return False


if __name__ == "__main__":
    # Run connection test
    test_connection()