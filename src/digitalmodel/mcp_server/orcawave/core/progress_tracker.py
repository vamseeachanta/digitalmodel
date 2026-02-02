#!/usr/bin/env python3
"""
Progress Tracking System for OrcaWave MCP
Provides time estimation, stage tracking, and progress monitoring
"""

import time
import json
from typing import Optional, Dict, Any, List, Tuple
from dataclasses import dataclass, field, asdict
from datetime import datetime, timedelta
from enum import Enum
import statistics
import structlog
from pathlib import Path

logger = structlog.get_logger()


class AnalysisStage(Enum):
    """Analysis stages with typical duration percentages"""
    INITIALIZATION = ("initialization", 0.05)  # 5% of total time
    GEOMETRY_IMPORT = ("geometry_import", 0.10)  # 10%
    MESH_GENERATION = ("mesh_generation", 0.20)  # 20%
    MESH_OPTIMIZATION = ("mesh_optimization", 0.10)  # 10%
    ANALYSIS_SETUP = ("analysis_setup", 0.05)  # 5%
    DIFFRACTION_ANALYSIS = ("diffraction_analysis", 0.40)  # 40%
    RESULTS_PROCESSING = ("results_processing", 0.05)  # 5%
    EXPORT = ("export", 0.05)  # 5%
    
    def __init__(self, display_name: str, typical_percentage: float):
        self.display_name = display_name
        self.typical_percentage = typical_percentage


@dataclass
class StageProgress:
    """Progress information for a single stage"""
    stage: AnalysisStage
    status: str  # 'pending', 'running', 'completed', 'failed', 'skipped'
    progress: float = 0.0  # 0-100
    start_time: Optional[float] = None
    end_time: Optional[float] = None
    duration: Optional[float] = None
    error_message: Optional[str] = None
    
    @property
    def elapsed_time(self) -> float:
        """Get elapsed time for this stage"""
        if self.start_time:
            end = self.end_time or time.time()
            return end - self.start_time
        return 0.0
    
    @property
    def is_complete(self) -> bool:
        """Check if stage is complete"""
        return self.status in ['completed', 'failed', 'skipped']


@dataclass
class FrequencyProgress:
    """Progress tracking for frequency analysis"""
    frequency: float
    direction_count: int
    completed_directions: int = 0
    start_time: Optional[float] = None
    end_time: Optional[float] = None
    
    @property
    def progress(self) -> float:
        """Calculate progress percentage"""
        if self.direction_count == 0:
            return 0.0
        return (self.completed_directions / self.direction_count) * 100
    
    @property
    def is_complete(self) -> bool:
        """Check if frequency is complete"""
        return self.completed_directions >= self.direction_count


@dataclass
class AnalysisProgress:
    """Complete analysis progress tracking"""
    analysis_id: str
    vessel_name: str
    total_frequencies: int
    total_directions: int
    water_depth: float
    start_time: float = field(default_factory=time.time)
    end_time: Optional[float] = None
    
    # Stage tracking
    stages: Dict[str, StageProgress] = field(default_factory=dict)
    current_stage: Optional[AnalysisStage] = None
    
    # Frequency tracking
    frequencies: Dict[float, FrequencyProgress] = field(default_factory=dict)
    current_frequency: Optional[float] = None
    
    # Time estimation
    estimated_total_duration: Optional[float] = None
    estimated_completion_time: Optional[float] = None
    
    # Historical data for estimation
    stage_duration_history: Dict[str, List[float]] = field(default_factory=dict)
    frequency_duration_history: List[float] = field(default_factory=list)
    
    @property
    def overall_progress(self) -> float:
        """Calculate overall progress percentage"""
        if not self.stages:
            return 0.0
        
        total_weight = 0.0
        weighted_progress = 0.0
        
        for stage in AnalysisStage:
            stage_key = stage.name
            if stage_key in self.stages:
                stage_progress = self.stages[stage_key]
                weight = stage.typical_percentage
                total_weight += weight
                
                if stage_progress.status == 'completed':
                    weighted_progress += weight * 100
                elif stage_progress.status == 'running':
                    weighted_progress += weight * stage_progress.progress
        
        if total_weight == 0:
            return 0.0
        
        return weighted_progress / total_weight
    
    @property
    def elapsed_time(self) -> float:
        """Get total elapsed time"""
        end = self.end_time or time.time()
        return end - self.start_time
    
    @property
    def remaining_time(self) -> Optional[float]:
        """Estimate remaining time"""
        if self.estimated_total_duration:
            return max(0, self.estimated_total_duration - self.elapsed_time)
        return None
    
    @property
    def is_complete(self) -> bool:
        """Check if analysis is complete"""
        return all(stage.is_complete for stage in self.stages.values())
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization"""
        return {
            "analysis_id": self.analysis_id,
            "vessel_name": self.vessel_name,
            "overall_progress": self.overall_progress,
            "elapsed_time": self.elapsed_time,
            "remaining_time": self.remaining_time,
            "estimated_completion": datetime.fromtimestamp(
                self.estimated_completion_time
            ).isoformat() if self.estimated_completion_time else None,
            "current_stage": self.current_stage.display_name if self.current_stage else None,
            "current_frequency": self.current_frequency,
            "stages": {
                name: {
                    "status": stage.status,
                    "progress": stage.progress,
                    "duration": stage.duration
                }
                for name, stage in self.stages.items()
            }
        }


class ProgressTracker:
    """
    Advanced progress tracking with time estimation and historical learning
    """
    
    def __init__(self, history_file: Optional[str] = None):
        """
        Initialize progress tracker.
        
        Args:
            history_file: Path to historical data file for better estimates
        """
        self.active_analyses: Dict[str, AnalysisProgress] = {}
        self.completed_analyses: List[AnalysisProgress] = []
        
        # Historical data for time estimation
        self.history_file = history_file or "orcawave_progress_history.json"
        self.historical_data = self._load_history()
        
        # Configuration
        self.max_history_size = 100
        self.estimation_confidence_threshold = 0.7
        
        logger.info("progress_tracker_initialized", history_file=self.history_file)
    
    def _load_history(self) -> Dict[str, Any]:
        """Load historical progress data"""
        try:
            history_path = Path(self.history_file)
            if history_path.exists():
                with open(history_path, 'r') as f:
                    return json.load(f)
        except Exception as e:
            logger.error("history_load_failed", error=str(e))
        
        return {
            "stage_durations": {},
            "frequency_durations": [],
            "vessel_complexities": {}
        }
    
    def _save_history(self):
        """Save historical progress data"""
        try:
            with open(self.history_file, 'w') as f:
                json.dump(self.historical_data, f, indent=2)
        except Exception as e:
            logger.error("history_save_failed", error=str(e))
    
    def start_analysis(self, analysis_id: str, vessel_name: str,
                      frequencies: List[float], directions: List[float],
                      water_depth: float = 200.0) -> AnalysisProgress:
        """
        Start tracking a new analysis.
        
        Args:
            analysis_id: Unique analysis identifier
            vessel_name: Vessel being analyzed
            frequencies: Analysis frequencies
            directions: Wave directions
            water_depth: Water depth
            
        Returns:
            AnalysisProgress object
        """
        progress = AnalysisProgress(
            analysis_id=analysis_id,
            vessel_name=vessel_name,
            total_frequencies=len(frequencies),
            total_directions=len(directions),
            water_depth=water_depth
        )
        
        # Initialize stages
        for stage in AnalysisStage:
            progress.stages[stage.name] = StageProgress(
                stage=stage,
                status='pending'
            )
        
        # Initialize frequency tracking
        for freq in frequencies:
            progress.frequencies[freq] = FrequencyProgress(
                frequency=freq,
                direction_count=len(directions)
            )
        
        # Estimate total duration
        progress.estimated_total_duration = self._estimate_total_duration(
            vessel_name, len(frequencies), len(directions)
        )
        
        if progress.estimated_total_duration:
            progress.estimated_completion_time = (
                progress.start_time + progress.estimated_total_duration
            )
        
        self.active_analyses[analysis_id] = progress
        
        logger.info("analysis_tracking_started",
                   id=analysis_id,
                   vessel=vessel_name,
                   frequencies=len(frequencies),
                   directions=len(directions),
                   estimated_duration=progress.estimated_total_duration)
        
        return progress
    
    def start_stage(self, analysis_id: str, stage: AnalysisStage) -> bool:
        """
        Mark a stage as started.
        
        Args:
            analysis_id: Analysis identifier
            stage: Stage to start
            
        Returns:
            Success status
        """
        if analysis_id not in self.active_analyses:
            logger.error("analysis_not_found", id=analysis_id)
            return False
        
        progress = self.active_analyses[analysis_id]
        stage_key = stage.name
        
        if stage_key in progress.stages:
            progress.stages[stage_key].status = 'running'
            progress.stages[stage_key].start_time = time.time()
            progress.stages[stage_key].progress = 0.0
            progress.current_stage = stage
            
            logger.info("stage_started",
                       analysis=analysis_id,
                       stage=stage.display_name)
            
            # Update time estimate
            self._update_time_estimate(progress)
            return True
        
        return False
    
    def update_stage_progress(self, analysis_id: str, stage: AnalysisStage,
                            progress_percentage: float) -> bool:
        """
        Update progress for a stage.
        
        Args:
            analysis_id: Analysis identifier
            stage: Stage to update
            progress_percentage: Progress (0-100)
            
        Returns:
            Success status
        """
        if analysis_id not in self.active_analyses:
            return False
        
        progress = self.active_analyses[analysis_id]
        stage_key = stage.name
        
        if stage_key in progress.stages:
            progress.stages[stage_key].progress = min(100.0, max(0.0, progress_percentage))
            
            # Update time estimate based on progress
            if progress.stages[stage_key].status == 'running':
                self._update_time_estimate(progress)
            
            return True
        
        return False
    
    def complete_stage(self, analysis_id: str, stage: AnalysisStage,
                      success: bool = True, error_message: Optional[str] = None) -> bool:
        """
        Mark a stage as completed.
        
        Args:
            analysis_id: Analysis identifier
            stage: Stage to complete
            success: Whether stage succeeded
            error_message: Error message if failed
            
        Returns:
            Success status
        """
        if analysis_id not in self.active_analyses:
            return False
        
        progress = self.active_analyses[analysis_id]
        stage_key = stage.name
        
        if stage_key in progress.stages:
            stage_progress = progress.stages[stage_key]
            stage_progress.end_time = time.time()
            stage_progress.duration = stage_progress.elapsed_time
            stage_progress.status = 'completed' if success else 'failed'
            stage_progress.progress = 100.0 if success else stage_progress.progress
            stage_progress.error_message = error_message
            
            # Record duration for learning
            if success and stage_progress.duration:
                if stage_key not in self.historical_data["stage_durations"]:
                    self.historical_data["stage_durations"][stage_key] = []
                
                self.historical_data["stage_durations"][stage_key].append(
                    stage_progress.duration
                )
                
                # Keep history limited
                if len(self.historical_data["stage_durations"][stage_key]) > self.max_history_size:
                    self.historical_data["stage_durations"][stage_key] = (
                        self.historical_data["stage_durations"][stage_key][-self.max_history_size:]
                    )
            
            logger.info("stage_completed",
                       analysis=analysis_id,
                       stage=stage.display_name,
                       success=success,
                       duration=stage_progress.duration)
            
            # Clear current stage if it matches
            if progress.current_stage == stage:
                progress.current_stage = None
            
            # Update time estimate
            self._update_time_estimate(progress)
            
            # Save history periodically
            if len(self.completed_analyses) % 5 == 0:
                self._save_history()
            
            return True
        
        return False
    
    def update_frequency_progress(self, analysis_id: str, frequency: float,
                                 completed_directions: int) -> bool:
        """
        Update progress for a frequency.
        
        Args:
            analysis_id: Analysis identifier
            frequency: Frequency value
            completed_directions: Number of completed directions
            
        Returns:
            Success status
        """
        if analysis_id not in self.active_analyses:
            return False
        
        progress = self.active_analyses[analysis_id]
        
        if frequency in progress.frequencies:
            freq_progress = progress.frequencies[frequency]
            freq_progress.completed_directions = completed_directions
            
            if freq_progress.start_time is None:
                freq_progress.start_time = time.time()
            
            if freq_progress.is_complete and freq_progress.end_time is None:
                freq_progress.end_time = time.time()
                
                # Record frequency duration
                duration = freq_progress.end_time - freq_progress.start_time
                self.historical_data["frequency_durations"].append(duration)
                
                # Keep history limited
                if len(self.historical_data["frequency_durations"]) > self.max_history_size:
                    self.historical_data["frequency_durations"] = (
                        self.historical_data["frequency_durations"][-self.max_history_size:]
                    )
            
            progress.current_frequency = frequency
            
            # Update diffraction stage progress based on frequencies
            completed_freqs = sum(1 for f in progress.frequencies.values() if f.is_complete)
            total_freqs = len(progress.frequencies)
            
            if total_freqs > 0:
                freq_progress_pct = (completed_freqs / total_freqs) * 100
                self.update_stage_progress(
                    analysis_id,
                    AnalysisStage.DIFFRACTION_ANALYSIS,
                    freq_progress_pct
                )
            
            return True
        
        return False
    
    def complete_analysis(self, analysis_id: str, success: bool = True) -> bool:
        """
        Mark an analysis as completed.
        
        Args:
            analysis_id: Analysis identifier
            success: Whether analysis succeeded
            
        Returns:
            Success status
        """
        if analysis_id not in self.active_analyses:
            return False
        
        progress = self.active_analyses[analysis_id]
        progress.end_time = time.time()
        
        # Move to completed
        self.completed_analyses.append(progress)
        del self.active_analyses[analysis_id]
        
        # Record vessel complexity
        if success:
            vessel_key = f"{progress.vessel_name}_{progress.total_frequencies}x{progress.total_directions}"
            self.historical_data["vessel_complexities"][vessel_key] = progress.elapsed_time
        
        # Save history
        self._save_history()
        
        logger.info("analysis_completed",
                   id=analysis_id,
                   success=success,
                   duration=progress.elapsed_time,
                   overall_progress=progress.overall_progress)
        
        return True
    
    def _estimate_total_duration(self, vessel_name: str,
                                num_frequencies: int,
                                num_directions: int) -> Optional[float]:
        """
        Estimate total analysis duration based on historical data.
        
        Args:
            vessel_name: Vessel name
            num_frequencies: Number of frequencies
            num_directions: Number of directions
            
        Returns:
            Estimated duration in seconds
        """
        # Check for exact vessel match
        vessel_key = f"{vessel_name}_{num_frequencies}x{num_directions}"
        if vessel_key in self.historical_data["vessel_complexities"]:
            return self.historical_data["vessel_complexities"][vessel_key]
        
        # Estimate based on stage durations
        total_estimate = 0.0
        confidence = 1.0
        
        for stage in AnalysisStage:
            stage_key = stage.name
            
            if stage_key in self.historical_data["stage_durations"]:
                durations = self.historical_data["stage_durations"][stage_key]
                if durations:
                    # Use median for robustness
                    stage_estimate = statistics.median(durations)
                    
                    # Scale diffraction analysis by frequency/direction count
                    if stage == AnalysisStage.DIFFRACTION_ANALYSIS:
                        if self.historical_data["frequency_durations"]:
                            freq_duration = statistics.median(
                                self.historical_data["frequency_durations"]
                            )
                            stage_estimate = freq_duration * num_frequencies
                    
                    total_estimate += stage_estimate
                else:
                    # Use typical percentage as fallback
                    confidence *= 0.8
                    # Rough estimate: 1 hour base + scaling
                    base_duration = 3600
                    stage_estimate = base_duration * stage.typical_percentage
                    
                    if stage == AnalysisStage.DIFFRACTION_ANALYSIS:
                        stage_estimate *= (num_frequencies * num_directions) / 100
                    
                    total_estimate += stage_estimate
            else:
                confidence *= 0.7
                # Fallback estimate
                base_duration = 3600
                total_estimate += base_duration * stage.typical_percentage
        
        # Only return estimate if confidence is high enough
        if confidence >= self.estimation_confidence_threshold:
            return total_estimate
        
        # Very rough fallback
        return 60 * (10 + num_frequencies * num_directions * 0.5)
    
    def _update_time_estimate(self, progress: AnalysisProgress):
        """
        Update time estimate based on current progress.
        
        Args:
            progress: Analysis progress object
        """
        # Calculate actual vs estimated progress
        elapsed = progress.elapsed_time
        progress_pct = progress.overall_progress
        
        if progress_pct > 10:  # Need some progress for accurate estimate
            # Extrapolate based on current rate
            estimated_total = (elapsed / progress_pct) * 100
            
            # Blend with original estimate
            if progress.estimated_total_duration:
                # Weight recent estimate more as we get more data
                weight = min(0.8, progress_pct / 100)
                progress.estimated_total_duration = (
                    estimated_total * weight +
                    progress.estimated_total_duration * (1 - weight)
                )
            else:
                progress.estimated_total_duration = estimated_total
            
            # Update completion time
            progress.estimated_completion_time = (
                progress.start_time + progress.estimated_total_duration
            )
    
    def get_progress(self, analysis_id: str) -> Optional[AnalysisProgress]:
        """
        Get progress for an analysis.
        
        Args:
            analysis_id: Analysis identifier
            
        Returns:
            AnalysisProgress object or None
        """
        return self.active_analyses.get(analysis_id)
    
    def get_all_active(self) -> Dict[str, AnalysisProgress]:
        """Get all active analyses"""
        return self.active_analyses.copy()
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get tracking statistics.
        
        Returns:
            Statistics dictionary
        """
        stats = {
            "active_analyses": len(self.active_analyses),
            "completed_analyses": len(self.completed_analyses),
            "historical_data_points": len(self.historical_data.get("frequency_durations", [])),
            "stage_estimates": {}
        }
        
        # Calculate average stage durations
        for stage_name, durations in self.historical_data.get("stage_durations", {}).items():
            if durations:
                stats["stage_estimates"][stage_name] = {
                    "mean": statistics.mean(durations),
                    "median": statistics.median(durations),
                    "samples": len(durations)
                }
        
        return stats


# Standalone test
def test_progress_tracker():
    """Test progress tracking functionality"""
    tracker = ProgressTracker()
    
    # Start analysis
    frequencies = [0.1, 0.5, 1.0, 1.5, 2.0]
    directions = [0, 45, 90, 135, 180, 225, 270, 315]
    
    progress = tracker.start_analysis(
        "test_001",
        "Test Vessel",
        frequencies,
        directions,
        water_depth=200.0
    )
    
    print(f"✓ Analysis started: {progress.analysis_id}")
    print(f"  Estimated duration: {progress.estimated_total_duration:.1f}s")
    
    # Simulate stage progression
    stages_to_test = [
        AnalysisStage.INITIALIZATION,
        AnalysisStage.GEOMETRY_IMPORT,
        AnalysisStage.MESH_GENERATION
    ]
    
    for stage in stages_to_test:
        tracker.start_stage("test_001", stage)
        time.sleep(0.1)
        tracker.update_stage_progress("test_001", stage, 50.0)
        time.sleep(0.1)
        tracker.complete_stage("test_001", stage, success=True)
        
        current = tracker.get_progress("test_001")
        print(f"  {stage.display_name}: {current.overall_progress:.1f}% complete")
    
    # Test frequency progress
    for i, freq in enumerate(frequencies[:2]):
        for j in range(len(directions)):
            tracker.update_frequency_progress("test_001", freq, j + 1)
            time.sleep(0.01)
    
    # Get final progress
    final = tracker.get_progress("test_001")
    print(f"\n✓ Final progress: {final.overall_progress:.1f}%")
    print(f"  Elapsed time: {final.elapsed_time:.1f}s")
    print(f"  Remaining time: {final.remaining_time:.1f}s" if final.remaining_time else "")
    
    # Complete analysis
    tracker.complete_analysis("test_001", success=True)
    
    # Get statistics
    stats = tracker.get_statistics()
    print(f"\n✓ Statistics:")
    print(f"  Active: {stats['active_analyses']}")
    print(f"  Completed: {stats['completed_analyses']}")
    
    return True


if __name__ == "__main__":
    test_progress_tracker()