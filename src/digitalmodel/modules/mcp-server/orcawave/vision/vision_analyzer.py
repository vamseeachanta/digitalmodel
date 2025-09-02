#!/usr/bin/env python3
"""
Vision Analysis Module for OrcaWave
Analyzes screenshots to extract information about mesh quality, progress, and results
"""

import json
import time
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
from enum import Enum
import base64
import io

import numpy as np
import structlog

try:
    import cv2
    import PIL.Image
    from PIL import Image
    CV2_AVAILABLE = True
except ImportError:
    CV2_AVAILABLE = False

try:
    import pytesseract
    OCR_AVAILABLE = True
except ImportError:
    OCR_AVAILABLE = False

logger = structlog.get_logger()


class AnalysisType(str, Enum):
    """Types of visual analysis"""
    MESH_QUALITY = "mesh_quality"
    PROGRESS = "progress"
    RESULTS = "results"
    WARNING = "warning"
    GEOMETRY = "geometry"
    CONVERGENCE = "convergence"


@dataclass
class MeshQualityVisual:
    """Visual assessment of mesh quality"""
    panel_distribution: str  # uniform, clustered, sparse
    symmetry_detected: bool
    waterline_visible: bool
    quality_score: float  # 0-1 visual quality estimate
    problem_areas: List[Dict[str, Any]]
    recommendations: List[str]


@dataclass
class ProgressInfo:
    """Analysis progress information"""
    percentage: float
    current_stage: str
    current_frequency: Optional[float]
    current_direction: Optional[float]
    time_elapsed: Optional[str]
    time_remaining: Optional[str]
    status_message: str


@dataclass
class ConvergenceInfo:
    """Convergence plot information"""
    is_converging: bool
    iteration_count: int
    residual_value: float
    trend: str  # improving, stagnant, diverging


class OrcaWaveVisionAnalyzer:
    """
    Vision-based analysis for OrcaWave screenshots
    Extracts information from GUI elements using computer vision and OCR
    """
    
    def __init__(self, use_ai_vision: bool = False):
        """
        Initialize vision analyzer
        
        Args:
            use_ai_vision: Whether to use AI vision APIs (GPT-4V, Claude)
        """
        self.use_ai_vision = use_ai_vision
        self.template_cache = {}
        self.last_analysis = {}
        
        # Color ranges for detection (BGR format)
        self.color_ranges = {
            "warning_yellow": ((0, 180, 180), (30, 255, 255)),
            "error_red": ((0, 0, 150), (50, 50, 255)),
            "success_green": ((0, 150, 0), (50, 255, 50)),
            "water_blue": ((150, 50, 0), (255, 150, 50)),
            "mesh_lines": ((0, 0, 0), (50, 50, 50))
        }
        
        logger.info("vision_analyzer_initialized", 
                   cv2_available=CV2_AVAILABLE,
                   ocr_available=OCR_AVAILABLE,
                   ai_vision=use_ai_vision)
    
    def analyze_screenshot(self, image: np.ndarray, 
                          analysis_type: AnalysisType) -> Dict[str, Any]:
        """
        Analyze a screenshot based on the requested type
        
        Args:
            image: Screenshot as numpy array
            analysis_type: Type of analysis to perform
            
        Returns:
            Analysis results dictionary
        """
        if not CV2_AVAILABLE:
            return {"error": "OpenCV not available"}
        
        try:
            if analysis_type == AnalysisType.MESH_QUALITY:
                return self.analyze_mesh_quality(image)
            elif analysis_type == AnalysisType.PROGRESS:
                return self.analyze_progress(image)
            elif analysis_type == AnalysisType.RESULTS:
                return self.analyze_results(image)
            elif analysis_type == AnalysisType.WARNING:
                return self.detect_warnings(image)
            elif analysis_type == AnalysisType.GEOMETRY:
                return self.analyze_geometry(image)
            elif analysis_type == AnalysisType.CONVERGENCE:
                return self.analyze_convergence(image)
            else:
                return {"error": f"Unknown analysis type: {analysis_type}"}
                
        except Exception as e:
            logger.error("analysis_failed", 
                        type=analysis_type.value,
                        error=str(e))
            return {"error": str(e)}
    
    def analyze_mesh_quality(self, image: np.ndarray) -> Dict[str, Any]:
        """
        Analyze mesh quality from screenshot
        
        Args:
            image: Mesh view screenshot
            
        Returns:
            Mesh quality analysis
        """
        quality = MeshQualityVisual(
            panel_distribution="unknown",
            symmetry_detected=False,
            waterline_visible=False,
            quality_score=0.0,
            problem_areas=[],
            recommendations=[]
        )
        
        try:
            # Convert to grayscale for edge detection
            gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
            
            # Detect mesh lines using edge detection
            edges = cv2.Canny(gray, 50, 150)
            
            # Find contours (mesh panels)
            contours, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL, 
                                          cv2.CHAIN_APPROX_SIMPLE)
            
            if len(contours) > 0:
                # Analyze panel distribution
                areas = [cv2.contourArea(c) for c in contours if cv2.contourArea(c) > 10]
                
                if areas:
                    mean_area = np.mean(areas)
                    std_area = np.std(areas)
                    cv_area = std_area / mean_area if mean_area > 0 else 1.0
                    
                    # Assess distribution uniformity
                    if cv_area < 0.3:
                        quality.panel_distribution = "uniform"
                        quality.quality_score = 0.9
                    elif cv_area < 0.6:
                        quality.panel_distribution = "moderate"
                        quality.quality_score = 0.7
                    else:
                        quality.panel_distribution = "irregular"
                        quality.quality_score = 0.5
                        quality.recommendations.append(
                            "Consider remeshing for more uniform panel distribution"
                        )
                    
                    # Check for symmetry
                    h, w = image.shape[:2]
                    left_half = image[:, :w//2]
                    right_half = cv2.flip(image[:, w//2:], 1)
                    
                    if left_half.shape == right_half.shape:
                        diff = cv2.absdiff(left_half, right_half)
                        symmetry_score = 1.0 - (np.mean(diff) / 255.0)
                        quality.symmetry_detected = symmetry_score > 0.7
                    
                    # Detect waterline (horizontal line with many intersections)
                    horizontal_lines = self._detect_horizontal_lines(edges)
                    quality.waterline_visible = len(horizontal_lines) > 0
                    
                    # Identify problem areas (very small or large panels)
                    for i, area in enumerate(areas):
                        if area < mean_area * 0.2:
                            quality.problem_areas.append({
                                "type": "small_panel",
                                "index": i,
                                "severity": "warning"
                            })
                        elif area > mean_area * 3:
                            quality.problem_areas.append({
                                "type": "large_panel",
                                "index": i,
                                "severity": "warning"
                            })
                    
                    # Generate recommendations
                    if not quality.symmetry_detected:
                        quality.recommendations.append(
                            "Enable symmetry to reduce computational effort"
                        )
                    
                    if not quality.waterline_visible:
                        quality.recommendations.append(
                            "Ensure waterline is properly defined with adequate refinement"
                        )
                    
                    logger.info("mesh_quality_analyzed",
                               panels=len(contours),
                               quality=quality.quality_score)
            
        except Exception as e:
            logger.error("mesh_analysis_failed", error=str(e))
        
        return {
            "panel_distribution": quality.panel_distribution,
            "symmetry_detected": quality.symmetry_detected,
            "waterline_visible": quality.waterline_visible,
            "quality_score": quality.quality_score,
            "problem_areas": quality.problem_areas,
            "recommendations": quality.recommendations,
            "panel_count_estimate": len(contours) if 'contours' in locals() else 0
        }
    
    def analyze_progress(self, image: np.ndarray) -> Dict[str, Any]:
        """
        Analyze progress dialog from screenshot
        
        Args:
            image: Progress dialog screenshot
            
        Returns:
            Progress information
        """
        progress = ProgressInfo(
            percentage=0.0,
            current_stage="Unknown",
            current_frequency=None,
            current_direction=None,
            time_elapsed=None,
            time_remaining=None,
            status_message="Analyzing..."
        )
        
        try:
            # Use OCR if available
            if OCR_AVAILABLE:
                text = pytesseract.image_to_string(image)
                lines = text.strip().split('\n')
                
                for line in lines:
                    line_lower = line.lower()
                    
                    # Extract percentage
                    if '%' in line:
                        import re
                        numbers = re.findall(r'(\d+(?:\.\d+)?)\s*%', line)
                        if numbers:
                            progress.percentage = float(numbers[0])
                    
                    # Extract frequency
                    if 'frequency' in line_lower:
                        numbers = re.findall(r'(\d+(?:\.\d+)?)', line)
                        if numbers:
                            progress.current_frequency = float(numbers[0])
                    
                    # Extract direction
                    if 'direction' in line_lower or 'heading' in line_lower:
                        numbers = re.findall(r'(\d+(?:\.\d+)?)', line)
                        if numbers:
                            progress.current_direction = float(numbers[0])
                    
                    # Extract times
                    if 'elapsed' in line_lower:
                        progress.time_elapsed = line.split(':')[-1].strip()
                    
                    if 'remaining' in line_lower or 'eta' in line_lower:
                        progress.time_remaining = line.split(':')[-1].strip()
                    
                    # Extract stage
                    if 'solving' in line_lower:
                        progress.current_stage = "Solving"
                    elif 'meshing' in line_lower:
                        progress.current_stage = "Meshing"
                    elif 'processing' in line_lower:
                        progress.current_stage = "Processing"
            
            else:
                # Fallback: Look for progress bar
                progress_bar = self._detect_progress_bar(image)
                if progress_bar:
                    progress.percentage = progress_bar
            
            # Detect if complete by looking for green color
            if self._detect_color_region(image, "success_green"):
                progress.percentage = 100.0
                progress.current_stage = "Complete"
                progress.status_message = "Analysis completed successfully"
            
            logger.info("progress_analyzed", percentage=progress.percentage)
            
        except Exception as e:
            logger.error("progress_analysis_failed", error=str(e))
        
        return {
            "percentage": progress.percentage,
            "current_stage": progress.current_stage,
            "current_frequency": progress.current_frequency,
            "current_direction": progress.current_direction,
            "time_elapsed": progress.time_elapsed,
            "time_remaining": progress.time_remaining,
            "status_message": progress.status_message
        }
    
    def analyze_results(self, image: np.ndarray) -> Dict[str, Any]:
        """
        Analyze results plots from screenshot
        
        Args:
            image: Results view screenshot
            
        Returns:
            Results analysis
        """
        results = {
            "plots_detected": [],
            "axes_labels": [],
            "data_trends": [],
            "anomalies": []
        }
        
        try:
            # Detect plot regions
            gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
            
            # Look for rectangular regions (plots)
            edges = cv2.Canny(gray, 50, 150)
            contours, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL,
                                          cv2.CHAIN_APPROX_SIMPLE)
            
            # Find large rectangular contours (likely plots)
            for contour in contours:
                area = cv2.contourArea(contour)
                if area > 10000:  # Minimum plot size
                    x, y, w, h = cv2.boundingRect(contour)
                    aspect_ratio = w / h if h > 0 else 0
                    
                    # Plots typically have aspect ratio between 0.5 and 2
                    if 0.5 < aspect_ratio < 2:
                        results["plots_detected"].append({
                            "location": (x, y, w, h),
                            "type": "2D_plot"
                        })
                        
                        # Extract plot region for further analysis
                        plot_region = image[y:y+h, x:x+w]
                        
                        # Analyze data trends
                        trend = self._analyze_plot_trend(plot_region)
                        if trend:
                            results["data_trends"].append(trend)
            
            # Use OCR for axis labels if available
            if OCR_AVAILABLE and results["plots_detected"]:
                for plot in results["plots_detected"]:
                    x, y, w, h = plot["location"]
                    
                    # Extract regions around axes
                    bottom_region = image[y+h-50:y+h, x:x+w]
                    left_region = image[y:y+h, x:x+50]
                    
                    # OCR on axis regions
                    x_label = pytesseract.image_to_string(bottom_region).strip()
                    y_label = pytesseract.image_to_string(left_region).strip()
                    
                    if x_label or y_label:
                        results["axes_labels"].append({
                            "x_axis": x_label,
                            "y_axis": y_label
                        })
            
            logger.info("results_analyzed", plots=len(results["plots_detected"]))
            
        except Exception as e:
            logger.error("results_analysis_failed", error=str(e))
        
        return results
    
    def analyze_convergence(self, image: np.ndarray) -> Dict[str, Any]:
        """
        Analyze convergence plot
        
        Args:
            image: Convergence plot screenshot
            
        Returns:
            Convergence information
        """
        convergence = ConvergenceInfo(
            is_converging=False,
            iteration_count=0,
            residual_value=1.0,
            trend="unknown"
        )
        
        try:
            # Look for downward trend in plot
            trend = self._analyze_plot_trend(image)
            
            if trend:
                if trend["direction"] == "decreasing":
                    convergence.is_converging = True
                    convergence.trend = "improving"
                elif trend["direction"] == "flat":
                    convergence.trend = "stagnant"
                else:
                    convergence.trend = "diverging"
                
                convergence.iteration_count = trend.get("data_points", 0)
            
            logger.info("convergence_analyzed", 
                       converging=convergence.is_converging)
            
        except Exception as e:
            logger.error("convergence_analysis_failed", error=str(e))
        
        return {
            "is_converging": convergence.is_converging,
            "iteration_count": convergence.iteration_count,
            "residual_value": convergence.residual_value,
            "trend": convergence.trend
        }
    
    def analyze_geometry(self, image: np.ndarray) -> Dict[str, Any]:
        """
        Analyze 3D geometry view
        
        Args:
            image: 3D view screenshot
            
        Returns:
            Geometry analysis
        """
        geometry = {
            "model_visible": False,
            "orientation": "unknown",
            "completeness": "unknown",
            "issues": []
        }
        
        try:
            # Check if model is visible (non-empty center region)
            h, w = image.shape[:2]
            center_region = image[h//4:3*h//4, w//4:3*w//4]
            
            # Check for non-background pixels
            gray = cv2.cvtColor(center_region, cv2.COLOR_BGR2GRAY)
            non_bg = np.count_nonzero(gray > 10)
            total = gray.size
            
            if non_bg / total > 0.1:  # At least 10% non-background
                geometry["model_visible"] = True
                
                # Check for completeness (closed surface)
                edges = cv2.Canny(gray, 50, 150)
                contours, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL,
                                              cv2.CHAIN_APPROX_SIMPLE)
                
                if contours:
                    # Large closed contour suggests complete model
                    largest_contour = max(contours, key=cv2.contourArea)
                    if cv2.isContourConvex(largest_contour):
                        geometry["completeness"] = "complete"
                    else:
                        geometry["completeness"] = "partial"
                        geometry["issues"].append("Model may have gaps or open surfaces")
            
            logger.info("geometry_analyzed", visible=geometry["model_visible"])
            
        except Exception as e:
            logger.error("geometry_analysis_failed", error=str(e))
        
        return geometry
    
    def detect_warnings(self, image: np.ndarray) -> Dict[str, Any]:
        """
        Detect warning or error dialogs
        
        Args:
            image: Screenshot to check
            
        Returns:
            Warning detection results
        """
        warnings = {
            "has_warning": False,
            "has_error": False,
            "messages": [],
            "severity": "none"
        }
        
        try:
            # Check for warning colors
            if self._detect_color_region(image, "warning_yellow"):
                warnings["has_warning"] = True
                warnings["severity"] = "warning"
                warnings["messages"].append("Warning dialog detected")
            
            # Check for error colors
            if self._detect_color_region(image, "error_red"):
                warnings["has_error"] = True
                warnings["severity"] = "error"
                warnings["messages"].append("Error dialog detected")
            
            # Use OCR to extract message text if available
            if OCR_AVAILABLE and (warnings["has_warning"] or warnings["has_error"]):
                text = pytesseract.image_to_string(image)
                if text.strip():
                    warnings["messages"].append(text.strip())
            
            logger.info("warnings_detected", 
                       has_warning=warnings["has_warning"],
                       has_error=warnings["has_error"])
            
        except Exception as e:
            logger.error("warning_detection_failed", error=str(e))
        
        return warnings
    
    def _detect_horizontal_lines(self, edges: np.ndarray) -> List[int]:
        """Detect horizontal lines (like waterline)"""
        lines = []
        try:
            # Use Hough transform to detect lines
            detected = cv2.HoughLinesP(edges, 1, np.pi/180, 100,
                                       minLineLength=100, maxLineGap=10)
            
            if detected is not None:
                for line in detected:
                    x1, y1, x2, y2 = line[0]
                    # Check if line is horizontal (small y difference)
                    if abs(y2 - y1) < 5:
                        lines.append(y1)
        except:
            pass
        
        return lines
    
    def _detect_progress_bar(self, image: np.ndarray) -> Optional[float]:
        """Detect and measure progress bar"""
        try:
            # Look for horizontal bar with filled portion
            gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
            
            # Find horizontal rectangles
            edges = cv2.Canny(gray, 50, 150)
            contours, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL,
                                          cv2.CHAIN_APPROX_SIMPLE)
            
            for contour in contours:
                x, y, w, h = cv2.boundingRect(contour)
                
                # Progress bars are typically wide and short
                if w > h * 5 and w > 100:
                    # Measure filled portion
                    bar_region = gray[y:y+h, x:x+w]
                    filled = np.count_nonzero(bar_region > 128)
                    total = bar_region.size
                    
                    if total > 0:
                        return (filled / total) * 100
        except:
            pass
        
        return None
    
    def _detect_color_region(self, image: np.ndarray, color_name: str) -> bool:
        """Detect if a specific color range is present"""
        if color_name not in self.color_ranges:
            return False
        
        try:
            lower, upper = self.color_ranges[color_name]
            mask = cv2.inRange(image, lower, upper)
            
            # Check if significant pixels match
            matches = np.count_nonzero(mask)
            total = mask.size
            
            return (matches / total) > 0.01  # At least 1% of image
        except:
            return False
    
    def _analyze_plot_trend(self, plot_image: np.ndarray) -> Optional[Dict[str, Any]]:
        """Analyze trend in a plot"""
        try:
            # Convert to grayscale
            gray = cv2.cvtColor(plot_image, cv2.COLOR_BGR2GRAY)
            
            # Find data points (dark pixels on light background)
            _, binary = cv2.threshold(gray, 127, 255, cv2.THRESH_BINARY_INV)
            
            # Get y-coordinates of data points
            points = np.where(binary > 0)
            
            if len(points[0]) > 10:  # Need enough points
                y_values = points[0]
                x_values = points[1]
                
                # Sort by x coordinate
                sorted_indices = np.argsort(x_values)
                y_sorted = y_values[sorted_indices]
                
                # Calculate trend (simple linear regression)
                if len(y_sorted) > 1:
                    coeffs = np.polyfit(range(len(y_sorted)), y_sorted, 1)
                    slope = coeffs[0]
                    
                    # Determine trend direction
                    if slope < -0.5:
                        direction = "decreasing"
                    elif slope > 0.5:
                        direction = "increasing"
                    else:
                        direction = "flat"
                    
                    return {
                        "direction": direction,
                        "slope": float(slope),
                        "data_points": len(y_sorted)
                    }
        except:
            pass
        
        return None
    
    def compare_screenshots(self, image1: np.ndarray, image2: np.ndarray) -> float:
        """
        Compare two screenshots for similarity
        
        Args:
            image1: First screenshot
            image2: Second screenshot
            
        Returns:
            Similarity score (0-1)
        """
        try:
            # Resize to same size if needed
            if image1.shape != image2.shape:
                h = min(image1.shape[0], image2.shape[0])
                w = min(image1.shape[1], image2.shape[1])
                image1 = cv2.resize(image1, (w, h))
                image2 = cv2.resize(image2, (w, h))
            
            # Calculate structural similarity
            diff = cv2.absdiff(image1, image2)
            similarity = 1.0 - (np.mean(diff) / 255.0)
            
            return similarity
            
        except Exception as e:
            logger.error("comparison_failed", error=str(e))
            return 0.0


# Test function
def test_vision_analyzer():
    """Test vision analyzer functionality"""
    analyzer = OrcaWaveVisionAnalyzer()
    
    # Create a test image (blank for now)
    test_image = np.ones((600, 800, 3), dtype=np.uint8) * 255
    
    # Test different analysis types
    print("Testing vision analyzer...")
    
    mesh_result = analyzer.analyze_screenshot(test_image, AnalysisType.MESH_QUALITY)
    print(f"Mesh analysis: {mesh_result}")
    
    progress_result = analyzer.analyze_screenshot(test_image, AnalysisType.PROGRESS)
    print(f"Progress analysis: {progress_result}")
    
    warning_result = analyzer.detect_warnings(test_image)
    print(f"Warning detection: {warning_result}")
    
    return True


if __name__ == "__main__":
    test_vision_analyzer()