#!/usr/bin/env python3
"""
Hybrid Control Coordinator for OrcaWave MCP
Synchronizes COM API operations with visual verification
"""

import asyncio
import time
from typing import Optional, Dict, Any, Callable, List
from dataclasses import dataclass
from enum import Enum
import threading
from concurrent.futures import ThreadPoolExecutor
import structlog

try:
    from ..api.orcawave_com import OrcaWaveAPI
    from ..vision.screen_capture import OrcaWaveScreenCapture
    from ..vision.vision_analyzer import OrcaWaveVisionAnalyzer
except ImportError:
    # Fallback for testing
    OrcaWaveAPI = None
    OrcaWaveScreenCapture = None
    OrcaWaveVisionAnalyzer = None

logger = structlog.get_logger()


class ControlMode(Enum):
    """Control mode for operations"""
    COM_ONLY = "com_only"  # Use only COM API
    VISION_ONLY = "vision_only"  # Use only vision/GUI
    HYBRID = "hybrid"  # Use both with verification
    ADAPTIVE = "adaptive"  # Automatically choose best method


class VerificationLevel(Enum):
    """Level of visual verification"""
    NONE = 0
    MINIMAL = 1  # Quick screenshot check
    STANDARD = 2  # Verify key elements
    THOROUGH = 3  # Full validation with retries


@dataclass
class OperationResult:
    """Result of a hybrid operation"""
    success: bool
    method_used: str  # 'com', 'vision', 'hybrid'
    com_result: Optional[Any] = None
    vision_result: Optional[Any] = None
    verification_passed: bool = True
    error: Optional[str] = None
    duration: float = 0.0
    retries: int = 0


@dataclass
class VerificationConfig:
    """Configuration for visual verification"""
    enabled: bool = True
    level: VerificationLevel = VerificationLevel.STANDARD
    timeout: float = 30.0
    retry_count: int = 3
    retry_delay: float = 2.0
    screenshot_interval: float = 1.0


class HybridCoordinator:
    """
    Coordinates hybrid control between COM API and vision-based GUI control.
    Provides synchronized operations with visual verification.
    """
    
    def __init__(self, 
                 api: Optional[OrcaWaveAPI] = None,
                 screen_capture: Optional[OrcaWaveScreenCapture] = None,
                 vision_analyzer: Optional[OrcaWaveVisionAnalyzer] = None,
                 control_mode: ControlMode = ControlMode.ADAPTIVE,
                 verification_config: Optional[VerificationConfig] = None):
        """
        Initialize hybrid coordinator.
        
        Args:
            api: OrcaWave COM API instance
            screen_capture: Screen capture instance
            vision_analyzer: Vision analyzer instance
            control_mode: Default control mode
            verification_config: Visual verification configuration
        """
        self.api = api or (OrcaWaveAPI() if OrcaWaveAPI else None)
        self.screen_capture = screen_capture or (OrcaWaveScreenCapture() if OrcaWaveScreenCapture else None)
        self.vision_analyzer = vision_analyzer or (OrcaWaveVisionAnalyzer() if OrcaWaveVisionAnalyzer else None)
        self.control_mode = control_mode
        self.verification_config = verification_config or VerificationConfig()
        
        # Operation history for adaptive learning
        self.operation_history = []
        self.success_rates = {"com": 1.0, "vision": 0.8, "hybrid": 0.95}
        
        # Thread pool for parallel operations
        self.executor = ThreadPoolExecutor(max_workers=4)
        
        # Lock for thread-safe operations
        self.lock = threading.Lock()
        
        # Visual verification callbacks
        self.verification_callbacks = {}
        
        logger.info("hybrid_coordinator_initialized", 
                   mode=control_mode.value,
                   verification_level=verification_config.level.name)
    
    def register_verification(self, operation: str, callback: Callable):
        """
        Register a visual verification callback for an operation.
        
        Args:
            operation: Operation name
            callback: Verification function (image) -> bool
        """
        self.verification_callbacks[operation] = callback
        logger.debug("verification_registered", operation=operation)
    
    async def execute_operation(self,
                               operation_name: str,
                               com_func: Optional[Callable] = None,
                               vision_func: Optional[Callable] = None,
                               verification_func: Optional[Callable] = None,
                               mode: Optional[ControlMode] = None,
                               **kwargs) -> OperationResult:
        """
        Execute an operation using the specified or adaptive control mode.
        
        Args:
            operation_name: Name of the operation
            com_func: COM API function to call
            vision_func: Vision-based function to call
            verification_func: Custom verification function
            mode: Override control mode for this operation
            **kwargs: Arguments to pass to functions
            
        Returns:
            OperationResult with execution details
        """
        start_time = time.time()
        mode = mode or self.control_mode
        
        # Adaptive mode selection
        if mode == ControlMode.ADAPTIVE:
            mode = self._select_best_mode(operation_name, com_func, vision_func)
        
        logger.info("executing_operation",
                   operation=operation_name,
                   mode=mode.value)
        
        try:
            if mode == ControlMode.COM_ONLY:
                result = await self._execute_com_only(operation_name, com_func, **kwargs)
            
            elif mode == ControlMode.VISION_ONLY:
                result = await self._execute_vision_only(operation_name, vision_func, **kwargs)
            
            elif mode == ControlMode.HYBRID:
                result = await self._execute_hybrid(operation_name, com_func, vision_func,
                                                   verification_func, **kwargs)
            
            else:
                raise ValueError(f"Unknown control mode: {mode}")
            
            # Record operation history
            result.duration = time.time() - start_time
            self._record_operation(operation_name, result)
            
            return result
            
        except Exception as e:
            logger.error("operation_failed",
                        operation=operation_name,
                        error=str(e))
            return OperationResult(
                success=False,
                method_used=mode.value,
                error=str(e),
                duration=time.time() - start_time
            )
    
    async def _execute_com_only(self, operation: str, com_func: Callable,
                               **kwargs) -> OperationResult:
        """
        Execute using only COM API.
        
        Args:
            operation: Operation name
            com_func: COM function to execute
            **kwargs: Function arguments
            
        Returns:
            OperationResult
        """
        if not com_func or not self.api:
            return OperationResult(
                success=False,
                method_used="com",
                error="COM API not available"
            )
        
        try:
            # Execute COM function
            com_result = await asyncio.get_event_loop().run_in_executor(
                self.executor, com_func, **kwargs
            )
            
            # Optional visual verification
            verification_passed = True
            if self.verification_config.enabled:
                verification_passed = await self._verify_visually(
                    operation, expected_result=com_result
                )
            
            return OperationResult(
                success=True,
                method_used="com",
                com_result=com_result,
                verification_passed=verification_passed
            )
            
        except Exception as e:
            logger.error("com_execution_failed", error=str(e))
            return OperationResult(
                success=False,
                method_used="com",
                error=str(e)
            )
    
    async def _execute_vision_only(self, operation: str, vision_func: Callable,
                                  **kwargs) -> OperationResult:
        """
        Execute using only vision/GUI control.
        
        Args:
            operation: Operation name
            vision_func: Vision function to execute
            **kwargs: Function arguments
            
        Returns:
            OperationResult
        """
        if not vision_func or not self.screen_capture:
            return OperationResult(
                success=False,
                method_used="vision",
                error="Vision control not available"
            )
        
        try:
            # Capture initial state
            initial_capture = self.screen_capture.capture_window()
            
            # Execute vision function
            vision_result = await asyncio.get_event_loop().run_in_executor(
                self.executor, vision_func, initial_capture, **kwargs
            )
            
            return OperationResult(
                success=True,
                method_used="vision",
                vision_result=vision_result
            )
            
        except Exception as e:
            logger.error("vision_execution_failed", error=str(e))
            return OperationResult(
                success=False,
                method_used="vision",
                error=str(e)
            )
    
    async def _execute_hybrid(self, operation: str,
                            com_func: Optional[Callable],
                            vision_func: Optional[Callable],
                            verification_func: Optional[Callable],
                            **kwargs) -> OperationResult:
        """
        Execute using hybrid control with verification.
        
        Args:
            operation: Operation name
            com_func: COM function
            vision_func: Vision function
            verification_func: Custom verification
            **kwargs: Function arguments
            
        Returns:
            OperationResult
        """
        retries = 0
        max_retries = self.verification_config.retry_count
        
        while retries <= max_retries:
            try:
                # Try COM first if available
                if com_func and self.api:
                    com_result = await asyncio.get_event_loop().run_in_executor(
                        self.executor, com_func, **kwargs
                    )
                    
                    # Verify with vision
                    if await self._verify_visually(operation, expected_result=com_result,
                                                  custom_verifier=verification_func):
                        return OperationResult(
                            success=True,
                            method_used="hybrid",
                            com_result=com_result,
                            verification_passed=True,
                            retries=retries
                        )
                    
                    logger.warning("com_verification_failed",
                                 operation=operation,
                                 retry=retries)
                
                # Fall back to vision if COM failed or unavailable
                if vision_func and self.screen_capture:
                    capture = self.screen_capture.capture_window()
                    vision_result = await asyncio.get_event_loop().run_in_executor(
                        self.executor, vision_func, capture, **kwargs
                    )
                    
                    return OperationResult(
                        success=True,
                        method_used="hybrid",
                        vision_result=vision_result,
                        verification_passed=True,
                        retries=retries
                    )
                
                retries += 1
                if retries <= max_retries:
                    await asyncio.sleep(self.verification_config.retry_delay)
                    
            except Exception as e:
                logger.error("hybrid_execution_error",
                           operation=operation,
                           error=str(e),
                           retry=retries)
                retries += 1
        
        return OperationResult(
            success=False,
            method_used="hybrid",
            error=f"Failed after {max_retries} retries",
            retries=max_retries
        )
    
    async def _verify_visually(self, operation: str,
                              expected_result: Any = None,
                              custom_verifier: Optional[Callable] = None) -> bool:
        """
        Perform visual verification of an operation.
        
        Args:
            operation: Operation name
            expected_result: Expected result from COM
            custom_verifier: Custom verification function
            
        Returns:
            True if verification passed
        """
        if not self.screen_capture or not self.vision_analyzer:
            logger.debug("visual_verification_skipped", reason="Components not available")
            return True
        
        try:
            # Capture current state
            capture = self.screen_capture.capture_window()
            if not capture.success:
                logger.warning("capture_failed_for_verification")
                return self.verification_config.level == VerificationLevel.MINIMAL
            
            # Use custom verifier if provided
            if custom_verifier:
                return custom_verifier(capture.image, expected_result)
            
            # Use registered verifier for operation
            if operation in self.verification_callbacks:
                return self.verification_callbacks[operation](capture.image, expected_result)
            
            # Default verification based on operation type
            if "mesh" in operation.lower():
                analysis = self.vision_analyzer.analyze_mesh_quality(capture.image)
                return analysis.get("quality_score", 0) > 0.7
            
            elif "progress" in operation.lower():
                progress = self.vision_analyzer.analyze_progress(capture.image)
                return progress.get("status") == "Running"
            
            elif "results" in operation.lower():
                results = self.vision_analyzer.analyze_results(capture.image)
                return results.get("has_plots", False)
            
            # Generic verification - check for errors
            warnings = self.vision_analyzer.detect_warnings(capture.image)
            return not warnings.get("has_error", False)
            
        except Exception as e:
            logger.error("visual_verification_error", error=str(e))
            return self.verification_config.level == VerificationLevel.MINIMAL
    
    def _select_best_mode(self, operation: str,
                         com_func: Optional[Callable],
                         vision_func: Optional[Callable]) -> ControlMode:
        """
        Select the best control mode based on history and availability.
        
        Args:
            operation: Operation name
            com_func: COM function availability
            vision_func: Vision function availability
            
        Returns:
            Selected ControlMode
        """
        # Check availability
        com_available = com_func is not None and self.api is not None
        vision_available = vision_func is not None and self.screen_capture is not None
        
        if not com_available and not vision_available:
            raise ValueError("No control method available")
        
        if not com_available:
            return ControlMode.VISION_ONLY
        
        if not vision_available:
            return ControlMode.COM_ONLY
        
        # Both available - use success rates
        if self.success_rates["hybrid"] > max(self.success_rates["com"],
                                              self.success_rates["vision"]):
            return ControlMode.HYBRID
        
        if self.success_rates["com"] > self.success_rates["vision"]:
            return ControlMode.COM_ONLY
        
        return ControlMode.VISION_ONLY
    
    def _record_operation(self, operation: str, result: OperationResult):
        """
        Record operation result for adaptive learning.
        
        Args:
            operation: Operation name
            result: Operation result
        """
        with self.lock:
            self.operation_history.append({
                "operation": operation,
                "method": result.method_used,
                "success": result.success,
                "duration": result.duration,
                "timestamp": time.time()
            })
            
            # Update success rates (exponential moving average)
            alpha = 0.1  # Learning rate
            method = result.method_used.replace("_only", "")
            if method in self.success_rates:
                old_rate = self.success_rates[method]
                new_rate = old_rate * (1 - alpha) + (1.0 if result.success else 0.0) * alpha
                self.success_rates[method] = new_rate
            
            # Keep history limited
            if len(self.operation_history) > 1000:
                self.operation_history = self.operation_history[-500:]
    
    async def run_analysis_with_monitoring(self,
                                          vessel_name: str,
                                          frequencies: List[float],
                                          directions: List[float],
                                          water_depth: float = 200.0) -> Dict[str, Any]:
        """
        Run a complete analysis with visual monitoring.
        
        Args:
            vessel_name: Vessel name
            frequencies: Analysis frequencies
            directions: Wave directions
            water_depth: Water depth
            
        Returns:
            Analysis results with monitoring data
        """
        results = {
            "vessel": vessel_name,
            "status": "initializing",
            "progress": 0,
            "stages": {},
            "monitoring": []
        }
        
        try:
            # Stage 1: Create vessel
            logger.info("creating_vessel", name=vessel_name)
            vessel_result = await self.execute_operation(
                "create_vessel",
                com_func=lambda: self.api.create_vessel(vessel_name),
                verification_func=lambda img, _: self.vision_analyzer.detect_vessel_created(img)
            )
            results["stages"]["vessel_creation"] = vessel_result.success
            results["progress"] = 10
            
            # Stage 2: Setup analysis
            logger.info("setting_up_analysis")
            setup_result = await self.execute_operation(
                "setup_analysis",
                com_func=lambda: self.api.setup_diffraction_analysis(
                    frequencies, directions, water_depth
                )
            )
            results["stages"]["setup"] = setup_result.success
            results["progress"] = 20
            
            # Stage 3: Run analysis with monitoring
            logger.info("running_analysis")
            results["status"] = "running"
            
            # Start monitoring task
            monitor_task = asyncio.create_task(
                self._monitor_analysis_progress(results)
            )
            
            # Run analysis
            analysis_result = await self.execute_operation(
                "run_analysis",
                com_func=lambda: self.api.run_analysis(),
                mode=ControlMode.HYBRID
            )
            
            # Stop monitoring
            monitor_task.cancel()
            try:
                await monitor_task
            except asyncio.CancelledError:
                pass
            
            results["stages"]["analysis"] = analysis_result.success
            results["progress"] = 100
            results["status"] = "completed" if analysis_result.success else "failed"
            
            # Stage 4: Extract results
            if analysis_result.success:
                logger.info("extracting_results")
                results_data = await self.execute_operation(
                    "get_results",
                    com_func=lambda: self.api.get_results(),
                    vision_func=lambda img: self.vision_analyzer.analyze_results(img)
                )
                
                if results_data.com_result:
                    results["data"] = results_data.com_result
                elif results_data.vision_result:
                    results["data"] = results_data.vision_result
            
            return results
            
        except Exception as e:
            logger.error("analysis_failed", error=str(e))
            results["status"] = "error"
            results["error"] = str(e)
            return results
    
    async def _monitor_analysis_progress(self, results: Dict[str, Any]):
        """
        Monitor analysis progress using visual feedback.
        
        Args:
            results: Results dictionary to update
        """
        while True:
            try:
                await asyncio.sleep(self.verification_config.screenshot_interval)
                
                # Capture and analyze progress
                capture = self.screen_capture.capture_window()
                if capture.success:
                    progress_info = self.vision_analyzer.analyze_progress(capture.image)
                    
                    if progress_info:
                        results["progress"] = progress_info.get("percentage", results["progress"])
                        results["monitoring"].append({
                            "timestamp": time.time(),
                            "progress": progress_info.get("percentage"),
                            "status": progress_info.get("status"),
                            "current_frequency": progress_info.get("current_frequency")
                        })
                        
                        logger.debug("progress_update",
                                   progress=progress_info.get("percentage"),
                                   status=progress_info.get("status"))
                    
                    # Check for warnings
                    warnings = self.vision_analyzer.detect_warnings(capture.image)
                    if warnings.get("has_warning") or warnings.get("has_error"):
                        results["monitoring"].append({
                            "timestamp": time.time(),
                            "type": "warning" if warnings.get("has_warning") else "error",
                            "message": warnings.get("message", "Unknown issue detected")
                        })
                        
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error("monitoring_error", error=str(e))
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get operation statistics.
        
        Returns:
            Statistics dictionary
        """
        with self.lock:
            if not self.operation_history:
                return {
                    "total_operations": 0,
                    "success_rates": self.success_rates,
                    "average_duration": 0
                }
            
            total = len(self.operation_history)
            successful = sum(1 for op in self.operation_history if op["success"])
            avg_duration = sum(op["duration"] for op in self.operation_history) / total
            
            # Group by method
            method_stats = {}
            for method in ["com", "vision", "hybrid"]:
                method_ops = [op for op in self.operation_history
                            if op["method"].replace("_only", "") == method]
                if method_ops:
                    method_stats[method] = {
                        "count": len(method_ops),
                        "success_rate": sum(1 for op in method_ops if op["success"]) / len(method_ops),
                        "avg_duration": sum(op["duration"] for op in method_ops) / len(method_ops)
                    }
            
            return {
                "total_operations": total,
                "overall_success_rate": successful / total,
                "success_rates": self.success_rates,
                "average_duration": avg_duration,
                "method_statistics": method_stats
            }
    
    def cleanup(self):
        """
        Cleanup resources.
        """
        self.executor.shutdown(wait=False)
        if self.screen_capture:
            self.screen_capture.cleanup()
        logger.info("hybrid_coordinator_cleanup")


# Standalone test function
async def test_hybrid_coordinator():
    """Test hybrid coordination functionality"""
    coordinator = HybridCoordinator()
    
    # Test operation execution
    result = await coordinator.execute_operation(
        "test_operation",
        com_func=lambda: {"status": "success", "data": "test"},
        mode=ControlMode.COM_ONLY
    )
    
    print(f"Operation result: {result.success}")
    print(f"Method used: {result.method_used}")
    print(f"Duration: {result.duration:.2f}s")
    
    # Get statistics
    stats = coordinator.get_statistics()
    print(f"\nStatistics: {stats}")
    
    coordinator.cleanup()
    return result.success


if __name__ == "__main__":
    import asyncio
    asyncio.run(test_hybrid_coordinator())