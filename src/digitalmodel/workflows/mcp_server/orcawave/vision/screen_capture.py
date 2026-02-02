#!/usr/bin/env python3
"""
Screen Capture Module for OrcaWave
Handles window detection and screenshot capture for visual monitoring
"""

import os
import time
import base64
from typing import Optional, Dict, List, Tuple, Any
from dataclasses import dataclass
from pathlib import Path
import logging

try:
    import pyautogui
    import PIL.Image
    import PIL.ImageGrab
    import numpy as np
    import cv2
except ImportError as e:
    logging.warning(f"Optional vision dependencies not installed: {e}")
    pyautogui = None
    PIL = None
    cv2 = None

try:
    import win32gui
    import win32con
    import win32ui
    import win32api
    from ctypes import windll
except ImportError:
    logging.warning("Windows-specific modules not available")
    win32gui = None

import structlog

logger = structlog.get_logger()


@dataclass
class WindowInfo:
    """Information about a window"""
    handle: int
    title: str
    class_name: str
    rect: Tuple[int, int, int, int]  # left, top, right, bottom
    is_visible: bool
    is_minimized: bool


@dataclass
class CaptureResult:
    """Result of a screen capture operation"""
    success: bool
    image: Optional[np.ndarray] = None
    image_base64: Optional[str] = None
    window_info: Optional[WindowInfo] = None
    error: Optional[str] = None
    timestamp: float = 0.0


class OrcaWaveScreenCapture:
    """
    Screen capture functionality for OrcaWave windows
    Handles window detection, screenshot capture, and region extraction
    """
    
    def __init__(self):
        """Initialize screen capture module"""
        self.orcawave_window_handle = None
        self.last_capture_time = 0
        self.capture_cache = {}
        self.min_capture_interval = 0.1  # Minimum time between captures
        
        # OrcaWave window identifiers
        self.window_patterns = [
            "OrcaWave",
            "Orcina - OrcaWave",
            "OrcaWave Professional"
        ]
        
        # Specific dialog patterns
        self.dialog_patterns = {
            "progress": ["Analysis Progress", "Running Analysis"],
            "mesh": ["Mesh View", "Panel Mesh"],
            "results": ["Results", "Hydrodynamic Coefficients"],
            "3d_view": ["3D View", "Geometry View"]
        }
        
        logger.info("screen_capture_initialized")
    
    def find_orcawave_windows(self) -> List[WindowInfo]:
        """
        Find all OrcaWave windows
        
        Returns:
            List of WindowInfo objects for OrcaWave windows
        """
        if not win32gui:
            logger.warning("Windows API not available")
            return []
        
        windows = []
        
        def enum_callback(hwnd, results):
            """Callback for window enumeration"""
            if win32gui.IsWindowVisible(hwnd):
                window_title = win32gui.GetWindowText(hwnd)
                class_name = win32gui.GetClassName(hwnd)
                
                # Check if this is an OrcaWave window
                for pattern in self.window_patterns:
                    if pattern.lower() in window_title.lower():
                        rect = win32gui.GetWindowRect(hwnd)
                        is_minimized = win32gui.IsIconic(hwnd)
                        
                        info = WindowInfo(
                            handle=hwnd,
                            title=window_title,
                            class_name=class_name,
                            rect=rect,
                            is_visible=True,
                            is_minimized=bool(is_minimized)
                        )
                        results.append(info)
                        break
        
        try:
            win32gui.EnumWindows(enum_callback, windows)
            logger.info("found_orcawave_windows", count=len(windows))
        except Exception as e:
            logger.error("window_enumeration_failed", error=str(e))
        
        return windows
    
    def get_main_window(self) -> Optional[WindowInfo]:
        """
        Get the main OrcaWave window
        
        Returns:
            WindowInfo for main window or None
        """
        windows = self.find_orcawave_windows()
        
        # Find the main window (usually the largest or first)
        for window in windows:
            if not window.is_minimized:
                self.orcawave_window_handle = window.handle
                return window
        
        # If all minimized, return first
        if windows:
            return windows[0]
        
        return None
    
    def bring_window_to_front(self, window_handle: int) -> bool:
        """
        Bring a window to the foreground
        
        Args:
            window_handle: Window handle
            
        Returns:
            Success status
        """
        if not win32gui:
            return False
        
        try:
            # Restore if minimized
            if win32gui.IsIconic(window_handle):
                win32gui.ShowWindow(window_handle, win32con.SW_RESTORE)
            
            # Bring to front
            win32gui.SetForegroundWindow(window_handle)
            time.sleep(0.1)  # Give window time to come to front
            
            logger.info("window_brought_to_front", handle=window_handle)
            return True
            
        except Exception as e:
            logger.error("bring_to_front_failed", error=str(e))
            return False
    
    def capture_window(self, window_handle: Optional[int] = None) -> CaptureResult:
        """
        Capture screenshot of OrcaWave window
        
        Args:
            window_handle: Specific window handle or None for main window
            
        Returns:
            CaptureResult with screenshot data
        """
        # Rate limiting
        current_time = time.time()
        if current_time - self.last_capture_time < self.min_capture_interval:
            time.sleep(self.min_capture_interval)
        
        try:
            # Get window handle
            if window_handle is None:
                window = self.get_main_window()
                if not window:
                    return CaptureResult(
                        success=False,
                        error="OrcaWave window not found"
                    )
                window_handle = window.handle
            else:
                # Get window info
                rect = win32gui.GetWindowRect(window_handle)
                title = win32gui.GetWindowText(window_handle)
                window = WindowInfo(
                    handle=window_handle,
                    title=title,
                    class_name="",
                    rect=rect,
                    is_visible=True,
                    is_minimized=False
                )
            
            # Bring window to front
            self.bring_window_to_front(window_handle)
            
            # Capture using Windows API for better quality
            image = self._capture_window_api(window_handle, window.rect)
            
            if image is not None:
                # Convert to base64 for transport
                image_base64 = self._image_to_base64(image)
                
                self.last_capture_time = current_time
                
                return CaptureResult(
                    success=True,
                    image=image,
                    image_base64=image_base64,
                    window_info=window,
                    timestamp=current_time
                )
            else:
                # Fallback to pyautogui
                return self._capture_fallback(window.rect)
                
        except Exception as e:
            logger.error("capture_failed", error=str(e))
            return CaptureResult(
                success=False,
                error=str(e)
            )
    
    def _capture_window_api(self, hwnd: int, rect: Tuple[int, int, int, int]) -> Optional[np.ndarray]:
        """
        Capture window using Windows API
        
        Args:
            hwnd: Window handle
            rect: Window rectangle
            
        Returns:
            Numpy array of image or None
        """
        if not win32gui or not win32ui:
            return None
        
        try:
            left, top, right, bottom = rect
            width = right - left
            height = bottom - top
            
            # Get window device context
            hwnd_dc = win32gui.GetWindowDC(hwnd)
            mfc_dc = win32ui.CreateDCFromHandle(hwnd_dc)
            save_dc = mfc_dc.CreateCompatibleDC()
            
            # Create bitmap
            save_bitmap = win32ui.CreateBitmap()
            save_bitmap.CreateCompatibleBitmap(mfc_dc, width, height)
            save_dc.SelectObject(save_bitmap)
            
            # Copy window to bitmap
            result = windll.user32.PrintWindow(hwnd, save_dc.GetSafeHdc(), 0)
            
            if result:
                # Convert to numpy array
                bmpinfo = save_bitmap.GetInfo()
                bmpstr = save_bitmap.GetBitmapBits(True)
                
                image = np.frombuffer(bmpstr, dtype='uint8')
                image = image.reshape((height, width, 4))
                image = cv2.cvtColor(image, cv2.COLOR_BGRA2RGB)
                
                # Clean up
                win32gui.DeleteObject(save_bitmap.GetHandle())
                save_dc.DeleteDC()
                mfc_dc.DeleteDC()
                win32gui.ReleaseDC(hwnd, hwnd_dc)
                
                return image
            
        except Exception as e:
            logger.error("windows_api_capture_failed", error=str(e))
        
        return None
    
    def _capture_fallback(self, rect: Tuple[int, int, int, int]) -> CaptureResult:
        """
        Fallback capture using pyautogui
        
        Args:
            rect: Window rectangle
            
        Returns:
            CaptureResult
        """
        if not pyautogui:
            return CaptureResult(
                success=False,
                error="pyautogui not available"
            )
        
        try:
            left, top, right, bottom = rect
            width = right - left
            height = bottom - top
            
            # Capture region
            screenshot = pyautogui.screenshot(region=(left, top, width, height))
            
            # Convert to numpy array
            image = np.array(screenshot)
            image = cv2.cvtColor(image, cv2.COLOR_RGB2BGR)
            
            # Convert to base64
            image_base64 = self._image_to_base64(image)
            
            return CaptureResult(
                success=True,
                image=image,
                image_base64=image_base64,
                timestamp=time.time()
            )
            
        except Exception as e:
            logger.error("fallback_capture_failed", error=str(e))
            return CaptureResult(
                success=False,
                error=str(e)
            )
    
    def capture_region(self, region_name: str) -> CaptureResult:
        """
        Capture specific region of OrcaWave window
        
        Args:
            region_name: Name of region (3d_view, mesh, results, progress)
            
        Returns:
            CaptureResult
        """
        # First capture full window
        full_capture = self.capture_window()
        
        if not full_capture.success:
            return full_capture
        
        # Find specific dialog or region
        # This would use template matching or OCR to find specific regions
        # For now, return full capture
        logger.info("region_capture_requested", region=region_name)
        
        return full_capture
    
    def capture_progress_dialog(self) -> Optional[Dict[str, Any]]:
        """
        Capture and parse progress dialog
        
        Returns:
            Progress information or None
        """
        capture = self.capture_region("progress")
        
        if not capture.success:
            return None
        
        # Parse progress from image
        # This would use OCR or template matching
        # For now, return mock data
        return {
            "percentage": 45,
            "status": "Running analysis",
            "current_frequency": 0.75,
            "estimated_time": 120
        }
    
    def capture_3d_view(self) -> CaptureResult:
        """Capture 3D geometry view"""
        return self.capture_region("3d_view")
    
    def capture_mesh_view(self) -> CaptureResult:
        """Capture mesh visualization"""
        return self.capture_region("mesh")
    
    def capture_results_plots(self) -> CaptureResult:
        """Capture results plots"""
        return self.capture_region("results")
    
    def _image_to_base64(self, image: np.ndarray) -> str:
        """
        Convert image to base64 string
        
        Args:
            image: Numpy array image
            
        Returns:
            Base64 encoded string
        """
        try:
            # Encode as PNG
            _, buffer = cv2.imencode('.png', image)
            image_base64 = base64.b64encode(buffer).decode('utf-8')
            return f"data:image/png;base64,{image_base64}"
        except Exception as e:
            logger.error("base64_encoding_failed", error=str(e))
            return ""
    
    def save_screenshot(self, capture: CaptureResult, filepath: str) -> bool:
        """
        Save screenshot to file
        
        Args:
            capture: CaptureResult to save
            filepath: Path to save image
            
        Returns:
            Success status
        """
        if not capture.success or capture.image is None:
            return False
        
        try:
            cv2.imwrite(filepath, capture.image)
            logger.info("screenshot_saved", path=filepath)
            return True
        except Exception as e:
            logger.error("save_screenshot_failed", error=str(e))
            return False
    
    def detect_warning_dialog(self, image: np.ndarray) -> bool:
        """
        Detect if a warning or error dialog is present
        
        Args:
            image: Screenshot image
            
        Returns:
            True if warning detected
        """
        # This would use template matching or color detection
        # Look for warning colors (yellow/red) or specific dialog patterns
        # For now, simplified implementation
        
        if image is None:
            return False
        
        # Check for warning colors (simplified)
        # Yellow: RGB(255, 255, 0), Red: RGB(255, 0, 0)
        # This is a placeholder - real implementation would be more sophisticated
        
        return False
    
    def cleanup(self):
        """Cleanup resources"""
        self.capture_cache.clear()
        logger.info("screen_capture_cleanup")


# Standalone test function
def test_screen_capture():
    """Test screen capture functionality"""
    capture = OrcaWaveScreenCapture()
    
    # Find OrcaWave windows
    windows = capture.find_orcawave_windows()
    print(f"Found {len(windows)} OrcaWave windows")
    
    for window in windows:
        print(f"  - {window.title} (Handle: {window.handle})")
    
    # Capture main window
    result = capture.capture_window()
    
    if result.success:
        print(f"✓ Screenshot captured successfully")
        if result.window_info:
            print(f"  Window: {result.window_info.title}")
            print(f"  Size: {result.window_info.rect}")
        
        # Save screenshot
        capture.save_screenshot(result, "orcawave_test.png")
    else:
        print(f"✗ Screenshot failed: {result.error}")
    
    return result.success


if __name__ == "__main__":
    test_screen_capture()