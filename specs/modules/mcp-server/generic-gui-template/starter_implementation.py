#!/usr/bin/env python3
"""
Generic MCP Server Template - Starter Implementation
Version: 1.0.0
Last Updated: 2025-08-25

This is a production-ready starter template for creating MCP servers
that interact with GUI applications using vision-based analysis.

Usage:
    python starter_implementation.py --program "YourApp" --config config.yml

Requirements:
    - FastMCP >= 2.0.5
    - Python >= 3.11
    - See pyproject.toml for full dependencies
"""

import asyncio
import os
from pathlib import Path
from typing import Dict, List, Optional, Any
from datetime import datetime
import json
import hashlib
from enum import Enum
from abc import ABC, abstractmethod

# FastMCP imports
from fastmcp import FastMCP, Context
from fastmcp.security import SecurityMiddleware
from fastmcp.caching import MultiTierCache
from fastmcp.monitoring import OpenTelemetryMiddleware

# GUI automation imports
import pyautogui
import PIL.Image
import cv2
import numpy as np

# Utilities
import structlog
from pydantic import BaseModel, Field
import redis
import yaml
from tenacity import retry, stop_after_attempt, wait_exponential

# Configure structured logging
structlog.configure(
    processors=[
        structlog.stdlib.filter_by_level,
        structlog.stdlib.add_logger_name,
        structlog.stdlib.add_log_level,
        structlog.stdlib.PositionalArgumentsFormatter(),
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.StackInfoRenderer(),
        structlog.processors.format_exc_info,
        structlog.processors.UnicodeDecoder(),
        structlog.processors.JSONRenderer()
    ],
    context_class=dict,
    logger_factory=structlog.stdlib.LoggerFactory(),
    cache_logger_on_first_use=True,
)

logger = structlog.get_logger()

# ============================================================================
# Configuration Models
# ============================================================================

class CaptureMode(str, Enum):
    FULL_SCREEN = "full_screen"
    WINDOW = "window"
    REGION = "region"

class VisionProvider(str, Enum):
    GPT4 = "gpt4"
    CLAUDE = "claude"
    LOCAL = "local"
    OLLAMA = "ollama"

class ServerConfig(BaseModel):
    """MCP Server configuration"""
    name: str = Field(default="gui-mcp-server")
    version: str = Field(default="1.0.0")
    host: str = Field(default="localhost")
    port: int = Field(default=3000)
    workers: int = Field(default=4)
    ssl_enabled: bool = Field(default=False)
    ssl_cert: Optional[str] = None
    ssl_key: Optional[str] = None

class SecurityConfig(BaseModel):
    """Security configuration"""
    enable_sandboxing: bool = Field(default=True)
    enable_rate_limiting: bool = Field(default=True)
    max_requests_per_minute: int = Field(default=100)
    enable_audit_logging: bool = Field(default=True)
    allowed_origins: List[str] = Field(default_factory=list)
    require_auth: bool = Field(default=False)

class CacheConfig(BaseModel):
    """Cache configuration"""
    l1_size: int = Field(default=1000)
    l2_redis_url: str = Field(default="redis://localhost:6379")
    l3_cdn_provider: Optional[str] = None
    default_ttl: int = Field(default=60)

class VisionConfig(BaseModel):
    """Vision provider configuration"""
    provider: VisionProvider = Field(default=VisionProvider.GPT4)
    api_key: Optional[str] = None
    model: str = Field(default="gpt-4-vision-preview")
    max_tokens: int = Field(default=1000)
    temperature: float = Field(default=0.3)
    confidence_threshold: float = Field(default=0.8)

class ProgramConfig(BaseModel):
    """Target program configuration"""
    name: str
    executable: Optional[str] = None
    window_title: Optional[str] = None
    capture_mode: CaptureMode = Field(default=CaptureMode.WINDOW)
    shortcuts: Dict[str, str] = Field(default_factory=dict)

class AppConfig(BaseModel):
    """Complete application configuration"""
    server: ServerConfig = Field(default_factory=ServerConfig)
    security: SecurityConfig = Field(default_factory=SecurityConfig)
    cache: CacheConfig = Field(default_factory=CacheConfig)
    vision: VisionConfig = Field(default_factory=VisionConfig)
    program: ProgramConfig

# ============================================================================
# Vision Provider Interface
# ============================================================================

class VisionProviderBase(ABC):
    """Base class for vision providers"""
    
    @abstractmethod
    async def analyze(self, image: bytes, prompt: str) -> Dict[str, Any]:
        """Analyze an image with the given prompt"""
        pass
    
    @abstractmethod
    async def detect_elements(self, image: bytes) -> List[Dict[str, Any]]:
        """Detect UI elements in the image"""
        pass
    
    @abstractmethod
    async def extract_text(self, image: bytes) -> str:
        """Extract text from the image using OCR"""
        pass

class GPT4VisionProvider(VisionProviderBase):
    """GPT-4 Vision API provider"""
    
    def __init__(self, config: VisionConfig):
        self.config = config
        # Initialize OpenAI client here
        
    @retry(stop=stop_after_attempt(3), wait=wait_exponential(multiplier=1, min=4, max=10))
    async def analyze(self, image: bytes, prompt: str) -> Dict[str, Any]:
        """Analyze image using GPT-4 Vision"""
        # Implement GPT-4 Vision API call
        logger.info("analyzing_with_gpt4", prompt=prompt[:50])
        return {
            "description": "Mock analysis result",
            "elements": [],
            "confidence": 0.95
        }
    
    async def detect_elements(self, image: bytes) -> List[Dict[str, Any]]:
        """Detect UI elements using GPT-4 Vision"""
        prompt = """Identify all UI elements in this image. For each element provide:
        - Type (button, input, menu, etc.)
        - Label or text
        - Approximate location (top-left x,y and width,height)
        - State (enabled, selected, etc.)
        Format as JSON array."""
        
        result = await self.analyze(image, prompt)
        return result.get("elements", [])
    
    async def extract_text(self, image: bytes) -> str:
        """Extract text using GPT-4 Vision"""
        prompt = "Extract all visible text from this image."
        result = await self.analyze(image, prompt)
        return result.get("text", "")

class LocalVisionProvider(VisionProviderBase):
    """Local vision processing using OpenCV and Tesseract"""
    
    def __init__(self, config: VisionConfig):
        self.config = config
        
    async def analyze(self, image: bytes, prompt: str) -> Dict[str, Any]:
        """Analyze using local computer vision"""
        # Convert bytes to numpy array
        nparr = np.frombuffer(image, np.uint8)
        img = cv2.imdecode(nparr, cv2.IMREAD_COLOR)
        
        # Perform basic analysis
        gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
        edges = cv2.Canny(gray, 50, 150)
        
        return {
            "description": "Local analysis complete",
            "elements": [],
            "confidence": 0.75
        }
    
    async def detect_elements(self, image: bytes) -> List[Dict[str, Any]]:
        """Detect elements using OpenCV"""
        # Implement contour detection and element classification
        return []
    
    async def extract_text(self, image: bytes) -> str:
        """Extract text using Tesseract OCR"""
        # Implement Tesseract OCR
        return ""

# ============================================================================
# Screen Capture Module
# ============================================================================

class ScreenCapture:
    """Handle screen capture operations"""
    
    def __init__(self, config: ProgramConfig):
        self.config = config
        
    async def capture(self, mode: Optional[CaptureMode] = None) -> bytes:
        """Capture screen based on mode"""
        mode = mode or self.config.capture_mode
        
        if mode == CaptureMode.FULL_SCREEN:
            screenshot = pyautogui.screenshot()
        elif mode == CaptureMode.WINDOW:
            # Find window by title and capture
            screenshot = await self._capture_window()
        elif mode == CaptureMode.REGION:
            # Capture specific region
            screenshot = await self._capture_region()
        else:
            screenshot = pyautogui.screenshot()
        
        # Convert to bytes
        import io
        buffer = io.BytesIO()
        screenshot.save(buffer, format='PNG')
        return buffer.getvalue()
    
    async def _capture_window(self) -> PIL.Image.Image:
        """Capture specific window"""
        # Platform-specific window capture
        # This is a simplified version
        return pyautogui.screenshot()
    
    async def _capture_region(self, x: int = 0, y: int = 0, 
                             width: int = 800, height: int = 600) -> PIL.Image.Image:
        """Capture specific region"""
        return pyautogui.screenshot(region=(x, y, width, height))

# ============================================================================
# GUI Controller
# ============================================================================

class GUIController:
    """Handle GUI automation operations"""
    
    def __init__(self, config: ProgramConfig):
        self.config = config
        pyautogui.FAILSAFE = True
        pyautogui.PAUSE = 0.1
        
    async def click(self, x: int, y: int, button: str = 'left', 
                   clicks: int = 1, interval: float = 0.0) -> bool:
        """Perform mouse click"""
        try:
            pyautogui.click(x, y, button=button, clicks=clicks, interval=interval)
            logger.info("click_performed", x=x, y=y, button=button)
            return True
        except Exception as e:
            logger.error("click_failed", error=str(e))
            return False
    
    async def type_text(self, text: str, interval: float = 0.0) -> bool:
        """Type text using keyboard"""
        try:
            pyautogui.typewrite(text, interval=interval)
            logger.info("text_typed", length=len(text))
            return True
        except Exception as e:
            logger.error("type_failed", error=str(e))
            return False
    
    async def press_key(self, key: str) -> bool:
        """Press a single key or key combination"""
        try:
            if '+' in key:
                # Handle key combinations like 'ctrl+s'
                pyautogui.hotkey(*key.split('+'))
            else:
                pyautogui.press(key)
            logger.info("key_pressed", key=key)
            return True
        except Exception as e:
            logger.error("key_press_failed", error=str(e))
            return False
    
    async def move_to(self, x: int, y: int, duration: float = 0.5) -> bool:
        """Move mouse to position"""
        try:
            pyautogui.moveTo(x, y, duration=duration)
            return True
        except Exception as e:
            logger.error("move_failed", error=str(e))
            return False

# ============================================================================
# Main MCP Server Implementation
# ============================================================================

class GenericMCPServer:
    """Generic MCP server for GUI automation"""
    
    def __init__(self, config_path: str):
        # Load configuration
        self.config = self._load_config(config_path)
        
        # Initialize FastMCP server
        self.mcp = FastMCP(
            name=self.config.server.name,
            version=self.config.server.version,
            description=f"MCP server for {self.config.program.name}"
        )
        
        # Add middleware
        self._setup_middleware()
        
        # Initialize components
        self.screen_capture = ScreenCapture(self.config.program)
        self.gui_controller = GUIController(self.config.program)
        self.vision_provider = self._create_vision_provider()
        self.cache = self._setup_cache()
        
        # Register MCP endpoints
        self._register_resources()
        self._register_tools()
        self._register_prompts()
        
        logger.info("server_initialized", config=self.config.server.name)
    
    def _load_config(self, config_path: str) -> AppConfig:
        """Load configuration from file"""
        with open(config_path, 'r') as f:
            config_data = yaml.safe_load(f)
        return AppConfig(**config_data)
    
    def _setup_middleware(self):
        """Setup server middleware"""
        # Security middleware
        self.mcp.add_middleware(SecurityMiddleware(
            enable_sandboxing=self.config.security.enable_sandboxing,
            enable_rate_limiting=self.config.security.enable_rate_limiting,
            max_requests_per_minute=self.config.security.max_requests_per_minute,
            enable_audit_logging=self.config.security.enable_audit_logging
        ))
        
        # Monitoring middleware
        self.mcp.add_middleware(OpenTelemetryMiddleware(
            service_name=self.config.server.name,
            enable_tracing=True,
            enable_metrics=True
        ))
    
    def _create_vision_provider(self) -> VisionProviderBase:
        """Create vision provider based on config"""
        if self.config.vision.provider == VisionProvider.GPT4:
            return GPT4VisionProvider(self.config.vision)
        elif self.config.vision.provider == VisionProvider.LOCAL:
            return LocalVisionProvider(self.config.vision)
        else:
            return LocalVisionProvider(self.config.vision)
    
    def _setup_cache(self) -> MultiTierCache:
        """Setup multi-tier cache"""
        return MultiTierCache(
            l1_size=self.config.cache.l1_size,
            l2_redis_url=self.config.cache.l2_redis_url,
            l3_cdn_config={"provider": self.config.cache.l3_cdn_provider}
        )
    
    def _register_resources(self):
        """Register MCP resources"""
        
        @self.mcp.resource("screenshot://current")
        async def get_current_screenshot(ctx: Context) -> Dict:
            """Get current screenshot with caching"""
            cache_key = f"screenshot_{ctx.session_id}_{datetime.now().minute}"
            
            # Try cache first
            if cached := await self.cache.get(cache_key):
                logger.info("screenshot_cache_hit", session=ctx.session_id)
                return cached
            
            # Capture new screenshot
            screenshot = await self.screen_capture.capture()
            
            result = {
                "data": screenshot.hex(),  # Convert to hex for JSON
                "format": "png",
                "timestamp": datetime.now().isoformat(),
                "mode": self.config.program.capture_mode.value
            }
            
            # Cache with TTL
            await self.cache.set(cache_key, result, ttl=5)
            
            return result
        
        @self.mcp.resource("ui://elements")
        async def get_ui_elements(ctx: Context) -> List[Dict]:
            """Get detected UI elements"""
            # Get screenshot
            screenshot_data = await get_current_screenshot(ctx)
            screenshot = bytes.fromhex(screenshot_data["data"])
            
            # Detect elements
            elements = await self.vision_provider.detect_elements(screenshot)
            
            return elements
        
        @self.mcp.resource("state://application")
        async def get_application_state(ctx: Context) -> Dict:
            """Get current application state"""
            return {
                "program": self.config.program.name,
                "window_title": self.config.program.window_title,
                "capture_mode": self.config.program.capture_mode.value,
                "session_id": ctx.session_id,
                "timestamp": datetime.now().isoformat()
            }
    
    def _register_tools(self):
        """Register MCP tools"""
        
        @self.mcp.tool("click")
        async def click(x: int, y: int, button: str = "left", ctx: Context = None) -> Dict:
            """Click at specified coordinates"""
            success = await self.gui_controller.click(x, y, button)
            return {
                "success": success,
                "action": "click",
                "coordinates": {"x": x, "y": y},
                "button": button
            }
        
        @self.mcp.tool("type")
        async def type_text(text: str, ctx: Context = None) -> Dict:
            """Type text at current cursor position"""
            success = await self.gui_controller.type_text(text)
            return {
                "success": success,
                "action": "type",
                "text_length": len(text)
            }
        
        @self.mcp.tool("press_key")
        async def press_key(key: str, ctx: Context = None) -> Dict:
            """Press a key or key combination"""
            success = await self.gui_controller.press_key(key)
            return {
                "success": success,
                "action": "press_key",
                "key": key
            }
        
        @self.mcp.tool("analyze")
        async def analyze(prompt: str, ctx: Context = None) -> Dict:
            """Analyze current screen with AI vision"""
            # Get screenshot
            screenshot_data = await self.mcp.resources["screenshot://current"](ctx)
            screenshot = bytes.fromhex(screenshot_data["data"])
            
            # Analyze with vision
            result = await self.vision_provider.analyze(screenshot, prompt)
            
            return {
                "analysis": result,
                "prompt": prompt,
                "timestamp": datetime.now().isoformat()
            }
        
        @self.mcp.tool("find_and_click")
        async def find_and_click(description: str, ctx: Context = None) -> Dict:
            """Find UI element by description and click it"""
            # Get elements
            elements = await self.mcp.resources["ui://elements"](ctx)
            
            # Find matching element (simplified)
            for element in elements:
                if description.lower() in element.get("label", "").lower():
                    x = element.get("x", 0) + element.get("width", 0) // 2
                    y = element.get("y", 0) + element.get("height", 0) // 2
                    success = await self.gui_controller.click(x, y)
                    return {
                        "success": success,
                        "element": element,
                        "action": "click"
                    }
            
            return {
                "success": False,
                "error": f"Element '{description}' not found"
            }
    
    def _register_prompts(self):
        """Register MCP prompts"""
        
        @self.mcp.prompt("understand_ui")
        def understand_ui_prompt() -> str:
            """Prompt for UI understanding"""
            return """Analyze the current screen and provide:
            1. Main UI components visible
            2. Available actions the user can take
            3. Current application state
            4. Any dialogs or notifications present
            Format as structured JSON."""
        
        @self.mcp.prompt("find_element")
        def find_element_prompt(description: str) -> str:
            """Prompt for element location"""
            return f"""Find the UI element matching: '{description}'
            Return:
            - Element type (button, input, menu, etc.)
            - Location (coordinates or path)
            - Current state (enabled, selected, etc.)
            - Suggested interaction method"""
    
    async def health_check(self) -> Dict:
        """Health check endpoint"""
        return {
            "status": "healthy",
            "server": self.config.server.name,
            "version": self.config.server.version,
            "timestamp": datetime.now().isoformat()
        }
    
    def run(self):
        """Start the MCP server"""
        logger.info("starting_server", 
                   host=self.config.server.host, 
                   port=self.config.server.port)
        
        # Add health check
        @self.mcp.health_check
        async def health():
            return await self.health_check()
        
        # Run server
        if self.config.server.ssl_enabled:
            self.mcp.run(
                host=self.config.server.host,
                port=self.config.server.port,
                workers=self.config.server.workers,
                ssl_certfile=self.config.server.ssl_cert,
                ssl_keyfile=self.config.server.ssl_key,
                reload=False
            )
        else:
            self.mcp.run(
                host=self.config.server.host,
                port=self.config.server.port,
                workers=self.config.server.workers,
                reload=False
            )

# ============================================================================
# CLI Entry Point
# ============================================================================

def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="Generic MCP Server for GUI Automation"
    )
    parser.add_argument(
        "--config",
        type=str,
        default="config/default.yml",
        help="Path to configuration file"
    )
    parser.add_argument(
        "--program",
        type=str,
        help="Override program name in config"
    )
    parser.add_argument(
        "--port",
        type=int,
        help="Override server port"
    )
    
    args = parser.parse_args()
    
    # Create and run server
    server = GenericMCPServer(args.config)
    
    # Override config if needed
    if args.program:
        server.config.program.name = args.program
    if args.port:
        server.config.server.port = args.port
    
    # Start server
    try:
        server.run()
    except KeyboardInterrupt:
        logger.info("server_shutdown")
    except Exception as e:
        logger.error("server_error", error=str(e), exc_info=True)

if __name__ == "__main__":
    main()