"""
FreeCAD Agent - Core agent implementation following Agent OS patterns
"""

import json
import asyncio
import time
from pathlib import Path
from typing import Dict, Any, List, Optional, Callable
from datetime import datetime
from loguru import logger

from ..api.wrapper import FreeCADAPIWrapper
from ..api.geometry import GeometryHandler
from .logging_config import setup_logging, ErrorHandler, log_performance
from .capabilities import CapabilityRegistry


class FreeCADAgent:
    """
    Main FreeCAD Agent class implementing Agent OS patterns
    """
    
    def __init__(self, config_path: Optional[str] = None):
        """
        Initialize the FreeCAD Agent
        
        Args:
            config_path: Path to configuration file
        """
        self.config = self._load_config(config_path)
        self.name = self.config.get('name', 'FreeCAD Agent')
        self.version = self.config.get('version', '1.0.0')
        
        # Set up logging
        setup_logging(self.config)
        logger.info(f"Initializing {self.name} v{self.version}")
        
        # Initialize components
        self.api_wrapper = FreeCADAPIWrapper(self.config)
        self.error_handler = ErrorHandler(self.config)
        self.capability_registry = CapabilityRegistry()
        
        # Register capabilities
        self._register_capabilities()
        
        # Performance tracking
        self.metrics = {
            'operations_count': 0,
            'errors_count': 0,
            'total_time': 0,
            'start_time': datetime.now()
        }
        
        logger.info(f"{self.name} initialized successfully")
    
    def _load_config(self, config_path: Optional[str]) -> Dict[str, Any]:
        """Load configuration from file or use defaults"""
        if config_path and Path(config_path).exists():
            with open(config_path, 'r') as f:
                return json.load(f)
        
        # Try default location
        default_config = Path(__file__).parent.parent.parent / "agent_config.json"
        if default_config.exists():
            with open(default_config, 'r') as f:
                return json.load(f)
        
        # Return minimal config
        return {
            'name': 'FreeCAD Agent',
            'version': '1.0.0',
            'settings': {
                'parallel_workers': 4,
                'cache_enabled': True,
                'log_level': 'INFO'
            }
        }
    
    def _register_capabilities(self) -> None:
        """Register agent capabilities"""
        # CAD operations
        self.capability_registry.register(
            'create_box',
            self.create_box,
            "Create a 3D box with specified dimensions"
        )
        self.capability_registry.register(
            'create_cylinder',
            self.create_cylinder,
            "Create a 3D cylinder"
        )
        self.capability_registry.register(
            'create_sphere',
            self.create_sphere,
            "Create a 3D sphere"
        )
        
        # Document operations
        self.capability_registry.register(
            'new_document',
            self.new_document,
            "Create a new FreeCAD document"
        )
        self.capability_registry.register(
            'open_document',
            self.open_document,
            "Open an existing FreeCAD document"
        )
        self.capability_registry.register(
            'save_document',
            self.save_document,
            "Save the current document"
        )
        self.capability_registry.register(
            'export_document',
            self.export_document,
            "Export document to various formats"
        )
        
        # Batch operations
        self.capability_registry.register(
            'batch_process',
            self.batch_process,
            "Process multiple files in parallel"
        )
        
        # Natural language
        self.capability_registry.register(
            'execute_prompt',
            self.execute_prompt,
            "Execute natural language CAD command"
        )
        
        logger.debug(f"Registered {len(self.capability_registry.capabilities)} capabilities")
    
    # Document Operations
    
    def new_document(self, name: str = "Unnamed") -> Dict[str, Any]:
        """Create a new document"""
        start_time = time.time()
        try:
            doc = self.api_wrapper.create_document(name)
            duration = time.time() - start_time
            log_performance("new_document", duration, {"name": name})
            self.metrics['operations_count'] += 1
            
            return {
                'success': True,
                'document': name,
                'message': f"Created document: {name}"
            }
        except Exception as e:
            self.metrics['errors_count'] += 1
            return self._handle_error(e, {'operation': 'new_document', 'name': name})
    
    def open_document(self, file_path: str) -> Dict[str, Any]:
        """Open an existing document"""
        start_time = time.time()
        try:
            doc = self.api_wrapper.open_document(file_path)
            duration = time.time() - start_time
            log_performance("open_document", duration, {"file": file_path})
            self.metrics['operations_count'] += 1
            
            return {
                'success': True,
                'document': Path(file_path).stem,
                'message': f"Opened document: {file_path}"
            }
        except Exception as e:
            self.metrics['errors_count'] += 1
            return self._handle_error(e, {'operation': 'open_document', 'file': file_path})
    
    def save_document(self, file_path: Optional[str] = None) -> Dict[str, Any]:
        """Save the current document"""
        start_time = time.time()
        try:
            success = self.api_wrapper.save_document(file_path)
            duration = time.time() - start_time
            log_performance("save_document", duration, {"file": file_path})
            self.metrics['operations_count'] += 1
            
            return {
                'success': success,
                'message': f"Document saved{f' to {file_path}' if file_path else ''}"
            }
        except Exception as e:
            self.metrics['errors_count'] += 1
            return self._handle_error(e, {'operation': 'save_document'})
    
    def export_document(self, file_path: str, format: Optional[str] = None) -> Dict[str, Any]:
        """Export document to specified format"""
        start_time = time.time()
        try:
            success = self.api_wrapper.export_document(file_path, format)
            duration = time.time() - start_time
            log_performance("export_document", duration, {"file": file_path, "format": format})
            self.metrics['operations_count'] += 1
            
            return {
                'success': success,
                'file': file_path,
                'format': format or Path(file_path).suffix[1:].upper(),
                'message': f"Exported to {file_path}"
            }
        except Exception as e:
            self.metrics['errors_count'] += 1
            return self._handle_error(e, {'operation': 'export_document'})
    
    # Object Creation Operations
    
    def create_box(self, length: float, width: float, height: float,
                  position: Optional[tuple] = None, name: Optional[str] = None) -> Dict[str, Any]:
        """Create a box object"""
        start_time = time.time()
        try:
            obj = self.api_wrapper.create_box(length, width, height, position, name)
            duration = time.time() - start_time
            log_performance("create_box", duration)
            self.metrics['operations_count'] += 1
            
            return {
                'success': obj is not None,
                'object': name or "Box",
                'dimensions': {'length': length, 'width': width, 'height': height},
                'message': f"Created box: {name or 'Box'}"
            }
        except Exception as e:
            self.metrics['errors_count'] += 1
            return self._handle_error(e, {'operation': 'create_box'})
    
    def create_cylinder(self, radius: float, height: float,
                       position: Optional[tuple] = None, name: Optional[str] = None) -> Dict[str, Any]:
        """Create a cylinder object"""
        start_time = time.time()
        try:
            obj = self.api_wrapper.create_cylinder(radius, height, position, name)
            duration = time.time() - start_time
            log_performance("create_cylinder", duration)
            self.metrics['operations_count'] += 1
            
            return {
                'success': obj is not None,
                'object': name or "Cylinder",
                'dimensions': {'radius': radius, 'height': height},
                'message': f"Created cylinder: {name or 'Cylinder'}"
            }
        except Exception as e:
            self.metrics['errors_count'] += 1
            return self._handle_error(e, {'operation': 'create_cylinder'})
    
    def create_sphere(self, radius: float, position: Optional[tuple] = None,
                     name: Optional[str] = None) -> Dict[str, Any]:
        """Create a sphere object"""
        start_time = time.time()
        try:
            obj = self.api_wrapper.create_sphere(radius, position, name)
            duration = time.time() - start_time
            log_performance("create_sphere", duration)
            self.metrics['operations_count'] += 1
            
            return {
                'success': obj is not None,
                'object': name or "Sphere",
                'dimensions': {'radius': radius},
                'message': f"Created sphere: {name or 'Sphere'}"
            }
        except Exception as e:
            self.metrics['errors_count'] += 1
            return self._handle_error(e, {'operation': 'create_sphere'})
    
    # Natural Language Processing
    
    def execute_prompt(self, prompt: str) -> Dict[str, Any]:
        """
        Execute a natural language CAD command
        
        Args:
            prompt: Natural language description of the operation
            
        Returns:
            Result dictionary
        """
        logger.info(f"Executing prompt: {prompt}")
        start_time = time.time()
        
        try:
            # Parse the prompt to identify intent
            result = self._parse_and_execute_prompt(prompt)
            
            duration = time.time() - start_time
            log_performance("execute_prompt", duration, {"prompt": prompt[:100]})
            self.metrics['operations_count'] += 1
            
            return result
        except Exception as e:
            self.metrics['errors_count'] += 1
            return self._handle_error(e, {'operation': 'execute_prompt', 'prompt': prompt})
    
    def _parse_and_execute_prompt(self, prompt: str) -> Dict[str, Any]:
        """Parse and execute natural language prompt"""
        prompt_lower = prompt.lower()
        
        # Simple pattern matching for demonstration
        if 'box' in prompt_lower or 'cube' in prompt_lower:
            # Extract dimensions if provided
            import re
            numbers = re.findall(r'\d+', prompt)
            if len(numbers) >= 3:
                return self.create_box(float(numbers[0]), float(numbers[1]), float(numbers[2]))
            else:
                return self.create_box(100, 100, 100)  # Default dimensions
        
        elif 'cylinder' in prompt_lower:
            import re
            numbers = re.findall(r'\d+', prompt)
            if len(numbers) >= 2:
                return self.create_cylinder(float(numbers[0]), float(numbers[1]))
            else:
                return self.create_cylinder(50, 100)  # Default dimensions
        
        elif 'sphere' in prompt_lower:
            import re
            numbers = re.findall(r'\d+', prompt)
            if len(numbers) >= 1:
                return self.create_sphere(float(numbers[0]))
            else:
                return self.create_sphere(50)  # Default radius
        
        elif 'save' in prompt_lower:
            return self.save_document()
        
        elif 'export' in prompt_lower:
            # Look for format
            if 'step' in prompt_lower:
                return self.export_document("output.step", "STEP")
            elif 'iges' in prompt_lower:
                return self.export_document("output.iges", "IGES")
            elif 'stl' in prompt_lower:
                return self.export_document("output.stl", "STL")
            else:
                return {'success': False, 'message': 'Export format not specified'}
        
        else:
            return {
                'success': False,
                'message': f"Could not understand prompt: {prompt}",
                'hint': "Try commands like: 'Create a box 100x50x25', 'Create a cylinder radius 20 height 100'"
            }
    
    # Batch Processing
    
    async def batch_process_async(self, files: List[str], operation: Callable,
                                 parallel_workers: Optional[int] = None) -> List[Dict[str, Any]]:
        """
        Process multiple files in parallel
        
        Args:
            files: List of file paths
            operation: Operation to perform on each file
            parallel_workers: Number of parallel workers
            
        Returns:
            List of results
        """
        workers = parallel_workers or self.config.get('settings', {}).get('parallel_workers', 4)
        semaphore = asyncio.Semaphore(workers)
        
        async def process_file(file_path: str) -> Dict[str, Any]:
            async with semaphore:
                try:
                    # Run operation in thread pool to avoid blocking
                    loop = asyncio.get_event_loop()
                    result = await loop.run_in_executor(None, operation, file_path)
                    return {
                        'file': file_path,
                        'success': True,
                        'result': result
                    }
                except Exception as e:
                    logger.error(f"Error processing {file_path}: {e}")
                    return {
                        'file': file_path,
                        'success': False,
                        'error': str(e)
                    }
        
        tasks = [process_file(f) for f in files]
        results = await asyncio.gather(*tasks)
        return results
    
    def batch_process(self, pattern: str = "*.FCStd", 
                     input_directory: str = ".",
                     operation: str = "export_step",
                     parallel_workers: Optional[int] = None) -> Dict[str, Any]:
        """
        Batch process files matching pattern
        
        Args:
            pattern: File pattern to match
            input_directory: Directory to search
            operation: Operation to perform
            parallel_workers: Number of parallel workers
            
        Returns:
            Processing results
        """
        logger.info(f"Batch processing: pattern={pattern}, operation={operation}")
        start_time = time.time()
        
        try:
            # Find matching files
            from pathlib import Path
            input_dir = Path(input_directory)
            files = list(input_dir.glob(pattern))
            
            if not files:
                return {
                    'success': False,
                    'message': f"No files matching pattern: {pattern}"
                }
            
            logger.info(f"Found {len(files)} files to process")
            
            # Define operation function
            if operation == "export_step":
                def op(file_path):
                    self.open_document(str(file_path))
                    output = str(file_path).replace('.FCStd', '.step')
                    return self.export_document(output, "STEP")
            else:
                return {
                    'success': False,
                    'message': f"Unknown operation: {operation}"
                }
            
            # Process files
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)
            results = loop.run_until_complete(
                self.batch_process_async([str(f) for f in files], op, parallel_workers)
            )
            
            duration = time.time() - start_time
            log_performance("batch_process", duration, {
                "files_count": len(files),
                "operation": operation
            })
            
            # Summary
            successful = sum(1 for r in results if r['success'])
            failed = len(results) - successful
            
            return {
                'success': True,
                'total_files': len(files),
                'successful': successful,
                'failed': failed,
                'duration': duration,
                'results': results
            }
            
        except Exception as e:
            self.metrics['errors_count'] += 1
            return self._handle_error(e, {'operation': 'batch_process'})
    
    # Utility Methods
    
    def _handle_error(self, error: Exception, context: Dict[str, Any]) -> Dict[str, Any]:
        """Handle errors uniformly"""
        recovery = self.error_handler.handle_error(error, context)
        
        if recovery:
            return {
                'success': True,
                'recovered': True,
                'result': recovery
            }
        
        return {
            'success': False,
            'error': str(error),
            'context': context
        }
    
    def get_capabilities(self) -> List[str]:
        """Get list of agent capabilities"""
        return list(self.capability_registry.capabilities.keys())
    
    def get_metrics(self) -> Dict[str, Any]:
        """Get performance metrics"""
        uptime = (datetime.now() - self.metrics['start_time']).total_seconds()
        
        return {
            'uptime_seconds': uptime,
            'operations_count': self.metrics['operations_count'],
            'errors_count': self.metrics['errors_count'],
            'error_rate': self.metrics['errors_count'] / max(self.metrics['operations_count'], 1),
            'avg_time_per_operation': self.metrics['total_time'] / max(self.metrics['operations_count'], 1)
        }
    
    def show_capabilities(self) -> None:
        """Display agent capabilities"""
        print(f"\n{self.name} v{self.version} - Capabilities:\n")
        print("-" * 60)
        
        for name, (func, description) in self.capability_registry.capabilities.items():
            print(f"  • {name}: {description}")
        
        print("-" * 60)
        print(f"\nTotal capabilities: {len(self.capability_registry.capabilities)}")
        
        if self.api_wrapper.freecad_available:
            print("Status: FreeCAD API available ✓")
        else:
            print("Status: Running in mock mode (FreeCAD not available)")
    
    def __repr__(self) -> str:
        return f"<{self.name} v{self.version} - {len(self.capability_registry.capabilities)} capabilities>"