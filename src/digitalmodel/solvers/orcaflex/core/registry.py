"""
Component Registry System with Factory Pattern

This module implements a registry system for dynamic component discovery and instantiation.
It uses the factory pattern to create instances of registered components on demand.
"""

import logging
from typing import Any, Dict, List, Optional, Type, Union, Callable
from enum import Enum
import inspect

from .interfaces import (
    AnalyzerInterface,
    ProcessorInterface,
    ExtractorInterface,
    WorkflowInterface
)
from .exceptions import ComponentNotFoundError, RegistrationError


class ComponentType(Enum):
    """Enumeration of component types."""
    ANALYZER = "analyzer"
    PROCESSOR = "processor"
    EXTRACTOR = "extractor"
    WORKFLOW = "workflow"
    VALIDATOR = "validator"
    CUSTOM = "custom"


class ComponentRegistry:
    """
    Singleton registry for managing OrcaFlex components.
    
    This registry provides:
    - Dynamic component registration
    - Factory-based instantiation
    - Component discovery and querying
    - Dependency injection support
    """
    
    _instance = None
    _registry: Dict[str, Dict[str, Any]] = {}
    _logger = logging.getLogger("ComponentRegistry")
    
    def __new__(cls):
        """Ensure singleton pattern."""
        if cls._instance is None:
            cls._instance = super(ComponentRegistry, cls).__new__(cls)
            cls._instance._initialize()
        return cls._instance
    
    def _initialize(self):
        """Initialize the registry."""
        self._registry = {
            ComponentType.ANALYZER.value: {},
            ComponentType.PROCESSOR.value: {},
            ComponentType.EXTRACTOR.value: {},
            ComponentType.WORKFLOW.value: {},
            ComponentType.VALIDATOR.value: {},
            ComponentType.CUSTOM.value: {}
        }
        self._logger.info("Component registry initialized")
    
    def register(self,
                 component_class: Type,
                 component_type: ComponentType,
                 name: Optional[str] = None,
                 version: str = "1.0.0",
                 metadata: Optional[Dict[str, Any]] = None) -> bool:
        """
        Register a component class in the registry.
        
        Args:
            component_class: The class to register
            component_type: Type of component
            name: Optional name (defaults to class name)
            version: Component version
            metadata: Additional metadata
            
        Returns:
            True if registration successful
            
        Raises:
            RegistrationError: If registration fails
        """
        if name is None:
            name = component_class.__name__
        
        # Validate component implements correct interface
        if not self._validate_component(component_class, component_type):
            raise RegistrationError(
                f"Component {name} does not implement required interface for {component_type.value}"
            )
        
        # Check for existing registration
        if name in self._registry[component_type.value]:
            existing_version = self._registry[component_type.value][name].get('version', 'unknown')
            self._logger.warning(
                f"Overwriting existing component: {name} v{existing_version} with v{version}"
            )
        
        # Register the component
        self._registry[component_type.value][name] = {
            'class': component_class,
            'version': version,
            'metadata': metadata or {},
            'instances': 0,
            'factory': self._create_factory(component_class)
        }
        
        self._logger.info(f"Registered {component_type.value}: {name} v{version}")
        return True
    
    def get(self,
            name: str,
            component_type: ComponentType,
            *args,
            **kwargs) -> Any:
        """
        Get an instance of a registered component.
        
        Args:
            name: Component name
            component_type: Type of component
            *args: Positional arguments for component constructor
            **kwargs: Keyword arguments for component constructor
            
        Returns:
            Instance of the requested component
            
        Raises:
            ComponentNotFoundError: If component not found
        """
        if component_type.value not in self._registry:
            raise ComponentNotFoundError(f"Unknown component type: {component_type.value}")
        
        if name not in self._registry[component_type.value]:
            available = list(self._registry[component_type.value].keys())
            raise ComponentNotFoundError(
                f"Component '{name}' not found. Available: {available}"
            )
        
        component_info = self._registry[component_type.value][name]
        factory = component_info['factory']
        
        # Create instance using factory
        instance = factory(*args, **kwargs)
        
        # Track instance count
        component_info['instances'] += 1
        
        self._logger.debug(
            f"Created instance of {name} (total instances: {component_info['instances']})"
        )
        
        return instance
    
    def list_components(self, component_type: Optional[ComponentType] = None) -> Dict[str, List[str]]:
        """
        List all registered components.
        
        Args:
            component_type: Optional filter by type
            
        Returns:
            Dictionary of component types and their registered components
        """
        if component_type:
            return {
                component_type.value: list(self._registry[component_type.value].keys())
            }
        
        return {
            comp_type: list(components.keys())
            for comp_type, components in self._registry.items()
            if components
        }
    
    def get_component_info(self, name: str, component_type: ComponentType) -> Dict[str, Any]:
        """
        Get detailed information about a component.
        
        Args:
            name: Component name
            component_type: Type of component
            
        Returns:
            Component information dictionary
        """
        if name not in self._registry.get(component_type.value, {}):
            raise ComponentNotFoundError(f"Component '{name}' not found")
        
        info = self._registry[component_type.value][name].copy()
        # Don't include the actual class or factory in the info
        info.pop('class', None)
        info.pop('factory', None)
        
        return info
    
    def unregister(self, name: str, component_type: ComponentType) -> bool:
        """
        Unregister a component.
        
        Args:
            name: Component name
            component_type: Type of component
            
        Returns:
            True if unregistration successful
        """
        if name in self._registry.get(component_type.value, {}):
            del self._registry[component_type.value][name]
            self._logger.info(f"Unregistered {component_type.value}: {name}")
            return True
        
        self._logger.warning(f"Component not found for unregistration: {name}")
        return False
    
    def clear(self, component_type: Optional[ComponentType] = None):
        """
        Clear registry.
        
        Args:
            component_type: Optional - clear only specific type
        """
        if component_type:
            self._registry[component_type.value].clear()
            self._logger.info(f"Cleared {component_type.value} registry")
        else:
            for comp_type in self._registry:
                self._registry[comp_type].clear()
            self._logger.info("Cleared entire registry")
    
    def _validate_component(self, component_class: Type, component_type: ComponentType) -> bool:
        """
        Validate that component implements required interface.
        
        Args:
            component_class: Class to validate
            component_type: Expected component type
            
        Returns:
            True if valid
        """
        interface_map = {
            ComponentType.ANALYZER: AnalyzerInterface,
            ComponentType.PROCESSOR: ProcessorInterface,
            ComponentType.EXTRACTOR: ExtractorInterface,
            ComponentType.WORKFLOW: WorkflowInterface
        }
        
        required_interface = interface_map.get(component_type)
        
        if required_interface:
            return issubclass(component_class, required_interface)
        
        # For custom components, just check it's a class
        return inspect.isclass(component_class)
    
    def _create_factory(self, component_class: Type) -> Callable:
        """
        Create a factory function for a component class.
        
        Args:
            component_class: Class to create factory for
            
        Returns:
            Factory function
        """
        def factory(*args, **kwargs):
            """Factory function for creating component instances."""
            return component_class(*args, **kwargs)
        
        return factory


# Global registry instance
_global_registry = ComponentRegistry()


def register_component(component_class: Type,
                      component_type: ComponentType,
                      name: Optional[str] = None,
                      version: str = "1.0.0",
                      metadata: Optional[Dict[str, Any]] = None) -> bool:
    """
    Convenience function to register a component in the global registry.
    
    This can also be used as a decorator:
    
    @register_component(component_type=ComponentType.ANALYZER)
    class MyAnalyzer(BaseAnalyzer):
        pass
    """
    def decorator(cls):
        _global_registry.register(cls, component_type, name, version, metadata)
        return cls
    
    if component_class is None:
        # Being used as a decorator
        return decorator
    else:
        # Direct registration
        return _global_registry.register(component_class, component_type, name, version, metadata)


def get_component(name: str, component_type: ComponentType, *args, **kwargs) -> Any:
    """
    Convenience function to get a component from the global registry.
    """
    return _global_registry.get(name, component_type, *args, **kwargs)


def list_components(component_type: Optional[ComponentType] = None) -> Dict[str, List[str]]:
    """
    Convenience function to list components in the global registry.
    """
    return _global_registry.list_components(component_type)


def get_registry() -> ComponentRegistry:
    """
    Get the global registry instance.
    """
    return _global_registry