"""
Capability Registry for FreeCAD Agent
Manages and tracks agent capabilities
"""

from typing import Dict, Callable, Any, List, Optional, Tuple
from loguru import logger


class CapabilityRegistry:
    """
    Registry for agent capabilities
    Provides a centralized way to manage and discover agent functions
    """
    
    def __init__(self):
        """Initialize the capability registry"""
        self.capabilities: Dict[str, Tuple[Callable, str]] = {}
        self.categories: Dict[str, List[str]] = {
            'document': [],
            'geometry': [],
            'sketch': [],
            'assembly': [],
            'fem': [],
            'batch': [],
            'nlp': [],
            'marine': [],
            'export': [],
            'utility': []
        }
    
    def register(self, name: str, function: Callable, description: str,
                category: Optional[str] = None) -> None:
        """
        Register a new capability
        
        Args:
            name: Capability name
            function: Function to execute
            description: Human-readable description
            category: Optional category for grouping
        """
        if name in self.capabilities:
            logger.warning(f"Capability '{name}' already registered, overwriting")
        
        self.capabilities[name] = (function, description)
        
        # Add to category if specified
        if category and category in self.categories:
            if name not in self.categories[category]:
                self.categories[category].append(name)
        
        logger.debug(f"Registered capability: {name}")
    
    def unregister(self, name: str) -> bool:
        """
        Unregister a capability
        
        Args:
            name: Capability name to remove
            
        Returns:
            True if removed, False if not found
        """
        if name in self.capabilities:
            del self.capabilities[name]
            
            # Remove from categories
            for category in self.categories.values():
                if name in category:
                    category.remove(name)
            
            logger.debug(f"Unregistered capability: {name}")
            return True
        
        return False
    
    def get_capability(self, name: str) -> Optional[Callable]:
        """
        Get a capability function by name
        
        Args:
            name: Capability name
            
        Returns:
            Function if found, None otherwise
        """
        if name in self.capabilities:
            return self.capabilities[name][0]
        return None
    
    def get_description(self, name: str) -> Optional[str]:
        """
        Get capability description
        
        Args:
            name: Capability name
            
        Returns:
            Description if found, None otherwise
        """
        if name in self.capabilities:
            return self.capabilities[name][1]
        return None
    
    def list_capabilities(self, category: Optional[str] = None) -> List[str]:
        """
        List all registered capabilities
        
        Args:
            category: Optional category filter
            
        Returns:
            List of capability names
        """
        if category and category in self.categories:
            return self.categories[category]
        return list(self.capabilities.keys())
    
    def get_categories(self) -> Dict[str, List[str]]:
        """
        Get all categories with their capabilities
        
        Returns:
            Dictionary of categories and capability lists
        """
        return {k: v for k, v in self.categories.items() if v}
    
    def execute(self, name: str, *args, **kwargs) -> Any:
        """
        Execute a capability by name
        
        Args:
            name: Capability name
            *args: Positional arguments
            **kwargs: Keyword arguments
            
        Returns:
            Result of capability execution
            
        Raises:
            ValueError: If capability not found
        """
        function = self.get_capability(name)
        if not function:
            raise ValueError(f"Capability '{name}' not found")
        
        logger.debug(f"Executing capability: {name}")
        return function(*args, **kwargs)
    
    def search(self, query: str) -> List[Tuple[str, str]]:
        """
        Search capabilities by name or description
        
        Args:
            query: Search query (case-insensitive)
            
        Returns:
            List of matching (name, description) tuples
        """
        query_lower = query.lower()
        matches = []
        
        for name, (func, description) in self.capabilities.items():
            if query_lower in name.lower() or query_lower in description.lower():
                matches.append((name, description))
        
        return matches
    
    def validate_capability(self, name: str) -> bool:
        """
        Check if a capability exists
        
        Args:
            name: Capability name
            
        Returns:
            True if capability exists
        """
        return name in self.capabilities
    
    def get_capability_info(self, name: str) -> Optional[Dict[str, Any]]:
        """
        Get detailed information about a capability
        
        Args:
            name: Capability name
            
        Returns:
            Dictionary with capability information
        """
        if name not in self.capabilities:
            return None
        
        func, description = self.capabilities[name]
        
        # Find category
        capability_category = None
        for cat, caps in self.categories.items():
            if name in caps:
                capability_category = cat
                break
        
        return {
            'name': name,
            'description': description,
            'category': capability_category,
            'function': func.__name__ if hasattr(func, '__name__') else str(func),
            'module': func.__module__ if hasattr(func, '__module__') else None,
            'signature': str(func.__annotations__) if hasattr(func, '__annotations__') else None
        }
    
    def bulk_register(self, capabilities: Dict[str, Tuple[Callable, str, Optional[str]]]) -> None:
        """
        Register multiple capabilities at once
        
        Args:
            capabilities: Dictionary of name -> (function, description, category)
        """
        for name, (func, desc, cat) in capabilities.items():
            self.register(name, func, desc, cat)
    
    def to_dict(self) -> Dict[str, Any]:
        """
        Export registry as dictionary
        
        Returns:
            Dictionary representation of the registry
        """
        return {
            'capabilities': {
                name: {
                    'description': desc,
                    'category': self._find_category(name)
                }
                for name, (func, desc) in self.capabilities.items()
            },
            'categories': self.get_categories(),
            'total_count': len(self.capabilities)
        }
    
    def _find_category(self, capability_name: str) -> Optional[str]:
        """Find the category of a capability"""
        for cat, caps in self.categories.items():
            if capability_name in caps:
                return cat
        return None
    
    def clear(self) -> None:
        """Clear all registered capabilities"""
        self.capabilities.clear()
        for category in self.categories:
            self.categories[category].clear()
        logger.debug("Cleared all capabilities")
    
    def __len__(self) -> int:
        """Return the number of registered capabilities"""
        return len(self.capabilities)
    
    def __contains__(self, name: str) -> bool:
        """Check if a capability is registered"""
        return name in self.capabilities
    
    def __repr__(self) -> str:
        """String representation of the registry"""
        return f"<CapabilityRegistry: {len(self.capabilities)} capabilities in {len(self.get_categories())} categories>"