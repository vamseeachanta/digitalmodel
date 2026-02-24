"""
Configuration Management Service for OrcaFlex Browser Interface
Handles configuration persistence, import/export, and user preferences
"""

import json
import yaml
from typing import Dict, List, Optional, Any, Union
from pathlib import Path
from dataclasses import dataclass, asdict, field
from datetime import datetime, timedelta
import logging
import hashlib
import base64
from enum import Enum

logger = logging.getLogger(__name__)


class ConfigurationType(Enum):
    """Types of configurations"""
    USER_SAVED = "user_saved"
    AUTO_SAVED = "auto_saved"
    SHARED = "shared"
    DEFAULT = "default"
    TEMPLATE = "template"


@dataclass
class Configuration:
    """Complete configuration with metadata"""
    id: str
    name: str
    type: ConfigurationType
    parameters: Dict[str, Any]
    created_at: datetime
    modified_at: datetime
    created_by: str
    description: Optional[str] = None
    tags: List[str] = field(default_factory=list)
    is_favorite: bool = False
    usage_count: int = 0
    last_used: Optional[datetime] = None
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization"""
        data = asdict(self)
        data['type'] = self.type.value
        data['created_at'] = self.created_at.isoformat()
        data['modified_at'] = self.modified_at.isoformat()
        if self.last_used:
            data['last_used'] = self.last_used.isoformat()
        return data
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'Configuration':
        """Create from dictionary"""
        data['type'] = ConfigurationType(data['type'])
        data['created_at'] = datetime.fromisoformat(data['created_at'])
        data['modified_at'] = datetime.fromisoformat(data['modified_at'])
        if data.get('last_used'):
            data['last_used'] = datetime.fromisoformat(data['last_used'])
        return cls(**data)


@dataclass
class UserPreferences:
    """User preferences for browser interface"""
    user_id: str
    default_vessel_type: str = "fsts"
    default_loading_condition: str = "l015"
    default_tide_level: str = "hwl"
    default_return_period: str = "0100yr"
    auto_save_enabled: bool = True
    auto_save_interval: int = 300  # seconds
    show_tooltips: bool = True
    show_validation_warnings: bool = True
    preferred_theme: str = "light"
    recent_configurations: List[str] = field(default_factory=list)
    favorite_configurations: List[str] = field(default_factory=list)
    custom_shortcuts: Dict[str, str] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'UserPreferences':
        """Create from dictionary"""
        return cls(**data)


class ConfigurationManager:
    """Manages configuration storage, retrieval, and user preferences"""
    
    def __init__(self, storage_path: Optional[Path] = None):
        """
        Initialize configuration manager
        
        Args:
            storage_path: Path for configuration storage
        """
        self.storage_path = storage_path or Path.home() / ".orcaflex_browser" / "configs"
        self.storage_path.mkdir(parents=True, exist_ok=True)
        
        # Storage locations
        self.configs_file = self.storage_path / "configurations.json"
        self.prefs_file = self.storage_path / "preferences.json"
        self.templates_file = self.storage_path / "templates.json"
        
        # In-memory cache
        self.configurations: Dict[str, Configuration] = {}
        self.preferences: Dict[str, UserPreferences] = {}
        self.templates: Dict[str, Configuration] = {}
        
        # Load existing data
        self._load_configurations()
        self._load_preferences()
        self._load_templates()
        
        # Auto-save timer
        self.last_auto_save = datetime.now()
    
    def _generate_id(self, prefix: str = "cfg") -> str:
        """Generate unique configuration ID"""
        timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
        random_suffix = hashlib.md5(str(datetime.now()).encode()).hexdigest()[:6]
        return f"{prefix}_{timestamp}_{random_suffix}"
    
    def _load_configurations(self):
        """Load configurations from storage"""
        if self.configs_file.exists():
            try:
                with open(self.configs_file, 'r') as f:
                    data = json.load(f)
                    for cfg_data in data:
                        cfg = Configuration.from_dict(cfg_data)
                        self.configurations[cfg.id] = cfg
                logger.info(f"Loaded {len(self.configurations)} configurations")
            except Exception as e:
                logger.error(f"Error loading configurations: {e}")
    
    def _save_configurations(self):
        """Save configurations to storage"""
        try:
            data = [cfg.to_dict() for cfg in self.configurations.values()]
            with open(self.configs_file, 'w') as f:
                json.dump(data, f, indent=2)
            logger.info(f"Saved {len(self.configurations)} configurations")
        except Exception as e:
            logger.error(f"Error saving configurations: {e}")
    
    def _load_preferences(self):
        """Load user preferences from storage"""
        if self.prefs_file.exists():
            try:
                with open(self.prefs_file, 'r') as f:
                    data = json.load(f)
                    for user_id, pref_data in data.items():
                        self.preferences[user_id] = UserPreferences.from_dict(pref_data)
                logger.info(f"Loaded preferences for {len(self.preferences)} users")
            except Exception as e:
                logger.error(f"Error loading preferences: {e}")
    
    def _save_preferences(self):
        """Save user preferences to storage"""
        try:
            data = {
                user_id: prefs.to_dict() 
                for user_id, prefs in self.preferences.items()
            }
            with open(self.prefs_file, 'w') as f:
                json.dump(data, f, indent=2)
            logger.info(f"Saved preferences for {len(self.preferences)} users")
        except Exception as e:
            logger.error(f"Error saving preferences: {e}")
    
    def _load_templates(self):
        """Load configuration templates"""
        # Load built-in templates
        self._create_default_templates()
        
        # Load custom templates from file
        if self.templates_file.exists():
            try:
                with open(self.templates_file, 'r') as f:
                    data = json.load(f)
                    for template_data in data:
                        template = Configuration.from_dict(template_data)
                        self.templates[template.id] = template
                logger.info(f"Loaded {len(self.templates)} templates")
            except Exception as e:
                logger.error(f"Error loading templates: {e}")
    
    def _create_default_templates(self):
        """Create default configuration templates"""
        templates = [
            {
                "name": "Maximum Loading Analysis",
                "description": "Configuration for maximum loading conditions",
                "parameters": {
                    "vessel_type": "fsts",
                    "loading_condition": "l095",
                    "tide_level": "hwl",
                    "return_period": "0100yr",
                    "wave_direction": "000deg",
                    "analysis_type": "03c",
                    "auto_max": False
                },
                "tags": ["maximum", "conservative", "design"]
            },
            {
                "name": "Minimum Loading Analysis",
                "description": "Configuration for minimum loading conditions",
                "parameters": {
                    "vessel_type": "fsts",
                    "loading_condition": "l015",
                    "tide_level": "lwl",
                    "return_period": "0001yr",
                    "wave_direction": "000deg",
                    "analysis_type": "03c",
                    "auto_max": False
                },
                "tags": ["minimum", "operational"]
            },
            {
                "name": "Storm Condition Analysis",
                "description": "Configuration for storm conditions",
                "parameters": {
                    "vessel_type": "fsts",
                    "loading_condition": "l050",
                    "tide_level": "hwl",
                    "return_period": "0100yr",
                    "wave_direction": "045deg",
                    "analysis_type": "03c",
                    "auto_max": False
                },
                "tags": ["storm", "extreme", "survival"]
            }
        ]
        
        for template_data in templates:
            template_id = self._generate_id("tpl")
            template = Configuration(
                id=template_id,
                name=template_data["name"],
                type=ConfigurationType.TEMPLATE,
                parameters=template_data["parameters"],
                created_at=datetime.now(),
                modified_at=datetime.now(),
                created_by="system",
                description=template_data["description"],
                tags=template_data["tags"]
            )
            self.templates[template_id] = template
    
    def save_configuration(
        self,
        name: str,
        parameters: Dict[str, Any],
        user_id: str,
        description: Optional[str] = None,
        tags: Optional[List[str]] = None,
        type: ConfigurationType = ConfigurationType.USER_SAVED
    ) -> Configuration:
        """
        Save a configuration
        
        Args:
            name: Configuration name
            parameters: Configuration parameters
            user_id: User ID
            description: Optional description
            tags: Optional tags
            type: Configuration type
            
        Returns:
            Saved configuration
        """
        config_id = self._generate_id()
        
        config = Configuration(
            id=config_id,
            name=name,
            type=type,
            parameters=parameters,
            created_at=datetime.now(),
            modified_at=datetime.now(),
            created_by=user_id,
            description=description,
            tags=tags or []
        )
        
        self.configurations[config_id] = config
        self._save_configurations()
        
        # Update user's recent configurations
        self._update_recent_configurations(user_id, config_id)
        
        logger.info(f"Saved configuration: {name} ({config_id})")
        return config
    
    def load_configuration(self, config_id: str) -> Optional[Configuration]:
        """
        Load a configuration by ID
        
        Args:
            config_id: Configuration ID
            
        Returns:
            Configuration if found
        """
        config = self.configurations.get(config_id)
        
        if config:
            # Update usage statistics
            config.usage_count += 1
            config.last_used = datetime.now()
            self._save_configurations()
            
        return config
    
    def update_configuration(
        self,
        config_id: str,
        parameters: Optional[Dict[str, Any]] = None,
        name: Optional[str] = None,
        description: Optional[str] = None,
        tags: Optional[List[str]] = None
    ) -> Optional[Configuration]:
        """
        Update an existing configuration
        
        Args:
            config_id: Configuration ID
            parameters: New parameters (optional)
            name: New name (optional)
            description: New description (optional)
            tags: New tags (optional)
            
        Returns:
            Updated configuration
        """
        config = self.configurations.get(config_id)
        
        if not config:
            logger.warning(f"Configuration not found: {config_id}")
            return None
        
        if parameters:
            config.parameters = parameters
        if name:
            config.name = name
        if description is not None:
            config.description = description
        if tags is not None:
            config.tags = tags
        
        config.modified_at = datetime.now()
        self._save_configurations()
        
        logger.info(f"Updated configuration: {config_id}")
        return config
    
    def delete_configuration(self, config_id: str) -> bool:
        """
        Delete a configuration
        
        Args:
            config_id: Configuration ID
            
        Returns:
            True if deleted successfully
        """
        if config_id in self.configurations:
            del self.configurations[config_id]
            self._save_configurations()
            logger.info(f"Deleted configuration: {config_id}")
            return True
        
        return False
    
    def list_configurations(
        self,
        user_id: Optional[str] = None,
        type: Optional[ConfigurationType] = None,
        tags: Optional[List[str]] = None
    ) -> List[Configuration]:
        """
        List configurations with optional filters
        
        Args:
            user_id: Filter by user
            type: Filter by type
            tags: Filter by tags
            
        Returns:
            List of configurations
        """
        configs = list(self.configurations.values())
        
        if user_id:
            configs = [c for c in configs if c.created_by == user_id]
        
        if type:
            configs = [c for c in configs if c.type == type]
        
        if tags:
            configs = [c for c in configs if any(tag in c.tags for tag in tags)]
        
        # Sort by last used, then modified date
        configs.sort(
            key=lambda c: (c.last_used or datetime.min, c.modified_at),
            reverse=True
        )
        
        return configs
    
    def export_configuration(
        self,
        config_id: str,
        format: str = "json"
    ) -> Optional[str]:
        """
        Export configuration to string
        
        Args:
            config_id: Configuration ID
            format: Export format (json, yaml, base64)
            
        Returns:
            Exported configuration string
        """
        config = self.configurations.get(config_id)
        
        if not config:
            return None
        
        data = config.to_dict()
        
        if format == "json":
            return json.dumps(data, indent=2)
        elif format == "yaml":
            return yaml.dump(data, default_flow_style=False)
        elif format == "base64":
            json_str = json.dumps(data)
            return base64.b64encode(json_str.encode()).decode()
        else:
            logger.error(f"Unknown export format: {format}")
            return None
    
    def import_configuration(
        self,
        data: str,
        format: str = "json",
        user_id: str = "imported"
    ) -> Optional[Configuration]:
        """
        Import configuration from string
        
        Args:
            data: Configuration data string
            format: Import format (json, yaml, base64)
            user_id: User ID for imported config
            
        Returns:
            Imported configuration
        """
        try:
            if format == "json":
                config_data = json.loads(data)
            elif format == "yaml":
                config_data = yaml.safe_load(data)
            elif format == "base64":
                json_str = base64.b64decode(data.encode()).decode()
                config_data = json.loads(json_str)
            else:
                logger.error(f"Unknown import format: {format}")
                return None
            
            # Create new configuration with new ID
            config_id = self._generate_id()
            config_data['id'] = config_id
            config_data['created_by'] = user_id
            config_data['created_at'] = datetime.now().isoformat()
            config_data['modified_at'] = datetime.now().isoformat()
            config_data['type'] = ConfigurationType.USER_SAVED.value
            
            config = Configuration.from_dict(config_data)
            self.configurations[config_id] = config
            self._save_configurations()
            
            logger.info(f"Imported configuration: {config.name} ({config_id})")
            return config
            
        except Exception as e:
            logger.error(f"Error importing configuration: {e}")
            return None
    
    def get_user_preferences(self, user_id: str) -> UserPreferences:
        """
        Get user preferences
        
        Args:
            user_id: User ID
            
        Returns:
            User preferences (creates default if not exists)
        """
        if user_id not in self.preferences:
            self.preferences[user_id] = UserPreferences(user_id=user_id)
            self._save_preferences()
        
        return self.preferences[user_id]
    
    def update_user_preferences(
        self,
        user_id: str,
        **kwargs
    ) -> UserPreferences:
        """
        Update user preferences
        
        Args:
            user_id: User ID
            **kwargs: Preference fields to update
            
        Returns:
            Updated preferences
        """
        prefs = self.get_user_preferences(user_id)
        
        for key, value in kwargs.items():
            if hasattr(prefs, key):
                setattr(prefs, key, value)
        
        self._save_preferences()
        logger.info(f"Updated preferences for user: {user_id}")
        
        return prefs
    
    def _update_recent_configurations(self, user_id: str, config_id: str):
        """Update user's recent configurations list"""
        prefs = self.get_user_preferences(user_id)
        
        # Remove if already in list
        if config_id in prefs.recent_configurations:
            prefs.recent_configurations.remove(config_id)
        
        # Add to front of list
        prefs.recent_configurations.insert(0, config_id)
        
        # Keep only last 10
        prefs.recent_configurations = prefs.recent_configurations[:10]
        
        self._save_preferences()
    
    def toggle_favorite(self, config_id: str, user_id: str) -> bool:
        """
        Toggle configuration favorite status
        
        Args:
            config_id: Configuration ID
            user_id: User ID
            
        Returns:
            New favorite status
        """
        config = self.configurations.get(config_id)
        prefs = self.get_user_preferences(user_id)
        
        if not config:
            return False
        
        config.is_favorite = not config.is_favorite
        
        if config.is_favorite:
            if config_id not in prefs.favorite_configurations:
                prefs.favorite_configurations.append(config_id)
        else:
            if config_id in prefs.favorite_configurations:
                prefs.favorite_configurations.remove(config_id)
        
        self._save_configurations()
        self._save_preferences()
        
        return config.is_favorite
    
    def auto_save_check(
        self,
        user_id: str,
        parameters: Dict[str, Any]
    ) -> Optional[Configuration]:
        """
        Check and perform auto-save if needed
        
        Args:
            user_id: User ID
            parameters: Current parameters
            
        Returns:
            Auto-saved configuration if saved
        """
        prefs = self.get_user_preferences(user_id)
        
        if not prefs.auto_save_enabled:
            return None
        
        now = datetime.now()
        time_since_save = (now - self.last_auto_save).total_seconds()
        
        if time_since_save >= prefs.auto_save_interval:
            config = self.save_configuration(
                name=f"Auto-save {now.strftime('%Y-%m-%d %H:%M')}",
                parameters=parameters,
                user_id=user_id,
                type=ConfigurationType.AUTO_SAVED
            )
            self.last_auto_save = now
            return config
        
        return None
    
    def cleanup_old_autosaves(self, days: int = 7):
        """
        Clean up old auto-saved configurations
        
        Args:
            days: Keep auto-saves from last N days
        """
        cutoff_date = datetime.now() - timedelta(days=days)
        
        to_delete = []
        for config_id, config in self.configurations.items():
            if (config.type == ConfigurationType.AUTO_SAVED and 
                config.created_at < cutoff_date):
                to_delete.append(config_id)
        
        for config_id in to_delete:
            del self.configurations[config_id]
        
        if to_delete:
            self._save_configurations()
            logger.info(f"Cleaned up {len(to_delete)} old auto-saves")
    
    def get_templates(self) -> List[Configuration]:
        """Get all configuration templates"""
        return list(self.templates.values())
    
    def apply_template(
        self,
        template_id: str,
        user_id: str,
        name: Optional[str] = None
    ) -> Optional[Configuration]:
        """
        Apply a template to create new configuration
        
        Args:
            template_id: Template ID
            user_id: User ID
            name: Optional custom name
            
        Returns:
            New configuration from template
        """
        template = self.templates.get(template_id)
        
        if not template:
            return None
        
        config_name = name or f"{template.name} (from template)"
        
        return self.save_configuration(
            name=config_name,
            parameters=template.parameters.copy(),
            user_id=user_id,
            description=f"Created from template: {template.name}",
            tags=template.tags.copy(),
            type=ConfigurationType.USER_SAVED
        )


# Example usage
if __name__ == "__main__":
    # Configure logging
    logging.basicConfig(level=logging.INFO)
    
    # Create manager
    manager = ConfigurationManager()
    
    # Save a configuration
    config = manager.save_configuration(
        name="Test Configuration",
        parameters={
            "vessel_type": "fsts",
            "loading_condition": "l095",
            "tide_level": "hwl",
            "return_period": "0100yr",
            "wave_direction": "000deg",
            "analysis_type": "03c",
            "auto_max": False
        },
        user_id="test_user",
        description="Test configuration for demonstration",
        tags=["test", "demo"]
    )
    
    print(f"Saved configuration: {config.id}")
    
    # Load configuration
    loaded = manager.load_configuration(config.id)
    print(f"Loaded: {loaded.name}")
    
    # Export configuration
    exported = manager.export_configuration(config.id, format="json")
    print(f"Exported JSON:\n{exported}")
    
    # List configurations
    configs = manager.list_configurations(user_id="test_user")
    print(f"User has {len(configs)} configurations")
    
    # Get user preferences
    prefs = manager.get_user_preferences("test_user")
    print(f"User preferences: auto_save={prefs.auto_save_enabled}")
    
    # Get templates
    templates = manager.get_templates()
    print(f"Available templates: {len(templates)}")