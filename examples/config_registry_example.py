#!/usr/bin/env python
"""
ConfigRegistry Integration Example
===================================

Demonstrates all features of the new ConfigRegistry system.
"""

import os
import sys
from pathlib import Path

# Add src to path for examples
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.config import ConfigRegistry, load_config


def main():
    print("=" * 70)
    print("ConfigRegistry Integration Example")
    print("=" * 70)
    
    # 1. Create registry (auto-detects base_configs/)
    print("\n1. Creating ConfigRegistry...")
    registry = ConfigRegistry()
    print(f"   Base path: {registry.base_path}")
    print(f"   Modules path: {registry.modules_path}")
    
    # 2. Auto-discovery
    print("\n2. Auto-discovering configurations...")
    configs = registry.list_configs()
    print(f"   Found {len(configs)} configurations:")
    for name in sorted(configs)[:10]:  # Show first 10
        print(f"   - {name}")
    if len(configs) > 10:
        print(f"   ... and {len(configs) - 10} more")
    
    # 3. Load a configuration
    if "mooring" in configs:
        print("\n3. Loading 'mooring' configuration...")
        config = registry.get_config("mooring")
        print(f"   Config keys: {list(config.keys())}")
        if "default" in config:
            print(f"   Default section: {list(config['default'].keys())[:5]}")
    
    # 4. Environment variable override demonstration
    print("\n4. Environment variable override...")
    os.environ["DM_LOG_LEVEL"] = "DEBUG"
    os.environ["DM_ANALYSIS__TOLERANCE"] = "0.0001"
    
    if "catenary" in configs:
        config = registry.get_config("catenary")
        if "default" in config:
            log_level = config["default"].get("log_level", "N/A")
            print(f"   Log level (with DM_LOG_LEVEL=DEBUG): {log_level}")
    
    # 5. Dot notation access
    print("\n5. Accessing nested values with dot notation...")
    if "mooring" in configs:
        value = registry.get_value("mooring", "default.log_level", default="INFO")
        print(f"   mooring.default.log_level = {value}")
    
    # 6. Caching demonstration
    print("\n6. Caching demonstration...")
    if "mooring" in configs:
        config1 = registry.get_config("mooring")
        config2 = registry.get_config("mooring")
        print(f"   Cache hit (same object): {config1 is config2}")
        
        registry.clear_cache()
        config3 = registry.get_config("mooring")
        print(f"   After cache clear (different object): {config1 is not config3}")
    
    # 7. JSON Schema generation
    print("\n7. JSON Schema generation...")
    if "mooring" in configs:
        schema = registry.generate_json_schema("mooring")
        print(f"   Generated schema keys: {list(schema.keys())}")
        print(f"   Schema has {len(schema.get('properties', {}))} top-level properties")
    
    # 8. Backward compatibility
    print("\n8. Backward compatibility (load_config function)...")
    try:
        if "mooring" in configs:
            config_path = registry._discovered_configs.get("mooring")
            if config_path:
                config = load_config(str(config_path))
                print(f"   Loaded via load_config(): {list(config.keys())[:3]}")
    except Exception as e:
        print(f"   Note: {e}")
    
    # 9. Hot reload
    print("\n9. Hot reload capability...")
    print(f"   Hot reload enabled: {registry.hot_reload_enabled}")
    registry.reload()
    print("   Configs reloaded successfully")
    
    print("\n" + "=" * 70)
    print("Integration example complete!")
    print(f"Total configurations available: {len(configs)}")
    print("=" * 70)


if __name__ == "__main__":
    main()
