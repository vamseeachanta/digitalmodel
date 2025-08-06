"""
Test Discovery Engine for automatic test file identification and categorization.
"""

import os
import re
import json
import hashlib
from pathlib import Path
from typing import Dict, List, Set, Optional, Any, Tuple
from dataclasses import dataclass, asdict
from datetime import datetime, timezone
import yaml

from test_automation.config import config
from test_automation.logging_config import get_logger, log_performance

logger = get_logger('test_automation.discovery')

@dataclass
class TestFile:
    """Represents a discovered test file."""
    path: str
    module: str
    category: str
    size_bytes: int
    last_modified: str
    dependencies: List[str]
    config_files: List[str]
    estimated_duration: float = 0.0
    requires_license: bool = False
    is_runnable: bool = True
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        return asdict(self)

@dataclass
class ModuleInfo:
    """Information about a discovered test module."""
    name: str
    path: str
    category: str
    test_files: List[TestFile]
    config_files: List[str]
    total_tests: int
    runnable_tests: int
    licensed_tests: int
    estimated_duration: float
    dependencies: Set[str]
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        data = asdict(self)
        data['dependencies'] = list(data['dependencies'])
        return data

class TestDiscoveryEngine:
    """
    Engine for discovering and categorizing test files across the repository.
    
    This engine automatically scans the test directory structure, identifies
    pytest-compatible test files, categorizes them by module type, and builds
    dependency maps for intelligent test execution.
    """
    
    def __init__(self, cache_enabled: bool = True):
        """
        Initialize the test discovery engine.
        
        Args:
            cache_enabled: Whether to use caching for discovery results
        """
        self.cache_enabled = cache_enabled
        self.cache_file = Path("test_discovery_cache.json")
        self.test_paths = [
            Path(config.paths.modules_dir),
            Path(config.paths.in_progress_dir),
            Path(config.paths.no_license_dir),
            Path(config.paths.local_temp_dir)
        ]
        
        # Test file patterns (pytest compatible)
        self.test_file_patterns = [
            r'^test_.*\.py$',
            r'^.*_test\.py$',
            r'^tests\.py$'
        ]
        
        # Configuration file patterns
        self.config_file_patterns = [
            r'.*\.yml$',
            r'.*\.yaml$',
            r'.*\.json$',
            r'.*\.cfg$',
            r'.*\.ini$'
        ]
        
        # Module categorization rules
        self.category_rules = {
            'engineering_analysis': [
                'aqwa', 'orcaflex', 'ansys', 'shear7', 'viv_analysis',
                'installation', 'marine_analysis', 'rao_analysis'
            ],
            'core_calculations': [
                'code_dnvrph103', 'fatigue_analysis', 'time_series', 
                'catenary_riser', 'pipe_capacity', 'pipeline',
                'plate_capacity', 'vertical_riser', 'umbilical_analysis'
            ],
            'integration_workflow': [
                'all_yml', 'transformation', 'ship_design', 'shapes',
                'rigging', 'vessels', 'mooring'
            ],
            'development_status': [
                'in_progress', 'no_license', 'local_temp', 'unresolved'
            ]
        }
        
        # Licensed software patterns
        self.licensed_software_patterns = [
            r'orcaflex',
            r'ansys',
            r'aqwa',
            r'shear7',
            r'nastran'
        ]
        
        self.discovered_modules: Dict[str, ModuleInfo] = {}
        self.discovery_stats = {
            'total_modules': 0,
            'total_test_files': 0,
            'runnable_tests': 0,
            'licensed_tests': 0,
            'last_discovery': None,
            'cache_hit': False
        }
    
    @log_performance
    def discover_modules(self, force_refresh: bool = False) -> Dict[str, ModuleInfo]:
        """
        Discover all test modules and categorize by type.
        
        Args:
            force_refresh: Force rediscovery even if cache exists
            
        Returns:
            Dictionary mapping module names to ModuleInfo objects
        """
        logger.info("Starting test module discovery")
        
        # Check cache first
        if self.cache_enabled and not force_refresh and self._load_from_cache():
            logger.info("Loaded discovery results from cache")
            self.discovery_stats['cache_hit'] = True
            return self.discovered_modules
        
        # Fresh discovery
        logger.info("Performing fresh test discovery")
        self.discovered_modules = {}
        
        for test_path in self.test_paths:
            if test_path.exists():
                logger.debug(f"Scanning directory: {test_path}")
                self._scan_directory(test_path)
            else:
                logger.warning(f"Test directory not found: {test_path}")
        
        # Build dependency maps
        self._build_dependency_maps()
        
        # Update statistics
        self._update_discovery_stats()
        
        # Cache results
        if self.cache_enabled:
            self._save_to_cache()
        
        logger.info(f"Discovery completed: {self.discovery_stats['total_modules']} modules, "
                   f"{self.discovery_stats['total_test_files']} test files")
        
        return self.discovered_modules
    
    def _scan_directory(self, directory: Path, parent_category: str = None) -> None:
        """
        Recursively scan directory for test files and modules.
        
        Args:
            directory: Directory to scan
            parent_category: Parent category for nested modules
        """
        if not directory.exists() or not directory.is_dir():
            return
        
        # Skip hidden directories and __pycache__
        if directory.name.startswith('.') or directory.name == '__pycache__':
            return
            
        # Look for test files in this directory
        test_files = []
        config_files = []
        
        for item in directory.iterdir():
            if item.is_file():
                if self._is_test_file(item.name):
                    test_file = self._create_test_file_info(item, directory.name, parent_category)
                    test_files.append(test_file)
                elif self._is_config_file(item.name):
                    config_files.append(str(item))
            elif item.is_dir():
                # Recursively scan subdirectories
                self._scan_directory(item, parent_category or directory.name)
        
        # Create module info if we found test files
        if test_files:
            module_name = directory.name
            category = self._categorize_module(module_name, parent_category)
            
            # Calculate statistics
            total_tests = len(test_files)
            runnable_tests = sum(1 for tf in test_files if tf.is_runnable)
            licensed_tests = sum(1 for tf in test_files if tf.requires_license)
            estimated_duration = sum(tf.estimated_duration for tf in test_files)
            
            # Collect dependencies
            dependencies = set()
            for tf in test_files:
                dependencies.update(tf.dependencies)
            
            module_info = ModuleInfo(
                name=module_name,
                path=str(directory),
                category=category,
                test_files=test_files,
                config_files=config_files,
                total_tests=total_tests,
                runnable_tests=runnable_tests,
                licensed_tests=licensed_tests,
                estimated_duration=estimated_duration,
                dependencies=dependencies
            )
            
            self.discovered_modules[module_name] = module_info
            logger.debug(f"Discovered module '{module_name}': {total_tests} tests, category '{category}'")
    
    def _is_test_file(self, filename: str) -> bool:
        """Check if file matches pytest test file patterns."""
        return any(re.match(pattern, filename) for pattern in self.test_file_patterns)
    
    def _is_config_file(self, filename: str) -> bool:
        """Check if file is a configuration file."""
        return any(re.match(pattern, filename) for pattern in self.config_file_patterns)
    
    def _create_test_file_info(self, file_path: Path, module_name: str, 
                             parent_category: str = None) -> TestFile:
        """Create TestFile object with metadata."""
        
        # Get file stats
        stat = file_path.stat()
        size_bytes = stat.st_size
        last_modified = datetime.fromtimestamp(stat.st_mtime, timezone.utc).isoformat()
        
        # Categorize module
        category = self._categorize_module(module_name, parent_category)
        
        # Extract dependencies from file content
        dependencies = self._extract_dependencies(file_path)
        
        # Find associated config files
        config_files = self._find_config_files(file_path.parent, file_path.stem)
        
        # Estimate duration based on file size and complexity
        estimated_duration = self._estimate_test_duration(file_path, size_bytes)
        
        # Check if requires licensed software
        requires_license = self._requires_license(file_path, module_name)
        
        # Determine if runnable
        is_runnable = not requires_license and category != 'development_status'
        
        return TestFile(
            path=str(file_path),
            module=module_name,
            category=category,
            size_bytes=size_bytes,
            last_modified=last_modified,
            dependencies=dependencies,
            config_files=config_files,
            estimated_duration=estimated_duration,
            requires_license=requires_license,
            is_runnable=is_runnable
        )
    
    def _categorize_module(self, module_name: str, parent_category: str = None) -> str:
        """Categorize module based on name and rules."""
        
        # Check parent category first
        if parent_category:
            for category, modules in self.category_rules.items():
                if parent_category in modules:
                    return category
        
        # Check direct module name
        for category, modules in self.category_rules.items():
            if module_name in modules:
                return category
        
        # Check partial matches
        for category, modules in self.category_rules.items():
            if any(keyword in module_name.lower() for keyword in modules):
                return category
        
        return 'uncategorized'
    
    def _extract_dependencies(self, file_path: Path) -> List[str]:
        """Extract Python import dependencies from test file."""
        dependencies = []
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                
                # Find import statements
                import_patterns = [
                    r'^import\s+([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)',
                    r'^from\s+([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\s+import',
                ]
                
                for line in content.split('\n'):
                    line = line.strip()
                    if line.startswith('#'):
                        continue
                        
                    for pattern in import_patterns:
                        match = re.match(pattern, line)
                        if match:
                            dep = match.group(1).split('.')[0]  # Get root module
                            if dep not in ['os', 'sys', 'pathlib', 'typing', 'datetime']:  # Skip stdlib
                                dependencies.append(dep)
                                
        except Exception as e:
            logger.warning(f"Error extracting dependencies from {file_path}: {e}")
        
        return list(set(dependencies))  # Remove duplicates
    
    def _find_config_files(self, directory: Path, test_name: str) -> List[str]:
        """Find configuration files associated with a test."""
        config_files = []
        
        # Look for files with same base name
        for ext in ['.yml', '.yaml', '.json', '.cfg']:
            config_file = directory / f"{test_name}{ext}"
            if config_file.exists():
                config_files.append(str(config_file))
        
        # Look for module-wide config files
        for pattern in ['*.yml', '*.yaml', '*.json']:
            for config_file in directory.glob(pattern):
                if str(config_file) not in config_files:
                    config_files.append(str(config_file))
        
        return config_files
    
    def _estimate_test_duration(self, file_path: Path, size_bytes: int) -> float:
        """Estimate test execution duration based on file analysis."""
        
        # Base estimation from file size (rough heuristic)
        base_duration = size_bytes / 1000  # 1 second per KB as baseline
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                
                # Adjust based on content complexity
                if 'orcaflex' in content.lower() or 'ansys' in content.lower():
                    base_duration *= 10  # Licensed software tests take longer
                
                if 'sleep' in content or 'time.sleep' in content:
                    base_duration *= 2  # Tests with delays
                
                if '@pytest.mark.slow' in content:
                    base_duration *= 5  # Explicitly marked slow tests
                
                # Count test functions
                test_function_count = len(re.findall(r'def test_\w+', content))
                if test_function_count > 1:
                    base_duration *= test_function_count * 0.5
                    
        except Exception as e:
            logger.warning(f"Error analyzing test file {file_path}: {e}")
        
        return min(max(base_duration, 1.0), 300.0)  # Clamp between 1s and 5min
    
    def _requires_license(self, file_path: Path, module_name: str) -> bool:
        """Check if test requires licensed software."""
        
        # Check module name against licensed patterns
        for pattern in self.licensed_software_patterns:
            if re.search(pattern, module_name.lower()):
                return True
        
        # Check file path
        if 'no_license' in str(file_path):
            return True
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read().lower()
                
                # Check for licensed software imports or usage
                for pattern in self.licensed_software_patterns:
                    if pattern in content:
                        return True
                        
        except Exception as e:
            logger.warning(f"Error checking license requirements for {file_path}: {e}")
        
        return False
    
    def _build_dependency_maps(self) -> None:
        """Build dependency relationships between modules."""
        
        for module_name, module_info in self.discovered_modules.items():
            # Cross-reference dependencies with discovered modules
            resolved_deps = set()
            
            for dep in module_info.dependencies:
                # Check if dependency is another discovered module
                if dep in self.discovered_modules:
                    resolved_deps.add(dep)
                # Check for partial matches (e.g., 'digitalmodel' matches 'digitalmodel.aqwa')
                for other_module in self.discovered_modules:
                    if dep in other_module or other_module in dep:
                        resolved_deps.add(other_module)
            
            # Update module with resolved dependencies
            module_info.dependencies = resolved_deps
            
            logger.debug(f"Module '{module_name}' dependencies: {resolved_deps}")
    
    def _update_discovery_stats(self) -> None:
        """Update discovery statistics."""
        
        total_modules = len(self.discovered_modules)
        total_test_files = sum(len(mod.test_files) for mod in self.discovered_modules.values())
        runnable_tests = sum(mod.runnable_tests for mod in self.discovered_modules.values())
        licensed_tests = sum(mod.licensed_tests for mod in self.discovered_modules.values())
        
        self.discovery_stats.update({
            'total_modules': total_modules,
            'total_test_files': total_test_files,
            'runnable_tests': runnable_tests,
            'licensed_tests': licensed_tests,
            'last_discovery': datetime.now(timezone.utc).isoformat(),
            'cache_hit': False
        })
    
    def get_runnable_tests(self) -> Dict[str, ModuleInfo]:
        """Filter tests that can be executed (exclude no_license, unresolved)."""
        return {
            name: module for name, module in self.discovered_modules.items()
            if module.runnable_tests > 0
        }
    
    def get_module_by_name(self, module_name: str) -> Optional[ModuleInfo]:
        """Get module information by name."""
        return self.discovered_modules.get(module_name)
    
    def get_modules_by_category(self, category: str) -> Dict[str, ModuleInfo]:
        """Get all modules in a specific category."""
        return {
            name: module for name, module in self.discovered_modules.items()
            if module.category == category
        }
    
    def validate_discovery_results(self) -> Dict[str, List[str]]:
        """Validate discovery results and return any issues found."""
        issues = {
            'missing_files': [],
            'invalid_paths': [],
            'empty_modules': [],
            'broken_dependencies': []
        }
        
        for module_name, module in self.discovered_modules.items():
            # Check if module path exists
            if not Path(module.path).exists():
                issues['invalid_paths'].append(f"Module '{module_name}' path does not exist: {module.path}")
            
            # Check if module has any test files
            if not module.test_files:
                issues['empty_modules'].append(f"Module '{module_name}' has no test files")
            
            # Check test file paths
            for test_file in module.test_files:
                if not Path(test_file.path).exists():
                    issues['missing_files'].append(f"Test file missing: {test_file.path}")
            
            # Check config file paths
            for config_file in module.config_files:
                if not Path(config_file).exists():
                    issues['missing_files'].append(f"Config file missing: {config_file}")
            
            # Check dependencies
            for dep in module.dependencies:
                if dep not in self.discovered_modules and not self._is_external_dependency(dep):
                    issues['broken_dependencies'].append(
                        f"Module '{module_name}' depends on unknown module '{dep}'"
                    )
        
        return issues
    
    def _is_external_dependency(self, dep: str) -> bool:
        """Check if dependency is an external package."""
        external_packages = {
            'pytest', 'numpy', 'pandas', 'matplotlib', 'scipy', 'yaml', 'json',
            'digitalmodel', 'assetutilities', 'orcfxapi', 'pyyaml'
        }
        return dep.lower() in external_packages
    
    def _save_to_cache(self) -> None:
        """Save discovery results to cache file."""
        try:
            cache_data = {
                'discovered_modules': {
                    name: module.to_dict() for name, module in self.discovered_modules.items()
                },
                'discovery_stats': self.discovery_stats,
                'cache_timestamp': datetime.now(timezone.utc).isoformat(),
                'cache_hash': self._calculate_cache_hash()
            }
            
            with open(self.cache_file, 'w', encoding='utf-8') as f:
                json.dump(cache_data, f, indent=2)
                
            logger.debug(f"Discovery results cached to {self.cache_file}")
            
        except Exception as e:
            logger.warning(f"Failed to save discovery cache: {e}")
    
    def _load_from_cache(self) -> bool:
        """Load discovery results from cache if valid."""
        try:
            if not self.cache_file.exists():
                return False
            
            with open(self.cache_file, 'r', encoding='utf-8') as f:
                cache_data = json.load(f)
            
            # Check if cache is still valid (hash of test directories unchanged)
            if cache_data.get('cache_hash') != self._calculate_cache_hash():
                logger.debug("Cache invalidated due to directory changes")
                return False
            
            # Reconstruct discovered modules
            self.discovered_modules = {}
            for name, module_data in cache_data.get('discovered_modules', {}).items():
                # Convert test files back to TestFile objects
                test_files = [TestFile(**tf_data) for tf_data in module_data['test_files']]
                
                # Create ModuleInfo with reconstructed data
                module_info = ModuleInfo(
                    name=module_data['name'],
                    path=module_data['path'],
                    category=module_data['category'],
                    test_files=test_files,
                    config_files=module_data['config_files'],
                    total_tests=module_data['total_tests'],
                    runnable_tests=module_data['runnable_tests'],
                    licensed_tests=module_data['licensed_tests'],
                    estimated_duration=module_data['estimated_duration'],
                    dependencies=set(module_data['dependencies'])
                )
                
                self.discovered_modules[name] = module_info
            
            # Restore discovery stats
            self.discovery_stats = cache_data.get('discovery_stats', {})
            
            return True
            
        except Exception as e:
            logger.warning(f"Failed to load discovery cache: {e}")
            return False
    
    def _calculate_cache_hash(self) -> str:
        """Calculate hash of test directories to detect changes."""
        hash_content = []
        
        for test_path in self.test_paths:
            if test_path.exists():
                # Add directory modification time and structure
                try:
                    stat = test_path.stat()
                    hash_content.append(f"{test_path}:{stat.st_mtime}")
                    
                    # Add file count and names for change detection
                    files = list(test_path.rglob('*.py'))
                    hash_content.append(f"count:{len(files)}")
                    hash_content.extend(str(f.relative_to(test_path)) for f in files[:100])  # Limit for performance
                    
                except Exception as e:
                    logger.warning(f"Error calculating hash for {test_path}: {e}")
        
        hash_string = '|'.join(sorted(hash_content))
        return hashlib.md5(hash_string.encode()).hexdigest()
    
    def clear_cache(self) -> None:
        """Clear discovery cache."""
        if self.cache_file.exists():
            self.cache_file.unlink()
            logger.info("Discovery cache cleared")
    
    def get_discovery_summary(self) -> Dict[str, Any]:
        """Get summary of discovery results."""
        return {
            'statistics': self.discovery_stats.copy(),
            'categories': {
                category: len([m for m in self.discovered_modules.values() if m.category == category])
                for category in set(m.category for m in self.discovered_modules.values())
            },
            'runnable_modules': len(self.get_runnable_tests()),
            'total_estimated_duration': sum(m.estimated_duration for m in self.discovered_modules.values()),
            'modules_with_dependencies': len([m for m in self.discovered_modules.values() if m.dependencies])
        }