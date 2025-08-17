#!/usr/bin/env python3
"""
Module Boundary Checker for Repository Propagation
===================================================
Ensures that repository-specific modules are not accidentally propagated
to other repositories. Each repository maintains sovereignty over its modules.
"""

import os
import sys
import yaml
from pathlib import Path
from typing import Dict, List, Set, Tuple

class ModuleBoundaryChecker:
    """
    Checks and enforces module boundaries to prevent cross-repository
    contamination during propagation.
    """
    
    # Repository-specific module mappings
    REPOSITORY_MODULES = {
        'digitalmodel': {
            'orcaflex': 'OrcaFlex hydrodynamic analysis',
            'aqwa': 'ANSYS AQWA analysis',
            'ansys': 'ANSYS structural analysis',
            'hydrodynamics': 'Hydrodynamic calculations'
        },
        'assetutilities': {
            'asset_management': 'Asset lifecycle management',
            'utilities': 'Common utilities',
            'maintenance': 'Maintenance tracking'
        },
        'frontierdeepwater': {
            'deepwater': 'Deepwater systems',
            'subsea': 'Subsea engineering',
            'risers': 'Riser analysis'
        }
    }
    
    # Generic components safe for propagation
    GENERIC_PATHS = [
        '.agent-os/commands/',
        '.agent-os/standards/',
        '.agent-os/patterns/',
        '.agent-os/hooks/',
        '.common-commands/'  # AssetUtilities hub
    ]
    
    # Paths that should NEVER be propagated
    RESTRICTED_PATHS = [
        'src/modules/',
        'tests/modules/',
        'configs/modules/',
        'docs/modules/'
    ]
    
    def __init__(self, repo_path: Path = None):
        """Initialize the boundary checker."""
        self.repo_path = repo_path or Path.cwd()
        self.repo_name = self._detect_repository()
        self.violations = []
        
    def _detect_repository(self) -> str:
        """Detect which repository we're in."""
        # Check for repository indicators
        repo_indicators = {
            'digitalmodel': ['src/modules/orcaflex', 'src/modules/aqwa'],
            'assetutilities': ['src/modules/asset_management', '.common-commands'],
            'frontierdeepwater': ['src/modules/deepwater', 'src/modules/subsea']
        }
        
        for repo_name, indicators in repo_indicators.items():
            for indicator in indicators:
                if (self.repo_path / indicator).exists():
                    return repo_name
        
        # Check git remote if available
        try:
            git_config = self.repo_path / '.git' / 'config'
            if git_config.exists():
                with open(git_config) as f:
                    content = f.read()
                    for repo_name in self.REPOSITORY_MODULES.keys():
                        if repo_name in content.lower():
                            return repo_name
        except:
            pass
        
        return 'unknown'
    
    def check_file_propagation_safety(self, file_path: Path) -> Tuple[bool, str]:
        """
        Check if a file is safe to propagate to other repositories.
        
        Returns:
            Tuple of (is_safe, reason)
        """
        rel_path = file_path.relative_to(self.repo_path) if file_path.is_absolute() else file_path
        path_str = str(rel_path).replace('\\', '/')
        
        # Check if in generic paths (safe)
        for generic_path in self.GENERIC_PATHS:
            if path_str.startswith(generic_path):
                return True, f"Generic component in {generic_path}"
        
        # Check if in restricted paths (not safe)
        for restricted_path in self.RESTRICTED_PATHS:
            if path_str.startswith(restricted_path):
                module_name = self._extract_module_name(path_str)
                return False, f"Repository-specific module: {module_name}"
        
        # Check for repository-specific markers in content
        if file_path.exists() and file_path.suffix in ['.py', '.yml', '.yaml', '.md']:
            content = file_path.read_text()
            
            # Check for module imports
            for repo, modules in self.REPOSITORY_MODULES.items():
                if repo != self.repo_name:
                    for module in modules.keys():
                        if f'modules.{module}' in content or f'modules/{module}' in content:
                            return False, f"Contains {repo}-specific module reference: {module}"
        
        # Default to safe if not in restricted areas
        return True, "No module-specific content detected"
    
    def _extract_module_name(self, path_str: str) -> str:
        """Extract module name from path."""
        parts = path_str.split('/')
        if 'modules' in parts:
            idx = parts.index('modules')
            if idx + 1 < len(parts):
                return parts[idx + 1]
        return 'unknown'
    
    def scan_propagation_list(self, files: List[Path]) -> Dict[str, List[str]]:
        """
        Scan a list of files intended for propagation.
        
        Returns:
            Dictionary with 'safe' and 'unsafe' file lists
        """
        result = {'safe': [], 'unsafe': [], 'warnings': []}
        
        for file_path in files:
            is_safe, reason = self.check_file_propagation_safety(file_path)
            
            if is_safe:
                result['safe'].append(str(file_path))
            else:
                result['unsafe'].append(f"{file_path}: {reason}")
                self.violations.append((file_path, reason))
        
        return result
    
    def generate_propagation_config(self, target_repos: List[str]) -> Dict:
        """
        Generate a safe propagation configuration.
        """
        config = {
            'source_repository': self.repo_name,
            'target_repositories': target_repos,
            'propagate': {
                'generic_commands': [],
                'patterns': [],
                'standards': []
            },
            'exclude': {
                'modules': [],
                'repository_specific': []
            }
        }
        
        # Add generic components
        for generic_path in self.GENERIC_PATHS:
            path = self.repo_path / generic_path
            if path.exists():
                if 'commands' in generic_path:
                    config['propagate']['generic_commands'].extend(
                        [str(f) for f in path.glob('*.py') if f.name != '__pycache__']
                    )
                elif 'patterns' in generic_path:
                    config['propagate']['patterns'].extend(
                        [str(f) for f in path.glob('*.md')]
                    )
                elif 'standards' in generic_path:
                    config['propagate']['standards'].extend(
                        [str(f) for f in path.glob('*.md')]
                    )
        
        # Add exclusions
        if self.repo_name in self.REPOSITORY_MODULES:
            config['exclude']['modules'] = list(self.REPOSITORY_MODULES[self.repo_name].keys())
        
        # Add all module paths to exclusions
        config['exclude']['repository_specific'] = self.RESTRICTED_PATHS
        
        return config
    
    def validate_incoming_propagation(self, source_repo: str, files: List[Path]) -> bool:
        """
        Validate files coming from another repository.
        """
        if source_repo == self.repo_name:
            print(f"Warning: Propagating from same repository ({source_repo})")
            return True
        
        # Check if source is trying to push its modules
        if source_repo in self.REPOSITORY_MODULES:
            source_modules = self.REPOSITORY_MODULES[source_repo]
            
            for file_path in files:
                path_str = str(file_path)
                for module in source_modules.keys():
                    if f'modules/{module}' in path_str or f'modules.{module}' in path_str:
                        print(f"ERROR: {source_repo} trying to propagate its module '{module}'")
                        print(f"  File: {file_path}")
                        return False
        
        return True
    
    def generate_report(self) -> str:
        """Generate a boundary check report."""
        lines = []
        lines.append("=" * 60)
        lines.append("Module Boundary Check Report")
        lines.append("=" * 60)
        lines.append(f"Repository: {self.repo_name}")
        lines.append(f"Path: {self.repo_path}")
        
        if self.repo_name in self.REPOSITORY_MODULES:
            lines.append(f"\nRepository Modules:")
            for module, desc in self.REPOSITORY_MODULES[self.repo_name].items():
                lines.append(f"  - {module}: {desc}")
        
        if self.violations:
            lines.append(f"\nBoundary Violations Found: {len(self.violations)}")
            for file_path, reason in self.violations:
                lines.append(f"  ✗ {file_path}")
                lines.append(f"    Reason: {reason}")
        else:
            lines.append("\n✓ No boundary violations found")
        
        lines.append("\nSafe for Propagation:")
        for generic_path in self.GENERIC_PATHS:
            if (self.repo_path / generic_path).exists():
                lines.append(f"  ✓ {generic_path}")
        
        lines.append("\nRestricted from Propagation:")
        for restricted in self.RESTRICTED_PATHS:
            lines.append(f"  ✗ {restricted}*")
        
        lines.append("=" * 60)
        return "\n".join(lines)


def main():
    """Main entry point for boundary checker."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description='Check module boundaries before propagation'
    )
    parser.add_argument(
        '--check',
        nargs='+',
        help='Files to check for propagation safety'
    )
    parser.add_argument(
        '--generate-config',
        action='store_true',
        help='Generate safe propagation configuration'
    )
    parser.add_argument(
        '--validate-incoming',
        help='Validate incoming propagation from repository'
    )
    parser.add_argument(
        '--targets',
        nargs='+',
        default=['all'],
        help='Target repositories for propagation'
    )
    
    args = parser.parse_args()
    
    checker = ModuleBoundaryChecker()
    
    if args.check:
        # Check specific files
        files = [Path(f) for f in args.check]
        result = checker.scan_propagation_list(files)
        
        print("\nPropagation Safety Check:")
        print("-" * 40)
        
        if result['safe']:
            print("\n✓ Safe to propagate:")
            for f in result['safe']:
                print(f"  - {f}")
        
        if result['unsafe']:
            print("\n✗ NOT safe to propagate:")
            for f in result['unsafe']:
                print(f"  - {f}")
        
    elif args.generate_config:
        # Generate propagation config
        config = checker.generate_propagation_config(args.targets)
        
        # Save config
        config_file = Path('.propagation-config.yml')
        with open(config_file, 'w') as f:
            yaml.dump(config, f, default_flow_style=False)
        
        print(f"Propagation configuration saved to: {config_file}")
        print(f"\nSafe to propagate {len(config['propagate']['generic_commands'])} generic commands")
        print(f"Excluding {len(config['exclude']['modules'])} repository-specific modules")
        
    elif args.validate_incoming:
        # Validate incoming propagation
        # This would be called by the receiving repository
        print(f"Validating incoming propagation from: {args.validate_incoming}")
        # Implementation would check incoming files
        
    else:
        # Default: Generate report
        print(checker.generate_report())
        
        if checker.violations:
            sys.exit(1)  # Exit with error if violations found


if __name__ == "__main__":
    main()