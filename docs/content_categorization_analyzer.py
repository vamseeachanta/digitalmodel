#!/usr/bin/env python3
"""
Content Categorization and Analysis Script
Part of the AI-Friendly Documentation Organization project.

This script analyzes the existing documentation structure and creates
a mapping for reorganizing content into the new hierarchical system.
"""

import os
import re
import json
from pathlib import Path
from typing import Dict, List, Tuple, Any
from collections import defaultdict, Counter


class ContentCategorizer:
    """Analyzes and categorizes documentation content for reorganization."""
    
    def __init__(self, docs_root: str = "K:\\github\\digitalmodel\\docs"):
        """Initialize with docs root directory."""
        self.docs_root = Path(docs_root)
        self.analysis_results = {}
        self.content_mapping = {}
        
        # Define categorization rules
        self.category_rules = {
            "software": {
                "prefixes": ["pkg_"],
                "description": "Tools and software-specific documentation",
                "subcategories": {}
            },
            "domains": {
                "prefixes": ["sub_"],
                "description": "Engineering subject matter expertise",
                "subcategories": {}
            },
            "legacy": {
                "prefixes": ["leg_"],
                "description": "Historical and superseded content",
                "subcategories": {}
            },
            "modules": {
                "exact_names": ["modules"],
                "description": "Active development modules",
                "subcategories": {}
            },
            "references": {
                "patterns": [r".*\.(pdf|doc|docx)$", r".*standards?.*", r".*codes?.*"],
                "description": "Standards, papers, and external documentation",
                "subcategories": {}
            },
            "guides": {
                "fallback": True,
                "description": "Cross-cutting procedural documentation",
                "subcategories": {}
            }
        }
        
    def analyze_current_structure(self) -> Dict[str, Any]:
        """Perform comprehensive analysis of current documentation structure."""
        print("Analyzing current documentation structure...")
        
        analysis = {
            "total_directories": 0,
            "total_files": 0,
            "prefix_distribution": defaultdict(int),
            "file_type_distribution": defaultdict(int),
            "depth_analysis": defaultdict(int),
            "content_categories": defaultdict(list),
            "engineering_domains": [],
            "software_tools": [],
            "legacy_modules": []
        }
        
        # Analyze all directories and files
        for item in self.docs_root.iterdir():
            if item.is_dir() and not item.name.startswith('.'):
                analysis["total_directories"] += 1
                self._analyze_directory(item, analysis)
                
        # Calculate summary statistics
        analysis["avg_files_per_directory"] = (
            analysis["total_files"] / analysis["total_directories"] 
            if analysis["total_directories"] > 0 else 0
        )
        
        self.analysis_results = analysis
        return analysis
        
    def _analyze_directory(self, directory: Path, analysis: Dict[str, Any]):
        """Analyze a single directory and update analysis results."""
        dir_name = directory.name
        
        # Analyze prefixes
        for prefix in ["pkg_", "sub_", "leg_"]:
            if dir_name.startswith(prefix):
                analysis["prefix_distribution"][prefix] += 1
                
                # Extract tool/domain names
                base_name = dir_name[4:]  # Remove prefix
                if prefix == "pkg_":
                    analysis["software_tools"].append(base_name)
                elif prefix == "sub_":
                    analysis["engineering_domains"].append(base_name)
                elif prefix == "leg_":
                    analysis["legacy_modules"].append(base_name)
                break
        else:
            # No prefix found
            analysis["prefix_distribution"]["no_prefix"] += 1
            
        # Analyze files in directory
        file_count = 0
        max_depth = 0
        
        for file_path in directory.rglob("*"):
            if file_path.is_file():
                file_count += 1
                analysis["total_files"] += 1
                
                # File type analysis
                suffix = file_path.suffix.lower()
                analysis["file_type_distribution"][suffix] += 1
                
                # Depth analysis
                try:
                    relative_path = file_path.relative_to(directory)
                    depth = len(relative_path.parts) - 1  # Subtract the file itself
                    max_depth = max(max_depth, depth)
                except ValueError:
                    pass
                    
        analysis["depth_analysis"][max_depth] += 1
        
        # Categorize content
        category = self._categorize_directory(dir_name)
        analysis["content_categories"][category].append({
            "name": dir_name,
            "file_count": file_count,
            "max_depth": max_depth
        })
        
    def _categorize_directory(self, dir_name: str) -> str:
        """Categorize a directory based on naming patterns."""
        # Check prefix-based rules
        for category, rules in self.category_rules.items():
            if "prefixes" in rules:
                for prefix in rules["prefixes"]:
                    if dir_name.startswith(prefix):
                        return category
                        
            if "exact_names" in rules:
                if dir_name in rules["exact_names"]:
                    return category
                    
        # Check pattern-based rules (for files within directories)
        # This would need file-level analysis for full implementation
        
        # Default to guides
        return "guides"
        
    def generate_migration_mapping(self) -> Dict[str, str]:
        """Generate mapping from old paths to new paths."""
        print("Generating migration mapping...")
        
        mapping = {}
        
        for item in self.docs_root.iterdir():
            if item.is_dir() and not item.name.startswith('.'):
                old_path = item.name
                new_path = self._generate_new_path(old_path)
                mapping[old_path] = new_path
                
        self.content_mapping = mapping
        return mapping
        
    def _generate_new_path(self, old_dir_name: str) -> str:
        """Generate new path for a directory based on categorization rules."""
        # Handle prefix-based categorization
        if old_dir_name.startswith("pkg_"):
            tool_name = old_dir_name[4:]
            return f"software/{tool_name}"
            
        elif old_dir_name.startswith("sub_"):
            domain_name = old_dir_name[4:].replace("_", "-")
            return f"domains/{domain_name}"
            
        elif old_dir_name.startswith("leg_"):
            module_name = old_dir_name[4:]
            return f"legacy/{module_name}"
            
        elif old_dir_name == "modules":
            return "modules"  # Keep as-is
            
        elif old_dir_name in ["_lib", "user_stories"]:
            return f"guides/{old_dir_name}"
            
        elif old_dir_name in ["ansys", "cathodic_protection", "lighting"]:
            # Handle special cases without prefixes
            if old_dir_name == "ansys":
                return "software/ansys"
            elif old_dir_name == "cathodic_protection":
                return "domains/cathodic-protection"
            elif old_dir_name == "lighting":
                return "domains/lighting"
                
        else:
            # Default to guides
            return f"guides/{old_dir_name}"
            
    def create_domain_structure(self) -> Dict[str, List[str]]:
        """Create subdirectory structure for engineering domains."""
        print("Creating domain-specific subdirectory structure...")
        
        domain_structure = {
            "software": [],
            "domains": [],
            "legacy": [],
            "modules": ["aqwa", "orcaflex", "orcawave", "catenary", "mooring", "rao-analysis", "ship-design"],
            "references": ["standards", "papers", "manuals"],
            "guides": ["procedures", "workflows", "best-practices"]
        }
        
        # Extract software tools
        for item in self.docs_root.iterdir():
            if item.is_dir():
                dir_name = item.name
                if dir_name.startswith("pkg_"):
                    tool_name = dir_name[4:]
                    domain_structure["software"].append(tool_name)
                elif dir_name == "ansys":
                    domain_structure["software"].append("ansys")
                    
        # Extract engineering domains
        engineering_domains = [
            "ship-design", "cathodic-protection", "drilling", "installation",
            "pipelines", "risers", "moorings", "fatigue", "hydrodynamics",
            "metocean", "vibrations", "welding", "rigging", "wind",
            "lng", "subsea-systems", "structural-analysis"
        ]
        
        for item in self.docs_root.iterdir():
            if item.is_dir():
                dir_name = item.name
                if dir_name.startswith("sub_"):
                    domain_name = dir_name[4:].replace("_", "-")
                    if domain_name not in domain_structure["domains"]:
                        domain_structure["domains"].append(domain_name)
                elif dir_name == "cathodic_protection":
                    domain_structure["domains"].append("cathodic-protection")
                elif dir_name == "lighting":
                    domain_structure["domains"].append("lighting")
                    
        # Extract legacy modules
        for item in self.docs_root.iterdir():
            if item.is_dir():
                dir_name = item.name
                if dir_name.startswith("leg_"):
                    module_name = dir_name[4:]
                    domain_structure["legacy"].append(module_name)
                    
        return domain_structure
        
    def save_analysis_results(self, output_file: str = "docs_analysis_results.json"):
        """Save analysis results to JSON file."""
        output_path = self.docs_root / output_file
        
        # Prepare data for JSON serialization
        serializable_results = {
            "analysis": dict(self.analysis_results),
            "mapping": self.content_mapping,
            "domain_structure": self.create_domain_structure()
        }
        
        # Convert defaultdict to regular dict and handle Counter objects
        def convert_for_json(obj):
            if isinstance(obj, defaultdict):
                return dict(obj)
            elif isinstance(obj, Counter):
                return dict(obj)
            return obj
            
        # Recursively convert the results
        def deep_convert(data):
            if isinstance(data, dict):
                return {k: deep_convert(v) for k, v in data.items()}
            elif isinstance(data, list):
                return [deep_convert(item) for item in data]
            else:
                return convert_for_json(data)
                
        serializable_results = deep_convert(serializable_results)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(serializable_results, f, indent=2, ensure_ascii=False)
            
        print(f"Analysis results saved to: {output_path}")
        return output_path
        
    def print_summary(self):
        """Print a summary of the analysis results."""
        if not self.analysis_results:
            print("No analysis results available. Run analyze_current_structure() first.")
            return
            
        print("\n" + "="*60)
        print("DOCUMENTATION STRUCTURE ANALYSIS SUMMARY")
        print("="*60)
        
        print(f"Total directories: {self.analysis_results['total_directories']}")
        print(f"Total files: {self.analysis_results['total_files']}")
        print(f"Average files per directory: {self.analysis_results['avg_files_per_directory']:.1f}")
        
        print("\nPrefix Distribution:")
        for prefix, count in self.analysis_results['prefix_distribution'].items():
            print(f"  {prefix}: {count}")
            
        print("\nTop File Types:")
        file_types = sorted(
            self.analysis_results['file_type_distribution'].items(), 
            key=lambda x: x[1], 
            reverse=True
        )[:10]
        for file_type, count in file_types:
            print(f"  {file_type or '(no extension)'}: {count}")
            
        print("\nEngineering Domains Found:")
        for domain in sorted(self.analysis_results['engineering_domains']):
            print(f"  - {domain}")
            
        print("\nSoftware Tools Found:")
        for tool in sorted(self.analysis_results['software_tools']):
            print(f"  - {tool}")
            
        print("\nLegacy Modules Found:")
        for module in sorted(self.analysis_results['legacy_modules']):
            print(f"  - {module}")
            
        print("\n" + "="*60)


def main():
    """Main function to run the content categorization analysis."""
    # Initialize the categorizer
    categorizer = ContentCategorizer()
    
    try:
        # Perform analysis
        print("Starting content categorization analysis...")
        analysis_results = categorizer.analyze_current_structure()
        
        # Generate migration mapping
        migration_mapping = categorizer.generate_migration_mapping()
        
        # Create domain structure
        domain_structure = categorizer.create_domain_structure()
        
        # Save results
        output_file = categorizer.save_analysis_results()
        
        # Print summary
        categorizer.print_summary()
        
        print(f"\nMigration mapping created for {len(migration_mapping)} directories")
        print(f"Results saved to: {output_file}")
        
        return True
        
    except Exception as e:
        print(f"Error during analysis: {e}")
        return False


if __name__ == "__main__":
    success = main()