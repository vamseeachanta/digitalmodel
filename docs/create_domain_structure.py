#!/usr/bin/env python3
"""
Domain Structure Creation Script
Part of the AI-Friendly Documentation Organization project.

This script creates the domain-specific subdirectory structure
based on the analysis results from content_categorization_analyzer.py
"""

import json
import os
from pathlib import Path
from typing import Dict, List


class DomainStructureCreator:
    """Creates domain-specific subdirectory structure."""
    
    def __init__(self, docs_root: str = "K:\\github\\digitalmodel\\docs"):
        """Initialize with docs root directory."""
        self.docs_root = Path(docs_root)
        self.analysis_file = self.docs_root / "docs_analysis_results.json"
        
    def load_analysis_results(self) -> Dict:
        """Load analysis results from JSON file."""
        if not self.analysis_file.exists():
            raise FileNotFoundError(f"Analysis file not found: {self.analysis_file}")
            
        with open(self.analysis_file, 'r', encoding='utf-8') as f:
            return json.load(f)
            
    def create_software_subdirectories(self, software_tools: List[str]):
        """Create subdirectories for software tools."""
        software_dir = self.docs_root / "software"
        
        print(f"Creating software subdirectories in {software_dir}...")
        
        for tool in software_tools:
            tool_dir = software_dir / tool
            tool_dir.mkdir(exist_ok=True)
            
            # Create standard subdirectories for software tools
            subdirs = ["tutorials", "automation", "examples", "references"]
            for subdir in subdirs:
                (tool_dir / subdir).mkdir(exist_ok=True)
                
            print(f"  Created: software/{tool}")
            
    def create_domain_subdirectories(self, engineering_domains: List[str]):
        """Create subdirectories for engineering domains."""
        domains_dir = self.docs_root / "domains"
        
        print(f"Creating domain subdirectories in {domains_dir}...")
        
        # Group related domains
        domain_groups = {
            "marine-systems": [
                "ship-design", "moorings", "risers", "subsea-systems",
                "installation", "marine-growth", "lng"
            ],
            "structural": [
                "fatigue", "vibrations", "welding", "rigging", 
                "plate-buckling", "platecapacity", "fem", "modal-analysis"
            ],
            "fluid-systems": [
                "pipelines", "pipecapacity", "pipe", "pumps",
                "pressure-vessels", "hydrodynamics"
            ],
            "offshore": [
                "drilling", "interventions", "production", "spm",
                "submarine-cables", "tcp", "umbilical", "cathodic-protection"
            ],
            "environmental": [
                "metocean", "wind", "viv", "wec"
            ],
            "analysis": [
                "process-engineering", "reservoir", "thermodynamics",
                "time-series", "unit-conversion", "risk"
            ],
            "infrastructure": [
                "cranes", "connections", "design", "lighting",
                "nde", "pressure-vessels"
            ],
            "emerging": [
                "ai", "biomedical", "digitaltwin", "gas-turbines",
                "mathcad", "osi-pi", "project-management"
            ]
        }
        
        # Create grouped structure
        for group, domains in domain_groups.items():
            group_dir = domains_dir / group
            group_dir.mkdir(exist_ok=True)
            
            for domain in domains:
                # Clean domain name (replace underscores with hyphens)
                clean_domain = domain.replace("_", "-")
                domain_dir = group_dir / clean_domain
                domain_dir.mkdir(exist_ok=True)
                
                # Create standard subdirectories for domains
                subdirs = ["theory", "calculations", "examples", "standards", "vendor-info"]
                for subdir in subdirs:
                    (domain_dir / subdir).mkdir(exist_ok=True)
                    
                print(f"  Created: domains/{group}/{clean_domain}")
                
        # Handle ungrouped domains
        ungrouped_domains = set(engineering_domains)
        for group_domains in domain_groups.values():
            ungrouped_domains -= set(d.replace("_", "-") for d in group_domains)
            
        if ungrouped_domains:
            misc_dir = domains_dir / "miscellaneous"
            misc_dir.mkdir(exist_ok=True)
            
            for domain in ungrouped_domains:
                clean_domain = domain.replace("_", "-")
                domain_dir = misc_dir / clean_domain
                domain_dir.mkdir(exist_ok=True)
                
                # Create standard subdirectories
                subdirs = ["theory", "calculations", "examples", "standards"]
                for subdir in subdirs:
                    (domain_dir / subdir).mkdir(exist_ok=True)
                    
                print(f"  Created: domains/miscellaneous/{clean_domain}")
                
    def create_legacy_subdirectories(self, legacy_modules: List[str]):
        """Create subdirectories for legacy modules."""
        legacy_dir = self.docs_root / "legacy"
        
        print(f"Creating legacy subdirectories in {legacy_dir}...")
        
        for module in legacy_modules:
            module_dir = legacy_dir / module
            module_dir.mkdir(exist_ok=True)
            
            # Create archive structure for legacy content
            subdirs = ["code", "documentation", "calculations", "superseded"]
            for subdir in subdirs:
                (module_dir / subdir).mkdir(exist_ok=True)
                
            print(f"  Created: legacy/{module}")
            
    def create_reference_subdirectories(self):
        """Create subdirectories for references."""
        references_dir = self.docs_root / "references"
        
        print(f"Creating reference subdirectories in {references_dir}...")
        
        # Create reference categories
        ref_categories = {
            "standards": ["api", "dnv", "abs", "asme", "iso", "ieee"],
            "papers": ["journals", "conferences", "technical-reports"],
            "manuals": ["software", "equipment", "procedures"],
            "codes": ["design-codes", "safety-codes", "environmental"],
            "data": ["material-properties", "environmental-data", "lookup-tables"]
        }
        
        for category, subcategories in ref_categories.items():
            category_dir = references_dir / category
            category_dir.mkdir(exist_ok=True)
            
            for subcategory in subcategories:
                subcat_dir = category_dir / subcategory
                subcat_dir.mkdir(exist_ok=True)
                
            print(f"  Created: references/{category} with {len(subcategories)} subcategories")
            
    def create_guide_subdirectories(self):
        """Create subdirectories for guides."""
        guides_dir = self.docs_root / "guides"
        
        print(f"Creating guide subdirectories in {guides_dir}...")
        
        # Create guide categories
        guide_categories = [
            "workflows", "best-practices", "procedures", 
            "troubleshooting", "getting-started", "advanced-topics"
        ]
        
        for category in guide_categories:
            category_dir = guides_dir / category
            category_dir.mkdir(exist_ok=True)
            print(f"  Created: guides/{category}")
            
    def create_modules_subdirectories(self):
        """Enhance the existing modules directory structure."""
        modules_dir = self.docs_root / "modules"
        
        print(f"Enhancing modules subdirectories in {modules_dir}...")
        
        # Known active modules
        active_modules = [
            "aqwa", "orcaflex", "orcawave", "catenary", 
            "mooring", "rao-analysis", "ship-design"
        ]
        
        for module in active_modules:
            module_dir = modules_dir / module
            if not module_dir.exists():
                module_dir.mkdir(exist_ok=True)
                
                # Create standard module structure
                subdirs = ["src", "tests", "docs", "examples", "configs"]
                for subdir in subdirs:
                    (module_dir / subdir).mkdir(exist_ok=True)
                    
                print(f"  Created: modules/{module}")
            else:
                print(f"  Enhanced: modules/{module} (already exists)")
                
    def create_index_files(self):
        """Create _index.md files for each major category."""
        print("Creating category index files...")
        
        categories = {
            "software": {
                "title": "Software Tools Documentation",
                "description": "Documentation for various software tools and applications used in engineering analysis."
            },
            "domains": {
                "title": "Engineering Domain Expertise", 
                "description": "Subject matter expertise organized by engineering disciplines and technical domains."
            },
            "modules": {
                "title": "Active Development Modules",
                "description": "Documentation for actively maintained software modules and libraries."
            },
            "legacy": {
                "title": "Legacy Code and Documentation",
                "description": "Historical code modules and superseded documentation preserved for reference."
            },
            "references": {
                "title": "Reference Materials",
                "description": "Standards, papers, manuals, and external reference documentation."
            },
            "guides": {
                "title": "Procedures and Guides",
                "description": "Cross-cutting procedural documentation and best practice guides."
            }
        }
        
        for category, info in categories.items():
            category_dir = self.docs_root / category
            index_file = category_dir / "_index.md"
            
            content = f"""# {info['title']}

{info['description']}

## Organization

This section is organized to provide:
- Clear categorization of content by purpose and scope
- Consistent naming conventions using kebab-case
- Maximum depth of 4 levels for predictable navigation
- Metadata-enhanced documentation for AI-friendly access

## Navigation

Browse the subdirectories to find specific tools, domains, or topics.
Each subdirectory contains focused documentation with clear scope boundaries.

## Contributing

When adding new content:
1. Follow the established directory structure
2. Use kebab-case naming conventions
3. Add appropriate metadata to markdown files
4. Maintain clear scope boundaries between categories
"""
            
            with open(index_file, 'w', encoding='utf-8') as f:
                f.write(content)
                
            print(f"  Created: {category}/_index.md")
            
    def create_complete_structure(self):
        """Create the complete domain-specific directory structure."""
        print("Creating complete domain-specific directory structure...")
        print("="*60)
        
        try:
            # Load analysis results
            results = self.load_analysis_results()
            domain_structure = results.get("domain_structure", {})
            
            # Create subdirectories for each category
            if "software" in domain_structure:
                self.create_software_subdirectories(domain_structure["software"])
                
            if "domains" in domain_structure:
                self.create_domain_subdirectories(domain_structure["domains"])
                
            if "legacy" in domain_structure:
                self.create_legacy_subdirectories(domain_structure["legacy"])
                
            # Create reference and guide structures
            self.create_reference_subdirectories()
            self.create_guide_subdirectories()
            
            # Enhance modules structure
            self.create_modules_subdirectories()
            
            # Create index files
            self.create_index_files()
            
            print("="*60)
            print("Domain structure creation completed successfully!")
            
            return True
            
        except Exception as e:
            print(f"Error creating domain structure: {e}")
            return False


def main():
    """Main function to create domain structure."""
    creator = DomainStructureCreator()
    success = creator.create_complete_structure()
    
    if success:
        print("\nDomain-specific subdirectory structure created successfully!")
        print("Next steps:")
        print("1. Run tests to verify structure compliance")
        print("2. Begin content migration using the generated mapping")
        print("3. Create navigation aids and cross-references")
    else:
        print("\nFailed to create domain structure. Check error messages above.")
        
    return success


if __name__ == "__main__":
    main()