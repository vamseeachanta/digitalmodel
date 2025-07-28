#!/usr/bin/env python3
"""
Automated Frontmatter Generation Script
Part of the AI-Friendly Documentation Organization project.

This script automatically generates and inserts YAML frontmatter
into markdown files to enable AI-friendly navigation and metadata.
"""

import os
import re
import json
import yaml
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime


class FrontmatterGenerator:
    """Generates and manages YAML frontmatter for markdown files."""
    
    def __init__(self, docs_root: str = "K:\\github\\digitalmodel\\docs"):
        """Initialize generator with docs root directory."""
        self.docs_root = Path(docs_root)
        self.analysis_file = self.docs_root / "docs_analysis_results.json"
        self.content_mapping = {}
        self.industry_standards_map = self._build_industry_standards_map()
        self.complexity_keywords = self._build_complexity_keywords()
        
    def _build_industry_standards_map(self) -> Dict[str, List[str]]:
        """Build mapping of content types to relevant industry standards."""
        return {
            "ship": ["API", "DNV", "ABS", "Lloyd's Register"],
            "offshore": ["API", "DNV", "ISO"],
            "pipeline": ["API", "ASME", "DNV"],
            "riser": ["API", "DNV", "ABS"],
            "mooring": ["API", "DNV", "ABS"],
            "fatigue": ["DNV", "ABS", "BS"],
            "welding": ["AWS", "ASME", "API"],
            "drilling": ["API", "IADC", "SPE"],
            "cathodic": ["NACE", "DNV", "ISO"],
            "pressure": ["ASME", "API", "PED"],
            "wind": ["IEC", "DNV", "API"],
            "structural": ["API", "AISC", "ABS"],
            "ansys": ["ANSYS", "FEA"],
            "orcaflex": ["OrcaFlex", "DNV"],
            "qgis": ["QGIS", "GIS"],
            "autocad": ["AutoCAD", "CAD"]
        }
        
    def _build_complexity_keywords(self) -> Dict[str, List[str]]:
        """Build mapping of complexity levels to keywords."""
        return {
            "beginner": [
                "intro", "introduction", "basic", "getting-started", 
                "tutorial", "overview", "guide", "simple"
            ],
            "intermediate": [
                "analysis", "design", "calculation", "procedure",
                "method", "workflow", "implementation"
            ],
            "advanced": [
                "optimization", "research", "theory", "algorithm",
                "automation", "scripting", "api", "development",
                "advanced", "expert"
            ]
        }
        
    def load_content_mapping(self) -> bool:
        """Load content mapping from analysis results."""
        try:
            if self.analysis_file.exists():
                with open(self.analysis_file, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                    self.content_mapping = data.get("mapping", {})
                    return True
            return False
        except Exception as e:
            print(f"Error loading content mapping: {e}")
            return False
            
    def has_frontmatter(self, file_path: Path) -> bool:
        """Check if a markdown file already has frontmatter."""
        try:
            content = file_path.read_text(encoding='utf-8')
            return content.strip().startswith('---')
        except Exception:
            return False
            
    def extract_existing_frontmatter(self, file_path: Path) -> Tuple[Optional[Dict], str]:
        """Extract existing frontmatter and content from a file."""
        try:
            content = file_path.read_text(encoding='utf-8')
            
            if not content.strip().startswith('---'):
                return None, content
                
            # Find the end of frontmatter
            lines = content.split('\n')
            end_idx = -1
            for i, line in enumerate(lines[1:], 1):
                if line.strip() == '---':
                    end_idx = i
                    break
                    
            if end_idx == -1:
                return None, content
                
            # Extract and parse frontmatter
            frontmatter_content = '\n'.join(lines[1:end_idx])
            remaining_content = '\n'.join(lines[end_idx + 1:])
            
            try:
                frontmatter = yaml.safe_load(frontmatter_content)
                return frontmatter, remaining_content
            except yaml.YAMLError:
                return None, content
                
        except Exception:
            return None, ""
            
    def determine_category(self, file_path: Path) -> str:
        """Determine the category for a file based on its path."""
        path_str = str(file_path)
        
        # Check if file is in new structure
        for category in ["software", "domains", "modules", "legacy", "references", "guides"]:
            if f"/{category}/" in path_str or f"\\{category}\\" in path_str:
                # Extract subcategory path
                parts = Path(path_str).parts
                try:
                    cat_idx = parts.index(category)
                    if cat_idx + 1 < len(parts):
                        subcategory_parts = parts[cat_idx + 1:-1]  # Exclude filename
                        subcategory = "/".join(subcategory_parts)
                        return f"{category}/{subcategory}" if subcategory else category
                except ValueError:
                    pass
                return category
                
        # Check if file is in old structure - use mapping
        if self.content_mapping:
            rel_path = file_path.relative_to(self.docs_root)
            parent_dir = rel_path.parts[0] if rel_path.parts else ""
            
            if parent_dir in self.content_mapping:
                return self.content_mapping[parent_dir]
                
        # Default categorization based on path patterns
        path_lower = path_str.lower()
        
        if any(prefix in path_lower for prefix in ["pkg_", "/software/"]):
            return "software"
        elif any(prefix in path_lower for prefix in ["sub_", "/domains/"]):
            return "domains"
        elif any(prefix in path_lower for prefix in ["leg_", "/legacy/"]):
            return "legacy"
        elif "/modules/" in path_lower:
            return "modules"
        elif any(word in path_lower for word in ["standard", "reference", "manual", "code"]):
            return "references"
        else:
            return "guides"
            
    def extract_title(self, file_path: Path, content: str) -> str:
        """Extract title from file content or generate from filename."""
        # Try to find first heading
        title_match = re.search(r'^#\s+(.+)$', content, re.MULTILINE)
        if title_match:
            title = title_match.group(1).strip()
            # Clean up title
            title = re.sub(r'[#*_`]', '', title).strip()
            return title
            
        # Generate from filename
        filename = file_path.stem
        
        # Handle special cases
        if filename.lower() == "readme":
            parent_name = file_path.parent.name
            return f"{parent_name.replace('_', ' ').replace('-', ' ').title()} Overview"
            
        if filename.lower().startswith("_"):
            return f"{filename[1:].replace('_', ' ').replace('-', ' ').title()} Index"
            
        # General filename to title conversion
        title = filename.replace('_', ' ').replace('-', ' ')
        title = re.sub(r'\b\w+\b', lambda m: m.group(0).title(), title)
        
        return title
        
    def generate_tags(self, file_path: Path, content: str, category: str) -> List[str]:
        """Generate relevant tags based on file path, content, and category."""
        tags = []
        
        # Extract from path
        path_parts = str(file_path).lower().split(os.sep)
        for part in path_parts:
            part_clean = part.replace('_', '-').replace('.md', '')
            if len(part_clean) > 2 and not part_clean.startswith(('pkg', 'sub', 'leg')):
                tags.append(part_clean)
                
        # Extract from content (headings and emphasis)
        content_lower = content.lower()
        
        # Look for technical terms and keywords
        technical_terms = [
            "analysis", "design", "calculation", "simulation", "modeling",
            "api", "standard", "code", "guideline", "procedure", "method",
            "ship", "offshore", "pipeline", "riser", "mooring", "drilling",
            "fatigue", "stress", "structural", "hydrodynamic", "stability",
            "installation", "marine", "subsea", "cathodic", "protection"
        ]
        
        for term in technical_terms:
            if term in content_lower and term not in tags:
                tags.append(term)
                
        # Add category-specific tags
        if "software" in category:
            tags.append("software-documentation")
        elif "domains" in category:
            tags.append("engineering-domain")
        elif "legacy" in category:
            tags.append("legacy-code")
        elif "modules" in category:
            tags.append("active-development")
            
        # Remove duplicates and limit to reasonable number
        tags = list(dict.fromkeys(tags))  # Preserve order while removing duplicates
        return tags[:8]  # Limit to 8 tags
        
    def determine_complexity(self, file_path: Path, content: str) -> str:
        """Determine complexity level based on content analysis."""
        content_lower = content.lower()
        filename_lower = file_path.name.lower()
        
        # Check for beginner indicators
        beginner_score = 0
        for keyword in self.complexity_keywords["beginner"]:
            if keyword in filename_lower:
                beginner_score += 2
            if keyword in content_lower:
                beginner_score += 1
                
        # Check for advanced indicators
        advanced_score = 0
        for keyword in self.complexity_keywords["advanced"]:
            if keyword in filename_lower:
                advanced_score += 2
            if keyword in content_lower:
                advanced_score += 1
                
        # Check for intermediate indicators
        intermediate_score = 0
        for keyword in self.complexity_keywords["intermediate"]:
            if keyword in filename_lower:
                intermediate_score += 2
            if keyword in content_lower:
                intermediate_score += 1
                
        # Additional complexity indicators
        if len(content) > 5000:  # Long documents tend to be more complex
            advanced_score += 1
        if content.count('```') > 3:  # Multiple code blocks
            advanced_score += 1
        if re.search(r'\b(equation|formula|algorithm)\b', content_lower):
            advanced_score += 2
            
        # Determine complexity based on scores
        if advanced_score >= max(beginner_score, intermediate_score):
            return "advanced"
        elif beginner_score >= intermediate_score:
            return "beginner"
        else:
            return "intermediate"
            
    def determine_industry_standards(self, file_path: Path, content: str) -> List[str]:
        """Determine relevant industry standards based on content."""
        standards = []
        content_lower = content.lower()
        path_lower = str(file_path).lower()
        
        # Check content and path against standards mapping
        for key, standard_list in self.industry_standards_map.items():
            if key in content_lower or key in path_lower:
                standards.extend(standard_list)
                
        # Look for explicit standard mentions
        standard_patterns = [
            r'\bapi[\s-]?\w+\b', r'\bdnv[\s-]?\w*\b', r'\babs[\s-]?\w*\b',
            r'\basme[\s-]?\w*\b', r'\biso[\s-]?\d+\b', r'\biec[\s-]?\d+\b',
            r'\bnace[\s-]?\w*\b', r'\baws[\s-]?\w*\b'
        ]
        
        for pattern in standard_patterns:
            matches = re.findall(pattern, content_lower)
            for match in matches:
                standard = match.upper().replace('-', ' ').strip()
                if standard not in standards:
                    standards.append(standard)
                    
        # Remove duplicates and limit
        return list(dict.fromkeys(standards))[:5]
        
    def find_related_files(self, file_path: Path, content: str) -> List[str]:
        """Find related files based on links and content similarity."""
        related = []
        
        # Extract existing markdown links
        link_pattern = r'\[([^\]]+)\]\(([^)]+)\)'
        matches = re.findall(link_pattern, content)
        
        for text, url in matches:
            if url.endswith('.md') and not url.startswith('http'):
                # Convert to relative path from docs root
                if url.startswith('../'):
                    # Handle relative paths
                    try:
                        target_path = (file_path.parent / url).resolve()
                        rel_path = target_path.relative_to(self.docs_root)
                        related.append(str(rel_path))
                    except (ValueError, OSError):
                        pass
                else:
                    related.append(url)
                    
        # Limit to reasonable number
        return related[:5]
        
    def generate_frontmatter(self, file_path: Path, force_category: Optional[str] = None) -> Dict[str, Any]:
        """Generate complete frontmatter for a markdown file."""
        try:
            # Read content
            existing_fm, content = self.extract_existing_frontmatter(file_path)
            
            # Determine category
            category = force_category or self.determine_category(file_path)
            
            # Extract/generate metadata
            title = self.extract_title(file_path, content)
            tags = self.generate_tags(file_path, content, category)
            complexity = self.determine_complexity(file_path, content)
            industry_standards = self.determine_industry_standards(file_path, content)
            related = self.find_related_files(file_path, content)
            
            # Build frontmatter
            frontmatter = {
                "title": title,
                "category": category,
                "tags": tags,
                "last_updated": datetime.now().strftime("%Y-%m-%d"),
                "status": "active",
                "complexity": complexity,
                "related": related,
                "industry_standards": industry_standards
            }
            
            # Merge with existing frontmatter if present
            if existing_fm:
                # Preserve certain fields from existing frontmatter
                preserve_fields = ["author", "date_created", "version", "custom_fields"]
                for field in preserve_fields:
                    if field in existing_fm:
                        frontmatter[field] = existing_fm[field]
                        
                # Update last_updated but preserve original creation info
                if "date_created" not in existing_fm and "last_updated" in existing_fm:
                    frontmatter["date_created"] = existing_fm["last_updated"]
                    
            return frontmatter
            
        except Exception as e:
            print(f"Error generating frontmatter for {file_path}: {e}")
            return {}
            
    def frontmatter_to_yaml(self, frontmatter: Dict[str, Any]) -> str:
        """Convert frontmatter dictionary to YAML string."""
        return yaml.dump(
            frontmatter, 
            default_flow_style=False, 
            allow_unicode=True,
            sort_keys=False
        )
        
    def insert_frontmatter(self, file_path: Path, frontmatter: Dict[str, Any]) -> bool:
        """Insert or update frontmatter in a markdown file."""
        try:
            existing_fm, content = self.extract_existing_frontmatter(file_path)
            
            # Generate YAML
            yaml_content = self.frontmatter_to_yaml(frontmatter)
            
            # Create new file content
            new_content = f"---\n{yaml_content}---\n\n{content.lstrip()}"
            
            # Write back to file
            file_path.write_text(new_content, encoding='utf-8')
            return True
            
        except Exception as e:
            print(f"Error inserting frontmatter into {file_path}: {e}")
            return False
            
    def process_all_markdown_files(self, dry_run: bool = True) -> Dict[str, Any]:
        """Process all markdown files in the docs directory."""
        print(f"Processing all markdown files (dry_run={dry_run})...")
        
        results = {
            "processed": 0,
            "skipped": 0,
            "errors": 0,
            "files": []
        }
        
        # Load content mapping
        self.load_content_mapping()
        
        # Find all markdown files
        md_files = list(self.docs_root.rglob("*.md"))
        print(f"Found {len(md_files)} markdown files")
        
        for md_file in md_files:
            try:
                # Skip if already has frontmatter (unless force update)
                if self.has_frontmatter(md_file):
                    print(f"Skipping {md_file.name} (already has frontmatter)")
                    results["skipped"] += 1
                    continue
                    
                # Generate frontmatter
                frontmatter = self.generate_frontmatter(md_file)
                
                if not frontmatter:
                    print(f"Could not generate frontmatter for {md_file}")
                    results["errors"] += 1
                    continue
                    
                # Apply changes if not dry run
                if not dry_run:
                    success = self.insert_frontmatter(md_file, frontmatter)
                    if not success:
                        results["errors"] += 1
                        continue
                        
                results["processed"] += 1
                results["files"].append({
                    "file": str(md_file.relative_to(self.docs_root)),
                    "title": frontmatter.get("title", ""),
                    "category": frontmatter.get("category", ""),
                    "tags": frontmatter.get("tags", [])
                })
                
                print(f"Processed: {md_file.name}")
                
            except Exception as e:
                print(f"Error processing {md_file}: {e}")
                results["errors"] += 1
                
        return results
        
    def update_existing_frontmatter(self, force_fields: Optional[List[str]] = None) -> Dict[str, Any]:
        """Update existing frontmatter with new metadata."""
        print("Updating existing frontmatter...")
        
        results = {
            "updated": 0,
            "skipped": 0,
            "errors": 0
        }
        
        force_fields = force_fields or ["last_updated", "tags", "related"]
        
        # Find all markdown files with frontmatter
        md_files = [f for f in self.docs_root.rglob("*.md") if self.has_frontmatter(f)]
        
        for md_file in md_files:
            try:
                existing_fm, content = self.extract_existing_frontmatter(md_file)
                if not existing_fm:
                    continue
                    
                # Generate new frontmatter
                new_fm = self.generate_frontmatter(md_file)
                
                # Update only specified fields
                updated = False
                for field in force_fields:
                    if field in new_fm and new_fm[field] != existing_fm.get(field):
                        existing_fm[field] = new_fm[field]
                        updated = True
                        
                if updated:
                    self.insert_frontmatter(md_file, existing_fm)
                    results["updated"] += 1
                    print(f"Updated: {md_file.name}")
                else:
                    results["skipped"] += 1
                    
            except Exception as e:
                print(f"Error updating {md_file}: {e}")
                results["errors"] += 1
                
        return results


def main():
    """Main function to run frontmatter generation."""
    generator = FrontmatterGenerator()
    
    print("AI-Friendly Documentation Frontmatter Generator")
    print("=" * 50)
    
    # First, run in dry-run mode to show what would be done
    print("\n1. Running dry-run analysis...")
    dry_results = generator.process_all_markdown_files(dry_run=True)
    
    print(f"\nDry-run results:")
    print(f"  Files to process: {dry_results['processed']}")
    print(f"  Files to skip: {dry_results['skipped']}")
    print(f"  Potential errors: {dry_results['errors']}")
    
    # Ask for confirmation
    if dry_results['processed'] > 0:
        print(f"\nReady to process {dry_results['processed']} files.")
        confirm = input("Proceed with frontmatter generation? (y/N): ")
        
        if confirm.lower() == 'y':
            print("\n2. Processing files...")
            results = generator.process_all_markdown_files(dry_run=False)
            
            print(f"\nProcessing complete:")
            print(f"  Files processed: {results['processed']}")
            print(f"  Files skipped: {results['skipped']}")
            print(f"  Errors: {results['errors']}")
            
            # Save results
            results_file = generator.docs_root / "frontmatter_generation_results.json"
            with open(results_file, 'w', encoding='utf-8') as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
            print(f"\nResults saved to: {results_file}")
        else:
            print("Frontmatter generation cancelled.")
    else:
        print("No files need frontmatter generation.")


if __name__ == "__main__":
    main()