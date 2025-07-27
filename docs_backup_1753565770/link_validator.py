#!/usr/bin/env python3
"""
Link Validation and Cross-Reference Checking Tools
Part of the AI-Friendly Documentation Organization project.

This script validates internal links and generates cross-reference
mappings to ensure documentation integrity after migration.
"""

import os
import re
import json
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple, Set
from urllib.parse import urlparse
import yaml


class LinkValidator:
    """Validates internal links and manages cross-references."""
    
    def __init__(self, docs_root: str = "K:\\github\\digitalmodel\\docs"):
        """Initialize validator with docs root directory."""
        self.docs_root = Path(docs_root)
        self.markdown_files = []
        self.link_patterns = [
            r'\[([^\]]+)\]\(([^)]+)\)',  # Standard markdown links
            r'<([^>]+\.md)>',           # Angle bracket links
            r'@([^/\s]+(?:/[^/\s]+)*\.md)',  # @ reference links
        ]
        self.cross_references = {}
        self.broken_links = {}
        self.external_links = {}
        
    def discover_markdown_files(self) -> List[Path]:
        """Discover all markdown files in the documentation."""
        print("Discovering markdown files...")
        
        self.markdown_files = list(self.docs_root.rglob("*.md"))
        print(f"Found {len(self.markdown_files)} markdown files")
        
        return self.markdown_files
        
    def extract_links_from_content(self, content: str) -> List[Tuple[str, str, str]]:
        """Extract all links from markdown content."""
        links = []
        
        for pattern in self.link_patterns:
            matches = re.finditer(pattern, content)
            for match in matches:
                if pattern == r'\[([^\]]+)\]\(([^)]+)\)':
                    # Standard markdown link [text](url)
                    text = match.group(1)
                    url = match.group(2)
                    link_type = "markdown"
                elif pattern == r'<([^>]+\.md)>':
                    # Angle bracket link <file.md>
                    text = match.group(1)
                    url = match.group(1)
                    link_type = "angle"
                elif pattern == r'@([^/\s]+(?:/[^/\s]+)*\.md)':
                    # @ reference link @path/file.md
                    text = match.group(1)
                    url = match.group(1)
                    link_type = "reference"
                    
                links.append((text, url, link_type))
                
        return links
        
    def extract_links_from_frontmatter(self, file_path: Path) -> List[str]:
        """Extract related links from frontmatter."""
        try:
            content = file_path.read_text(encoding='utf-8')
            
            if not content.strip().startswith('---'):
                return []
                
            # Find frontmatter section
            lines = content.split('\n')
            end_idx = -1
            for i, line in enumerate(lines[1:], 1):
                if line.strip() == '---':
                    end_idx = i
                    break
                    
            if end_idx == -1:
                return []
                
            frontmatter_content = '\n'.join(lines[1:end_idx])
            
            try:
                frontmatter = yaml.safe_load(frontmatter_content)
                related = frontmatter.get('related', [])
                return related if isinstance(related, list) else []
            except yaml.YAMLError:
                return []
                
        except Exception:
            return []
            
    def is_external_link(self, url: str) -> bool:
        """Check if a URL is an external link."""
        parsed = urlparse(url)
        return bool(parsed.scheme) or url.startswith('//') or url.startswith('mailto:')
        
    def resolve_relative_path(self, file_path: Path, link_url: str) -> Optional[Path]:
        """Resolve a relative link URL to an absolute path."""
        try:
            # Handle different link formats
            if link_url.startswith('/'):
                # Absolute path from docs root
                target_path = self.docs_root / link_url.lstrip('/')
            elif link_url.startswith('../'):
                # Relative path going up
                target_path = (file_path.parent / link_url).resolve()
            elif link_url.startswith('./'):
                # Relative path in same directory
                target_path = (file_path.parent / link_url[2:]).resolve()
            else:
                # Relative path in same directory (no prefix)
                target_path = (file_path.parent / link_url).resolve()
                
            # Normalize path separators
            target_path = Path(str(target_path).replace('\\', '/'))
            
            return target_path
            
        except Exception:
            return None
            
    def validate_file_links(self, file_path: Path) -> Dict[str, Any]:
        """Validate all links in a single file."""
        try:
            content = file_path.read_text(encoding='utf-8')
        except Exception as e:
            return {
                "file": str(file_path),
                "error": f"Could not read file: {e}",
                "valid_links": [],
                "broken_links": [],
                "external_links": [],
                "frontmatter_links": []
            }
            
        # Extract links from content and frontmatter
        content_links = self.extract_links_from_content(content)
        frontmatter_links = self.extract_links_from_frontmatter(file_path)
        
        valid_links = []
        broken_links = []
        external_links = []
        
        # Validate content links
        for text, url, link_type in content_links:
            # Skip anchors and fragments
            clean_url = url.split('#')[0] if '#' in url else url
            
            if self.is_external_link(clean_url):
                external_links.append({
                    "text": text,
                    "url": url,
                    "type": link_type
                })
                continue
                
            # Resolve and validate internal link
            target_path = self.resolve_relative_path(file_path, clean_url)
            
            if target_path and target_path.exists():
                valid_links.append({
                    "text": text,
                    "url": url,
                    "type": link_type,
                    "target": str(target_path.relative_to(self.docs_root))
                })
            else:
                broken_links.append({
                    "text": text,
                    "url": url,
                    "type": link_type,
                    "expected_target": str(target_path) if target_path else "Unknown"
                })
                
        # Validate frontmatter links
        frontmatter_valid = []
        frontmatter_broken = []
        
        for url in frontmatter_links:
            target_path = self.resolve_relative_path(file_path, url)
            
            if target_path and target_path.exists():
                frontmatter_valid.append({
                    "url": url,
                    "target": str(target_path.relative_to(self.docs_root))
                })
            else:
                frontmatter_broken.append({
                    "url": url,
                    "expected_target": str(target_path) if target_path else "Unknown"
                })
                
        return {
            "file": str(file_path.relative_to(self.docs_root)),
            "valid_links": valid_links,
            "broken_links": broken_links,
            "external_links": external_links,
            "frontmatter_links": {
                "valid": frontmatter_valid,
                "broken": frontmatter_broken
            },
            "total_links": len(content_links) + len(frontmatter_links)
        }
        
    def validate_all_links(self) -> Dict[str, Any]:
        """Validate links in all markdown files."""
        print("Validating all internal links...")
        
        if not self.markdown_files:
            self.discover_markdown_files()
            
        validation_results = {
            "files_processed": 0,
            "files_with_errors": 0,
            "total_links": 0,
            "valid_links": 0,
            "broken_links": 0,
            "external_links": 0,
            "file_results": []
        }
        
        for file_path in self.markdown_files:
            file_result = self.validate_file_links(file_path)
            validation_results["file_results"].append(file_result)
            
            if "error" in file_result:
                validation_results["files_with_errors"] += 1
            else:
                validation_results["total_links"] += file_result["total_links"]
                validation_results["valid_links"] += len(file_result["valid_links"])
                validation_results["broken_links"] += len(file_result["broken_links"])
                validation_results["external_links"] += len(file_result["external_links"])
                
                # Add to broken links collection
                if file_result["broken_links"]:
                    self.broken_links[str(file_path)] = file_result["broken_links"]
                    
            validation_results["files_processed"] += 1
            
            if validation_results["files_processed"] % 50 == 0:
                print(f"  Processed {validation_results['files_processed']} files...")
                
        return validation_results
        
    def generate_cross_reference_map(self) -> Dict[str, Any]:
        """Generate comprehensive cross-reference mapping."""
        print("Generating cross-reference map...")
        
        if not self.markdown_files:
            self.discover_markdown_files()
            
        cross_ref_map = {
            "files": {},
            "incoming_links": {},  # Files that are linked TO
            "outgoing_links": {},  # Files that link FROM
            "orphaned_files": [],  # Files with no incoming links
            "isolated_files": [],  # Files with no incoming or outgoing links
            "hub_files": []        # Files with many incoming links
        }
        
        # First pass: collect all outgoing links
        for file_path in self.markdown_files:
            file_key = str(file_path.relative_to(self.docs_root))
            file_result = self.validate_file_links(file_path)
            
            outgoing_links = []
            
            # Add valid content links
            for link in file_result.get("valid_links", []):
                outgoing_links.append(link["target"])
                
            # Add valid frontmatter links
            fm_links = file_result.get("frontmatter_links", {})
            for link in fm_links.get("valid", []):
                outgoing_links.append(link["target"])
                
            cross_ref_map["files"][file_key] = {
                "outgoing_links": outgoing_links,
                "incoming_links": [],  # Will be populated in second pass
                "total_outgoing": len(outgoing_links)
            }
            
            cross_ref_map["outgoing_links"][file_key] = outgoing_links
            
        # Second pass: calculate incoming links
        for file_key, file_data in cross_ref_map["files"].items():
            for target in file_data["outgoing_links"]:
                if target in cross_ref_map["files"]:
                    cross_ref_map["files"][target]["incoming_links"].append(file_key)
                    
        # Calculate incoming link counts and identify special categories
        for file_key, file_data in cross_ref_map["files"].items():
            incoming_count = len(file_data["incoming_links"])
            outgoing_count = file_data["total_outgoing"]
            
            file_data["total_incoming"] = incoming_count
            
            # Categorize files
            if incoming_count == 0:
                cross_ref_map["orphaned_files"].append(file_key)
                
            if incoming_count == 0 and outgoing_count == 0:
                cross_ref_map["isolated_files"].append(file_key)
                
            if incoming_count >= 5:  # Files with 5+ incoming links
                cross_ref_map["hub_files"].append({
                    "file": file_key,
                    "incoming_links": incoming_count
                })
                
        # Sort hub files by incoming link count
        cross_ref_map["hub_files"].sort(key=lambda x: x["incoming_links"], reverse=True)
        
        # Store for later use
        self.cross_references = cross_ref_map
        
        return cross_ref_map
        
    def generate_link_report(self) -> str:
        """Generate a comprehensive link validation report."""
        validation_results = self.validate_all_links()
        cross_ref_map = self.generate_cross_reference_map()
        
        report = []
        report.append("# Documentation Link Validation Report")
        report.append(f"Generated: {os.environ.get('DATE', 'Unknown')}")
        report.append("")
        
        # Summary statistics
        report.append("## Summary")
        report.append(f"- **Files processed**: {validation_results['files_processed']}")
        report.append(f"- **Total links found**: {validation_results['total_links']}")
        report.append(f"- **Valid internal links**: {validation_results['valid_links']}")
        report.append(f"- **Broken internal links**: {validation_results['broken_links']}")
        report.append(f"- **External links**: {validation_results['external_links']}")
        report.append(f"- **Files with errors**: {validation_results['files_with_errors']}")
        report.append("")
        
        # Cross-reference statistics
        report.append("## Cross-Reference Analysis")
        report.append(f"- **Orphaned files** (no incoming links): {len(cross_ref_map['orphaned_files'])}")
        report.append(f"- **Isolated files** (no links at all): {len(cross_ref_map['isolated_files'])}")
        report.append(f"- **Hub files** (5+ incoming links): {len(cross_ref_map['hub_files'])}")
        report.append("")
        
        # Top hub files
        if cross_ref_map['hub_files']:
            report.append("### Most Referenced Files")
            for hub in cross_ref_map['hub_files'][:10]:
                report.append(f"- `{hub['file']}` ({hub['incoming_links']} incoming links)")
            report.append("")
            
        # Broken links by file
        if validation_results['broken_links'] > 0:
            report.append("## Broken Links by File")
            for file_result in validation_results['file_results']:
                if file_result.get('broken_links'):
                    report.append(f"### {file_result['file']}")
                    for broken in file_result['broken_links']:
                        report.append(f"- `{broken['url']}` (expected: `{broken['expected_target']}`)")
                    report.append("")
                    
        # Orphaned files
        if cross_ref_map['orphaned_files']:
            report.append("## Orphaned Files")
            report.append("Files that are not linked to by any other documentation:")
            report.append("")
            for orphan in cross_ref_map['orphaned_files'][:20]:
                report.append(f"- `{orphan}`")
            if len(cross_ref_map['orphaned_files']) > 20:
                report.append(f"- ... and {len(cross_ref_map['orphaned_files']) - 20} more")
            report.append("")
            
        return "\n".join(report)
        
    def fix_common_link_issues(self, dry_run: bool = True) -> Dict[str, Any]:
        """Attempt to fix common link issues automatically."""
        print(f"Analyzing common link issues (dry_run={dry_run})...")
        
        fixes = {
            "fixes_applied": 0,
            "fixes_suggested": 0,
            "files_modified": [],
            "suggestions": []
        }
        
        # First validate all links to identify issues
        validation_results = self.validate_all_links()
        
        for file_result in validation_results['file_results']:
            if not file_result.get('broken_links'):
                continue
                
            file_path = self.docs_root / file_result['file']
            content = file_path.read_text(encoding='utf-8')
            modified_content = content
            file_modified = False
            
            for broken in file_result['broken_links']:
                url = broken['url']
                
                # Try to find similar files
                similar_files = self._find_similar_files(url)
                
                if similar_files:
                    suggestion = {
                        "file": file_result['file'],
                        "broken_link": url,
                        "suggestions": similar_files[:3]  # Top 3 suggestions
                    }
                    fixes["suggestions"].append(suggestion)
                    fixes["fixes_suggested"] += 1
                    
                    # For very confident matches, apply fix automatically
                    if not dry_run and len(similar_files) == 1:
                        best_match = similar_files[0]
                        if best_match['confidence'] > 0.8:
                            # Replace the broken link
                            old_pattern = re.escape(url)
                            new_url = best_match['file']
                            modified_content = re.sub(
                                f'\\]\\({old_pattern}\\)',
                                f']({new_url})',
                                modified_content
                            )
                            file_modified = True
                            
            if file_modified and not dry_run:
                file_path.write_text(modified_content, encoding='utf-8')
                fixes["files_modified"].append(file_result['file'])
                fixes["fixes_applied"] += 1
                
        return fixes
        
    def _find_similar_files(self, broken_url: str) -> List[Dict[str, Any]]:
        """Find files similar to a broken URL."""
        similar_files = []
        
        # Extract filename from broken URL
        broken_filename = Path(broken_url).name
        broken_stem = Path(broken_url).stem
        
        for md_file in self.markdown_files:
            file_rel = md_file.relative_to(self.docs_root)
            filename = md_file.name
            stem = md_file.stem
            
            confidence = 0.0
            
            # Exact filename match
            if filename == broken_filename:
                confidence = 1.0
            # Exact stem match
            elif stem == broken_stem:
                confidence = 0.9
            # Stem similarity
            elif broken_stem.lower() in stem.lower() or stem.lower() in broken_stem.lower():
                confidence = 0.7
            # Filename similarity
            elif broken_filename.lower() in filename.lower():
                confidence = 0.5
                
            if confidence > 0:
                similar_files.append({
                    "file": str(file_rel),
                    "confidence": confidence,
                    "reason": f"Filename similarity: {filename}"
                })
                
        # Sort by confidence
        similar_files.sort(key=lambda x: x['confidence'], reverse=True)
        return similar_files
        
    def save_results(self, output_dir: Optional[Path] = None) -> Dict[str, Path]:
        """Save validation results to files."""
        if output_dir is None:
            output_dir = self.docs_root
            
        saved_files = {}
        
        # Save validation results
        validation_results = self.validate_all_links()
        validation_file = output_dir / "link_validation_results.json"
        with open(validation_file, 'w', encoding='utf-8') as f:
            json.dump(validation_results, f, indent=2, ensure_ascii=False)
        saved_files["validation"] = validation_file
        
        # Save cross-reference map
        cross_ref_map = self.generate_cross_reference_map()
        cross_ref_file = output_dir / "cross_reference_map.json"
        with open(cross_ref_file, 'w', encoding='utf-8') as f:
            json.dump(cross_ref_map, f, indent=2, ensure_ascii=False)
        saved_files["cross_references"] = cross_ref_file
        
        # Save report
        report = self.generate_link_report()
        report_file = output_dir / "link_validation_report.md"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(report)
        saved_files["report"] = report_file
        
        return saved_files


def main():
    """Main function to run link validation."""
    validator = LinkValidator()
    
    print("AI-Friendly Documentation Link Validator")
    print("=" * 50)
    
    # Discover files
    validator.discover_markdown_files()
    
    print("1. Validating all links...")
    validation_results = validator.validate_all_links()
    
    print(f"\nValidation Results:")
    print(f"  Files processed: {validation_results['files_processed']}")
    print(f"  Total links: {validation_results['total_links']}")
    print(f"  Valid links: {validation_results['valid_links']}")
    print(f"  Broken links: {validation_results['broken_links']}")
    print(f"  External links: {validation_results['external_links']}")
    
    print("\n2. Generating cross-reference map...")
    cross_ref_map = validator.generate_cross_reference_map()
    
    print(f"\nCross-Reference Analysis:")
    print(f"  Orphaned files: {len(cross_ref_map['orphaned_files'])}")
    print(f"  Isolated files: {len(cross_ref_map['isolated_files'])}")
    print(f"  Hub files: {len(cross_ref_map['hub_files'])}")
    
    print("\n3. Analyzing fixable issues...")
    fix_analysis = validator.fix_common_link_issues(dry_run=True)
    
    print(f"\nFix Analysis:")
    print(f"  Fixable issues: {fix_analysis['fixes_suggested']}")
    
    print("\n4. Saving results...")
    saved_files = validator.save_results()
    
    print(f"\nResults saved:")
    for file_type, file_path in saved_files.items():
        print(f"  {file_type}: {file_path}")
        
    print("\nLink validation completed!")


if __name__ == "__main__":
    main()