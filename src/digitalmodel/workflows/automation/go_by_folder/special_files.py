"""
Special files handler for Create Go-By Folder Tool
Handles symlinks, hard links, and other special file types
"""

import os
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import logging

logger = logging.getLogger(__name__)


class SpecialFilesHandler:
    """Handle special files like symlinks, hard links, etc."""
    
    def __init__(self):
        """Initialize special files handler."""
        self.symlinks = []
        self.hardlinks = {}
        self.special_files = []
        self.decisions = {}
        
    def handle_symlinks(self, symlink_registry: Dict) -> Dict:
        """
        Process symlinks and determine handling strategy.
        
        Args:
            symlink_registry: Dictionary of discovered symlinks
            
        Returns:
            Dictionary with handling decisions
        """
        if not symlink_registry:
            return {'action': 'none', 'count': 0}
        
        symlink_list = []
        for rel_path, info in symlink_registry.items():
            symlink_list.append({
                'path': str(rel_path),
                'target': str(info.get('target', 'unknown')),
                'type': info.get('type', 'unknown'),
                'valid': info.get('valid', False)
            })
        
        # Log symlinks found
        logger.info(f"Found {len(symlink_list)} symlinks")
        for link in symlink_list[:5]:  # Show first 5
            logger.debug(f"  {link['path']} -> {link['target']}")
        
        # Store for documentation
        self.symlinks = symlink_list
        
        # Return decision (would prompt user in full implementation)
        return {
            'action': 'document',
            'count': len(symlink_list),
            'symlinks': symlink_list
        }
    
    def check_hardlinks(self, file_path: Path) -> bool:
        """
        Check if file is a hard link (has multiple names).
        
        Args:
            file_path: Path to check
            
        Returns:
            True if file has multiple hard links
        """
        try:
            stat = file_path.stat()
            # On Windows, st_nlink is always 1, so this mainly works on Unix
            return stat.st_nlink > 1
        except Exception as e:
            logger.debug(f"Could not check hard links for {file_path}: {e}")
            return False
    
    def identify_special_files(self, files: List[Dict]) -> List[Dict]:
        """
        Identify special files that need special handling.
        
        Args:
            files: List of file information dictionaries
            
        Returns:
            List of special files found
        """
        special = []
        
        for file_info in files:
            file_path = file_info.get('path')
            if not file_path:
                continue
            
            # Check for various special cases
            special_type = None
            
            # Check file extension for special types
            ext = file_info.get('extension', '').lower()
            
            # Socket files (Unix)
            if ext == '.sock':
                special_type = 'socket'
            
            # Named pipes (FIFOs)
            elif ext in ['.fifo', '.pipe']:
                special_type = 'pipe'
            
            # Device files
            elif ext == '.device':
                special_type = 'device'
            
            # Check if it's a hard link
            elif self.check_hardlinks(file_path):
                special_type = 'hardlink'
                # Track inode/device for grouping hard links
                try:
                    stat = file_path.stat()
                    key = (stat.st_dev, stat.st_ino)
                    if key not in self.hardlinks:
                        self.hardlinks[key] = []
                    self.hardlinks[key].append(file_path)
                except Exception:
                    pass
            
            if special_type:
                special.append({
                    'path': file_path,
                    'type': special_type,
                    'info': file_info
                })
        
        self.special_files = special
        
        if special:
            logger.info(f"Found {len(special)} special files")
            
        return special
    
    def prompt_user_decision(self, prompt_type: str, context: Dict) -> str:
        """
        Simulate user prompt for decision (placeholder for actual implementation).
        
        Args:
            prompt_type: Type of prompt (symlinks, compressed, etc.)
            context: Context information for the prompt
            
        Returns:
            Decision string
        """
        # In full implementation, this would actually prompt the user
        # For now, return sensible defaults
        
        if prompt_type == 'symlinks':
            # Default: document symlinks but don't follow them
            return 'document'
        
        elif prompt_type == 'compressed':
            # Default: keep compressed files as-is
            return 'keep'
        
        elif prompt_type == 'large_duplicate':
            # Default: keep first, skip others
            return 'keep_first'
        
        else:
            return 'skip'
    
    def handle_compressed_files(self, files: List[Dict]) -> Tuple[List[Dict], List[Dict]]:
        """
        Identify and handle compressed files.
        
        Args:
            files: List of file information
            
        Returns:
            Tuple of (files_to_process, compressed_files)
        """
        compressed_extensions = {
            '.zip', '.tar', '.gz', '.bz2', '.xz', '.7z', '.rar',
            '.tar.gz', '.tar.bz2', '.tar.xz', '.tgz'
        }
        
        compressed = []
        regular = []
        
        for file_info in files:
            ext = file_info.get('extension', '').lower()
            
            # Check for compound extensions
            name = file_info.get('relative_path', Path('')).name.lower()
            is_compressed = False
            
            for comp_ext in compressed_extensions:
                if name.endswith(comp_ext):
                    is_compressed = True
                    break
            
            if is_compressed:
                compressed.append(file_info)
                logger.debug(f"Found compressed file: {file_info.get('relative_path')}")
            else:
                regular.append(file_info)
        
        if compressed:
            logger.info(f"Found {len(compressed)} compressed files")
            # Would prompt user here in full implementation
            decision = self.prompt_user_decision('compressed', {'count': len(compressed)})
            self.decisions['compressed_files'] = decision
        
        return regular, compressed
    
    def handle_duplicates(self, files: List[Dict]) -> Tuple[List[Dict], Dict]:
        """
        Identify and handle duplicate files based on hash.
        
        Args:
            files: List of file information with hashes
            
        Returns:
            Tuple of (unique_files, duplicate_map)
        """
        hash_map = {}
        duplicates = {}
        unique_files = []
        
        for file_info in files:
            file_hash = file_info.get('hash')
            if not file_hash:
                # No hash, consider unique
                unique_files.append(file_info)
                continue
            
            if file_hash not in hash_map:
                hash_map[file_hash] = file_info
                unique_files.append(file_info)
            else:
                # Duplicate found
                if file_hash not in duplicates:
                    duplicates[file_hash] = [hash_map[file_hash]]
                duplicates[file_hash].append(file_info)
        
        # Handle large duplicates
        for file_hash, dup_list in duplicates.items():
            if dup_list and dup_list[0].get('size', 0) > 50 * 1024:  # >50KB
                logger.info(
                    f"Found {len(dup_list)} duplicates of "
                    f"{dup_list[0].get('relative_path')} ({dup_list[0].get('size', 0)} bytes)"
                )
                # Would prompt user here in full implementation
                decision = self.prompt_user_decision('large_duplicate', {
                    'count': len(dup_list),
                    'size': dup_list[0].get('size', 0)
                })
                self.decisions[f'duplicate_{file_hash}'] = decision
        
        return unique_files, duplicates
    
    def create_documentation(self, target_path: Path) -> None:
        """
        Create documentation for special files handling.
        
        Args:
            target_path: Path to target go-by folder
        """
        if not (self.symlinks or self.special_files or self.decisions):
            return
        
        doc_content = "# Special Files Handling\n\n"
        
        if self.symlinks:
            doc_content += "## Symbolic Links\n\n"
            doc_content += f"Found {len(self.symlinks)} symbolic links:\n\n"
            
            for link in self.symlinks[:20]:  # Show first 20
                doc_content += f"- `{link['path']}` → `{link['target']}` "
                doc_content += f"({'valid' if link.get('valid') else 'broken'})\n"
            
            if len(self.symlinks) > 20:
                doc_content += f"\n... and {len(self.symlinks) - 20} more\n"
            
            doc_content += "\n**Decision**: Document only (not followed)\n\n"
        
        if self.hardlinks:
            doc_content += "## Hard Links\n\n"
            doc_content += f"Found {len(self.hardlinks)} groups of hard-linked files:\n\n"
            
            for i, (key, paths) in enumerate(self.hardlinks.items(), 1):
                if i > 10:  # Show first 10 groups
                    doc_content += f"\n... and {len(self.hardlinks) - 10} more groups\n"
                    break
                doc_content += f"Group {i} ({len(paths)} files):\n"
                for path in paths[:3]:
                    doc_content += f"  - {path}\n"
                if len(paths) > 3:
                    doc_content += f"  ... and {len(paths) - 3} more\n"
        
        if self.special_files:
            doc_content += "## Other Special Files\n\n"
            by_type = {}
            for special in self.special_files:
                ftype = special.get('type', 'unknown')
                if ftype not in by_type:
                    by_type[ftype] = []
                by_type[ftype].append(special)
            
            for ftype, items in by_type.items():
                doc_content += f"- **{ftype}**: {len(items)} files\n"
        
        if self.decisions:
            doc_content += "\n## Handling Decisions\n\n"
            for key, decision in self.decisions.items():
                doc_content += f"- **{key}**: {decision}\n"
        
        # Write documentation
        doc_path = target_path / 'SPECIAL_FILES.md'
        doc_path.write_text(doc_content, encoding='utf-8')
        logger.info(f"Created special files documentation at {doc_path}")