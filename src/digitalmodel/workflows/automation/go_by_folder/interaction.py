"""
User interaction module for Create Go-By Folder Tool
"""

import sys
import os
from pathlib import Path
from typing import Optional, List, Dict, Any, Tuple
from enum import Enum
import logging
from datetime import datetime

logger = logging.getLogger(__name__)


class Decision(Enum):
    """User decision types."""
    YES = "yes"
    NO = "no"
    YES_TO_ALL = "yes_to_all"
    NO_TO_ALL = "no_to_all"
    SKIP = "skip"
    ABORT = "abort"


class InteractionMode(Enum):
    """Interaction modes."""
    INTERACTIVE = "interactive"
    AUTO_YES = "auto_yes"
    AUTO_NO = "auto_no"
    SILENT = "silent"


class UserInteraction:
    """Handle user interactions for go-by folder creation."""
    
    def __init__(self, mode: InteractionMode = InteractionMode.INTERACTIVE):
        """
        Initialize user interaction handler.
        
        Args:
            mode: Interaction mode
        """
        self.mode = mode
        self.decisions_log = []
        self._remember_all = {}  # Remember "all" decisions
        self._is_tty = sys.stdin.isatty()
    
    def confirm_overwrite(self, path: Path) -> bool:
        """
        Confirm overwriting existing folder.
        
        Args:
            path: Path to folder
            
        Returns:
            True if should overwrite
        """
        if not path.exists():
            return True
        
        if self.mode == InteractionMode.AUTO_YES:
            return True
        elif self.mode == InteractionMode.AUTO_NO:
            return False
        elif self.mode == InteractionMode.SILENT:
            return False
        
        message = f"Folder '{path}' already exists. Overwrite?"
        decision = self._prompt_decision(message, "overwrite")
        
        return decision in [Decision.YES, Decision.YES_TO_ALL]
    
    def confirm_large_folder(self, size: int, file_count: int) -> bool:
        """
        Confirm processing large folder.
        
        Args:
            size: Folder size in bytes
            file_count: Number of files
            
        Returns:
            True if should proceed
        """
        if self.mode in [InteractionMode.AUTO_YES, InteractionMode.SILENT]:
            return True
        elif self.mode == InteractionMode.AUTO_NO:
            return False
        
        size_gb = size / (1024 ** 3)
        message = (
            f"WARNING: Large folder detected:\n"
            f"   Size: {size_gb:.2f} GB\n"
            f"   Files: {file_count:,}\n"
            f"   This may take several minutes. Continue?"
        )
        
        decision = self._prompt_decision(message, "large_folder")
        return decision in [Decision.YES, Decision.YES_TO_ALL]
    
    def handle_compressed_file(self, file_path: Path, size: int) -> str:
        """
        Handle compressed file decision.
        
        Args:
            file_path: Path to compressed file
            size: File size in bytes
            
        Returns:
            'extract', 'keep', or 'skip'
        """
        if self.mode == InteractionMode.SILENT:
            return 'keep'
        
        # Check remembered decisions
        if 'compressed_files' in self._remember_all:
            return self._remember_all['compressed_files']
        
        if self.mode == InteractionMode.AUTO_YES:
            return 'extract'
        elif self.mode == InteractionMode.AUTO_NO:
            return 'keep'
        
        size_mb = size / (1024 * 1024)
        message = (
            f"Found compressed file: {file_path.name}\n"
            f"   Size: {size_mb:.2f} MB\n"
            f"   Options:\n"
            f"   1) Extract and process contents\n"
            f"   2) Keep as compressed file\n"
            f"   3) Skip this file\n"
            f"   Choice (1/2/3)?"
        )
        
        choice = self._prompt_choice(message, ['1', '2', '3'], 'compressed_file')
        
        if choice == '1':
            return 'extract'
        elif choice == '2':
            return 'keep'
        else:
            return 'skip'
    
    def handle_symlink(self, symlink_path: Path, target_path: Path) -> str:
        """
        Handle symbolic link decision.
        
        Args:
            symlink_path: Path to symlink
            target_path: Target of symlink
            
        Returns:
            'follow', 'preserve', or 'skip'
        """
        if self.mode == InteractionMode.SILENT:
            return 'preserve'
        
        # Check remembered decisions
        if 'symlinks' in self._remember_all:
            return self._remember_all['symlinks']
        
        if self.mode == InteractionMode.AUTO_YES:
            return 'follow'
        elif self.mode == InteractionMode.AUTO_NO:
            return 'preserve'
        
        message = (
            f"Found symbolic link: {symlink_path.name}\n"
            f"   Target: {target_path}\n"
            f"   Options:\n"
            f"   1) Follow link and process target\n"
            f"   2) Preserve as symlink\n"
            f"   3) Skip this link\n"
            f"   Choice (1/2/3)?"
        )
        
        choice = self._prompt_choice(message, ['1', '2', '3'], 'symlink')
        
        if choice == '1':
            return 'follow'
        elif choice == '2':
            return 'preserve'
        else:
            return 'skip'
    
    def handle_sensitive_file(self, file_path: Path, reason: str) -> bool:
        """
        Handle potentially sensitive file.
        
        Args:
            file_path: Path to file
            reason: Reason why file might be sensitive
            
        Returns:
            True if should process
        """
        if self.mode in [InteractionMode.AUTO_YES, InteractionMode.SILENT]:
            # Never auto-process sensitive files
            logger.warning(f"Skipping potentially sensitive file: {file_path} ({reason})")
            return False
        elif self.mode == InteractionMode.AUTO_NO:
            return False
        
        message = (
            f"WARNING: Potentially sensitive file detected:\n"
            f"   File: {file_path.name}\n"
            f"   Reason: {reason}\n"
            f"   Include in go-by folder?"
        )
        
        decision = self._prompt_decision(message, "sensitive_file")
        return decision in [Decision.YES, Decision.YES_TO_ALL]
    
    def show_preview(self, stats: Dict[str, Any]) -> bool:
        """
        Show preview and confirm proceeding.
        
        Args:
            stats: Statistics dictionary
            
        Returns:
            True if should proceed
        """
        if self.mode in [InteractionMode.AUTO_YES, InteractionMode.SILENT]:
            return True
        elif self.mode == InteractionMode.AUTO_NO:
            return False
        
        print("\n" + "=" * 60)
        print(">> Preview of go-by folder creation:")
        print("=" * 60)
        
        print(f"\n   Source: {stats.get('source_path', 'Unknown')}")
        print(f"   Target: {stats.get('target_path', 'Unknown')}")
        
        print(f"\nStatistics:")
        print(f"   Total files: {stats.get('total_files', 0):,}")
        print(f"   Total size: {self._format_size(stats.get('total_size', 0))}")
        print(f"   Estimated reduction: >99%")
        
        if 'file_types' in stats:
            print(f"\nFile types: {len(stats['file_types'])}")
            # Show top 5 file types
            sorted_types = sorted(
                stats['file_types'].items(),
                key=lambda x: x[1] if isinstance(x[1], int) else x[1].get('count', 0),
                reverse=True
            )
            for ext, info in sorted_types[:5]:
                count = info if isinstance(info, int) else info.get('count', 0)
                print(f"   {ext}: {count} files")
            if len(sorted_types) > 5:
                print(f"   ... and {len(sorted_types) - 5} more types")
        
        print("\n" + "=" * 60)
        
        decision = self._prompt_decision("\nProceed with go-by folder creation?", "proceed")
        return decision in [Decision.YES, Decision.YES_TO_ALL]
    
    def report_completion(self, success: bool, stats: Dict[str, Any]) -> None:
        """
        Report completion to user.
        
        Args:
            success: Whether operation succeeded
            stats: Completion statistics
        """
        if self.mode == InteractionMode.SILENT:
            return
        
        print("\n" + "=" * 60)
        
        if success:
            print(">> Go-by folder created successfully!")
        else:
            print(">> Go-by folder creation completed with issues")
        
        if 'target_path' in stats:
            print(f"\n   Location: {stats['target_path']}")
        
        if 'final_size' in stats:
            original = stats.get('original_size', 0)
            final = stats['final_size']
            reduction = ((original - final) / original * 100) if original > 0 else 0
            
            print(f"\nSize reduction:")
            print(f"   Original: {self._format_size(original)}")
            print(f"   Final: {self._format_size(final)}")
            print(f"   Reduction: {reduction:.1f}%")
        
        if 'processing_time' in stats:
            print(f"\n⏱️  Processing time: {stats['processing_time']:.1f} seconds")
        
        if 'errors' in stats and stats['errors']:
            print(f"\n   Errors: {stats['errors']}")
        
        print("\n" + "=" * 60)
    
    def prompt_custom(self, message: str, options: Optional[List[str]] = None) -> str:
        """
        Custom prompt for user input.
        
        Args:
            message: Prompt message
            options: Optional list of valid options
            
        Returns:
            User response
        """
        if not self._is_tty or self.mode == InteractionMode.SILENT:
            return options[0] if options else ""
        
        if self.mode == InteractionMode.AUTO_YES:
            return options[0] if options else "yes"
        elif self.mode == InteractionMode.AUTO_NO:
            return options[-1] if options else "no"
        
        print(f"\n{message}")
        
        if options:
            print(f"Options: {', '.join(options)}")
            while True:
                response = input("> ").strip().lower()
                if response in [o.lower() for o in options]:
                    return response
                print(f"Invalid option. Choose from: {', '.join(options)}")
        else:
            return input("> ").strip()
    
    def _prompt_decision(self, message: str, context: str) -> Decision:
        """
        Prompt for yes/no decision.
        
        Args:
            message: Prompt message
            context: Context for remembering decisions
            
        Returns:
            User decision
        """
        # Check if we have a remembered "all" decision
        if context in self._remember_all:
            return self._remember_all[context]
        
        if not self._is_tty:
            return Decision.NO
        
        print(f"\n{message}")
        print("Options: [y]es, [n]o, [a]ll yes, [N]one, [s]kip, [q]uit")
        
        while True:
            response = input("> ").strip().lower()
            
            if response in ['y', 'yes']:
                decision = Decision.YES
            elif response in ['n', 'no']:
                decision = Decision.NO
            elif response in ['a', 'all']:
                decision = Decision.YES_TO_ALL
                self._remember_all[context] = Decision.YES
            elif response in ['none', 'N']:
                decision = Decision.NO_TO_ALL
                self._remember_all[context] = Decision.NO
            elif response in ['s', 'skip']:
                decision = Decision.SKIP
            elif response in ['q', 'quit', 'abort']:
                decision = Decision.ABORT
            else:
                print("Invalid option. Please choose: y/n/a/N/s/q")
                continue
            
            # Log decision
            self.decisions_log.append({
                'timestamp': datetime.now().isoformat(),
                'context': context,
                'message': message[:100],
                'decision': decision.value
            })
            
            return decision
    
    def _prompt_choice(self, message: str, options: List[str], context: str) -> str:
        """
        Prompt for choice from options.
        
        Args:
            message: Prompt message
            options: List of valid options
            context: Context for logging
            
        Returns:
            Selected option
        """
        if not self._is_tty:
            return options[0]
        
        print(f"\n{message}")
        
        while True:
            response = input("> ").strip()
            
            if response in options:
                # Log choice
                self.decisions_log.append({
                    'timestamp': datetime.now().isoformat(),
                    'context': context,
                    'message': message[:100],
                    'choice': response
                })
                return response
            
            print(f"Invalid option. Choose from: {', '.join(options)}")
    
    def _format_size(self, size_bytes: int) -> str:
        """Format size in human-readable format."""
        for unit in ['B', 'KB', 'MB', 'GB']:
            if size_bytes < 1024.0:
                return f"{size_bytes:.2f}{unit}"
            size_bytes /= 1024.0
        return f"{size_bytes:.2f}TB"


def create_interaction_handler(config: Dict[str, Any]) -> UserInteraction:
    """
    Create interaction handler from configuration.
    
    Args:
        config: Configuration dictionary
        
    Returns:
        Configured UserInteraction instance
    """
    # Determine mode
    if config.get('yes_to_all', False):
        mode = InteractionMode.AUTO_YES
    elif config.get('no_interaction', False):
        mode = InteractionMode.SILENT
    elif config.get('auto_no', False):
        mode = InteractionMode.AUTO_NO
    else:
        mode = InteractionMode.INTERACTIVE
    
    return UserInteraction(mode=mode)