"""
Checkpoint and recovery module for Create Go-By Folder Tool
"""

import json
import pickle
import shutil
from pathlib import Path
from typing import Dict, Any, Optional, List
from datetime import datetime
from dataclasses import dataclass, asdict, field
import logging
import hashlib

logger = logging.getLogger(__name__)


@dataclass
class CheckpointState:
    """Checkpoint state information."""
    version: str = "1.0.0"
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())
    source_path: str = ""
    target_path: str = ""
    config: Dict = field(default_factory=dict)
    progress: Dict = field(default_factory=dict)
    scan_results: Dict = field(default_factory=dict)
    patterns: Dict = field(default_factory=dict)
    preservation_stats: Dict = field(default_factory=dict)
    processed_files: List[str] = field(default_factory=list)
    failed_files: List[str] = field(default_factory=list)
    current_phase: str = "initialization"
    phase_progress: Dict = field(default_factory=dict)
    
    def to_dict(self) -> Dict:
        """Convert to dictionary."""
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'CheckpointState':
        """Create from dictionary."""
        return cls(**data)


class CheckpointManager:
    """Manage checkpoints for resume capability."""
    
    CHECKPOINT_DIR = '.go_by_checkpoint'
    CHECKPOINT_FILE = 'checkpoint.json'
    BACKUP_FILE = 'checkpoint.backup'
    STATE_FILE = 'state.pkl'
    
    def __init__(self, target_path: Path, enable_backup: bool = True):
        """
        Initialize checkpoint manager.
        
        Args:
            target_path: Target folder path
            enable_backup: Enable checkpoint backups
        """
        self.target_path = Path(target_path)
        self.checkpoint_dir = self.target_path / self.CHECKPOINT_DIR
        self.enable_backup = enable_backup
        self.state = CheckpointState()
        self._auto_save_interval = 100  # Auto-save every N files
        self._last_save_count = 0
    
    def initialize(self, source_path: Path, config: Dict) -> None:
        """
        Initialize checkpoint system.
        
        Args:
            source_path: Source folder path
            config: Configuration dictionary
        """
        self.checkpoint_dir.mkdir(parents=True, exist_ok=True)
        
        # Initialize state
        self.state.source_path = str(source_path)
        self.state.target_path = str(self.target_path)
        self.state.config = config
        self.state.timestamp = datetime.now().isoformat()
        
        # Create initial checkpoint
        self.save()
        logger.info(f"Checkpoint system initialized at {self.checkpoint_dir}")
    
    def save(self, force: bool = False) -> bool:
        """
        Save checkpoint.
        
        Args:
            force: Force save even if interval not reached
            
        Returns:
            True if saved successfully
        """
        try:
            # Check if we should auto-save
            if not force:
                processed_count = len(self.state.processed_files)
                if processed_count - self._last_save_count < self._auto_save_interval:
                    return True  # Skip save, not enough progress
            
            # Backup existing checkpoint if enabled
            if self.enable_backup:
                self._create_backup()
            
            # Save JSON checkpoint
            checkpoint_file = self.checkpoint_dir / self.CHECKPOINT_FILE
            with open(checkpoint_file, 'w', encoding='utf-8') as f:
                json.dump(self.state.to_dict(), f, indent=2, default=str)
            
            # Save pickle state for complex objects
            state_file = self.checkpoint_dir / self.STATE_FILE
            with open(state_file, 'wb') as f:
                pickle.dump(self.state, f)
            
            # Create checksum for verification
            self._create_checksum()
            
            self._last_save_count = len(self.state.processed_files)
            
            logger.debug(f"Checkpoint saved: {len(self.state.processed_files)} files processed")
            return True
            
        except Exception as e:
            logger.error(f"Failed to save checkpoint: {e}")
            return False
    
    def load(self) -> Optional[CheckpointState]:
        """
        Load checkpoint.
        
        Returns:
            Checkpoint state if loaded successfully, None otherwise
        """
        checkpoint_file = self.checkpoint_dir / self.CHECKPOINT_FILE
        state_file = self.checkpoint_dir / self.STATE_FILE
        
        if not checkpoint_file.exists():
            logger.info("No checkpoint found")
            return None
        
        try:
            # Verify checksum
            if not self._verify_checksum():
                logger.warning("Checkpoint checksum verification failed")
                if self.enable_backup:
                    logger.info("Attempting to load from backup")
                    return self._load_from_backup()
                return None
            
            # Try loading pickle state first (more complete)
            if state_file.exists():
                try:
                    with open(state_file, 'rb') as f:
                        self.state = pickle.load(f)
                    logger.info(f"Loaded checkpoint from {state_file}")
                    return self.state
                except Exception as e:
                    logger.warning(f"Failed to load pickle state: {e}")
            
            # Fall back to JSON
            with open(checkpoint_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
            
            self.state = CheckpointState.from_dict(data)
            logger.info(f"Loaded checkpoint from {checkpoint_file}")
            
            # Log resume information
            logger.info(f"Resuming from checkpoint:")
            logger.info(f"  - Phase: {self.state.current_phase}")
            logger.info(f"  - Files processed: {len(self.state.processed_files)}")
            logger.info(f"  - Timestamp: {self.state.timestamp}")
            
            return self.state
            
        except Exception as e:
            logger.error(f"Failed to load checkpoint: {e}")
            if self.enable_backup:
                return self._load_from_backup()
            return None
    
    def update_progress(self, 
                       phase: Optional[str] = None,
                       processed_file: Optional[str] = None,
                       failed_file: Optional[str] = None,
                       **kwargs) -> None:
        """
        Update checkpoint progress.
        
        Args:
            phase: Current phase
            processed_file: File that was processed
            failed_file: File that failed
            **kwargs: Additional progress data
        """
        if phase:
            self.state.current_phase = phase
        
        if processed_file:
            if processed_file not in self.state.processed_files:
                self.state.processed_files.append(processed_file)
        
        if failed_file:
            if failed_file not in self.state.failed_files:
                self.state.failed_files.append(failed_file)
        
        # Update additional progress data
        self.state.progress.update(kwargs)
        
        # Auto-save if interval reached
        if len(self.state.processed_files) % self._auto_save_interval == 0:
            self.save()
    
    def update_phase_progress(self, phase: str, **kwargs) -> None:
        """
        Update phase-specific progress.
        
        Args:
            phase: Phase name
            **kwargs: Phase progress data
        """
        if phase not in self.state.phase_progress:
            self.state.phase_progress[phase] = {}
        
        self.state.phase_progress[phase].update(kwargs)
        self.state.current_phase = phase
    
    def mark_complete(self) -> None:
        """Mark operation as complete and clean up checkpoints."""
        try:
            # Save final state
            self.state.current_phase = "completed"
            self.state.progress['completed'] = True
            self.state.progress['completion_time'] = datetime.now().isoformat()
            self.save(force=True)
            
            # Optionally remove checkpoint directory
            if self.state.config.get('remove_checkpoint_on_success', True):
                self.cleanup()
            else:
                logger.info(f"Checkpoint preserved at {self.checkpoint_dir}")
                
        except Exception as e:
            logger.warning(f"Error marking checkpoint complete: {e}")
    
    def cleanup(self) -> None:
        """Remove checkpoint directory."""
        try:
            if self.checkpoint_dir.exists():
                shutil.rmtree(self.checkpoint_dir)
                logger.info("Checkpoint directory removed")
        except Exception as e:
            logger.warning(f"Failed to remove checkpoint directory: {e}")
    
    def get_resume_info(self) -> Dict[str, Any]:
        """
        Get information for resuming.
        
        Returns:
            Resume information dictionary
        """
        return {
            'can_resume': self.checkpoint_dir.exists(),
            'current_phase': self.state.current_phase,
            'files_processed': len(self.state.processed_files),
            'files_failed': len(self.state.failed_files),
            'last_checkpoint': self.state.timestamp,
            'source_path': self.state.source_path,
            'target_path': self.state.target_path
        }
    
    def should_skip_file(self, file_path: str) -> bool:
        """
        Check if file should be skipped (already processed).
        
        Args:
            file_path: File path to check
            
        Returns:
            True if file should be skipped
        """
        return file_path in self.state.processed_files
    
    def rollback(self, to_phase: Optional[str] = None) -> bool:
        """
        Rollback to a previous phase.
        
        Args:
            to_phase: Phase to rollback to (None for previous phase)
            
        Returns:
            True if rollback successful
        """
        try:
            if to_phase:
                # Rollback to specific phase
                if to_phase not in self.state.phase_progress:
                    logger.error(f"Phase {to_phase} not found in checkpoint")
                    return False
                
                # Reset progress after this phase
                phases = list(self.state.phase_progress.keys())
                phase_index = phases.index(to_phase)
                
                # Remove progress for subsequent phases
                for phase in phases[phase_index + 1:]:
                    del self.state.phase_progress[phase]
                
                self.state.current_phase = to_phase
                
            else:
                # Rollback to previous phase
                phases = list(self.state.phase_progress.keys())
                if len(phases) > 1:
                    del self.state.phase_progress[phases[-1]]
                    self.state.current_phase = phases[-2]
                else:
                    logger.warning("Cannot rollback further")
                    return False
            
            # Save rollback state
            self.save(force=True)
            logger.info(f"Rolled back to phase: {self.state.current_phase}")
            return True
            
        except Exception as e:
            logger.error(f"Rollback failed: {e}")
            return False
    
    def _create_backup(self) -> None:
        """Create backup of current checkpoint."""
        try:
            checkpoint_file = self.checkpoint_dir / self.CHECKPOINT_FILE
            backup_file = self.checkpoint_dir / self.BACKUP_FILE
            
            if checkpoint_file.exists():
                shutil.copy2(checkpoint_file, backup_file)
                
                # Also backup state file
                state_file = self.checkpoint_dir / self.STATE_FILE
                if state_file.exists():
                    backup_state = self.checkpoint_dir / 'state.backup'
                    shutil.copy2(state_file, backup_state)
                    
        except Exception as e:
            logger.warning(f"Failed to create backup: {e}")
    
    def _load_from_backup(self) -> Optional[CheckpointState]:
        """Load from backup checkpoint."""
        try:
            backup_file = self.checkpoint_dir / self.BACKUP_FILE
            backup_state = self.checkpoint_dir / 'state.backup'
            
            if not backup_file.exists():
                logger.warning("No backup checkpoint found")
                return None
            
            # Try loading backup state file first
            if backup_state.exists():
                try:
                    with open(backup_state, 'rb') as f:
                        self.state = pickle.load(f)
                    logger.info("Loaded from backup state file")
                    return self.state
                except Exception as e:
                    logger.warning(f"Failed to load backup state: {e}")
            
            # Fall back to JSON backup
            with open(backup_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
            
            self.state = CheckpointState.from_dict(data)
            logger.info("Loaded from backup checkpoint")
            return self.state
            
        except Exception as e:
            logger.error(f"Failed to load from backup: {e}")
            return None
    
    def _create_checksum(self) -> None:
        """Create checksum for checkpoint verification."""
        try:
            checkpoint_file = self.checkpoint_dir / self.CHECKPOINT_FILE
            checksum_file = self.checkpoint_dir / 'checkpoint.sha256'
            
            if checkpoint_file.exists():
                hasher = hashlib.sha256()
                with open(checkpoint_file, 'rb') as f:
                    hasher.update(f.read())
                
                checksum = hasher.hexdigest()
                checksum_file.write_text(checksum)
                
        except Exception as e:
            logger.warning(f"Failed to create checksum: {e}")
    
    def _verify_checksum(self) -> bool:
        """
        Verify checkpoint checksum.
        
        Returns:
            True if checksum is valid
        """
        try:
            checkpoint_file = self.checkpoint_dir / self.CHECKPOINT_FILE
            checksum_file = self.checkpoint_dir / 'checkpoint.sha256'
            
            if not checksum_file.exists():
                return True  # No checksum to verify
            
            stored_checksum = checksum_file.read_text().strip()
            
            hasher = hashlib.sha256()
            with open(checkpoint_file, 'rb') as f:
                hasher.update(f.read())
            
            current_checksum = hasher.hexdigest()
            
            return stored_checksum == current_checksum
            
        except Exception as e:
            logger.warning(f"Checksum verification failed: {e}")
            return False