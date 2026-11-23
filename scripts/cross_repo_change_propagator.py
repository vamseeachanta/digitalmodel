#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ABOUTME: Cross-repository change propagation tool for workspace-hub.
Propagates configuration files, skills, and standards across multiple repositories.

Usage:
    python scripts/cross_repo_change_propagator.py --file <path> --dry-run
    python scripts/cross_repo_change_propagator.py --file <path> --interactive
    python scripts/cross_repo_change_propagator.py --file <path> --auto --create-branch
"""

import argparse
import fnmatch
import json
import os
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import yaml

# Fix Windows console encoding for emojis
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8', errors='replace')


@dataclass
class PropagationResult:
    """Result of propagating changes to a repository."""

    repo_name: str
    success: bool
    files_changed: List[str] = field(default_factory=list)
    skipped_reason: Optional[str] = None
    conflicts: List[str] = field(default_factory=list)
    commit_hash: Optional[str] = None
    branch_name: Optional[str] = None
    backup_commit: Optional[str] = None


@dataclass
class PropagationConfig:
    """Configuration loaded from propagation-rules.yaml."""

    global_patterns: List[str]
    conditional_rules: List[Dict]
    template_merge: Dict
    repository_tags: Dict[str, List[str]]
    exclude_patterns: List[str]
    safety_config: Dict
    reporting_config: Dict
    base_path: Path


class CrossRepoChangePropagator:
    """Propagates changes across multiple repositories with safety checks."""

    def __init__(self, config_path: Path, workspace_root: Path):
        """
        Initialize the propagator.

        Args:
            config_path: Path to propagation-rules.yaml
            workspace_root: Root directory of the workspace
        """
        self.workspace_root = workspace_root
        self.config = self._load_config(config_path)
        self.propagation_id = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.results: List[PropagationResult] = []

    def _load_config(self, config_path: Path) -> PropagationConfig:
        """Load and parse configuration file."""
        if not config_path.exists():
            raise FileNotFoundError(f"Configuration file not found: {config_path}")

        with open(config_path, 'r', encoding='utf-8') as f:
            config_data = yaml.safe_load(f)

        rules = config_data['propagation_rules']
        discovery = config_data['repository_discovery']
        safety = config_data['safety']
        reporting = config_data['reporting']

        return PropagationConfig(
            global_patterns=rules['global'],
            conditional_rules=rules['conditional'],
            template_merge=rules['template_merge'],
            repository_tags=discovery.get('repository_tags', {}),
            exclude_patterns=discovery.get('exclude', []),
            safety_config=safety,
            reporting_config=reporting,
            base_path=Path(discovery['base_path'])
        )

    def _get_active_repositories(self, filter_tags: Optional[List[str]] = None) -> List[Tuple[str, Path]]:
        """
        Discover active repositories in the workspace.

        Args:
            filter_tags: Optional list of tags to filter repositories

        Returns:
            List of (repo_name, repo_path) tuples
        """
        repositories = []

        for repo_name, tags in self.config.repository_tags.items():
            if 'active' not in tags:
                continue

            if filter_tags:
                if not any(tag in tags for tag in filter_tags):
                    continue

            repo_path = self.config.base_path / repo_name
            if repo_path.exists() and (repo_path / '.git').exists():
                repositories.append((repo_name, repo_path))

        return repositories

    def _matches_pattern(self, file_path: str, pattern: str) -> bool:
        """Check if file path matches a glob pattern."""
        # Handle wildcards in patterns
        pattern_path = Path(pattern)
        file_path_obj = Path(file_path)

        # Direct match
        if fnmatch.fnmatch(str(file_path_obj), pattern):
            return True

        # Match with wildcards in directory structure
        if '*' in pattern:
            if fnmatch.fnmatch(str(file_path_obj), pattern):
                return True
            # Try matching just the relevant parts
            if pattern.startswith('.claude/skills/'):
                skill_pattern = pattern.replace('.claude/skills/', '')
                if file_path.startswith('.claude/skills/'):
                    skill_name = file_path.replace('.claude/skills/', '')
                    if fnmatch.fnmatch(skill_name, skill_pattern):
                        return True

        return False

    def _should_propagate_to_repo(self, file_path: str, repo_tags: List[str]) -> Tuple[bool, str]:
        """
        Determine if file should propagate to a repository.

        Args:
            file_path: Relative path of the file
            repo_tags: Tags associated with the repository

        Returns:
            Tuple of (should_propagate, reason)
        """
        # Check global patterns
        for pattern in self.config.global_patterns:
            if self._matches_pattern(file_path, pattern):
                return True, f"Global pattern: {pattern}"

        # Check conditional rules
        for rule in self.config.conditional_rules:
            pattern = rule['path']
            condition = rule['condition']

            if self._matches_pattern(file_path, pattern):
                if condition in repo_tags:
                    return True, f"Conditional: {rule['description']}"
                else:
                    return False, f"Tag mismatch: requires '{condition}', has {repo_tags}"

        return False, "No matching propagation rule"

    def _is_excluded(self, file_path: str) -> bool:
        """Check if file matches exclusion patterns."""
        for pattern in self.config.exclude_patterns:
            if fnmatch.fnmatch(str(file_path), pattern):
                return True
        return False

    def _run_git_command(self, repo_path: Path, command: List[str]) -> Tuple[bool, str]:
        """
        Run a git command in a repository.

        Args:
            repo_path: Path to repository
            command: Git command arguments

        Returns:
            Tuple of (success, output/error)
        """
        try:
            result = subprocess.run(
                ['git'] + command,
                cwd=repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            return True, result.stdout.strip()
        except subprocess.CalledProcessError as e:
            return False, e.stderr.strip()

    def _create_backup_commit(self, repo_path: Path, repo_name: str) -> Optional[str]:
        """Create a backup commit before propagation."""
        # Get current commit hash
        success, commit_hash = self._run_git_command(repo_path, ['rev-parse', 'HEAD'])
        if not success:
            print(f"⚠️  Warning: Could not get commit hash for {repo_name}")
            return None

        return commit_hash

    def _create_propagation_branch(self, repo_path: Path, repo_name: str) -> Tuple[bool, Optional[str]]:
        """Create a new branch for propagation changes."""
        date_str = datetime.now().strftime("%Y%m%d")
        branch_format = self.config.safety_config['branch_format']
        branch_name = branch_format.format(
            prefix=self.config.safety_config['branch_prefix'],
            date=date_str
        )

        # Check if branch already exists
        success, _ = self._run_git_command(repo_path, ['rev-parse', '--verify', branch_name])
        if success:
            # Branch exists, append timestamp
            branch_name = f"{branch_name}_{datetime.now().strftime('%H%M%S')}"

        # Create and checkout branch
        success, output = self._run_git_command(repo_path, ['checkout', '-b', branch_name])
        if not success:
            print(f"❌ Failed to create branch {branch_name} in {repo_name}: {output}")
            return False, None

        print(f"  ✓ Created branch: {branch_name}")
        return True, branch_name

    def _check_file_conflicts(self, repo_path: Path, file_path: str) -> bool:
        """Check if file has local modifications."""
        success, output = self._run_git_command(repo_path, ['status', '--porcelain', file_path])
        if not success:
            return False

        # If output is not empty, file has modifications
        return len(output.strip()) > 0

    def _copy_file(self, source_path: Path, dest_path: Path) -> bool:
        """
        Copy file from source to destination.

        Args:
            source_path: Source file path
            dest_path: Destination file path

        Returns:
            True if successful
        """
        try:
            # Create parent directories if needed
            dest_path.parent.mkdir(parents=True, exist_ok=True)

            # Copy file
            shutil.copy2(source_path, dest_path)
            return True
        except Exception as e:
            print(f"❌ Error copying file: {e}")
            return False

    def _merge_template_file(self, source_path: Path, dest_path: Path, file_rel_path: str) -> bool:
        """
        Merge template file (like CLAUDE.md) preserving repo-specific sections.

        Args:
            source_path: Source file path
            dest_path: Destination file path
            file_rel_path: Relative file path for template matching

        Returns:
            True if successful
        """
        # Check if this file has template merge configuration
        template_config = self.config.template_merge.get(file_rel_path)
        if not template_config or not template_config.get('enabled'):
            # No template merge, just copy
            return self._copy_file(source_path, dest_path)

        # Read source and destination files
        try:
            with open(source_path, 'r', encoding='utf-8') as f:
                source_content = f.read()

            if dest_path.exists():
                with open(dest_path, 'r', encoding='utf-8') as f:
                    dest_content = f.read()
            else:
                dest_content = ""

            # Extract managed sections from source
            markers = template_config['markers']
            managed_start = markers['managed_start']
            managed_end = markers['managed_end']
            repo_start = markers['repo_specific_start']
            repo_end = markers['repo_specific_end']

            # Extract managed content from source
            managed_pattern = rf'{re.escape(managed_start)}(.*?){re.escape(managed_end)}'
            managed_match = re.search(managed_pattern, source_content, re.DOTALL)
            managed_content = managed_match.group(0) if managed_match else ""

            # Extract repo-specific content from destination
            repo_pattern = rf'{re.escape(repo_start)}(.*?){re.escape(repo_end)}'
            repo_match = re.search(repo_pattern, dest_content, re.DOTALL)
            repo_specific_content = repo_match.group(0) if repo_match else f"{repo_start}\n\n{repo_end}"

            # Build merged content
            if managed_content and repo_specific_content:
                merged_content = f"{managed_content}\n\n{repo_specific_content}\n"
            elif managed_content:
                merged_content = managed_content
            else:
                # No managed sections found, copy entire file
                merged_content = source_content

            # Write merged content
            dest_path.parent.mkdir(parents=True, exist_ok=True)
            with open(dest_path, 'w', encoding='utf-8') as f:
                f.write(merged_content)

            return True

        except Exception as e:
            print(f"❌ Error merging template file: {e}")
            return False

    def _commit_changes(self, repo_path: Path, file_list: List[str]) -> Optional[str]:
        """
        Commit propagated changes.

        Args:
            repo_path: Repository path
            file_list: List of files to commit

        Returns:
            Commit hash if successful, None otherwise
        """
        # Stage files
        for file_path in file_list:
            success, output = self._run_git_command(repo_path, ['add', file_path])
            if not success:
                print(f"⚠️  Warning: Could not stage {file_path}: {output}")

        # Create commit message
        commit_msg_template = self.config.safety_config['commit_message_template']
        commit_msg = commit_msg_template.format(
            file_list='\n'.join(f'  - {f}' for f in file_list),
            source_repo='digitalmodel',
            propagation_id=self.propagation_id,
            date=datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        )

        # Commit
        success, output = self._run_git_command(repo_path, ['commit', '-m', commit_msg])
        if not success:
            print(f"⚠️  Warning: Could not commit changes: {output}")
            return None

        # Get commit hash
        success, commit_hash = self._run_git_command(repo_path, ['rev-parse', 'HEAD'])
        return commit_hash if success else None

    def propagate_file(
        self,
        file_path: str,
        target_repos: Optional[List[str]] = None,
        dry_run: bool = True,
        interactive: bool = False,
        create_branch: bool = True
    ) -> List[PropagationResult]:
        """
        Propagate a file to target repositories.

        Args:
            file_path: Relative path of file to propagate (from workspace root)
            target_repos: Optional list of specific repositories to target
            dry_run: If True, show what would be done without making changes
            interactive: If True, ask for confirmation before each repository
            create_branch: If True, create a propagation branch

        Returns:
            List of PropagationResult objects
        """
        source_file = self.workspace_root / file_path

        if not source_file.exists():
            print(f"❌ Source file not found: {source_file}")
            return []

        if self._is_excluded(file_path):
            print(f"⚠️  File is excluded by patterns: {file_path}")
            return []

        print(f"\n{'='*80}")
        print(f"📋 Propagating: {file_path}")
        print(f"{'='*80}\n")

        # Get target repositories
        if target_repos:
            repositories = [
                (name, self.config.base_path / name)
                for name in target_repos
                if (self.config.base_path / name).exists()
            ]
        else:
            repositories = self._get_active_repositories()

        results = []

        for repo_name, repo_path in repositories:
            # Skip source repository
            if repo_name == 'digitalmodel':
                continue

            repo_tags = self.config.repository_tags.get(repo_name, [])

            # Check if file should propagate to this repo
            should_propagate, reason = self._should_propagate_to_repo(file_path, repo_tags)

            if not should_propagate:
                result = PropagationResult(
                    repo_name=repo_name,
                    success=False,
                    skipped_reason=reason
                )
                results.append(result)
                print(f"⏭️  Skipping {repo_name}: {reason}")
                continue

            print(f"\n📦 Processing {repo_name}")
            print(f"   Reason: {reason}")

            dest_file = repo_path / file_path

            # Check for conflicts
            if dest_file.exists() and self._check_file_conflicts(repo_path, file_path):
                conflict_msg = "File has local modifications"
                result = PropagationResult(
                    repo_name=repo_name,
                    success=False,
                    skipped_reason=conflict_msg,
                    conflicts=[file_path]
                )
                results.append(result)
                print(f"  ⚠️  Conflict: {conflict_msg}")

                if self.config.safety_config['on_conflict'] == 'skip':
                    continue

            # Interactive mode: show diff and ask for confirmation
            if interactive and not dry_run:
                if dest_file.exists():
                    print("\n  📄 Current file exists. Showing diff:")
                    # Show diff
                    success, diff = self._run_git_command(
                        repo_path,
                        ['diff', '--no-index', '--', str(dest_file), str(source_file)]
                    )
                    if diff:
                        print(f"\n{diff}\n")

                response = input(f"  Apply changes to {repo_name}? [y/N]: ").strip().lower()
                if response != 'y':
                    result = PropagationResult(
                        repo_name=repo_name,
                        success=False,
                        skipped_reason="User declined"
                    )
                    results.append(result)
                    print("  ⏭️  Skipped by user")
                    continue

            if dry_run:
                print(f"  🔍 DRY RUN: Would propagate to {dest_file}")
                result = PropagationResult(
                    repo_name=repo_name,
                    success=True,
                    files_changed=[file_path]
                )
                results.append(result)
                continue

            # Create backup commit
            backup_commit = None
            if self.config.safety_config['create_backup']:
                backup_commit = self._create_backup_commit(repo_path, repo_name)
                if backup_commit:
                    print(f"  ✓ Backup commit: {backup_commit[:8]}")

            # Create propagation branch
            branch_name = None
            if create_branch:
                success, branch_name = self._create_propagation_branch(repo_path, repo_name)
                if not success:
                    result = PropagationResult(
                        repo_name=repo_name,
                        success=False,
                        skipped_reason="Failed to create branch",
                        backup_commit=backup_commit
                    )
                    results.append(result)
                    continue

            # Copy or merge file
            if file_path in self.config.template_merge:
                success = self._merge_template_file(source_file, dest_file, file_path)
                operation = "Merged"
            else:
                success = self._copy_file(source_file, dest_file)
                operation = "Copied"

            if not success:
                result = PropagationResult(
                    repo_name=repo_name,
                    success=False,
                    skipped_reason=f"Failed to {operation.lower()} file",
                    backup_commit=backup_commit,
                    branch_name=branch_name
                )
                results.append(result)
                continue

            print(f"  ✓ {operation} file")

            # Commit changes
            commit_hash = self._commit_changes(repo_path, [file_path])

            result = PropagationResult(
                repo_name=repo_name,
                success=True,
                files_changed=[file_path],
                commit_hash=commit_hash,
                branch_name=branch_name,
                backup_commit=backup_commit
            )
            results.append(result)

            if commit_hash:
                print(f"  ✓ Committed: {commit_hash[:8]}")

            print(f"  ✅ Successfully propagated to {repo_name}")

        self.results.extend(results)
        return results

    def generate_report(self, output_path: Optional[Path] = None) -> str:
        """
        Generate propagation report.

        Args:
            output_path: Optional path to save report

        Returns:
            Report content as string
        """
        if output_path is None:
            report_dir = self.workspace_root / self.config.reporting_config['report_path']
            report_dir.mkdir(parents=True, exist_ok=True)
            output_path = report_dir / f"propagation_{self.propagation_id}.md"

        # Count statistics
        successful = [r for r in self.results if r.success]
        skipped = [r for r in self.results if not r.success]
        with_conflicts = [r for r in self.results if r.conflicts]

        # Generate report
        report_lines = [
            f"# Propagation Report",
            f"",
            f"**Propagation ID:** {self.propagation_id}",
            f"**Date:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
            f"",
            f"## Summary",
            f"",
            f"- **Total repositories processed:** {len(self.results)}",
            f"- **Successfully updated:** {len(successful)}",
            f"- **Skipped:** {len(skipped)}",
            f"- **Conflicts detected:** {len(with_conflicts)}",
            f"",
        ]

        # Successful updates
        if successful:
            report_lines.extend([
                f"## ✅ Successfully Updated ({len(successful)})",
                f"",
            ])

            for result in successful:
                report_lines.append(f"### {result.repo_name}")
                report_lines.append(f"")
                report_lines.append(f"- **Files changed:** {', '.join(result.files_changed)}")
                if result.branch_name:
                    report_lines.append(f"- **Branch:** `{result.branch_name}`")
                if result.commit_hash:
                    report_lines.append(f"- **Commit:** `{result.commit_hash[:8]}`")
                if result.backup_commit:
                    report_lines.append(f"- **Backup commit:** `{result.backup_commit[:8]}`")
                report_lines.append(f"")

        # Skipped repositories
        if skipped:
            report_lines.extend([
                f"## ⏭️  Skipped Repositories ({len(skipped)})",
                f"",
            ])

            for result in skipped:
                report_lines.append(f"### {result.repo_name}")
                report_lines.append(f"")
                report_lines.append(f"- **Reason:** {result.skipped_reason}")
                report_lines.append(f"")

        # Conflicts
        if with_conflicts:
            report_lines.extend([
                f"## ⚠️  Conflicts Detected ({len(with_conflicts)})",
                f"",
            ])

            for result in with_conflicts:
                report_lines.append(f"### {result.repo_name}")
                report_lines.append(f"")
                report_lines.append(f"- **Conflicting files:** {', '.join(result.conflicts)}")
                report_lines.append(f"- **Reason:** {result.skipped_reason}")
                report_lines.append(f"")

        report_content = '\n'.join(report_lines)

        # Save report
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(report_content)

        print(f"\n📊 Report saved to: {output_path}")

        return report_content


def main():
    """Main entry point for CLI."""
    parser = argparse.ArgumentParser(
        description="Propagate changes across workspace repositories",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Dry run (default)
  python scripts/cross_repo_change_propagator.py --file .claude/settings.json --dry-run

  # Interactive mode
  python scripts/cross_repo_change_propagator.py --file .claude/skills/new-skill/SKILL.md --interactive

  # Auto-propagate with branch creation
  python scripts/cross_repo_change_propagator.py --file .agent-os/standards/FILE_ORGANIZATION_STANDARDS.md --auto --create-branch

  # Target specific repositories
  python scripts/cross_repo_change_propagator.py --file .claude/settings.json --repos repo1 repo2 --auto
        """
    )

    parser.add_argument(
        '--file',
        required=True,
        help='Relative path of file to propagate (from workspace root)'
    )

    parser.add_argument(
        '--repos',
        nargs='+',
        help='Specific repositories to target (default: all active repos)'
    )

    parser.add_argument(
        '--dry-run',
        action='store_true',
        default=False,
        help='Show what would be done without making changes (default: True if --auto not specified)'
    )

    parser.add_argument(
        '--interactive',
        action='store_true',
        help='Ask for confirmation before each repository'
    )

    parser.add_argument(
        '--auto',
        action='store_true',
        help='Automatically propagate without prompts (enables actual changes)'
    )

    parser.add_argument(
        '--create-branch',
        action='store_true',
        default=True,
        help='Create propagation branch for changes (default: True)'
    )

    parser.add_argument(
        '--no-branch',
        action='store_true',
        help='Do not create propagation branch'
    )

    parser.add_argument(
        '--config',
        type=Path,
        help='Path to propagation-rules.yaml (default: .claude/propagation-rules.yaml)'
    )

    parser.add_argument(
        '--workspace',
        type=Path,
        help='Workspace root directory (default: auto-detect)'
    )

    args = parser.parse_args()

    # Determine workspace root
    if args.workspace:
        workspace_root = args.workspace
    else:
        # Auto-detect: find directory containing .claude/propagation-rules.yaml
        current = Path.cwd()
        while current != current.parent:
            if (current / '.claude' / 'propagation-rules.yaml').exists():
                workspace_root = current
                break
            current = current.parent
        else:
            print("❌ Could not find workspace root. Use --workspace flag.")
            sys.exit(1)

    # Determine config path
    if args.config:
        config_path = args.config
    else:
        config_path = workspace_root / '.claude' / 'propagation-rules.yaml'

    # Determine dry-run mode
    dry_run = args.dry_run or not args.auto

    # Determine branch creation
    create_branch = args.create_branch and not args.no_branch

    try:
        # Initialize propagator
        propagator = CrossRepoChangePropagator(config_path, workspace_root)

        # Show mode
        if dry_run:
            print("\n🔍 DRY RUN MODE - No changes will be made\n")
        elif args.interactive:
            print("\n🤝 INTERACTIVE MODE - Confirmation required per repository\n")
        else:
            print("\n🚀 AUTO MODE - Changes will be applied automatically\n")

        # Propagate file
        results = propagator.propagate_file(
            file_path=args.file,
            target_repos=args.repos,
            dry_run=dry_run,
            interactive=args.interactive,
            create_branch=create_branch
        )

        # Generate report
        print("\n" + "="*80)
        report = propagator.generate_report()
        print("="*80)

        # Print summary
        successful = sum(1 for r in results if r.success)
        skipped = sum(1 for r in results if not r.success)

        print(f"\n✨ Propagation complete!")
        print(f"   Successfully updated: {successful}")
        print(f"   Skipped: {skipped}")

        if dry_run:
            print(f"\n💡 This was a dry run. Use --auto to apply changes.")
        elif args.auto and create_branch:
            print(f"\n💡 Changes committed to propagation branches.")
            print(f"   Review and merge branches in each repository.")

    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == '__main__':
    main()
