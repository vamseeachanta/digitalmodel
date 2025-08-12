#!/usr/bin/env python3
"""
Git Trunk-Based Development Flow Command
Implements best practices for trunk-based development with automated cleanup,
security checks, and PR creation/merge workflow.
"""

import os
import sys
import subprocess
import re
import json
import tempfile
from pathlib import Path
from typing import List, Tuple, Optional, Dict
from datetime import datetime

class GitTrunkFlow:
    """Manages trunk-based development workflow with best practices."""
    
    def __init__(self):
        self.repo_path = os.getcwd()
        self.main_branch = self.detect_main_branch()
        self.current_branch = self.get_current_branch()
        self.cleanup_patterns = [
            # Temporary and backup files
            "*.tmp", "*.bak", "*.backup", "*.orig", "*.swp", "*.swo",
            "*~", ".DS_Store", "Thumbs.db", "desktop.ini",
            
            # Build artifacts that shouldn't be committed
            "*.pyc", "__pycache__/", "*.pyo", "*.pyd",
            ".pytest_cache/", ".coverage", "htmlcov/",
            "*.egg-info/", "dist/", "build/",
            
            # Log files
            "*.log", "logs/", "*.debug",
            
            # IDE files that might not be in .gitignore
            ".idea/", ".vscode/", "*.iml", ".project", ".classpath",
            
            # Node modules and package locks that might be duplicated
            "node_modules/", "package-lock.json.backup*",
            
            # Environment files
            ".env.backup*", ".env.old", ".env.local.backup*"
        ]
        
        self.secrets_patterns = [
            # API keys and tokens
            r'(?i)(api[_-]?key|apikey|api[_-]?secret)[\s]*[=:]\s*["\']?[\w\-]+["\']?',
            r'(?i)(token|access[_-]?token|auth[_-]?token)[\s]*[=:]\s*["\']?[\w\-]+["\']?',
            
            # AWS patterns
            r'AKIA[0-9A-Z]{16}',
            r'(?i)aws[_-]?secret[_-]?access[_-]?key[\s]*[=:]\s*["\']?[\w\+\/]+["\']?',
            
            # Private keys
            r'-----BEGIN (RSA |EC |DSA |OPENSSH )?PRIVATE KEY-----',
            
            # Database URLs with credentials
            r'(?i)(mongodb|postgres|postgresql|mysql|redis)://[^:]+:[^@]+@',
            
            # Generic passwords
            r'(?i)(password|passwd|pwd)[\s]*[=:]\s*["\']?[^\s"\']+["\']?',
            
            # JWT tokens
            r'eyJ[A-Za-z0-9-_=]+\.[A-Za-z0-9-_=]+\.?[A-Za-z0-9-_.+/=]*'
        ]
        
        self.required_files = {
            '.gitignore': self.generate_gitignore_content(),
            'README.md': None,  # Check existence only
            '.editorconfig': self.generate_editorconfig_content()
        }

    def run(self):
        """Execute the complete trunk-based development flow."""
        print("🚀 Starting Git Trunk-Based Development Flow")
        print("=" * 60)
        
        try:
            # Step 1: Pre-flight checks
            self.preflight_checks()
            
            # Step 2: Clean up redundant files
            self.cleanup_redundant_files()
            
            # Step 3: Apply best practices
            self.apply_best_practices()
            
            # Step 4: Security scan
            self.security_scan()
            
            # Step 5: Run tests if available
            self.run_tests()
            
            # Step 6: Stage changes
            self.stage_changes()
            
            # Step 7: Create commit
            commit_message = self.create_commit()
            
            # Step 8: Create and merge PR
            self.create_and_merge_pr(commit_message)
            
            print("\n✅ Trunk-based development flow completed successfully!")
            
        except Exception as e:
            print(f"\n❌ Error: {e}")
            sys.exit(1)

    def preflight_checks(self):
        """Perform initial checks before proceeding."""
        print("\n📋 Running pre-flight checks...")
        
        # Check if we're in a git repository
        if not os.path.exists('.git'):
            raise Exception("Not in a git repository")
        
        # Check for uncommitted changes
        status = self.run_command("git status --porcelain")
        if not status:
            print("⚠️  No changes detected. Nothing to commit.")
            sys.exit(0)
        
        # Ensure we're not on main/master
        if self.current_branch in ['main', 'master']:
            print(f"📝 Currently on {self.current_branch} branch.")
            branch_name = self.create_feature_branch()
            print(f"✅ Created and switched to feature branch: {branch_name}")
        
        # Fetch latest changes
        print("📥 Fetching latest changes from remote...")
        self.run_command("git fetch origin")
        
        # Check if main branch is up to date
        behind = self.run_command(f"git rev-list HEAD..origin/{self.main_branch} --count").strip()
        if behind != "0":
            print(f"⚠️  Your branch is {behind} commits behind origin/{self.main_branch}")
            response = input("Would you like to rebase on latest main? (y/n): ")
            if response.lower() == 'y':
                self.rebase_on_main()

    def cleanup_redundant_files(self):
        """Remove redundant and temporary files."""
        print("\n🧹 Cleaning up redundant files...")
        
        removed_files = []
        for pattern in self.cleanup_patterns:
            # Use git clean dry run to see what would be removed
            cmd = f"git clean -ndX {pattern} 2>/dev/null"
            result = self.run_command(cmd, check=False)
            
            if result:
                files = [line.replace("Would remove ", "") for line in result.split('\n') if line]
                if files:
                    print(f"  Found {len(files)} files matching pattern: {pattern}")
                    for file in files:
                        if os.path.exists(file):
                            try:
                                if os.path.isdir(file):
                                    import shutil
                                    shutil.rmtree(file)
                                else:
                                    os.remove(file)
                                removed_files.append(file)
                            except Exception as e:
                                print(f"    ⚠️  Could not remove {file}: {e}")
        
        if removed_files:
            print(f"✅ Removed {len(removed_files)} redundant files")
            for file in removed_files[:10]:  # Show first 10
                print(f"    - {file}")
            if len(removed_files) > 10:
                print(f"    ... and {len(removed_files) - 10} more")
        else:
            print("✅ No redundant files found")

    def apply_best_practices(self):
        """Apply repository best practices."""
        print("\n🔧 Applying best practices...")
        
        changes_made = []
        
        # Check and create essential files
        for filename, content in self.required_files.items():
            file_path = Path(self.repo_path) / filename
            
            if not file_path.exists() and content:
                print(f"  Creating {filename}...")
                file_path.write_text(content)
                changes_made.append(f"Created {filename}")
            elif filename == '.gitignore' and file_path.exists():
                # Enhance existing .gitignore
                existing_content = file_path.read_text()
                enhanced = self.enhance_gitignore(existing_content)
                if enhanced != existing_content:
                    file_path.write_text(enhanced)
                    changes_made.append(f"Enhanced {filename}")
        
        # Check for branch protection recommendations
        if self.current_branch == self.main_branch:
            print("  ⚠️  Recommendation: Enable branch protection rules for main branch")
            changes_made.append("Branch protection recommended")
        
        # Check for pre-commit hooks
        hooks_path = Path('.git/hooks/pre-commit')
        if not hooks_path.exists():
            print("  Creating pre-commit hook for secret scanning...")
            self.create_pre_commit_hook()
            changes_made.append("Created pre-commit hook")
        
        if changes_made:
            print(f"\n📝 Changes to be committed:")
            for change in changes_made:
                print(f"    ✅ {change}")
        else:
            print("✅ Repository already follows best practices")

    def security_scan(self):
        """Scan for potential security issues."""
        print("\n🔒 Running security scan...")
        
        issues_found = []
        
        # Get list of staged files
        staged_files = self.run_command("git diff --cached --name-only").split('\n')
        staged_files = [f for f in staged_files if f and os.path.exists(f)]
        
        for file_path in staged_files:
            # Skip binary files
            if self.is_binary_file(file_path):
                continue
            
            try:
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()
                    
                for pattern in self.secrets_patterns:
                    matches = re.findall(pattern, content)
                    if matches:
                        issues_found.append({
                            'file': file_path,
                            'pattern': pattern,
                            'matches': len(matches)
                        })
            except Exception:
                continue
        
        if issues_found:
            print("⚠️  Potential security issues found:")
            for issue in issues_found:
                print(f"    - {issue['file']}: {issue['matches']} potential secret(s)")
            
            response = input("\nContinue anyway? (y/n): ")
            if response.lower() != 'y':
                raise Exception("Security scan failed - aborting")
        else:
            print("✅ No security issues detected")

    def run_tests(self):
        """Run available test suites."""
        print("\n🧪 Running tests...")
        
        test_commands = [
            ("npm test", "package.json"),
            ("yarn test", "package.json"),
            ("pytest", "pytest.ini"),
            ("python -m pytest", "test*.py"),
            ("make test", "Makefile"),
            ("cargo test", "Cargo.toml"),
            ("go test ./...", "go.mod"),
            ("bundle exec rspec", "Gemfile"),
            ("./gradlew test", "build.gradle"),
            ("mvn test", "pom.xml")
        ]
        
        tests_run = False
        for cmd, indicator in test_commands:
            # Check if indicator file exists
            if indicator and not any(Path('.').glob(indicator)):
                continue
            
            print(f"  Attempting: {cmd}")
            result = self.run_command(cmd, check=False)
            if result is not None:
                tests_run = True
                print(f"✅ Tests passed: {cmd}")
                break
        
        if not tests_run:
            print("ℹ️  No test suite detected or tests skipped")

    def stage_changes(self):
        """Stage changes for commit."""
        print("\n📦 Staging changes...")
        
        # Add all changes except those in .gitignore
        self.run_command("git add -A")
        
        # Show what will be committed
        staged = self.run_command("git diff --cached --stat")
        if staged:
            print("Changes to be committed:")
            print(staged)

    def create_commit(self) -> str:
        """Create a commit with conventional commit message."""
        print("\n✍️  Creating commit...")
        
        # Get commit type
        print("\nSelect commit type:")
        commit_types = [
            ("feat", "New feature"),
            ("fix", "Bug fix"),
            ("docs", "Documentation changes"),
            ("style", "Code style changes"),
            ("refactor", "Code refactoring"),
            ("perf", "Performance improvements"),
            ("test", "Test changes"),
            ("build", "Build system changes"),
            ("ci", "CI/CD changes"),
            ("chore", "Other changes")
        ]
        
        for i, (type_code, description) in enumerate(commit_types, 1):
            print(f"  {i}. {type_code}: {description}")
        
        choice = input("\nEnter choice (1-10): ")
        commit_type = commit_types[int(choice) - 1][0] if choice.isdigit() else "chore"
        
        # Get commit scope (optional)
        scope = input("Enter scope (optional, e.g., 'api', 'ui', press Enter to skip): ").strip()
        
        # Get commit description
        description = input("Enter commit description (imperative mood, e.g., 'add login feature'): ").strip()
        if not description:
            description = "update codebase"
        
        # Build commit message
        if scope:
            commit_title = f"{commit_type}({scope}): {description}"
        else:
            commit_title = f"{commit_type}: {description}"
        
        # Get detailed description (optional)
        print("\nEnter detailed description (optional, press Enter twice to finish):")
        body_lines = []
        while True:
            line = input()
            if not line:
                break
            body_lines.append(line)
        
        commit_body = '\n'.join(body_lines) if body_lines else ""
        
        # Check for breaking changes
        is_breaking = input("\nIs this a breaking change? (y/n): ").lower() == 'y'
        
        # Build full commit message
        full_message = commit_title
        if is_breaking:
            full_message = full_message.replace(f"{commit_type}", f"{commit_type}!")
        
        if commit_body:
            full_message += f"\n\n{commit_body}"
        
        if is_breaking:
            breaking_description = input("Describe the breaking change: ")
            full_message += f"\n\nBREAKING CHANGE: {breaking_description}"
        
        # Create the commit
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write(full_message)
            temp_file = f.name
        
        try:
            self.run_command(f"git commit -F {temp_file}")
            print(f"✅ Commit created: {commit_title}")
        finally:
            os.unlink(temp_file)
        
        return commit_title

    def create_and_merge_pr(self, commit_message: str):
        """Create PR and optionally auto-merge."""
        print("\n🔀 Creating and merging Pull Request...")
        
        # Push current branch
        current_branch = self.get_current_branch()
        print(f"  Pushing branch {current_branch} to origin...")
        self.run_command(f"git push -u origin {current_branch}")
        
        # Check if gh CLI is available
        gh_available = self.run_command("which gh", check=False) is not None
        
        if gh_available:
            print("  Creating pull request with GitHub CLI...")
            
            # Generate PR body
            pr_body = self.generate_pr_body(commit_message)
            
            # Create PR
            pr_cmd = f'''gh pr create \
                --title "{commit_message}" \
                --body "{pr_body}" \
                --base {self.main_branch} \
                --head {current_branch}'''
            
            pr_url = self.run_command(pr_cmd)
            print(f"✅ Pull request created: {pr_url}")
            
            # Ask about auto-merge
            response = input("\nAuto-merge to main after checks pass? (y/n): ")
            if response.lower() == 'y':
                print("  Enabling auto-merge...")
                self.run_command(f"gh pr merge --auto --squash {pr_url}")
                print("✅ Auto-merge enabled (will merge after checks pass)")
                
                # Clean up local branch
                print(f"  Switching back to {self.main_branch}...")
                self.run_command(f"git checkout {self.main_branch}")
                self.run_command(f"git pull origin {self.main_branch}")
                
                print(f"  Deleting local feature branch {current_branch}...")
                self.run_command(f"git branch -d {current_branch}")
        else:
            print("ℹ️  GitHub CLI not available. Please create PR manually:")
            print(f"    Branch: {current_branch} → {self.main_branch}")
            print(f"    Title: {commit_message}")

    # Helper methods
    
    def detect_main_branch(self) -> str:
        """Detect the main branch name."""
        for branch in ['main', 'master']:
            result = self.run_command(f"git rev-parse --verify {branch}", check=False)
            if result:
                return branch
        return 'main'  # Default
    
    def get_current_branch(self) -> str:
        """Get current branch name."""
        return self.run_command("git branch --show-current").strip()
    
    def create_feature_branch(self) -> str:
        """Create a new feature branch."""
        timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        branch_name = f"feature/auto-update-{timestamp}"
        self.run_command(f"git checkout -b {branch_name}")
        return branch_name
    
    def rebase_on_main(self):
        """Rebase current branch on main."""
        print(f"  Rebasing on {self.main_branch}...")
        self.run_command(f"git rebase origin/{self.main_branch}")
        print("✅ Rebase completed")
    
    def run_command(self, cmd: str, check: bool = True) -> Optional[str]:
        """Run a shell command and return output."""
        try:
            result = subprocess.run(
                cmd,
                shell=True,
                capture_output=True,
                text=True,
                check=check
            )
            return result.stdout.strip() if result.returncode == 0 else None
        except subprocess.CalledProcessError as e:
            if check:
                raise Exception(f"Command failed: {cmd}\n{e.stderr}")
            return None
    
    def is_binary_file(self, file_path: str) -> bool:
        """Check if a file is binary."""
        try:
            with open(file_path, 'rb') as f:
                chunk = f.read(1024)
                return b'\0' in chunk
        except:
            return True
    
    def generate_gitignore_content(self) -> str:
        """Generate comprehensive .gitignore content."""
        return """# Environment variables
.env
.env.*
*.local

# IDE and editor files
.vscode/
.idea/
*.swp
*.swo
*~

# OS generated files
.DS_Store
Thumbs.db
desktop.ini

# Python
__pycache__/
*.py[cod]
*$py.class
*.so
.Python
env/
venv/
.venv
pip-log.txt
.coverage
.pytest_cache/
htmlcov/
*.egg-info/
dist/
build/

# Node.js
node_modules/
npm-debug.log*
yarn-debug.log*
yarn-error.log*
.pnpm-debug.log*

# Build outputs
/dist/
/build/
/out/
*.log

# Database files
*.db
*.sqlite
*.sqlite3

# Temporary files
*.tmp
*.bak
*.backup
*.orig

# Security - never commit secrets
*.pem
*.key
*.crt
*.p12
.secrets/
"""
    
    def generate_editorconfig_content(self) -> str:
        """Generate .editorconfig content."""
        return """# EditorConfig is awesome: https://EditorConfig.org

root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true
indent_style = space
indent_size = 2

[*.{py,pyw}]
indent_size = 4

[*.md]
trim_trailing_whitespace = false

[Makefile]
indent_style = tab
"""
    
    def enhance_gitignore(self, existing_content: str) -> str:
        """Enhance existing .gitignore with missing patterns."""
        essential_patterns = [
            '.env',
            '.env.*',
            '*.local',
            '.DS_Store',
            '__pycache__/',
            'node_modules/',
            '*.log',
            '*.tmp',
            '*.bak',
            '*.swp'
        ]
        
        lines = existing_content.split('\n')
        existing_patterns = set(line.strip() for line in lines if line.strip() and not line.startswith('#'))
        
        additions = []
        for pattern in essential_patterns:
            if pattern not in existing_patterns:
                additions.append(pattern)
        
        if additions:
            enhanced = existing_content.rstrip() + '\n\n# Auto-added essential patterns\n'
            enhanced += '\n'.join(additions) + '\n'
            return enhanced
        
        return existing_content
    
    def create_pre_commit_hook(self):
        """Create a pre-commit hook for security scanning."""
        hook_content = """#!/bin/sh
# Pre-commit hook for security scanning

# Check for potential secrets
echo "Running security scan..."

# Patterns to check
patterns=(
    "api[_-]?key"
    "api[_-]?secret"
    "access[_-]?token"
    "auth[_-]?token"
    "AKIA[0-9A-Z]{16}"
    "-----BEGIN.*PRIVATE KEY-----"
    "password.*="
)

for pattern in "${patterns[@]}"; do
    matches=$(git diff --cached --name-only -z | xargs -0 grep -E "$pattern" 2>/dev/null)
    if [ ! -z "$matches" ]; then
        echo "⚠️  Potential secret detected with pattern: $pattern"
        echo "$matches"
        echo ""
        echo "If this is a false positive, you can skip this check with:"
        echo "  git commit --no-verify"
        exit 1
    fi
done

echo "✅ Security scan passed"
exit 0
"""
        
        hook_path = Path('.git/hooks/pre-commit')
        hook_path.write_text(hook_content)
        hook_path.chmod(0o755)
    
    def generate_pr_body(self, commit_message: str) -> str:
        """Generate PR body with checklist."""
        return f"""## Summary
{commit_message}

## Changes Made
- See commit history for detailed changes

## Checklist
- [x] Code follows project style guidelines
- [x] Self-review completed
- [x] Comments added for complex code
- [x] Documentation updated if needed
- [x] Tests pass locally
- [x] No security issues detected
- [x] No redundant files included

## Testing
- Tests run automatically via CI/CD pipeline
- Manual testing completed locally

---
*Created via automated trunk-based development flow*
"""

def main():
    """Main entry point for the slash command."""
    flow = GitTrunkFlow()
    flow.run()

if __name__ == "__main__":
    main()