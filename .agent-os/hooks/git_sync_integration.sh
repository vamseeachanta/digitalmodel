#!/bin/bash
# Git Sync Integration for Verification Hooks
# This ensures hooks are always synced across all repositories

set -euo pipefail

# Source verification hooks
HOOKS_DIR="$(dirname "$0")"
source "$HOOKS_DIR/verification_hooks.sh"

# ============================================================================
# GIT SYNC WITH VERIFICATION
# ============================================================================

git_sync_with_verification() {
    local repo_path="${1:-.}"
    local branch="${2:-master}"
    
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    echo -e "${YELLOW}GIT SYNC WITH VERIFICATION${NC}"
    echo -e "${YELLOW}Repository: $repo_path${NC}"
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    
    # Pre-sync verification
    echo -e "${BLUE}[PHASE 1]${NC} Pre-sync verification"
    verify_dir_exists "$repo_path/.git" "Git repository"
    
    cd "$repo_path"
    
    # Check current status
    echo -e "${BLUE}[PHASE 2]${NC} Checking repository status"
    git status --short
    
    # Fetch updates
    echo -e "${BLUE}[PHASE 3]${NC} Fetching from remote"
    verify_return_code "git fetch --all --prune" "Fetch all remotes"
    
    # Pull changes
    echo -e "${BLUE}[PHASE 4]${NC} Pulling changes"
    verify_return_code "git pull origin $branch" "Pull from origin/$branch"
    
    # Verify hooks are present
    echo -e "${BLUE}[PHASE 5]${NC} Verifying hooks"
    verify_file_exists ".agent-os/hooks/verification_hooks.sh" "Bash hooks"
    verify_file_exists ".agent-os/hooks/verification_hooks.py" "Python hooks"
    
    # Check for uncommitted changes
    echo -e "${BLUE}[PHASE 6]${NC} Post-sync status"
    if [[ -n $(git status --porcelain) ]]; then
        echo -e "${YELLOW}[⚠ WARN]${NC} Uncommitted changes present"
        git status --short
    else
        echo -e "${GREEN}[✓ CLEAN]${NC} Working tree clean"
    fi
    
    echo -e "${GREEN}[✓ COMPLETE]${NC} Git sync with verification complete"
}

# ============================================================================
# SYNC HOOKS TO OTHER REPOSITORIES
# ============================================================================

sync_hooks_to_repo() {
    local target_repo="$1"
    local source_hooks_dir="${2:-.agent-os/hooks}"
    
    echo -e "${BLUE}[SYNC]${NC} Syncing hooks to: $target_repo"
    
    if [[ ! -d "$target_repo" ]]; then
        echo -e "${RED}[✗ SKIP]${NC} Repository not found: $target_repo"
        return 1
    fi
    
    # Create hooks directory if it doesn't exist
    local target_hooks_dir="$target_repo/.agent-os/hooks"
    if [[ ! -d "$target_hooks_dir" ]]; then
        echo -e "${YELLOW}[CREATE]${NC} Creating hooks directory"
        mkdir -p "$target_hooks_dir"
    fi
    
    # Copy hook files
    echo -e "${BLUE}[COPY]${NC} Copying hook files"
    cp -f "$source_hooks_dir/verification_hooks.sh" "$target_hooks_dir/"
    cp -f "$source_hooks_dir/verification_hooks.py" "$target_hooks_dir/"
    cp -f "$source_hooks_dir/README.md" "$target_hooks_dir/"
    
    # Verify copy
    verify_file_exists "$target_hooks_dir/verification_hooks.sh" "Bash hooks copied"
    verify_file_exists "$target_hooks_dir/verification_hooks.py" "Python hooks copied"
    
    # Add to git if needed
    cd "$target_repo"
    if [[ -n $(git status --porcelain .agent-os/hooks/) ]]; then
        echo -e "${YELLOW}[GIT]${NC} Adding hooks to git"
        git add .agent-os/hooks/
        echo -e "${GREEN}[✓ ADDED]${NC} Hooks added to git"
    else
        echo -e "${GREEN}[✓ CURRENT]${NC} Hooks already up to date"
    fi
    
    return 0
}

# ============================================================================
# SYNC ALL REPOSITORIES WITH HOOKS
# ============================================================================

sync_all_repos_with_hooks() {
    local base_dir="${1:-/d/github}"
    local source_repo="${2:-digitalmodel}"
    
    echo -e "${YELLOW}╔═══════════════════════════════════════════════════════╗${NC}"
    echo -e "${YELLOW}║     SYNCING ALL REPOSITORIES WITH HOOKS              ║${NC}"
    echo -e "${YELLOW}╚═══════════════════════════════════════════════════════╝${NC}"
    
    # Get list of repositories
    local repos=($(ls -d "$base_dir"/*/.git 2>/dev/null | xargs -n1 dirname | xargs -n1 basename))
    
    echo -e "${BLUE}[INFO]${NC} Found ${#repos[@]} repositories"
    
    local success_count=0
    local fail_count=0
    
    # Source hooks directory
    local source_hooks="$base_dir/$source_repo/.agent-os/hooks"
    
    # Verify source hooks exist
    if [[ ! -d "$source_hooks" ]]; then
        echo -e "${RED}[✗ ERROR]${NC} Source hooks not found: $source_hooks"
        return 1
    fi
    
    # Sync each repository
    for repo in "${repos[@]}"; do
        if [[ "$repo" == "$source_repo" ]]; then
            echo -e "${BLUE}[SKIP]${NC} Skipping source repository: $repo"
            continue
        fi
        
        echo ""
        echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
        echo -e "${BLUE}Repository: $repo${NC}"
        echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
        
        # First sync the repository
        if git_sync_with_verification "$base_dir/$repo"; then
            # Then sync the hooks
            if sync_hooks_to_repo "$base_dir/$repo" "$source_hooks"; then
                ((success_count++))
                echo -e "${GREEN}[✓ SUCCESS]${NC} $repo synced with hooks"
            else
                ((fail_count++))
                echo -e "${RED}[✗ FAIL]${NC} $repo hook sync failed"
            fi
        else
            ((fail_count++))
            echo -e "${RED}[✗ FAIL]${NC} $repo git sync failed"
        fi
    done
    
    # Summary
    echo ""
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    echo -e "${YELLOW}SYNC SUMMARY${NC}"
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}Successful:${NC} $success_count"
    echo -e "${RED}Failed:${NC} $fail_count"
    echo -e "${BLUE}Total:${NC} ${#repos[@]}"
    
    if [[ $fail_count -eq 0 ]]; then
        echo -e "${GREEN}╔═══════════════════════════════════╗${NC}"
        echo -e "${GREEN}║     ALL REPOS SYNCED ✓            ║${NC}"
        echo -e "${GREEN}╚═══════════════════════════════════╝${NC}"
        return 0
    else
        echo -e "${RED}╔═══════════════════════════════════╗${NC}"
        echo -e "${RED}║     SOME REPOS FAILED ✗           ║${NC}"
        echo -e "${RED}╚═══════════════════════════════════╝${NC}"
        return 1
    fi
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # Script is being executed directly
    
    case "${1:-}" in
        "sync")
            # Sync current repository
            git_sync_with_verification "${2:-.}"
            ;;
        "sync-all")
            # Sync all repositories with hooks
            sync_all_repos_with_hooks "${2:-/d/github}" "${3:-digitalmodel}"
            ;;
        "copy-hooks")
            # Copy hooks to specific repository
            sync_hooks_to_repo "${2}" "${3:-.agent-os/hooks}"
            ;;
        *)
            echo "Usage: $0 {sync|sync-all|copy-hooks} [options]"
            echo ""
            echo "Commands:"
            echo "  sync [repo-path]           - Sync single repository with verification"
            echo "  sync-all [base-dir]        - Sync all repositories and distribute hooks"
            echo "  copy-hooks <target-repo>   - Copy hooks to target repository"
            echo ""
            echo "Examples:"
            echo "  $0 sync                    # Sync current directory"
            echo "  $0 sync /d/github/myrepo   # Sync specific repo"
            echo "  $0 sync-all                # Sync all repos in /d/github"
            echo "  $0 copy-hooks ../otherrepo # Copy hooks to another repo"
            exit 1
            ;;
    esac
fi

# Export functions for use in other scripts
export -f git_sync_with_verification
export -f sync_hooks_to_repo
export -f sync_all_repos_with_hooks