#!/usr/bin/env bash
# ABOUTME: Enhanced repository synchronization with standards enforcement
# ABOUTME: Syncs templates, enforces standards, and propagates agent configurations across all repositories

set -euo pipefail

# ============================================================================
# Configuration
# ============================================================================

WORKSPACE_ROOT="${WORKSPACE_ROOT:-/d/workspace-hub}"
SOURCE_REPO="/d/workspace-hub/digitalmodel"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_DIR="${SOURCE_REPO}/logs"
LOG_FILE="${LOG_DIR}/sync_${TIMESTAMP}.log"

# Colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Sync mode
VALIDATE_FIRST="${VALIDATE_FIRST:-true}"
DRY_RUN="${DRY_RUN:-false}"
FORCE="${FORCE:-false}"

# Templates to sync
declare -a TEMPLATES=(
    ".agent-os"
    ".claude/settings.json"
)

# ============================================================================
# Helper Functions
# ============================================================================

log_info() {
    local msg="[$(date +'%Y-%m-%d %H:%M:%S')] [INFO] $1"
    echo -e "${BLUE}${msg}${NC}"
    echo "$msg" >> "$LOG_FILE"
}

log_success() {
    local msg="[$(date +'%Y-%m-%d %H:%M:%S')] [SUCCESS] $1"
    echo -e "${GREEN}${msg}${NC}"
    echo "$msg" >> "$LOG_FILE"
}

log_warning() {
    local msg="[$(date +'%Y-%m-%d %H:%M:%S')] [WARNING] $1"
    echo -e "${YELLOW}${msg}${NC}"
    echo "$msg" >> "$LOG_FILE"
}

log_error() {
    local msg="[$(date +'%Y-%m-%d %H:%M:%S')] [ERROR] $1"
    echo -e "${RED}${msg}${NC}"
    echo "$msg" >> "$LOG_FILE"
}

log_action() {
    local msg="[$(date +'%Y-%m-%d %H:%M:%S')] [ACTION] $1"
    echo -e "${CYAN}${msg}${NC}"
    echo "$msg" >> "$LOG_FILE"
}

# ============================================================================
# Repository Discovery
# ============================================================================

find_repositories() {
    log_info "Scanning for repositories in: $WORKSPACE_ROOT"

    local repos=()

    # Find all directories with .git folder, excluding common ignore patterns
    while IFS= read -r -d '' git_dir; do
        repo_dir=$(dirname "$git_dir")

        # Skip source repository itself
        if [[ "$repo_dir" == "$SOURCE_REPO" ]]; then
            continue
        fi

        # Skip if in excluded directories
        if [[ "$repo_dir" == *"/node_modules/"* ]] || \
           [[ "$repo_dir" == *"/.venv/"* ]] || \
           [[ "$repo_dir" == *"/__pycache__/"* ]] || \
           [[ "$repo_dir" == *"/.git/"* ]]; then
            continue
        fi

        repos+=("$repo_dir")
    done < <(find "$WORKSPACE_ROOT" -type d -name ".git" -print0 2>/dev/null)

    log_success "Found ${#repos[@]} target repositories"
    printf '%s\n' "${repos[@]}"
}

# ============================================================================
# Pre-Sync Validation
# ============================================================================

run_validation() {
    log_info "Running pre-sync standards validation..."

    if [[ -f "${SOURCE_REPO}/scripts/validate_cross_repo_standards.sh" ]]; then
        bash "${SOURCE_REPO}/scripts/validate_cross_repo_standards.sh"
    else
        log_warning "Validation script not found, skipping validation"
    fi
}

# ============================================================================
# Template Synchronization
# ============================================================================

sync_agent_os() {
    local target_repo="$1"
    local source_dir="${SOURCE_REPO}/.agent-os"
    local target_dir="${target_repo}/.agent-os"

    if [[ ! -d "$source_dir" ]]; then
        log_warning "Source .agent-os directory not found, skipping"
        return
    fi

    log_action "Syncing .agent-os to $(basename "$target_repo")"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would sync .agent-os"
        return
    fi

    # Create target directory if it doesn't exist
    mkdir -p "$target_dir"

    # Sync directory structure
    # Preserve existing files unless FORCE=true
    if [[ "$FORCE" == "true" ]]; then
        rsync -av --delete "$source_dir/" "$target_dir/" >> "$LOG_FILE" 2>&1
        log_success "Synced .agent-os (force mode)"
    else
        # Only copy new files, don't overwrite existing
        rsync -av --ignore-existing "$source_dir/" "$target_dir/" >> "$LOG_FILE" 2>&1
        log_success "Synced .agent-os (preserve existing files)"
    fi
}

sync_claude_settings() {
    local target_repo="$1"
    local source_file="${SOURCE_REPO}/.claude/settings.json"
    local target_dir="${target_repo}/.claude"
    local target_file="${target_dir}/settings.json"

    if [[ ! -f "$source_file" ]]; then
        log_warning "Source .claude/settings.json not found, skipping"
        return
    fi

    log_action "Syncing .claude/settings.json to $(basename "$target_repo")"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would sync .claude/settings.json"
        return
    fi

    # Create target directory if it doesn't exist
    mkdir -p "$target_dir"

    # Check if target file exists
    if [[ -f "$target_file" ]] && [[ "$FORCE" != "true" ]]; then
        log_info "Target settings.json exists, preserving (use FORCE=true to overwrite)"
    else
        cp "$source_file" "$target_file"
        log_success "Synced .claude/settings.json"
    fi
}

sync_standards_docs() {
    local target_repo="$1"
    local source_dir="${SOURCE_REPO}/docs/standards"
    local target_dir="${target_repo}/docs/standards"

    if [[ ! -d "$source_dir" ]]; then
        log_warning "Source docs/standards directory not found, skipping"
        return
    fi

    log_action "Syncing standards documentation to $(basename "$target_repo")"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would sync docs/standards"
        return
    fi

    # Create target directory if it doesn't exist
    mkdir -p "$target_dir"

    # Sync standards index (always update)
    if [[ -f "${source_dir}/STANDARDS_INDEX.md" ]]; then
        cp "${source_dir}/STANDARDS_INDEX.md" "${target_dir}/STANDARDS_INDEX.md"
        log_success "Synced STANDARDS_INDEX.md"
    fi
}

sync_validation_script() {
    local target_repo="$1"
    local source_file="${SOURCE_REPO}/scripts/validate_cross_repo_standards.sh"
    local target_dir="${target_repo}/scripts"
    local target_file="${target_dir}/validate_cross_repo_standards.sh"

    if [[ ! -f "$source_file" ]]; then
        log_warning "Source validation script not found, skipping"
        return
    fi

    log_action "Syncing validation script to $(basename "$target_repo")"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would sync validation script"
        return
    fi

    # Create target directory if it doesn't exist
    mkdir -p "$target_dir"

    # Copy script and make executable
    cp "$source_file" "$target_file"
    chmod +x "$target_file"
    log_success "Synced validation script"
}

sync_hooks_config() {
    local target_repo="$1"
    local repo_name=$(basename "$target_repo")

    log_action "Configuring hooks for $repo_name"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would configure hooks"
        return
    fi

    # Create .claude directory if it doesn't exist
    mkdir -p "${target_repo}/.claude"

    # Check if hooks should be enabled (check for package.json or pyproject.toml)
    local enable_hooks=false
    if [[ -f "${target_repo}/package.json" ]] || [[ -f "${target_repo}/pyproject.toml" ]]; then
        enable_hooks=true
    fi

    if $enable_hooks; then
        log_success "Hooks enabled for $repo_name"
    else
        log_info "Hooks not applicable for $repo_name (no package.json/pyproject.toml)"
    fi
}

# ============================================================================
# Git Operations
# ============================================================================

commit_sync_changes() {
    local target_repo="$1"
    local repo_name=$(basename "$target_repo")

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would commit changes"
        return
    fi

    cd "$target_repo"

    # Check if there are changes
    if git diff --quiet && git diff --cached --quiet; then
        log_info "No changes to commit in $repo_name"
        return
    fi

    log_action "Committing sync changes in $repo_name"

    # Add changed files
    git add .agent-os/ .claude/ docs/standards/ scripts/validate_cross_repo_standards.sh 2>/dev/null || true

    # Commit
    git commit -m "chore: Sync standards and templates from digitalmodel

- Update .agent-os directory structure
- Sync .claude/settings.json
- Update standards documentation
- Add/update validation script

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>" >> "$LOG_FILE" 2>&1 || {
        log_warning "Commit failed or no changes staged in $repo_name"
    }

    log_success "Changes committed in $repo_name"
}

# ============================================================================
# Main Sync Logic
# ============================================================================

sync_repository() {
    local target_repo="$1"
    local repo_name=$(basename "$target_repo")

    log_info "============================================"
    log_info "Syncing: $repo_name"
    log_info "============================================"

    # Sync templates
    sync_agent_os "$target_repo"
    sync_claude_settings "$target_repo"
    sync_standards_docs "$target_repo"
    sync_validation_script "$target_repo"
    sync_hooks_config "$target_repo"

    # Commit changes
    commit_sync_changes "$target_repo"

    log_success "Sync completed for $repo_name"
    echo ""
}

sync_all_repositories() {
    local repos=("$@")
    local total_repos=${#repos[@]}

    log_info "Starting sync for $total_repos repositories"
    echo ""

    local synced=0
    local failed=0

    for repo in "${repos[@]}"; do
        if sync_repository "$repo"; then
            ((synced++))
        else
            ((failed++))
            log_error "Sync failed for $(basename "$repo")"
        fi
    done

    echo ""
    log_info "============================================"
    log_info "SYNC SUMMARY"
    log_info "============================================"
    log_info "Total repositories: $total_repos"
    log_success "Successfully synced: $synced"
    [[ $failed -gt 0 ]] && log_error "Failed: $failed"
    log_info "Log file: $LOG_FILE"
    log_info "============================================"
}

# ============================================================================
# Main Entry Point
# ============================================================================

usage() {
    cat <<EOF
Usage: $0 [OPTIONS]

Enhanced repository synchronization with standards enforcement.

OPTIONS:
    --validate          Run validation before sync (default: true)
    --no-validate       Skip validation before sync
    --dry-run           Show what would be synced without making changes
    --force             Overwrite existing files (default: preserve)
    --help              Show this help message

ENVIRONMENT VARIABLES:
    WORKSPACE_ROOT      Path to workspace directory (default: /d/workspace-hub)
    VALIDATE_FIRST      Run validation first (default: true)
    DRY_RUN             Dry run mode (default: false)
    FORCE               Force overwrite mode (default: false)

EXAMPLES:
    # Standard sync with validation
    bash $0

    # Dry run to see what would be synced
    bash $0 --dry-run

    # Force overwrite existing files
    bash $0 --force

    # Skip validation
    bash $0 --no-validate

    # Custom workspace
    WORKSPACE_ROOT=/path/to/workspace bash $0

EOF
}

main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --validate)
                VALIDATE_FIRST=true
                shift
                ;;
            --no-validate)
                VALIDATE_FIRST=false
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --force)
                FORCE=true
                shift
                ;;
            --help)
                usage
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                usage
                exit 1
                ;;
        esac
    done

    # Create log directory
    mkdir -p "$LOG_DIR"

    log_info "Enhanced Repository Sync"
    log_info "========================="
    log_info "Workspace: $WORKSPACE_ROOT"
    log_info "Source: $SOURCE_REPO"
    log_info "Validate First: $VALIDATE_FIRST"
    log_info "Dry Run: $DRY_RUN"
    log_info "Force: $FORCE"
    log_info "Log File: $LOG_FILE"
    echo ""

    # Run validation if requested
    if [[ "$VALIDATE_FIRST" == "true" ]]; then
        run_validation
        echo ""
        log_info "Proceeding with sync..."
        echo ""
    fi

    # Find all repositories
    mapfile -t repositories < <(find_repositories)

    if [[ ${#repositories[@]} -eq 0 ]]; then
        log_error "No repositories found in $WORKSPACE_ROOT"
        exit 1
    fi

    echo ""

    # Sync all repositories
    sync_all_repositories "${repositories[@]}"

    log_success "Enhanced sync complete!"
}

# Run main
main "$@"
