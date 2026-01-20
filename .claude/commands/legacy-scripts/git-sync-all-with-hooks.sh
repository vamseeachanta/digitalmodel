#!/bin/bash
# Enhanced git sync --all command with verification hooks integration
# This replaces the standard git sync --all to include hook distribution

set -euo pipefail

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOOKS_DIR="$(dirname "$SCRIPT_DIR")/hooks"

# Source verification hooks
if [[ -f "$HOOKS_DIR/verification_hooks.sh" ]]; then
    source "$HOOKS_DIR/verification_hooks.sh"
    echo -e "${GREEN}[✓ LOADED]${NC} Verification hooks loaded"
else
    echo "Warning: Verification hooks not found, continuing without verification"
fi

# Source git sync integration
if [[ -f "$HOOKS_DIR/git_sync_integration.sh" ]]; then
    source "$HOOKS_DIR/git_sync_integration.sh"
    echo -e "${GREEN}[✓ LOADED]${NC} Git sync integration loaded"
else
    echo "Error: Git sync integration not found"
    exit 1
fi

# Main execution
echo -e "${YELLOW}╔═══════════════════════════════════════════════════════╗${NC}"
echo -e "${YELLOW}║     GIT SYNC --ALL WITH VERIFICATION HOOKS           ║${NC}"
echo -e "${YELLOW}╚═══════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${BLUE}[INFO]${NC} This enhanced version includes:"
echo -e "  • Verification of all operations"
echo -e "  • Distribution of hooks to all repositories"
echo -e "  • Return code checking"
echo -e "  • Output validation"
echo -e "  • Comprehensive error reporting"
echo ""

# Parse arguments
BASE_DIR="${1:-/d/github}"
SOURCE_REPO="${2:-digitalmodel}"

# Run the sync with hooks
sync_all_repos_with_hooks "$BASE_DIR" "$SOURCE_REPO"

# Exit with appropriate code
exit $?