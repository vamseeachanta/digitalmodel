#!/bin/bash
# validate_context.sh - Validate context file sizes against limits
# Usage: ./validate_context.sh [repo_name]

set -e

# Configuration
WORKSPACE_ROOT="${WORKSPACE_ROOT:-D:/workspace-hub}"
GLOBAL_CLAUDE="$HOME/.claude/CLAUDE.md"
MAX_GLOBAL=2048      # 2KB
MAX_WORKSPACE=4096   # 4KB
MAX_PROJECT=8192     # 8KB
MAX_LOCAL=2048       # 2KB

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Results
VIOLATIONS=0
WARNINGS=0

check_file() {
    local file="$1"
    local max_size="$2"
    local label="$3"

    if [ -f "$file" ]; then
        size=$(wc -c < "$file" 2>/dev/null || echo 0)
        lines=$(wc -l < "$file" 2>/dev/null || echo 0)

        if [ "$size" -gt "$max_size" ]; then
            echo -e "${RED}VIOLATION${NC}: $label"
            echo "  File: $file"
            echo "  Size: ${size} bytes (limit: ${max_size})"
            echo "  Lines: ${lines}"
            echo "  Over by: $((size - max_size)) bytes"
            ((VIOLATIONS++))
        elif [ "$size" -gt $((max_size * 80 / 100)) ]; then
            echo -e "${YELLOW}WARNING${NC}: $label approaching limit"
            echo "  File: $file"
            echo "  Size: ${size} bytes (limit: ${max_size}, ${size}%)"
            ((WARNINGS++))
        else
            echo -e "${GREEN}OK${NC}: $label - ${size} bytes (${lines} lines)"
        fi
    else
        echo -e "${YELLOW}SKIP${NC}: $label - file not found"
    fi
}

echo "=========================================="
echo "Context File Validation Report"
echo "Date: $(date '+%Y-%m-%d %H:%M:%S')"
echo "=========================================="
echo ""

# Check global CLAUDE.md
echo "=== Global Context ==="
check_file "$GLOBAL_CLAUDE" "$MAX_GLOBAL" "Global CLAUDE.md"
echo ""

# Check workspace CLAUDE.md
echo "=== Workspace Context ==="
check_file "$WORKSPACE_ROOT/CLAUDE.md" "$MAX_WORKSPACE" "Workspace CLAUDE.md"
echo ""

# Check project CLAUDE.md files
echo "=== Project Context Files ==="

if [ -n "$1" ]; then
    # Single repo specified
    repos=("$1")
else
    # All repos with .claude directories
    repos=($(find "$WORKSPACE_ROOT" -maxdepth 2 -name ".claude" -type d | xargs -I{} dirname {} | xargs -I{} basename {}))
fi

for repo in "${repos[@]}"; do
    repo_path="$WORKSPACE_ROOT/$repo"
    if [ -d "$repo_path" ] && [ -f "$repo_path/CLAUDE.md" ]; then
        check_file "$repo_path/CLAUDE.md" "$MAX_PROJECT" "$repo/CLAUDE.md"

        # Check local if exists
        if [ -f "$repo_path/CLAUDE.local.md" ]; then
            check_file "$repo_path/CLAUDE.local.md" "$MAX_LOCAL" "$repo/CLAUDE.local.md"
        fi
    fi
done

echo ""
echo "=========================================="
echo "Summary"
echo "=========================================="
echo "Violations: $VIOLATIONS"
echo "Warnings: $WARNINGS"

if [ "$VIOLATIONS" -gt 0 ]; then
    echo -e "${RED}ACTION REQUIRED: Fix violations before proceeding${NC}"
    exit 1
elif [ "$WARNINGS" -gt 0 ]; then
    echo -e "${YELLOW}Consider optimizing files approaching limits${NC}"
    exit 0
else
    echo -e "${GREEN}All context files within limits${NC}"
    exit 0
fi
