#!/bin/bash
# improve_context.sh - Apply approved improvements to context files
# Usage: ./improve_context.sh [--dry-run] [--apply]

set -e

WORKSPACE_ROOT="${WORKSPACE_ROOT:-D:/workspace-hub}"
DRY_RUN=true
SUGGESTIONS_FILE="$WORKSPACE_ROOT/.claude/state/improvement-suggestions.yaml"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --apply)
            DRY_RUN=false
            shift
            ;;
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=========================================="
echo "Context Improvement Tool"
echo "Mode: $([ "$DRY_RUN" = true ] && echo 'DRY RUN' || echo 'APPLY')"
echo "=========================================="
echo ""

# Generate improvement suggestions
generate_suggestions() {
    echo "Generating improvement suggestions..."

    mkdir -p "$(dirname "$SUGGESTIONS_FILE")"

    cat > "$SUGGESTIONS_FILE" << EOF
# Improvement Suggestions
# Generated: $(date '+%Y-%m-%d %H:%M:%S')
# Review and approve before applying

suggestions:
EOF

    # Check for oversized files
    for repo_path in "$WORKSPACE_ROOT"/*/; do
        repo=$(basename "$repo_path")
        claude_file="$repo_path/CLAUDE.md"

        if [ -f "$claude_file" ]; then
            size=$(wc -c < "$claude_file")

            if [ "$size" -gt 8192 ]; then
                echo "  - repo: $repo" >> "$SUGGESTIONS_FILE"
                echo "    action: reduce_size" >> "$SUGGESTIONS_FILE"
                echo "    current_size: $size" >> "$SUGGESTIONS_FILE"
                echo "    target_size: 8192" >> "$SUGGESTIONS_FILE"
                echo "    suggestion: Extract reference material to .claude/docs/" >> "$SUGGESTIONS_FILE"
                echo ""
            fi
        fi
    done

    # Check for missing reference docs
    for repo_path in "$WORKSPACE_ROOT"/*/; do
        repo=$(basename "$repo_path")
        docs_dir="$repo_path/.claude/docs"

        if [ ! -d "$docs_dir" ] && [ -f "$repo_path/CLAUDE.md" ]; then
            echo "  - repo: $repo" >> "$SUGGESTIONS_FILE"
            echo "    action: create_docs_dir" >> "$SUGGESTIONS_FILE"
            echo "    suggestion: Create .claude/docs/ for reference documentation" >> "$SUGGESTIONS_FILE"
            echo ""
        fi
    done

    echo "Suggestions written to: $SUGGESTIONS_FILE"
}

# Apply improvements
apply_improvements() {
    if [ ! -f "$SUGGESTIONS_FILE" ]; then
        echo "No suggestions file found. Run generate first."
        exit 1
    fi

    echo "Applying approved improvements..."

    # Copy reference docs to repos missing them
    for repo_path in "$WORKSPACE_ROOT"/*/; do
        repo=$(basename "$repo_path")
        docs_dir="$repo_path/.claude/docs"

        if [ ! -d "$docs_dir" ] && [ -d "$WORKSPACE_ROOT/digitalmodel/.claude/docs" ]; then
            if [ "$DRY_RUN" = true ]; then
                echo "[DRY RUN] Would create $docs_dir and copy reference docs"
            else
                mkdir -p "$docs_dir"
                cp "$WORKSPACE_ROOT/digitalmodel/.claude/docs/"*.md "$docs_dir/" 2>/dev/null || true
                echo "Created $docs_dir with reference docs"
            fi
        fi
    done

    echo "Improvements applied."
}

# Main
generate_suggestions
echo ""

if [ "$DRY_RUN" = false ]; then
    apply_improvements
else
    echo "Run with --apply to apply improvements"
fi
