#!/bin/bash
# analyze_patterns.sh - Analyze git history to identify instruction patterns
# Usage: ./analyze_patterns.sh [days]

set -e

WORKSPACE_ROOT="${WORKSPACE_ROOT:-D:/workspace-hub}"
DAYS="${1:-30}"
OUTPUT_FILE="$WORKSPACE_ROOT/.claude/state/context-patterns.yaml"

mkdir -p "$(dirname "$OUTPUT_FILE")"

echo "=========================================="
echo "Context Pattern Analysis"
echo "Analyzing last $DAYS days of commits"
echo "=========================================="
echo ""

# Initialize output
cat > "$OUTPUT_FILE" << EOF
# Context Patterns Analysis
# Generated: $(date '+%Y-%m-%d %H:%M:%S')
# Period: Last $DAYS days

patterns:
  claude_md_changes: []
  frequent_instructions: []
  redundant_content: []

suggestions:
  additions: []
  removals: []
  consolidations: []

metrics:
  total_commits_analyzed: 0
  claude_md_commits: 0
  repos_analyzed: 0
EOF

TOTAL_COMMITS=0
CLAUDE_COMMITS=0
REPOS_ANALYZED=0

# Analyze each repo
for repo_path in "$WORKSPACE_ROOT"/*/; do
    repo=$(basename "$repo_path")

    if [ -d "$repo_path/.git" ]; then
        ((REPOS_ANALYZED++))

        cd "$repo_path"

        # Count commits touching CLAUDE.md
        claude_commits=$(git log --since="$DAYS days ago" --oneline -- "CLAUDE.md" ".claude/" 2>/dev/null | wc -l || echo 0)
        total=$(git log --since="$DAYS days ago" --oneline 2>/dev/null | wc -l || echo 0)

        CLAUDE_COMMITS=$((CLAUDE_COMMITS + claude_commits))
        TOTAL_COMMITS=$((TOTAL_COMMITS + total))

        if [ "$claude_commits" -gt 0 ]; then
            echo "Repo: $repo - $claude_commits CLAUDE.md changes (of $total total)"

            # Get commit messages for CLAUDE.md changes
            git log --since="$DAYS days ago" --oneline -- "CLAUDE.md" ".claude/" 2>/dev/null | head -5
            echo ""
        fi

        cd "$WORKSPACE_ROOT"
    fi
done

# Update metrics in output
sed -i "s/total_commits_analyzed: 0/total_commits_analyzed: $TOTAL_COMMITS/" "$OUTPUT_FILE"
sed -i "s/claude_md_commits: 0/claude_md_commits: $CLAUDE_COMMITS/" "$OUTPUT_FILE"
sed -i "s/repos_analyzed: 0/repos_analyzed: $REPOS_ANALYZED/" "$OUTPUT_FILE"

echo "=========================================="
echo "Summary"
echo "=========================================="
echo "Repos analyzed: $REPOS_ANALYZED"
echo "Total commits: $TOTAL_COMMITS"
echo "CLAUDE.md commits: $CLAUDE_COMMITS"
echo "Output: $OUTPUT_FILE"

# Analyze for redundancy across CLAUDE.md files
echo ""
echo "=== Checking for Redundant Content ==="

# Find common lines across multiple CLAUDE.md files
common_lines=$(for f in "$WORKSPACE_ROOT"/*/CLAUDE.md; do cat "$f" 2>/dev/null; done | sort | uniq -c | sort -rn | head -10)

if [ -n "$common_lines" ]; then
    echo "Most repeated lines across CLAUDE.md files:"
    echo "$common_lines"
fi

echo ""
echo "Pattern analysis complete. Review $OUTPUT_FILE for details."
