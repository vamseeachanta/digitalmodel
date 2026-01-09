#!/bin/bash
# ABOUTME: Suggest appropriate Claude model based on task description
# ABOUTME: Uses keyword matching and complexity analysis

set -e

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

# Get repository and task from args or prompt
REPO="${1:-}"
TASK="${2:-}"

if [ -z "$REPO" ] || [ -z "$TASK" ]; then
    echo -e "${CYAN}Model Selection Helper${NC}"
    echo ""
    read -p "Repository: " REPO
    read -p "Task description: " TASK
    echo ""
fi

# Determine repository tier
WORK_TIER1="workspace-hub|digitalmodel|energy|frontierdeepwater"
WORK_TIER2="aceengineercode|assetutilities|worldenergydata|rock-oil-field|teamresumes"
WORK_TIER3="doris|saipem|OGManufacturing|seanation"
PERSONAL_ACTIVE="aceengineer-admin|aceengineer-website"
PERSONAL_EXPERIMENTAL="hobbies|sd-work|acma-projects|achantas-data"

# Complexity indicators
OPUS_KEYWORDS="architecture|refactor|design|security|complex|multi-file|algorithm|optimization|strategy|planning|cross-repository|performance|migration"
SONNET_KEYWORDS="implement|feature|bug|fix|code review|documentation|test|update|add|create|build"
HAIKU_KEYWORDS="check|status|simple|quick|template|list|grep|find|search|summary|validation|exists|show|display"

# Convert task to lowercase for matching
TASK_LOWER=$(echo "$TASK" | tr '[:upper:]' '[:lower:]')

# Score the complexity
complexity=0

# Keyword matching (mutually exclusive checks, first match wins)
if echo "$TASK_LOWER" | grep -qE "$OPUS_KEYWORDS"; then
    ((complexity+=3))
elif echo "$TASK_LOWER" | grep -qE "$SONNET_KEYWORDS"; then
    ((complexity+=1))
elif echo "$TASK_LOWER" | grep -qE "$HAIKU_KEYWORDS"; then
    ((complexity-=2))
fi

# Word count (longer descriptions = more complex)
word_count=$(echo "$TASK" | wc -w)
if [ "$word_count" -gt 15 ]; then
    ((complexity+=1))
elif [ "$word_count" -lt 5 ]; then
    ((complexity-=1))
fi

# Repository tier adjustment
if echo "$REPO" | grep -qE "$WORK_TIER1"; then
    repo_tier="Work Tier 1 (Production)"
    ((complexity+=1))  # Bias toward higher quality
elif echo "$REPO" | grep -qE "$WORK_TIER2"; then
    repo_tier="Work Tier 2 (Active)"
elif echo "$REPO" | grep -qE "$WORK_TIER3"; then
    repo_tier="Work Tier 3 (Maintenance)"
    ((complexity-=1))  # Bias toward efficiency
elif echo "$REPO" | grep -qE "$PERSONAL_ACTIVE"; then
    repo_tier="Personal (Active)"
elif echo "$REPO" | grep -qE "$PERSONAL_EXPERIMENTAL"; then
    repo_tier="Personal (Experimental)"
    ((complexity-=1))  # Bias toward efficiency
else
    repo_tier="Unknown"
fi

# Determine model
if [ "$complexity" -ge 3 ]; then
    model="opus"
    color=$GREEN
    confidence="High"
elif [ "$complexity" -le 0 ]; then
    model="haiku"
    color=$YELLOW
    confidence="High"
else
    model="sonnet"
    color=$BLUE
    confidence="Medium"
fi

# Display recommendation
echo -e "${CYAN}═══════════════════════════════════════${NC}"
echo -e "${CYAN}  Model Recommendation${NC}"
echo -e "${CYAN}═══════════════════════════════════════${NC}"
echo ""
echo -e "  Repository: ${CYAN}$REPO${NC}"
echo -e "  Tier: ${repo_tier}"
echo ""
echo -e "  Task: ${TASK}"
echo -e "  Complexity Score: ${complexity}"
echo ""
echo -e "  ${color}Recommended Model: ${model^^}${NC}"
echo -e "  Confidence: ${confidence}"
echo ""

# Reasoning
echo -e "${CYAN}Reasoning:${NC}"
if echo "$TASK_LOWER" | grep -qE "$OPUS_KEYWORDS"; then
    echo "  • Complex keywords detected (architecture, refactor, design, etc.)"
fi
if echo "$TASK_LOWER" | grep -qE "$SONNET_KEYWORDS"; then
    echo "  • Standard implementation keywords detected (implement, feature, fix, etc.)"
fi
if echo "$TASK_LOWER" | grep -qE "$HAIKU_KEYWORDS"; then
    echo "  • Simple task indicators (check, status, quick, etc.)"
fi
if [ "$word_count" -gt 15 ]; then
    echo "  • Detailed task description suggests complexity"
fi
if [ "$word_count" -lt 5 ]; then
    echo "  • Brief description suggests simple task"
fi
echo "  • Repository tier: $repo_tier"
echo ""

# Check current usage and warn if needed
USAGE_LOG="${HOME}/.workspace-hub/claude_usage.log"
if [ -f "$USAGE_LOG" ]; then
    today_date=$(date +%Y-%m-%d)
    sonnet_today=$(grep "$today_date" "$USAGE_LOG" 2>/dev/null | grep -c "sonnet" || true)
    total_today=$(grep "$today_date" "$USAGE_LOG" 2>/dev/null | grep -cE "opus|sonnet|haiku" || true)

    if [ "${total_today:-0}" -gt 0 ]; then
        sonnet_pct=$((sonnet_today * 100 / total_today))
        if [ "$sonnet_pct" -gt 60 ] && [ "$model" = "sonnet" ]; then
            echo -e "${YELLOW}⚠️  Warning: Sonnet usage today is ${sonnet_pct}%${NC}"
            echo -e "   Consider using ${GREEN}Opus${NC} or ${YELLOW}Haiku${NC} instead"
            echo ""
        fi
    fi
fi

# Alternative suggestions
echo -e "${CYAN}Alternatives:${NC}"
case $model in
    opus)
        echo -e "  • ${BLUE}Sonnet${NC} - If task is more standard than complex"
        ;;
    sonnet)
        echo -e "  • ${GREEN}Opus${NC} - If task requires deeper analysis"
        echo -e "  • ${YELLOW}Haiku${NC} - If task is simpler than expected"
        ;;
    haiku)
        echo -e "  • ${BLUE}Sonnet${NC} - If task needs higher quality output"
        ;;
esac

echo ""
echo -e "${CYAN}═══════════════════════════════════════${NC}"
echo ""

# Option to log this prediction
read -p "Use this recommendation? (y/n): " use_rec
if [ "$use_rec" = "y" ]; then
    # Log to usage monitor if available
    if [ -x "./scripts/monitoring/check_claude_usage.sh" ]; then
        read -p "Estimated tokens (or press Enter to skip): " tokens
        ./scripts/monitoring/check_claude_usage.sh log "$model" "$REPO" "$TASK" "${tokens:-0}"
    fi
fi
