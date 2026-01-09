#!/bin/bash
# ABOUTME: Claude usage monitoring script for workspace-hub
# ABOUTME: Tracks model usage and provides recommendations

set -e

# Colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Usage tracking file
USAGE_LOG="${HOME}/.workspace-hub/claude_usage.log"
USAGE_DIR="$(dirname "$USAGE_LOG")"

# Ensure usage directory exists
mkdir -p "$USAGE_DIR"

# Create usage log if it doesn't exist
if [ ! -f "$USAGE_LOG" ]; then
    echo "# Claude Usage Log" > "$USAGE_LOG"
    echo "# Format: TIMESTAMP|MODEL|REPOSITORY|TASK|TOKENS" >> "$USAGE_LOG"
fi

# Function to log usage
log_usage() {
    local model="$1"
    local repository="$2"
    local task="$3"
    local tokens="${4:-0}"

    echo "$(date +%Y-%m-%d_%H:%M:%S)|${model}|${repository}|${task}|${tokens}" >> "$USAGE_LOG"
}

# Function to get usage summary
get_usage_summary() {
    local period="$1"  # today, week, month

    case "$period" in
        today)
            filter_date=$(date +%Y-%m-%d)
            ;;
        week)
            # Last 7 days
            filter_date=$(date -d '7 days ago' +%Y-%m-%d)
            ;;
        month)
            # Last 30 days
            filter_date=$(date -d '30 days ago' +%Y-%m-%d)
            ;;
        *)
            filter_date=$(date +%Y-%m-%d)
            ;;
    esac

    # Count usage by model (handle empty log file)
    if [ ! -f "$USAGE_LOG" ]; then
        opus_count=0
        sonnet_count=0
        haiku_count=0
    else
        opus_count=$(grep "$filter_date" "$USAGE_LOG" 2>/dev/null | grep -c "opus" || true)
        sonnet_count=$(grep "$filter_date" "$USAGE_LOG" 2>/dev/null | grep -c "sonnet" || true)
        haiku_count=$(grep "$filter_date" "$USAGE_LOG" 2>/dev/null | grep -c "haiku" || true)

        # Ensure counts are numeric
        opus_count=${opus_count:-0}
        sonnet_count=${sonnet_count:-0}
        haiku_count=${haiku_count:-0}
    fi

    total=$((opus_count + sonnet_count + haiku_count))

    if [ "$total" -eq 0 ]; then
        echo -e "${YELLOW}No usage recorded for $period${NC}"
        return
    fi

    opus_pct=$((opus_count * 100 / total))
    sonnet_pct=$((sonnet_count * 100 / total))
    haiku_pct=$((haiku_count * 100 / total))

    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo -e "${CYAN}  Claude Usage Summary - ${period}${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo ""
    echo -e "  Total tasks: ${BLUE}${total}${NC}"
    echo ""
    echo -e "  ${GREEN}Opus:${NC}   ${opus_count} tasks (${opus_pct}%)"
    echo -e "  ${BLUE}Sonnet:${NC} ${sonnet_count} tasks (${sonnet_pct}%)"
    echo -e "  ${YELLOW}Haiku:${NC}  ${haiku_count} tasks (${haiku_pct}%)"
    echo ""

    # Provide recommendations
    if [ "$sonnet_pct" -gt 50 ]; then
        echo -e "${RED}⚠️  High Sonnet usage (${sonnet_pct}%)${NC}"
        echo -e "   Consider shifting tasks to Opus or Haiku"
        echo ""
    fi

    if [ "$haiku_pct" -lt 20 ]; then
        echo -e "${YELLOW}ℹ️  Low Haiku usage (${haiku_pct}%)${NC}"
        echo -e "   Opportunity to use Haiku for more quick tasks"
        echo ""
    fi

    # Optimal distribution reminder
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo -e "  ${GREEN}Target Distribution:${NC}"
    echo -e "  Opus: 30% | Sonnet: 40% | Haiku: 30%"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo ""
}

# Function to check current API usage (requires manual input)
check_api_usage() {
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo -e "${CYAN}  Current API Usage Check${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo ""
    echo "Go to: ${BLUE}https://claude.ai/settings/usage${NC}"
    echo ""
    echo "Enter current usage percentages:"
    echo ""

    read -p "Session usage (%): " session_usage
    read -p "Sonnet weekly usage (%): " sonnet_usage
    read -p "Overall weekly usage (%): " overall_usage

    echo ""
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo -e "${CYAN}  Usage Analysis${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo ""

    # Session analysis
    if [ "$session_usage" -gt 80 ]; then
        echo -e "${RED}⚠️  Session: ${session_usage}% - CRITICAL${NC}"
        echo "   Wait for session reset or batch remaining work"
    elif [ "$session_usage" -gt 60 ]; then
        echo -e "${YELLOW}⚠️  Session: ${session_usage}% - HIGH${NC}"
        echo "   Plan remaining tasks carefully"
    else
        echo -e "${GREEN}✓ Session: ${session_usage}% - GOOD${NC}"
    fi

    echo ""

    # Sonnet analysis
    if [ "$sonnet_usage" -gt 80 ]; then
        echo -e "${RED}⚠️  Sonnet: ${sonnet_usage}% - CRITICAL${NC}"
        echo "   STOP using Sonnet - switch to Opus/Haiku immediately"
    elif [ "$sonnet_usage" -gt 60 ]; then
        echo -e "${YELLOW}⚠️  Sonnet: ${sonnet_usage}% - HIGH${NC}"
        echo "   Minimize Sonnet usage - prefer Opus or Haiku"
    else
        echo -e "${GREEN}✓ Sonnet: ${sonnet_usage}% - GOOD${NC}"
    fi

    echo ""

    # Overall analysis
    if [ "$overall_usage" -gt 80 ]; then
        echo -e "${RED}⚠️  Overall: ${overall_usage}% - CRITICAL${NC}"
        echo "   Defer non-critical work until Tuesday reset"
    elif [ "$overall_usage" -gt 60 ]; then
        echo -e "${YELLOW}⚠️  Overall: ${overall_usage}% - HIGH${NC}"
        echo "   Monitor closely and prioritize important tasks"
    else
        echo -e "${GREEN}✓ Overall: ${overall_usage}% - GOOD${NC}"
    fi

    echo ""
    echo -e "${CYAN}═══════════════════════════════════════${NC}"

    # Save to log
    echo "$(date +%Y-%m-%d_%H:%M:%S)|CHECK|session:${session_usage}|sonnet:${sonnet_usage}|overall:${overall_usage}" >> "$USAGE_LOG"
}

# Function to show recommendations
show_recommendations() {
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo -e "${CYAN}  Model Selection Recommendations${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo ""
    echo -e "${GREEN}Use Opus for:${NC}"
    echo "  • Architecture decisions"
    echo "  • Multi-file refactoring (>5 files)"
    echo "  • Complex algorithm design"
    echo "  • Security-critical code review"
    echo ""
    echo -e "${BLUE}Use Sonnet for:${NC}"
    echo "  • Standard feature implementation"
    echo "  • Code review (single PR)"
    echo "  • Documentation writing"
    echo "  • Bug fixing"
    echo ""
    echo -e "${YELLOW}Use Haiku for:${NC}"
    echo "  • File existence checks"
    echo "  • Quick status updates"
    echo "  • Log analysis"
    echo "  • Template generation"
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo ""
}

# Function to generate weekly report
generate_weekly_report() {
    local report_file="$HOME/.workspace-hub/weekly_report_$(date +%Y%m%d).md"

    cat > "$report_file" << EOF
# Claude Usage Report - Week of $(date +%Y-%m-%d)

## Summary

$(get_usage_summary week)

## Repository Usage

EOF

    # Group by repository
    grep "$(date -d '7 days ago' +%Y-%m-%d)" "$USAGE_LOG" 2>/dev/null | \
        awk -F'|' '{print $3}' | sort | uniq -c | sort -rn | \
        while read count repo; do
            echo "- **${repo}:** ${count} tasks" >> "$report_file"
        done

    cat >> "$report_file" << EOF

## Model Distribution Target

- Opus: 30%
- Sonnet: 40%
- Haiku: 30%

## Recommendations

Review this report and adjust usage patterns for next week.

---
Generated: $(date +%Y-%m-%d_%H:%M:%S)
EOF

    echo -e "${GREEN}✓ Report generated: $report_file${NC}"
}

# Main menu
show_menu() {
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo -e "${CYAN}  Claude Usage Monitoring${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo ""
    echo "  1) Check current API usage"
    echo "  2) View today's summary"
    echo "  3) View this week's summary"
    echo "  4) View this month's summary"
    echo "  5) Show model selection recommendations"
    echo "  6) Generate weekly report"
    echo "  7) Log a task"
    echo "  0) Exit"
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo ""
}

# Command-line arguments
case "${1:-menu}" in
    check)
        check_api_usage
        ;;
    today)
        get_usage_summary today
        ;;
    week)
        get_usage_summary week
        ;;
    month)
        get_usage_summary month
        ;;
    recommendations|rec)
        show_recommendations
        ;;
    report)
        generate_weekly_report
        ;;
    log)
        if [ "$#" -lt 4 ]; then
            echo "Usage: $0 log <model> <repository> <task> [tokens]"
            echo "Example: $0 log sonnet digitalmodel 'Feature implementation' 2500"
            exit 1
        fi
        log_usage "$2" "$3" "$4" "${5:-0}"
        echo -e "${GREEN}✓ Logged: $2 task for $3${NC}"
        ;;
    menu|*)
        # Interactive menu
        while true; do
            show_menu
            read -p "Select option: " choice

            case $choice in
                1) check_api_usage ;;
                2) get_usage_summary today ;;
                3) get_usage_summary week ;;
                4) get_usage_summary month ;;
                5) show_recommendations ;;
                6) generate_weekly_report ;;
                7)
                    echo "Model (opus/sonnet/haiku): "
                    read model
                    echo "Repository: "
                    read repo
                    echo "Task description: "
                    read task
                    echo "Tokens (optional): "
                    read tokens
                    log_usage "$model" "$repo" "$task" "${tokens:-0}"
                    echo -e "${GREEN}✓ Task logged${NC}"
                    ;;
                0)
                    echo "Goodbye!"
                    exit 0
                    ;;
                *)
                    echo -e "${RED}Invalid option${NC}"
                    ;;
            esac

            echo ""
            read -p "Press Enter to continue..."
        done
        ;;
esac
