#!/bin/bash
# Portable statusline for Claude Code - works across machines/users
# Location: workspace-hub/.claude/statusline-command.sh
# Shows: model | workspace | [context bar] percentage

# Read JSON input from stdin
input=$(cat)

# Extract values from JSON using jq
model_name=$(echo "$input" | jq -r '.model.display_name // "Unknown"')
current_dir=$(echo "$input" | jq -r '.workspace.current_dir // ""')
project_dir=$(echo "$input" | jq -r '.workspace.project_dir // ""')
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // 0')

# Get workspace name (repo name or relative path)
if [ -n "$project_dir" ]; then
    workspace="$(basename "$project_dir")"
else
    workspace="$(basename "$current_dir" 2>/dev/null || echo "~")"
fi

# Create context progress bar (20 chars wide)
bar_width=20
used_bars=$(printf "%.0f" "$(echo "$used_pct * $bar_width / 100" | bc -l 2>/dev/null || echo 0)")
[ -z "$used_bars" ] && used_bars=0
[ "$used_bars" -gt "$bar_width" ] && used_bars=$bar_width
remaining_bars=$((bar_width - used_bars))

# Build progress bar with ANSI colors
bar=""
for ((i=0; i<used_bars; i++)); do
    if (( $(echo "$used_pct < 70" | bc -l 2>/dev/null || echo 1) )); then
        bar+="\033[32m█\033[0m"  # Green
    elif (( $(echo "$used_pct < 85" | bc -l 2>/dev/null || echo 0) )); then
        bar+="\033[33m█\033[0m"  # Yellow
    else
        bar+="\033[31m█\033[0m"  # Red
    fi
done
for ((i=0; i<remaining_bars; i++)); do
    bar+="\033[2m░\033[0m"  # Dim
done

# Format percentage
used_pct_fmt=$(printf "%.1f" "$used_pct" 2>/dev/null || echo "0.0")

# Output: model | workspace | [bar] pct%
printf "\033[2m%s\033[0m | \033[36m%s\033[0m | [%b] \033[1m%s%%\033[0m" \
    "$model_name" "$workspace" "$bar" "$used_pct_fmt"
