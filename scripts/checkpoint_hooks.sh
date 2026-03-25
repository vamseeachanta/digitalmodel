#!/bin/bash
# ABOUTME: Claude-flow hooks integration for automatic checkpointing
# Integrates with claude-flow hooks to automatically create checkpoints

set -e

# Configuration
CHECKPOINT_CLI="python $(dirname "$0")/checkpoint_cli.py"
REPO_ROOT=$(git rev-parse --show-toplevel)

# Check if checkpoints are enabled
if [ "${CLAUDE_FLOW_CHECKPOINTS_ENABLED:-true}" = "false" ]; then
    exit 0
fi

# Hook: pre-task
pre_task_hook() {
    local description="$1"
    local workflow_id="${2:-default}"
    local phase="${3:-unknown}"

    echo "🔄 Creating pre-task checkpoint..."
    $CHECKPOINT_CLI create "$workflow_id" "$phase" \
        --description "Pre-task: $description" \
        --tags pre-task
}

# Hook: post-task
post_task_hook() {
    local task_id="$1"
    local workflow_id="${2:-default}"
    local phase="${3:-unknown}"

    echo "✓ Creating post-task checkpoint..."
    $CHECKPOINT_CLI create "$workflow_id" "$phase" \
        --description "Post-task: $task_id" \
        --tags post-task
}

# Hook: session-end
session_end_hook() {
    local workflow_id="${1:-default}"

    echo "🧹 Running checkpoint cleanup..."
    $CHECKPOINT_CLI clean
}

# Hook: phase checkpoint (SPARC phases)
phase_checkpoint_hook() {
    local phase="$1"
    local workflow_id="${2:-default}"

    echo "📍 Creating $phase phase checkpoint..."
    $CHECKPOINT_CLI create "$workflow_id" "$phase" \
        --description "SPARC phase: $phase" \
        --tags "sparc-phase" "$phase"
}

# Main entry point
main() {
    local hook_type="$1"
    shift

    case "$hook_type" in
        pre-task)
            pre_task_hook "$@"
            ;;
        post-task)
            post_task_hook "$@"
            ;;
        session-end)
            session_end_hook "$@"
            ;;
        phase)
            phase_checkpoint_hook "$@"
            ;;
        *)
            echo "Unknown hook type: $hook_type" >&2
            exit 1
            ;;
    esac
}

# Run if executed directly
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main "$@"
fi
