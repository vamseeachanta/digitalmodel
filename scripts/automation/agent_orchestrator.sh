#!/bin/bash

# AI Agent Orchestrator with Intelligent Selection
# Enhanced with multi-factor agent selection algorithm

set -e

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

WORKSPACE_ROOT="/mnt/github/workspace-hub"
REGISTRY_FILE="$WORKSPACE_ROOT/config/ai-agents-registry.json"
SELECTOR_SCRIPT="src/digitalmodel/modules/automation/intelligent_agent_selector.py"
WEIGHTS_CONFIG="config/agent-selection-weights.yaml"

# Usage function
usage() {
    echo "Usage: $0 <task-type> <description> [options]"
    echo ""
    echo "Task Types:"
    echo "  code-generation      - Generate new code"
    echo "  code-refactoring     - Refactor existing code"
    echo "  test-creation        - Create tests"
    echo "  code-review          - Review code quality"
    echo "  architecture-design  - Design system architecture"
    echo "  spec-creation        - Create specifications"
    echo "  requirement-analysis - Analyze requirements"
    echo "  documentation        - Create documentation"
    echo "  bug-fixing           - Fix bugs"
    echo "  migration            - Migrate code/framework"
    echo "  performance-opt      - Optimize performance"
    echo "  security-audit       - Security review"
    echo ""
    echo "Options:"
    echo "  --agent NAME         - Force specific agent (skip intelligent selection)"
    echo "  --with-review        - Include automated review"
    echo "  --complexity LEVEL   - simple|moderate|complex"
    echo "  --domain DOMAIN      - Specify technology domain"
    echo "  --output FILE        - Save results to FILE"
    echo "  --verbose            - Detailed output"
    echo "  --intelligent        - Use intelligent agent selection (default)"
    echo "  --show-reasoning     - Show selection reasoning"
    echo "  --agent-loads JSON   - Specify current agent loads"
    echo ""
    echo "Examples:"
    echo "  $0 code-generation \"Create user authentication API\" --with-review"
    echo "  $0 code-refactoring \"Optimize database queries\" --intelligent --show-reasoning"
    echo "  $0 spec-creation \"Design payment processing\" --agent-loads '{\"agent-a\":2}'"
    exit 1
}

# Check arguments
if [ $# -lt 2 ]; then
    usage
fi

TASK_TYPE=$1
DESCRIPTION=$2
FORCED_AGENT=""
WITH_REVIEW=false
COMPLEXITY="moderate"
DOMAIN=""
OUTPUT_FILE=""
VERBOSE=false
USE_INTELLIGENT=true
SHOW_REASONING=false
AGENT_LOADS=""

# Parse options
shift 2
while [ $# -gt 0 ]; do
    case $1 in
        --agent)
            FORCED_AGENT=$2
            USE_INTELLIGENT=false
            shift
            ;;
        --with-review)
            WITH_REVIEW=true
            ;;
        --complexity)
            COMPLEXITY=$2
            shift
            ;;
        --domain)
            DOMAIN=$2
            shift
            ;;
        --output)
            OUTPUT_FILE=$2
            shift
            ;;
        --verbose)
            VERBOSE=true
            ;;
        --intelligent)
            USE_INTELLIGENT=true
            ;;
        --show-reasoning)
            SHOW_REASONING=true
            ;;
        --agent-loads)
            AGENT_LOADS=$2
            shift
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
    shift
done

echo -e "${PURPLE}========================================${NC}"
echo -e "${PURPLE}AI Agent Orchestrator (Enhanced)${NC}"
echo -e "${PURPLE}========================================${NC}"
echo ""

# Check registry exists
if [ ! -f "$REGISTRY_FILE" ]; then
    echo -e "${RED}Error: Agent registry not found at $REGISTRY_FILE${NC}"
    exit 1
fi

echo -e "${YELLOW}Task Configuration:${NC}"
echo -e "${BLUE}  Type: $TASK_TYPE${NC}"
echo -e "${BLUE}  Description: $DESCRIPTION${NC}"
echo -e "${BLUE}  Complexity: $COMPLEXITY${NC}"
[ -n "$DOMAIN" ] && echo -e "${BLUE}  Domain: $DOMAIN${NC}"
echo ""

# Activate uv environment if available
if [ -f ".venv/bin/activate" ]; then
    source .venv/bin/activate
elif [ -f ".venv/Scripts/activate" ]; then
    source .venv/Scripts/activate
fi

# Select agent using intelligent selection or fallback
if [ "$USE_INTELLIGENT" == "true" ] && [ -z "$FORCED_AGENT" ]; then
    echo -e "${CYAN}========================================${NC}"
    echo -e "${CYAN}Intelligent Agent Selection${NC}"
    echo -e "${CYAN}========================================${NC}"
    echo ""

    # Check if Python selector exists
    if [ ! -f "$SELECTOR_SCRIPT" ]; then
        echo -e "${YELLOW}Warning: Intelligent selector not found at $SELECTOR_SCRIPT${NC}"
        echo -e "${YELLOW}Falling back to registry-based selection${NC}"
        echo ""
        USE_INTELLIGENT=false
    else
        # Build selector command
        SELECTOR_CMD="python $SELECTOR_SCRIPT \"$TASK_TYPE\" \"$DESCRIPTION\""

        if [ -f "$WEIGHTS_CONFIG" ]; then
            SELECTOR_CMD="$SELECTOR_CMD --config $WEIGHTS_CONFIG"
        fi

        if [ -f "$REGISTRY_FILE" ]; then
            SELECTOR_CMD="$SELECTOR_CMD --registry $REGISTRY_FILE"
        fi

        if [ -n "$AGENT_LOADS" ]; then
            SELECTOR_CMD="$SELECTOR_CMD --load '$AGENT_LOADS'"
        fi

        # Run intelligent selector
        echo -e "${BLUE}Running intelligent selection algorithm...${NC}"
        SELECTION_RESULT=$(eval $SELECTOR_CMD 2>&1)
        SELECTOR_EXIT=$?

        if [ $SELECTOR_EXIT -ne 0 ]; then
            echo -e "${RED}Error running intelligent selector:${NC}"
            echo "$SELECTION_RESULT"
            echo -e "${YELLOW}Falling back to registry-based selection${NC}"
            echo ""
            USE_INTELLIGENT=false
        else
            # Parse JSON result
            PRIMARY_AGENT=$(echo "$SELECTION_RESULT" | jq -r '.agent')
            CONFIDENCE=$(echo "$SELECTION_RESULT" | jq -r '.confidence')
            REASONING=$(echo "$SELECTION_RESULT" | jq -r '.reasoning')
            ALTERNATIVES=$(echo "$SELECTION_RESULT" | jq -r '.alternatives[]?.agent // empty' | tr '\n' ', ' | sed 's/,$//')

            if [ "$PRIMARY_AGENT" == "null" ] || [ -z "$PRIMARY_AGENT" ]; then
                echo -e "${RED}No suitable agent found${NC}"
                exit 1
            fi

            echo -e "${GREEN}✓ Selected agent: $PRIMARY_AGENT${NC}"
            echo -e "${BLUE}  Confidence: ${CONFIDENCE}${NC}"

            if [ "$SHOW_REASONING" == "true" ] || [ "$VERBOSE" == "true" ]; then
                echo -e "${BLUE}  Reasoning: $REASONING${NC}"

                # Show component scores
                echo ""
                echo -e "${YELLOW}Score Breakdown:${NC}"
                echo "$SELECTION_RESULT" | jq -r '.scores | to_entries[] | "  \(.key): \(.value)"' | while read -r line; do
                    echo -e "${BLUE}$line${NC}"
                done
            fi

            if [ -n "$ALTERNATIVES" ]; then
                echo -e "${BLUE}  Alternatives: $ALTERNATIVES${NC}"
            fi
            echo ""
        fi
    fi
fi

# Fallback to registry-based selection
if [ "$USE_INTELLIGENT" == "false" ]; then
    if [ -n "$FORCED_AGENT" ]; then
        PRIMARY_AGENT=$FORCED_AGENT
        echo -e "${YELLOW}Using forced agent: $PRIMARY_AGENT${NC}"
    else
        # Get recommended agent from registry based on complexity
        if [ "$COMPLEXITY" == "simple" ]; then
            PRIMARY_AGENT=$(jq -r ".agentSelectionRules.complexityThresholds.simple.primary" "$REGISTRY_FILE")
        elif [ "$COMPLEXITY" == "complex" ]; then
            PRIMARY_AGENT=$(jq -r ".agentSelectionRules.complexityThresholds.complex.primary" "$REGISTRY_FILE")
        else
            # Try to get task-specific agent
            PRIMARY_AGENT=$(jq -r ".taskTypeAgentMapping.\"${TASK_TYPE}\".primary // \"claude-sonnet-4.5\"" "$REGISTRY_FILE")
        fi

        echo -e "${YELLOW}Selected agent: $PRIMARY_AGENT${NC}"
        echo -e "${BLUE}  Based on: task type + complexity${NC}"
    fi

    # Get alternative agents
    ALTERNATIVES=$(jq -r ".taskTypeAgentMapping.\"${TASK_TYPE}\".alternatives[]? // empty" "$REGISTRY_FILE")
    if [ -n "$ALTERNATIVES" ]; then
        echo -e "${BLUE}  Alternatives: $(echo $ALTERNATIVES | tr '\n' ', ' | sed 's/,$//')${NC}"
    fi
    echo ""
fi

# Get agent platform and capabilities
AGENT_PLATFORM=$(jq -r ".agents.\"${PRIMARY_AGENT}\".platform // \"unknown\"" "$REGISTRY_FILE")
AGENT_TYPE=$(jq -r ".agents.\"${PRIMARY_AGENT}\".type // \"unknown\"" "$REGISTRY_FILE")

echo -e "${YELLOW}Agent Details:${NC}"
echo -e "${BLUE}  Platform: $AGENT_PLATFORM${NC}"
echo -e "${BLUE}  Type: $AGENT_TYPE${NC}"
echo ""

# Generate execution command based on platform
echo -e "${YELLOW}Generating execution plan...${NC}"
echo ""

case $AGENT_PLATFORM in
    "claude-code")
        echo -e "${PURPLE}Execution Plan (Claude Code):${NC}"
        echo -e "${BLUE}1. Use Claude Code Task tool to spawn agent${NC}"
        echo -e "${BLUE}2. Provide task description and requirements${NC}"
        echo -e "${BLUE}3. Monitor progress through hooks${NC}"
        if [ "$WITH_REVIEW" == "true" ]; then
            REVIEWER=$(jq -r ".taskTypeAgentMapping.\"${TASK_TYPE}\".reviewers[0] // \"claude-flow-reviewer\"" "$REGISTRY_FILE")
            echo -e "${BLUE}4. Execute review with: $REVIEWER${NC}"
        fi
        echo ""
        echo -e "${PURPLE}Claude Code Command:${NC}"
        echo -e "${GREEN}Task(\"$PRIMARY_AGENT\", \"$DESCRIPTION\", \"${PRIMARY_AGENT}\")${NC}"
        ;;

    "factory-ai")
        echo -e "${PURPLE}Execution Plan (Factory.ai):${NC}"
        echo -e "${BLUE}1. Execute droid command with task${NC}"
        echo -e "${BLUE}2. Monitor droid output${NC}"
        echo -e "${BLUE}3. Review changes before committing${NC}"
        if [ "$WITH_REVIEW" == "true" ]; then
            echo -e "${BLUE}4. Run automated review${NC}"
        fi
        echo ""
        echo -e "${PURPLE}Factory.ai Command:${NC}"
        echo -e "${GREEN}droid exec \"$DESCRIPTION\"${NC}"
        ;;

    "claude-flow")
        echo -e "${PURPLE}Execution Plan (Claude Flow):${NC}"
        echo -e "${BLUE}1. Initialize swarm if needed${NC}"
        echo -e "${BLUE}2. Spawn $PRIMARY_AGENT via Task tool${NC}"
        echo -e "${BLUE}3. Execute with SPARC methodology${NC}"
        echo -e "${BLUE}4. Run coordination hooks${NC}"
        if [ "$WITH_REVIEW" == "true" ]; then
            echo -e "${BLUE}5. Execute review phase${NC}"
        fi
        echo ""
        echo -e "${PURPLE}Claude Flow Commands:${NC}"
        echo -e "${GREEN}npx claude-flow@alpha hooks pre-task --description \"$DESCRIPTION\"${NC}"
        echo -e "${GREEN}Task(\"$PRIMARY_AGENT\", \"$DESCRIPTION\", \"${PRIMARY_AGENT}\")${NC}"
        echo -e "${GREEN}npx claude-flow@alpha hooks post-task${NC}"
        ;;

    "spec-kit")
        echo -e "${PURPLE}Execution Plan (Spec-Kit):${NC}"
        echo -e "${BLUE}1. Use specify CLI for spec operations${NC}"
        echo -e "${BLUE}2. Validate spec evolution${NC}"
        echo -e "${BLUE}3. Track requirements${NC}"
        echo ""
        echo -e "${PURPLE}Spec-Kit Command:${NC}"
        echo -e "${GREEN}specify create \"$DESCRIPTION\"${NC}"
        ;;

    "agent-os")
        echo -e "${PURPLE}Execution Plan (Agent OS):${NC}"
        echo -e "${BLUE}1. Follow Agent OS workflow instructions${NC}"
        echo -e "${BLUE}2. Create specs via @~/.agent-os/instructions/create-spec.md${NC}"
        echo -e "${BLUE}3. Execute via @~/.agent-os/instructions/execute-tasks.md${NC}"
        echo ""
        ;;

    *)
        echo -e "${RED}Unknown agent platform: $AGENT_PLATFORM${NC}"
        exit 1
        ;;
esac

# If review requested, show review plan
if [ "$WITH_REVIEW" == "true" ]; then
    echo ""
    echo -e "${YELLOW}Review Configuration:${NC}"

    REVIEWERS=$(jq -r ".taskTypeAgentMapping.\"${TASK_TYPE}\".reviewers[]? // empty" "$REGISTRY_FILE")
    echo -e "${BLUE}Recommended reviewers:${NC}"
    echo "$REVIEWERS" | while read -r reviewer; do
        echo -e "${BLUE}  - $reviewer${NC}"
    done

    echo ""
    echo -e "${PURPLE}Review Execution:${NC}"
    echo -e "${GREEN}# After task completion, run:${NC}"
    REVIEW_PHASE=$(echo "$TASK_TYPE" | sed 's/-/ /g')
    echo -e "${GREEN}./scripts/automation/gate_pass_review.sh $REVIEW_PHASE . --auto${NC}"
fi

# Generate orchestration report
TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')

if [ -n "$OUTPUT_FILE" ]; then
    cat > "$OUTPUT_FILE" << EOF
# AI Agent Orchestration Report

**Timestamp:** $TIMESTAMP
**Task Type:** $TASK_TYPE
**Complexity:** $COMPLEXITY

## Task Description

$DESCRIPTION

## Agent Selection

- **Primary Agent:** $PRIMARY_AGENT
- **Platform:** $AGENT_PLATFORM
- **Type:** $AGENT_TYPE
$(if [ "$USE_INTELLIGENT" == "true" ]; then echo "- **Selection Method:** Intelligent (multi-factor scoring)"; echo "- **Confidence:** $CONFIDENCE"; else echo "- **Selection Method:** Registry-based"; fi)

$(if [ "$USE_INTELLIGENT" == "true" ] && [ "$SHOW_REASONING" == "true" ]; then echo "### Selection Reasoning"; echo ""; echo "$REASONING"; echo ""; echo "### Score Breakdown"; echo ""; echo '```json'; echo "$SELECTION_RESULT" | jq '.scores'; echo '```'; fi)

## Alternative Agents

$(echo "$ALTERNATIVES" | sed 's/^/- /')

## Execution Plan

See terminal output for platform-specific execution commands.

## Review Configuration

$(if [ "$WITH_REVIEW" == "true" ]; then echo "Automated review enabled with:"; echo "$REVIEWERS" | sed 's/^/- /'; else echo "No automated review requested"; fi)

---
*Generated by agent_orchestrator.sh (Enhanced with Intelligent Selection)*
EOF

    echo ""
    echo -e "${BLUE}Report saved to: $OUTPUT_FILE${NC}"
fi

echo ""
echo -e "${PURPLE}========================================${NC}"
echo -e "${PURPLE}Orchestration Complete${NC}"
echo -e "${PURPLE}========================================${NC}"
echo ""
echo -e "${YELLOW}Next Steps:${NC}"
echo -e "${BLUE}1. Execute the recommended command(s) above${NC}"
echo -e "${BLUE}2. Monitor progress through agent output${NC}"
if [ "$WITH_REVIEW" == "true" ]; then
    echo -e "${BLUE}3. Run gate-pass review when complete${NC}"
fi
echo -e "${BLUE}4. Commit changes if review passes${NC}"
echo ""

# Show quick reference
echo -e "${YELLOW}Quick Reference:${NC}"
echo -e "${BLUE}  View agent capabilities: jq '.agents.\"$PRIMARY_AGENT\"' $REGISTRY_FILE${NC}"
echo -e "${BLUE}  Update registry: ./scripts/automation/update_ai_agents_daily.sh${NC}"
echo -e "${BLUE}  Run gate-pass review: ./scripts/automation/gate_pass_review.sh <phase> .${NC}"
if [ "$USE_INTELLIGENT" == "true" ]; then
    echo -e "${BLUE}  Adjust selection weights: Edit $WEIGHTS_CONFIG${NC}"
    echo -e "${BLUE}  View selection history: sqlite3 .claude-flow/agent-performance.db${NC}"
fi
