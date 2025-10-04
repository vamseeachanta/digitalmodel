#!/bin/bash

# Gate-Pass Review System
# Automated review checkpoints for SPARC methodology phases

set -e

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

WORKSPACE_ROOT="/mnt/github/workspace-hub"
REGISTRY_FILE="$WORKSPACE_ROOT/modules/config/ai-agents-registry.json"

# Usage function
usage() {
    echo "Usage: $0 <phase> <target-directory> [options]"
    echo ""
    echo "Phases:"
    echo "  specification  - Review requirements and specs"
    echo "  pseudocode     - Review algorithm design"
    echo "  architecture   - Review system design"
    echo "  implementation - Review code implementation"
    echo "  testing        - Review test coverage and quality"
    echo "  refinement     - Review optimizations and refactoring"
    echo "  completion     - Final integration review"
    echo ""
    echo "Options:"
    echo "  --auto         - Auto-approve if pass criteria met"
    echo "  --report FILE  - Save review report to FILE"
    echo "  --verbose      - Detailed output"
    exit 1
}

# Check arguments
if [ $# -lt 2 ]; then
    usage
fi

PHASE=$1
TARGET_DIR=$2
AUTO_APPROVE=false
REPORT_FILE=""
VERBOSE=false

# Parse options
shift 2
while [ $# -gt 0 ]; do
    case $1 in
        --auto)
            AUTO_APPROVE=true
            ;;
        --report)
            REPORT_FILE=$2
            shift
            ;;
        --verbose)
            VERBOSE=true
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
    shift
done

# Validate phase
case $PHASE in
    specification|pseudocode|architecture|implementation|testing|refinement|completion)
        ;;
    *)
        echo -e "${RED}Error: Invalid phase '$PHASE'${NC}"
        usage
        ;;
esac

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Gate-Pass Review: ${PHASE^} Phase${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Get reviewer configuration from registry
if [ ! -f "$REGISTRY_FILE" ]; then
    echo -e "${RED}Error: Agent registry not found${NC}"
    exit 1
fi

PRIMARY_REVIEWER=$(jq -r ".gatePassReviewMatrix.\"${PHASE}-phase\".primaryReviewer" "$REGISTRY_FILE")
SECONDARY_REVIEWER=$(jq -r ".gatePassReviewMatrix.\"${PHASE}-phase\".secondaryReviewer" "$REGISTRY_FILE")
CHECKPOINTS=$(jq -r ".gatePassReviewMatrix.\"${PHASE}-phase\".checkpoints[]" "$REGISTRY_FILE")

echo -e "${YELLOW}Review Configuration:${NC}"
echo -e "${BLUE}  Primary Reviewer: $PRIMARY_REVIEWER${NC}"
echo -e "${BLUE}  Secondary Reviewer: $SECONDARY_REVIEWER${NC}"
echo -e "${BLUE}  Target: $TARGET_DIR${NC}"
echo ""

echo -e "${YELLOW}Checkpoints:${NC}"
echo "$CHECKPOINTS" | while read -r checkpoint; do
    echo -e "${BLUE}  - $checkpoint${NC}"
done
echo ""

# Initialize review report
REVIEW_TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')
REVIEW_REPORT=""

add_to_report() {
    REVIEW_REPORT="${REVIEW_REPORT}$1\n"
}

add_to_report "# Gate-Pass Review Report"
add_to_report ""
add_to_report "**Phase:** ${PHASE^}"
add_to_report "**Target:** $TARGET_DIR"
add_to_report "**Timestamp:** $REVIEW_TIMESTAMP"
add_to_report "**Primary Reviewer:** $PRIMARY_REVIEWER"
add_to_report "**Secondary Reviewer:** $SECONDARY_REVIEWER"
add_to_report ""
add_to_report "## Checkpoints"
add_to_report ""

# Simulate checkpoint reviews (in production, this would call actual AI agents)
echo -e "${YELLOW}Executing checkpoint reviews...${NC}"
echo ""

PASS_COUNT=0
FAIL_COUNT=0
TOTAL_CHECKPOINTS=0

echo "$CHECKPOINTS" | while read -r checkpoint; do
    ((TOTAL_CHECKPOINTS++))
    echo -e "${BLUE}Checkpoint: ${checkpoint}${NC}"

    # In production, this would invoke the appropriate AI agent
    # For now, we'll simulate with file checks

    case $checkpoint in
        requirements-completeness|spec-validation)
            if [ -f "$TARGET_DIR/spec.md" ] || [ -f "$TARGET_DIR/.agent-os/specs/"*"/spec.md" ]; then
                echo -e "${GREEN}  ✓ PASS${NC}"
                ((PASS_COUNT++))
                add_to_report "- ✓ **$checkpoint**: PASS"
            else
                echo -e "${RED}  ✗ FAIL - No spec file found${NC}"
                ((FAIL_COUNT++))
                add_to_report "- ✗ **$checkpoint**: FAIL - No spec file found"
            fi
            ;;

        test-coverage|test-quality)
            if find "$TARGET_DIR" -type f -name "*test*.py" -o -name "*test*.js" | grep -q .; then
                echo -e "${GREEN}  ✓ PASS${NC}"
                ((PASS_COUNT++))
                add_to_report "- ✓ **$checkpoint**: PASS"
            else
                echo -e "${YELLOW}  ⚠ WARNING - Limited test files${NC}"
                add_to_report "- ⚠ **$checkpoint**: WARNING - Limited test files"
            fi
            ;;

        code-quality|style-compliance)
            if [ -f "$TARGET_DIR/pyproject.toml" ] || [ -f "$TARGET_DIR/package.json" ]; then
                echo -e "${GREEN}  ✓ PASS${NC}"
                ((PASS_COUNT++))
                add_to_report "- ✓ **$checkpoint**: PASS"
            else
                echo -e "${YELLOW}  ⚠ WARNING - No project config found${NC}"
                add_to_report "- ⚠ **$checkpoint**: WARNING - No project config found"
            fi
            ;;

        *)
            echo -e "${BLUE}  ℹ INFO - Checkpoint requires manual review${NC}"
            add_to_report "- ℹ **$checkpoint**: Requires manual review"
            ;;
    esac
    echo ""
done

# Calculate pass rate
if [ $TOTAL_CHECKPOINTS -gt 0 ]; then
    PASS_RATE=$((PASS_COUNT * 100 / TOTAL_CHECKPOINTS))
else
    PASS_RATE=0
fi

add_to_report ""
add_to_report "## Results"
add_to_report ""
add_to_report "- **Total Checkpoints:** $TOTAL_CHECKPOINTS"
add_to_report "- **Passed:** $PASS_COUNT"
add_to_report "- **Failed:** $FAIL_COUNT"
add_to_report "- **Pass Rate:** ${PASS_RATE}%"
add_to_report ""

# Get pass criteria from registry
REQUIRED_PASS_RATE=80  # Default minimum

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Review Results${NC}"
echo -e "${BLUE}========================================${NC}"
echo -e "Total Checkpoints: $TOTAL_CHECKPOINTS"
echo -e "Passed: ${GREEN}$PASS_COUNT${NC}"
echo -e "Failed: ${RED}$FAIL_COUNT${NC}"
echo -e "Pass Rate: ${PASS_RATE}%"
echo ""

# Determine overall result
if [ $PASS_RATE -ge $REQUIRED_PASS_RATE ]; then
    echo -e "${GREEN}✓ GATE-PASS: APPROVED${NC}"
    add_to_report "## Overall Result"
    add_to_report ""
    add_to_report "**Status:** ✓ APPROVED"
    add_to_report ""
    add_to_report "The ${PHASE} phase has met the quality criteria and is approved to proceed."
    EXIT_CODE=0
else
    echo -e "${RED}✗ GATE-PASS: REJECTED${NC}"
    echo -e "${YELLOW}Required pass rate: ${REQUIRED_PASS_RATE}%${NC}"
    add_to_report "## Overall Result"
    add_to_report ""
    add_to_report "**Status:** ✗ REJECTED"
    add_to_report ""
    add_to_report "The ${PHASE} phase did not meet the minimum quality criteria (${REQUIRED_PASS_RATE}% required)."
    add_to_report ""
    add_to_report "### Required Actions"
    add_to_report ""
    add_to_report "Please address the failed checkpoints before proceeding."
    EXIT_CODE=1
fi

echo ""

# Save report if requested
if [ -n "$REPORT_FILE" ]; then
    echo -e "$REVIEW_REPORT" > "$REPORT_FILE"
    echo -e "${BLUE}Report saved to: $REPORT_FILE${NC}"
fi

# Show recommended next steps
add_to_report ""
add_to_report "## Recommended Next Steps"
add_to_report ""

case $PHASE in
    specification)
        add_to_report "1. Review and refine requirements with stakeholders"
        add_to_report "2. Validate spec with \`specify validate\`"
        add_to_report "3. Proceed to pseudocode phase: \`npx claude-flow sparc run pseudocode\`"
        ;;
    pseudocode)
        add_to_report "1. Review algorithm complexity and edge cases"
        add_to_report "2. Proceed to architecture phase"
        ;;
    architecture)
        add_to_report "1. Document architectural decisions in \`.agent-os/product/decisions.md\`"
        add_to_report "2. Proceed to implementation phase: \`npx claude-flow sparc tdd\`"
        ;;
    implementation)
        add_to_report "1. Ensure all tests pass"
        add_to_report "2. Run code review: \`$0 testing $TARGET_DIR\`"
        ;;
    testing)
        add_to_report "1. Improve test coverage to 90%+"
        add_to_report "2. Proceed to refinement phase"
        ;;
    refinement)
        add_to_report "1. Apply performance optimizations"
        add_to_report "2. Update documentation"
        add_to_report "3. Proceed to completion phase"
        ;;
    completion)
        add_to_report "1. Create pull request"
        add_to_report "2. Deploy to staging environment"
        add_to_report "3. Update product roadmap"
        ;;
esac

add_to_report ""
add_to_report "---"
add_to_report "*Generated by gate_pass_review.sh*"

echo -e "${BLUE}For detailed review, invoke primary reviewer: $PRIMARY_REVIEWER${NC}"
echo -e "${BLUE}For second opinion, invoke secondary reviewer: $SECONDARY_REVIEWER${NC}"

exit $EXIT_CODE
