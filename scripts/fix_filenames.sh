#!/bin/bash
# ABOUTME: Automated script to fix problematic filenames (spaces, commas, special chars)
# ABOUTME: Creates backup before making changes, provides rollback capability

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DRY_RUN=false
CREATE_BACKUP=true
BACKUP_DIR="backups/filename_fix_$(date +%Y%m%d_%H%M%S)"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --no-backup)
            CREATE_BACKUP=false
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--dry-run] [--no-backup]"
            exit 1
            ;;
    esac
done

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║       digitalmodel - Filename Fix Utility                 ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}⚠️  DRY RUN MODE - No changes will be made${NC}"
    echo ""
fi

# Function to sanitize filename
sanitize_filename() {
    local filename="$1"
    # Replace spaces with underscores
    filename="${filename// /_}"
    # Replace commas with underscores
    filename="${filename//,/_}"
    # Replace multiple underscores with single
    filename=$(echo "$filename" | sed 's/__*/_/g')
    echo "$filename"
}

# Count problematic files
echo -e "${BLUE}📊 Scanning for problematic files...${NC}"
FILES_WITH_SPACES=$(find src -name "* *" -type f 2>/dev/null | wc -l)
DIRS_WITH_SPACES=$(find src -name "* *" -type d 2>/dev/null | wc -l)
FILES_WITH_COMMAS=$(find src -name "*,*" -type f 2>/dev/null | wc -l)
DIRS_WITH_COMMAS=$(find src -name "*,*" -type d 2>/dev/null | wc -l)

TOTAL_FILES=$((FILES_WITH_SPACES + FILES_WITH_COMMAS))
TOTAL_DIRS=$((DIRS_WITH_SPACES + DIRS_WITH_COMMAS))
TOTAL_ITEMS=$((TOTAL_FILES + TOTAL_DIRS))

echo ""
echo -e "Files with spaces:  ${YELLOW}$FILES_WITH_SPACES${NC}"
echo -e "Dirs with spaces:   ${YELLOW}$DIRS_WITH_SPACES${NC}"
echo -e "Files with commas:  ${YELLOW}$FILES_WITH_COMMAS${NC}"
echo -e "Dirs with commas:   ${YELLOW}$DIRS_WITH_COMMAS${NC}"
echo -e "─────────────────────────────────"
echo -e "Total items to fix: ${RED}$TOTAL_ITEMS${NC}"
echo ""

if [ "$TOTAL_ITEMS" -eq 0 ]; then
    echo -e "${GREEN}✅ No problematic files found! Repository is clean.${NC}"
    exit 0
fi

# Create backup if requested
if [ "$CREATE_BACKUP" = true ] && [ "$DRY_RUN" = false ]; then
    echo -e "${BLUE}💾 Creating backup...${NC}"
    mkdir -p "$BACKUP_DIR"

    # Save list of files that will be changed
    {
        find src -name "* *" -o -name "*,*"
    } > "$BACKUP_DIR/files_to_rename.txt"

    echo -e "${GREEN}✓${NC} Backup created in: $BACKUP_DIR"
    echo ""
fi

# Show preview of changes
echo -e "${BLUE}📝 Preview of changes (first 10):${NC}"
echo ""

preview_count=0
{
    find src -depth -name "* *" -o -name "*,*"
} | while read -r item; do
    if [ $preview_count -lt 10 ]; then
        basename_old=$(basename "$item")
        basename_new=$(sanitize_filename "$basename_old")
        dirname_path=$(dirname "$item")

        if [ "$basename_old" != "$basename_new" ]; then
            echo -e "  ${RED}$basename_old${NC}"
            echo -e "  ${GREEN}→ $basename_new${NC}"
            echo ""
            ((preview_count++))
        fi
    fi
done

if [ "$TOTAL_ITEMS" -gt 10 ]; then
    echo -e "  ... and $((TOTAL_ITEMS - 10)) more items"
    echo ""
fi

# Ask for confirmation if not dry run
if [ "$DRY_RUN" = false ]; then
    echo -e "${YELLOW}⚠️  This will rename $TOTAL_ITEMS files and directories.${NC}"
    read -p "Proceed with renaming? (yes/no): " -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy][Ee][Ss]$ ]]; then
        echo -e "${YELLOW}Aborted by user.${NC}"
        exit 0
    fi
fi

# Perform renaming
echo -e "${BLUE}🔧 Renaming files and directories...${NC}"
echo ""

renamed_count=0
failed_count=0

# Rename files first, then directories (depth-first to avoid path issues)
{
    find src -depth -name "* *" -type f
    find src -depth -name "*,*" -type f
    find src -depth -name "* *" -type d
    find src -depth -name "*,*" -type d
} | while read -r item; do
    basename_old=$(basename "$item")
    basename_new=$(sanitize_filename "$basename_old")
    dirname_path=$(dirname "$item")
    new_path="$dirname_path/$basename_new"

    if [ "$basename_old" != "$basename_new" ]; then
        if [ "$DRY_RUN" = true ]; then
            echo -e "${BLUE}[DRY RUN]${NC} Would rename:"
            echo -e "  $item"
            echo -e "  → $new_path"
            ((renamed_count++))
        else
            if mv "$item" "$new_path" 2>/dev/null; then
                echo -e "${GREEN}✓${NC} Renamed: $basename_old → $basename_new"
                ((renamed_count++))
            else
                echo -e "${RED}✗${NC} Failed: $basename_old"
                ((failed_count++))
            fi
        fi
    fi
done

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}✅ Renaming complete!${NC}"
echo ""
echo -e "Successfully renamed: ${GREEN}$renamed_count${NC} items"
if [ $failed_count -gt 0 ]; then
    echo -e "Failed:               ${RED}$failed_count${NC} items"
fi
echo ""

if [ "$DRY_RUN" = false ] && [ $renamed_count -gt 0 ]; then
    echo -e "${YELLOW}📝 NEXT STEPS:${NC}"
    echo ""
    echo "1. Update imports in Python files:"
    echo "   - Use your IDE's refactoring tools (recommended)"
    echo "   - Or run: python scripts/update_imports_after_rename.py"
    echo ""
    echo "2. Run tests to verify nothing broke:"
    echo "   pytest tests/ -v"
    echo ""
    echo "3. If everything works, commit the changes:"
    echo "   git add -A"
    echo "   git commit -m \"Fix: Rename files with spaces/commas to snake_case\""
    echo ""
    echo "4. If you need to rollback:"
    echo "   - Restore from backup: $BACKUP_DIR"
    echo "   - Or use: git restore ."
    echo ""
fi

if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}💡 To apply changes, run without --dry-run flag${NC}"
    echo ""
fi

exit 0
