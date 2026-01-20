# Path Conventions for AI Agents

## CRITICAL: Path Format Requirements

### Windows Path Handling in Git Bash/MinGW
When working in Git Bash or MinGW environment on Windows:

#### ALWAYS USE Unix-style paths:
- ✅ `/d/github/digitalmodel/` 
- ✅ `/c/Users/username/`
- ❌ `D:\github\digitalmodel\`
- ❌ `D:/github/digitalmodel/`

#### Path Conversion Rules:
1. **Drive letters**: `D:` → `/d`
2. **Backslashes**: `\` → `/`
3. **Lowercase drive**: Always use lowercase drive letters

### Common Path Mistakes to Avoid

#### 1. Direct Windows Path in Bash Commands
```bash
# WRONG
cd D:\github\digitalmodel\specs

# CORRECT
cd /d/github/digitalmodel/specs
```

#### 2. Mixed Path Styles
```bash
# WRONG
cd D:/github/digitalmodel/specs

# CORRECT
cd /d/github/digitalmodel/specs
```

#### 3. Forgetting Path Context
```bash
# WRONG (assumes Windows CMD)
dir D:\github\*

# CORRECT (Git Bash)
ls /d/github/*
```

### Tool-Specific Path Requirements

#### Read/Write Tools (Python/System)
- Can use Windows paths: `D:\github\digitalmodel\file.py`
- Preferred for file operations in Write/Read tools

#### Bash Commands
- MUST use Unix paths: `/d/github/digitalmodel/`
- Required for cd, ls, find, grep, etc.

#### Python Scripts
- Can handle both formats
- Use `Path` from pathlib for consistency
- Example:
  ```python
  from pathlib import Path
  base_path = Path(__file__).parent  # Works on all platforms
  ```

### Quick Reference

| Context | Format | Example |
|---------|--------|---------|
| Bash cd | Unix | `/d/github/digitalmodel/` |
| Bash ls | Unix | `/d/github/digitalmodel/*.py` |
| Python read | Windows OK | `D:\github\digitalmodel\file.py` |
| Write tool | Windows OK | `D:\github\digitalmodel\output.txt` |
| Git operations | Unix | `/d/github/digitalmodel/.git` |

### Environment Variables
When checking paths:
```bash
# Use pwd to verify current directory
pwd  # Returns: /d/github/digitalmodel

# NOT
echo %CD%  # This is Windows CMD syntax
```

### Debugging Path Issues
If a command fails with "No such file or directory":
1. Check path format (Unix vs Windows)
2. Verify with `pwd` command
3. Use `ls` to confirm directory exists
4. Convert Windows → Unix format if in Bash

### Memory Aid
**In Bash/Git environment:**
- Forward slashes ONLY
- Lowercase drive letters
- Unix-style paths ALWAYS

This prevents common errors like:
- "No such file or directory" 
- "cd: D:githubdigitalmodel: No such file or directory"
- Command syntax errors