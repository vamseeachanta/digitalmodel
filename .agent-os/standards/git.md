# Git Workflow and Conventions

## Branch Strategy

### Branch Types
- `master` or `main` - Production-ready code
- `develop` - Integration branch for features
- `feature/<issue-number>_<description>` - New features
- `bugfix/<issue-number>_<description>` - Bug fixes
- `hotfix/<issue-number>_<description>` - Critical production fixes
- `release/<version>` - Release preparation

### Branch Naming Examples
- `feature/82_parallel_process`
- `bugfix/101_fix_memory_leak`
- `hotfix/99_critical_calculation_error`

## Commit Messages

### Format
```
<type>: <subject>

<body>

<footer>
```

### Types
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Test additions or modifications
- `chore`: Build process or auxiliary tool changes
- `perf`: Performance improvements

### Examples
```
feat: Add parallel processing for OrcaFlex post-processing

- Implement ProcessPoolExecutor for concurrent file processing
- Add configurable worker pool settings
- Maintain backward compatibility with sequential processing

Closes #82
```

```
fix: Correct von Mises stress calculation for edge cases

Handle zero stress components without division by zero.
Add validation for negative input values.

Fixes #105
```

## Pull Request Process

### PR Title Format
Same as commit message format: `<type>: <description>`

### PR Description Template
```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Testing
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Manual testing completed

## Checklist
- [ ] Code follows project style guidelines
- [ ] Self-review completed
- [ ] Comments added for complex sections
- [ ] Documentation updated
- [ ] No new warnings generated
```

## Code Review Guidelines

### For Authors
1. Keep PRs focused and small
2. Provide context in PR description
3. Respond to all review comments
4. Update based on feedback
5. Ensure CI passes before review

### For Reviewers
1. Check functionality matches requirements
2. Verify tests are adequate
3. Ensure code follows standards
4. Look for potential performance issues
5. Suggest improvements constructively

## Git Commands Reference

### Feature Development
```bash
# Create feature branch
git checkout -b feature/123_new_feature

# Regular commits during development
git add .
git commit -m "feat: Implement calculation module"

# Update from main branch
git fetch origin
git rebase origin/master

# Push feature branch
git push -u origin feature/123_new_feature
```

### Fixing Conflicts
```bash
# Update your branch
git fetch origin
git rebase origin/master

# If conflicts occur
# Fix conflicts in files
git add <fixed-files>
git rebase --continue
```

### Cleaning Up
```bash
# Delete local branch
git branch -d feature/123_new_feature

# Delete remote branch
git push origin --delete feature/123_new_feature

# Prune deleted remote branches
git remote prune origin
```

## Best Practices

### Commit Practices
1. Make atomic commits (one logical change)
2. Commit early and often
3. Write meaningful commit messages
4. Don't commit commented-out code
5. Review changes before committing

### Merge vs Rebase
- Use rebase for feature branches to maintain linear history
- Use merge for release branches
- Never rebase public branches

### Security
1. Never commit secrets or credentials
2. Use `.gitignore` for sensitive files
3. Review diff for accidental inclusions
4. Use environment variables for config

### File Management
1. Don't commit generated files
2. Keep binary files minimal
3. Use Git LFS for large files if needed
4. Maintain clean `.gitignore`

## Git Hooks (Optional)

### Pre-commit Hook Example
```bash
#!/bin/sh
# Run tests before commit
python -m pytest tests/unit/

# Check code style
python -m flake8 src/

# Block commit if tests fail
if [ $? -ne 0 ]; then
    echo "Tests must pass before commit!"
    exit 1
fi
```