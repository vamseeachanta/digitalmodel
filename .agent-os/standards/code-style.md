# Code Style Guide

> Version: 1.0.0
> Last Updated: 2025-08-06

## General Formatting

### Indentation
- Use 2 spaces for indentation (never tabs)
- Maintain consistent indentation throughout files
- Align nested structures for readability

### Naming Conventions
- **Methods and Variables**: Use snake_case (e.g., `user_profile`, `calculate_total`)
- **Classes and Modules**: Use PascalCase (e.g., `UserProfile`, `PaymentProcessor`)
- **Constants**: Use UPPER_SNAKE_CASE (e.g., `MAX_RETRY_COUNT`)

### String Formatting
- Use single quotes for strings: `'Hello World'`
- Use double quotes only when interpolation is needed
- Use template literals for multi-line strings or complex interpolation

## HTML/Template Formatting

### Structure Rules
- Use 2 spaces for indentation
- Place nested elements on new lines with proper indentation
- Content between tags should be on its own line when multi-line

### Attribute Formatting
- Place each HTML attribute on its own line
- Align attributes vertically
- Keep the closing `>` on the same line as the last attribute

## Tailwind CSS preferences

### Multi-line CSS classes in markup
- Use a unique multi-line formatting style when writing Tailwind CSS classes in HTML markup and ERB tags
- The top-most line should be the smallest size (no responsive prefix)
- Each line below it should be the next responsive size up
- Each line of CSS classes should be aligned vertically
- Focus and hover classes should be on their own additional dedicated lines

## Code Comments

### When to Comment
- Add brief comments above non-obvious business logic
- Document complex algorithms or calculations
- Explain the "why" behind implementation choices

### Comment Maintenance
- Never remove existing comments unless removing the associated code
- Update comments when modifying code to maintain accuracy
- Keep comments concise and relevant
