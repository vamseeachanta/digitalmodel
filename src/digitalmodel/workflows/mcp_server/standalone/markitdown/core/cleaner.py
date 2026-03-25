"""
Markdown cleaner for removing special characters and fixing formulas
"""

import re
from typing import Dict, Optional


class MarkdownCleaner:
    """Clean markdown content from special characters and fix formulas"""
    
    # Common problematic Unicode characters and their replacements
    UNICODE_REPLACEMENTS = {
        # Invisible characters
        '\u200b': '',  # Zero-width space
        '\u200c': '',  # Zero-width non-joiner
        '\u200d': '',  # Zero-width joiner
        '\ufeff': '',  # Zero-width no-break space
        '\u2060': '',  # Word joiner
        '\xa0': ' ',   # Non-breaking space
        '\u2009': ' ',  # Thin space
        '\u202f': ' ',  # Narrow no-break space
        '\u00a0': ' ',  # Another non-breaking space
        
        # Special characters that cause rendering issues
        'Â': '',  # Common Word artifact
        'â': '',  # Another Word artifact
        '​': '',  # Zero-width space (different encoding)
        
        # Smart quotes and punctuation
        ''': "'",
        ''': "'",
        '"': '"',
        '"': '"',
        '–': '-',   # En dash
        '—': '--',  # Em dash
        '…': '...',  # Ellipsis
        
        # Math symbols (optional replacement)
        '×': 'x',   # Multiplication
        '÷': '/',   # Division
        '≤': '<=',  # Less than or equal
        '≥': '>=',  # Greater than or equal
        '≠': '!=',  # Not equal
        '±': '+/-', # Plus minus
        '∞': 'inf', # Infinity
        '√': 'sqrt', # Square root
        
        # Greek letters (optional replacement)
        'Δ': 'Delta',
        '∆': 'Delta',
        'δ': 'delta',
        'σ': 'sigma',
        'Σ': 'Sum',
        'π': 'pi',
        'θ': 'theta',
        'α': 'alpha',
        'β': 'beta',
        'γ': 'gamma',
        'μ': 'mu',
        'λ': 'lambda',
        'ρ': 'rho',
        
        # Superscripts and subscripts
        '²': '^2',
        '³': '^3',
        '¹': '^1',
        '⁰': '^0',
        '⁴': '^4',
        '⁵': '^5',
        '⁶': '^6',
        '⁷': '^7',
        '⁸': '^8',
        '⁹': '^9',
        
        # Other symbols
        '·': '*',   # Middle dot
        '•': '*',   # Bullet
        '°': ' degrees',  # Degree symbol
        '©': '(c)',  # Copyright
        '®': '(R)',  # Registered
        '™': '(TM)', # Trademark
        '€': 'EUR',  # Euro
        '£': 'GBP',  # Pound
        '¥': 'JPY',  # Yen
        '§': 'Section',  # Section symbol
    }
    
    @staticmethod
    def clean_special_characters(
        content: str,
        remove_unicode: bool = True,
        fix_quotes: bool = True,
        fix_math_symbols: bool = True,
        fix_greek_letters: bool = False,
        pure_ascii: bool = False
    ) -> str:
        """
        Remove special characters from markdown content
        
        Args:
            content: The markdown content to clean
            remove_unicode: Remove invisible Unicode characters
            fix_quotes: Replace smart quotes with regular quotes
            fix_math_symbols: Replace math symbols with ASCII equivalents
            fix_greek_letters: Replace Greek letters with names
            pure_ascii: Convert to pure ASCII only (most aggressive)
            
        Returns:
            Cleaned markdown content
        """
        if pure_ascii:
            # Most aggressive cleaning - only ASCII characters
            return MarkdownCleaner._to_pure_ascii(content)
        
        # Apply selected replacements
        replacements = {}
        
        if remove_unicode:
            # Add invisible characters
            for char, replacement in MarkdownCleaner.UNICODE_REPLACEMENTS.items():
                try:
                    if len(char) == 1 and (ord(char) < 32 or char in ['\u200b', '\u200c', '\u200d', '\ufeff', '\u2060', '\xa0', '\u2009', '\u202f', 'Â', 'â', '​']):
                        replacements[char] = replacement
                except:
                    continue  # Skip problematic characters
        
        if fix_quotes:
            # Add quote replacements
            quote_chars = [''', ''', '"', '"', '–', '—', '…']
            for char in quote_chars:
                if char in MarkdownCleaner.UNICODE_REPLACEMENTS and len(char) == 1:
                    replacements[char] = MarkdownCleaner.UNICODE_REPLACEMENTS[char]
        
        if fix_math_symbols:
            # Add math symbol replacements
            math_chars = ['×', '÷', '≤', '≥', '≠', '±', '∞', '√', '²', '³', '¹', '⁰', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹']
            for char in math_chars:
                if char in MarkdownCleaner.UNICODE_REPLACEMENTS and len(char) == 1:
                    replacements[char] = MarkdownCleaner.UNICODE_REPLACEMENTS[char]
        
        if fix_greek_letters:
            # Add Greek letter replacements
            greek_chars = ['Δ', '∆', 'δ', 'σ', 'Σ', 'π', 'θ', 'α', 'β', 'γ', 'μ', 'λ', 'ρ']
            for char in greek_chars:
                if char in MarkdownCleaner.UNICODE_REPLACEMENTS and len(char) == 1:
                    replacements[char] = MarkdownCleaner.UNICODE_REPLACEMENTS[char]
        
        # Apply replacements
        for old, new in replacements.items():
            content = content.replace(old, new)
        
        return content
    
    @staticmethod
    def fix_latex_formulas(content: str, fix_underscores: bool = True, fix_delimiters: bool = True) -> str:
        """
        Fix LaTeX formula issues in markdown
        
        Args:
            content: The markdown content
            fix_underscores: Fix escaped underscores in formulas
            fix_delimiters: Ensure proper formula delimiters
            
        Returns:
            Content with fixed formulas
        """
        if fix_underscores:
            # Fix escaped underscores in math mode
            # Match LaTeX blocks and fix underscores
            def fix_math_underscores(match):
                math_content = match.group(1)
                # Remove backslashes before underscores
                math_content = math_content.replace('\\_', '_')
                return f'$${math_content}$$'
            
            # Fix display math blocks
            content = re.sub(r'\$\$(.*?)\$\$', fix_math_underscores, content, flags=re.DOTALL)
            
            # Fix inline math
            def fix_inline_math(match):
                math_content = match.group(1)
                math_content = math_content.replace('\\_', '_')
                return f'${math_content}$'
            
            content = re.sub(r'\$([^$\n]+)\$', fix_inline_math, content)
        
        if fix_delimiters:
            # Ensure proper spacing around formula delimiters
            content = re.sub(r'(\$\$)\s*([^$]+?)\s*(\$\$)', r'\1\2\3', content)
            content = re.sub(r'(\$)\s*([^$\n]+?)\s*(\$)', r'\1\2\3', content)
        
        return content
    
    @staticmethod
    def _to_pure_ascii(content: str) -> str:
        """
        Convert content to pure ASCII only
        
        Args:
            content: The content to convert
            
        Returns:
            Pure ASCII content
        """
        ascii_chars = []
        for char in content:
            try:
                code = ord(char) if len(char) == 1 else 0
            except:
                continue  # Skip problematic characters
                
            if 32 <= code <= 126:  # Printable ASCII
                ascii_chars.append(char)
            elif code == 10:  # Newline
                ascii_chars.append('\n')
            elif code == 9:  # Tab
                ascii_chars.append('    ')  # Convert to spaces
            elif code == 13:  # Carriage return
                continue  # Skip
            # All other characters are skipped
        
        return ''.join(ascii_chars)
    
    @staticmethod
    def clean_formatting(content: str) -> str:
        """
        Clean up formatting issues
        
        Args:
            content: The markdown content
            
        Returns:
            Content with cleaned formatting
        """
        # Remove multiple spaces (except at line beginning)
        lines = content.split('\n')
        cleaned_lines = []
        
        for line in lines:
            # Remove trailing spaces
            line = line.rstrip()
            
            # Fix multiple spaces in content (preserve indentation)
            if line.strip():
                indent = len(line) - len(line.lstrip())
                rest = line.lstrip()
                rest = re.sub(r'\s+', ' ', rest)
                line = ' ' * indent + rest
            
            cleaned_lines.append(line)
        
        content = '\n'.join(cleaned_lines)
        
        # Remove multiple blank lines
        content = re.sub(r'\n{3,}', '\n\n', content)
        
        return content
    
    @staticmethod
    def full_clean(
        content: str,
        remove_unicode: bool = True,
        fix_quotes: bool = True,
        fix_math_symbols: bool = True,
        fix_greek_letters: bool = False,
        pure_ascii: bool = False,
        fix_latex: bool = True,
        clean_format: bool = True
    ) -> str:
        """
        Perform full cleaning of markdown content
        
        Args:
            content: The markdown content to clean
            remove_unicode: Remove invisible Unicode characters
            fix_quotes: Replace smart quotes with regular quotes
            fix_math_symbols: Replace math symbols with ASCII equivalents
            fix_greek_letters: Replace Greek letters with names
            pure_ascii: Convert to pure ASCII only
            fix_latex: Fix LaTeX formula issues
            clean_format: Clean up formatting issues
            
        Returns:
            Fully cleaned markdown content
        """
        # Clean special characters
        content = MarkdownCleaner.clean_special_characters(
            content,
            remove_unicode=remove_unicode,
            fix_quotes=fix_quotes,
            fix_math_symbols=fix_math_symbols,
            fix_greek_letters=fix_greek_letters,
            pure_ascii=pure_ascii
        )
        
        # Fix LaTeX formulas if not pure ASCII
        if fix_latex and not pure_ascii:
            content = MarkdownCleaner.fix_latex_formulas(content)
        
        # Clean formatting
        if clean_format:
            content = MarkdownCleaner.clean_formatting(content)
        
        return content