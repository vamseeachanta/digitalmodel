"""
Authentication service for JWT token management and user verification.
Provides secure authentication with session management and token validation.
"""

import logging
from datetime import datetime, timedelta
from typing import Optional, Dict, Any
from uuid import UUID, uuid4

import jwt
from passlib.context import CryptContext
from fastapi import HTTPException, status

from app.config import get_settings
from app.core.cache import get_cache_service

logger = logging.getLogger(__name__)
settings = get_settings()


class AuthService:
    """
    JWT-based authentication service with Redis session management.
    Provides secure token generation, validation, and user session tracking.
    """
    
    def __init__(self):
        """Initialize authentication service."""
        self.secret_key = settings.secret_key
        self.algorithm = settings.algorithm
        self.access_token_expire_minutes = settings.access_token_expire_minutes
        self.pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
        self.cache_service = get_cache_service()
    
    def verify_password(self, plain_password: str, hashed_password: str) -> bool:
        """Verify a plaintext password against its hash."""
        return self.pwd_context.verify(plain_password, hashed_password)
    
    def get_password_hash(self, password: str) -> str:
        """Generate password hash."""
        return self.pwd_context.hash(password)
    
    def create_access_token(self, user_id: str, user_data: Dict[str, Any] = None) -> str:
        """
        Create JWT access token with user information.
        
        Args:
            user_id: Unique user identifier
            user_data: Additional user data to include in token
            
        Returns:
            JWT access token string
        """
        to_encode = {
            "sub": user_id,
            "iat": datetime.utcnow(),
            "exp": datetime.utcnow() + timedelta(minutes=self.access_token_expire_minutes),
            "type": "access_token",
            "jti": str(uuid4())  # JWT ID for token tracking
        }
        
        # Add user data if provided
        if user_data:
            to_encode.update(user_data)
        
        try:
            encoded_jwt = jwt.encode(to_encode, self.secret_key, algorithm=self.algorithm)
            logger.debug(f"Created access token for user {user_id}")
            return encoded_jwt
            
        except Exception as e:
            logger.error(f"Error creating access token: {e}")
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail="Could not create access token"
            )
    
    async def verify_token(self, token: str) -> str:
        """
        Verify JWT token and extract user ID.
        
        Args:
            token: JWT token string
            
        Returns:
            User ID from token
            
        Raises:
            HTTPException: If token is invalid or expired
        """
        try:
            # Decode token
            payload = jwt.decode(token, self.secret_key, algorithms=[self.algorithm])
            
            # Extract user ID
            user_id: str = payload.get("sub")
            if user_id is None:
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Invalid token: missing user ID",
                    headers={"WWW-Authenticate": "Bearer"},
                )
            
            # Check token type
            token_type = payload.get("type")
            if token_type != "access_token":
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Invalid token type",
                    headers={"WWW-Authenticate": "Bearer"},
                )
            
            # Check if token is blacklisted
            jti = payload.get("jti")
            if jti and await self._is_token_blacklisted(jti):
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Token has been revoked",
                    headers={"WWW-Authenticate": "Bearer"},
                )
            
            # Update user activity (in background)
            if jti:
                await self._update_user_activity(user_id, jti)
            
            return user_id
            
        except jwt.ExpiredSignatureError:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Token has expired",
                headers={"WWW-Authenticate": "Bearer"},
            )
        except jwt.JWTError as e:
            logger.warning(f"JWT validation error: {e}")
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Could not validate credentials",
                headers={"WWW-Authenticate": "Bearer"},
            )
    
    async def revoke_token(self, token: str) -> bool:
        """
        Revoke (blacklist) a JWT token.
        
        Args:
            token: JWT token to revoke
            
        Returns:
            True if token was successfully revoked
        """
        try:
            # Decode token to get JTI and expiration
            payload = jwt.decode(token, self.secret_key, algorithms=[self.algorithm])
            jti = payload.get("jti")
            exp = payload.get("exp")
            
            if not jti:
                logger.warning("Cannot revoke token: missing JTI")
                return False
            
            # Calculate TTL based on token expiration
            exp_datetime = datetime.fromtimestamp(exp)
            ttl = int((exp_datetime - datetime.utcnow()).total_seconds())
            
            if ttl > 0:
                # Add to blacklist with TTL matching token expiration
                blacklist_key = f"blacklisted_token:{jti}"
                await self.cache_service.set(blacklist_key, True, ttl=ttl)
                
                logger.info(f"Token revoked: {jti}")
                return True
            else:
                logger.debug("Token already expired, no need to blacklist")
                return True
                
        except jwt.JWTError as e:
            logger.warning(f"Error revoking token: {e}")
            return False
        except Exception as e:
            logger.error(f"Unexpected error revoking token: {e}")
            return False
    
    async def _is_token_blacklisted(self, jti: str) -> bool:
        """Check if token JTI is blacklisted."""
        blacklist_key = f"blacklisted_token:{jti}"
        return await self.cache_service.exists(blacklist_key)
    
    async def _update_user_activity(self, user_id: str, jti: str) -> None:
        """Update user activity timestamp and active tokens."""
        try:
            # Update last activity
            activity_key = f"user_activity:{user_id}"
            await self.cache_service.set(
                activity_key,
                {
                    "last_seen": datetime.utcnow().isoformat(),
                    "current_token": jti
                },
                ttl=self.access_token_expire_minutes * 60
            )
            
        except Exception as e:
            logger.error(f"Error updating user activity: {e}")
    
    async def get_user_sessions(self, user_id: str) -> Dict[str, Any]:
        """
        Get active sessions for a user.
        
        Args:
            user_id: User identifier
            
        Returns:
            Dictionary with session information
        """
        try:
            activity_key = f"user_activity:{user_id}"
            activity_data = await self.cache_service.get(activity_key)
            
            if not activity_data:
                return {
                    "user_id": user_id,
                    "active_sessions": 0,
                    "last_seen": None
                }
            
            return {
                "user_id": user_id,
                "active_sessions": 1,  # Simplified - could track multiple tokens
                "last_seen": activity_data.get("last_seen"),
                "current_token": activity_data.get("current_token")
            }
            
        except Exception as e:
            logger.error(f"Error getting user sessions: {e}")
            return {
                "user_id": user_id,
                "active_sessions": 0,
                "last_seen": None,
                "error": str(e)
            }
    
    async def revoke_user_sessions(self, user_id: str) -> int:
        """
        Revoke all active sessions for a user.
        
        Args:
            user_id: User identifier
            
        Returns:
            Number of sessions revoked
        """
        try:
            # Get user activity to find current token
            activity_key = f"user_activity:{user_id}"
            activity_data = await self.cache_service.get(activity_key)
            
            revoked_count = 0
            
            if activity_data and activity_data.get("current_token"):
                # Blacklist current token
                jti = activity_data["current_token"]
                blacklist_key = f"blacklisted_token:{jti}"
                await self.cache_service.set(
                    blacklist_key,
                    True,
                    ttl=self.access_token_expire_minutes * 60
                )
                revoked_count += 1
            
            # Clear user activity
            await self.cache_service.delete(activity_key)
            
            logger.info(f"Revoked {revoked_count} sessions for user {user_id}")
            return revoked_count
            
        except Exception as e:
            logger.error(f"Error revoking user sessions: {e}")
            return 0
    
    def decode_token_payload(self, token: str) -> Dict[str, Any]:
        """
        Decode token payload without verification (for debugging/monitoring).
        
        Args:
            token: JWT token string
            
        Returns:
            Token payload dictionary
        """
        try:
            # Decode without verification
            payload = jwt.decode(token, options={"verify_signature": False})
            return payload
        except Exception as e:
            logger.error(f"Error decoding token payload: {e}")
            return {}
    
    async def create_refresh_token(self, user_id: str) -> str:
        """
        Create refresh token for extended sessions.
        
        Args:
            user_id: User identifier
            
        Returns:
            Refresh token string
        """
        to_encode = {
            "sub": user_id,
            "iat": datetime.utcnow(),
            "exp": datetime.utcnow() + timedelta(days=30),  # 30 day expiration
            "type": "refresh_token",
            "jti": str(uuid4())
        }
        
        try:
            encoded_jwt = jwt.encode(to_encode, self.secret_key, algorithm=self.algorithm)
            
            # Store refresh token in cache for validation
            refresh_key = f"refresh_token:{user_id}:{to_encode['jti']}"
            await self.cache_service.set(
                refresh_key,
                {"issued_at": datetime.utcnow().isoformat()},
                ttl=30 * 24 * 60 * 60  # 30 days
            )
            
            logger.debug(f"Created refresh token for user {user_id}")
            return encoded_jwt
            
        except Exception as e:
            logger.error(f"Error creating refresh token: {e}")
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail="Could not create refresh token"
            )
    
    async def refresh_access_token(self, refresh_token: str) -> str:
        """
        Create new access token from refresh token.
        
        Args:
            refresh_token: Valid refresh token
            
        Returns:
            New access token
        """
        try:
            # Decode refresh token
            payload = jwt.decode(refresh_token, self.secret_key, algorithms=[self.algorithm])
            
            # Verify token type
            if payload.get("type") != "refresh_token":
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Invalid token type"
                )
            
            user_id = payload.get("sub")
            jti = payload.get("jti")
            
            if not user_id or not jti:
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Invalid refresh token"
                )
            
            # Verify refresh token exists in cache
            refresh_key = f"refresh_token:{user_id}:{jti}"
            if not await self.cache_service.exists(refresh_key):
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="Refresh token has been revoked"
                )
            
            # Create new access token
            new_access_token = self.create_access_token(user_id)
            
            logger.debug(f"Refreshed access token for user {user_id}")
            return new_access_token
            
        except jwt.ExpiredSignatureError:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Refresh token has expired"
            )
        except jwt.JWTError as e:
            logger.warning(f"Refresh token validation error: {e}")
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Invalid refresh token"
            )
    
    async def get_auth_stats(self) -> Dict[str, Any]:
        """Get authentication service statistics."""
        try:
            # Get cache stats
            cache_stats = await self.cache_service.get_stats()
            
            # Count active users (simplified - based on activity keys)
            # This is a basic implementation - could be more sophisticated
            active_users = 0
            blacklisted_tokens = 0
            
            return {
                "service_status": "operational",
                "cache_status": cache_stats.get("status", "unknown"),
                "active_users": active_users,
                "blacklisted_tokens": blacklisted_tokens,
                "token_expire_minutes": self.access_token_expire_minutes,
                "algorithm": self.algorithm
            }
            
        except Exception as e:
            logger.error(f"Error getting auth stats: {e}")
            return {
                "service_status": "error",
                "error": str(e)
            }


# Global auth service instance
_auth_service = None


def get_auth_service() -> AuthService:
    """Get global authentication service instance."""
    global _auth_service
    if _auth_service is None:
        _auth_service = AuthService()
    return _auth_service


async def verify_token(token: str) -> str:
    """
    Convenience function for token verification.
    
    Args:
        token: JWT token string
        
    Returns:
        User ID from token
    """
    auth_service = get_auth_service()
    return await auth_service.verify_token(token)


async def create_access_token(user_id: str, user_data: Dict[str, Any] = None) -> str:
    """
    Convenience function for token creation.
    
    Args:
        user_id: User identifier
        user_data: Additional user data
        
    Returns:
        JWT access token
    """
    auth_service = get_auth_service()
    return auth_service.create_access_token(user_id, user_data)