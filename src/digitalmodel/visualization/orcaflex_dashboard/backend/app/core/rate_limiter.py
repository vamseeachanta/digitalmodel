"""
Rate limiting decorator for API endpoints.
Provides protection against abuse with configurable limits and Redis-backed counters.
"""

import logging
import time
from functools import wraps
from typing import Callable, Dict, Any
from datetime import datetime

from fastapi import HTTPException, Request
from starlette.responses import Response

from app.core.cache import get_cache_service

logger = logging.getLogger(__name__)


class RateLimitExceeded(HTTPException):
    """Custom exception for rate limit exceeded."""
    
    def __init__(self, detail: str, retry_after: int = None):
        super().__init__(status_code=429, detail=detail)
        self.retry_after = retry_after


class RateLimiter:
    """
    Redis-backed rate limiter with sliding window implementation.
    Provides accurate rate limiting with minimal memory overhead.
    """
    
    def __init__(self, max_calls: int, time_window: int, key_func: Callable[[Request], str] = None):
        """
        Initialize rate limiter.
        
        Args:
            max_calls: Maximum calls allowed in time window
            time_window: Time window in seconds
            key_func: Function to generate rate limit key from request
        """
        self.max_calls = max_calls
        self.time_window = time_window
        self.key_func = key_func or self._default_key_func
        self.cache_service = get_cache_service()
    
    def _default_key_func(self, request: Request) -> str:
        """Default key function using client IP and endpoint."""
        client_ip = self._get_client_ip(request)
        endpoint = f"{request.method}:{request.url.path}"
        return f"rate_limit:{client_ip}:{endpoint}"
    
    def _get_client_ip(self, request: Request) -> str:
        """Extract client IP from request headers."""
        # Check for forwarded IP headers (for load balancers/proxies)
        forwarded_for = request.headers.get("X-Forwarded-For")
        if forwarded_for:
            # Take the first IP in case of multiple IPs
            return forwarded_for.split(",")[0].strip()
        
        real_ip = request.headers.get("X-Real-IP")
        if real_ip:
            return real_ip
        
        # Fall back to direct client IP
        client_host = getattr(request.client, "host", "unknown")
        return client_host
    
    async def is_allowed(self, request: Request) -> tuple[bool, Dict[str, Any]]:
        """
        Check if request is allowed based on rate limits.
        Returns (allowed, info_dict) with rate limit information.
        """
        key = self.key_func(request)
        current_time = int(time.time())
        
        # Use sliding window with Redis sorted sets for accuracy
        window_start = current_time - self.time_window
        
        try:
            if not self.cache_service.enabled:
                # Rate limiting disabled if Redis unavailable
                return True, {
                    "allowed": True,
                    "limit": self.max_calls,
                    "remaining": self.max_calls,
                    "reset_time": current_time + self.time_window,
                    "retry_after": None
                }
            
            # Connect to Redis if needed
            if not self.cache_service.redis:
                await self.cache_service.connect()
            
            redis = self.cache_service.redis
            
            # Use pipeline for atomic operations
            pipe = redis.pipeline()
            
            # Remove expired entries from sorted set
            pipe.zremrangebyscore(key, 0, window_start)
            
            # Count current requests in window
            pipe.zcard(key)
            
            # Add current request
            pipe.zadd(key, {str(current_time): current_time})
            
            # Set expiration for the key
            pipe.expire(key, self.time_window + 1)
            
            results = await pipe.execute()
            request_count = results[1] + 1  # Include current request
            
            allowed = request_count <= self.max_calls
            remaining = max(0, self.max_calls - request_count)
            
            # Calculate reset time (when oldest request in window expires)
            reset_time = current_time + self.time_window
            retry_after = None
            
            if not allowed:
                # Get timestamp of oldest request to calculate retry_after
                oldest_requests = await redis.zrange(key, 0, 0, withscores=True)
                if oldest_requests:
                    oldest_time = int(oldest_requests[0][1])
                    retry_after = oldest_time + self.time_window - current_time
                    retry_after = max(1, retry_after)  # At least 1 second
                
                # Remove the current request since it's not allowed
                await redis.zrem(key, str(current_time))
            
            return allowed, {
                "allowed": allowed,
                "limit": self.max_calls,
                "remaining": remaining,
                "reset_time": reset_time,
                "retry_after": retry_after,
                "window": self.time_window,
                "current_count": request_count
            }
            
        except Exception as e:
            logger.error(f"Rate limiting error for key {key}: {e}")
            # Allow request if rate limiting fails
            return True, {
                "allowed": True,
                "limit": self.max_calls,
                "remaining": self.max_calls,
                "reset_time": current_time + self.time_window,
                "retry_after": None,
                "error": "rate_limiting_unavailable"
            }
    
    def __call__(self, func: Callable) -> Callable:
        """Decorator function for rate limiting."""
        @wraps(func)
        async def wrapper(*args, **kwargs):
            # Extract request from function arguments
            request = None
            for arg in args:
                if isinstance(arg, Request):
                    request = arg
                    break
            
            # Try to find request in kwargs
            if not request:
                request = kwargs.get("request")
            
            if not request:
                logger.warning("Rate limiter: Could not find Request object in function arguments")
                # Allow request if we can't find Request object
                return await func(*args, **kwargs)
            
            # Check rate limit
            allowed, info = await self.is_allowed(request)
            
            if not allowed:
                error_detail = (f"Rate limit exceeded. "
                               f"Maximum {self.max_calls} requests per {self.time_window} seconds. "
                               f"Try again in {info['retry_after']} seconds.")
                
                logger.warning(f"Rate limit exceeded for {self._get_client_ip(request)} "
                              f"on {request.method} {request.url.path}")
                
                raise RateLimitExceeded(error_detail, info['retry_after'])
            
            # Add rate limit headers to response
            result = await func(*args, **kwargs)
            
            # Add headers if result is a Response object
            if isinstance(result, Response):
                result.headers["X-RateLimit-Limit"] = str(info['limit'])
                result.headers["X-RateLimit-Remaining"] = str(info['remaining'])
                result.headers["X-RateLimit-Reset"] = str(info['reset_time'])
                result.headers["X-RateLimit-Window"] = str(info['window'])
            
            return result
            
        return wrapper


def rate_limit(max_calls: int, time_window: int, key_func: Callable[[Request], str] = None):
    """
    Rate limiting decorator factory.
    
    Args:
        max_calls: Maximum calls allowed in time window
        time_window: Time window in seconds
        key_func: Optional custom key function
    
    Example:
        @rate_limit(max_calls=100, time_window=60)
        async def my_endpoint():
            pass
    """
    return RateLimiter(max_calls, time_window, key_func)


class UserBasedRateLimiter(RateLimiter):
    """Rate limiter based on authenticated user ID instead of IP."""
    
    def _default_key_func(self, request: Request) -> str:
        """Generate key using user ID from JWT token."""
        # Try to extract user ID from Authorization header
        auth_header = request.headers.get("Authorization")
        user_id = "anonymous"
        
        if auth_header and auth_header.startswith("Bearer "):
            try:
                # This is a simplified version - in practice, you'd decode the JWT
                # For now, use IP as fallback
                user_id = self._get_client_ip(request)
            except Exception:
                user_id = self._get_client_ip(request)
        else:
            user_id = self._get_client_ip(request)
        
        endpoint = f"{request.method}:{request.url.path}"
        return f"rate_limit:user:{user_id}:{endpoint}"


def user_rate_limit(max_calls: int, time_window: int, key_func: Callable[[Request], str] = None):
    """
    User-based rate limiting decorator factory.
    
    Args:
        max_calls: Maximum calls allowed in time window
        time_window: Time window in seconds
        key_func: Optional custom key function
    """
    return UserBasedRateLimiter(max_calls, time_window, key_func)


class AdaptiveRateLimiter(RateLimiter):
    """
    Adaptive rate limiter that adjusts limits based on system load.
    Reduces limits during high load periods.
    """
    
    def __init__(self, max_calls: int, time_window: int, key_func: Callable[[Request], str] = None,
                 load_threshold: float = 0.8, min_calls: int = None):
        """
        Initialize adaptive rate limiter.
        
        Args:
            max_calls: Maximum calls allowed in time window (during normal load)
            time_window: Time window in seconds
            key_func: Function to generate rate limit key from request
            load_threshold: System load threshold (0.0-1.0) above which to reduce limits
            min_calls: Minimum calls allowed even under high load
        """
        super().__init__(max_calls, time_window, key_func)
        self.base_max_calls = max_calls
        self.load_threshold = load_threshold
        self.min_calls = min_calls or max(1, max_calls // 4)  # Default to 25% of max
    
    async def _get_system_load(self) -> float:
        """
        Get current system load indicator.
        This could be based on Redis memory usage, CPU, active connections, etc.
        """
        try:
            if not self.cache_service.enabled or not self.cache_service.redis:
                return 0.0
            
            info = await self.cache_service.redis.info()
            used_memory = info.get("used_memory", 0)
            max_memory = info.get("maxmemory", 0)
            
            if max_memory > 0:
                return used_memory / max_memory
            
            # Use connected clients as load indicator if no memory info
            connected_clients = info.get("connected_clients", 0)
            return min(1.0, connected_clients / 100)  # Normalize to 0-1
            
        except Exception as e:
            logger.error(f"Error getting system load: {e}")
            return 0.0
    
    async def is_allowed(self, request: Request) -> tuple[bool, Dict[str, Any]]:
        """Check if request is allowed with adaptive limits."""
        # Get current system load
        system_load = await self._get_system_load()
        
        # Adjust max_calls based on system load
        if system_load > self.load_threshold:
            # Reduce limits during high load
            load_factor = max(0.1, 1.0 - ((system_load - self.load_threshold) / (1.0 - self.load_threshold)))
            adjusted_max_calls = max(self.min_calls, int(self.base_max_calls * load_factor))
        else:
            adjusted_max_calls = self.base_max_calls
        
        # Temporarily adjust max_calls for this check
        original_max_calls = self.max_calls
        self.max_calls = adjusted_max_calls
        
        try:
            allowed, info = await super().is_allowed(request)
            
            # Add load information to response
            info.update({
                "base_limit": self.base_max_calls,
                "adjusted_limit": adjusted_max_calls,
                "system_load": system_load,
                "load_threshold": self.load_threshold
            })
            
            return allowed, info
            
        finally:
            # Restore original max_calls
            self.max_calls = original_max_calls


def adaptive_rate_limit(max_calls: int, time_window: int, load_threshold: float = 0.8,
                       min_calls: int = None, key_func: Callable[[Request], str] = None):
    """
    Adaptive rate limiting decorator factory.
    
    Args:
        max_calls: Maximum calls allowed in time window (during normal load)
        time_window: Time window in seconds
        load_threshold: System load threshold above which to reduce limits
        min_calls: Minimum calls allowed even under high load
        key_func: Optional custom key function
    """
    return AdaptiveRateLimiter(max_calls, time_window, key_func, load_threshold, min_calls)


async def get_rate_limit_status(request: Request, max_calls: int, time_window: int) -> Dict[str, Any]:
    """
    Get current rate limit status for a request without checking/incrementing counters.
    Useful for monitoring and debugging.
    """
    rate_limiter = RateLimiter(max_calls, time_window)
    key = rate_limiter.key_func(request)
    current_time = int(time.time())
    window_start = current_time - time_window
    
    try:
        if not rate_limiter.cache_service.enabled:
            return {"status": "disabled", "reason": "redis_unavailable"}
        
        if not rate_limiter.cache_service.redis:
            await rate_limiter.cache_service.connect()
        
        redis = rate_limiter.cache_service.redis
        
        # Count requests in current window without modifying
        await redis.zremrangebyscore(key, 0, window_start)
        current_count = await redis.zcard(key)
        
        remaining = max(0, max_calls - current_count)
        reset_time = current_time + time_window
        
        return {
            "status": "active",
            "limit": max_calls,
            "remaining": remaining,
            "current_count": current_count,
            "reset_time": reset_time,
            "window": time_window,
            "key": key
        }
        
    except Exception as e:
        logger.error(f"Error getting rate limit status: {e}")
        return {"status": "error", "error": str(e)}