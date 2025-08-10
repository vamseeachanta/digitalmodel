"""
Redis cache service for performance optimization
"""

import json
from typing import Any, Optional

import redis.asyncio as redis

from utils.config import settings
from utils.logger import setup_logger

logger = setup_logger(__name__)

# Global Redis client
redis_client: Optional[redis.Redis] = None


async def init_redis() -> None:
    """Initialize Redis connection"""
    global redis_client
    
    try:
        redis_client = redis.from_url(
            settings.redis_url,
            encoding="utf-8",
            decode_responses=True,
        )
        
        # Test connection
        await redis_client.ping()
        logger.info("Redis connection established")
    except Exception as e:
        logger.error(f"Failed to connect to Redis: {e}")
        # Don't raise - cache is optional
        redis_client = None


async def get_cache(key: str) -> Optional[Any]:
    """
    Get value from cache
    
    Args:
        key: Cache key
    
    Returns:
        Cached value or None if not found
    """
    if not redis_client:
        return None
    
    try:
        value = await redis_client.get(key)
        if value:
            return json.loads(value)
    except Exception as e:
        logger.warning(f"Cache get error for key {key}: {e}")
    
    return None


async def set_cache(
    key: str,
    value: Any,
    ttl: Optional[int] = None,
) -> bool:
    """
    Set value in cache
    
    Args:
        key: Cache key
        value: Value to cache
        ttl: Time to live in seconds (defaults to settings.cache_ttl)
    
    Returns:
        True if successful, False otherwise
    """
    if not redis_client:
        return False
    
    try:
        serialized = json.dumps(value)
        ttl = ttl or settings.cache_ttl
        await redis_client.setex(key, ttl, serialized)
        return True
    except Exception as e:
        logger.warning(f"Cache set error for key {key}: {e}")
        return False


async def delete_cache(pattern: str) -> int:
    """
    Delete cache entries matching pattern
    
    Args:
        pattern: Key pattern (supports wildcards)
    
    Returns:
        Number of deleted keys
    """
    if not redis_client:
        return 0
    
    try:
        keys = await redis_client.keys(pattern)
        if keys:
            return await redis_client.delete(*keys)
    except Exception as e:
        logger.warning(f"Cache delete error for pattern {pattern}: {e}")
    
    return 0