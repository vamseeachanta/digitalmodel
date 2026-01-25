def use_unit(unit):
    import functools

    import pint
    """Have a function return a Quantity with given unit"""
    use_unit.ureg = pint.UnitRegistry()

    def decorator_use_unit(func):

        @functools.wraps(func)
        def wrapper_use_unit(*args, **kwargs):
            value = func(*args, **kwargs)
            return value * use_unit.ureg(unit)

        return wrapper_use_unit

    return decorator_use_unit


@use_unit("meters per second")
def average_speed(distance, duration):
    return distance / duration


if __name__ == '__main__':
    bolt = average_speed(100, 9.58)
    print(bolt)
    print(bolt.to("km per hour"))
    print(bolt.to("mph").m)
