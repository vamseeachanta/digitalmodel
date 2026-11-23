from collections.abc import Mapping


class AttributeDict(dict):
    """A dictionary subclass that allows attribute-style access to its items.

    Inherits from dict and sets __dict__ to self, enabling both
    dict-style (d['key']) and attribute-style (d.key) access.

    Examples:
        >>> d = AttributeDict({'key': 'value'})
        >>> d.key
        'value'
    """

    def __init__(self, *args, **kwargs):
        """Initialize AttributeDict with the same arguments as dict.

        Args:
            *args: Variable length argument list passed to dict.
            **kwargs: Arbitrary keyword arguments passed to dict.
        """
        super(AttributeDict, self).__init__(*args, **kwargs)
        self.__dict__ = self


def update_deep_dictionary(d, u):
    """Recursively update a nested dictionary with values from another dictionary.

    Performs a deep merge of dictionary `u` into dictionary `d`. Nested
    dictionaries are merged recursively rather than being overwritten.

    Args:
        d: The base dictionary to update. Can be a Mapping or will be
            replaced entirely if not a Mapping.
        u: The dictionary containing update values.

    Returns:
        The updated dictionary with values from `u` merged into `d`.

    Examples:
        >>> d = {'a': {'b': 1, 'c': 2}}
        >>> u = {'a': {'b': 3}}
        >>> update_deep_dictionary(d, u)
        {'a': {'b': 3, 'c': 2}}
    """
    for k, v in u.items():
        # this condition handles the problem
        if not isinstance(d, Mapping):
            d = u
        elif isinstance(v, Mapping):
            r = update_deep_dictionary(d.get(k, {}), v)
            d[k] = r
        else:
            d[k] = u[k]

    return d


if __name__ == "__main__":

    pass
    # # update_deep_attribute_dictionary not working
    # example_dictionary = ({'employee': {'name': 'vamsee', 'age': 35, 'marks':{'science':10, 'math': 50}}})
    # # update_dict_to_attribute_dict(example_dictionary)
    # update_deep_attribute_dictionary(example_dictionary)
    # print(type(example_dictionary))
    # print(type(example_dictionary['employee']))
    # print("Finished")
