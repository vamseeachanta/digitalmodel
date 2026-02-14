from collections.abc import Mapping


class AttributeDict(dict):

    def __init__(self, *args, **kwargs):
        super(AttributeDict, self).__init__(*args, **kwargs)
        self.__dict__ = self


def update_deep_dictionary(d, u):
    ''' for updating values in deep dictionary'''
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
