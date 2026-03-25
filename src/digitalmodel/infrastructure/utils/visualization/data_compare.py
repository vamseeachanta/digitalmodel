class Compare:

    def __init__(self):
        pass

    def find_dff_in_2_Dicts(self, d1, d2, path=""):
        '''Reference: https://stackoverflow.com/questions/27265939/comparing-python-dictionaries-and-nested-dictionaries/35065035'''
        for k in d1:
            if (k not in d2):
                print(path, ":")
                print(k + " as key not in d2", "\n")
            else:
                if type(d1[k]) is dict:
                    if path == "":
                        path = k
                    else:
                        path = path + "->" + k
                    find_dff_in_Dicts(d1[k], d2[k], path)
                else:
                    if d1[k] != d2[k]:
                        print(path, ":")
                        print(" - ", k, " : ", d1[k])
                        print(" + ", k, " : ", d2[k])


class CompareTests:

    def __init__(self):
        pass

    def test_find_dff_in_2_Dicts(self):
        d1 = {'a': {'b': {'cs': 10}, 'd': {'cs': 20}}}
        d2 = {'a': {'b': {'cs': 30}, 'd': {'cs': 20}}, 'newa': {'q': {'cs': 50}}}
        print("comparing d1 to d2:")
        print(find_dff_in_Dicts(d1, d2))
        print("comparing d2 to d1:")
        print(find_dff_in_Dicts(d2, d1))


if __name__ == '__main__':
    ct = CompareTests()
    ct.test_find_dff_in_2_Dicts()
