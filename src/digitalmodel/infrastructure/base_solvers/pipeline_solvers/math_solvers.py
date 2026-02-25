class Polynomial():

    def solve_by_coefficients(self, coefficients):
        '''
        Author: Vamsee Achanta
        Date Updated: 2018-17-07
        Objective: To solve a polynomial equation using coefficients
        '''
        import numpy as np

        # coefficients = [3.2, 2, 1]
        solution = np.roots(coefficients)
        return solution

    def solve_using_sympy(self):
        '''
        Author: Vamsee Achanta
        Date Updated: 2018-17-07
        Objective: To solve a polynomial equation using symbolic language
        '''
        from sympy import solve, symbols

        x = symbols("x")
        solution = solve(x**2 - 2, x)
        print(solution)

        print(solution[0])
        print(float(solution[0]))


class Scipy_Interpolation():

    def solve_y_for_X(self, X, y, n_neighbors, input_array_to_predict):
        from sklearn.neighbors import KNeighborsRegressor
        neigh = KNeighborsRegressor(n_neighbors=2)
        neigh.fit(X, y)
        return neigh.predict([input_array_to_predict])


class FFT_Methods():

    def numpy_fft(self):
        import matplotlib.pyplot as plt
        import numpy as np
        import scipy.fftpack

        # https://stackoverflow.com/questions/25735153/plotting-a-fast-fourier-transform-in-python
        # Number of samplepoints
        N = 600
        # sample spacing
        T = 1.0 / 800.0
        x = np.linspace(0.0, N * T, N)
        y = np.sin(50.0 * 2.0 * np.pi * x) + 0.5 * np.sin(80.0 * 2.0 * np.pi * x)
        yf = scipy.fftpack.fft(y)
        xf = np.linspace(0.0, 1.0 / (2.0 * T), N / 2)

        fig, ax = plt.subplots()
        ax.plot(xf, 2.0 / N * np.abs(yf[:N // 2]))
        plt.show()


class Geometry():

    def max_distance_for_spatial_sets(self, spatial_data_sets):
        import random as rd

        import numpy as np
        from scipy.spatial.distance import pdist, squareform

        # Running a sample example data
        spatial_data_sets_sample = np.array([np.array([rd.randint(-5, 5) for x in range(3)]) for y in range(500)])

        D = pdist(spatial_data_sets)
        D = squareform(D)
        distance_max = D.max()
        distance_list = list(D[0])
        distance_list.sort()
        distance_min = distance_list[1]
        max_data_set = np.where(D == distance_max)
        min_data_set = np.where(D == distance_min)
        result = {
            'distance_max': distance_max,
            'distance_min': distance_min,
            'max_data_set': max_data_set[0],
            'min_data_set': min_data_set[0]
        }

        return result
