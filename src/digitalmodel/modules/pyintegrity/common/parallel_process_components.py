class ParallelProcessComponents():

    def __init__(self, cfg):
        self.cfg = cfg

    def get_number_of_workers(self, percentage_utilization=90):
        import math
        available_cpus = self.available_cpu_count()
        number_of_workers = math.floor(available_cpus * percentage_utilization/100)
        return number_of_workers

    def write(self, i, x):
        return (i, "---", x)

    def example_1(self):
        from multiprocessing import Pool

        a = ["1", "2", "3", "11", "12", "13"]
        b = ["4", "5", "6", "14", "15", "16"]
        number_of_workers = self.get_number_of_workers(percentage_utilization = 50)
        pool = Pool(number_of_workers)

        result_array = pool.starmap(self.write, zip(a, b))
        pool.close()
        pool.join()
        print(result_array)

    def example_2(self):
        from multiprocessing import Pool
        number_of_workers = self.get_number_of_workers()
        pool = Pool(number_of_workers)

        args = ((foo, bar, foobar, baz)
                for foo in range(2)
                for bar in range(2)
                for baz in range(2)
                for foobar in range(3))
        print(args)
        result_array = pool.starmap(self.calculation, args)
        pool.close()
        pool.join()
        print(result_array)

    def calculation(self, foo, bar, foobar, baz):
        print("variables used for foo, bar, foobar and baz are: {}, {}, {}, {}" .format(foo, bar, foobar, baz))
        result = foo * bar * foobar * baz
        return result

    def available_cpu_count(self):
        """ Number of available virtual or physical CPUs on this system, i.e.
        user/real as output by time(1) when called with an optimally scaling
        userspace-only program"""

        # cpuset
        # cpuset may restrict the number of *available* processors
        try:
            import re
            m = re.search(r'(?m)^Cpus_allowed:\s*(.*)$',
                          open('/proc/self/status').read())
            if m:
                res = bin(int(m.group(1).replace(',', ''), 16)).count('1')
                if res > 0:
                    return res
        except IOError:
            pass

        # Python 2.6+
        try:
            import multiprocessing
            return multiprocessing.cpu_count()
        except (ImportError, NotImplementedError):
            pass

        # https://github.com/giampaolo/psutil
        try:
            import psutil
            return psutil.cpu_count()   # psutil.NUM_CPUS on old versions
        except (ImportError, AttributeError):
            pass

        # POSIX
        try:
            import os
            res = int(os.sysconf('SC_NPROCESSORS_ONLN'))

            if res > 0:
                return res
        except (AttributeError, ValueError):
            pass

        # Windows
        try:
            import os
            res = int(os.environ['NUMBER_OF_PROCESSORS'])

            if res > 0:
                return res
        except (KeyError, ValueError):
            pass

        # jython
        try:
            from java.lang import Runtime
            runtime = Runtime.getRuntime()
            res = runtime.availableProcessors()
            if res > 0:
                return res
        except ImportError:
            pass

        # BSD
        try:
            import subprocess
            sysctl = subprocess.Popen(['sysctl', '-n', 'hw.ncpu'],
                                      stdout=subprocess.PIPE)
            scStdout = sysctl.communicate()[0]
            res = int(scStdout)

            if res > 0:
                return res
        except (OSError, ValueError):
            pass

        # Linux
        try:
            res = open('/proc/cpuinfo').read().count('processor\t:')

            if res > 0:
                return res
        except IOError:
            pass

        # Solaris
        try:
            import os
            import re
            pseudoDevices = os.listdir('/devices/pseudo/')
            res = 0
            for pd in pseudoDevices:
                if re.match(r'^cpuid@[0-9]+$', pd):
                    res += 1

            if res > 0:
                return res
        except OSError:
            pass

        # Other UNIXes (heuristic)
        try:
            try:
                dmesg = open('/var/run/dmesg.boot').read()
            except IOError:
                dmesgProcess = subprocess.Popen(['dmesg'], stdout=subprocess.PIPE)
                dmesg = dmesgProcess.communicate()[0]

            res = 0
            while '\ncpu' + str(res) + ':' in dmesg:
                res += 1

            if res > 0:
                return res
        except OSError:
            pass

        raise Exception('Can not determine number of CPUs on this system')

if __name__ == '__main__':
    pp = ParallelProcessComponents(cfg=None)
    pp.example_1()
    pp.example_2()
