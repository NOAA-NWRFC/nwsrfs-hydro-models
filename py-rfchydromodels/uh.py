import numpy as np
import math

class UH:
    def __init__(self, shape, scale, timestep, length = 1000):
        '''
        Class constructor for UH class
        :param shape: UH parameter
        :param scale: UH parameter
        :param timestep: UH timestep in hours
        :param length: max length of the UH, should be long enough to include all UH points
        '''
        self.shape = shape
        self.scale = scale
        # this code needs timestep in days
        self.timestep = timestep / 24
        self.length = length

    def get_ord_dimensionless(self):
        '''
        Get ordinates of the unit hydrograph based on a 2-parameter (shape,scale) gamma model
        :return: numpy array with unit hydrograph ordinates, The length of the array will
                 vary based on the parameters and timestep.
        '''
        uh = np.full([self.length],0.0)

        # could use self.gf here instead of math.gamma, but I trust the python builtin more
        toc = np.log(math.gamma(self.shape) * self.scale)
        for i in range(self.length):
            top = (i+1) * self.timestep / self.scale
            tor = (self.shape-1) * np.log(top)-top-toc
            uh[i] = 0
            if tor > -8.0:
                #import pdb;
                #pdb.set_trace()
                uh[i] = np.exp(tor)
            else:
                if i > 0:
                    uh[i] = 0.0
                    break

        s = sum(uh)
        s = 1.0e-5 if s == 0 else s
        # turn it into a unit hydrograph (sums to 1)
        uh = uh / s
        # dont return all the trailing zero values
        first0 = -1
        for i in range(self.length):
            if uh[i] == 0:
                first0 = i
                break

        if first0 == max(range(self.length)):
            print('UH may have been truncated, increase length')
            return uh
        else:
            return uh[0:first0]

    def gf(self,x):
        '''
        Gamma function, single precision accuracy
        :param x: real number
        :return: gamma(x)
        '''
        h = 1
        if x <= 0:
            return (np.nan)
        while True:
            if (x > 0) & (x < 2):
                h /= x
                x += 1
            elif x == 2:
                return h
            elif (x > 2) & (x <= 3):
                x -= 2
                h *= (((((((.0016063118 * x + 0.0051589951) * x + 0.0044511400) * x + .0721101567
                     ) * x + .0821117404) * x + .4117741955) * x + .4227874605) * x + .9999999758)
                return h
            else: # x>3
                x -= 1
                h *= x

    def get_ord_cfs_in(self,area):
        '''
        Return UH ordinates (a numpy array) in units of cfs/in
        :param area: Basin area in square miles
        :return:
        '''
        return self.get_ord_dimensionless() * 5280**2 * 24 / 12 / 86400 / self.timestep * area

if __name__ == '__main__':
    from uh import UH

    shape = 1.5
    scale = 2
    timestep = 6 # hours
    area = 100 # square miles

    uh = UH(shape, scale, timestep)
    uh_cfs_in = uh.get_ord_cfs_in(area)