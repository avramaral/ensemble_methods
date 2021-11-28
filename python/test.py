import sys
print(sys.version)

import numpy as np
print(np.__version__)



a = np.array([[1, 2, 4], [5, 5.2, 6]])
print(a)

np.savetxt('test.csv', a, delimiter = ',')

print('Andre')