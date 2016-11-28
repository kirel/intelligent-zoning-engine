import redis
import numpy as np

r = redis.Redis(host='localhost', port=6379)

r.set('status', 'stop')

dat = np.random.randn(4)
settings = np.random.randn(4)

r.set('data', dat)
r.set('settings', settings)