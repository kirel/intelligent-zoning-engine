import time
import redis
import numpy as np

status_file = 'status_file.txt'
data_file = 'data_file.csv'
settings_file = 'settings.csv'

r = redis.Redis(host='localhost', port=6379)


def start_stop():
	with open(status_file, 'r') as f:
		command = f.read()

	return command	

def restart():
	print('restarted')
	return

def send_data(res):
	np.savetxt(data_file, res, delimiter=',', fmt='%.2f')

	return


def get_settings():
	with open(settings_file, 'r') as f_settings:
		new_settings = np.loadtxt(f_settings, delimiter=',')

	return new_settings


def optimize():
	print('entered optimizing')
	settings = np.array([0,0,0,0])
	
	for i in range(10):
		command = start_stop()
		if command == 'stop\n':
			return

		new_settings = get_settings()
		if not np.all(new_settings == settings):
			settings = new_settings
			restart()
			
		print('I am optimizing')
		time.sleep(3)
		res = np.random.randn(4)
		
		send_data(res)

	return res
	

def run_process():
	with open(data_file, 'w') as f:
		res = np.random.randn(4)
		np.savetxt(data_file, res, delimiter=',', fmt='%.2f')
	
	while True:

		command = start_stop()
		if command == 'start':
			optimize()

	return

def main():
	run_process()


if __name__ == '__main__':
	main()



