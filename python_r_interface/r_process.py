import time
import numpy as np

status_file = 'status_file.txt'
data_file = 'data_file.csv'


def start_optimizing():
	with open(status_file, 'w') as f:
		f.write("optimize")

	return


def run_process():

	res = np.array([0, 0, 0, 0])

	for j in range(3):
		start_optimizing()

		with open(status_file, 'r') as f:
			status = f.read()

		while status != 'done':
			# import ipdb; ipdb.set_trace()
			with open(data_file, 'r') as f:
					data = np.loadtxt(f, delimiter=',')

			if np.array_equal(res, data) == False:
				res = data
				print('something changed')

			with open(status_file, 'r') as f:
				status = f.read()
			time.sleep(1)
		print(j)

	return


def main():
	run_process()

	return


if __name__ == '__main__':
	main()