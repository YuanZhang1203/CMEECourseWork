from subprocess import Popen, PIPE

output1 = Popen('python3 -m cProfile LV1.py', stdout=PIPE, shell=True).stdout.read().decode()
output2 = Popen('python3 -m cProfile LV2.py 1 0.1 1.5 0.75', stdout=PIPE, shell=True).stdout.read().decode()
output3 = Popen('python3 -m cProfile LV3.py 1 0.1 1.5 0.75', stdout=PIPE, shell=True).stdout.read().decode()
output4 = Popen('python3 -m cProfile LV4.py 1 0.1 1.5 0.75', stdout=PIPE, shell=True).stdout.read().decode()
print(output1)
print(output2)
print(output3)
print(output4)

