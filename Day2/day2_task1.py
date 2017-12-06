import sys

with open(sys.argv[1]) as fp:
    lines = fp.readlines()
    sum = 0
    for line in lines:
        nums = list(map(int, line.split('\t')))
        sum += max(nums) - min(nums)
    print(sum)
