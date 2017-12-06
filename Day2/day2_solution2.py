import sys


def even_divide(nums):
    nums.sort()
    length = len(nums)
    for i in range(length-1):
        for j in range(i+1, length):
            if nums[j] % nums[i] == 0:
                return nums[j] // nums[i]
    raise "Something went wrong!"


with open(sys.argv[1]) as fp:
    lines = fp.readlines()
    sum = 0
    for line in lines:
        nums = list(map(int, line.split('\t')))
        sum += even_divide(nums)
    print(sum)
