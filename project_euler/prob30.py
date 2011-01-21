#!/usr/bin/python

nums = [x**5 for x in range(0,10)]

num = 0
sums = []
for a in range(0,10):
    for b in range(0,10):
        for c in range(0,10):
            for d in range(0,10):
                for e in range(0,10):
                    for f in range(0,10):
                        if num == nums[a]+nums[b]+nums[c]+nums[d]+nums[e]+nums[f]:
                            sums.append(num)
                            print num
                        num += 1

print sum(sums) - 1
