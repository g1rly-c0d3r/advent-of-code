from sys import argv as av

def area(tup):
    l,w,h = tup
    area = 2*l*w + 2*l*h + 2*w*h

    

if __name__ == "__main__":
    f = open(av[1], "r")
    lines = f.readlines()
    nums = []

    for line in lines:
        nums.append(tuple(map(int, line.strip().split('x'))))

    for area in map(area,nums):
        print(area)
        
    

