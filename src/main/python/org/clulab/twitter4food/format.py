import random

# input file
full = open("overweightDataFull.txt", 'r')

# UNCOMMENT THESE AND THE LINES AT THE END TO WRITE TO THE FILES
# trainFile = open("overweightTrain.txt", 'w')
# devFile = open("overweightDev.txt", 'w')
# testFile = open("overweightTest.txt", 'w')

overweight = []
notOverweight = []

isFirst = True
toAdd = ""

for line in full:
    if line == "Overweight\n" or line == "Not overweight\n":
        if not isFirst:
            if line == "Overweight\n":
                overweight.append(toAdd)
            elif line == "Not overweight\n":
                notOverweight.append(toAdd)
        else:
            isFirst = False
        toAdd = line
    else:
        toAdd += line

first = int(len(overweight) * 0.6)
second = (len(overweight) - first) / 2

firstN = int(len(notOverweight) * 0.6)
secondN = (len(notOverweight) - firstN) / 2

print str(len(overweight))
print str(len(notOverweight))

train = overweight[0:first]
dev = overweight[first:first+second]
test = overweight[first+second:]

print str(len(train))
print str(len(dev))
print str(len(test))
print

train += notOverweight[0:firstN]
dev += notOverweight[firstN:firstN+secondN]
test += notOverweight[firstN+secondN:]

print str(len(train))
print str(len(dev))
print str(len(test))

# for account in train:
#     trainFile.write(account)
# for account in dev:
#     devFile.write(account)
# for account in test:
#     testFile.write(account)  