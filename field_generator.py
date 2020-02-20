# Generator written by Valentin Sergeev and improved by Daniil Manakovskiy

with open("map.txt") as f:
    content = f.readlines()
i, j = 0, 0

out = open("input.pl", "w")
out.write(":- module(input, [h/2,o/2,t/2]).\n\n")
pattern = "%s(%d, %d).\n"

mapper = {"O": "o", "T": "t", "H": "h"}
output = []

for line in reversed(content):
    for c in line:
        if c in mapper:
            output.append(pattern % (mapper[c], i, j))
        j += 1
    j = 0
    i += 1

# out.write("---------------------")
out.write("".join(sorted(output)))
