from collections import defaultdict

with open("template.txt") as handle:
    lines = handle.read().splitlines()
    template = lines[0]
    conversions = {tuple(pair): insertion for pair, insertion in [line.split(" -> ") for line in lines[2:]]}

counts = defaultdict(int)

def expand(left: str, right: str, depth: int) -> None:
    if depth == 0:
        counts[left] += 1
        return
    mid = conversions[(left, right)]
    expand(left, mid, depth - 1)
    expand(mid, right, depth - 1)

for i, char in enumerate(template[1:]):
    expand(template[i], char, 30)

# since it was never a left value, add the final char
counts[template[-1]] += 1

print(max(counts.values()) - min(counts.values()))