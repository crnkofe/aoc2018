import sys

with open(sys.argv[1]) as f:
    a = set()
    current_frequency = 0
    original_input = list(map(int, [line for line in f]))
    print(original_input)
    i = 0
    while current_frequency not in a: # and i < len(original_input):
        a.add(int(current_frequency))
        current_frequency += original_input[i % len(original_input)]
        print(current_frequency)
        i += 1
    print(current_frequency, i, i / len(original_input), i % len(original_input))
