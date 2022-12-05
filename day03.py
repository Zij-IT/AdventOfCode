data = ''

with open('./input/day03.txt') as f:
    data = f.read()
    
def chunk(ls, n):
    for i in range(0, len(ls), n):
        yield ls[i:i + n]
    
def split(ls):
    le = len(ls)
    half = int(le / 2)
    return (ls[0: half], ls[half:le])

def first(ls):
    return ls[0]

def score(ch):
    ich = ord(ch)
    return ich - 65 + 27 if ich & 32 == 0 else ich - 97 + 1 

def score_list(ls):
    return sum(map(lambda x: score(first(x)), ls))

processed = [line for line in data.split("\n") if line.strip() != '']
splitted = map(split, processed)
chunked = chunk(processed, 3);

part_one = score_list([[z for z in x if z in y] for (x, y) in splitted]) 
part_two = score_list([[ch for ch in x if ch in y and ch in z] for [x, y, z] in chunked])

print(part_one)
print(part_two)