import 'dart:io';

class Range {
  final int start;
  final int end;

  Range(this.start, this.end);
}

class RangePair {
  final Range fst;
  final Range snd;

  RangePair(this.fst, this.snd);
}

void main() async {
  final input = (await File("./input/day04.txt").readAsString())
      .trim()
      .split('\n')
      .map((s) => s.replaceAll(',', '-').split('-').map(int.parse).toList())
      .map((arr) => RangePair(Range(arr[0], arr[1]), Range(arr[2], arr[3])));

  List<RangePair> list = List.from(input);
  List<RangePair> copy = List.from(input);

  copy.retainWhere(fullyOverlaps);
  print("Part 1: " + copy.length.toString());

  list.retainWhere(partiallyOverlaps);
  print("Part 2: " + list.length.toString());
}

bool partiallyOverlaps(RangePair rp) {
  final left = rp.fst;
  final right = rp.snd;

  return (right.start >= left.start && right.start <= left.end) ||
      (right.end >= left.start && right.end <= left.end) ||
      (left.start >= right.start && left.start <= right.end) ||
      (left.end >= right.start && left.end <= right.end);
}

bool fullyOverlaps(RangePair rp) {
  final left = rp.fst;
  final right = rp.snd;

  return (right.start >= left.start && right.end <= left.end) ||
      (left.start >= right.start && left.end <= right.end);
}
