echo "Day 1:"
runhaskell day01.hs
echo "\n"

echo "Day 2:"
gcc day02.c -O2 && ./a.out && rm ./a.out
echo "\n"

echo "Day 3:"
python3 day03.py
echo "\n"

echo "Day 4:"
dart run day04.dart
echo "\n"

echo "Day 5:"
rustc day05.rs && ./day05 && rm day05
echo "\n"
