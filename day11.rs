#[derive(Clone, Copy)]
enum Op {
    AddOld,
    AddN(usize),
    MulOld,
    MulN(usize),
}

impl Op {
    fn run(self, x: usize) -> usize {
        match self {
            Op::AddOld => x + x,
            Op::AddN(n) => x + n,
            Op::MulOld => x * x,
            Op::MulN(n) => x * n,
        }
    }
}

#[derive(Clone, Copy)]
struct Test {
    divisor: usize,
    on_true: usize,
    on_false: usize,
}

impl Test {
    fn run(self, x: usize) -> usize {
        let Test {
            divisor,
            on_true,
            on_false,
        } = self;

        if x % divisor == 0 {
            on_true
        } else {
            on_false
        }
    }
}

#[derive(Clone)]
struct Monkey {
    items: Vec<usize>,
    passed: usize,
    divisor: usize,
    op: Op,
    test: Test,
}

fn parse_monkey(lines: &mut dyn Iterator<Item = &str>) -> Monkey {
    let _ = lines.next().unwrap();

    let items = lines
        .next()
        .unwrap()
        .split(" ")
        .skip(2)
        .map(|x| x.trim_end_matches(",").parse().unwrap())
        .collect();

    let mut ops = lines.next().unwrap().trim().split(" ").skip(4);
    let op = match (ops.next().unwrap(), ops.next().unwrap()) {
        ("+", "old") => Op::AddOld,
        ("*", "old") => Op::MulOld,
        ("+", n) => Op::AddN(n.parse().unwrap()),
        ("*", n) => Op::MulN(n.parse().unwrap()),
        _ => unimplemented!(),
    };

    let mut get_next_nth = move |n| {
        lines
            .next()
            .unwrap()
            .split(" ")
            .nth(n)
            .unwrap()
            .parse()
            .unwrap()
    };

    let divisor = get_next_nth(3);

    let test = Test {
        divisor,
        on_true: get_next_nth(5),
        on_false: get_next_nth(5),
    };

    Monkey {
        items,
        test,
        divisor,
        op,
        passed: 0,
    }
}

fn play_round<F: Fn(usize) -> usize>(monks: &mut [Monkey], worry_reducer: &F) {
    for x in 0..monks.len() {
        let items = std::mem::take(&mut monks[x].items);
        monks[x].passed += items.len();

        for item in items {
            let val = worry_reducer(monks[x].op.run(item));
            let goal = monks[x].test.run(val);
            monks[goal].items.push(val);
        }
    }
}

fn part_x<F: Fn(usize) -> usize>(monks: &mut [Monkey], rounds: usize, worry_red: F) -> usize {
    for _ in 0..rounds {
        play_round(monks, &worry_red);
    }

    monks.sort_by_key(|x| x.passed);
    monks.iter().map(|m| m.passed).rev().take(2).product()
}

fn part_one(monks: &mut [Monkey]) -> usize {
    part_x(monks, 20, |x| x / 3)
}

fn part_two(monks: &mut [Monkey]) -> usize {
    // Yay number theory. Chinese Remainder Theorem to the rescue
    let crt: usize = monks.iter().map(|x| x.divisor).product();
    part_x(monks, 10_000, |x| x % crt)
}

fn main() {
    let now = std::time::Instant::now();
    let mut lines = include_str!("./input/day11.txt").lines().map(|x| x.trim());
    let mut monkeys = Vec::new();

    loop {
        monkeys.push(parse_monkey(&mut lines));
        if lines.next().is_none() {
            break;
        }
    }

    println!("Part 1: {}", part_one(&mut monkeys.clone()));
    println!("Part 2: {}", part_two(&mut monkeys));
    println!("Time: {}ms", now.elapsed().as_millis());
}
