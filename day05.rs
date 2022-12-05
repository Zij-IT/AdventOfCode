#![allow(dead_code)]

#[derive(Debug, Clone, Copy)]
struct Crate(char);

#[derive(Debug, Clone, Copy)]
struct Move {
    count: u8,
    from: u8,
    to: u8,
}

fn parse_crates(input: &str) -> Vec<Vec<Crate>> {
    let height = input.lines().count() - 1;
    let width = input.lines().next().map(str::len).unwrap() / 4 + 1;

    let mut crates = Vec::with_capacity(input.len() / 3);
    let mut input = input.chars();

    loop {
        if input.next() == Some('[') {
            crates.push(Some(Crate(input.next().unwrap())));
        } else {
            crates.push(None);
            input.next();
        }

        input.next();

        if input.next().is_none() {
            break;
        }
    }

    // Transpose the matrix :D
    let mut out = vec![vec![None; height]; width];
    for x in 0..width {
        for y in 0..height {
            let input_idx = x + y * width;
            out[x][y] = crates[input_idx];
        }
    }

    out.into_iter()
        .map(|x| x.into_iter().flatten().rev().collect())
        .collect()
}

fn parse_moves(input: &str) -> Vec<Move> {
    input
        .split("\n")
        .filter(|x| !x.is_empty())
        .map(|line| {
            let mut words = line.split(" ");

            words.next(); // move

            let count = words.next().unwrap().parse().unwrap();

            words.next(); // from

            let from = words.next().unwrap().parse().unwrap();

            words.next(); // to

            let to = words.next().unwrap().parse().unwrap();

            Move { count, from, to }
        })
        .collect()
}

fn part_one(mut crates: Vec<Vec<Crate>>, moves: Vec<Move>) {
    for Move { from, to, count } in moves {
        let from = &mut crates[usize::from(from) - 1];
        let split = from
            .split_off(from.len() - usize::from(count))
            .into_iter()
            .rev()
            .collect::<Vec<_>>();
        crates[usize::from(to) - 1].extend_from_slice(&split)
    }

    let ans = crates
        .into_iter()
        .map(|x| x.into_iter().last().unwrap())
        .map(|Crate(x)| x)
        .collect::<String>();

    println!("Part 1: {ans}");
}

fn part_two(mut crates: Vec<Vec<Crate>>, moves: Vec<Move>) {
    for Move { from, to, count } in moves {
        let from = &mut crates[usize::from(from) - 1];
        let split = from
            .split_off(from.len() - usize::from(count))
            .into_iter()
            // .rev() // Literally only this line is different :D
            .collect::<Vec<_>>();
        crates[usize::from(to) - 1].extend_from_slice(&split)
    }

    let ans = crates
        .into_iter()
        .map(|x| x.into_iter().last().unwrap())
        .map(|Crate(x)| x)
        .collect::<String>();

    println!("Part 2: {ans}");
}

fn main() {
    let mut input = INPUT.split("\n\n");
    let crates = input.next().map(parse_crates).unwrap();
    let moves = input.next().map(parse_moves).unwrap();

    part_one(crates.clone(), moves.clone());
    part_two(crates, moves);
}

const INPUT: &str = include_str!("./input/day05.txt");

const TEST_INPUT: &str = include_str!("./test_input/day05.txt");
