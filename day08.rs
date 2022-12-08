fn get_neighbors(x: usize, y: usize, width: usize, height: usize) -> Vec<(usize, usize)> {
    let mut neighbors = vec![];

    if x != 0 {
        neighbors.push((x - 1, y));
    }

    if x != width - 1 {
        neighbors.push((x + 1, y));
    }

    if y != 0 {
        neighbors.push((x, y - 1));
    }

    if y != height - 1 {
        neighbors.push((x, y + 1));
    }

    neighbors
}

fn part_one(input: &[u8], width: usize, height: usize) -> usize {
    let mut sum = 0;
    for x in 0..width {
        for y in 0..height {
            let me = input[x + y * width];

            let other = |x| input[x + y * width];
            let vis_left = (0..x).map(other).all(|x| x < me);
            let vis_right = (x + 1..width).map(other).all(|x| x < me);

            let other = |y| input[x + y * width];
            let vis_up = (0..y).map(other).all(|y| y < me);
            let vis_down = (y + 1..height).map(other).all(|y| y < me);

            sum += if vis_left
                || vis_right
                || vis_up
                || vis_down
                || x == 0
                || x == width - 1
                || y == 0
                || y == height - 1
            {
                1
            } else {
                0
            };
        }
    }

    sum
}

fn part_two(input: &[u8], width: usize, height: usize) -> usize {
    let mut score = 0;

    for x in 0..width {
        for y in 0..height {
            let me = input[x + y * width];

            let other = |x| input[x + y * width];

            let mut left = 0;
            for nx in (0..x).rev() {
                if other(nx) >= me {
                    left += 1;
                    break;
                }
                left += 1;
            }

            let mut right = 0;
            for nx in x + 1..width {
                if other(nx) >= me {
                    right += 1;
                    break;
                }
                right += 1;
            }

            let other = |y| input[x + y * width];

            let mut up = 0;
            for ny in (0..y).rev() {
                if other(ny) >= me {
                    up += 1;
                    break;
                }
                up += 1;
            }

            let mut down = 0;
            for ny in y + 1..height {
                if other(ny) >= me {
                    down += 1;
                    break;
                }
                down += 1;
            }

            let t_score = left * right * up * down;
            score = t_score.max(score);
        }
    }

    score
}

fn main() {
    let input = include_str!("./input/day08.txt").trim();

    let width = input.lines().next().unwrap().len();
    let height = input.lines().count();

    let input: Vec<u8> = input
        .lines()
        .map(|x| x.chars().map(|y| y.to_digit(10).unwrap() as u8))
        .flatten()
        .collect();

    println!("{}", part_one(&input, width, height));
    println!("{}", part_two(&input, width, height));
}
