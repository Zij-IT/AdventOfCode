use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
struct Dir<'a> {
    sub_dirs: Vec<Rc<RefCell<Dir<'a>>>>,
    size: usize,
    name: &'a str,
}

fn get_root<'a>(input: &'a str) -> Rc<RefCell<Dir<'a>>> {
    let root = Rc::new(RefCell::new(Dir {
        sub_dirs: Vec::new(),
        size: 0,
        name: "/",
    }));

    let mut stack: Vec<Rc<RefCell<Dir>>> = Vec::new();
    let mut current = root.clone();

    for line in input.lines() {
        let parts: Vec<_> = line.split(' ').collect();

        if parts[0] == "$" {
            if parts[1] == "cd" {
                if parts[2] == ".." {
                    current = stack.pop().unwrap().clone();
                } else {
                    let new = Rc::new(RefCell::new(Dir {
                        sub_dirs: Vec::new(),
                        size: 0,
                        name: parts[2],
                    }));
                    current.borrow_mut().sub_dirs.push(new.clone());
                    stack.push(current.clone());
                    current = new.clone();
                }
            }
        } else if let Some(size) = parts[0].parse::<usize>().ok() {
            let new = Rc::new(RefCell::new(Dir {
                sub_dirs: vec![],
                size,
                name: parts[1],
            }));
            current.borrow_mut().sub_dirs.push(new);
        }
    }

    root
}

fn update_size<'a>(root: &mut Dir<'a>) {
    let mut sum = 0;

    for mut sub in &mut root.sub_dirs {
        update_size(&mut sub.borrow_mut());
        sum += sub.borrow().size;
    }

    if root.size == 0 {
        root.size = sum;
    }
}

fn part_one<'a>(root: &Dir<'a>) -> usize {
    let mut sum = 0;

    if root.size <= 100_000 && !root.sub_dirs.is_empty() {
        sum += root.size;
    }

    for sub in &root.sub_dirs {
        sum += part_one(&sub.borrow());
    }

    sum
}

fn part_two<'a>(root: &Dir<'a>, req_space: usize) -> usize {
    // No deleting files
    if root.sub_dirs.is_empty() {
        return usize::MAX;
    }

    let mut min_size = root.size;
    for sub in &root.sub_dirs {
        let sub_size = part_two(&sub.borrow(), req_space);
        if sub_size >= req_space {
            min_size = min_size.min(sub_size);
        }
    }

    min_size
}

fn main() {
    let file = include_str!("./input/day07.txt").trim();
    let root = get_root(file);

    update_size(&mut root.borrow_mut());

    println!("{}", part_one(&root.borrow()));
    println!(
        "{}",
        part_two(
            &root.borrow(),
            30_000_000 - (70_000_000 - root.borrow().size)
        )
    );
}
