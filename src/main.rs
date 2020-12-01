#![feature(bool_to_option)]
use std::io::prelude::*;
use std::fs;

fn day11() {
    let input = String::from_utf8_lossy(&fs::read("day1").unwrap())
        .lines()
        .map(|s| s.parse::<u64>().unwrap())
        .collect::<Vec<_>>();

    let result = input
        .iter()
        .enumerate()
        .filter_map(|(i, x)| {
            input
                .iter()
                .skip(i)
                .filter_map(move |y| if x + y == 2020 {Some(*y)} else {None})
                .next()
                .map(|y| Some(y * x))
        })
        .next()
        .unwrap()
        .unwrap();

    println!("{}", result);
}

fn day12() {
    let input = String::from_utf8_lossy(&fs::read("day1").unwrap())
        .lines()
        .map(|s| s.parse::<u64>().unwrap())
        .collect::<Vec<_>>();

    let alternatives = (0..2020)
        .map(move |x| {
            (0..2020)
                .map(move |y| {
                    (0..2020)
                        .map(move |z| (x, y, z))
                })
        })
        .flatten()
        .flatten()
        .filter(|(x, y, z)| x + y + z == 2020)
        .filter_map(|(x, y, z)| {
            (input.contains(&x) && input.contains(&y) && input.contains(&z))
                .then_some(x * y * z)
        })
        .next()
        .unwrap();

    println!("{}", alternatives)

}

fn main() {
    day12();
}
