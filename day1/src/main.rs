fn main() {
    let pings = include_str!("../input")
        .lines()
        .map(str::parse::<i32>)
        .map(Result::unwrap)
        .collect::<Vec<_>>();
    let p1_diffs = pings.windows(2).map(|els| {
        let a = els.get(0).unwrap();
        let b = els.get(1).unwrap();
        *b - *a
    });
    let positive = p1_diffs.filter(|x| *x > 0).count();
    println!("p1: {}", positive);

    // p2
    let positive_2 = pings
        .windows(3)
        .map(|group| group.iter().sum())
        .collect::<Vec<i32>>()
        .windows(2)
        .filter_map(|els| {
            let a = els.get(0).unwrap();
            let b = els.get(1).unwrap();
            let diff = *b - *a;
            if diff > 0 {
                Some(diff)
            } else {
                None
            }
        })
        .count();
    println!("p2: {}", positive_2);
}
