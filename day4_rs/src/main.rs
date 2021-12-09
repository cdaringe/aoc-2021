fn parse_input() -> (Vec<u32>, Vec<Vec<u32>>) {
    let int_lines: Vec<Vec<u32>> = include_str!("../input")
        .lines()
        .map(|l| l.split(|c| c == ',' || c == ' '))
        .map(|l| l.map(|c| str::parse::<u32>(c).unwrap()).collect())
        .collect();
    let mut int_lines_iter = int_lines.iter();
    let nums = int_lines_iter.next().unwrap().to_owned();
    let boards = int_lines_iter.fold(vec!(), |mut boards: Vec<Vec<u32>>, mut row| {
        let mut tail_ = boards.iter().last();
        let tail = tail_.unwrap().as_mut();
        if tail.len() < 25 {
            tail.append(&mut row.to_vec());
        } else {
            boards.push(row.to_vec());
        }
        boards
    });
    (nums, boards)
}
fn main() {
    // let (nums, boards) = parse_input();
    // let score_p1 = p1();
    // println!("{}", score_p1);
}
