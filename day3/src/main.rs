fn bitvec_to_int(v: &Vec<u32>) -> u32 {
    v.iter().fold(0, |result, &bit| (result << 1) ^ bit)
}

fn p1() {
    let binary_nums = include_str!("../input").lines().collect::<Vec<_>>();
    let count_nums = binary_nums.len();
    let (gam_v, eps_v) = binary_nums
        .iter()
        .map(|l| {
            l.chars()
                .map(|c| if c == '1' { 1 as u8 } else { 0 as u8 })
                .collect::<Vec<u8>>()
        })
        .fold(vec![], |mut acc: Vec<u32>, bit_set: Vec<u8>| {
            bit_set
                .iter()
                .enumerate()
                .for_each(|(i, &bit)| match acc.get(i) {
                    Some(&x) => {
                        acc[i] = x + bit as u32;
                    }
                    None => {
                        acc.push(bit as u32);
                    }
                });
            acc
        })
        .iter()
        .fold(
            (Vec::with_capacity(12), Vec::with_capacity(12)),
            |(mut gam, mut eps): (Vec<u32>, Vec<u32>), &count| {
                if count > (count_nums as u32 / 2) {
                    gam.push(1);
                    eps.push(0);
                } else {
                    gam.push(0);
                    eps.push(1);
                }
                (gam, eps)
            },
        );
    let gam = bitvec_to_int(&gam_v);
    let eps = bitvec_to_int(&eps_v);
    println!("{}", gam * eps);
}
fn main() {
    p1();
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
