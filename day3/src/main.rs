fn bitvec_to_int(v: &Vec<u32>) -> u32 {
    v.iter().fold(0, |result, &bit| (result << 1) ^ bit)
}

fn bitvec_to_int_u8(v: &Vec<u8>) -> u32 {
    v.iter().fold(0, |result, &bit| (result << 1) ^ bit as u32)
}

fn p1(binary_chars: &Vec<&str>) -> u32 {
    let count_nums = binary_chars.len();
    let (gam_v, eps_v) = binary_chars
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
    gam * eps
}

#[derive(Debug, PartialEq, Eq)]
enum BitPos {
    HI,
    LO,
}

fn count_bits_at(v: &Vec<Vec<u8>>, bit_idx: usize, pos: BitPos) -> u32 {
    v.iter().fold(0, |acc, bits| match bits.get(bit_idx) {
        Some(&x) => {
            if (pos == BitPos::HI && x == 1) || (pos == BitPos::LO && x == 0) {
                acc + 1
            } else {
                acc
            }
        }
        None => panic!("whoopsie"),
    })
}

fn as_u8_vec(v: &Vec<&str>) -> Vec<Vec<u8>> {
    v.iter()
        .map(|l| {
            l.chars()
                .map(|c| if c == '1' { 1 as u8 } else { 0 as u8 })
                .collect::<Vec<u8>>()
        })
        .collect()
}

fn p2(binary_chars: &Vec<&str>) -> u32 {
    let oxy = (0..binary_chars.first().unwrap().len())
        .fold(as_u8_vec(binary_chars), |rem, bit_idx| {
            if rem.len() == 1 {
                return rem;
            }
            let half = rem.len() as f32 / 2.;
            let most_common = if count_bits_at(&rem, bit_idx, BitPos::HI) as f32 >= half {
                BitPos::HI
            } else {
                BitPos::LO
            };
            let next_rem = rem
                .iter()
                .filter_map(|bits| match bits.get(bit_idx) {
                    Some(&x) => {
                        if x == 1 && most_common == BitPos::HI
                            || x == 0 && most_common == BitPos::LO
                        {
                            Some(bits.clone())
                        } else {
                            None
                        }
                    }
                    _ => panic!("blast"),
                })
                .collect::<Vec<Vec<u8>>>();
            next_rem
        })
        .first()
        .and_then(|x| Some(bitvec_to_int_u8(x)))
        .unwrap();

    let c02 = (0..12)
        .fold(as_u8_vec(binary_chars), |rem, bit_idx| {
            if rem.len() == 1 {
                return rem;
            }
            let half = rem.len() as f32 / 2.;
            let count_lo = count_bits_at(&rem, bit_idx, BitPos::LO);
            let least_common = if count_lo as f32 <= half {
                BitPos::LO
            } else {
                BitPos::HI
            };
            let next_rem = rem
                .iter()
                .filter_map(|bits| match bits.get(bit_idx) {
                    Some(&x) => {
                        if x == 1 && least_common == BitPos::HI
                            || x == 0 && least_common == BitPos::LO
                        {
                            Some(bits.clone())
                        } else {
                            None
                        }
                    }
                    _ => panic!("blast"),
                })
                .collect::<Vec<Vec<u8>>>();
            next_rem
        })
        .last()
        .and_then(|x| Some(bitvec_to_int_u8(x)))
        .unwrap();
    oxy * c02
}

fn main() {
    let binary_chars = include_str!("../input").lines().collect::<Vec<_>>();
    println!("{}", p1(&binary_chars));
    println!("{}", p2(&binary_chars));
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        let binary_chars = include_str!("../input.test").lines().collect::<Vec<_>>();
        p2(&binary_chars);
    }
}
