struct Sub {
    z: u64,
    x: u64,
    aim: u64,
}
impl Sub {
    pub fn new() -> Sub {
        Sub {
            x: 0 as u64,
            z: 0 as u64,
            aim: 0 as u64,
        }
    }
    pub fn process(&mut self, cmd: &SubCmd) -> &mut Self {
        match cmd {
            SubCmd::Up(scalar) => {
                self.aim -= scalar;
            }
            SubCmd::Down(scalar) => {
                self.aim += scalar;
            }
            SubCmd::Forward(scalar) => {
                self.x += scalar;
                self.z = self.z + (self.aim * scalar);
            }
        };
        self
    }
    pub fn process_many(&mut self, cmds: &Vec<SubCmd>) -> &mut Self {
        cmds.iter().for_each(|cmd| {
            self.process(cmd);
        });
        self
    }
}

impl std::fmt::Display for Sub {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "x: {}, z: {}", self.x, self.z)
    }
}

enum SubCmd {
    Forward(u64),
    Down(u64),
    Up(u64),
}

fn main() {
    let cmds = include_str!("../input")
        .lines()
        .map(|l| {
            let parts = l.split(" ").collect::<Vec<_>>();
            let cmd = parts.get(0).unwrap();
            let scalar = parts.get(1).unwrap().parse::<u64>().unwrap();
            match *cmd {
                "up" => SubCmd::Up(scalar),
                "down" => SubCmd::Down(scalar),
                "forward" => SubCmd::Forward(scalar),
                _ => panic!("unsupported"),
            }
        })
        .collect::<Vec<_>>();
    let mut sub = Sub::new();
    sub.process_many(&cmds);
    println!("pos: {}", sub);
    println!("x * z: {}", sub.z * sub.x);
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
