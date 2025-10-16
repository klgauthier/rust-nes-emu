use std::fmt::Display;

#[derive(Clone, Copy, Debug)]
pub enum Arguments {
    None,
    One(u8),
    Two(u16),
}

impl Arguments {
    pub fn get_arg1(&self) -> Option<u8> {
        match &self {
            Arguments::None => None,
            Arguments::One(arg) => Some(*arg),
            Arguments::Two(arg) => Some((arg & 0xFF) as u8),
        }
    }
    
    pub fn get_arg2(&self) -> Option<u8> {
        match &self {
            Arguments::None => None,
            Arguments::One(_) => None,
            Arguments::Two(arg) => Some((arg >> 8) as u8),
        }
    }

    pub fn to_separate_args_string(&self) -> String {
        match &self {
            Arguments::None => "     ".to_string(),
            Arguments::One(arg) => format!("{:0>2X}   ", arg),
            Arguments::Two(_) => format!("{:0>2X} {:0>2X}", self.get_arg1().unwrap(), self.get_arg2().unwrap()),
        }
    }

    pub fn combine_args(arg1: u8, arg2: u8) -> u16 {
        ((arg2 as u16) << 8) | (arg1 as u16)
    }
}

impl Display for Arguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Arguments::None => write!(f, ""),
            Arguments::One(arg) => write!(f, "{:0>2X}", arg),
            Arguments::Two(arg) => write!(f, "{:0>4X}", arg),
        }
        
    }
}

impl From<u8> for Arguments {
    fn from(byte: u8) -> Self {
        Arguments::One(byte)
    }
}

impl From<(u8, u8)> for Arguments {
    fn from(args: (u8, u8)) -> Self {
        Arguments::Two(Self::combine_args(args.0, args.1))
    }
}

impl From<u16> for Arguments {
    fn from(args: u16) -> Self {
        Arguments::Two(args)
    }
}