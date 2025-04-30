#[repr(u8)]
pub enum ParseError {
    Done, // not exactly an error, but it's convenient
    UnexpectedToken,
}

// TODO: after overthinking, I may as well just go with recursive descent.
