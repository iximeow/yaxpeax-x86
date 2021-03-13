pub struct InstructionSink<'buf> {
    pub buf: &'buf mut [u8],
    pub offs: usize,
}

impl<'a> core::fmt::Write for InstructionSink<'a> {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for b in s.bytes() {
            if self.offs < self.buf.len() {
                self.buf[self.offs] = b;
                self.offs += 1;
            } else {
                break;
            }
        }

        Ok(())
    }
}

