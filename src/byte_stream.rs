use std::collections::VecDeque;

use bytes::{Buf, Bytes};
use either::Either;
use typle::typle;

#[derive(Debug, Default, Clone)]
enum ByteChunks {
    #[default]
    Empty,
    One(Bytes),
    Multiple(VecDeque<Bytes>),
}

#[derive(Debug, Default, Clone)]
pub struct ByteStream {
    start: u64,
    chunks: ByteChunks,
}

impl ByteStream {
    pub fn new(position: u64) -> Self {
        Self {
            start: position,
            chunks: ByteChunks::Empty,
        }
    }

    pub fn position(&self) -> u64 {
        self.start
    }

    fn push_bytes(&mut self, bytes: Bytes) {
        if bytes.is_empty() {
            return;
        }
        self.chunks = match std::mem::take(&mut self.chunks) {
            ByteChunks::Empty => ByteChunks::One(bytes),
            ByteChunks::One(first) => ByteChunks::Multiple(VecDeque::from([first, bytes])),
            ByteChunks::Multiple(mut v) => {
                v.push_back(bytes);
                ByteChunks::Multiple(v)
            }
        }
    }

    pub fn merge(&mut self, stream: ByteStream) {
        match self.chunks {
            ByteChunks::Empty => {
                self.chunks = stream.chunks;
            }
            _ => match stream.chunks {
                ByteChunks::Empty => {}
                ByteChunks::One(bytes) => {
                    self.push_bytes(bytes);
                }
                ByteChunks::Multiple(chunks) => {
                    chunks.into_iter().for_each(|bytes| self.push_bytes(bytes));
                }
            },
        }
    }

    pub fn append(&mut self, stream: &ByteStream) {
        match &stream.chunks {
            ByteChunks::Empty => {}
            ByteChunks::One(bytes) => {
                self.push_bytes(bytes.clone());
            }
            ByteChunks::Multiple(chunks) => {
                chunks
                    .iter()
                    .for_each(|bytes| self.push_bytes(bytes.clone()));
            }
        }
    }

    // Given a slice, how many bytes match?
    pub fn common_prefix_length(&self, slice: &[u8]) -> usize {
        let mut count = 0;
        for (b1, b2) in self.iter().zip(slice) {
            if b1 == b2 {
                count += 1;
            } else {
                break;
            }
        }
        count
    }

    pub fn take_before(&mut self, pos: impl TryInto<u64>) -> Self {
        let pos = pos
            .try_into()
            .unwrap_or_else(|_e| panic!("Cannot convert position to u64"));
        let index = self.start;
        if pos == 0 {
            return ByteStream {
                start: index,
                chunks: ByteChunks::Empty,
            };
        }
        match &mut self.chunks {
            ByteChunks::Empty => {
                panic!("cannot take {pos} bytes from empty stream");
            }
            ByteChunks::One(bytes) => match pos.cmp(&(bytes.len() as u64)) {
                std::cmp::Ordering::Less => {
                    self.start += pos;
                    ByteStream {
                        start: index,
                        chunks: ByteChunks::One(bytes.split_to(pos as usize)),
                    }
                }
                std::cmp::Ordering::Equal => std::mem::replace(
                    self,
                    ByteStream {
                        start: index + pos,
                        chunks: ByteChunks::Empty,
                    },
                ),
                std::cmp::Ordering::Greater => {
                    panic!("cannot take {pos} bytes from stream");
                }
            },
            ByteChunks::Multiple(chunks) => {
                let mut nchunk = 0;
                let mut nbytes = pos;
                for bytes in chunks.iter_mut() {
                    if nbytes < bytes.len() as u64 {
                        if nchunk == 0 {
                            self.start += pos;
                            return ByteStream {
                                start: index,
                                chunks: ByteChunks::One(bytes.split_to(pos as usize)),
                            };
                        } else {
                            break;
                        }
                    } else {
                        nchunk += 1;
                        nbytes -= bytes.len() as u64;
                        if nbytes == 0 {
                            break;
                        }
                    }
                }
                assert!(nchunk != 0);
                assert!(nbytes <= usize::MAX as u64);
                if nchunk == chunks.len() {
                    if nbytes == 0 {
                        return std::mem::replace(
                            self,
                            ByteStream {
                                start: index + pos,
                                chunks: ByteChunks::Empty,
                            },
                        );
                    } else {
                        panic!("cannot take {pos} bytes from stream");
                    }
                }
                if nchunk == 1 && nbytes == 0 {
                    self.start += pos;
                    return ByteStream {
                        start: index,
                        chunks: ByteChunks::One(chunks.pop_front().unwrap()),
                    };
                }
                if nchunk == chunks.len() - 1 {
                    let bytes = if nbytes == 0 {
                        chunks.pop_back().unwrap()
                    } else {
                        chunks.back_mut().unwrap().split_off(nbytes as usize)
                    };
                    return std::mem::replace(
                        self,
                        ByteStream {
                            start: index + pos,
                            chunks: ByteChunks::One(bytes),
                        },
                    );
                }
                // Both sides will contain multiple chunks
                let mut back_chunks = chunks.split_off(nchunk);
                if nbytes > 0 {
                    chunks.push_back(back_chunks.front_mut().unwrap().split_to(nbytes as usize));
                }
                std::mem::replace(
                    self,
                    ByteStream {
                        start: index + pos,
                        chunks: ByteChunks::Multiple(back_chunks),
                    },
                )
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.chunks, ByteChunks::Empty)
    }

    pub fn fill_slice(&self, slice: &mut [u8]) -> usize {
        match &self.chunks {
            ByteChunks::Empty => 0,
            ByteChunks::One(bytes) => {
                let len = bytes.len();
                slice[..len].copy_from_slice(bytes);
                len
            }
            ByteChunks::Multiple(chunks) => {
                let mut copied = 0;
                for bytes in chunks {
                    let len = bytes.len();
                    slice[copied..][..len].copy_from_slice(bytes);
                    copied += len;
                }
                copied
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &u8> {
        match &self.chunks {
            ByteChunks::Empty => Either::Left(Either::Left(std::iter::empty())),
            ByteChunks::One(bytes) => Either::Left(Either::Right(bytes.iter())),
            ByteChunks::Multiple(v) => Either::Right(v.iter().flat_map(|bytes| bytes.iter())),
        }
    }
}

impl From<Bytes> for ByteStream {
    fn from(bytes: Bytes) -> Self {
        if bytes.is_empty() {
            ByteStream::default()
        } else {
            ByteStream {
                start: 0,
                chunks: ByteChunks::One(bytes),
            }
        }
    }
}

impl From<&'static str> for ByteStream {
    fn from(s: &'static str) -> Self {
        if s.is_empty() {
            ByteStream::default()
        } else {
            ByteStream {
                start: 0,
                chunks: ByteChunks::One(Bytes::from_static(s.as_bytes())),
            }
        }
    }
}

#[typle(Tuple for 1..=12)]
impl<T> From<T> for ByteStream
where
    T: Tuple,
    T<_>: Into<ByteStream>,
{
    fn from(value: T) -> Self {
        #[allow(unused_mut)]
        let mut byte_stream = value.0.into();
        for typle_index!(i) in 1..T::LEN {
            byte_stream.merge(value[[i]].into());
        }
        byte_stream
    }
}

impl FromIterator<ByteStream> for ByteStream {
    fn from_iter<T: IntoIterator<Item = ByteStream>>(iter: T) -> Self {
        let mut result = ByteStream::default();
        for bs in iter {
            result.merge(bs);
        }
        result
    }
}

impl Buf for ByteStream {
    fn remaining(&self) -> usize {
        match &self.chunks {
            ByteChunks::Empty => 0,
            ByteChunks::One(bytes) => bytes.len(),
            ByteChunks::Multiple(chunks) => chunks.iter().map(|bytes| bytes.len()).sum(),
        }
    }

    fn chunk(&self) -> &[u8] {
        match &self.chunks {
            ByteChunks::Empty => &[],
            ByteChunks::One(bytes) => bytes,
            ByteChunks::Multiple(chunks) => chunks.front().unwrap(),
        }
    }

    fn advance(&mut self, mut cnt: usize) {
        assert!(cnt < self.remaining());
        self.start += cnt as u64;
        match &mut self.chunks {
            ByteChunks::Empty => {}
            ByteChunks::One(bytes) => {
                bytes.advance(cnt);
                if bytes.is_empty() {
                    self.chunks = ByteChunks::Empty;
                }
            }
            ByteChunks::Multiple(chunks) => {
                loop {
                    let Some(bytes) = chunks.front_mut() else {
                        assert!(cnt == 0);
                        break;
                    };
                    if cnt < bytes.len() {
                        bytes.advance(cnt);
                        break;
                    } else {
                        cnt -= bytes.len();
                        chunks.pop_front();
                    }
                }
                match chunks.len() {
                    0 => {
                        self.chunks = ByteChunks::Empty;
                    }
                    1 => {
                        self.chunks = ByteChunks::One(chunks.pop_front().unwrap());
                    }
                    _ => {}
                }
            }
        }
    }
}

impl ToString for ByteStream {
    fn to_string(&self) -> String {
        match &self.chunks {
            ByteChunks::Empty => String::new(),
            ByteChunks::One(bytes) => String::from_utf8_lossy(bytes).into_owned(),
            ByteChunks::Multiple(chunks) => {
                let mut bytes_iter = chunks.iter();
                let mut vec = Vec::from(bytes_iter.next().unwrap().clone());
                for bytes in bytes_iter {
                    vec.extend_from_slice(bytes)
                }
                String::from_utf8_lossy(&vec).into_owned()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use bytes::Bytes;

    use super::{ByteChunks, ByteStream};

    #[test]
    fn test_single_byte_take() {
        let s = "abcdefghijklmnopqrstuvwxyz";
        for i in 0..=s.len() {
            let mut back = ByteStream::from(s);
            let front = back.take_before(i as u64);
            let (expected_front, expected_back) = s.split_at(i);
            assert_eq!(front.to_string(), expected_front);
            assert_eq!(back.to_string(), expected_back);
        }
    }

    #[test]
    fn test_multi_byte_take() {
        let s = "abcdefghijklmnopqrstuvwxyz";
        for i in 0..=s.len() {
            let mut back = ByteStream {
                start: 0,
                chunks: ByteChunks::Multiple(
                    s.as_bytes()
                        .chunks(6)
                        .map(Bytes::from_static)
                        .collect::<VecDeque<_>>(),
                ),
            };
            let front = back.take_before(i as u64);
            let (expected_front, expected_back) = s.split_at(i);
            assert_eq!(front.to_string(), expected_front);
            assert_eq!(front.position(), 0);
            assert_eq!(back.to_string(), expected_back);
            assert_eq!(back.position(), i as u64);
        }
    }
}
