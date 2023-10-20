use std::collections::VecDeque;
use std::string::FromUtf8Error;

use bytes::{Buf, Bytes};
use either::Either;

#[derive(Debug, Default, Clone)]
enum ByteChunks {
    #[default]
    Empty,
    One(Bytes),
    Multiple(VecDeque<Bytes>),
}

#[derive(Debug, Default, Clone)]
pub struct ByteStream {
    chunks: ByteChunks,
}

impl ByteStream {
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

    pub fn extend(&mut self, stream: ByteStream) {
        if stream.is_empty() {
            return;
        }
        match self.chunks {
            ByteChunks::Empty => {
                *self = stream;
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

    fn get_before(&self, mut pos: usize) -> Self {
        assert!(pos <= self.remaining());
        let mut byte_stream = ByteStream::default();
        match &self.chunks {
            ByteChunks::Empty => {}
            ByteChunks::One(bytes) => {
                byte_stream.push_bytes(bytes.slice(..pos));
            }
            ByteChunks::Multiple(chunks) => {
                for bytes in chunks {
                    if bytes.len() < pos {
                        byte_stream.push_bytes(bytes.slice(..pos));
                        break;
                    } else {
                        byte_stream.push_bytes(bytes.clone());
                        pos -= bytes.len();
                    }
                }
            }
        }
        byte_stream
    }

    pub fn take_before(&mut self, pos: usize) -> Self {
        if pos == self.remaining() {
            return std::mem::take(self);
        }
        let byte_stream = self.get_before(pos);
        self.advance(pos);
        byte_stream
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

    fn into_iter(self) -> impl Iterator<Item = u8> {
        match self.chunks {
            ByteChunks::Empty => Either::Left(Bytes::new().into_iter()),
            ByteChunks::One(bytes) => Either::Left(bytes.into_iter()),
            ByteChunks::Multiple(v) => {
                Either::Right(v.into_iter().flat_map(|bytes| bytes.into_iter()))
            }
        }
    }

    fn into_vec(self) -> Vec<u8> {
        match self.chunks {
            ByteChunks::Empty => Vec::new(),
            ByteChunks::One(bytes) => bytes.into(),
            ByteChunks::Multiple(chunks) => {
                let mut bytes_iter = chunks.into_iter();
                let mut vec = Vec::from(bytes_iter.next().unwrap());
                for bytes in bytes_iter {
                    vec.extend_from_slice(&*bytes)
                }
                vec
            }
        }
    }

    fn into_string(self) -> Result<String, FromUtf8Error> {
        String::from_utf8(self.into_vec())
    }
}

impl From<Bytes> for ByteStream {
    fn from(bytes: Bytes) -> Self {
        ByteStream {
            chunks: ByteChunks::One(bytes),
        }
    }
}

impl From<&'static str> for ByteStream {
    fn from(s: &'static str) -> Self {
        ByteStream {
            chunks: ByteChunks::One(Bytes::from_static(s.as_bytes())),
        }
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
            ByteChunks::One(bytes) => &bytes,
            ByteChunks::Multiple(chunks) => chunks.front().unwrap(),
        }
    }

    fn advance(&mut self, mut cnt: usize) {
        assert!(cnt < self.remaining());
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
                    vec.extend_from_slice(&*bytes)
                }
                String::from_utf8_lossy(&vec).into_owned()
            }
        }
    }
}
