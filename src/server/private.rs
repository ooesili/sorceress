// Sorceress
// Copyright (C) 2021  Wesley Merkel
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use super::{Command, Reply};
use rosc::{OscMessage, OscPacket, OscType};

pub struct ReplyMatcher {
    f: Box<dyn Fn(&Reply) -> bool + Send>,
}

impl ReplyMatcher {
    pub fn new<F>(f: F) -> ReplyMatcher
    where
        F: Fn(&Reply) -> bool + Send + 'static,
    {
        ReplyMatcher { f: Box::new(f) }
    }

    pub fn matches(&self, reply: &Reply) -> bool {
        (self.f)(reply)
    }
}

#[derive(Debug)]
pub struct Packet(pub OscPacket);

impl Packet {
    pub fn addr(&self) -> Option<&str> {
        match self.0 {
            OscPacket::Bundle(_) => None,
            OscPacket::Message(ref message) => Some(&message.addr),
        }
    }
}

impl Command for Packet {
    fn into_packet(self) -> Packet {
        self
    }
}

pub struct Message(OscMessage);

impl Message {
    pub fn addr(addr: impl Into<String>) -> Message {
        Message(OscMessage {
            addr: addr.into(),
            args: Vec::new(),
        })
    }

    pub fn arg<T: Into<OscType>>(mut self, arg: T) -> Message {
        self.0.args.push(arg.into());
        self
    }

    pub fn args<I, T>(mut self, args: I) -> Message
    where
        I: IntoIterator<Item = T>,
        T: Into<OscType>,
    {
        self.0.args.extend(args.into_iter().map(T::into));
        self
    }

    pub fn optional<T>(mut self, arg: Option<T>) -> Message
    where
        T: Into<OscType>,
    {
        if let Some(arg) = arg {
            self.0.args.push(arg.into());
        }
        self
    }

    pub fn into_packet(self) -> Packet {
        Packet(OscPacket::Message(self.0))
    }
}
