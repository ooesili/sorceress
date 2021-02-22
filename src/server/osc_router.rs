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

use rosc::{OscMessage, OscType};
use std::collections::HashMap;

pub struct Router<T> {
    matchers: Vec<Matcher<T>>,
}

impl<T> Default for Router<T> {
    fn default() -> Router<T> {
        Router {
            matchers: Vec::new(),
        }
    }
}

impl<T> Router<T> {
    pub fn addr(&mut self, addr: &'static str) -> &mut Matcher<T> {
        let matcher = Matcher::addr(addr);
        self.matchers.push(matcher);
        let last_index = self.matchers.len() - 1;
        &mut self.matchers[last_index]
    }

    pub fn route(&self, message: OscMessage) -> Option<T> {
        for matcher in self.matchers.iter() {
            if let Some(mut args) = matcher.match_msg(&message) {
                return matcher.handler.as_ref().expect("TODO make this impossible")(&mut args);
            }
        }
        None
    }
}

pub struct Matcher<T> {
    addr: &'static str,
    actions: Vec<MatchAction>,
    handler: Option<Box<dyn Fn(&mut MatchedArgs) -> Option<T>>>,
}

impl<T> Matcher<T> {
    fn addr(addr: &'static str) -> Matcher<T> {
        Matcher {
            addr,
            actions: vec![],
            handler: None,
        }
    }

    pub fn expect_str(&mut self, value: &'static str) -> &mut Self {
        self.actions.push(MatchAction::Expect(value.into()));
        self
    }

    pub fn capture(&mut self, name: &'static str) -> &mut Self {
        self.actions.push(MatchAction::Capture(name));
        self
    }

    pub fn capture_optional(&mut self, name: &'static str) -> &mut Self {
        self.actions.push(MatchAction::CaptureOptional(name));
        self
    }

    pub fn capture_rest(&mut self) -> &mut Self {
        self.actions.push(MatchAction::CaptureRest);
        self
    }

    pub fn handle<F>(&mut self, f: F)
    where
        F: Fn(&mut MatchedArgs) -> Option<T> + 'static,
    {
        self.handler = Some(Box::new(f))
    }

    fn match_msg(&self, message: &OscMessage) -> Option<MatchedArgs> {
        if message.addr != self.addr {
            return None;
        }

        let mut args = message.args.iter().fuse();
        let mut matched_args = MatchedArgs::default();
        for action in self.actions.iter() {
            match (action, args.next()) {
                (MatchAction::Expect(expected), Some(arg)) => {
                    if expected != arg {
                        return None;
                    }
                }
                (MatchAction::Capture(name), Some(arg)) => {
                    matched_args.args.insert(name, arg.clone());
                }
                (MatchAction::CaptureOptional(name), Some(arg)) => {
                    matched_args.args.insert(name, arg.clone());
                }
                (MatchAction::CaptureRest, arg) => {
                    matched_args.rest = Some(arg.into_iter().chain(args).cloned().collect());
                    return Some(matched_args);
                }
                (MatchAction::CaptureOptional(_), None) => {}
                (MatchAction::Expect(_), None) => return None,
                (MatchAction::Capture(_), None) => return None,
            }
        }
        if args.next().is_some() {
            return None;
        }
        Some(matched_args)
    }
}

#[derive(Debug)]
enum MatchAction {
    Expect(OscType),
    Capture(&'static str),
    CaptureOptional(&'static str),
    CaptureRest,
}

#[derive(Debug, Default)]
pub struct MatchedArgs {
    args: HashMap<&'static str, OscType>,
    rest: Option<Vec<OscType>>,
}

impl MatchedArgs {
    pub fn int(&mut self, name: &'static str) -> Option<i32> {
        match self.args.remove(name) {
            Some(OscType::Int(x)) => Some(x),
            _ => None,
        }
    }

    pub fn string(&mut self, name: &'static str) -> Option<String> {
        match self.args.remove(name) {
            Some(OscType::String(x)) => Some(x),
            _ => None,
        }
    }

    pub fn rest(&mut self) -> Option<Vec<OscType>> {
        self.rest.take()
    }
}
