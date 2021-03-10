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

//! An OSC interface to SuperCollider.
//!
//! This module provides a low level interface for interacting with the SuperCollider server using
//! the Open Sound Control (OSC) protocol. The primary components provided by this module are:
//!
//! * The [`Server`] type - A SuperCollider server client. Handles communication with a server,
//!   including converting messages to and from OSC.
//! * The command structs and the [`Command`] trait - The SuperCollider server is controlled by
//!   sending it command messages. This module provides a struct for every command recognized by
//!   SuperCollider. All of these commands structs implement the [`Command`] trait which the
//!   [`Server::send`] method accepts as an argument.
//! * The async commands and the [`AsyncCommand`] trait - Many commands are not immediately
//!   processed by SuperCollider but are instead handled by a background thread so that they do not
//!   steal CPU time from the main audio synthesis thread. These commands send a reply back to the
//!   client when the complete. These commands all contain the word **Asynchronous** in their
//!   documentation and implement the [`AsyncCommand`] trait in addition to the [`Command`] trait.
//!   This trait allows commands to be used with the [`Server::send_sync`] method which blocks
//!   until the command is fully processed and the corresponding reply is received by the client.
//! * The [`Reply`] type - An enum type containing all possible asynchronous replies sent by the
//!   SuperCollider server to the client.
//!
//! # Commands
//!
//! This module provides a struct every command recognized by SuperCollider. Each command struct
//! provides a [builder interface] for creating it. The required fields for each command are given
//! as positional arguments to the [constructor] (the `new` method on each type), and the optional
//! fields can be set using builder methods.
//!
//! ```
//! # use sorceress::server::BufferAllocate;
//! // Required fields
//! let buffer_number = 1;
//! let number_of_frames = 65536;
//!
//! let command = BufferAllocate::new(buffer_number, number_of_frames)
//!     // Optional field
//!     .number_of_channels(2);
//! ```
//!
//! Here is the full list of commands:
//!
//! **Note:** commands that are not yet implemented will have broken links and will not render as
//! links in a web browser.
//!
//
// This section is ordered the same way as it in the following document:
//
// https://doc.sccode.org/Reference/Server-Command-Reference.html
//
// The commented out commands have not yet been implemented. They should be uncommented in place as
// they are implemented.
//
//! ## Master Control Commands
//!
//! [`Quit`],
// [`Status`],
//! [`Notify`],
//! [`Command`],
// [`DumpOSC`],
//! [`Sync`],
//! [`ClearSched`],
//! [`Error`],
// [`Version`]
//!
//! ## Synth Definition Commands
//!
//! [`SynthDefRecv`],
// [`SynthDefLoad`],
// [`SynthDefLoadDir`],
//! [`SynthDefFree`]
//!
//! ## Node Commmands
//!
//! [`NodeFree`],
// [`NodeRun`],
//! [`NodeSet`],
// [`NodeSetRange`],
// [`NodeFill`],
// [`NodeMap`],
// [`NodeMapRange`],
// [`NodeMapAudio`],
// [`NodeMapAudioRange`],
// [`NodeBefore`],
// [`NodeAfter`],
// [`NodeQuery`],
// [`NodeTrace`],
// [`NodeOrder`]
//!
//! ## Synth Commands
//!
//! [`SynthNew`],
// [`SynthGet`],
// [`SynthGetRange`],
// [`SynthRemoveID`]
//!
//! ## Group Commands
//!
// [`GroupNew`],
// [`GroupNewParallel`],
// [`GroupHead`],
// [`GroupTail`],
//! [`GroupFreeAll`],
// [`GroupDeepFree`],
// [`GroupDumpTree`],
// [`GroupQueryTree`]
//
// ## Unit Generator Commands
//
// [`UGenCommand`]
//!
//! ## Buffer Commands
//!
//! [`BufferAllocate`],
//! [`BufferAllocateRead`],
// [`BufferAllocateReadChannel`],
//! [`BufferRead`],
// [`BufferReadChannel`],
//! [`BufferWrite`],
//! [`BufferFree`],
// [`BufferZero`],
// [`BufferSet`],
// [`BufferSetRange`],
// [`BufferFill`],
// [`BufferGen`],
//! [`BufferClose`],
//! [`BufferQuery`],
// [`BufferGet`],
// [`BufferGetRange`]
//
// ## ControlBus
//
// [`ControlBusSet`],
// [`ControlBusSetRange`],
// [`ControlBusFill`],
// [`ControlBusGet`],
// [`ControlBusGetRange`]
//
// > (2021-06-06) Omitted - `d_freeAll` - not documented
// > (2021-06-06) Omitted - `n_cmd` - not documented and commented out in the supercollider code
// > (2021-06-06) Omitted - `s_newargs` - not documented
//!
//! [UDP]: https://en.wikipedia.org/wiki/User_Datagram_Protocol
//! [builder interface]: https://rust-unofficial.github.io/patterns/patterns/builder.html
//! [constructor]: https://rust-unofficial.github.io/patterns/idioms/ctor.html

mod osc_router;
mod private;

use private::{Message, Packet, ReplyMatcher};
use rosc::{
    decoder::decode, encoder::encode, OscBundle, OscError, OscMessage, OscPacket, OscTime, OscType,
};
use std::{
    error, fmt, io,
    net::{ToSocketAddrs, UdpSocket},
    sync::{
        atomic::{AtomicI32, Ordering},
        mpsc, Arc, Mutex,
    },
    thread,
    time::{Duration, SystemTime},
};

// TODO: graceful server shutdown and change documentation on `Server::subscribe` to say that the
// sender can be dropped and receiving code should act accordingly.

/// A SuperCollider server.
///
/// `Server` is safe to clone and use concurrently by multiple threads. See [the module level
/// documentation](self) for more.
#[derive(Clone)]
pub struct Server(Arc<ServerInner>);

impl fmt::Debug for Server {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let subscribers_len = &self.0.subscribers.lock().unwrap().len();
        f.debug_struct("Server")
            .field("socket", &self.0.socket)
            .field("subscribers", &subscribers_len)
            .finish()
    }
}

struct ServerInner {
    socket: UdpSocket,
    subscribers: Mutex<Vec<mpsc::Sender<Reply>>>,

    // IDs used in /sync commands only need to be unique per client, so we do not need to persist
    // them.
    sync_id_counter: AtomicI32,
}

impl Server {
    /// Connects to an externally running server using the given UDP address.
    ///
    /// This function will not boot an SuperCollider server. You must start one separately.
    ///
    /// # Arguments
    ///
    /// * `address` - The UDP address of the SuperCollider server.
    ///
    /// # Errors
    ///
    /// Returns an error if a UDP socket cannot be created or if the UDP socket cannot connect to
    /// the `server_address`.
    pub fn connect<A: ToSocketAddrs>(server_address: A) -> Result<Server> {
        let socket = std::net::UdpSocket::bind("0.0.0.0:0")
            .map_err(|err| Error(ErrorInner::UdpBind(err)))?;
        socket
            .connect(server_address)
            .map_err(|err| Error(ErrorInner::UdpConnect(err)))?;
        let server = Server(Arc::new(ServerInner {
            socket,
            subscribers: Mutex::new(Vec::new()),
            sync_id_counter: AtomicI32::new(0),
        }));
        let reader = server.clone();
        thread::spawn(move || reader.recv_loop());
        Ok(server)
    }

    /// Subscribes to replies from the server.
    ///
    /// Returns the [`Receiver`](std::sync::mpsc::Receiver) end of an asynchronous channel that
    /// receives each reply sent by the SuperCollider server. The
    /// [`Sender`](std::sync::mpsc::Receiver) on the other end of the returned channel will never
    /// hang up first, so you can safely [`unwrap`](std::result::Result::unwrap) the errors
    /// returned by [`Receiver::recv()`](std::sync::mpsc::Receiver::recv).
    ///
    /// If you no longer wish to receive from the channel you should drop it prompty. Messages will
    /// accumulate in the channel until the `Receiver` is is dropped.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use sorceress::server::Server;
    ///
    /// let server = Server::connect("127.0.0.1:57110")?;
    /// let replies = server.subscribe();
    /// loop {
    ///     let reply = replies.recv().unwrap();
    ///     println!("reply received: {:?}", reply);
    /// }
    /// # sorceress::server::Result::Ok(())
    /// ```
    pub fn subscribe(&self) -> mpsc::Receiver<Reply> {
        let (sender, receiver) = mpsc::channel();
        self.0.subscribers.lock().unwrap().push(sender);
        receiver
    }

    /// Sends a command and waits it to complete.
    ///
    /// Many commands are **Asynchronous** meaning they are executed by the SuperCollider server on
    /// a backround thread, then send a reply back to the client when complete. This function
    /// understands which replies correspond to each command and uses that information to block
    /// until the command is complete and returns the reply.
    ///
    /// # Errors
    ///
    /// Returns an error if the command cannot be sent to the server.
    pub fn send_sync(&self, command: impl AsyncCommand) -> Result<Reply> {
        let reply_matcher = command.reply_matcher();
        let packet = command.into_packet();

        let replies = self.subscribe();
        self.send(packet)?;
        loop {
            let reply = replies.recv().unwrap();
            if reply_matcher.matches(&reply) {
                return Ok(reply);
            }
        }
    }

    /// Sends a command to the server.
    ///
    /// Sends a command to the SuperCollider server and immediately returns. This method will not
    /// wait for asynchronous commands to complete; for that, see [`send_sync`](Server::send_sync).
    ///
    /// # Errors
    ///
    /// Returns an error if:
    ///
    /// * The command cannot be encoded into an OSC packet.
    /// * The OSC packet cannot be sent to the server.
    pub fn send(&self, command: impl Command) -> Result<()> {
        log::debug!("send: {:?}", command);
        let packet = command.into_packet();
        let bytes = encode(&packet.0).map_err(|err| Error(ErrorInner::OscEncode(err)))?;
        self.0
            .socket
            .send(&bytes)
            .map_err(|err| Error(ErrorInner::Send(err)))?;
        Ok(())
    }

    fn recv_loop(self) {
        const MTU: usize = 65536;
        let mut buffer = [0_u8; MTU];

        loop {
            match self.recv(&mut buffer) {
                Ok(packet) => match packet {
                    OscPacket::Message(message) => {
                        if let Some(reply) = Reply::parse(&message) {
                            let mut subscribers = self.0.subscribers.lock().unwrap();
                            let mut offset = 0;
                            for i in 0..subscribers.len() {
                                if subscribers[i - offset].send(reply.clone()).is_err() {
                                    subscribers.swap_remove(i);
                                    offset += 1;
                                }
                            }
                        }
                    }
                    OscPacket::Bundle(bundle) => {
                        log::error!("unexpected bundle in server response: {:?}", bundle)
                    }
                },
                Err(err) => log::error!("error receiving next packet from server: {}", err),
            }
        }
    }

    fn recv(&self, buffer: &mut [u8]) -> Result<OscPacket> {
        let len = self
            .0
            .socket
            .recv(buffer)
            .map_err(|err| Error(ErrorInner::Recv(err)))?;
        let packet = decode(&buffer[..len]).map_err(|err| Error(ErrorInner::OscDecode(err)))?;
        log::debug!("recv: {:?}", packet);
        Ok(packet)
    }

    /// Clears all scheduled bundles and frees all synths.
    ///
    /// # Errors
    ///
    /// Returns an error if the any commands cannot be sent to the server.
    pub fn reset(&self) -> Result<()> {
        self.send(GroupFreeAll::new(vec![0]))?;
        self.send(ClearSched::new())?;
        self.sync()?;
        Ok(())
    }

    /// Waits for asynchronous commands to complete.
    ///
    /// Blocks until all asynchronous commands started before the current moment complete.
    ///
    /// # Errors
    ///
    /// Returns an error if the [`Sync`] command cannot be sent to the server.
    pub fn sync(&self) -> Result<()> {
        self.send_sync(Sync::new(self.next_sync_id()))?;
        Ok(())
    }

    fn next_sync_id(&self) -> i32 {
        self.0.sync_id_counter.fetch_add(1, Ordering::Relaxed)
    }
}

/// A specialized [`Result`] type for server operations.
///
/// Most of the functions and methods that can fail in this module return this type.
pub type Result<T> = std::result::Result<T, Error>;

/// The error type returned by [`Server`] operations.
#[derive(Debug)]
pub struct Error(ErrorInner);

// This will be kept private for now. We can make this enum public after we kick the tires more.
// We will have to figure out a way to hide OscError.
#[derive(Debug)]
enum ErrorInner {
    UdpBind(io::Error),
    UdpConnect(io::Error),
    Send(io::Error),
    Recv(io::Error),
    OscDecode(OscError),
    OscEncode(OscError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            ErrorInner::UdpBind(e) => write!(f, "binding to UDP socket: {}", e),
            ErrorInner::UdpConnect(e) => write!(f, "connecting UDP socket to server: {}", e),
            ErrorInner::Send(e) => write!(f, "sending message to server: {}", e),
            ErrorInner::Recv(e) => write!(f, "receiving message from server: {}", e),
            ErrorInner::OscDecode(e) => write!(f, "decoding OSC packet: {:?}", e),
            ErrorInner::OscEncode(_) => write!(f, "encoding OSC packet"),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match &self.0 {
            ErrorInner::UdpBind(e) => Some(e),
            ErrorInner::UdpConnect(e) => Some(e),
            ErrorInner::Send(e) => Some(e),
            ErrorInner::Recv(e) => Some(e),
            ErrorInner::OscDecode(_) => None,
            ErrorInner::OscEncode(_) => None,
        }
    }
}

/// An add action on the [`SynthNew`] command.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AddAction {
    /// Add the new node to the the head of the group specified by the add target ID.
    HeadOfGroup = 0,
    /// Add the new node to the the tail of the group specified by the add target ID.
    TailOfGroup = 1,
    /// Add the new node just before the node specified by the add target ID.
    BeforeNode = 2,
    /// Add the new node just after the node specified by the add target ID.
    AfterNode = 3,
    /// The new node replaces the node specified by the add target ID. The target node is freed.
    ReplaceNode = 4,
}

impl Default for AddAction {
    /// Returns `AddAction::HeadOfGroup`.
    fn default() -> AddAction {
        AddAction::HeadOfGroup
    }
}

/// A synth control's ID and value pair.
///
/// Used to set initial values for controls when creating new synths with [`SynthNew`].
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Control {
    id: ControlID,
    value: ControlValue,
}

impl Control {
    /// Creates a new control using the given control ID and value. The control ID may be specified
    /// as a [`String`], [`&str`](str), or [`i32`]. Constant control values may be specified as
    /// [`i32`] or [`f32`] values. Audio or control bus values must be constructed explicitly using
    /// the [`ControlValue::AudioBus`] or [`ControlValue::ControlBus`] enum variants.
    pub fn new(id: impl Into<ControlID>, value: impl Into<ControlValue>) -> Control {
        Control {
            id: id.into(),
            value: value.into(),
        }
    }
}

/// A range of adjacent synth controls.
///
/// Specifies values for a range of adjacent controls. Used in [`SynthNew`] and [`NodeSet`].
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ControlRange {
    start_id: ControlID,
    values: Vec<ControlValue>,
}

impl ControlRange {
    /// Create a new list of values for a range of adjcent controls.
    pub fn new<V>(
        start_id: impl Into<ControlID>,
        values: impl IntoIterator<Item = V>,
    ) -> ControlRange
    where
        V: Into<ControlValue>,
    {
        ControlRange {
            start_id: start_id.into(),
            values: values.into_iter().map(V::into).collect(),
        }
    }

    fn into_osc_args(self) -> Vec<OscType> {
        if self.values.len() == 1 {
            vec![
                self.start_id.into_osc_type(),
                self.values[0].into_osc_type(),
            ]
        } else {
            vec![
                self.start_id.into_osc_type(),
                OscType::Array(
                    self.values
                        .into_iter()
                        .map(|value| value.into_osc_type())
                        .collect(),
                ),
            ]
        }
    }
}

impl From<Control> for ControlRange {
    /// Converts to a `ControlRange` with a single value.
    fn from(control: Control) -> ControlRange {
        ControlRange {
            start_id: control.id,
            values: vec![control.value],
        }
    }
}

/// An identifier for a synth definition's control.
///
/// The controls in a synth definition can be identified by their number or by name. Used when
/// creating [`Control`] values.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ControlID {
    Index(i32),
    Name(String),
}

impl From<i32> for ControlID {
    fn from(index: i32) -> Self {
        Self::Index(index)
    }
}

impl From<String> for ControlID {
    fn from(name: String) -> Self {
        ControlID::Name(name)
    }
}

impl From<&str> for ControlID {
    fn from(name: &str) -> Self {
        ControlID::Name(name.to_owned())
    }
}

impl ControlID {
    fn into_osc_type(self) -> OscType {
        match self {
            ControlID::Index(index) => index.into(),
            ControlID::Name(name) => name.into(),
        }
    }
}

/// The value of a synth definition's control.
///
/// Used when creating [`Control`] values.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum ControlValue {
    /// Specifies an constant integer value.
    Int(i32),

    /// Specifies an constant floating point value.
    Float(f32),

    /// Specifies the number of an control bus.
    ControlBus(i32),

    /// Specifies the number of an audio bus.
    AudioBus(i32),
}

impl From<i32> for ControlValue {
    fn from(n: i32) -> ControlValue {
        ControlValue::Int(n)
    }
}

impl From<f32> for ControlValue {
    fn from(n: f32) -> ControlValue {
        ControlValue::Float(n)
    }
}

impl ControlValue {
    fn into_osc_type(self) -> OscType {
        match self {
            ControlValue::Int(x) => x.into(),
            ControlValue::Float(x) => x.into(),
            ControlValue::ControlBus(x) => format!("c{}", x).into(),
            ControlValue::AudioBus(x) => format!("a{}", x).into(),
        }
    }
}

/// A list of commands and a time tag.
///
/// An bundle contains zero or more [`Command`] values and and a time tag. The contained commands
/// should be applied at the given time tag.
#[derive(Debug, Clone, PartialEq)]
pub struct Bundle(OscBundle);

impl Bundle {
    pub fn new<I, C>(time: SystemTime, commands: I) -> Bundle
    where
        I: IntoIterator<Item = C>,
        C: Command,
    {
        Bundle(OscBundle {
            timetag: Self::osc_time(time),
            content: commands
                .into_iter()
                .map(|command| command.into_packet().0)
                .collect(),
        })
    }

    fn osc_time(time: SystemTime) -> OscTime {
        const UNIX_OFFSET: u64 = 2_208_988_800; // From RFC 5905
        const TWO_POW_32: f64 = 4294967296.0;

        let unix_time = time.duration_since(SystemTime::UNIX_EPOCH).unwrap(); // Safe absent time machines
        let unix_offset = Duration::new(UNIX_OFFSET, 0);
        let epoch_time = unix_offset + unix_time;
        let ts_secs = epoch_time.as_secs() as u32;
        let ts_nanos = epoch_time.subsec_nanos() as f64;
        let ts_frac = ((ts_nanos * TWO_POW_32) / 1.0e9).round() as u32;
        (ts_secs, ts_frac)
    }
}

impl Command for Bundle {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Packet(OscPacket::Bundle(self.0))
    }
}

/// OSC commands accepted by SuperCollider.
///
/// This trait cannot be implemented by types outside of this crate.
pub trait Command: fmt::Debug {
    #[doc(hidden)]
    fn into_packet(self) -> Packet;
}

/// OSC commands that are executed on a background thread and send a reply back to the client when
/// complete.
pub trait AsyncCommand: Command {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher;
}

// =========================================================
// ==================== Master Controls ====================
// =========================================================

/// Quit program. Exits the synthesis server.
///
/// **Asynchronous**. Replies to the sender with [`Reply::Done`] just before completion.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Quit {
    // This prevents the struct from being instantiated directly so that we can give all commands a
    // uniform API with constructors called `new`.
    _hidden: (),
}

impl Quit {
    /// Creates a new `Quit` command.
    // We don't want to implement Default here because want to keep the API consistent across all
    // commands.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Quit {
        Quit { _hidden: () }
    }
}

impl Command for Quit {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/quit").into_packet()
    }
}

impl AsyncCommand for Quit {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        ReplyMatcher::new(|reply| matches!(reply, Reply::Done))
    }
}

/// Register to receive notifications from server.
///
/// **Asynchronous**. Replies to the sender with a [`Reply::NotifyDone`] message containing a
/// client ID and an optional `max_logins` parameter when complete. If this client has
/// registered for notifications before, the client ID may be the same. Otherwise it will be a
/// new one.  Clients can use this ID in multi-client situations to avoid conflicts when
/// allocating resources such as node IDs, bus indices, and buffer numbers. `max_logins` is
/// only returned when the client ID argument is supplied in this command.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Notify {
    setting: NotifySetting,
    client_id: Option<i32>,
}

impl Notify {
    /// Creates a new `Notify` command.
    ///
    /// # Arguments
    ///
    /// * `setting` - If you give [`NotifySetting::Start`], the server will remember your return
    ///   address and send you notifications. If you give [`NotifySetting::Stop`], the server will
    ///   stop sending notifications to your address.
    pub fn new(setting: NotifySetting) -> Notify {
        Notify {
            setting,
            client_id: None,
        }
    }

    /// Set the optional client ID.
    pub fn client_id(mut self, client_id: i32) -> Notify {
        self.client_id = Some(client_id);
        self
    }
}

impl Command for Notify {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/notify")
            .arg(self.setting as i32)
            .optional(self.client_id)
            .into_packet()
    }
}

impl AsyncCommand for Notify {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        ReplyMatcher::new(|reply| matches!(reply, Reply::NotifyDone { .. }))
    }
}

/// Allocate buffer space.
///
/// Allocates zero filled buffer with the given number of channels and capacity in frames.
///
/// **Asynchronous**. Replies to the sender with a [`Reply::BufferAllocateDone`] message containing
/// the same buffer number specified in this command when complete.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BufferAllocate {
    buffer_number: i32,
    number_of_frames: i32,
    number_of_channels: i32,
    on_completion: Option<Vec<u8>>,
}

impl BufferAllocate {
    /// Creates a new `BufferAllocate` command.
    ///
    /// # Arguments
    ///
    /// * `buffer_number` - The buffer number.
    /// * `number_of_frames` - The capacity of the buffer, specified by a number of frames.
    pub fn new(buffer_number: i32, number_of_frames: i32) -> BufferAllocate {
        BufferAllocate {
            buffer_number,
            number_of_frames,
            number_of_channels: 1,
            on_completion: None,
        }
    }

    /// Number of channels. Defaults to 1.
    pub fn number_of_channels(mut self, number_of_channels: i32) -> BufferAllocate {
        self.number_of_channels = number_of_channels;
        self
    }

    /// An optional OSC message to execute on completion.
    pub fn on_completion(mut self, message: Vec<u8>) -> BufferAllocate {
        self.on_completion = Some(message);
        self
    }
}

impl Command for BufferAllocate {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/b_alloc")
            .arg(self.buffer_number)
            .arg(self.number_of_frames)
            .arg(self.number_of_channels)
            .optional(self.on_completion)
            .into_packet()
    }
}

impl AsyncCommand for BufferAllocate {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        let command_buffer_number = self.buffer_number;
        ReplyMatcher::new(move |reply| {
            matches!(reply,
                Reply::BufferAllocateDone { buffer_number }
                if *buffer_number == command_buffer_number
            )
        })
    }
}

/// Allocate buffer space and reads a sound file.
///
/// Allocates buffer with the given number of channels and up to the given numer of frames,
/// depending on the size of the file. Reads sound file data from the given starting frame in the
/// file. If the number of frames argument is less than or equal to zero, the entire file is read.
/// The sound file will determine how many channels are allocated for the buffer.
///
/// **Asynchronous**. Replies to the sender with a [`Reply::BufferAllocateReadDone`] message
/// containing the same buffer number specified in this command when complete.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BufferAllocateRead {
    buffer_number: i32,
    file_path: String,
    starting_frame: i32,
    number_of_frames: i32,
    on_completion: Option<Vec<u8>>,
}

impl BufferAllocateRead {
    /// Creates a new `BufferAllocateRead` command.
    ///
    /// # Arguments
    ///
    /// * `buffer_number` - The buffer number.
    /// * `file_path` - The path to a sound file on the SuperCollider server's filesystem.
    pub fn new(buffer_number: i32, file_path: impl Into<String>) -> BufferAllocateRead {
        BufferAllocateRead {
            buffer_number,
            file_path: file_path.into(),
            starting_frame: 0,
            number_of_frames: 0,
            on_completion: None,
        }
    }

    /// The frame number in the file where SuperCollider will begin reading. Defaults to 0.
    pub fn starting_frame(mut self, starting_frame: i32) -> BufferAllocateRead {
        self.starting_frame = starting_frame;
        self
    }

    /// The maximum number of frames to read from the file. Defaults to 0.
    pub fn number_of_frames(mut self, number_of_frames: i32) -> BufferAllocateRead {
        self.number_of_frames = number_of_frames;
        self
    }

    /// An optional OSC message to execute on completion.
    pub fn on_completion(mut self, message: Vec<u8>) -> BufferAllocateRead {
        self.on_completion = Some(message);
        self
    }
}

impl Command for BufferAllocateRead {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/b_allocRead")
            .arg(self.buffer_number)
            .arg(self.file_path)
            .arg(self.starting_frame)
            .arg(self.number_of_frames)
            .optional(self.on_completion)
            .into_packet()
    }
}

impl AsyncCommand for BufferAllocateRead {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        let command_buffer_number = self.buffer_number;
        ReplyMatcher::new(move |reply| {
            matches!(reply,
                Reply::BufferAllocateReadDone { buffer_number }
                if *buffer_number == command_buffer_number
            )
        })
    }
}

/// Read sound file data into an existing buffer.
///
/// Reads sound file data from the given starting frame in the file and writes it to the given
/// starting frame in the buffer. If number of frames is less than zero, the entire file is read.
/// If reading a file to be used by the `DiskIn` UGen then you will want to set `leave_file_open`
/// to one, otherwise set it to zero.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BufferRead {
    buffer_number: i32,
    file_path: String,
    file_starting_frame: i32,
    number_of_frames: i32,
    buffer_starting_frame: i32,
    leave_file_open: i32,
    on_completion: Option<Vec<u8>>,
}

impl BufferRead {
    /// Creates a new `BufferRead` command.
    ///
    /// * `buffer_number` - The buffer number.
    /// * `file_path` - The path to a sound file on the SuperCollider server's filesystem.
    pub fn new(buffer_number: i32, file_path: impl Into<String>) -> BufferRead {
        BufferRead {
            buffer_number,
            file_path: file_path.into(),
            file_starting_frame: 0,
            number_of_frames: -1,
            buffer_starting_frame: 0,
            leave_file_open: 0,
            on_completion: None,
        }
    }

    /// The frame number in the file where SuperCollider will begin reading data.
    pub fn file_starting_frame(mut self, frame_number: i32) -> BufferRead {
        self.file_starting_frame = frame_number;
        self
    }

    /// The number of frames to read from the file.
    pub fn number_of_frames(mut self, number_of_frames: i32) -> BufferRead {
        self.number_of_frames = number_of_frames;
        self
    }

    /// The frame number in the buffer where SuperCollider will begin writing data.
    pub fn buffer_starting_frame(mut self, frame_number: i32) -> BufferRead {
        self.buffer_starting_frame = frame_number;
        self
    }

    /// Leave the file open after reading. This is only useful in combination with the `DiskIn` UGen.
    pub fn leave_file_open(mut self) -> BufferRead {
        self.leave_file_open = 1;
        self
    }

    /// An optional OSC message to execute on completion.
    pub fn on_completion(mut self, message: Vec<u8>) -> BufferRead {
        self.on_completion = Some(message);
        self
    }
}

impl Command for BufferRead {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/b_read")
            .arg(self.buffer_number)
            .arg(self.file_path)
            .arg(self.file_starting_frame)
            .arg(self.number_of_frames)
            .arg(self.buffer_starting_frame)
            .arg(self.leave_file_open)
            .optional(self.on_completion)
            .into_packet()
    }
}

impl AsyncCommand for BufferRead {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        let command_buffer_number = self.buffer_number;
        ReplyMatcher::new(move |reply| {
            matches!(reply,
                Reply::BufferReadDone { buffer_number }
                if *buffer_number == command_buffer_number
            )
        })
    }
}

/// Close soundfile.
///
/// After using a buffer with the `DiskOut` UGen, close the soundfile and write header
/// information.
///
/// **Asynchronous**. Replies to the sender with a [`Reply::BufferCloseDone`] message when
/// complete.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BufferClose {
    buffer_number: i32,
    on_completion: Option<Vec<u8>>,
}

impl BufferClose {
    /// Creates a new `BufferClose` command.
    ///
    /// # Arguments
    ///
    /// * `buffer_number` - The buffer number.
    pub fn new(buffer_number: i32) -> BufferClose {
        BufferClose {
            buffer_number,
            on_completion: None,
        }
    }

    /// An optional OSC message to execute on completion.
    pub fn on_completion(mut self, message: Vec<u8>) -> BufferClose {
        self.on_completion = Some(message);
        self
    }
}

impl Command for BufferClose {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/b_close")
            .arg(self.buffer_number)
            .optional(self.on_completion)
            .into_packet()
    }
}

impl AsyncCommand for BufferClose {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        let command_buffer_number = self.buffer_number;
        ReplyMatcher::new(move |reply| {
            matches!(reply,
                Reply::BufferCloseDone { buffer_number }
                if *buffer_number == command_buffer_number
            )
        })
    }
}

/// Get information on previously allocated buffers.
///
/// Replies to the sender with a [`Reply::BufferInfo`] message containing information on each
/// buffer specified in the command. See the documentation of [`Reply::BufferInfo`] and
/// [`BufferInfo`] for more details.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BufferQuery {
    buffer_numbers: Vec<i32>,
}

impl BufferQuery {
    /// Creates a new `BufferQuery` command.
    ///
    /// # Arguments
    ///
    /// * `buffer_numbers` - The numbers of the buffers to query. Information on all of the
    /// specified buffers will be returned in a single [`Reply::BufferInfo`] message.
    pub fn new(buffer_numbers: impl IntoIterator<Item = i32>) -> BufferQuery {
        BufferQuery {
            buffer_numbers: buffer_numbers.into_iter().collect(),
        }
    }
}

impl Command for BufferQuery {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/b_query")
            .args(self.buffer_numbers)
            .into_packet()
    }
}

impl AsyncCommand for BufferQuery {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        let command_buffer_numbers = self.buffer_numbers.clone();
        ReplyMatcher::new(move |reply| {
            matches!(reply,
                Reply::BufferInfo { buffers }
                if buffers.iter().map(|buffer| &buffer.buffer_number).eq(&command_buffer_numbers)
            )
        })
    }
}

/// An reply to an asynchronous command.
///
/// If a command's description contains the word **Asynchronous**, then that command will be passed
/// to a background thread to complete so as not to steal CPU time from the audio synthesis thread.
/// All asynchronous commands send a reply to the client when they complete. `Reply` enumerates all
/// of the possible asynchronous replies.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
#[non_exhaustive]
pub enum Reply {
    /// Sent in reponse to asynchronous commands where no additional information is required.
    Done,

    /// Sent in response to [`Notify`].
    NotifyDone {
        /// Clients can use this ID in multi-client situations to avoid conflicts when allocating
        /// resources such as node IDs, bus indices, and buffer numbers.
        client_id: i32,

        /// Only returned when the client ID argument is supplied in this command. maxLogins is not
        /// supported by supernova.
        max_logins: Option<i32>,
    },

    /// Sent in response to [`Sync`].
    Synced {
        /// The unique ID sent with the corresponding [`Sync`] command.
        id: i32,
    },

    /// Sent in response to [`SynthDefRecv`].
    SynthDefRecvDone,

    /// Sent in response to [`BufferAllocate`].
    BufferAllocateDone {
        /// Identifies which buffer just finished allocating.
        buffer_number: i32,
    },

    /// Sent in response to [`BufferAllocateRead`].
    BufferAllocateReadDone {
        /// Identifies which buffer just finished being allocated and read.
        buffer_number: i32,
    },

    /// Sent in response to [`BufferRead`].
    BufferReadDone {
        /// Identifies which buffer just finished being read.
        buffer_number: i32,
    },

    /// Sent in response to [`BufferWrite`].
    BufferWriteDone {
        /// Identifies which buffer just finished being allocated and read.
        buffer_number: i32,
    },

    /// Sent in response to [`BufferFree`].
    BufferFreeDone {
        /// Identifies which buffer was just freed.
        buffer_number: i32,
    },

    /// Sent in response to [`BufferClose`].
    BufferCloseDone {
        /// Identifies which buffer was just closed.
        buffer_number: i32,
    },

    /// Sent in response to [`BufferQuery`].
    BufferInfo {
        /// Information on all of the buffer numbers specificed in the query command.
        buffers: Vec<BufferInfo>,
    },

    /// An error occured.
    #[non_exhaustive]
    Fail {
        /// The name of the command that failed.
        command: String,
        /// The error message.
        error: String,
        // TODO: this command can take an optional 3rd "other" type argument. We should figure out
        // what possible values can be given and handle them.
    },
}

impl Reply {
    fn parse(message: &OscMessage) -> Option<Reply> {
        let mut router = osc_router::Router::default();
        router.addr("/done").handle(|_| Some(Reply::Done));
        router
            .addr("/done")
            .expect_str("/notify")
            .capture("client_id")
            .capture_optional("max_logins")
            .handle(|args| {
                Some(Reply::NotifyDone {
                    client_id: args.int("client_id")?,
                    max_logins: args.int("max_logins"),
                })
            });
        router.addr("/synced").capture("id").handle(|args| {
            Some(Reply::Synced {
                id: args.int("id")?,
            })
        });
        router
            .addr("/done")
            .expect_str("/d_recv")
            .handle(|_| Some(Reply::SynthDefRecvDone));
        router
            .addr("/done")
            .expect_str("/b_alloc")
            .capture("buffer_number")
            .handle(|args| {
                Some(Reply::BufferAllocateDone {
                    buffer_number: args.int("buffer_number")?,
                })
            });
        router
            .addr("/done")
            .expect_str("/b_allocRead")
            .capture("buffer_number")
            .handle(|args| {
                Some(Reply::BufferAllocateReadDone {
                    buffer_number: args.int("buffer_number")?,
                })
            });
        router
            .addr("/done")
            .expect_str("/b_read")
            .capture("buffer_number")
            .handle(|args| {
                Some(Reply::BufferReadDone {
                    buffer_number: args.int("buffer_number")?,
                })
            });
        router
            .addr("/done")
            .expect_str("/b_write")
            .capture("buffer_number")
            .handle(|args| {
                Some(Reply::BufferWriteDone {
                    buffer_number: args.int("buffer_number")?,
                })
            });
        router
            .addr("/done")
            .expect_str("/b_free")
            .capture("buffer_number")
            .handle(|args| {
                Some(Reply::BufferFreeDone {
                    buffer_number: args.int("buffer_number")?,
                })
            });
        router
            .addr("/done")
            .expect_str("/b_close")
            .capture("buffer_number")
            .handle(|args| {
                Some(Reply::BufferCloseDone {
                    buffer_number: args.int("buffer_number")?,
                })
            });
        router.addr("/b_info").capture_rest().handle(|args| {
            let buffers = args.rest()?
            .chunks(4)
            .map(|chunk| {
                match *chunk {
                    [
                        OscType::Int(buffer_number),
                        OscType::Int(number_of_frames),
                        OscType::Int(number_of_channels),
                        OscType::Float(sample_rate),
                    ] => Some(BufferInfo {
                        buffer_number,
                        number_of_frames,
                        number_of_channels,
                        sample_rate,
                    }),
                    _ => None,
                }
            }).collect::<Option<Vec<_>>>()?;
            Some(Reply::BufferInfo { buffers })
        });
        router
            .addr("/fail")
            .capture("command")
            .capture("error")
            .handle(|args| {
                Some(Reply::Fail {
                    command: args.string("command")?,
                    error: args.string("error")?,
                })
            });

        router.route(message.clone())
    }
}

/// A setting on [`Notify`].
///
/// See the documentation on [`Notify`] for an explanation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NotifySetting {
    /// Start sending notifications to the sender.
    Start = 1,

    /// Stop sending notifications to the sender.
    Stop = 0,
}

//
// ========== Master Controls ==========
//

/// Notify when async commands have completed.
///
/// Replies with a [`Reply::Synced`] message all asynchronous commands received before this one
/// have completed. The reply will contain the sent unique ID.
///
/// **Asynchronous**. Replies to the sender with a [`Reply::Synced`] containing the sent unique ID
/// message when complete.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Sync {
    id: i32,
}

impl Sync {
    /// Notify when async commands have completed.
    ///
    /// # Arguments
    /// * `id` - A unique number identifying this command.
    pub fn new(id: i32) -> Sync {
        Sync { id }
    }
}

impl Command for Sync {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/sync").arg(self.id).into_packet()
    }
}

impl AsyncCommand for Sync {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        let command_id = self.id;
        ReplyMatcher::new(move |reply| {
            matches!(reply,
                Reply::Synced { id }
                if *id == command_id
            )
        })
    }
}

/// Clear all scheduled bundles.
///
/// Removes all bundles from the scheduling queue.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ClearSched {
    // This prevents the struct from being instantiated directly so that we can give all commands a
    // uniform API with constructors called `new`.
    _hidden: (),
}

impl ClearSched {
    /// Creates a new `ClearSched` command.
    // We don't want to implement Default here because want to keep the API consistent across all
    // commands.
    #[allow(clippy::new_without_default)]
    pub fn new() -> ClearSched {
        ClearSched { _hidden: () }
    }
}

impl Command for ClearSched {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/clearSched").into_packet()
    }
}

//
// ========== Synth Definition Commands ==========
//

/// Receive a synth definition file.
///
/// Loads a file of synth definitions from a buffer in the message. Resident definitions with the
/// same names are overwritten.
///
/// **Asynchronous**. Replies to the sender with a [`Reply::Done`] message when complete.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct SynthDefRecv {
    data: Vec<u8>,
    on_completion: Option<Vec<u8>>,
}

impl SynthDefRecv {
    /// Creates a new `SynthDefRecv` command.
    ///
    /// # Arguments
    ///
    /// * `data` - A serialized set of synth definition in the [SynthDef2 file format].
    ///
    /// [SynthDef2 file format]: https://doc.sccode.org/Reference/Synth-Definition-File-Format.html
    pub fn new(data: &[u8]) -> SynthDefRecv {
        SynthDefRecv {
            data: data.to_vec(),
            on_completion: None,
        }
    }

    /// An optional OSC message to execute on completion.
    pub fn on_completion(mut self, message: Vec<u8>) -> SynthDefRecv {
        self.on_completion = Some(message);
        self
    }
}

impl Command for SynthDefRecv {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/d_recv")
            .arg(self.data)
            .optional(self.on_completion)
            .into_packet()
    }
}

impl AsyncCommand for SynthDefRecv {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        ReplyMatcher::new(move |reply| matches!(reply, Reply::SynthDefRecvDone))
    }
}

/// Delete a synth definition.
///
/// Removes a synth definition by name. The definition is removed immediately, and SuperCollider
/// does not wait for synth nodes based on that definition to end. Multiple synth definition names
/// may be specified in a single command.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct SynthDefFree {
    names: Vec<String>,
}

impl SynthDefFree {
    /// Creates a new `SynthDefFree` command.
    ///
    /// # Arguments
    ///
    /// * `names` - The names of the synth definitions to free.
    pub fn new<I, S>(names: I) -> SynthDefFree
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        SynthDefFree {
            names: names.into_iter().map(|s| s.into()).collect(),
        }
    }
}

impl Command for SynthDefFree {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/d_free").args(self.names).into_packet()
    }
}

//
// ========== Node Commands ==========
//

/// Delete a node.
///
/// Stops a node abruptly, removes it from its group, and frees its memory. Multiple node IDs may
/// be specified in a single command. Using this method can cause a click if the node is not silent
/// at the time it is freed.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct NodeFree {
    node_ids: Vec<i32>,
}

impl NodeFree {
    /// Creates a new `NodeFree` command.
    ///
    /// # Arguments
    ///
    /// * `node_ids` - The IDs of the nodes to free.
    pub fn new(node_ids: impl IntoIterator<Item = i32>) -> NodeFree {
        NodeFree {
            node_ids: node_ids.into_iter().collect(),
        }
    }
}

impl Command for NodeFree {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/n_free").args(self.node_ids).into_packet()
    }
}

/// Set a node's control value(s).
///
/// Takes a list of pairs of control IDs and values and sets the controls to those values. If the
/// node is a group, then it sets the controls of every node in the group.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct NodeSet {
    node_id: i32,
    controls: Vec<ControlRange>,
}

impl NodeSet {
    /// Creates a new `NodeSet` command.
    ///
    /// # Arguments
    ///
    /// * `node_id` - The ID of the node that this command will affect.
    /// * `controls` - Controls to set on the synth.
    ///
    /// Each item in `controls` can be one of the following types:
    ///
    /// * [`Control`] - a control index or name and a value.
    /// * [`ControlRange`] - a starting index or name and a range of values. The values are applied
    ///   sequentially starting at the indexed or named control.
    pub fn new<C>(node_id: i32, controls: impl IntoIterator<Item = C>) -> NodeSet
    where
        C: Into<ControlRange>,
    {
        NodeSet {
            node_id,
            controls: controls.into_iter().map(C::into).collect(),
        }
    }
}

impl Command for NodeSet {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/n_set")
            .arg(self.node_id)
            .args(
                self.controls
                    .into_iter()
                    .flat_map(ControlRange::into_osc_args),
            )
            .into_packet()
    }
}

//
// ========== Synth Commands ==========
//

/// Create a new synth.
///
/// Create a new synth from a synth definition, give it an ID, and add it to the tree of nodes.
/// There are four ways to add the node to the tree as determined by the [`AddAction`] argument.
/// Controls may be set when creating the synth. The control arguments are the same as for the
/// [`NodeSet`] command.
///
/// If you send `SynthNew` with a synth ID of -1, then the server will generate an ID for you. The
/// server reserves all negative IDs. Since you don't know what the ID is, you cannot talk to this
/// node directly later. So this is useful for nodes that are of finite duration and that get the
/// control information they need from arguments and buses or messages directed to their group. In
/// addition no notifications are sent when there are changes of state for this node, such as
/// `/go`, `/end`, `/on`, or `/off`.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct SynthNew {
    synthdef_name: String,
    synth_id: i32,
    add_action: AddAction,
    add_target_id: i32,
    controls: Vec<ControlRange>,
}

impl SynthNew {
    /// Creates a new `SynthNew` command.
    ///
    /// # Arguments
    ///
    /// * `synthdef_name` - The name of the synth definition to use.
    /// * `add_target_id` - The ID used by [`add_action`](SynthNew::add_action). See [`AddAction`]
    ///   for more details.
    pub fn new(synthdef_name: impl Into<String>, add_target_id: i32) -> SynthNew {
        SynthNew {
            synthdef_name: synthdef_name.into(),
            synth_id: -1,
            add_target_id,
            controls: Vec::new(),
            add_action: AddAction::default(),
        }
    }

    /// Set controls on the crated synth.
    ///
    /// Each item in `controls` can be one of the following types:
    ///
    /// * [`Control`] - a control index or name and a value.
    /// * [`ControlRange`] - a starting index or name and a range of values. The values are applied
    ///   sequentially starting at the indexed or named control.
    pub fn controls<C>(mut self, controls: impl IntoIterator<Item = C>) -> SynthNew
    where
        C: Into<ControlRange>,
    {
        self.controls = controls.into_iter().map(C::into).collect();
        self
    }

    /// Sets the add action. See [`AddAction`] for more details. Defaults to
    /// [`AddAction::HeadOfGroup`].
    pub fn add_action(mut self, add_action: AddAction) -> SynthNew {
        self.add_action = add_action;
        self
    }

    /// Set the synth ID to create. Defaults to -1 which tells the server to generate an ID.
    pub fn synth_id(mut self, synth_id: i32) -> SynthNew {
        self.synth_id = synth_id;
        self
    }
}

impl Command for SynthNew {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/s_new")
            .arg(self.synthdef_name)
            .arg(self.synth_id)
            .arg(self.add_action as i32)
            .arg(self.add_target_id)
            .args(
                self.controls
                    .into_iter()
                    .flat_map(ControlRange::into_osc_args),
            )
            .into_packet()
    }
}

//
// ========== Group Commands ==========
//

// GroupNew
// GroupNewParallel
// GroupHead
// GroupTail

/// Deletes all nodes in a group.
///
/// Frees all nodes in a group, releasing their IDs. Multiple groups may be specified in a single
/// command.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct GroupFreeAll {
    group_ids: Vec<i32>,
}

impl GroupFreeAll {
    /// Creates a new `GroupFreeAll` command.
    ///
    /// # Arguments
    ///
    /// * `group_ids` - The IDs of the groups to delete.
    pub fn new(group_ids: impl IntoIterator<Item = i32>) -> GroupFreeAll {
        GroupFreeAll {
            group_ids: group_ids.into_iter().collect(),
        }
    }
}

impl Command for GroupFreeAll {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/g_freeAll")
            .args(self.group_ids)
            .into_packet()
    }
}

// GroupDeepFree
// GroupDumpTree
// GroupQueryTree

//
// ========== Buffer Commands ==========
//

/// Write sound file data.
///
/// Writes the contents of a buffer to a sound file on the SuperCollider server's filesystem.
///
/// Not all combinations of header format and sample format are possible. If the number of frames
/// is less than zero, all samples from the starting frame to the end of the buffer are written. If
/// opening a file to be used by `DiskOut` UGen then you will want to call `leave_file_open()`. If
/// `leave_file_open()` is called then the file is created, but no frames are written until the
/// `DiskOut` UGen does so.
///
/// **Asynchronous**. Replies to the sender with a [`Reply::BufferWriteDone`] message when
/// complete.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BufferWrite {
    buffer_number: i32,
    file_path: String,
    header_format: HeaderFormat,
    sample_format: SampleFormat,
    number_of_frames: i32,
    starting_frame: i32,
    leave_file_open: i32,
    on_completion: Option<Vec<u8>>,
}

impl BufferWrite {
    /// Creates a new `BufferWrite` command.
    ///
    /// # Arguments
    ///
    /// * `buffer_number` - The buffer number.
    /// * `file_path` - The path to a sound file on the SuperCollider server's filesystem.
    /// * `header_format` - The header format of the resulting audio file.
    pub fn new(
        buffer_number: i32,
        file_path: impl Into<String>,
        header_format: HeaderFormat,
        sample_format: SampleFormat,
    ) -> BufferWrite {
        BufferWrite {
            buffer_number,
            file_path: file_path.into(),
            header_format,
            sample_format,
            number_of_frames: -1,
            starting_frame: 0,
            leave_file_open: 0,
            on_completion: None,
        }
    }

    /// The number of frames to write. Defaults to writing all frames from the starting frame to
    /// end of the buffer.
    pub fn number_of_frames(mut self, number_of_frames: i32) -> BufferWrite {
        self.number_of_frames = number_of_frames;
        self
    }

    /// The frame number in the buffer where SuperCollider will begin reading data. Defaults to 0.
    pub fn starting_frame(mut self, starting_frame: i32) -> BufferWrite {
        self.starting_frame = starting_frame;
        self
    }

    /// Leave the file open after writing. This is only useful in combination with the
    /// [`DiskOut`](crate::ugen::DiskOut) UGen.
    pub fn leave_file_open(mut self) -> BufferWrite {
        self.leave_file_open = 1;
        self
    }

    /// An optional OSC message to execute on completion.
    pub fn on_completion(mut self, message: Vec<u8>) -> BufferWrite {
        self.on_completion = Some(message);
        self
    }
}

impl Command for BufferWrite {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/b_write")
            .arg(self.buffer_number)
            .arg(self.file_path)
            .arg(self.header_format.to_string())
            .arg(self.sample_format.to_string())
            .arg(self.number_of_frames)
            .arg(self.starting_frame)
            .arg(self.leave_file_open)
            .optional(self.on_completion)
            .into_packet()
    }
}

impl AsyncCommand for BufferWrite {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        let command_buffer_number = self.buffer_number;
        ReplyMatcher::new(move |reply| {
            matches!(reply,
                Reply::BufferWriteDone { buffer_number }
                if *buffer_number == command_buffer_number
            )
        })
    }
}

/// Free buffer data.
///
/// Frees buffer space allocated for this buffer and releases its ID.
///
/// **Asynchronous**. Replies to the sender with a [`Reply::BufferFreeDone`] message when complete.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct BufferFree {
    buffer_number: i32,
    on_completion: Option<Vec<u8>>,
}

impl BufferFree {
    /// Creates a new `BufferFree` command.
    ///
    /// # Arguments
    ///
    /// - `buffer_number` - The buffer number.
    pub fn new(buffer_number: i32) -> BufferFree {
        BufferFree {
            buffer_number,
            on_completion: None,
        }
    }

    /// An optional OSC message to execute on completion.
    pub fn on_completion(mut self, message: Vec<u8>) -> BufferFree {
        self.on_completion = Some(message);
        self
    }
}

impl Command for BufferFree {
    #[doc(hidden)]
    fn into_packet(self) -> Packet {
        Message::addr("/b_free")
            .arg(self.buffer_number)
            .optional(self.on_completion)
            .into_packet()
    }
}

impl AsyncCommand for BufferFree {
    #[doc(hidden)]
    fn reply_matcher(&self) -> ReplyMatcher {
        let command_buffer_number = self.buffer_number;
        ReplyMatcher::new(move |reply| {
            matches!(reply,
                Reply::BufferFreeDone { buffer_number }
                if *buffer_number == command_buffer_number
            )
        })
    }
}

//
// ========== Shared Response Types ==========
//

/// Information on a specific buffer as obtained by [`BufferQuery`].
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct BufferInfo {
    /// The buffer number.
    pub buffer_number: i32,

    /// The capacity of the buffers, specified by a number of frames.
    pub number_of_frames: i32,

    /// The number of channels.
    pub number_of_channels: i32,

    /// The sample rate of the buffer in hertz.
    pub sample_rate: f32,
}

/// An audio file header format.
///
/// This is used by the [`BufferWrite`] command.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HeaderFormat {
    Aiff,
    Next,
    Wav,
    Icram,
    Raw,
}

impl fmt::Display for HeaderFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HeaderFormat::Aiff => write!(f, "aiff"),
            HeaderFormat::Next => write!(f, "next"),
            HeaderFormat::Wav => write!(f, "wav"),
            HeaderFormat::Icram => write!(f, "icram"),
            HeaderFormat::Raw => write!(f, "raw"),
        }
    }
}

/// An audio file sample format.
///
/// This is used by the [`BufferWrite`] command.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SampleFormat {
    Int8,
    Int16,
    Int24,
    Int32,
    Float,
    Double,
    Mulaw,
    Alaw,
}

impl fmt::Display for SampleFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SampleFormat::Int8 => write!(f, "int8"),
            SampleFormat::Int16 => write!(f, "int16"),
            SampleFormat::Int24 => write!(f, "int24"),
            SampleFormat::Int32 => write!(f, "int32"),
            SampleFormat::Float => write!(f, "float"),
            SampleFormat::Double => write!(f, "double"),
            SampleFormat::Mulaw => write!(f, "mulaw"),
            SampleFormat::Alaw => write!(f, "alaw"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_replies() {
        assert_eq!(reply("/done", no_args()), Some(Reply::Done));
        assert_eq!(
            reply("/done", vec![arg("/notify"), arg(1)]),
            Some(Reply::NotifyDone {
                client_id: 1,
                max_logins: None,
            })
        );
        assert_eq!(
            reply("/done", vec![arg("/notify"), arg(2), arg(3)]),
            Some(Reply::NotifyDone {
                client_id: 2,
                max_logins: Some(3),
            })
        );
        assert_eq!(reply("/synced", vec![1]), Some(Reply::Synced { id: 1 }));
        assert_eq!(
            // TODO: this is not documented in the server command reference
            reply("/done", vec![arg("/d_recv")]),
            Some(Reply::SynthDefRecvDone)
        );
        assert_eq!(
            reply("/done", vec![arg("/b_alloc"), arg(1)]),
            Some(Reply::BufferAllocateDone { buffer_number: 1 })
        );
        assert_eq!(
            reply("/done", vec![arg("/b_allocRead"), arg(1)]),
            Some(Reply::BufferAllocateReadDone { buffer_number: 1 })
        );
        assert_eq!(
            reply("/done", vec![arg("/b_read"), arg(1)]),
            Some(Reply::BufferReadDone { buffer_number: 1 })
        );
        assert_eq!(
            reply("/done", vec![arg("/b_write"), arg(1)]),
            Some(Reply::BufferWriteDone { buffer_number: 1 })
        );
        assert_eq!(
            reply("/done", vec![arg("/b_free"), arg(1)]),
            Some(Reply::BufferFreeDone { buffer_number: 1 })
        );
        assert_eq!(
            reply("/done", vec![arg("/b_close"), arg(1)]),
            Some(Reply::BufferCloseDone { buffer_number: 1 })
        );
        assert_eq!(
            reply("/b_info", vec![arg(1), arg(2), arg(3), OscType::Float(4.0)]),
            Some(Reply::BufferInfo {
                buffers: vec![BufferInfo {
                    buffer_number: 1,
                    number_of_frames: 2,
                    number_of_channels: 3,
                    sample_rate: 4.0,
                }]
            })
        );
        assert_eq!(
            reply("/fail", vec![arg("/cmdName"), arg("oops")]),
            Some(Reply::Fail {
                command: "/cmdName".to_owned(),
                error: "oops".to_owned(),
            })
        );
    }

    fn no_args() -> Vec<OscType> {
        Vec::new()
    }

    fn reply<I, T>(addr: &'static str, args: I) -> Option<Reply>
    where
        I: IntoIterator<Item = T>,
        T: Into<OscType>,
    {
        Reply::parse(&OscMessage {
            addr: addr.to_owned(),
            args: args.into_iter().map(T::into).collect(),
        })
    }

    fn arg(x: impl Into<OscType>) -> OscType {
        x.into()
    }
}
