use std::ops::Deref;
use std::path::PathBuf;

use serde::Deserialize;
use structopt::StructOpt;
use tmux_interface::{AttachSession, NewSession, NewWindow, SelectWindow, SendKeys, SplitWindow, TmuxInterface};

const ORIGINAL_WINDOW_NAME: &str = "__DEFAULT__";

#[derive(Debug, Deserialize)]
struct Setup {
	file: Option<String>,
	socket_name: Option<String>,
	session: Option<Session>,
	#[serde(rename = "window")]
	windows: Vec<Window>,
	rebuild: Option<bool>,
}

#[derive(Debug, Deserialize, Default)]
struct Session {
	name: Option<String>,
	select: Option<String>,
}

#[derive(Debug, Deserialize)]
struct Window {
	name: Option<String>,
	layout: String,
	#[serde(rename = "pane")]
	panes: Option<Vec<Pane>>,
}

#[derive(Debug, Deserialize)]
struct Pane {
	name: Option<String>,
	command: Option<String>,
}

#[derive(Debug, StructOpt)]
#[structopt(name = "txl", about = "A tmux layout manager.")]
struct Args {
	/// TOML file that contains a txl layout.
	#[structopt(parse(from_os_str))]
	file: PathBuf,

	/// If true, txl will destroy the previous session if it exists, and rebuild everything.
	#[structopt(long)]
	rebuild: bool,
}


macro_rules! handle {
	($expr:expr, |$err:ident| $err_handler:expr) => {{
		match $expr {
			Ok(v) => v,
			Err($err) => {
				$err_handler
			}
		}
	}};
}

fn main() {
	let Args {
		file: path,
		rebuild,
	} = <_>::from_args();

	let (path, input) = {
		let mut path = path;

		macro_rules! not {
		    ($ext:literal) => {{
		    	path.set_extension($ext);
		    	!path.exists()
		    }};
		}

		if !path.exists() && not!("txl") && not!("toml") {
			path.set_extension("");
			eprintln!("Unable to locate file: {}[.txl|.toml]", path.display());
			return;
		}

		match std::fs::read_to_string(&path) {
			Ok(value) => (path, value),
			Err(err) => {
				eprintln!("Unable to read file: {}\n{}", path.display(), err);
				return;
			}
		}
	};

	let Setup {
		file,
		socket_name,
		session,
		windows,
		rebuild: rebuilding,
	} = handle!(toml::from_str(&input), |err| {
		eprintln!("Input file (\"{}\") contains invalid toml: {}", path.display(), err);
		return;
	});

	let rebuild = rebuild || rebuilding.unwrap_or(false);

	let file = file.as_ref().map(Deref::deref);
	let socket_name = socket_name.as_ref().map(Deref::deref);

	let Session {
		name: session_name,
		select,
	} = session.unwrap_or_default();

	let session_name = session_name.as_ref().map(Deref::deref);
	let select = select.as_ref().map(Deref::deref);

//	println!("{:#?}", windows);

	let tmux = TmuxInterface {
		file,
		socket_name,
		..Default::default()
	};

	{ // Setting up the session and whatnot.
		let has_session = handle!(tmux.has_session(session_name), |err| {
			eprintln!("Unable to check if session already exists: {}", err);
			return;
		});

		if has_session {
			if !rebuild {
				// Well, we're not allowed to rebuild, so attach ourselves, and we're done.
				attach_to(&tmux, session_name);
			}

			println!("Found session... Destroying..");
			handle!(tmux.kill_session(Some(false), None, session_name), |err| {
				eprintln!("Unable to kill session with the same name: {}", err);
				return;
			});
		}

		let has_session = handle!(tmux.has_session(session_name), |err| {
			eprintln!("Unable to check if session already exists: {}", err);
			return;
		});
		if has_session {
			// I've had some weird sessions where they just keep on sticking around.
			// Stupidest solution I've found is to just kill the server... :|
			handle!(tmux.kill_server(), |err| {
				eprintln!("Unable to kill server: {}", err);
				return;
			});
		}

		let (width, height) = if let Some((w, h)) = term_size::dimensions() {
			(Some(w), Some(h))
		} else {
			(None, None)
		};

		let new_session = NewSession {
			session_name,
			detached: Some(true),
			width,
			height,
			..Default::default()
		};

		match tmux.new_session(&new_session) {
			Ok(v) => if !v.is_empty() {
				eprintln!("Unable to create new session: {}", v);
				return;
			}
			Err(err) => {
				eprintln!("Unable to create new session: {}", err);
				return;
			}
		}
	}

	// We rename the first window, so we can locate and remove it later.
	match tmux.rename_window(None, ORIGINAL_WINDOW_NAME) {
		Ok(v) => if !v.status.success() {
			eprintln!("Unable to rename default window: {:?}", v);
			return;
		}
		Err(err) => {
			eprintln!("Unable to rename default window: {}", err);
			return;
		}
	}

	// This is where we need to build the actual layout...

	for Window {
		name: window_name,
		layout,
		panes,
	} in windows {
		let window_name = window_name.as_ref().map(Deref::deref);
		let panes = panes.unwrap_or_default();

		{ // Tell tmux to create the window
			let new_window = NewWindow {
				detached: Some(false),
				window_name,
				..Default::default()
			};
			match tmux.new_window(new_window) {
				Ok(v) => if !v.is_empty() {
					eprintln!("Unable to create new window: {}", v);
					return;
				}
				Err(err) => {
					eprintln!("Unable to create new window: {}", err);
					return;
				}
			}
		}

		for Action(direction, target, percentage) in parse_layout(&layout) {
			let selected = format!("{}", target + 1);
			let percentage = (percentage * 100f32) as usize;

			let split_window = SplitWindow {
				target_pane: Some(&selected),
				vertical: Some(direction == Direction::Vertical),
				horizontal: Some(direction == Direction::Horizontal),
				percentage: Some(percentage),
				..Default::default()
			};

			match tmux.split_window(&split_window) {
				Ok(v) => if !v.is_empty() {
					eprintln!("Unable to split window: {}", v);
					return;
				}
				Err(err) => {
					eprintln!("Unable to split window: {}", err);
					return;
				}
			}
		}

		let mut target = 1;
		for pane in panes {
			let target = {
				let old = target;
				target += 1;
				old
			};

			let command = if let Some(ref value) = pane.command {
				value.deref()
			} else {
				continue;
			};

			let selected = format!("{}", target);
			let keys = vec![
				command,
				"C-m"
			];

			let send_keys = SendKeys {
				target_pane: Some(&selected),
				key: keys,
				..Default::default()
			};

			match tmux.send_keys(&send_keys) {
				Ok(v) => if !v.status.success() {
					eprintln!("Unable to send command ({}) to pane {}: {:?}", command, target, v);
				}
				Err(err) => {
					eprintln!("Unable to send command ({}) to pane {}: {}", command, target, err);
				}
			}
		}
	}

	// Kill the first window, as tmux just adds it, but we don't want or need it.
	match tmux.kill_window(None, Some(ORIGINAL_WINDOW_NAME)) {
		Ok(v) => if !v.status.success() {
			eprintln!("Unable to kill default window: {:?}", v);
			return;
		}
		Err(err) => {
			eprintln!("Unable to kill default window: {}", err);
			return;
		}
	}

	if let Some(value) = select {
		let select_window = SelectWindow {
			target_window: Some(value),
			..Default::default()
		};

		match tmux.select_window(&select_window) {
			Ok(v) => if !v.status.success() {
				eprintln!("Unable to select window: {:?}", v);
				return;
			}
			Err(err) => {
				eprintln!("Unable to select window: {}", err);
				return;
			}
		}
	}

	// We're done here, so we can attach to the session, and promptly fuck off.
	attach_to(&tmux, session_name);
}

fn attach_to(tmux: &TmuxInterface, session_name: Option<&str>) -> ! {
	let attach_session = AttachSession {
		target_session: session_name,
		detach_other: Some(true),
		..Default::default()
	};

	tmux.attach_session_with(&attach_session, |args| {
		println!("{:?}", args);

		#[cfg(any(unix, macos))]
			{
				let program = tmux.tmux.unwrap_or("tmux");

				use exec::Command;
				let mut command = Command::new(program);
				command.args(&args);
				let error = command.exec();
				panic!("{}", error);
			}
		#[cfg(not(any(unix, macos)))]
			{
				compile_error!("Windows doesn't support 'execvp'");
			}
	});

	panic!("Failed to attach to tmux session: {:?}", session_name);
}

#[derive(Debug, PartialEq)]
enum Direction {
	Vertical,
	Horizontal,
}

#[derive(Debug)]
struct Action(Direction, u8, f32);

fn parse_layout(layout: &str) -> Vec<Action> {
	let mut rects = determine_rectangles(layout);

	let mut actions = vec![];

	while let Some((parent_index, child_index, ordinal)) = find_first_pair(&rects) {
		let child = rects.remove(child_index);
		let parent = &mut rects[parent_index];
//		println!("{:?} <= {:?}", parent.c, child.c);

		match ordinal {
			Ordinal::South => {
				let old_height = child.height;
				let new_height = parent.height + child.height;
				let percentage = old_height as f32 / new_height as f32;
				parent.height = new_height;
				actions.push(Action(Direction::Vertical, parent_index as u8, percentage));
			}
			Ordinal::East => {
				let old_width = child.width;
				let new_width = parent.width + child.width;
				let percentage = old_width as f32 / new_width as f32;
				parent.width = new_width;
				actions.push(Action(Direction::Horizontal, parent_index as u8, percentage));
			}
			_ => panic!("Someone changed the ORDINALS constant..."),
		}
	}

	actions.reverse();

	actions
}

fn find_first_pair(rects: &Vec<Rect>) -> Option<(usize, usize, Ordinal)> {
	const ORDINALS: &[Ordinal] = &[
		Ordinal::South,
		Ordinal::East,
	];

	for (left_index, rect) in rects.iter().enumerate() {
		for ordinal in ORDINALS {
			let left_edge = rect.edge(*ordinal);
			if let Some(right_index) = rects.iter().position(|r| r != rect && r.edge(ordinal.opposite()) == left_edge) {
				return Some((left_index, right_index, *ordinal));
			}
		}
	}

	None
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Ordinal {
	North,
	South,
	East,
	West,
}

impl Ordinal {
	fn opposite(&self) -> Ordinal {
		match self {
			Ordinal::North => Ordinal::South,
			Ordinal::South => Ordinal::North,
			Ordinal::East => Ordinal::West,
			Ordinal::West => Ordinal::East,
		}
	}
}

#[derive(Debug, PartialEq)]
struct Rect {
	c: char,
	x: u8,
	y: u8,
	width: u8,
	height: u8,
}

impl Rect {
	fn edge(&self, direction: Ordinal) -> Edge {
		match direction {
			Ordinal::North => {
				Edge {
					x: self.x,
					y: self.y,
					length: self.width,
				}
			}
			Ordinal::East => {
				Edge {
					x: self.x + self.width,
					y: self.y,
					length: self.height,
				}
			}
			Ordinal::South => {
				Edge {
					x: self.x,
					y: self.y + self.height,
					length: self.width,
				}
			}
			Ordinal::West => {
				Edge {
					x: self.x,
					y: self.y,
					length: self.height,
				}
			}
		}
	}
}

#[derive(Debug, PartialEq)]
struct Edge {
	x: u8,
	y: u8,
	length: u8,
}

fn determine_rectangles(layout: &str) -> Vec<Rect> {
	let (width, height, chars) = sanitise_input(layout);

	macro_rules! point {
	    ($index:ident) => {{
			let x = $index % width;
			let y = $index / width;
			(x, y)
		}};
	}

	let mut index = 0usize;
	let mut rects = vec![];
	let mut bit_mask = vec![false; chars.len()];

	while index < chars.len() {
		if bit_mask[index] {
			index += 1;
			continue;
		}
		let c = chars[index];

		let rect_width = {
			let mut rect_width = width;
			for offset in 1..width {
				let right = index + offset;

				if right >= chars.len() || chars[right] != c {
					rect_width = offset;
					break;
				}
			};
			rect_width
		};

		let rect_height = {
			let mut rect_height = height;
			for offset in 1..height {
				let below = index + (offset * width);

				if below >= chars.len() || chars[below] != c {
					rect_height = offset;
					break;
				}
			}
			rect_height
		};

		for y_offset in 0..rect_height {
			for x_offset in 0..rect_width {
				let bit_index = index + x_offset + (y_offset * width);
				if chars[bit_index] != c {
					panic!("Invalid character at {:?}. [expected: {:?}, found: {:?}]", point!(bit_index), c, chars[bit_index]);
				}
				bit_mask[bit_index] = true;
			}
		}

		let (x, y) = point!(index);

		rects.push(Rect {
			c,
			x: x as u8,
			y: y as u8,
			width: rect_width as u8,
			height: rect_height as u8,
		});

		index += 1;
	}

	rects
}

fn sanitise_input(layout: &str) -> (usize, usize, Vec<char>) {
	#[derive(PartialEq)]
	enum Mode {
		SkipWhitespace,
		WidthCounting,
		HeightCounting,
	}
	use Mode::*;

	// It basically treats any whitespace as newlines...
	// If you give it something like "000\n000 000"
	// It'll think you've given it a 3x3 of '0'

	let mut mode = SkipWhitespace;
	let mut width = 0usize;
	let mut running_width = 0usize;
	let mut height = 0usize;
	let mut chars = vec![];
	for c in layout.chars() {
		if c.is_ascii_whitespace() {
			if mode == WidthCounting {
				if width == 0 {
					width = running_width;
					running_width = 0;
				} else {
					if width != running_width {
						panic!("Width's do not match! (pos:{})", chars.len());
					}
					running_width = 0;
				}
				mode = HeightCounting;
			}
			if mode == HeightCounting {
				height += 1;
				mode = SkipWhitespace;
			}
			continue;
		}
		if mode == SkipWhitespace {
			mode = WidthCounting;
		}
		if mode == WidthCounting {
			running_width += 1;
		}
		chars.push(c);
	}
	let expected = (width * height) as usize;
	if expected != chars.len() {
		panic!("Unexpected character count. [expected: {}, got: {}]", expected, chars.len());
	}

	(width, height, chars)
}


