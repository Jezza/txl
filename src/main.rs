use std::ops::Deref;
use std::path::PathBuf;

use serde::Deserialize;
use structopt::StructOpt;
use tmux_interface::{
	AttachSession,
	NewSession,
	NewWindow,
	SplitWindow,
	TmuxInterface
};

const ORIGINAL_WINDOW_NAME: &str = "__DEFAULT__";

#[derive(Debug, Deserialize)]
struct Setup {
	file: Option<String>,
	socket_name: Option<String>,
	session: Option<Session>,
	#[serde(rename = "window")]
	windows: Vec<Window>,
}

#[derive(Debug, Deserialize, Default)]
struct Session {
	name: Option<String>,
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
}

fn main() {
	let Args {
		file,
	} = <_>::from_args();

	let input = {
		let mut path = file;

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
			Ok(value) => value,
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
		windows
	} = toml::from_str(&input).unwrap();

	let file = file.as_ref().map(Deref::deref);
	let socket_name = socket_name.as_ref().map(Deref::deref);

	let Session {
		name: session_name,
	} = session.unwrap_or_default();

	let session_name = session_name.as_ref().map(Deref::deref);

//	println!("{:#?}", windows);

	let tmux = TmuxInterface {
		file,
		socket_name,
		..Default::default()
	};

	{ // Setting up the session and whatnot.
		let has_session = tmux.has_session(session_name).unwrap();

		if has_session {
			println!("Found session... Destroying..");
			tmux.kill_session(Some(false), None, session_name).unwrap();
		}

		let has_session = tmux.has_session(session_name).unwrap();
		if has_session {
			tmux.kill_server().unwrap();
		}

		let new_session = NewSession {
			session_name,
			detached: Some(true),
			..Default::default()
		};

		let output = tmux.new_session(&new_session).unwrap();
		println!("new-session: {:?}", output);
	}

	// We rename the first window, so we can locate and remove it later.
	let output = tmux.rename_window(None, ORIGINAL_WINDOW_NAME).unwrap();
	println!("Renaming default window: {:?}", output);

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
			let output = tmux.new_window(new_window).unwrap();
			println!("new-window: {:?}", output);
		}

		for action in parse_layout(&layout) {
			println!("{:?}", action);

			let Action(direction, target, percentage) = action;

			let selected = format!("{}", target + 1);
			let percentage = (percentage * 100f32) as usize;

			let split_window = SplitWindow {
				target_pane: Some(&selected),
				vertical: Some(direction == Direction::Vertical),
				horizontal: Some(direction == Direction::Horizontal),
				percentage: Some(percentage),
				..Default::default()
			};

			let output = tmux.split_window(&split_window).unwrap();
			println!("split-window: {:?}", output);
		}

		for _pane in panes {
			// Select pane, then send-keys.
		}
	}

	// Kill the first window, as tmux just adds it, but we don't want or need it.
	let output = tmux.kill_window(None, Some(ORIGINAL_WINDOW_NAME)).unwrap();
	println!("Killing default window: {:?}", output);

	{ // We're done here, so we can attach to the session, and promptly fuck off.
		let attach_session = AttachSession {
			target_session: session_name,
			detach_other: Some(true),
			..Default::default()
		};

		println!("Attaching to session!");
		tmux.attach_session_with(&attach_session, |args| {
			println!("{:?}", args);

//			use exec::Command;
//			let mut command = Command::new("tmux");
//			command.args(&args);
//			let error = command.exec();
//			panic!("{}", error);
		});
	}
}

#[derive(Debug, PartialEq)]
enum Direction {
	Vertical,
	Horizontal,
}

#[derive(Debug)]
struct Action(Direction, u8, f32);

fn parse_layout(layout: &str) -> Vec<Action> {
	let (width, height, mut rects) = determine_rectangles(layout);

	let mut actions = vec![];

	while let Some((parent, child, ordinal)) = find_first_pair(&rects) {
		let merging = rects.remove(child);
		let remaining = &mut rects[parent];
		println!("{:?} <= {:?}", remaining.c, merging.c);

		match ordinal {
			Ordinal::South => {
				remaining.height += merging.height;
				let percentage = merging.height as f32 / height as f32;
				actions.push(Action(Direction::Vertical, parent as u8, percentage));
			}
			Ordinal::East => {
				remaining.width += merging.width;
				let percentage = merging.width as f32 / width as f32;
				actions.push(Action(Direction::Horizontal, parent as u8, percentage));
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

	for (left, rect) in rects.iter().enumerate() {
		for ordinal in ORDINALS {
			let edge = rect.edge(*ordinal);
			if let Some(right) = rects.iter().position(|r| r != rect && r.edge(ordinal.opposite()) == edge) {
				return Some((left, right, *ordinal));
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

fn determine_rectangles(layout: &str) -> (usize, usize, Vec<Rect>) {
	let (width, height, chars) = process_input(layout);

	macro_rules! point {
	    ($index:ident) => {
			{
				let x = $index % width;
				let y = $index / width;
				(x, y)
			}
	    };
	}

	let mut index = 0usize;
	let mut bit_mask = vec![false; chars.len()];

	let mut rects = vec![];

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

		let rect = Rect {
			c,
			x: x as u8,
			y: y as u8,
			width: rect_width as u8,
			height: rect_height as u8,
		};
		rects.push(rect);

		index += 1;
	}

	(width, height, rects)
}

fn process_input(layout: &str) -> (usize, usize, Vec<char>) {
	#[derive(PartialEq)]
	enum Mode {
		WidthCounting,
		HeightCounting,
		SkipWhitespace,
	}
	use Mode::*;

//	println!("======");
//	println!("{}", layout);
//	println!("======");
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
						panic!("Unexpected character: {:?} @ pos:{}", chars.last().unwrap(), chars.len());
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
		panic!("Ah, shit... expected: {}, got: {}", expected, chars.len());
	}

//	println!("======");
//	println!("Width: {}", width);
//	println!("Height: {}", height);
//	println!("{:?}", chars);
//	println!("======");

	(width, height, chars)
}


