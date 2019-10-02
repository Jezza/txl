use std::ops::Deref;

use serde::Deserialize;
use tmux_interface::NewSession;
use tmux_interface::TmuxInterface;

#[derive(Debug, Deserialize)]
struct Setup {
	file: Option<String>,
	socket_name: Option<String>,
	session: Option<Session>,
	#[serde(rename = "window")]
	windows: Vec<Window>,
}

#[derive(Default, Debug, Deserialize)]
struct Session {
	name: Option<String>,
}

#[derive(Debug, Deserialize)]
struct Window {
	name: Option<String>,
	layout: Option<String>,
	#[serde(rename = "pane")]
	panes: Option<Vec<Pane>>,
}

#[derive(Debug, Deserialize)]
struct Pane {
	name: Option<String>,
	command: Option<String>,
}

fn main0() {
	let layout = r#"
	0012
	0034
	5555
	"#;
	let actions = parse_layout(layout);
	for action in actions {
		println!("{:?}", action);
	}
}

fn main() {
	let input = r##"
        #file = "/home/amazon/.config/tmux/jezza.conf"
        socket_name = "jezza"

        [session]
        name = "Hello!"

        [[window]]
        name = "My first window!"
        layout = '''
        0012
        0034
        5555
        '''

        [[window.pane]]
        command = '''
        echo "Number 1!"
        echo "Thing?"
        '''

        [[window.pane]]
        command = '''
        echo "Number 2!"
        '''

        [[window.pane]]
        command = '''
        echo "Number 3!"
        '''
        [[window.pane]]
        command = '''
        echo "Number 4!"
        '''
        [[window.pane]]
        command = '''
        echo "Number 5!"
        '''
        [[window.pane]]
        command = '''
        echo "Number 6!"
        '''
    "##;
	let Setup {
		file,
		socket_name,
		session,
		windows
	} = toml::from_str(input).unwrap();

	let Session {
		name: session_name,
	} = session.unwrap_or_default();
	let session_name = session_name.as_ref().map(Deref::deref);

	println!("{:#?}", windows);

	let tmux = TmuxInterface {
		file: file.as_ref().map(Deref::deref),
		socket_name: socket_name.as_ref().map(Deref::deref),
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
			shell_command: Some("bash -l"),
			..Default::default()
		};

		let output = tmux.new_session(&new_session).unwrap();
		println!("Creating session: {}", output);
	}

	let output = tmux.rename_window(None, "__DEFAULT__").unwrap();
	println!("Renaming default window: {:?}", output);

	// This is where we need to build the actual layout...

	for window in windows {
		{
			// Creating window
			use tmux_interface::NewWindow;
			let new_window = NewWindow {
				detached: Some(false),
				window_name: window.name.as_ref().map(Deref::deref),
				..Default::default()
			};
			let output = tmux.new_window(new_window).unwrap();
			println!("Creating window: {}", output);
		}

		let layout = window.layout.unwrap();
		for action in parse_layout(&layout) {
			println!("{:?}", action);

			let Action {
				vertical,
				target,
				percentage,
			} = action;

			let selected = format!("{}", target + 1);
			let percentage = (percentage * 100f32) as usize;
			use tmux_interface::SplitWindow;
			let split_window = SplitWindow {
				target_pane: Some(&selected),
				vertical: Some(vertical),
				horizontal: Some(!vertical),
				percentage: Some(percentage),
				..Default::default()
			};

			let output = tmux.split_window(&split_window).unwrap();
			println!("{}", output);
		}
	}

	let output = tmux.kill_window(None, Some("__DEFAULT__")).unwrap();
	println!("Killing default window: {:?}", output);


	{
		// We're done here, so we can attach to the session, and promptly fuck off.
		use tmux_interface::AttachSession;
		let attach_session = AttachSession {
			target_session: session_name,
			detach_other: Some(true),
			..Default::default()
		};

		println!("Attaching to session!");
		tmux.attach_session_with(&attach_session, |args| {
			use exec::Command;
			println!("{:?}", args);

			let mut command = Command::new("tmux");
			command.args(&args);
			let error = command.exec();
			panic!("{}", error);
		});
	}
}

#[derive(Debug)]
struct Action {
	vertical: bool,
	target: u8,
	percentage: f32,
}

impl Action {
	fn split_v(target: u8, percentage: f32) -> Self {
		Action {
			vertical: true,
			target,
			percentage,
		}
	}

	fn split_h(target: u8, percentage: f32) -> Self {
		Action {
			vertical: false,
			target,
			percentage,
		}
	}
}

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
				actions.push(Action::split_v(parent as u8, percentage));
			}
			Ordinal::East => {
				remaining.width += merging.width;
				let percentage = merging.width as f32 / width as f32;
				actions.push(Action::split_h(parent as u8, percentage));
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
			if let Some((right, value)) = rects.iter()
				.enumerate()
				.find(|(i, r)| *r != rect && r.edge(ordinal.opposite()) == edge) {
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
	let (width, height, chars) = beautify_input(layout);

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
		let index_point = point!(index);

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

		let rect = Rect {
			c,
			x: index_point.0 as u8,
			y: index_point.1 as u8,
			width: rect_width as u8,
			height: rect_height as u8,
		};
		rects.push(rect);

		index += 1;
	}

	(width, height, rects)
}

fn beautify_input(layout: &str) -> (usize, usize, Vec<char>) {
	#[derive(PartialEq)]
	enum Mode {
		SkipStartingWhitespace,
		WidthCounting,
		HeightCounting,
		SkipWhitespace,
	}
	use Mode::*;

	println!("======");
	println!("{}", layout);
	println!("======");
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

	println!("======");
	println!("Width: {}", width);
	println!("Height: {}", height);
	println!("{:?}", chars);
	println!("======");

	(width, height, chars)
}


