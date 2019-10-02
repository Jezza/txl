### txl - tmux layout manager

[WIP] - I broke it while I was away from work, so I can't fix it today or tomorrow...  
(Currently the windows and panes are created, but the commands aren't yet sent...)

I was getting annoyed at the lack of good,
simple, clean, portable layout management for tmux.

So, I made this.

---

Declare a `txl|toml` file:

```toml
[[window]]
layout = '''
001
001
222
'''

[[window.pane]]
command = '''
echo "Main pane!"
'''

[[window.pane]]
command = '''
echo "Right pane!"
'''

[[window.pane]]
command = '''
echo "Bottom pane!"
'''
```

Install [rustup](https://rustup.rs/), then install txl:

```shell script
cargo install --git https://github.com/Jezza/txl
```

Then you can just `txl my_file`, and it'll set everything up.  
(Assuming you have tmux installed...)


