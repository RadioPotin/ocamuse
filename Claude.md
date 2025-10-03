# Ocamuse - OCaml Assistant for MUSic Exploration

## Project Overview

**Ocamuse** is an interactive terminal-based GUI application for learning music theory on fretted instruments (guitar, bass, etc.). Built entirely in OCaml using the lambda-term TUI library, it provides real-time visualization of scales, modes, chords, arpeggios, and chord diagrams directly in your terminal.

**Author**: Dario Pinto <laradiopotin@gmail.com>
**License**: ISC
**Status**: Active development

*"This is a passion project I've been thinking about for a long time to help
myself out with figuring out the more annoying concepts of music theory lol. I
hope it might help someone else out there, and will definitely continue
expanding this further with time. Thank you! - Dario"

---

## For Guitarists: Quick Start

### What Does Ocamuse Do?

Ocamuse helps you **visualize music theory concepts** on your fretboard:

- **View scales and modes** across the entire fretboard
- **See chord shapes** with diagrams showing finger positions
- **Explore arpeggios** with notes highlighted in patterns
- **Switch between tunings** (standard, drop D, open tunings)
- **Color-coded visualization** to understand note relationships

### Features for Musicians

#### 1. Multiple Fretboard Views
- **Plain View** - Compact, linear representation
- **Fretted View** - Classic chord diagram style with boxes
- **Interline View** - Guitar tab-style with horizontal lines

#### 2. Scale and Mode Explorer
- Visualize all 7 diatonic modes (Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian)
- See the entire scale across the fretboard with color-coded degrees
- Understand interval relationships visually

#### 3. Chord Diagrams
- Display all diatonic chords in your current key
- View multiple voicings for each chord
- Navigate different inversions
- Fret numbers show you exactly where to play

#### 4. Arpeggio Mode
- Highlight arpeggio patterns for diatonic chords
- See note positions across multiple octaves
- Practice patterns in any key

#### 5. Color Themes

#### 6. Tuning Presets

### Understanding the Display

#### Context Panel (Right)
Shows your current musical context:
- **Tuning** - Current string tuning
- **Key** - Root note and mode name (e.g., "E Dorian")
- **Scale** - All notes in the current scale
- **Highlighting** - What's currently highlighted (tonality/chord/arpeggio)
- **Color Theme** - Active color scheme
- **Diatonic Chords** - All chords in the key with Roman numerals
- **View Mode** - Current display mode

#### Fretboard (Center/Top)
- Color-coded notes based on your selected theme
- String numbers on the left (6 = low E, 1 = high E)
- Fret markers show position on neck
- Notes displayed as letter names with alterations

#### Chord Diagrams

---

## Architecture (for the devs)

*"This project slept for a long time. I discovered Claude and thought i'd use
to make this what i had planned back when i thought i'd manage to free the time
for it lol. This is my first vibe code, but the original code was imperative
but based on a simple type model that has allowed it to become a codebase of
rather acceptable sanity. Thank you! - Dario"*

### Core Components

The project follows a modular architecture with clear separation of concerns:

#### Music Theory & Data
1. **types.ml** - Core type definitions (notes, chords, modes, display types)
2. **config.ml** - Configuration with tuning presets and color palettes
3. **theory.ml** - Music theory algorithms (scales, modes, chords, voicings)
4. **fretboard.ml** - Fretboard data structure with coordinate lookup
5. **color_theme.ml** - Color theme system for note visualization

#### Application State
6. **app_state.ml** - Application state machine managing interactive modes
7. **chord_mode.ml** - Chord selection and diagram mode logic
8. **arpeggio_mode.ml** - Arpeggio pattern mode logic
9. **selectors.ml** - Interactive selector widgets (tonality, tuning)

#### Display & Rendering
10. **display.ml** - Main display orchestration and view selection
11. **draw.ml** - Draw dispatcher
12. **draw_primitives.ml** - Shared drawing utilities
13. **draw_plain.ml** - Plain view rendering
14. **draw_fretted.ml** - Fretted view rendering
15. **draw_interline.ml** - Interline/tab view rendering
16. **draw_chord_diagram.ml** - Chord diagram visualization

#### UI Framework
17. **classes.ml** - Lambda-term widget classes and main event routing
18. **widgets.ml** - UI panels (context panel, help panel, keybindings modal)
19. **ocamuse.ml** - Application entry point

#### Utilities
20. **color.ml** - Color management utilities
21. **conv.ml** - Safe conversion utilities with Result types
22. **pp.ml** - Pretty-printing formatters using Fmt library

### Dependencies

- **lambda-term** (3.3.2) - Terminal UI library (vendored in project)
- **lwt** - Cooperative threading library for async operations
- **zed** - Text editing library (used by lambda-term)
- **fmt** - Pretty-printing library for formatted output

---

## Key Data Structures

### Music Theory Types

```ocaml
(** Base note without alteration *)
type base_note = A | B | C | D | E | F | G

(** Note with alteration (sharps/flats) *)
type note = {
  base : base_note;
  alteration : int  (* positive for sharps, negative for flats *)
}

(** Seven diatonic modes *)
type mode = C_mode | D_mode | E_mode | F_mode | G_mode | A_mode | B_mode

(** Guitar tuning is a list of open string notes *)
type tuning = note list

(** Chord types *)
type chord =
  | Major | Minor | Dimin | Augment
  | Suspend2 | Suspend4
  | Major7 | Domin7 | Minor7 | HalfDim7
  | Sixth | MinorSixth

(** Diatonic triads (simpler for diatonic chord sequences) *)
type diatonic_triad = Major | Minor | Diminished

(** Color theme selection *)
type color_theme =
  | ChromaticGradient
  | DiatonicDegrees
  | CustomPalette of string

(** What to highlight on the fretboard *)
type highlight_source =
  | Tonality of mode * note
  | Chord of note * chord
  | Arpeggio of note * chord
```

### Application State

```ocaml
(** Application modes - each has different keyboard shortcuts *)
type app_mode =
  | Normal                                    (* Default fretboard display *)
  | TonalitySelection of tonality_state       (* Selecting root and mode *)
  | TuningSelection of tuning_state           (* Selecting tuning *)
  | ChordMode of chord_state                  (* Chord diagrams *)
  | ArpeggioMode of arpeggio_state            (* Arpeggio patterns *)
  | ProgressionMode of progression_state      (* Chord progressions *)

(** Chord mode state with voicings *)
and chord_state = {
  selected_chord : (note * diatonic_triad) option;
  inversion : int;                            (* 0, 1, or 2 *)
  voicings : Theory.chord_voicing list;
  voicing_index : int;
}

(** Complete application state *)
type t = {
  mode : app_mode ref;
  focus : focus_target ref;
  context : ocamuse_structure;
  help_visible : bool ref;
  keybindings_modal_visible : bool ref;
  debug_mode : bool ref;
}
```

---

## Development Guidelines

### Project Structure

```
ocamuse/
├── src/                      (* All source code *)
│   ├── types.ml              (* Core types *)
│   ├── config.ml             (* Configuration *)
│   ├── theory.ml             (* Music theory *)
│   ├── app_state.ml          (* State machine *)
│   ├── classes.ml            (* Main UI *)
│   ├── widgets.ml            (* UI components *)
│   ├── draw_*.ml             (* Rendering *)
│   └── ...
├── dune-project              (* Dune build config *)
├── ocamuse.opam              (* Package definition *)
└── Claude.md                 (* This file *)
```

### Adding a New Feature

#### 1. Define Types (types.ml)
```ocaml
(* Add new types for your feature *)
type new_feature = (* ... *)
```

#### 2. Add Theory Logic (theory.ml)
```ocaml
(* Implement music theory calculations *)
let calculate_new_feature root mode = (* ... *)
```

#### 3. Create State (app_state.ml)
```ocaml
(* Add mode and state *)
type app_mode =
  | (* ... *)
  | NewFeatureMode of new_feature_state

and new_feature_state = {
  (* state fields *)
}
```

#### 4. Add Input Handler (new_feature_mode.ml)
```ocaml
let handle_input state event =
  match !(state.App_state.mode) with
  | App_state.NewFeatureMode fstate ->
      match event with
      | Key { code = Escape; _ } ->
          App_state.return_to_normal state;
          true
      | (* other keys *)
  | _ -> false
```

#### 5. Create Widget (selectors.ml or widgets.ml)
```ocaml
class new_feature_widget fstate app_state ocamuse_context =
  object
    inherit LTerm_widget.t "new_feature"

    method! draw ctx _focused =
      (* drawing logic *)

    method! can_focus = false
  end
```

#### 6. Wire Up in Main (classes.ml)
```ocaml
(* Add key binding *)
| Key { code = Char c; _ } when c = 'x' ->
    App_state.enter_new_feature_mode app_state;
    let widget = new Selectors.new_feature_widget (* ... *) in
    bottom_left_frame#set (widget :> LTerm_widget.t);
    self#queue_draw;
    true

(* Add mode routing *)
| App_state.NewFeatureMode _ ->
    if New_feature_mode.handle_input app_state event then begin
      (* update widget *)
      self#queue_draw;
      true
    end else false
```

#### 7. Update Help (widgets.ml)
```ocaml
(* Add to keybindings_modal *)
draw_line "  x - New feature";
```

(Or allow for the help menu to welcome more options)

## Entry Point Flow

```
ocamuse.ml:run()
  │
  ├─→ Create default_context()
  │     - Initialize fretboard with tuning
  │     - Set default mode (C major)
  │     - Set default display (Plain view)
  │
  ├─→ Create layer system
  │     prepare_simple_run() → (do_run, push_layer, pop_layer, exit)
  │
  ├─→ Create main_box widget
  │     - Build layout (top/bottom, left/right panels)
  │     - Register event handlers
  │     - Set up focus management
  │
  └─→ Run event loop
        do_run main_box
          │
          └─→ Event loop
                ├─→ User presses key
                ├─→ Event goes to focused layer
                ├─→ Handler updates state
                ├─→ queue_draw() triggers redraw
                └─→ display.ml → draw.ml → draw_*.ml
```

---

## Current Status

### ✅ Completed Features

- [x] Interactive terminal UI with lambda-term
- [x] Multiple fretboard views (Plain, Fretted, Interline)
- [x] Mode visualization (all 7 diatonic modes)
- [x] Tuning presets (Standard, Drop D, Open G, Open D, 7-string)
- [x] Color themes (Diatonic, Chromatic, Custom palettes)
- [x] Chord diagrams with voicings
- [x] Arpeggio mode
- [x] Context panel showing musical information
- [x] Help modal with keybindings
- [x] Tonality selection (interactive key change)
- [x] Tuning selection (interactive tuning change)
- [x] Debug mode (border visualization)
- [x] Layer-based modal system
- [x] Safe error handling (Result types)
- [x] Configurable presets

### 🚧 In Progress

- [ ] Progression builder mode
- [ ] Custom tuning editor
- [ ] Animation for arpeggio patterns

### 📋 Planned Features

#### High Priority
- [ ] MIDI output
- [ ] Save/load presets
- [ ] Custom color palette editor
- [ ] More chord types (7ths, 9ths, altered chords)

#### Medium Priority
- [ ] Scale finder (from note selection)
- [ ] Chord progression suggestions
- [ ] Tab notation export
- [ ] Practice mode with randomization

#### Low Priority
- [ ] Markov chain progressions
- [ ] PDF export
- [ ] Web-based version
- [ ] Audio playback

---

## Troubleshooting

### Build Issues

**Problem**: `dune build` fails
```bash
# Clean and rebuild
dune clean
dune build
```

**Problem**: Missing dependencies
```bash
# Install via opam
opam install . --deps-only
```

### Runtime Issues

**Problem**: Colors look wrong
- Try different color themes with `k`
- Check terminal supports 256 colors
- Some terminals have limited color palettes

**Problem**: Layout is broken
- Terminal might be too small (minimum ~80×24)
- Try resizing terminal window
- Press `Ctrl+L` to force redraw

**Problem**: Keys don't work
- Check if terminal is capturing keys (some terminals intercept shortcuts)
- Try in different terminal emulator
- Press `?` to see keybindings

### Known Limitations

- Requires terminal with 256-color support
- Minimum terminal size: ~80 columns × 24 rows
- Mouse support varies by terminal
- No Windows support (Unix/Linux/macOS only)

---

## Contributing

### Refactoring Opportunities

See `REFACTORING_ROADMAP.md` for planned improvements:

**High Priority:**
- Type safety improvements
- Performance optimizations
- Better error messages

**Medium Priority:**
- Reduce imperative loops (use fold/map)
- Simplify display type hierarchy
- Extract spacing logic abstractions

**Low Priority:**
- Strong event types
- Better module organization
- Domain types for progressions

### Git Workflow

```bash
# Create feature branch
git checkout -b feature/new-feature

# Make changes
# ... edit files ...

# Format code
dune build @fmt
dune promote

# Test
dune build
dune exec ocamuse

# Commit
git add .
git commit -m "Add new feature"

# Push
git push origin feature/new-feature
```

---

## Resources

### Lambda-Term Documentation
- Source: `lambda-term.3.3.2/` (vendored in project)
- Examples: `lambda-term.3.3.2/examples/`
- Key modules:
  - `LTerm_widget` - Widget base classes
  - `LTerm_draw` - Drawing primitives
  - `LTerm_style` - Text styling
  - `LTerm_event` - Event handling
  - `LTerm_geom` - Geometry (rect, size)

### Music Theory References
- Mode intervals: Wikipedia "Mode (music)"
- Chord theory: Wikipedia "Chord (music)"
- Fretboard layout: Standard guitar tuning

### OCaml Resources
- OCaml.org - Official documentation
- Real World OCaml - Book
- Dune - Build system docs

---

**Last Updated**: 2025-10-04
**Claude Version**: Sonnet 4.5

---

*This document is maintained as a living reference for developers and users of Ocamuse. Contributions and corrections welcome!*
