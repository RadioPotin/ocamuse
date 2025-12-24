# Ocamuse - OCaml Assistant for MUSic Exploration

## Project Overview

**Ocamuse** is an interactive terminal-based GUI application for learning music theory on fretted instruments (guitar, bass, etc.). Built entirely in OCaml using the lambda-term TUI library, it provides real-time visualization of scales, modes, and musical concepts directly in your terminal.

---

## Quick Start

```bash
# Build
dune build

# Run
dune exec ocamuse

# Or directly
./_build/default/src/ocamuse.exe
```

---

## Current Features

### Fretboard Views
- **Plain View** - Compact, linear representation
- **Fretted View** - Classic chord diagram style with boxes
- **Interline View** - Guitar tab-style with horizontal lines

### Scale System (45 Scales in 7 Categories)

| Category | Scales |
|----------|--------|
| **Diatonic** (7) | Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian |
| **Harmonic Minor** (7) | Harmonic Minor, Locrian nat6, Ionian #5, Dorian #4, Phrygian Dominant, Lydian #2, Super Locrian bb7 |
| **Melodic Minor** (7) | Melodic Minor, Dorian b2, Lydian Augmented, Lydian Dominant, Mixolydian b6, Locrian nat2, Altered |
| **Pentatonic/Blues** (6) | Major Pentatonic, Minor Pentatonic, Blues Major, Blues Minor, Hirajoshi, In Sen |
| **Symmetric** (6) | Whole Tone, Diminished HW, Diminished WH, Chromatic, Augmented, Tritone |
| **Ethnic** (8) | Hungarian Minor, Double Harmonic (Gypsy), Persian, Arabian, Japanese, Egyptian, Neapolitan Minor/Major |
| **Bebop** (4) | Bebop Dominant, Bebop Major, Bebop Minor, Bebop Dorian |

### Tuning Presets
- Standard 6-string (E A D G B E)
- Drop D (D A D G B E)
- Standard 7-string (B E A D G B E)
- Drop A 7-string
- Open G, Open D

### Color Themes
- Circle of Fifths (default)
- Chromatic Rainbow
- Warm/Cool Tones
- High Contrast
- Pastel
- Monochrome Blue

---

## Keybindings

| Key | Action |
|-----|--------|
| `t` | Open tonality selector (root + scale) |
| `u` | Open tuning selector |
| `m` | Cycle scales within current category |
| `v` | Cycle fretboard views |
| `k` | Cycle color themes |
| `r` | Reset to tonality highlighting |
| `h` | Toggle help panel |
| `?` | Toggle keybindings modal |
| `d` | Toggle debug borders |
| `Esc` | Exit selector / Quit app |

### Tonality Selector Navigation
| Key | Action |
|-----|--------|
| `a-g` | Set root note |
| `#` | Add sharp |
| `b` | Add flat |
| `Up/Down` | Browse categories/scales |
| `Left/Right` | Navigate selection steps |
| `Enter` | Confirm selection |

---

## Architecture

### Project Structure

```
ocamuse/
├── src/
│   ├── ocamuse.ml          # Entry point
│   ├── types.ml            # Core types (scale_type, note, chord, etc.)
│   ├── config.ml           # Tunings, color palettes, defaults
│   ├── app_state.ml        # Application state machine
│   ├── classes.ml          # Main widget container, event routing
│   ├── selectors.ml        # Tonality/tuning selector widgets
│   ├── widgets.ml          # Context panel, help, keybindings modal
│   ├── display.ml          # Scale computation, view orchestration
│   ├── draw.ml             # Draw dispatcher
│   ├── draw_plain.ml       # Plain view rendering
│   ├── draw_fretted.ml     # Fretted view rendering
│   ├── draw_interline.ml   # Interline view rendering
│   ├── draw_primitives.ml  # Shared drawing utilities
│   ├── fretboard.ml        # Fretboard data structure
│   ├── color.ml            # Color utilities
│   ├── color_theme.ml      # Theme cycling
│   ├── conv.ml             # Conversion utilities
│   ├── pp.ml               # Pretty-printing formatters
│   ├── multi_view/         # Multi-panel system (WIP)
│   │   ├── layout.ml       # Layout configuration
│   │   ├── panel.ml        # Panel types
│   │   ├── sync.ml         # Panel synchronization
│   │   ├── views/
│   │   │   └── harmony_view.ml
│   │   └── suggestions/
│   │       └── chord_suggestions.ml
│   └── widgets/
│       └── mini_fretboard.ml
├── ocaml-music-theory/     # Music theory library (subproject)
│   ├── src/
│   │   ├── note.ml
│   │   ├── pitch.ml
│   │   ├── interval.ml
│   │   ├── scales/
│   │   │   ├── mode.ml
│   │   │   ├── scale.ml
│   │   │   └── circle_of_fifths.ml
│   │   └── harmony/
│   │       └── chord.ml
│   └── test/
├── dune-project
├── ocamuse.opam
└── CLAUDE.md               # This file
```

### Core Types (types.ml)

```ocaml
(** Scale categories *)
type scale_category =
  | Diatonic | HarmonicMinor | MelodicMinor
  | PentatonicBlues | Symmetric | Ethnic | Bebop

(** 45 scale types *)
type scale_type =
  (* Diatonic *)
  | Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
  (* Harmonic Minor modes *)
  | HarmonicMinor | LocrianNat6 | IonianSharp5 | DorianSharp4
  | PhrygianDominant | LydianSharp2 | SuperLocrianbb7
  (* Melodic Minor modes *)
  | MelodicMinor | DorianFlat2 | LydianAugmented | LydianDominant
  | MixolydianFlat6 | LocrianNat2 | Altered
  (* Pentatonic & Blues *)
  | MajorPentatonic | MinorPentatonic | BluesMajor | BluesMinor
  | Hirajoshi | InSen
  (* Symmetric *)
  | WholeTone | DiminishedHW | DiminishedWH | Chromatic | Augmented | Tritone
  (* Ethnic *)
  | HungarianMinor | DoubleHarmonic | Persian | Arabian
  | Japanese | Egyptian | NeapolitanMinor | NeapolitanMajor
  (* Bebop *)
  | BebopDominant | BebopMajor | BebopMinor | BebopDorian

(** Note with alteration *)
type note = { base : base_note; alteration : int }

(** What to highlight on fretboard *)
type highlight_source =
  | Tonality of scale_type * note
  | Chord of note * chord
  | Arpeggio of note * chord

(** Main application context *)
type ocamuse_structure = {
  display_mode : display ref;
  mutable fretboard : fretboard_data;
  base_colour : base_colour ref;
  mutable tuning : tuning;
  mutable root_note : note;
  mutable scale : scale_type;
  mutable highlight_source : highlight_source;
  mutable color_theme : color_theme;
}
```

### Application State Machine (app_state.ml)

```ocaml
type app_mode =
  | Normal                              (* Default fretboard view *)
  | TonalitySelection of tonality_state (* Selecting root + scale *)
  | TuningSelection of tuning_state     (* Selecting tuning *)
  | MultiView of multi_view_state       (* Multi-panel mode - WIP *)

type tonality_state = {
  temp_root : note option;
  temp_scale : scale_type option;
  temp_category : scale_category;
  category_index : int;
  scale_index : int;
  selection_step : selection_step;
}

type selection_step =
  | SelectingRoot
  | SelectingAlteration
  | SelectingCategory
  | SelectingScale
```

### Key Modules

| Module | Purpose |
|--------|---------|
| `display.ml` | Scale interval definitions, `build_tonality`, category helpers, view orchestration |
| `classes.ml` | Main `main_box` widget, keyboard event routing, panel management |
| `selectors.ml` | `handle_tonality_input`, `handle_tuning_input`, selector widgets |
| `app_state.ml` | Mode transitions, state machine |
| `config.ml` | `Tunings` module, `Palettes` module |

---

## Development Practices

### Code Style
- Uses `Fmt` library for pretty-printing
- Lambda-term widgets inherit from `LTerm_widget.t`
- State managed via mutable refs in `ocamuse_structure`
- Mode-based input handling (Normal, TonalitySelection, etc.)

### Adding a New Scale
1. Add constructor to `scale_type` in `types.ml`
2. Add intervals in `scale_intervals` in `display.ml`
3. Add to appropriate category in `scales_in_category`
4. Add display name in `scale_name`
5. Add to `category_of_scale`
6. Add short name in `pp.ml` `pp_scale_type`

### Adding a New Mode/Feature
1. Add mode constructor to `app_mode` in `app_state.ml`
2. Add state type if needed
3. Add `enter_*` transition function
4. Add input handler in `selectors.ml` or new file
5. Add widget class
6. Wire up keybinding in `classes.ml` Normal handler
7. Add mode case in `classes.ml` event dispatcher
8. Update keybindings in `widgets.ml`

### Build Commands

```bash
dune build              # Compile
dune exec ocamuse       # Run
dune build @fmt         # Check formatting
dune promote            # Apply format fixes
dune clean              # Clean build
```

---

## Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| lambda-term | 3.3.2 | Terminal UI framework |
| lwt | - | Async/cooperative threading |
| zed | - | Text editing (used by lambda-term) |
| fmt | - | Pretty-printing |
| ocaml-music-theory | local | Music theory calculations |

---

## Current Status

### Completed
- [x] 3 fretboard views (Plain, Fretted, Interline)
- [x] 45 scales in 7 categories
- [x] Category-based scale selector UI
- [x] Tuning selector with 6 presets
- [x] 7 color themes
- [x] Context panel showing current key/scale
- [x] Help panel and keybindings modal
- [x] Debug mode with border visualization
- [x] Pitch class matching for enharmonic notes

### Work in Progress
- [ ] Multi-view panel system (harmony explorer)
- [ ] Chord suggestions panel
- [ ] Mini fretboard widget

### Planned
- [ ] Chord diagrams with voicings
- [ ] Arpeggio patterns
- [ ] Custom tuning editor
- [ ] MIDI output
- [ ] Save/load presets

---

## Troubleshooting

**Build fails**: `dune clean && dune build`

**Colors wrong**: Try `k` to cycle themes. Ensure 256-color terminal.

**Layout broken**: Terminal too small (min 80x24). Resize or `Ctrl+L`.

**Keys not working**: Some terminals intercept keys. Try different terminal.

---

## Recent Changes

### 2025-12-24
- Expanded from 7 diatonic modes to 45 scales in 7 categories
- New category-based tonality selector UI
- Replaced `mode` type with `scale_type`
- Added scale interval lookup table
- Cleaned up dead code (chord_mode.ml, arpeggio_mode.ml, draw_chord_diagram.ml)

---

**Last Updated**: 2025-12-24
