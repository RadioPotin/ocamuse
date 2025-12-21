# OCaml Music Theory

A comprehensive music theory library for OCaml, providing types and functions for working with notes, intervals, scales, chords, and the circle of fifths.

## Features

### Core Modules (v0.1.0)

- **Pitch**: Chromatic pitch classes (0-11) with modular arithmetic
- **Interval**: Musical intervals with proper music theory semantics
- **Note**: Notes with letter names (A-G) and accidentals (sharps/flats)

### Scales & Harmony Modules (v0.2.0)

- **Mode**: Seven diatonic modes with brightness ordering and characteristic intervals
- **Scale**: Scale construction, degree access, diatonic harmony generation
- **Circle of Fifths**: Complete navigation, key signatures, modulation analysis
- **Chord**: Comprehensive chord types (triads, 7ths, extensions), inversions, voice leading

### Coming Soon

- **Instruments**: Fretboard models, tunings, voicings
- **IO**: Advanced parsing and formatting
- **Extended Harmony**: Chord progressions, voice leading rules, functional harmony

## Installation

### From Source (Development)

```bash
git clone https://github.com/yourusername/ocaml-music-theory.git
cd ocaml-music-theory
dune build
dune install
```

### In Your Project

Add to your `dune` file:

```lisp
(executable
 (name my_program)
 (libraries ocaml-music-theory))
```

## Quick Start

```ocaml
open Music_theory

(* Work with pitch classes *)
let c = Core.Pitch.c in
let g = Core.Pitch.add c 7 in  (* Transpose up a fifth *)
Printf.printf "C + 7 = %s\n" (Core.Pitch.to_string ~prefer:`Sharp g)
(* Output: C + 7 = G *)

(* Use typed intervals *)
let fifth = Core.Interval.PerfectFifth in
Printf.printf "Perfect fifth = %d semitones\n"
  (Core.Interval.to_semitones fifth)
(* Output: Perfect fifth = 7 semitones *)

(* Create and manipulate notes *)
let c_sharp = Core.Note.c_sharp in
let d_flat = Core.Note.d_flat in
Printf.printf "C# = Db? %b\n" (Core.Note.enharmonic_eq c_sharp d_flat)
(* Output: C# = Db? true *)

(* Build scales and generate harmony *)
let c_major = Scales.Scale.major Core.Note.c in
let triads = Scales.Scale.diatonic_triads c_major in
Printf.printf "I chord in C major: %s\n"
  (Core.Note.to_string (fst (List.hd triads)))
(* Output: I chord in C major: C *)

(* Navigate the circle of fifths *)
let c_pos = Scales.Circle_of_fifths.of_note Core.Note.c in
let g_pos = Scales.Circle_of_fifths.fifth_above c_pos in
Printf.printf "G major has %d sharps\n"
  (Scales.Circle_of_fifths.sharps_count g_pos)
(* Output: G major has 1 sharps *)

(* Construct and analyze chords *)
let g7 = Harmony.Chord.dominant7 Core.Note.g in
Printf.printf "G7 chord: %s\n" (Harmony.Chord.to_string_with_notes g7)
(* Output: G7 chord: G7: B D F G *)
```

## Examples

### Basic Usage

```bash
dune exec ./examples/basic_usage.exe
```

Demonstrates core functionality:
- Pitch class arithmetic
- Interval operations and inversions
- Note construction and enharmonic equivalence
- String parsing

### Comprehensive Theory Showcase

```bash
dune exec ./examples/theory_showcase.exe
```

Demonstrates advanced features:
- Scales and modes (Ionian, Dorian, Lydian, etc.)
- Automatic diatonic harmony generation (triads and seventh chords)
- Chord construction and inversions
- Circle of fifths navigation and key signatures
- Modulation and modal interchange
- Mode brightness ordering
- Scale and chord analysis

## API Overview

### Music_theory.Core.Pitch

Pitch classes represent the 12 chromatic pitches (C=0, C#=1, ..., B=11).

```ocaml
val c : t
val c_sharp : t
val d_flat : t
(* ... all 12 pitch classes *)

val of_int : int -> t  (* Mod 12 *)
val to_int : t -> int
val add : t -> int -> t  (* Transpose *)
val diff : t -> t -> int  (* Distance in semitones *)
val to_string : prefer:[`Sharp | `Flat] -> t -> string
```

### Music_theory.Core.Interval

Musical intervals as first-class types.

```ocaml
type t =
  | Unison | MinorSecond | MajorSecond
  | MinorThird | MajorThird
  | PerfectFourth | Tritone | PerfectFifth
  | MinorSixth | MajorSixth
  | MinorSeventh | MajorSeventh
  | Octave

val to_semitones : t -> int
val of_semitones : int -> t option
val invert : t -> t  (* e.g., M3 -> m6 *)
val is_perfect : t -> bool
val is_major : t -> bool
val is_minor : t -> bool
val letter_distance : t -> int  (* For proper note naming *)
```

### Music_theory.Core.Note

Notes with letter names and accidentals.

```ocaml
type base = A | B | C | D | E | F | G
type t = { base : base; alteration : int }

val c : t
val c_sharp : t
val d_flat : t
(* ... named constructors for common notes *)

val make : base -> int -> t
val natural : base -> t
val sharp : base -> t
val flat : base -> t

val to_pitch : t -> Pitch.t
val of_pitch : Pitch.t -> prefer:[`Sharp | `Flat] -> t
val enharmonic_eq : t -> t -> bool

val to_string : ?unicode:bool -> t -> string
val of_string : string -> (t, string) result

val next_base : base -> base
val prev_base : base -> base
val base_distance : base -> int
```

### Music_theory.Scales.Mode

Seven diatonic modes with brightness ordering.

```ocaml
type t = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian

val intervals : t -> Interval.t list
(** Interval pattern for the mode *)

val brightness : t -> int
(** Brightness score from -3 (Locrian) to +3 (Lydian) *)

val rotate : t -> int -> t
(** Rotate mode by n steps (e.g., Ionian -> Dorian) *)

val characteristic_interval : t -> Interval.t
(** The interval that defines the mode's character *)

val is_major : t -> bool
val is_minor : t -> bool
```

### Music_theory.Scales.Scale

Scale construction and diatonic harmony generation.

```ocaml
type t = { root : Note.t; mode : Mode.t; degrees : Note.t list }

val make : Note.t -> Mode.t -> t
val major : Note.t -> t
val minor : Note.t -> t
val harmonic_minor : Note.t -> t
val melodic_minor : Note.t -> t

(* Access scale degrees *)
val tonic : t -> Note.t
val dominant : t -> Note.t
val leading_tone : t -> Note.t

(* Generate diatonic harmony *)
val diatonic_triads : t -> (Note.t * triad_quality) list
val diatonic_sevenths : t -> (Note.t * seventh_quality) list

(* Key relationships *)
val relative_minor : t -> t
val relative_major : t -> t
val parallel_mode : t -> Mode.t -> t
```

### Music_theory.Scales.Circle_of_fifths

Navigate the circle of fifths and analyze key relationships.

```ocaml
type position = int  (* 0-11 *)

val of_note : Note.t -> position
val to_note : position -> prefer:[`Sharp | `Flat] -> Note.t

(* Navigation *)
val fifth_above : position -> position
val fifth_below : position -> position
val move_clockwise : position -> int -> position

(* Key signatures *)
val sharps_count : position -> int
val flats_count : position -> int
val key_signature : position -> Note.base list

(* Modulation *)
val closely_related_keys : position -> position list
val relationship : position -> position -> relationship
val secondary_dominant : position -> Note.t -> position option
```

### Music_theory.Harmony.Chord

Comprehensive chord construction and analysis.

```ocaml
type triad_quality = Major | Minor | Diminished | Augmented | Sus2 | Sus4
type seventh_quality = Major7 | Minor7 | Dominant7 | HalfDiminished7 | Diminished7

type t = { root : Note.t; chord_type : chord_type; notes : Note.t list }

(* Construction *)
val major : Note.t -> t
val minor : Note.t -> t
val dominant7 : Note.t -> t
val half_diminished7 : Note.t -> t

(* Inversions *)
val invert : t -> inversion -> Note.t list
val bass_note : t -> inversion -> Note.t

(* Analysis *)
val contains : t -> Note.t -> bool
val function_in_key : Scale.t -> t -> chord_function option
val tension_level : t -> int

(* Voice leading *)
val voice_leading_distance : Note.t list -> Note.t list -> int
val common_tones : t -> t -> Note.t list
```

## Testing

Run the test suite:

```bash
dune runtest
```

Current test coverage:
- **Pitch**: 6 tests (construction, arithmetic, distance, string conversion)
- **Interval**: 6 tests (conversion, inversion, qualities, letter distance)
- **Note**: 6 tests (construction, pitch conversion, enharmonic equivalence, parsing)
- **Scales**: 10 tests (mode intervals, brightness, rotation, scale construction, diatonic harmony, key relationships)
- **Harmony**: 11 tests (chord construction, inversions, quality analysis, diatonic harmony, chord functions, relationships)

Total: **39 tests**, all passing ✓

## Design Principles

1. **Type Safety**: Use types to prevent invalid musical operations
2. **Immutability**: All operations return new values
3. **Explicit Over Implicit**: Make musical concepts explicit in types
4. **Zero Dependencies**: Pure OCaml, no external dependencies
5. **Functional Style**: Avoid mutation, use standard library idioms

## Use Cases

- **Music Applications**: Build synthesizers, sequencers, notation software
- **Educational Tools**: Teach music theory interactively
- **Algorithmic Composition**: Generate music programmatically
- **Music Analysis**: Analyze existing compositions
- **Instrument Tools**: Guitar/piano chord finders, scale visualizers

## Architecture

The library is organized into modules by musical concept:

```
Music_theory
├── Core
│   ├── Pitch      (Chromatic pitch classes)
│   ├── Interval   (Musical intervals)
│   └── Note       (Notes with letter names)
├── Scales
│   ├── Mode              (Diatonic modes) ✓
│   ├── Scale             (Scale construction & harmony) ✓
│   └── Circle_of_fifths  (Circle navigation & modulation) ✓
└── Harmony
    └── Chord             (Chords, inversions, voice leading) ✓

(Future modules)
├── Instruments
│   ├── Tuning
│   └── Fretboard
└── Io
    ├── Parsing
    └── Formatting
```

## Roadmap

### v0.2.0 - Scales & Modes ✓ Complete
- [x] Diatonic modes (Ionian, Dorian, Phrygian, etc.)
- [x] Scale construction from intervals
- [x] Circle of fifths relationships
- [x] Mode brightness ordering
- [x] Automatic diatonic harmony generation
- [x] Key signatures and modulation analysis

### v0.3.0 - Harmony (Partially Complete)
- [x] Chord types (triads, 7ths, extended)
- [x] Chord inversions
- [x] Basic voice leading
- [ ] Chord progressions and sequences
- [ ] Advanced voice leading rules
- [ ] Functional harmony analysis

### v0.4.0 - Instruments
- [ ] Guitar/bass fretboard models
- [ ] Chord voicing finder
- [ ] Tuning systems
- [ ] Position-based visualization

### v1.0.0 - Stable Release
- [ ] Complete API documentation
- [ ] Performance optimizations
- [ ] MIDI support
- [ ] MusicXML import/export

## Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `dune runtest`
5. Format code: `dune build @fmt && dune promote`
6. Submit a pull request

## License

ISC License - see LICENSE file for details

## Credits

Developed by Dario Pinto as part of the [Ocamuse](https://github.com/yourusername/ocamuse) project.

Built with OCaml and Dune.

## Related Projects

- **Ocamuse**: Terminal-based guitar fretboard visualizer using this library
- (More coming soon)

---

**Status**: v0.2.0 - Core, Scales & Harmony modules complete
**Build**: ✓ Passing
**Tests**: 39/39 passing
**Documentation**: [API Docs](./docs) (run `dune build @doc`)

## What's New in v0.2.0

- **Mode System**: Complete implementation of 7 diatonic modes with brightness ordering
- **Scale Construction**: Build major, minor, and modal scales with proper note naming
- **Circle of Fifths**: Navigate the circle, calculate key signatures, analyze modulation
- **Automatic Harmony**: Generate diatonic triads and seventh chords for any scale
- **Chord System**: Comprehensive chord types with inversions and voice leading
- **21 New Tests**: Full coverage of scales, modes, chords, and harmonic relationships
