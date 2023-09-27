open Rtpmidi
open Midi

val device : channel:int -> unit -> Device.t
val write_midi_message : Device.t -> GENERIC_MIDI_MESSAGE.t -> unit
