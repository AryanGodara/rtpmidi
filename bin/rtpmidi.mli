type midi_message_type =
  | NOTE_OFF
  | NOTE_ON
  | POLY_PRESSURE
  | CONTROL_CHANGE
  | PROGRAM_CHANGE
  | CHANNEL_PRESSURE
  | PITCH_BEND
  | SYSTEM_EXCLUSIVE
  | TIME_CODE
  | SONG_POSITION
  | SONG_SELECT
  | TUNE_REQUEST
  | END_OF_EXCLUSIVE
  | TIMING_CLOCK
  | START
  | CONTINUE
  | STOP
  | ACTIVE_SENSING
  | SYSTEM_RESET

module GENERIC_MIDI_MESSAGE : sig
  type t = {
    message_type : midi_message_type;
    status_byte : int;
    channel : int;
    data1 : int;
    data2 : int;
    timestamp : int;
    sysex_data : string option;
  }

  val message_type : t -> midi_message_type
  (** Returns the MIDI message type of the given message *)

  val create : 
    message_type:midi_message_type -> 
    channel:int -> 
    data1:int -> 
    data2:int -> 
    ?sysex_data:string option -> 
    timestamp:int -> 
    unit ->
    t
  (** Creates a new MIDI message with the given message type, channel, data bytes, and timestamp *)
end

module SERIALIZER : sig
  val serialize : GENERIC_MIDI_MESSAGE.t -> Bytes.t
  (** Serializes the given MIDI message to a byte array *)

  val deserialize : Bytes.t -> GENERIC_MIDI_MESSAGE.t
  (** Deserializes the given byte array to a MIDI message *)
end

type specific_midi_message =
  | NOTE_OFF of {
      note : char;
      velocity : char;
      channel : int;
      timestamp : int;
    }
  | NOTE_ON of {
      note : char;
      velocity : char;
      channel : int;
      timestamp : int;
    }
  | POLY_PRESSURE of {
      note : char;
      pressure : char;
      channel : int;
      timestamp : int;
    }
  | CONTROL_CHANGE of {
      controller : char;
      value : char;
      channel : int;
      timestamp : int;
    }
  | PROGRAM_CHANGE of { program : char; channel : int; timestamp : int }
  | CHANNEL_PRESSURE of { pressure : char; channel : int; timestamp : int }
  | PITCH_BEND of { value : int; channel : int; timestamp : int }
  | SYSTEM_EXCLUSIVE of { data : int; sysex_data : string option ; timestamp : int }
  | TIME_CODE of { value : char; timestamp : int }
  | SONG_POSITION of { position : int; timestamp : int }
  | SONG_SELECT of { song : char; timestamp : int }
  | TUNE_REQUEST of { timestamp : int }
  | END_OF_EXCLUSIVE of { timestamp : int }
  | TIMING_CLOCK of { timestamp : int }
  | START of { timestamp : int }
  | CONTINUE of { timestamp : int }
  | STOP of { timestamp : int }
  | ACTIVE_SENSING of { timestamp : int }
  | SYSTEM_RESET of { timestamp : int }
      (** Represents a MIDI message with specific data values *)

val to_specific_midi_message : GENERIC_MIDI_MESSAGE.t -> specific_midi_message
(** Converts the given MIDI message to a specific MIDI message *)

val of_specific_midi_message : specific_midi_message ->  GENERIC_MIDI_MESSAGE.t
(** Converts the given specific Midi message to a Generic MIDI message *)
