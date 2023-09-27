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

module GENERIC_MIDI_MESSAGE = struct
  type t = {
    message_type : midi_message_type;
    status_byte : int;
    channel : int;
    data1 : int;
    data2 : int;
    timestamp : int;
    sysex_data : string option
  }

  let message_type (message : t) = message.message_type

  let create ~message_type ~channel ~data1 ~data2 ?(sysex_data = None) ~timestamp () =
    let status_byte =
      match message_type with
      | NOTE_OFF -> 0x80
      | NOTE_ON -> 0x90
      | POLY_PRESSURE -> 0xA0
      | CONTROL_CHANGE -> 0xB0
      | PROGRAM_CHANGE -> 0xC0
      | CHANNEL_PRESSURE -> 0xD0
      | PITCH_BEND -> 0xE0
      | SYSTEM_EXCLUSIVE -> 0xF0
      | TIME_CODE -> 0xF1
      | SONG_POSITION -> 0xF2
      | SONG_SELECT -> 0xF3
      | TUNE_REQUEST -> 0xF6
      | END_OF_EXCLUSIVE -> 0xF7
      | TIMING_CLOCK -> 0xF8
      | START -> 0xFA
      | CONTINUE -> 0xFB
      | STOP -> 0xFC
      | ACTIVE_SENSING -> 0xFE
      | SYSTEM_RESET -> 0xFF
    in
    { message_type; status_byte; channel; data1; data2; sysex_data; timestamp }
end

module SERIALIZER = struct
  let serialize (message : GENERIC_MIDI_MESSAGE.t) : Bytes.t =
    let status_byte =
      Char.chr
        (message.GENERIC_MIDI_MESSAGE.status_byte lor message.GENERIC_MIDI_MESSAGE.channel)
    in
    let data1_byte = Char.chr message.GENERIC_MIDI_MESSAGE.data1 in
    let data2_byte = Char.chr message.GENERIC_MIDI_MESSAGE.data2 in
    let bytes =
      match message.GENERIC_MIDI_MESSAGE.message_type with
      | NOTE_OFF ->
          Bytes.of_string
            (Printf.sprintf "%c%c%c" (Char.chr 0x80) data1_byte data2_byte)
      | NOTE_ON ->
          Bytes.of_string
            (Printf.sprintf "%c%c%c" status_byte data1_byte data2_byte)
      | POLY_PRESSURE ->
          Bytes.of_string
            (Printf.sprintf "%c%c%c"
               (char_of_int (0xA0 lor message.GENERIC_MIDI_MESSAGE.channel))
               data1_byte data2_byte)
      | CONTROL_CHANGE ->
          Bytes.of_string
            (Printf.sprintf "%c%c%c"
               (char_of_int (0xB0 lor message.GENERIC_MIDI_MESSAGE.channel))
               data1_byte data2_byte)
      | PROGRAM_CHANGE ->
          Bytes.of_string
            (Printf.sprintf "%c%c"
               (char_of_int (0xC0 lor message.GENERIC_MIDI_MESSAGE.channel))
               data1_byte)
      | CHANNEL_PRESSURE ->
          Bytes.of_string
            (Printf.sprintf "%c%c"
               (char_of_int (0xD0 lor message.GENERIC_MIDI_MESSAGE.channel))
               data1_byte)
      | PITCH_BEND ->
          Bytes.of_string
            (Printf.sprintf "%c%c%c"
               (char_of_int (0xE0 lor message.GENERIC_MIDI_MESSAGE.channel))
               data1_byte data2_byte)
      | SYSTEM_EXCLUSIVE ->
        let sysex_data = match message.GENERIC_MIDI_MESSAGE.sysex_data with
          | Some data -> data
          | None -> failwith "Invalid MIDI message"
      in
        let sysex_bytes = Bytes.of_string sysex_data in
        let sysex_length = Bytes.length sysex_bytes in
        let header_byte = Char.chr 0xF0 in
        let footer_byte = Char.chr 0xF7 in
        let bytes = Bytes.create (sysex_length + 2) in
        Bytes.set bytes 0 header_byte;
        Bytes.blit sysex_bytes 0 bytes 1 sysex_length;
        Bytes.set bytes (sysex_length + 1) footer_byte;
        bytes
      | TIME_CODE -> 
          let nibble1 = message.GENERIC_MIDI_MESSAGE.data1 lsr 4 in
          let nibble2 = message.GENERIC_MIDI_MESSAGE.data1 land 0x0F in
          Bytes.of_string (Printf.sprintf "%c%c" (Char.chr 0xF1) (Char.chr ((nibble1 lsl 4) lor nibble2)))
      | SONG_POSITION -> 
          let position = (message.GENERIC_MIDI_MESSAGE.data1 lsl 7) lor message.GENERIC_MIDI_MESSAGE.data2 in
          Bytes.of_string (Printf.sprintf "%c%c%c" (Char.chr 0xF2) (Char.chr (position land 0x7F)) (Char.chr (position lsr 7)))
      | SONG_SELECT -> 
          Bytes.of_string (Printf.sprintf "%c%c" (Char.chr 0xF3) data1_byte)
      | TUNE_REQUEST -> 
          Bytes.of_string (Char.chr 0xF6 |> String.make 1)
      | END_OF_EXCLUSIVE -> 
          Bytes.of_string (Char.chr 0xF7 |> String.make 1)
      | TIMING_CLOCK -> 
          Bytes.of_string (Char.chr 0xF8 |> String.make 1)
      | START -> 
          Bytes.of_string (Char.chr 0xFA |> String.make 1)
      | CONTINUE -> 
          Bytes.of_string (Char.chr 0xFB |> String.make 1)
      | STOP -> 
          Bytes.of_string (Char.chr 0xFC |> String.make 1)
      | ACTIVE_SENSING -> 
          Bytes.of_string (Char.chr 0xFE |> String.make 1)
      | SYSTEM_RESET -> 
          Bytes.of_string (Char.chr 0xFF |> String.make 1)
    in
    bytes

  let deserialize bytes =
    let length = Bytes.length bytes in
    if length < 1 || length > 3 then failwith "Invalid MIDI message bytes";
    let status_byte = Bytes.get bytes 0 in
    let message_type =
      match int_of_char status_byte land 0xF0 with
      | 0x80 -> NOTE_OFF
      | 0x90 -> NOTE_ON
      | 0xA0 -> POLY_PRESSURE
      | 0xB0 -> CONTROL_CHANGE
      | 0xC0 -> PROGRAM_CHANGE
      | 0xD0 -> CHANNEL_PRESSURE
      | 0xE0 -> PITCH_BEND
      | 0xF0 -> SYSTEM_EXCLUSIVE
      | 0xF1 -> TIME_CODE
      | 0xF2 -> SONG_POSITION
      | 0xF3 -> SONG_SELECT
      | 0xF6 -> TUNE_REQUEST
      | 0xF7 -> END_OF_EXCLUSIVE
      | 0xF8 -> TIMING_CLOCK
      | 0xFA -> START
      | 0xFB -> CONTINUE
      | 0xFC -> STOP
      | 0xFE -> ACTIVE_SENSING
      | 0xFF -> SYSTEM_RESET
      | _ -> failwith "Invalid MIDI message status byte"
    in
    let channel = int_of_char status_byte land 0x0F in
    let data1 = Bytes.get bytes 1 in
    let data2 = Bytes.get bytes 2 in
    let timestamp = 0 in
    let message =
      GENERIC_MIDI_MESSAGE.create ~message_type ~channel ~data1:(int_of_char data1)
        ~data2:(int_of_char data2) ~timestamp ()
    in
    message
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

let to_specific_midi_message (midi_message : GENERIC_MIDI_MESSAGE.t) :
    specific_midi_message =
  match midi_message.message_type with
  | NOTE_OFF ->
      NOTE_OFF
        {
          note = char_of_int midi_message.data1;
          velocity = char_of_int midi_message.data2;
          channel = midi_message.channel;
          timestamp = midi_message.timestamp;
        }
  | NOTE_ON ->
      NOTE_ON
        {
          note = char_of_int midi_message.data1;
          velocity = char_of_int midi_message.data2;
          channel = midi_message.channel;
          timestamp = midi_message.timestamp;
        }
  | POLY_PRESSURE ->
      POLY_PRESSURE
        {
          note = char_of_int midi_message.data1;
          pressure = char_of_int midi_message.data2;
          channel = midi_message.channel;
          timestamp = midi_message.timestamp;
        }
  | CONTROL_CHANGE ->
      CONTROL_CHANGE
        {
          controller = char_of_int midi_message.data1;
          value = char_of_int midi_message.data2;
          channel = midi_message.channel;
          timestamp = midi_message.timestamp;
        }
  | PROGRAM_CHANGE ->
      PROGRAM_CHANGE
        {
          program = char_of_int midi_message.data1;
          channel = midi_message.channel;
          timestamp = midi_message.timestamp;
        }
  | CHANNEL_PRESSURE ->
      CHANNEL_PRESSURE
        {
          pressure = char_of_int midi_message.data1;
          channel = midi_message.channel;
          timestamp = midi_message.timestamp;
        }
  | PITCH_BEND ->
      PITCH_BEND
        {
          value = (midi_message.data2 lsl 7) lor midi_message.data1;
          channel = midi_message.channel;
          timestamp = midi_message.timestamp;
        }
  | SYSTEM_EXCLUSIVE ->
      SYSTEM_EXCLUSIVE
        {
          data = midi_message.data1;
          sysex_data = midi_message.sysex_data;
          timestamp = midi_message.timestamp;
        }
  | TIME_CODE ->
      TIME_CODE
        {
          value = char_of_int midi_message.data1;
          timestamp = midi_message.timestamp;
        }
  | SONG_POSITION ->
      SONG_POSITION
        {
          position = (midi_message.data2 lsl 7) lor midi_message.data1;
          timestamp = midi_message.timestamp;
        }
  | SONG_SELECT ->
      SONG_SELECT
        {
          song = char_of_int midi_message.data1;
          timestamp = midi_message.timestamp;
        }
  | TUNE_REQUEST ->
      TUNE_REQUEST { timestamp = midi_message.timestamp }
  | END_OF_EXCLUSIVE ->
      END_OF_EXCLUSIVE { timestamp = midi_message.timestamp }
  | TIMING_CLOCK ->
      TIMING_CLOCK { timestamp = midi_message.timestamp }
  | START -> START { timestamp = midi_message.timestamp }
  | CONTINUE -> CONTINUE { timestamp = midi_message.timestamp }
  | STOP -> STOP { timestamp = midi_message.timestamp }
  | ACTIVE_SENSING ->
      ACTIVE_SENSING { timestamp = midi_message.timestamp }
  | SYSTEM_RESET ->
      SYSTEM_RESET { timestamp = midi_message.timestamp }


let of_specific_midi_message (message : specific_midi_message) : GENERIC_MIDI_MESSAGE.t =
  match message with
  | NOTE_OFF { note; velocity; channel; timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:NOTE_OFF ~channel ~data1:(int_of_char note) ~data2:(int_of_char velocity) ~timestamp ~sysex_data:None ()
  | NOTE_ON { note; velocity; channel; timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:NOTE_ON ~channel ~data1:(int_of_char note) ~data2:(int_of_char velocity) ~timestamp ~sysex_data:None ()
  | POLY_PRESSURE { note; pressure; channel; timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:POLY_PRESSURE ~channel ~data1:(int_of_char note) ~data2:(int_of_char pressure) ~timestamp ~sysex_data:None ()
  | CONTROL_CHANGE { controller; value; channel; timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:CONTROL_CHANGE ~channel ~data1:(int_of_char controller) ~data2:(int_of_char value) ~timestamp ~sysex_data:None ()
  | PROGRAM_CHANGE { program; channel; timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:PROGRAM_CHANGE ~channel ~data1:(int_of_char program) ~data2:0 ~timestamp ~sysex_data:None ()
  | CHANNEL_PRESSURE { pressure; channel; timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:CHANNEL_PRESSURE ~channel ~data1:(int_of_char pressure) ~data2:0 ~timestamp ~sysex_data:None ()
  | PITCH_BEND { value; channel; timestamp } ->
      let data1 = value land 0x7F in
      let data2 = (value lsr 7) land 0x7F in
      GENERIC_MIDI_MESSAGE.create ~message_type:PITCH_BEND ~channel ~data1 ~data2 ~timestamp ~sysex_data:None ()
  | SYSTEM_EXCLUSIVE { data; sysex_data=_; timestamp } ->
      let sysex_data =  Bytes.make 1 (char_of_int data) |> String.of_bytes in
      GENERIC_MIDI_MESSAGE.create ~message_type:SYSTEM_EXCLUSIVE ~channel:0 ~data1:0 ~data2:0 ~timestamp ~sysex_data:(Some sysex_data) ()
  | TIME_CODE { value; timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:TIME_CODE ~channel:0 ~data1:(int_of_char value) ~data2:0 ~timestamp ~sysex_data:None ()
  | SONG_POSITION { position; timestamp } ->
      let data1 = position land 0x7F in
      let data2 = (position lsr 7) land 0x7F in
      GENERIC_MIDI_MESSAGE.create ~message_type:SONG_POSITION ~channel:0 ~data1 ~data2 ~timestamp ~sysex_data:None ()
  | SONG_SELECT { song; timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:SONG_SELECT ~channel:0 ~data1:(int_of_char song) ~data2:0 ~timestamp ~sysex_data:None ()
  | TUNE_REQUEST { timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:TUNE_REQUEST ~channel:0 ~data1:0 ~data2:0 ~timestamp ~sysex_data:None ()
  | END_OF_EXCLUSIVE { timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:END_OF_EXCLUSIVE ~channel:0 ~data1:0 ~data2:0 ~timestamp ~sysex_data:None ()
  | TIMING_CLOCK { timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:TIMING_CLOCK ~channel:0 ~data1:0 ~data2:0 ~timestamp ~sysex_data:None ()
  | START { timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:START ~channel:0 ~data1:0 ~data2:0 ~timestamp ~sysex_data:None ()
  | CONTINUE { timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:CONTINUE ~channel:0 ~data1:0 ~data2:0 ~timestamp ~sysex_data:None ()
  | STOP { timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:STOP ~channel:0 ~data1:0 ~data2:0 ~timestamp ~sysex_data:None ()
  | ACTIVE_SENSING { timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:ACTIVE_SENSING ~channel:0 ~data1:0 ~data2:0 ~timestamp ~sysex_data:None ()
  | SYSTEM_RESET { timestamp } ->
      GENERIC_MIDI_MESSAGE.create ~message_type:SYSTEM_RESET ~channel:0 ~data1:0 ~data2:0 ~timestamp ~sysex_data:None ()