open Rtpmidi
open Midi

let device ~channel () = Midi.Device.create (channel + 1)

let write_midi_message device message =
  match message.GENERIC_MIDI_MESSAGE.message_type with
  | NOTE_ON ->
      [
        message_on
          ~note:(char_of_int message.data1)
          ~timestamp:(Int32.of_int message.timestamp)
          ~volume:(char_of_int message.data2)
          ~channel:message.channel ();
      ]
      |> write_output @@ device
  | NOTE_OFF ->
      [
        message_off
          ~note:(char_of_int message.data1)
          ~timestamp:(Int32.of_int message.timestamp)
          ~volume:(char_of_int message.data2)
          ~channel:message.channel ();
      ]
      |> write_output @@ device
  | POLY_PRESSURE ->
      [
        message_poly_pressure
          ~note:(char_of_int message.data1)
          ~pressure:(char_of_int message.data2)
          ~timestamp:(Int32.of_int message.timestamp)
          ~channel:message.channel ();
      ]
      |> write_output @@ device
  | CONTROL_CHANGE ->
      [
        message_control_change
          ~controller:(char_of_int message.data1)
          ~value:(char_of_int message.data2)
          ~timestamp:(Int32.of_int message.timestamp)
          ~channel:message.channel ();
      ]
      |> write_output @@ device
  | PROGRAM_CHANGE ->
      [
        message_program_change
          ~program:(char_of_int message.data1)
          ~timestamp:(Int32.of_int message.timestamp)
          ~channel:message.channel ();
      ]
      |> write_output @@ device
  | CHANNEL_PRESSURE ->
      [
        message_channel_pressure
          ~pressure:(char_of_int message.data1)
          ~timestamp:(Int32.of_int message.timestamp)
          ~channel:message.channel ();
      ]
      |> write_output @@ device
  | PITCH_BEND ->
      [
        message_pitch_bend
          ~value:((message.data2 lsl 7) lor message.data1)
          ~timestamp:(Int32.of_int message.timestamp)
          ~channel:message.channel ();
      ]
      |> write_output @@ device
  | SYSTEM_EXCLUSIVE ->
      [
        message_system_exclusive
          ~data:(char_of_int message.data1)
          ~timestamp:(Int32.of_int message.timestamp)
          ();
      ]
      |> write_output @@ device
  | TIME_CODE ->
      [
        message_time_code
          ~value:(char_of_int message.data1)
          ~timestamp:(Int32.of_int message.timestamp)
          ();
      ]
      |> write_output @@ device
  | SONG_POSITION ->
      [
        message_song_position
          ~position:((message.data2 lsl 7) lor message.data1)
          ~timestamp:(Int32.of_int message.timestamp)
          ();
      ]
      |> write_output @@ device
  | SONG_SELECT ->
      [
        message_song_select
          ~song:(char_of_int message.data1)
          ~timestamp:(Int32.of_int message.timestamp)
          ();
      ]
      |> write_output @@ device
  | TUNE_REQUEST ->
      [ message_tune_request ~timestamp:(Int32.of_int message.timestamp) () ]
      |> write_output @@ device
  | END_OF_EXCLUSIVE ->
      [
        message_end_of_exclusive ~timestamp:(Int32.of_int message.timestamp) ();
      ]
      |> write_output @@ device
  | TIMING_CLOCK ->
      [ message_timing_clock ~timestamp:(Int32.of_int message.timestamp) () ]
      |> write_output @@ device
  | START ->
      [ message_start ~timestamp:(Int32.of_int message.timestamp) () ]
      |> write_output @@ device
  | CONTINUE ->
      [ message_continue ~timestamp:(Int32.of_int message.timestamp) () ]
      |> write_output @@ device
  | STOP ->
      [ message_stop ~timestamp:(Int32.of_int message.timestamp) () ]
      |> write_output @@ device
  | ACTIVE_SENSING ->
      [ message_active_sensing ~timestamp:(Int32.of_int message.timestamp) () ]
      |> write_output @@ device
  | SYSTEM_RESET ->
      [ message_system_reset ~timestamp:(Int32.of_int message.timestamp) () ]
      |> write_output @@ device
