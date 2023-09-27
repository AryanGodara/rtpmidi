open Midi_util.Syntax
module Event = Portmidi.Portmidi_event

(* ? MODULE DEVICE *)
module Device = struct
  type t = { device_id : int; device : Portmidi.Output_stream.t }

  let create device_id =
    Printf.printf "Creating device with id %s\n" (string_of_int device_id);
    match Portmidi.open_output ~device_id ~buffer_size:0l ~latency:1l with
    | Error _ ->
        Printf.printf "Can't find midi device with id: %i\nIs it connected?\n"
          device_id;
        exit 1
    | Ok device -> { device; device_id }

  let shutdown { device; device_id = _ } =
    print_endline "Shutting down midi device";
    let* _ =
      Portmidi.write_output device
        [
          Event.create ~status:'\176' ~data1:'\123' ~data2:'\000' ~timestamp:0l;
        ]
    in
    Portmidi.close_output device
end

(** Portmidi)error has function to convert it to type sexp; then use sexp
    library to convert it to string *)
let error_to_string msg =
  Portmidi.Portmidi_error.sexp_of_t msg |> Sexplib0.Sexp.to_string

let init () =
  Printf.printf "Initializing Midi";
  match Portmidi.initialize () with
  | Ok () -> ()
  | Error _ -> failwith "error initializing portmidi"

let message_on ~note ~timestamp ~volume ~channel () =
  let channel = 15 land channel in
  let status = char_of_int (144 lor channel) in
  Event.create ~status ~data1:note ~data2:volume ~timestamp

let message_off ~note ~timestamp ~volume ~channel () =
  let channel = 15 land channel in
  let status = char_of_int (128 lor channel) in
  Event.create ~status ~data1:note ~data2:volume ~timestamp

let message_poly_pressure ~note ~pressure ~timestamp ~channel () =
  let channel = 15 land channel in
  let status = char_of_int (160 lor channel) in
  Event.create ~status ~data1:note ~data2:pressure ~timestamp

let message_control_change ~controller ~value ~timestamp ~channel () =
  let channel = 15 land channel in
  let status = char_of_int (176 lor channel) in
  Event.create ~status ~data1:controller ~data2:value ~timestamp

let message_program_change ~program ~timestamp ~channel () =
  let channel = 15 land channel in
  let status = char_of_int (192 lor channel) in
  Event.create ~status ~data1:program ~timestamp ~data2:(char_of_int 0)

let message_channel_pressure ~pressure ~timestamp ~channel () =
  let channel = 15 land channel in
  let status = char_of_int (208 lor channel) in
  Event.create ~status ~data1:pressure ~timestamp ~data2:(char_of_int 0)

let message_pitch_bend ~value ~timestamp ~channel () =
  let channel = 15 land channel in
  let status = char_of_int (224 lor channel) in
  let data1 = char_of_int (value land 0x7f) in
  let data2 = char_of_int ((value lsr 7) land 0x7f) in
  Event.create ~status ~data1 ~data2 ~timestamp

let message_system_exclusive ~data ~timestamp () =
  let status = '\240' in
  Event.create ~status ~data1:data ~timestamp ~data2:(char_of_int 0)

let message_time_code ~value ~timestamp () =
  let status = '\241' in
  Event.create ~status ~data1:value ~timestamp ~data2:(char_of_int 0)

let message_song_position ~position ~timestamp () =
  let status = '\242' in
  let data1 = char_of_int (position land 0x7f) in
  let data2 = char_of_int ((position lsr 7) land 0x7f) in
  Event.create ~status ~data1 ~data2 ~timestamp

let message_song_select ~song ~timestamp () =
  let status = '\243' in
  Event.create ~status ~data1:song ~timestamp ~data2:(char_of_int 0)

let message_tune_request ~timestamp () =
  let status = '\246' in
  Event.create ~status ~timestamp ~data1:(char_of_int 0) ~data2:(char_of_int 0)

let message_end_of_exclusive ~timestamp () =
  let status = '\247' in
  Event.create ~status ~timestamp ~data1:(char_of_int 0) ~data2:(char_of_int 0)

let message_timing_clock ~timestamp () =
  let status = '\248' in
  Event.create ~status ~timestamp ~data1:(char_of_int 0) ~data2:(char_of_int 0)

let message_start ~timestamp () =
  let status = '\250' in
  Event.create ~status ~timestamp ~data1:(char_of_int 0) ~data2:(char_of_int 0)

let message_continue ~timestamp () =
  let status = '\251' in
  Event.create ~status ~timestamp ~data1:(char_of_int 0) ~data2:(char_of_int 0)

let message_stop ~timestamp () =
  let status = '\252' in
  Event.create ~status ~timestamp ~data1:(char_of_int 0) ~data2:(char_of_int 0)

let message_active_sensing ~timestamp () =
  let status = '\254' in
  Event.create ~status ~timestamp ~data1:(char_of_int 0) ~data2:(char_of_int 0)

let message_system_reset ~timestamp () =
  let status = '\255' in
  Event.create ~status ~timestamp ~data1:(char_of_int 0) ~data2:(char_of_int 0)

let handle_error = function
  | Ok _ -> ()
  | Error e -> Printf.printf "Encountered error: %s\n" (error_to_string e)

let write_output { Device.device; Device.device_id = _ } msg =
  Portmidi.write_output device msg |> handle_error
