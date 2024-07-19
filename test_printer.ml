open Printer

let help_test =
  let in_chan, out_chan =
    let in_d, out_d = Unix.pipe () in
    Unix.in_channel_of_descr in_d, Unix.out_channel_of_descr out_d
  in
  set_output out_chan;
  help "test_name" ["one line"; "two line"; "three lines"];
  Out_channel.close out_chan;
  let data = In_channel.input_all in_chan in
  assert (String.equal "test_name\none line\ntwo line\nthree lines\n" data)
