open Printer

let help_test =
  let expected =
    "drop-merlin\ncreate a .merlin file in project directory for better lsp integration.\nin addition, you will have to pass \"--fallback-read-dot-merlin\" flag to ocamllsp command\n"
  in
  let in_chan, out_chan =
    let in_d, out_d = Unix.pipe () in
    Unix.in_channel_of_descr in_d, Unix.out_channel_of_descr out_d
  in
  set_output out_chan;
  help "drop-merlin";
  Out_channel.close out_chan;
  let data = In_channel.input_all in_chan in
  assert String.(expected = data)
