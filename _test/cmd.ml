open Cmd

let build_test =
  let expected = "test prefix -firstflag -secondflag value" in
  let output = ("test prefix", [])
  |> Cmd.(add_flag (Some (Fsingle "-firstflag")))
  |> Cmd.(add_flag (Some (Fdouble ("-secondflag",  "value"))))
  |> Cmd.to_string
  in assert String.(output = expected)
