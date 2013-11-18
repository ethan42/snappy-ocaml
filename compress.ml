open Snappy

let file_to_string filename =
  let file = open_in filename in
  let rec read_all bytes =
    let tryagain, byte =
      try true, input_byte file
      with End_of_file -> (false, 0)
    in
    if tryagain then
      read_all (byte :: bytes)
    else
      BatString.of_list (List.rev_map char_of_int bytes)
  in
  let input = read_all [] in
  close_in file;
  input


let () =
  let filename = Sys.argv.(1) in
  let contents = file_to_string filename in
  let input_length = String.length contents in
  match snappy_compress contents input_length 100000 with
  | SNAPPY_OK, compr, compr_length ->
    let compr = BatString.init compr_length (fun i -> compr.(i)) in
    output_string stdout compr
  | _, _, _ ->
    failwith "Failed to compress"
