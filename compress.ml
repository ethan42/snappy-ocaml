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

let compress ?(limit=10000) str =
  let input_length = String.length str in
  match snappy_compress str input_length limit with
  | SNAPPY_OK, compr, compr_length ->
    BatString.init compr_length (fun i -> compr.(i))
  | _, _, _ ->
    failwith "Failed to compress"

let decompress ?(limit=10000) str =
  let input_length = String.length str in
  match snappy_uncompress str input_length limit with
  | SNAPPY_OK, uncompr, uncompr_length ->
    BatString.init uncompr_length (fun i -> uncompr.(i))
  | _ ->
    failwith "Failed to uncompress"

let () =
  let filename = Sys.argv.(1) in
  let contents = file_to_string filename in
  let compressed = compress contents in
  output_string stdout compressed;
  let decompressed = decompress compressed in
  output_string stdout decompressed
