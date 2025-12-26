
let (>>=) r f = Result.bind r f

let (let*) = Result.bind


let result_all_map f results =
  List.fold_right
    (fun result acc ->
      match result, acc with
      | Ok x, Ok xs -> Ok (x :: xs)
      | Error e, _ -> Error e
      | _, Error e -> Error e)
    results 
    (Ok []) 
    |> Result.map f

let result_all results =
  List.fold_right
    (fun result acc ->
      match result, acc with
      | Ok x, Ok xs -> Ok (x :: xs)
      | Error e, _ -> Error e
      | _, Error e -> Error e)
      results
      (Ok [])
