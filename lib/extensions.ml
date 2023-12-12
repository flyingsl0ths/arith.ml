let ( <<< ) f g x = f @@ g x
let rec until pred f a = if pred a then a else until pred f @@ f a

module Array = struct
  include Array

  let tl = function _ :: cs -> cs | [] -> []
end

module String = struct
  include String

  let tl = function "" -> "" | cs -> String.sub cs 1 (String.length cs - 1)
  let hd = function "" -> ' ' | cs -> String.get cs 0
  let null = function "" -> true | _ -> false

  let span f = function
    | "" -> ("", "")
    | cs ->
        let rec span' acc cs' =
          let hd' = hd cs' in
          match cs' with
          | "" -> acc
          | cs'' when f hd' ->
              let rest = tl cs'' in
              span' (fst acc ^ Char.escaped @@ hd', rest) rest
          | _ -> acc
        in
        span' ("", cs) cs

  let dropWhile f = function
    | "" -> ""
    | cs ->
        let rec dropWhile' cs' =
          let hd' = hd cs' in
          match cs' with
          | cs'' when f hd' ->
              let rest = tl cs'' in
              dropWhile' rest
          | _ -> cs'
        in
        dropWhile' cs

  let drop n cs =
    let length' = length cs in
    let rec drop' n' cs' =
      match cs' with _ when n' != 0 -> drop' (n' - 1) @@ tl cs' | _ -> cs'
    in
    match cs with
    | _ when n <= 0 -> cs
    | _ when n > length' -> ""
    | _ -> drop' n cs

  let take n = function
    | "" -> ""
    | cs when n > 0 && n <= length cs ->
        let rec take' n' acc = function
          | cs' when n' != n ->
              take' (n' + 1) (acc ^ Char.escaped @@ hd cs') @@ tl cs'
          | _ -> acc
        in
        take' 0 "" cs
    | cs -> cs
end

module Char = struct
  include Char

  let is_digit c = c >= '0' && c <= '9'
end
