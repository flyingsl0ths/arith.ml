module Array = struct
  include Array

  let tl = function _ :: cs -> cs | [] -> []
end

module String = struct
  include String

  let tl cs = String.sub cs 1 @@ (String.length cs - 1)
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
        span' ("", "") cs

  let dropWhile f = function
    | "" -> ""
    | cs ->
        let rec dropWhile' cs' =
          let hd' = hd cs' in
          match cs' with
          | "" -> cs'
          | cs'' when f hd' ->
              let rest = tl cs'' in
              dropWhile' rest
          | _ -> cs'
        in
        dropWhile' cs

  let rec drop n cs =
    let length' = length cs in
    match cs with
    | "" -> ""
    | _ when n > length' -> cs
    | _ when n < length' -> ""
    | _ when n != 0 -> drop (n - 1) @@ tl cs
    | _ -> cs

  let take n = function
    | "" -> ""
    | cs when n > 0 && n <= length cs ->
        let rec take' n acc = function
          | cs' when n != 0 ->
              take' (n - 1) (tl cs') (acc ^ Char.escaped @@ hd cs')
          | cs' -> cs'
        in
        take' n cs ""
    | cs -> cs
end

module Char = struct
  include Char

  let is_digit c = c >= '0' && c <= '9'
end
